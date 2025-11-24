;; ------------------------------------------------------------
;; Prediction Market Smart Contract
;; Users bet STX on outcomes, and winners share the prize pool.
;; ------------------------------------------------------------

(define-constant ERR_BETTING_CLOSED    (err u100))
(define-constant ERR_ZERO_AMOUNT       (err u101))
(define-constant ERR_ALREADY_BET       (err u102))
(define-constant ERR_INVALID_OUTCOME   (err u103))
(define-constant ERR_NOT_AUTHORIZED    (err u104))
(define-constant ERR_WINNER_NOT_SET    (err u105))
(define-constant ERR_ALREADY_CLAIMED   (err u106))
(define-constant ERR_NOT_WINNER        (err u107))
(define-constant ERR_TRANSFER_FAILED   (err u108))

;; ------------------------------------------------------------
;; Contract state
;; ------------------------------------------------------------
(define-data-var admin principal tx-sender)

;; Market configuration
(define-data-var market-open bool true)
(define-data-var winning-outcome (optional uint) none) ;; 1 = Team A, 2 = Team B

;; Total bets
(define-data-var total-pool uint u0)
(define-data-var total-bets-teamA uint u0)
(define-data-var total-bets-teamB uint u0)

;; Store each bettors bet
(define-map bets principal
  (tuple (amount uint) (outcome uint) (claimed bool)))

;; ------------------------------------------------------------
;; Public: Place a bet on Team A (1) or Team B (2)
;; ------------------------------------------------------------
(define-public (place-bet (outcome uint) (amount uint))
  (begin
    (asserts! (var-get market-open) ERR_BETTING_CLOSED)
    (asserts! (> amount u0) ERR_ZERO_AMOUNT)
    (asserts! (is-none (map-get? bets tx-sender)) ERR_ALREADY_BET)
    (asserts! (or (is-eq outcome u1) (is-eq outcome u2)) ERR_INVALID_OUTCOME)
    
    (map-set bets tx-sender (tuple (amount amount) (outcome outcome) (claimed false)))
    (var-set total-pool (+ (var-get total-pool) amount))
    
    (if (is-eq outcome u1)
      (var-set total-bets-teamA (+ (var-get total-bets-teamA) amount))
      (var-set total-bets-teamB (+ (var-get total-bets-teamB) amount)))
    
    (ok (tuple (bettor tx-sender) (outcome outcome) (amount amount)))
  ))


;; ------------------------------------------------------------
;; Admin: Close betting (no more bets allowed)
;; ------------------------------------------------------------
(define-public (close-betting)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_NOT_AUTHORIZED)
    (var-set market-open false)
    (ok "Betting closed")
  )
)

;; ------------------------------------------------------------
;; Admin: Declare winning outcome (1 or 2)
;; ------------------------------------------------------------
(define-public (declare-winner (outcome uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_NOT_AUTHORIZED)
    (asserts! (or (is-eq outcome u1) (is-eq outcome u2)) ERR_INVALID_OUTCOME)
    (var-set winning-outcome (some outcome))
    (ok (tuple (winner outcome)))
  )
)

;; ------------------------------------------------------------
;; User: Claim reward after winner is declared
;; ------------------------------------------------------------
(define-public (claim-reward)
  (match (map-get? bets tx-sender) bet
    (let (
          (amount (get amount bet))
          (outcome (get outcome bet))
          (claimed (get claimed bet))
        )
      (begin
        (asserts! (is-eq claimed false) ERR_ALREADY_CLAIMED)
        (match (var-get winning-outcome) winner
          (let ((win-outcome winner))
            (asserts! (is-eq outcome win-outcome) ERR_NOT_WINNER)
            ;; determine pool and share
            (let (
                  (total-win-pool (if (is-eq win-outcome u1)
                                       (var-get total-bets-teamA)
                                       (var-get total-bets-teamB)))
                )
              (let (
                    (reward (/ (* (var-get total-pool) amount) total-win-pool))
                  )
                (map-set bets tx-sender (tuple (amount amount) (outcome outcome) (claimed true)))
                (match (as-contract (stx-transfer? reward (as-contract tx-sender) tx-sender))
                  success (ok (tuple (reward reward) (outcome outcome)))
                  error ERR_TRANSFER_FAILED
                )
              )
            )
          )
          ERR_WINNER_NOT_SET
        )
      )
    )
    (err u109) ;; no bet found
  )
)

;; ------------------------------------------------------------
;; Read-only functions for transparency
;; ------------------------------------------------------------
(define-read-only (get-bet (who principal))
  (ok (map-get? bets who))
)

(define-read-only (get-market-status)
  (ok (tuple
        (market-open (var-get market-open))
        (total-pool (var-get total-pool))
        (teamA (var-get total-bets-teamA))
        (teamB (var-get total-bets-teamB))
        (winner (var-get winning-outcome))
      ))
)

(define-read-only (get-admin)
  (ok (var-get admin))
)