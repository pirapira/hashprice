;;;; for testing

(define difficulty 5006860589)
(define reward 25)
(define fee_per_block 0.08926687)
(define usdbtc 435)

;;;; for testing done



(define total_hash_until_block
  (* difficulty (expt 2 32)))

(define btc_hash
  (/ (+ reward fee_per_block)
     total_hash_until_block))
;; is this really the btc / hash?


(define hash_price
  (* usdbtc btc_hash))
