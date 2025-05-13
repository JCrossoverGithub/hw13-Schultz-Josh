#lang 450lang



;; Data Definitions

;; A Queen is a List of three elements:
;;   - the symbol 'Q
;;   - an integer row in [1..n]
;;   - an integer column in [1..n]
;; Interp: a queen’s position on an n x n chessboard.
(bind/rec queen?
  (lm (x)
    (and (list? x)
         (not (empty? x))
         (symbol=? (1st x) 'Q)
         (integer? (2nd x))
         (integer? (1st (rst (rst x)))))))

;; Q : Integer Integer -> Queen
(bind/rec Q
  (lm (r c)
    (li 'Q r c)))

;; qx : Queen -> Integer
(bind/rec qx
  (lm (q)
    (2nd q)))

;; qy : Queen -> Integer
(bind/rec qy
  (lm (q)
    (1st (rst (rst q)))))


;; List Helpers

;; all? : (Any -> Boolean) List -> Boolean
(bind/rec all?
  (lm (pred lst)
    (cond
      [(empty? lst) (> 1 0)]
      [else
       (and (pred (1st lst))
            (all? pred (rst lst)))])))

;; any? : (Any -> Boolean) List -> Boolean
(bind/rec any?
  (lm (pred lst)
    (cond
      [(empty? lst) (> 0 1)]
      [else
       (or (pred (1st lst))
           (any? pred (rst lst)))])))

;; has? : Any List -> Boolean
(bind/rec has?
  (lm (val lst)
    (any? (lm (x) (~= x val))
          lst)))

;; has-dup? : List -> Boolean
(bind/rec has-dup?
  (lm (lst)
    (cond
      [(empty? lst)       (> 0 1)]
      [(empty? (rst lst)) (> 0 1)]
      [else
       (or (has? (1st lst) (rst lst))
           (has-dup? (rst lst)))])))


;; Safety Predicates

;; safe? : Queen Queen -> Boolean
(bind/rec safe?
  (lm (q1 q2)
    (bind (r1 (qx q1))
      (bind (c1 (qy q1))
        (bind (r2 (qx q2))
          (bind (c2 (qy q2))
            (bind (same-row?  (~= r1 r2))
              (bind (same-col?  (~= c1 c2))
                (bind (same-diag?
                       (= (abs (+ r1 (× r2 -1)))
                          (abs (+ c1 (× c2 -1)))))
                  (¬ (or same-row? same-col? same-diag?)))))))))))

;; safe-to-add? : Queen List -> Boolean
(bind/rec safe-to-add?
  (lm (q qs)
    (all? (lm (other) (safe? q other)) qs)))

;; all-safe? : List -> Boolean
(bind/rec all-safe?
  (lm (qs)
    (cond
      [(empty? qs)       (> 1 0)]
      [(empty? (rst qs)) (> 1 0)]
      [else
       (and (safe-to-add? (1st qs) (rst qs))
            (all-safe?    (rst qs)))])))


;; length: List -> Integer
(bind/rec length
  (lm (lst)
    (cond
      [(empty? lst) 0]
      [else (+ 1 (length (rst lst)))])))


;; nqueens: Integer -> List of Queen or false
(bind/rec nqueens
  (lm (n)
    (bind/rec (helper
               (lm (row acc)
                 (cond
                   [(> row n) acc]
                   [else
                    (bind/rec (try-col
                               (lm (col)
                                 (cond
                                   [(> col n) (> 0 1)]
                                   [else
                                    (bind (cand (Q row col))
                                      (if (safe-to-add? cand acc)
                                          (bind (res (helper (+ row 1)
                                                            (cns cand acc)))
                                            (if res res
                                                (try-col (+ col 1))))
                                          (try-col (+ col 1))))])))
                      (try-col 1))])))

      ;; Start the search at row 1 with an empty board
      (helper 1 mt))))


;; valid-solution? : Integer List -> Boolean
(bind/rec valid-solution?
  (lm (n qs)
    (and (= (length qs) n)
         (¬ (has-dup? qs))
         (all-safe? qs))))
