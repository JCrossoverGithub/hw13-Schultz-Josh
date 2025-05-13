#lang 450lang


;; Q‐Constructor and Accessors

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

;; all? : (Any->Boolean) List -> Boolean
(bind/rec all?
  (lm (pred lst)
    (iffy (~= lst mt)
          (> 1 0)
          (∧ (pred (1st lst))
             (all? pred (rst lst))))))

;; any? : (Any->Boolean) List -> Boolean
(bind/rec any?
  (lm (pred lst)
    (iffy (~= lst mt)
          (> 0 1)
          (∨ (pred (1st lst))
             (any? pred (rst lst))))))

;; has? : Any List -> Boolean
(bind/rec has?
  (lm (v lst)
    (any? (lm (x) (~= x v))
          lst)))

;; has-dup? : List -> Boolean
(bind/rec has-dup?
  (lm (lst)
    (iffy (~= lst mt)
          (> 0 1)
          (iffy (~= (rst lst) mt)
                (> 0 1)
                (∨ (has? (1st lst) (rst lst))
                   (has-dup? (rst lst)))))))

;; Safety Predicates

;; safe? : Q Q -> Boolean
;; True iff two queens do NOT share row, column, or diagonal.
(bind/rec safe?
  (lm (q1 q2)
    (bind [r1 (qx q1)]
      (bind [c1 (qy q1)]
        (bind [r2 (qx q2)]
          (bind [c2 (qy q2)]
            (¬
             (∨ (~= r1 r2)
                (~= c1 c2)
                (= (abs (+ r1 (× r2 -1)))
                   (abs (+ c1 (× c2 -1))))))))))))


;; safe-to-add? : Q ListofQ -> Boolean
(bind/rec safe-to-add?
  (lm (q qs)
    (all? (lm (o) (safe? q o)) qs)))

;; all-safe? : ListofQ -> Boolean
(bind/rec all-safe?
  (lm (qs)
    (iffy (~= qs mt)
          (> 1 0)
          (iffy (~= (rst qs) mt)
                (> 1 0)
                (∧ (safe-to-add? (1st qs) (rst qs))
                   (all-safe?    (rst qs)))))))

;; length

;; length : List -> Integer
(bind/rec length
  (lm (lst)
    (iffy (~= lst mt)
          0
          (+ 1 (length (rst lst))))))


;; nqueens : Int -> ListofQ or 'FALSE!
(bind/rec nqueens
  (lm (n)
    (bind/rec [helper
                (lm (row acc)
                  (iffy (> row n)
                        acc
                        (bind/rec [try-col
                                    (lm (col)
                                      (iffy (> col n)
                                            (> 0 1)
                                            (bind [cand (Q row col)])
                                              (bind [res
                                                      (helper
                                                        (+ row 1)
                                                        (cns cand acc))])
                                                (iffy (~= res (> 0 1))
                                                      res
                                                      (try-col (+ col 1)))))])
                            (try-col 1)))]
      (helper 1 mt))))

;; valid-solution? : Int ListofQ -> Bool
(bind/rec valid-solution?
  (lm (n qs)
    (∧ (= (length qs) n)
       (¬ (has-dup? qs))
       (all-safe? qs))))
