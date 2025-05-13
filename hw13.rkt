#lang 450lang


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

;; length : List -> Integer
(bind/rec length
  (lm (lst)
    (iffy (~= lst mt)
          0
          (+ 1 (length (rst lst))))))

;; Testing length
(chk= (length mt)                            0)   ;; empty list
(chk= (length (li 42))                       1)   ;; single-element list
(chk= (length (li 1 2 3 4 5))                 5)   ;; multiple elements
(chk= (length (li mt mt mt))                 3)   ;; list of lists
(chk= (length (cns 'a (cns 'b (rst (cns 'c mt))))) 3) ;; constructed with cns/rst

;; all? : (Any->Boolean) List -> Boolean
(bind/rec all?
  (lm (pred lst)
    (iffy (~= lst mt)
          (> 1 0)
          (∧ (pred (1st lst))
             (all? pred (rst lst))))))

;; Testing all?
;; Predicate always true
(chk  (all? (lm (x) TRUE!) mt)              )   ;; empty list => true
(chk  (all? (lm (x) (> x -1)) (li 0 5 10)))   ;; all non-negative
;; Predicate sometimes false
(chknot (all? (lm (x) (> x 5)) (li 1 6 10)))  ;; element 1 fails
(chknot (all? (lm (x) TRUE!) (cns FALSE! mt))) ;; contains FALSE!

;; any? : (Any->Boolean) List -> Boolean
(bind/rec any?
  (lm (pred lst)
    (iffy (~= lst mt)
          (> 0 1)
          (∨ (pred (1st lst))
             (any? pred (rst lst))))))

;; Testing any?
;; Empty list yields false
(chknot (any? (lm (x) TRUE!) mt))
(chknot (any? (lm (x) (> x 0)) mt))
;; At least one true
(chk  (any? (lm (x) (> x 0)) (li -1 0 1)))
(chk  (any? (lm (x) (~= x 'foo)) (li 'bar 'foo 'baz)))
;; No element satisfies
(chknot (any? (lm (x) (< x 0)) (li 1 2 3)))
(chknot (any? (lm (x) (~= x 'qux)) (li 'a 'b 'c)))

;; has? : Any List -> Boolean
(bind/rec has?
  (lm (v lst)
    (any? (lm (x) (~= x v))
          lst)))

;; Testing has?
(chknot (has? 'z mt))                       ;; empty list
(chk    (has? 3 (li 1 2 3 4)))               ;; present at end
(chk    (has? 'a (li 'a 'b 'c)))             ;; present at front
(chknot (has? 'x (li 'a 'b 'c)))             ;; absent
;; Duplicates detection uses ~=, so string/number equivalence
(chk    (has? "1" (li 1 "1" '1)))

;; has-dup? : List -> Boolean
(bind/rec has-dup?
  (lm (lst)
    (iffy (~= lst mt)
          (> 0 1)
          (iffy (~= (rst lst) mt)
                (> 0 1)
                (∨ (has? (1st lst) (rst lst))
                   (has-dup? (rst lst)))))))

;; Testing has-dup?
(chknot (has-dup? mt))                      ;; empty list
(chknot (has-dup? (li 1)))                   ;; one element
(chk    (has-dup? (li 1 1)))                 ;; adjacent duplicates
(chk    (has-dup? (li 'a 'b 'a 'c)))         ;; non-adjacent duplicates
(chknot (has-dup? (li 1 2 3 4 5)))           ;; all distinct


;; safe? : Queen Queen -> Boolean
(bind/rec safe?
  (lm (q1 q2)
    (¬
     (∨ (~= (qx q1) (qx q2))
        (~= (qy q1) (qy q2))
        (~= (abs (+ (qx q1) (× (qx q2) -1)))
            (abs (+ (qy q1) (× (qy q2) -1))))))))

;; Testing safe?
;; Same position => threat on diagonal => unsafe
(chknot (safe? (Q 1 1) (Q 1 1)))
;; Same row
(chknot (safe? (Q 2 3) (Q 2 5)))
;; Same column
(chknot (safe? (Q 4 6) (Q 1 6)))
;; Same diagonal (difference in rows = difference in cols)
(chknot (safe? (Q 1 1) (Q 3 3)))
(chknot (safe? (Q 5 2) (Q 3 4)))
;; Truly safe positions
(chk    (safe? (Q 1 4) (Q 2 6)))
(chk    (safe? (Q 3 2) (Q 5 5)))

;; safe-to-add? : Queen ListofQueen -> Boolean
(bind/rec safe-to-add?
  (lm (q qs)
    (all? (lm (o) (safe? q o))
          qs)))

;; Testing safe-to-add?
;; Adding to empty is always safe
(chk (safe-to-add? (Q 1 1) mt))
;; Safe addition to a safe list
(chk (safe-to-add? (Q 3 1) (li (Q 1 4) (Q 2 6))))
;; Unsafe addition (row conflict)
(chknot (safe-to-add? (Q 2 8) (li (Q 2 3) (Q 4 5))))
;; Unsafe addition (diagonal conflict)
(chknot (safe-to-add? (Q 5 5) (li (Q 3 3) (Q 2 7))))

;; all-safe? : ListofQueen -> Boolean
(bind/rec all-safe?
  (lm (qs)
    (iffy (~= qs mt)
          (> 1 0)
          (iffy (~= (rst qs) mt)
                (> 1 0)
                (∧ (safe-to-add? (1st qs) (rst qs))
                   (all-safe? (rst qs)))))))

;; Testing all-safe?
(chk  (all-safe? mt))                       ;; empty list
(chk  (all-safe? (li (Q 1 2) (Q 2 4) (Q 3 6)))) ;; no threats
(chknot (all-safe? (li (Q 1 1) (Q 2 2) (Q 3 3)))) ;; diagonal chain
(chknot (all-safe? (li (Q 1 5) (Q 1 6) (Q 2 3)))) ;; row conflict


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
                                          (bind [cand (Q row col)]
                                            (iffy (safe-to-add? cand acc)
                                                  (bind [sol
                                                         (helper
                                                           (+ row 1)
                                                           (cns cand acc))]
                                                    (iffy sol
                                                          sol
                                                          (try-col (+ col 1))))
                                                  (try-col (+ col 1))))))]
                            (try-col 1))))]
      (helper 1 mt))))

;; Testing nqueens
;; n = 0
(chk= (nqueens 0) mt)
;; n = 1 (only one placement)
(chk= (nqueens 1) (li (Q 1 1)))
(chk  (valid-solution? 1 (nqueens 1)))
;; No solutions exist for n=2,3
(chknot (nqueens 2))
(chknot (nqueens 3))
;; Classic 4-queens solution (one of several possible)
(chk  (valid-solution? 4 (nqueens 4)))
(chk= (length (nqueens 4)) 4)

;; valid-solution? : Integer ListofQueen -> Boolean
(bind/rec valid-solution?
  (lm (n qs)
    (∧ (~= (length qs) n)
       (¬ (has-dup? qs))
       (all-safe? qs))))


;; Testing valid-solution?
;; Correct solution for n=4
(chk  (valid-solution? 4
        (li (Q 2 4) (Q 4 1) (Q 1 3) (Q 3 2))))
;; Wrong length
(chknot (valid-solution? 5 (li (Q 1 1) (Q 2 2) (Q 3 3) (Q 4 4))))
;; Duplicate positions
(chknot (valid-solution? 4 (li (Q 1 2) (Q 2 3) (Q 1 2) (Q 4 1))))
;; Diagonal conflict
(chknot (valid-solution? 3 (li (Q 1 1) (Q 2 3) (Q 3 2))))
