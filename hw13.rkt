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

;; all? : (Any->Boolean) List -> Boolean
(bind/rec all?
  (lm (pred lst)
    (iffy
      (~= lst mt)
      (> 1 0)
      (∧ (pred (1st lst))
         (all? pred (rst lst))))))

;; any? : (Any->Boolean) List -> Boolean
(bind/rec any?
  (lm (pred lst)
    (iffy
      (~= lst mt)
      (> 0 1)
      (∨ (pred (1st lst))
         (any? pred (rst lst))))))

;; has? : Any List -> Boolean
(bind/rec has?
  (lm (v lst)
    (any? (lm (x) (~= x v)) lst)))

;; has-dup? : List -> Boolean
(bind/rec has-dup?
  (lm (lst)
    (iffy
      (~= lst mt)
      (> 0 1)
      (iffy
        (~= (rst lst) mt)
        (> 0 1)
        (∨ (has? (1st lst) (rst lst))
           (has-dup? (rst lst)))))))


;; safe? : Queen Queen -> Boolean
(bind/rec safe?
  (lm (q1 q2)
    (¬
      (∨
        (~= (qx q1) (qx q2))
        (~= (qy q1) (qy q2))
        (~= (abs (+ (qx q1) (× (qx q2) -1)))
            (abs (+ (qy q1) (× (qy q2) -1))))))))

;; safe-to-add? : Queen ListofQueen -> Boolean
(bind/rec safe-to-add?
  (lm (q qs)
    (all? (lm (o) (safe? q o)) qs)))

;; all-safe? : ListofQueen -> Boolean
(bind/rec all-safe?
  (lm (qs)
    (iffy
      (~= qs mt)
      (> 1 0)
      (iffy
        (~= (rst qs) mt)
        (> 1 0)
        (∧ (safe-to-add? (1st qs) (rst qs))
           (all-safe? (rst qs)))))))

;; nqueens : Int -> ListofQ or 'FALSE!
(bind/rec nqueens
  (lm (n)
    (bind/rec [search
      (lm (row col acc)
        (iffy
          (> row n)
          acc
          (iffy
            (> col n)
            'FALSE!
            (bind [cand (Q row col)]
              (iffy
                (safe-to-add? cand acc)
                (bind [sol (search (+ row 1) 1 (cns cand acc)) ]
                  (iffy
                    (~= sol 'FALSE!)
                    sol
                    (search row (+ col 1) acc)))
                (search row (+ col 1) acc))))))]  

      (search 1 1 mt))))

;; valid-solution? : Integer ListofQueen -> Boolean
(bind/rec valid-solution?
  (lm (n qs)
    (∧
      (~= (length qs) n)
      (¬ (has-dup? qs))
      (all-safe? qs))))
