;;; Lisp Implemenations of Lodash Math functions

;;TODO: reimplement simple math operations similar to rambda's operationBy functions.

;; add: accepts two numbers and returns their sum
(defun _add (augend addend) (+ augend addend))

;; divide: accepts two numbers and returns their quotient
(defun _divide (dividend divisor) (/ dividend divisor))


;; max: given a list returns the max value in the list
(defun _max (arr)
  (defun maxBy-iter (arr highest)
    (if (null arr) highest
      (if (> (car arr) highest)
        (maxBy-iter (cdr arr) (car arr))
        (maxBy-iter (cdr arr) highest))))

  (maxBy-iter (cdr arr) (car arr))
)

;; length: helper function that computes the length of a list

;; mean: given a list of numbers returns the mean of the values in the list
(defun _mean (arr)
  (defun length (arr)
    (if (null arr) 0
      (+ 1 (length (cdr arr))))
  )
  (/ (_sum arr) (length arr))
)

;; min: given a list and a criterion function to rank items returns the smallest ranked item in the list
(defun _min (arr proc)
  (defun maxBy-iter (arr highest)
    (if (null arr) highest
      (if (< (proc (car arr)) (proc highest))
        (maxBy-iter (cdr arr) (car arr))
        (maxBy-iter (cdr arr) highest))))

  (maxBy-iter (cdr arr) (car arr))
)

;; multiply accepts two numbers and returns their product
(defun _multiply (multiplier multiplicand) (* multiplier multiplicand))

;; subtract: accepts two numbers and returns the first - second
(defun _subtract (minuend subtrahend) (- minuend subtrahend))

;; sum: takes a list of numbers and returns their sum
(defun _sum (nums)
  (if (null nums) 0
    (+ (car nums) (_sum (cdr nums))))
)
