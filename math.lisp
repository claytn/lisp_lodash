;;; Lisp Implemenations of Lodash Math functions

;; addBy: accepts a single addend and returns a function that will add a given number by the addend
(defun _addBy (addend) (lambda (augend) (+ augend addend)))

;; divideBy: accepts a single divisor and returns a function that accepts a dividend and returns the quotient of the dividend and divisor
(defun _divideBy (divisor) (lambda (dividend) (/ dividend divisor)))

;;;TODO: GENERALIZE THE MAX/MIN PROCEDURES

;; max: given a list returns the max value in the list
(defun _max (nlist)
  (defun max-iter (arr highest)
    (if (null arr) highest
      (if (> (car arr) highest)
        (max-iter (cdr arr) (car arr))
        (max-iter (cdr arr) highest))))

  (max-iter (cdr nlist) (car nlist))
)

;; maxBy: accepts a list and a function to determine the rank of the elements within the list. Returns the highest ranked element within the list.
(defun _maxBy (plist proc)
  (defun maxBy-iter (arr highest)
    (if (null arr) highest
      (if (> (funcall proc (car arr)) (funcall proc highest))
        (maxBy-iter (cdr arr) (car arr))
        (maxBy-iter (cdr arr) highest))))

  (maxBy-iter (cdr plist) (car plist))
)

;; helper function for finding length of a list
(defun length (arr)
  (if (null arr) 0
    (+ 1 (length (cdr arr))))
)

;; mean: given a list of numbers returns the mean of the values in the list
(defun _mean (arr)
  (/ (_sum arr) (length arr)))

;; meanBy: given a list and a function, returns the mean of the values returned by the passed in function.
(defun _meanBy (arr proc)
  (/ (_sumBy arr proc) (length arr)))

;; min: given a list and a criterion function to rank items returns the smallest ranked item in the list
(defun _min (nlist proc)
  (defun maxBy-iter (arr highest)
    (if (null arr) highest
      (if (< car arr) highest)
        (maxBy-iter (cdr arr) (car arr))
        (maxBy-iter (cdr arr) highest)))

  (maxBy-iter (cdr nlist) (car nlist))
)

;; minBy: accepts a list and a function to determine the rank of the elements within the list. Returns the lowest ranked element within the list.
(defun _minBy (plist proc)
  (defun minBy-iter (arr lowest)
    (if (null arr) lowest
      (if (< (funcall proc (car arr)) (funcall proc lowest))
        (maxBy-iter (cdr arr) (car arr))
        (maxBy-iter (cdr arr) lowest))))

  (minBy-iter (cdr plist) (car plist))
)

;; multiply accepts two numbers and returns their product
(defun _multiplyBy (multiplicand) (lambda (multiplier) (* multiplier multiplicand)))

;; subtract: accepts two numbers and returns the first - second
(defun _subtractBy (subtrahend) (lambda (minuend) (- minuend subtrahend)))

;; sum: takes a list of numbers and returns their sum
(defun _sum (nums)
  (if (null nums) 0
    (+ (car nums) (_sum (cdr nums)))))

;; sumBy: accepts a list and a function which determines the value to be summed for a single list entry.
(defun _sumBy (arr proc)
  (if (null arr) 0
    (+ (funcall proc (car arr)) (_sum (cdr arr)))))
