;;; Lisp Implemenations of Lodash Math functions

;; addBy: accepts a single addend and returns a function that will add a given number by the addend
(defun _addBy (addend) (lambda (augend) (+ augend addend)))

;; divideBy: accepts a single divisor and returns a function that accepts a dividend and returns the quotient of the dividend and divisor
(defun _divideBy (divisor) (lambda (dividend) (/ dividend divisor)))

;; minmax: helper function to generalize min/max functions
(defun minmax (nlist op)
  (defun minmax-iter (arr est)
    (if (null arr) est
      (if (funcall op (car arr) est)
        (minmax-iter (cdr arr) (car arr))
        (minmax-iter (cdr arr) est))))

  (minmax-iter (cdr nlist) (car nlist))
)

;; minmaxBy: helper function to generalize minBy/maxBy functions
(defun minmaxBy (plist proc op)
  (defun minmaxBy-iter (arr est)
    (if (null arr) est
      (if (funcall op (funcall proc (car arr)) (funcall proc est))
        (minmaxBy-iter (cdr arr) (car arr))
        (minmaxBy-iter (cdr arr) est))))

  (minmaxBy-iter (cdr plist) (car plist))
)

;; max: given a list returns the max value in the list
(defun _max (nlist)
  (_minmax nlist #'>))

;; maxBy: accepts a list and a function to determine the rank of the elements within the list. Returns the highest ranked element within the list.
(defun _maxBy (plist proc)
  (minmaxBy plist proc #'>))

;; mean: given a list of numbers returns the mean of the values in the list
(defun _mean (arr)
  (/ (_sum arr) (length arr)))

;; meanBy: given a list and a function, returns the mean of the values returned by the passed in function.
(defun _meanBy (arr proc)
  (/ (_sumBy arr proc) (length arr)))

;; min: given a list, returns the smallest ranked item in the list
(defun _min (nlist)
  (_minmax nlist #'<))

;; minBy: accepts a list and a function to determine the rank of the elements within the list. Returns element with lowest rank.
(defun _minBy (plist proc)
  (minmaxBy plist proc #'<))

;; multiply accepts a multiplicand and returns a function to accept a multiplier and return their product
(defun _multiplyBy (multiplicand) (lambda (multiplier) (* multiplier multiplicand)))

;; subtract: accepts a single subtrahend and returns a function that accepts another number and returns the difference
(defun _subtractBy (subtrahend) (lambda (minuend) (- minuend subtrahend)))

;; sum: takes a list of numbers and returns their sum
(defun _sum (nums)
  (if (null nums) 0
    (+ (car nums) (_sum (cdr nums)))))

;; sumBy: accepts a list and a function which determines the value to be summed for a single list entry.
(defun _sumBy (arr proc)
  (if (null arr) 0
    (+ (funcall proc (car arr)) (_sum (cdr arr)))))
