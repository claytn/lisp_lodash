;;; Lisp Implemenations of Lodash Collection functions

;; every: given a list and a function, checks the function returns true for all values in the list
(defun _every (plist proc)
  (defun every-iter (arr)
    (if (null arr) t
      (if (funcall proc (car arr)) (every-iter (cdr arr)) nil)))
  (if (null plist) nil (every-iter plist)))

;; filter: given a list and a function, returns a list of the values that made the function return true
(defun _filter (plist proc)
  (if (null plist) nil
    (if (funcall proc (car plist))
      (cons (car plist) (_filter (cdr plist) proc))
      (_filter (cdr plist) proc))))

;; find: given a list and a function, returns the first element in the list that make the given function return a truthy value
(defun _find (plist proc)
  (if (null plist) nil
    (if (funcall proc (car plist)) (car plist)
      (_find (cdr plist) proc))))

;; forEach: given a list and a function runs each element in the list through the function
(defun _forEach (plist proc)
  (if (null plist) nil
    (progn
      (funcall proc (car plist))
      (_forEach (cdr plist) proc))))

;; map: given a list and a function returns a new list containing the return values of each element in the original list run through the function
(defun _map (plist proc)
  (if (null plist) nil
    (cons (funcall proc (car plist)) (_map (cdr plist) proc))))

;; size: given a list returns the length of the list
(defun _size (plist)
  (if (null plist) 0
    (+ 1 (_size plist))))

;; some: given a list and function, returns true if at least one of the elements makes the function evaluate to true
(defun _some (plist proc)
  (if (null plist) nil
    (if (funcall proc (car plist)) t (_some (cdr plist) proc))))
