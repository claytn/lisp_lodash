;;; Lisp Implemenations of Lodash Collection functions

;; filter: given a list and a function, returns a list of the values that made the function return true
(defun _filter (plist proc)
  (if (null plist) nil
    (if (funcall proc (car plist))
      (cons (car plist) (_filter (cdr plist) proc))
      (_filter (cdr plist) proc))))

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
