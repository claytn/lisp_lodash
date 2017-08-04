;;; Lisp Implemenations of the Lodash Array functions.
;;I purposely will be using Common Lisp Lists instead of the Arrays. This provides more of a challenge to implement these functions which is why I created this repo in the first place.

;; find: returns the element that makes the provided function return a truthy value
(defun _find (plist proc)
  (if (null plist) nil
    (if (funcall proc (car plist)) (car plist)
        (_find (cdr plist) proc))))

;; findIndex: returns the index of the first item in the list that makes the provided function return a truthy value
(defun _findIndex (plist proc)
  (defun findIndex-iter (rest index)
    (if (null rest) nil
      (if (funcall proc (car rest)) index
          (findIndex-iter (cdr rest) (inc index)))))
  (findIndex-iter plist 0))

;; head: returns the first element in a provided list
(defun _head (plist) (car plist))

;; indexOf: returns the index of the first element in plist equal to value
(defun _indexOf (plist val)
  (defun indexOf-iter (rest index)
    (if (null rest) nil
      (if (eql (car rest) val) index
          (indexOf-iter (cdr rest) (inc index)))))
  (indexOf-iter plist 0))


;; last: returns the last element in a provided list
(defun _last (plist)
  (if (null (cdr plist)) (car plist)
    (_last (cdr plist))))


;; reverse: returns a reversed version of the passed in list. (This is not a deep reverse. Inner lists themselves will not contain reversed elements)
(defun _reverse (plist)
  (defun reverse-iter (rest revd)
    (if (null rest) revd
        (reverse-iter (cdr rest) (cons (car rest) revd))))
  (reverse-iter plist nil))
