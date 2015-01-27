(defclass lm-cons ()
  ((internal-list :accessor internal-list :initarg :internal-list))
  (:default-initargs :internal-list '()))

(defvar *test-instance* (make-instance 'lm-cons))

(defun lm-listp (lisp-object)
  (typep lisp-object 'lm-cons))

(defmethod check-lm-list (lm-lisp-object func-location)
  (unless (lm-listp lm-lisp-object)
    (error "Non-list (improper list?) argument in ~S" func-location)))


;;; the following should generate an error
;;; (check-lm-list "whoops" "whoops")
;;; (for-each *test-instance* #'print)
;;; (setf *test-instance* (make-instance 'lm-cons :internal-list '("string1" "string2")))

(defmethod init ((obj lm-cons) a b)
  (make-instance (cons a b)))

(defmethod lm-for-each ((obj lm-cons) func)
  (dolist (item (internal-list obj))
    (funcall func item)))

(defmethod lm-map ((func function) (obj lm-cons))
  (mapcar func (internal-list obj)))

(defmethod lm-last ((obj lm-cons))
  (last (internal-list obj)))

(defmethod lm-reverse ((obj lm-cons))
  (reverse (internal-list obj)))

(defmethod lm-nreverse ((obj lm-cons))
  (nreverse (internal-list obj)))

(defmethod lm-append (lm-lists)
  (apply #'append (mapcar (lambda (lm-list) (internal-list obj)) lm-lists)))

(defmethod lm-nconc (lm-lists)
  (apply #'nconc (mapcar (lambda (lm-list) (internal-list obj)) lm-lists)))

(defmethod lm-revappend ((list lm-cons) (tail lm-cons))
  (lm-cons (reverse (internal-list list)) tail))

(defmethod lm-from-array (vector)
  (make-instance 'lm-cons :internal-list (map 'list (lambda (item) iitem) vector)))

(defmethod lm-length ((obj lm-cons))
  (length (internal-list obj)))

(defmethod lm-to-array ((obj lm-cons))
  (vector (internal-list obj)))

(defmethod lm-cons (obj1 obj2)
  (make-instance 'lm-cons :internal-list (cons obj1 obj2)))

(defmethod lm-dotp (obj1)
  (let ((list (internal-list obj1)))
    (if (consp list)
	(loop
	   (if (null list)
	       (return nil)
	       (if (atom list)
		   (return t)))
	   (setf list (cdr list)))
	nil)))

(defmethod lm-elt ((obj lm-cons) i)
  (elt (internal-list obj) i))

(defmethod lm-find ((obj lm-cons) item cmp)
  (find item (internal-list obj) :test cmp))

(defmethod lm-car ((obj lm-cons))
  (car (internal-list obj)))

(defmethod lm-cdr ((obj lm-cons))
  (make-instance 'lm-cons :internal-list (cdr (internal-list obj))))


(defmethod lm-caar (list)
  (car(car(list))))

(defmethod lm-cadr (list)
  (car(cdr(list))))

(defmethod lm-cdar (list)
  (cdr(car(list))))

(defmethod lm-cddr (list)
  (cdr(cdr(list))))

(defmethod lm-caaar (list)
  (car(car(car(list)))))

(defmethod lm-caadr (list)
  (car(car(cdr(list)))))

(defmethod lm-cadar (list)
  (car(cdr(car(list)))))

(defmethod lm-caddr (list)
  (car(cdr(cdr(list)))))

(defmethod lm-cdaar (list)
  (cdr(car(car(list)))))

(defmethod lm-cdadr (list)
  (cdr(car(cdr(list)))))

(defmethod lm-cddar (list)
  (cdr(cdr(car(list)))))

(defmethod lm-cdddr (list)
  (cdr(cdr(cdr(list)))))

(defmethod lm-caaaar (list)
  (car(car(car(car(list))))))

(defmethod lm-caaadr (list)
  (car(car(car(cdr(list))))))

(defmethod lm-caadar (list)
  (car(car(cdr(car(list))))))

(defmethod lm-caaddr (list)
  (car(car(cdr(cdr(list))))))

(defmethod lm-cadaar (list)
  (car(cdr(car(car(list))))))

(defmethod lm-cadadr (list)
  (car(cdr(car(cdr(list))))))

(defmethod lm-caddar (list)
  (car(cdr(cdr(car(list))))))

(defmethod lm-cadddr (list)
  (car(cdr(cdr(cdr(list))))))

(defmethod lm-cdaaar (list)
  (cdr(car(car(car(list))))))

(defmethod lm-cdaadr (list)
  (cdr(car(car(cdr(list))))))

(defmethod lm-cdadar (list)
  (cdr(car(cdr(car(list))))))

(defmethod lm-cdaddr (list)
  (cdr(car(cdr(cdr(list))))))

(defmethod lm-cddaar (list)
  (cdr(cdr(car(car(list))))))

(defmethod lm-cddadr (list)
  (cdr(cdr(car(cdr(list))))))

(defmethod lm-cdddar (list)
  (cdr(cdr(cdr(car(list))))))

(defmethod lm-cddddr (list)
  (cdr(cdr(cdr(cdr(list))))))
