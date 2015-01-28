; Copyright (c) 2005-2009 by Juliusz Chroboczek

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.


(in-package :clvm)


;;;; Tests of the low-level interface, which is not meant for human beings.
;;;; See the high-level interface below if you are looking for examples.

;;; A trivial lexer

(defun list-lexer (list)
  #'(lambda ()
      (let ((x (pop list)))
        (values x x))))

;;; A few grammars from the Dragon book

(defun make-grammar-4-31 ()
  (make-grammar :terminals '(+ * id lb rb)
                :start-symbol 'e
                :productions
                (list (make-production 'e '(e + tt))
                      (make-production 'e '(tt) :action #'identity)
                      (make-production 'tt '(tt * f))
                      (make-production 'tt '(f) :action #'identity)
                      (make-production 'f '(lb e rb))
                      (make-production 'f '(id) :action #'identity))))

;; This grammar is LALR(1) but not SLR

(defun make-grammar-4-20 ()
  (make-grammar :terminals '(id * =)
                :start-symbol 's
                :productions
                (list (make-production 's '(l = r))
                      (make-production 's '(r))
                      (make-production 'l '(* r))
                      (make-production 'l '(id))
                      (make-production 'r '(l)))))

(defun make-grammar-4-21 ()
  (make-grammar :terminals '(c d)
                :start-symbol 's
                :productions
                (list (make-production 's '(cc cc))
                      (make-production 'cc '(c cc))
                      (make-production 'cc '(d)))))

;;; Epsilon-reductions on the left and right side

(defun make-grammar-epsilon-left ()
  (make-grammar :terminals '(id)
                :start-symbol 's
                :productions
                (list (make-production 's '())
                      (make-production 's '(s id)))))

(defun make-grammar-epsilon-right ()
  (make-grammar :terminals '(id)
                :start-symbol 's
                :productions
                (list (make-production 's '())
                      (make-production 's '(id s)))))

(defun tests-low ()
  (let ((parser-4-31 (make-parser (make-grammar-4-31)))
        (parser-4-20 (make-parser (make-grammar-4-20)))
        (parser-4-21 (make-parser (make-grammar-4-21)))
        (parser-epsilon-left (make-parser (make-grammar-epsilon-left)))
        (parser-epsilon-right (make-parser (make-grammar-epsilon-right))))
    (flet ((parse (parser list) (parse-with-lexer (list-lexer list) parser)))
      (expect (parse parser-4-31 '(lb id + id * id rb))
              '(lb (id + (id * id)) rb))
      (expect (parse parser-4-31 '(lb id * id + id rb))
              '(lb ((id * id) + id) rb))
      (expect (parse parser-4-20 '(* id = * * id))
              '((* ((id))) = ((* ((* ((id))))))))
      (expect (parse parser-4-21 '(c d c d))
              '((c (d)) (c (d))))
      (expect (parse parser-epsilon-left '()) '())
      (expect (parse parser-epsilon-left '(id)) '(nil id))
      (expect (parse parser-epsilon-left '(id id)) '((nil id) id))
      (expect (parse parser-epsilon-right '()) '())
      (expect (parse parser-epsilon-right '(id)) '(id nil))
      (expect (parse parser-epsilon-right '(id id)) '(id (id nil)))
      t)))

;;;; Tests of the high-level interface

(defun digitp (c) (member c '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))

(defclass parse-string ()
  ((internal-string :accessor internal-string :initarg :internal-string)))

(defun make-parse-string (str)
  (make-instance 'parse-string :internal-string str))

(defun read-regexp (stream)
  (let ((str (read-escaped stream #\/ #\/ t))
	(mods
	 ((alpha-char-p c)
	  (let ((buffer (make-array 10 :element-type 'character
				    :fill-pointer 0)))
	    (do ((c c (read-char stream nil nil)))
		((not (member c '(#\y #\m #\g #\i)))
		 (unless (null c) (unread-char c stream))
		 (values 'id (string-downcase (copy-seq buffer))))
	      (vector-push-extend c buffer))))))
    (make-reg-exp str mods)))

(defun skip (stream expected)
  (unless (eq (read-char stream) expected)
    (error "Expecting character ~a from stream" expected)))

(defun read-escaped (stream start end include-escape)
  (skip stream start)
  (let ((escaped  nil)
	(str  ""))
    (let ((buffer (make-array 10 :element-type 'character
                                 :fill-pointer 0)))
      (do ((c c (read-char stream nil nil)))
	  ((and (not escaped) (eq c end))
	   (copy-seq buffer))
	(cond
	  (escaped (vector-push-extend c buffer)
		   (setf escaped nil))
	  ((eq c #\\)
	   (if include-escape (vector-push-extend c buffer))
	   (setf escape t))
	  (t (vector-push-extend c buffer)))))))


(defun simple-lexer (stream)
  (let ((c (read-char stream nil nil)))
    (cond
      ((null c) (values nil nil))
      ((member c '(#\, #\Space #\Tab #\Newline)) (simple-lexer stream))
      ((member c '(#\+ #\- #\* #\/ #\( #\) #\[ #\]))
       (let ((v (intern (string c))))
         (values v v)))
      ((digitp c)
       (let ((buffer (make-array 10 :element-type 'character
                                 :fill-pointer 0)))
         (do ((c c (read-char stream nil nil)))
             ((or (null c) (not (digitp c)))
              (unless (null c) (unread-char c stream))
              (values 'int (read-from-string buffer)))
           (vector-push-extend c buffer))))
      ((eq c #\")
       ;; read ordinary string
       (progn
	 (unread-char c stream)
	 (multiple-value-bind (str length)
	     (read stream)
	   (values 'str (make-parse-string str)))))
      ((eq c #\/)
       ;; read Javascript style regular expression
       (progn
	 (unread-char c stream)
	 (values 'str (read-regexp stream))))
      ((alpha-char-p c)
       (let ((buffer (make-array 10 :element-type 'character
                                 :fill-pointer 0)))
         (do ((c c (read-char stream nil nil)))
             ((or (null c) (not (or (alphanumericp c)
				    (eq #\_ c))))
              (unless (null c) (unread-char c stream))
              (values 'id (copy-seq buffer)))
           (vector-push-extend c buffer))))
      (t (error "Lexing error")))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun k-2-3 (a b c) (declare (ignore a c)) b)
)

(with-input-from-string (s e)
             (list (simple-lexer s)
		   (simple-lexer s)
		   (simple-lexer s)
		   (simple-lexer s)))

(setf e "(\"x\"+3)+y*z")
(setf e (concatenate 'string
		     "CONST(\"%\"),"
		     "PRIM(s(\"%FIND-PACKAGE\",\"%\"),1),"
		     ",GSET(s(\"*PACKAGE*\",\"%\")),"
		     "POP()"))


(define-parser *fasl-parser*
  (:start-symbol args)
  (:terminals (int str id + - * / |(| |)| |[| |]|))
  (:precedence ((:left * /) (:left + -)))
  (expression
   (id |(| args |)|)
   (|[| args |]|)
   (|[| |]|)
   (id |(| |)|)
   id
   int
   str
   (|(| expression |)| #'k-2-3))
  (args
   expression
   (args expression)))

(defun parse (parser filename)
  (with-open-file (stream filename)
    (parse-with-lexer #'(lambda () (simple-lexer stream)) parser)))

(defun tests-hi ()
  (flet ((parse (parser e)
           (with-input-from-string (s e)
             (parse-with-lexer #'(lambda () (simple-lexer s)) parser))))
    (let ((*package* (find-package '#:yacc-tests)))
      (let ((e "(x+3)+y*z") (v '(("x" + 3) + ("y" * "z"))))
        (expect (parse *left-expression-parser* e) v)
        (expect (parse *precedence-left-expression-parser* e) v)
        (expect (parse *precedence-right-expression-parser* e) v)
        (expect (parse *precedence-nonassoc-expression-parser* e) v))
      (let ((e "x+5/3*(12+y)/3+z"))
        (let ((v '(("x" + (((5 / 3) * (12 + "y")) / 3)) + "z")))
          (expect (parse *left-expression-parser* e) v)
          (expect (parse *precedence-left-expression-parser* e) v))
        (let ((v '("x" + ((5 / (3 * ((12 + "y") / 3))) + "z"))))
          (expect (parse *precedence-right-expression-parser* e) v))
        (let ((v '("x" + (5 / (3 * ((12 + "y") / (3 + "z")))))))
          (expect (parse *ambiguous-expression-parser* e) v))
        (expect-condition yacc-parse-error
          (parse *precedence-nonassoc-expression-parser* e)))
      (dolist (e '("5/3*(" "5/3)"))
        (expect-condition yacc-parse-error
          (parse *left-expression-parser* e))
        (expect-condition yacc-parse-error
          (parse *ambiguous-expression-parser* e))
        (expect-condition yacc-parse-error
          (parse *precedence-left-expression-parser* e))
        (expect-condition yacc-parse-error
          (parse *precedence-right-expression-parser* e)))))
  t)

(defun tests ()
  (tests-low)
  (tests-hi)
  t)
