(in-package :sl)
(import '(sl-ffi:defun-js))

;; (defun load (pathspec &key (verbose *load-verbose*) (print *load-print*)
;;              (if-does-not-exist t) (external-format :default))
;;   "Load the file given by PATHSPEC into the Lisp environment, returning
;;    T on success."
;;   (flet ()
;;     ;; Case 1: stream.
;;     (when (streamp pathspec)
;;       (return-from load (load-stream pathspec (fasl-header-p pathspec))))
;;     (let ((pathname (pathname pathspec)))
;;       ;; Case 2: Open as binary, try to process as a fasl.
;;       (with-open-stream
;;           (stream (or (open pathspec :element-type '(unsigned-byte 8)
;;                             :if-does-not-exist nil)
;;                       (when (null (pathname-type pathspec))
;;                         (let ((defaulted-pathname
;;                                (probe-load-defaults pathspec)))
;;                           (if defaulted-pathname
;;                               (progn (setq pathname defaulted-pathname)
;;                                      (open pathname
;;                                            :if-does-not-exist
;;                                            (if if-does-not-exist :error nil)
;;                                            :element-type '(unsigned-byte 8))))))
;;                       (if if-does-not-exist
;;                           (error 'simple-file-error
;;                                  :pathname pathspec
;;                                  :format-control
;;                                  "~@<Couldn't load ~S: file does not exist.~@:>"
;;                                  :format-arguments (list pathspec)))))
;;         (unless stream
;;           (return-from load nil))
;;         (let* ((real (probe-file stream))
;;                (should-be-fasl-p
;;                 (and real (string-equal (pathname-type real) *fasl-file-type*))))
;;           ;; Don't allow empty .fasls, and assume other empty files
;;           ;; are source files.
;;           (when (and (or should-be-fasl-p (not (eql 0 (file-length stream))))
;;                      (fasl-header-p stream :errorp should-be-fasl-p))
;;             (return-from load (load-stream stream t)))))
;;       ;; Case 3: Open using the gived external format, process as source.
;;       (with-open-file (stream pathname :external-format external-format)
;;         (load-stream stream nil)))))
