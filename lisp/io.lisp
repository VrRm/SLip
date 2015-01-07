

(in-package :sl)
(import '(sl-ffi:defun-js))


;; (export '(load))

;; (defun-js primitive-load ()
;;   "window.recompile_all();")

;; (defun load ()
;;   (primitive-load))


;; (sb!impl::fd-stream-fd-type stream)

;; (defun load-stream (stream faslp)
;;   (when (and (fd-stream-p stream)
;; 	     (eq (sb!impl::fd-stream-fd-type stream) :directory))
;;     (error 'simple-file-error
;; 	   :pathname (pathname stream)
;; 	   :format-control
;; 	   "Can't LOAD a directory: ~s."
;; 	   :format-arguments (list (pathname stream))))
;;   (let* (;; Bindings required by ANSI.
;; 	 (*readtable* *readtable*)
;; 	 (*package* (sane-package))
;; 	 ;; FIXME: we should probably document the circumstances
;; 	 ;; where *LOAD-PATHNAME* and *LOAD-TRUENAME* aren't
;; 	 ;; pathnames during LOAD.  ANSI makes no exceptions here.
;; 	 (*load-pathname* (handler-case (pathname stream)
;; 			    ;; FIXME: it should probably be a type
;; 			    ;; error to try to get a pathname for a
;; 			    ;; stream that doesn't have one, but I
;; 			    ;; don't know if we guarantee that.
;; 			    (error () nil)))
;; 	 (*load-truename* (when *load-pathname*
;; 			    (handler-case (truename stream)
;; 			      (file-error () nil))))
;; 	 ;; Bindings used internally.
;; 	 (*load-depth* (1+ *load-depth*))
;; 	 ;; KLUDGE: I can't find in the ANSI spec where it says
;; 	 ;; that DECLAIM/PROCLAIM of optimization policy should
;; 	 ;; have file scope. CMU CL did this, and it seems
;; 	 ;; reasonable, but it might not be right; after all,
;; 	 ;; things like (PROCLAIM '(TYPE ..)) don't have file
;; 	 ;; scope, and I can't find anything under PROCLAIM or
;; 	 ;; COMPILE-FILE or LOAD or OPTIMIZE which justifies this
;; 	 ;; behavior. Hmm. -- WHN 2001-04-06
;; 	 (sb!c::*policy* sb!c::*policy*))
;;     (return-from load
;;       (if faslp
;; 	  (load-as-fasl stream verbose print)
;; 	  (sb!c:with-compiler-error-resignalling
;; 	      (load-as-source stream :verbose verbose
;; 			      :print print))))))

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
