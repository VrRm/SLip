;;; this file contains helper functions that will be called from Ymacs
;;; to perform certain things, such as evaluating code or getting
;;; symbol completion.

;; (defpackage :ymacs
;;   (:use :sl))

(in-package :ymacs)

(import '(sl-ffi:defun-js))

(defun-js %make-desktop ()
  "var WINDOW = window.opener || window.parent;
   window.child_window.make_desktop(WINDOW);")


;; (defun-js %get-parent-window ()
;;   "return window.opener || window.parent;")

;; (defun-js make-desktop-alt ()
;;   (%make-desktop-alt (%get-parent-window)))

;; (defun-js %make-desktop-alt (w)
;;   "window.child_window.make_desktop(w);")
