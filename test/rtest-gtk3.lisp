(defpackage :gtk-test
  (:use :fiveam :cffi :common-lisp)
  (:export #:run!)
  (:import-from :glib-test #:list-children
                           #:list-interfaces
                           #:list-properties
                           #:list-interface-properties
                           #:list-interface-prerequisites
                           #:list-signals
                           #:list-flags-item-name
                           #:list-flags-item-value
                           #:list-flags-item-nick
                           #:list-enum-item-name
                           #:list-enum-item-value
                           #:list-enum-item-nick)
  (:export                 #:list-children
                           #:list-interfaces
                           #:list-properties
                           #:list-interface-properties
                           #:list-interface-prerequisites
                           #:list-signals
                           #:list-style-properties          ; for GTK3
                           #:list-child-properties          ; for GTK3
                           #:list-flags-item-name
                           #:list-flags-item-value
                           #:list-flags-item-nick
                           #:list-enum-item-name
                           #:list-enum-item-value
                           #:list-enum-item-nick)
  (:import-from :glib)
  (:import-from :gobject)
  (:import-from :gio)
  (:import-from :gtk)
  (:import-from :gdk))

(in-package :gtk-test)

(defvar *first-run-gtk-test* t)

(def-suite gtk-test)
(def-suite gtk-suite :in gtk-test)
(def-suite gdk-suite :in gtk-test)

;; Ensure directory for the output of test results
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ensure-directories-exist
      (asdf:system-relative-pathname :cl-cffi-gtk3 "test/out/")))

;; Get the pathname for a file in the testsuite
(defun sys-path (filename &optional (system :cl-cffi-gtk3))
  (asdf:system-relative-pathname system
                                 (concatenate 'string "test/" filename)))

;; See https://www.embeddeduse.com/2019/08/26/qt-compare-two-floats/
(defun approx-equal (x y &optional (eps-factor 1.0d-1))
  (or (< (abs (- x y)) eps-factor)
      (< (abs (- x y)) (* eps-factor (max (abs x) (abs y))))))

;; A sorted list of the class style property names without inherited properties
(defun list-style-properties (gtype)
  (sort (set-difference (mapcar #'g:param-spec-name
                                (gtk:widget-class-list-style-properties gtype))
                        (mapcar #'g:param-spec-name
                                (gtk:widget-class-list-style-properties
                                  (g:type-parent gtype)))
                        :test #'string=)
        #'string<))

;; A sorted list of the class child property names
(defun list-child-properties (gtype)
  (sort (mapcar #'g:param-spec-name
                (gtk:container-class-list-child-properties gtype))
        #'string<))

;;; 2024-6-23
