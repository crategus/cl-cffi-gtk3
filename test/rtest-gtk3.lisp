(defpackage :gtk-test
  (:use :fiveam :common-lisp)
  (:export #:run!
           #:list-child-properties
           #:list-style-properties
           #:approx-equal)
  (:import-from :glib-test  #:*first-run-testsuite*
                            #:run-repeat
                            #:approx-equal)
  (:import-from :glib)
  (:import-from :gobject)
  (:import-from :gio)
  (:import-from :gtk)
  (:import-from :gdk))

(in-package :gtk-test)

(defvar *first-run-gtk-test* t)

(def-suite gtk-test)
(def-suite gdk-suite :in gtk-test)
(def-suite gtk-suite :in gtk-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Set the current package for the testsuite
  (setf (glib-sys:get-current-package) "cl-cffi-gtk3")
  ;; Ensure directory for the output of test results
  (ensure-directories-exist
      (asdf:system-relative-pathname :cl-cffi-gtk3 "test/out/")))

;; Sorted list of the class style property names without inherited properties
(defun list-style-properties (gtype)
  (sort (set-difference (mapcar #'g:param-spec-name
                                (gtk:widget-class-list-style-properties gtype))
                        (mapcar #'g:param-spec-name
                                (gtk:widget-class-list-style-properties
                                  (g:type-parent gtype)))
                        :test #'string=)
        #'string<))

;; Sorted list of the class child property names
(defun list-child-properties (gtype)
  (sort (mapcar #'g:param-spec-name
                (gtk:container-class-list-child-properties gtype))
        #'string<))

;; Create and fill a GTK:LIST-STORE for use as a model
(defun create-list-store-for-package (&optional (package "GTK"))
  (let ((store (make-instance 'gtk:list-store
                              :column-types '("gint" "gchararray" "gboolean")))
        (counter 0))
    ;; Fill in external symbols of the given package
    (do-external-symbols (symbol (find-package package))
      ;; Add a new row to the model
      (gtk:list-store-set store
                          (gtk:list-store-append store)
                          (incf counter)
                          (symbol-name symbol)
                          nil))
    ;; Return the new list store
    store))

;;; 2025-2-23
