;;; ----------------------------------------------------------------------------
;;; gtk-child-properties.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;; TODO: Consider to remove the implementation of container-call-get-property
;; and container-call-set-property. We have the container-child-property
;; function.

#+nil
(defun container-call-get-property (container child property gtype)
  (with-foreign-object (gvalue '(:struct g:value))
    (g:value-init gvalue (g:gtype gtype))
    (%container-child-property container child property gvalue)
    (prog1
      (parse-g-value gvalue)
      (g:value-unset gvalue))))

#+nil
(defun container-call-set-property (container child property value gtype)
  (with-foreign-object (gvalue '(:struct g:value))
    (set-g-value gvalue value (g:gtype gtype) :zero-g-value t)
    (%container-child-set-property container child property gvalue)
    (g:value-unset gvalue)
    (values)))

(defmacro define-child-property (container-type             ; "GtkFixed"
                                 property-name              ; fixed-child-x
                                 property-gname             ; "x"
                                 property-type              ; "gint"
                                 readable writable export)
  (when (stringp container-type)
    (setf container-type (gobject:symbol-for-gtype container-type)))
  `(progn
     ,@(when readable
         (list `(defun ,property-name (container child)
                  (assert (typep container ',container-type)
                          nil
                          "Type error for ~a in the ~a function"
                          container ',property-name)
                  (container-child-property container
                                            child
                                            ,property-gname
                                            ,property-type))))
     ,@(when writable
         (list `(defun (setf ,property-name) (value container child)
                  (assert (typep container ',container-type)
                          nil
                          "Type error for ~a in the ~a function"
                          container ',property-name)
                  (setf (container-child-property container
                                                  child
                                                  ,property-gname
                                                  ,property-type)
                        value)
                  value)))
     ,@(when export
         (list `(export ',property-name)))))

(defun child-property-name (type-name property-name package-name)
  (intern (format nil "~A-CHILD-~A"
                  (symbol-name (gobject:symbol-for-gtype type-name))
                  (string-upcase property-name))
          (find-package package-name)))

(defun generate-child-properties (&optional (type-root "GtkContainer")
                                            (package-name "GTK"))
  (setf type-root (g:gtype type-root))
  (append
    (loop for property in (container-class-list-child-properties type-root)
          collect
               `(define-child-property
                    ,(gobject:gtype-name type-root)
                    ,(child-property-name (gobject:gtype-name type-root)
                                          (gobject:%param-spec-name property)
                                          package-name)
                  ,(gobject:%param-spec-name property)
                  ,(gobject:gtype-name (gobject:%param-spec-type property))
                  ,(gobject:%param-spec-readable property)
                  ,(gobject:%param-spec-writable property)
                  t))
    (loop for subclass in (g:type-children type-root)
          appending (generate-child-properties subclass package-name))))

