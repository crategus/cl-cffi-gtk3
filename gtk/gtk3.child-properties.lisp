;;; ----------------------------------------------------------------------------
;;; gtk3.child-properties.lisp
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
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

;;; --- End of file gtk3.child-properties.lisp ---------------------------------
