;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk3-init.lisp
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

(defpackage :gtk3-init
  (:use :common-lisp))

(in-package :gtk3-init)

(glib-init:at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cffi:define-foreign-library gdk3
      ((:and :unix (:not :darwin))
       (:or "libgdk-3.so.0" "libgdk-3.so"))
      (:darwin (:or "libgdk-3.0.dylib"
                    "libgdk-3.dylib"
                    "libgdk-x11-3.0.0.dylib"
                    "libgdk-x11-3.0.dylib"))
      (:windows "libgdk-3-0.dll")
      (t "libgdk-3-0")))
  (cffi:use-foreign-library gdk3))

(glib-init:at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cffi:define-foreign-library gtk3
      ((:and :unix (:not :darwin))
       (:or "libgtk-3.so.0" "libgtk-3.so"))
      (:darwin (:or "libgtk-3.0.dylib"
                    "libgtk-3.dylib"
                    "libgtk-x11-3.0.0.dylib"
                    "libgtk-x11-3.0.dylib"))
      (:windows (:or "libgtk-3-0.dll" "libgtk-win32-2.0-0.dll"))
      (t "libgtk-3-0"))
    ;; push the hostname on *features*
    (pushnew (intern (string-upcase (machine-instance)) :keyword) *features*)
    (pushnew :gtk3 *features*))
  (cffi:use-foreign-library gtk3))

(glib-init:push-library-version-features gtk
    ;; We can not call the Lisp functions gtk:major-version and
    ;; gtk:minor-version because GTK is not compiled at this time.
    (cffi:foreign-funcall "gtk_get_major_version" :size)
    (cffi:foreign-funcall "gtk_get_minor_version" :size)
    3 10
    3 12
    3 14
    3 16
    3 18
    3 20
    3 22
    3 24
    )

;;; --- End of file cl-cffi-gtk3-init.lisp -------------------------------------
