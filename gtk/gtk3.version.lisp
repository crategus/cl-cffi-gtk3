;;; ----------------------------------------------------------------------------
;;; gtk3.version.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;;
;;; Version Information
;;;
;;;     Variables and functions to check the GTK version
;;;
;;; Functions
;;;
;;;     gtk_get_major_version
;;;     gtk_get_minor_version
;;;     gtk_get_micro_version
;;;     gtk_get_binary_age                                  not exported
;;;     gtk_get_interface_age                               not exported
;;;     gtk_check_version
;;; ----------------------------------------------------------------------------

(in-package :gtk)

(defvar +cl-cffi-gtk3-build-time+ (multiple-value-list (get-decoded-time)))
(defvar +cl-cffi-gtk3-version+
        (slot-value (asdf:find-system :cl-cffi-gtk3) 'asdf:version))

;;; ----------------------------------------------------------------------------
;;; gtk_get_major_version
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_major_version" major-version) :int
 #+liber-documentation
 "@version{2023-07-07}
  @return{The major version number of the GTK library.}
  @begin{short}
    Returns the major version number of the GTK library.
  @end{short}
  This function is in the library, so it represents the GTK library your code
  is running against.
  @see-function{gtk:check-version}")

(export 'major-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_minor_version
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_minor_version" minor-version) :int
 #+liber-documentation
 "@version{2023-07-07}
  @return{The minor version number of the GTK library.}
  @begin{short}
    Returns the minor version number of the GTK library.
  @end{short}
  This function is in the library, so it represents the GTK library your code
  is are running against.
  @see-function{gtk:check-version}")

(export 'minor-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_micro_version
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_micro_version" micro-version) :int
 #+liber-documentation
 "@version{2023-07-07}
  @return{The micro version number of the GTK library.}
  @begin{short}
    Returns the micro version number of the GTK library.
  @end{short}
  This function is in the library, so it represents the GTK library your code
  is are running against.
  @see-function{gtk:check-version}")

(export 'micro-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_binary_age                                      not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_binary_age" gtk-binary-age) :int
 #+liber-documentation
 "@version{#2020-08-23}
  @return{The binary age of the GTK library.}
  @begin{short}
    Returns the binary age as passed to @code{libtool} when building the GTK
    library the process is running against.
  @end{short}
  If @code{libtool} means nothing to you, do not worry about it.
  @see-function{gtk-interface-age}")

;;; ----------------------------------------------------------------------------
;;; gtk_get_interface_age                                   not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_interface_age" gtk-interface-age) :int
 #+liber-documentation
 "@version{#2020-08-23}
  @return{The interface age of the GTK library.}
  @begin{short}
    Returns the interface age as passed to @code{libtool} when building the
    GTK library the process is running against.
  @end{short}
  If @code{libtool} means nothing to you, do not worry about it.
  @see-function{gtk-binary-age}")

;;; ----------------------------------------------------------------------------
;;; gtk_check_version
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_check_version" check-version)
    (:string :free-from-foreign nil)
 #+liber-documentation
 "@version{2025-07-07}
  @argument[major]{an unsigned integer for the required major version}
  @argument[minor]{an unsigned integer for required minor version}
  @argument[micro]{an unsigned integer for required micro version}
  @begin{return}
    @code{Nil} if the GTK library is compatible with the given version,
    or a string describing the version mismatch.
  @end{return}
  @begin{short}
    Checks that the GTK library in use is compatible with the given version.
  @end{short}
  Compatibility is defined by two things: first the version of the running
  library is newer than the version @code{major}.@code{minor}.@code{micro}.
  Second the running library must be binary compatible with the version
  @code{major}.@code{minor}.@code{micro} (same major version).
  @see-function{gtk:major-version}
  @see-function{gtk:minor-version}
  @see-function{gtk:micro-version}"
  (major :uint)
  (minor :uint)
  (micro :uint))

(export 'check-version)

;;; ----------------------------------------------------------------------------

(defun cl-cffi-gtk-build-info ()
 #+liber-documentation
 "@version{2023-07-07}
  @begin{short}
    Provides informations about the installation and the versions of the
    loaded libraries.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
* (gtk:cl-cffi-gtk-build-info)
cl-cffi-gtk3 version: 0.1.0
cl-cffi-gtk3 build date: 20:52 7/7/2023
GTK version: 3.24.37
GLIB version: 2.76.1
GDK-Pixbuf version: 2.42.10
Pango version: 1.50.12
Cairo version: 1.16.0
Machine type: X86-64
Machine version: Intel(R) Core(TM) i5-5200U CPU @@ 2.20GHz
Software type: Linux
Software version: 6.2.0-24-generic
Lisp implementation type: SBCL
Lisp implementation version: 2.2.9.debian
NIL
    @end{pre}
  @end{dictionary}
  @see-function{gtk:major-version}
  @see-function{gtk:minor-version}
  @see-function{gtk:micro-version}
  @see-symbol{glib:+major-version+}
  @see-symbol{glib:+minor-version+}
  @see-symbol{glib:+micro-version+}
  @see-symbol{gdk-pixbuf:+version+}
  @see-function{pango:version-string}
  @see-function{cairo:version-string}"
  (format t "~&cl-cffi-gtk3 version: ~a~%" +cl-cffi-gtk3-version+)
  (format t "cl-cffi-gtk3 build date: ~a:~a ~a/~a/~a~%"
          (third +cl-cffi-gtk3-build-time+)
          (second +cl-cffi-gtk3-build-time+)
          (fifth +cl-cffi-gtk3-build-time+)
          (fourth +cl-cffi-gtk3-build-time+)
          (sixth +cl-cffi-gtk3-build-time+))
  (format t "GTK version: ~a.~a.~a~%"
          (major-version)
          (minor-version)
          (micro-version))
  (format t "GLIB version: ~a.~a.~a~%"
          glib:+major-version+
          glib:+minor-version+
          glib:+micro-version+)
  (format t "GDK-Pixbuf version: ~a~%" gdk-pixbuf:+version+)
  (format t "Pango version: ~a~%" (pango:version-string))
  (format t "Cairo version: ~a~%" (cairo:version-string))
  (format t "Machine type: ~a~%" (machine-type))
  (format t "Machine version: ~a~%" (machine-version))
  (format t "Software type: ~a~%" (software-type))
  (format t "Software version: ~A~%" (software-version))
  (format t "Lisp implementation type: ~a~%" (lisp-implementation-type))
  (format t "Lisp implementation version: ~a~%" (lisp-implementation-version))
  nil)

(export 'cl-cffi-gtk-build-info)

;;; --- End of file gtk3.version.lisp ------------------------------------------
