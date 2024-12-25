;;; ----------------------------------------------------------------------------
;;; liber.lisp
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
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

#-liber-documentation
(push :liber-documentation *features*)

(asdf:load-system :liber)
(asdf:load-system :cl-cffi-glib :force t)
(asdf:load-system :cl-cffi-cairo :force t)
(asdf:load-system :cl-cffi-pango :force t)
(asdf:load-system :cl-cffi-gdk-pixbuf :force t)
(asdf:load-system :cl-cffi-gtk3 :force t)

(defpackage :liber-gtk3
  (:use :common-lisp)
  (:import-from :liber)
  (:export :generate-html
           :generate-html-single-page))

(in-package :liber-gtk3)

(unexport 'glib:allocate-stable-pointer :glib)
(unexport 'glib:stable-pointer-destroy-notify :glib)
(unexport 'glib:get-stable-pointer-value :glib)
(unexport 'glib:free-stable-pointer :glib)
(unexport 'glib:with-stable-pointer :glib)
(unexport 'glib:get-boxed-info :glib)
(unexport 'glib:with-error :glib)
(unexport 'glib:with-ignore-error :glib)
(unexport 'glib:with-catching-to-error :glib)
(unexport 'glib:boxed :glib)
(unexport 'glib:gtype :glib)
(unexport 'glib:gtype-id :glib)
(unexport 'glib:gtype-name :glib)
(unexport 'glib:symbol-for-gtype :glib)

(unexport 'glib:with-gboxed-array :glib)

(unexport 'gobject:*debug-gc* :gobject)
(unexport 'gobject:*debug-subclass* :gobject)
(unexport 'gobject:*gobject-debug* :gobject)

(unexport 'gobject:create-fn-ref :gobject)
(unexport 'gobject:define-cb-methods :gobject)
(unexport 'gobject:define-vtable :gobject)
(unexport 'gobject:get-gtype-definition :gobject)
(unexport 'gobject:get-genum-definition :gobject)
(unexport 'gobject:get-gflags-definition :gobject)
(unexport 'gobject:get-enum-items :gobject)
(unexport 'gobject:enum-item-name :gobject)
(unexport 'gobject:enum-item-nick :gobject)
(unexport 'gobject:enum-item-value :gobject)
(unexport 'gobject:get-flags-items :gobject)
(unexport 'gobject:flags-item-name :gobject)
(unexport 'gobject:flags-item-nick :gobject)
(unexport 'gobject:flags-item-value :gobject)
(unexport 'gobject:gobject-class :gobject)
(unexport 'gobject:get-gvalue :gobject)
(unexport 'gobject:set-gvalue :gobject)
(unexport 'gobject::g-initially-unowned :gobject)
(unexport 'gobject:get-lisp-name-exception :gobject)
(unexport 'gobject:register-object-type-implementation :gobject)
(unexport 'gobject:initially-unowned :gobject)

#-windows
(progn
  (unexport 'gdk:x11-cursor :gdk)
  (unexport 'gdk:x11-device-manager-core :gdk)
  (unexport 'gdk:x11-device-manager-xi2 :gdk)
  (unexport 'gdk:x11-device-xi2 :gdk))

(unexport 'gtk:ensure-gtk-main :gtk)

;; Unexport the symbols from gdk-pixbuf for the documentation to avoid
;; duplication of the symbols, these symbols are documented in its own package.

;; Symbols from gdk-pixbuf.structure.lisp
(unexport 'gdk:colorspace :gdk)
(unexport 'gdk:pixbuf :gdk)
(unexport 'gdk:pixbuf-bits-per-sample :gdk)
(unexport 'gdk:pixbuf-colorspace :gdk)
(unexport 'gdk:pixbuf-has-alpha :gdk)
(unexport 'gdk:pixbuf-height :gdk)
(unexport 'gdk:pixbuf-n-channels :gdk)
(unexport 'gdk:pixbuf-pixel-bytes :gdk)
(unexport 'gdk:pixbuf-pixels :gdk)
(unexport 'gdk:pixbuf-rowstride :gdk)
(unexport 'gdk:pixbuf-width :gdk)
(unexport 'gdk:pixbuf-pixels-with-length :gdk)
(unexport 'gdk:pixbuf-byte-length :gdk)
(unexport 'gdk:pixbuf-option :gdk)
(unexport 'gdk:pixbuf-remove-option :gdk)
(unexport 'gdk:pixbuf-copy-options :gdk)
(unexport 'gdk:pixbuf-read-pixels :gdk)
;; Symbols from gdk-pixbuf.load.lisp
(unexport 'gdk:pixbuf-file-info :gdk)
(unexport 'gdk:pixbuf-new-from-file :gdk)
(unexport 'gdk:pixbuf-new-from-file-at-size :gdk)
(unexport 'gdk:pixbuf-new-from-file-at-scale :gdk)
(unexport 'gdk:pixbuf-new-from-resource :gdk)
(unexport 'gdk:pixbuf-new-from-resource-at-scale :gdk)
;; Symbols from gdk-pixbuf.loader.lisp
(unexport 'gdk:pixbuf-loader :gdk)
(unexport 'gdk:pixbuf-loader-new :gdk)
(unexport 'gdk:pixbuf-loader-write :gdk)
(unexport 'gdk:pixbuf-loader-set-size :gdk)
(unexport 'gdk:pixbuf-loader-pixbuf :gdk)
(unexport 'gdk:pixbuf-loader-animation :gdk)
(unexport 'gdk:pixbuf-loader-close :gdk)
;; Symbols from gdk-pixbuf.save.lisp
(unexport 'gdk:pixbuf-save :gdk)
;; Symbols from gdk-pixbuf.memory.lisp
(unexport 'gdk:pixbuf-new :gdk)
(unexport 'gdk:pixbuf-new-subpixbuf :gdk)
(unexport 'gdk:pixbuf-copy :gdk)
;; Symbols from gdk-pixbuf.scaling.lisp
(unexport 'gdk:pixbuf-interp-type :gdk)
(unexport 'gdk:pixbuf-rotation :gdk)
(unexport 'gdk:pixbuf-scale-simple :gdk)
(unexport 'gdk:pixbuf-scale :gdk)
(unexport 'gdk:pixbuf-composite-color-simple :gdk)
(unexport 'gdk:pixbuf-composite :gdk)
(unexport 'gdk:pixbuf-composite-color :gdk)
(unexport 'gdk:pixbuf-rotate-simple :gdk)
(unexport 'gdk:pixbuf-flip :gdk)
;; Symbols from gdk-pixbuf.utilities.lisp
(unexport 'gdk:pixbuf-add-alpha :gdk)
(unexport 'gdk:pixbuf-copy-area :gdk)
(unexport 'gdk:pixbuf-fill :gdk)
;; Symbols from gdk-pixbuf.animation.lisp
(unexport 'gdk:pixbuf-animation :gdk)
(unexport 'gdk:pixbuf-animation-loop :gdk)
(unexport 'gdk:pixbuf-animation-new-from-file :gdk)
(unexport 'gdk:pixbuf-animation-new-from-resource :gdk)
(unexport 'gdk:pixbuf-animation-static-image :gdk)

;;; ---------------------------------------------------------------------------

(defun generate-html ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-gtk3)))
         (output-directory (merge-pathnames "../books/cl-cffi-gtk3/" base)))
    (format t "Generate HTML to ~a~%" output-directory)
    (liber:generate-html-documentation
      '(:gtk
        :gdk
        :gdk-pixbuf
        :glib
        :gobject
        :gio
        :pango
        :cairo)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-gtk3 API documentation"
      :heading "cl-cffi-gtk3"
      :css "crategus.css"
      :single-page-p nil
      :paginate-section-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(defun generate-html-single-page ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-gtk3)))
         (output-directory
             (merge-pathnames "../books/cl-cffi-gtk3/single-page/" base)))
    (format t "Generate Single PAGE HTML to ~a~%" output-directory)
    (liber:generate-html-documentation
      '(:gtk
        :gdk
        :gdk-pixbuf
        :glib
        :gobject
        :gio
        :pango
        :cairo)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-gtk3 API documentation (single page)"
      :heading "cl-cffi-gtk3"
      :css "crategus.css"
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

;;; --- End of file liber.lisp -------------------------------------------------
