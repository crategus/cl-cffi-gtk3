;;; ----------------------------------------------------------------------------
;;; gtk3.accel-label.lisp
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
;;; GtkAccelLabel
;;;
;;;     A label which displays an accelerator key on the right of the text.
;;;
;;; Types and Values
;;;
;;;     GtkAccelLabel
;;;
;;; Functions
;;;
;;;     gtk_accel_label_new
;;;     gtk_accel_label_set_accel_closure
;;;     gtk_accel_label_get_accel_widget
;;;     gtk_accel_label_set_accel_widget
;;;     gtk_accel_label_get_accel_width
;;;     gtk_accel_label_set_accel
;;;     gtk_accel_label_get_accel
;;;     gtk_accel_label_refetch
;;;
;;; Properties
;;;
;;;     accel-closure
;;;     accel-widget
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkMisc
;;;                 ╰── GtkLabel
;;;                     ╰── GtkAccelLabel
;;;
;;; Implemented Interfaces
;;;     GtkAccelLabel implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAccelLabel
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAccelLabel" accel-label
  (:superclass label
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_accel_label_get_type")
  ((accel-closure
    accel-label-accel-closure
    "accel-closure" "GClosure" t t)
   (accel-widget
    accel-label-accel-widget
    "accel-widget" "GtkWidget" t t)))

#+liber-documentation
(setf (documentation 'accel-label 'type)
 "@version{#2025-06-28}
  @begin{short}
    The @class{gtk:accel-label} widget is a subclass of the @class{gtk:label}
    class that also displays an accelerator key on the right of the label text,
    for example @kbd{Ctrl+Q}.
  @end{short}
  It is commonly used in menus to show the keyboard short-cuts for commands.

  @image[accel-label]{Figure: GtkAccelLabel}

  The accelerator key to display is not set explicitly. Instead, the accel label
  displays the accelerators which have been added to a particular widget. This
  widget is set by calling the @fun{gtk:accel-label-accel-widget} function.

  For example, a @class{gtk:menu-item} widget may have an accelerator added to
  emit the @sig[gtk:menu-item]{activate} signal when the @kbd{Ctrl+Q} key
  combination is pressed. A @class{gtk:accel-label} widget is created and added
  to the @class{gtk:menu-item} widget, and the
  @fun{gtk:accel-label-accel-widget} function is called with the
  @class{gtk:menu-item} widget as the second argument. The accel label will now
  display @code{\"Ctrl+Q\"} after its label.

  Note that creating a @class{gtk:menu-item} widget with the
  @fun{gtk:menu-item-new-with-label} function, or one of the similar functions
  for the @class{gtk:check-menu-item} and @class{gtk:radio-menu-item} widgets,
  automatically adds a @class{gtk:accel-label} widget to the
  @class{gtk:menu-item} widget and calls the @fun{gtk:accel-label-accel-widget}
  function to set it up for you.

  A accel label will only display accelerators which have the
  @val[gtk:accel-flags]{:visible} value of the @sym{gtk:accel-flags} flags set.
  A accel label can display multiple accelerators and even signal names, though
  it is almost always used to display just one accelerator key.
  @begin[Examples]{dictionary}
    Creating a menu item with an accelerator key.
    @begin{pre}
(let (...
      (item-file-quit (make-instance 'gtk:menu-item
                                     :label \"Quit\")))
  ;; Add an accelerator to the QUIT menu item
  (let ((group (gtk:accel-group-new)))
    (gtk:window-add-accel-group window group)
    (gtk:widget-add-accelerator item-file-quit
                                \"activate\"
                                group
                                (gdk:keyval-from-name \"q\")
                                :control-mask
                                :visible)
    ...)
...)
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:accel-label-new}
  @see-slot{gtk:accel-label-accel-closure}
  @see-slot{gtk:accel-label-accel-widget}
  @see-class{gtk:label}
  @see-class{gtk:menu-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:accel-label-accel-closure ------------------------------------------

;; TODO: GClosure is in the C implementatin a boxed type, but not in Lisp.
;; Therefore the accessor for accel-closure does not work.

#+liber-documentation
(setf (documentation (liber:slot-documentation "accel-closure" 'accel-label) t)
 "The @code{accel-closure} property of type @sym{g:closure} (Read / Write) @br{}
  The closure to be monitored for accelerator changes.")

#+liber-documentation
(setf (liber:alias-for-function 'accel-label-accel-closure)
      "Accessor"
      (documentation 'accel-label-accel-closure 'function)
 "@version{#2023-03-15}
  @syntax{(gtk:accel-label-accel-closure object) => closure}
  @syntax{(setf (gtk:accel-label-accel-closure object) closure)}
  @argument[label]{a @class{gtk:accel-label} widget}
  @argument[closure]{a @sym{g:closure} instance to monitor for accelerator
    changes}
  @begin{short}
    Accessor of the @slot[gtk:accel-label]{accel-closure} slot of the
    @class{gtk:accel-label} class.
  @end{short}
  The @fun{gtk:accel-label-accel-closure} function gets the closure to be
  monitored by this accelerator. The @setf{gtk:accel-label-accel-closure}
  function sets the closure. The closure must be connected to an accelerator
  group, see the @code{gtk_accel_group_connect()} function.
  @see-class{gtk:accel-label}
  @see-symbol{g:closure}")

;;; --- gtk:accel-label-accel-widget -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accel-widget" 'accel-label) t)
 "The @code{accel-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  The widget to be monitored for accelerator changes.")

#+liber-documentation
(setf (liber:alias-for-function 'accel-label-accel-widget)
      "Accessor"
      (documentation 'accel-label-accel-widget 'function)
 "@version{#2023-03-15}
  @syntax{(gtk:accel-label-accel-widget object) => widget}
  @syntax{(setf (gtk:accel-label-accel-widget object) widget)}
  @argument[label]{a @class{gtk:accel-label} widget}
  @argument[widget]{a @class{gtk:widget} object to be monitored}
  @begin{short}
    Accessor of the @slot[gtk:accel-label]{accel-widget} slot of the
    @class{gtk:accel-label} class.
  @end{short}
  The @fun{gtk:accel-label-accel-widget} function returns the widget monitored
  by the accelerator label. The @setf{gtk:accel-label-accel-widget} function
  sets the widget.
  @see-class{gtk:accel-label}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_new
;;; ----------------------------------------------------------------------------

(declaim (inline accel-label-new))

(defun accel-label-new (text)
 #+liber-documentation
 "@version{#2025-06-17}
  @argument[text]{a string for the text of the label}
  @return{The new @class{gtk:accel-label} widget.}
  @begin{short}
    Creates a new accel label.
  @end{short}
  @see-class{gtk:accel-label}"
  (make-instance 'accel-label
                 :label text))

(export 'accel-label-new)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_get_accel_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_label_get_accel_width" accel-label-accel-width) :int
  #+liber-documentation
 "@version{#2023-03-15}
  @argument[label]{a @class{gtk:accel-label} widget}
  @return{The integer with the width needed to display the accelerator key(s).}
  @begin{short}
    Returns the width needed to display the accelerator key(s).
  @end{short}
  This is used by menus to align all of the @class{gtk:menu-item} widgets, and
  should not be needed by applications.
  @see-class{gtk:accel-label}
  @see-class{gtk:menu-item}"
  (label (g:object accel-label)))

(export 'accel-label-accel-width)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_set_accel
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_label_set_accel" accel-label-set-accel) :void
 #+liber-documentation
 "@version{#2025-06-28}
  @argument[label]{a @class{gtk:accel-label} widget}
  @argument[key]{an unsigned integer for a keyval, or 0}
  @argument[mods]{a @sym{gdk:modifier-type} modifier mask for the accel}
  @begin{short}
    Manually sets a keyval and modifier mask as the accelerator rendered by
    @arg{label}.
  @end{short}
  If a keyval and modifier are explicitly set then these values are used
  regardless of any associated accel closure or widget. Providing an @arg{key}
  argument of 0 removes the manual setting.
  @see-class{gtk:accel-label}
  @see-symbol{gdk:modifier-type}"
  (label (g:object accel-label))
  (key :uint)
  (mods gdk:modifier-type))

(export 'accel-label-set-accel)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_get_accel
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_label_get_accel" %accel-label-get-accel) :void
  (label (g:object accel-label))
  (key (:pointer :uint))
  (mods (:pointer gdk:modifier-type)))

(defun accel-label-get-accel (label)
 #+liber-documentation
 "@version{#2025-06-28}
  @syntax{(gtk:accel-label-accel label) => key, mods}
  @argument[label]{a @class{gtk:accel-label} widget}
  @argument[key]{an unsigned integer for a keyval}
  @argument[mods]{a @sym{gdk:modifier-type} modifier mask}
  @begin{short}
    Gets the keyval and modifier mask set with the
    @fun{gtk:accel-label-set-accel} function.
  @end{short}
  @see-class{gtk:accel-label}
  @see-symbol{gdk:modifier-type}
  @see-function{gtk:accel-label-set-accel}"
  (cffi:with-foreign-objects ((key :uint)
                              (mods 'gdk:modifier-type))
    (%accel-label-get-accel label key mods)
    (values (cffi:mem-ref key :uint)
            (cffi:mem-ref mods 'gdk:modifier-type))))

(export 'accel-label-get-accel)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_label_refetch
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_label_refetch" accel-label-refetch) :boolean
 #+liber-documentation
 "@version{#2023-03-15}
  @argument[label]{a @class{gtk:accel-label} widget}
  @return{Always returns @em{false}.}
  @begin{short}
    Recreates the string representing the accelerator keys.
  @end{short}
  This should not be needed since the string is automatically updated whenever
  accelerators are added or removed from the associated widget.
  @see-class{gtk:accel-label}"
  (label (g:object accel-label)))

(export 'accel-label-refetch)

;;; End of file gtk3.accel-label.lisp ------------------------------------------
