;;; ----------------------------------------------------------------------------
;;; gtk3.event-box.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkEventBox
;;;
;;;     A widget used to catch events for widgets which do not have their own
;;;     window
;;;
;;; Types and Values
;;;
;;;     GtkEventBox
;;;
;;; Functions
;;;
;;;     gtk_event_box_new
;;;     gtk_event_box_set_above_child                      Accessor
;;;     gtk_event_box_get_above_child                      Accessor
;;;     gtk_event_box_set_visible_window                   Accessor
;;;     gtk_event_box_get_visible_window                   Accessor
;;;
;;; Properties
;;;
;;;     above-child
;;;     visible-window
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkEventBox
;;;
;;; Implemented Interfaces
;;;
;;;     GtkEventBox implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkEventBox
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkEventBox" event-box
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_event_box_get_type")
  ((above-child
    event-box-above-child
    "above-child" "gboolean" t t)
   (visible-window
    event-box-visible-window
    "visible-window" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'event-box 'type)
 "@version{2023-2-23}
  @begin{short}
    The @class{gtk:event-box} widget is a subclass of the @class{gtk:bin} class
    which also has its own window.
  @end{short}
  It is useful since it allows you to catch events for widgets which do not
  have their own window.
  @begin[Example]{dictionary}
    This example demonstrates the usage of a @class{gtk:event-box} widget - a
    label is created and set up so that a mouse-click on the label causes the
    program to exit.
   @begin{pre}
(defun example-event-box ()
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :type :toplevel
                                 :title \"Example Event Box\"
                                 :default-height 150
                                 :border-width 24))
          (eventbox (make-instance 'gtk:event-box))
          (label (make-instance 'gtk:label
                                :ellipsize :end
                                :label
                                \"Click here to quit this Example Event Box.\")))
      (g:signal-connect window \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      ;; Set the available events for the event box
      (setf (gtk:widget-events eventbox) :button-press-mask)
      ;; Connect a signal handler to the eventbox
      (g:signal-connect eventbox \"button-press-event\"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (gtk:widget-destroy window)))
      ;; Add the label to the event box and the event box to the window
      (gtk:container-add eventbox label)
      (gtk:container-add window eventbox)
      ;; Realize the event box
      (gtk:widget-realize eventbox)
      ;; Set a new cursor for the event box
      (setf (gdk:window-cursor (gtk:widget-window eventbox))
            (gdk:cursor-new-from-name (gdk:display-default) \"pointer\"))
      ;; Show the window
      (gtk:widget-show-all window))))
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:event-box-new}
  @see-slot{gtk:event-box-above-child}
  @see-slot{gtk:event-box-visible-window}
  @see-class{gtk:bin}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- event-box-above-child --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "above-child" 'event-box) t)
 "The @code{above-child} property of type @code{:boolean} (Read / Write) @br{}
  Whether the event-trapping window of the eventbox is above the window of the
  child widget as opposed to below it. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'event-box-above-child)
      "Accessor"
      (documentation 'event-box-above-child 'function)
 "@version{2023-2-23}
  @syntax[]{(gtk:event-box-above-child object) => above-child}
  @syntax[]{(setf gtk:event-box-above-child object) above-child)}
  @argument[object]{a @class{gtk:event-box} widget}
  @argument[above-child]{@em{true} if the event box window is above its child}
  @begin{short}
    Accessor of the @slot[gtk:event-box]{above-child} slot of the
    @class{gtk:event-box} class.
  @end{short}
  The @fun{gtk:event-box-above-child} function returns whether the event box
  window is above or below the windows of its child. The
  @setf{gtk:event-box-above-child} function sets whether the event box window
  is positioned above the windows of its child, as opposed to below it.

  If the window is above, all events inside the event box will go to the event
  box. If the window is below, events in windows of child widgets will first
  got to that widget, and then to its parents.

  The default is to keep the window below the child.
  @see-class{gtk:event-box}")

;;; --- event-box-visible-window -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible-window" 'event-box) t)
 "The @code{visible-window} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the event box is visible, as opposed to invisible and only used to
  trap events. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'event-box-visible-window)
      "Accessor"
      (documentation 'event-box-visible-window 'function)
 "@version{2023-2-23}
  @syntax[]{(gtk:event-box-visible-window object) => visible-window}
  @syntax[]{(setf gtk:event-box-visible-window object) visible-window)}
  @argument[object]{a @class{gtk:event-box} widget}
  @argument[visible-window]{@em{true} to make the event box have a visible
    window}
  @begin{short}
    Accessor of the @slot[gtk:event-box]{visible-window} slot of the
    @class{gtk:event-box} class.
  @end{short}
  The @fun{gtk:event-box-visible-window} function returns whether the event box
  has a visible window. The @setf{gtk:event-box-visible-window} function sets
  whether the event box uses a visible or invisible child window. The default
  is to use visible windows.

  In an invisible window event box, the window that the event box creates is a
  @code{:input-only} window, which means that it is invisible and only serves
  to receive events.

  A visible window event box creates a visible @code{:input-output} window that
  acts as the parent window for all the widgets contained in the event box.

  You should generally make your event box invisible if you just want to trap
  events. Creating a visible window may cause artifacts that are visible to
  the user, especially if the user is using a theme with gradients or pixmaps.

  The main reason to create a non input-only event box is if you want to set
  the background to a different color or draw on it.
  @begin[Note]{dictionary}
    There is one unexpected issue for an invisible event box that has its window
    below the child. See the @fun{gtk:event-box-above-child} function. Since
    the input-only window is not an ancestor window of any windows that
    descendent widgets of the event box create, events on these windows are not
    propagated up by the windowing system, but only by GTK. The practical
    effect of this is if an event is not in the event mask for the descendant
    window, see the @fun{gtk:widget-add-events} function, it will not be
    received by the event box.

    This problem does not occur for visible event boxes, because in that case,
    the event box window is actually the ancestor of the descendant windows,
    not just at the same place on the screen.
  @end{dictionary}
  @see-class{gtk:event-box}
  @see-function{gtk:event-box-above-child}
  @see-function{gtk:widget-add-events}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline event-box-new))

(defun event-box-new ()
 #+liber-documentation
 "@version{2023-2-23}
  @return{The new @class{gtk:event-box} widget.}
  @begin{short}
    Creates a new event box.
  @end{short}
  @see-class{gtk:event-box}"
  (make-instance 'event-box))

(export 'event-box-new)

;;; --- End of file gtk3.event-box.lisp ----------------------------------------
