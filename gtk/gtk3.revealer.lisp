;;; ----------------------------------------------------------------------------
;;; gtk3.revealer.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; GtkRevealer
;;;
;;;     Hide and show with animation
;;;
;;; Types and Values
;;;
;;;     GtkRevealer
;;;     GtkRevealerTransitionType
;;;
;;; Functions
;;;
;;;     gtk_revealer_new
;;;     gtk_revealer_get_reveal_child                      Accessor
;;;     gtk_revealer_set_reveal_child                      Accessor
;;;     gtk_revealer_get_child_revealed                    Accessor
;;;     gtk_revealer_get_transition_duration               Accessor
;;;     gtk_revealer_set_transition_duration               Accessor
;;;     gtk_revealer_get_transition_type                   Accessor
;;;     gtk_revealer_set_transition_type                   Accessor
;;;
;;; Properties
;;;
;;;     child-revealed
;;;     reveal-child
;;;     transition-duration
;;;     transition-type
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkRevealer
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRevealer implements AtkImplementorIface and GtkBuildable.
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkRevealerTransitionType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkRevealerTransitionType" revealer-transition-type
  (:export t
   :type-initializer "gtk_revealer_transition_type_get_type")
  (:none 0)
  (:crossfade 1)
  (:slide-right 2)
  (:slide-left 3)
  (:slide-up 4)
  (:slide-down 5))

#+liber-documentation
(setf (liber:alias-for-symbol 'revealer-transition-type)
      "GEnum"
      (liber:symbol-documentation 'revealer-transition-type)
 "@version{#2023-3-24}
  @begin{short}
    These enumeration values describe the possible transitions when the child
    widget of a @class{gtk:revealer} widget is shown or hidden.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkRevealerTransitionType\" revealer-transition-type
  (:export t
   :type-initializer \"gtk_revealer_transition_type_get_type\")
  (:none 0)
  (:crossfade 1)
  (:slide-right 2)
  (:slide-left 3)
  (:slide-up 4)
  (:slide-down 5))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No transition}
    @entry[:crossfade]{Fade in.}
    @entry[:slide-right]{Slide in from the left.}
    @entry[:slide-left]{Slide in from the right.}
    @entry[:slide-up]{Slide in from the bottom.}
    @entry[:slide-down]{Slide in from the top.}
  @end{table}
  @see-class{gtk:revealer}")

;;; ----------------------------------------------------------------------------
;;; struct GtkRevealer
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRevealer" revealer
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_revealer_get_type")
  ((child-revealed
    revealer-child-revealed
    "child-revealed" "gboolean" t nil)
   (reveal-child
    revealer-reveal-child
    "reveal-child" "gboolean" t t)
   (transition-duration
    revealer-transition-duration
    "transition-duration" "guint" t t)
   (transition-type
    revealer-transition-type
    "transition-type" "GtkRevealerTransitionType" t t)))

#+liber-documentation
(setf (documentation 'revealer 'type)
 "@version{#2023-3-24}
  @begin{short}
    The @sym{gtk:revealer} widget is a container which animates the transition
    of its child widget from invisible to visible.
  @end{short}
  The style of transition can be controlled with a value of the
  @fun{gtk:revealer-transition-type} enumeration. These animations respect
  the @slot[gtk:settings]{gtk-enable-animations} setting.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:revealer} implementation has a single CSS node with name
    @code{revealer}.
  @end{dictionary}
  @see-constructor{gtk:revealer-new}
  @see-slot{gtk:revealer-child-revealed}
  @see-slot{gtk:revealer-reveal-child}
  @see-slot{gtk:revealer-transition-duration}
  @see-slot{gtk:revealer-transition-type}
  @see-class{gtk:expander}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- revealer-child-revealed ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child-revealed" 'revealer) t)
 "The @code{child-revealed} property of type @code{:boolean} (Read) @br{}
  Whether the child widget is revealed and the animation target reached. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'revealer-child-revealed)
      "Accessor"
      (documentation 'revealer-child-revealed 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:revealer-child-revealed object) => revealed}
  @argument[object]{a @class{gtk:revealer} widget}
  @argument[revealed]{a boolean whether the child widget is revealed}
  @begin{short}
    Accessor of the @slot[gtk:revealer]{child-revealed} slot of the
    @class{gtk:revealer} class.
  @end{short}
  The @sym{gtk:revealer-child-revealed} function returns whether the child 
  widget is fully revealed, in other words whether the transition to the 
  revealed state is completed.
  @see-class{gtk:revealer}")

;;; --- revealer-reveal-child --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "reveal-child" 'revealer) t)
 "The @code{reveal-child} property of type @code{:boolean} (Read / Write) @br{}
  Whether the container should reveal the child widget. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'revealer-reveal-child)
      "Accessor"
      (documentation 'revealer-reveal-child 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:revealer-reveal-child object) => reveal}
  @syntax[]{(setf (gtk:revealer-reveal-child object) reveal)}
  @argument[object]{a @class{gtk:revealer} widget}
  @argument[reveal]{@em{true} to reveal the child widget}
  @begin{short}
    Accessor of the @slot[gtk:revealer]{reveal-child} slot of the
    @class{gtk:revealer} class.
  @end{short}
  The @sym{gtk:revealer-reveal-child} function returns whether the child widget 
  is currently revealed. The @sym{(setf gtk:revealer-reveal-child)} function 
  tells the revealer to reveal or conceal its child widget.

  This function returns @em{true} as soon as the transition to the revealed
  state is started. To learn whether the child widget is fully revealed, i.e.
  the transition is completed, use the @fun{gtk:revealer-child-revealed}
  function. The transition will be animated with the current transition type of
  the revealer.
  @see-class{gtk:revealer}
  @see-function{gtk:revealer-child-revealed}")

;;; --- revealer-transition-duration -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "transition-duration"
                                               'revealer) t)
 "The @code{transition-duration} property of type @code{:uint} (Read / Write)
  @br{}
  The animation duration, in milliseconds. @br{}
  Default value: 250")

#+liber-documentation
(setf (liber:alias-for-function 'revealer-transition-duration)
      "Accessor"
      (documentation 'revealer-transition-duration 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:revealer-transition-duration object) => duration}
  @syntax[]{(setf (gtk:revealer-transition-duration object) duration)}
  @argument[object]{a @class{gtk:revealer} widget}
  @argument[duration]{an unsigned integer with the duration, in milliseconds}
  @begin{short}
    Accessor of the @slot[gtk:revealer]{transition-duration} slot of the
    @class{gtk:revealer} class.
  @end{short}
  The @sym{gtk:revealer-transition-duration} function returns the amount of 
  time in milliseconds that transitions will take. The
  @sym{(setf gtk:revealer-transition-duration)} function sets the duration.
  @see-class{gtk:revealer}")

;;; --- revealer-transition-type -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "transition-type" 'revealer) t)
 "The @code{transition-type} property of type
  @symbol{gtk:revealer-transition-type} (Read / Write) @br{}
  The type of animation used to transition. @br{}
  Default value: @code{:slide-down}")

#+liber-documentation
(setf (liber:alias-for-function 'revealer-transition-type)
      "Accessor"
      (documentation 'revealer-transition-type 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:revealer-transition-type object) => setting}
  @syntax[]{(setf (gtk:revealer-transition-type object) setting)}
  @argument[object]{a @class{gtk:revealer} widget}
  @argument[setting]{a value of the @symbol{gtk:revealer-transition-type}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk:revealer]{transition-type} slot of the
    @class{gtk:revealer} class.
  @end{short}
  The @sym{gtk:revealer-transition-type} function gets the type of animation 
  that will be used for transitions in the revealer. The
  @sym{(setf gtk:revealer-transition-duration)} function sets the type of 
  animation. Available types include various kinds of fades and slides.
  @see-class{gtk:revealer}
  @see-symbol{gtk:revealer-transition-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_revealer_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline revealer-new))

(defun revealer-new ()
 #+liber-documentation
 "@version{#2023-3-24}
  @return{The new @class{gtk:revealer} widget.}
  @short{Creates a new revealer.}
  @see-class{gtk:revealer}"
  (make-instance 'revealer))

(export 'revealer-new)

;;; --- End of file gtk3.revealer.lisp -----------------------------------------
