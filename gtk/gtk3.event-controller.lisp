;;; ----------------------------------------------------------------------------
;;; gtk3.event-controller.lisp
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
;;; GtkEventController
;;;
;;;     Self-contained handler of series of events
;;;
;;; Types and Values
;;;
;;;     GtkEventController
;;;     GtkPropagationPhase
;;;
;;; Functions
;;;
;;;     gtk_event_controller_get_propagation_phase         Accessor
;;;     gtk_event_controller_set_propagation_phase         Accessor
;;;     gtk_event_controller_handle_event
;;;     gtk_event_controller_get_widget                    Accessor
;;;     gtk_event_controller_reset
;;;
;;; Properties
;;;
;;;     propagation-phase
;;;     widget
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ├── GtkEventControllerKey
;;;         ├── GtkEventControllerMotion
;;;         ├── GtkEventControllerScroll
;;;         ├── GtkGesture
;;;         ╰── GtkPadController
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkPropagationPhase
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkPropagationPhase" propagation-phase
  (:export t
   :type-initializer "gtk_propagation_phase_get_type")
  (:phase-none 0)
  (:phase-capture 1)
  (:phase-bubble 2)
  (:phase-target 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'propagation-phase)
      "GEnum"
      (liber:symbol-documentation 'propagation-phase)
 "@version{#2023-1-21}
  @begin{short}
    Describes the stage at which events are fed into a
    @class{gtk:event-controller} object.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GtkPropagationPhase\" propagation-phase
  (:export t
   :type-initializer \"gtk_propagation_phase_get_type\")
  (:phase-none 0)
  (:phase-capture 1)
  (:phase-bubble 2)
  (:phase-target 3))
  @end{pre}
  @begin[code]{table}
    @entry[:phase-none]{Events are not delivered automatically. Those can be
      manually fed through the @fun{gtk:event-controller-handle-event} function.
      This should only be used when full control about when, or whether the
      controller handles the event is needed.}
    @entry[:phase-capture]{Events are delivered in the capture phase. The
      capture phase happens before the bubble phase, runs from the toplevel down
      to the event widget. This option should only be used on containers that
      might possibly handle events before their children do.}
    @entry[:phase-bubble]{Events are delivered in the bubble phase. The bubble
      phase happens after the capture phase, and before the default handlers are
      run. This phase runs from the event widget, up to the toplevel.}
    @entry[:phase-target]{Events are delivered in the default widget event
      handlers, note that widget implementations must chain up on button,
      motion, touch and grab broken handlers for controllers in this phase to
      be run.}
  @end{table}
  @see-class{gtk:event-controller}
  @see-function{gtk:event-controller-handle-event}")

;;; ----------------------------------------------------------------------------
;;; struct GtkEventController
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkEventController" event-controller
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_get_type")
  ((propagation-phase
    event-controller-propagation-phase
    "propagation-phase" "GtkPropagationPhase" t t)
   (widget
    event-controller-widget
    "widget" "GtkWidget" t nil)))

#+liber-documentation
(setf (documentation 'event-controller 'type)
 "@version{#2023-1-21}
  @begin{short}
    The @sym{gtk:event-controller} object is a base, low-level implementation
    for event controllers.
  @end{short}
  Those react to a series of @class{gdk:event} objects, and possibly trigger
  actions as a consequence of those.
  @see-slot{gtk:event-controller-propagate-phase}
  @see-slot{gtk:event-controller-widget}
  @see-class{gdk:event}
  @see-class{gtk:gesture}
  @see-symbol{gtk:propagation-phase}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- event-controller-propagation-phase -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "propagation-phase"
                                               'event-controller) t)
 "The @code{propagation-phase} property of type
  @symbol{gtk:propagation-phase} (Read / Write) @br{}
  The propagation phase at which this controller will handle events. @br{}
  Default value: @code{:phase-bubble}")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-propagation-phase)
      "Accessor"
      (documentation 'event-controller-propagation-phase 'function)
 "@version{#2023-1-21}
  @syntax[]{(gtk:event-controller-propagation-phase object) => phase)}
  @syntax[]{(setf (gtk:event-controller-propagation-phase object) phase)}
  @argument[object]{a @class{gtk:event-controller} object}
  @argument[phase]{a propagation phase of @symbol{gtk:propagation-phase} type}
  @begin{short}
    Accessor of the @slot[gtk:event-controller]{propagation-phase} slot of the
    @class{gtk:event-controller} class.
  @end{short}
  The @sym{gtk:event-controller-propagation-phase} function gets the propagation
  phase at which controller handles events. The
  @sym{(setf gtk:event-controller-propagation-phase)} function sets the
  propagation phase at which a controller handles events.

  If @arg{phase} is @code{:phase-none}, no automatic event handling will be
  performed, but other additional gesture maintenance will. In that phase, the
  events can be managed by calling the @fun{gtk:event-controller-handle-event}
  function.
  @see-class{gtk:event-controller}
  @see-symbol{gtk:propagation-phase}
  @see-function{gtk:event-controller-handle-event}")

;;; --- event-controller-widget ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "widget" 'event-controller) t)
 "The @code{widget} property of type @class{gtk:widget} (Read / Write) @br{}
  The widget receiving the @class{gdk:event} that the controller will handle.
  @br{}")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-widget)
      "Accessor"
      (documentation 'event-controller-widget 'function)
 "@version{#2023-1-21}
  @syntax[]{(gtk:event-controller-widget object) => widget)}
  @syntax[]{(setf (gtk:event-controller-widget object) widget)}
  @argument[object]{a @class{gtk:event-controller} object}
  @begin{short}
    Accessor of the @slot[gtk:event-controller]{widget} slot of the
    @class{gtk:event-controller} class.
  @end{short}
  The @sym{gtk:event-controller-widget} function returns the @class{gtk:widget}
  object this controller relates to.
  @see-class{gtk:event-controller}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_handle_event ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_event_controller_handle_event"
               event-controller-handle-event) :boolean
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[controller]{a @class{gtk:event-controller} object}
  @argument[event]{a @class{gdk:event} object}
  @return{@em{True} if @arg{event} was potentially useful to trigger the
    controller action}
  @begin{short}
    Feeds an events into the controller, so it can be interpreted and the
    controller actions triggered.
  @end{short}
  @see-class{gtk:event-controller}
  @see-class{gdk:event}"
  (controller (g:object event-controller))
  (event (g:boxed gdk:event)))

(export 'event-controller-handle-event)

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_reset ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_event_controller_reset" event-controller-reset) :void
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[controller]{a @class{gtk:event-controller} object}
  @begin{short}
    Resets the controller to a clean state.
  @end{short}
  Every interaction the controller did through \"handle-event\" will be dropped
  at this point.
  @see-class{gtk:event-controller}"
  (controller (g:object event-controller)))

(export 'event-controller-reset)

;;; --- End of file gtk3.event-controller.lisp ---------------------------------
