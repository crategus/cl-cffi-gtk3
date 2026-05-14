(in-package :gtk-test)

(def-suite gtk-event-controller :in gtk-test)
(in-suite gtk-event-controller)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPropagationPhase

(test gtk-propagation-phase
  ;; Check type
  (is (g:type-is-enum "GtkPropagationPhase"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPropagationPhase")
          (g:gtype (cffi:foreign-funcall "gtk_propagation_phase_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:propagation-phase
          (glib:symbol-for-gtype "GtkPropagationPhase")))
  ;; Check names
  (is (equal '("GTK_PHASE_NONE" "GTK_PHASE_CAPTURE" "GTK_PHASE_BUBBLE"
               "GTK_PHASE_TARGET")
             (glib-test:list-enum-item-names "GtkPropagationPhase")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkPropagationPhase")))
  ;; Check nick names
  (is (equal '("none" "capture" "bubble" "target")
             (glib-test:list-enum-item-nicks "GtkPropagationPhase")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPropagationPhase" GTK:PROPAGATION-PHASE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_propagation_phase_get_type")
                                    (:NONE 0)
                                    (:CAPTURE 1)
                                    (:BUBBLE 2)
                                    (:TARGET 3))
             (gobject:get-gtype-definition "GtkPropagationPhase"))))

;;;     GtkEventController

(test gtk-event-controller-class
  ;; Check type
  (is (g:type-is-object "GtkEventController"))
  ;; Check registered name
  (is (eq 'gtk:event-controller
          (glib:symbol-for-gtype "GtkEventController")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEventController")
          (g:gtype (cffi:foreign-funcall "gtk_event_controller_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkEventController")))
  ;; Check children
  (is (equal '("GtkEventControllerKey" "GtkEventControllerMotion"
               "GtkEventControllerScroll" "GtkGesture" "GtkPadController")
             (glib-test:list-children "GtkEventController")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkEventController")))
  ;; Check class properties
  (is (equal '("propagation-phase" "widget")
             (glib-test:list-properties "GtkEventController")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkEventController")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEventController" GTK:EVENT-CONTROLLER
                      (:SUPERCLASS GOBJECT:OBJECT :EXPORT T :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_event_controller_get_type")
                      ((PROPAGATION-PHASE EVENT-CONTROLLER-PROPAGATION-PHASE
                        "propagation-phase" "GtkPropagationPhase" T T)
                       (WIDGET EVENT-CONTROLLER-WIDGET "widget" "GtkWidget" T
                        NIL)))
             (gobject:get-gtype-definition "GtkEventController"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-event-controller-properties
  (glib-test:with-check-memory (controller)
    (is (typep (setf controller (gtk:event-controller-key-new)) 'gtk:event-controller))
    (is (eq :bubble (gtk:event-controller-propagation-phase controller)))
    (is (eq :capture
            (setf (gtk:event-controller-propagation-phase controller) :capture)))
    (is (eq :capture (gtk:event-controller-propagation-phase controller)))
    (is-false (gtk:event-controller-widget controller))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_handle_event

;;;     gtk_event_controller_reset

(test gtk-event-controller-reset
  (glib-test:with-check-memory (controller)
    (setf controller (gtk:event-controller-key-new))
    (is-false (gtk:event-controller-reset controller))))

;;; 2026-05-10
