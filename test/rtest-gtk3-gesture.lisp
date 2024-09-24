(in-package :gtk-test)

(def-suite gtk-gesture :in gtk-suite)
(in-suite gtk-gesture)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventSequenceState

(test gtk-event-sequence-state
  ;; Check type
  (is (g:type-is-enum "GtkEventSequenceState"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEventSequenceState")
          (g:gtype (cffi:foreign-funcall "gtk_event_sequence_state_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:event-sequence-state
          (glib:symbol-for-gtype "GtkEventSequenceState")))
  ;; Check names
  (is (equal '("GTK_EVENT_SEQUENCE_NONE" "GTK_EVENT_SEQUENCE_CLAIMED"
               "GTK_EVENT_SEQUENCE_DENIED")
             (glib-test:list-enum-item-names "GtkEventSequenceState")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkEventSequenceState")))
  ;; Check nick names
  (is (equal '("none" "claimed" "denied")
             (glib-test:list-enum-item-nicks "GtkEventSequenceState")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkEventSequenceState"
                                    GTK:EVENT-SEQUENCE-STATE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_event_sequence_state_get_type")
                       (:NONE 0)
                       (:CLAIMED 1)
                       (:DENIED 2))
             (gobject:get-gtype-definition "GtkEventSequenceState"))))

;;;     GtkGesture

(test gtk-gesture-class
  ;; Check type
  (is (g:type-is-object "GtkGesture"))
  ;; Check registered name
  (is (eq 'gtk:gesture
          (glib:symbol-for-gtype "GtkGesture")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGesture")
          (g:gtype (cffi:foreign-funcall "gtk_gesture_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkGesture")))
  ;; Check children
  (is (equal '("GtkGestureRotate" "GtkGestureSingle" "GtkGestureZoom")
             (glib-test:list-children "GtkGesture")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkGesture")))
  ;; Check class properties
  (is (equal '("n-points" "window")
             (glib-test:list-properties "GtkGesture")))
  ;; Check signals
  (is (equal '("begin" "cancel" "end" "sequence-state-changed" "update")
             (glib-test:list-signals "GtkGesture")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGesture" GTK:GESTURE
                       (:SUPERCLASS GTK:EVENT-CONTROLLER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_gesture_get_type")
                       ((N-POINTS GESTURE-N-POINTS "n-points" "guint" T NIL)
                        (WINDOW GESTURE-WINDOW "window" "GdkWindow" T T)))
             (gobject:get-gtype-definition "GtkGesture"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-gesture-properties
  (let ((gesture (make-instance 'gtk:gesture-single)))
    (is (= 1 (gtk:gesture-n-points gesture)))
    (is-false (gtk:gesture-window gesture))))

;;; --- Signals ----------------------------------------------------------------

;;;     begin
;;;     cancel
;;;     end
;;;     sequence-state-changed
;;;     update

;;; --- Functions --------------------------------------------------------------

;;;     gtk_gesture_get_device
;;;     gtk_gesture_is_active
;;;     gtk_gesture_is_recognized
;;;     gtk_gesture_get_sequence_state
;;;     gtk_gesture_set_sequence_state
;;;     gtk_gesture_set_state
;;;     gtk_gesture_get_sequences
;;;     gtk_gesture_handles_sequence
;;;     gtk_gesture_get_last_updated_sequence
;;;     gtk_gesture_get_last_event
;;;     gtk_gesture_get_point
;;;     gtk_gesture_get_bounding_box
;;;     gtk_gesture_get_bounding_box_center
;;;     gtk_gesture_group
;;;     gtk_gesture_ungroup
;;;     gtk_gesture_get_group
;;;     gtk_gesture_is_grouped_with

;;; 2024-9-23
