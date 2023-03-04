(in-package :gtk-test)

(def-suite gtk-event-box :in gtk-suite)
(in-suite gtk-event-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventBox

(test event-box-class
  ;; Type check
  (is (g:type-is-object "GtkEventBox"))
  ;; Check the registered name
  (is (eq 'gtk:event-box
          (gobject:symbol-for-gtype "GtkEventBox")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEventBox")
          (g:gtype (cffi:foreign-funcall "gtk_event_box_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkEventBox")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkEventBox")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkEventBox")))
  ;; Check the class properties
  (is (equal '("above-child" "visible-window")
             (list-properties "GtkEventBox")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkEventBox")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkEventBox")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkEventBox")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkEventBox" GTK-EVENT-BOX
                       (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_event_box_get_type")
                       ((ABOVE-CHILD GTK-EVENT-BOX-ABOVE-CHILD "above-child"
                         "gboolean" T T)
                        (VISIBLE-WINDOW GTK-EVENT-BOX-VISIBLE-WINDOW
                         "visible-window" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkEventBox"))))

;;; --- Properties -------------------------------------------------------------

(test event-box-properties
  (let ((box (make-instance 'gtk:event-box)))
    (is-false (gtk:event-box-above-child box))
    (is-true (gtk:event-box-visible-window box))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_box_new

(test event-box-new
  (is (typep (gtk:event-box-new) 'gtk:event-box)))

;;; --- 2023-3-4 ---------------------------------------------------------------
