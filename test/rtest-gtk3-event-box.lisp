(in-package :gtk-test)

(def-suite gtk-event-box :in gtk-suite)
(in-suite gtk-event-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventBox

(test gtk-event-box-class
  ;; Check type
  (is (g:type-is-object "GtkEventBox"))
  ;; Check registered name
  (is (eq 'gtk:event-box
          (glib:symbol-for-gtype "GtkEventBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEventBox")
          (g:gtype (cffi:foreign-funcall "gtk_event_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkEventBox")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkEventBox")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkEventBox")))
  ;; Check class properties
  (is (equal '("above-child" "visible-window")
             (glib-test:list-properties "GtkEventBox")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkEventBox")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkEventBox")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkEventBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEventBox" GTK:EVENT-BOX
                       (:SUPERCLASS GTK:BIN
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_event_box_get_type")
                       ((ABOVE-CHILD EVENT-BOX-ABOVE-CHILD
                         "above-child" "gboolean" T T)
                        (VISIBLE-WINDOW EVENT-BOX-VISIBLE-WINDOW
                         "visible-window" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkEventBox"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-event-box-properties
  (let ((box (make-instance 'gtk:event-box)))
    (is-false (gtk:event-box-above-child box))
    (is-true (gtk:event-box-visible-window box))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_box_new

(test gtk-event-box-new
  (is (typep (gtk:event-box-new) 'gtk:event-box)))

;;; 2024-9-23
