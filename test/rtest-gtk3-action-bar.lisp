(in-package :gtk-test)

(def-suite gtk-action-bar :in gtk-suite)
(in-suite gtk-action-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkActionBar

(test gtk-action-bar-class
  ;; Check type
  (is (g:type-is-object "GtkActionBar"))
  ;; Check registered name
  (is (eq 'gtk:action-bar
          (glib:symbol-for-gtype "GtkActionBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkActionBar")
          (g:gtype (cffi:foreign-funcall "gtk_action_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkActionBar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkActionBar")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkActionBar")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkActionBar")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkActionBar")))
  ;; Check child properties
  (is (equal '("pack-type" "position")
             (gtk-test:list-child-properties "GtkActionBar")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkActionBar")))
  ;; CSS information
  (is (string= "actionbar"
               (gtk:widget-class-css-name "GtkActionBar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkActionBar" GTK:ACTION-BAR
                      (:SUPERCLASS GTK:BIN
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_action_bar_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkActionBar"))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-action-bar-properties
  (glib-test:with-check-memory (bar button)
    (setf bar (make-instance 'gtk:action-bar))
    (setf button (make-instance 'gtk:button))
    (is-false (gtk:container-add bar button))
    (is (eq :start (gtk:action-bar-child-pack-type bar button)))
    (is (= 0 (gtk:action-bar-child-position bar button)))
    ;; Remove button from action bar
    (gtk:container-remove bar button)))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_action_bar_new

(test gtk-action-bar-new
  (glib-test:with-check-memory (bar)
    (is (typep (setf bar (gtk:action-bar-new)) 'gtk:action-bar))))

;;;     gtk_action_bar_pack_start
;;;     gtk_action_bar_pack_end

(test gtk-action-bar-pack-start/end
  (glib-test:with-check-memory (bar button1 button2)
    (setf bar (gtk:action-bar-new))
    (setf button1 (gtk:button-new))
    (setf button2 (gtk:button-new))
    (is-false (gtk:action-bar-pack-end bar button2))
    (is-false (gtk:action-bar-pack-start bar button1))
    (is (eq :start (gtk:action-bar-child-pack-type bar button1)))
    (is (= 1 (gtk:action-bar-child-position bar button1)))
    (is (eq :end (gtk:action-bar-child-pack-type bar button2)))
    (is (= 0 (gtk:action-bar-child-position bar button2)))
    ;; Remove buttons from action bar
    (is-false (gtk:container-remove bar button1))
    (is-false (gtk:container-remove bar button2))))

;;;     gtk_action_bar_get_center_widget
;;;     gtk_action_bar_set_center_widget

(test gtk-action-bar-center-widget
  (glib-test:with-check-memory (bar button)
    (setf bar (gtk:action-bar-new))
    (setf button (gtk:button-new))
    (is (eq button (setf (gtk:action-bar-center-widget bar) button)))
    (is (eq button (gtk:action-bar-center-widget bar)))
    ;; Remove button from action bar
    (is-false (gtk:container-remove bar button))))

;;; 2026-06-12
