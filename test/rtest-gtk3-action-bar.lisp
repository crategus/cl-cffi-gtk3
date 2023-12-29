(in-package :gtk-test)

(def-suite gtk-action-bar :in gtk-suite)
(in-suite gtk-action-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkActionBar

(test gtk-action-bar-class
  ;; Type check
  (is (g:type-is-object "GtkActionBar"))
  ;; Check the registered name
  (is (eq 'gtk:action-bar
          (glib:symbol-for-gtype "GtkActionBar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkActionBar")
          (g:gtype (cffi:foreign-funcall "gtk_action_bar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkActionBar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkActionBar")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkActionBar")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkActionBar")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkActionBar")))
  ;; Check the child properties
  (is (equal '("pack-type" "position")
             (list-child-properties "GtkActionBar")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkActionBar")))
  ;; CSS information
  (is (string= "actionbar"
               (gtk:widget-class-css-name "GtkActionBar")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkActionBar" GTK-ACTION-BAR
                               (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable")
                                :TYPE-INITIALIZER "gtk_action_bar_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkActionBar"))))

;;; --- Child Properties -------------------------------------------------------

;;;     pack-type
;;;     position

(test gtk-action-bar-properties
  (let ((bar (make-instance 'gtk:action-bar))
        (button (make-instance 'gtk:button)))
    (is-false (gtk:container-add bar button))
    (is (eq :start (gtk:action-bar-child-pack-type bar button)))
    (is (= 0 (gtk:action-bar-child-position bar button)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_action_bar_new

(test gtk-action-bar-new
  (is (typep (gtk:action-bar-new) 'gtk:action-bar)))

;;;     gtk_action_bar_pack_start
;;;     gtk_action_bar_pack_end

(test gtk-action-bar-pack-start/end
  (let ((bar (make-instance 'gtk:action-bar))
        (button1 (make-instance 'gtk:button))
        (button2 (make-instance 'gtk:button)))
    (is-false (gtk:action-bar-pack-end bar button2))
    (is-false (gtk:action-bar-pack-start bar button1))
    (is (eq :start (gtk:action-bar-child-pack-type bar button1)))
    (is (= 1 (gtk:action-bar-child-position bar button1)))
    (is (eq :end (gtk:action-bar-child-pack-type bar button2)))
    (is (= 0 (gtk:action-bar-child-position bar button2)))))

;;;     gtk_action_bar_get_center_widget
;;;     gtk_action_bar_set_center_widget

(test gtk-action-bar-center-widget
  (let ((bar (make-instance 'gtk:action-bar))
        (button (make-instance 'gtk:button)))
    (is (eq button (setf (gtk:action-bar-center-widget bar) button)))
    (is (eq button (gtk:action-bar-center-widget bar)))))

;;; 2023-12-29
