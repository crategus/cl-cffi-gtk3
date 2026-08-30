(in-package :gtk-test)

(def-suite gtk-volume-button :in gtk-suite)
(in-suite gtk-volume-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkVolumeButton

(test gtk-volume-button-class
  ;; Check type
  (is (g:type-is-object "GtkVolumeButton"))
  ;; Check registered name
  (is (eq 'gtk:volume-button
          (glib:symbol-for-gtype "GtkVolumeButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkVolumeButton")
          (g:gtype (cffi:foreign-funcall "gtk_volume_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkScaleButton")
          (g:type-parent "GtkVolumeButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkVolumeButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable" "GtkOrientable")
             (glib-test:list-interfaces "GtkVolumeButton")))
  ;; Check class properties
  (is (equal '("use-symbolic")
             (glib-test:list-properties "GtkVolumeButton")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkVolumeButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkVolumeButton")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkVolumeButton")))
  ;; Check CSS information
  (is (string= "button"
               (gtk:widget-class-css-name "GtkVolumeButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkVolumeButton" GTK:VOLUME-BUTTON
                      (:SUPERCLASS GTK:SCALE-BUTTON
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkActionable"
                        "GtkActivatable" "GtkBuildable" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_volume_button_get_type")
                      ((USE-SYMBOLIC VOLUME-BUTTON-USE-SYMBOLIC
                        "use-symbolic" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkVolumeButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     use-symbolic

(test gtk-volume-button-properties
  (glib-test:with-check-memory (button)
    (is (typep (setf button (make-instance 'gtk:volume-button)) 'gtk:volume-button))
    (is-true (gtk:volume-button-use-symbolic button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_volume_button_new

(test gtk-volume-button-new
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:volume-button-new)) 'gtk:volume-button))))

;;; 2026-06-27
