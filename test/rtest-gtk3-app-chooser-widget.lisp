(in-package :gtk-test)

(def-suite gtk-app-chooser-widget :in gtk-suite)
(in-suite gtk-app-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserWidget

(test gtk-app-chooser-widget-class
  ;; Check type
  (is (g:type-is-object "GtkAppChooserWidget"))
  ;; Check registered name
  (is (eq 'gtk:app-chooser-widget
          (glib:symbol-for-gtype "GtkAppChooserWidget")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAppChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_widget_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkAppChooserWidget")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAppChooserWidget")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
               "GtkAppChooser")
             (glib-test:list-interfaces "GtkAppChooserWidget")))
  ;; Check class properties
  (is (equal '("content-type" "default-text" "show-all" "show-default"
               "show-fallback" "show-other" "show-recommended")
             (glib-test:list-properties "GtkAppChooserWidget")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkAppChooserWidget")))
  ;; Check child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (gtk-test:list-child-properties "GtkAppChooserWidget")))
  ;; Check signals
  (is (equal '("application-activated" "application-selected" "populate-popup")
             (glib-test:list-signals "GtkAppChooserWidget")))
  ;; Check CSS information
  (is (string= "appchooser"
               (gtk:widget-class-css-name "GtkAppChooserWidget")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAppChooserWidget" GTK:APP-CHOOSER-WIDGET
                      (:SUPERCLASS GTK:BOX
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkAppChooser" "GtkBuildable"
                        "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_app_chooser_widget_get_type")
                      ((DEFAULT-TEXT APP-CHOOSER-WIDGET-DEFAULT-TEXT
                        "default-text" "gchararray" T T)
                       (SHOW-ALL APP-CHOOSER-WIDGET-SHOW-ALL "show-all"
                        "gboolean" T T)
                       (SHOW-DEFAULT APP-CHOOSER-WIDGET-SHOW-DEFAULT
                        "show-default" "gboolean" T T)
                       (SHOW-FALLBACK APP-CHOOSER-WIDGET-SHOW-FALLBACK
                        "show-fallback" "gboolean" T T)
                       (SHOW-OTHER APP-CHOOSER-WIDGET-SHOW-OTHER "show-other"
                        "gboolean" T T)
                       (SHOW-RECOMMENDED APP-CHOOSER-WIDGET-SHOW-RECOMMENDED
                        "show-recommended" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkAppChooserWidget"))))

;;; --- Signals ----------------------------------------------------------------

;;;     application-activated
;;;     application-selected
;;;     populate-popup

;;; --- Properties -------------------------------------------------------------

;;;     default-text
;;;     show-all
;;;     show-default
;;;     show-fallback
;;;     show-other
;;;     show-recommended

(test gtk-app-chooser-widget-properties
  (glib-test:with-check-memory (chooser)
    (setf chooser (gtk:app-chooser-widget-new "text/plain"))
    ;; Check properties
    (is-false (gtk:app-chooser-widget-default-text chooser))
    (is-false (gtk:app-chooser-widget-show-all chooser))
    (is-false (gtk:app-chooser-widget-show-default chooser))
    (is-false (gtk:app-chooser-widget-show-fallback chooser))
    (is-false (gtk:app-chooser-widget-show-other chooser))
    (is-true (gtk:app-chooser-widget-show-recommended chooser))
))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_widget_new

(test gtk-app-chooser-widget-new
  (glib-test:with-check-memory (chooser)
    (is (typep (setf chooser
                     (gtk:app-chooser-widget-new "plain/text")) 'gtk:app-chooser-widget))))

;;; 2026-06-02
