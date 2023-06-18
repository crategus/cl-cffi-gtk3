(in-package :gtk-test)

(def-suite gtk-color-chooser :in gtk-suite)
(in-suite gtk-color-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorChooser

(test gtk-color-chooser-interface
  ;; Type check
  (is (g:type-is-interface "GtkColorChooser"))
  ;; Check the registered name
  (is (eq 'gtk:color-chooser
          (glib:symbol-for-gtype "GtkColorChooser")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColorChooser")
          (g:gtype (cffi:foreign-funcall "gtk_color_chooser_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '("rgba" "use-alpha")
             (list-interface-properties "GtkColorChooser")))
  ;; Check the signals
  (is (equal '("color-activated")
             (list-signals "GtkColorChooser")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkColorChooser" GTK-COLOR-CHOOSER
                    (:EXPORT T :TYPE-INITIALIZER "gtk_color_chooser_get_type")
                    (RGBA GTK-COLOR-CHOOSER-RGBA "rgba" "GdkRGBA" T T)
                    (USE-ALPHA GTK-COLOR-CHOOSER-USE-ALPHA "use-alpha"
                     "gboolean" T T))
             (gobject:get-g-type-definition "GtkColorChooser"))))

;;; --- Properties -------------------------------------------------------------

;;;     rgba
;;;     use-alpha

(test gtk-color-chooser-properties
  (let ((chooser (make-instance 'gtk:color-button)))
    (is (gdk:rgba-equal (gdk:rgba-new :red 0 :green 0 :blue 0 :alpha 1.0)
                        (gtk:color-chooser-rgba chooser)))
    (is-false (gtk:color-chooser-use-alpha chooser))))

;;; --- Signals ----------------------------------------------------------------

;;;     color-activated

(test gtk-color-chooser-color-activated-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "color-activated"
                                                "GtkColorChooser"))))
    (is (string= "color-activated" (g:signal-query-signal-name query)))
    (is (string= "GtkColorChooser"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GdkRGBA")
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_chooser_add_palette

;;; --- 2023-6-12 --------------------------------------------------------------
