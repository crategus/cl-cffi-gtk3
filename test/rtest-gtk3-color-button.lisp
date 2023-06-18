(in-package :gtk-test)

(def-suite gtk-color-button :in gtk-suite)
(in-suite gtk-color-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorButton

(test gtk-color-button-class
  ;; Type check
  (is (g:type-is-object "GtkColorButton"))
  ;; Check the registered name
  (is (eq 'gtk:color-button
          (glib:symbol-for-gtype "GtkColorButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColorButton")
          (g:gtype (cffi:foreign-funcall "gtk_color_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkColorButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkColorButton")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable" "GtkColorChooser")
             (list-interfaces "GtkColorButton")))
  ;; Check the class properties
  (is (equal '("alpha" "color" "rgba" "show-editor" "title" "use-alpha")
             (list-properties "GtkColorButton")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkColorButton")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkColorButton")))
  ;; Check the signals
  (is (equal '("color-set")
             (list-signals "GtkColorButton")))
  ;; CSS information
  (is (string= "button"
               (gtk:widget-class-css-name "GtkColorButton")))
  (is (string=
"[button.color:dir(ltr)]
  colorswatch.activatable:dir(ltr)
    overlay
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:color-button))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkColorButton" GTK-COLOR-BUTTON
                       (:SUPERCLASS GTK-BUTTON :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                         "GtkBuildable" "GtkColorChooser")
                        :TYPE-INITIALIZER "gtk_color_button_get_type")
                       ((ALPHA GTK-COLOR-BUTTON-ALPHA "alpha" "guint" T T)
                        (COLOR GTK-COLOR-BUTTON-COLOR "color" "GdkColor" T T)
                        (RGBA GTK-COLOR-BUTTON-RGBA "rgba" "GdkRGBA" T T)
                        (SHOW-EDITOR GTK-COLOR-BUTTON-SHOW-EDITOR "show-editor"
                         "gboolean" T T)
                        (TITLE GTK-COLOR-BUTTON-TITLE "title" "gchararray" T T)
                        (USE-ALPHA GTK-COLOR-BUTTON-USE-ALPHA "use-alpha"
                         "gboolean" T T)))
             (gobject:get-g-type-definition "GtkColorButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     alpha
;;;     color
;;;     rgba
;;;     show-editor
;;;     title
;;;     use-alpha

(test gtk-color-button-properties
  (let ((button (make-instance 'gtk:color-button)))
    (is (=  65535 (gtk:color-button-alpha button)))
    (is (gdk:color-equal (gdk:color-new :pixel 0 :red 0 :green 0 :blue 0)
                         (gtk:color-button-color button)))
    (is (gdk:rgba-equal (gdk:rgba-new :red 0.0 :green 0.0 :blue 0.0 :alpha 1.0)
                        (gtk:color-button-rgba button)))
    (is-false (gtk:color-button-show-editor button))
    (is (string= "Wählen Sie eine Farbe" (gtk:color-button-title button)))
    (is-false (gtk:color-button-use-alpha button))))

;;; --- Signals ----------------------------------------------------------------

;;;     color-set

(test gtk-color-button-color-set-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "color-set" "GtkColorButton"))))
    (is (string= "color-set" (g:signal-query-signal-name query)))
    (is (string= "GtkColorButton"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_button_new

(test gtk-color-button-new
  (is (typep (gtk:color-button-new) 'gtk:color-button)))

;;;     gtk_color_button_new_with_color

(test gtk-color-button-new-with-color
  (is (typep (gtk:color-button-new-with-color (gdk:color-new :red 65535))
             'gtk:color-button)))

;;;     gtk_color_button_new_with_rgba

(test gtk-color-button-new-with-rgba
  (is (typep (gtk:color-button-new-with-rgba (gdk:rgba-new :red 1.0))
             'gtk:color-button)))

;;; --- 2023-6-14 --------------------------------------------------------------
