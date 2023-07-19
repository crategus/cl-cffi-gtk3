(in-package :gtk-test)

(def-suite gtk-color-chooser-widget :in gtk-suite)
(in-suite gtk-color-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorChooserWidget

(test gtk-color-chooser-widget-class
  ;; Type check
  (is (g:type-is-object "GtkColorChooserWidget"))
  ;; Check the registered name
  (is (eq 'gtk:color-chooser-widget
          (glib:symbol-for-gtype "GtkColorChooserWidget")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColorChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_color_chooser_widget_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkColorChooserWidget")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkColorChooserWidget")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
               "GtkColorChooser")
             (list-interfaces "GtkColorChooserWidget")))
  ;; Check the class properties
  (is (equal '("rgba" "show-editor" "use-alpha")
             (list-properties "GtkColorChooserWidget")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkColorChooserWidget")))
  ;; Check the child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (list-child-properties "GtkColorChooserWidget")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkColorChooserWidget")))
  ;; CSS information
  (is (string= "colorchooser"
               (gtk:widget-class-css-name "GtkColorChooserWidget")))
  #-windows
  (is (string=
"[colorchooser.vertical:dir(ltr)]
  box.vertical:dir(ltr)
    grid.horizontal:dir(ltr)
      colorswatch.top.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.bottom.activatable.dark:dir(ltr)
        overlay
      colorswatch.top.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.bottom.activatable.dark:dir(ltr)
        overlay
      colorswatch.top.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.bottom.activatable.light:dir(ltr)
        overlay
      colorswatch.top.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.bottom.activatable.dark:dir(ltr)
        overlay
      colorswatch.top.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.bottom.activatable.dark:dir(ltr)
        overlay
      colorswatch.top.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.bottom.activatable.dark:dir(ltr)
        overlay
      colorswatch.top.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.bottom.activatable.dark:dir(ltr)
        overlay
      colorswatch.top.activatable.light:selected:dir(ltr)
        overlay:selected:dir(ltr)
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.bottom.activatable.light:dir(ltr)
        overlay
      colorswatch.top.activatable.dark:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.bottom.activatable.dark:dir(ltr)
        overlay
    label:dir(ltr)
    box.horizontal:dir(ltr)
      colorswatch#add-color-button.activatable:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
      colorswatch.activatable.light:dir(ltr)
        overlay
      colorswatch.activatable.dark:dir(ltr)
        overlay
  box.horizontal:dir(ltr)
    [box.horizontal:dir(ltr)]
      overlay:dir(ltr)
        grid.horizontal:dir(ltr)
          button.circular:dir(ltr)
            image:dir(ltr)
          colorswatch#editor-color-sample.light:dir(ltr)
            overlay
          entry:dir(ltr)
            undershoot.left:dir(ltr)
            undershoot.right:dir(ltr)
          scale.vertical.color.marks-after:dir(ltr)
            contents
              trough:dir(ltr)
                slider:dir(ltr)
          scale.color.horizontal.marks-before:dir(ltr)
            contents
              trough:dir(ltr)
                slider:dir(ltr)
          widget:dir(ltr)
        [box.osd.popover.horizontal:dir(ltr)]
          grid.horizontal:dir(ltr)
            label:dir(ltr)
            label:dir(ltr)
            spinbutton.horizontal:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
              entry:dir(ltr)
              button.down:disabled:dir(ltr)
              button.up:dir(ltr)
            spinbutton.horizontal:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
              entry:dir(ltr)
              button.down:disabled:dir(ltr)
              button.up:dir(ltr)
        [box.osd.popover.horizontal:dir(ltr)]
          grid.horizontal:dir(ltr)
            label:dir(ltr)
            spinbutton.horizontal:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
              entry:dir(ltr)
              button.down:disabled:dir(ltr)
              button.up:dir(ltr)
        [box.osd.popover.horizontal:dir(ltr)]
          grid.horizontal:dir(ltr)
            label:dir(ltr)
            spinbutton.horizontal:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
              entry:dir(ltr)
              button.down:disabled:dir(ltr)
              button.up:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:color-chooser-widget))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkColorChooserWidget"
                                     GTK-COLOR-CHOOSER-WIDGET
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkColorChooser"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_color_chooser_widget_get_type")
                       ((SHOW-EDITOR GTK-COLOR-CHOOSER-WIDGET-SHOW-EDITOR
                         "show-editor" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkColorChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

;;;     show-editor

(test gtk-color-chooser-widget-properties
  (let ((widget (make-instance 'gtk:color-chooser-widget)))
    (is-false (gtk:color-chooser-widget-show-editor widget))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_chooser_widget_new

(test gtk-color-chooser-widget-new
  (is (typep (gtk:color-chooser-widget-new) 'gtk:color-chooser-widget)))

;;; --- 2023-6-14 --------------------------------------------------------------
