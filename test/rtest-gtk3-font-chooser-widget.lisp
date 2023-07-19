(in-package :gtk-test)

(def-suite gtk-font-chooser-widget :in gtk-suite)
(in-suite gtk-font-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontChooserWidget

(test gtk-font-chooser-widget-class
  ;; Type check
  (is (g:type-is-object "GtkFontChooserWidget"))
  ;; Check the registered name
  (is (eq 'gtk:font-chooser-widget
          (glib:symbol-for-gtype "GtkFontChooserWidget")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFontChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_widget_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkFontChooserWidget")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFontChooserWidget")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
               "GtkFontChooser")
             (list-interfaces "GtkFontChooserWidget")))
  ;; Check the class properties
  (is (equal '("font" "font-desc" "font-features" "language" "level"
               "preview-text" "show-preview-entry" "tweak-action")
             (list-properties "GtkFontChooserWidget")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkFontChooserWidget")))
  ;; Check the child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (list-child-properties "GtkFontChooserWidget")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFontChooserWidget")))
  ;; CSS information
  (is (string= "fontchooser"
               (gtk:widget-class-css-name "GtkFontChooserWidget")))
  #-windows
  (is (string= "[fontchooser.horizontal:dir(ltr)]
  stack:dir(ltr)
    grid.horizontal:dir(ltr)
      entry.search:dir(ltr)
        image.left:disabled:dir(ltr)
        undershoot.left:dir(ltr)
        undershoot.right:dir(ltr)
        image.right:disabled:dir(ltr)
      stack:dir(ltr)
        grid.horizontal:dir(ltr)
          scrolledwindow.frame:dir(ltr)
            overshoot.left:dir(ltr)
            undershoot.left:dir(ltr)
            overshoot.right:dir(ltr)
            undershoot.right:dir(ltr)
            overshoot.top:dir(ltr)
            undershoot.top:dir(ltr)
            overshoot.bottom:dir(ltr)
            undershoot.bottom:dir(ltr)
            scrollbar.bottom.horizontal:dir(ltr)
              contents
                trough:dir(ltr)
                  slider:dir(ltr)
            scrollbar.vertical.right:dir(ltr)
              contents
                trough:dir(ltr)
                  slider:dir(ltr)
            treeview.view:dir(ltr)
              header:dir(ltr)
                button:dir(ltr)
                  box.horizontal:dir(ltr)
                    widget:dir(ltr)
                      label:dir(ltr)
                    [image:dir(ltr)]
          entry:dir(ltr)
            undershoot.left:dir(ltr)
            undershoot.right:dir(ltr)
          label:dir(ltr)
          scale.horizontal.marks-after:dir(ltr)
            contents
              trough:dir(ltr)
                slider:dir(ltr)
                highlight.top:dir(ltr)
            marks.bottom:dir(ltr)
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
              mark:dir(ltr)
                indicator
          spinbutton.horizontal:dir(ltr)
            undershoot.left:dir(ltr)
            undershoot.right:dir(ltr)
            entry:dir(ltr)
            button.down:dir(ltr)
            button.up:dir(ltr)
        grid.dim-label.horizontal:dir(ltr)
          image:dir(ltr)
          label:dir(ltr)
    box.vertical:dir(ltr)
      label:dir(ltr)
      entry:dir(ltr)
        undershoot.left:dir(ltr)
        undershoot.right:dir(ltr)
      scrolledwindow.view.frame:dir(ltr)
        overshoot.left:dir(ltr)
        undershoot.left:dir(ltr)
        overshoot.right:dir(ltr)
        undershoot.right:dir(ltr)
        overshoot.top:dir(ltr)
        undershoot.top:dir(ltr)
        overshoot.bottom:dir(ltr)
        undershoot.bottom:dir(ltr)
        scrollbar.bottom.horizontal:dir(ltr)
          contents
            trough:dir(ltr)
              slider:dir(ltr)
        scrollbar.vertical.right:dir(ltr)
          contents
            trough:dir(ltr)
              slider:dir(ltr)
        viewport.frame:dir(ltr)
          box.vertical:dir(ltr)
            grid.horizontal:dir(ltr)
              label:dir(ltr)
              scale.horizontal.marks-after:dir(ltr)
                contents
                  trough:dir(ltr)
                    slider:dir(ltr)
                    highlight.top:dir(ltr)
                marks.bottom:dir(ltr)
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
                  mark:dir(ltr)
                    indicator
              spinbutton.horizontal:dir(ltr)
                undershoot.left:dir(ltr)
                undershoot.right:dir(ltr)
                entry:dir(ltr)
                button.down:dir(ltr)
                button.up:dir(ltr)
            box.vertical:dir(ltr)
              [box.vertical:dir(ltr)]
                label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
              [box.vertical:dir(ltr)]
                label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
              [box.vertical:dir(ltr)]
                label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  radiobutton.text-button:dir(ltr):checked
                    radio:dir(ltr):checked
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  radiobutton.text-button:dir(ltr)
                    radio:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  radiobutton.text-button:dir(ltr)
                    radio:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
              [box.vertical:dir(ltr)]
                label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  radiobutton.text-button:dir(ltr):checked
                    radio:dir(ltr):checked
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  radiobutton.text-button:dir(ltr)
                    radio:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  radiobutton.text-button:dir(ltr)
                    radio:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
              [box.vertical:dir(ltr)]
                label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
              [box.vertical:dir(ltr)]
                label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
                [box.horizontal:dir(ltr)]
                  checkbutton.text-button:indeterminate:dir(ltr)
                    check:indeterminate:dir(ltr)
                    label:dir(ltr)
                  label:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:font-chooser-widget))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFontChooserWidget"
                                     GTK-FONT-CHOOSER-WIDGET
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkFontChooser"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_font_chooser_widget_get_type")
                       ((TWEAK-ACTION GTK-FONT-CHOOSER-WIDGET-TWEAK-ACTION
                         "tweak-action" "GAction" T NIL)))
             (gobject:get-g-type-definition "GtkFontChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

;;;     tweak-action

(test gtk-font-chooser-widget-properties
  (let ((widget (make-instance 'gtk:font-chooser-widget)))
    (is (typep (gtk:font-chooser-widget-tweak-action widget) 'g:simple-action))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_chooser_widget_new

(test gtk-font-chooser-widget-new
  (is (typep (gtk:font-chooser-widget-new) 'gtk:font-chooser-widget)))

;;; --- 2023-6-16 --------------------------------------------------------------
