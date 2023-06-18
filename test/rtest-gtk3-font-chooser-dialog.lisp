(in-package :gtk-test)

(def-suite gtk-font-chooser-dialog :in gtk-suite)
(in-suite gtk-font-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontChooserDialog

(test gtk-font-chooser-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkFontChooserDialog"))
  ;; Check the registered name
  (is (eq 'gtk:font-chooser-dialog
          (glib:symbol-for-gtype "GtkFontChooserDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFontChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_dialog_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkFontChooserDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFontChooserDialog")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkFontChooser")
             (list-interfaces "GtkFontChooserDialog")))
  ;; Check the class properties
  (is (equal '("font" "font-desc" "font-features" "language" "level"
               "preview-text" "show-preview-entry")
             (list-properties "GtkFontChooserDialog")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkFontChooserDialog")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkFontChooserDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFontChooserDialog")))
  ;; CSS information
  (is (string= "dialog"
               (gtk:widget-class-css-name "GtkFontChooserDialog")))
  #-windows
  (is (string=
"[dialog.background.csd:dir(ltr)]
  decoration:dir(ltr)
  headerbar.titlebar:dir(ltr)
    box.vertical:dir(ltr)
      label.title:dir(ltr)
      [label.subtitle:dir(ltr)]
    button.text-button.suggested-action.default:dir(ltr)
      label:dir(ltr)
    button.text-button:dir(ltr)
      label:dir(ltr)
  box.vertical.dialog-vbox:dir(ltr)
    fontchooser.vertical:dir(ltr)
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
    [box.horizontal.dialog-action-box:dir(ltr)]
      buttonbox.horizontal.dialog-action-area:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:font-chooser-dialog))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkFontChooserDialog"
                                     GTK-FONT-CHOOSER-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkFontChooser")
                        :TYPE-INITIALIZER "gtk_font_chooser_dialog_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkFontChooserDialog"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_chooser_dialog_new

(test gtk-font-chooser-dialog-new.1
  (is (typep (gtk:font-chooser-dialog-new "title" nil)
             'gtk:font-chooser-dialog)))

(test gtk-font-chooser-dialog-new.2
  (is (typep (gtk:font-chooser-dialog-new nil nil)
             'gtk:font-chooser-dialog)))

;;; --- 2023-6-16 --------------------------------------------------------------
