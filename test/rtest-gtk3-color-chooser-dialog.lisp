(in-package :gtk-test)

(def-suite gtk-color-chooser-dialog :in gtk-suite)
(in-suite gtk-color-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorChooserDialog

(test gtk-color-chooser-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkColorChooserDialog"))
  ;; Check the registered name
  (is (eq 'gtk:color-chooser-dialog
          (glib:symbol-for-gtype "GtkColorChooserDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColorChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_color_chooser_dialog_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkColorChooserDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkColorChooserDialog")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkColorChooser")
             (list-interfaces "GtkColorChooserDialog")))
  ;; Check the class properties
  (is (equal '("rgba" "show-editor" "use-alpha")
             (list-properties "GtkColorChooserDialog")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkColorChooserDialog")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkColorChooserDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkColorChooserDialog")))
  ;; CSS information
  (is (string= "dialog"
               (gtk:widget-class-css-name "GtkColorChooserDialog")))
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
    colorchooser.vertical:dir(ltr)
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
    [box.horizontal.dialog-action-box:dir(ltr)]
      buttonbox.horizontal.dialog-action-area:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:color-chooser-dialog))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkColorChooserDialog"
                                     GTK-COLOR-CHOOSER-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable"
                         "GtkColorChooser")
                        :TYPE-INITIALIZER "gtk_color_chooser_dialog_get_type")
                       ((SHOW-EDITOR GTK-COLOR-CHOOSER-DIALOG-SHOW-EDITOR
                         "show-editor" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkColorChooserDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     show-editor

(test gtk-color-chooser-dialog-properties
  (let ((dialog (make-instance 'gtk:color-chooser-dialog)))
    (is-false (gtk:color-chooser-dialog-show-editor dialog))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_chooser_dialog_new

(test gtk-color-chooser-dialog-new.1
  (is (typep (gtk:color-chooser-dialog-new "title" nil)
             'gtk:color-chooser-dialog)))

;; TODO: We get a warning:
;;   (sbcl:13528): Gtk-WARNING **: Can't set a parent on a toplevel widget

#+nil
(test gtk-color-chooser-dialog-new.2
  (is (typep (gtk:color-chooser-dialog-new "title" (gtk:window-new :toplevel))
             'gtk:color-chooser-dialog)))

;;; --- 2023-6-14 --------------------------------------------------------------
