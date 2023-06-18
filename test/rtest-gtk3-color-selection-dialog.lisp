(in-package :gtk-test)

(def-suite gtk-color-selection-dialog :in gtk-suite)
(in-suite gtk-color-selection-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorSelectionDialog

(test gtk-color-selection-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkColorSelectionDialog"))
  ;; Check the registered name
  (is (eq 'gtk:color-selection-dialog
          (glib:symbol-for-gtype "GtkColorSelectionDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColorSelectionDialog")
          (g:gtype (cffi:foreign-funcall "gtk_color_selection_dialog_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkColorSelectionDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkColorSelectionDialog")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkColorSelectionDialog")))
  ;; Check the class properties
  (is (equal '("cancel-button" "color-selection" "help-button" "ok-button")
             (list-properties "GtkColorSelectionDialog")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkColorSelectionDialog")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkColorSelectionDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkColorSelectionDialog")))
  ;; CSS information
  (is (string= "dialog"
               (gtk:widget-class-css-name "GtkColorSelectionDialog")))
  #-windows
  (is (string=
"[dialog.background:dir(ltr)]
  decoration:dir(ltr)
  box.vertical.dialog-vbox:dir(ltr)
    box.vertical:dir(ltr)
      box.horizontal:dir(ltr)
        box.vertical:dir(ltr)
          widget:dir(ltr)
          box.horizontal:dir(ltr)
            frame:dir(ltr)
              border:dir(ltr)
              box.horizontal:dir(ltr)
                widget:dir(ltr)
                widget:dir(ltr)
            button:dir(ltr)
              image:dir(ltr)
        box.vertical:dir(ltr)
          grid.horizontal:dir(ltr)
            label:dir(ltr)
            spinbutton.horizontal:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
              entry:dir(ltr)
              button.down:dir(ltr)
              button.up:dir(ltr)
            label:dir(ltr)
            spinbutton.horizontal:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
              entry:dir(ltr)
              button.down:disabled:dir(ltr)
              button.up:dir(ltr)
            label:dir(ltr)
            spinbutton.horizontal:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
              entry:dir(ltr)
              button.down:dir(ltr)
              button.up:disabled:dir(ltr)
            label:dir(ltr)
            spinbutton.horizontal:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
              entry:dir(ltr)
              button.down:dir(ltr)
              button.up:disabled:dir(ltr)
            label:dir(ltr)
            spinbutton.horizontal:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
              entry:dir(ltr)
              button.down:dir(ltr)
              button.up:disabled:dir(ltr)
            label:dir(ltr)
            spinbutton.horizontal:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
              entry:dir(ltr)
              button.down:dir(ltr)
              button.up:disabled:dir(ltr)
            separator.horizontal:dir(ltr)
            [label:dir(ltr)]
            [scale.horizontal:dir(ltr)]
              contents
                trough:dir(ltr)
                  slider:dir(ltr)
                  highlight.top:dir(ltr)
            [entry:dir(ltr)]
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
            label:dir(ltr)
            entry:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
          [box.vertical:dir(ltr)]
            label:dir(ltr)
            grid.horizontal:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
              frame:dir(ltr)
                border:dir(ltr)
                widget:dir(ltr)
    box.horizontal.dialog-action-box:dir(ltr)
      buttonbox.horizontal.dialog-action-area:dir(ltr)
        button.text-button:dir(ltr)
          label:dir(ltr)
        button.text-button.default:dir(ltr)
          label:dir(ltr)
        [button.text-button:dir(ltr)]
          label:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:color-selection-dialog))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkColorSelectionDialog"
                                     GTK-COLOR-SELECTION-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER
                        "gtk_color_selection_dialog_get_type")
                       ((CANCEL-BUTTON GTK-COLOR-SELECTION-DIALOG-CANCEL-BUTTON
                         "cancel-button" "GtkWidget" T NIL)
                        (COLOR-SELECTION
                         GTK-COLOR-SELECTION-DIALOG-COLOR-SELECTION
                         "color-selection" "GtkWidget" T NIL)
                        (HELP-BUTTON GTK-COLOR-SELECTION-DIALOG-HELP-BUTTON
                         "help-button" "GtkWidget" T NIL)
                        (OK-BUTTON GTK-COLOR-SELECTION-DIALOG-OK-BUTTON
                         "ok-button" "GtkWidget" T NIL)))
             (gobject:get-g-type-definition "GtkColorSelectionDialog"))))


;;; --- Properties -------------------------------------------------------------

;;;     cancel-button
;;;     color-selection
;;;     help-button
;;;     ok-button

(test gtk-color-selection-dialog-properties
  (let ((dialog (make-instance 'gtk:color-selection-dialog)))
    (is (typep (gtk:color-selection-dialog-cancel-button dialog) 'gtk:button))
    (is (typep (gtk:color-selection-dialog-color-selection dialog)
               'gtk:color-selection))
    (is (typep (gtk:color-selection-dialog-help-button dialog) 'gtk:button))
    (is (typep (gtk:color-selection-dialog-ok-button dialog) 'gtk:button))))

;;; --- Functions --------------------------------------------------------------

;;;    gtk_color_selection_dialog_new

(test gtk-color-selection-dialog-new.1
  (is (typep (gtk:color-selection-dialog-new "title")
             'gtk:color-selection-dialog)))

(test gtk-color-selection-dialog-new.2
  (let ((dialog (gtk:color-selection-dialog-new "title")))
    (is (string= "title" (gtk:window-title dialog)))))

(test gtk-color-selection-dialog-new.3
  (let ((dialog (gtk:color-selection-dialog-new nil)))
    (is-false (gtk:window-title dialog))))

;;; --- 2023-6-15 --------------------------------------------------------------
