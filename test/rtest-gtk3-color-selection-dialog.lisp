(in-package :gtk-test)

(def-suite gtk-color-selection-dialog :in gtk-suite)
(in-suite gtk-color-selection-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorSelectionDialog

(test gtk-color-selection-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkColorSelectionDialog"))
  ;; Check registered name
  (is (eq 'gtk:color-selection-dialog
          (glib:symbol-for-gtype "GtkColorSelectionDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkColorSelectionDialog")
          (g:gtype (cffi:foreign-funcall "gtk_color_selection_dialog_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkColorSelectionDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkColorSelectionDialog")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkColorSelectionDialog")))
  ;; Check class properties
  (is (equal '("cancel-button" "color-selection" "help-button" "ok-button")
             (glib-test:list-properties "GtkColorSelectionDialog")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkColorSelectionDialog")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkColorSelectionDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkColorSelectionDialog")))
  ;; CSS information
  (is (string= "dialog"
               (gtk:widget-class-css-name "GtkColorSelectionDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkColorSelectionDialog"
                                      GTK:COLOR-SELECTION-DIALOG
                       (:SUPERCLASS GTK:DIALOG
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_color_selection_dialog_get_type")
                       ((CANCEL-BUTTON COLOR-SELECTION-DIALOG-CANCEL-BUTTON
                         "cancel-button" "GtkWidget" T NIL)
                        (COLOR-SELECTION COLOR-SELECTION-DIALOG-COLOR-SELECTION
                         "color-selection" "GtkWidget" T NIL)
                        (HELP-BUTTON COLOR-SELECTION-DIALOG-HELP-BUTTON
                         "help-button" "GtkWidget" T NIL)
                        (OK-BUTTON COLOR-SELECTION-DIALOG-OK-BUTTON
                         "ok-button" "GtkWidget" T NIL)))
             (gobject:get-gtype-definition "GtkColorSelectionDialog"))))

;;; --- Properties -------------------------------------------------------------

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

;;; 2024-9-23
