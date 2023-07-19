(in-package :gtk-test)

(def-suite gtk-native-dialog :in gtk-suite)
(in-suite gtk-native-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNativeDialog

(test gtk-native-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkNativeDialog"))
  ;; Check the registered name
  (is (eq 'gtk:native-dialog
          (glib:symbol-for-gtype "GtkNativeDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkNativeDialog")
          (g:gtype (cffi:foreign-funcall "gtk_native_dialog_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkNativeDialog")))
  ;; Check the children
  (is (equal '("GtkFileChooserNative")
             (list-children "GtkNativeDialog")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkNativeDialog")))
  ;; Check the class properties
  (is (equal '("modal" "title" "transient-for" "visible")
             (list-properties "GtkNativeDialog")))
  ;; Check the signals
  (is (equal '("response")
             (list-signals "GtkNativeDialog")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkNativeDialog" GTK-NATIVE-DIALOG
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_native_dialog_get_type")
                       ((MODAL GTK-NATIVE-DIALOG-MODAL "modal" "gboolean" T T)
                        (TITLE GTK-NATIVE-DIALOG-TITLE "title" "gchararray" T
                         T)
                        (TRANSIENT-FOR GTK-NATIVE-DIALOG-TRANSIENT-FOR
                         "transient-for" "GtkWindow" T T)
                        (VISIBLE GTK-NATIVE-DIALOG-VISIBLE "visible" "gboolean"
                         T T)))
             (gobject:get-g-type-definition "GtkNativeDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     modal
;;;     title
;;;     transient-for
;;;     visible

(test gtk-native-dialog-properties
  (let ((dialog (make-instance 'gtk:file-chooser-native)))
    (is-false (gtk:native-dialog-modal dialog))
    (is-false (gtk:native-dialog-title dialog))
    (is-false (gtk:native-dialog-transient-for dialog))
    (is-false (gtk:native-dialog-visible dialog))))

;;; --- Signals ----------------------------------------------------------------

;;;     response

(test gtk-native-dialog-response-signal
  (let ((query (g:signal-query (g:signal-lookup "response" "GtkNativeDialog"))))
    (is (string= "response" (g:signal-query-signal-name query)))
    (is (string= "GtkNativeDialog"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gint")
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_native_dialog_show
;;;     gtk_native_dialog_hide
;;;     gtk_native_dialog_destroy
;;;     gtk_native_dialog_run

;;; --- 2023-6-11 --------------------------------------------------------------
