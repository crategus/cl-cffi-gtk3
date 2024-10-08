(in-package :gtk-test)

(def-suite gtk-native-dialog :in gtk-suite)
(in-suite gtk-native-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNativeDialog

(test gtk-native-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkNativeDialog"))
  ;; Check registered name
  (is (eq 'gtk:native-dialog
          (glib:symbol-for-gtype "GtkNativeDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNativeDialog")
          (g:gtype (cffi:foreign-funcall "gtk_native_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkNativeDialog")))
  ;; Check children
  (is (equal '("GtkFileChooserNative")
             (glib-test:list-children "GtkNativeDialog")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkNativeDialog")))
  ;; Check class properties
  (is (equal '("modal" "title" "transient-for" "visible")
             (glib-test:list-properties "GtkNativeDialog")))
  ;; Check signals
  (is (equal '("response")
             (glib-test:list-signals "GtkNativeDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkNativeDialog" GTK:NATIVE-DIALOG
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_native_dialog_get_type")
                       ((MODAL NATIVE-DIALOG-MODAL "modal" "gboolean" T T)
                        (TITLE NATIVE-DIALOG-TITLE "title" "gchararray" T T)
                        (TRANSIENT-FOR NATIVE-DIALOG-TRANSIENT-FOR
                         "transient-for" "GtkWindow" T T)
                        (VISIBLE NATIVE-DIALOG-VISIBLE
                         "visible" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkNativeDialog"))))

;;; --- Properties -------------------------------------------------------------

;; TODO: We get an error on Windows:
;;
;; GTK-NATIVE-DIALOG-PROPERTIES in GTK-NATIVE-DIALOG []:
;;      Unexpected Error: #<SB-WIN32:EXCEPTION {10048ECF73}>
;; An exception occurred in context #.(SB-SYS:INT-SAP #X009693A0):
;; #.(SB-SYS:INT-SAP #X00969FD0). (Exception code: 1722).

#-windows
(test gtk-native-dialog-properties
  (let ((dialog (make-instance 'gtk:file-chooser-native)))
    (is-false (gtk:native-dialog-modal dialog))
    (is-false (gtk:native-dialog-title dialog))
    (is-false (gtk:native-dialog-transient-for dialog))
    (is-false (gtk:native-dialog-visible dialog))))

;;; --- Signals ----------------------------------------------------------------

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

;;; 2024-9-21
