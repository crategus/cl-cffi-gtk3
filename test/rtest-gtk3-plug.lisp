(in-package :gtk-test)

(def-suite gtk-plug :in gtk-test)
(in-suite gtk-plug)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPlug

(test gtk-plug-class
  ;; Check type
  (is (g:type-is-object "GtkPlug"))
  ;; Check registered name
  (is (eq 'gtk:plug
          (glib:symbol-for-gtype "GtkPlug")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPlug")
          (g:gtype (cffi:foreign-funcall "gtk_plug_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWindow")
          (g:type-parent "GtkPlug")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkPlug")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkPlug")))
  ;; Check class properties
  (is (equal '("embedded" "socket-window")
             (glib-test:list-properties "GtkPlug")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkPlug")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkPlug")))
  ;; Check signals
  (is (equal '("embedded")
             (glib-test:list-signals "GtkPlug")))
  ;; Check CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkPlug")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPlug" GTK:PLUG
                      (:SUPERCLASS GTK:WINDOW
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_plug_get_type")
                      ((EMBEDDED PLUG-EMBEDDED "embedded" "gboolean" T NIL)
                       (SOCKET-WINDOW PLUG-SOCKET-WINDOW "socket-window"
                        "GdkWindow" T NIL)))
             (gobject:get-gtype-definition "GtkPlug"))))

;;; --- Signals ----------------------------------------------------------------

;;;     embedded

;;; --- Properties -------------------------------------------------------------

;;;     embedded
;;;     socket-window

;;; --- Functions --------------------------------------------------------------
;;;
;;;     gtk_plug_construct
;;;     gtk_plug_construct_for_display
;;;     gtk_plug_new
;;;     gtk_plug_new_for_display
;;;     gtk_plug_get_id

;;; 2026-06-04
