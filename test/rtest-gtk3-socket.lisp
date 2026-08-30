(in-package :gtk-test)

(def-suite gtk-socket :in gtk-test)
(in-suite gtk-socket)

;;; --- Types and Value --------------------------------------------------------

;;;     GtkSocket

(test gtk-socket-class
  ;; Check type
  (is (g:type-is-object "GtkSocket"))
  ;; Check registered name
  (is (eq 'gtk:socket
          (glib:symbol-for-gtype "GtkSocket")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSocket")
          (g:gtype (cffi:foreign-funcall "gtk_socket_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer")
          (g:type-parent "GtkSocket")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSocket")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkSocket")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkSocket")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkSocket")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkSocket")))
  ;; Check signals
  (is (equal '("plug-added" "plug-removed")
             (glib-test:list-signals "GtkSocket")))
  ;; Check CSS information
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkSocket")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSocket" GTK:SOCKET
                      (:SUPERCLASS GTK:CONTAINER
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_socket_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkSocket"))))

;;; --- Signals ----------------------------------------------------------------

;;;     plug-added
;;;     plug-removed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_socket_new
;;;     gtk_socket_add_id
;;;     gtk_socket_get_id
;;;     gtk_socket_get_plug_window

;;; 2026-06-04
