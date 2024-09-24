(in-package :gtk-test)

(def-suite gtk-offscreen-window :in gtk-suite)
(in-suite gtk-offscreen-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkOffscreenWindow

(test gtk-offscreen-window-class
  ;; Check type
  (is (g:type-is-object "GtkOffscreenWindow"))
  ;; Check registered name
  (is (eq 'gtk:offscreen-window
          (glib:symbol-for-gtype "GtkOffscreenWindow")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkOffscreenWindow")
          (g:gtype (cffi:foreign-funcall "gtk_offscreen_window_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWindow")
          (g:type-parent "GtkOffscreenWindow")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkOffscreenWindow")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkOffscreenWindow")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkOffscreenWindow")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkOffscreenWindow")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkOffscreenWindow")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkOffscreenWindow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkOffscreenWindow" GTK:OFFSCREEN-WINDOW
                       (:SUPERCLASS GTK:WINDOW
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_offscreen_window_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkOffscreenWindow"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_offscreen_window_new

(test gtk-offscreen-window-new
  (is (typep (gtk:offscreen-window-new) 'gtk:offscreen-window)))

;;;     gtk_offscreen_window_get_surface

#+nil
(test gtk-offscreen-window-surface
  (let ((offscreen (gtk:offscreen-window-new)))
    (is-false (gtk:offscreen-window-surface offscreen))))

;;;     gtk_offscreen_window_get_pixbuf

#+nil
(test gtk-offscreen-window-pixbuf
  (let ((offscreen (gtk:offscreen-window-new)))
    (is-false (gtk:offscreen-window-pixbuf offscreen))))

;;; 2024-9-22
