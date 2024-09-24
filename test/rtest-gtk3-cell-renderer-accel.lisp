(in-package :gtk-test)

(def-suite gtk-cell-renderer-accel :in gtk-suite)
(in-suite gtk-cell-renderer-accel)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererAccelMode

(test gtk-cell-renderer-accel-mode
  ;; Check type
  (is (g:type-is-enum "GtkCellRendererAccelMode"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererAccelMode")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_accel_mode_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-accel-mode
          (glib:symbol-for-gtype "GtkCellRendererAccelMode")))
  ;; Check names
  #-windows
  (is (equal '("GTK_CELL_RENDERER_ACCEL_MODE_GTK"
               "GTK_CELL_RENDERER_ACCEL_MODE_OTHER"
               "GTK_CELL_RENDERER_ACCEL_MODE_MODIFIER_TAP")
             (glib-test:list-enum-item-names "GtkCellRendererAccelMode")))
  #+windows
  (is (equal '("GTK_CELL_RENDERER_ACCEL_MODE_GTK"
               "GTK_CELL_RENDERER_ACCEL_MODE_OTHER")
             (glib-test:list-enum-item-names "GtkCellRendererAccelMode")))
  ;; Check values
  #-windows
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkCellRendererAccelMode")))
  #+windows
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkCellRendererAccelMode")))
  ;; Check nick names
  #-windows
  (is (equal '("gtk" "other" "modifier-tap")
             (glib-test:list-enum-item-nicks "GtkCellRendererAccelMode")))
  #+windows
  (is (equal '("gtk" "other")
             (glib-test:list-enum-item-nicks "GtkCellRendererAccelMode")))
  ;; Check enum definition
  #-windows
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkCellRendererAccelMode"
                                     GTK:CELL-RENDERER-ACCEL-MODE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_cell_renderer_accel_mode_get_type")
                       (:GTK 0)
                       (:OTHER 1)
                       (:MODIFIER-TAP 2))
             (gobject:get-gtype-definition "GtkCellRendererAccelMode")))
  #+windows
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkCellRendererAccelMode"
                                     GTK:CELL-RENDERER-ACCEL-MODE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_cell_renderer_accel_mode_get_type")
                       (:GTK 0)
                       (:OTHER 1))
             (gobject:get-gtype-definition "GtkCellRendererAccelMode"))))

;;;     GtkCellRendererAccel

(test gtk-cell-renderer-accel-class
  ;; Check type
  (is (g:type-is-object "GtkCellRendererAccel"))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-accel
          (glib:symbol-for-gtype "GtkCellRendererAccel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererAccel")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_accel_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCellRendererText")
          (g:type-parent "GtkCellRendererAccel")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCellRendererAccel")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCellRendererAccel")))
  ;; Check class properties
  (is (equal '("accel-key" "accel-mode" "accel-mods" "keycode")
             (glib-test:list-properties "GtkCellRendererAccel")))
  ;; Check signals
  (is (equal '("accel-cleared" "accel-edited")
             (glib-test:list-signals "GtkCellRendererAccel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellRendererAccel"
                                       GTK:CELL-RENDERER-ACCEL
                       (:SUPERCLASS GTK:CELL-RENDERER-TEXT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_cell_renderer_accel_get_type")
                       ((ACCEL-KEY CELL-RENDERER-ACCEL-ACCEL-KEY
                         "accel-key" "guint" T T)
                        (ACCEL-MODE CELL-RENDERER-ACCEL-ACCEL-MODE
                         "accel-mode" "GtkCellRendererAccelMode" T T)
                        (ACCEL-MODS CELL-RENDERER-ACCEL-ACCEL-MODS
                         "accel-mods" "GdkModifierType" T T)
                        (KEYCODE CELL-RENDERER-ACCEL-KEYCODE
                         "keycode" "guint" T T)))
             (gobject:get-gtype-definition "GtkCellRendererAccel"))))

;;; --- Properties -------------------------------------------------------------

;;;     accel-key
;;;     accel-mode
;;;     accel-mods
;;;     keycode

;;; --- Signals ----------------------------------------------------------------

;;;     accel-cleared
;;;     accel-edited

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_accel_new

;;; 2024-9-22
