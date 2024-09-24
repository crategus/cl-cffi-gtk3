(in-package :gtk-test)

(def-suite gtk-tool-palette :in gtk-suite)
(in-suite gtk-tool-palette)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkToolPaletteDragTargets

(test gtk-tool-palette-drag-targets
  ;; Check type
  (is (g:type-is-flags "GtkToolPaletteDragTargets"))
  ;; Check registered name
  (is (eq 'gtk:tool-palette-drag-targets
          (glib:symbol-for-gtype "GtkToolPaletteDragTargets")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkToolPaletteDragTargets")
          (g:gtype (cffi:foreign-funcall "gtk_tool_palette_drag_targets_get_type"
                                  :size))))
  ;; Check names
  (is (equal '("GTK_TOOL_PALETTE_DRAG_ITEMS" "GTK_TOOL_PALETTE_DRAG_GROUPS")
             (glib-test:list-flags-item-names "GtkToolPaletteDragTargets")))
  ;; Check values
  (is (equal '(1 2)
             (glib-test:list-flags-item-values "GtkToolPaletteDragTargets")))
  ;; Check nick names
  (is (equal '("items" "groups")
             (glib-test:list-flags-item-nicks "GtkToolPaletteDragTargets")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkToolPaletteDragTargets"
                                     GTK:TOOL-PALETTE-DRAG-TARGETS
                       (:EXPORT T
                        :TYPE-INITIALIZER
                        "gtk_tool_palette_drag_targets_get_type")
                       (:ITEMS 1)
                       (:GROUPS 2))
             (gobject:get-gtype-definition "GtkToolPaletteDragTargets"))))

;;;     GtkToolPalette

(test gtk-tool-palette-class
  ;; Type check
  (is (g:type-is-object "GtkToolPalette"))
  ;; Check registered name
  (is (eq 'gtk:tool-palette
          (glib:symbol-for-gtype "GtkToolPalette")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkToolPalette")
          (g:gtype (cffi:foreign-funcall "gtk_tool_palette_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer") (g:type-parent "GtkToolPalette")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkToolPalette")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
               "GtkScrollable")
             (glib-test:list-interfaces "GtkToolPalette")))
  ;; Check class properties
  (is (equal '("hadjustment" "hscroll-policy" "icon-size" "icon-size-set"
               "orientation" "toolbar-style" "vadjustment" "vscroll-policy")
             (glib-test:list-properties "GtkToolPalette")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkToolPalette")))
  ;; Check child properties
  (is (equal '("exclusive" "expand")
             (gtk-test:list-child-properties "GtkToolPalette")))
  (is (equal '()
             (glib-test:list-signals "GtkToolPalette")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkToolPalette" GTK:TOOL-PALETTE
                       (:SUPERCLASS GTK:CONTAINER
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
                         "GtkScrollable")
                        :TYPE-INITIALIZER "gtk_tool_palette_get_type")
                       ((ICON-SIZE TOOL-PALETTE-ICON-SIZE
                         "icon-size" "GtkIconSize" T T)
                        (ICON-SIZE-SET TOOL-PALETTE-ICON-SIZE-SET
                         "icon-size-set" "gboolean" T T)
                        (TOOLBAR-STYLE TOOL-PALETTE-TOOLBAR-STYLE
                         "toolbar-style" "GtkToolbarStyle" T T)))
             (gobject:get-gtype-definition "GtkToolPalette"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tool_palette_new
;;;     gtk_tool_palette_get_exclusive
;;;     gtk_tool_palette_set_exclusive
;;;     gtk_tool_palette_get_expand
;;;     gtk_tool_palette_set_expand
;;;     gtk_tool_palette_get_group_position
;;;     gtk_tool_palette_set_group_position
;;;     gtk_tool_palette_get_icon_size
;;;     gtk_tool_palette_set_icon_size
;;;     gtk_tool_palette_unset_icon_size
;;;     gtk_tool_palette_get_style
;;;     gtk_tool_palette_set_style
;;;     gtk_tool_palette_unset_style

;;;     gtk_tool_palette_add_drag_dest
;;;     gtk_tool_palette_get_drag_item

;;;     gtk_tool_palette_get_drag_target_group

(test gtk-tool-palette-drag-target-group
  (is (equal '("application/x-gtk-tool-palette-group" (:SAME-APP) 0)
             (gtk:tool-palette-drag-target-group))))

;;;     gtk_tool_palette_get_drag_target_item

(test gtk-tool-palette-drag-target-item
  (is (equal '("application/x-gtk-tool-palette-item" (:SAME-APP) 0)
             (gtk:tool-palette-drag-target-item))))

;;;     gtk_tool_palette_get_drop_group
;;;     gtk_tool_palette_get_drop_item
;;;
;;;
;;;     gtk_tool_palette_set_drag_source
;;;     gtk_tool_palette_get_hadjustment
;;;     gtk_tool_palette_get_vadjustment

;;; 2024-9-22
