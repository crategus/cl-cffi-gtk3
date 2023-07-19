(in-package :gtk-test)

(def-suite gtk-tool-palette :in gtk-suite)
(in-suite gtk-tool-palette)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkToolPaletteDragTargets

(test tool-palette-drag-targets
  ;; Check the type
  (is (g:type-is-flags "GtkToolPaletteDragTargets"))
  ;; Check the registered name
  (is (eq 'gtk:tool-palette-drag-targets
          (glib:symbol-for-gtype "GtkToolPaletteDragTargets")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkToolPaletteDragTargets")
          (g:gtype (cffi:foreign-funcall "gtk_tool_palette_drag_targets_get_type"
                                  :size))))
  ;; Check the names
  (is (equal '("GTK_TOOL_PALETTE_DRAG_ITEMS" "GTK_TOOL_PALETTE_DRAG_GROUPS")
             (list-flags-item-name "GtkToolPaletteDragTargets")))
  ;; Check the values
  (is (equal '(1 2)
             (list-flags-item-value "GtkToolPaletteDragTargets")))
  ;; Check the nick names
  (is (equal '("items" "groups")
             (list-flags-item-nick "GtkToolPaletteDragTargets")))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkToolPaletteDragTargets"
                              GTK-TOOL-PALETTE-DRAG-TARGETS
                              (:EXPORT T
                               :TYPE-INITIALIZER
                               "gtk_tool_palette_drag_targets_get_type")
                              (:ITEMS 1)
                              (:GROUPS 2))
             (gobject:get-g-type-definition "GtkToolPaletteDragTargets"))))

;;;     GtkToolPalette

(test tool-palette-class
  ;; Type check
  (is (g:type-is-object "GtkToolPalette"))
  ;; Check the registered name
  (is (eq 'gtk:tool-palette
          (glib:symbol-for-gtype "GtkToolPalette")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkToolPalette")
          (g:gtype (cffi:foreign-funcall "gtk_tool_palette_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkContainer") (g:type-parent "GtkToolPalette")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkToolPalette")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
               "GtkScrollable")
             (list-interfaces "GtkToolPalette")))
  ;; Check the class properties
  (is (equal '("hadjustment" "hscroll-policy" "icon-size" "icon-size-set"
               "orientation" "toolbar-style" "vadjustment" "vscroll-policy")
             (list-properties "GtkToolPalette")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-style-properties "GtkToolPalette")))
  ;; Get the names of the child properties
  (is (equal '("exclusive" "expand")
             (list-child-properties "GtkToolPalette")))
  (is (equal '()
             (list-signals "GtkToolPalette")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkToolPalette" GTK-TOOL-PALETTE
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
                         "GtkScrollable")
                        :TYPE-INITIALIZER "gtk_tool_palette_get_type")
                       ((ICON-SIZE GTK-TOOL-PALETTE-ICON-SIZE "icon-size"
                         "GtkIconSize" T T)
                        (ICON-SIZE-SET GTK-TOOL-PALETTE-ICON-SIZE-SET
                         "icon-size-set" "gboolean" T T)
                        (TOOLBAR-STYLE GTK-TOOL-PALETTE-TOOLBAR-STYLE
                         "toolbar-style" "GtkToolbarStyle" T T)))
             (gobject:get-g-type-definition "GtkToolPalette"))))

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

(test tool-palette-drag-target-group
  (is (equal '("application/x-gtk-tool-palette-group" (:SAME-APP) 0)
             (gtk:tool-palette-drag-target-group))))

;;;     gtk_tool_palette_get_drag_target_item

(test tool-palette-drag-target-item
  (is (equal '("application/x-gtk-tool-palette-item" (:SAME-APP) 0)
             (gtk:tool-palette-drag-target-item))))

;;;     gtk_tool_palette_get_drop_group
;;;     gtk_tool_palette_get_drop_item
;;;
;;;
;;;     gtk_tool_palette_set_drag_source
;;;     gtk_tool_palette_get_hadjustment
;;;     gtk_tool_palette_get_vadjustment

;;; --- 2023-5-29 --------------------------------------------------------------
