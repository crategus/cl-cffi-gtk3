(in-package :gtk-test)

(def-suite gtk-icon-view :in gtk-suite)
(in-suite gtk-icon-view)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkIconViewDropPosition

(test gtk-icon-view-drop-position
  ;; Check the type
  (is (g:type-is-enum "GtkIconViewDropPosition"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkIconViewDropPosition")
          (g:gtype (cffi:foreign-funcall "gtk_icon_view_drop_position_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:icon-view-drop-position
          (glib:symbol-for-gtype "GtkIconViewDropPosition")))
  ;; Check the names
  (is (equal '("GTK_ICON_VIEW_NO_DROP" "GTK_ICON_VIEW_DROP_INTO"
               "GTK_ICON_VIEW_DROP_LEFT" "GTK_ICON_VIEW_DROP_RIGHT"
               "GTK_ICON_VIEW_DROP_ABOVE" "GTK_ICON_VIEW_DROP_BELOW")
             (list-enum-item-name "GtkIconViewDropPosition")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5)
             (list-enum-item-value "GtkIconViewDropPosition")))
  ;; Check the nick names
  (is (equal '("no-drop" "drop-into" "drop-left" "drop-right" "drop-above"
               "drop-below")
             (list-enum-item-nick "GtkIconViewDropPosition")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkIconViewDropPosition"
                                     GTK-ICON-VIEW-DROP-POSITION
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_icon_view_drop_position_get_type")
                                     (:NO-DROP 0)
                                     (:DROP-INTO 1)
                                     (:DROP-LEFT 2)
                                     (:DROP-RIGHT 3)
                                     (:DROP-ABOVE 4)
                                     (:DROP-BELOW 5))
             (gobject:get-g-type-definition "GtkIconViewDropPosition"))))

;;;     GtkIconView

(test gtk-icon-view-class
  ;; Type check
  (is (g:type-is-object "GtkIconView"))
  ;; Check the registered name
  (is (eq 'gtk:icon-view
          (glib:symbol-for-gtype "GtkIconView")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkIconView")
          (g:gtype (cffi:foreign-funcall "gtk_icon_view_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkContainer")
          (g:type-parent "GtkIconView")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkIconView")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkCellLayout"
               "GtkScrollable")
             (list-interfaces "GtkIconView")))
  ;; Check the class properties
  (is (equal '("activate-on-single-click" "cell-area" "column-spacing" "columns"
               "hadjustment" "hscroll-policy" "item-orientation" "item-padding"
               "item-width" "markup-column" "model" "pixbuf-column"
               "reorderable" "row-spacing" "selection-mode" "spacing"
               "text-column" "tooltip-column" "vadjustment" "vscroll-policy")
             (list-properties "GtkIconView")))
  ;; Check the style properties
  (is (equal '("selection-box-alpha" "selection-box-color")
             (list-style-properties "GtkIconView")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkIconView")))
  ;; Check the signals
  (is (equal '("activate-cursor-item" "item-activated" "move-cursor"
               "select-all" "select-cursor-item" "selection-changed"
               "toggle-cursor-item" "unselect-all")
             (list-signals "GtkIconView")))
  ;; CSS information
  (is (string= "iconview"
               (gtk:widget-class-css-name "GtkIconView")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkIconView" GTK-ICON-VIEW
                               (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable"
                                 "GtkCellLayout" "GtkScrollable")
                                :TYPE-INITIALIZER "gtk_icon_view_get_type")
                               ((ACTIVATE-ON-SINGLE-CLICK
                                 GTK-ICON-VIEW-ACTIVATE-ON-SINGLE-CLICK
                                 "activate-on-single-click" "gboolean" T T)
                                (CELL-AREA GTK-ICON-VIEW-CELL-AREA "cell-area"
                                 "GtkCellArea" T NIL)
                                (COLUMN-SPACING GTK-ICON-VIEW-COLUMN-SPACING
                                 "column-spacing" "gint" T T)
                                (COLUMNS GTK-ICON-VIEW-COLUMNS "columns" "gint"
                                 T T)
                                (ITEM-ORIENTATION
                                 GTK-ICON-VIEW-ITEM-ORIENTATION
                                 "item-orientation" "GtkOrientation" T T)
                                (ITEM-PADDING GTK-ICON-VIEW-ITEM-PADDING
                                 "item-padding" "gint" T T)
                                (ITEM-WIDTH GTK-ICON-VIEW-ITEM-WIDTH
                                 "item-width" "gint" T T)
                                (MARGIN GTK-ICON-VIEW-MARGIN "margin" "gint" T
                                 T)
                                (MARKUP-COLUMN GTK-ICON-VIEW-MARKUP-COLUMN
                                 "markup-column" "gint" T T)
                                (MODEL GTK-ICON-VIEW-MODEL "model"
                                 "GtkTreeModel" T T)
                                (PIXBUF-COLUMN GTK-ICON-VIEW-PIXBUF-COLUMN
                                 "pixbuf-column" "gint" T T)
                                (REORDERABLE GTK-ICON-VIEW-REORDERABLE
                                 "reorderable" "gboolean" T T)
                                (ROW-SPACING GTK-ICON-VIEW-ROW-SPACING
                                 "row-spacing" "gint" T T)
                                (SELECTION-MODE GTK-ICON-VIEW-SELECTION-MODE
                                 "selection-mode" "GtkSelectionMode" T T)
                                (SPACING GTK-ICON-VIEW-SPACING "spacing" "gint"
                                 T T)
                                (TEXT-COLUMN GTK-ICON-VIEW-TEXT-COLUMN
                                 "text-column" "gint" T T)
                                (TOOLTIP-COLUMN GTK-ICON-VIEW-TOOLTIP-COLUMN
                                 "tooltip-column" "gint" T T)))
             (gobject:get-g-type-definition "GtkIconView"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-icon-view-properties
  (let ((view (make-instance 'gtk:icon-view)))
    (is-false (gtk:icon-view-activate-on-single-click view))
    (is (typep (gtk:icon-view-cell-area view) 'gtk:cell-area-box))
    (is (= 6 (gtk:icon-view-column-spacing view)))
    (is (= -1 (gtk:icon-view-columns view)))
    (is (eq :vertical (gtk:icon-view-item-orientation view)))
    (is (= 6 (gtk:icon-view-item-padding view)))
    (is (= -1 (gtk:icon-view-item-width view)))
    (is (= 6 (gtk:icon-view-margin view)))
    (is (= -1 (gtk:icon-view-markup-column view)))
    (is-false (gtk:icon-view-model view))
    (is (= -1 (gtk:icon-view-pixbuf-column view)))
    (is-false (gtk:icon-view-reorderable view))
    (is (= 6 (gtk:icon-view-row-spacing view)))
    (is (eq :single (gtk:icon-view-selection-mode view)))
    (is (= 0 (gtk:icon-view-spacing view)))
    (is (= -1 (gtk:icon-view-text-column view)))
    (is (= -1 (gtk:icon-view-tooltip-column view)))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-icon-view-style-properties
  (let ((view (make-instance 'gtk:icon-view)))
    (is (= 64 (gtk:widget-style-property view "selection-box-alpha")))
    (is-false (gtk:widget-style-property view "selection-box-color"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate-cursor-item
;;;     item-activated
;;;     move-cursor
;;;     select-all
;;;     select-cursor-item
;;;     selection-changed
;;;     toggle-cursor-item
;;;     unselect-all

;;; --- Functions --------------------------------------------------------------

;;;     GtkIconViewForeachFunc

;;;     gtk_icon_view_new

(test gtk-icon-view-new
  (is (typep (gtk:icon-view-new) 'gtk:icon-view)))

;;;     gtk_icon_view_new_with_area

(test gtk-icon-view-new-with-area
  (let ((box (gtk:cell-area-box-new)))
    (is (typep (gtk:icon-view-new-with-area box) 'gtk:icon-view))))

;;;     gtk_icon_view_new_with_model

(test gtk-icon-view-new-with-model
  (let ((model (create-and-fill-list-store)))
    (is (typep (gtk:icon-view-new-with-model model) 'gtk:icon-view))))

;;;     gtk_icon_view_get_path_at_pos
;;;     gtk_icon_view_get_item_at_pos
;;;     gtk_icon_view_convert_widget_to_bin_window_coords
;;;     gtk_icon_view_set_cursor
;;;     gtk_icon_view_get_cursor
;;;     gtk_icon_view_selected_foreach
;;;     gtk_icon_view_get_cell_rect
;;;     gtk_icon_view_select_path
;;;     gtk_icon_view_unselect_path
;;;     gtk_icon_view_path_is_selected
;;;     gtk_icon_view_get_selected_items
;;;     gtk_icon_view_select_all
;;;     gtk_icon_view_unselect_all
;;;     gtk_icon_view_item_activated
;;;     gtk_icon_view_scroll_to_path
;;;     gtk_icon_view_get_visible_range
;;;     gtk_icon_view_set_tooltip_item
;;;     gtk_icon_view_set_tooltip_cell
;;;     gtk_icon_view_get_tooltip_context
;;;     gtk_icon_view_get_item_row
;;;     gtk_icon_view_get_item_column

;;;     gtk_icon_view_enable_model_drag_source

(test gtk-icon-view-enable-model-drag-source
  (let ((targets '(("text/html" :none 0)
                   ("STRING" :none 1)
                   ("number" :none 2)
                   ("image/jpeg" :none 3)
                   ("text/uri-list" :none 4)))
        (view (make-instance 'gtk:icon-view)))
    (is-false (gtk:icon-view-enable-model-drag-source view
                                                      :button1-mask
                                                      targets
                                                      :copy))))

;;;     gtk_icon_view_enable_model_drag_dest

(test gtk-icon-view-enable-model-drag-dest
  (let ((targets '(("text/html" :none 0)
                   ("STRING" :none 1)
                   ("number" :none 2)
                   ("image/jpeg" :none 3)
                   ("text/uri-list" :none 4)))
        (view (make-instance 'gtk:icon-view)))
    (is-false (gtk:icon-view-enable-model-drag-dest view targets :copy))))

;;;     gtk_icon_view_unset_model_drag_source
;;;     gtk_icon_view_unset_model_drag_dest
;;;     gtk_icon_view_set_drag_dest_item
;;;     gtk_icon_view_get_drag_dest_item
;;;     gtk_icon_view_get_dest_item_at_pos
;;;     gtk_icon_view_create_drag_icon

;;; 2024-3-24
