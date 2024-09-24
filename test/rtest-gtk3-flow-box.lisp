(in-package :gtk-test)

(def-suite gtk-flow-box :in gtk-suite)
(in-suite gtk-flow-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFlowBoxChild

(test gtk-flow-box-child-class
  ;; Check type
  (is (g:type-is-object "GtkFlowBoxChild"))
  ;; Check registered name
  (is (eq 'gtk:flow-box-child
          (glib:symbol-for-gtype "GtkFlowBoxChild")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFlowBoxChild")
          (g:gtype (cffi:foreign-funcall "gtk_flow_box_child_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin") (g:type-parent "GtkFlowBoxChild")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFlowBoxChild")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkFlowBoxChild")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkFlowBoxChild")))
  ;; Check style properties.
  (is (equal '()
             (gtk-test:list-style-properties "GtkFlowBoxChild")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkFlowBoxChild")))
  ;; Check signals
  (is (equal '("activate")
             (glib-test:list-signals "GtkFlowBoxChild")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFlowBoxChild" GTK:FLOW-BOX-CHILD
                       (:SUPERCLASS GTK:BIN
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_flow_box_child_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkFlowBoxChild"))))

;;;     GtkFlowBox

(test gtk-flow-box-class
  ;; Check type
  (is (g:type-is-object "GtkFlowBox"))
  ;; Check registered name
  (is (eq 'gtk:flow-box
          (glib:symbol-for-gtype "GtkFlowBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFlowBox")
          (g:gtype (cffi:foreign-funcall "gtk_flow_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer") (g:type-parent "GtkFlowBox")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFlowBox")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkFlowBox")))
  ;; Check class properties
  (is (equal '("activate-on-single-click" "column-spacing" "homogeneous"
               "max-children-per-line" "min-children-per-line" "orientation"
               "row-spacing" "selection-mode")
             (glib-test:list-properties "GtkFlowBox")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkFlowBox")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkFlowBox")))
  ;; Check signals
  (is (equal '("activate-cursor-child" "child-activated" "move-cursor"
               "select-all" "selected-children-changed" "toggle-cursor-child"
               "unselect-all")
             (glib-test:list-signals "GtkFlowBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFlowBox" GTK:FLOW-BOX
                       (:SUPERCLASS GTK:CONTAINER
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_flow_box_get_type")
                       ((ACTIVATE-ON-SINGLE-CLICK
                         FLOW-BOX-ACTIVATE-ON-SINGLE-CLICK
                         "activate-on-single-click" "gboolean" T T)
                        (COLUMN-SPACING FLOW-BOX-COLUMN-SPACING
                         "column-spacing" "guint" T T)
                        (HOMOGENEOUS FLOW-BOX-HOMOGENEOUS
                         "homogeneous" "gboolean" T T)
                        (MAX-CHILDREN-PER-LINE FLOW-BOX-MAX-CHILDREN-PER-LINE
                         "max-children-per-line" "guint" T T)
                        (MIN-CHILDREN-PER-LINE FLOW-BOX-MIN-CHILDREN-PER-LINE
                         "min-children-per-line" "guint" T T)
                        (ROW-SPACING FLOW-BOX-ROW-SPACING
                         "row-spacing" "guint" T T)
                        (SELECTION-MODE FLOW-BOX-SELECTION-MODE
                         "selection-mode" "GtkSelectionMode" T T)))
             (gobject:get-gtype-definition "GtkFlowBox"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-flow-box-properties
  (let ((flowbox (make-instance 'gtk:flow-box)))
    (is-true (gtk:flow-box-activate-on-single-click flowbox))
    (is (= 0 (gtk:flow-box-column-spacing flowbox)))
    (is-false (gtk:flow-box-homogeneous flowbox))
    (is (= 7 (gtk:flow-box-max-children-per-line flowbox)))
    (is (= 0 (gtk:flow-box-min-children-per-line flowbox)))
    (is (= 0 (gtk:flow-box-row-spacing flowbox)))
    (is (eq :single (gtk:flow-box-selection-mode flowbox)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_flow_box_new
;;;     gtk_flow_box_insert
;;;     gtk_flow_box_get_child_at_index
;;;     gtk_flow_box_get_child_at_pos
;;;     gtk_flow_box_set_hadjustment
;;;     gtk_flow_box_set_vadjustment
;;;     gtk_flow_box_set_homogeneous
;;;     gtk_flow_box_get_homogeneous
;;;     gtk_flow_box_set_row_spacing
;;;     gtk_flow_box_get_row_spacing
;;;     gtk_flow_box_set_column_spacing
;;;     gtk_flow_box_get_column_spacing
;;;     gtk_flow_box_set_min_children_per_line
;;;     gtk_flow_box_get_min_children_per_line
;;;     gtk_flow_box_set_max_children_per_line
;;;     gtk_flow_box_get_max_children_per_line
;;;     gtk_flow_box_set_activate_on_single_click
;;;     gtk_flow_box_get_activate_on_single_click
;;;     GtkFlowBoxForeachFunc
;;;     gtk_flow_box_selected_foreach
;;;     gtk_flow_box_get_selected_children
;;;     gtk_flow_box_select_child
;;;     gtk_flow_box_unselect_child
;;;     gtk_flow_box_select_all
;;;     gtk_flow_box_unselect_all
;;;     gtk_flow_box_set_selection_mode
;;;     gtk_flow_box_get_selection_mode
;;;     GtkFlowBoxFilterFunc
;;;     gtk_flow_box_set_filter_func
;;;     gtk_flow_box_invalidate_filter
;;;     GtkFlowBoxSortFunc
;;;     gtk_flow_box_set_sort_func
;;;     gtk_flow_box_invalidate_sort
;;;     GtkFlowBoxCreateWidgetFunc
;;;     gtk_flow_box_bind_model

;;;     gtk_flow_box_child_new
;;;     gtk_flow_box_child_get_index
;;;     gtk_flow_box_child_is_selected
;;;     gtk_flow_box_child_changed

;;; 2024-9-21
