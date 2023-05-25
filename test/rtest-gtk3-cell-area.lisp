(in-package :gtk-test)

(def-suite gtk-cell-area :in gtk-suite)
(in-suite gtk-cell-area)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellArea

(test gtk-cell-area-class
  ;; Type check
  (is (g:type-is-object "GtkCellArea"))
  ;; Check the registered name
  (is (eq 'gtk:cell-area
          (gobject:symbol-for-gtype "GtkCellArea")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellArea")
          (g:gtype (cffi:foreign-funcall "gtk_cell_area_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GInitiallyUnowned")
          (g:type-parent "GtkCellArea")))
  ;; Check the children
  (is (equal '("GtkCellAreaBox")
             (list-children "GtkCellArea")))
  ;; Check the interfaces
  (is (equal '("GtkCellLayout" "GtkBuildable")
             (list-interfaces "GtkCellArea")))
  ;; Check the class properties
  (is (equal '("edit-widget" "edited-cell" "focus-cell")
             (list-properties "GtkCellArea")))
  ;; Check the signals
  (is (equal '("add-editable" "apply-attributes" "focus-changed"
               "remove-editable")
             (list-signals "GtkCellArea")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkCellArea" GTK-CELL-AREA
                       (:SUPERCLASS G-INITIALLY-UNOWNED :EXPORT T :INTERFACES
                        ("GtkBuildable" "GtkCellLayout") :TYPE-INITIALIZER
                        "gtk_cell_area_get_type")
                       ((EDIT-WIDGET GTK-CELL-AREA-EDIT-WIDGET "edit-widget"
                         "GtkCellEditable" T NIL)
                        (EDITED-CELL GTK-CELL-AREA-EDITED-CELL "edited-cell"
                         "GtkCellRenderer" T NIL)
                        (FOCUS-CELL GTK-CELL-AREA-FOCUS-CELL "focus-cell"
                         "GtkCellRenderer" T T)))
             (gobject:get-g-type-definition "GtkCellArea"))))

;;; --- Properties -------------------------------------------------------------

;;;     edit-widget
;;;     edited-cell
;;;     focus-cell

(test gtk-cell-area-properties
  ;; Create a GtkCellAreaBox object, GtkCellArea is a abstract class.
  (let ((area (make-instance 'gtk:cell-area-box)))
    (is-false (gtk:cell-area-edit-widget area))
    (is-false (gtk:cell-area-edited-cell area))
    (is-false (gtk:cell-area-focus-cell area))))

;;; --- Signals ----------------------------------------------------------------

;;;     add-editable
;;;     apply-attributes
;;;     focus-changed
;;;     remove-editable

;;; --- Functions --------------------------------------------------------------

;;;     GtkCellCallback
;;;     GtkCellAllocCallback

;;;     GTK_CELL_AREA_WARN_INVALID_CELL_PROPERTY_ID

;;;     gtk_cell_area_add
;;;     gtk_cell_area_remove
;;;     gtk_cell_area_has_renderer

;;;     gtk_cell_area_foreach

(test gtk-cell-area-foreach
  (let ((area (gtk:cell-area-box-new))
        (message nil))
    ;; Add five cell renderer to the area box
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-pixbuf-new))
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-progress-new))
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-spinner-new))
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-text-new))
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-toggle-new))
    ;; Collect the types of the renderers
    (gtk:cell-area-foreach area
                           (lambda (renderer)
                             (push (type-of renderer) message)
                             nil))
    ;; Check the types of the renderers
    (is (equal '(GTK:CELL-RENDERER-PIXBUF GTK:CELL-RENDERER-PROGRESS
                 GTK:CELL-RENDERER-SPINNER GTK:CELL-RENDERER-TEXT
                 GTK:CELL-RENDERER-TOGGLE)
               (reverse message)))))

;;;     gtk_cell_area_foreach_alloc

#-windows
(test gtk-cell-area-foreach-alloc
  (let ((area (gtk:cell-area-box-new))
        (widget (make-instance 'gtk:window
                               :type :toplevel
                               :width-request 120
                               :height-request 60))
        (message nil))
    ;; Realize the window
    (gtk:widget-realize widget)
    ;; Add five cell renderers to the area box
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-text-new))
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-toggle-new))
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-pixbuf-new))
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-progress-new))
    (gtk:cell-area-box-pack-start area (gtk:cell-renderer-spinner-new))
    ;; Collect for each renderer information about the layout
    (gtk:cell-area-foreach-alloc
            area
            (gtk:cell-area-create-context area)
            widget
            (gtk:widget-allocation widget)
            (gtk:widget-allocation widget)
            (lambda (renderer cell background)
              (push (list (type-of renderer)
                          (list (gdk:rectangle-x cell)
                                (gdk:rectangle-width cell))
                          (list (gdk:rectangle-x background)
                                (gdk:rectangle-width background)))
                    message)
              nil))
    ;; Check the result
    (is (equal '((GTK:CELL-RENDERER-TEXT (0 35) (0 35))
                 (GTK:CELL-RENDERER-TOGGLE (35 51) (35 51))
                 (GTK:CELL-RENDERER-PIXBUF (86 31) (86 31))
                 (GTK:CELL-RENDERER-PROGRESS (117 68) (117 68))
                 (GTK:CELL-RENDERER-SPINNER (185 47) (185 47)))
               (reverse message)))))

;;;     gtk_cell_area_event
;;;     gtk_cell_area_render
;;;     gtk_cell_area_get_cell_allocation
;;;     gtk_cell_area_get_cell_at_position
;;;     gtk_cell_area_create_context
;;;     gtk_cell_area_copy_context
;;;     gtk_cell_area_get_request_mode
;;;     gtk_cell_area_get_preferred_width
;;;     gtk_cell_area_get_preferred_height_for_width
;;;     gtk_cell_area_get_preferred_height
;;;     gtk_cell_area_get_preferred_width_for_height
;;;     gtk_cell_area_get_current_path_string
;;;     gtk_cell_area_apply_attributes
;;;     gtk_cell_area_attribute_connect
;;;     gtk_cell_area_attribute_disconnect
;;;     gtk_cell_area_attribute_get_column ()
;;;     gtk_cell_area_class_install_cell_property

;;;     gtk_cell_area_class_find_cell_property

(test gtk-cell-area-class-find-cell-property
  (let ((area (make-instance 'gtk:cell-area-box))
        (renderer (make-instance 'gtk:cell-renderer-pixbuf)))
    (is-false (gtk:cell-area-add area renderer))
    (is (cffi:pointerp (gtk:cell-area-class-find-cell-property "GtkCellAreaBox"
                                                               "align")))
    (is (cffi:pointerp (gtk:cell-area-class-find-cell-property "GtkCellAreaBox"
                                                               "expand")))
    (is (cffi:pointerp (gtk:cell-area-class-find-cell-property "GtkCellAreaBox"
                                                               "fixed-size")))
    (is (cffi:pointerp (gtk:cell-area-class-find-cell-property "GtkCellAreaBox"
                                                               "pack-type")))))

;;;     gtk_cell_area_class_list_cell_properties

(test gtk-cell-area-class-list-cell-properties
  (is (equal '("align" "expand" "fixed-size" "pack-type")
             (sort (mapcar #'g:param-spec-name
                           (gtk:cell-area-class-list-cell-properties
                                                              "GtkCellAreaBox"))
                   #'string<))))

;;;     gtk_cell_area_add_with_properties

;;;     gtk_cell_area_cell_set
;;;     gtk_cell_area_cell_get

(test gtk-cell-area-cell-set/get
  (let ((area (make-instance 'gtk:cell-area-box))
        (renderer (make-instance 'gtk:cell-renderer-pixbuf)))
    (is-false (gtk:cell-area-add area renderer))
    (is-false (gtk:cell-area-cell-set area
                                      renderer
                                      "align" t
                                      "expand" t
                                      "fixed-size" nil
                                      "pack-type" :end))
    (is (equal '(t t nil :end)
               (gtk:cell-area-cell-get area
                                       renderer
                                       "align"
                                       "expand"
                                       "fixed-size"
                                       "pack-type")))))

;;;     gtk_cell_area_cell_set_valist
;;;     gtk_cell_area_cell_get_valist

;;;     gtk_cell_area_cell_set_property
;;;     gtk_cell_area_cell_get_property

(test gtk-cell-area-cell-property
  (let ((area (make-instance 'gtk:cell-area-box))
        (renderer (make-instance 'gtk:cell-renderer-pixbuf)))
    (is-false (gtk:cell-area-add area renderer))
    (is-false (gtk:cell-area-cell-property area renderer "align"))
    (is-false (gtk:cell-area-cell-property area renderer "expand"))
    (is-true (gtk:cell-area-cell-property area renderer "fixed-size"))
    (is (eq :start (gtk:cell-area-cell-property area renderer "pack-type")))))

;;;     gtk_cell_area_is_activatable
;;;     gtk_cell_area_activate
;;;     gtk_cell_area_focus
;;;     gtk_cell_area_add_focus_sibling
;;;     gtk_cell_area_remove_focus_sibling
;;;     gtk_cell_area_is_focus_sibling
;;;     gtk_cell_area_get_focus_siblings
;;;     gtk_cell_area_get_focus_from_sibling
;;;     gtk_cell_area_activate_cell
;;;     gtk_cell_area_stop_editing
;;;     gtk_cell_area_inner_cell_area
;;;     gtk_cell_area_request_renderer

;;; --- 2023-5-13 --------------------------------------------------------------
