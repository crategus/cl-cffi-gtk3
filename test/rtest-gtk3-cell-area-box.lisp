(in-package :gtk-test)

(def-suite gtk-cell-area-box :in gtk-suite)
(in-suite gtk-cell-area-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellAreaBox

(test cell-area-box-class
  ;; Type check
  (is (g:type-is-object "GtkCellAreaBox"))
  ;; Check the registered name
  (is (eq 'gtk:cell-area-box
          (gobject:symbol-for-gtype "GtkCellAreaBox")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellAreaBox")
          (g:gtype (cffi:foreign-funcall "gtk_cell_area_box_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCellArea")
          (g:type-parent "GtkCellAreaBox")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCellAreaBox")))
  ;; Check the interfaces
  (is (equal '("GtkCellLayout" "GtkBuildable" "GtkOrientable")
             (list-interfaces "GtkCellAreaBox")))
  ;; Check the class properties
  (is (equal '("orientation" "spacing")
             (list-properties "GtkCellAreaBox")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkCellAreaBox")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkCellAreaBox" GTK-CELL-AREA-BOX
                       (:SUPERCLASS GTK-CELL-AREA :EXPORT T :INTERFACES
                        ("GtkBuildable" "GtkCellLayout" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_cell_area_box_get_type")
                       ((SPACING GTK-CELL-AREA-BOX-SPACING "spacing" "gint" T
                         T)))
             (gobject:get-g-type-definition "GtkCellAreaBox"))))

;;; --- Properties -------------------------------------------------------------

;;;     spacing

;;; --- Child Properties -------------------------------------------------------

;;;     align
;;;     expand
;;;     fixed-size
;;;     pack-type

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_area_box_new
;;;     gtk_cell_area_box_pack_start
;;;     gtk_cell_area_box_pack_end
;;;     gtk_cell_area_box_get_spacing                      Accessor
;;;     gtk_cell_area_box_set_spacing                      Accessor

;;; --- 2023-2-20 --------------------------------------------------------------
