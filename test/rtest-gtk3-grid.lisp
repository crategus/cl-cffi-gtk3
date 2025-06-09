(in-package :gtk-test)

(def-suite gtk-grid :in gtk-suite)
(in-suite gtk-grid)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGrid

(test gtk-grid-class
  ;; Check type
  (is (g:type-is-object "GtkGrid"))
  ;; Check registered name
  (is (eq 'gtk:grid
          (glib:symbol-for-gtype "GtkGrid")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkGrid")
          (g:gtype (cffi:foreign-funcall "gtk_grid_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer") (g:type-parent "GtkGrid")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkGrid")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkGrid")))
  ;; Check class properties
  (is (equal '("baseline-row" "column-homogeneous" "column-spacing"
               "orientation" "row-homogeneous" "row-spacing")
             (glib-test:list-properties "GtkGrid")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkGrid")))
  ;; Check child properties
  (is (equal '("height" "left-attach" "top-attach" "width")
             (gtk-test:list-child-properties "GtkGrid")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkGrid")))
  ;; CSS information
  (is (string= "grid"
               (gtk:widget-class-css-name "GtkGrid")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkGrid" GTK:GRID
                      (:SUPERCLASS GTK:CONTAINER
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_grid_get_type")
                      ((BASELINE-ROW GRID-BASELINE-ROW
                        "baseline-row" "gint" T T)
                       (COLUMN-HOMOGENEOUS GRID-COLUMN-HOMOGENEOUS
                        "column-homogeneous" "gboolean" T T)
                       (COLUMN-SPACING GRID-COLUMN-SPACING
                        "column-spacing" "gint" T T)
                       (ROW-HOMOGENEOUS GRID-ROW-HOMOGENEOUS
                        "row-homogeneous" "gboolean" T T)
                       (ROW-SPACING GRID-ROW-SPACING
                        "row-spacing" "gint" T T)))
             (gobject:get-gtype-definition "GtkGrid"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-grid-properties
  (glib-test:with-check-memory (grid)
    (is (typep (setf grid (make-instance 'gtk:grid)) 'gtk:grid))
    (is (= 0 (gtk:grid-baseline-row grid)))
    (is-false (gtk:grid-column-homogeneous grid))
    (is (= 0 (gtk:grid-column-spacing grid)))
    (is-false (gtk:grid-row-homogeneous grid))
    (is (= 0 (gtk:grid-row-spacing grid)))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-grid-child-properties
  (glib-test:with-check-memory (grid child)
    (is (typep (setf grid (make-instance 'gtk:grid)) 'gtk:grid))
    (is (typep (setf child (make-instance 'gtk:button)) 'gtk:button))
    (is-false (gtk:grid-attach grid child 0 0 1 1 ))
    (is (= 1 (gtk:grid-child-height grid child)))
    (is (= 0 (gtk:grid-child-left-attach grid child)))
    (is (= 0 (gtk:grid-child-top-attach grid child)))
    (is (= 1 (gtk:grid-child-width grid child)))
    ;; Remove references
    (is-false (gtk:grid-remove-row grid 0))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_grid_new

(test gtk-grid-new
  (glib-test:with-check-memory (grid)
    (is (typep (setf grid (gtk:grid-new)) 'gtk:grid))))

;;;     gtk_grid_attach
;;;     gtk_grid_attach_next_to
;;;     gtk_grid_get_child_at

(test gtk-grid-attach
  (glib-test:with-check-memory (grid button1 button2 button3 button4)
    (is (typep (setf grid (make-instance 'gtk:grid)) 'gtk:grid))
    (is (typep (setf button1 (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf button2 (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf button3 (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf button4 (make-instance 'gtk:button)) 'gtk:button))
    (is (= 1 (g:object-ref-count button1)))
    (gtk:grid-attach grid button1 0 0 2 1)
    (gtk:grid-attach grid button2 1 1 1 2)
    (gtk:grid-attach-next-to grid button3 button1 :right 1 1)
    (gtk:grid-attach-next-to grid button4 button2 :left 1 1)
    (is (= 2 (g:object-ref-count button1)))

    (is (eq button1 (gtk:grid-child-at grid 0 0)))
    (let ((button (gtk:grid-child-at grid 0 0)))
      (is (= 2 (g:object-ref-count button)))
      (is (= 0 (gtk:grid-child-left-attach grid button)))
      (is (= 0 (gtk:grid-child-top-attach grid button)))
      (is (= 2 (gtk:grid-child-width grid button)))
      (is (= 1 (gtk:grid-child-height grid button))))

    (is (eq button2 (gtk:grid-child-at grid 1 1)))
    (let ((button (gtk:grid-child-at grid 1 1)))
      (is (= 2 (g:object-ref-count button)))
      (is (= 1 (gtk:grid-child-left-attach grid button)))
      (is (= 1 (gtk:grid-child-top-attach grid button)))
      (is (= 1 (gtk:grid-child-width grid button)))
      (is (= 2 (gtk:grid-child-height grid button))))

    (is (eq button3 (gtk:grid-child-at grid 2 0)))
    (let ((button (gtk:grid-child-at grid 2 0)))
      (is (= 2 (g:object-ref-count button)))
      (is (= 2 (gtk:grid-child-left-attach grid button)))
      (is (= 0 (gtk:grid-child-top-attach grid button)))
      (is (= 1 (gtk:grid-child-width grid button)))
      (is (= 1 (gtk:grid-child-height grid button))))

    (is (eq button4 (gtk:grid-child-at grid 0 1)))
    (let ((button (gtk:grid-child-at grid 0 1)))
      (is (= 2 (g:object-ref-count button)))
      (is (= 0 (gtk:grid-child-left-attach grid button)))
      (is (= 1 (gtk:grid-child-top-attach grid button)))
      (is (= 1 (gtk:grid-child-width grid button)))
      (is (= 1 (gtk:grid-child-height grid button))))
    ;; Remove references
    (is-false (gtk:grid-remove-row grid 2))
    (is-false (gtk:grid-remove-row grid 1))
    (is-false (gtk:grid-remove-row grid 0))))

;;;     gtk_grid_insert_row
;;;     gtk_grid_insert_column
;;;     gtk_grid_remove_row
;;;     gtk_grid_remove_column

(test gtk-grid-insert
  (glib-test:with-check-memory (grid button1 button2 button3 button4)
    (is (typep (setf grid (make-instance 'gtk:grid)) 'gtk:grid))
    (is (typep (setf button1 (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf button2 (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf button3 (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf button4 (make-instance 'gtk:button)) 'gtk:button))
    (is (= 1 (g:object-ref-count button1)))
    (gtk:grid-attach grid button1 0 0 2 1)
    (gtk:grid-attach grid button2 1 1 1 2)
    (gtk:grid-attach-next-to grid button3 button1 :right 1 1)
    (gtk:grid-attach-next-to grid button4 button2 :left 1 1)
    ;; Insert a row and a column
    (gtk:grid-insert-row grid 1)
    (gtk:grid-insert-column grid 1)
    (is (= 2 (g:object-ref-count button1)))

    (is (eq button1 (gtk:grid-child-at grid 0 0)))
    (let ((button (gtk:grid-child-at grid 0 0)))
      (is (= 2 (g:object-ref-count button)))
      (is (= 0 (gtk:grid-child-left-attach grid button)))
      (is (= 0 (gtk:grid-child-top-attach grid button)))
      (is (= 3 (gtk:grid-child-width grid button)))  ; new width
      (is (= 1 (gtk:grid-child-height grid button))))

    (is (eq button2 (gtk:grid-child-at grid 2 2)))
    (let ((button (gtk:grid-child-at grid 2 2))) ; new position
      (is (= 2 (g:object-ref-count button)))
      (is (= 2 (gtk:grid-child-left-attach grid button)))
      (is (= 2 (gtk:grid-child-top-attach grid button)))
      (is (= 1 (gtk:grid-child-width grid button)))
      (is (= 2 (gtk:grid-child-height grid button))))

    (is (eq button3 (gtk:grid-child-at grid 3 0)))
    (let ((button (gtk:grid-child-at grid 3 0))) ; new position
      (is (= 2 (g:object-ref-count button)))
      (is (= 3 (gtk:grid-child-left-attach grid button)))
      (is (= 0 (gtk:grid-child-top-attach grid button)))
      (is (= 1 (gtk:grid-child-width grid button)))
      (is (= 1 (gtk:grid-child-height grid button))))

    (is (eq button4 (gtk:grid-child-at grid 0 2)))
    (let ((button (gtk:grid-child-at grid 0 2))) ; new position
      (is (= 2 (g:object-ref-count button)))
      (is (= 0 (gtk:grid-child-left-attach grid button)))
      (is (= 2 (gtk:grid-child-top-attach grid button)))
      (is (= 1 (gtk:grid-child-width grid button)))
      (is (= 1 (gtk:grid-child-height grid button))))

    ;; Remove a row and a column
    (gtk:grid-remove-row grid 1)
    (gtk:grid-remove-column grid 1)

    (is (eq button1 (gtk:grid-child-at grid 0 0)))
    (let ((button (gtk:grid-child-at grid 0 0)))
      (is (= 2 (g:object-ref-count button)))
      (is (= 0 (gtk:grid-child-left-attach grid button)))
      (is (= 0 (gtk:grid-child-top-attach grid button)))
      (is (= 2 (gtk:grid-child-width grid button)))
      (is (= 1 (gtk:grid-child-height grid button))))

    (is (eq button2 (gtk:grid-child-at grid 1 1)))
    (let ((button (gtk:grid-child-at grid 1 1)))
      (is (= 2 (g:object-ref-count button)))
      (is (= 1 (gtk:grid-child-left-attach grid button)))
      (is (= 1 (gtk:grid-child-top-attach grid button)))
      (is (= 1 (gtk:grid-child-width grid button)))
      (is (= 2 (gtk:grid-child-height grid button))))

    (is (eq button3 (gtk:grid-child-at grid 2 0)))
    (let ((button (gtk:grid-child-at grid 2 0)))
      (is (= 2 (g:object-ref-count button)))
      (is (= 2 (gtk:grid-child-left-attach grid button)))
      (is (= 0 (gtk:grid-child-top-attach grid button)))
      (is (= 1 (gtk:grid-child-width grid button)))
      (is (= 1 (gtk:grid-child-height grid button))))

    (is (eq button4 (gtk:grid-child-at grid 0 1)))
    (let ((button (gtk:grid-child-at grid 0 1)))
      (is (= 2 (g:object-ref-count button)))
      (is (= 0 (gtk:grid-child-left-attach grid button)))
      (is (= 1 (gtk:grid-child-top-attach grid button)))
      (is (= 1 (gtk:grid-child-width grid button)))
      (is (= 1 (gtk:grid-child-height grid button))))
    ;; Remove references
    (is-false (gtk:grid-remove-row grid 2))
    (is-false (gtk:grid-remove-row grid 1))
    (is-false (gtk:grid-remove-row grid 0))))

;;;     gtk_grid_insert_next_to

(test gtk-grid-insert-next-to
  (glib-test:with-check-memory (grid button1 button2 button3 button4)
    (is (typep (setf grid (make-instance 'gtk:grid)) 'gtk:grid))
    (is (typep (setf button1 (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf button2 (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf button3 (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf button4 (make-instance 'gtk:button)) 'gtk:button))
    (is (= 1 (g:object-ref-count button1)))
    (gtk:grid-attach grid button1 0 0 2 1)
    (gtk:grid-attach grid button2 1 1 1 2)
    (gtk:grid-attach-next-to grid button3 button1 :right 1 1)
    (gtk:grid-attach-next-to grid button4 button2 :left 1 1)
    (gtk:grid-insert-next-to grid button2 :left)
    (gtk:grid-insert-next-to grid button2 :top)
    (is (= 2 (g:object-ref-count button1)))

    (is (eq button1 (gtk:grid-child-at grid 0 0)))
    (let ((button (gtk:grid-child-at grid 0 0)))
      (is (= 2 (g:object-ref-count button)))
      (is (= 0 (gtk:grid-child-left-attach grid button)))
      (is (= 0 (gtk:grid-child-top-attach grid button)))
      (is (= 3 (gtk:grid-child-width grid button)))  ; new width
      (is (= 1 (gtk:grid-child-height grid button))))

    (is (eq button2 (gtk:grid-child-at grid 2 2)))
    (let ((button (gtk:grid-child-at grid 2 2))) ; new position
      (is (= 2 (g:object-ref-count button)))
      (is (= 2 (gtk:grid-child-left-attach grid button)))
      (is (= 2 (gtk:grid-child-top-attach grid button)))
      (is (= 1 (gtk:grid-child-width grid button)))
      (is (= 2 (gtk:grid-child-height grid button))))

    (is (eq button3 (gtk:grid-child-at grid 3 0)))
    (let ((button (gtk:grid-child-at grid 3 0))) ; new position
      (is (= 2 (g:object-ref-count button)))
      (is (= 3 (gtk:grid-child-left-attach grid button)))
      (is (= 0 (gtk:grid-child-top-attach grid button)))
      (is (= 1 (gtk:grid-child-width grid button)))
      (is (= 1 (gtk:grid-child-height grid button))))

    (is (eq button4 (gtk:grid-child-at grid 0 2)))
    (let ((button (gtk:grid-child-at grid 0 2))) ; new position
      (is (= 2 (g:object-ref-count button)))
      (is (= 0 (gtk:grid-child-left-attach grid button)))
      (is (= 2 (gtk:grid-child-top-attach grid button)))
      (is (= 1 (gtk:grid-child-width grid button)))
      (is (= 1 (gtk:grid-child-height grid button))))
    ;; Remove references
    (is-false (gtk:grid-remove-row grid 3))
    (is-false (gtk:grid-remove-row grid 2))
    (is-false (gtk:grid-remove-row grid 1))
    (is-false (gtk:grid-remove-row grid 0))))

;;;     gtk_grid_get_row_baseline_position
;;;     gtk_grid_set_row_baseline_position

(test gtk-grid-row-baseline-position
  (glib-test:with-check-memory (grid button)
    (is (typep (setf grid (make-instance 'gtk:grid)) 'gtk:grid))
    (is (typep (setf button (make-instance 'gtk:button)) 'gtk:button))
    (is-false (gtk:grid-attach grid button 0 0 1 1))
    (is (eq :right (gtk:grid-row-baseline-position grid 0)))
    (is (eq :left (setf (gtk:grid-row-baseline-position grid 0) :left)))
    (is (eq :left (gtk:grid-row-baseline-position grid 0)))
    ;; Remove references
    (is-false (gtk:grid-remove-row grid 0))))

;;; 2025-06-06
