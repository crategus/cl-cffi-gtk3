(in-package :gtk-test)

(def-suite gtk-cell-renderer-spin :in gtk-suite)
(in-suite gtk-cell-renderer-spin)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererSpin

(test gtk-cell-renderer-spin-class
  ;; Check type
  (is (g:type-is-object "GtkCellRendererSpin"))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-spin
          (glib:symbol-for-gtype "GtkCellRendererSpin")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererSpin")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_spin_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCellRendererText")
          (g:type-parent "GtkCellRendererSpin")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCellRendererSpin")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCellRendererSpin")))
  ;; Check class properties
  (is (equal '("adjustment" "climb-rate" "digits")
             (glib-test:list-properties "GtkCellRendererSpin")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCellRendererSpin")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellRendererSpin"
                                       GTK:CELL-RENDERER-SPIN
                       (:SUPERCLASS GTK:CELL-RENDERER-TEXT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_cell_renderer_spin_get_type")
                       ((ADJUSTMENT CELL-RENDERER-SPIN-ADJUSTMENT
                         "adjustment" "GtkAdjustment" T T)
                        (CLIMB-RATE CELL-RENDERER-SPIN-CLIMB-RATE
                         "climb-rate" "gdouble" T T)
                        (DIGITS CELL-RENDERER-SPIN-DIGITS "digits" "guint" T T)))
             (gobject:get-gtype-definition "GtkCellRendererSpin"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-spin-properties
  (let ((cell (make-instance 'gtk:cell-renderer-spin)))
    (is-false (gtk:cell-renderer-spin-adjustment cell))
    (is (typep  (setf (gtk:cell-renderer-spin-adjustment cell)
                      (make-instance 'gtk:adjustment)) 'gtk:adjustment))
    (is (typep (gtk:cell-renderer-spin-adjustment cell) 'gtk:adjustment))
    (is (= 0.0 (gtk:cell-renderer-spin-climb-rate cell)))
    (is (= 0 (gtk:cell-renderer-spin-digits cell)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_spin_new

(test gtk-cell-renderer-spin-new
  (is (typep (gtk:cell-renderer-spin-new) 'gtk:cell-renderer-spin)))

;;; 2024-9-22
