(in-package :gtk-test)

(def-suite gtk-layout :in gtk-suite)
(in-suite gtk-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLayout

(test gtk-layout-class
  ;; Check type
  (is (g:type-is-object "GtkLayout"))
  ;; Check registered name
  (is (eq 'gtk:layout
          (glib:symbol-for-gtype "GtkLayout")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLayout")
          (g:gtype (cffi:foreign-funcall "gtk_layout_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer") (g:type-parent "GtkLayout")))
  ;; Check children
  (is (equal '()
             (list-children "GtkLayout")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkScrollable")
             (list-interfaces "GtkLayout")))
  ;; Check class properties
  (is (equal '("hadjustment" "height" "hscroll-policy" "vadjustment"
               "vscroll-policy" "width")
             (list-properties "GtkLayout")))
  ;; Check style properties.
  (is (equal '()
             (list-style-properties "GtkLayout")))
  ;; Check child properties
  (is (equal '("x" "y")
             (list-child-properties "GtkLayout")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkLayout" GTK-LAYOUT
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkScrollable")
                        :TYPE-INITIALIZER "gtk_layout_get_type")
                       ((HEIGHT GTK-LAYOUT-HEIGHT "height" "guint" T T)
                        (WIDTH GTK-LAYOUT-WIDTH "width" "guint" T T)))
             (gobject:get-g-type-definition "GtkLayout"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-layout-properties
  (let ((layout (make-instance 'gtk:layout)))
    ;; height
    (is (= 100 (gtk:layout-height layout)))
    (is (= 200 (setf (gtk:layout-height layout) 200)))
    (is (= 200 (gtk:layout-height layout)))
    ;; width
    (is (= 100 (gtk:layout-width layout)))
    (is (= 200 (setf (gtk:layout-width layout) 200)))
    (is (= 200 (gtk:layout-width layout)))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-layout-child-properties
  (let ((layout (make-instance 'gtk:layout))
        (child (make-instance 'gtk:frame)))
    (is-false (gtk:container-add layout child))
    ;; x
    (is (=  0 (gtk:layout-child-x layout child)))
    (is (= 10 (setf (gtk:layout-child-x layout child) 10)))
    (is (= 10 (gtk:layout-child-x layout child)))
    ;; y
    (is (=  0 (gtk:layout-child-y layout child)))
    (is (= 20 (setf (gtk:layout-child-y layout child) 20)))
    (is (= 20 (gtk:layout-child-y layout child)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_layout_new

(test gtk-layout-new
  (let ((layout (make-instance 'gtk:layout)))
    (is (eq 'gtk:layout (type-of (gtk:layout-new))))
    (is (eq 'gtk:adjustment (type-of (gtk:scrollable-hadjustment layout))))
    (is (eq 'gtk:adjustment (type-of (gtk:scrollable-vadjustment layout)))))
  (let* ((adjustment (make-instance 'gtk:adjustment))
         (layout (gtk:layout-new adjustment adjustment)))
    (is (eq 'gtk:layout (type-of layout)))
    (is (equal adjustment (gtk:scrollable-hadjustment layout)))
    (is (equal adjustment (gtk:scrollable-vadjustment layout))))
  (let* ((adjustment (make-instance 'gtk:adjustment))
         (layout (gtk:layout-new adjustment)))
    (is (eq 'gtk:layout (type-of layout)))
    (is (equal adjustment (gtk:scrollable-hadjustment layout))))
  (let* ((adjustment (make-instance 'gtk:adjustment))
         (layout (gtk:layout-new nil adjustment)))
    (is (eq 'gtk:layout (type-of layout)))
    (is (equal adjustment (gtk:scrollable-vadjustment layout)))))

;;;     gtk_layout_put

(test gtk-layout-put
  (let ((layout (make-instance 'gtk:layout))
        (button (make-instance 'gtk:button)))
    ;; Put a button in the layout
    (is-false (gtk:layout-put layout button 10 20))
    (is (= 10 (gtk:layout-child-x layout button)))
    (is (= 20 (gtk:layout-child-y layout button)))))

;;;     gtk_layout_move

(test gtk-layout-move
  (let ((layout (make-instance 'gtk:layout))
        (button (make-instance 'gtk:button)))
    ;; Add a button to the layout
    (is-false (gtk:container-add layout button))
    (is (=  0 (gtk:layout-child-x layout button)))
    (is (=  0 (gtk:layout-child-y layout button)))
    ;; Move the button
    (is-false (gtk:layout-move layout button 10 20))
    (is (= 10 (gtk:layout-child-x layout button)))
    (is (= 20 (gtk:layout-child-y layout button)))))

;;;     gtk_layout_size

(test gtk-layout-size
  (let ((layout (make-instance 'gtk:layout)))
    (is (equal '(100 100)
               (multiple-value-list (gtk:layout-size layout))))
    (is (equal '(200 200)
               (multiple-value-list (setf (gtk:layout-size layout)
                                          '(200 200)))))
    (is (equal '(200 200) (multiple-value-list (gtk:layout-size layout))))))

;;;     gtk_layout_get_hadjustment                         deprecated
;;;     gtk_layout_get_vadjustment                         deprecated
;;;     gtk_layout_set_hadjustment                         deprecated
;;;     gtk_layout_set_vadjustment                         deprecated

;;;     gtk_layout_get_bin_window

;;; 2024-4-10
