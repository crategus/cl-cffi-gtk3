(in-package :gtk-test)

(def-suite gtk-box :in gtk-suite)
(in-suite gtk-box)

;; GtkPrinterOptionWidget is a child of GtkBox
#-windows
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:foreign-funcall "gtk_printer_option_widget_get_type" :size))

;;; --- Types and Values -------------------------------------------------------

(test gtk-box-class
  ;; Check type
  (is (g:type-is-object "GtkBox"))
  ;; Check registered name
  (is (eq 'gtk:box
          (glib:symbol-for-gtype "GtkBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBox")
          (g:gtype (cffi:foreign-funcall "gtk_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer")
          (g:type-parent "GtkBox")))
  ;; Check children
  #-windows
  (when *first-run-testsuite*
    (is (equal '("GtkAppChooserWidget" "GtkButtonBox" "GtkColorChooserWidget"
                 "GtkColorSelection" "GtkFileChooserButton"
                 "GtkFileChooserWidget" "GtkFontChooserWidget"
                 "GtkInfoBar" "GtkPlacesView"
                 "GtkPrinterOptionWidget" "GtkRecentChooserWidget"
                 "GtkShortcutsGroup" "GtkShortcutsSection"
                 "GtkShortcutsShortcut" "GtkStackSwitcher" "GtkStatusbar"
                 "GtkVBox")
               (glib-test:list-children "GtkBox"))))
  #+windows
  (when *first-run-testsuite*
    (is (equal '("GtkAppChooserWidget" "GtkButtonBox" "GtkColorChooserWidget"
                 "GtkColorSelection" "GtkFileChooserButton"
                 "GtkFileChooserWidget" "GtkFontChooserWidget"
                 "GtkInfoBar" "GtkRecentChooserWidget"
                 "GtkShortcutsGroup" "GtkShortcutsSection"
                 "GtkShortcutsShortcut" "GtkStackSwitcher" "GtkStatusbar"
                 "GtkVBox")
               (glib-test:list-children "GtkBox"))))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkBox")))
  ;; Check class properties
  (is (equal '("baseline-position" "homogeneous" "orientation" "spacing")
             (glib-test:list-properties "GtkBox")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkBox")))
  ;; Check child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (gtk-test:list-child-properties "GtkBox")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkBox")))
  ;; Check CSS information
  (is (string= "box"
               (gtk:widget-class-css-name "GtkBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkBox" GTK:BOX
                      (:SUPERCLASS GTK:CONTAINER
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_box_get_type")
                      ((BASELINE-POSITION BOX-BASELINE-POSITION
                        "baseline-position" "GtkBaselinePosition" T T)
                       (HOMOGENEOUS BOX-HOMOGENEOUS
                        "homogeneous" "gboolean" T T)
                       (SPACING BOX-SPACING "spacing" "gint" T T)))
             (gobject:get-gtype-definition "GtkBox"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-box-properties
  (glib-test:with-check-memory (box)
    (setf box (make-instance 'gtk:box :orientation :vertical :spacing 12))
    (is (eq :vertical (gtk:orientable-orientation box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 12 (gtk:box-spacing box)))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-box-child-properties
  (glib-test:with-check-memory (box button)
    (setf box (make-instance 'gtk:box :orientation :vertical))
    (setf button (make-instance 'gtk:button))
    (is-false (gtk:container-add box button))
    (is-false (gtk:box-child-expand box button))
    (is-true (gtk:box-child-fill box button))
    (is (eq :start (gtk:box-child-pack-type box button)))
    (is (= 0 (gtk:box-child-padding box button)))
    (is (= 0 (gtk:box-child-position box button)))
    ;; Remove references
    (is-false (gtk:container-remove box button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_box_new

(test gtk-box-new.1
  (glib-test:with-check-memory (box)
    (setf box (gtk:box-new :vertical 12))
    (is (eq :vertical (gtk:orientable-orientation box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 12 (gtk:box-spacing box)))))

(test gtk-box-new.2
  (glib-test:with-check-memory (box)
    (setf box (gtk:box-new :horizontal))
    (is (eq :horizontal (gtk:orientable-orientation box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 0 (gtk:box-spacing box)))))

(test gtk-box-new.3
  (glib-test:with-check-memory (box)
    (setf box (make-instance 'gtk:box))
    (is (eq :horizontal (gtk:orientable-orientation box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 0 (gtk:box-spacing box)))))

(test gtk-box-new.4
  (glib-test:with-check-memory (box)
    (setf box (make-instance 'gtk:box))
    (is (eq :horizontal (gtk:orientable-orientation box)))
    (is (eq :center (gtk:box-baseline-position box)))
    (is-false (gtk:box-homogeneous box))
    (is (= 0 (gtk:box-spacing box)))))


(test gtk-box-new.5
  (glib-test:with-check-memory (box)
    (setf box (make-instance 'gtk:box
                             :orientation :vertical
                             :baseline-position :top
                             :homogeneous t
                             :spacing 12))
    (is (eq :vertical (gtk:orientable-orientation box)))
    (is (eq :top (gtk:box-baseline-position box)))
    (is-true (gtk:box-homogeneous box))
    (is (= 12 (gtk:box-spacing box)))))

;;;     gtk_box_pack_start

(test gtk-box-pack-start
  (glib-test:with-check-memory (box button1 button2 button3)
    (setf box (make-instance 'gtk:box :orientation :vertical))
    (setf button1 (make-instance 'gtk:button))
    (setf button2 (make-instance 'gtk:button))
    (setf button3 (make-instance 'gtk:button))
    ;; Pack first button
    (is-false (gtk:box-pack-start box button1))
    (is (= 0 (gtk:box-child-position box button1)))
    ;; Pack second button
    (is-false (gtk:box-pack-start box button2))
    (is (= 0 (gtk:box-child-position box button1)))
    (is (= 1 (gtk:box-child-position box button2)))
    ;; Pack third button
    (is-false (gtk:box-pack-start box button3))
    (is (= 0 (gtk:box-child-position box button1)))
    (is (= 1 (gtk:box-child-position box button2)))
    (is (= 2 (gtk:box-child-position box button3)))
    ;; Check the pack type
    (is (eq :start (gtk:box-child-pack-type box button1)))
    (is (eq :start (gtk:box-child-pack-type box button2)))
    (is (eq :start (gtk:box-child-pack-type box button3)))
    ;; Remove references
    (is-false (gtk:container-remove box button1))
    (is-false (gtk:container-remove box button2))
    (is-false (gtk:container-remove box button3))))

;;;     gtk_box_pack_end

(test gtk-box-pack-end
  (glib-test:with-check-memory (box button1 button2 button3)
    (setf box (make-instance 'gtk:box :orientation :vertical))
    (setf button1 (make-instance 'gtk:button))
    (setf button2 (make-instance 'gtk:button))
    (setf button3 (make-instance 'gtk:button))
    ;; Pack first button
    (is-false (gtk:box-pack-end box button1))
    (is (= 0 (gtk:box-child-position box button1)))
    ;; Pack second button
    (is-false (gtk:box-pack-end box button2))
    (is (= 0 (gtk:box-child-position box button1)))
    (is (= 1 (gtk:box-child-position box button2)))
    ;; Pack third button
    (is-false (gtk:box-pack-end box button3))
    (is (= 0 (gtk:box-child-position box button1)))
    (is (= 1 (gtk:box-child-position box button2)))
    (is (= 2 (gtk:box-child-position box button3)))
    ;; Check the pack type
    (is (eq :end (gtk:box-child-pack-type box button1)))
    (is (eq :end (gtk:box-child-pack-type box button2)))
    (is (eq :end (gtk:box-child-pack-type box button3)))
    ;; Remove references
    (is-false (gtk:container-remove box button1))
    (is-false (gtk:container-remove box button2))
    (is-false (gtk:container-remove box button3))))

;;;     gtk_box_reorder_child

(test gtk-box-reorder-child
  (glib-test:with-check-memory (box label button image)
    (setf box (make-instance 'gtk:box :orientation :vertical))
    (setf label (make-instance 'gtk:label))
    (setf button (make-instance 'gtk:button))
    (setf image (make-instance 'gtk:image))
    ;; Pack three widgets in the box
    (is-false (gtk:box-pack-start box label))
    (is-false (gtk:box-pack-start box button))
    (is-false (gtk:box-pack-start box image))
    ;; Check the position of the children
    (is (= 0 (gtk:box-child-position box label)))
    (is (= 1 (gtk:box-child-position box button)))
    (is (= 2 (gtk:box-child-position box image)))
    ;; Reorder the children
    (gtk:box-reorder-child box label 1)
    ;; Check again the position of the children
    (is (= 1 (gtk:box-child-position box label)))
    (is (= 0 (gtk:box-child-position box button)))
    (is (= 2 (gtk:box-child-position box image)))
    ;; Reorder the children
    (gtk:box-reorder-child box label 2)
    ;; Check again the position of the children
    (is (= 2 (gtk:box-child-position box label)))
    (is (= 0 (gtk:box-child-position box button)))
    (is (= 1 (gtk:box-child-position box image)))
    ;; Remove references
    (is-false (gtk:container-remove box label))
    (is-false (gtk:container-remove box button))
    (is-false (gtk:container-remove box image))))

;;;     gtk_box_query_child_packing
;;;     gtk_box_child_packing

(test gtk-box-child-packing
  (glib-test:with-check-memory (box button)
    (setf box (make-instance 'gtk:box))
    (setf button (make-instance 'gtk:button))
    ;; Pack a button in the box
    (is-false (gtk:container-add box button))
    ;; Query and check the child properties
    (multiple-value-bind (expand fill padding pack-type)
        (gtk:box-query-child-packing box button)
      (is-false expand)
      (is-true fill)
      (is (= 0 padding))
      (is (eq :start pack-type)))
    ;; Set new child properties
    (is (eq :end (gtk:box-child-packing box button t nil 10 :end)))
    ;; Query and check the child properties
    (multiple-value-bind (expand fill padding pack-type)
        (gtk:box-query-child-packing box button)
      (is-true expand)
      (is-false fill)
      (is (= 10 padding))
      (is (eq :end pack-type)))
    ;; Remove references
    (is-false (gtk:container-remove box button))))

;;;     gtk_box_center_widget

(test gtk-box-center-widget
  (glib-test:with-check-memory (box button)
    (setf box (make-instance 'gtk:box :orientation :vertical))
    ;; No center widget set
    (is-false (gtk:box-center-widget box))
    ;; Set a center widget
    (is (eq 'gtk:button
            (type-of (setf (gtk:box-center-widget box)
                           (setf button (make-instance 'gtk:button))))))
    ;; Retrieve the center widget
    (is (eq 'gtk:button (type-of (gtk:box-center-widget box))))
    ;; Remove references
    (is-false (setf (gtk:box-center-widget box) nil))))

;;; 2025-04-26
