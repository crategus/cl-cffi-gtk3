(in-package :gtk-test)

(def-suite gtk-bin :in gtk-suite)
(in-suite gtk-bin)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBin

(test gtk-bin-class
  ;; Type check
  (is (g:type-is-object "GtkBin"))
  ;; Check the registered name
  (is (eq 'gtk:bin
          (glib:symbol-for-gtype "GtkBin")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkBin")
          (g:gtype (cffi:foreign-funcall "gtk_bin_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkContainer") (g:type-parent "GtkBin")))
  ;; Check the children
  (is (equal '("GtkActionBar" "GtkAlignment" "GtkButton" "GtkComboBox"
               "GtkEventBox" "GtkExpander" "GtkFlowBoxChild" "GtkFrame"
               "GtkHandleBox" "GtkListBoxRow" "GtkMenuItem" "GtkOverlay"
               "GtkPopover" "GtkRevealer" "GtkScrolledWindow" "GtkSearchBar"
               "GtkStackSidebar" "GtkToolItem" "GtkViewport" "GtkWindow")
             (list-children "GtkBin")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkBin")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkBin")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-style-properties "GtkBin")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-child-properties "GtkBin")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkBin")))
  ;; CSS information
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkBin")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkBin" GTK-BIN
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_bin_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkBin"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_bin_get_child

(test gtk-bin-child
  (let ((bin (make-instance 'gtk:frame))
        (child (make-instance 'gtk:label)))
    (is-false (gtk:container-add bin child))
    (is-true (gtk:bin-child bin))
    (is (typep (gtk:bin-child bin) 'gtk:label))))

;;; --- 2023-12-30 -------------------------------------------------------------
