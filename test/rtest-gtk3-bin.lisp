(in-package :gtk-test)

(def-suite gtk-bin :in gtk-suite)
(in-suite gtk-bin)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBin

(test gtk-bin-class
  ;; Check type
  (is (g:type-is-object "GtkBin"))
  ;; Check registered name
  (is (eq 'gtk:bin
          (glib:symbol-for-gtype "GtkBin")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBin")
          (g:gtype (cffi:foreign-funcall "gtk_bin_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer") (g:type-parent "GtkBin")))
  ;; Check children
  (is (equal '("GtkActionBar" "GtkAlignment" "GtkButton" "GtkComboBox"
               "GtkEventBox" "GtkExpander" "GtkFlowBoxChild" "GtkFrame"
               "GtkHandleBox" "GtkListBoxRow" "GtkMenuItem" "GtkOverlay"
               "GtkPopover" "GtkRevealer" "GtkScrolledWindow" "GtkSearchBar"
               "GtkStackSidebar" "GtkToolItem" "GtkViewport" "GtkWindow")
             (glib-test:list-children "GtkBin")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkBin")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkBin")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkBin")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkBin")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkBin")))
  ;; CSS information
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkBin")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkBin" GTK:BIN
                       (:SUPERCLASS GTK:CONTAINER
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_bin_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkBin"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_bin_get_child

(test gtk-bin-child
  (let ((bin (make-instance 'gtk:frame))
        (child (make-instance 'gtk:label)))
    (is-false (gtk:container-add bin child))
    (is-true (gtk:bin-child bin))
    (is (typep (gtk:bin-child bin) 'gtk:label))))

;;; 2024-9-21
