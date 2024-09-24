(in-package :gtk-test)

(def-suite gtk-header-bar :in gtk-suite)
(in-suite gtk-header-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkHeaderBar

(test gtk-header-bar-class
  ;; Check type
  (is (g:type-is-object "GtkHeaderBar"))
  ;; Check registered name
  (is (eq 'gtk:header-bar
          (glib:symbol-for-gtype "GtkHeaderBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkHeaderBar")
          (g:gtype (cffi:foreign-funcall "gtk_header_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer")
          (g:type-parent "GtkHeaderBar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkHeaderBar")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkHeaderBar")))
  ;; Check class properties
  (is (equal '("custom-title" "decoration-layout" "decoration-layout-set"
               "has-subtitle" "show-close-button" "spacing" "subtitle" "title")
             (glib-test:list-properties "GtkHeaderBar")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkHeaderBar")))
  ;; Check child properties
  (is (equal '("pack-type" "position")
             (gtk-test:list-child-properties "GtkHeaderBar")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkHeaderBar")))
  ;; CSS information
  (is (string= "headerbar"
               (gtk:widget-class-css-name "GtkHeaderBar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkHeaderBar" GTK:HEADER-BAR
                       (:SUPERCLASS GTK:CONTAINER
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_header_bar_get_type")
                       ((CUSTOM-TITLE HEADER-BAR-CUSTOM-TITLE
                         "custom-title" "GtkWidget" T T)
                        (DECORATION-LAYOUT HEADER-BAR-DECORATION-LAYOUT
                         "decoration-layout" "gchararray" T T)
                        (DECORATION-LAYOUT-SET HEADER-BAR-DECORATION-LAYOUT-SET
                         "decoration-layout-set" "gboolean" T T)
                        (HAS-SUBTITLE HEADER-BAR-HAS-SUBTITLE
                         "has-subtitle" "gboolean" T T)
                        (SHOW-CLOSE-BUTTON HEADER-BAR-SHOW-CLOSE-BUTTON
                         "show-close-button" "gboolean" T T)
                        (SPACING HEADER-BAR-SPACING "spacing" "gint" T T)
                        (SUBTITLE HEADER-BAR-SUBTITLE
                         "subtitle" "gchararray" T T)
                        (TITLE HEADER-BAR-TITLE "title" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkHeaderBar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-header-bar-properties
  (let ((bar (make-instance 'gtk:header-bar)))
    (is-false (gtk:header-bar-custom-title bar))
    (is-false (gtk:header-bar-decoration-layout bar))
    (is-false (gtk:header-bar-decoration-layout-set bar))
    (is-true (gtk:header-bar-has-subtitle bar))
    (is-false (gtk:header-bar-show-close-button bar))
    (is (= 6 (gtk:header-bar-spacing bar)))
    (is-false (gtk:header-bar-subtitle bar))
    (is-false (gtk:header-bar-title bar))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-header-bar-child-properties
  (let ((bar (make-instance 'gtk:header-bar))
        (button1 (make-instance 'gtk:button))
        (button2 (make-instance 'gtk:button)))
    (is-false (gtk:header-bar-pack-start bar button1))
    (is (eq :start (gtk:header-bar-child-pack-type bar button1)))
    (is (= 0 (gtk:header-bar-child-position bar button1)))
    (is-false (gtk:header-bar-pack-end bar button2))
    (is (eq :end (gtk:header-bar-child-pack-type bar button2)))
    (is (= 1 (gtk:header-bar-child-position bar button2)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_header_bar_new

(test gtk-header-bar-new
  (is (typep (gtk:header-bar-new) 'gtk:header-bar)))

;;;     gtk_header_bar_pack_start
;;;     gtk_header_bar_pack_end

;;; 2024-9-21
