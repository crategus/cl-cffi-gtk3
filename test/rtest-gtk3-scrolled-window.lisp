(in-package :gtk-test)

(def-suite gtk-scrolled-window :in gtk-suite)
(in-suite gtk-scrolled-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPolicyType

(test gtk-policy-type
  ;; Check type
  (is (g:type-is-enum "GtkPolicyType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPolicyType")
          (g:gtype (cffi:foreign-funcall "gtk_policy_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:policy-type
          (glib:symbol-for-gtype "GtkPolicyType")))
  ;; Check names
  (is (equal '("GTK_POLICY_ALWAYS" "GTK_POLICY_AUTOMATIC" "GTK_POLICY_NEVER"
               "GTK_POLICY_EXTERNAL")
             (glib-test:list-enum-item-names "GtkPolicyType")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkPolicyType")))
  ;; Check nick names
  (is (equal '("always" "automatic" "never" "external")
             (glib-test:list-enum-item-nicks "GtkPolicyType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPolicyType" GTK:POLICY-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_policy_type_get_type")
                       (:ALWAYS 0)
                       (:AUTOMATIC 1)
                       (:NEVER 2)
                       (:EXTERNAL 3))
             (gobject:get-gtype-definition "GtkPolicyType"))))

;;;     GtkCornerType

(test gtk-corner-type
  ;; Check type
  (is (g:type-is-enum "GtkCornerType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCornerType")
          (g:gtype (cffi:foreign-funcall "gtk_corner_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:policy-type
          (glib:symbol-for-gtype "GtkPolicyType")))
  ;; Check names
  (is (equal '("GTK_CORNER_TOP_LEFT" "GTK_CORNER_BOTTOM_LEFT"
               "GTK_CORNER_TOP_RIGHT" "GTK_CORNER_BOTTOM_RIGHT")
             (glib-test:list-enum-item-names "GtkCornerType")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkCornerType")))
  ;; Check nick names
  (is (equal '("top-left" "bottom-left" "top-right" "bottom-right")
             (glib-test:list-enum-item-nicks "GtkCornerType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkCornerType" GTK:CORNER-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_corner_type_get_type")
                       (:TOP-LEFT 0)
                       (:BOTTOM-LEFT 1)
                       (:TOP-RIGHT 2)
                       (:BOTTOM-RIGHT 3))
             (gobject:get-gtype-definition "GtkCornerType"))))

;;;     GtkScrolledWindow

(test gtk-scrolled-window-class
  ;; Check type
  (is (g:type-is-object "GtkScrolledWindow"))
  ;; Check registered name
  (is (eq 'gtk:scrolled-window
          (glib:symbol-for-gtype "GtkScrolledWindow")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkScrolledWindow")
          (g:gtype (cffi:foreign-funcall "gtk_scrolled_window_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkScrolledWindow")))
  ;; Check children
  (is (equal '("GtkPlacesSidebar")
             (glib-test:list-children "GtkScrolledWindow")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkScrolledWindow")))
  ;; Check class properties
  (is (equal '("hadjustment" "hscrollbar-policy" "kinetic-scrolling"
               "max-content-height" "max-content-width" "min-content-height"
               "min-content-width" "overlay-scrolling"
               "propagate-natural-height" "propagate-natural-width"
               "shadow-type" "vadjustment" "vscrollbar-policy"
               "window-placement" "window-placement-set")
             (glib-test:list-properties "GtkScrolledWindow")))
  ;; Check style properties
  (is (equal '("scrollbar-spacing" "scrollbars-within-bevel")
             (gtk-test:list-style-properties "GtkScrolledWindow")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkScrolledWindow")))
  ;; Check signals
  (is (equal '("edge-overshot" "edge-reached" "move-focus-out" "scroll-child")
             (glib-test:list-signals "GtkScrolledWindow")))
  ;; CSS information
  (is (string= "scrolledwindow"
               (gtk:widget-class-css-name "GtkScrolledWindow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkScrolledWindow" GTK:SCROLLED-WINDOW
                       (:SUPERCLASS GTK:BIN
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_scrolled_window_get_type")
                       ((HADJUSTMENT SCROLLED-WINDOW-HADJUSTMENT
                         "hadjustment" "GtkAdjustment" T T)
                        (HSCROLLBAR-POLICY SCROLLED-WINDOW-HSCROLLBAR-POLICY
                         "hscrollbar-policy" "GtkPolicyType" T T)
                        (KINETIC-SCROLLING SCROLLED-WINDOW-KINETIC-SCROLLING
                         "kinetic-scrolling" "gboolean" T T)
                        (MAX-CONTENT-HEIGHT SCROLLED-WINDOW-MAX-CONTENT-HEIGHT
                         "max-content-height" "gint" T T)
                        (MAX-CONTENT-WIDTH SCROLLED-WINDOW-MAX-CONTENT-WIDTH
                         "max-content-width" "gint" T T)
                        (MIN-CONTENT-HEIGHT SCROLLED-WINDOW-MIN-CONTENT-HEIGHT
                         "min-content-height" "gint" T T)
                        (MIN-CONTENT-WIDTH SCROLLED-WINDOW-MIN-CONTENT-WIDTH
                         "min-content-width" "gint" T T)
                        (OVERLAY-SCROLLING SCROLLED-WINDOW-OVERLAY-SCROLLING
                         "overlay-scrolling" "gboolean" T T)
                        (PROPAGATE-NATURAL-HEIGHT
                         SCROLLED-WINDOW-PROPAGATE-NATURAL-HEIGHT
                         "propagate-natural-height" "gboolean" T T)
                        (PROPAGATE-NATURAL-WIDTH
                         SCROLLED-WINDOW-PROPAGATE-NATURAL-WIDTH
                         "propagate-natural-width" "gboolean" T T)
                        (SHADOW-TYPE SCROLLED-WINDOW-SHADOW-TYPE
                         "shadow-type" "GtkShadowType" T T)
                        (VADJUSTMENT SCROLLED-WINDOW-VADJUSTMENT
                         "vadjustment" "GtkAdjustment" T T)
                        (VSCROLLBAR-POLICY SCROLLED-WINDOW-VSCROLLBAR-POLICY
                         "vscrollbar-policy" "GtkPolicyType" T T)
                        (WINDOW-PLACEMENT SCROLLED-WINDOW-WINDOW-PLACEMENT
                         "window-placement" "GtkCornerType" T T)
                        (WINDOW-PLACEMENT-SET
                         SCROLLED-WINDOW-WINDOW-PLACEMENT-SET
                         "window-placement-set" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkScrolledWindow"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-scrolled-window-properties
  (let ((window (make-instance 'gtk:scrolled-window)))
    (is (eq 'gtk:adjustment (type-of (gtk:scrolled-window-hadjustment window))))
    (is (eq :automatic (gtk:scrolled-window-hscrollbar-policy window)))
    (is-true (gtk:scrolled-window-kinetic-scrolling window))
    (is (= -1 (gtk:scrolled-window-max-content-height window)))
    (is (= -1 (gtk:scrolled-window-max-content-width window)))
    (is (= -1 (gtk:scrolled-window-min-content-height window)))
    (is (= -1 (gtk:scrolled-window-min-content-width window)))
    (is-true (gtk:scrolled-window-overlay-scrolling window))
    (is-false (gtk:scrolled-window-propagate-natural-height window))
    (is-false (gtk:scrolled-window-propagate-natural-width window))
    (is (eq :none (gtk:scrolled-window-shadow-type window)))
    (is (eq 'gtk:adjustment (type-of (gtk:scrolled-window-vadjustment window))))
    (is (eq :automatic (gtk:scrolled-window-vscrollbar-policy window)))
    (is (eq :top-left (gtk:scrolled-window-window-placement window)))
    (is-true (gtk:scrolled-window-window-placement-set window))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-scrolled-window-style-properties
  (let ((window (make-instance 'gtk:scrolled-window)))
    (is (= 0 (gtk:widget-style-property window "scrollbar-spacing")))
    (is-false (gtk:widget-style-property window "scrollbar-within-bevel"))))

;;; --- Signals ----------------------------------------------------------------

;;;              void    edge-overshot               Run Last
;;;              void    edge-reached                Run Last
;;;              void    move-focus-out              Action
;;;          gboolean    scroll-child                Action

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scrolled_window_new

(test gtk-scrolled-window-new
  (is (eq 'gtk:scrolled-window (type-of (gtk:scrolled-window-new))))
  (is (eq 'gtk:scrolled-window
          (type-of (gtk:scrolled-window-new (make-instance 'gtk:adjustment)))))
  (is (eq 'gtk:scrolled-window
          (type-of (gtk:scrolled-window-new nil
                                            (make-instance 'gtk:adjustment)))))
  (is (eq 'gtk:scrolled-window
          (type-of (gtk:scrolled-window-new (make-instance 'gtk:adjustment)
                                            (make-instance 'gtk:adjustment))))))

;;;     gtk_scrolled_window_get_hscrollbar
;;;     gtk_scrolled_window_get_vscrollbar

(test gtk-scrolled-window-scrollbar
  (let ((window (make-instance 'gtk:scrolled-window)))
    (is (eq 'gtk:scrollbar (type-of (gtk:scrolled-window-hscrollbar window))))
    (is (eq 'gtk:scrollbar (type-of (gtk:scrolled-window-vscrollbar window))))))

;;;     gtk_scrolled_window_get_policy
;;;     gtk_scrolled_window_set_policy

(test gtk-scrolled-window-policy
  (let ((window (make-instance 'gtk:scrolled-window)))
    (is (equal (list :automatic :automatic)
               (multiple-value-list (gtk:scrolled-window-policy window))))
    (is (equal (list :never :external)
               (multiple-value-list
                   (setf (gtk:scrolled-window-policy window) '(:never :external)))))
    (is (equal (list :never :external)
               (multiple-value-list (gtk:scrolled-window-policy window))))))

;;;     gtk_scrolled_window_add_with_viewport

(test gtk-scrolled-window-add-with-viewport
  (let ((window (make-instance 'gtk:scrolled-window))
        (button (make-instance 'gtk:button)))
    ;; Use the deprecated function gtk:scrolled-window-add-with-viewport
    (is-false (gtk:scrolled-window-add-with-viewport window button))
    (is (eq 'gtk:viewport (type-of (gtk:bin-child window))))
    (is (eq 'gtk:button (type-of (gtk:bin-child (gtk:bin-child window))))))
  (let ((window (make-instance 'gtk:scrolled-window))
        (button (make-instance 'gtk:button)))
    ;; Use the function gtk:container-add
    (is-false (gtk:container-add window button))
    (is (eq 'gtk:viewport (type-of (gtk:bin-child window))))
    (is (eq 'gtk:button (type-of (gtk:bin-child (gtk:bin-child window)))))))

;;;     gtk_scrolled_window_get_placement
;;;     gtk_scrolled_window_set_placement

(test gtk-scrolled-window-placement
  (let ((window (make-instance 'gtk:scrolled-window)))
    (is (eq :top-left (gtk:scrolled-window-placement window)))
    (is (eq :top-right (setf (gtk:scrolled-window-placement window) :top-right)))
    (is (eq :top-right (gtk:scrolled-window-placement window)))))

;;;     gtk_scrolled_window_unset_placement

(test gtk-scrolled-window-unset-placement
  (let ((window (make-instance 'gtk:scrolled-window)))
    (is (eq :top-right (setf (gtk:scrolled-window-placement window) :top-right)))
    (is (eq :top-right (gtk:scrolled-window-placement window)))
    (is-false (gtk:scrolled-window-unset-placement window))
    (is (eq :top-left (gtk:scrolled-window-placement window)))))

;;;     gtk_scrolled_window_get_capture_button_press
;;;     gtk_scrolled_window_set_capture_button_press

(test gtk-scrolled-window-capture-button-press
  (let ((window (make-instance 'gtk:scrolled-window)))
    (is-true (gtk:scrolled-window-capture-button-press window))
    (is-false (setf (gtk:scrolled-window-capture-button-press window) nil))
    (is-false (gtk:scrolled-window-capture-button-press window))))

;;; 2024-9-21
