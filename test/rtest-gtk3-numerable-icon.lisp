(in-package :gtk-test)

(def-suite gtk-numerable-icon :in gtk-suite)
(in-suite gtk-numerable-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNumerableIcon

(test numerable-icon-class
  ;; Type check
  (is (g:type-is-object "GtkNumerableIcon"))
  ;; Check the registered name
  (is (eq 'gtk:numerable-icon
          (gobject:symbol-for-gtype "GtkNumerableIcon")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkNumerableIcon")
          (g:gtype (cffi:foreign-funcall "gtk_numerable_icon_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GEmblemedIcon")
          (g:type-parent "GtkNumerableIcon")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkNumerableIcon")))
  ;; Check the interfaces
  (is (equal '("GIcon")
             (list-interfaces "GtkNumerableIcon")))
  ;; Check the class properties
  (is (equal '("background-icon" "background-icon-name" "count" "label"
               "style-context")
             (list-properties "GtkNumerableIcon")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkNumerableIcon")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkNumerableIcon" GTK-NUMERABLE-ICON
                       (:SUPERCLASS G-EMBLEMED-ICON :EXPORT T :INTERFACES
                        ("GIcon") :TYPE-INITIALIZER
                        "gtk_numerable_icon_get_type")
                       ((BACKGROUND-ICON GTK-NUMERABLE-ICON-BACKGROUND-ICON
                         "background-icon" "GIcon" T T)
                        (BACKGROUND-ICON-NAME
                         GTK-NUMERABLE-ICON-BACKGROUND-ICON-NAME
                         "background-icon-name" "gchararray" T T)
                        (COUNT GTK-NUMERABLE-ICON-COUNT "count" "gint" T T)
                        (LABEL GTK-NUMERABLE-ICON-LABEL "label" "gchararray" T
                         T)
                        (STYLE-CONTEXT GTK-NUMERABLE-ICON-STYLE-CONTEXT
                         "style-context" "GtkStyleContext" T T)))
             (gobject:get-g-type-definition "GtkNumerableIcon"))))

;;; --- Properties -------------------------------------------------------------

;;;     background-icon
;;;     background-icon-name
;;;     count
;;;     label
;;;     style-context

(test numerable-icon-properties
  (let ((icon (make-instance 'gtk:numerable-icon
                             :gicon (g:themed-icon-new "gtk-ok")
                             :count 11)))
    (is-false (gtk:numerable-icon-background-icon icon))
    (is-false (gtk:numerable-icon-background-icon-name icon))
    (is (= 11 (gtk:numerable-icon-count icon)))
    (is-false (gtk:numerable-icon-label icon))
    (is-false (gtk:numerable-icon-style-context icon))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_numerable_icon_new

(test numerable-icon-new
  (is (typep (gtk:numerable-icon-new (g:themed-icon-new "gtk-ok"))
             'gtk:numerable-icon)))

;;;     gtk_numerable_icon_new_with_style_context

;; TODO: Improve the test. We get a warning:
;; (sbcl:8869): Gtk-CRITICAL **: 23:43:53.125: gtk_style_context_set_path:
;; assertion 'GTK_IS_CSS_PATH_NODE (root)' failed

#+nil
(test numerable-icon-new-with-style-context
  (let* ((widget (make-instance 'gtk:button))
         (context (gtk:widget-style-context widget))
         (icon (g:themed-icon-new "gtk-ok")))
    (is (typep (gtk:numerable-icon-new-with-style-context icon context)
               'gtk:numerable-icon))))

;;; 2022-12-21
