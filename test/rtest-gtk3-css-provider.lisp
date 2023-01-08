(in-package :gtk-test)

(def-suite gtk-css-provider :in gtk-suite)
(in-suite gtk-css-provider)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCssProvider

(test css-provider-class
  ;; Type check
  (is (g:type-is-object "GtkCssProvider"))
  ;; Check the registered name
  (is (eq 'gtk:css-provider
          (gobject:symbol-for-gtype "GtkCssProvider")))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkCssProvider")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCssProvider")))
  ;; Check the interfaces
  (is (equal '("GtkStyleProvider" "GtkStyleProviderPrivate")
             (list-interfaces "GtkCssProvider")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkCssProvider")))
  ;; Check the signals
  (is (equal '("parsing-error")
             (list-signals "GtkCssProvider")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkCssProvider" GTK-CSS-PROVIDER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkStyleProvider" "GtkStyleProviderPrivate")
                        :TYPE-INITIALIZER "gtk_css_provider_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkCssProvider"))))

;;;     GtkCssProviderError
;;;     GTK_CSS_PROVIDER_ERROR

;;;     GtkCssSectionType

(test css-section-type
  ;; Check the type
  (is-true (g:type-is-enum "GtkCssSectionType"))
  ;; Check the registered name
  (is (eql 'gtk:css-section-type
           (gobject:symbol-for-gtype "GtkCssSectionType")))
  ;; Check the names
  (is (equal '("GTK_CSS_SECTION_DOCUMENT" "GTK_CSS_SECTION_IMPORT"
               "GTK_CSS_SECTION_COLOR_DEFINITION" "GTK_CSS_SECTION_BINDING_SET"
               "GTK_CSS_SECTION_RULESET" "GTK_CSS_SECTION_SELECTOR"
               "GTK_CSS_SECTION_DECLARATION" "GTK_CSS_SECTION_VALUE"
               "GTK_CSS_SECTION_KEYFRAMES")
             (list-enum-item-name "GtkCssSectionType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8)
             (list-enum-item-value "GtkCssSectionType")))
  ;; Check the nick names
  (is (equal '("document" "import" "color-definition" "binding-set" "ruleset"
               "selector" "declaration" "value" "keyframes")
             (list-enum-item-nick "GtkCssSectionType")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkCssSectionType"
                             GTK-CSS-SECTION-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_css_section_type_get_type")
                             (:DOCUMENT 0)
                             (:IMPORT 1)
                             (:COLOR-DEFINITION 2)
                             (:BINDING-SET 3)
                             (:RULESET 4)
                             (:SELECTOR 5)
                             (:DECLARATION 6)
                             (:VALUE 7)
                             (:KEYFRAMES 8))
             (gobject::get-g-type-definition "GtkCssSectionType"))))

;;;     GtkCssSection


;;; --- Functions --------------------------------------------------------------

;;;     gtk_css_provider_get_default

(test css-provider-default
  (is (eq 'gtk:css-provider (type-of (gtk:css-provider-default)))))

;;;     gtk_css_provider_get_named
;;;     gtk_css_provider_load_from_data
;;;     gtk_css_provider_load_from_file

;;;     gtk_css_provider_load_from_path

(test css-provider-load-from-path
  (let ((provider (gtk:css-provider-new)))
    (is-true (gtk:css-provider-load-from-path provider
                 (sys-path "test/resources/rtest-gtk-css-provider.css")))
    (is-true (stringp (gtk:css-provider-to-string provider)))))

;;;     gtk_css_provider_load_from_resource
;;;     gtk_css_provider_new

;;;     gtk_css_provider_to_string

(test css-provider-to-string
  (let ((provider (gtk:css-provider-new)))
    (is (string= "" (gtk:css-provider-to-string provider)))))

;;;     gtk_css_section_get_end_line
;;;     gtk_css_section_get_end_position

;;;     gtk_css_section_get_file
;;;     gtk_css_section_get_parent
;;;     gtk_css_section_get_section_type

;;;     gtk_css_section_get_start_line
;;;     gtk_css_section_get_start_position

;;;     gtk_css_section_ref
;;;     gtk_css_section_unref

;;; 2022-12-14
