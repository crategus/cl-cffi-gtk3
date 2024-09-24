(in-package :gtk-test)

(def-suite gtk-css-provider :in gtk-suite)
(in-suite gtk-css-provider)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCssProvider

(test gtk-css-provider-class
  ;; Check type
  (is (g:type-is-object "GtkCssProvider"))
  ;; Check registered name
  (is (eq 'gtk:css-provider
          (glib:symbol-for-gtype "GtkCssProvider")))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkCssProvider")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCssProvider")))
  ;; Check interfaces
  (is (equal '("GtkStyleProvider" "GtkStyleProviderPrivate")
             (glib-test:list-interfaces "GtkCssProvider")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkCssProvider")))
  ;; Check signals
  (is (equal '("parsing-error")
             (glib-test:list-signals "GtkCssProvider")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCssProvider" GTK:CSS-PROVIDER
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES
                        ("GtkStyleProvider" "GtkStyleProviderPrivate")
                        :TYPE-INITIALIZER "gtk_css_provider_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkCssProvider"))))

;;;     GtkCssProviderError
;;;     GTK_CSS_PROVIDER_ERROR

;;;     GtkCssSectionType

(test gtk-css-section-type
  ;; Check type
  (is-true (g:type-is-enum "GtkCssSectionType"))
  ;; Check registered name
  (is (eql 'gtk:css-section-type
           (glib:symbol-for-gtype "GtkCssSectionType")))
  ;; Check names
  (is (equal '("GTK_CSS_SECTION_DOCUMENT" "GTK_CSS_SECTION_IMPORT"
               "GTK_CSS_SECTION_COLOR_DEFINITION" "GTK_CSS_SECTION_BINDING_SET"
               "GTK_CSS_SECTION_RULESET" "GTK_CSS_SECTION_SELECTOR"
               "GTK_CSS_SECTION_DECLARATION" "GTK_CSS_SECTION_VALUE"
               "GTK_CSS_SECTION_KEYFRAMES")
             (glib-test:list-enum-item-names "GtkCssSectionType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8)
             (glib-test:list-enum-item-values "GtkCssSectionType")))
  ;; Check nick names
  (is (equal '("document" "import" "color-definition" "binding-set" "ruleset"
               "selector" "declaration" "value" "keyframes")
             (glib-test:list-enum-item-nicks "GtkCssSectionType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkCssSectionType" GTK:CSS-SECTION-TYPE
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
             (gobject:get-gtype-definition "GtkCssSectionType"))))

;;;     GtkCssSection

(test gtk-css-section-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkCssSection"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCssSection")
          (g:gtype (cffi:foreign-funcall "gtk_css_section_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:css-section
          (glib:symbol-for-gtype "GtkCssSection"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_css_provider_get_default

(test gtk-css-provider-default
  (is (eq 'gtk:css-provider (type-of (gtk:css-provider-default)))))

;;;     gtk_css_provider_get_named

(test gtk-css-provider-named
  (is (typep (gtk:css-provider-named "Yaru" "dark") 'gtk:css-provider)))

;;;     gtk_css_provider_load_from_data

(test gtk-css-provider-load-from-data
  (let ((provider (gtk:css-provider-new)))
    (is-true (gtk:css-provider-load-from-data provider
                                              "button {
                                                 padding: 3px; }
                                               button > label {
                                                 color: black;
                                                 background-color: yellow; }
                                               button:first-child > label {
                                                 background-color: red; }
                                               button:last-child > label {
                                                 background-color : green; }"))
    (is (string=
"button {
  padding-bottom: 3px;
  padding-left: 3px;
  padding-right: 3px;
  padding-top: 3px;
}

button > label {
  background-color: rgb(255,255,0);
  color: rgb(0,0,0);
}

button:first-child > label {
  background-color: rgb(255,0,0);
}

button:last-child > label {
  background-color: rgb(0,128,0);
}
"
     (gtk:css-provider-to-string provider)))))

;;;     gtk_css_provider_load_from_file

;;;     gtk_css_provider_load_from_path

(test gtk-css-provider-load-from-path
  (let ((path (glib-sys:sys-path "test/resource/rtest-gtk-css-provider.css"))
        (provider (gtk:css-provider-new)))
    (is-true (gtk:css-provider-load-from-path provider path))
    (is-true (stringp (gtk:css-provider-to-string provider)))))

;;;     gtk_css_provider_load_from_resource

;;;     gtk_css_provider_new

(test gtk-css-provider-new
  (is (typep (gtk:css-provider-new) 'gtk:css-provider)))

;;;     gtk_css_provider_to_string

(test gtk-css-provider-to-string
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

;;; 2024-9-21
