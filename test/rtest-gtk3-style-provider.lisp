(in-package :gtk-test)

(def-suite gtk-style-provider :in gtk-suite)
(in-suite gtk-style-provider)

;;; --- Types and Values -------------------------------------------------------

;;;     GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
;;;     GTK_STYLE_PROVIDER_PRIORITY_THEME
;;;     GTK_STYLE_PROVIDER_PRIORITY_SETTINGS
;;;     GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
;;;     GTK_STYLE_PROVIDER_PRIORITY_USER

(test gtk-priority-values
  (is (=   1 +gtk-priority-fallback+))
  (is (= 200 +gtk-priority-theme+))
  (is (= 400 +gtk-priority-settings+))
  (is (= 600 +gtk-priority-application+))
  (is (= 800 +gtk-priority-user+)))

;;;     GtkStyleProvider

(test style-provider-interface
  ;; Type check
  (is-true (g:type-is-interface "GtkStyleProvider"))
  ;; Check the registered name
  (is (eq 'gtk:style-provider
          (gobject:symbol-for-gtype "GtkStyleProvider")))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GtkStyleProvider")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkStyleProvider"
                                  GTK-STYLE-PROVIDER
                                  (:EXPORT T
                                   :TYPE-INITIALIZER
                                   "gtk_style_provider_get_type"))
             (gobject:get-g-type-definition "GtkStyleProvider"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_style_provider_get_icon_factory                missing / deprecated
;;;     gtk_style_provider_get_style                       missing / ddprectaed

;;;     gtk-style-provider-style-property

(test style-provider-style-property
  (let* ((widget (make-instance 'gtk:button))
         (path (gtk:widget-path widget))
         (pspec (gtk:widget-class-find-style-property "GtkButton"
                                                      "focus-padding"))
         (provider (gtk:css-provider-new)))
    ;; Check the arguments
    (is (eq 'gtk:widget-path (type-of path)))
    (is (g:is-param-spec pspec))
    (is (eq 'gtk:css-provider (type-of provider)))
    ;; The default value of the focus-padding style property is 0.
    (is (= 0 (gtk:style-provider-style-property provider path :normal pspec)))))

;;; 2022-12-10
