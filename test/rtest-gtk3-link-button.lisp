(in-package :gtk-test)

(def-suite gtk-link-button :in gtk-suite)
(in-suite gtk-link-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLinkButton

(test gtk-link-button-class
  ;; Check type
  (is (g:type-is-object "GtkLinkButton"))
  ;; Check registered name
  (is (eq 'gtk:link-button
          (glib:symbol-for-gtype "GtkLinkButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLinkButton")
          (g:gtype (cffi:foreign-funcall "gtk_link_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkLinkButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkLinkButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable")
             (glib-test:list-interfaces "GtkLinkButton")))
  ;; Check class properties
  (is (equal '("uri" "visited")
             (glib-test:list-properties "GtkLinkButton")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkLinkButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkLinkButton")))
  ;; Check signals
  (is (equal '("activate-link")
             (glib-test:list-signals "GtkLinkButton")))
  ;; Check CSS information
  (is (string= "button"
               (gtk:widget-class-css-name "GtkLinkButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkLinkButton" GTK:LINK-BUTTON
                      (:SUPERCLASS GTK:BUTTON :EXPORT T :INTERFACES
                       ("AtkImplementorIface" "GtkActionable"
                        "GtkActivatable" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_link_button_get_type")
                      ((URI LINK-BUTTON-URI "uri" "gchararray" T T)
                       (VISITED LINK-BUTTON-VISITED "visited" "gboolean" T
                        T)))
             (gobject:get-gtype-definition "GtkLinkButton"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate-link

(test gtk-link-button-activate-link-signal
  (let* ((name "activate-link")
         (gtype (g:gtype "GtkLinkButton"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     uri
;;;     visited

(test gtk-link-button-properties
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:link-button-new "URI")) 'gtk:link-button))
    (is (string= "URI" (gtk:link-button-uri button)))
    (is-false (gtk:link-button-visited button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_link_button_new

(test gtk-link-button-new
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:link-button-new "URI")) 'gtk:link-button))))

;;;     gtk_link_button_new_with_label

(test gtk-link-button-new-with-label
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:link-button-new-with-label "URI" "LABEL"))
               'gtk:link-button))))

;;; 2026-06-27
