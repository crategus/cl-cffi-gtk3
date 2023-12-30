(in-package :gtk-test)

(def-suite gtk-assistant :in gtk-suite)
(in-suite gtk-assistant)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAssistantPageType

;;;     GtkAssistant

(test gtk-assistant-class
  ;; Type check
  (is (g:type-is-object "GtkAssistant"))
  ;; Check the registered name
  (is (eq 'gtk:assistant
          (glib:symbol-for-gtype "GtkAssistant")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAssistant")
          (g:gtype (cffi:foreign-funcall "gtk_assistant_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWindow")
          (g:type-parent "GtkAssistant")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAssistant")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkAssistant")))
  ;; Check the class properties
  (is (equal '("use-header-bar")
             (list-properties "GtkAssistant")))
  ;; Get the names of the style properties
  (is (equal '("content-padding" "header-padding")
             (list-style-properties "GtkAssistant")))
  ;; Get the names of the child properties
  (is (equal '("complete" "has-padding" "header-image" "page-type"
               "sidebar-image" "title")
             (list-child-properties "GtkAssistant")))
  ;; Check the signals
  (is (equal '("apply" "cancel" "close" "escape" "prepare")
             (list-signals "GtkAssistant")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAssistant" GTK-ASSISTANT
                       (:SUPERCLASS GTK-WINDOW :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_assistant_get_type")
                       ((USE-HEADER-BAR GTK-ASSISTANT-USE-HEADER-BAR
                         "use-header-bar" "gint" T NIL)))
             (gobject:get-g-type-definition "GtkAssistant"))))

;;; --- Properties -------------------------------------------------------------

;;;     use-header-bar

(test gtk-assistant-properties
  (let ((assistant (make-instance 'gtk:assistant)))
    (is-true (gtk:assistant-use-header-bar assistant))))

;;; --- Child Properties -------------------------------------------------------

;;;     complete
;;;     has-padding
;;;     header-image                                        not exported
;;;     page-type
;;;     sidebar-image                                       not exported
;;;     title

(test gtk-assistant-child-properties
  (let ((assistant (make-instance 'gtk:assistant))
        (page (make-instance 'gtk:box)))
    (is (= 0 (gtk:assistant-append-page assistant page)))
    (is-false (gtk:assistant-child-complete assistant page))
    (is-true (gtk:assistant-child-has-padding assistant page))
    #+nil
    (is-false (gtk:assistant-child-header-image assistant page))
    (is (eq :content (gtk:assistant-child-page-type assistant page)))
    #+nil
    (is-false (gtk:assistant-child-sidebar-image assistant page))
    (is-false (gtk:assistant-child-title assistant page))))

;;; --- Style Properties -------------------------------------------------------

;;;     content-padding
;;;     header-padding

(test gtk-assistant-style-properties
  (let ((assistant (make-instance 'gtk:assistant)))
    (is (= 1 (gtk:widget-style-property assistant "content-padding")))
    (is (= 6 (gtk:widget-style-property assistant "header-padding")))))

;;; --- Signals ----------------------------------------------------------------

;;;     apply
;;;     cancel
;;;     close
;;;     escape
;;;     prepare

;;; --- Functions --------------------------------------------------------------

;;;     gtk_assistant_new

(test gtk-assistant-new
  (is (typep (gtk:assistant-new) 'gtk:assistant)))

;;;     gtk_assistant_get_current_page
;;;     gtk_assistant_set_current_page
;;;     gtk_assistant_get_n_pages
;;;     gtk_assistant_get_nth_page
;;;     gtk_assistant_prepend_page
;;;     gtk_assistant_append_page
;;;     gtk_assistant_insert_page
;;;     gtk_assistant_remove_page

;;;     GtkAssistantPageFunc
;;;     gtk_assistant_set_forward_page_func

;;;     gtk_assistant_set_page_type
;;;     gtk_assistant_get_page_type
;;;     gtk_assistant_set_page_title
;;;     gtk_assistant_get_page_title
;;;     gtk_assistant_set_page_header_image                * deprecated
;;;     gtk_assistant_get_page_header_image                * deprecated
;;;     gtk_assistant_set_page_side_image                  * deprecated
;;;     gtk_assistant_get_page_side_image                  * deprecated
;;;     gtk_assistant_set_page_complete
;;;     gtk_assistant_get_page_complete
;;;     gtk_assistant_set_page_has_padding
;;;     gtk_assistant_get_page_has_padding
;;;     gtk_assistant_add_action_widget
;;;     gtk_assistant_remove_action_widget
;;;     gtk_assistant_update_buttons_state
;;;     gtk_assistant_commit
;;;     gtk_assistant_next_page
;;;     gtk_assistant_previous_page

;;; 2023-12-30
