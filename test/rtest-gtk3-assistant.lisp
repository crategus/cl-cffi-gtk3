(in-package :gtk-test)

(def-suite gtk-assistant :in gtk-suite)
(in-suite gtk-assistant)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAssistantPageType

(test gtk-assistant-page-type
  ;; Check type
  (is (g:type-is-enum "GtkAssistantPageType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAssistantPageType")
          (g:gtype (cffi:foreign-funcall "gtk_assistant_page_type_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:assistant-page-type
          (glib:symbol-for-gtype "GtkAssistantPageType")))
  ;; Check names
  (is (equal '("GTK_ASSISTANT_PAGE_CONTENT" "GTK_ASSISTANT_PAGE_INTRO"
               "GTK_ASSISTANT_PAGE_CONFIRM" "GTK_ASSISTANT_PAGE_SUMMARY"
               "GTK_ASSISTANT_PAGE_PROGRESS" "GTK_ASSISTANT_PAGE_CUSTOM")
             (glib-test:list-enum-item-names "GtkAssistantPageType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (glib-test:list-enum-item-values "GtkAssistantPageType")))
  ;; Check nick names
  (is (equal '("content" "intro" "confirm" "summary" "progress" "custom")
             (glib-test:list-enum-item-nicks "GtkAssistantPageType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkAssistantPageType"
                                    GTK:ASSISTANT-PAGE-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_assistant_page_type_get_type")
                                    (:CONTENT 0)
                                    (:INTRO 1)
                                    (:CONFIRM 2)
                                    (:SUMMARY 3)
                                    (:PROGRESS 4)
                                    (:CUSTOM 5))
             (gobject:get-gtype-definition "GtkAssistantPageType"))))

;;;     GtkAssistant

(test gtk-assistant-class
  ;; Check type
  (is (g:type-is-object "GtkAssistant"))
  ;; Check registered name
  (is (eq 'gtk:assistant
          (glib:symbol-for-gtype "GtkAssistant")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAssistant")
          (g:gtype (cffi:foreign-funcall "gtk_assistant_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWindow")
          (g:type-parent "GtkAssistant")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAssistant")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkAssistant")))
  ;; Check class properties
  (is (equal '("use-header-bar")
             (glib-test:list-properties "GtkAssistant")))
  ;; Check style properties
  (is (equal '("content-padding" "header-padding")
             (gtk-test:list-style-properties "GtkAssistant")))
  ;; Check child properties
  (is (equal '("complete" "has-padding" "header-image" "page-type"
               "sidebar-image" "title")
             (gtk-test:list-child-properties "GtkAssistant")))
  ;; Check signals
  (is (equal '("apply" "cancel" "close" "escape" "prepare")
             (glib-test:list-signals "GtkAssistant")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAssistant" GTK:ASSISTANT
                      (:SUPERCLASS GTK:WINDOW
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_assistant_get_type")
                      ((USE-HEADER-BAR ASSISTANT-USE-HEADER-BAR
                        "use-header-bar" "gint" T NIL)))
             (gobject:get-gtype-definition "GtkAssistant"))))

;;; --- Properties -------------------------------------------------------------

;;;     use-header-bar

(test gtk-assistant-properties
  (glib-test:with-check-memory (assistant)
    (is (typep (setf assistant (make-instance 'gtk:assistant)) 'gtk:assistant))
    (is-true (gtk:assistant-use-header-bar assistant))
    (is-false (gtk:widget-destroy assistant))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-assistant-child-properties
  (glib-test:with-check-memory (assistant page)
    (is (typep (setf assistant (make-instance 'gtk:assistant)) 'gtk:assistant))
    (is (typep (setf page (make-instance 'gtk:box)) 'gtk:box))
    (is (= 0 (gtk:assistant-append-page assistant page)))
    (is-false (gtk:assistant-child-complete assistant page))
    (is-true (gtk:assistant-child-has-padding assistant page))
    #+nil
    (is-false (gtk:assistant-child-header-image assistant page))
    (is (eq :content (gtk:assistant-child-page-type assistant page)))
    #+nil
    (is-false (gtk:assistant-child-sidebar-image assistant page))
    (is-false (gtk:assistant-child-title assistant page))
    (is-false (gtk:widget-destroy assistant))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-assistant-style-properties
  (glib-test:with-check-memory (assistant)
    (is (typep (setf assistant (make-instance 'gtk:assistant)) 'gtk:assistant))
    (is (= 1 (gtk:widget-style-property assistant "content-padding")))
    (is (= 6 (gtk:widget-style-property assistant "header-padding")))
    (is-false (gtk:widget-destroy assistant))))

;;; --- Signals ----------------------------------------------------------------

;;;     apply
;;;     cancel
;;;     close
;;;     escape
;;;     prepare

;;; --- Functions --------------------------------------------------------------

;;;     gtk_assistant_new

(test gtk-assistant-new
  (glib-test:with-check-memory (assistant)
    (is (typep (setf assistant (gtk:assistant-new)) 'gtk:assistant))
    (is-false (gtk:widget-destroy assistant))))

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
;;;     gtk_assistant_set_page_header_image                 Deprecated 3.2
;;;     gtk_assistant_get_page_header_image                 Deprecated 3.2
;;;     gtk_assistant_set_page_side_image                   Deprecated 3.2
;;;     gtk_assistant_get_page_side_image                   Deprecated 3.2
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

;;; 2025-06-06
