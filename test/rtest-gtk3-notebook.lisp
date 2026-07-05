(in-package :gtk-test)

(def-suite gtk-notebook :in gtk-suite)
(in-suite gtk-notebook)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNotebookTab

(test gtk-notebook-tab
  ;; Check type
  (is (g:type-is-enum "GtkNotebookTab"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNotebookTab")
          (g:gtype (cffi:foreign-funcall "gtk_notebook_tab_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:notebook-tab
          (glib:symbol-for-gtype "GtkNotebookTab")))
  ;; Check names
  (is (equal '("GTK_NOTEBOOK_TAB_FIRST" "GTK_NOTEBOOK_TAB_LAST")
             (glib-test:list-enum-item-names "GtkNotebookTab")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkNotebookTab")))
  ;; Check nick names
  (is (equal '("first" "last")
             (glib-test:list-enum-item-nicks "GtkNotebookTab")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkNotebookTab" GTK:NOTEBOOK-TAB
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_notebook_tab_get_type")
                                    (:FIRST 0)
                                    (:LAST 1))
             (gobject:get-gtype-definition "GtkNotebookTab"))))

;;;     GtkNotebook

(test gtk-notebook-class
  ;; Check type
  (is (g:type-is-object "GtkNotebook"))
  ;; Check registered name
  (is (eq 'gtk:notebook
          (glib:symbol-for-gtype "GtkNotebook")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNotebook")
          (g:gtype (cffi:foreign-funcall "gtk_notebook_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer") (g:type-parent "GtkNotebook")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkNotebook")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkNotebook")))
  ;; Check class properties
  (is (equal '("enable-popup" "group-name" "page" "scrollable" "show-border"
               "show-tabs" "tab-pos")
             (glib-test:list-properties "GtkNotebook")))
  ;; Check style properties
  (is (equal '("arrow-spacing" "has-backward-stepper" "has-forward-stepper"
               "has-secondary-backward-stepper" "has-secondary-forward-stepper"
               "has-tab-gap" "initial-gap" "tab-curvature" "tab-overlap")
             (gtk-test:list-style-properties "GtkNotebook")))
  ;; Check child properties
  (is (equal '("detachable" "menu-label" "position" "reorderable" "tab-expand"
               "tab-fill" "tab-label")
             (gtk-test:list-child-properties "GtkNotebook")))
  ;; Check signals
  (is (equal '("change-current-page" "create-window" "focus-tab"
               "move-focus-out" "page-added" "page-removed" "page-reordered"
               "reorder-tab" "select-page" "switch-page")
             (glib-test:list-signals "GtkNotebook")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkNotebook" GTK:NOTEBOOK
                      (:SUPERCLASS GTK:CONTAINER
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_notebook_get_type")
                      ((ENABLE-POPUP NOTEBOOK-ENABLE-POPUP
                        "enable-popup" "gboolean" T T)
                       (GROUP-NAME NOTEBOOK-GROUP-NAME
                        "group-name" "gchararray" T T)
                       (PAGE NOTEBOOK-PAGE "page" "gint" T T)
                       (SCROLLABLE NOTEBOOK-SCROLLABLE
                        "scrollable" "gboolean" T T)
                       (SHOW-BORDER NOTEBOOK-SHOW-BORDER
                        "show-border" "gboolean" T T)
                       (SHOW-TABS NOTEBOOK-SHOW-TABS
                        "show-tabs" "gboolean" T T)
                       (TAB-POS NOTEBOOK-TAB-POS
                        "tab-pos" "GtkPositionType" T T)))
             (gobject:get-gtype-definition "GtkNotebook"))))

;;; --- Signals ----------------------------------------------------------------

;;;     change-current-page

(test gtk-notebook-change-current-page-signal
  (let* ((name "change-current-page")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     create-window

(test gtk-notebook-create-window-signal
  (let* ((name "create-window")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "GtkNotebook") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget" "gint" "gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     focus-tab

(test gtk-notebook-focus-tab-signal
  (let* ((name "focus-tab")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkNotebookTab")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     move-focus-out

(test gtk-notebook-move-focus-out-signal
  (let* ((name "move-focus-out")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkDirectionType")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     page-added

(test gtk-notebook-page-added-signal
  (let* ((name "page-added")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     page-removed

(test gtk-notebook-page-removed-signal
  (let* ((name "page-removed")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     page-reordered

(test gtk-notebook-page-reordered-signal
  (let* ((name "page-reordered")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     reorder-tab

(test gtk-notebook-reorder-tab-signal
  (let* ((name "reorder-tab")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkDirectionType" "gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     select-page

(test gtk-notebook-select-page-signal
  (let* ((name "select-page")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     switch-page

(test gtk-notebook-switch-page-signal
  (let* ((name "switch-page")
         (gtype (g:gtype "GtkNotebook"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget" "guint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-notebook-properties
  (glib-test:with-check-memory (notebook)
    (setf notebook (make-instance 'gtk:notebook))
    (is-false (gtk:notebook-enable-popup notebook))
    (is-true (setf (gtk:notebook-enable-popup notebook) t))
    (is-false (gtk:notebook-group-name notebook))
    (is (string= "tab" (setf (gtk:notebook-group-name notebook) "tab")))
    (is (= -1 (gtk:notebook-page notebook)))
    (is (= 2 (setf (gtk:notebook-page notebook) 2)))
    (is-false (gtk:notebook-scrollable notebook))
    (is-true (setf (gtk:notebook-scrollable notebook) t))
    (is-true (gtk:notebook-show-border notebook))
    (is-false (setf (gtk:notebook-show-border notebook) nil))
    (is-true (gtk:notebook-show-tabs notebook))
    (is-false (setf (gtk:notebook-show-tabs notebook) nil))
    (is (eq :top (gtk:notebook-tab-pos notebook)))
    (is (eq :left (setf (gtk:notebook-tab-pos notebook) :left)))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-notebook-child-properties
  (glib-test:with-check-memory (notebook child label)
    (setf notebook (make-instance 'gtk:notebook))
    (setf child (make-instance 'gtk:frame))
    (setf label (make-instance 'gtk:label :label "label"))
    (is (= 0 (gtk:notebook-append-page notebook child label)))
    (is-false (gtk:notebook-child-detachable notebook child))
    (is-true (setf (gtk:notebook-child-detachable notebook child) t))
    (is-false (gtk:notebook-child-menu-label notebook child))
    (is (string= "label" (setf (gtk:notebook-child-menu-label notebook child) "label")))
    (is (= 0 (gtk:notebook-child-position notebook child)))
    (is (= 1 (setf (gtk:notebook-child-position notebook child) 1)))
    (is-false (gtk:notebook-child-reorderable notebook child))
    (is-true (setf (gtk:notebook-child-reorderable notebook child) t))
    (is-false (gtk:notebook-child-tab-expand notebook child))
    (is-true (setf (gtk:notebook-child-tab-expand notebook child) t))
    (is-true (gtk:notebook-child-tab-fill notebook child))
    (is-false (setf (gtk:notebook-child-tab-fill notebook child) nil))
    (is (string= "label" (gtk:notebook-child-tab-label notebook child)))
    (is (string= "text" (setf (gtk:notebook-child-tab-label notebook child) "text")))
    ;; Remove page from notebook
    (is-false (gtk:notebook-remove-page notebook -1))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-notebook-style-properties
  (glib-test:with-check-memory (notebook)
    (setf notebook (make-instance 'gtk:notebook))
    (is (= 0 (gtk:widget-style-property notebook "arrow-spacing")))
    (is-true (gtk:widget-style-property notebook "has-backward-stepper"))
    (is-true (gtk:widget-style-property notebook "has-forward-stepper"))
    (is-false (gtk:widget-style-property notebook "has-secondary-backward-stepper"))
    (is-false (gtk:widget-style-property notebook "has-secondary-forward-stepper"))
    (is-true (gtk:widget-style-property notebook "has-tab-gap"))
    (is (= 0 (gtk:widget-style-property notebook "initial-gap")))
    (is (= 1 (gtk:widget-style-property notebook "tab-curvature")))
    (is (= 2 (gtk:widget-style-property notebook "tab-overlap")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_notebook_new

(test gtk-notebook-new
  (glib-test:with-check-memory (notebook)
    (is (typep (setf notebook (gtk:notebook-new)) 'gtk:notebook))))

;;;     gtk_notebook_append_page
;;;     gtk_notebook_append_page_menu
;;;     gtk_notebook_prepend_page
;;;     gtk_notebook_prepend_page_menu
;;;     gtk_notebook_insert_page
;;;     gtk_notebook_insert_page_menu
;;;     gtk_notebook_remove_page

(test gtk-notebook-add-page.1
  (glib-test:with-check-memory (notebook)
    (setf notebook (make-instance 'gtk:notebook))
    (let ((page1 (make-instance 'gtk:frame))
          (page2 (make-instance 'gtk:frame))
          (page3 (make-instance 'gtk:frame))
          (page4 (make-instance 'gtk:frame))
          (page5 (make-instance 'gtk:frame))
          (page6 (make-instance 'gtk:frame))
          (label1 (make-instance 'gtk:label :label "label1"))
          (label2 (make-instance 'gtk:label :label "label2"))
          (label3 (make-instance 'gtk:label :label "label3")))

      (is (= 0 (gtk:notebook-append-page notebook page1 nil)))
      (is (= 1 (gtk:notebook-append-page notebook page2 label1)))

      (is (= 0 (gtk:notebook-prepend-page notebook page3 nil)))
      (is (= 0 (gtk:notebook-prepend-page notebook page4 label2)))

      (is (= 3 (gtk:notebook-insert-page notebook page5 nil 3)))
      (is (= 3 (gtk:notebook-insert-page notebook page6 label3 3)))

      (is (= 6 (length (gtk:container-children notebook))))
      (is-false (gtk:notebook-remove-page notebook 0))
      (is (= 5 (length (gtk:container-children notebook))))
      (is-false (gtk:notebook-remove-page notebook page6))
      (is (= 4 (length (gtk:container-children notebook))))
      ;; Remove pages from notebook
      (is-false (gtk:notebook-remove-page notebook page2))
      (is-false (gtk:notebook-remove-page notebook page3))
      (is-false (gtk:notebook-remove-page notebook page4))
      (is-false (gtk:notebook-remove-page notebook page5)))))

(test gtk-notebook-add-page.2
  (glib-test:with-check-memory (notebook)
  (setf notebook (make-instance 'gtk:notebook))
    (let ((page1 (make-instance 'gtk:frame))
          (page2 (make-instance 'gtk:frame))
          (page3 (make-instance 'gtk:frame))
          (label1 (make-instance 'gtk:label :label "label1"))
          (label2 (make-instance 'gtk:label :label "label2"))
          (label3 (make-instance 'gtk:label :label "label3"))
          (menu-label1 (make-instance 'gtk:label :label "menu-label1"))
          (menu-label2 (make-instance 'gtk:label :label "menu-label2"))
          (menu-label3 (make-instance 'gtk:label :label "menu-label3")))

      (is (= 0 (gtk:notebook-append-page-menu notebook page1 label1 menu-label1)))
      (is (= 0 (gtk:notebook-prepend-page-menu notebook page2 label2 menu-label2)))
      (is (= 1 (gtk:notebook-insert-page-menu notebook page3 label3 menu-label3 1)))
      ;; Remove pages from notebook
      (is-false (gtk:notebook-remove-page notebook page1))
      (is-false (gtk:notebook-remove-page notebook page2))
      (is-false (gtk:notebook-remove-page notebook page3)))))

(test gtk-notebook-add-page.3
  (glib-test:with-check-memory (notebook)
  (setf notebook (make-instance 'gtk:notebook))
    (let ((page1 (make-instance 'gtk:frame))
          (page2 (make-instance 'gtk:frame))
          (page3 (make-instance 'gtk:frame))
          (page4 (make-instance 'gtk:frame))
          (page5 (make-instance 'gtk:frame))
          (page6 (make-instance 'gtk:frame))
          (label1 (make-instance 'gtk:label :label "label1"))
          (label2 (make-instance 'gtk:label :label "label2"))
          (label3 (make-instance 'gtk:label :label "label3"))
          (label4 (make-instance 'gtk:label :label "label4"))
          (label5 (make-instance 'gtk:label :label "label5"))
          (label6 (make-instance 'gtk:label :label "label6"))
          (menu-label1 (make-instance 'gtk:label :label "menu-label1"))
          (menu-label2 (make-instance 'gtk:label :label "menu-label2"))
          (menu-label3 (make-instance 'gtk:label :label "menu-label3")))

      (is (= 0 (gtk:notebook-add-page notebook page1 label1)))
      (is (= 0 (gtk:notebook-add-page notebook page2 label2 :position :start)))
      (is (= 1 (gtk:notebook-add-page notebook page3 label3 :position 1)))

      (is (= 3 (gtk:notebook-add-page notebook page4 label4 :menu menu-label1)))
      (is (= 0 (gtk:notebook-add-page notebook page5 label5 :position :start
                                                            :menu menu-label2)))
      (is (= 1 (gtk:notebook-add-page notebook page6 label6 :position 1
                                                            :menu menu-label3)))
      ;; Remove pages from notebook
      (is-false (gtk:notebook-remove-page notebook page1))
      (is-false (gtk:notebook-remove-page notebook page2))
      (is-false (gtk:notebook-remove-page notebook page3))
      (is-false (gtk:notebook-remove-page notebook page4))
      (is-false (gtk:notebook-remove-page notebook page5))
      (is-false (gtk:notebook-remove-page notebook page6)))))

;;;     gtk_notebook_detach_tab
;;;     gtk_notebook_page_num
;;;     gtk_notebook_next_page
;;;     gtk_notebook_prev_page
;;;     gtk_notebook_reorder_child

;;;     gtk_notebook_popup_enable
;;;     gtk_notebook_popup_disable
;;;     gtk_notebook_get_current_page
;;;     gtk_notebook_get_menu_label
;;;     gtk_notebook_get_nth_page
;;;     gtk_notebook_get_n_pages
;;;     gtk_notebook_get_tab_label
;;;     gtk_notebook_set_menu_label
;;;     gtk_notebook_set_menu_label_text
;;;     gtk_notebook_set_tab_label
;;;     gtk_notebook_set_tab_label_text
;;;     gtk_notebook_set_tab_reorderable
;;;     gtk_notebook_set_tab_detachable
;;;     gtk_notebook_get_menu_label_text

;;;     gtk_notebook_get_tab_label_text

;;;     gtk_notebook_get_tab_reorderable
;;;     gtk_notebook_get_tab_detachable
;;;     gtk_notebook_get_tab_hborder                       deprecated
;;;     gtk_notebook_get_tab_vborder                       deprecated
;;;     gtk_notebook_set_current_page

;;;     gtk_notebook_set_action_widget
;;;     gtk_notebook_get_action_widget

;;; 2026-06-20
