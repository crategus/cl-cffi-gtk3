(in-package :gtk-test)

(def-suite gtk-container :in gtk-suite)
(in-suite gtk-container)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkResizeMode

(test gtk-resize-mode
  ;; Check the type
  (is (g:type-is-enum "GtkResizeMode"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkResizeMode")
          (g:gtype (cffi:foreign-funcall "gtk_resize_mode_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:resize-mode
          (glib:symbol-for-gtype "GtkResizeMode")))
  ;; Check the names
  (is (equal '("GTK_RESIZE_PARENT" "GTK_RESIZE_QUEUE" "GTK_RESIZE_IMMEDIATE")
             (list-enum-item-name "GtkResizeMode")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkResizeMode")))
  ;; Check the nick names
  (is (equal '("parent" "queue" "immediate")
             (list-enum-item-nick "GtkResizeMode")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkResizeMode"
                             GTK-RESIZE-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_resize_mode_get_type")
                             (:PARENT 0)
                             (:QUEUE 1)
                             (:IMMEDIATE 2))
             (gobject:get-g-type-definition "GtkResizeMode"))))

;;;     GtkContainer

(test gtk-container-class
  ;; Type check
  (is (g:type-is-object "GtkContainer"))
  ;; Check the registered name
  (is (eq 'gtk:container
          (glib:symbol-for-gtype "GtkContainer")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkContainer")
          (g:gtype (cffi:foreign-funcall "gtk_container_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkContainer")))
  ;; Check the children
  #-windows
  (is (or (equal '("GtkBin" "GtkBox" "GtkFixed" "GtkFlowBox" "GtkGrid"
                   "GtkHeaderBar" "GtkIconView" "GtkLayout" "GtkListBox"
                   "GtkMenuShell" "GtkNotebook" "GtkPaned" "GtkSocket"
                   "GtkStack" "GtkTable" "GtkTextView" "GtkToolItemGroup"
                   "GtkToolPalette" "GtkToolbar" "GtkTreeView")
                 (list-children "GtkContainer"))
          (equal '("GtkBin" "GtkBox" "GtkFixed" "GtkFlowBox" "GtkGrid"
                   "GtkHeaderBar" "GtkIconView" "GtkLayout" "GtkListBox"
                   "GtkMenuShell" "GtkNotebook" "GtkPaned" "GtkPathBar"
                   "GtkSocket" "GtkStack" "GtkTable" "GtkTextView"
                   "GtkToolItemGroup" "GtkToolPalette" "GtkToolbar"
                   "GtkTreeView")
                 (list-children "GtkContainer"))))
  #+windows
  (is (or (equal '("GtkBin" "GtkBox" "GtkFixed" "GtkFlowBox" "GtkGrid"
                   "GtkHeaderBar" "GtkIconView" "GtkLayout" "GtkListBox"
                   "GtkMenuShell" "GtkNotebook" "GtkPaned" "GtkStack" "GtkTable"
                   "GtkTextView" "GtkToolItemGroup" "GtkToolPalette"
                   "GtkToolbar" "GtkTreeView")
                 (list-children "GtkContainer"))
          (equal '("GtkBin" "GtkBox" "GtkFixed" "GtkFlowBox" "GtkGrid"
                   "GtkHeaderBar" "GtkIconView" "GtkLayout" "GtkListBox"
                   "GtkMenuShell" "GtkNotebook" "GtkPaned" "GtkPathBar"
                   "GtkStack" "GtkTable" "GtkTextView" "GtkToolItemGroup"
                   "GtkToolPalette" "GtkToolbar" "GtkTreeView")
                 (list-children "GtkContainer"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkContainer")))
  ;; Check the class properties
  (is (equal '("border-width" "child" "resize-mode")
             (list-properties "GtkContainer")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-style-properties "GtkContainer")))
  ;; Get the names of the child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (list-child-properties "GtkBox")))
  ;; Check the signals
  (is (equal '("add" "check-resize" "remove" "set-focus-child")
             (list-signals "GtkContainer")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkContainer" GTK-CONTAINER
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_container_get_type")
                       ((BORDER-WIDTH GTK-CONTAINER-BORDER-WIDTH "border-width"
                         "guint" T T)
                        (CHILD GTK-CONTAINER-CHILD "child" "GtkWidget" NIL T)
                        (RESIZE-MODE GTK-CONTAINER-RESIZE-MODE "resize-mode"
                         "GtkResizeMode" T T)))
             (gobject:get-g-type-definition "GtkContainer"))))

;;; ----------------------------------------------------------------------------
;;; Check accessor functions of GtContainer
;;; ----------------------------------------------------------------------------

;;; --- gtk_container_border_width ---------------------------------------------

(test gtk-container-border-width.1
  (let ((box (make-instance 'gtk:box)))
    (is (eql 0 (gtk:container-border-width box)))
    (setf (gtk:container-border-width box) 12)
    (is (eql 12 (gtk:container-border-width box)))))

(test gtk-container-border-width.2
  (let ((box (make-instance 'gtk:box :border-width 12)))
    (is (eql 12 (gtk:container-border-width box)))))

;;; --- gtk_container_child ----------------------------------------------------

(test gtk-container-child.1
  (let ((box (make-instance 'gtk:box)))
    ;; The CHILD property is not readable
    (signals (error) (gtk:container-child box))))

(test gtk-container-child.2
  (let ((box (make-instance 'gtk:box))
        (button (make-instance 'gtk:button)))
    ;; Put a button into the box
    (setf (gtk:container-child box) button)
    (is (equal (list button)
               (gtk:container-children box)))))

;;; --- gtk_container_resize_mode ----------------------------------------------

(test gtk-container-resize-mode
  (let ((box (make-instance 'gtk:box)))
    (is (eql :parent (gtk:container-resize-mode box)))
    (setf (gtk:container-resize-mode box) :queue)
    (is (eql :queue (gtk:container-resize-mode box)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_container_add
;;;     gtk_container_remove

(test gtk-container-add/remove
  (let ((box (make-instance 'gtk:box)))
    ;; Add widgets to the container
    (is (= 0 (length (gtk:container-children box))))
    (is-false (gtk:container-add box (make-instance 'gtk:button)))
    (is (= 1 (length (gtk:container-children box))))
    (is-false (gtk:container-add box (make-instance 'gtk:button)))
    (is (= 2 (length (gtk:container-children box))))
    ;; Remove widgets from the container
    (is-false (gtk:container-remove box (first (gtk:container-children box))))
    (is (= 1 (length (gtk:container-children box))))
    (is-false (gtk:container-remove box (first (gtk:container-children box))))
    (is (= 0 (length (gtk:container-children box))))))

;;;     gtk_container_add_with_properties

(test gtk-container-add-with-properties
  (let ((box (make-instance 'gtk:box :orientation :vertical))
        (button (make-instance 'gtk:button)))
    (is-false (gtk:container-add-with-properties box button
                                                 "expand" t
                                                 "fill" nil
                                                 "pack-type" :end
                                                 "padding" 6))
    (is (equal '(t nil :end 6 0)
               (gtk:container-child-get box button
                                        "expand"
                                        "fill"
                                        "pack-type"
                                        "padding"
                                        "position")))))

;;;     gtk_container_check_resize

;;;     gtk_container_foreach

(test gtk-container-foreach
  (let ((box (make-instance 'gtk:box :orientation :vertical))
        (collect nil))
    ;; Add 10 label to the container
    (dotimes (i 10)
      (is-false (gtk:container-add box (make-instance 'gtk:label))))
    ;; Check the length of the list of children
    (is (= 10 (length (gtk:container-children box))))
    ;; Collect the labels
    (is-false (gtk:container-foreach box
                  (lambda (widget) (push widget collect))))
    ;; Check the number of labels
    (is (= 10 (length collect)))
    ;; Set the string "text" on the labels in the box
    (is-false (gtk:container-foreach box
                  (lambda (widget) (setf (gtk:label-text widget) "text"))))
    ;; Read the string "text" of the labels in the box
    (is-false (gtk:container-foreach box
                  (lambda (widget)
                    (is (string= "text" (gtk:label-text widget))))))))

;;;     gtk_container_get_children

(test gtk-container-children
  (let ((container (make-instance 'gtk:box :orientation :vertical))
        (button (make-instance 'gtk:button))
        (label (make-instance 'gtk:label)))
    ;; Add to widgets to the container
    (is-false (gtk:container-add container button))
    (is-false (gtk:container-add container label))
    ;; Check the children of the container
    (is (= 2 (length (gtk:container-children container))))
    (is (member button (gtk:container-children container) :test #'eq))
    (is (member label (gtk:container-children container) :test #'eq))))

;;;     gtk_container_get_path_for_child

(test gtk-container-path-for-child
  (let ((box (make-instance 'gtk:box :orientation :vertical))
        (child (make-instance 'gtk:label)))
    (is-false (gtk:container-add box child))
    (is (eq 'gtk:widget-path
            (type-of (gtk:container-path-for-child box child))))
    (is (string= "box:dir-ltr.vertical label:dir-ltr"
                 (gtk:widget-path-to-string
                     (gtk:container-path-for-child box child))))))

;;;     gtk_container_set_reallocate_redraws

;;;     gtk_container_get_focus_child
;;;     gtk_container_set_focus_child

(test gtk-container-focus-child
  (let ((box (make-instance 'gtk:box :orientation :vertical))
        (button1 (make-instance 'gtk:button))
        (button2 (make-instance 'gtk:button))
        (button3 (make-instance 'gtk:button)))

    (is-false (gtk:container-add box button1))
    (is-false (gtk:container-add box button2))
    (is-false (gtk:container-add box button3))

    (is-false (gtk:container-focus-child box))

    (is-true (gtk:widget-can-focus button1))
    (is-false (gtk:widget-grab-focus button1))
    (is (equal button1 (gtk:container-focus-child box)))

    (is-true (gtk:widget-can-focus button2))
    (is (equal button2 (setf (gtk:container-focus-child box) button2)))
    (is (equal button2 (gtk:container-focus-child box)))))

;;;     gtk_container_get_focus_vadjustment
;;;     gtk_container_set_focus_vadjustment
;;;     gtk_container_get_focus_hadjustment
;;;     gtk_container_set_focus_hadjustment
;;;     gtk_container_resize_children

;;;     gtk_container_child_type

(test gtk-container-child-type.1
  (let ((box (make-instance 'gtk:box)))
    (is (string= "GtkWidget" (g:type-name (gtk:container-child-type box))))))

(test gtk-container-child-type.2
  (let ((paned (make-instance 'gtk:paned))
        (frame1 (make-instance 'gtk:frame))
        (frame2 (make-instance 'gtk:frame)))
    (is (string= "GtkWidget" (g:type-name (gtk:container-child-type paned))))
    (is-false (gtk:paned-add1 paned frame1))
    (is-false (gtk:paned-add2 paned frame2))
    (is (string= "void" (g:type-name (gtk:container-child-type paned))))))

;;;     gtk_container_child_get

(test gtk-container-child-get
  (let ((box (make-instance 'gtk:box :orientation :vertical))
        (button (make-instance 'gtk:button)))
    (is-false (gtk:container-add box button))
    (is (equal '(nil t :start 0 0)
               (gtk:container-child-get box button
                                        "expand"
                                        "fill"
                                        "pack-type"
                                        "padding"
                                        "position")))))

;;;     gtk_container_child_set

(test gtk-container-child-set
  (let ((box (make-instance 'gtk:box :orientation :vertical))
        (button (make-instance 'gtk:button)))
    (is-false (gtk:container-add box button))
    (is-false (gtk:container-child-set box button
                                       "expand" t
                                       "fill" nil
                                       "pack-type" :end
                                       "padding" 6))
    (is (equal '(t nil :end 6 0)
               (gtk:container-child-get box button
                                        "expand"
                                        "fill"
                                        "pack-type"
                                        "padding"
                                        "position")))))

;;;     gtk_container_child_property

(test gtk-container-child-property.1
  (let ((box (make-instance 'gtk:box))
        (button (make-instance 'gtk:button)))
    ;; Add a button to the box
    (is-false (gtk:container-add box button))
    ;; Get the values of the child properties of GtkBox
    (is-false (gtk:container-child-property box button "expand"))
    (is-true (gtk:container-child-property box button "fill"))
    (is (eq :start (gtk:container-child-property box button "pack-type")))
    (is (= 0 (gtk:container-child-property box button "padding")))
    (is (= 0 (gtk:container-child-property box button "position")))))

;;;     (setf container-child-property)

(test gtk-container-child-property.2
  (let ((box (make-instance 'gtk:box))
        (button (make-instance 'gtk:button)))
    ;; Add a button to the box
    (is-false (gtk:container-add box button))
    ;; Set the values of the child properties of GtkBox
    (is-true (setf (gtk:container-child-property box button "expand") t))
    (is-true (gtk:container-child-property box button "expand"))
    (is-false (setf (gtk:container-child-property box button "fill") nil))
    (is-false (gtk:container-child-property box button "fill"))
    (is (eq :end
            (setf (gtk:container-child-property box button "pack-type") :end)))
    (is (eq :end (gtk:container-child-property box button "pack-type")))
    (is (= 6 (setf (gtk:container-child-property box button "padding") 6)))
    (is (= 6 (gtk:container-child-property box button "padding")))))

(test gtk-container-child-property.3
  (let ((box (make-instance 'gtk:box))
        (button (make-instance 'gtk:button)))
    ;; Add a button to the box
    (is-false (gtk:container-add box button))
    ;; Set the values of the child properties of GtkBox
    (is-true (setf (gtk:container-child-property box
                                                 button
                                                 "expand"
                                                 "gboolean") t))
    (is-true (gtk:container-child-property box button "expand" "gboolean"))
    (is-false (setf (gtk:container-child-property box button "fill" "gboolean")
                    nil))
    (is-false (gtk:container-child-property box button "fill" "gboolean"))
    (is (eq :end
            (setf (gtk:container-child-property box button
                                                    "pack-type"
                                                    "GtkPackType") :end)))
    (is (eq :end (gtk:container-child-property box button "pack-type"
                                                          "GtkPackType")))
    (is (= 6 (setf (gtk:container-child-property box button "padding" "gint")
                   6)))
    (is (= 6 (gtk:container-child-property box button "padding" "gint")))))

;;;     gtk_container_child_get_valist
;;;     gtk_container_child_set_valist
;;;     gtk_container_child_notify
;;;     gtk_container_child_notify_by_pspec

;;;     gtk_container_forall

(test gtk-container-forall
  (let ((box (make-instance 'gtk:box :orientation :vertical))
        (collect nil))
    ;; Add 10 label to the container
    (dotimes (i 10)
      (is-false (gtk:container-add box (make-instance 'gtk:label))))
    ;; Check the length of the list of children, we have no internal children
    (is (= 10 (length (gtk:container-children box))))
    ;; Collect the labels
    (is-false (gtk:container-forall box
                                    (lambda (widget)
                                    (push widget collect))))
    ;; Check the number of labels
    (is (= 10 (length collect)))
    ;; Set the string "text" on the labels in the box
    (is-false (gtk:container-forall box
                  (lambda (widget) (setf (gtk:label-text widget) "text"))))
    ;; Read the string "text" of the labels in the box
    (is-false (gtk:container-forall box
                  (lambda (widget)
                    (is (string= "text" (gtk:label-text widget))))))))

;;;     gtk_container_propagate_draw

;;;     gtk_container_get_focus_chain
;;;     gtk_container_set_focus_chain
;;;     gtk_container_unset_focus_chain

(test gtk-container-focus-chain
  (let ((box (make-instance 'gtk:box :orientation :vertical))
        (button1 (make-instance 'gtk:button))
        (button2 (make-instance 'gtk:button))
        (button3 (make-instance 'gtk:button)))

    (is-false (gtk:container-add box button1))
    (is-false (gtk:container-add box button2))
    (is-false (gtk:container-add box button3))

    (is-false (gtk:container-focus-chain box))

    (is (listp (setf (gtk:container-focus-chain box)
                     (list button1 button2 button3))))

    (is (equal button1 (first (gtk:container-focus-chain box))))
    (is (equal button2 (second (gtk:container-focus-chain box))))
    (is (equal button3 (third (gtk:container-focus-chain box))))

    (is-false (gtk:container-unset-focus-chain box))
    (is-false (gtk:container-focus-chain box))))

;;;     gtk_container_class_find_child_property

;; Find a child property of type gboolean

(test gtk-container-class-find-child-property.1
  (with-foreign-objects ((value '(:struct g:value)))
    (is-true (g:value-init value "gboolean"))
    (let ((pspec (gtk:container-class-find-child-property "GtkBox" "expand")))
      ;; Type checks
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamBoolean") (g:param-spec-type pspec)))
      (is (string= "GParamBoolean" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "gboolean") (g:param-spec-value-type pspec)))
      ;; Check the default value
      (is (eq (g:gtype "gboolean")
              (g:value-type (g:param-spec-default-value pspec))))
      (is-false (gobject:parse-g-value (g:param-spec-default-value pspec)))
      (is-false (g:param-value-set-default pspec value))
      (is-false (gobject:parse-g-value value))
      ;; Check the infos about the parameter
      (is (string= "expand" (g:param-spec-name pspec)))
      (is (stringp (g:param-spec-nick pspec)))
      (is (stringp (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value)))))

;; Find a child property of an enumeration type

(test gtk-container-class-find-child-property.2
  (with-foreign-objects ((value '(:struct g:value)))
    (is-true (g:value-init value "GtkPackType"))
    (let ((pspec (gtk:container-class-find-child-property "GtkBox"
                                                          "pack-type")))
      ;; Type checks
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamEnum") (g:param-spec-type pspec)))
      (is (string= "GParamEnum" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "GtkPackType") (g:param-spec-value-type pspec)))
      ;; Check the default value
      (is (eq (g:gtype "GtkPackType")
              (g:value-type (g:param-spec-default-value pspec))))
      (is (eq :start
              (gobject:parse-g-value (g:param-spec-default-value pspec))))
      (is-false (g:param-value-set-default pspec value))
      (is (eq :start (gobject:parse-g-value value)))
      ;; Check the infos about the parameter
      (is (string= "pack-type" (g:param-spec-name pspec)))
      (is (stringp (g:param-spec-nick pspec)))
      (is (stringp (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value)))))

;; Find a child property of type gint

(test gtk-container-class-find-child-property.3
  (with-foreign-objects ((value '(:struct g:value)))
    (is-true (g:value-init value "gint"))
    (let ((pspec (gtk:container-class-find-child-property "GtkBox" "position")))
      ;; Type checks
      (is-true (g:type-is-param (g:type-from-instance pspec)))
      (is-true (g:is-param-spec pspec))
      (is (eq (g:gtype "GParamInt") (g:param-spec-type pspec)))
      (is (string= "GParamInt" (g:param-spec-type-name pspec)))
      (is (eq (g:gtype "gint") (g:param-spec-value-type pspec)))
      ;; Check the default value
      (is (eq (g:gtype "gint")
              (g:value-type (g:param-spec-default-value pspec))))
      (is (= 0 (gobject:parse-g-value (g:param-spec-default-value pspec))))
      (is-false (g:param-value-set-default pspec value))
      (is (= 0 (gobject:parse-g-value value)))
      ;; Check the infos about the parameter
      (is (string= "position" (g:param-spec-name pspec)))
      (is (stringp (g:param-spec-nick pspec)))
      (is (stringp (g:param-spec-blurb pspec)))
      ;; Unset the GValue
      (is-false (g:value-unset value)))))

;;;     gtk_container_class_install_child_property
;;;     gtk_container_class_install_child_properties

;;;     gtk_container_class_list_child_properties

(test gtk-container-class-list-child-properties
    (is (listp (gtk:container-class-list-child-properties "GtkBox")))
    (is (equal '("expand" "fill" "padding" "pack-type" "position")
               (mapcar #'g:param-spec-name
                       (gtk:container-class-list-child-properties "GtkBox")))))

;;;     gtk_container_class_handle_border_width

;;; --- 2023-6-17 --------------------------------------------------------------
