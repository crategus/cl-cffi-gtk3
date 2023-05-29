(in-package :gtk-test)

(def-suite gtk-statusbar :in gtk-suite)
(in-suite gtk-statusbar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStatusbar

(test statusbar-class
  ;; Type check
  (is (g:type-is-object "GtkStatusbar"))
  ;; Check the registered name
  (is (eq 'gtk:statusbar
          (glib:symbol-for-gtype "GtkStatusbar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStatusbar")
          (g:gtype (cffi:foreign-funcall "gtk_statusbar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBox") (g:type-parent "GtkStatusbar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkStatusbar")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (list-interfaces "GtkStatusbar")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkStatusbar")))
  ;; Get the names of the style properties.
  (is (equal '("shadow-type")
             (list-style-properties "GtkStatusbar")))
  ;; Get the names of the child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (list-child-properties "GtkStatusbar")))
  ;; Check the signals
  (is (equal '("text-popped" "text-pushed")
             (list-signals "GtkStatusbar")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkStatusbar" GTK-STATUSBAR
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_statusbar_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkStatusbar"))))

;;; --- Style Properties -------------------------------------------------------

(test statusbar-style-properties
  (let ((statusbar (make-instance 'gtk:statusbar)))
    (is (eq :in (gtk:widget-style-property statusbar "shadow-type")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_statusbar_new

(test statusbar-new
  (is (eq 'gtk:statusbar (type-of (gtk:statusbar-new)))))

;;;     gtk_statusbar_get_context_id

(test statusbar-context-id
  (let ((statusbar (gtk:statusbar-new)))
    ;; Get the context IDs
    (is (= 1 (gtk:statusbar-context-id statusbar "context1")))
    (is (= 2 (gtk:statusbar-context-id statusbar "context2")))
    (is (= 3 (gtk:statusbar-context-id statusbar "context3")))
    ;; Read the context IDs
    (is (= 1 (gtk:statusbar-context-id statusbar "context1")))
    (is (= 2 (gtk:statusbar-context-id statusbar "context2")))
    (is (= 3 (gtk:statusbar-context-id statusbar "context3")))))

;;;     gtk_statusbar_push
;;;     gtk_statusbar_pop
;;;     gtk_statusbar_remove
;;;     gtk_statusbar_remove_all

(test statusbar-push
  (let* ((statusbar (gtk:statusbar-new))
         (context-id-1 (gtk:statusbar-context-id statusbar "context1"))
         (context-id-2 (gtk:statusbar-context-id statusbar "context2"))
         message-id-1
         message-id-2)

    (is (eq 'gtk:statusbar (type-of statusbar)))
    (is (= 1 context-id-1))
    (is (= 2 context-id-2))
    ;; Set some message IDs on the contexts
    (is (= 1 (setf message-id-1
                   (gtk:statusbar-push statusbar context-id-1 "message1"))))
    (is (= 2 (setf message-id-2
                   (gtk:statusbar-push statusbar context-id-1 "message2"))))
    (is (= 3 (setf message-id-1
                   (gtk:statusbar-push statusbar context-id-2 "message1"))))
    (is (= 4 (setf message-id-2
                   (gtk:statusbar-push statusbar context-id-2 "message2"))))
    ;; Remove some messages
    (is-false (gtk:statusbar-remove statusbar "context1" 1))
    (is-false (gtk:statusbar-remove-all statusbar "context2"))))

;;;     gtk_statusbar_get_message_area

(test statusbar-message-area
  (let ((statusbar (gtk:statusbar-new)))
    (is (eq 'gtk:box (type-of (gtk:statusbar-message-area statusbar))))))

;;; --- 2023-5-29 --------------------------------------------------------------
