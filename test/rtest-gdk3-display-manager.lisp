(in-package :gtk-test)

(def-suite gdk-display-manager :in gdk-suite)
(in-suite gdk-display-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDisplayManager

(test gdk-display-manager-class
  ;; Check type
  (is (g:type-is-object "GdkDisplayManager"))
  ;; Check registered name
  (is (eq 'gdk:display-manager
          (glib:symbol-for-gtype "GdkDisplayManager")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDisplayManager")
          (g:gtype (cffi:foreign-funcall "gdk_display_manager_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDisplayManager")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkDisplayManager")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkDisplayManager")))
  ;; Check class properties
  (is (equal '("default-display")
             (glib-test:list-properties "GdkDisplayManager")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDisplayManager" GDK:DISPLAY-MANAGER
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_display_manager_get_type")
                       ((DEFAULT-DISPLAY DISPLAY-MANAGER-DEFAULT-DISPLAY
                         "default-display" "GdkDisplay" T T)))
             (gobject:get-gtype-definition "GdkDisplayManager"))))

;;; --- Properties -------------------------------------------------------------

;;;   gdk-display-manager-default-display

(test gdk-display-manager-default-display.1
  (let ((manager (gdk:display-manager-get)))
    (is (typep (gdk:display-manager-default-display manager) 'gdk:display))))

(test gdk-display-manager-default-display.2
  (let* ((manager (gdk:display-manager-get))
         (display (gdk:display-manager-default-display manager)))
    (setf (gdk:display-manager-default-display manager) display)
    (is (eq display
            (gdk:display-manager-default-display manager)))))

;;; --- Signals ----------------------------------------------------------------

(test gdk-display-manager-display-opened-signal
  (let* ((name "display-opened")
         (gtype (g:gtype "GdkDisplayManager"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GdkDisplay")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-display-manager-get

(test gdk-display-manager-get
  (is (typep (gdk:display-manager-get) 'gdk:display-manager)))

;;;     gdk-display-manager-list-displays

(test gdk-display-manager-list-displays
  (let ((manager (gdk:display-manager-get)))
    (is (listp (gdk:display-manager-list-displays manager)))
    (is (every (lambda (x) (typep x 'gdk:display))
               (gdk:display-manager-list-displays manager)))))

;;;     gdk-display-manager-open-display

(test gdk-display-manager-open-display
  (let* ((manager (gdk:display-manager-get))
         (name (gdk:display-name (gdk:display-manager-default-display manager))))
    (is-true (gdk:display-manager-open-display manager name))))

;;; 2024-9-21
