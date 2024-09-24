(in-package :gtk-test)

(def-suite gtk-popover :in gtk-suite)
(in-suite gtk-popover)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPopoverConstraint

(test gtk-popover-constraint
  ;; Check type
  (is (g:type-is-enum "GtkPopoverConstraint"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPopoverConstraint")
          (g:gtype (cffi:foreign-funcall "gtk_popover_constraint_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:popover-constraint
          (glib:symbol-for-gtype "GtkPopoverConstraint")))
  ;; Check names
  (is (equal '("GTK_POPOVER_CONSTRAINT_NONE" "GTK_POPOVER_CONSTRAINT_WINDOW")
             (glib-test:list-enum-item-names "GtkPopoverConstraint")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkPopoverConstraint")))
  ;; Check nick names
  (is (equal '("none" "window")
             (glib-test:list-enum-item-nicks "GtkPopoverConstraint")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPopoverConstraint"
                                     GTK:POPOVER-CONSTRAINT
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_popover_constraint_get_type")
                       (:NONE 0)
                       (:WINDOW 1))
             (gobject:get-gtype-definition "GtkPopoverConstraint"))))

;;;     GtkPopover

(test gtk-popover-class
  ;; Check type
  (is (g:type-is-object "GtkPopover"))
  ;; Check registered name
  (is (eq 'gtk:popover
          (glib:symbol-for-gtype "GtkPopover")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPopover")
          (g:gtype (cffi:foreign-funcall "gtk_popover_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkPopover")))
  ;; Check children
  (is (equal '("GtkPopoverMenu")
             (glib-test:list-children "GtkPopover")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkPopover")))
  ;; Check class properties
  (is (equal '("constrain-to" "modal" "pointing-to" "position" "relative-to"
               "transitions-enabled")
             (glib-test:list-properties "GtkPopover")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkPopover")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkPopover")))
  ;; Check signals
  (is (equal '("closed")
             (glib-test:list-signals "GtkPopover")))
  ;; CSS information
  (is (string= "popover"
               (gtk:widget-class-css-name "GtkPopover")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPopover" GTK:POPOVER
                       (:SUPERCLASS GTK:BIN
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_popover_get_type")
                       ((CONSTRAIN-TO POPOVER-CONSTRAIN-TO
                         "constrain-to" "GtkPopoverConstraint" T T)
                        (MODAL POPOVER-MODAL "modal" "gboolean" T T)
                        (POINTING-TO POPOVER-POINTING-TO
                         "pointing-to" "GdkRectangle" T T)
                        (POSITION POPOVER-POSITION
                         "position" "GtkPositionType" T T)
                        (RELATIVE-TO POPOVER-RELATIVE-TO
                         "relative-to" "GtkWidget" T T)
                        (TRANSITIONS-ENABLED POPOVER-TRANSITIONS-ENABLED
                         "transitions-enabled" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkPopover"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-popover-properties
  (let ((popover (make-instance 'gtk:popover)))
    (is (eq :window (gtk:popover-constrain-to popover)))
    (is-true (gtk:popover-modal popover))
    (is (gdk:rectangle-equal (gdk:rectangle-new)
                             (gtk:popover-pointing-to popover)))
    (is (eq :top (gtk:popover-position popover)))
    (is-false (gtk:popover-relative-to popover))
    (is-true (gtk:popover-transitions-enabled popover))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-popover-closed-signal
  (let* ((name "closed")
         (gtype (g:gtype "GtkPopover"))
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
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_popover_new

(test gtk-popover-new
  (let ((button (make-instance 'gtk:button))
        popover)
    (is (typep (setf popover
                     (gtk:popover-new button)) 'gtk:popover))
    (is (eq :window (gtk:popover-constrain-to popover)))
    (is-true (gtk:popover-modal popover))
    (is (gdk:rectangle-equal (gdk:rectangle-new)
                             (gtk:popover-pointing-to popover)))
    (is (eq :top (gtk:popover-position popover)))
    (is (eq button (gtk:popover-relative-to popover)))
    (is-true (gtk:popover-transitions-enabled popover))))

;;;     gtk_popover_new_from_model

(test gtk-popover-new-from-model
  (let ((button (make-instance 'gtk:button))
        (menu (make-instance 'g:menu))
        popover)
    (is (typep (setf popover
                     (gtk:popover-new-from-model button menu)) 'gtk:popover))
    (is (eq :window (gtk:popover-constrain-to popover)))
    (is-true (gtk:popover-modal popover))
    (is (gdk:rectangle-equal (gdk:rectangle-new)
                             (gtk:popover-pointing-to popover)))
    (is (eq :top (gtk:popover-position popover)))
    (is (eq button (gtk:popover-relative-to popover)))
    (is-true (gtk:popover-transitions-enabled popover))))

;;;     gtk_popover_bind_model

(test gtk-popover-bind-model
  (let* ((button (make-instance 'gtk:button))
         (menu (make-instance 'g:menu))
         (popover (gtk:popover-new button)))
    (is-false (gtk:popover-bind-model popover menu))
    (is-false (gtk:popover-bind-model popover menu "app"))
    (is-false (gtk:popover-bind-model popover nil))))

;;;     gtk_popover_popup
;;;     gtk_popover_popdown

;;;     gtk_popover_set_default_widget
;;;     gtk_popover_get_default_widget

(test gtk-popover-default-widget
  (let* ((button (make-instance 'gtk:button :can-default t))
         (popover (gtk:popover-new button)))
    (is-false (gtk:popover-default-widget popover))
    (is (eq button (setf (gtk:popover-default-widget popover) button)))
    (is (eq button (gtk:popover-default-widget popover)))))

;;; 2024-9-21
