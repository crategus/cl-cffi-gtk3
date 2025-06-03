(in-package :gtk-test)

(def-suite gtk-drag-and-drop :in gtk-suite)
(in-suite gtk-drag-and-drop)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDestDefaults

(test gtk-dest-defaults
  ;; Check type
  (is (g:type-is-flags "GtkDestDefaults"))
  ;; Check registered name
  (is (eq 'gtk:dest-defaults
          (glib:symbol-for-gtype "GtkDestDefaults")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDestDefaults")
          (g:gtype (cffi:foreign-funcall "gtk_dest_defaults_get_type" :size))))
  ;; Check names
  (is (equal '("GTK_DEST_DEFAULT_MOTION" "GTK_DEST_DEFAULT_HIGHLIGHT"
               "GTK_DEST_DEFAULT_DROP" "GTK_DEST_DEFAULT_ALL")
             (glib-test:list-flags-item-names "GtkDestDefaults")))
  ;; Check values
  (is (equal '(1 2 4 7)
             (glib-test:list-flags-item-values "GtkDestDefaults")))
  ;; Check nick names
  (is (equal '("motion" "highlight" "drop" "all")
             (glib-test:list-flags-item-nicks "GtkDestDefaults")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkDestDefaults" GTK:DEST-DEFAULTS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_dest_defaults_get_type")
                                     (:MOTION 1)
                                     (:HIGHLIGHT 2)
                                     (:DROP 4)
                                     (:ALL 7))
             (gobject:get-gtype-definition "GtkDestDefaults"))))

;;;     GtkTargetFlags  --> gtk.selection.lisp

;;;     GtkDragResult

(test gtk-drag-result
  ;; Check type
  (is (g:type-is-enum "GtkDragResult"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDragResult")
          (g:gtype (cffi:foreign-funcall "gtk_drag_result_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:drag-result
          (glib:symbol-for-gtype "GtkDragResult")))
  ;; Check names
  (is (equal '("GTK_DRAG_RESULT_SUCCESS" "GTK_DRAG_RESULT_NO_TARGET"
               "GTK_DRAG_RESULT_USER_CANCELLED"
               "GTK_DRAG_RESULT_TIMEOUT_EXPIRED"
               "GTK_DRAG_RESULT_GRAB_BROKEN" "GTK_DRAG_RESULT_ERROR")
             (glib-test:list-enum-item-names "GtkDragResult")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (glib-test:list-enum-item-values "GtkDragResult")))
  ;; Check nick names
  (is (equal '("success" "no-target" "user-cancelled" "timeout-expired"
               "grab-broken" "error")
             (glib-test:list-enum-item-nicks "GtkDragResult")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkDragResult" GTK:DRAG-RESULT
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_drag_result_get_type")
                                    (:SUCCESS 0)
                                    (:NO-TARGET 1)
                                    (:USER-CANCELLED 2)
                                    (:TIMEOUT-EXPIRED 3)
                                    (:GRAB-BROKEN 4)
                                    (:ERROR 5))
             (gobject:get-gtype-definition "GtkDragResult"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drag_dest_set

(test gtk-drag-dest-set
  (let ((tlist (list (list "INTEGER" 0 0)
                     (list "STRING" 0 1)
                     (list "text/plain" 0 1)
                     (list "application/x-rootwindow-drop" 0 2)))
        (dest (make-instance 'gtk:button)))
    (is-false (gtk:drag-dest-set dest          ; widget that will accept a drop
                                 '(::all)      ; default actions for dest Dnd
                                 tlist         ; list of targets to support
                                 '(:copy)))))  ; what to with dropped data

;;;     gtk_drag_dest_set_proxy
;;;     gtk_drag_dest_unset

;;;     gtk_drag_dest_find_target

;;;     gtk_drag_dest_get_target_list
;;;     gtk_drag_dest_set_target_list

(test gtk-drag-dest-target-list
  (let ((tlist (list (list "INTEGER" 0 0)
                     (list "STRING" 0 1)
                     (list "text/plain" 0 1)
                     (list "application/x-rootwindow-drop" 0 2)))
        (dest (make-instance 'gtk:button)))
    (is-false (gtk:drag-dest-set dest        ; widget that will accept a drop
                                 '(::all)    ; default actions for dest Dnd
                                 tlist       ; list of targets to support
                                 '(:copy)))  ; what to with dropped data
    (is (typep (gtk:drag-dest-target-list dest) 'gtk:target-list))
    (is (equal '(("INTEGER" NIL 0)
                 ("STRING" NIL 1)
                 ("text/plain" NIL 1)
                 ("application/x-rootwindow-drop" NIL 2))
               (gtk:target-table-new-from-list
                   (gtk:drag-dest-target-list dest))))))

;;;     gtk_drag_dest_add_text_targets
;;;     gtk_drag_dest_add_image_targets
;;;     gtk_drag_dest_add_uri_targets

;;;     gtk_drag_dest_set_track_motion
;;;     gtk_drag_dest_get_track_motion
;;;     gtk_drag_finish
;;;     gtk_drag_get_data
;;;     gtk_drag_get_source_widget
;;;     gtk_drag_highlight
;;;     gtk_drag_unhighlight
;;;     gtk_drag_begin
;;;     gtk_drag_begin_with_coordinates
;;;     gtk_drag_cancel
;;;     gtk_drag_set_icon_widget
;;;     gtk_drag_set_icon_pixbuf
;;;     gtk_drag_set_icon_stock
;;;     gtk_drag_set_icon_surface
;;;     gtk_drag_set_icon_name
;;;     gtk_drag_set_icon_gicon
;;;     gtk_drag_set_icon_default
;;;     gtk_drag_check_threshold

;;;     gtk_drag_source_set

(test gtk-drag-source-set
  (let ((tlist (list (list "INTEGER" 0 0)
                     (list "STRING" 0 1)
                     (list "text/plain" 0 1)
                     (list "application/x-rootwindow-drop" 0 2)))
        (source (make-instance 'gtk:button)))
    (is-false (gtk:drag-source-set source        ; widget will be dragable
                                   :button1-mask ; modifier that will start drag
                                   tlist         ; lists of targets to support
                                   :copy))       ; what to do with dropped data
))

;;;     gtk_drag_source_set_icon_pixbuf
;;;     gtk_drag_source_set_icon_stock
;;;     gtk_drag_source_set_icon_name
;;;     gtk_drag_source_set_icon_gicon

;;;     gtk_drag_source_unset

;; TODO: Fix the example, we get no gtk:target-list

(test gtk-drag-source-unset
  (let ((tlist (list (list "INTEGER" 0 0)
                     (list "STRING" 0 1)
                     (list "text/plain" 0 1)
                     (list "application/x-rootwindow-drop" 0 2)))
        (source (make-instance 'gtk:button)))
    (is-false (gtk:drag-source-set source        ; widget will be dragable
                                   :button1-mask ; modifier that will start drag
                                   tlist         ; lists of targets to support
                                   :copy))       ; what to do with dropped data
    (is (typep (gtk:drag-source-target-list source) 'gtk:target-list))
    (is (equal '(("INTEGER" NIL 0)
                 ("STRING" NIL 1)
                 ("text/plain" NIL 1)
                 ("application/x-rootwindow-drop" NIL 2))
               (gtk:target-table-new-from-list
                   (gtk:drag-source-target-list source))))
    (is-false (gtk:drag-source-unset source))
    ;; No gtk:target-list
    (is-false (gtk:drag-source-target-list source))
))

;;;     gtk_drag_source_set_target_list
;;;     gtk_drag_source_get_target_list

(test gtk-drag-source-target-list
  (let ((tlist (list (list "INTEGER" 0 0)
                     (list "STRING" 0 1)
                     (list "text/plain" 0 1)
                     (list "application/x-rootwindow-drop" 0 2)))
        (source (make-instance 'gtk:button)))
    (is-false (gtk:drag-source-set source        ; widget will be dragable
                                   :button1-mask ; modifier that will start drag
                                   tlist         ; lists of targets to support
                                   :copy))       ; what to do with dropped data
    (is (typep (gtk:drag-source-target-list source) 'gtk:target-list))
    (is (equal '(("INTEGER" NIL 0)
                 ("STRING" NIL 1)
                 ("text/plain" NIL 1)
                 ("application/x-rootwindow-drop" NIL 2))
               (gtk:target-table-new-from-list
                   (gtk:drag-source-target-list source))))))

;;;     gtk_drag_source_add_text_targets

(test gtk-drag-source-add-text-targets
  (let ((source (make-instance 'gtk:button)))
    (is-false (gtk:drag-source-set source :button1-mask nil :copy))
    (is-false (gtk:drag-source-add-text-targets source))
    #-windows
    (is (equal '(("UTF8_STRING" NIL 0)
                 ("COMPOUND_TEXT" NIL 0)
                 ("TEXT" NIL 0)
                 ("STRING" NIL 0)
                 ("text/plain;charset=utf-8" NIL 0)
                 ("text/plain" NIL 0))
               (gtk:target-table-new-from-list
                   (gtk:drag-source-target-list source))))
    #+windows
    (is (equal '(("UTF8_STRING" NIL 0)
                 ("COMPOUND_TEXT" NIL 0)
                 ("TEXT" NIL 0)
                 ("STRING" NIL 0)
                 ("text/plain;charset=utf-8" NIL 0)
                 ("text/plain;charset=CP1252" NIL 0)
                 ("text/plain" NIL 0))
               (gtk:target-table-new-from-list
                   (gtk:drag-source-target-list source))))))

;;;     gtk_drag_source_add_image_targets

#-windows
(test gtk-drag-source-add-image-targets
  (let ((source (make-instance 'gtk:button)))
    (is-false (gtk:drag-source-set source :button1-mask nil :copy))
    (is-false (gtk:drag-source-add-image-targets source))
    (is (equal '(("image/png" NIL 0)
                 ("image/jpeg" NIL 0)
                 ("image/avif" NIL 0)
                 ("image/bmp" NIL 0)
                 ("image/x-bmp" NIL 0)
                 ("image/x-MS-bmp" NIL 0)
                 ("image/x-icon" NIL 0)
                 ("image/x-ico" NIL 0)
                 ("image/x-win-bitmap" NIL 0)
                 ("image/vnd.microsoft.icon" NIL 0)
                 ("application/ico" NIL 0)
                 ("image/ico" NIL 0)
                 ("image/icon" NIL 0)
                 ("text/ico" NIL 0)
                 ("image/jxl" NIL 0)
                 ("image/tiff" NIL 0)
                 ("image/webp" NIL 0)
                 ("audio/x-riff" NIL 0))
               (gtk:target-table-new-from-list
                   (gtk:drag-source-target-list source))))))

#+windows
(test gtk-drag-source-add-image-targets
  (let ((source (make-instance 'gtk:button)))
    (is-false (gtk:drag-source-set source :button1-mask nil :copy))
    (is-false (gtk:drag-source-add-image-targets source))
    (is (equal '(("image/png" NIL 0)
                 ("image/bmp" NIL 0)
                 ("image/x-bmp" NIL 0)
                 ("image/x-MS-bmp" NIL 0)
                 ("image/x-icon" NIL 0)
                 ("image/x-ico" NIL 0)
                 ("image/x-win-bitmap" NIL 0)
                 ("image/vnd.microsoft.icon" NIL 0)
                 ("application/ico" NIL 0)
                 ("image/ico" NIL 0)
                 ("image/icon" NIL 0)
                 ("text/ico" NIL 0)
                 ("image/jpeg" NIL 0)
                 ("image/tiff" NIL 0)
                 ("image/bmp" NIL 0)
                 ("image/x-bmp" NIL 0)
                 ("image/x-MS-bmp" NIL 0)
                 ("image/x-icon" NIL 0)
                 ("image/x-ico" NIL 0)
                 ("image/x-win-bitmap" NIL 0)
                 ("image/vnd.microsoft.icon" NIL 0)
                 ("application/ico" NIL 0)
                 ("image/ico" NIL 0)
                 ("image/icon" NIL 0)
                 ("text/ico" NIL 0)
                 ("image/jpeg" NIL 0)
                 ("image/jxl" NIL 0)
                 ("image/png" NIL 0)
                 ("image/tiff" NIL 0))
               (gtk:target-table-new-from-list
                   (gtk:drag-source-target-list source))))))

;;;     gtk_drag_source_add_uri_targets

#-windows
(test gtk-drag-source-add-uri-targets
  (let ((source (make-instance 'gtk:button)))
    (is-false (gtk:drag-source-set source :button1-mask nil :copy))
    (is-false (gtk:drag-source-add-uri-targets source))
    (is (equal '(("text/uri-list" NIL 0)
                 ("application/vnd.portal.filetransfer" NIL 0)
                 ("application/vnd.portal.files" NIL 0))
               (gtk:target-table-new-from-list
                   (gtk:drag-source-target-list source))))))

;;; 2025-06-02
