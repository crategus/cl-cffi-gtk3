(in-package :gtk-test)

(def-suite gdk-drag-and-drop :in gdk-suite)
(in-suite gdk-drag-and-drop)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDragCancelReason

(test gdk-drag-cancel-reason
  ;; Check type
  (is (g:type-is-enum "GdkDragCancelReason"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDragCancelReason")
          (g:gtype
              (cffi:foreign-funcall "gdk_drag_cancel_reason_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:drag-cancel-reason
          (glib:symbol-for-gtype "GdkDragCancelReason")))
  ;; Check names
  (is (equal '("GDK_DRAG_CANCEL_NO_TARGET" "GDK_DRAG_CANCEL_USER_CANCELLED"
               "GDK_DRAG_CANCEL_ERROR")
             (glib-test:list-enum-item-names "GdkDragCancelReason")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GdkDragCancelReason")))
  ;; Check nick names
  (is (equal '("no-target" "user-cancelled" "error")
             (glib-test:list-enum-item-nicks "GdkDragCancelReason")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkDragCancelReason" GDK:DRAG-CANCEL-REASON
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_drag_cancel_reason_get_type")
                       (:NO-TARGET 0)
                       (:USER-CANCELLED 1)
                       (:ERROR 2))
             (gobject:get-gtype-definition "GdkDragCancelReason"))))

;;;     GdkDragProtocol

(test gdk-drag-protocol
  ;; Check type
  (is (g:type-is-enum "GdkDragProtocol"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDragProtocol")
          (g:gtype (cffi:foreign-funcall "gdk_drag_protocol_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:drag-protocol
          (glib:symbol-for-gtype "GdkDragProtocol")))
  ;; Check names
  (is (equal '("GDK_DRAG_PROTO_NONE" "GDK_DRAG_PROTO_MOTIF"
               "GDK_DRAG_PROTO_XDND" "GDK_DRAG_PROTO_ROOTWIN"
               "GDK_DRAG_PROTO_WIN32_DROPFILES" "GDK_DRAG_PROTO_OLE2"
               "GDK_DRAG_PROTO_LOCAL" "GDK_DRAG_PROTO_WAYLAND")
             (glib-test:list-enum-item-names "GdkDragProtocol")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7)
             (glib-test:list-enum-item-values "GdkDragProtocol")))
  ;; Check nick names
  (is (equal '("none" "motif" "xdnd" "rootwin" "win32-dropfiles" "ole2" "local"
               "wayland")
             (glib-test:list-enum-item-nicks "GdkDragProtocol")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkDragProtocol" GDK:DRAG-PROTOCOL
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_drag_protocol_get_type")
                       (:NONE 0)
                       (:MOTIF 1)
                       (:XDND 2)
                       (:ROOTWIN 3)
                       (:WIN32-DROPFILES 4)
                       (:OLE2 5)
                       (:LOCAL 6)
                       (:WAYLAND 7))
             (gobject:get-gtype-definition "GdkDragProtocol"))))

;;;     GdkDragAction

(test gdk-drag-action
  ;; Check type
  (is (g:type-is-flags "GdkDragAction"))
  ;; Check registered name
  (is (eq 'gdk:drag-action
          (glib:symbol-for-gtype "GdkDragAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDragAction")
          (g:gtype (cffi:foreign-funcall "gdk_drag_action_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_ACTION_DEFAULT" "GDK_ACTION_COPY" "GDK_ACTION_MOVE"
               "GDK_ACTION_LINK" "GDK_ACTION_PRIVATE" "GDK_ACTION_ASK")
             (glib-test:list-flags-item-names "GdkDragAction")))
  ;; Check values
  (is (equal '(1 2 4 8 16 32)
             (glib-test:list-flags-item-values "GdkDragAction")))
  ;; Check nick names
  (is (equal '("default" "copy" "move" "link" "private" "ask")
             (glib-test:list-flags-item-nicks "GdkDragAction")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkDragAction" GDK:DRAG-ACTION
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_drag_action_get_type")
                       (:DEFAULT 1)
                       (:COPY 2)
                       (:MOVE 4)
                       (:LINK 8)
                       (:PRIVATE 16)
                       (:ASK 32))
             (gobject:get-gtype-definition "GdkDragAction"))))

;;;     GdkDragContext

#-windows
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:foreign-funcall "gdk_x11_drag_context_get_type" :size))

#+windows
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:foreign-funcall "gdk_win32_drag_context_get_type" :size))

(test gdk-drag-context
  ;; Check type
  (is (g:type-is-object "GdkDragContext"))
  ;; Check registered name
  (is (eq 'gdk:drag-context
          (glib:symbol-for-gtype "GdkDragContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDragContext")
          (g:gtype (cffi:foreign-funcall "gdk_drag_context_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GdkDragContext")))
  ;; Check children
  #-windows
  (is (member "GdkX11DragContext"
              (glib-test:list-children "GdkDragContext") :test #'string=))
  #+windows
  (is (equal '("GdkWin32DragContext")
             (glib-test:list-children "GdkDragContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkDragContext")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GdkDragContext")))
  ;; Check signals
  (is (equal '("action-changed" "cancel" "dnd-finished" "drop-performed")
             (glib-test:list-signals "GdkDragContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDragContext" GDK:DRAG-CONTEXT
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_drag_context_get_type")
                       NIL)
             (gobject:get-gtype-definition "GdkDragContext"))))

;;; --- Signals ----------------------------------------------------------------

;;;     void    action-changed    Run Last
;;;     void    cancel            Run Last
;;;     void    dnd-finished      Run Last
;;;     void    drop-performed    Run Last

#+nil
(test gdk-drag-context-signals
  ;; Check the list of signals
  (is (equal '("action-changed" "cancel" "dnd-finished" "drop-performed")
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GdkDragContext"))
                   #'string<)))
  ;; Query info for the "action-changed" signal
  (let ((query (g-signal-query (g-signal-lookup "action-changed"
                                                "GdkDragContext"))))
    (is (string= "action-changed" (g-signal-query-signal-name query)))
    (is (string= "GdkDragContext"
                 (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g-signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '("GdkDragAction")
               (sort (mapcar #'g-type-name (g-signal-query-param-types query))
                     #'string<)))
    (is-false (g-signal-query-signal-detail query)))
  ;; Query info for the "cancel" signal
  (let ((query (g-signal-query (g-signal-lookup "cancel"
                                                "GdkDragContext"))))
    (is (string= "cancel" (g-signal-query-signal-name query)))
    (is (string= "GdkDragContext"
                 (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g-signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '("GdkDragCancelReason")
               (sort (mapcar #'g-type-name (g-signal-query-param-types query))
                     #'string<)))
    (is-false (g-signal-query-signal-detail query)))
  ;; Query info for the "dnd-finished" signal
  (let ((query (g-signal-query (g-signal-lookup "dnd-finished"
                                                "GdkDragContext"))))
    (is (string= "dnd-finished" (g-signal-query-signal-name query)))
    (is (string= "GdkDragContext"
                 (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g-signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g-type-name (g-signal-query-param-types query))
                     #'string<)))
    (is-false (g-signal-query-signal-detail query)))
  ;; Query info for the "drop-performed" signal
  (let ((query (g-signal-query (g-signal-lookup "drop-performed"
                                                "GdkDragContext"))))
    (is (string= "drop-performed" (g-signal-query-signal-name query)))
    (is (string= "GdkDragContext"
                 (g-type-name (g-signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g-signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g-type-name (g-signal-query-return-type query))))
    (is (equal '("gint")
               (sort (mapcar #'g-type-name (g-signal-query-param-types query))
                     #'string<)))
    (is-false (g-signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_drag_get_selection
;;;     gdk_drag_abort
;;;     gdk_drop_reply
;;;     gdk_drag_drop
;;;     gdk_drag_drop_done
;;;     gdk_drag_find_window_for_screen

;;;     gdk_drag_begin

(test gdk-drag-begin
  (let ((widget (make-instance 'gtk:window :type :toplevel)))
    (gtk:widget-realize widget)
    (let ((window (gtk:widget-window widget)))
      (is (typep (gdk:drag-begin window (list "STRING")) 'gdk:drag-context))
      (is (typep (gdk:drag-begin window (list "STRING" "PIXMAP"))
                 'gdk:drag-context)))))

;;;     gdk_drag_begin_for_device
;;;     gdk_drag_begin_from_point
;;;     gdk_drag_motion
;;;     gdk_drop_finish
;;;     gdk_drag_status
;;;     gdk_drag_drop_succeeded
;;;     gdk_window_get_drag_protocol
;;;     gdk_drag_context_get_actions
;;;     gdk_drag_context_get_suggested_action
;;;     gdk_drag_context_get_selected_action

;;;     gdk_drag_context_list_targets

(test gdk-drag-context-list-targets
  (let ((widget (make-instance 'gtk:window :type :toplevel)))
    (gtk:widget-realize widget)
    (let* ((window (gtk:widget-window widget))
           (context (gdk:drag-begin window (list "STRING" "PIXMAP"))))
      (is (equal '("STRING" "PIXMAP")
                 (gdk:drag-context-list-targets context))))))

;;;     gdk_drag_context_get_device
;;;     gdk_drag_context_set_device
;;;     gdk_drag_context_get_source_window
;;;     gdk_drag_context_get_dest_window
;;;     gdk_drag_context_get_protocol
;;;     gdk_drag_context_get_drag_window
;;;     gdk_drag_context_set_hotspot
;;;     gdk_drag_context_manage_dnd

;;; 2024-9-22
