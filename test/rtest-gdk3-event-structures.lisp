(in-package :gtk-test)

(def-suite gdk-event-structures :in gdk-suite)
(in-suite gdk-event-structures)

;;;     GdkScrollDirection

(test gdk-scroll-direction
  ;; Check type
  (is-true (g:type-is-enum "GdkScrollDirection"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkScrollDirection")
          (g:gtype (cffi:foreign-funcall "gdk_scroll_direction_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:scroll-direction
          (glib:symbol-for-gtype "GdkScrollDirection")))
  ;; Check names
  (is (equal '("GDK_SCROLL_UP" "GDK_SCROLL_DOWN" "GDK_SCROLL_LEFT"
               "GDK_SCROLL_RIGHT" "GDK_SCROLL_SMOOTH")
             (glib-test:list-enum-item-names "GdkScrollDirection")))
  ;; Check values
  (is (equal '(0 1 2 3 4)
             (glib-test:list-enum-item-values "GdkScrollDirection")))
  ;; Check nick names
  (is (equal '("up" "down" "left" "right" "smooth")
             (glib-test:list-enum-item-nicks "GdkScrollDirection")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkScrollDirection" GDK:SCROLL-DIRECTION
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_scroll_direction_get_type")
                       (:UP 0)
                       (:DOWN 1)
                       (:LEFT 2)
                       (:RIGHT 3)
                       (:SMOOTH 4))
             (gobject:get-gtype-definition "GdkScrollDirection"))))

;;;     GdkVisibilityState

(test gdk-visibility-state
  ;; Check type
  (is-true (g:type-is-enum "GdkVisibilityState"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkVisibilityState")
          (g:gtype (cffi:foreign-funcall "gdk_visibility_state_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:visibility-state
          (glib:symbol-for-gtype "GdkVisibilityState")))
  ;; Check names
  (is (equal '("GDK_VISIBILITY_UNOBSCURED" "GDK_VISIBILITY_PARTIAL"
               "GDK_VISIBILITY_FULLY_OBSCURED")
             (glib-test:list-enum-item-names "GdkVisibilityState")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GdkVisibilityState")))
  ;; Check nick names
  (is (equal '("unobscured" "partial" "fully-obscured")
             (glib-test:list-enum-item-nicks "GdkVisibilityState")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkVisibilityState" GDK:VISIBILITY-STATE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_visibility_state_get_type")
                       (:UNOBSCURED 0)
                       (:PARTIAL 1)
                       (:FULLY-OBSCURED 2))
             (gobject:get-gtype-definition "GdkVisibilityState"))))

;;;     GdkCrossingMode

(test gdk-crossing-mode
  ;; Check type
  (is-true (g:type-is-enum "GdkCrossingMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkCrossingMode")
          (g:gtype (cffi:foreign-funcall "gdk_crossing_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:crossing-mode
          (glib:symbol-for-gtype "GdkCrossingMode")))
  ;; Check names
  (is (equal '("GDK_CROSSING_NORMAL" "GDK_CROSSING_GRAB" "GDK_CROSSING_UNGRAB"
               "GDK_CROSSING_GTK_GRAB" "GDK_CROSSING_GTK_UNGRAB"
               "GDK_CROSSING_STATE_CHANGED" "GDK_CROSSING_TOUCH_BEGIN"
               "GDK_CROSSING_TOUCH_END" "GDK_CROSSING_DEVICE_SWITCH")
             (glib-test:list-enum-item-names "GdkCrossingMode")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8)
             (glib-test:list-enum-item-values "GdkCrossingMode")))
  ;; Check nick names
  (is (equal '("normal" "grab" "ungrab" "gtk-grab" "gtk-ungrab" "state-changed"
               "touch-begin" "touch-end" "device-switch")
             (glib-test:list-enum-item-nicks "GdkCrossingMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkCrossingMode" GDK:CROSSING-MODE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_crossing_mode_get_type")
                       (:NORMAL 0)
                       (:GRAB 1)
                       (:UNGRAB 2)
                       (:GTK-GRAB 3)
                       (:GTK-UNGRAB 4)
                       (:STATE-CHANGED 5)
                       (:TOUCH-BEGIN 6)
                       (:TOUCH-END 7)
                       (:DEVICE-SWITCH 8))
             (gobject:get-gtype-definition "GdkCrossingMode"))))

;;;     GdkNotifyType

(test gdk-notify-type
  ;; Check type
  (is-true (g:type-is-enum "GdkNotifyType"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkNotifyType")
          (g:gtype (cffi:foreign-funcall "gdk_notify_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:notify-type
          (glib:symbol-for-gtype "GdkNotifyType")))
  ;; Check names
  (is (equal '("GDK_NOTIFY_ANCESTOR" "GDK_NOTIFY_VIRTUAL" "GDK_NOTIFY_INFERIOR"
               "GDK_NOTIFY_NONLINEAR" "GDK_NOTIFY_NONLINEAR_VIRTUAL"
               "GDK_NOTIFY_UNKNOWN")
             (glib-test:list-enum-item-names "GdkNotifyType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (glib-test:list-enum-item-values "GdkNotifyType")))
  ;; Check nick names
  (is (equal '("ancestor" "virtual" "inferior" "nonlinear" "nonlinear-virtual"
               "unknown")
             (glib-test:list-enum-item-nicks "GdkNotifyType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkNotifyType" GDK:NOTIFY-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_notify_type_get_type")
                       (:ANCESTOR 0)
                       (:VIRTUAL 1)
                       (:INFERIOR 2)
                       (:NONLINEAR 3)
                       (:NONLINEAR-VIRTUAL 4)
                       (:UNKNOWN 5))
             (gobject:get-gtype-definition "GdkNotifyType"))))

;;;     GdkPropertyState

(test gdk-property-state
  ;; Check type
  (is-true (g:type-is-enum "GdkPropertyState"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkPropertyState")
          (g:gtype (cffi:foreign-funcall "gdk_property_state_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:property-state
          (glib:symbol-for-gtype "GdkPropertyState")))
  ;; Check names
  (is (equal '("GDK_PROPERTY_NEW_VALUE" "GDK_PROPERTY_DELETE")
             (glib-test:list-enum-item-names "GdkPropertyState")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GdkPropertyState")))
  ;; Check nick names
  (is (equal '("new-value" "delete")
             (glib-test:list-enum-item-nicks "GdkPropertyState")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkPropertyState" GDK:PROPERTY-STATE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_property_state_get_type")
                       (:NEW-VALUE 0)
                       (:DELETE 1))
             (gobject:get-gtype-definition "GdkPropertyState"))))

;;;     GdkWindowState

(test gdk-window-state
  ;; Check type
  (is (g:type-is-flags "GdkWindowState"))
  ;; Check registered name
  (is (eq 'gdk:window-state
          (glib:symbol-for-gtype "GdkWindowState")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkWindowState")
          (g:gtype (cffi:foreign-funcall "gdk_window_state_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_WINDOW_STATE_WITHDRAWN" "GDK_WINDOW_STATE_ICONIFIED"
               "GDK_WINDOW_STATE_MAXIMIZED" "GDK_WINDOW_STATE_STICKY"
               "GDK_WINDOW_STATE_FULLSCREEN" "GDK_WINDOW_STATE_ABOVE"
               "GDK_WINDOW_STATE_BELOW" "GDK_WINDOW_STATE_FOCUSED"
               "GDK_WINDOW_STATE_TILED" "GDK_WINDOW_STATE_TOP_TILED"
               "GDK_WINDOW_STATE_TOP_RESIZABLE" "GDK_WINDOW_STATE_RIGHT_TILED"
               "GDK_WINDOW_STATE_RIGHT_RESIZABLE"
               "GDK_WINDOW_STATE_BOTTOM_TILED"
               "GDK_WINDOW_STATE_BOTTOM_RESIZABLE"
               "GDK_WINDOW_STATE_LEFT_TILED" "GDK_WINDOW_STATE_LEFT_RESIZABLE")
             (glib-test:list-flags-item-names "GdkWindowState")))
  ;; Check values
  (is (equal '(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768
               65536)
             (glib-test:list-flags-item-values "GdkWindowState")))
  ;; Check nick names
  (is (equal '("withdrawn" "iconified" "maximized" "sticky" "fullscreen" "above"
               "below" "focused" "tiled" "top-tiled" "top-resizable"
               "right-tiled" "right-resizable" "bottom-tiled" "bottom-resizable"
               "left-tiled" "left-resizable")
             (glib-test:list-flags-item-nicks "GdkWindowState")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkWindowState" GDK:WINDOW-STATE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_window_state_get_type")
                       (:WITHDRAWN 1)
                       (:ICONIFIED 2)
                       (:MAXIMIZED 4)
                       (:STICKY 8)
                       (:FULLSCREEN 16)
                       (:ABOVE 32)
                       (:BELOW 64)
                       (:FOCUSED 128)
                       (:TILED 256)
                       (:TOP-TILED 512)
                       (:TOP-RESIZABLE 1024)
                       (:RIGHT-TILED 2048)
                       (:RIGHT-RESIZABLE 4096)
                       (:BOTTOM-TILED 8192)
                       (:BOTTOM-RESIZABLE 16384)
                       (:LEFT-TILED 32768)
                       (:LEFT-RESIZABLE 65536))
             (gobject:get-gtype-definition "GdkWindowState"))))

;;;     GdkSettingAction

(test gdk-setting-action
  ;; Check type
  (is-true (g:type-is-enum "GdkSettingAction"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkSettingAction")
          (g:gtype (cffi:foreign-funcall "gdk_setting_action_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:setting-action
          (glib:symbol-for-gtype "GdkSettingAction")))
  ;; Check names
  (is (equal '("GDK_SETTING_ACTION_NEW" "GDK_SETTING_ACTION_CHANGED"
               "GDK_SETTING_ACTION_DELETED")
             (glib-test:list-enum-item-names "GdkSettingAction")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GdkSettingAction")))
  ;; Check nick names
  (is (equal '("new" "changed" "deleted")
             (glib-test:list-enum-item-nicks "GdkSettingAction")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkSettingAction" GDK:SETTING-ACTION
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_setting_action_get_type")
                       (:NEW 0)
                       (:CHANGED 1)
                       (:DELETED 2))
             (gobject:get-gtype-definition "GdkSettingAction"))))

;;;     GdkOwnerChange

(test gdk-owner-change
  ;; Check type
  (is-true (g:type-is-enum "GdkOwnerChange"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkOwnerChange")
          (g:gtype (cffi:foreign-funcall "gdk_owner_change_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:owner-change
          (glib:symbol-for-gtype "GdkOwnerChange")))
  ;; Check names
  (is (equal '("GDK_OWNER_CHANGE_NEW_OWNER" "GDK_OWNER_CHANGE_DESTROY"
               "GDK_OWNER_CHANGE_CLOSE")
             (glib-test:list-enum-item-names "GdkOwnerChange")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GdkOwnerChange")))
  ;; Check nick names
  (is (equal '("new-owner" "destroy" "close")
             (glib-test:list-enum-item-nicks "GdkOwnerChange")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkOwnerChange" GDK:OWNER-CHANGE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_owner_change_get_type")
                       (:NEW-OWNER 0)
                       (:DESTROY 1)
                       (:CLOSE 2))
             (gobject:get-gtype-definition "GdkOwnerChange"))))

;;;     GdkEventType        <-- gdk.events.lisp

(test gdk-event-type
  ;; Check type
  (is-true (g:type-is-enum "GdkEventType"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkEventType")
          (g:gtype (cffi:foreign-funcall "gdk_event_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:event-type
          (glib:symbol-for-gtype "GdkEventType")))
  ;; Check names
  (is (equal '("GDK_NOTHING" "GDK_DELETE" "GDK_DESTROY" "GDK_EXPOSE"
               "GDK_MOTION_NOTIFY" "GDK_BUTTON_PRESS" "GDK_2BUTTON_PRESS"
               "GDK_DOUBLE_BUTTON_PRESS" "GDK_3BUTTON_PRESS"
               "GDK_TRIPLE_BUTTON_PRESS" "GDK_BUTTON_RELEASE" "GDK_KEY_PRESS"
               "GDK_KEY_RELEASE" "GDK_ENTER_NOTIFY" "GDK_LEAVE_NOTIFY"
               "GDK_FOCUS_CHANGE" "GDK_CONFIGURE" "GDK_MAP" "GDK_UNMAP"
               "GDK_PROPERTY_NOTIFY" "GDK_SELECTION_CLEAR"
               "GDK_SELECTION_REQUEST" "GDK_SELECTION_NOTIFY" "GDK_PROXIMITY_IN"
               "GDK_PROXIMITY_OUT" "GDK_DRAG_ENTER" "GDK_DRAG_LEAVE"
               "GDK_DRAG_MOTION" "GDK_DRAG_STATUS" "GDK_DROP_START"
               "GDK_DROP_FINISHED" "GDK_CLIENT_EVENT" "GDK_VISIBILITY_NOTIFY"
               "GDK_SCROLL" "GDK_WINDOW_STATE" "GDK_SETTING" "GDK_OWNER_CHANGE"
               "GDK_GRAB_BROKEN" "GDK_DAMAGE" "GDK_TOUCH_BEGIN"
               "GDK_TOUCH_UPDATE" "GDK_TOUCH_END" "GDK_TOUCH_CANCEL"
               "GDK_TOUCHPAD_SWIPE" "GDK_TOUCHPAD_PINCH" "GDK_PAD_BUTTON_PRESS"
               "GDK_PAD_BUTTON_RELEASE" "GDK_PAD_RING" "GDK_PAD_STRIP"
               "GDK_PAD_GROUP_MODE" "GDK_EVENT_LAST")
             (glib-test:list-enum-item-names "GdkEventType")))
  ;; Check values
  (is (equal '(-1 0 1 2 3 4 5 5 6 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
               22 23 24 25 26 27 28 29 31 32 33 34 35 36 37 38 39 40 41 42 43
               44 45 46 47 48)
             (glib-test:list-enum-item-values "GdkEventType")))
  ;; Check nick names
  (is (equal '("nothing" "delete" "destroy" "expose" "motion-notify"
               "button-press" "2button-press" "double-button-press"
               "3button-press" "triple-button-press" "button-release"
               "key-press" "key-release" "enter-notify" "leave-notify"
               "focus-change" "configure" "map" "unmap" "property-notify"
               "selection-clear" "selection-request" "selection-notify"
               "proximity-in" "proximity-out" "drag-enter" "drag-leave"
               "drag-motion" "drag-status" "drop-start" "drop-finished"
               "client-event" "visibility-notify" "scroll" "window-state"
               "setting" "owner-change" "grab-broken" "damage" "touch-begin"
               "touch-update" "touch-end" "touch-cancel" "touchpad-swipe"
               "touchpad-pinch" "pad-button-press" "pad-button-release"
               "pad-ring" "pad-strip" "pad-group-mode" "event-last")
             (glib-test:list-enum-item-nicks "GdkEventType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkEventType" GDK:EVENT-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_event_type_get_type")
                       (:NOTHING -1)
                       (:DELETE 0)
                       (:DESTROY 1)
                       (:EXPOSE 2)
                       (:MOTION-NOTIFY 3)
                       (:BUTTON-PRESS 4)
                       (:2BUTTON-PRESS 5)
                       (:DOUBLE-BUTTON-PRESS 5)
                       (:3BUTTON-PRESS 6)
                       (:TRIPLE-BUTTON-PRESS 6)
                       (:BUTTON-RELEASE 7)
                       (:KEY-PRESS 8)
                       (:KEY-RELEASE 9)
                       (:ENTER-NOTIFY 10)
                       (:LEAVE-NOTIFY 11)
                       (:FOCUS-CHANGE 12)
                       (:CONFIGURE 13)
                       (:MAP 14)
                       (:UNMAP 15)
                       (:PROPERTY-NOTIFY 16)
                       (:SELECTION-CLEAR 17)
                       (:SELECTION-REQUEST 18)
                       (:SELECTION-NOTIFY 19)
                       (:PROXIMITY-IN 20)
                       (:PROXIMITY-OUT 21)
                       (:DRAG-ENTER 22)
                       (:DRAG-LEAVE 23)
                       (:DRAG-MOTION 24)
                       (:DRAG-STATUS 25)
                       (:DROP-START 26)
                       (:DROP-FINISHED 27)
                       (:CLIENT-EVENT 28)
                       (:VISIBILITY-NOTIFY 29)
                       (:SCROLL 31)
                       (:WINDOW-STATE 32)
                       (:SETTING 33)
                       (:OWNER-CHANGE 34)
                       (:GRAB-BROKEN 35)
                       (:DAMAGE 36)
                       (:TOUCH-BEGIN 37)
                       (:TOUCH-UPDATE 38)
                       (:TOUCH-END 39)
                       (:TOUCH-CANCEL 40)
                       (:TOUCHPAD-SWIPE 41)
                       (:TOUCHPAD-PINCH 42)
                       (:PAD-BUTTON-PRESS 43)
                       (:PAD-BUTTON-RELEASE 44)
                       (:PAD-RING 45)
                       (:PAD-STRIP 46)
                       (:PAD-GROUP-MODE 47)
                       (:EVENT-LAST 48))
             (gobject:get-gtype-definition "GdkEventType"))))

;;;     GdkModifierType     <-- gdk.window.lisp

(test gdk-modifier-type
  ;; Check type
  (is (g:type-is-flags "GdkModifierType"))
  ;; Check registered name
  (is (eq 'gdk:modifier-type
          (glib:symbol-for-gtype "GdkModifierType")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkModifierType")
          (g:gtype (cffi:foreign-funcall "gdk_modifier_type_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_SHIFT_MASK" "GDK_LOCK_MASK" "GDK_CONTROL_MASK"
               "GDK_MOD1_MASK" "GDK_MOD2_MASK" "GDK_MOD3_MASK" "GDK_MOD4_MASK"
               "GDK_MOD5_MASK" "GDK_BUTTON1_MASK" "GDK_BUTTON2_MASK"
               "GDK_BUTTON3_MASK" "GDK_BUTTON4_MASK" "GDK_BUTTON5_MASK"
               "GDK_MODIFIER_RESERVED_13_MASK" "GDK_MODIFIER_RESERVED_14_MASK"
               "GDK_MODIFIER_RESERVED_15_MASK" "GDK_MODIFIER_RESERVED_16_MASK"
               "GDK_MODIFIER_RESERVED_17_MASK" "GDK_MODIFIER_RESERVED_18_MASK"
               "GDK_MODIFIER_RESERVED_19_MASK" "GDK_MODIFIER_RESERVED_20_MASK"
               "GDK_MODIFIER_RESERVED_21_MASK" "GDK_MODIFIER_RESERVED_22_MASK"
               "GDK_MODIFIER_RESERVED_23_MASK" "GDK_MODIFIER_RESERVED_24_MASK"
               "GDK_MODIFIER_RESERVED_25_MASK" "GDK_SUPER_MASK" "GDK_HYPER_MASK"
               "GDK_META_MASK" "GDK_MODIFIER_RESERVED_29_MASK"
               "GDK_RELEASE_MASK" "GDK_MODIFIER_MASK")
             (glib-test:list-flags-item-names "GdkModifierType")))
  ;; Check values
  (is (equal '(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768
               65536 131072 262144 524288 1048576 2097152 4194304 8388608
               16777216 33554432 67108864 134217728 268435456 536870912
               1073741824 1543512063)
             (glib-test:list-flags-item-values "GdkModifierType")))
  ;; Check nick names
  (is (equal '("shift-mask" "lock-mask" "control-mask" "mod1-mask" "mod2-mask"
               "mod3-mask" "mod4-mask" "mod5-mask" "button1-mask" "button2-mask"
               "button3-mask" "button4-mask" "button5-mask"
               "modifier-reserved-13-mask" "modifier-reserved-14-mask"
               "modifier-reserved-15-mask" "modifier-reserved-16-mask"
               "modifier-reserved-17-mask" "modifier-reserved-18-mask"
               "modifier-reserved-19-mask" "modifier-reserved-20-mask"
               "modifier-reserved-21-mask" "modifier-reserved-22-mask"
               "modifier-reserved-23-mask" "modifier-reserved-24-mask"
               "modifier-reserved-25-mask" "super-mask" "hyper-mask" "meta-mask"
               "modifier-reserved-29-mask" "release-mask" "modifier-mask")
             (glib-test:list-flags-item-nicks "GdkModifierType")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkModifierType" GDK:MODIFIER-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_modifier_type_get_type")
                       (:SHIFT-MASK 1)
                       (:LOCK-MASK 2)
                       (:CONTROL-MASK 4)
                       (:MOD1-MASK 8)
                       (:MOD2-MASK 16)
                       (:MOD3-MASK 32)
                       (:MOD4-MASK 64)
                       (:MOD5-MASK 128)
                       (:BUTTON1-MASK 256)
                       (:BUTTON2-MASK 512)
                       (:BUTTON3-MASK 1024)
                       (:BUTTON4-MASK 2048)
                       (:BUTTON5-MASK 4096)
                       (:MODIFIER-RESERVED-13-MASK 8192)
                       (:MODIFIER-RESERVED-14-MASK 16384)
                       (:MODIFIER-RESERVED-15-MASK 32768)
                       (:MODIFIER-RESERVED-16-MASK 65536)
                       (:MODIFIER-RESERVED-17-MASK 131072)
                       (:MODIFIER-RESERVED-18-MASK 262144)
                       (:MODIFIER-RESERVED-19-MASK 524288)
                       (:MODIFIER-RESERVED-20-MASK 1048576)
                       (:MODIFIER-RESERVED-21-MASK 2097152)
                       (:MODIFIER-RESERVED-22-MASK 4194304)
                       (:MODIFIER-RESERVED-23-MASK 8388608)
                       (:MODIFIER-RESERVED-24-MASK 16777216)
                       (:MODIFIER-RESERVED-25-MASK 33554432)
                       (:SUPER-MASK 67108864)
                       (:HYPER-MASK 134217728)
                       (:META-MASK 268435456)
                       (:MODIFIER-RESERVED-29-MASK 536870912)
                       (:RELEASE-MASK 1073741824)
                       (:MODIFIER-MASK 1543512063))
             (gobject:get-gtype-definition "GdkModifierType"))))

;;;     GdkEventMask        <-- gdk.events.lisp

(test gdk-event-mask
  ;; Check type
  (is (g:type-is-flags "GdkEventMask"))
  ;; Check registered name
  (is (eq 'gdk:event-mask
          (glib:symbol-for-gtype "GdkEventMask")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkEventMask")
          (g:gtype (cffi:foreign-funcall "gdk_event_mask_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_EXPOSURE_MASK" "GDK_POINTER_MOTION_MASK"
               "GDK_POINTER_MOTION_HINT_MASK" "GDK_BUTTON_MOTION_MASK"
               "GDK_BUTTON1_MOTION_MASK" "GDK_BUTTON2_MOTION_MASK"
               "GDK_BUTTON3_MOTION_MASK" "GDK_BUTTON_PRESS_MASK"
               "GDK_BUTTON_RELEASE_MASK" "GDK_KEY_PRESS_MASK"
               "GDK_KEY_RELEASE_MASK" "GDK_ENTER_NOTIFY_MASK"
               "GDK_LEAVE_NOTIFY_MASK" "GDK_FOCUS_CHANGE_MASK"
               "GDK_STRUCTURE_MASK" "GDK_PROPERTY_CHANGE_MASK"
               "GDK_VISIBILITY_NOTIFY_MASK" "GDK_PROXIMITY_IN_MASK"
               "GDK_PROXIMITY_OUT_MASK" "GDK_SUBSTRUCTURE_MASK"
               "GDK_SCROLL_MASK" "GDK_TOUCH_MASK" "GDK_SMOOTH_SCROLL_MASK"
               "GDK_TOUCHPAD_GESTURE_MASK" "GDK_TABLET_PAD_MASK"
               "GDK_ALL_EVENTS_MASK")
             (glib-test:list-flags-item-names "GdkEventMask")))
  ;; Check values
  (is (equal '(2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536
               131072 262144 524288 1048576 2097152 4194304 8388608 16777216
               33554432 67108862)
             (glib-test:list-flags-item-values "GdkEventMask")))
  ;; Check nick names
  (is (equal '("exposure-mask" "pointer-motion-mask" "pointer-motion-hint-mask"
               "button-motion-mask" "button1-motion-mask" "button2-motion-mask"
               "button3-motion-mask" "button-press-mask" "button-release-mask"
               "key-press-mask" "key-release-mask" "enter-notify-mask"
               "leave-notify-mask" "focus-change-mask" "structure-mask"
               "property-change-mask" "visibility-notify-mask"
               "proximity-in-mask" "proximity-out-mask" "substructure-mask"
               "scroll-mask" "touch-mask" "smooth-scroll-mask"
               "touchpad-gesture-mask" "tablet-pad-mask" "all-events-mask")
             (glib-test:list-flags-item-nicks "GdkEventMask")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkEventMask" GDK:EVENT-MASK
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_event_mask_get_type")
                       (:EXPOSURE-MASK 2)
                       (:POINTER-MOTION-MASK 4)
                       (:POINTER-MOTION-HINT-MASK 8)
                       (:BUTTON-MOTION-MASK 16)
                       (:BUTTON1-MOTION-MASK 32)
                       (:BUTTON2-MOTION-MASK 64)
                       (:BUTTON3-MOTION-MASK 128)
                       (:BUTTON-PRESS-MASK 256)
                       (:BUTTON-RELEASE-MASK 512)
                       (:KEY-PRESS-MASK 1024)
                       (:KEY-RELEASE-MASK 2048)
                       (:ENTER-NOTIFY-MASK 4096)
                       (:LEAVE-NOTIFY-MASK 8192)
                       (:FOCUS-CHANGE-MASK 16384)
                       (:STRUCTURE-MASK 32768)
                       (:PROPERTY-CHANGE-MASK 65536)
                       (:VISIBILITY-NOTIFY-MASK 131072)
                       (:PROXIMITY-IN-MASK 262144)
                       (:PROXIMITY-OUT-MASK 524288)
                       (:SUBSTRUCTURE-MASK 1048576)
                       (:SCROLL-MASK 2097152)
                       (:TOUCH-MASK 4194304)
                       (:SMOOTH-SCROLL-MASK 8388608)
                       (:TOUCHPAD-GESTURE-MASK 16777216)
                       (:TABLET-PAD-MASK 33554432)
                       (:ALL-EVENTS-MASK 67108862))
             (gobject:get-gtype-definition "GdkEventMask"))))

;;;     GdkEventSequence    <-- gdk-events.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:foreign-funcall "gdk_event_sequence_get_type" :size))

(test gdk-event-sequence-boxed
  ;; Check type
  (is (g:type-is-boxed "GdkEventSequence"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkEventSequence")
          (g:gtype (cffi:foreign-funcall "gdk_event_sequence_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:event-sequence
          (glib:symbol-for-gtype "GdkEventSequence"))))

;;;     GdkEvent
;;;     GdkEventAny

(test gdk-event-any
  (let ((event (gdk:event-new :nothing)))
    (is (typep event 'gdk:event))
    ;; Common slots
    (is (eq :nothing (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))))

;;;     GdkEventKey

(test gdk-event-key.1
  (let ((event (gdk:event-new :key-press)))
    (is (typep event 'gdk:event-key))
    ;; Common slots
    (is (eq :key-press (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-key
    (is (= 0 (gdk:event-key-time event)))
    (is (= 0 (gdk:event-key-state event)))
    (is (= 0 (gdk:event-key-keyval event)))
    (is (= 0 (gdk:event-key-length event)))
    (is (string= "" (gdk:event-key-string event)))
    (is (= 0 (gdk:event-key-hardware-keycode event)))
    (is (= 0 (gdk:event-key-group event)))
    (is (= 0 (gdk:event-key-is-modifier event)))))

(test gdk-event-key.2
  (let ((event (gdk:event-new :key-release)))
    (is (typep event 'gdk:event-key))
    ;; Common slots
    (is (eq :key-release (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-key
    (is (= 0 (gdk:event-key-time event)))
    (is (= 0 (gdk:event-key-state event)))
    (is (= 0 (gdk:event-key-keyval event)))
    (is (= 0 (gdk:event-key-length event)))
    (is (string= "" (gdk:event-key-string event)))
    (is (= 0 (gdk:event-key-hardware-keycode event)))
    (is (= 0 (gdk:event-key-group event)))
    (is (= 0 (gdk:event-key-is-modifier event)))))

;;;     GdkEventButton

(test gdk-event-button.1
  (let ((event (gdk:event-new :button-press)))
    (is (typep event 'gdk:event-button))
    ;; Common slots
    (is (eq :button-press (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-button
    (is (= 0 (gdk:event-button-time event)))
    (is (= 0.0d0 (gdk:event-button-x event)))
    (is (= 0.0d0 (gdk:event-button-y event)))
    (is (equal '(0.0d0 0.0d0) (gdk:event-button-axes event)))
    (is (= 0 (gdk:event-button-state event)))
    (is (= 0 (gdk:event-button-button event)))
    (is (cffi:null-pointer-p (gdk:event-button-device event)))
    (is (= 0.0d0 (gdk:event-button-x-root event)))
    (is (= 0.0d0 (gdk:event-button-y-root event)))))

(test gdk-event-button.2
  (let ((event (gdk:event-new :double-button-press)))
    (is (typep event 'gdk:event-button))
    ;; Common slots
    (is (eq :double-button-press (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-button
    (is (= 0 (gdk:event-button-time event)))
    (is (= 0.0d0 (gdk:event-button-x event)))
    (is (= 0.0d0 (gdk:event-button-y event)))
    (is (equal '(0.0d0 0.0d0) (gdk:event-button-axes event)))
    (is (= 0 (gdk:event-button-state event)))
    (is (= 0 (gdk:event-button-button event)))
    (is (cffi:null-pointer-p (gdk:event-button-device event)))
    (is (= 0.0d0 (gdk:event-button-x-root event)))
    (is (= 0.0d0 (gdk:event-button-y-root event)))))

(test gdk-event-button.3
  (let ((event (gdk:event-new :triple-button-press)))
    (is (typep event 'gdk:event-button))
    ;; Common slots
    (is (eq :triple-button-press (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-button
    (is (= 0 (gdk:event-button-time event)))
    (is (= 0.0d0 (gdk:event-button-x event)))
    (is (= 0.0d0 (gdk:event-button-y event)))
    (is (equal '(0.0d0 0.0d0) (gdk:event-button-axes event)))
    (is (= 0 (gdk:event-button-state event)))
    (is (= 0 (gdk:event-button-button event)))
    (is (cffi:null-pointer-p (gdk:event-button-device event)))
    (is (= 0.0d0 (gdk:event-button-x-root event)))
    (is (= 0.0d0 (gdk:event-button-y-root event)))))

(test gdk-event-button.4
  (let ((event (gdk:event-new :button-release)))
    (is (typep event 'gdk:event-button))
    ;; Common slots
    (is (eq :button-release (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-button
    (is (= 0 (gdk:event-button-time event)))
    (is (= 0.0d0 (gdk:event-button-x event)))
    (is (= 0.0d0 (gdk:event-button-y event)))
    (is (equal '(0.0d0 0.0d0) (gdk:event-button-axes event)))
    (is (= 0 (gdk:event-button-state event)))
    (is (= 0 (gdk:event-button-button event)))
    (is (cffi:null-pointer-p (gdk:event-button-device event)))
    (is (= 0.0d0 (gdk:event-button-x-root event)))
    (is (= 0.0d0 (gdk:event-button-y-root event)))))

;;;     GdkEventTouch

(test gdk-event-touch
  (let ((event (gdk:event-new :touch-begin)))
    (is (typep event 'gdk:event-touch))
    ;; Common slots
    (is (eq :touch-begin (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-touch
    (is (= 0 (gdk:event-touch-time event)))
    (is (= 0 (gdk:event-touch-x event)))
    (is (= 0 (gdk:event-touch-y event)))
    (is (equal '(0.0d0 0.0d0) (gdk:event-touch-axes event)))
    (is-false (gdk:event-touch-state event))
    ;; FIXME: We can not initialize sequence from Lisp side
    (is-false (gdk:event-touch-sequence event))
    (is-false (gdk:event-touch-emulating-pointer event))
    (is-false (gdk:event-touch-device event))
    (is (= 0 (gdk:event-touch-x-root event)))
    (is (= 0 (gdk:event-touch-y-root event)))))

;;;     GdkEventScroll

(test gdk-event-scroll
  (let ((event (gdk:event-new :scroll)))
    (is (typep event 'gdk:event-scroll))
    ;; Common slots
    (is (eq :scroll (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-scroll
    (is (= 0 (gdk:event-scroll-time event)))
    (is (= 0 (gdk:event-scroll-x event)))
    (is (= 0 (gdk:event-scroll-y event)))
    (is-false (gdk:event-scroll-state event))
    (is (eq :up (gdk:event-scroll-direction event)))
    (is-false (gdk:event-scroll-device event))
    (is (= 0 (gdk:event-scroll-x-root event)))
    (is (= 0 (gdk:event-scroll-y-root event)))
    (is (= 0 (gdk:event-scroll-delta-x event)))
    (is (= 0 (gdk:event-scroll-delta-y event)))))

;;;     GdkEventMotion

(test gdk-event-motion
  (let ((event (gdk:event-new :motion-notify)))
    (is (typep event 'gdk:event-motion))
    ;; Common slots
    (is (eq :motion-notify (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-motion
    (is (= 0 (gdk:event-motion-time event)))
    (is (= 0.0d0 (gdk:event-motion-x event)))
    (is (= 0.0d0 (gdk:event-motion-y event)))
    (is (equal '(0.0d0 0.0d0) (gdk:event-motion-axes event)))
    (is (= 0 (gdk:event-motion-state event)))
    (is (= 0 (gdk:event-motion-is-hint event)))
    (is (cffi:null-pointer-p (gdk:event-motion-device event)))
    (is (= 0.0d0 (gdk:event-motion-x-root event)))
    (is (= 0.0d0 (gdk:event-motion-y-root event)))))

;;;     GdkEventExpose

(test gdk-event-expose
  (let ((event (gdk:event-new :expose)))
    (is (typep event 'gdk:event-expose))
    ;; Common slots
    (is (eq :expose (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-expose
    (is (typep (gdk:event-expose-area event) 'gdk:rectangle))
    (is-true (cffi:null-pointer-p (gdk:event-expose-region event)))
    (is (= 0 (gdk:event-expose-count event)))))

;;;     GdkEventVisibility

(test gdk-event-visibility
  (let ((event (gdk:event-new :visibility-notify)))
    (is (typep event 'gdk:event-visibility))
    ;; Common slots
    (is (eq :visibility-notify (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-visibility
    (is (eq :unobscured (gdk:event-visibility-state event)))))

;;;     GdkEventCrossing

(test gdk-event-crossing
  (let ((event (gdk:event-new :enter-notify)))
    (is (typep event 'gdk:event-crossing))
    ;; Common slots
    (is (eq :enter-notify (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-crossing
    (is (cffi:null-pointer-p (gdk:event-crossing-subwindow event)))
    (is (= 0 (gdk:event-crossing-time event)))
    (is (= 0.0d0 (gdk:event-crossing-x event)))
    (is (= 0.0d0 (gdk:event-crossing-y event)))
    (is (= 0.0d0 (gdk:event-crossing-x-root event)))
    (is (= 0.0d0 (gdk:event-crossing-y-root event)))
    (is (eq :normal (gdk:event-crossing-mode event)))
    (is (eq :ancestor (gdk:event-crossing-detail event)))
    (is-false (gdk:event-crossing-focus event))
    (is (= 0 (gdk:event-crossing-state event)))))

;;;     GdkEventFocus

(test gdk-event-focus
  (let ((event (gdk:event-new :focus-change)))
    (is (typep event 'gdk:event-focus))
    ;; Common slots
    (is (eq :focus-change (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-focus
    (is (= 0 (gdk:event-focus-in event)))))

;;;     GdkEventConfigure

(test gdk-event-configure
  (let ((event (gdk:event-new :configure)))
    (is (typep event 'gdk:event-configure))
    ;; Common slots
    (is (eq :configure (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-configure
    (is (= 0 (gdk:event-configure-x event)))
    (is (= 0 (gdk:event-configure-y event)))
    (is (= 0 (gdk:event-configure-width event)))
    (is (= 0 (gdk:event-configure-height event)))))

;;;     GdkEventProperty

(test gdk-event-property
  (let ((event (gdk:event-new :property-notify)))
    (is (typep event 'gdk:event-property))
    ;; Common slots
    (is (eq :property-notify (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-property
    (is (cffi:null-pointer-p (gdk:event-property-atom event)))
    (is (= 0 (gdk:event-property-time event)))
    (is (eq :new-value (gdk:event-property-state event)))))

;;;     GdkEventSelection

(test gdk-event-selection
  (let ((event (gdk:event-new :selection-clear)))
    (is (typep event 'gdk:event-selection))
    ;; Common slots
    (is (eq :selection-clear (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-selection
    (is (cffi:null-pointer-p (gdk:event-selection-selection event)))
    (is (cffi:null-pointer-p (gdk:event-selection-target event)))
    (is (cffi:null-pointer-p (gdk:event-selection-property event)))
    (is (= 0 (gdk:event-selection-time event)))
    (is-false (gdk:event-selection-requestor event))))

;;;     GdkEventDND

(test gdk-event-dnd
  (let ((event (gdk:event-new :drag-enter)))
    (is (typep event 'gdk:event-dnd))
    ;; Common slots
    (is (eq :drag-enter (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-dnd
    (is-false (gdk:event-dnd-context event))
    (is (= 0 (gdk:event-dnd-time event)))
    (is (= 0 (gdk:event-dnd-x-root event)))
    (is (= 0 (gdk:event-dnd-y-root event)))))

;;;     GdkEventProximity

(test gdk-event-proximity
  (let ((event (gdk:event-new :proximity-in)))
    (is (typep event 'gdk:event-proximity))
    ;; Common slots
    (is (eq :proximity-in (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-proximity
    (is (= 0 (gdk:event-proximity-time event)))
    (is-false (gdk:event-proximity-device event))))

;;;     GdkEventWindowState

(test gdk-event-window-state
  (let ((event (gdk:event-new :window-state)))
    (is (typep event 'gdk:event-window-state ))
    ;; Common slots
    (is (eq :window-state (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-window-state
    (is-false (gdk:event-window-state-changed-mask event))
    (is-false (gdk:event-window-state-new-window-state event))))

;;;     GdkEventSetting

(test gdk-event-setting
  (let ((event (gdk:event-new :setting)))
    (is (typep event 'gdk:event-setting))
    ;; Common slots
    (is (eq :setting (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-setting
    (is (eq :new (gdk:event-setting-action event)))
    (is-false (gdk:event-setting-name event))))

;;;     GdkEventOwnerChange

(test gdk-event-owner-change
  (let ((event (gdk:event-new :owner-change)))
    (is (typep event 'gdk:event-owner-change))
    ;; Common slots
    (is (eq :owner-change (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-owner-change
    (is-false (gdk:event-owner-change-owner event))
    (is (eq :new-owner (gdk:event-owner-change-reason event)))
    (is (cffi:null-pointer-p (gdk:event-owner-change-selection event)))
    (is (= 0 (gdk:event-owner-change-time event)))
    (is (= 0 (gdk:event-owner-change-selection-time event)))))

;;;     GdkEventGrabBroken

(test gdk-event-grab-broken
  (let ((event (gdk:event-new :grab-broken)))
    (is (typep event 'gdk:event-grab-broken))
    ;; Common slots
    (is (eq :grab-broken (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-grab-broken
    (is-false (gdk:event-grab-broken-keyboard event))
    (is-false (gdk:event-grab-broken-implicit event))
    (is-false (gdk:event-grab-broken-grab-window event))))

;;;     GdkEventTouchpadSwipe

(test gdk-event-touchpad-swipe
  (let ((event (gdk:event-new :touchpad-swipe)))
    (is (typep event 'gdk:event-touchpad-swipe))
    ;; Common slots
    (is (eq :touchpad-swipe (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-touchpad-swipe
    (is (= 0 (gdk:event-touchpad-swipe-phase event)))
    (is (= 0 (gdk:event-touchpad-swipe-n-fingers event)))
    (is (= 0 (gdk:event-touchpad-swipe-time event)))
    (is (= 0 (gdk:event-touchpad-swipe-x event)))
    (is (= 0 (gdk:event-touchpad-swipe-y event)))
    (is (= 0 (gdk:event-touchpad-swipe-dx event)))
    (is (= 0 (gdk:event-touchpad-swipe-dy event)))
    (is (= 0 (gdk:event-touchpad-swipe-x-root event)))
    (is (= 0 (gdk:event-touchpad-swipe-y-root event)))
    (is-false (gdk:event-touchpad-swipe-state event))))

;;;     GdkEventTouchpadPinch

(test gdk-event-touchpad-pinch
  (let ((event (gdk:event-new :touchpad-pinch)))
    (is (typep event 'gdk:event-touchpad-pinch))
    ;; Common slots
    (is (eq :touchpad-pinch (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-touchpad-pinch
    (is (= 0 (gdk:event-touchpad-pinch-phase event)))
    (is (= 0 (gdk:event-touchpad-pinch-n-fingers event)))
    (is (= 0 (gdk:event-touchpad-pinch-time event)))
    (is (= 0 (gdk:event-touchpad-pinch-x event)))
    (is (= 0 (gdk:event-touchpad-pinch-y event)))
    (is (= 0 (gdk:event-touchpad-pinch-dx event)))
    (is (= 0 (gdk:event-touchpad-pinch-dy event)))
    (is (= 0 (gdk:event-touchpad-pinch-angle-delta event)))
    (is (= 0 (gdk:event-touchpad-pinch-scale event)))
    (is (= 0 (gdk:event-touchpad-pinch-x-root event)))
    (is (= 0 (gdk:event-touchpad-pinch-y-root event)))
    (is-false (gdk:event-touchpad-pinch-state event))))

;;;     GdkEventPadButton

(test gdk-event-pad-button
  (let ((event (gdk:event-new :pad-button-press)))
    (is (typep event 'gdk:event-pad-button))
    ;; Common slots
    (is (eq :pad-button-press (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-pad-button
    (is (= 0 (gdk:event-pad-button-time event)))
    (is (= 0 (gdk:event-pad-button-group event)))
    (is (= 0 (gdk:event-pad-button-button event)))
    (is (= 0 (gdk:event-pad-button-mode event)))))

;;;     GdkEventPadAxis

(test gdk-event-pad-axis
  (let ((event (gdk:event-new :pad-ring)))
    (is (typep event 'gdk:event-pad-axis))
    ;; Common slots
    (is (eq :pad-ring (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-pad-axis
    (is (= 0 (gdk:event-pad-axis-time event)))
    (is (= 0 (gdk:event-pad-axis-group event)))
    (is (= 0 (gdk:event-pad-axis-index event)))
    (is (= 0 (gdk:event-pad-axis-mode event)))
    (is (= 0 (gdk:event-pad-axis-value event)))))

;;;     GdkEventPadGroupMode

(test gdk-event-pad-group-mode
  (let ((event (gdk:event-new :pad-group-mode)))
    (is (typep event 'gdk:event-pad-group-mode))
    ;; Common slots
    (is (eq :pad-group-mode (gdk:event-type event)))
    (is-false (gdk:event-window event))
    (is-false (gdk:event-send-event event))
    ;; Slots for gdk:event-pad-group-mode
    (is (= 0 (gdk:event-pad-group-mode-time event)))
    (is (= 0 (gdk:event-pad-group-mode-group event)))
    (is (= 0 (gdk:event-pad-group-mode-mode event)))))

;;; 2024-9-22
