(in-package :gtk-test)

(def-suite gdk-window :in gdk-suite)
(in-suite gdk-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkWindowType

(test gdk-window-type
  ;; Check type
  (is (g:type-is-enum "GtkWindowType"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkWindowType")
          (g:gtype (cffi:foreign-funcall "gdk_window_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:window-type
          (glib:symbol-for-gtype "GtkWindowType")))
  ;; Check names
  (is (equal '("GDK_WINDOW_ROOT" "GDK_WINDOW_TOPLEVEL" "GDK_WINDOW_CHILD"
               "GDK_WINDOW_TEMP" "GDK_WINDOW_FOREIGN" "GDK_WINDOW_OFFSCREEN"
               "GDK_WINDOW_SUBSURFACE")
             (glib-test:list-enum-item-names "GdkWindowType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6)
             (glib-test:list-enum-item-values "GdkWindowType")))
  ;; Check nick names
  (is (equal '("root" "toplevel" "child" "temp" "foreign" "offscreen"
               "subsurface")
             (glib-test:list-enum-item-nicks "GdkWindowType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkWindowType" GDK:WINDOW-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_window_type_get_type")
                       (:ROOT 0)
                       (:TOPLEVEL 1)
                       (:CHILD 2)
                       (:TEMP 3)
                       (:FOREIGN 4)
                       (:OFFSCREEN 5)
                       (:SUBSURFACE 6))
             (gobject:get-gtype-definition "GdkWindowType"))))

;;;     GdkWindowWindowClass

(test gdk-window-window-class
  ;; Check type
  (is (g:type-is-enum "GdkWindowWindowClass"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkWindowWindowClass")
          (g:gtype (cffi:foreign-funcall "gdk_window_window_class_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:window-window-class
          (glib:symbol-for-gtype "GdkWindowWindowClass")))
  ;; Check names
  (is (equal '("GDK_INPUT_OUTPUT" "GDK_INPUT_ONLY")
             (glib-test:list-enum-item-names "GdkWindowWindowClass")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GdkWindowWindowClass")))
  ;; Check nick names
  (is (equal '("input-output" "input-only")
             (glib-test:list-enum-item-nicks "GdkWindowWindowClass")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkWindowWindowClass"
                                    GDK:WINDOW-WINDOW-CLASS
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_window_window_class_get_type")
                       (:INPUT-OUTPUT 0)
                       (:INPUT-ONLY 1))
             (gobject:get-gtype-definition "GdkWindowWindowClass"))))

;;;     GdkWindowHints

(test gdk-window-hints
  ;; Check type
  (is (g:type-is-flags "GdkWindowHints"))
  ;; Check registered name
  (is (eq 'gdk:window-hints
          (glib:symbol-for-gtype "GdkWindowHints")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkWindowHints")
          (g:gtype (cffi:foreign-funcall "gdk_window_hints_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_HINT_POS" "GDK_HINT_MIN_SIZE" "GDK_HINT_MAX_SIZE"
               "GDK_HINT_BASE_SIZE" "GDK_HINT_ASPECT" "GDK_HINT_RESIZE_INC"
               "GDK_HINT_WIN_GRAVITY" "GDK_HINT_USER_POS" "GDK_HINT_USER_SIZE")
             (glib-test:list-flags-item-names "GdkWindowHints")))
  ;; Check values
  (is (equal '(1 2 4 8 16 32 64 128 256)
             (glib-test:list-flags-item-values "GdkWindowHints")))
  ;; Check nick names
  (is (equal '("pos" "min-size" "max-size" "base-size" "aspect" "resize-inc"
               "win-gravity" "user-pos" "user-size")
             (glib-test:list-flags-item-nicks "GdkWindowHints")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkWindowHints" GDK:WINDOW-HINTS
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_window_hints_get_type")
                       (:POS 1)
                       (:MIN-SIZE 2)
                       (:MAX-SIZE 4)
                       (:BASE-SIZE 8)
                       (:ASPECT 16)
                       (:RESIZE-INC 32)
                       (:WIN-GRAVITY 64)
                       (:USER-POS 128)
                       (:USER-SIZE 256))
             (gobject:get-gtype-definition "GdkWindowHints"))))

;;;     GdkGravity

(test gdk-gravity
  ;; Check type
  (is (g:type-is-enum "GdkGravity"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkGravity")
          (g:gtype (cffi:foreign-funcall "gdk_gravity_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:gravity
          (glib:symbol-for-gtype "GdkGravity")))
  ;; Check names
  (is (equal '("GDK_GRAVITY_NORTH_WEST" "GDK_GRAVITY_NORTH"
               "GDK_GRAVITY_NORTH_EAST" "GDK_GRAVITY_WEST" "GDK_GRAVITY_CENTER"
               "GDK_GRAVITY_EAST" "GDK_GRAVITY_SOUTH_WEST" "GDK_GRAVITY_SOUTH"
               "GDK_GRAVITY_SOUTH_EAST" "GDK_GRAVITY_STATIC")
             (glib-test:list-enum-item-names "GdkGravity")))
  ;; Check values
  (is (equal '(1 2 3 4 5 6 7 8 9 10)
             (glib-test:list-enum-item-values "GdkGravity")))
  ;; Check nick names
  (is (equal '("north-west" "north" "north-east" "west" "center" "east"
               "south-west" "south" "south-east" "static")
             (glib-test:list-enum-item-nicks "GdkGravity")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkGravity" GDK:GRAVITY
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_gravity_get_type")
                       (:NORTH-WEST 1)
                       (:NORTH 2)
                       (:NORTH-EAST 3)
                       (:WEST 4)
                       (:CENTER 5)
                       (:EAST 6)
                       (:SOUTH-WEST 7)
                       (:SOUTH 8)
                       (:SOUTH-EAST 9)
                       (:STATIC 10))
             (gobject:get-gtype-definition "GdkGravity"))))

;;;     GdkGeometry

(test gdk-geometry-structure
  ;; Slot names of the structure
  (is (equal '(GDK::BASE-HEIGHT GDK::BASE-WIDTH GDK::HEIGHT-INCREMENT
               GDK::MAX-ASPECT GDK::MAX-HEIGHT GDK::MAX-WIDTH GDK::MIN-ASPECT
               GDK::MIN-HEIGHT GDK::MIN-WIDTH GDK::WIDTH-INCREMENT GDK::WIN-GRAVITY)
             (sort (cffi:foreign-slot-names '(:struct gdk:geometry)) #'string<))))

(test gdk-geometry-values
  (cffi:with-foreign-object (ptr '(:struct gdk:geometry))
    ;; Initialize the slots
    (cffi:with-foreign-slots ((gdk::min-width
                               gdk::min-height
                               gdk::max-width
                               gdk::max-height
                               gdk::base-width
                               gdk::base-height
                               gdk::width-increment
                               gdk::height-increment
                               gdk::min-aspect
                               gdk::max-aspect
                               gdk::win-gravity)
                              ptr (:struct gdk:geometry))
    (setf gdk::min-width 1
          gdk::min-height 2
          gdk::max-width 3
          gdk::max-height 4
          gdk::base-height 5
          gdk::base-width 6
          gdk::base-height 7
          gdk::width-increment 8
          gdk::height-increment 9
          gdk::min-aspect 1.5d0
          gdk::max-aspect 2.5d0
          gdk::win-gravity :north))
    ;; Return a list with the coordinates
    (cffi:with-foreign-slots ((gdk::base-width
                               gdk::base-height
                               gdk::min-aspect
                               gdk::max-aspect
                               gdk::win-gravity)
                              ptr (:struct gdk:geometry))
      (is (equal '(6 7 1.5d0 2.5d0 :north)
                 (list gdk::base-width
                       gdk::base-height
                       gdk::min-aspect
                       gdk::max-aspect
                       gdk::win-gravity))))))

;;;     GdkAnchorHints

(test gdk-anchor-hints
  ;; Check type
  (is (g:type-is-flags "GdkAnchorHints"))
  ;; Check registered name
  (is (eq 'gdk:anchor-hints
          (glib:symbol-for-gtype "GdkAnchorHints")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkAnchorHints")
          (g:gtype (cffi:foreign-funcall "gdk_anchor_hints_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_ANCHOR_FLIP_X" "GDK_ANCHOR_FLIP_Y" "GDK_ANCHOR_SLIDE_X"
               "GDK_ANCHOR_SLIDE_Y" "GDK_ANCHOR_RESIZE_X" "GDK_ANCHOR_RESIZE_Y"
               "GDK_ANCHOR_FLIP" "GDK_ANCHOR_SLIDE" "GDK_ANCHOR_RESIZE")
             (glib-test:list-flags-item-names "GdkAnchorHints")))
  ;; Check values
  (is (equal '(1 2 4 8 16 32 3 12 48)
             (glib-test:list-flags-item-values "GdkAnchorHints")))
  ;; Check nick names
  (is (equal '("flip-x" "flip-y" "slide-x" "slide-y" "resize-x" "resize-y"
               "flip" "slide" "resize")
             (glib-test:list-flags-item-nicks "GdkAnchorHints")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkAnchorHints" GDK:ANCHOR-HINTS
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_anchor_hints_get_type")
                       (:FLIP-X 1)
                       (:FLIP-Y 2)
                       (:SLIDE-X 4)
                       (:SLIDE-Y 8)
                       (:RESIZE-X 16)
                       (:RESIZE-Y 32)
                       (:FLIP 3)
                       (:SLIDE 12)
                       (:RESIZE 48))
             (gobject:get-gtype-definition "GdkAnchorHints"))))

;;;     GdkWindowEdge

(test gdk-window-edge
  ;; Check type
  (is (g:type-is-enum "GdkWindowEdge"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkWindowEdge")
          (g:gtype (cffi:foreign-funcall "gdk_window_edge_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:window-edge
          (glib:symbol-for-gtype "GdkWindowEdge")))
  ;; Check names
  (is (equal '("GDK_WINDOW_EDGE_NORTH_WEST" "GDK_WINDOW_EDGE_NORTH"
               "GDK_WINDOW_EDGE_NORTH_EAST" "GDK_WINDOW_EDGE_WEST"
               "GDK_WINDOW_EDGE_EAST" "GDK_WINDOW_EDGE_SOUTH_WEST"
               "GDK_WINDOW_EDGE_SOUTH" "GDK_WINDOW_EDGE_SOUTH_EAST")
             (glib-test:list-enum-item-names "GdkWindowEdge")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7)
             (glib-test:list-enum-item-values "GdkWindowEdge")))
  ;; Check nick names
  (is (equal '("north-west" "north" "north-east" "west" "east" "south-west"
               "south" "south-east")
             (glib-test:list-enum-item-nicks "GdkWindowEdge")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkWindowEdge" GDK:WINDOW-EDGE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_window_edge_get_type")
                       (:NORTH-WEST 0)
                       (:NORTH 1)
                       (:NORTH-EAST 2)
                       (:WEST 3)
                       (:EAST 4)
                       (:SOUTH-WEST 5)
                       (:SOUTH 6)
                       (:SOUTH-EAST 7))
             (gobject:get-gtype-definition "GdkWindowEdge"))))

;;;     GdkWindowTypeHint

(test gdk-window-type-hint
  ;; Check type
  (is (g:type-is-enum "GdkWindowTypeHint"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkWindowTypeHint")
          (g:gtype (cffi:foreign-funcall "gdk_window_type_hint_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:window-type-hint
          (glib:symbol-for-gtype "GdkWindowTypeHint")))
  ;; Check names
  (is (equal '("GDK_WINDOW_TYPE_HINT_NORMAL" "GDK_WINDOW_TYPE_HINT_DIALOG"
               "GDK_WINDOW_TYPE_HINT_MENU" "GDK_WINDOW_TYPE_HINT_TOOLBAR"
               "GDK_WINDOW_TYPE_HINT_SPLASHSCREEN"
               "GDK_WINDOW_TYPE_HINT_UTILITY" "GDK_WINDOW_TYPE_HINT_DOCK"
               "GDK_WINDOW_TYPE_HINT_DESKTOP"
               "GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU"
               "GDK_WINDOW_TYPE_HINT_POPUP_MENU" "GDK_WINDOW_TYPE_HINT_TOOLTIP"
               "GDK_WINDOW_TYPE_HINT_NOTIFICATION" "GDK_WINDOW_TYPE_HINT_COMBO"
               "GDK_WINDOW_TYPE_HINT_DND")
             (glib-test:list-enum-item-names "GdkWindowTypeHint")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13)
             (glib-test:list-enum-item-values "GdkWindowTypeHint")))
  ;; Check nick names
  (is (equal '("normal" "dialog" "menu" "toolbar" "splashscreen" "utility"
               "dock" "desktop" "dropdown-menu" "popup-menu" "tooltip"
               "notification" "combo" "dnd")
             (glib-test:list-enum-item-nicks "GdkWindowTypeHint")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkWindowTypeHint" GDK:WINDOW-TYPE-HINT
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_window_type_hint_get_type")
                       (:NORMAL 0)
                       (:DIALOG 1)
                       (:MENU 2)
                       (:TOOLBAR 3)
                       (:SPLASHSCREEN 4)
                       (:UTILITY 5)
                       (:DOCK 6)
                       (:DESKTOP 7)
                       (:DROPDOWN-MENU 8)
                       (:POPUP-MENU 9)
                       (:TOOLTIP 10)
                       (:NOTIFICATION 11)
                       (:COMBO 12)
                       (:DND 13))
             (gobject:get-gtype-definition "GdkWindowTypeHint"))))

;;;     GdkWindowAttr

(test gdk-window-attr-structure
  ;; Slot names of the structure
  (is (equal '(GDK:CURSOR GDK:EVENT-MASK GDK::HEIGHT GDK::OVERRIDE-REDIRECT
               GDK::TITLE GDK::TYPE-HINT GDK:VISUAL GDK::WCLASS GDK::WIDTH
               GDK:WINDOW-TYPE GDK::WMCLASS-CLASS GDK::WMCLASS-NAME GDK::X GDK::Y)
             (sort (cffi:foreign-slot-names '(:struct gdk:window-attr)) #'string<))))

(test gdk-window-attr-values
  (cffi:with-foreign-object (ptr '(:struct gdk:window-attr))
    ;; Initialize the slots
    (cffi:with-foreign-slots ((gdk::title
                               gdk::event-mask
                               gdk::x
                               gdk::y)
                              ptr (:struct gdk:window-attr))
    (setf gdk::title "title"
          gdk::event-mask nil
          gdk::x 10
          gdk::y 20))
    ;; Return a list with the coordinates
    (cffi:with-foreign-slots ((gdk::title
                               gdk::event-mask
                               gdk::x
                               gdk::y) ptr (:struct gdk:window-attr))
      (is (equal '("title" nil 10 20)
                 (list gdk::title gdk::event-mask gdk::x gdk::y))))))

;;;     GdkWindowAttributesType

(test gdk-window-attributes-type
  ;; Check type
  (is (g:type-is-flags "GdkWindowAttributesType"))
  ;; Check registered name
  (is (eq 'gdk:window-attributes-type
          (glib:symbol-for-gtype "GdkWindowAttributesType")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkWindowAttributesType")
          (g:gtype (cffi:foreign-funcall "gdk_window_attributes_type_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GDK_WA_TITLE" "GDK_WA_X" "GDK_WA_Y" "GDK_WA_CURSOR"
               "GDK_WA_VISUAL" "GDK_WA_WMCLASS" "GDK_WA_NOREDIR"
               "GDK_WA_TYPE_HINT")
             (glib-test:list-flags-item-names "GdkWindowAttributesType")))
  ;; Check values
  (is (equal '(2 4 8 16 32 64 128 256)
             (glib-test:list-flags-item-values "GdkWindowAttributesType")))
  ;; Check nick names
  (is (equal '("title" "x" "y" "cursor" "visual" "wmclass" "noredir"
               "type-hint")
             (glib-test:list-flags-item-nicks "GdkWindowAttributesType")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkWindowAttributesType"
                                     GDK:WINDOW-ATTRIBUTES-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_window_attributes_type_get_type")
                       (:TITLE 2)
                       (:X 4)
                       (:Y 8)
                       (:CURSOR 16)
                       (:VISUAL 32)
                       (:WMCLASS 64)
                       (:NOREDIR 128)
                       (:TYPE-HINT 256))
             (gobject:get-gtype-definition "GdkWindowAttributesType"))))

;;;     GdkFullscreenMode

(test gdk-fullscreen-mode
  ;; Check type
  (is (g:type-is-enum "GdkFullscreenMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkFullscreenMode")
          (g:gtype (cffi:foreign-funcall "gdk_fullscreen_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:fullscreen-mode
          (glib:symbol-for-gtype "GdkFullscreenMode")))
  ;; Check names
  (is (equal '("GDK_FULLSCREEN_ON_CURRENT_MONITOR"
               "GDK_FULLSCREEN_ON_ALL_MONITORS")
             (glib-test:list-enum-item-names "GdkFullscreenMode")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GdkFullscreenMode")))
  ;; Check nick names
  (is (equal '("current-monitor" "all-monitors")
             (glib-test:list-enum-item-nicks "GdkFullscreenMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkFullscreenMode" GDK:FULLSCREEN-MODE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_fullscreen_mode_get_type")
                       (:CURRENT-MONITOR 0)
                       (:ALL-MONITORS 1))
             (gobject:get-gtype-definition "GdkFullscreenMode"))))

;;;     GdkFilterReturn                                    not exported

;;;     GdkModifierIntent

(test gdk-modifier-intent
  ;; Check type
  (is (g:type-is-enum "GdkModifierIntent"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkModifierIntent")
          (g:gtype (cffi:foreign-funcall "gdk_modifier_intent_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:modifier-intent
          (glib:symbol-for-gtype "GdkModifierIntent")))
  ;; Check names
  (is (equal '("GDK_MODIFIER_INTENT_PRIMARY_ACCELERATOR"
               "GDK_MODIFIER_INTENT_CONTEXT_MENU"
               "GDK_MODIFIER_INTENT_EXTEND_SELECTION"
               "GDK_MODIFIER_INTENT_MODIFY_SELECTION"
               "GDK_MODIFIER_INTENT_NO_TEXT_INPUT"
               "GDK_MODIFIER_INTENT_SHIFT_GROUP"
               "GDK_MODIFIER_INTENT_DEFAULT_MOD_MASK")
             (glib-test:list-enum-item-names "GdkModifierIntent")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6)
             (glib-test:list-enum-item-values "GdkModifierIntent")))
  ;; Check nick names
  (is (equal '("primary-accelerator" "context-menu" "extend-selection"
               "modify-selection" "no-text-input" "shift-group"
               "default-mod-mask")
             (glib-test:list-enum-item-nicks "GdkModifierIntent")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkModifierIntent" GDK:MODIFIER-INTENT
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_modifier_intent_get_type")
                       (:PRIMARY-ACCELERATOR 0)
                       (:CONTEXT-MENU 1)
                       (:EXTEND-SELECTION 2)
                       (:MODIFY-SELECTION 3)
                       (:NO-TEXT-INPUT 4)
                       (:SHIFT-GROUP 5)
                       (:DEFAULT-MOD-MASK 6))
             (gobject:get-gtype-definition "GdkModifierIntent"))))

;;;     GdkWMDecoration

(test gdk-wm-decoration
  ;; Check type
  (is (g:type-is-flags "GdkWMDecoration"))
  ;; Check registered name
  (is (eq 'gdk:wm-decoration
          (glib:symbol-for-gtype "GdkWMDecoration")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkWMDecoration")
          (g:gtype (cffi:foreign-funcall "gdk_wm_decoration_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_DECOR_ALL" "GDK_DECOR_BORDER" "GDK_DECOR_RESIZEH"
               "GDK_DECOR_TITLE" "GDK_DECOR_MENU" "GDK_DECOR_MINIMIZE"
               "GDK_DECOR_MAXIMIZE")
             (glib-test:list-flags-item-names "GdkWMDecoration")))
  ;; Check values
  (is (equal '(1 2 4 8 16 32 64)
             (glib-test:list-flags-item-values "GdkWMDecoration")))
  ;; Check nick names
  (is (equal '("all" "border" "resizeh" "title" "menu" "minimize" "maximize")
             (glib-test:list-flags-item-nicks "GdkWMDecoration")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkWMDecoration" GDK:WM-DECORATION
                       (:EXPORT T)
                       (:ALL 1)
                       (:BORDER 2)
                       (:RESIZEH 4)
                       (:TITLE 8)
                       (:MENU 16)
                       (:MINIMIZE 32)
                       (:MAXIMIZE 64))
             (gobject:get-gtype-definition "GdkWMDecoration"))))

;;;     GdkWMFunction

(test gdk-wm-function
  ;; Check type
  (is (g:type-is-flags "GdkWMFunction"))
  ;; Check registered name
  (is (eq 'gdk:wm-function
          (glib:symbol-for-gtype "GdkWMFunction")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkWMFunction")
          (g:gtype (cffi:foreign-funcall "gdk_wm_function_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_FUNC_ALL" "GDK_FUNC_RESIZE" "GDK_FUNC_MOVE"
               "GDK_FUNC_MINIMIZE" "GDK_FUNC_MAXIMIZE" "GDK_FUNC_CLOSE")
             (glib-test:list-flags-item-names "GdkWMFunction")))
  ;; Check values
  (is (equal '(1 2 4 8 16 32)
             (glib-test:list-flags-item-values "GdkWMFunction")))
  ;; Check nick names
  (is (equal '("all" "resize" "move" "minimize" "maximize" "close")
             (glib-test:list-flags-item-nicks "GdkWMFunction")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkWMFunction" GDK:WM-FUNCTION
                       (:EXPORT T)
                       (:ALL 1)
                       (:RESIZE 2)
                       (:MOVE 4)
                       (:MINIMIZE 8)
                       (:MAXIMIZE 16)
                       (:CLOSE 32))
             (gobject:get-gtype-definition "GdkWMFunction"))))

;;;     GdkWindow

(test gdk-window-class
  ;; Check type
  (is (g:type-is-object "GdkWindow"))
  ;; Check registered name
  (is (eq 'gdk:window
          (glib:symbol-for-gtype "GdkWindow")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkWindow")
          (g:gtype (cffi:foreign-funcall "gdk_window_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkWindow")))
  ;; Check children
  ;; TODO: Ensure that the children a present when testing
  #+nil
  (is (or (member "GdkX11Window"
                 (glib-test:list-children "GdkWindow"))
          (member "GdkWaylandWindow"
                 (glib-test:list-children "GdkWindow"))))
  #+windows
  (is (or (equal '("GdkWin32Window")
                 (glib-test:list-children "GdkWindow"))
          (equal '("GdkBroadwayDisplay" "GdkWin32Display")
                 (glib-test:list-children "GdkWindow"))))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkWindow")))
  ;; Check class properties
  (is (equal '("cursor")
             (glib-test:list-properties "GdkWindow")))
  ;; Check signals
  (is (equal '("create-surface" "from-embedder" "moved-to-rect"
               "pick-embedded-child" "to-embedder")
             (glib-test:list-signals "GdkWindow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkWindow" GDK:WINDOW
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_window_get_type")
                       ((CURSOR WINDOW-CURSOR "cursor" "GdkCursor" T T)))
             (gobject:get-gtype-definition "GdkWindow"))))

;;; --- Properties -------------------------------------------------------------

(test gdk-window-properties
  (let ((window (gdk:default-root-window))
        (display (gdk:display-default)))
    (is (typep (setf (gdk:window-cursor window)
                     (gdk:cursor-new-from-name display "pointer")) 'gdk:cursor))
    (is (typep (gdk:window-cursor window) 'gdk:cursor))))

;;; --- Signals ----------------------------------------------------------------

;;;     create-surface

;; Does not work, because CairoSurface cannot be created from the Lisp side.

#+nil
(test gdk-window-create-surface-signal
  (let* ((message nil)
         (window (gdk:default-root-window))
         (handler-id (g-signal-connect window "create-surface"
                       (lambda (object width height)
                         (setf message "Signal create-surface")
                         (is (typep object 'gdk:window))
                         (is (integerp width))
                         (is (integerp height))
                         nil))))
    ;; Emit the signal
    (is-true (g-signal-emit window "create-surface" 100 100))
    (is (string= "Signal create-surface" message))
    (is-false (g-signal-handler-disconnect window handler-id))))

;;;     from-embedder

#+nil
(test gdk-window-from-embedder-signal
  (cffi:with-foreign-objects ((offscreen-x :double) (offscreen-y :double))
    (let* ((message nil)
           (window (gdk:default-root-window))
           (handler-id (g-signal-connect window "from-embedder"
                         (lambda (object
                                  embedder-x embedder-y offscreen-x offscreen-y)
                           (setf message "Signal from-embedder")
                           (is (typep object 'gdk:window))
                           (is (typep embedder-x 'double-float))
                           (is (typep embedder-y 'double-float))
                           (is (cffi:pointerp offscreen-x))
                           (is (cffi:pointerp offscreen-y))
                           t))))
      ;; Emit the signal
      (is-false (g-signal-emit window
                               "from-embedder"
                               100 100 offscreen-x offscreen-y))
      (is (string= "Signal from-embedder" message))
      (is (typep (cffi:mem-ref offscreen-x :double) 'double-float))
      (is (typep (cffi:mem-ref offscreen-y :double) 'double-float))
      (is-false (g-signal-handler-disconnect window handler-id)))))

;;;     moved-to-rect

#+nil
(test gdk-window-move-to-rect-signal
  (cffi:with-foreign-objects ((flipped-rect '(g:boxed gdk:rectangle))
                              (final-rect '(g:boxed gdk:rectangle)))
    (let* ((message nil)
           (window (gdk:default-root-window))
           (handler-id (g-signal-connect window "moved-to-rect"
                         (lambda (object
                                  flipped-rect final-rect flipped-x flipped-y)
                           (setf message "Signal moved-to-rect")
                           (is (typep object 'gdk:window))
                           (is (cffi:pointerp flipped-rect))
                           (is (cffi:pointerp final-rect))
                           (is-false flipped-x)
                           (is-false flipped-y)
                           t))))
      ;; Emit the signal
      (is-false (g-signal-emit window
                               "moved-to-rect"
                               flipped-rect final-rect nil nil))
      (is (string= "Signal moved-to-rect" message))
      (is (typep (cffi:convert-from-foreign flipped-rect
                                            '(g:boxed gdk:rectangle))
                 'gdk:rectangle))
      (is (typep (cffi:convert-from-foreign final-rect
                                            '(g:boxed gdk:rectangle))
                 'gdk:rectangle))
      (is-false (g-signal-handler-disconnect window handler-id)))))

;;;     pick-embedded-child

#+nil
(test gdk-window-pick-embedded-child-signal
  (let* ((message nil)
         (window (gdk:default-root-window))
         (handler-id (g-signal-connect window "pick-embedded-child"
                       (lambda (object x y)
                         (setf message "Signal pick-embedded-child")
                         (is (typep object 'gdk:window))
                         (is (typep x 'double-float))
                         (is (typep y 'double-float))
                         nil))))
    ;; Emit the signal
    (is-false (g-signal-emit window "pick-embedded-child" 100.0d0 100.0d0))
    (is (string= "Signal pick-embedded-child" message))
    (is-false (g-signal-handler-disconnect window handler-id))))

;;;     to-embedder

#+nil
(test gdk-window-to-embedder-signal
  (cffi:with-foreign-objects ((embedder-x :double) (embedder-y :double))
    (let* ((message nil)
           (window (gdk:default-root-window))
           (handler-id (g-signal-connect window "to-embedder"
                         (lambda (object offscreen-x offscreen-y
                                         embedder-x embedder-y)
                           (setf message "Signal to-embedder")
                           (is (typep object 'gdk:window))
                           (is (typep offscreen-x 'double-float))
                           (is (typep offscreen-y 'double-float))
                           (is (cffi:pointerp embedder-x))
                           (is (cffi:pointerp embedder-y))
                           t))))
      ;; Emit the signal
      (is-false (g-signal-emit window "to-embedder"
                               100 100 embedder-x embedder-y))
      (is (string= "Signal to-embedder" message))
      (is (typep (cffi:mem-ref embedder-x :double) 'double-float))
      (is (typep (cffi:mem-ref embedder-y :double) 'double-float))
      (is-false (g-signal-handler-disconnect window handler-id)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-window-new

(test gdk-window-new
  (cffi:with-foreign-object (attr '(:struct gdk:window-attr))
    (cffi:with-foreign-slots ((gdk::title
                               gdk::x
                               gdk::y
                               gdk::window-type)
                               attr (:struct gdk:window-attr))
  (setf gdk::title "title")
  (setf gdk::x 10)
  (setf gdk::y 20)
  (setf gdk::window-type :toplevel)
  (is (typep (gdk:window-new nil attr nil) 'gdk:window))
  (is (typep (gdk:window-new nil attr '(:x :y :title)) 'gdk:window)))))

;;;     gdk-window-destroy
;;;     gdk_window_is_destroyed

(test gdk-window-destroy
  (cffi:with-foreign-object (attr '(:struct gdk:window-attr))
    (cffi:with-foreign-slots ((gdk::window-type)
                              attr (:struct gdk:window-attr))
    (setf gdk::window-type :toplevel)
    (let ((window (gdk:window-new nil attr nil)))
      (is-false (gdk:window-destroy window))
      (is-true (gdk:window-is-destroyed window))))))

;;;     gdk-window-window-type

(test gdk-window-window-type.1
  (cffi:with-foreign-object (attr '(:struct gdk:window-attr))
    (cffi:with-foreign-slots ((gdk::window-type)
                              attr (:struct gdk:window-attr))
    (setf gdk::window-type :toplevel)
    (let ((window (gdk:window-new nil attr nil)))
      (is (eq :toplevel (gdk:window-window-type window)))))))

;; FIXME: Fails on Windows. Is this a problem with the keyword :child?

;; --------------------------------
;; WINDOW-WINDOW-TYPE.2 in GDK-WINDOW []:
;;      Unexpected Error: #<SIMPLE-ERROR "~S is not defined as a value for
;;      enum type ~S." {1008CCBEE3}>
;; -1 is not defined as a value for enum type
;; #<CFFI::FOREIGN-ENUM GDK:WINDOW-TYPE>..
;; --------------------------------

#-windows
(test gdk-window-window-type.2
  (cffi:with-foreign-object (attr '(:struct gdk:window-attr))
    (cffi:with-foreign-slots ((gdk::window-type)
                              attr (:struct gdk:window-attr))
      (is (= 2 (cffi:foreign-enum-value 'gdk:window-type
                                        (setf gdk::window-type :child))))
      (let ((window (gdk:window-new nil attr nil)))
        (is (eq :child (gdk:window-window-type window)))))))

;;;     gdk_window_display

(test gdk-window-display
  (is (typep (gdk:window-display (gdk:default-root-window)) 'gdk:display)))

;;;     gdk_window_screen

(test gdk-window-screen
  (is (typep (gdk:window-screen (gdk:default-root-window)) 'gdk:screen)))

;;;     gdk_window_visual

(test gdk-window-visual
  (is (typep (gdk:window-visual (gdk:default-root-window)) 'gdk:visual)))

;;;     gdk_window_at_pointer                              deprecated
;;;     gdk_window_show
;;;     gdk_window_show_unraised
;;;     gdk_window_hide

;;;     gdk_window_is_visible
;;;     gdk_window_is_viewable
;;;     gdk_window_is_input_only
;;;     gdk_window_is_shaped
;;;     gdk_window_get_state
;;;     gdk_window_withdraw
;;;     gdk_window_iconify
;;;     gdk_window_deiconify
;;;     gdk_window_stick
;;;     gdk_window_unstick
;;;     gdk_window_maximize
;;;     gdk_window_unmaximize
;;;     gdk_window_fullscreen
;;;     gdk_window_fullscreen_on_monitor
;;;     gdk_window_unfullscreen
;;;     gdk_window_get_fullscreen_mode
;;;     gdk_window_set_fullscreen_mode
;;;     gdk_window_set_keep_above
;;;     gdk_window_set_keep_below
;;;     gdk_window_set_opacity
;;;     gdk_window_set_composited                          deprecated
;;;     gdk_window_get_composited                          deprecated
;;;     gdk_window_set_pass_through
;;;     gdk_window_get_pass_through
;;;     gdk_window_move
;;;     gdk_window_resize
;;;     gdk_window_move_resize
;;;     gdk_window_scroll
;;;     gdk_window_move_to_rect
;;;     gdk_window_move_region
;;;     gdk_window_flush                                   deprecated
;;;     gdk_window_has_native
;;;     gdk_window_ensure_native
;;;     gdk_window_reparent
;;;     gdk_window_raise
;;;     gdk_window_lower
;;;     gdk_window_restack
;;;     gdk_window_focus
;;;     gdk_window_register_dnd
;;;     gdk_window_begin_resize_drag
;;;     gdk_window_begin_resize_drag_for_device
;;;     gdk_window_begin_move_drag
;;;     gdk_window_begin_move_drag_for_device
;;;     gdk_window_show_window_menu
;;;     gdk_window_constrain_size
;;;     gdk_window_beep
;;;     gdk_window_get_scale_factor
;;;     gdk_window_set_opaque_region
;;;     gdk_window_create_gl_context
;;;     gdk_window_mark_paint_from_clip
;;;     gdk_window_get_clip_region
;;;     gdk_window_begin_paint_rect                        deprecated
;;;     gdk_window_begin_paint_region                      deprecated
;;;     gdk_window_end_paint                               deprecated
;;;     gdk_window_begin_draw_frame
;;;     gdk_window_end_draw_frame
;;;     gdk_window_get_visible_region
;;;     GdkWindowInvalidateHandlerFunc
;;;     gdk_window_set_invalidate_handler
;;;     gdk_window_invalidate_rect
;;;     gdk_window_invalidate_region
;;;     GdkWindowChildFunc
;;;     gdk_window_invalidate_maybe_recurse
;;;     gdk_window_get_update_area
;;;     gdk_window_freeze_updates
;;;     gdk_window_thaw_updates
;;;     gdk_window_process_all_updates                     deprecated
;;;     gdk_window_process_updates                         deprecated
;;;     gdk_window_set_debug_updates                       deprecated
;;;     gdk_window_enable_synchronized_configure           deprecated
;;;     gdk_window_configure_finished                      deprecated
;;;     gdk_window_get_frame_clock
;;;     gdk_window_set_user_data
;;;     gdk_window_set_override_redirect
;;;     gdk_window_set_accept_focus
;;;     gdk_window_get_accept_focus
;;;     gdk_window_set_focus_on_map
;;;     gdk_window_get_focus_on_map
;;;     gdk_window_add_filter
;;;     gdk_window_remove_filter
;;;     gdk_window_shape_combine_region
;;;     gdk_window_set_child_shapes
;;;     gdk_window_merge_child_shapes
;;;     gdk_window_input_shape_combine_region
;;;     gdk_window_set_child_input_shapes
;;;     gdk_window_merge_child_input_shapes
;;;     gdk_window_set_static_gravities                    deprecated
;;;     gdk_window_set_title
;;;     gdk_window_set_background                          deprecated
;;;     gdk_window_set_background_rgba                     deprecated
;;;     gdk_window_set_background_pattern                  deprecated
;;;     gdk_window_get_background_pattern                  deprecated
;;;     gdk_window_set_cursor                              Accessor
;;;     gdk_window_get_cursor                              Accessor
;;;     gdk_window_get_user_data

;;;     gdk_window_geometry

#-windows
(test gdk-window-geometry
  (cffi:with-foreign-object (attr '(:struct gdk:window-attr))
    (cffi:with-foreign-slots ((gdk::window-type
                               gdk::x
                               gdk::y
                               gdk::width
                               gdk::height)
                              attr (:struct gdk:window-attr))
    (setf gdk::window-type :toplevel)
    (setf gdk::x 10
          gdk::y 20
          gdk::width 100
          gdk::height 200)
    (let ((window (gdk:window-new nil attr nil)))
      (is (equal '(0 0 100 200)
                 (multiple-value-list (gdk:window-geometry window))))))))

#+windows
(test gdk-window-geometry
  (cffi:with-foreign-object (attr '(:struct gdk:window-attr))
    (cffi:with-foreign-slots ((gdk::window-type
                               gdk::x
                               gdk::y
                               gdk::width
                               gdk::height)
                              attr (:struct gdk:window-attr))
    (setf gdk::window-type :toplevel)
    (setf gdk::x 10
          gdk::y 20
          gdk::width 100
          gdk::height 200)
    (let ((window (gdk:window-new nil attr nil)))
      (is (equal '(8 31 120 200)
                 (multiple-value-list (gdk:window-geometry window))))))))

;;;     gdk_window_set_geometry_hints

(test gdk-window-set-geometry-hints
  (cffi:with-foreign-object (attr '(:struct gdk:window-attr))
    (cffi:with-foreign-slots ((gdk::window-type
                               gdk::x
                               gdk::y
                               gdk::width
                               gdk::height)
                              attr (:struct gdk:window-attr))
    (setf gdk::window-type :toplevel)
    (setf gdk::x 10
          gdk::y 20
          gdk::width 100
          gdk::height 200)
    (let ((window (gdk:window-new nil attr nil)))
      (cffi:with-foreign-object (geometry '(:struct gdk:geometry))
        (cffi:with-foreign-slots ((gdk::min-width
                                   gdk::min-height
                                   gdk::max-width
                                   gdk::max-height
                                   gdk::min-aspect
                                   gdk::max-aspect
                                   gdk::win-gravity)
                                  geometry (:struct gdk:geometry))
          (setf gdk::min-width 10
                gdk::min-height 20
                gdk::max-width 30
                gdk::max-height 40
                gdk::min-aspect 1.5d0
                gdk::max-aspect 2.5d0
                gdk::win-gravity :north)
          (is-false (gdk:window-set-geometry-hints window geometry nil))))))))

;;;     gdk_window_width
;;;     gdk_window_height
;;;     gdk_window_position

(test gdk-window-width/height
  (cffi:with-foreign-object (attr '(:struct gdk:window-attr))
    (cffi:with-foreign-slots ((gdk::window-type
                               gdk::x
                               gdk::y
                               gdk::width
                               gdk::height)
                              attr (:struct gdk:window-attr))
    (setf gdk::window-type :toplevel)
    (setf gdk::x 10
          gdk::y 20
          gdk::width 100
          gdk::height 200)
    (let ((window (gdk:window-new nil attr '(:x :y))))
      (is (= 100 (gdk:window-width window)))
      (is (= 200 (gdk:window-height window)))
      (is (equal '(10 20)
                 (multiple-value-list (gdk:window-position window))))))))

;;;     gdk_window_set_icon_list
;;;     gdk_window_set_modal_hint
;;;     gdk_window_get_modal_hint
;;;     gdk_window_set_type_hint
;;;     gdk_window_get_type_hint
;;;     gdk_window_set_shadow_width ()
;;;     gdk_window_set_skip_taskbar_hint
;;;     gdk_window_set_skip_pager_hint
;;;     gdk_window_set_urgency_hint
;;;     gdk_window_get_root_origin
;;;     gdk_window_get_frame_extents
;;;     gdk_window_get_origin
;;;     gdk_window_get_root_coords
;;;     gdk_window_get_pointer                             deprecated
;;;     gdk_window_get_device_position
;;;     gdk_window_get_device_position_double
;;;     gdk_window_get_parent
;;;     gdk_window_get_toplevel
;;;     gdk_window_get_children
;;;     gdk_window_get_children_with_user_data
;;;     gdk_window_peek_children
;;;     gdk_window_get_events
;;;     gdk_window_set_events
;;;     gdk_window_set_icon_name
;;;     gdk_window_set_transient_for
;;;     gdk_window_set_role
;;;     gdk_window_set_startup_id
;;;     gdk_window_set_group
;;;     gdk_window_get_group
;;;     gdk_window_set_decorations
;;;     gdk_window_get_decorations
;;;     gdk_window_set_functions

;;;     gdk_default_root_window

(test gdk-default-root-window
  (let ((root-window (gdk:default-root-window)))
    (is (typep root-window 'gdk:window))
    (is-true (integerp (gdk:window-width root-window)))
    (is-true (integerp (gdk:window-height root-window)))))

;;;     gdk_window_get_support_multidevice
;;;     gdk_window_set_support_multidevice
;;;     gdk_window_get_device_cursor
;;;     gdk_window_set_device_cursor
;;;     gdk_window_get_device_events
;;;     gdk_window_set_device_events
;;;     gdk_window_get_source_events
;;;     gdk_window_set_source_events
;;;     gdk_window_get_event_compression
;;;     gdk_window_set_event_compression
;;;     gdk_offscreen_window_get_surface
;;;     gdk_offscreen_window_set_embedder
;;;     gdk_offscreen_window_get_embedder
;;;     gdk_window_geometry_changed

;;;     gdk_window_coords_from_parent

(test gdk-window-coords-from-parent
  (let ((window (gdk:default-root-window)))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (gdk:window-coords-from-parent window 10.0d0 20.0d0))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (gdk:window-coords-from-parent window 10.0 20.0))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (gdk:window-coords-from-parent window 10 20))))))

;;;     gdk_window_coords_to_parent

(test gdk-window-coords-to-parent
  (let ((window (gdk:default-root-window)))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (gdk:window-coords-to-parent window 10.0d0 20.0d0))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (gdk:window-coords-to-parent window 10.0 20.0))))
    (is (equal '(10.0d0 20.0d0)
               (multiple-value-list
                 (gdk:window-coords-to-parent window 10 20))))))

;;;     gdk_window_get_effective_parent
;;;     gdk_window_get_effective_toplevel

;;; 2025-09-17
