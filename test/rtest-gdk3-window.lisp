(in-package :gtk-test)

(def-suite gdk-window :in gdk-suite)
(in-suite gdk-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkWindowType

(test window-type
  ;; Check the type
  (is (g:type-is-enum "GtkWindowType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkWindowType")
          (g:gtype (cffi:foreign-funcall "gdk_window_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:window-type
          (gobject:symbol-for-gtype "GtkWindowType")))
  ;; Check the names
  (is (equal '("GDK_WINDOW_ROOT" "GDK_WINDOW_TOPLEVEL" "GDK_WINDOW_CHILD"
               "GDK_WINDOW_TEMP" "GDK_WINDOW_FOREIGN" "GDK_WINDOW_OFFSCREEN"
               "GDK_WINDOW_SUBSURFACE")
             (list-enum-item-name "GdkWindowType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6)
             (list-enum-item-value "GdkWindowType")))
  ;; Check the nick names
  (is (equal '("root" "toplevel" "child" "temp" "foreign" "offscreen"
               "subsurface")
             (list-enum-item-nick "GdkWindowType")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkWindowType"
                             GDK-WINDOW-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_window_type_get_type")
                             (:ROOT 0)
                             (:TOPLEVEL 1)
                             (:CHILD 2)
                             (:TEMP 3)
                             (:FOREIGN 4)
                             (:OFFSCREEN 5)
                             (:SUBSURFACE 6))
             (gobject:get-g-type-definition "GdkWindowType"))))

;;;     GdkWindowWindowClass

(test window-window-class
  ;; Check the type
  (is (g:type-is-enum "GdkWindowWindowClass"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkWindowWindowClass")
          (g:gtype (cffi:foreign-funcall "gdk_window_window_class_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gdk:window-window-class
          (gobject:symbol-for-gtype "GdkWindowWindowClass")))
  ;; Check the names
  (is (equal '("GDK_INPUT_OUTPUT" "GDK_INPUT_ONLY")
             (list-enum-item-name "GdkWindowWindowClass")))
  ;; Check the values
  (is (equal '(0 1)
             (list-enum-item-value "GdkWindowWindowClass")))
  ;; Check the nick names
  (is (equal '("input-output" "input-only")
             (list-enum-item-nick "GdkWindowWindowClass")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkWindowWindowClass"
                             GDK-WINDOW-WINDOW-CLASS
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gdk_window_window_class_get_type")
                             (:INPUT-OUTPUT 0)
                             (:INPUT-ONLY 1))
             (gobject:get-g-type-definition "GdkWindowWindowClass"))))

;;;     GdkWindowHints

(test window-hints
  ;; Check the type
  (is (g:type-is-flags "GdkWindowHints"))
  ;; Check the registered name
  (is (eq 'gdk:window-hints
          (gobject:symbol-for-gtype "GdkWindowHints")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkWindowHints")
          (g:gtype (cffi:foreign-funcall "gdk_window_hints_get_type" :size))))
  ;; Check the names
  (is (equal '("GDK_HINT_POS" "GDK_HINT_MIN_SIZE" "GDK_HINT_MAX_SIZE"
               "GDK_HINT_BASE_SIZE" "GDK_HINT_ASPECT" "GDK_HINT_RESIZE_INC"
               "GDK_HINT_WIN_GRAVITY" "GDK_HINT_USER_POS" "GDK_HINT_USER_SIZE")
             (list-flags-item-name "GdkWindowHints")))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32 64 128 256)
             (list-flags-item-value "GdkWindowHints")))
  ;; Check the nick names
  (is (equal '("pos" "min-size" "max-size" "base-size" "aspect" "resize-inc"
               "win-gravity" "user-pos" "user-size")
             (list-flags-item-nick "GdkWindowHints")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkWindowHints"
                              GDK-WINDOW-HINTS
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
             (gobject:get-g-type-definition "GdkWindowHints"))))

;;;     GdkGravity

(test gravity
  ;; Check the type
  (is (g:type-is-enum "GdkGravity"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkGravity")
          (g:gtype (cffi:foreign-funcall "gdk_gravity_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:gravity
          (gobject:symbol-for-gtype "GdkGravity")))
  ;; Check the names
  (is (equal '("GDK_GRAVITY_NORTH_WEST" "GDK_GRAVITY_NORTH"
               "GDK_GRAVITY_NORTH_EAST" "GDK_GRAVITY_WEST" "GDK_GRAVITY_CENTER"
               "GDK_GRAVITY_EAST" "GDK_GRAVITY_SOUTH_WEST" "GDK_GRAVITY_SOUTH"
               "GDK_GRAVITY_SOUTH_EAST" "GDK_GRAVITY_STATIC")
             (list-enum-item-name "GdkGravity")))
  ;; Check the values
  (is (equal '(1 2 3 4 5 6 7 8 9 10)
             (list-enum-item-value "GdkGravity")))
  ;; Check the nick names
  (is (equal '("north-west" "north" "north-east" "west" "center" "east"
               "south-west" "south" "south-east" "static")
             (list-enum-item-nick "GdkGravity")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkGravity"
                             GDK-GRAVITY
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
             (gobject:get-g-type-definition "GdkGravity"))))

;;;     GdkGeometry

(test geometry-structure
  ;; Slot names of the structure
  (is (equal '(GDK::TITLE GDK::EVENT-MASK GDK::X GDK::Y GDK::WIDTH GDK::HEIGHT
               GDK::WCLASS GDK::VISUAL GDK::WINDOW-TYPE GDK::CURSOR
               GDK::WMCLASS-NAME GDK::WMCLASS-CLASS GDK::OVERRIDE-REDIRECT
               GDK::TYPE-HINT)
             (cffi:foreign-slot-names '(:struct gdk:window-attr)))))

(test geometry-values
  (with-foreign-object (ptr '(:struct gdk:geometry))
    ;; Initialize the slots
    (setf (cffi:foreign-slot-value ptr '(:struct gdk:geometry) 'gdk::min-width) 0
          (cffi:foreign-slot-value ptr '(:struct gdk:geometry) 'gdk::min-height) 0
          (cffi:foreign-slot-value ptr '(:struct gdk:geometry) 'gdk::max-width) 0
          (cffi:foreign-slot-value ptr '(:struct gdk:geometry) 'gdk::max-height) 0
          (cffi:foreign-slot-value ptr '(:struct gdk:geometry) 'gdk::max-height) 0
          (cffi:foreign-slot-value ptr '(:struct gdk:geometry) 'gdk::base-width) 0
          (cffi:foreign-slot-value ptr '(:struct gdk:geometry) 'gdk::base-height) 0
          (cffi:foreign-slot-value ptr '(:struct gdk:geometry) 'gdk::width-increment) 0
          (cffi:foreign-slot-value ptr '(:struct gdk:geometry) 'gdk::height-increment) 0
          (cffi:foreign-slot-value ptr '(:struct gdk:geometry) 'gdk::min-aspect) 0.0d0
          (cffi:foreign-slot-value ptr '(:struct gdk:geometry) 'gdk::max-aspect) 0.0d0
          (cffi:foreign-slot-value ptr '(:struct gdk:geometry) 'gdk::win-gravity) 0)
    ;; Return a list with the coordinates
    (with-foreign-slots ((gdk::base-width
                          gdk::base-height
                          gdk::min-aspect
                          gdk::max-aspect)
                         ptr (:struct gdk:geometry))
      (is (equal '(0 0 0.0d0 0.0d0)
                 (list gdk::base-width
                       gdk::base-height
                       gdk::min-aspect
                       gdk::max-aspect))))))

(test make-geometry
  (let ((geometry (gdk:make-geometry :base-width 10
                                     :base-height 20
                                     :min-aspect 1.0d0
                                     :max-aspect 2.0d0)))
    (with-foreign-slots ((gdk::base-width
                          gdk::base-height
                          gdk::min-aspect gdk::max-aspect)
                         geometry (:struct gdk:geometry))
      (is (equal '(10 20 1.0d0 2.0d0)
                 (list gdk::base-width
                       gdk::base-height
                       gdk::min-aspect
                       gdk::max-aspect))))))

;;;     GdkAnchorHints

(test anchor-hints
  ;; Check the type
  (is (g:type-is-flags "GdkAnchorHints"))
  ;; Check the registered name
  (is (eq 'gdk:anchor-hints
          (gobject:symbol-for-gtype "GdkAnchorHints")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkAnchorHints")
          (g:gtype (cffi:foreign-funcall "gdk_anchor_hints_get_type" :size))))
  ;; Check the names
  (is (equal '("GDK_ANCHOR_FLIP_X" "GDK_ANCHOR_FLIP_Y" "GDK_ANCHOR_SLIDE_X"
               "GDK_ANCHOR_SLIDE_Y" "GDK_ANCHOR_RESIZE_X" "GDK_ANCHOR_RESIZE_Y"
               "GDK_ANCHOR_FLIP" "GDK_ANCHOR_SLIDE" "GDK_ANCHOR_RESIZE")
             (list-flags-item-name "GdkAnchorHints")))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32 3 12 48)
             (list-flags-item-value "GdkAnchorHints")))
  ;; Check the nick names
  (is (equal '("flip-x" "flip-y" "slide-x" "slide-y" "resize-x" "resize-y"
               "flip" "slide" "resize")
             (list-flags-item-nick "GdkAnchorHints")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkAnchorHints"
                              GDK-ANCHOR-HINTS
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
             (gobject:get-g-type-definition "GdkAnchorHints"))))

;;;     GdkWindowEdge

(test window-edge
  ;; Check the type
  (is (g:type-is-enum "GdkWindowEdge"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkWindowEdge")
          (g:gtype (cffi:foreign-funcall "gdk_window_edge_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:window-edge
          (gobject:symbol-for-gtype "GdkWindowEdge")))
  ;; Check the names
  (is (equal '("GDK_WINDOW_EDGE_NORTH_WEST" "GDK_WINDOW_EDGE_NORTH"
               "GDK_WINDOW_EDGE_NORTH_EAST" "GDK_WINDOW_EDGE_WEST"
               "GDK_WINDOW_EDGE_EAST" "GDK_WINDOW_EDGE_SOUTH_WEST"
               "GDK_WINDOW_EDGE_SOUTH" "GDK_WINDOW_EDGE_SOUTH_EAST")
             (list-enum-item-name "GdkWindowEdge")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (list-enum-item-value "GdkWindowEdge")))
  ;; Check the nick names
  (is (equal '("north-west" "north" "north-east" "west" "east" "south-west"
               "south" "south-east")
             (list-enum-item-nick "GdkWindowEdge")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkWindowEdge"
                             GDK-WINDOW-EDGE
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
             (gobject:get-g-type-definition "GdkWindowEdge"))))

;;;     GdkWindowTypeHint

(test window-type-hint
  ;; Check the type
  (is (g:type-is-enum "GdkWindowTypeHint"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkWindowTypeHint")
          (g:gtype (cffi:foreign-funcall "gdk_window_type_hint_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:window-type-hint
          (gobject:symbol-for-gtype "GdkWindowTypeHint")))
  ;; Check the names
  (is (equal '("GDK_WINDOW_TYPE_HINT_NORMAL" "GDK_WINDOW_TYPE_HINT_DIALOG"
               "GDK_WINDOW_TYPE_HINT_MENU" "GDK_WINDOW_TYPE_HINT_TOOLBAR"
               "GDK_WINDOW_TYPE_HINT_SPLASHSCREEN"
               "GDK_WINDOW_TYPE_HINT_UTILITY" "GDK_WINDOW_TYPE_HINT_DOCK"
               "GDK_WINDOW_TYPE_HINT_DESKTOP"
               "GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU"
               "GDK_WINDOW_TYPE_HINT_POPUP_MENU" "GDK_WINDOW_TYPE_HINT_TOOLTIP"
               "GDK_WINDOW_TYPE_HINT_NOTIFICATION" "GDK_WINDOW_TYPE_HINT_COMBO"
               "GDK_WINDOW_TYPE_HINT_DND")
             (list-enum-item-name "GdkWindowTypeHint")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13)
             (list-enum-item-value "GdkWindowTypeHint")))
  ;; Check the nick names
  (is (equal '("normal" "dialog" "menu" "toolbar" "splashscreen" "utility"
               "dock" "desktop" "dropdown-menu" "popup-menu" "tooltip"
               "notification" "combo" "dnd")
             (list-enum-item-nick "GdkWindowTypeHint")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkWindowTypeHint"
                             GDK-WINDOW-TYPE-HINT
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
             (gobject:get-g-type-definition "GdkWindowTypeHint"))))

;;;     GdkWindowAttr

(test window-attr-structure
  ;; Slot names of the structure
  (is (equal '(GDK::TITLE
               GDK::EVENT-MASK
               GDK::X
               GDK::Y
               GDK::WIDTH
               GDK::HEIGHT
               GDK::WCLASS
               GDK::VISUAL
               GDK::WINDOW-TYPE
               GDK::CURSOR
               GDK::WMCLASS-NAME
               GDK::WMCLASS-CLASS
               GDK::OVERRIDE-REDIRECT
               GDK::TYPE-HINT)
             (cffi:foreign-slot-names '(:struct gdk:window-attr)))))

(test window-attr-values
  (with-foreign-object (ptr '(:struct gdk:window-attr))
    ;; Initialize the slots
    (setf (cffi:foreign-slot-value ptr '(:struct gdk:window-attr) 'gdk::title) "title"
          (cffi:foreign-slot-value ptr '(:struct gdk:window-attr) 'gdk::event-mask) nil
          (cffi:foreign-slot-value ptr '(:struct gdk:window-attr) 'gdk::x) 10
          (cffi:foreign-slot-value ptr '(:struct gdk:window-attr) 'gdk::y) 20)
    ;; Return a list with the coordinates
    (with-foreign-slots ((gdk::x gdk::y) ptr (:struct gdk:window-attr))
      (is (equal '(10 20)
                 (list gdk::x gdk::y))))))

(test make-window-attr
  (let ((attr (gdk:make-window-attr)))
    (with-foreign-slots ((gdk::x gdk::y) attr (:struct gdk:window-attr))
      (is (equal '(0 0)
                 (list gdk::x gdk::y))))))

;;;     GdkWindowAttributesType

(test window-attributes-type
  ;; Check the type
  (is (g:type-is-flags "GdkWindowAttributesType"))
  ;; Check the registered name
  (is (eq 'gdk:window-attributes-type
          (gobject:symbol-for-gtype "GdkWindowAttributesType")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkWindowAttributesType")
          (g:gtype (cffi:foreign-funcall "gdk_window_attributes_type_get_type"
                                         :size))))
  ;; Check the names
  (is (equal '("GDK_WA_TITLE" "GDK_WA_X" "GDK_WA_Y" "GDK_WA_CURSOR"
               "GDK_WA_VISUAL" "GDK_WA_WMCLASS" "GDK_WA_NOREDIR"
               "GDK_WA_TYPE_HINT")
             (list-flags-item-name "GdkWindowAttributesType")))
  ;; Check the values
  (is (equal '(2 4 8 16 32 64 128 256)
             (list-flags-item-value "GdkWindowAttributesType")))
  ;; Check the nick names
  (is (equal '("title" "x" "y" "cursor" "visual" "wmclass" "noredir"
               "type-hint")
             (list-flags-item-nick "GdkWindowAttributesType")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkWindowAttributesType"
                              GDK-WINDOW-ATTRIBUTES-TYPE
                              (:EXPORT T
                               :TYPE-INITIALIZER
                               "gdk_window_attributes_type_get_type")
                              (:TITLE 2)
                              (:X 4)
                              (:Y 8)
                              (:CURSOR 16)
                              (:VISUAL 32)
                              (:WMCLASS 64)
                              (:NOREDIR 128)
                              (:TYPE-HINT 256))
             (gobject:get-g-type-definition "GdkWindowAttributesType"))))

;;;     GdkFullscreenMode

(test fullscreen-mode
  ;; Check the type
  (is (g:type-is-enum "GdkFullscreenMode"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkFullscreenMode")
          (g:gtype (cffi:foreign-funcall "gdk_fullscreen_mode_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:fullscreen-mode
          (gobject:symbol-for-gtype "GdkFullscreenMode")))
  ;; Check the names
  (is (equal '("GDK_FULLSCREEN_ON_CURRENT_MONITOR"
               "GDK_FULLSCREEN_ON_ALL_MONITORS")
             (list-enum-item-name "GdkFullscreenMode")))
  ;; Check the values
  (is (equal '(0 1)
             (list-enum-item-value "GdkFullscreenMode")))
  ;; Check the nick names
  (is (equal '("current-monitor" "all-monitors")
             (list-enum-item-nick "GdkFullscreenMode")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkFullscreenMode"
                             GDK-FULLSCREEN-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_fullscreen_mode_get_type")
                             (:CURRENT-MONITOR 0)
                             (:ALL-MONITORS 1))
             (gobject:get-g-type-definition "GdkFullscreenMode"))))

;;;     GdkFilterReturn

(test filter-return
  ;; Check the type
  (is (g:type-is-enum "GdkFilterReturn"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkFilterReturn")
          (g:gtype (cffi:foreign-funcall "gdk_filter_return_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:filter-return
          (gobject:symbol-for-gtype "GdkFilterReturn")))
  ;; Check the names
  (is (equal '("GDK_FILTER_CONTINUE" "GDK_FILTER_TRANSLATE" "GDK_FILTER_REMOVE")
             (list-enum-item-name "GdkFilterReturn")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GdkFilterReturn")))
  ;; Check the nick names
  (is (equal '("continue" "translate" "remove")
             (list-enum-item-nick "GdkFilterReturn")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkFilterReturn"
                             GDK-FILTER-RETURN
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_filter_return_get_type")
                             (:CONTINUE 0)
                             (:TRANSLATE 1)
                             (:REMOVE 2))
             (gobject:get-g-type-definition "GdkFilterReturn"))))

;;;     GdkModifierIntent

(test modifier-intent
  ;; Check the type
  (is (g:type-is-enum "GdkModifierIntent"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkModifierIntent")
          (g:gtype (cffi:foreign-funcall "gdk_modifier_intent_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:modifier-intent
          (gobject:symbol-for-gtype "GdkModifierIntent")))
  ;; Check the names
  (is (equal '("GDK_MODIFIER_INTENT_PRIMARY_ACCELERATOR"
               "GDK_MODIFIER_INTENT_CONTEXT_MENU"
               "GDK_MODIFIER_INTENT_EXTEND_SELECTION"
               "GDK_MODIFIER_INTENT_MODIFY_SELECTION"
               "GDK_MODIFIER_INTENT_NO_TEXT_INPUT"
               "GDK_MODIFIER_INTENT_SHIFT_GROUP"
               "GDK_MODIFIER_INTENT_DEFAULT_MOD_MASK")
             (list-enum-item-name "GdkModifierIntent")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6)
             (list-enum-item-value "GdkModifierIntent")))
  ;; Check the nick names
  (is (equal '("primary-accelerator" "context-menu" "extend-selection"
               "modify-selection" "no-text-input" "shift-group"
               "default-mod-mask")
             (list-enum-item-nick "GdkModifierIntent")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkModifierIntent" GDK-MODIFIER-INTENT
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_modifier_intent_get_type")
                             (:PRIMARY-ACCELERATOR 0)
                             (:CONTEXT-MENU 1)
                             (:EXTEND-SELECTION 2)
                             (:MODIFY-SELECTION 3)
                             (:NO-TEXT-INPUT 4)
                             (:SHIFT-GROUP 5)
                             (:DEFAULT-MOD-MASK 6))
             (gobject:get-g-type-definition "GdkModifierIntent"))))

;;;     GdkWMDecoration

(test wm-decoration
  ;; Check the type
  (is (g:type-is-flags "GdkWMDecoration"))
  ;; Check the registered name
  (is (eq 'gdk:wm-decoration
          (gobject:symbol-for-gtype "GdkWMDecoration")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkWMDecoration")
          (g:gtype (cffi:foreign-funcall "gdk_wm_decoration_get_type" :size))))
  ;; Check the names
  (is (equal '("GDK_DECOR_ALL" "GDK_DECOR_BORDER" "GDK_DECOR_RESIZEH"
               "GDK_DECOR_TITLE" "GDK_DECOR_MENU" "GDK_DECOR_MINIMIZE"
               "GDK_DECOR_MAXIMIZE")
             (list-flags-item-name "GdkWMDecoration")))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32 64)
             (list-flags-item-value "GdkWMDecoration")))
  ;; Check the nick names
  (is (equal '("all" "border" "resizeh" "title" "menu" "minimize" "maximize")
             (list-flags-item-nick "GdkWMDecoration")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkWMDecoration" GDK-W-M-DECORATION
                              (:EXPORT T)
                              (:ALL 1)
                              (:BORDER 2)
                              (:RESIZEH 4)
                              (:TITLE 8)
                              (:MENU 16)
                              (:MINIMIZE 32)
                              (:MAXIMIZE 64))
             (gobject:get-g-type-definition "GdkWMDecoration"))))

;;;     GdkWMFunction

(test wm-function
  ;; Check the type
  (is (g:type-is-flags "GdkWMFunction"))
  ;; Check the registered name
  (is (eq 'gdk:wm-function
          (gobject:symbol-for-gtype "GdkWMFunction")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkWMFunction")
          (g:gtype (cffi:foreign-funcall "gdk_wm_function_get_type" :size))))
  ;; Check the names
  (is (equal '("GDK_FUNC_ALL" "GDK_FUNC_RESIZE" "GDK_FUNC_MOVE"
               "GDK_FUNC_MINIMIZE" "GDK_FUNC_MAXIMIZE" "GDK_FUNC_CLOSE")
             (list-flags-item-name "GdkWMFunction")))
  ;; Check the values
  (is (equal '(1 2 4 8 16 32)
             (list-flags-item-value "GdkWMFunction")))
  ;; Check the nick names
  (is (equal '("all" "resize" "move" "minimize" "maximize" "close")
             (list-flags-item-nick "GdkWMFunction")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkWMFunction" GDK-W-M-FUNCTION
                              (:EXPORT T)
                              (:ALL 1)
                              (:RESIZE 2)
                              (:MOVE 4)
                              (:MINIMIZE 8)
                              (:MAXIMIZE 16)
                              (:CLOSE 32))
             (gobject:get-g-type-definition "GdkWMFunction"))))

;;;     GdkWindow

(test window-class
  ;; Type check
  (is (g:type-is-object "GdkWindow"))
  ;; Check the registered name
  (is (eq 'gdk:window
          (gobject:symbol-for-gtype "GdkWindow")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkWindow")
          (g:gtype (cffi:foreign-funcall "gdk_window_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkWindow")))
  ;; Check the children
  ;; TODO: Ensure that the children a present when testing
  #+nil
  (is (or (member "GdkX11Window"
                 (list-children "GdkWindow"))
          (member "GdkWaylandWindow"
                 (list-children "GdkWindow"))))
  #+windows
  (is (or (equal '("GdkWin32Window")
                 (list-children "GdkWindow"))
          (equal '("GdkBroadwayDisplay" "GdkWin32Display")
                 (list-children "GdkWindow"))))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkWindow")))
  ;; Check the class properties
  (is (equal '("cursor")
             (list-properties "GdkWindow")))
  (is (equal '("create-surface" "from-embedder" "moved-to-rect"
               "pick-embedded-child" "to-embedder")
             (list-signals "GdkWindow")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkWindow" GDK-WINDOW
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_window_get_type")
                       ((CURSOR GDK-WINDOW-CURSOR "cursor" "GdkCursor" T T)))
             (gobject:get-g-type-definition "GdkWindow"))))

;;; --- Properties -------------------------------------------------------------

(test window-properties
  (let ((window (gdk:default-root-window))
        (display (gdk:display-default)))
    (is (typep (setf (gdk:window-cursor window)
                     (gdk:cursor-new-from-name display "pointer")) 'gdk:cursor))
    (is (typep (gdk:window-cursor window) 'gdk:cursor))))

;;; --- Signals ----------------------------------------------------------------

;;;     create-surface

;; Does not work, because CairoSurface cannot be created from the Lisp side.

#+nil
(test window-create-surface-signal
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
(test window-from-embedder-signal
  (with-foreign-objects ((offscreen-x :double) (offscreen-y :double))
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
(test window-move-to-rect-signal
  (with-foreign-objects ((flipped-rect '(g:boxed gdk:rectangle))
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
(test window-pick-embedded-child-signal
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
(test window-to-embedder-signal
  (with-foreign-objects ((embedder-x :double) (embedder-y :double))
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

(test window-new
  (is (typep (gdk:window-new nil (gdk:make-window-attr) nil) 'gdk:window))
  (is (typep (gdk:window-new nil (gdk:make-window-attr) '(:x :y :title))
             'gdk:window)))

;;;     gdk-window-destroy

(test window-destroy
  (let ((window (gdk:window-new nil (gdk:make-window-attr) nil)))
    (is-false (gdk:window-destroy window))))

;;;     gdk-window-window-type

(test window-window-type.1
  (let ((window (gdk:window-new nil
                                (gdk:make-window-attr :window-type :toplevel)
                                nil)))
    (is (eq :toplevel (gdk:window-window-type window)))))

#-windows ; TODO: Gives a Warning on Windows. Check agein for Linux.
(test window-window-type.2
  (let ((window (gdk:window-new nil
                                (gdk:make-window-attr :window-type :child)
                                nil)))
    (is (eq :child (gdk:window-window-type window)))))

;;;     gdk_window_display

(test window-display
  (is (typep (gdk:window-display (gdk:default-root-window)) 'gdk:display)))

;;;     gdk_window_screen

(test window-screen
  (is (typep (gdk:window-screen (gdk:default-root-window)) 'gdk:screen)))

;;;     gdk_window_visual

(test window-visual
  (is (typep (gdk:window-visual (gdk:default-root-window)) 'gdk:visual)))

;;;     gdk_window_at_pointer                              deprecated
;;;     gdk_window_show
;;;     gdk_window_show_unraised
;;;     gdk_window_hide
;;;     gdk_window_is_destroyed
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
(test window-geometry
  (let ((window (gdk:window-new nil
                                (gdk:make-window-attr :x 10
                                                      :y 20
                                                      :width 100
                                                      :height 200)
                                nil)))
    (is (equal '(0 0 100 200)
               (multiple-value-list (gdk:window-geometry window)))))
  (let ((window (gdk:window-new nil
                                (gdk:make-window-attr :x 10
                                                      :y 20
                                                      :width 100
                                                      :height 200)
                                '(:x :y))))
    (is (equal '(10 20 100 200)
        (multiple-value-list (gdk:window-geometry window))))))

;;;     gdk_window_set_geometry_hints

(test window-set-geometry-hints
  (let ((window (gdk:window-new nil (gdk:make-window-attr) nil)))
    (is-false (gdk:window-set-geometry-hints window (gdk:make-geometry) nil))))

;;;     gdk_window_width
;;;     gdk_window_height

(test window-width/height
  (let ((window (gdk:window-new nil
                                (gdk:make-window-attr :width 100 :height 200)
                                nil)))
    (is (= 100 (gdk:window-width window)))
    (is (= 200 (gdk:window-height window)))))

;;;     gdk_window_set_icon_list
;;;     gdk_window_set_modal_hint
;;;     gdk_window_get_modal_hint
;;;     gdk_window_set_type_hint
;;;     gdk_window_get_type_hint
;;;     gdk_window_set_shadow_width ()
;;;     gdk_window_set_skip_taskbar_hint
;;;     gdk_window_set_skip_pager_hint
;;;     gdk_window_set_urgency_hint

;;;     gdk_window_position

(test window-position
  (let ((window (gdk:window-new nil (gdk:make-window-attr) nil)))
    (is (equal '(0 0) (multiple-value-list (gdk:window-position window)))))
  (let ((window (gdk:window-new nil
                                (gdk:make-window-attr :x 10 :y 20)
                                '(:x :y))))
    (is (equal '(10 20) (multiple-value-list (gdk:window-position window))))))

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

(test default-root-window
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

(test window-coords-from-parent
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

(test window-coords-to-parent
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

;;; --- 2023-1-8 ---------------------------------------------------------------
