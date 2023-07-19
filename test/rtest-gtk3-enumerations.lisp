(in-package :gtk-test)

(def-suite gtk-enumerations :in gtk-suite)
(in-suite gtk-enumerations)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBaselinePosition

(test gtk-base-line-position
  ;; Check the type
  (is (g:type-is-enum "GtkBaselinePosition"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkBaselinePosition")
          (g:gtype (cffi:foreign-funcall "gtk_baseline_position_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:baseline-position
          (glib:symbol-for-gtype "GtkBaselinePosition")))
  ;; Check the names
  (is (equal '("GTK_RELIEF_NORMAL" "GTK_RELIEF_HALF" "GTK_RELIEF_NONE")
             (list-enum-item-name "GtkReliefStyle")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkReliefStyle")))
  ;; Check the nick names
  (is (equal '("top" "center" "bottom")
             (list-enum-item-nick "GtkBaselinePosition")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkBaselinePosition"
                             GTK-BASELINE-POSITION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_baseline_position_get_type")
                             (:TOP 0)
                             (:CENTER 1)
                             (:BOTTOM 2))
             (gobject:get-g-type-definition "GtkBaselinePosition"))))

;;;     GtkDeleteType

(test gtk-delete-type
  ;; Check the type
  (is (g:type-is-enum "GtkDeleteType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkDeleteType")
          (g:gtype (cffi:foreign-funcall "gtk_delete_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:delete-type
          (glib:symbol-for-gtype "GtkDeleteType")))
  ;; Check the names
  (is (equal '("GTK_DELETE_CHARS" "GTK_DELETE_WORD_ENDS" "GTK_DELETE_WORDS"
               "GTK_DELETE_DISPLAY_LINES" "GTK_DELETE_DISPLAY_LINE_ENDS"
               "GTK_DELETE_PARAGRAPH_ENDS" "GTK_DELETE_PARAGRAPHS"
               "GTK_DELETE_WHITESPACE")
             (list-enum-item-name "GtkDeleteType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (list-enum-item-value "GtkDeleteType")))
  ;; Check the nick names
  (is (equal '("chars" "word-ends" "words" "display-lines" "display-line-ends"
               "paragraph-ends" "paragraphs" "whitespace")
             (list-enum-item-nick "GtkDeleteType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkDeleteType"
                             GTK-DELETE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_delete_type_get_type")
                             (:CHARS 0)
                             (:WORD-ENDS 1)
                             (:WORDS 2)
                             (:DISPLAY-LINES 3)
                             (:DISPLAY-LINE-ENDS 4)
                             (:PARAGRAPH-ENDS 5)
                             (:PARAGRAPHS 6)
                             (:WHITESPACE 7))
             (gobject:get-g-type-definition "GtkDeleteType"))))

;;;     GtkDirectionType

(test gtk-direction-type
  ;; Check the type
  (is (g:type-is-enum "GtkDirectionType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkDirectionType")
          (g:gtype (cffi:foreign-funcall "gtk_direction_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:direction-type
          (glib:symbol-for-gtype "GtkDirectionType")))
  ;; Check the names
  (is (equal '("GTK_DIR_TAB_FORWARD" "GTK_DIR_TAB_BACKWARD" "GTK_DIR_UP"
               "GTK_DIR_DOWN" "GTK_DIR_LEFT" "GTK_DIR_RIGHT")
             (list-enum-item-name "GtkDirectionType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5)
             (list-enum-item-value "GtkDirectionType")))
  ;; Check the nick names
  (is (equal '("tab-forward" "tab-backward" "up" "down" "left" "right")
             (list-enum-item-nick "GtkDirectionType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkDirectionType"
                             GTK-DIRECTION-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_direction_type_get_type")
                             (:TAB-FORWARD 0)
                             (:TAB-BACKWARD 1)
                             (:UP 2)
                             (:DOWN 3)
                             (:LEFT 4)
                             (:RIGHT 5))
             (gobject:get-g-type-definition "GtkDirectionType"))))

;;;     GtkJustification

(test gtk-justification
  ;; Check the type
  (is (g:type-is-enum "GtkJustification"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkJustification")
          (g:gtype (cffi:foreign-funcall "gtk_justification_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:justification
          (glib:symbol-for-gtype "GtkJustification")))
  ;; Check the names
  (is (equal '("GTK_JUSTIFY_LEFT" "GTK_JUSTIFY_RIGHT" "GTK_JUSTIFY_CENTER"
               "GTK_JUSTIFY_FILL")
             (list-enum-item-name "GtkJustification")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkJustification")))
  ;; Check the nick names
  (is (equal '("left" "right" "center" "fill")
             (list-enum-item-nick "GtkJustification")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkJustification"
                             GTK-JUSTIFICATION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_justification_get_type")
                             (:LEFT 0)
                             (:RIGHT 1)
                             (:CENTER 2)
                             (:FILL 3))
             (gobject:get-g-type-definition "GtkJustification"))))

;;;     GtkMovementStep

(test gtk-movement-step
  ;; Check the type
  (is (g:type-is-enum "GtkMovementStep"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkMovementStep")
          (g:gtype (cffi:foreign-funcall "gtk_movement_step_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:movement-step
          (glib:symbol-for-gtype "GtkMovementStep")))
  ;; Check the names
  (is (equal '("GTK_MOVEMENT_LOGICAL_POSITIONS" "GTK_MOVEMENT_VISUAL_POSITIONS"
               "GTK_MOVEMENT_WORDS" "GTK_MOVEMENT_DISPLAY_LINES"
               "GTK_MOVEMENT_DISPLAY_LINE_ENDS" "GTK_MOVEMENT_PARAGRAPHS"
               "GTK_MOVEMENT_PARAGRAPH_ENDS" "GTK_MOVEMENT_PAGES"
               "GTK_MOVEMENT_BUFFER_ENDS" "GTK_MOVEMENT_HORIZONTAL_PAGES")
             (list-enum-item-name "GtkMovementStep")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9)
             (list-enum-item-value "GtkMovementStep")))
  ;; Check the nick names
  (is (equal '("logical-positions" "visual-positions" "words" "display-lines"
               "display-line-ends" "paragraphs" "paragraph-ends" "pages"
               "buffer-ends" "horizontal-pages")
             (list-enum-item-nick "GtkMovementStep")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkMovementStep"
                             GTK-MOVEMENT-STEP
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_movement_step_get_type")
                             (:LOGICAL-POSITIONS 0)
                             (:VISUAL-POSITIONS 1)
                             (:WORDS 2)
                             (:DISPLAY-LINES 3)
                             (:DISPLAY-LINE-ENDS 4)
                             (:PARAGRAPHS 5)
                             (:PARAGRAPH-ENDS 6)
                             (:PAGES 7)
                             (:BUFFER-ENDS 8)
                             (:HORIZONTAL-PAGES 9))
             (gobject:get-g-type-definition "GtkMovementStep"))))

;;;     GtkOrientation

(test gtk-orientation
  ;; Check the type
  (is (g:type-is-enum "GtkOrientation"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkOrientation")
          (g:gtype (cffi:foreign-funcall "gtk_orientation_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:orientation
          (glib:symbol-for-gtype "GtkOrientation")))
  ;; Check the names
  (is (equal '("GTK_ORIENTATION_HORIZONTAL" "GTK_ORIENTATION_VERTICAL")
             (list-enum-item-name "GtkOrientation")))
  ;; Check the values
  (is (equal '(0 1)
             (list-enum-item-value "GtkOrientation")))
  ;; Check the nick names
  (is (equal '("horizontal" "vertical")
             (list-enum-item-nick "GtkOrientation")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkOrientation"
                             GTK-ORIENTATION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_orientation_get_type")
                             (:HORIZONTAL 0)
                             (:VERTICAL 1))
             (gobject:get-g-type-definition "GtkOrientation"))))

;;;     GtkPackType

(test gtk-pack-type
  ;; Check the type
  (is (g:type-is-enum "GtkPackType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPackType")
          (g:gtype (cffi:foreign-funcall "gtk_pack_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:pack-type
          (glib:symbol-for-gtype "GtkPackType")))
  ;; Check the names
  (is (equal '("GTK_PACK_START" "GTK_PACK_END")
             (list-enum-item-name "GtkPackType")))
  ;; Check the values
  (is (equal '(0 1)
             (list-enum-item-value "GtkPackType")))
  ;; Check the nick names
  (is (equal '("start" "end")
             (list-enum-item-nick "GtkPackType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPackType"
                             GTK-PACK-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_pack_type_get_type")
                             (:START 0)
                             (:END 1))
             (gobject:get-g-type-definition "GtkPackType"))))

;;;     GtkPositionType

(test gtk-position-type
  ;; Check the type
  (is (g:type-is-enum "GtkPositionType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPositionType")
          (g:gtype (cffi:foreign-funcall "gtk_position_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:position-type
          (glib:symbol-for-gtype "GtkPositionType")))
  ;; Check the names
  (is (equal '("GTK_POS_LEFT" "GTK_POS_RIGHT" "GTK_POS_TOP" "GTK_POS_BOTTOM")
             (list-enum-item-name "GtkPositionType")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkPositionType")))
  ;; Check the nick names
  (is (equal '("left" "right" "top" "bottom")
             (list-enum-item-nick "GtkPositionType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPositionType"
                             GTK-POSITION-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_position_type_get_type")
                             (:LEFT 0)
                             (:RIGHT 1)
                             (:TOP 2)
                             (:BOTTOM 3))
             (gobject:get-g-type-definition "GtkPositionType"))))

;;;     GtkReliefStyle

(test gtk-relief-style
  ;; Check the type
  (is (g:type-is-enum "GtkReliefStyle"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkReliefStyle")
          (g:gtype (cffi:foreign-funcall "gtk_relief_style_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:relief-style
          (glib:symbol-for-gtype "GtkReliefStyle")))
  ;; Check the names
  (is (equal '("GTK_RELIEF_NORMAL" "GTK_RELIEF_HALF" "GTK_RELIEF_NONE")
             (list-enum-item-name "GtkReliefStyle")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkReliefStyle")))
  ;; Check the nick names
  (is (equal '("normal" "half" "none")
             (list-enum-item-nick "GtkReliefStyle")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkReliefStyle"
                             GTK-RELIEF-STYLE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_relief_style_get_type")
                             (:NORMAL 0)
                             (:HALF 1)
                             (:NONE 2))
             (gobject:get-g-type-definition "GtkReliefStyle"))))

;;;     GtkScrollStep

;; not exported

;;;     GtkScrollType

(test gtk-scroll-type
  ;; Check the type
  (is (g:type-is-enum "GtkScrollType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkScrollType")
          (g:gtype (cffi:foreign-funcall "gtk_scroll_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:scroll-type
          (glib:symbol-for-gtype "GtkScrollType")))
  ;; Check the names
  (is (equal '("GTK_SCROLL_NONE" "GTK_SCROLL_JUMP" "GTK_SCROLL_STEP_BACKWARD"
               "GTK_SCROLL_STEP_FORWARD" "GTK_SCROLL_PAGE_BACKWARD"
               "GTK_SCROLL_PAGE_FORWARD" "GTK_SCROLL_STEP_UP"
               "GTK_SCROLL_STEP_DOWN" "GTK_SCROLL_PAGE_UP"
               "GTK_SCROLL_PAGE_DOWN" "GTK_SCROLL_STEP_LEFT"
               "GTK_SCROLL_STEP_RIGHT" "GTK_SCROLL_PAGE_LEFT"
               "GTK_SCROLL_PAGE_RIGHT" "GTK_SCROLL_START" "GTK_SCROLL_END")
             (list-enum-item-name "GtkScrollType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
             (list-enum-item-value "GtkScrollType")))
  ;; Check the nick names
  (is (equal '("none" "jump" "step-backward" "step-forward" "page-backward"
               "page-forward" "step-up" "step-down" "page-up" "page-down"
               "step-left" "step-right" "page-left" "page-right" "start" "end")
             (list-enum-item-nick "GtkScrollType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkScrollType"
                             GTK-SCROLL-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_scroll_type_get_type")
                             (:NONE 0)
                             (:JUMP 1)
                             (:STEP-BACKWARD 2)
                             (:STEP-FORWARD 3)
                             (:PAGE-BACKWARD 4)
                             (:PAGE-FORWARD 5)
                             (:STEP-UP 6)
                             (:STEP-DOWN 7)
                             (:PAGE-UP 8)
                             (:PAGE-DOWN 9)
                             (:STEP-LEFT 10)
                             (:STEP-RIGHT 11)
                             (:PAGE-LEFT 12)
                             (:PAGE-RIGHT 13)
                             (:START 14)
                             (:END 15))
             (gobject:get-g-type-definition "GtkScrollType"))))

;;;     GtkSelectionMode

(test gtk-selection-mode
  ;; Check the type
  (is (g:type-is-enum "GtkSelectionMode"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSelectionMode")
          (g:gtype (cffi:foreign-funcall "gtk_selection_mode_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:selection-mode
          (glib:symbol-for-gtype "GtkSelectionMode")))
  ;; Check the names
  (is (equal '("GTK_SELECTION_NONE" "GTK_SELECTION_SINGLE"
               "GTK_SELECTION_BROWSE" "GTK_SELECTION_MULTIPLE")
             (list-enum-item-name "GtkSelectionMode")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkSelectionMode")))
  ;; Check the nick names
  (is (equal '("none" "single" "browse" "multiple")
             (list-enum-item-nick "GtkSelectionMode")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSelectionMode"
                             GTK-SELECTION-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_selection_mode_get_type")
                             (:NONE 0)
                             (:SINGLE 1)
                             (:BROWSE 2)
                             (:MULTIPLE 3))
             (gobject:get-g-type-definition "GtkSelectionMode"))))

;;;     GtkShadowType

(test gtk-shadow-type
  ;; Check the type
  (is (g:type-is-enum "GtkShadowType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShadowType")
          (g:gtype (cffi:foreign-funcall "gtk_shadow_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:shadow-type
          (glib:symbol-for-gtype "GtkShadowType")))
  ;; Check the names
  (is (equal '("GTK_SHADOW_NONE" "GTK_SHADOW_IN" "GTK_SHADOW_OUT"
               "GTK_SHADOW_ETCHED_IN" "GTK_SHADOW_ETCHED_OUT")
             (list-enum-item-name "GtkShadowType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4)
             (list-enum-item-value "GtkShadowType")))
  ;; Check the nick names
  (is (equal '("none" "in" "out" "etched-in" "etched-out")
             (list-enum-item-nick "GtkShadowType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkShadowType"
                             GTK-SHADOW-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_shadow_type_get_type")
                             (:NONE 0)
                             (:IN 1)
                             (:OUT 2)
                             (:ETCHED-IN 3)
                             (:ETCHED-OUT 4))
             (gobject:get-g-type-definition "GtkShadowType"))))

;;;     GtkStateFlags

(test gtk-state-flags
  ;; Check the type
  (is (g:type-is-flags "GtkStateFlags"))
  ;; Check the registered name
  (is (eq 'gtk:state-flags
          (glib:symbol-for-gtype "GtkStateFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStateFlags")
          (g:gtype (cffi:foreign-funcall "gtk_state_flags_get_type" :size))))
  ;; Check the names
  (is (equal '("GTK_STATE_FLAG_NORMAL" "GTK_STATE_FLAG_ACTIVE"
               "GTK_STATE_FLAG_PRELIGHT" "GTK_STATE_FLAG_SELECTED"
               "GTK_STATE_FLAG_INSENSITIVE" "GTK_STATE_FLAG_INCONSISTENT"
               "GTK_STATE_FLAG_FOCUSED" "GTK_STATE_FLAG_BACKDROP"
               "GTK_STATE_FLAG_DIR_LTR" "GTK_STATE_FLAG_DIR_RTL"
               "GTK_STATE_FLAG_LINK" "GTK_STATE_FLAG_VISITED"
               "GTK_STATE_FLAG_CHECKED" "GTK_STATE_FLAG_DROP_ACTIVE")
             (list-flags-item-name "GtkStateFlags")))
  ;; Check the values
  (is (equal '(0 1 2 4 8 16 32 64 128 256 512 1024 2048 4096)
             (list-flags-item-value "GtkStateFlags")))
  ;; Check the nick names
  (is (equal '("normal" "active" "prelight" "selected" "insensitive"
               "inconsistent" "focused" "backdrop" "dir-ltr" "dir-rtl" "link"
               "visited" "checked" "drop-active")
             (list-flags-item-nick "GtkStateFlags")))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkStateFlags"
                              GTK-STATE-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gtk_state_flags_get_type")
                              (:NORMAL 0)
                              (:ACTIVE 1)
                              (:PRELIGHT 2)
                              (:SELECTED 4)
                              (:INSENSITIVE 8)
                              (:INCONSISTENT 16)
                              (:FOCUSED 32)
                              (:BACKDROP 64)
                              (:DIR-LTR 128)
                              (:DIR-RTL 256)
                              (:LINK 512)
                              (:VISITED 1024)
                              (:CHECKED 2048)
                              (:DROP-ACTIVE 4096))
             (gobject:get-g-type-definition "GtkStateFlags"))))

;;;     GtkToolbarStyle

(test gtk-toolbar-style
  ;; Check the type
  (is (g:type-is-enum "GtkToolbarStyle"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkToolbarStyle")
          (g:gtype (cffi:foreign-funcall "gtk_toolbar_style_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:toolbar-style
          (glib:symbol-for-gtype "GtkToolbarStyle")))
  ;; Check the names
  (is (equal '("GTK_TOOLBAR_ICONS" "GTK_TOOLBAR_TEXT" "GTK_TOOLBAR_BOTH"
               "GTK_TOOLBAR_BOTH_HORIZ")
             (list-enum-item-name "GtkToolbarStyle")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkToolbarStyle")))
  ;; Check the nick names
  (is (equal '("icons" "text" "both" "both-horiz")
             (list-enum-item-nick "GtkToolbarStyle")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkToolbarStyle"
                             GTK-TOOLBAR-STYLE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_toolbar_style_get_type")
                             (:ICONS 0)
                             (:TEXT 1)
                             (:BOTH 2)
                             (:BOTH-HORIZ 3))
             (gobject:get-g-type-definition "GtkToolbarStyle"))))

;;;     GtkSortType

(test gtk-sort-type
  ;; Check the type
  (is (g:type-is-enum "GtkSortType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSortType")
          (g:gtype (cffi:foreign-funcall "gtk_sort_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:sort-type
          (glib:symbol-for-gtype "GtkSortType")))
  ;; Check the names
  (is (equal '("GTK_SORT_ASCENDING" "GTK_SORT_DESCENDING")
             (list-enum-item-name "GtkSortType")))
  ;; Check the values
  (is (equal '(0 1)
             (list-enum-item-value "GtkSortType")))
  ;; Check the nick names
  (is (equal '("ascending" "descending")
             (list-enum-item-nick "GtkSortType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkSortType"
                             GTK-SORT-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_sort_type_get_type")
                             (:ASCENDING 0)
                             (:DESCENDING 1))
             (gobject:get-g-type-definition "GtkSortType"))))

;;;     GtkTextDirection  <--- from gtk.widget.lisp

(test gtk-text-direction
  ;; Check the type
  (is (g:type-is-enum "GtkTextDirection"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTextDirection")
          (g:gtype (cffi:foreign-funcall "gtk_text_direction_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:text-direction
          (glib:symbol-for-gtype "GtkTextDirection")))
  ;; Check the names
  (is (equal '("GTK_TEXT_DIR_NONE" "GTK_TEXT_DIR_LTR" "GTK_TEXT_DIR_RTL")
             (list-enum-item-name "GtkTextDirection")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkTextDirection")))
  ;; Check the nick names
  (is (equal '("none" "ltr" "rtl")
             (list-enum-item-nick "GtkTextDirection")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkTextDirection"
                             GTK-TEXT-DIRECTION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_text_direction_get_type")
                             (:NONE 0)
                             (:LTR 1)
                             (:RTL 2))
             (gobject:get-g-type-definition "GtkTextDirection"))))

;;;     GtkExpanderStyle

(test gtk-expander-style
  ;; Check the type
  (is (g:type-is-enum "GtkExpanderStyle"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkExpanderStyle")
          (g:gtype (cffi:foreign-funcall "gtk_expander_style_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:expander-style
          (glib:symbol-for-gtype "GtkExpanderStyle")))
  ;; Check the names
  (is (equal '("GTK_EXPANDER_COLLAPSED" "GTK_EXPANDER_SEMI_COLLAPSED"
               "GTK_EXPANDER_SEMI_EXPANDED" "GTK_EXPANDER_EXPANDED")
             (list-enum-item-name "GtkExpanderStyle")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkExpanderStyle")))
  ;; Check the nick names
  (is (equal '("collapsed" "semi-collapsed" "semi-expanded" "expanded")
             (list-enum-item-nick "GtkExpanderStyle")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkExpanderStyle"
                             GTK-EXPANDER-STYLE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_expander_style_get_type")
                             (:COLLAPSED 0)
                             (:SEMI-COLLAPSED 1)
                             (:SEMI-EXPANDED 2)
                             (:EXPANDED 3))
             (gobject:get-g-type-definition "GtkExpanderStyle"))))

;;;     GtkStateType

(test gtk-state-type
  ;; Check the type
  (is (g:type-is-enum "GtkStateType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStateType")
          (g:gtype (cffi:foreign-funcall "gtk_state_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:state-type
          (glib:symbol-for-gtype "GtkStateType")))
  ;; Check the names
  (is (equal '("GTK_STATE_NORMAL" "GTK_STATE_ACTIVE" "GTK_STATE_PRELIGHT"
               "GTK_STATE_SELECTED" "GTK_STATE_INSENSITIVE"
               "GTK_STATE_INCONSISTENT" "GTK_STATE_FOCUSED")
             (list-enum-item-name "GtkStateType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6)
             (list-enum-item-value "GtkStateType")))
  ;; Check the nick names
  (is (equal '("normal" "active" "prelight" "selected" "insensitive"
               "inconsistent" "focused")
             (list-enum-item-nick "GtkStateType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkStateType"
                             GTK-STATE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_state_type_get_type")
                             (:NORMAL 0)
                             (:ACTIVE 1)
                             (:PRELIGHT 2)
                             (:SELECTED 3)
                             (:INSENSITIVE 4)
                             (:INCONSISTENT 5)
                             (:FOCUSED 6))
             (gobject:get-g-type-definition "GtkStateType"))))

;;; --- 2023-7-19 --------------------------------------------------------------
