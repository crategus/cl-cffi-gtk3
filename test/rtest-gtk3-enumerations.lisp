(in-package :gtk-test)

(def-suite gtk-enumerations :in gtk-suite)
(in-suite gtk-enumerations)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBaselinePosition

(test base-line-position
  ;; Check the type
  (is (g:type-is-enum "GtkBaselinePosition"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkBaselinePosition")
          (g:gtype (cffi:foreign-funcall "gtk_baseline_position_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:baseline-position
          (gobject:symbol-for-gtype "GtkBaselinePosition")))
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
  (is (equal '(DEFINE-G-ENUM "GtkBaselinePosition"
                             GTK-BASELINE-POSITION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_baseline_position_get_type")
                             (:TOP 0)
                             (:CENTER 1)
                             (:BOTTOM 2))
             (gobject:get-g-type-definition "GtkBaselinePosition"))))

;;;     GtkDeleteType

(test delete-type
  ;; Check the type
  (is (g:type-is-enum "GtkDeleteType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkDeleteType")
          (g:gtype (cffi:foreign-funcall "gtk_delete_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:delete-type
          (gobject:symbol-for-gtype "GtkDeleteType")))
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
  (is (equal '(DEFINE-G-ENUM "GtkDeleteType"
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

(test direction-type
  ;; Check the type
  (is (g:type-is-enum "GtkDirectionType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkDirectionType")
          (g:gtype (cffi:foreign-funcall "gtk_direction_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:direction-type
          (gobject:symbol-for-gtype "GtkDirectionType")))
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
  (is (equal '(DEFINE-G-ENUM "GtkDirectionType"
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

(test justification
  ;; Check the type
  (is (g:type-is-enum "GtkJustification"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkJustification")
          (g:gtype (cffi:foreign-funcall "gtk_justification_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:justification
          (gobject:symbol-for-gtype "GtkJustification")))
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
  (is (equal '(DEFINE-G-ENUM "GtkJustification"
                             GTK-JUSTIFICATION
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_justification_get_type")
                             (:LEFT 0)
                             (:RIGHT 1)
                             (:CENTER 2)
                             (:FILL 3))
             (gobject:get-g-type-definition "GtkJustification"))))

;;;     GtkMovementStep
;;;     GtkOrientation
;;;     GtkPackType
;;;     GtkPositionType

;;;     GtkReliefStyle

(test relief-style
  ;; Check the type
  (is (g:type-is-enum "GtkReliefStyle"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkReliefStyle")
          (g:gtype (cffi:foreign-funcall "gtk_relief_style_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:relief-style
          (gobject:symbol-for-gtype "GtkReliefStyle")))
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
  (is (equal '(DEFINE-G-ENUM "GtkReliefStyle"
                             GTK-RELIEF-STYLE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_relief_style_get_type")
                             (:NORMAL 0)
                             (:HALF 1)
                             (:NONE 2))
             (gobject:get-g-type-definition "GtkReliefStyle"))))

;;;     GtkScrollStep
;;;     GtkScrollType
;;;     GtkSelectionMode
;;;     GtkShadowType
;;;     GtkStateFlags
;;;     GtkToolbarStyle
;;;     GtkSortType

;;;     GtkTextDirection  <--- from gtk.widget.lisp

;;;     GtkExpanderStyle
;;;     GtkStateType

;;; --- 2023-2-15 --------------------------------------------------------------
