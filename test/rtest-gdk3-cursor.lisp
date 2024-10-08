(in-package :gtk-test)

(def-suite gdk-cursor :in gdk-suite)
(in-suite gdk-cursor)

#+windows
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:foreign-funcall "gdk_win32_cursor_get_type" :size))

;;; --- GdkCursorType ----------------------------------------------------------

(test gdk-cursor-type
  ;; Check type
  (is (g:type-is-enum "GdkCursorType"))
  ;; Check registered name
  (is (eq 'gdk:cursor-type
          (glib:symbol-for-gtype "GdkCursorType")))
  ;; Check names
  (is (equal '("GDK_X_CURSOR" "GDK_ARROW" "GDK_BASED_ARROW_DOWN"
               "GDK_BASED_ARROW_UP" "GDK_BOAT" "GDK_BOGOSITY"
               "GDK_BOTTOM_LEFT_CORNER" "GDK_BOTTOM_RIGHT_CORNER"
               "GDK_BOTTOM_SIDE" "GDK_BOTTOM_TEE" "GDK_BOX_SPIRAL"
               "GDK_CENTER_PTR" "GDK_CIRCLE" "GDK_CLOCK" "GDK_COFFEE_MUG"
               "GDK_CROSS" "GDK_CROSS_REVERSE" "GDK_CROSSHAIR"
               "GDK_DIAMOND_CROSS" "GDK_DOT" "GDK_DOTBOX" "GDK_DOUBLE_ARROW"
               "GDK_DRAFT_LARGE" "GDK_DRAFT_SMALL" "GDK_DRAPED_BOX"
               "GDK_EXCHANGE" "GDK_FLEUR" "GDK_GOBBLER" "GDK_GUMBY" "GDK_HAND1"
               "GDK_HAND2" "GDK_HEART" "GDK_ICON" "GDK_IRON_CROSS"
               "GDK_LEFT_PTR" "GDK_LEFT_SIDE" "GDK_LEFT_TEE" "GDK_LEFTBUTTON"
               "GDK_LL_ANGLE" "GDK_LR_ANGLE" "GDK_MAN" "GDK_MIDDLEBUTTON"
               "GDK_MOUSE" "GDK_PENCIL" "GDK_PIRATE" "GDK_PLUS"
               "GDK_QUESTION_ARROW" "GDK_RIGHT_PTR" "GDK_RIGHT_SIDE"
               "GDK_RIGHT_TEE" "GDK_RIGHTBUTTON" "GDK_RTL_LOGO" "GDK_SAILBOAT"
               "GDK_SB_DOWN_ARROW" "GDK_SB_H_DOUBLE_ARROW" "GDK_SB_LEFT_ARROW"
               "GDK_SB_RIGHT_ARROW" "GDK_SB_UP_ARROW" "GDK_SB_V_DOUBLE_ARROW"
               "GDK_SHUTTLE" "GDK_SIZING" "GDK_SPIDER" "GDK_SPRAYCAN"
               "GDK_STAR" "GDK_TARGET" "GDK_TCROSS" "GDK_TOP_LEFT_ARROW"
               "GDK_TOP_LEFT_CORNER" "GDK_TOP_RIGHT_CORNER" "GDK_TOP_SIDE"
               "GDK_TOP_TEE" "GDK_TREK" "GDK_UL_ANGLE" "GDK_UMBRELLA"
               "GDK_UR_ANGLE" "GDK_WATCH" "GDK_XTERM" "GDK_LAST_CURSOR"
               "GDK_BLANK_CURSOR" "GDK_CURSOR_IS_PIXMAP")
             (glib-test:list-enum-item-names "GdkCursorType")))
  ;; Check values
  (is (equal '(0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44
               46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86
               88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120
               122 124 126 128 130 132 134 136 138 140 142 144 146 148 150 152
               153 -2 -1)
             (glib-test:list-enum-item-values "GdkCursorType")))
  ;; Check names
  (is (equal '("x-cursor" "arrow" "based-arrow-down" "based-arrow-up" "boat"
               "bogosity" "bottom-left-corner" "bottom-right-corner"
               "bottom-side" "bottom-tee" "box-spiral" "center-ptr" "circle"
               "clock" "coffee-mug" "cross" "cross-reverse" "crosshair"
               "diamond-cross" "dot" "dotbox" "double-arrow" "draft-large"
               "draft-small" "draped-box" "exchange" "fleur" "gobbler" "gumby"
               "hand1" "hand2" "heart" "icon" "iron-cross" "left-ptr"
               "left-side" "left-tee" "leftbutton" "ll-angle" "lr-angle" "man"
               "middlebutton" "mouse" "pencil" "pirate" "plus" "question-arrow"
               "right-ptr" "right-side" "right-tee" "rightbutton" "rtl-logo"
               "sailboat" "sb-down-arrow" "sb-h-double-arrow" "sb-left-arrow"
               "sb-right-arrow" "sb-up-arrow" "sb-v-double-arrow" "shuttle"
               "sizing" "spider" "spraycan" "star" "target" "tcross"
               "top-left-arrow" "top-left-corner" "top-right-corner" "top-side"
               "top-tee" "trek" "ul-angle" "umbrella" "ur-angle" "watch"
               "xterm" "last-cursor" "blank-cursor" "cursor-is-pixmap")
             (glib-test:list-enum-item-nicks "GdkCursorType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkCursorType" GDK:CURSOR-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_cursor_type_get_type")
                       (:X-CURSOR 0)
                       (:ARROW 2)
                       (:BASED-ARROW-DOWN 4)
                       (:BASED-ARROW-UP 6)
                       (:BOAT 8)
                       (:BOGOSITY 10)
                       (:BOTTOM-LEFT-CORNER 12)
                       (:BOTTOM-RIGHT-CORNER 14)
                       (:BOTTOM-SIDE 16)
                       (:BOTTOM-TEE 18)
                       (:BOX-SPIRAL 20)
                       (:CENTER-PTR 22)
                       (:CIRCLE 24)
                       (:CLOCK 26)
                       (:COFFEE-MUG 28)
                       (:CROSS 30)
                       (:CROSS-REVERSE 32)
                       (:CROSSHAIR 34)
                       (:DIAMOND-CROSS 36)
                       (:DOT 38)
                       (:DOTBOX 40)
                       (:DOUBLE-ARROW 42)
                       (:DRAFT-LARGE 44)
                       (:DRAFT-SMALL 46)
                       (:DRAPED-BOX 48)
                       (:EXCHANGE 50)
                       (:FLEUR 52)
                       (:GOBBLER 54)
                       (:GUMBY 56)
                       (:HAND1 58)
                       (:HAND2 60)
                       (:HEART 62)
                       (:ICON 64)
                       (:IRON-CROSS 66)
                       (:LEFT-PTR 68)
                       (:LEFT-SIDE 70)
                       (:LEFT-TEE 72)
                       (:LEFTBUTTON 74)
                       (:LL-ANGLE 76)
                       (:LR-ANGLE 78)
                       (:MAN 80)
                       (:MIDDLEBUTTON 82)
                       (:MOUSE 84)
                       (:PENCIL 86)
                       (:PIRATE 88)
                       (:PLUS 90)
                       (:QUESTION-ARROW 92)
                       (:RIGHT-PTR 94)
                       (:RIGHT-SIDE 96)
                       (:RIGHT-TEE 98)
                       (:RIGHTBUTTON 100)
                       (:RTL-LOGO 102)
                       (:SAILBOAT 104)
                       (:SB-DOWN-ARROW 106)
                       (:SB-H-DOUBLE-ARROW 108)
                       (:SB-LEFT-ARROW 110)
                       (:SB-RIGHT-ARROW 112)
                       (:SB-UP-ARROW 114)
                       (:SB-V-DOUBLE-ARROW 116)
                       (:SHUTTLE 118)
                       (:SIZING 120)
                       (:SPIDER 122)
                       (:SPRAYCAN 124)
                       (:STAR 126)
                       (:TARGET 128)
                       (:TCROSS 130)
                       (:TOP-LEFT-ARROW 132)
                       (:TOP-LEFT-CORNER 134)
                       (:TOP-RIGHT-CORNER 136)
                       (:TOP-SIDE 138)
                       (:TOP-TEE 140)
                       (:TREK 142)
                       (:UL-ANGLE 144)
                       (:UMBRELLA 146)
                       (:UR-ANGLE 148)
                       (:WATCH 150)
                       (:XTERM 152)
                       (:LAST-CURSOR 153)
                       (:BLANK-CURSOR -2)
                       (:CURSOR-IS-PIXMAP -1))
             (gobject:get-gtype-definition "GdkCursorType"))))

;;; --- GdkCursor --------------------------------------------------------------

;; Create a new cursor to make sure that GdkWaylandCursor is present.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gdk:cursor-new :watch))

(test gdk-cursor-class
  ;; Check tyüe
  (is (g:type-is-object "GdkCursor"))
  ;; Check registered name
  (is (eq 'gdk:cursor
          (glib:symbol-for-gtype "GdkCursor")))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkCursor")))
  ;; Check children
  ;; TODO: Ensure the children are present when testing
  #-windows
  (is (equal '("GdkWaylandCursor" "GdkX11Cursor")
             (glib-test:list-children "GdkCursor")))
  #+windows
  (is (equal '("GdkWin32Cursor")
             (glib-test:list-children "GdkCursor")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkCursor")))
  ;; Check class properties
  (is (equal '("cursor-type" "display")
             (glib-test:list-properties "GdkCursor")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkCursor" GDK:CURSOR
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_cursor_get_type")
                       ((CURSOR-TYPE CURSOR-CURSOR-TYPE
                         "cursor-type" "GdkCursorType" T NIL)
                        (DISPLAY CURSOR-DISPLAY "display" "GdkDisplay" T NIL)))
             (gobject:get-gtype-definition "GdkCursor"))))

;;; --- gdk:cursor-properties --------------------------------------------------

(test gdk-cursor-properties
  (let ((cursor (gdk:cursor-new-for-display (gdk:display-default) :hand1)))
    (is (typep (gdk:cursor-display cursor) 'gdk:display))
    (is (or (eq :hand1 (gdk:cursor-cursor-type cursor))
            (eq :cursor-is-pixmap (gdk:cursor-cursor-type cursor))))))

;;; --- gdk:cursor-new ---------------------------------------------------------

(test gdk-cursor-new
  (let ((cursor (gdk:cursor-new :watch)))
    (is (typep cursor 'gdk:cursor))
    ;; TODO: We get :watch or :cursor-is-pixmap, check this more careful
    (is (or (eq :watch (gdk:cursor-cursor-type cursor))
            (eq :cursor-is-pixmap (gdk:cursor-cursor-type cursor))))
    (is (typep (gdk:cursor-display cursor) 'gdk:display))))

;;; --- gdk:cursor-new-from-pixbuf ---------------------------------------------

(test gdk-cursor-new-from-pixbuf
  (let* ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png"))
         (pixbuf (gdk:pixbuf-new-from-file path))
         (display (gdk:display-default)))
    (is (typep pixbuf 'gdk:pixbuf))
    (is (typep (gdk:cursor-new-from-pixbuf display pixbuf 12 12) 'gdk:cursor))))

;;; --- gdk:cursor-new-from-surface --------------------------------------------

(test gdk-cursor-new-from-surface
  (cairo:with-image-surface (surface :rgb24 120 120)
    (let ((display (gdk:display-default)))
      (is (typep (gdk:cursor-new-from-surface display surface 0.0 0.0)
                 'gdk:cursor)))))

;;; --- gdk:cursor-new-from-name -----------------------------------------------

(test gdk-cursor-new-from-name
  (let ((display (gdk:display-default)))
    (is (typep (gdk:cursor-new-from-name display "pointer") 'gdk:cursor))))

;;; --- gdk:cursor-new-for-display ---------------------------------------------

(test gdk-cursor-new-for-display
  (let ((display (gdk:display-default)))
    (is (typep (gdk:cursor-new-for-display display :hand1) 'gdk:cursor))))

;;; --- gdk:cursor-image -------------------------------------------------------

(test gdk-cursor-image
  (let* ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png"))
         (pixbuf (gdk:pixbuf-new-from-file path))
         (display (gdk:display-default))
         cursor)
    (is (typep pixbuf 'gdk-pixbuf:pixbuf))
    (is (typep (setf cursor
                     (gdk:cursor-new-from-pixbuf display pixbuf 12 12))
               'gdk:cursor))
    #-windows
    (is-false (gdk:cursor-image cursor))
    #+windows
    (is (typep (gdk:cursor-image cursor) 'gdk-pixbuf:pixbuf))))

;;; --- gdk:cursor-surface -----------------------------------------------------

(test gdk-cursor-surface
  (let ((cursor (gdk:cursor-new :hand1)))
    (multiple-value-bind (surface xhot yhot)
        (gdk:cursor-surface cursor)
      (is (cffi:pointerp surface))
      #-windows
      (is (approx-equal 0.0d0 xhot))
      #+windows
      (is (approx-equal 6.0d0 xhot))
      (is (approx-equal 0.0d0 yhot)))))

;;; 2024-9-22
