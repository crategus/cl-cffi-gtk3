(in-package :gtk-test)

(def-suite gdk-visual :in gdk-suite)
(in-suite gdk-visual)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkVisualType

(test visual-type
  ;; Check the type
  (is (g:type-is-enum "GdkVisualType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkVisualType")
          (g:gtype (cffi:foreign-funcall "gdk_visual_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:visual-type
          (gobject:symbol-for-gtype "GdkVisualType")))
  ;; Check the names
  (is (equal '("GDK_VISUAL_STATIC_GRAY" "GDK_VISUAL_GRAYSCALE"
               "GDK_VISUAL_STATIC_COLOR" "GDK_VISUAL_PSEUDO_COLOR"
               "GDK_VISUAL_TRUE_COLOR" "GDK_VISUAL_DIRECT_COLOR")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "GdkVisualType"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "GdkVisualType"))))
  ;; Check the nick names
  (is (equal '("static-gray" "grayscale" "static-color" "pseudo-color"
               "true-color" "direct-color")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "GdkVisualType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkVisualType"
                             GDK-VISUAL-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_visual_type_get_type")
                             (:STATIC-GRAY 0)
                             (:GRAYSCALE 1)
                             (:STATIC-COLOR 2)
                             (:PSEUDO-COLOR 3)
                             (:TRUE-COLOR 4)
                             (:DIRECT-COLOR 5))
             (gobject:get-g-type-definition "GdkVisualType"))))

;;;     GdkByteOrder

(test byte-order
  ;; Check the type
  (is (g:type-is-enum "GdkByteOrder"))
  ;; Check the type initializer
  (is (eq (g:gtype"GdkByteOrder")
          (g:gtype (cffi:foreign-funcall "gdk_byte_order_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:byte-order
          (gobject:symbol-for-gtype "GdkByteOrder")))
  ;; Check the names
  (is (equal '("GDK_LSB_FIRST" "GDK_MSB_FIRST")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "GdkByteOrder"))))
  ;; Check the values
  (is (equal '(0 1)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "GdkByteOrder"))))
  ;; Check the nick names
  (is (equal '("lsb-first" "msb-first")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "GdkByteOrder"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkByteOrder"
                             GDK-BYTE-ORDER
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_byte_order_get_type")
                             (:LSB-FIRST 0)
                             (:MSB-FIRST 1))
             (gobject:get-g-type-definition "GdkByteOrder"))))

;;;     GdkVisual

(test visual-class
  ;; Type check
  (is (g:type-is-object "GdkVisual"))
  ;; Check the registered name
  (is (eq 'gdk:visual
          (gobject:symbol-for-gtype "GdkVisual")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkVisual")
          (g:gtype (cffi:foreign-funcall "gdk_visual_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkVisual")))
  ;; Check the children
  #-windows
  (is (or (equal '("GdkX11Visual")
                 (list-children "GdkVisual"))
          (equal '("GdkWaylandVisual")
                 (list-children "GdkVisual"))))
  #+windows
  (is (equal '()
             (list-children "GdkVisual")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkVisual")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GdkVisual")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GdkVisual")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkVisual" GDK-VISUAL
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_visual_get_type")
                       NIL)
             (gobject:get-g-type-definition "GdkVisual"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-query-depths                                   deprecated

(test query-depths
  (is (listp (gdk:query-depths)))
  (is (every #'integerp (gdk:query-depths))))

;;;     gdk-query-visual-types                             deprecated

(test query-visual-types
  (is (listp (gdk:query-visual-types)))
  (is (member :true-color (gdk:query-visual-types))))

;;;     gdk-list-visuals                                   deprecated

(test list-visuals
  (is (> (length (gdk:list-visuals)) 0))
  (is (every (lambda (x) (eq x 'gdk:visual))
             (mapcar #'type-of (gdk:list-visuals)))))

;;;     gdk-visual-bits-per-rgb                            deprecated

(test visual-bits-per-rgb
  (let ((visual (gdk:visual-system)))
    (is (integerp (gdk:visual-bits-per-rgb visual)))))

;;;     gdk-visual-blue-pixel-details

(test visual-blue-pixel-details
  (let ((visual (gdk:visual-system)))
    (multiple-value-bind (mask shift prec)
        (gdk:visual-blue-pixel-details visual)
      (is (= #xff mask))
      (is (= 0 shift))
      (is (= 8 prec)))))

;;;     gdk-visual-byte-order                              deprecated

(test visual-byte-order
  (let ((visual (gdk:visual-system)))
    (is (eq :lsb-first (gdk:visual-byte-order visual)))))

;;;     gdk-visual-colormap-size                           deprecated

#+nil
(test visual-colormap-size
  (let ((visual (gdk:visual-system)))
    (is (= 256 (gdk:visual-colormap-size visual)))))

;;;     gdk-visual-depth

(test visual-depth
  (let ((visual (gdk:visual-system)))
    #-windows
    (is (or (= 24 (gdk:visual-depth visual))
            (= 32 (gdk:visual-depth visual))))
    #+windows
    (is (= 24 (gdk:visual-depth visual)))))

;;;     gdk-visual-green-pixel-details

(test visual-green-pixel-details
  (let ((visual (gdk:visual-system)))
    (multiple-value-bind (mask shift prec)
        (gdk:visual-green-pixel-details visual)
      (is (= #xff00 mask))
      (is (= 8 shift))
      (is (= 8 prec)))))

;;;     gdk-visual-red-pixel-details

(test visual-red-pixel-details
  (let ((visual (gdk:visual-system)))
    (multiple-value-bind (mask shift prec)
        (gdk:visual-red-pixel-details visual)
      (is (= #xff0000 mask))
      (is (= 16 shift))
      (is (= 8 prec)))))

;;;     gdk-visual-visual-type

(test visual-visual-type
  (let ((visual (gdk:visual-system)))
    (is (member (gdk:visual-visual-type visual)
                '(:static-gray :grayscale :static-color :pseudo-color
                  :true-color :direct-color)))))

;;;     gdk-visual-best-depth                              deprecated

(test visual-best-depth
  (is (integerp (gdk:visual-best-depth)))
  (is (member (gdk:visual-best-depth)
              (mapcar #'gdk:visual-depth (gdk:list-visuals)))))

;;;     gdk-visual-best-type                               deprecated

(test visual-best-type
  (is (member (gdk:visual-best-type)
              '(:static-gray :grayscale :static-color :pseudo-color
                :true-color :direct-color)))
  (is (member (gdk:visual-best-type)
              (mapcar #'gdk:visual-visual-type (gdk:list-visuals)))))

;;;     gdk-visual-system                                  deprecated

(test visual-system
  (is (eq 'gdk:visual (type-of (gdk:visual-system)))))

;;;     gdk-visual-best                                    deprecated

(test visual-best
  (is (eq 'gdk:visual (type-of (gdk:visual-best))))
  (is (= 32 (gdk:visual-depth (gdk:visual-best))))
  (is (eq :true-color (gdk:visual-visual-type (gdk:visual-best)))))

;;;     gdk-visual-best-with-depth                         deprecated

#+nil
(test visual-best-with-depth
  (is-false (gdk:visual-best-with-depth 64))
  (is (eq 'gdk:visual (type-of (gdk:visual-best-with-depth 32))))
  (is (eq 'gdk:visual (type-of (gdk:visual-best-with-depth 24)))))

;;;     gdk-visual-best-with-type                          deprecated

#+nil
(test visual-best-with-type
  (is (eq 'gdk:visual (type-of (gdk:visual-best-with-type :true-color))))
  (is (eq 'gdk:visual (type-of (gdk:visual-best-with-type :direct-color)))))

;;;     gdk-visual-best-with-both                          deprecated

#+nil
(test visual-best-with-both
  (is (eq 'gdk:visual (type-of (gdk:visual-best-with-both 32 :true-color))))
  (is (eq 'gdk:visual (type-of (gdk:visual-best-with-both 24 :true-color))))
  (is-false (gdk:visual-best-with-both 32 :direct-color))
  (is (eq 'gdk:visual (type-of (gdk:visual-best-with-both 24 :direct-color)))))

;;;     gdk visual-screen

(test visual-screen
  (is (eq 'gdk:screen (type-of (gdk:visual-screen (gdk:visual-system))))))

;;; --- 2023-3-3 ---------------------------------------------------------------
