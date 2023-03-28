(in-package :gtk-test)

(def-suite gdk-key-values :in gdk-suite)
(in-suite gdk-key-values)

;;;     GdkKeymap

(test keymap-class
  ;; Type check
  (is (g:type-is-object "GdkKeymap"))
  ;; Check the registered name
  (is (eq 'gdk:keymap
          (gobject:symbol-for-gtype "GdkKeymap")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkKeymap")
          (g:gtype (cffi:foreign-funcall "gdk_keymap_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkKeymap")))
  ;; Check the children
  #-windows
  (is (or (equal '("GdkX11Keymap")
                 (list-children "GdkKeymap"))
          (equal '("GdkWaylandKeymap")
                 (list-children "GdkKeymap"))))
  #+windows
  (is (equal '("GdkWin32Keymap")
             (list-children "GdkKeymap")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkKeymap")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GdkKeymap")))
  ;; Check the signals
  (is (equal '("direction-changed" "keys-changed" "state-changed")
             (list-signals "GdkKeymap")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkKeymap" GDK-KEYMAP
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_keymap_get_type")
                       NIL)
             (gobject:get-g-type-definition "GdkKeymap"))))

;;;   gdk_keymap_get_default

(test keymap-default
  (is (typep (gdk:keymap-default) 'gdk:keymap)))

;;;   gdk_keymap_get_for_display

(test keymap-for-display
  (let ((display (gdk:display-default)))
    (is (typep (gdk:keymap-for-display display) 'gdk:keymap))))

;;;   gdk_keymap_lookup_key

#-windows
(test keymap-lookup-key
  (let ((keymap (gdk:keymap-for-display (gdk:display-default))))
    (is (=  43 (gdk:keymap-lookup-key keymap 35 0 0)))
    (is (=  42 (gdk:keymap-lookup-key keymap 35 0 1)))
    (is (= 126 (gdk:keymap-lookup-key keymap 35 0 2)))
    (is (= 175 (gdk:keymap-lookup-key keymap 35 0 3)))))

;;;     gdk_keymap_translate_keyboard_state

#-windows
(test keymap-translate-keyboard-state
  (let ((keymap (gdk:keymap-for-display (gdk:display-default))))
    ;; The key "+" with the name "plus"
    (is (or (equal '(43 0 0 (:SHIFT-MASK :LOCK-MASK :MOD5-MASK))
                   (multiple-value-list
                       (gdk:keymap-translate-keyboard-state keymap
                                                            35
                                                            0
                                                            0)))
            (equal '(43 0 0 NIL)
                   (multiple-value-list
                       (gdk:keymap-translate-keyboard-state keymap
                                                            35
                                                            0
                                                            0)))))
    ;; The key "-" with the name "minus"
    (is (or (equal '(42 0 1 (:SHIFT-MASK :LOCK-MASK :MOD5-MASK))
                   (multiple-value-list
                       (gdk:keymap-translate-keyboard-state keymap
                                                            35
                                                            :shift-mask
                                                            0)))
            (equal '(42 0 1 (:SHIFT-MASK))
                   (multiple-value-list
                       (gdk:keymap-translate-keyboard-state keymap
                                                            35
                                                            :shift-mask
                                                            0)))))
    ;; The key "~" with the name "asciitilde"
    (is (or (equal '(126 0 2 (:SHIFT-MASK :LOCK-MASK :MOD5-MASK))
                   (multiple-value-list
                       (gdk:keymap-translate-keyboard-state keymap
                                                            35
                                                            :mod5-mask
                                                            0)))
            (equal '(126 0 2 (:MOD5-MASK))
                   (multiple-value-list
                       (gdk:keymap-translate-keyboard-state keymap
                                                            35
                                                            :mod5-mask
                                                            0)))))))

;;;     gdk_keymap_get_entries_for_keyval

#-windows
(test gdk-keymap-entries-for-keyval
  (let ((keymap (gdk:keymap-for-display (gdk:display-default))))
    (is (or (equal '((35 0 0))
                   (gdk:keymap-entries-for-keyval keymap 43))
            (equal '((35 0 0) (35 1 0))
                   (gdk:keymap-entries-for-keyval keymap 43))))
    (is (or (equal '((35 0 1))
                   (gdk:keymap-entries-for-keyval keymap 42))
            (equal '((35 0 1) (35 1 1))
                   (gdk:keymap-entries-for-keyval keymap 42))))
    (is (or (equal '((35 0 2))
                   (gdk:keymap-entries-for-keyval keymap 126))
            (equal '((35 0 2) (35 1 2))
                   (gdk:keymap-entries-for-keyval keymap 126))))
    (is (or (equal '((35 0 3))
                   (gdk:keymap-entries-for-keyval keymap 175))
            (equal '((35 0 3) (35 1 3))
                   (gdk:keymap-entries-for-keyval keymap 175))))))

;;;     gdk_keymap_get_entries_for_keycode

#-windows
(test keymap-entries-for-keycode
  (let ((keymap (gdk:keymap-for-display (gdk:display-default))))
    (is (or (equal '((43 35 0 0) (42 35 0 1) (126 35 0 2) (175 35 0 3))
                   (gdk:keymap-entries-for-keycode keymap 35))
            (equal '((43 35 0 0) (43 35 0 1) (43 35 0 2) (43 35 0 3) (43 35 1 0)
                     (43 35 1 1) (43 35 1 2) (43 35 1 3))
                   (gdk:keymap-entries-for-keycode keymap 35))))))

;;;     gdk_keymap_get_direction

(test keymap-direction
  (is (eq :ltr (gdk:keymap-direction (gdk:keymap-default)))))

;;;     gdk_keymap_have_bidi_layouts

(test keymap-have-bidi-layouts
  (is-false (gdk:keymap-have-bidi-layouts (gdk:keymap-default))))

;;;     gdk_keymap_get_caps_lock_state

(test keymap-caps-lock-state
  (is-false (gdk:keymap-caps-lock-state (gdk:keymap-default))))

;;;     gdk_keymap_get_num_lock_state

(test gdk-keymap-num-lock-state
  (is (typep (gdk:keymap-num-lock-state (gdk:keymap-default)) 'boolean)))

;;;     gdk_keymap_get_scroll_lock_state

(test keymap-scroll-lock-state
  (is-false (gdk:keymap-scroll-lock-state (gdk:keymap-default))))

;;;     gdk_keymap_get_modifier_state

(test keymap-modifier-state
  #-windows
  (is (or (equal '(:MOD2-MASK)
                 (gdk:keymap-modifier-state (gdk:keymap-default)))
          (equal '()
                 (gdk:keymap-modifier-state (gdk:keymap-default)))))
  #+windows
  (is (equal '()
             (gdk:keymap-modifier-state (gdk:keymap-default)))))

;;;     gdk_keymap_add_virtual_modifiers

(test keymap-add-virtual-modifiers
  (let ((keymap (gdk:keymap-for-display (gdk:display-default))))
    #-windows
    (is (equal '(:MOD4-MASK :SUPER-MASK :HYPER-MASK)
               (gdk:keymap-add-virtual-modifiers keymap '(:mod4-mask))))
    #+windows
    (is (equal '(:MOD4-MASK)
               (gdk:keymap-add-virtual-modifiers keymap '(:mod4-mask))))))

;;;     gdk_keymap_map_virtual_modifiers

#-windows
(test keymap-map-virtual-modifiers
  (let ((keymap (gdk:keymap-for-display (gdk:display-default))))
    (is (equal '(:MOD4-MASK :SUPER-MASK)
               (gdk:keymap-map-virtual-modifiers keymap '(:super-mask))))))

;;;     gdk_keymap_get_modifier_mask

(test keymap-modifier-mask
  (let ((keymap (gdk:keymap-for-display (gdk:display-default))))
    (is (equal '(:control-mask)
               (gdk:keymap-modifier-mask keymap :primary-accelerator)))
    (is (equal '()
               (gdk:keymap-modifier-mask keymap :context-menu)))
    (is (equal '(:SHIFT-MASK)
               (gdk:keymap-modifier-mask keymap :extend-selection)))
    (is (equal '(:CONTROL-MASK)
               (gdk:keymap-modifier-mask keymap :modify-selection)))
    (is (equal '(:CONTROL-MASK :MOD1-MASK)
               (gdk:keymap-modifier-mask keymap :no-text-input)))
    (is (equal '()
               (gdk:keymap-modifier-mask keymap :shift-group)))
    (is (equal '(:SHIFT-MASK :CONTROL-MASK :MOD1-MASK :SUPER-MASK
                 :HYPER-MASK :META-MASK)
               (gdk:keymap-modifier-mask keymap :default-mod-mask)))))

;;;     gdk_keyval_name

(test keyval-name
  (is (string= "A" (gdk:keyval-name 65)))
  (is (string= "B" (gdk:keyval-name 66))))

;;;     gdk_keyval_from_name

(test keyval-from-name
  (is (= 65 (gdk:keyval-from-name "A")))
  (is (= 66 (gdk:keyval-from-name "B"))))

;;;     gdk_keyval_convert_case

(test keyval-convert-case
  (is (= 97 (gdk:keyval-convert-case 65))))

;;;     gdk_keyval_to_upper

(test keyval-to-upper
  (is (= 65 (gdk:keyval-to-upper 97))))

;;;     gdk_keyval_to_lower

(test keyval-to-lower
  (is (= 97 (gdk:keyval-to-lower 65))))

;;;     gdk_keyval_is_upper

(test keyval-is-upper
  (is-true  (gdk:keyval-is-upper (gdk:keyval-from-name "A")))
  (is-false (gdk:keyval-is-upper (gdk:keyval-from-name "a"))))

;;;     gdk_keyval_is_lower

(test keyval-is-lower
  (is-true  (gdk:keyval-is-upper (gdk:keyval-from-name "A")))
  (is-false (gdk:keyval-is-upper (gdk:keyval-from-name "a"))))

;;;     gdk_keyval_to_unicode

(test keyval-to-unicode
  (is (eq #\A  (gdk:keyval-to-unicode (gdk:keyval-from-name "A")))))

;;;     gdk_unicode_to_keyval

(test unicode-to-keyval
  (is (eq 65 (gdk:unicode-to-keyval #\A))))

;;; --- 2023-3-26 --------------------------------------------------------------
