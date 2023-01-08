(in-package :gtk-test)

(def-suite gtk-accel-map :in gtk-suite)
(in-suite gtk-accel-map)

;;;   GtkAccelMap

(test accel-map-class
  ;; Type check
  (is (g:type-is-object "GtkAccelMap"))
  ;; Check the registered name
  (is (eq 'gtk:accel-map
          (gobject:symbol-for-gtype "GtkAccelMap")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccelMap")
          (g:gtype (cffi:foreign-funcall "gtk_accel_map_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkAccelMap")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAccelMap")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkAccelMap")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkAccelMap")))
  ;; Check the signals
  (is (equal '("changed")
             (list-signals "GtkAccelMap")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAccelMap" GTK-ACCEL-MAP
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_accel_map_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkAccelMap"))))

;;;   gtk_accel_map_add_entry
;;;   gtk_accel_map_lookup_entry

(test accel-map-lookup-entry
  ;; Add an accelerator
  (gtk:accel-map-add-entry "<Test>/Edit/Look" (char-code #\l) '(:control-mask))
  (multiple-value-bind (key mods)
      ;; Lookup the accelerator
      (gtk:accel-map-lookup-entry "<Test>/Edit/Look")
    (is (= (char-code #\l) key))
    (is (equal '(:control-mask) mods))))

;;;   gtk_accel_map_change_entry

(test accel-map-change-entry
  (if (not (gtk:accel-map-lookup-entry "<Test>/Edit/Change"))
      ;; Add an accelerator for <Test>/Edit/Change
      (gtk:accel-map-add-entry "<Test>/Edit/Change"
                               (char-code #\s) '(:control-mask)))
  ;; Change the accelerator
  (gtk:accel-map-change-entry "<Test>/Edit/Change"
                              (char-code #\C)
                              '(:control-mask)
                              t)
  (multiple-value-bind (key mods)
      (gtk:accel-map-lookup-entry "<Test>/Edit/Change")
    (is (= (char-code #\C) key))
    (is (equal '(:control-mask) mods))))

;;;   gtk_accel_map_load
;;;   gtk_accel_map_save

(test accel-map-save
  ;; Add an accelerator
  (gtk:accel-map-add-entry "<Test>/Edit/Save" (char-code #\s) '(:control-mask))
  (multiple-value-bind (key mods)
      ;; Lookup the accelerator
      (gtk:accel-map-lookup-entry "<Test>/Edit/Save")
    (is (= (char-code #\s) key))
    (is (equal '(:control-mask) mods)))
  (gtk:accel-map-save (sys-path "test/rtest-gtk3-accel-map.rc")))

;;;   gtk_accel_map_foreach
;;;   gtk_accel_map_load_fd
;;;   gtk_accel_map_save_fd
;;;   gtk_accel_map_load_scanner
;;;   gtk_accel_map_add_filter
;;;   gtk_accel_map_foreach_unfiltered

;;;   gtk_accel_map_get

(test accel-map-get
  (is (eql 'gtk:accel-map (type-of (gtk:accel-map-get)))))

;;;   gtk_accel_map_lock_path
;;;   gtk_accel_map_unlock_path

;;; 2022-12-13
