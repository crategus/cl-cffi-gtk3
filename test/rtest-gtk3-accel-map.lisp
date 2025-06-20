(in-package :gtk-test)

(def-suite gtk-accel-map :in gtk-suite)
(in-suite gtk-accel-map)

;;;   GtkAccelMap

(test gtk-accel-map-class
  ;; Check type
  (is (g:type-is-object "GtkAccelMap"))
  ;; Check registered name
  (is (eq 'gtk:accel-map
          (glib:symbol-for-gtype "GtkAccelMap")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccelMap")
          (g:gtype (cffi:foreign-funcall "gtk_accel_map_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkAccelMap")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAccelMap")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkAccelMap")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkAccelMap")))
  ;; Check signals
  (is (equal '("changed")
             (glib-test:list-signals "GtkAccelMap")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAccelMap" GTK:ACCEL-MAP
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_accel_map_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkAccelMap"))))

;;;   gtk_accel_map_add_entry
;;;   gtk_accel_map_lookup_entry

(test gtk-accel-map-lookup-entry
  ;; Add an accelerator
  (gtk:accel-map-add-entry "<Test>/Edit/Look" (char-code #\l) '(:control-mask))
  (multiple-value-bind (key mods)
      ;; Lookup the accelerator
      (gtk:accel-map-lookup-entry "<Test>/Edit/Look")
    (is (= (char-code #\l) key))
    (is (equal '(:control-mask) mods))))

;;;   gtk_accel_map_change_entry

(test gtk-accel-map-change-entry
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

(test gtk-accel-map-load/save
  ;; Add an accelerator
  (gtk:accel-map-add-entry "<Test>/Edit/Save" (char-code #\s) '(:control-mask))
  (multiple-value-bind (key mods)
      ;; Lookup the accelerator
      (gtk:accel-map-lookup-entry "<Test>/Edit/Save")
    (is (= (char-code #\s) key))
    (is (equal '(:control-mask) mods)))
  (is-false (gtk:accel-map-save
                (glib-sys:sys-path "test/out/rtest-gtk3-accel-map.rc")))
  (is-false (gtK:accel-map-load
                (glib-sys:sys-path "test/out/rtest-gtk3-accel-map.rc"))))

;;;   gtk_accel_map_foreach
;;;   gtk_accel_map_load_fd
;;;   gtk_accel_map_save_fd
;;;   gtk_accel_map_load_scanner
;;;   gtk_accel_map_add_filter
;;;   gtk_accel_map_foreach_unfiltered

;;;   gtk_accel_map_get

;; The returned accel map is a global object of the C library

(test gtk-accel-map-get
  (glib-test:with-check-memory ((accelmap 2) :strong 1)
    (is (typep (setf accelmap (gtk:accel-map-get)) 'gtk:accel-map))))

;;;   gtk_accel_map_lock_path
;;;   gtk_accel_map_unlock_path

;;; 2025-06-18
