(in-package :gtk-test)

(def-suite gdk-rgba :in gdk-suite)
(in-suite gdk-rgba)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkRGBA

(test rgba-structure
  ;; Type check
  (is (g:type-is-a (g:gtype "GdkRGBA") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkRGBA")
          (g:gtype (cffi:foreign-funcall "gdk_rgba_get_type" :size)))))

;;; --- Accessors --------------------------------------------------------------

(test rgba-accessors
  (let ((rgba (gdk:rgba-new :red 0.1d0 :green 0.2d0 :blue 0.3d0 :alpha 0.5d0)))
    (is (= 0.1d0 (gdk:rgba-red rgba)))
    (is (= 0.2d0 (gdk:rgba-green rgba)))
    (is (= 0.3d0 (gdk:rgba-blue rgba)))
    (is (= 0.5d0 (gdk:rgba-alpha rgba)))))

;;; --- Functions --------------------------------------------------------------

;;;    gdk_rgba_new

(test rgba-new
  (is (typep (gdk:rgba-new) 'gdk:rgba))
  (is (typep (gdk:rgba-new :red 0.1 :green 0.2 :blue 0.3 :alpha 0.5) 'gdk:rgba)))

;;;     gdk_rgba_copy

(test rgba-copy
  (let ((color (gdk:rgba-new :red 0.1 :green 0.2 :blue 0.3 :alpha 0.4)))
    (is (gdk:rgba-equal color (gdk:rgba-copy color)))))

;;;     gdk_rgba_parse

(test rgba-parse
  (is (typep (gdk:rgba-parse "red") 'gdk:rgba))
  (is (= 1.0d0 (gdk:rgba-red (gdk:rgba-parse "red")))))

;;;     gdk_rgba_equal

(test rgba-equal
  (is (gdk:rgba-equal (gdk:rgba-parse "red") (gdk:rgba-parse "red"))))

;;;     gdk_rgba_hash

(test rgba-hash
  (is (integerp (gdk:rgba-hash (gdk:rgba-parse "red")))))

;;;     gdk_rgba_to_string

(test rgba-to-string
  (is (string= "rgb(255,0,0)" (gdk:rgba-to-string (gdk:rgba-parse "red")))))

;;; 2022-12-11
