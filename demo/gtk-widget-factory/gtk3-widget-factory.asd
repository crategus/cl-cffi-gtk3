;;;; gtk3-widget-factory.asd

(asdf:defsystem :gtk3-widget-factory
  :name "gtk3-widget-factory"
  :author "Dieter Kaiser"
  :version "0.1.0"
  :license "MIT"
  :serial t
  :depends-on (:cl-cffi-gtk3)
  :components ((:file "gtk3-widget-factory")))

;;; 2024-1-6
