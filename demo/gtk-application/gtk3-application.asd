;;;; gtk3-application.asd

(asdf:defsystem :gtk3-application
  :author "Dieter Kaiser"
  :license "MIT"
  :serial t
  :depends-on (:cl-cffi-gtk3)
  :components ((:file "gtk3-application")
               (:file "application-command-line")
               (:file "application-inhibit")
               (:file "application-menu")
               (:file "application-notification")
               (:file "application-properties")
               (:file "application-simple")
               (:file "bloatpad")
               (:file "sunny")
              ))

;;; 2024-1-6
