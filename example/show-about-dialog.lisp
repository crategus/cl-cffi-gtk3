(in-package :gtk3-example)

(defun license-text ()
  (format nil
          "Permission is hereby granted, free of charge, to any person ~
           obtaining a copy of this software and associated documentation ~
           files (the 'Software'), to deal in the Software without ~
           restriction, including without limitation the rights to use, copy, ~
           modify, merge, publish, distribute, sublicense, and/or sell copies ~
           of the Software, and to permit persons to whom the Software is ~
           furnished to do so, subject to the following conditions: ~%~% ~
           The above copyright notice and this permission notice shall be ~
           included in all copies or substantial portions of the ~
           Software. ~% ~% ~
           THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, ~
           EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF ~
           MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND ~
           NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS ~
           BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ~
           ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN ~
           CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE ~
           SOFTWARE."))

(defun example-show-about-dialog ()
  (within-main-loop
    (let (;; Create a toplevel window
          (window (gtk:window-new :toplevel))
          (button (make-instance 'gtk:button :label "Show About Dialog")))
      ;; Signal handler for the window to handle the signal "destroy"
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g:signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           ;; Create and show an about dialog
           (gtk:show-about-dialog window :title "About Dialog"
                                         :program-name "ExampleDialog"
                                         :version "0.00"
                                         :copyright "(c) Dieter Kaiser"
                                         :website
                                         "http:\\github.com/crategus/cl-cffi-gtk3"
                                         :website-label "Project web site"
                                         :license (license-text)
                                         :authors '("Dieter Kaiser")
                                         :documenters '("Dieter Kaiser")
                                         :artists '("None")
                                         :logo-icon-name
                                         "applications-development"
                                         :wrap-license t)))
      ;; Add the button to the window
      (gtk:container-add window button)
      ;; Show the window
      (gtk:widget-show-all window))))

