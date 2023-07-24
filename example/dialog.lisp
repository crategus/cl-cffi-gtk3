;;;; Example Dialog Windows - 2023-7-24

(in-package :gtk3-example)

(defun dialog-license-text ()
  (format nil
          "Permission is hereby granted, free of charge, to any person ~
           obtaining a copy of this software and associated documentation ~
           files (the 'Software'), to deal in the Software without ~
           restriction, including without limitation the rights to use, copy, ~
           modify, merge, publish, distribute, sublicense, and/or sell copies ~
           of the Software, and to permit persons to whom the Software is ~
           furnished to do so, subject to the following conditions: ~
           ~%~% ~
           The above copyright notice and this permission notice shall be ~
           included in all copies or substantial portions of the Software. ~
           ~%~% ~
           THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, ~
           EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF ~
           MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND ~
           NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS ~
           BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ~
           ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN ~
           CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE ~
           SOFTWARE."))

(defun create-dialog (&optional (headerbar-p -1))
  (let ((dialog (make-instance 'gtk:dialog
                               :use-header-bar headerbar-p
                               :title "Dialog Window")))
    ;; Add a border width to the vbox of the content area
    (setf (gtk:container-border-width (gtk:dialog-content-area dialog)) 12)
    ;; Add a label widget with text to the content area
    (let ((vbox (make-instance 'gtk:box
                               :orientation :vertical
                               :border-width 12))
          (label (make-instance 'gtk:label
                                :wrap t
                                :label
                                (format nil
                                        "The content area is the place to ~
                                         put in the widgets."))))
      (gtk:box-pack-start vbox label)
      (gtk:box-pack-start (gtk:dialog-content-area dialog) vbox)
      ;; Show the content area of the dialog
      (gtk:widget-show-all (gtk:dialog-content-area dialog)))
    ;; Add buttons with a stock id to the action area
    (gtk:dialog-add-button dialog "gtk-yes" :yes)
    (gtk:dialog-add-button dialog "gtk-cancel" :cancel)
    (gtk:dialog-set-default-response dialog :cancel)
    ;; Run the dialog and print the message on the console
    (format t "Response is: ~s~%" (gtk:dialog-run dialog))
    ;; Destroy the dialog
    (gtk:widget-destroy dialog)))

(defun create-message-dialog (&optional (mtype :info))
  (let ((dialog (make-instance 'gtk:message-dialog
                               :message-type mtype
                               :buttons :cancel
                               :text "Message Dialog"
                               :secondary-text
                               (format nil
                                       "This is a message dialog of type ~a."
                                       mtype))))
    ;; Run the message dialog
    (gtk:dialog-run dialog)
    ;; Destroy the message dialog
    (gtk:widget-destroy dialog)))

(defun create-about-dialog (&optional (headerbar-p 1))
  (let* ((version (asdf:component-version (asdf:find-system :gtk3-example nil)))
         (dialog (make-instance 'gtk:about-dialog
                                :use-header-bar headerbar-p
                                :program-name "GTK Demo"
                                :version version
                                :copyright "(c) Dieter Kaiser"
                                :website
                                "github.com/crategus/cl-cffi-gtk"
                                :website-label "Project web site"
                                :license (dialog-license-text)
                                :authors '("Dieter Kaiser")
                                :documenters '("Dieter Kaiser")
                                :artists '("None")
                                :logo-icon-name "applications-development"
                                :wrap-license t)))
    ;; Run the about dialog
    (gtk:dialog-run dialog)
    ;; Destroy the about dialog
    (gtk:widget-destroy dialog)))

(defun example-dialogs (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :application application
                                 :type :toplevel
                                 :title "Example Dialogs"
                                 :default-width 270
                                 :border-width 12))
          (vbox (make-instance 'gtk:box
                               :orientation :vertical
                               :spacing 6))
          (check (make-instance 'gtk:check-button
                                :margin-bottom 18
                                :label "Show Dialog with Header Bar"))
          (radio (gtk:radio-button-new-with-label nil "Info")))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show dialog
      (let ((button (make-instance 'gtk:button
                                   :label "Show Dialog")))
        (gtk:box-pack-start vbox button)
        (g:signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             ;; Create and show the dialog
             (create-dialog (if (gtk:toggle-button-active check)
                                1
                                -1)))))
      ;; Show about dialog
      (let ((button (make-instance 'gtk:button
                                   :label "Show About Dialog")))
        (gtk:box-pack-start vbox button)
        (g:signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             ;; Create and show the about dialog
             (create-about-dialog (if (gtk:toggle-button-active check)
                                      1
                                      -1)))))
      ;; Add the check button to the vertical box
      (gtk:box-pack-start vbox check)
      ;; Show message dialog
      (let ((mtype :info)
            (button (make-instance 'gtk:button
                                   :label "Show Message Dialog")))
        (gtk:box-pack-start vbox button)
        (g:signal-connect button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             ;; Select the active radio button
             (dolist (radio (gtk:radio-button-get-group radio))
               (when (gtk:toggle-button-active radio)
                 (setf mtype
                       (cdr (assoc (gtk:button-label radio)
                                   '(("Info" . :info)
                                     ("Warning" . :warning)
                                     ("Question" . :question)
                                     ("Error" . :error))
                                   :test #'string=)))))
             ;; Create and show the message dialog
             (create-message-dialog mtype))))
      ;; Add the radio buttons to select the message type
      (let ((hbox (make-instance 'gtk:box
                                 :orientation :horizontal)))
        (gtk:box-pack-start hbox radio)
        (setf radio
              (gtk:radio-button-new-with-label
                                          (gtk:radio-button-get-group radio)
                                          "Warning"))
        (gtk:box-pack-start hbox radio)
        (setf radio
              (gtk:radio-button-new-with-label
                                          (gtk:radio-button-get-group radio)
                                          "Question"))
        (gtk:box-pack-start hbox radio)
        (setf radio
              (gtk:radio-button-new-with-label
                                          (gtk:radio-button-get-group radio)
                                          "Error"))
        (gtk:box-pack-start hbox radio)
        (gtk:container-add vbox hbox))
      ;; Pack and show the widgets
      (gtk:container-add window vbox)
      (gtk:widget-show-all window))))
