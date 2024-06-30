(in-package :gtk-test)

(def-suite gdk-threads :in gdk-suite)
(in-suite gdk-threads)

(defparameter gdk-threads
              '(gdk:threads-init
                gdk:threads-enter
                gdk:threads-leave
                gdk:threads-add-idle
                gdk:threads-add-timeout
                gdk:threads-add-timeout-seconds))

(export 'gdk-threads)

;;; --- Functions --------------------------------------------------------------

;;;     with-threads-lock

;;;     gdk_threads_init
;;;     gdk_threads_enter
;;;     gdk_threads_leave

;;;     gdk_threads_add_idle
;;;     gdk_threads_add_timeout
;;;     gdk_threads_add_timeout_seconds


;;; 2024-6-29
