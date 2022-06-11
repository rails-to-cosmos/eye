;; -*- lexical-binding: t; -*-

(require 'eye)

(defvar eye-date-time-data '())

(defconst eye-date-time-schema
  (list (make-ctbl:cmodel :title "Key")
        (make-ctbl:cmodel :title "Value")))

(defun eye-date-time-daemon ()
  (setq eye-date-time-data
        (let ((now (current-time)))
          (a-assoc eye-date-time-data
                   :date (format-time-string "%d %b %Y, %a" now)
                   :time (format-time-string "%H:%M:%S %Z" now)))))

(eye-def-widget date-time
  :model (make-ctbl:model
          :column-model eye-date-time-schema
          :data (cl-loop for (key . value) in eye-date-time-data
                   collect (list (->> key
                                      symbol-name
                                      s-titleize
                                      (s-replace-regexp "^:" ""))
                                 value)))
  :timers (list
           (a-list :fn #'eye-date-time-daemon))
  :on-click (lambda ()
              (message "Hello")))

(provide 'eye-date-time)
