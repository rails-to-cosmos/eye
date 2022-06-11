;; -*- lexical-binding: t; -*-

(require 'eye)
(require 'battery)

(defvar eye-battery-status-data '())

(defconst eye-battery-status-schema
  (list (make-ctbl:cmodel :title "Key")
        (make-ctbl:cmodel :title "Value")))

(defun eye-battery-status-daemon ()
  (setq eye-battery-status-data
        (let ((battery-status (funcall battery-status-function)))
          (a-assoc eye-battery-status-data
                   :power (battery-format "%L" battery-status)
                   :battery (battery-format "%B" battery-status)
                   :load (battery-format "%p%%" battery-status)
                   :remaining (battery-format "%t" battery-status)))))

(eye-def-widget battery-status
  :model (make-ctbl:model
          :column-model eye-battery-status-schema
          :data (cl-loop for (key . value) in eye-battery-status-data
                   collect (list (->> key
                                      symbol-name
                                      s-titleize
                                      (s-replace-regexp "^:" ""))
                                 value)))
  :timers (list
           (a-list :fn #'eye-battery-status-daemon))
  :on-click (lambda ()
              (message "Hello")))

(provide 'eye-battery-status)
