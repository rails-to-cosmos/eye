;; -*- lexical-binding: t; -*-

(require 'eye)
(require 'battery)

(eye-def-widget battery
  :daemon (lambda (context)
            (let ((battery-status (funcall battery-status-function)))
              (a-assoc context
                       :power (battery-format "%L" battery-status)
                       :battery (battery-format "%B" battery-status)
                       :load (battery-format "%p%%" battery-status)
                       :remaining (battery-format "%t" battery-status))))
  :lighter (lambda (context)
             (let ((load (string-to-number (a-get* context :load))))
               (list
                (propertize
                 (cond ((> load 90) "")
                       ((> load 60) "")
                       ((> load 40) "")
                       ((> load 5) "")
                       (t ""))
                 'display '((height 2)
                            (raise -0.14)))

                mode-line-front-space

                (format "%d%s" load "%%")))))

;; (defvar eye-battery-status-data '())

;; (defconst eye-battery-status-schema
;;   (list (make-ctbl:cmodel :title "Key")
;;         (make-ctbl:cmodel :title "Value")))

;; (eye-def-widget battery-status
;;   :model (make-ctbl:model
;;           :column-model eye-battery-status-schema
;;           :data (cl-loop for (key . value) in eye-battery-status-data
;;                    collect (list (->> key
;;                                       symbol-name
;;                                       s-titleize
;;                                       (s-replace-regexp "^:" ""))
;;                                  value)))
;;   :timers (list
;;            (a-list :fn #'eye-battery-status-daemon))
;;   :on-click (lambda ()
;;               (message "Hello")))

(provide 'eye-battery-status)
