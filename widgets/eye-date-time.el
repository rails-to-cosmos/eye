;; -*- lexical-binding: t; -*-

(require 'eye-panel)

(eye-let dt
  (let* ((now (current-time))
         (date (format-time-string "%d %b, %a" now))
         (time (format-time-string "%H:%M" now)))
    (svg-image (multiline-svg date (a-list :text time :font-weight "bold")))))

;; (defconst eye-dt-schema
;;   (list (make-ctbl:cmodel :title "Key")
;;         (make-ctbl:cmodel :title "Value")))

;; (eye-def-widget date-time
;;   :model (make-ctbl:model
;;           :column-model eye-date-time-schema
;;           :data (cl-loop for (key . value) in eye-date-time-data
;;                    collect (list (->> key
;;                                       symbol-name
;;                                       s-titleize
;;                                       (s-replace-regexp "^:" ""))
;;                                  value)))
;;   :timers (list
;;            (a-list :fn #'eye-date-time-daemon))
;;   :on-click (lambda ()
;;               (message "Hello")))

(provide 'eye-date-time)
