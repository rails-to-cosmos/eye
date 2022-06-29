;; -*- lexical-binding: t; -*-

(require 'eye-panel)

(eye-def-widget datetime
  :observer (function current-time)
  :width 12
  :mapper (a-list 'date (format-time-string "%d %b, %a" .datetime)
                  'time (format-time-string "%H:%M:%S" .datetime))
  :lighter (list .date (a-list :text .time :font-weight "bold"))
  :repeat 1)

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
