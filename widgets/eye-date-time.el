;; -*- lexical-binding: t; -*-

(require 'eye-panel)

(eye-def-widget datetime
  (promise-chain (promise:make-thread (function current-time))
    (thena (a-list :date (format-time-string "%d %b, %a" result)
                   :time (format-time-string "%H:%M" result))))
  :lighter (eyecon (a-get result :date)
                   (a-list :text (a-get result :time)
                           :font-weight "bold"))
  :repeat 10)

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
