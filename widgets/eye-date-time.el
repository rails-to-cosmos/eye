;; -*- lexical-binding: t; -*-

(require 'eye)

(eye-def-widget datetime
  :lighter (lambda (context)
             (format "%s %s"
                     (a-get* context :date)
                     (a-get* context :time)))
  :daemon (lambda (context)
            (let ((now (current-time)))
              (a-assoc context
                       :date (format-time-string "%a, %d %b" now)
                       :time (format-time-string "%H:%M" now)))))

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
