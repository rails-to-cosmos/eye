;; -*- lexical-binding: t; -*-

(require 'eye)

(eye-def-widget datetime
  :daemon (lambda (context)
            (let ((now (current-time)))
              (a-assoc context
                       :date (format-time-string "%d %b, %a" now)
                       :time (format-time-string "%H:%M" now))))
  :lighter (lambda (context)
             (svg-image (multiline-svg
                         (a-get context :date)
                         (a-list :text (a-get context :time)
                                 :font-weight "bold")))))

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
