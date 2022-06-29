;; -*- lexical-binding: t; -*-

(require 'eye-panel)

(eye-def-widget messages
  :width 50
  :height 4
  :observer (lambda () (with-current-buffer (messages-buffer)
                    (save-excursion
                      (cl-loop repeat 5
                         initially (goto-char (point-max))
                         unless (bobp)
                         collect (prog1 (s-trim (thing-at-point 'line))
                                   (forward-line -1))))))
  :lighter (cl-loop for message in .messages
              collect (a-list :text (substring message 0 (min (length message) 100))
                              ;; (format "%s %s"
                              ;;         (format-time-string "%H:%M:%S" (current-time))
                              ;;         )
                              :font-size (/ eye-panel-font-size 1.1))))

(provide 'eye-log)
