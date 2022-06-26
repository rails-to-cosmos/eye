;; -*- lexical-binding: t; -*-

(require 'eye-panel)

(eye-def-widget messages
  :observer (lambda () (with-current-buffer (messages-buffer)
                    (save-excursion
                      (cl-loop repeat 5
                         initially (goto-char (point-max))
                         unless (bobp)
                         collect (prog1 (s-trim (thing-at-point 'line))
                                   (forward-line -1))))))
  :lighter (apply #'eyecon (cl-loop for message in .messages
                              collect (a-list :text message :font-size (/ eye-panel-font-size 1.5)))))

(provide 'eye-log)
