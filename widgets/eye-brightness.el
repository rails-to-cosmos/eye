;; -*- lexical-binding: t; -*-

(require 'eye-panel)

(eye-let brightness
  (let* ((brightness (car (last (s-split " " (s-trim (shell-command-to-string "brightness")))))))
    (eyecon "Brightness" (a-list :text brightness :font-weight "bold"))))

(provide 'eye-brightness)
