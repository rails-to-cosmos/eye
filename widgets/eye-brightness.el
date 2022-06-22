;; -*- lexical-binding: t; -*-

(require 'eye-panel)

(eye-def-widget brightness
  (promise-chain (promise:make-process '("brightness"))
    (thena (car (last (s-split " " (s-trim (s-join "\n" result)))))))
  :lighter (eyecon "Brightness" (a-list :text result :font-weight "bold")))

(provide 'eye-brightness)
