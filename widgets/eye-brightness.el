;; -*- lexical-binding: t; -*-

(require 'eye-panel)

(eye-def-widget brightness
  :observer (promise-chain (promise:make-process '("brightnessctl"))
              (thena (a-list
                      'brightness (car (last (s-split " " (s-trim (s-join "\n" result))))))))
  :lighter (eyecon "Brightness" (a-list :text .brightness :font-weight "bold")))

(provide 'eye-brightness)
