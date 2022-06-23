;; -*- lexical-binding: t; -*-

(eye-def-widget space
  (promise-chain (promise:make-process '("pamixer" "--get-volume"))
    (thena (s-trim (s-join "\n" result))))
  :lighter (eyecon "Volume"
                   (a-list :text result
                           :font-weight "bold")))

(provide 'eye-space)
