;; -*- lexical-binding: t; -*-

(require 'eye)

(eye-def-widget brightness
  :daemon (lambda (context)
            (a-list
             :brightness (string-to-number (car (last (s-split " " (s-trim (shell-command-to-string "brightness"))))))))
  :lighter (lambda (context)
             (let* ((brightness (a-get context :brightness))
                    (data (eval (cond
                                  ((> brightness 0) 'audio-volume-low)
                                  (t 'audio-volume-muted)))))
               (create-image data 'svg t :scale 1))))

(provide 'eye-brightness)
