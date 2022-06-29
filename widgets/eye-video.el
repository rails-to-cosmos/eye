;; -*- lexical-binding: t; -*-

(require 'promise)
(require 'eye-panel)

(eye-def-widget video
  :width 6
  :observer '("nvidia-smi" "--query-gpu=temperature.gpu" "--format=csv,noheader")
  :lighter (list "Video"
                 (a-list :text (format "%s°C" (car .video))
                         :font-weight "bold")))

(provide 'eye-video)
