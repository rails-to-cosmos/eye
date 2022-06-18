;; -*- lexical-binding: t; -*-

(require 'eye)
(require 'battery)

(defconst battery-low-symbolic
  "<svg width=\"16\" height=\"16\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <g fill=\"#dedede\" fill-rule=\"evenodd\">
  <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h9c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h9c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-9c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  <rect x=\"2\" y=\"5\" width=\"3\" height=\"6\" rx=\".48747\" ry=\".54584\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m14 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
 </g>
</svg>")

(defconst battery-caution-charging-symbolic "<svg width=\"16\" height=\"16\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <g fill=\"#dedede\">
  <path d=\"m6.834 4-3.334 5h3l-0.75 3h0.41602l3.334-5h-3l0.75-3z\" enable-background=\"new\"/>
  <g fill-rule=\"evenodd\">
   <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h9c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h9c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-9c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
   <path d=\"m2.4434 5c-0.24577 0-0.44336 0.24253-0.44336 0.54492v4.9102c0 0.3024 0.19759 0.54492 0.44336 0.54492h1.1133c0.24577 0 0.44336-0.24253 0.44336-0.54492v-0.45508h-0.5c-0.79859-1.711e-4 -1.2749-0.89013-0.83203-1.5547l1.332-1.998v-0.90234c0-0.3024-0.19759-0.54492-0.44336-0.54492h-1.1133z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
   <path d=\"m14 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  </g>
 </g>
</svg>")

(defconst battery-caution-symbolic "<svg width=\"16\" height=\"16\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <g fill=\"#d33636\" fill-rule=\"evenodd\">
  <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h9c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h9c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-9c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  <rect x=\"2\" y=\"5\" width=\"2\" height=\"6\" rx=\".52837\" ry=\".54584\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m14 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
 </g>
</svg>")


;; (with-current-buffer (get-buffer-create "*svg*")
;;   (setq test-svg (svg-create 16 16 :fill-color "#dedede" :fill-rule "evenodd"))

;;   (delete-region (point-min) (point-max))
;;   (insert-image (svg-image test-svg :scale 5))

;;   (svg-path test-svg '((moveto ((0 . 0)))
;;                        (lineto ((10 . 10)))
;;                        (curveto ((2 1 1 2 2 2)
;;                                  (3 2 0 1 1 1))))
;;             :enable-background "new"
;;             :stroke-color "green"
;;             :style "paint-order:stroke fill markers")

;;   (svg-rectangle test-svg 2 5 3 6
;;                  :rx 0.48747
;;                  :ry 0.54584
;;                  :enable-background "new"
;;                  :style "paint-order:stroke fill markers")

;;   ;; (with-temp-file "/tmp/1.svg"
;;   ;;   (svg-print test-svg))
;;   )


(eye-def-widget battery
  :daemon (lambda (context)
            (let ((battery-status (funcall battery-status-function)))
              (a-list
               :power (battery-format "%L" battery-status)
               :load (battery-format "%p%%" battery-status)
               :remaining (battery-format "%t" battery-status))))
  :lighter (lambda (context)
             ;; (let* ((load (string-to-number (a-get context :load)))
             ;;        (adapter-p (string= "ac" (downcase (a-get context :power)))))
             ;;   (svg-image (multiline-svg
             ;;               (if adapter-p "Adapter" "Battery")
             ;;               (a-list :text (format "%d%%" load)
             ;;                       :font-weight "bold"))))

             ;; (create-image battery-low-symbolic 'svg t)

             (let* ((load (string-to-number (a-get context :load)))
                    (adapter-p (string= "ac" (downcase (a-get context :power))))
                    (filename (cond
                                (adapter-p (cond
                                             ((> load 95) "battery-full-charging-symbolic")
                                             ((> load 80) "battery-level-90-charging-symbolic")
                                             ((> load 60) "battery-level-70-charging-symbolic")
                                             ((> load 40) "battery-medium-charging-symbolic")
                                             ((> load 30) "battery-level-30-charging-symbolic")
                                             ((> load 5) "battery-level-10-charging-symbolic")
                                             ((> load 0) "battery-empty-charging-symbolic")))
                                (t (cond
                                     ((> load 95) "battery-full-symbolic")
                                     ((> load 80) "battery-level-90-symbolic")
                                     ((> load 60) "battery-level-70-symbolic")
                                     ((> load 40) "battery-level-50-symbolic")
                                     ((> load 30) "battery-level-30-symbolic")
                                     ((> load 15) "battery-level-20-symbolic")
                                     ((> load 5) "battery-level-10-symbolic")
                                     ((> load 0) "battery-empty-symbolic"))))))
               (create-image (format "/home/akatovda/.emacs.d/stuff/eye/widgets/battery/%s.svg" filename)))))

;; ((> load 90) (propertize "" 'display display 'face '(:foreground "#27ae60" :family "FontAwesome")))
;; ((> load 60) (propertize "" 'display display 'face '(:foreground "#f1c40f" :family "FontAwesome")))
;; ((> load 40) (propertize "" 'display display 'face '(:foreground "#f39c12" :family "FontAwesome")))
;; ((> load 5) (propertize "" 'display display 'face '(:foreground "#e67e22" :family "FontAwesome")))


;; (defvar eye-battery-status-data '())

;; (defconst eye-battery-status-schema
;;   (list (make-ctbl:cmodel :title "Key")
;;         (make-ctbl:cmodel :title "Value")))

;; (eye-def-widget battery-status
;;   :model (make-ctbl:model
;;           :column-model eye-battery-status-schema
;;           :data (cl-loop for (key . value) in eye-battery-status-data
;;                    collect (list (->> key
;;                                       symbol-name
;;                                       s-titleize
;;                                       (s-replace-regexp "^:" ""))
;;                                  value)))
;;   :timers (list
;;            (a-list :fn #'eye-battery-status-daemon))
;;   :on-click (lambda ()
;;               (message "Hello")))

(provide 'eye-battery-status)
