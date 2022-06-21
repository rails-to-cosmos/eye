;; -*- lexical-binding: t; -*-

(require 'eye-panel)
(require 'battery)

;; (with-current-buffer (get-buffer-create "*Paint*")
;;   (delete-region (point-min) (point-max))
;;   (let ((svg (svg-create 16 16 :viewBox "0 0 16 16" :fill "white")))
;;     (svg-path svg '((moveto ((9.585 . 2.568)))
;;                     (elliptical-arc ((0.5 0.5 0 0 1 0.226 0.58)))
;;                     (horizontal-lineto ((8.677 . 6.832))))
;;               :relative nil
;;               :fill-color "lightblue")
;;     (svg-print svg)
;;     (insert "\n")
;;     (svg-insert-image svg)))


(defconst battery-charging
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" fill=\"currentColor\" class=\"bi bi-battery-charging\" viewBox=\"0 0 16 16\">
  <path fill=\"#ffffff\" d=\"M9.585 2.568a.5.5 0 0 1 .226.58L8.677 6.832h1.99a.5.5 0 0 1 .364.843l-5.334 5.667a.5.5 0 0 1-.842-.49L5.99 9.167H4a.5.5 0 0 1-.364-.843l5.333-5.667a.5.5 0 0 1 .616-.09z\"/>
  <path fill=\"#ffffff\" d=\"M2 4h4.332l-.94 1H2a1 1 0 0 0-1 1v4a1 1 0 0 0 1 1h2.38l-.308 1H2a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2z\"/>
  <path fill=\"#ffffff\" d=\"M2 6h2.45L2.908 7.639A1.5 1.5 0 0 0 3.313 10H2V6zm8.595-2-.308 1H12a1 1 0 0 1 1 1v4a1 1 0 0 1-1 1H9.276l-.942 1H12a2 2 0 0 0 2-2V6a2 2 0 0 0-2-2h-1.405z\"/>
  <path fill=\"#ffffff\" d=\"M12 10h-1.783l1.542-1.639c.097-.103.178-.218.241-.34V10zm0-3.354V6h-.646a1.5 1.5 0 0 1 .646.646zM16 8a1.5 1.5 0 0 1-1.5 1.5v-3A1.5 1.5 0 0 1 16 8z\"/>
</svg>"
  )

(defconst battery-000-charging "<svg width=\"24\" height=\"24\" enable-background=\"new\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\">
 <title>Paper Symbolic Icon Theme</title>
 <g transform=\"translate(2,4)\" fill=\"#ffffff\">
  <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h13c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h13c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-13c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" fill-rule=\"evenodd\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m18 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" fill-rule=\"evenodd\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m8.834 4-3.334 5h3l-0.75 3h0.41602l3.334-5h-3l0.75-3z\" enable-background=\"new\"/>
 </g>
</svg>")

(defconst battery-000 "<svg width=\"24\" height=\"24\" enable-background=\"new\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <title>Paper Symbolic Icon Theme</title>
 <g transform=\"translate(2,-4)\" fill=\"#d33636\" fill-rule=\"evenodd\">
  <path d=\"m2 11c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h13c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h13c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-13c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m18 14v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
 </g>
</svg>")

(defconst battery-020-charging "<svg width=\"24\" height=\"24\" enable-background=\"new\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <title>Paper Symbolic Icon Theme</title>
 <g transform=\"translate(1,-4)\" fill=\"#ffffff\">
  <g transform=\"translate(1,8)\">
   <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h13c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h13c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-13c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" fill-rule=\"evenodd\" style=\"paint-order:stroke fill markers\"/>
   <path d=\"m18 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" fill-rule=\"evenodd\" style=\"paint-order:stroke fill markers\"/>
   <path d=\"m8.834 4-3.334 5h3l-0.75 3h0.41602l3.334-5h-3l0.75-3z\" enable-background=\"new\"/>
  </g>
  <path fill=\"#B6E63E\" d=\"m3.4824 13c-0.26703 0-0.48242 0.24253-0.48242 0.54492v4.9102c0 0.3024 0.21539 0.54492 0.48242 0.54492h2.0352c0.26703 0 0.48242-0.24253 0.48242-0.54492v-0.5918c-0.46977-0.27296-0.67079-0.90961-0.33203-1.418l0.33203-0.49805v-2.4023c0-0.3024-0.21539-0.54492-0.48242-0.54492h-2.0352z\" enable-background=\"new\" fill-rule=\"evenodd\" style=\"paint-order:stroke fill markers\"/>
 </g>
</svg>")

(defconst battery-020 "<svg width=\"24\" height=\"24\" enable-background=\"new\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <title>Paper Symbolic Icon Theme</title>
 <g transform=\"translate(2,4)\" fill=\"#d33636\" fill-rule=\"evenodd\">
  <rect x=\"2\" y=\"5\" width=\"3\" height=\"6\" rx=\".5048\" ry=\".54584\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h13c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h13c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-13c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m18 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
 </g>
</svg>")

(defconst battery-040-charging "<svg width=\"24\" height=\"24\" enable-background=\"new\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <title>Paper Symbolic Icon Theme</title>
 <g transform=\"translate(2,4)\" fill=\"#ffffff\">
  <g fill-rule=\"evenodd\">
   <path fill=\"#B6E63E\" d=\"m2.5312 5c-0.29412 0-0.53125 0.24253-0.53125 0.54492v4.9102c0 0.30239 0.23713 0.54492 0.53125 0.54492h3.9375c0.29412 0 0.53125-0.24253 0.53125-0.54492v-0.45508h-1.5c-0.79859-1.712e-4 -1.2749-0.89013-0.83203-1.5547l2.1836-3.2754c-0.096507-0.10352-0.23169-0.16992-0.38281-0.16992h-3.9375z\" style=\"paint-order:stroke fill markers\"/>
   <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h13c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h13c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-13c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
   <path d=\"m18 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  </g>
  <path d=\"m8.834 4-3.334 5h3l-0.75 3h0.41602l3.334-5h-3l0.75-3z\" enable-background=\"new\"/>
 </g>
</svg>")

(defconst battery-060-charging "<svg width=\"24\" height=\"24\" enable-background=\"new\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <title>Paper Symbolic Icon Theme</title>
 <g transform=\"translate(2,4)\" fill=\"#ffffff\">
  <path d=\"m8.834 4-3.334 5h3l-0.75 3h0.41602l3.334-5h-3l0.75-3z\" enable-background=\"new\"/>
  <g fill-rule=\"evenodd\">
   <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h13c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h13c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-13c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
   <path fill=\"#B6E63E\" d=\"m2.5508 5c-0.30514 0-0.55078 0.24564-0.55078 0.55078v4.8984c0 0.30514 0.24564 0.55078 0.55078 0.55078h4.418l0.25-1h-1.7188c-0.79859-1.712e-4 -1.2749-0.89013-0.83203-1.5547l2.2969-3.4453h-4.4141zm7.4805 0-0.25 1h1.7188c0.1877 4.03e-5 0.35566 0.052848 0.5 0.13672v-0.58594c0-0.30514-0.24564-0.55078-0.55078-0.55078h-1.418zm1.9688 3.0527-1.9648 2.9473h1.4141c0.30514 0 0.55078-0.24564 0.55078-0.55078v-2.3965z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
   <path d=\"m18 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  </g>
 </g>
</svg>")

(defconst battery-060 "<svg width=\"24\" height=\"24\" enable-background=\"new\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <title>Paper Symbolic Icon Theme</title>
 <g transform=\"translate(2,4)\" fill=\"#ffffff\" fill-rule=\"evenodd\">
  <rect x=\"2\" y=\"5\" width=\"10\" height=\"6\" rx=\".5236\" ry=\".54584\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h13c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h13c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-13c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m18 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
 </g>
</svg>")

(defconst battery-080-charging "<svg width=\"24\" height=\"24\" enable-background=\"new\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <title>Paper Symbolic Icon Theme</title>
 <g transform=\"translate(2,4)\" fill=\"#ffffff\">
  <path  d=\"m8.834 4-3.334 5h3l-0.75 3h0.41602l3.334-5h-3l0.75-3z\" enable-background=\"new\"/>
  <g fill-rule=\"evenodd\">
   <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h13c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h13c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-13c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
   <path fill=\"#B6E63E\" d=\"m2.5508 5c-0.30514 0-0.55078 0.24564-0.55078 0.55078v4.8984c0 0.30514 0.24564 0.55078 0.55078 0.55078h4.418l0.25-1h-1.7188c-0.79859-1.712e-4 -1.2749-0.89013-0.83203-1.5547l2.2969-3.4453h-4.4141zm7.4805 0-0.25 1h1.7188c0.1877 4.03e-5 0.35566 0.052848 0.5 0.13672v-0.58594c0-0.30514-0.24564-0.55078-0.55078-0.55078h-1.418zm1.9688 3.0527-1.9648 2.9473h1.4141c0.30514 0 0.55078-0.24564 0.55078-0.55078v-2.3965z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
   <path d=\"m18 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  </g>
 </g>
</svg>")

(defconst battery-080 "<svg width=\"24\" height=\"24\" enable-background=\"new\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <title>Paper Symbolic Icon Theme</title>
 <g transform=\"translate(2,4)\" fill=\"#ffffff\" fill-rule=\"evenodd\">
  <rect x=\"2\" y=\"5\" width=\"10\" height=\"6\" rx=\".5236\" ry=\".54584\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h13c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h13c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-13c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m18 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
 </g>
</svg>")

(defconst battery-100-charging "<svg width=\"24\" height=\"24\" enable-background=\"new\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <title>Paper Symbolic Icon Theme</title>
 <g transform=\"translate(2,4)\" fill=\"#ffffff\">
  <g fill-rule=\"evenodd\">
   <path d=\"m2.5215 5c-0.28865 0-0.52148 0.24253-0.52148 0.54492v4.9102c0 0.30239 0.23284 0.54492 0.52148 0.54492h4.4473l0.25-1h-1.7188c-0.79859-1.712e-4 -1.2749-0.89013-0.83203-1.5547l2.2969-3.4453h-4.4434zm7.5098 0-0.25 1h1.7188c0.79859 1.713e-4 1.2749 0.89013 0.83203 1.5547l-2.2969 3.4453h4.4434c0.28865 0 0.52148-0.24253 0.52148-0.54492v-4.9102c0-0.30239-0.23284-0.54492-0.52148-0.54492h-4.4473z\" fill=\"#B6E63E\" style=\"paint-order:stroke fill markers\"/>
   <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h13c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h13c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-13c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
   <path d=\"m18 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  </g>
  <path d=\"m8.834 4-3.334 5h3l-0.75 3h0.41602l3.334-5h-3l0.75-3z\" enable-background=\"new\"/>
 </g>
</svg>")

(defconst battery-100 "<svg width=\"24\" height=\"24\" enable-background=\"new\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
 <title>Paper Symbolic Icon Theme</title>
 <g transform=\"translate(2,4)\" fill=\"#ffffff\" fill-rule=\"evenodd\">
  <rect x=\"2\" y=\"5\" width=\"13\" height=\"6\" rx=\".52103\" ry=\".54584\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m2 3c-1.108 0-2 0.892-2 2v6c0 1.108 0.892 2 2 2h13c1.108 0 2-0.892 2-2v-6c0-1.108-0.892-2-2-2zm0 1h13c0.554 0 1 0.446 1 1v6c0 0.554-0.446 1-1 1h-13c-0.554 0-1-0.446-1-1v-6c0-0.554 0.446-1 1-1z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
  <path d=\"m18 6v4a2 2 0 0 0 2-2 2 2 0 0 0-2-2z\" enable-background=\"new\" style=\"paint-order:stroke fill markers\"/>
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

(eye-let battery
  (let* ((battery-status (funcall battery-status-function))
         (power (battery-format "%L" battery-status))
         (load (string-to-number (battery-format "%p%%" battery-status)))
         (remaining (battery-format "%t" battery-status))
         (adapter-p (string= "ac" (downcase power)))
         (icon (eval (cond
                       (adapter-p (cond
                                    ((> load 90) 'battery-100-charging)
                                    ((> load 70) 'battery-080-charging)
                                    ((> load 50) 'battery-060-charging)
                                    ((> load 30) 'battery-040-charging)
                                    ((> load 10) 'battery-020-charging)
                                    ((> load 0) 'battery-000-charging)))
                       (t (cond
                            ((> load 90) 'battery-100)
                            ((> load 70) 'battery-080)
                            ((> load 30) 'battery-060)
                            ((> load 10) 'battery-020)
                            ((> load 0) 'battery-000)))))))
    (eyecon
     (a-list :text "Battery")
     (a-list :text (format "%s%%" load)
             :font-weight "bold"))
    ;; (create-image icon 'svg t :scale 1)
    ))

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
