;; -*- lexical-binding: t; -*-

(require 'eye)
(require 'battery)

(eye-def-widget battery
  :daemon (lambda (context)
            (let ((battery-status (funcall battery-status-function)))
              (a-list
               :power (battery-format "%L" battery-status)
               :load (battery-format "%p%%" battery-status)
               :remaining (battery-format "%t" battery-status))))
  :lighter (lambda (context)
             (svg-image (multiline-svg
                         (let* ((load (string-to-number (a-get context :load)))
                                (adapter-p (string= "ac" (downcase (a-get context :power)))))
                           (cond (adapter-p (cond ((> load 90) "battery-full-charging-symbolic")
                                                  ((> load 60) "battery-good-charging-symbolic")
                                                  ((> load 30) "battery-medium-charging-symbolic")
                                                  ((> load 5) "battery-caution-charging-symbolic")
                                                  ((> load 0) "battery-empty-charging-symbolic")))
                                 (t (cond ((> load 90) "battery-full-symbolic")
                                          ((> load 60) "battery-good-symbolic")
                                          ((> load 30) "battery-medium-symbolic")
                                          ((> load 5) "battery-caution-symbolic")
                                          ((> Load 0) "battery-empty-symbolic")))))))
             ;; (let* ((load (string-to-number (a-get context :load)))
             ;;        (adapter-p (string= "ac" (downcase (a-get context :power))))
             ;;        (filename (cond (adapter-p (cond ((> load 90) "battery-full-charging-symbolic")
             ;;                                         ((> load 60) "battery-good-charging-symbolic")
             ;;                                         ((> load 30) "battery-medium-charging-symbolic")
             ;;                                         ((> load 5) "battery-caution-charging-symbolic")
             ;;                                         ((> load 0) "battery-empty-charging-symbolic")))
             ;;                        (t (cond ((> load 90) "battery-full-symbolic")
             ;;                                 ((> load 60) "battery-good-symbolic")
             ;;                                 ((> load 30) "battery-medium-symbolic")
             ;;                                 ((> load 5) "battery-caution-symbolic")
             ;;                                 ((> Load 0) "battery-empty-symbolic"))))))
             ;;   (create-image (format "/home/akatovda/.emacs.d/stuff/eye/widgets/battery/%s.svg" filename)))
             ))

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
