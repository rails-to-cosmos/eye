;; -*- lexical-binding: t; -*-

(require 'promise)
(require 'eye-panel)

(eye-def-widget bluetooth
  :width 10
  :observer '("bluetoothctl" "--" "show")
  :mapper (apply #'a-merge (--map
                            (cl-destructuring-bind (key &optional val &rest _) (s-split ": " it)
                              (let ((key (s-trim key))
                                    (yes? (string= val "yes")))
                                (case (intern (downcase key))
                                  ('powered (a-list 'powered yes?))
                                  ('discoverable (a-list 'discoverable yes?))
                                  ('pairable (a-list 'pairable yes?))
                                  ('discovering (a-list 'discovering yes?))
                                  ('name (a-list 'name val))
                                  ('alias (a-list 'alias val)))))
                            .bluetooth))
  :lighter (list "Bluetooth"
                 (a-list :text (cond (.powered "on")
                                     (t "off"))
                         :font-weight "bold"))
  :persist (cond (.powered (start-process "bluetoothctl" "*exwm*" "bluetoothctl" "--" "power" "on"))
                 (t (start-process "bluetoothctl" "*exwm*" "bluetoothctl" "--" "power" "off"))))

(provide 'eye-bluetooth)
