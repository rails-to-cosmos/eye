;; -*- lexical-binding: t; -*-

(require 'promise)
(require 'eye-panel)

(eye-def-widget bluetooth
  (promise-chain (promise:make-process '("bluetoothctl" "--" "show"))
    (thena (s-split "\n" (s-join "\n" result)))
    (thena (apply #'a-merge (--map
                             (cl-destructuring-bind (key &optional val &rest _) (s-split ": " it)
                               (let ((key (s-trim key))
                                     (yes? (string= val "yes")))
                                 (cond ((string= key "Powered") (a-list :powered yes?))
                                       ((string= key "Discoverable") (a-list :discoverable yes?))
                                       ((string= key "Pairable") (a-list :pairable yes?))
                                       ((string= key "Discovering") (a-list :discovering yes?))
                                       ((string= key "Name") (a-list :name val))
                                       ((string= key "Alias") (a-list :alias val)))))
                             result))))

  :lighter (eyecon "Bluetooth" (a-list :text (cond ((a-get* result :powered) "on")
                                                   (t "off"))
                                       :font-weight "bold"))

  :persist (cond ((a-get* result :powered) (start-process "bluetoothctl" "*exwm*" "bluetoothctl" "--" "power" "on"))
                 (t (start-process "bluetoothctl" "*exwm*" "bluetoothctl" "--" "power" "off"))))

(provide 'eye-bluetooth)
