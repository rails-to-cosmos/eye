;; -*- lexical-binding: t; -*-

(require 'promise)
(require 'eye-panel)

(eye-def-widget bluetooth
  (promise-chain (promise:make-process '("bluetoothctl" "--" "show"))
    (thena (s-split "\n" (s-join "\n" result)))
    (thena (cl-loop for line in result
              collect (cl-destructuring-bind (key &optional val &rest _) (s-split ": " line)
                        (cond ((string= (s-trim key) "Powered") (a-list :powered (string= val "yes")))
                              ((string= (s-trim key) "Discoverable") (a-list :discoverable (string= val "yes")))
                              ((string= (s-trim key) "Pairable") (a-list :pairable (string= val "yes")))
                              ((string= (s-trim key) "Discovering") (a-list :discovering (string= val "yes")))
                              ((string= (s-trim key) "Name") (a-list :name val))
                              ((string= (s-trim key) "Alias") (a-list :alias val))))
              into params
              finally (return (apply #'a-merge params))))
    (thena (eyecon "Bluetooth" (a-list :text (cond ((a-get* result :powered) "on")
                                                   (t "off"))
                                       :font-weight "bold")))))

(provide 'eye-bluetooth)
