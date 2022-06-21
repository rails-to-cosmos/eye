;; -*- lexical-binding: t; -*-

(require 'eye-panel)

(eye-let bluetooth
  (let* ((data (cl-loop for line in (s-split "\n" (shell-command-to-string "bluetoothctl -- show"))
                  collect (cl-destructuring-bind (key &optional val &rest _) (s-split ": " line)
                            (cond ((string= (s-trim key) "Powered") (a-list :powered (string= val "yes")))
                                  ((string= (s-trim key) "Discoverable") (a-list :discoverable (string= val "yes")))
                                  ((string= (s-trim key) "Pairable") (a-list :pairable (string= val "yes")))
                                  ((string= (s-trim key) "Discovering") (a-list :discovering (string= val "yes")))
                                  ((string= (s-trim key) "Name") (a-list :name val))
                                  ((string= (s-trim key) "Alias") (a-list :alias val))))
                  into params
                  finally (return (apply #'a-merge params)))))
    (svg-image (multiline-svg "Bluetooth" (a-list :text (cond ((a-get data :powered) "on")
                                                              (t "off"))
                                                  :font-weight "bold")))))

(provide 'eye-bluetooth)
