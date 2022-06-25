;; -*- lexical-binding: t; -*-

(require 'eye-panel)

(defun boolsort (lhs rhs)
  (cond
    ((not (or lhs rhs)) 0)
    (lhs -1)
    (rhs 1)
    (t 0)))

(defun sigsort (lhs rhs)
  (ctbl:sort-number-lessp
   (s-count-matches "_" lhs)
   (s-count-matches "_" rhs)))

(defun numsort (lhs rhs)
  "Sort collection of strings by first number parsed."
  (ctbl:sort-number-lessp
   (string-to-number lhs)
   (string-to-number rhs)))

;; (defvar eye-network-manager-data (a-list)
;;   "Main data storage.")

(defconst eye-network-manager-schema
  (list (make-ctbl:cmodel :title "*"
                          :align 'right
                          :min-width 1
                          :sorter #'boolsort)
        (make-ctbl:cmodel :title "Name"
                          :align 'left)
        (make-ctbl:cmodel :title "Signal"
                          :align 'left
                          :sorter #'sigsort)
        (make-ctbl:cmodel :title "Channel"
                          :align 'left
                          :sorter #'numsort)
        (make-ctbl:cmodel :title "Rate"
                          :align 'left
                          :sorter #'numsort)
        (make-ctbl:cmodel :title "Security"
                          :align 'left)))

;; (eye-def-widget network-manager
;;   :model (make-ctbl:model
;;           :column-model eye-network-manager-schema
;;           :data eye-network-manager-data
;;           :sort-state '(3))
;;   :timers (list
;;            (a-list :fn #'eye-network-manager-daemon))
;;   :on-click (lambda ()
;;               (let (cp (ctbl:cp-get-component))
;;                 (message "CTable : Click Hook [%S] [%S] [%S]"
;;                          (ctbl:cp-get-selected component)
;;                          (ctbl:cp-get-selected-data-row component)
;;                          (ctbl:cp-get-selected-data-cell component)))))

(cl-defun eye-wifi-parse-networks (status &optional (separator ":") (escape-symbol "\\"))
  (cl-loop for line in (s-split "\n" status)
     collect (cl-destructuring-bind (in-use bssid ssid mode channel rate signal bars security)
                 (let ((parted (let ((group 0))
                                 (--partition-by (if (s-ends-with? escape-symbol it)
                                                     (1+ group)
                                                   (incf group))
                                                 (s-split separator line)))))
                   (cl-loop for chunk in parted
                      if (= 1 (length chunk))
                      collect (car chunk)
                      else
                      collect (s-replace escape-symbol "" (s-join separator chunk))))
               (when ssid
                 (a-list :in-use (string= in-use "*")
                         :ssid ssid
                         :bars bars
                         :channel channel
                         :rate rate
                         :security security)))))

(eye-def-widget wifi

  (promise-chain (promise-all (list (promise:make-process '("nmcli" "radio" "wifi"))
                                    (promise:make-process '("nmcli" "networking" "connectivity"))
                                    (promise:make-process '("nmcli" "-t" "device" "wifi"))))
    (thena (cl-loop for output across result
              collect (s-trim (s-join "\n" output))))
    (thena (cl-destructuring-bind (enabled connectivity networks) result
             (a-list :enabled enabled
                     :connectivity connectivity
                     :networks (eye-wifi-parse-available-networks networks)))))

  :lighter (let-alist result
             (eyecon "Wi-Fi"
                     (a-list :text (cond
                                     ((not .:enabled) "disabled")
                                     ((string= .:connectivity "limited") "limited")
                                     (t (if-let (current-network (--first (a-get it :in-use) .:networks))
                                            (let-alist current-network
                                              (let* ((max-bars 4)
                                                     (act-bars (- max-bars (s-count-matches "_" .:bars))))
                                                (format "%d%%" (/ (* 100 act-bars) max-bars))))

                                          "on")))
                             :font-weight "bold"))))

(provide 'eye-wifi)
