;; -*- lexical-binding: t; -*-

(require 'eye)

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

(cl-defun eye-network-manager-parser (line &optional (separator ":") (escape-symbol "\\"))
  (let ((parted (let ((group 0))
                  (--partition-by (if (s-ends-with? escape-symbol it)
                                      (1+ group)
                                    (incf group))
                                  (s-split separator line)))))
    (cl-loop for chunk in parted
       if (= 1 (length chunk))
       collect (car chunk)
       else
       collect (s-replace escape-symbol "" (s-join separator chunk)))))

(defun eye-network-manager-daemon ()
  (interactive)
  (let* ((buffer (get-buffer-create "*eye-network-manager*"))
         (process (start-process "nmcli" buffer "nmcli" "-t" "device" "wifi"))
         (data nil))

    (set-process-filter process
                        (lambda (process output)
                          (cl-loop
                             for line in (s-split "\n" output)
                             do (-let [(in-use bssid ssid mode channel rate signal bars security) (eye-network-manager-parser line)]
                                  (when ssid
                                    (cl-pushnew (list (string= in-use "*") ssid bars channel rate security) data))))))

    (set-process-sentinel process
                          (lambda (process status)
                            (when (string= (s-trim status) "finished")
                              (setq eye-network-manager-data data))))))

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

(eye-def-widget network-manager
  :daemon (lambda (context)
            (a-assoc context
                     :enabled (s-trim (shell-command-to-string "nmcli radio wifi"))
                     :connectivity (s-trim (shell-command-to-string "nmcli networking connectivity"))))
  :lighter (lambda (context)
             (let ((enabled (string= "enabled" (a-get* context :enabled)))
                   (connectivity (a-get* context :connectivity)))
               (list
                (propertize
                 (cond
                   ((not enabled) "")
                   ((string= connectivity "limited") "")
                   (t ""))
                 ;; 'display '((height 1.2)
                 ;;            (raise -0.1))
                 )))))

(provide 'eye-network-manager)
