;; -*- lexical-binding: t; -*-

(require 'a)
(require 'dash)
(require 'pcase)
(require 's)
(require 'simple)
(require 'ctable)

(defgroup eye nil
  "Bunch of timers."
  :tag "Eye")

(defgroup eye-faces nil
  "Faces for eye."
  :tag "Eye Faces"
  :group 'faces)

(defvar eye-widgets (a-list)
  "List of widgets to show in reports.")

(defface eye-lighter
    '((((class color) (min-colors 88) (background light)) (:foreground "darkgreen" :height 1.2 :family "FontAwesome"))
      (((class color) (min-colors 88) (background dark)) (:foreground "#2ecc71" :height 1.2 :family "FontAwesome"))
      (((class color) (min-colors 16) (background light)) (:foreground "darkgreen" :height 1.2 :family "FontAwesome"))
      (((class color) (min-colors 16) (background dark)) (:foreground "#2ecc71" :height 1.2 :family "FontAwesome"))
      (((class color) (min-colors 8)) (:foreground "green" :bold t))
      (t (:bold t)))
  "Face used for eye in your modeline."
  :group 'eye-faces)

(defconst eye-lighter
  #(" " 0 1 (rear-nonsticky t display nil font-lock-face eye-lighter face eye-lighter)))

(define-minor-mode eye-mode
    "Monitor exwm system in a background."
  nil eye-lighter nil)

(defvar eye-daemon-refresh-timer
  (let ((time (current-time))
        (repeat 1)
        (timer (timer-create))
        (fn #'eye-activate-widgets))
    (timer-set-time timer time repeat)
    (timer-set-function timer fn)
    timer))

(defvar eye-buffer "*eye*")

(defface eye-view-lighter
    '((((class color) (min-colors 88) (background light)) (:foreground "#8e44ad" :height 1.2 :family "file-icons"))
      (((class color) (min-colors 88) (background dark)) (:foreground "#9b59b6" :height 1.2 :family "file-icons"))
      (((class color) (min-colors 16) (background light)) (:foreground "#8e44ad" :height 1.2 :family "file-icons"))
      (((class color) (min-colors 16) (background dark)) (:foreground "#9b59b6" :height 1.2 :family "file-icons"))
      (((class color) (min-colors 8)) (:foreground "magenta" :bold t))
      (t (:bold t)))
  "Face used for eye in your modeline."
  :group 'eye-faces)

(defconst eye-view-lighter
  #(" " 0 1 (rear-nonsticky t display (raise -0.11) font-lock-face eye-view-lighter face eye-view-lighter)))

(defvar eye-view-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)

    (define-key map "n" 'forward-line)
    (define-key map "p" 'previous-line)
    (define-key map "g" 'eye-refresh)

    map))

(define-derived-mode eye-view-mode special-mode
  eye-view-lighter
  "Special mode extended to work with ctbl.")

(defun ctbl::sort-current ()
  (interactive)
  (let ((cp (ctbl:cp-get-component))
        (cell-id (ctbl:cursor-to-nearest-cell)))
    (when (and cp cell-id)
      (ctbl:cmodel-sort-action cp (cdr (ctbl:cp-get-selected cp)))

      (ctbl:cp-set-selected-cell cp cell-id))))

;; (defun ctbl::filter-current ()
;;   (interactive)
;;   (let ((cp (ctbl:cp-get-component))
;;         (cell-id (ctbl:cursor-to-nearest-cell)))
;;     (when (and cp cell-id)
;;       (ctbl:cmodel-sort-action cp (cdr (ctbl:cp-get-selected cp)))
;;       (ctbl:cp-set-selected-cell cp cell-id))))

(defun ctbl::navi-move-backward ()
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (cell-id (ctbl:cursor-to-nearest-cell))
         (row-id (car cell-id)))
    (cl-flet ((cursor-moved-p () (not (eql cell-id (ctbl:cursor-to-nearest-cell)))))
      (ctbl:navi-move-left)
      (unless (cursor-moved-p)
        (ctbl:navi-move-up)
        (if (cursor-moved-p)
            (ctbl:navi-move-right-most)
          (forward-line -4))))))

(defun ctbl::navi-move-forward ()
  (interactive)
  (when-let (cp (condition-case nil
                    (ctbl:cp-get-component)
                  (error nil)))
    (let* ((cell-id (ctbl:cursor-to-nearest-cell))
           (row-id (car cell-id)))
      (cl-flet ((cursor-moved-p () (not (eql cell-id (ctbl:cursor-to-nearest-cell)))))
        (ctbl:navi-move-right)
        (unless (cursor-moved-p)
          (ctbl:navi-move-down)
          (if (cursor-moved-p)
              (ctbl:navi-move-left-most)
            (forward-line 2)))))))

(defun ctbl::navi-move-down ()
  (interactive)
  (when-let (cp (condition-case nil
                    (ctbl:cp-get-component)
                  (error nil)))
    (let* ((cell-id (ctbl:cursor-to-nearest-cell))
           (row-id (car cell-id)))
      (cl-flet ((cursor-moved-p () (not (eql cell-id (ctbl:cursor-to-nearest-cell)))))
        (ctbl:navi-move-down)
        (unless (cursor-moved-p)
          (forward-line 2))))))

(defun ctbl::navi-move-up ()
  (interactive)
  (when-let (cp (condition-case nil
                    (ctbl:cp-get-component)
                  (error nil)))
    (let* ((cell-id (ctbl:cursor-to-nearest-cell))
           (row-id (car cell-id)))
      (cl-flet ((cursor-moved-p () (not (eql cell-id (ctbl:cursor-to-nearest-cell)))))
        (ctbl:navi-move-up)
        (unless (cursor-moved-p)
          (forward-line -4))))))

(defvar eye-view-map
  (ctbl:define-keymap
   '(("k" . ctbl::navi-move-up)
     ("j" . ctbl::navi-move-down)
     ("h" . ctbl:navi-move-left)
     ("l" . ctbl:navi-move-right)

     ("p" . ctbl::navi-move-up)
     ("n" . ctbl::navi-move-down)
     ("b" . ctbl:navi-move-left)
     ("f" . ctbl:navi-move-right)

     ("c" . ctbl:navi-jump-to-column)

     ("e" . ctbl:navi-move-right-most)
     ("a" . ctbl:navi-move-left-most)

     ("g" . eye-refresh)

     ("?" . ctbl:describe-bindings)

     ([mouse-1] . ctbl:navi-on-click)
     ("C-m" . ctbl:navi-on-click)
     ("RET" . ctbl:navi-on-click)
     ("TAB" . ctbl::navi-move-forward)
     ("<backtab>" . ctbl::navi-move-backward)

     ("^" . ctbl::sort-current))))

(define-globalized-minor-mode global-eye-mode
    eye-mode eye-mode nil
    (cond (global-eye-mode (timer-activate eye-daemon-refresh-timer)
                           (with-current-buffer (get-buffer-create eye-buffer)
                             (eye-view-mode)))
          (t (cancel-timer eye-daemon-refresh-timer)
             (eye-deactivate-widgets))))

(cl-defmacro eye-def-widget (name &key model timers on-click on-update)
  (declare (indent 1))
  `(let ((widget-id (intern (format ":%s" (quote ,name))))
         (timers (cl-loop for timer-config in ,timers
                    collect (let ((time (current-time))
                                  (repeat 1) ;; TODO grab from config
                                  (timer (timer-create))
                                  (fn (a-get timer-config :fn)))
                              (timer-set-time timer time repeat)
                              (timer-set-function timer fn)
                              timer)))
         (component (with-current-buffer (get-buffer-create eye-buffer)
                      (eye-view-mode)
                      (let ((inhibit-read-only t))
                        (goto-char (point-max))
                        (unless (= (point) (point-min))
                          (insert "\n"))
                        (ctbl:create-table-component-region
                         :model (eval ,model)
                         :keymap eye-view-map)))))

     (when (a-get eye-widgets widget-id)
       ;; TODO remove widget from eye buffer
       (mapc #'cancel-timer (a-get* eye-widgets widget-id :timers)))

     (when ,on-click
       (ctbl:cp-add-click-hook component ,on-click))

     (when ,on-update
       (ctbl:cp-add-update-hook component ,on-update))

     (setq eye-widgets
           (a-assoc eye-widgets
                    widget-id
                    (a-list :model (quote ,model)
                            :timers timers
                            :component component)))))

(cl-defun eye-activate-widgets ()
  (cl-loop for (widget-id . widget) in eye-widgets
     do (cl-loop for timer in (a-get* widget :timers)
           do (timer-activate timer))))

(cl-defun eye-deactivate-widgets ()
  (cl-loop for (widget-id . widget) in eye-widgets
     do (cl-loop for timer in (a-get* widget :timers)
           do (cancel-timer timer))))

(cl-defun eye-refresh ()
  (interactive)
  (when-let (cp (condition-case nil
                    (ctbl:cp-get-component)
                  (error nil)))
    (cl-loop for (widget . params) in eye-widgets
       when (eql (a-get params :component) cp)
       do (ctbl:cp-set-model cp (eval (a-get params :model))))))

(provide 'eye)
