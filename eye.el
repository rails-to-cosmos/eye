;; -*- lexical-binding: t; -*-

(require 'a)
(require 'dash)
(require 'pcase)
(require 's)
(require 'simple)
(require 'ctable)
(require 'svg)

(require 'eye-faces)

(defgroup eye nil
  "Bunch of timers."
  :tag "Eye")

(defvar eye-widgets (a-list)
  "List of widgets to show in reports.")

(defvar eye-lighter
  (propertize ""
              'face 'eye-lighter
              'display '((height 2)
                         (raise 0))))

(defconst eye-view-lighter
  #(" " 0 1 (rear-nonsticky t display nil font-lock-face eye-view-lighter face eye-view-lighter)))

(define-minor-mode eye-mode
    "Monitor exwm system in a background."
  nil
  (:eval
   (list mode-line-front-space eye-lighter))
  nil)

(defvar eye-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)

    (define-key map "n" 'forward-line)
    (define-key map "p" 'previous-line)
    (define-key map "g" 'eye-refresh)

    map))

(define-minor-mode eye-view-mode
    "Special mode extended to work with multiple ctables."
  nil eye-view-lighter eye-view-mode-map
  (cond (eye-view-mode (read-only-mode t))
        (t (read-only-mode nil))))

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
    (cond (global-eye-mode (timer-activate eye-panel-refresh-timer))
          (t (cancel-timer eye-panel-refresh-timer))))

(cl-defmacro eye-def-widget (name &key lighter daemon (repeat 1))
  (declare (indent 1))
  (let ((vdata (intern (format "eye-%s-data" name)))
        (vlighter (intern (format "eye-%s-lighter" name)))
        (vtimer (intern (format "eye-%s-timer" name)))
        (vmode (intern (format "eye-%s-mode" name)))
        (vglobal-mode (intern (format "global-eye-%s-mode" name)))
        (vdaemon (intern (format "eye-%s-daemon" name))))
    `(let ((widget-id (quote ,name)))

       (when (a-get eye-widgets widget-id)
         (user-error "Widget already defined: %s" widget-id))

       (defvar ,vdata (a-list)
         "Widget data storage.")

       (defvar ,vlighter eye-lighter
         "Widget default lighter.")

       (cl-defun ,vdaemon ()
         (setq ,vdata (a-merge ,vdata (funcall ,daemon ,vdata)))
         (setq ,vlighter (funcall ,lighter ,vdata)))

       (defvar ,vtimer
         (let ((time (current-time))
               (repeat ,repeat)
               (timer (timer-create))
               (fn (quote ,vdaemon)))
           (timer-set-time timer time repeat)
           (timer-set-function timer fn)
           timer))

       (define-minor-mode ,vmode "Widget minor mode.")

       ;; (:eval (list mode-line-front-space ,vlighter))

       (define-globalized-minor-mode ,vglobal-mode
           ,vmode ,vmode nil
           (cond (,vglobal-mode (timer-activate ,vtimer))
                 (t (cancel-timer ,vtimer)
                    (setq ,vlighter (funcall ,lighter ,vdata))))))))

;; (cl-defun eye-widget-insert (widget-id)
;;   (let ((component (ctbl:create-table-component-region
;;                     :model (eval (a-get* eye-widgets widget-id :model))
;;                     :keymap eye-view-map)))
;;     (ctbl:cp-add-click-hook component (a-get* eye-widgets widget-id :on-click))
;;     (ctbl:cp-add-update-hook component (a-get* eye-widgets widget-id :on-update))))

;; (cl-defun eye-activate-widgets ()
;;   (cl-loop for (widget-id . widget) in eye-widgets
;;      do (mapc #'timer-activate (a-get* widget :timers))))

;; (cl-defun eye-deactivate-widgets ()
;;   (cl-loop for (widget-id . widget) in eye-widgets
;;      do (mapc #'cancel-timer (a-get* widget :timers))))

;; (cl-defun eye-refresh ()
;;   (interactive)
;;   (when-let (cp (condition-case nil
;;                     (ctbl:cp-get-component)
;;                   (error nil)))
;;     (cl-loop for (widget . params) in eye-widgets
;;        when (eql (a-get params :component) cp)
;;        do (ctbl:cp-set-model cp (eval (a-get params :model))))))

(defvar eye-panel-format (list))

(defvar eye-panel-buffer-name "*Eye*")

(defvar eye-panel-refresh-timer
  (let ((time (current-time))
        (repeat 1)
        (timer (timer-create))
        (fn #'eye-panel-refresh))
    (timer-set-time timer time repeat)
    (timer-set-function timer fn)
    timer))

(defun eye-panel-refresh ()
  (when-let (window (get-buffer-window "*Eye*"))
    (with-current-buffer "*Eye*"
      (let ((inhibit-read-only t)
            (window-height (window-body-height window t))
            content-height)
        (delete-region (point-min) (point-max))
        (cl-loop for widget in eye-panel-format
           when (symbolp widget)
           do (insert-image (eval (intern (format "eye-%s-lighter" widget)))))))))

(defun eye-panel ()
  (interactive)

  (when (get-buffer-window eye-panel-buffer-name)
    (eye-panel-quit))

  (let ((buffer (get-buffer-create eye-panel-buffer-name)))
    (with-current-buffer buffer
      (eye-view-mode)

      (let ((window (display-buffer-in-side-window
                     buffer
                     (a-list 'side 'top
                             'dedicated t
                             'window-parameters (a-list 'no-other-window t
                                                        'no-delete-other-windows t
                                                        'mode-line-format 'none
                                                        'header-line-format 'none
                                                        'tab-line-format 'none)))))
        (set-window-text-height window 1)
        (setq-local cursor-type nil
                    window-size-fixed 'height)))))

(defun eye-panel-quit ()
  (interactive)
  (delete-windows-on eye-panel-buffer-name)
  (kill-buffer eye-panel-buffer-name))

(defun multiline-svg (&rest lines)
  (let* ((font (face-attribute 'default :font))
         (info (font-info font))
         (name (aref info 1))
         (font-family (font-get font :family))
         (size (font-get font :size))
         (height (font-get font :height))
         (font-weight "normal")
         (average-width (aref info 11))
         (space-width (aref info 10))
         (letter-spacing 1))
    (cl-loop for line in lines
       with default-params = (a-list
                              :font-family font-family
                              :font-size size
                              :font-width average-width
                              :letter-spacing letter-spacing
                              :font-weight font-weight
                              :fill "white")
       collect (cond ((listp line) (a-merge default-params line))
                     ((stringp line) (a-merge default-params (a-list :text line))))
       into svg-lines
       finally (return (let* ((width (* (+ 1 letter-spacing average-width) (-max (mapcar #'length lines))))
                              (height (* (line-pixel-height) (length lines)))
                              (svg (svg-create width height))
                              (margin-top 0))
                         (cl-loop for svg-line in svg-lines
                            do (incf margin-top (a-get svg-line :font-size))
                            do (svg-text svg (a-get svg-line :text)
                                         :font-family (a-get svg-line :font-family)
                                         :font-weight (a-get svg-line :font-weight)
                                         :letter-spacing (a-get svg-line :letter-spacing)
                                         :font-size (a-get svg-line :font-size)
                                         :fill (a-get svg-line :fill)
                                         :x 0
                                         :y margin-top))
                         svg)))))

(provide 'eye)
