;; -*- lexical-binding: t; -*-

(require 'a)
(require 'dash)
(require 'pcase)
(require 's)
(require 'simple)
(require 'ctable)

;; (image-type-available-p TYPE)
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

       (defvar ,vlighter nil
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
(defvar eye-panel-font-size 14)
(defvar eye-panel-text-height nil)
(defvar eye-panel-letter-spacing 0)

(defvar eye-panel-refresh-timer
  (let ((time (current-time))
        (repeat 1)
        (timer (timer-create))
        (fn #'eye-panel-refresh))
    (timer-set-time timer time repeat)
    (timer-set-function timer fn)
    timer))

(defun eye-panel-refresh ()
  (when-let (window (get-buffer-window eye-panel-buffer-name))
    (with-current-buffer eye-panel-buffer-name
      (let ((inhibit-read-only t)
            content-height)
        (delete-region (point-min) (point-max))
        (cl-loop for widget in eye-panel-format
           when (symbolp widget)
           do (when-let (image (funcall (intern (format "eye-%s-lighter" widget))))
                (insert-image image)
                (insert-image (svg-image (multiline-svg "")))))))))

(cl-defun eye-panel (&optional (panel-height 2))
  (interactive)

  (when (get-buffer-window eye-panel-buffer-name)
    (eye-panel-quit))

  (let ((buffer (get-buffer-create eye-panel-buffer-name)))
    (with-current-buffer buffer
      (let ((window (display-buffer-in-side-window
                     buffer
                     (a-list 'side 'top
                             'dedicated t
                             'window-parameters (a-list 'no-other-window t
                                                        'no-delete-other-windows t
                                                        'mode-line-format 'none
                                                        'header-line-format 'none
                                                        'tab-line-format 'none)))))
        (set-window-text-height window panel-height)
        (insert "Loading, please wait...")

        (let ((eye-panel-text-pixel-size (window-text-pixel-size window (point-min) (+ 1 (point-min)))))
          (setq
                eye-panel-font-height (cdr eye-panel-text-pixel-size)
                eye-panel-font-width (car eye-panel-text-pixel-size)
                eye-panel-height (window-text-height window t)
                cursor-type nil))

        (delete-region (point-min) (point-max))
        (setq window-size-fixed 'height)))))

(defun eye-panel-quit ()
  (interactive)
  (delete-windows-on eye-panel-buffer-name)
  (kill-buffer eye-panel-buffer-name))

(defun multiline-svg (&rest lines)
  (let* ((font (face-attribute 'default :family))
         (font-weight "normal")
         (default-params (a-list
                          :font-family (face-attribute 'default :family)
                          :font-size eye-panel-font-size
                          :letter-spacing eye-panel-letter-spacing
                          :font-weight font-weight
                          :fill "white"))
         (svg-lines (cl-loop for line in lines
                       collect (cond ((listp line) (a-merge default-params line))
                                     ((stringp line) (a-merge default-params (a-list :text line))))))

         (image-width (* (+ eye-panel-letter-spacing  ;; TODO figure out maximum letter spacing
                            eye-panel-font-width)
                         (1+ (-max (mapcar #'length lines)))))
         (image-height eye-panel-height)
         (svg (svg-create image-width image-height))
         (margin-top (/ (- image-height (* (length svg-lines) eye-panel-font-size)) (1+ (length svg-lines)))) ;; TODO figure out panel font size for each line
         )

    ;; (svg-rectangle svg 0 0 image-width image-height
    ;;                :stroke-width 2
    ;;                :stroke-color "#4CB5F5")

    (cl-loop for svg-line in svg-lines
       do
         (incf margin-top (a-get svg-line :font-size))
         (svg-text svg (a-get svg-line :text)
                   :font-family (a-get svg-line :font-family)
                   :font-weight (a-get svg-line :font-weight)
                   :letter-spacing (a-get svg-line :letter-spacing)
                   :font-size (a-get svg-line :font-size)
                   :fill (a-get svg-line :fill)
                   :x 0
                   :y margin-top))

    svg))

(cl-defmacro eye-let (widget-name let-forms)
  (declare (indent 1) (debug t))
  (cl-assert (listp let-forms) t (format "let-forms: list expected but %s found" (type-of let-forms)))
  (cl-assert (eq (car let-forms) 'let*) t (format "let-forms should start with let*"))

  (let ((widget-vars (a-list
                      :data (intern (format "eye-%s-data" widget-name))
                      :lighter (intern (format "eye-%s-lighter" widget-name))
                      :timer (intern (format "eye-%s-timer" widget-name))
                      :mode (intern (format "eye-%s-mode" widget-name))
                      :global-mode (intern (format "global-eye-%s-mode" widget-name))
                      :daemon (intern (format "eye-%s-daemon" widget-name))))
        (widget-data (append '(a-list)
                             (cl-loop for (key val) in (cadr let-forms)
                                append (list (intern (format ":%s" key)) key)))))

    `(progn
       (define-minor-mode ,(a-get widget-vars :mode) "Widget minor mode.")

       (define-globalized-minor-mode ,(a-get widget-vars :global-mode)
           ,(a-get widget-vars :mode) ,(a-get widget-vars :mode) nil
           (cond (,(a-get widget-vars :global-mode) (timer-activate ,(a-get widget-vars :timer)))
                 (t (cancel-timer ,(a-get widget-vars :timer)))))

       (defvar ,(a-get widget-vars :data) (a-list) "Widget data storage.")

       (cl-defun ,(a-get widget-vars :daemon) ()
         (let* (,@(cadr let-forms))
           (setq ,(a-get widget-vars :data) (a-merge ,(a-get widget-vars :data) ,widget-data))))

       (cl-defun ,(a-get widget-vars :lighter) ()
         (let ,(cl-loop for (key val) in (cadr let-forms)
                  collect (list key `(a-get ,(a-get widget-vars :data)
                                            ,(intern (format ":%s" key)))))
           ,@(cddr let-forms)))

       (defvar ,(a-get widget-vars :timer)
         (let ((time (current-time))
               (repeat 1)
               (timer (timer-create)))
           (timer-set-time timer time repeat)
           (timer-set-function timer (quote ,(a-get widget-vars :daemon)))
           timer)))))

(provide 'eye)
