(require 'promise)

(defvar eye-panel-format (list))
(defvar eye-panel-buffer-name "*Eye Panel*")
(defvar eye-panel-font-size 14)
(defvar eye-panel-text-height nil)
(defvar eye-panel-letter-spacing 0)
(defvar eye-panel-height-lines 2)
(defvar eye-panel-refresh-timer
  (let ((time (current-time))
        (repeat 1)
        (timer (timer-create))
        (fn #'eye-panel-refresh))
    (timer-set-time timer time repeat)
    (timer-set-function timer fn)
    timer))

(defun eyecon (&rest lines)
  (let* ((font (face-attribute 'default :family))
         (font-weight "normal")
         (lines (cl-loop
                   with config = (a-list :font-family (face-attribute 'default :family)
                                         :font-size eye-panel-font-size
                                         :letter-spacing eye-panel-letter-spacing
                                         :font-weight font-weight
                                         :fill (if (eq 'dark (frame-parameter nil 'background-mode))
                                                   "white"
                                                 "black"))
                   for line in lines
                   collect (cond ((listp line) (a-merge config line))
                                 ((stringp line) (a-merge config (a-list :text line))))))
         (image-width (* (+ eye-panel-letter-spacing ;; TODO figure out maximum letter spacing
                            eye-panel-font-width)
                         (1+ (-max (--map (length (a-get it :text)) lines)))))
         (image-height eye-panel-height)
         (svg (svg-create image-width image-height))
         (margin-top (/ (- image-height (* (length lines) eye-panel-font-size)) (1+ (length lines)))) ;; TODO figure out panel font size for each line
         )

    ;; (svg-rectangle svg 0 0 image-width image-height
    ;;                :stroke-width 2
    ;;                :stroke-color "#4CB5F5")

    (cl-loop for line in lines
       do
         (incf margin-top (a-get line :font-size))
         (svg-text svg (a-get line :text)
                   :font-family (a-get line :font-family)
                   :font-weight (a-get line :font-weight)
                   :letter-spacing (a-get line :letter-spacing)
                   :font-size (a-get line :font-size)
                   ;; :stroke "white"
                   ;; :stroke-width 0.5
                   :fill (a-get line :fill)
                   :x 0
                   :y margin-top))

    (svg-image svg)))

(defun eye-separator ()
  (eyecon ""))

(define-minor-mode eye-panel-mode
    "Draw simple panel.")

(define-globalized-minor-mode global-eye-panel-mode
    eye-panel-mode eye-panel-mode nil
    (when (get-buffer-window eye-panel-buffer-name)
      (eye-panel-quit))
    (cond (global-eye-panel-mode
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
                 (set-window-text-height window eye-panel-height-lines)
                 (insert "Loading, please wait...")

                 (let ((eye-panel-text-pixel-size (window-text-pixel-size window (point-min) (+ 1 (point-min)))))
                   (setq eye-panel-font-height (cdr eye-panel-text-pixel-size)
                         eye-panel-font-width (car eye-panel-text-pixel-size)
                         eye-panel-height (window-text-height window t)
                         cursor-type nil))

                 (delete-region (point-min) (point-max))
                 (setq window-size-fixed 'height))))
           (timer-activate eye-panel-refresh-timer))
          (t (cancel-timer eye-panel-refresh-timer)
             (cl-loop for widget in eye-widgets
                do (eval (list (intern (format "global-eye-%s-mode" widget)) -1))))))

(defun eye-panel-refresh ()
  (when-let (window (get-buffer-window eye-panel-buffer-name))
    (with-current-buffer eye-panel-buffer-name
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))

        (cl-loop for widget in eye-widgets
           if (member widget eye-panel-format)
           ;; (and
           ;;  (not (eval (intern (format "global-eye-%s-mode" widget)))))
           do (eval (list (intern (format "global-eye-%s-mode" widget)) +1))
           else
           do (eval (list (intern (format "global-eye-%s-mode" widget)) -1)))

        (cl-loop for widget in eye-panel-format
           do (when-let (image (eval (intern (format "eye-%s-lighter" widget))))
                (insert-image image)
                (insert-image (eye-separator))))))))

(defun eye-panel-quit ()
  (interactive)
  (delete-windows-on eye-panel-buffer-name)
  (kill-buffer eye-panel-buffer-name))

(defvar eye-widgets (list)
  "List of widgets to show in reports.")

(cl-defmacro eye-def-widget (name store &key lighter persist)
  (declare (indent 1) (debug t))
  (let ((vars (a-list
               :store (intern (format "eye-%s-store" name))
               :lighter (intern (format "eye-%s-lighter" name))
               :timer (intern (format "eye-%s-timer" name))
               :mode (intern (format "eye-%s-mode" name))
               :global-mode (intern (format "global-eye-%s-mode" name))
               :daemon (intern (format "eye-%s-daemon" name))
               :init (intern (format "eye-%s-init" name))
               :quit (intern (format "eye-%s-quit" name)))))
    `(progn
       (cl-pushnew (quote ,name) eye-widgets)
       (define-minor-mode ,(a-get vars :mode) "Widget minor mode.")

       (defvar ,(a-get vars :lighter) (a-list) "Widget icon.")

       (define-globalized-minor-mode ,(a-get vars :global-mode)
           ,(a-get vars :mode) ,(a-get vars :mode) nil
           (cond (,(a-get vars :global-mode) (timer-activate ,(a-get vars :timer)))
                 (t (cancel-timer ,(a-get vars :timer)))))

       (cl-defun ,(a-get vars :daemon) ()
         (let ((store ,store))
           (promise-chain store
             (thena (setq ,(a-get vars :store) result)
                    result)
             (thena ,lighter)
             (catcha (setq eye-panel-format (delq (quote ,name) eye-panel-format))
                     (message "Widget \"%s\" has been disabled due to refresh error: %s" (quote ,name) reason))
             (done (lambda (result)
                     (setq ,(a-get vars :lighter) result))))))

       ,(when persist
          `(list (require 'persist)

                 (cl-defun ,(a-get vars :init) ()
                   (setq ,(a-get vars :store) (persist-load (quote ,(a-get vars :store))))
                   (let ((result ,(a-get vars :store)))
                     ,persist))

                 (cl-defun ,(a-get vars :quit) ()
                   (persist-save (quote ,(a-get vars :store))))

                 (funcall (function ,(a-get vars :init)))
                 (add-hook 'kill-emacs-hook (function ,(a-get vars :quit)))))

       (defvar ,(a-get vars :timer)
         (let ((time (current-time))
               (repeat 1)
               (timer (timer-create)))
           (timer-set-time timer time repeat)
           (timer-set-function timer (quote ,(a-get vars :daemon)))
           timer)))))

(provide 'eye-panel)
