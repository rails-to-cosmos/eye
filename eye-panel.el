(require 'promise)

(defvar eye-panel-format (list))

(defcustom eye-panel-buffer-name "*Eye Panel*"
  "Buffer name for eye panel.")

(defcustom eye-panel-letter-spacing 0
  "Default letter spacing for eye panel.")

(defcustom eye-panel-height-lines 2
  "Eye panel height in lines.")

(defcustom eye-panel-font-size 14
  "Default font size for eye panel.")

(defconst eye-panel-config (a-list 'side 'top
                                   'dedicated t
                                   'window-parameters (a-list 'no-other-window t
                                                              'no-delete-other-windows t
                                                              'mode-line-format 'none
                                                              'header-line-format 'none
                                                              'tab-line-format 'none)))

(defconst eye-panel-font (a-list :font-family (face-attribute 'default :family)
                                 :font-size eye-panel-font-size
                                 :letter-spacing eye-panel-letter-spacing
                                 :font-weight "normal"
                                 :fill (if (eq 'dark (frame-parameter nil 'background-mode))
                                           "white"
                                         "black")))

(defun eyecon-create (width height)
  (apply #'eyecon (cl-loop for j to height
                     collect (make-string width ?\ ))))

(defun eyecon-update (icon &rest lines)
  (let* ((config eye-panel-font)
         (lines (cl-loop for line in lines
                   collect (cond ((listp line) (a-merge config line))
                                 ((stringp line) (a-merge config (a-list :text line))))))
         (svg icon)
         (margin-top (/ (- eye-panel-height (* (length lines) eye-panel-font-size)) (1+ (length lines)))) ;; TODO figure out panel font size for each line
         )

    (cl-loop for line in lines
       for id from 0
       do
         (incf margin-top (a-get line :font-size))
         (svg-text svg (xml-escape-string (a-get line :text))
                   :id (format "%d" id)
                   :font-family (a-get line :font-family)
                   :font-weight (a-get line :font-weight)
                   :letter-spacing (a-get line :letter-spacing)
                   :font-size (a-get line :font-size)
                   ;; :stroke "white"
                   ;; :stroke-width 0.5
                   :fill (a-get line :fill)
                   :x 0
                   :y margin-top))

    svg))

(defun eyecon (&rest lines)
  (let* ((config eye-panel-font)
         (lines (cl-loop for line in lines
                   collect (cond ((listp line) (a-merge config line))
                                 ((stringp line) (a-merge config (a-list :text line))))))
         (image-width (min (* (+ eye-panel-letter-spacing ;; TODO figure out maximum letter spacing
                                 eye-panel-font-width)
                              (1+ (-max (--map (length (a-get it :text)) lines))))
                           500))
         (image-height eye-panel-height)
         (svg (svg-create image-width image-height))
         (margin-top (/ (- image-height (* (length lines) eye-panel-font-size)) (1+ (length lines)))) ;; TODO figure out panel font size for each line
         )

    (svg-rectangle svg 0 0 image-width image-height
                   :fill (if (eq 'dark (frame-parameter nil 'background-mode))
                             "black"
                           "white")
                   :stroke-width 2
                   :stroke-color "#4CB5F5")

    (cl-loop for line in lines
       for id from 0
       do
         (incf margin-top (a-get line :font-size))
         (svg-text svg (xml-escape-string (a-get line :text))
                   :id (format "%d" id)
                   :font-family (a-get line :font-family)
                   :font-weight (a-get line :font-weight)
                   :letter-spacing (a-get line :letter-spacing)
                   :font-size (a-get line :font-size)
                   ;; :stroke "white"
                   ;; :stroke-width 0.5
                   :fill (a-get line :fill)
                   :x 0
                   :y margin-top))

    svg))

(defun eye-alist-p (list)
  "Non-null if and only if LIST is an alist with simple keys."
  (while (consp list)
    (setq list (if (and (consp (car list))
                        (atom (caar list)))
                   (cdr list)
                 'not-alist)))
  (null list))

(define-minor-mode eye-panel-mode
    "Draw simple panel.")

(define-globalized-minor-mode global-eye-panel-mode
    eye-panel-mode eye-panel-mode nil
    (if global-eye-panel-mode
        (eye-panel-init)
      (eye-panel-quit)))

(defun eye-panel-init ()
  (let ((buffer (get-buffer-create eye-panel-buffer-name)))
    (with-current-buffer buffer
      (let ((window (display-buffer-in-side-window buffer eye-panel-config)))
        (set-window-text-height window eye-panel-height-lines)
        (insert "Loading, please wait...")

        (let ((eye-panel-text-pixel-size (window-text-pixel-size window (point-min) (+ 1 (point-min)))))
          (setq eye-panel-font-height (cdr eye-panel-text-pixel-size)
                eye-panel-font-width (car eye-panel-text-pixel-size)
                eye-panel-height (window-text-height window t)
                cursor-type nil))

        (let ((inhibit-read-only t))
          (delete-region (point-min) (point-max))

          (cl-loop for widget in eye-widgets
             if (member widget eye-panel-format)
             do (eval (list (intern (format "global-eye-%s-mode" widget)) +1))
             else
             do (eval (list (intern (format "global-eye-%s-mode" widget)) -1)))

          (cl-loop for widget in eye-panel-format
             do
               (eval (read (format "(setq eye-%s-eyecon (eyecon-create %d %d))"
                                   widget
                                   (eval (intern (format "eye-%s-width" widget)))
                                   (eval (intern (format "eye-%s-height" widget))))))
               (svg-insert-image (eval (intern (format "eye-%s-eyecon" widget))))))

        (setq window-size-fixed 'height)))))

(defun eye-panel-quit ()
  (interactive)
  (when (get-buffer-window eye-panel-buffer-name)
    (delete-windows-on eye-panel-buffer-name)
    (kill-buffer eye-panel-buffer-name))

  (cl-loop for widget in eye-widgets
     do (eval (list (intern (format "global-eye-%s-mode" widget)) -1))))

(defvar eye-widgets (list)
  "List of widgets to show in reports.")

(cl-defmacro eye-def-widget (name &key process observer (mapper 'result) lighter persist (repeat 1) (width 50) (height 2))
  (declare (indent 1) (debug t))
  (let-alist (a-list 'data (intern (format "eye-%s-data" name))
                     'lighter (intern (format "eye-%s-lighter" name))
                     'eyecon (intern (format "eye-%s-eyecon" name))
                     'width (intern (format "eye-%s-width" name))
                     'height (intern (format "eye-%s-height" name))
                     'timer (intern (format "eye-%s-timer" name))
                     'mode (intern (format "eye-%s-mode" name))
                     'global-mode (intern (format "global-eye-%s-mode" name))
                     'observer (intern (format "eye-%s-observer" name))
                     'init (intern (format "eye-%s-init" name))
                     'quit (intern (format "eye-%s-quit" name)))
    `(progn
       (cl-pushnew (quote ,name) eye-widgets)
       (define-minor-mode ,.mode "Widget minor mode.")

       (defvar ,.timer nil)
       (setq ,.width ,width)
       (setq ,.height ,height)
       (defvar ,.eyecon (list) "Widget svg")

       (require 'persist)
       (persist-defvar ,.lighter (list) "Widget icon data.")
       (persist-defvar ,.data (a-list) "Widget data store.")
       (let-alist ,.data ,persist)

       (define-globalized-minor-mode ,.global-mode
           ,.mode ,.mode nil
           (cond (,.global-mode (timer-activate ,.timer)
                                (,.observer))
                 (t (cancel-timer ,.timer))))

       (cl-defun ,.observer ()
         (promise-chain (let ((observer ,observer))
                          (cond ((promise-class-p observer) observer)
                                ((functionp observer) (promise:make-thread observer))
                                ((listp observer) (promise-chain (promise:make-process observer)
                                                    (thena (mapcar #'s-trim (s-split "\n" (s-join "\n" result))))))
                                ((numberp observer) (promise:make-thread (lambda (&rest _) (number-to-string observer))))
                                (t (promise:make-thread (lambda (&rest _) observer)))))
           (thena (cond ((eye-alist-p result) result)
                        (t (a-list (quote ,name) result))))
           (thena (let-alist result ,mapper))
           (thena (setq ,.data result) result)
           (thena (setq ,.lighter (let-alist result ,lighter)))
           (thena (apply #'eyecon-update ,.eyecon ,.lighter))
           (catcha (setq ,.lighter (list (s-titleize (format "%s" (quote ,name))) "?"))
                   (apply #'eyecon-update ,.eyecon ,.lighter)
                   (message "Widget \"%s\" refresh error: %s" (quote ,name) reason))))

       (condition-case nil
           (cancel-timer ,.timer)
         (error nil))

       (setq ,.timer
             (let ((time (current-time))
                   (timer (timer-create)))
               (timer-set-time timer time ,repeat)
               (timer-set-function timer (quote ,.observer))
               timer)))))

(provide 'eye-panel)
