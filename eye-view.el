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

(defvar eyecons (a-list)
  "List of widgets to show in reports.")

(defvar eye-lighter
  (propertize ""
              'face 'eye-lighter
              'display '((height 2)
                         (raise 0))))

(defconst eye-view-lighter
  #(" " 0 1 (rear-nonsticky t display nil font-lock-face eye-view-lighter face eye-view-lighter)))

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

;; (cl-defun eyecon-insert (widget-id)
;;   (let ((component (ctbl:create-table-component-region
;;                     :model (eval (a-get* eyecons widget-id :model))
;;                     :keymap eye-view-map)))
;;     (ctbl:cp-add-click-hook component (a-get* eyecons widget-id :on-click))
;;     (ctbl:cp-add-update-hook component (a-get* eyecons widget-id :on-update))))

;; (cl-defun eye-activate-widgets ()
;;   (cl-loop for (widget-id . widget) in eyecons
;;      do (mapc #'timer-activate (a-get* widget :timers))))

;; (cl-defun eye-deactivate-widgets ()
;;   (cl-loop for (widget-id . widget) in eyecons
;;      do (mapc #'cancel-timer (a-get* widget :timers))))

;; (cl-defun eye-refresh ()
;;   (interactive)
;;   (when-let (cp (condition-case nil
;;                     (ctbl:cp-get-component)
;;                   (error nil)))
;;     (cl-loop for (widget . params) in eyecons
;;        when (eql (a-get params :component) cp)
;;        do (ctbl:cp-set-model cp (eval (a-get params :model))))))



(provide 'eye)
