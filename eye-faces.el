;; -*- lexical-binding: t; -*-

(defgroup eye-faces nil
  "Faces for eye."
  :tag "Eye Faces"
  :group 'faces)

(defface eye-lighter
    '((((class color) (min-colors 88) (background light)) (:foreground "darkgreen"))
      (((class color) (min-colors 88) (background dark)) (:foreground "#2ecc71"))
      (((class color) (min-colors 16) (background light)) (:foreground "darkgreen"))
      (((class color) (min-colors 16) (background dark)) (:foreground "#2ecc71"))
      (((class color) (min-colors 8)) (:foreground "green"))
      (t (:bold t)))
  "Face used for eye in your modeline."
  :group 'eye-faces)

(defface eye-view-lighter
    '((((class color) (min-colors 88) (background light)) (:foreground "#8e44ad" :height 1.2))
      (((class color) (min-colors 88) (background dark)) (:foreground "#9b59b6" :height 1.2))
      (((class color) (min-colors 16) (background light)) (:foreground "#8e44ad" :height 1.2))
      (((class color) (min-colors 16) (background dark)) (:foreground "#9b59b6" :height 1.2))
      (((class color) (min-colors 8)) (:foreground "magenta"))
      (t (:bold t)))
  "Face used for eye in your modeline."
  :group 'eye-faces)

(provide 'eye-faces)
