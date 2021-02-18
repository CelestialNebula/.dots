;;; -*- lexical-binding: t -*-

;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize t
      initial-frame-alist '((width . 88)
                            (height . 39))
      default-frame-alist initial-frame-alist)

(push '(menu-bar-lines . 0) default-frame-alist) ; ~emacs -nw~
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(set-face-attribute 'default nil
                    :font "DejaVu Sans Mono")

(defun my/lower-gc ()
  "Bring gc closer to default."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))
(add-hook 'emacs-startup-hook #'my/lower-gc)
