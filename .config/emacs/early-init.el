;;; -*- lexical-binding: t -*-
;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; https://github.com/hlissner/doom-emacs/blob/665b627b7c07c8d29ec8d334588cecc2ba308248/docs/faq.org#how-does-doom-start-up-so-quickly
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup nil)

(setq initial-frame-alist '((width . 97)
                            (height . 49))
      default-frame-alist initial-frame-alist)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

(set-face-attribute 'default nil
                    :font "DejaVu Sans Mono")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))
