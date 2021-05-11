;;; -*- lexical-binding: t -*-

;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
;; https://github.com/hlissner/doom-emacs/blob/e46736e44d06db2eac3b3c1e19b498876a8b398a/docs/faq.org#how-does-doom-start-up-so-quickly
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      frame-title-format '("Emacs: %*%+%@ | %b | "user-login-name"@"system-name)
      initial-frame-alist '((width . 88)
                            (height . 39))
      default-frame-alist initial-frame-alist)

(push '(menu-bar-lines) default-frame-alist) ; ~emacs -nw~
(push '(tool-bar-lines) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      inhibit-startup-screen t
      window-divider-default-places t
      window-divider-default-bottom-width 0
      window-divider-default-right-width 1)
(window-divider-mode t)

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 100)

(setq custom-file null-device
      make-backup-files nil
      package-enable-at-startup nil
      ;; https://depp.brause.cc/dotemacs/#org95dddf4
      ;; "Emacs tries saving your buffers if it receives a fatal signal (including
      ;; module segfaults).  This is batshit insane, I prefer a clean exit over
      ;; silent corruption."
      ;; It does seem better to lose some rather than risking corruption?
      attempt-stack-overflow-recovery nil
      attempt-orderly-shutdown-on-fatal-signal nil)

(defun my/emacs-startup-in-echo-area ()
  "Display the package number & loading time in the echo area."
  (message "GNU Emacs %d.%d loaded %d packages in %.3f seconds."
           emacs-major-version emacs-minor-version
           (let ((number-of-packages
                  (length (directory-files
                           (concat user-emacs-directory "etc/straight/build/") nil "[^.]"))))
             number-of-packages)
           (float-time
            (time-subtract (current-time) before-init-time))))
(defun display-startup-echo-area-message ()
  "The default for this can be disabled with `inhibit-startup-echo-area-message',
the problem is that it only takes effect if you do it with the customization
buffer or if I put my user name in the command... which is nonsense.

https://depp.brause.cc/dotemacs/#org2cdacdd
https://emacshorrors.com/posts/advertising-your-freedom.html"
  (my/emacs-startup-in-echo-area))

(setq straight-check-for-modifications '(find-when-checking))

(defun my/lower-gc ()
  "Bring gc closer to default."
  (setq gc-cons-threshold 16777216      ; 16mb
        gc-cons-percentage 0.1))
(add-hook 'emacs-startup-hook #'my/lower-gc)
