;;; -*- lexical-binding: t; -*-

;; Do Ut Des

;; Emacs outshines all other editing software in approximately the same way
;; that the noonday sun does the stars.  It is not just bigger and brighter;
;; it simply makes everything else vanish.
;;   In the Beginning... Was the Command Line by Neal Stephenson (1999)

;; Interesting things others have done:
;; https://github.com/caisah/emacs.dz
;; https://github.com/a13/emacs.d
;; https://gitlab.com/LuisHP/emacs.d/-/tree/master

;; Package Setup with straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(use-package emacs
  :hook (prog-mode . flyspell-prog-mode)
  :custom
  (frame-title-format '("Emacs: %*%+%@ | %b | "user-login-name"@"system-name))
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (initial-major-mode 'fundamental-mode)
  (blink-cursor-mode nil)
  (custom-file null-device "Don't store customization's.")
  (make-backup-files nil)
  (auto-save-file-name-transforms
   '((".*" "~/.config/emacs/auto-save-list/" t)))
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  (uniquify-buffer-name-style 'forward)
  (require-final-newline t)
  (isearch-lazy-count t)
  (use-package-always-defer t)
  (calendar-week-start-day 6)	; https://sachachua.com/blog/2010/11/week-beginnings/
  (show-paren-delay 0)
  :config
  (show-paren-mode t)
  (setq-default fill-column 80
		word-wrap t
		truncate-lines t)
  (ido-mode t)
  (global-display-line-numbers-mode t)
  (global-hl-line-mode t)
  (defun my/emacs-startup-in-echo-area ()
    "Display the package number/loading time in the echo area."
    (message "GNU Emacs %d.%d loaded %d packages in %.3f seconds."
	     emacs-major-version emacs-minor-version
	     (let ((number-of-packages
		    (length (directory-files (concat user-emacs-directory "straight/build/") nil "[^.]"))))
	       number-of-packages)
             (float-time
	      (time-subtract (current-time) before-init-time))))
  (add-hook 'emacs-startup-hook 'my/emacs-startup-in-echo-area)
  (setq my/toggle-maximize-list nil)
  (defun my/toggle-maximize-window ()
    "Maximize current window (like default tmux: prefix+z).

Bugs:
Doesn't keep your spot: zoom at line 100 then go to 500 and unzoom and
  it's back at 100.  (I don't know if I mind this?)
Can become nested, so always close other windows before you toggle again.

https://old.reddit.com/r/emacs/comments/gtfxg4/zoommonocle_a_buffer/fsbe7da/"
    (interactive)
    (if (one-window-p)
	(progn
	  (set 'my/toggle-maximize-list 'nil)
	  (jump-to-register '_))
      (set 'my/toggle-maximize-list 't)
      (window-configuration-to-register '_)
      (delete-other-windows)))
  (defun my/whitelist-whitespace-hook ()
    "Turn on 'show-trailing-whitespace' for specified modes.
Too much stuff has trailing whitespace when you turn it on globally."
    (setq show-trailing-whitespace t))
  (dolist (hook '(org-mode-hook text-mode-hook prog-mode-hook conf-mode-hook))
    (add-hook hook 'my/whitelist-whitespace-hook))
  ;; https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
  ;; https://github.com/cryon/dotemacs/blob/bf233aebb823af57299d93a31d8363f112e1f117/auto/modeline.el
  (setq-default
   mode-line-format
   '("%e"
     mode-line-front-space	; xah-fly-keys uses this for C|I
     mode-line-mule-info
     mode-line-modified
     mode-line-remote
     (:propertize
      (:eval
       (if (frame-parameter nil 'client)
	   "&"))
      help-echo "emacsclient frame")
     (:eval
      (propertize "%3C"
		  'face (when (>= (current-column) 80)
			  'mode-line-80col-face)))
     " | %p | "
     mode-name
     mode-line-process
     " | "
     (:propertize "%b"
		  face mode-line-buffer-face)
     (:eval
      (if (string= "t" my/toggle-maximize-list)
	  (propertize "Z"
		      'face 'mode-line-misc-face)))
     (:eval
      (propertize "%n "
		  'face 'mode-line-misc-face))
     mode-line-misc-info
     (:eval
      (cond
       (vc-mode
        (propertize "Ω"
                    'face 'mode-line-misc-face
                    'help-echo "Do Ut Des"))))
     (vc-mode vc-mode)
     (:eval
      (cond
       (vc-mode
        (propertize "Ω"
                    'face 'mode-line-misc-face
                    'help-echo "Do Ut Des"))))
     " "
     (:propertize
      (:eval (my/shorten-directory default-directory 30)))))
  ;; https://emacs.stackexchange.com/questions/10955/customize-vc-mode-appearance-in-mode-line/10957#10957
  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval
	     (replace-regexp-in-string "^.*:\\|.*-" "" vc-mode))))
  (defun my/shorten-directory (dir max-length)
    "Show up to `MAX-LENGTH' characters of a directory name `DIR'."
    (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
          (output ""))
      (when (and path (equal "" (car path)))
        (setq path (cdr path)))
      (while (and path (< (length output) (- max-length 4)))
        (setq output (concat (car path) "/" output))
        (setq path (cdr path)))
      (when path
        (setq output (concat "Ψ/" output)))
      output))
  (defun my/mode-line-double-flash ()
    "Flash the mode line twice during an exception (like ^g)."
    (let ((flash-sec (/ 1.0 20)))
      (invert-face 'mode-line)
      (run-with-timer flash-sec nil #'invert-face 'mode-line)
      (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
      (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))
  (setq visible-bell nil
        ring-bell-function 'my/mode-line-double-flash))

(use-package org
  :straight t
  :custom
  (org-startup-folded t)
  (org-log-done t)
  (org-adapt-indentation nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0))

(use-package dired
  :hook (dired-mode . toggle-truncate-lines)
  :custom
  (dired-recursive-copies 'always)
  (delete-by-moving-to-trash t "Use FreeDesktop.orgs trash (~/.local/share/Trash/)")
  (dired-listing-switches "-ahl --group-directories-first")
  :bind (:map dired-mode-map
              ("z" . dired-smart-shell-command)))

(use-package xah-fly-keys
  :straight t
  :demand t
  :custom
  (xah-fly-keys-set-layout "dvorak")
  :config		; I still need this in case I just call ~emacs~
  (xah-fly-keys t)
  ;; This seems inelegant but it does seem to work...
  (defun my/clear-set-dot-keybinds ()
    "Clear keybinds and set them if they match specific 'major-mode'.
Don't want keybinds to propagate to other modes, so empty the 'dot-keymap'.
Check the major-mode for the keybinds to set, if it doesn't match don't set any.

https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Hooks.html
'window-buffer-change-functions' may be what I should use?  But I couldn't get it to work.
https://stackoverflow.com/questions/47456134/emacs-lisp-hooks-for-detecting-change-of-active-buffer"
    (xah-fly--define-keys
     (define-prefix-command 'xah-fly-dot-keymap)
     '())
    (define-key xah-fly-dot-keymap (kbd "g") 'magit-status)
    (cond
     ((equal major-mode 'org-mode)
      (define-key xah-fly-dot-keymap (kbd "s") 'org-store-link)
      (define-key xah-fly-dot-keymap (kbd "l") 'org-insert-link)
      (define-key xah-fly-dot-keymap (kbd "t") 'org-todo)
      (define-key xah-fly-dot-keymap (kbd "o") 'org-open-at-point)
      (define-key xah-fly-dot-keymap (kbd "a") 'org-mark-ring-goto)
      (define-key xah-fly-dot-keymap (kbd "n") 'org-insert-structure-template)
      (define-key xah-fly-dot-keymap (kbd "d") 'org-export-dispatch))
     ((equal major-mode 'dired-mode)
      (define-key xah-fly-dot-keymap (kbd "s") 'org-store-link))
     ((equal major-mode 'sh-mode)
      (define-key xah-fly-dot-keymap (kbd "o") 'my/manual-posix-sh-shellcheck))
     ((equal major-mode 'python-mode)
      ;; Interactive Python
      (define-key xah-fly-dot-keymap (kbd "w") 'elpy-shell-switch-to-shell)
      (define-key xah-fly-dot-keymap (kbd "u") 'elpy-shell-send-region-or-buffer)
      (define-key xah-fly-dot-keymap (kbd "e") 'elpy-shell-send-statement-and-step)
      ;; Completion
      (define-key xah-fly-dot-keymap (kbd "/") 'elpy-company-backend)
      ;; Documentation
      (define-key xah-fly-dot-keymap (kbd "h") 'elpy-doc)
      ;; Navigation
      (define-key xah-fly-dot-keymap (kbd "v") 'elpy-goto-definition-other-window)
      (define-key xah-fly-dot-keymap (kbd "z") 'elpy-occur-definitions)
      (define-key xah-fly-dot-keymap (kbd "m") 'elpy-goto-assignment)
      ;; Syntax checking
      (define-key xah-fly-dot-keymap (kbd "o") 'elpy-check)
      ;; Refactoring
      (define-key xah-fly-dot-keymap (kbd "p") 'elpy-multiedit-python-symbol-at-point)
      (define-key xah-fly-dot-keymap (kbd "TAB") 'elpy-format-code)
      ;; Debugging
      (define-key xah-fly-dot-keymap (kbd ".") 'elpy-pdb-debug-buffer)
      (define-key xah-fly-dot-keymap (kbd ",") 'elpy-pdb-toggle-breakpoint-at-point)
      (define-key xah-fly-dot-keymap (kbd "'") 'elpy-pdb-break-at-point)
      (define-key xah-fly-dot-keymap (kbd "y") 'elpy-pdb-debug-last-exception)
      ;; Projects
      (define-key xah-fly-dot-keymap (kbd "r") 'elpy-set-project-root)
      (define-key xah-fly-dot-keymap (kbd "b") 'elpy-rgrep-symbol))))
  (setq window-state-change-hook 'my/clear-set-dot-keybinds)
  :bind (:map xah-fly-command-map
	      ("'" . xah-fill-or-unfill)
	      ("SPC '" . xah-reformat-lines)
	      ("b" . isearch-forward-regexp)
	      ("SPC r s" . string-rectangle)
	      ("SPC z" . my/toggle-maximize-window)
	      ("SPC v" . display-buffer)))

(use-package which-key
  :straight t
  :defer 1
  :custom
  (which-key-idle-delay 0.4)
  :config
  (which-key-mode))

(use-package srcery-theme
  :straight t
  :init
  (advice-add 'load-theme :after #'my/customize-srcery-theme)
  :custom
  (srcery-org-height nil)
  ;; https://emacs.stackexchange.com/questions/48365/custom-theme-set-faces-does-not-work-in-emacs-27/52804#52804
  (custom--inhibit-theme-enable nil)
  :config
  (defun my/customize-srcery-theme (_theme &rest _args)
    "Tweaks to make srcery-theme more appealing.

https://old.reddit.com/r/emacs/comments/geisxd/what_is_a_best_way_to_modify_theme_downloaded/fpnns1q/"
    (when (member 'srcery custom-enabled-themes)
      (make-face 'mode-line-80col-face)
      (make-face 'mode-line-buffer-face)
      (make-face 'mode-line-misc-face)
      ;; colors from srcery-theme.el
      (let* ((srcery-class '((class color) (min-colors 257)))
	     (srcery-256-class '((class color) (min-colors 89)))
	     (srcery-256-red            "red")
	     (srcery-256-green          "green"))
	(custom-theme-set-faces
	 'srcery
	 `(fringe
	   ((,srcery-class (:background "#000000"))))
	 `(flyspell-duplicate
	   ((,srcery-class (:foreground ,srcery-green :underline (:style wave)))
	    (,srcery-256-class (:foreground ,srcery-256-green :underline t))))
	 `(flyspell-incorrect
	   ((,srcery-class (:foreground ,srcery-red :underline (:style wave)))
	    (,srcery-256-class (:foreground ,srcery-256-red :underline t))))
	 `(line-number
	   ((,srcery-class (:foreground "#918175" :background "#0f0e0d"))
	    (,srcery-256-class (:foreground "#918175" :background "#0f0e0d"))))
	 `(mode-line-inactive
	   ((,srcery-class (:foreground "#918175" :background "#303030"))
	    (,srcery-256-class (:foreground "#918175" :background "#303030"))))
	 `(mode-line-80col-face
	   ((,srcery-class (:foreground "#ff7f50" :background "#0f0e0d"))
	    (,srcery-256-class (:foreground "#ff7f50" :background "#0f0e0d"))))
	 `(mode-line-buffer-face
	   ((,srcery-class (:weight bold))
	    (,srcery-256-class (:weight bold))))
	 `(mode-line-misc-face
	   ((,srcery-class (:foreground "#ff7f50"))
	    (,srcery-256-class (:foreground "#ff7f50")))))))))
;; Can't be put under the 'use-package' section?
(load-theme 'srcery t)

;; Programming
(defun my/prog-display-header ()
  "Display the heading with the path to the file."
  (setq header-line-format '("%f")))
(add-hook 'prog-mode-hook 'my/prog-display-header)

(use-package magit
  :straight t)

(use-package flycheck
  :straight t
  :hook ((prog-mode elpy-mode-hook) . flycheck-mode))

;; Shell
(use-package sh-mode
  :init
  (defun my/manual-posix-sh-shellcheck ()
    "I don't think flycheck can just do POSIX sh?  So this takes the current file
and runs 'shellcheck -axs sh '."
    (interactive)
    (shell-command
     (concat "shellcheck -axs sh "
	     (shell-quote-argument buffer-file-name))))
  :custom
  (sh-basic-offset 8))

;; Python
(use-package elpy
  :straight t
  :hook (python-mode . elpy-enable)
  :custom
  (elpy-get-info-from-shell t)
  :config
  (delq 'elpy-module-flymake elpy-modules)
  (delq 'elpy-module-highlight-indentation elpy-modules)
  (setenv "WORKON_HOME" (expand-file-name "~/venv/"))
  (defalias 'workon 'pyvenv-workon))

;; https://github.com/jwiegley/use-package#conditional-loading
;; https://stackoverflow.com/questions/15958448/settings-only-for-gui-terminal-emacs/15962540#15962540
;; (defun my/is-in-terminal ()
;;   "Return t if GNU Emacs is running in a terminal."
;;   (not (display-graphic-p)))
;; (defmacro when-term (&rest body)
;;   "Works just like `progn' but will only evaluate expressions in VAR when Emacs
;; is running in a terminal else just nil."
;;   `(when (my/is-in-terminal) ,@body))
;; (when-term
;;  )

(defun my/server-fix-up ()
  "Make sure 'xah-fly-keys' is starting in command-mode.

https://github.com/xahlee/xah-fly-keys/issues/103
https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Tips-08.org#configuring-the-ui-for-new-frames"
  (xah-fly-keys t))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'my/server-fix-up))
