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

;; *ToDo*
;; 1. If I use ~emacs --daemon~, I'll have to fix some things.  It currently
;; starts in insert mode (but still shows the 'C').
;; This is the issue: https://github.com/xahlee/xah-fly-keys/issues/103
;; 2. themes/mode-line: Interesting read, about some problems I might run into:
;; https://stackoverflow.com/questions/22127337/emacs-how-to-get-the-default-theme/22129687#22129687
;; Change the 'mode-name' colour for specific stuff, like running
;; 'find-grep|rgrep' and see how 'mode-line-process' has a colour (it looks
;; really out of place when it's on the non-active mode-line when the
;; 'mode-name' itself isn't in colour).
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html#Mode-Line-Format
;; In org-mode the comment (=) format takes precedent over the spelling one
;; (flyspell) inside comments, so I can't see if something is spelled wrong.
;; So how do I make the spelling one take precedent?
;; Inside comments (=) '~' already takes precedent.  So when you highlight '='
;; (which is orange) and it has '~' (which is blue) it will show that section as
;; blue inside the comment.
;; Maybe something with 'font-lock-face'?
;; https://www.emacswiki.org/emacs/FaceMenuPlus
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Properties.html
;; 3. How can I remap stuff that needs to be "nested" under another keymap?
;; The keybinds under org & elpy, I need to only be active under their
;; respective modes only in 'xah-fly-command-map' because if I just leave
;; it to 'org|elpy-mode-map' it steals my SPC in insert mode?
;; Actually I need some of the org stuff for dired like: 'org-store-link'
;; I don't think it can be it's own mode cause if I open another buffer that has
;; new keybinds set it will mess up, tho I might be able to just have it
;; continuously check which major-mode I'm in and reset?
;; Maybe something involving the following:
;; derived-mode-p, xah-fly-dot-keymap, xah-fly-command-map,
;; xah-fly--define-keys, xah-fly-insert-state-q
;; Maybe do something like this?
;; https://github.com/xahlee/xah-fly-keys/issues/107
;; https://github.com/xahlee/xah-fly-keys/issues/108
;; The best way is probably something like 'evil-define-key':
;; https://github.com/emacs-evil/evil/search?q=evil-define-key
;; https://emacs.stackexchange.com/questions/10856/how-do-i-set-up-key-bindings-for-modes-in-a-specific-evil-state
;; Maybe follow this?
;; https://github.com/jwiegley/use-package/issues/885

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
(use-package use-package
  :custom
  (use-package-always-defer t))

(use-package emacs
  :custom
  (frame-title-format '("Emacs: %*%+%@ | %b | "user-login-name"@"system-name""))
  (blink-cursor-mode nil)
  (fill-column 80)
  (uniquify-buffer-name-style 'forward)
  (show-paren-delay 0)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  (word-wrap t)
  (custom-file null-device "Don't store customization's.")
  (isearch-lazy-count t)
  (require-final-newline t)
  ;; Keep all backup and auto-save files in one directory.
  (backup-directory-alist
   '(("." . "~/.config/emacs/backups/")))
  (auto-save-file-name-transforms
   '((".*" "~/.config/emacs/auto-save-list/" t)))
  (backup-by-copying t)
  (delete-old-versions t)
  (version-control t)
  ;; https://sachachua.com/blog/2010/11/week-beginnings/
  (calendar-week-start-day 6 "Set Saturday as the first day of the week.")
  :config
  (show-paren-mode t)
  (ido-mode t)
  (global-display-line-numbers-mode t)
  (global-hl-line-mode t)
  (setq toggle-maximize-list '())
  (defun my/toggle-maximize-window ()
    "Maximize current window (like default tmux: prefix+z).
Doesn't keep your spot: zoom at line 100 then go to 500 and unzoom and it's
back at 100.  And it can become nested, so always close other windows
before you toggle again.

https://old.reddit.com/r/emacs/comments/gtfxg4/zoommonocle_a_buffer/fsbe7da/"
    (interactive)
    (if (one-window-p)
	(progn
	  (set 'toggle-maximize-list 'false)
	  (jump-to-register '_))
      (set 'toggle-maximize-list 'true)
      (window-configuration-to-register '_)
      (delete-other-windows)))
  (defun my/calendar-show-week (arg)
    "Displaying week number in calendar-mode."
    (interactive "P")
    (copy-face font-lock-constant-face 'calendar-iso-week-face)
    (set-face-attribute
     'calendar-iso-week-face nil :height 0.7)
    (setq calendar-intermonth-text
          (and arg
               '(propertize
                 (format "%2d"
                         (car (calendar-iso-from-absolute
                               (calendar-absolute-from-gregorian
                                (list month day year)))))
                 'font-lock-face 'calendar-iso-week-face))))
  (my/calendar-show-week t)
  (defun my/whitelist-whitespace-hook ()
    "Turn on 'show-trailing-whitespace' for specified modes.
Too much stuff has trailing whitespace when you just turn it on globally."
    (setq show-trailing-whitespace t))
  (add-hook 'org-mode-hook 'my/whitelist-whitespace-hook)
  (add-hook 'text-mode-hook 'my/whitelist-whitespace-hook)
  (add-hook 'prog-mode-hook 'my/whitelist-whitespace-hook)
  (add-hook 'conf-mode-hook 'my/whitelist-whitespace-hook)
  ;; https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
  ;; https://github.com/cryon/dotemacs/blob/master/auto/modeline.el
  (setq-default
   mode-line-format
   '(
     ;; When GNU/Emacs is nearly out of memory for Lisp objects, a brief
     ;; message saying so.  (Don't think I've ever seen this.)
     "%e"
     mode-line-front-space     		; xah-fly-keys uses this for C|I
     mode-line-mule-info
     mode-line-modified
     mode-line-remote
     mode-line-client			; Is it an emacsclient frames
     mode-line-misc-info
     ;; Current column number, including warning for +80 columns
     (:eval
      (propertize "%3C"
		  'face (when (>= (current-column) 80)
			  'mode-line-80col-face)))
     ;; How far in the file: All, Top, Bottom, Percent through the file ie. 40%
     " | %p | "
     mode-name
     mode-line-process
     " | "
     (:propertize "%b"
		  face mode-line-buffer-face)
     (:eval
      (if (string= "true" toggle-maximize-list)
	  (propertize "Z"
		      'face 'mode-line-misc-face)))
     (:eval
      (propertize "%n "
		  'face 'mode-line-misc-face))
     ;; Shows if the file is maintained with version control
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
     ;; Buffer path
     " "
     (:propertize
      (:eval (my/shorten-directory default-directory 30)))))
  ;; https://emacs.stackexchange.com/questions/10955/customize-vc-mode-appearance-in-mode-line/10957#10957
  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval (replace-regexp-in-string "^.*:\\|.*-" "" vc-mode))))
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

(use-package dired
  :custom
  (dired-recursive-copies 'always)
  (delete-by-moving-to-trash t "Use FreeDesktop.orgs trash")
  (dired-listing-switches "--all --group-directories-first --human-readable -l")
  :bind (:map dired-mode-map
              ("z" . shell-command)))

(use-package flyspell
  :hook ((text-mode . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode)))

(use-package tramp
  :straight t
  :custom
  (tramp-default-method "ssh")
  ;; Backup remote files like local files.
  (tramp-backup-directory-alist backup-directory-alist))

(use-package "startup"
  :custom
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  ;; https://gitlab.com/LuisHP/emacs.d/-/blob/master/main.org
  ;; "If you’re concerned about startup time (I definitely am) make sure to set
  ;; initial-major-mode to fundamental-mode. If you set it to a mode that
  ;; you’ve customized with hooks and/or packages, it will trigger them."
  ;; (initial-major-mode 'fundamental-mode)
  (initial-major-mode 'text-mode)
  :config
  (defun my/emacs-startup-in-echo-area ()
    "Display the package number/loading time in the echo area."
    (message "GNU/Emacs loaded %d packages in %s seconds."
	     (setq mylist (length (directory-files "~/.config/emacs/straight/build" nil "[^.]")))
             (format "%.3f"
                     (float-time
		      ;; Seems to make more sense to use 'after-init-time' but
		      ;; that gives a faster time.
		      ;; (time-subtract after-init-time before-init-time)))))
		      (time-subtract (current-time) before-init-time)))))
  (add-hook 'emacs-startup-hook 'my/emacs-startup-in-echo-area))

;; Not really a bug (just visually) but something I need to keep in mind.  In
;; 'whitespace-mode' if TAB is after a longer word, it may only TAB out to ~1
;; space.  Without 'whitespace-mode' it will look normal and match up but if you
;; call 'whitespace-mode' since TAB's represent 8 spaces it moves to look like 8
;; and messes up the alignment.
(use-package xah-fly-keys
  :straight t
  :demand t
  :custom
  (xah-fly-keys-set-layout "dvorak")
  :config
  (xah-fly-keys t)
  :bind (:map xah-fly-command-map
	      ("'" . xah-fill-or-unfill)
	      ("SPC '" . xah-reformat-lines)
	      ("b" . isearch-forward-regexp)
	      ("SPC r s" . string-rectangle)
	      ("SPC z" . my/toggle-maximize-window)))

(use-package org
  :straight t
  :custom
  (org-startup-truncated nil)
  (org-startup-folded t)
  (org-log-done t)
  (org-adapt-indentation nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  :bind (:map xah-fly-dot-keymap
  ;; :bind (:map org-mode-map
	      ("s" . org-store-link)
	      ("l" . org-insert-link)
	      ("t" . org-todo)
	      ("o" . org-open-at-point)
	      ("a" . org-mark-ring-goto)
	      ("n" . org-insert-structure-template)
	      ("d" . org-export-dispatch)))

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
;; I can't get this to work with 'my/customize-srcery-theme' when I put
;; it under the use-package section.
(load-theme 'srcery t)

(use-package which-key
  :straight t
  :defer 1
  :custom
  (which-key-idle-delay 0.4)
  :config
  (which-key-mode))

;; Programming
(defun my/display-header ()
  "Display the heading with the path to the file."
  (setq header-line-format
        '("%f")))
(add-hook 'prog-mode-hook 'my/display-header)

(defun my/prog-truncate-lines ()
  "Turn on 'truncate-lines'."
  (toggle-truncate-lines t))
(add-hook 'prog-mode-hook 'my/prog-truncate-lines)

;; Indentation can't insert tabs
;; I might not need this with editerconfig?
;; https://old.reddit.com/r/javascript/comments/c8drjo/nobody_talks_about_the_real_reason_to_use_tabs/
;; (setq-default indent-tabs-mode t)

(use-package magit
  :straight t
  :bind (:map xah-fly-dot-keymap
	      ("g" . magit-status)))

(use-package flycheck
  :straight t
  :hook ((prog-mode elpy-mode-hook) . flycheck-mode))

;; Shell
(use-package sh-mode
  :init
  (defun my/manual-posix-sh-shellcheck ()
    "I don't think flycheck can just do POSIX sh?  So this takes the current file
and runs 'shellcheck --check-sourced -s sh --external-sources '."
    (interactive)
    (shell-command
     (concat "shellcheck --check-sourced -s sh --external-sources "
	     (shell-quote-argument buffer-file-name))))
  :custom
  (sh-basic-offset 8)
  :bind (:map xah-fly-dot-keymap
  ;; This doesn't seem to do anything?
  ;; :bind (:map sh-mode-map
	      ("f" . my/manual-posix-sh-shellcheck)))

;; Python
(use-package python-mode
  :custom
  (python-shell-interpreter "python3"))

(use-package elpy
  :straight t
  :hook (python-mode . elpy-enable)
  :custom
  (elpy-modules '(elpy-module-sane-defaults
		  elpy-module-company
		  elpy-module-eldoc
		  ;; elpy-module-flymake
		  elpy-module-pyvenv
		  elpy-module-yasnippet
		  elpy-module-django))
  (elpy-get-info-from-shell t)
  (elpy-rpc-python-command "/usr/local/bin/python3")
  :bind (:map xah-fly-dot-keymap
  ;; :bind (:map elpy-mode-map
	      ;; Interactive Python
	      ("w" . elpy-shell-switch-to-shell)
	      ("u" . elpy-shell-send-region-or-buffer)
	      ("e" . elpy-shell-send-statement-and-step)
	      ;; Completion
	      ("/" . elpy-company-backend)
	      ;; Documentation
	      ("h" . elpy-doc)
	      ;; Navigation
	      ("v" . elpy-goto-definition-other-window)
	      ("z" . elpy-occur-definitions)
	      ("m" . elpy-goto-assignment)
	      ;; Syntax checking
	      ("j" . elpy-check)
	      ;; Refactoring
	      ("p" . elpy-multiedit-python-symbol-at-point)
	      ("TAB" . elpy-format-code)
	      ;; Debugging
	      ("." . elpy-pdb-debug-buffer)
	      ("," . elpy-pdb-toggle-breakpoint-at-point)
	      ("'" . elpy-pdb-break-at-point)
	      ("y" . elpy-pdb-debug-last-exception)
	      ;; Projects
	      ("r" . elpy-set-project-root)
	      ("b" . elpy-rgrep-symbol)))

;; https://stackoverflow.com/questions/15958448/settings-only-for-gui-terminal-emacs/15962540#15962540
;; (defun my/is-in-terminal ()
;;   "Return t if GNU/Emacs is running in a terminal."
;;   (not (display-graphic-p)))

;; (defmacro when-term (&rest body)
;;   "Works just like `progn' but will only evaluate expressions in VAR when Emacs
;; is running in a terminal else just nil."
;;   `(when (my/is-in-terminal) ,@body))

;; (when-term
;;  )
