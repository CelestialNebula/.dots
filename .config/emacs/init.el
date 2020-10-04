;;; -*- lexical-binding: t -*-
;; Do Ut Des
;; Emacs outshines all other editing software in approximately the same way
;; that the noonday sun does the stars.  It is not just bigger and brighter;
;; it simply makes everything else vanish.
;; In the Beginning... Was the Command Line by Neal Stephenson (1999)

;; Interesting things others have done:
;; https://github.com/caisah/emacs.dz
;; https://github.com/a13/emacs.d
;; https://gitlab.com/LuisHP/emacs.d/-/tree/master

;; *TODO*
;; 1. Find a way to move ~{10-20} lines up and down, I would rather it be
;; on the base level but I don't think any convenient keys are left.  Tho I
;; suppose it wouldn't need to be /too/ convenient.
;; 'SPC-.', "'", and '=', are all open.
;; = is nice in Org cause I don't have to go to insert.
;; 2. themes/mode-line: Can I make a file with just the edits I did to the theme
;; and just have it load that after the theme? Judging by what
;; 'how-to-get-the-default-theme' says probably not.
;; https://stackoverflow.com/questions/22127337/emacs-how-to-get-the-default-theme/22129687#22129687
;; Actually maybe I can:
;; https://old.reddit.com/r/emacs/comments/geisxd/what_is_a_best_way_to_modify_theme_downloaded/fpnns1q/
;; Find a way to change the buffer name colour only on active windows.
;; Maybe also change the buffer name colour for specific stuff?  Like running
;; 'find-grep|rgrep' and see how the process thing has a colour (it looks really out
;; of place when it's on the non-active mode-line when the buffer-name itself
;; isn't in colour).  Then again when I find a way to change the colour on
;; non-active windows it will probably be fine.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html#Mode-Line-Format
;; https://emacs.stackexchange.com/questions/10033/change-mode-line-buffer-id-face-for-inactive-windows
;; 3. If I use emacs --daemon, I'll have to fix some things.  It starts with
;; *server* and it "prepends" the start message in the *scratch* buffer.  The
;; modeline also seems to blend in to the background.  (But not on the
;; non-active ones.)  I think this also starts in insert mode but shows the C?
;; This is the issue about it starting in insert mode.
;; https://github.com/xahlee/xah-fly-keys/issues/103
;; 4. Anyway to move image-dired to /tmp/ so it's not persistent?  I don't
;; need/want to keep the thumbnails.  (Can I point it to the null-device just
;; like custom-file?)
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/image-dired.el
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/image-dired.el#L624
;; This is old and might not work/have unintended consequences
;; https://old.reddit.com/r/emacs/comments/3weyg6/is_there_a_package_to_group_data_files_into_one/
;; 5. How does 'xah-fly-keys' take away key-chords ie. SPC (wait 5 mins) g, will
;; close the current buffer.  Anyway I can do that for CTRL so the few GNU/Emacs
;; commands I still need don't have chords?
;; 6. 'toggle-maximize-window' Need to find a way to get a status on the
;; mode-line for when I'm zoomed in (Z) maybe add it before the Narrow thing in
;; the 'mode-line-format'.
;; 7. Since I moved to straight.el seeing how many packages I have with '(length
;; package-activated-list)' no longer works.
;; https://github.com/raxod502/straight.el/issues/262#issuecomment-376928443
;; https://github.com/hlissner/doom-emacs/blob/8b52e8ca0f0873ba8b4e569d7fd7d2e80a4a45ab/core/core.el#L430
;; If I can decipher the code from doom I can get an idea of how to do it.

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
  (show-paren-delay 0)             ; This must be before show-paren-mode.
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
  (defun toggle-maximize-window ()
    "Maximize current window (like default tmux: prefix+z).
Doesn't keep your spot: zoom at line 100 then go to 500
and unzoom and it's back at 100.

https://old.reddit.com/r/emacs/comments/gtfxg4/zoommonocle_a_buffer/fsbe7da/"
    (interactive)
    (if (one-window-p)
	(jump-to-register '_)
      (progn
	(window-configuration-to-register '_)
	(delete-other-windows))))
  (defun calendar-show-week (arg)
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
  (calendar-show-week t)
  (defun whitelist-whitespace-hook ()
    "'show-trailing-whitespace'"
    (setq show-trailing-whitespace t))
  (add-hook 'org-mode-hook 'whitelist-whitespace-hook)
  (add-hook 'text-mode-hook 'whitelist-whitespace-hook)
  (add-hook 'prog-mode-hook 'whitelist-whitespace-hook)
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
     mode-line-client		    ; Is it a emacsclient frames
     "%n"			    ; Show 'Narrow' when narrowing is in effect
     " %4l:"			    ; Line position
     ;; Current column number, including warning for +80 columns
     (:eval
      (propertize "%3C" 'face
                  (when (>= (current-column) 80)
                    'mode-line-80col-face)))
     ;; How far in the file: All, Top, Bottom, Percent through the file ie. 40%
     " | %p | "
     mode-name
     mode-line-process
     " | %b "
     ;; Shows if the file is maintained with version control
     (:eval
      (cond
       (vc-mode
        (propertize "Ω"
                    'face 'mode-line-vc-face
                    'help-echo "Do Ut Des"))))
     (vc-mode vc-mode)
     (:eval
      (cond
       (vc-mode
        (propertize "Ω"
                    'face 'mode-line-vc-face
                    'help-echo "Do Ut Des"))))
     ;; Buffer path
     " "
     (:propertize
      (:eval (shorten-directory default-directory 30)))))
  ;; https://emacs.stackexchange.com/questions/10955/customize-vc-mode-appearance-in-mode-line/10957#10957
  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval (replace-regexp-in-string "^ Git-" "" vc-mode))))
  (defun shorten-directory (dir max-length)
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
  (defun double-flash-mode-line ()
    "Flash the mode line twice during an exception (like ^g)."
    (let ((flash-sec (/ 1.0 20)))
      (invert-face 'mode-line)
      (run-with-timer flash-sec nil #'invert-face 'mode-line)
      (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
      (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))
  (setq visible-bell nil
        ring-bell-function 'double-flash-mode-line))

(use-package dired
  :custom
  (dired-recursive-copies 'always)
  (delete-by-moving-to-trash t "Use FreeDesktop.orgs trash")
  (dired-listing-switches "--all --escape --group-directories-first --human-readable -l")
  :bind (:map dired-mode-map
              ("z" . shell-command)))

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
  :bind
  ("C-c l" . org-store-link))
  ;; ("C-c a" . org-agenda)

(use-package flyspell
  :hook ((text-mode . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode)))

(use-package tramp
  ;; https://github.com/raxod502/straight.el/issues/490
  ;; I used to get this ^ error when :straight t is uncommented but so far so
  ;; good
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
  ;; Any other way to do this for the starting buffer?  Cause this is every
  ;; buffer that's new after the starting too. Which means I wouldn't get the
  ;; 'show-trailing-whitespace' on new buffers.
  (initial-major-mode 'text-mode)
  :config
  (defun emacs-startup-in-echo-area ()
    "Display the package number/loading time in the echo area."
    (message "GNU/Emacs loaded XX packages in %s seconds."
             ;; "GNU/Emacs loaded %d packages in %s seconds."
             ;; (length package-activated-list)
             (format "%.3f"
                     (float-time (time-subtract (current-time) before-init-time)))))
  (add-hook 'emacs-startup-hook 'emacs-startup-in-echo-area))

(use-package xah-fly-keys
  :straight t
  :demand t
  :custom
  (xah-fly-keys-set-layout "dvorak")
  :config
  (xah-fly-keys t)
  :bind (:map xah-fly-command-map
	      ("SPC z" . toggle-maximize-window)))

(use-package srcery-theme
  :straight t
  :init
  (add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
  (load-theme 'srcery t))

(use-package which-key
  :straight t
  :demand t
  :custom
  (which-key-idle-delay 0.4)
  :config
  (which-key-mode))

;; Programming
(defun display-header ()
  "Display the heading with the path to the file."
  (setq header-line-format
        '("%f")))
(add-hook 'prog-mode-hook 'display-header)

(defun prog-truncate-lines ()
  "Turn on 'truncate-lines'."
  (toggle-truncate-lines t))
(add-hook 'prog-mode-hook 'prog-truncate-lines)

;; Indentation can't insert tabs
;; I might not need this with editerconfig?
;; https://old.reddit.com/r/javascript/comments/c8drjo/nobody_talks_about_the_real_reason_to_use_tabs/
;; (setq-default indent-tabs-mode nil)

(use-package magit
  :straight t
  :bind
  ("C-x g" . magit-status))

(use-package flycheck
  :straight t
  :hook ((prog-mode elpy-mode-hook) . flycheck-mode))

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
  :bind (:map elpy-mode-map
	      ;; Interactive Python
	      ("SPC . w" . elpy-shell-switch-to-shell)
	      ("SPC . u" . elpy-shell-send-region-or-buffer)
	      ("SPC . e" . elpy-shell-send-statement-and-step)
	      ;; Completion
	      ("SPC . /" . elpy-company-backend)
	      ;; Documentation
	      ("SPC . h" . elpy-doc)
	      ;; Navigation
	      ("SPC . v" . elpy-goto-definition-other-window)
	      ("SPC . z" . elpy-occur-definitions)
	      ("SPC . m" . elpy-goto-assignment)
	      ;; Syntax checking
	      ("SPC . j" . elpy-check)
	      ;; Refactoring
	      ("SPC . p" . elpy-multiedit-python-symbol-at-point)
	      ("SPC . TAB" . elpy-format-code)
	      ;; Debugging
	      ("SPC . ." . elpy-pdb-debug-buffer)
	      ("SPC . ," . elpy-pdb-toggle-breakpoint-at-point)
	      ("SPC . '" . elpy-pdb-break-at-point)
	      ("SPC . y" . elpy-pdb-debug-last-exception)
	      ;; Projects
	      ("SPC . r" . elpy-set-project-root)
	      ("SPC . b" . elpy-rgrep-symbol)))

;; Doesn't work in terminal
(use-package company-quickhelp
  :straight t
  :hook (prog-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-delay 0.1)
  (company-quickhelp-max-lines 30)
  (company-quickhelp-color-foreground "purple")
  (company-quickhelp-color-background "gray"))

;; https://stackoverflow.com/questions/15958448/settings-only-for-gui-terminal-emacs/15962540#15962540
(defun is-in-terminal()
  "Return t if GNU/Emacs is running in a terminal."
  (not (display-graphic-p)))

(defmacro when-term (&rest body)
  "Works just like `progn' but will only evaluate expressions in VAR when Emacs
is running in a terminal else just nil."
  `(when (is-in-terminal) ,@body))

(when-term
 (menu-bar-mode 0))
