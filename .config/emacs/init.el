;;; -*- lexical-binding: t -*-

;; Do Ut Des

;; Emacs outshines all other editing software in approximately the same way
;; that the noonday sun does the stars.  It is not just bigger and brighter;
;; it simply makes everything else vanish.
;;   In the Beginning... Was the Command Line by Neal Stephenson (1999)

;; Interesting things others have done:
;; https://github.com/caisah/emacs.dz
;; https://github.com/a13/emacs.d
;; https://gitlab.com/LuisHP/emacs.d

;; Package Setup with straight.el
(defvar straight-base-dir
  (expand-file-name "etc/" user-emacs-directory))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
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
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name "var/auto-saves/" user-emacs-directory) t)))
  (auto-save-list-file-prefix
   (expand-file-name "var/auto-saves/sessions/" user-emacs-directory))
  (bookmark-default-file
   (expand-file-name "var/bookmarks.el" user-emacs-directory))
  (ido-save-directory-list-file
   (expand-file-name "var/ido-save-directory-list.el" user-emacs-directory))
  (url-cache-directory
   (expand-file-name "var/url/cache/" user-emacs-directory))
  (url-configuration-directory
   (expand-file-name "var/url/configuration/" user-emacs-directory))
  (blink-cursor-mode nil)
  (calendar-week-start-day 6)   ; https://sachachua.com/blog/2010/11/week-beginnings/
  (display-line-numbers-grow-only t)    ; Still needed for manual calls
  (display-line-numbers-width-start t)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Local-Variables.html#File-Local-Variables
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Safe-File-Variables.html#Safe-File-Variables
  ;; https://www.emacswiki.org/emacs/FileLocalVariables
  (enable-local-variables nil)
  (enable-local-eval nil)
  (fill-column 79)
  (require-final-newline t)
  (uniquify-buffer-name-style 'forward)
  (word-wrap t)
  (isearch-lazy-count t)
  (use-package-always-defer t)  ; Keep forgetting about this and it's a pain
  :config
  (global-hl-line-mode t)
  (setq my/toggle-maximize-list '())
  (defun my/toggle-maximize-window ()
    "Maximize current window (like tmux: prefix+z).

Doesn't keep your spot: zoom at line 100 then go to 500 and unzoom and
it's back at 100.

https://old.reddit.com/r/emacs/comments/gtfxg4/zoommonocle_a_buffer/fsbe7da/

TODO
Can become 'nested', so always close other windows before you toggle again.
  Make it like tmux, where it will unmaximize when you are 'nested' and hit the
keybind to make another window (and open the new window)."
    (interactive)
    (if (one-window-p)
        (progn
          (set 'my/toggle-maximize-list 'nil)
          (jump-to-register '_))
      (set 'my/toggle-maximize-list 't)
      (window-configuration-to-register '_)
      (delete-other-windows)))
  (defun my/allowlist-whitespace ()
    "Turn on `show-trailing-whitespace' for specified modes.

Currently: `conf-mode', `org-mode', `prog-mode', `text-mode'

Too much stuff has trailing whitespace when you turn it on globally."
    (setq show-trailing-whitespace t))
  (dolist (hook '(conf-mode-hook org-mode-hook prog-mode-hook text-mode-hook))
    (add-hook hook #'my/allowlist-whitespace))
  ;; https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
  ;; https://github.com/cryon/dotemacs/blob/bf233aebb823af57299d93a31d8363f112e1f117/auto/modeline.el
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
  (setq-default
   mode-line-format
   '("%e"
     mode-line-front-space    ; xah-fly-keys uses this for C|I
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
                  'face (when (>= (current-column) 79)
                          'mode-line-80col-face)))
     " | %p | "
     mode-name
     mode-line-process
     " | "
     "%b"
     (:eval
      (if (equal 't my/toggle-maximize-list)
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
        (propertize "Ω "
                    'face 'mode-line-misc-face
                    'help-echo "Do Ut Des"))))
     (:propertize
      (:eval (my/shorten-directory default-directory 30)))
     mode-line-end-spaces))
  ;; https://emacs.stackexchange.com/questions/10955/customize-vc-mode-appearance-in-mode-line/10957#10957
  (setcdr (assq 'vc-mode mode-line-format)
          '((:eval
             (replace-regexp-in-string "^ Git[-:]" "" vc-mode))))
  (defun my/mode-line-double-flash ()
    "Flash the mode line twice during an exception (like ^g)."
    (let ((flash-sec (/ 1.0 20)))
      (invert-face 'mode-line)
      (run-with-timer flash-sec nil #'invert-face 'mode-line)
      (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
      (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))
  (setq ring-bell-function #'my/mode-line-double-flash)
  :hook
  (Info-mode . xah-fly-insert-mode-activate)
  (Info-selection . xah-fly-insert-mode-activate))

(use-package dired
  :custom
  (delete-by-moving-to-trash t)    ; Use FreeDesktop.orgs trash ($XDG_DATA_HOME/Trash/)
  (dired-recursive-copies 'always)
  (ls-lisp-ignore-case nil)
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-verbosity '(links uid gid))
  :config
  (require 'ls-lisp)
  :bind (:map dired-mode-map
              ("z" . dired-smart-shell-command)))

(use-package eshell
  :custom
  (eshell-directory-name
   (expand-file-name "var/eshell" user-emacs-directory))
  (eshell-aliases-name
   (expand-file-name "etc/eshell/aliases" user-emacs-directory))
  (eshell-prompt-function
   (lambda ()
     (concat
      (abbreviate-file-name (eshell/pwd)) "\n"
      ;; Exit status is always 0 for Lisp built-in commands
      (if (not (equal '0 eshell-last-command-status))
          (format "%s" eshell-last-command-status))
      (if (= (user-uid) 0) "# " "$ "))))
  ;; This only matches the last line cause that's where my prompt is.  Is that fine?
  (eshell-prompt-regexp "^[[:digit:]]*[#$] "))

(use-package org
  :defer 1            ; This is so I can bind:map to `xah-fly-command-map' here
  :custom
  (org-agenda-files (list
                     (expand-file-name "var/org/agenda/" user-emacs-directory)))
  (org-id-locations-file
   (expand-file-name "var/org/id-locations.el" user-emacs-directory))
  (org-adapt-indentation nil)
  (org-agenda-custom-commands
   '(("o" "Overview"
      ((agenda "")
       (todo "")))))
  (org-agenda-start-on-weekday 6)
  (org-agenda-start-with-log-mode t)
  (org-agenda-time-leading-zero t)
  ;; This does not work like I expect.  | = mouse point
  ;; You can't delete from the start: * test|...
  ;; but you can remove from the end: * test...|
  ;; Also `xah-delete-current-text-block' works on a folded heading.
  (org-catch-invisible-edits 'error)
  (org-edit-src-content-indentation 0)
  (org-id-link-to-org-use-id 'use-existing)
  (org-log-done 'time)
  (org-src-fontify-natively t)
  (org-startup-folded t)
  :config
  (require 'org-id)
  (defun my/org-agenda ()
    "Start `org-agenda' in Overview."
    (interactive)
    (org-agenda nil "o"))
  :bind (:map xah-fly-command-map
              ("SPC n s" . my/org-agenda)))

;; TODO
;; `balance-windows' will also balance these.
;; `which-key' opens in a side-window(?) or at least opens in that section
;; `my/toggle-maximize-window' breaks when one of these is open.
(use-package window
  :custom
  (display-buffer-alist
   '(("\\*e?shell\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -2)
      (window-height . 0.25)
      (window-parameters . ((no-delete-other-windows . t)
                            (mode-line-format . (" " mode-name mode-line-process)))))
     ("\\*Messages\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 1)
      (window-height . 0.25)
      (window-parameters . ((no-delete-other-windows . t)
                            (mode-line-format . (" " mode-name)))))
     ("\\*Python\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . 0.25)
      (window-parameters . ((no-delete-other-windows . t)
                            (mode-line-format . (" " mode-name " | %b")))))))
  :bind ("<f12>" . window-toggle-side-windows))

(use-package xah-fly-keys
  :straight t
  :demand t
  :custom
  (xah-fly-keys-set-layout "dvorak")
  :config           ; Needed for just calling ~emacs~
  (xah-fly-keys t)
  (defun my/xterm-cursor ()
    "Change the cursor shape in xterm to match the GUI (xah-fly-keys)."
    (unless (display-graphic-p)
      (if xah-fly-insert-state-q
          (send-string-to-terminal "\033[6 q") ; Vertical bar
        (send-string-to-terminal "\033[2 q")))) ; Box
  ;; This seems inelegant but it does seem to work...
  ;; Ya kinda bad cause I have to define all my `xah-fly-dot-keymap' stuff in
  ;; here.
  (defun my/clear-set-dot-keybinds ()
    "Clear `xah-fly-dot-keymap' then based on the `major-mode' set keybinds.

Currently: `dired-mode', `Info-mode', `org-mode', `python-mode', `sh-mode'"
    (xah-fly--define-keys
     ;; Don't want keybinds to propagate to other modes
     (define-prefix-command 'xah-fly-dot-keymap)
     '(("g" . magit-status)
       ("d" . magit-file-dispatch)))
    (cond
     ((equal major-mode 'dired-mode)
      (define-key xah-fly-dot-keymap (kbd "s") #'org-store-link))
     ((equal major-mode 'Info-mode)
      (define-key xah-fly-dot-keymap (kbd "s") #'org-store-link))
     ((equal major-mode 'org-mode)
      (define-key xah-fly-dot-keymap (kbd "p") #'org-schedule)
      (define-key xah-fly-dot-keymap (kbd "a") #'org-mark-ring-goto)
      (define-key xah-fly-dot-keymap (kbd "o") #'org-open-at-point)
      (define-key xah-fly-dot-keymap (kbd "i") #'org-id-get-create)
      (define-key xah-fly-dot-keymap (kbd "l") #'org-insert-link)
      (define-key xah-fly-dot-keymap (kbd "d") #'org-export-dispatch)
      (define-key xah-fly-dot-keymap (kbd "t") #'org-todo)
      (define-key xah-fly-dot-keymap (kbd "n") #'org-insert-structure-template)
      (define-key xah-fly-dot-keymap (kbd "s") #'org-store-link))
     ((equal major-mode 'python-mode)
      ;; Interactive Python
      (define-key xah-fly-dot-keymap (kbd "w") #'elpy-shell-switch-to-shell)
      (define-key xah-fly-dot-keymap (kbd "u") #'elpy-shell-send-region-or-buffer)
      (define-key xah-fly-dot-keymap (kbd "e") #'elpy-shell-send-statement-and-step)
      ;; Completion
      (define-key xah-fly-dot-keymap (kbd "/") #'elpy-company-backend)
      ;; Documentation
      (define-key xah-fly-dot-keymap (kbd "h") #'elpy-doc)
      ;; Navigation
      (define-key xah-fly-dot-keymap (kbd "v") #'elpy-goto-definition-other-window)
      (define-key xah-fly-dot-keymap (kbd "z") #'elpy-occur-definitions)
      (define-key xah-fly-dot-keymap (kbd "m") #'elpy-goto-assignment)
      ;; Syntax checking
      (define-key xah-fly-dot-keymap (kbd "o") #'elpy-check)
      ;; Refactoring
      (define-key xah-fly-dot-keymap (kbd "p") #'elpy-multiedit-python-symbol-at-point)
      (define-key xah-fly-dot-keymap (kbd "TAB") #'elpy-format-code)
      ;; Debugging
      (define-key xah-fly-dot-keymap (kbd ".") #'elpy-pdb-debug-buffer)
      (define-key xah-fly-dot-keymap (kbd ",") #'elpy-pdb-toggle-breakpoint-at-point)
      (define-key xah-fly-dot-keymap (kbd "'") #'elpy-pdb-break-at-point)
      (define-key xah-fly-dot-keymap (kbd "y") #'elpy-pdb-debug-last-exception)
      ;; Projects
      (define-key xah-fly-dot-keymap (kbd "r") #'elpy-set-project-root)
      (define-key xah-fly-dot-keymap (kbd "b") #'elpy-rgrep-symbol))
     ((equal major-mode 'sh-mode)
      (define-key xah-fly-dot-keymap (kbd "o") #'my/manual-posix-sh-shellcheck))))
  (setq window-state-change-hook #'my/clear-set-dot-keybinds)
  :hook
  (xah-fly-command-mode-activate . my/xterm-cursor)
  (xah-fly-insert-mode-activate . my/xterm-cursor)
  :bind (:map xah-fly-command-map
              ("'" . xah-fill-or-unfill)
              ("b" . isearch-forward-regexp)
              ("SPC v" . display-buffer)
              ("SPC z" . my/toggle-maximize-window)
              ("SPC r s" . string-rectangle)
              ("SPC t q" . save-buffers-kill-emacs)))

(use-package which-key
  :straight t
  :defer 1
  :custom
  (which-key-idle-delay 0.4)
  :config
  (which-key-add-key-based-replacements
    "SPC ." "mode local"
    "SPC c" "open/show buffer"
    "SPC e" "insert"
    "SPC h" "help"
    "SPC n" "misc"
    "SPC r" "section effects"
    "SPC t" "misc-rare"
    "SPC w" "elisp functions")
  (which-key-mode))

;; TODO
;; Additions, deletions, and changes of comments (;) in *.el files, is
;; unreadable.  Using `dired-diff' (=), select test1.el and compare with
;; test.el.  To play around with the colors I can use: M-x customize-face FACE.
;; `magit' doesn't have this problem, see how they did it.
;; Simple example:
;; test.el:
;; (message "cat") ; test
;; (message "cat") ; cat
;; (message "cat") ; mk
;; (message "cat")
;; test1.el:
;; (message "cat") ; test
;; (message "cat") ; dog
;; (message "dog") ;
(use-package srcery-theme
  :straight t
  :custom
  (srcery-org-height nil)
  :config
  (defun my/customize-srcery-theme (_theme &rest _args)
    "Tweaks to `srcery-theme'.

https://old.reddit.com/r/emacs/comments/geisxd/what_is_a_best_way_to_modify_theme_downloaded/fpnns1q/"
    (when (member 'srcery custom-enabled-themes)
      (make-face 'mode-line-80col-face)
      (make-face 'mode-line-misc-face)
      ;; colors from `srcery-theme.el'
      (let* ((srcery-class '((class color) (min-colors 257)))
             (srcery-256-class '((class color) (min-colors 89))))
        (custom-theme-set-faces
         'srcery
         `(dired-perm-write
           ((,srcery-class (:foreground "#FF8700" :weight bold :underline t))
            (,srcery-256-class (:foreground "color-208" :weight bold :underline t))))
         `(flyspell-incorrect
           ((,srcery-class (:foreground "#EF2F27" :underline (:style wave)))
            (,srcery-256-class (:foreground "red" :underline t))))
         `(mode-line-inactive
           ((,srcery-class (:foreground "#7E7E7E" :background "#303030"))
            (,srcery-256-class (:foreground "color-244" :background "color-236"))))
         `(mode-line-80col-face
           ((,srcery-class (:foreground "#FF7F50" :background "#0F0E0D"))
            (,srcery-256-class (:foreground "#FF7F50" :background "#0F0E0D"))))
         `(mode-line-misc-face
           ((,srcery-class (:foreground "#FF7F50"))
            (,srcery-256-class (:foreground "#FF7F50")))))))))
;; https://emacs.stackexchange.com/questions/48365/custom-theme-set-faces-does-not-work-in-emacs-27/52804#52804
(setq custom--inhibit-theme-enable nil)
(advice-add 'load-theme :after #'my/customize-srcery-theme)
(load-theme 'srcery t)

(use-package elfeed
  :straight t
  :custom
  (elfeed-db-directory
   (expand-file-name "var/elfeed" user-emacs-directory))
  (elfeed-enclosure-default-dir
   (expand-file-name "var/elfeed/enclosures" user-emacs-directory))
  (elfeed-search-filter "@7-days-ago +unread")
  :config
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "youtube\\.com"
                                :add '(youtube video)))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "reddit\\.com"
                                :add '(reddit)))
  :hook
  (elfeed-search-mode . xah-fly-insert-mode-activate)
  :bind (:map xah-fly-n-keymap
              ("q" . elfeed)
              :map elfeed-search-mode-map
              ("SPC" . next-line)
              ("DEL" . previous-line)
              ("j" . elfeed-search-yank)
              :map elfeed-show-mode-map
              ("j" . elfeed-show-yank)))

(use-package selectrum
  :straight t
  :custom
  (selectrum-mode t))

(use-package prescient
  :straight t
  :custom
  (prescient-persist-mode t))

(use-package selectrum-prescient
  :straight t
  :custom
  (selectrum-prescient-mode t))

(use-package prog-mode
  :custom
  (eldoc-idle-delay 0)
  (show-paren-delay 0)
  :config
  (show-paren-mode t)
  (defun my/prog-truncate-display-header ()
    "Truncate lines & display the heading with the path to the file."
    (setq truncate-lines t)
    (setq header-line-format '("%f")))
  :hook
  (prog-mode . flyspell-prog-mode)
  (prog-mode . my/prog-truncate-display-header))

(use-package magit
  :straight t
  :custom
  (transient-history-file
   (expand-file-name "var/transient/history.el" user-emacs-directory))
  (transient-levels-file
   (expand-file-name "etc/transient/levels.el" user-emacs-directory))
  (transient-values-file
   (expand-file-name "etc/transient/values.el" user-emacs-directory)))

(use-package flycheck
  :straight t
  :hook
  (prog-mode . flycheck-mode))

(use-package elisp-mode
  :config
  (setq find-function-C-source-directory "~/src/emacs/src")
  (defun my/no-tabs-save-check-parens ()
    "No tabs in Elisp and check for unbalanced parentheses after a save."
    (setq indent-tabs-mode nil)
    (add-hook 'after-save-hook #'check-parens nil t))
  :hook
  (emacs-lisp-mode . my/no-tabs-save-check-parens))

;; TODO
;; After '\' it inserts spaces:
;; echo cat \
;;      dog
;; I want 0 spaces:
;; echo cat \
;; dog
;; Looks like the '\' is escaping
;; https://superuser.com/questions/1037043/emacs-suppress-indent-after-in-shell-script-mode
;; https://emacs.stackexchange.com/questions/28343/sh-script-alignment-issues?noredirect=1
(use-package sh-script
  :custom
  (sh-basic-offset 8)
  ;; No indent for case patterns, one indent for lists.
  (sh-indent-for-case-label 0)
  (sh-indent-for-case-alt '+)
  :config
  (defun my/manual-posix-sh-shellcheck ()
    "I don't think `flycheck' can just do POSIX sh?  So this takes the current file
and runs ~shellcheck -axs sh `buffer-file-name'~."
    (interactive)
    (shell-command
     (concat "shellcheck -axs sh "
             (shell-quote-argument buffer-file-name)))))

(use-package python
  :custom
  (python-shell-interpreter "python3"))

(use-package elpy
  :straight t
  :custom
  (elpy-rpc-virtualenv-path
   (expand-file-name "var/elpy" user-emacs-directory))
  (elpy-rpc-python-command "python3")
  (elpy-formatter "black")
  (elpy-get-info-from-shell t)
  (yas-snippet-dirs
   (expand-file-name "etc/yasnippet/" user-emacs-directory))
  :config
  (defalias 'workon 'pyvenv-workon)
  (delq 'elpy-module-flymake elpy-modules)
  (delq 'elpy-module-highlight-indentation elpy-modules)
  (setenv "WORKON_HOME" (expand-file-name "~/src/venv/"))
  (eval-after-load 'yasnippet
    '(make-directory
      (expand-file-name "etc/yasnippet/snippets/" user-emacs-directory) t))
  :hook
  (python-mode . elpy-enable))

(let ((personal-stuff (expand-file-name "personal.el" user-emacs-directory)))
  (when (file-readable-p personal-stuff)
    (load-file personal-stuff)))

(defun my/server-fix-up ()
  "Make sure `xah-fly-keys' is starting in command-mode.

https://github.com/xahlee/xah-fly-keys/issues/103"
  (xah-fly-keys t))

(when (daemonp)
  (add-hook 'server-after-make-frame-hook #'my/server-fix-up))
