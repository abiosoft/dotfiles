;; Minimal UI
;;; Code: configuration
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
;; menubar is actually useful in GUI
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (menu-bar-mode   -1))
;; disable blinking cursor
(blink-cursor-mode 0)
;; hopefully improve flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;; macOS specific
(let ((is-mac (string-equal system-type "darwin")))
  (when is-mac
    ;; make fonts look better with anti-aliasing
    (setq mac-allow-anti-aliasing t)
    ;; delete files by moving them to the trash
    (setq delete-by-moving-to-trash t)
    (setq trash-directory "~/.Trash")

    ;; Set modifier keys
    (setq mac-option-modifier 'meta) ;; Bind meta to ALT
    (setq mac-command-modifier 'control) ;; Bind apple/command to super if you want
    (setq mac-function-modifier 'hyper) ;; Bind function key to hyper if you want
    (setq mac-right-option-modifier 'meta) ;; unbind right key for accented input

    ;; Make forward delete work
    (global-set-key (kbd "<H-backspace>") 'delete-forward-char)))

;;; Font
;; Set default font
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 150 ;; this is 1/10 pt e.g. 140 = 14pt
                    :weight 'normal
                    :width 'normal)
(set-face-attribute 'fixed-pitch nil
                    :family "JetBrains Mono"
                    :height 150 ;; this is 1/10 pt e.g. 140 = 14pt
                    :weight 'normal
                    :width 'normal)
(set-face-attribute 'variable-pitch nil
                    :family "BlinkMacSystemFont"
                    :height 170 ;; this is 1/10 pt e.g. 140 = 14pt
                    :weight 'normal
                    :width 'normal)

;; Ask "y" or "n" instead of "yes" or "no". Yes, laziness is great.
(fset 'yes-or-no-p 'y-or-n-p)


;;; Functions

;; lookup symbol under cursor
(defun ab/help-symbol-lookup()
  "Lookup the symbol under cursor."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-describe-thing-at-point)
    (describe-symbol (symbol-at-point))))

;; split window right and move to it
(defun ab/window-split-right ()
  "Split windows right and move to the new split"
  (interactive)
  (split-window-right)
  (windmove-right))

;; split window down and move to it
(defun ab/window-split-down ()
  "Split windows down and move to the new split"
  (interactive)
  (split-window-below)
  (windmove-down))

;;; eshell helper functions
(defun ab/eshell-insert-mode ()
  "jump to end of buffer i.e. the prompt before switching to insert mode"
  (interactive)
  (end-of-buffer)
  (end-of-line)
  (evil-insert-state))

;; new eshell
(defun ab/new-eshell ()
  "create a new eshell"
  (interactive)
  (if (projectile-project-root)
      (projectile-run-eshell 'N)
    (eshell 'N)))

;; new eshell in split
(defun ab/new-eshell-in-split ()
  "create a new eshell in new split.
if in eshell already; create a vertical split,
otherwise create a horizontal split"
  (interactive)
  (if (derived-mode-p 'eshell-mode)
      (ab/window-split-down)
    (ab/window-split-right))
  (ab/new-eshell))

;; customize eshell prompt
;; found at https://stackoverflow.com/a/59236830
(defun ab/eshell-prompt-function ()
  "use custom prompt for eshell that displays
current directly and lambda in new line"
  (setq eshell-prompt-regexp "^λ: ")
  (format "%s\nλ: " (abbreviate-file-name (eshell/pwd))))

;; close window on eshell exit
(defun ab/eshell-kill-window-on-exit ()
  "delete window when eshell is terminated
as long as the window is not the only window"
  (when (not (one-window-p))
    (delete-window)))

;; choose between eshell windows
(defun ab/select-or-create-eshell-name(name)
  "Select eshell by name or create a new eshell with name"
  (interactive)
  (if (string= name "new eshell")
      (ab/new-eshell)
    (switch-to-buffer name)))

;; select or create eshell, used by the prior function.
(defun ab/select-or-create-eshell()
  "Select or create eshell via a completing-read prompt"
  (interactive)
  (let* (
         (project-buffers (if (projectile-project-name)
                              (projectile-project-buffers)
                            (buffer-list)))
         (eshell-buffers
          (cl-remove-if-not (lambda (n)
                              (eq (buffer-local-value 'major-mode n) 'eshell-mode))
                            project-buffers))
         (eshell-buffer-names (mapcar 'buffer-name eshell-buffers))
         (empty (eq 0 (length eshell-buffer-names))))
    (if empty
        (ab/select-or-create-eshell-name "new eshell")
      (let (
            (selected-shell (completing-read "select eshell: " (cons "new eshell" eshell-buffer-names))))
        (ab/select-or-create-eshell-name selected-shell)))))

;; eshell is somehow notorious and refusing to release C-n.
;; binding only works by using hook
(defun bind-eshell-keys()
  (evil-define-key 'normal eshell-mode-map (kbd "C-n") nil)
  (evil-define-key 'normal eshell-mode-map (kbd "i") 'ab/eshell-insert-mode)
  (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-n") nil)
  (evil-collection-define-key 'insert 'eshell-mode-map (kbd "C-n") nil))

;; customize eshell mode
(defun ab/eshell-mode-config ()
  (bind-eshell-keys)
  (setq show-trailing-whitespace nil))

;; org-babel display results in new buffer
;; credit: https://emacs.stackexchange.com/a/27190
(defun ab/org-babel-to-buffer ()
  "A function to efficiently feed babel code block result to a separate buffer"
  (interactive)
  (org-open-at-point)
  (org-babel-remove-result)
  (with-current-buffer "*Org Babel Results*"
    (when (and
           (> (length (buffer-string)) 2)
           (string-prefix-p "{" (buffer-substring 1 2)))
      (json-mode))
    (evil-motion-state)))

;; custom configuration for org mode
(defun ab/org-mode-config ()
  "To use with `org-mode-hook'"
  (git-gutter-mode -1) ;; attempt to explicity disable git-gutter.
  (local-set-key (kbd "C-c C-S-c") 'ab/org-babel-to-buffer))
(add-hook 'org-mode-hook 'ab/org-mode-config)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(setq org-ellipsis " ⤵")

;; launch new emacs instance
(defun launch-new-emacs()
  "launch new emacs process.
it will be launched as a child process of this,
therefore closing this emacs will close all extra emacs.
"
  (interactive)
  (call-process-shell-command "emacs &"))

;; launch new debug emacs instance
(defun launch-new-emacs-with-debug()
  "launch new emacs process with --debug-init flag.
it will be launched as a child process of this,
therefore closing this emacs will close all extra emacs.
"
  (interactive)
  (call-process-shell-command "emacs --debug-init &"))

;; launch new vanilla emacs instance
(defun launch-new-vanilla-emacs()
  "launch new emacs process.
it will be launched as a child process of this,
therefore closing this emacs will close all extra emacs.
"
  (interactive)
  (call-process-shell-command "emacs -Q &"))

;; process management
(defun ab/start-command(&optional command)
  "Start command as a background process"

  (interactive)

  ;; prompt for command if missing
  (unless (bound-and-true-p command)
    (setq command (read-string "Command: ")))

  ;; exit if not a projectile project
  (unless (projectile-project-name) (error "not in a projectile project"))

  ;; initiate the list of running commands, if not set.
  (unless (bound-and-true-p ab/command-list)
    (setq ab/command-list (make-hash-table :test 'equal)))

  ;; start process
  (let ((process-name (ab/start-command--command-name)))
    (puthash process-name command ab/command-list)
    (with-current-buffer process-name
      (goto-char (point-min))
      (erase-buffer))
    (start-process-shell-command process-name process-name command)))

(defun ab/kill-command ()
  "Kill previously started command"
  (interactive)
  (kill-process (ab/start-command--command-name)))

(defun ab/restart-command ()
  "Restart the command"
  (interactive)
  (when (ab/kill-command)
    (ab/start-command (gethash (ab/start-command--command-name) ab/command-list))))

(defun ab/start-command--command-name ()
  "Get the command name, derived from projectile project name"
  (concat (projectile-project-name) ":cmd"))


;; Toggle Window Maximize
;; Credit: https://github.com/hlissner/doom-emacs/blob/59a6cb72be1d5f706590208d2ca5213f5a837deb/core/autoload/ui.el#L106
(defvar doom--maximize-last-wconf nil)
;;;###autoload
(defun doom/window-maximize-buffer ()
  "Close other windows to focus on this one. Activate again to undo this. If the
window changes before then, the undo expires.
Alternatively, use `doom/window-enlargen'."
  (interactive)
  (setq doom--maximize-last-wconf
        (if (and (null (cdr (cl-remove-if #'window-dedicated-p (window-list))))
                 doom--maximize-last-wconf)
            (ignore (set-window-configuration doom--maximize-last-wconf))
          (prog1 (current-window-configuration)
            (delete-other-windows)))))


;;; Package configs
;; boostrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)

      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; setup use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))


;;; Global
;; Theming/Appearance
(use-package doom-modeline
  :config
  (setq doom-modeline-icon nil)
  :hook (after-init . doom-modeline-mode))
(setq modus-themes-operandi-color-overrides
    '((bg-main . "#eeeeee")
      (bg-hl-line . "#dddddd")
      (bg-hl-alt . "#eaddd0")
      (bg-dim . "#e8e8e8")
      (bg-inactive . "#dedede")
      (fg-main . "#222222")))
(setq modus-themes-vivendi-theme-syntax 'faint)
;; tweak theme background a bit
(setq modus-themes-vivendi-color-overrides
    '((bg-main . "#222222")
      (fg-main . "#dddddd")
      (fg-dim . "#c0c6e0")
      (bg-dim . "#333333")
      (bg-alt . "#181732")
      (bg-hl-line . "#444444")))
(load-theme 'modus-vivendi t)

;; Show colons in modeline
(column-number-mode 1)
;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)


;;; Ivy/Counsel setup
;; flx for fuzzy and sorting
(use-package flx)

;; the ivy parent package
(use-package counsel
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function #'ignore)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  :config
  (ivy-mode 1))

;; other counsel key bindings
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)


;;; use 4 tab space
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


;;; Whitespaces
;; cleanup whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)
;; Show trailing white spaces
(setq-default show-trailing-whitespace t)


;;; customize eshell prompt
(setq eshell-prompt-function #'ab/eshell-prompt-function)
(setq comint-prompt-read-only t)
(setq eshell-scroll-to-bottom-on-input t)
;;; terminate eshell window on exit
(advice-add 'eshell-life-is-too-much :after 'ab/eshell-kill-window-on-exit)
;;; customize eshell mode with configs and custom keybindings
(add-hook 'eshell-mode-hook 'ab/eshell-mode-config)


;;; Projectile
(use-package projectile
  :init
  (setq projectile-require-project-root t)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-mode-line-prefix "P")
  ;; we mainly want projects defined by a few markers and we always want to take the top-most marker.
  ;; Reorder so other cases are secondary
  (setq projectile-project-root-files #'( ".projectile" "go.mod" "package.json" ))
  (setq projectile-project-root-functions #'(projectile-root-top-down
                                             projectile-root-top-down-recurring
                                             projectile-root-bottom-up
                                             projectile-root-local)))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;;; General life improvements to emacs

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Set locale to UTF8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Improving scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; PATH
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))


;;; Programming Languages
(use-package go-mode)
(use-package python-mode)
(use-package rust-mode)
(use-package web-mode)
(use-package vue-mode)
(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))
(use-package graphql-mode)
(use-package yaml-mode)
(use-package json-mode)


;; prettier
(use-package prettier-js
  :after (typescript-mode)
  :config
  (setq prettier-js-args '("--tab-width" "2" "--arrow-parens" "avoid"))
  :hook (typescript-mode . prettier-js-mode))

;; Language Server Protocol

;;; Yasnippets
(use-package yasnippet
  :config (yas-global-mode 1))
(use-package yasnippet-snippets)


;; Programming life improvements

;; display relative line numbers for programming modes
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; Auto-add parenthesis pair
(add-hook 'prog-mode-hook 'electric-pair-local-mode)

;; highlight current line
(global-hl-line-mode t)
;; focus help window
(setq help-window-select t)

;; multiple cursors
(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; Magit
;; getting errors until I installed magit-popup and with-editor
(use-package magit-popup ; make sure it is installed
  :demand t) ; make sure it is loaded
(use-package with-editor ; make sure it is installed
  :demand t) ; make sure it is loaded
(use-package magit
  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-diff-refine-ignore-whitespace nil))


;;; git gutter
(use-package git-gutter
  :init
  (setq git-gutter:disabled-modes '(org-mode))
  :config
  (global-git-gutter-mode 1))


;;; improve ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)


;;; improve dired
(setq dired-dwim-target t)


;;; force all popups to show as a real popup
(use-package popwin)
(popwin-mode 1)


;;; Undo
(use-package undo-tree
  :config
  (global-undo-tree-mode))


;;; EVIL mode
;; Configs that must be set before loading evil mode.
(setq evil-lookup-func #'ab/help-symbol-lookup)
(setq evil-toggle-key "C-z")
(setq evil-want-C-i-jump nil)
(setq evil-want-minibuffer t)
(setq evil-undo-system 'undo-tree)

;; change model texts
(setq evil-normal-state-tag "<NORMAL>")
(setq evil-insert-state-tag "<INSERT>")
(setq evil-visual-state-tag "<VISUAL>")
(setq evil-emacs-state-tag "<EMACS>")
(setq evil-motion-state-tag "<MOTION>")
(setq evil-replace-state-tag "<REPLACE>")
(setq evil-operator-state-tag "<OPERATOR>")
(setq evil-normal-state-message nil)
(setq evil-insert-state-message nil)
(setq evil-emacs-state-message nil)
(setq evil-visual-state-message nil)
(setq evil-motion-state-message nil)
(setq evil-replace-state-message nil)
(setq evil-operator-state-message nil)

;; setup package
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
;; evil everywhere
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;; free C-p for personal use except in emacs mode. I use it for file selection in other editors.
(define-key evil-normal-state-local-map (kbd "C-p") nil)
(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key evil-insert-state-map (kbd "C-p") nil)
(define-key evil-visual-state-map (kbd "C-p") nil)
(define-key evil-motion-state-map (kbd "C-p") nil)
(global-set-key (kbd "C-p") nil)
(global-set-key (kbd "<normal-state> C-p") nil)
;; free C-n for personal use except in emacs mode. my favourite tmux mapping
(define-key evil-normal-state-local-map (kbd "C-n") nil)
(define-key evil-normal-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-visual-state-map (kbd "C-n") nil)
(define-key evil-motion-state-map (kbd "C-n") nil)
(global-set-key (kbd "C-n") nil)
(global-set-key (kbd "<normal-state> C-n") nil)

;; key bindings
(define-key evil-normal-state-map (kbd ", SPC") 'evil-ex-nohighlight)
(define-key evil-normal-state-map (kbd ",cc") 'comment-line)
(define-key evil-visual-state-map (kbd ",cc") 'comment-line)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-visual-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file)
(define-key evil-insert-state-map (kbd "C-p") 'counsel-projectile-find-file)
;; exit motion state with `q`. it's mainly for viewing contents
(define-key evil-motion-state-map (kbd "q") 'kill-buffer-and-window)
;; always escape to normal mode including emacs mode
(define-key evil-emacs-state-map [escape] 'evil-normal-state)
;; always start in evil state https://github.com/noctuid/evil-guide#make-evil-normal-state-the-initial-state-always
(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)

;; specify cursor types for modes
;; insert mode disabled, therefore cursor not set
(use-package evil-terminal-cursor-changer
  :hook (after-init . (lambda()
                        (unless (display-graphic-p)
                          (evil-terminal-cursor-changer-activate))))
  :init
  (setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; |
  (setq evil-emacs-state-cursor  'bar)) ; |

;; prevent quitting emacs accidentally
(evil-ex-define-cmd "q[uit]" 'kill-buffer-and-window)


;; custom keybindings for window management. same as tmux
(global-set-key (kbd "C-n z") 'doom/window-maximize-buffer)
(global-set-key (kbd "C-n h") 'windmove-left)
(global-set-key (kbd "C-n H") 'windmove-swap-states-left)
(global-set-key (kbd "C-n l") 'windmove-right)
(global-set-key (kbd "C-n L") 'windmove-swap-states-right)
(global-set-key (kbd "C-n j") 'windmove-down)
(global-set-key (kbd "C-n J") 'windmove-swap-states-down)
(global-set-key (kbd "C-n k") 'windmove-up)
(global-set-key (kbd "C-n K") 'windmove-swap-states-up)
(global-set-key (kbd "C-n %") 'ab/window-split-right)
(global-set-key (kbd "C-n \"") 'ab/window-split-down)
(global-set-key (kbd "C-n x") 'evil-window-delete)
(global-set-key (kbd "C-n b b") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-n b n") 'projectile-next-project-buffer)
(global-set-key (kbd "C-n b p") 'projectile-previous-project-buffer)
(global-set-key (kbd "C-n b x") 'kill-buffer)
(global-set-key (kbd "C-n b r") 'rename-buffer)
(global-set-key (kbd "C-n :") 'counsel-M-x)
(global-set-key (kbd "C-n s") 'tab-bar-select-tab-by-name)
(global-set-key (kbd "C-n c") 'tab-new)
(global-set-key (kbd "C-n $") 'tab-rename)
(global-set-key (kbd "C-n n") 'tab-next)
(global-set-key (kbd "C-n p") 'tab-previous)
(global-set-key (kbd "C-n X") 'tab-close)
;; custom keybindings for code editing
(global-set-key (kbd "C-h .") 'ab/help-symbol-lookup)
(global-set-key (kbd "C-c f") 'counsel-projectile-find-file)
(global-set-key (kbd "C-c i") 'counsel-imenu)
(global-set-key (kbd "C-c ;") 'comment-line)
;; ivy/counsel bindings
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x :") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
(evil-define-key '(insert normal) ivy-minibuffer-map
  (kbd "C-j") 'ivy-next-line
  (kbd "C-k") 'ivy-previous-line)
;; eshell terminal bindings
(global-set-key (kbd "C-`") 'ab/new-eshell-in-split)
(global-set-key (kbd "C-~") 'ab/select-or-create-eshell)
;; process management
(global-set-key (kbd "C-n r") 'ab/restart-command)

;; autocomplete
(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  :config
  (global-company-mode 1))
;; quickhelp for autocomplete
(use-package company-quickhelp
  :init
  (setq company-quickhelp-delay 1.0)
  :config
  (company-quickhelp-mode 1))
;; bind keys to company mode
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (evil-define-key 'insert company-active-map (kbd "<return>") #'company-complete)
  (define-key company-active-map (kbd "<return>") #'company-complete))


;;; rest client
;; restclient package
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode))
  :bind (:map restclient-mode-map
              ("C-c C-f" . json-mode-beautify)))
;; autocomplete in restclient
(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

;; counsel-jq for real time jq json filter
(use-package counsel-jq
  :config
  (with-eval-after-load "json-mode"
    (define-key json-mode-map (kbd "C-c C-j") #'counsel-jq)
    (define-key json-mode-map (kbd "C-c C-f") #'json-mode-beautify)))

;;; auto tail log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))


;;; org babel
;; no need to confirm evaluation for all languages, some are harmless
(setq org-confirm-babel-evaluate nil)
;; other org useful configs
(setq org-src-fontify-natively t)
(setq org-adapt-indentation nil)
(setq org-log-done 'time)
(setq org-log-done-with-time t)
(setq org-imenu-depth 7)
;; improve visibility with indentations.
(setq org-startup-indented t)
;;; improve bullets
(use-package org-superstar
  :after org
  :hook (org-mode . (lambda () (org-superstar-mode 1))))
(setq inhibit-compacting-font-caches t) ; fix slowdowns
;;; add non-core languages
;; typescript
(use-package ob-typescript)
;; go
(use-package ob-go
  :straight (ob-go :type git :host github :repo "pope/ob-go"))
;; http
(use-package ob-http)
;; restclient
(use-package ob-restclient)
;; graphql
(use-package ob-graphql)
;;; agenda
(setq org-agenda-files (list
                        "~/org-agenda"))
(setq org-default-notes-file "~/org-agenda/notes.org")
(setq org-default-tasks-file "~/org-agenda/tasks.org")
(setq org-todo-keywords '((sequence "TODO(t)" "PREPARING(p)" "STARTED(s)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-todo-keyword-faces '(("DONE" :foreground "forest green" :strike-through nil :weight normal)
                               ("CANCELLED" :strike-through nil :weight normal)))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
;;; org code executions
(setq org-babel-python-command "python3")
(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell . t)
                               (python . t)
                               (js . t)
                               (typescript . t)
                               (go . t)
                               (org . t)
                               (emacs-lisp . t)
                               (http . t)
                               (restclient . t)
                               (java . t)
                               (makefile . t)
                               (sql . t)
                               (awk . t)
                               (graphql . t)))
;; org roam
(use-package org-roam)

;; other org keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c s") 'org-store-link)
(global-set-key (kbd "C-c t") 'org-tags-view)
;; exit edit mode with 'C-c C-c' to stay consistent with other places like magit
(eval-after-load 'org-src
  '(define-key org-src-mode-map
     (kbd "C-c C-c") #'org-edit-src-exit))

;;; Docker
(use-package docker
  :bind ("C-c d" . docker))
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile.*\\'" . dockerfile-mode)))

;;; startup
(setq inhibit-startup-screen +1)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)


;; set font again
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "BlinkMacSystemFont" :height 170 :weight normal))))
 '(fixed-pitch ((t ( :family "JetBrains Mono" :height 150))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:inherit (shadow fixed-pitch)))))
 '(org-block-end-line ((t (:inherit (shadow fixed-pitch)))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 ;; '(org-document-info ((t (:foreground "dark orange"))))
 '(org-date ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-footnote ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
