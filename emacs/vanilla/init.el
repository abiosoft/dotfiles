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

;; add custom scripts to path
(add-to-list 'load-path (expand-file-name "~/dotfiles/emacs/vanilla/libs"))

;;; Functions
(defun ab/help-symbol-lookup()
  "Lookup the symbol under cursor."
  (interactive)
  (describe-symbol (symbol-at-point)))
(defun ab/window-split-right ()
  "Split windows right and move to the new split"
  (interactive)
  (split-window-right)
  (windmove-right))
(defun ab/window-split-down ()
  "Split windows down and move to the new split"
  (interactive)
  (split-window-below)
  (windmove-down))
;; Create new workspace
(defun ab/workspace-new()
  "Create a new workspace using perspective.el"
  (interactive)
  (persp-switch "new workspace"))
;; tslint fix
;; not used but leaving it for future references e.g. syntax for shell command
(defun ab/tslint-fix-file ()
  "Apply tslint --fix to the current file"
  (interactive)
  (message "tslint --fixing the file" (buffer-file-name))
  (call-process-shell-command (concat "tslint --fix --config " (projectile-project-root) "tslint.json " (buffer-file-name) " &") nil 0))
  ;; (revert-buffer t t))
;;; eshell helper functions
(defun ab/eshell-insert-mode ()
  "jump to end of buffer i.e. the prompt before switching to insert mode"
  (interactive)
  (end-of-buffer)
  (end-of-line)
  (evil-insert-state))
(defun ab/new-eshell ()
  "create a new eshell"
  (interactive)
  (if (projectile-project-root)
      (projectile-run-eshell 'N)
    (eshell 'N)))
(defun ab/new-eshell-in-split ()
  "create a new eshell in new split.
if in eshell already create a vertical split,
otherwise create a horizontal split"
  (interactive)
  (if (derived-mode-p 'eshell-mode)
      (ab/window-split-down)
    (ab/window-split-right))
  (ab/new-eshell))
;;; found at https://stackoverflow.com/a/59236830
(defun ab/eshell-prompt-function ()
  "use custom prompt for eshell that displays
current directly and lambda in new line"
  (setq eshell-prompt-regexp "^λ: ")
  (format "%s\nλ: " (abbreviate-file-name (eshell/pwd))))
(defun ab/eshell-kill-window-on-exit ()
  "delete window when eshell is terminated
as long as the window is not the only window"
  (when (not (one-window-p))
    (delete-window)))

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

;;; Font
;; Set default font
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 140 ;; this is 1/10 pt e.g. 140 = 14pt
                    :weight 'normal
                    :width 'normal)

;;; Package configs
;; boostrap straight.el
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
;; setup use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))


;;; Global
;; Theming/Appearance
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-icon nil)
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-persp-name t)
  :hook (after-init . doom-modeline-mode))
(use-package modus-operandi-theme
  :ensure t
  :config
  (setq modus-operandi-theme-override-colors-alist
        '(("bg-main" . "#eeeeee")
          ("bg-hl-line" . "#dddddd")
          ("bg-hl-alt" . "#eaddd0")
          ("bg-dim" . "#e8e8e8")
          ("bg-inactive" . "#dedede")
          ("fg-main" . "#222222"))))
(use-package modus-vivendi-theme
  :ensure t
  :config
  (setq modus-vivendi-theme-syntax 'faint)
  ;; tweak theme background a bit
  (setq modus-vivendi-theme-override-colors-alist
        '(("bg-main" . "#222222")
          ("fg-main" . "#eeeeee")
          ("bg-dim" . "#333333")
          ("bg-alt" . "#181732")
          ("bg-hl-line" . "#444444"))))
(load-theme 'modus-vivendi t)
;; org mode theme
(require 'org-beautify-theme)
(load-theme 'org-beautify t)


;; Show colons in modeline
(column-number-mode 1)
;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)


;;; Ivy/Counsel setup
;; flx for fuzzy and sorting
(use-package flx
  :ensure t)

;; the ivy parent package
(use-package counsel
  :ensure t
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


;;; use 4 tab space
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


;;; customize eshell prompt
(setq eshell-prompt-function #'ab/eshell-prompt-function)
(setq comint-prompt-read-only t)
(setq eshell-scroll-to-bottom-on-input t)
;;; terminate eshell window on exit
(advice-add 'eshell-life-is-too-much :after 'ab/eshell-kill-window-on-exit)

;;; Projectile
(use-package projectile
  :ensure t
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
  :ensure t
  :config
  (counsel-projectile-mode 1))
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; PATH
(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))


;;; Programming Languages
(use-package go-mode
  :ensure t)
(use-package python-mode
  :ensure t)
(use-package rust-mode
  :ensure t)
(use-package web-mode
  :ensure t)
(use-package vue-mode
  :ensure t)
(use-package typescript-mode
  :ensure t)
(use-package graphql-mode
  :ensure t)

;; prettier
(use-package prettier-js
  :ensure t
  :after (typescript-mode)
  :config
  (setq prettier-js-args '("--tab-width" "2" "--arrow-parens" "avoid"))
  :hook (typescript-mode . prettier-js-mode))

;; Language Server Protocol

;; if you want to change prefix for lsp-mode keybindings.
(setq lsp-keymap-prefix "C-c l")
(use-package lsp-mode
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
            (go-mode . lsp)
            (typescript-mode . lsp)
            ;; if you want which-key integration
            (lsp-mode . lsp-enable-which-key-integration))
    :config
    (setq lsp-completion-provider :capf)
    (setq lsp-idle-delay 0.500)
    (setq lsp-ui-doc-enable nil)
    (setq lsp-lens-enable nil)
    (setq lsp-headerline-breadcrumb-enable nil)
    (setq lsp-signature-render-documentation nil)
    :commands lsp)

;;; Yasnippets
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)


;; display line numbers for programming modes
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; highlight current line
(global-hl-line-mode t)
;; focus help window
(setq help-window-select t)


;;; Magit
;; getting errors until I installed magit-popup and with-editor
(use-package magit-popup
  :ensure t ; make sure it is installed
  :demand t) ; make sure it is loaded
(use-package with-editor
  :ensure t ; make sure it is installed
  :demand t) ; make sure it is loaded
(use-package magit
  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-diff-refine-ignore-whitespace nil)
  :ensure t)


;;; git gutter
(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:disabled-modes '(org-mode org-indent-mode))
  (global-git-gutter-mode +1))


;;; improve ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)


;;; improve dired
(setq dired-dwim-target t)


;;; force all popups to show as a real popup
(use-package popwin
  :ensure t)
(popwin-mode 1)


;; workspace
(use-package perspective
  :ensure t
  :config
  (setq persp-show-modestring nil)
  (persp-mode))


;;; EVIL mode
;; Configs that must be set before loading evil mode.
(setq evil-lookup-func #'ab/help-symbol-lookup)
(setq evil-toggle-key "C-z")
(setq evil-want-C-i-jump nil)

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
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
;; evil everywhere
(use-package evil-collection
  :after evil
  :ensure t
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
;; eshell is somehow notorious and refusing to release C-n
;; binding only works by using hook
(defun bind-eshell-keys()
  (evil-define-key 'normal eshell-mode-map (kbd "C-n") nil)
  (evil-define-key 'normal eshell-mode-map (kbd "i") 'ab/eshell-insert-mode)
  (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-n") nil)
  (evil-collection-define-key 'insert 'eshell-mode-map (kbd "C-n") nil))
(add-hook 'eshell-mode-hook 'bind-eshell-keys)

;; key bindings
(define-key evil-normal-state-map (kbd ", SPC") 'evil-ex-nohighlight)
(define-key evil-normal-state-map (kbd ",cc") 'comment-line)
(define-key evil-visual-state-map (kbd ",cc") 'comment-line)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-visual-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "C-g") 'counsel-imenu)
(define-key evil-insert-state-map (kbd "C-g") 'counsel-imenu)
(define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file)
(define-key evil-insert-state-map (kbd "C-p") 'counsel-projectile-find-file)
;; always escape to normal mode including emacs mode
(define-key evil-emacs-state-map [escape] 'evil-normal-state)
;; always start in evil state https://github.com/noctuid/evil-guide#make-evil-normal-state-the-initial-state-always
(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)

;; specify cursor types for modes
;; insert mode disabled, therefore cursor not set
(use-package evil-terminal-cursor-changer
  :ensure t
  :hook (after-init . (lambda()
                        (unless (display-graphic-p)
                          (evil-terminal-cursor-changer-activate))))
  :init
  (setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; |
  (setq evil-emacs-state-cursor  'bar)) ; |



;; custom keybinding for window management
;; follow emacs convention as much as possible
(global-set-key (kbd "C-x 1") 'doom/window-maximize-buffer)
(global-set-key (kbd "C-x 3") 'ab/window-split-right)
(global-set-key (kbd "C-x 2") 'ab/window-split-down)
(global-set-key (kbd "C-x b") 'persp-ivy-switch-buffer)
(global-set-key (kbd "C-x k") 'persp-kill-buffer*)
(global-set-key (kbd "C-x :") 'counsel-M-x)
(global-set-key (kbd "C-x p .") 'persp-switch)
(global-set-key (kbd "C-x p 2") 'ab/workspace-new)
(global-set-key (kbd "C-x p r") 'persp-rename)
(global-set-key (kbd "C-x p o") 'persp-next)
(global-set-key (kbd "C-x p 0") 'persp-kill)
;; custom keybindings for window management. same as tmux
(global-set-key (kbd "C-n z") 'doom/window-maximize-buffer)
(global-set-key (kbd "C-n h") 'evil-window-left)
(global-set-key (kbd "C-n H") 'evil-window-move-far-left)
(global-set-key (kbd "C-n l") 'evil-window-right)
(global-set-key (kbd "C-n L") 'evil-window-move-far-right)
(global-set-key (kbd "C-n j") 'evil-window-down)
(global-set-key (kbd "C-n J") 'evil-window-move-very-bottom)
(global-set-key (kbd "C-n k") 'evil-window-up)
(global-set-key (kbd "C-n K") 'evil-window-move-very-top)
(global-set-key (kbd "C-n %") 'ab/window-split-right)
(global-set-key (kbd "C-n \"") 'ab/window-split-down)
(global-set-key (kbd "C-n x") 'evil-window-delete)
(global-set-key (kbd "C-n b b") 'persp-ivy-switch-buffer)
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
;; ivy/counsel bindings
(global-set-key (kbd "C-x :") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
;; eshell terminal bindings
(global-set-key (kbd "C-`") 'ab/new-eshell-in-split)
(global-set-key (kbd "C-~") 'ab/new-eshell)

;; autocomplete
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  :config
  (global-company-mode 1))
;; quickhelp for autocomplete
(use-package company-quickhelp
  :ensure t
  :init
  (setq company-quickhelp-delay 1.0)
  :config
  (company-quickhelp-mode 1))
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (evil-define-key 'insert company-active-map (kbd "<return>") #'company-complete)
  (define-key company-active-map (kbd "<return>") #'company-complete))


;;; rest client
;; optional deps
(use-package json-mode
  :ensure t)
(use-package jq-mode
  :ensure t
  :config
  (with-eval-after-load "json-mode"
    (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively)))
;; restclient package
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode))
  :bind (:map restclient-mode-map
	          ("C-c C-f" . json-mode-beautify)))
;; autocomplete in restclient
(use-package company-restclient
  :ensure t
  :config
  (add-to-list 'company-backends 'company-restclient))


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
;; improve visibility with indentations.
(setq org-startup-indented t)
;;; improve bullets
(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . (lambda () (org-superstar-mode 1))))
;;; add non-core languages
;; typescript
(use-package ob-typescript
  :ensure t)
;; go
(use-package ob-go
  :ensure t
  :straight (ob-go :type git :host github :repo "pope/ob-go"))
;; http
(use-package ob-http
  :ensure t)
;; restclient
(use-package ob-restclient
  :ensure t)
;; graphql
(use-package ob-graphql
  :ensure t)
;;; agenda
(setq org-agenda-files (list
                        "~/org-agenda"))
(setq org-default-notes-file "~/org-agenda/notes.org")
(setq org-default-tasks-file "~/org-agenda/tasks.org")
(setq org-todo-keywords '((sequence "TODO(t)" "PREPARING(p)" "STARTED(s)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-todo-keyword-faces '(("DONE" :foreground "#44bd43" :strike-through nil)
                               ("CANCELLED" :foreground "#ff6480" :strike-through nil)))
(use-package evil-org
  :ensure t
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
;; other org keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c t") 'org-tags-view)
;; exit edit mode with 'C-c C-c' to stay consistent with other places like magit
(eval-after-load 'org-src
  '(define-key org-src-mode-map
     (kbd "C-c C-c") #'org-edit-src-exit))

;;; fine tuning performance
;;; gotten from https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2528fd9e96d9e95db04aee15c558b752d4716ebd622a4367ba7128d0fa8618e7" "31deed4ac5d0b65dc051a1da3611ef52411490b2b6e7c2058c13c7190f7e199b" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "e72f5955ec6d8585b8ddb2accc2a4cb78d28629483ef3dcfee00ef3745e2292f" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "7b3ce93a17ce4fc6389bba8ecb9fee9a1e4e01027a5f3532cc47d160fe303d5a" "7e22a8dcf2adcd8b330eab2ed6023fa20ba3b17704d4b186fa9c53f1fab3d4d2" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "JetBrains Mono")))))
