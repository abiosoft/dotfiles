;;; Minimal UI
;;; Code: configuration
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
;; menubar is actually useful
;; (menu-bar-mode   -1)

;;; Functions
(defun my-lookup()
  "Lookup the symbol under cursor."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-describe-thing-at-point)
    (describe-symbol (symbol-at-point))))


;;; Font
;; Set default font
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-14"))
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 160))
;; disable bold
(mapc
 (lambda (face)
   (when (eq (face-attribute face :weight) 'bold)
     (set-face-attribute face nil :weight 'normal)))
 (face-list))


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
  :hook (after-init . doom-modeline-mode))
(use-package modus-operandi-theme
  :ensure t)
(use-package modus-vivendi-theme
   :ensure t
   :config
   (setq modus-vivendi-theme-syntax 'faint)
   ;; tweak theme background a bit
   (setq modus-vivendi-theme-override-colors-alist
            '(("bg-main" . "#222222")
              ("bg-dim" . "#333333")
              ("bg-alt" . "#181732")
              ("bg-hl-line" . "#333333"))))
(load-theme 'modus-operandi t)

;; Show colons in modeline
(column-number-mode 1)
;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)


;;; EVIL mode
;; Configs that must be set before loading evil mode.
(setq evil-want-C-i-jump nil)
(setq evil-lookup-func #'my-lookup)
(setq evil-toggle-key "C-,")

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
  :config
  (evil-mode 1))

;; key bindings
(define-key evil-normal-state-map (kbd ", SPC") 'evil-ex-nohighlight)
(define-key evil-normal-state-map (kbd ",cc") 'comment-line)
(define-key evil-visual-state-map (kbd ",cc") 'comment-line)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
;; always escape to normal mode including emacs mode
(define-key evil-emacs-state-map [escape] 'evil-normal-state)
;; temporary execution in emacs state
;; (define-key evil-normal-state-map (kbd "i") 'evil-emacs-state)
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
  (setq evil-emacs-state-cursor  'bar)) ; |

;; Disable insert mode, emacs is better at handling the insert
;; This needs to come after other evil configs
(defalias 'evil-insert-state 'evil-emacs-state)

;;; Ivy/Counsel setup
(use-package counsel
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function #'ignore)
  :config
  (ivy-mode 1))
;; other counsel key bindings
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x :") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


;;; VTerm
(use-package vterm
  :ensure t)
;; (add-to-list 'load-path "~/projects/emacs/emacs-libvterm")
;; (require 'vterm)
;; (unless (display-graphic-p)
;;   (define-key vterm-mode-map [up]    '(lambda () (interactive) (vterm-send-key "<up>")))
;;   (define-key vterm-mode-map [down]  '(lambda () (interactive) (vterm-send-key "<down>")))
;;   (define-key vterm-mode-map [right] '(lambda () (interactive) (vterm-send-key "<right>")))
;;   (define-key vterm-mode-map [left]  '(lambda () (interactive) (vterm-send-key "<left>")))
;;   (define-key vterm-mode-map [tab]   '(lambda () (interactive) (vterm-send-key "<tab>")))
;;   (define-key vterm-mode-map (kbd "DEL") '(lambda () (interactive) (vterm-send-key "<backspace>")))
;;   (define-key vterm-mode-map (kbd "RET") '(lambda () (interactive) (vterm-send-key "<return>"))))


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
(setq indent-line-function 'insert-tab)


;;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  (setq projectile--mode-line "P"))
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

;; autocomplete
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  :config
  (global-company-mode 1))
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))


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
(use-package flycheck
  :ensure t)
;; Language Server Protocol
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-auto-configure t)
  (setq lsp-enable-snippet nil)
  :hook
  (go-mode . lsp)
  (rust-mode . lsp)
  (python-mode . lsp)
  (before-save . 'lsp-format-buffer)
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-use-webkit nil)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-modeline-diagnostics-enable t)
  :commands lsp-ui-mode)
(add-hook 'lsp-ui-doc-frame-hook
          (lambda (frame _w)
            (set-face-attribute 'default frame :font "Jetbrains Mono")))
(use-package company-lsp
  :ensure t
  :commands company-lsp)
(use-package lsp-ivy
  :ensure t)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)
(use-package dap-mode
  :ensure t)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'lsp-mode-hook 'flycheck-mode)


;;; Text Editing visual
;; disable blinking cursor
(blink-cursor-mode 0)

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
  :ensure t)


;;; git gutter
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))


;;; improve ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-vertically)


;;; smooth scrolling like vim and other editors
(use-package smooth-scrolling
  :ensure t
  :init
  (setq smooth-scroll-margin 5)
  :config
  (smooth-scrolling-mode 1))


;;; force all popups to show as a real popup
(use-package popwin
  :ensure t)
(popwin-mode 1)


;;; Custom keybindings
(global-set-key (kbd "C-h i") 'my-lookup)
(global-set-key (kbd "C-h i") 'my-lookup)
 
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7b3ce93a17ce4fc6389bba8ecb9fee9a1e4e01027a5f3532cc47d160fe303d5a" "7e22a8dcf2adcd8b330eab2ed6023fa20ba3b17704d4b186fa9c53f1fab3d4d2" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
