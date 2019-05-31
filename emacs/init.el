
;; Functions
(defun my-lsp-describe-thing-at-point ()
  "Display the full documentation of the thing at point."
  (interactive)
  (let ((contents (-some->> (lsp--text-document-position-params)
                            (lsp--make-request "textDocument/hover")
                            (lsp--send-request)
                            (gethash "contents"))))
    (if (and contents (not (equal contents "")) )
        (pop-to-buffer
         (with-current-buffer (get-buffer-create "*Help*")
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (lsp--render-on-hover-content contents t))
             (goto-char (point-min))
             (view-mode t)
             (current-buffer))))
      (lsp--info "No content at point."))))

(defun my-lsp-help()
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (let ((sw (selected-window)))
        (my-lsp-describe-thing-at-point)
        (evil-window-move-very-bottom)
        (select-window sw))
    (describe-function (symbol-at-point)))
  (move-help-window-down))

(defun move-help-window-down()
  (interactive)
  (let ((sw (selected-window))
        (hw (get-buffer-window "*Help*")))
    (select-window hw)
    (evil-window-move-very-bottom)
    (select-window sw)))

(defun my-lsp-format ()
  (interactive)
  (delete-trailing-whitespace)
  (if (bound-and-true-p lsp-mode)
      (progn
        (lsp-format-buffer)
        (lsp-organize-imports))
    (indent-region (region-beginning) (region-end))))

(defun close-help-window ()
  (interactive)
  (let ((w (get-buffer-window "*Help*")))
    (if w (delete-window w))))

(defun move-forward ()
  (interactive)
  (forward-char)
  (close-help-window))

(defun move-backward ()
  (interactive)
  (backward-char)
  (close-help-window))

(defun move-up ()
  (interactive)
  (evil-previous-line)
  (close-help-window))

(defun move-down ()
  (interactive)
  (evil-next-line)
  (close-help-window))

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Font
;; Set default font
(add-to-list 'default-frame-alist '(font . "SF Mono-16"))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))
;; disable bold
(mapc
 (lambda (face)
   (when (eq (face-attribute face :weight) 'bold)
     (set-face-attribute face nil :weight 'normal)))
 (face-list))

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Custom keybindings
;; Functions
(defun split-right ()
  (interactive)
  (split-window-right)
  (windmove-right))
(defun split-down ()
  (interactive)
  (split-window-below)
  (windmove-down))
;;; Global
(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :global-prefix "C-b"
   :prefix "C-w"
   :non-normal-prefix "C-b"
   ;; "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
   ;; direct keys
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "C-b" '(helm-M-x :which-key "M-x")
   "f"  '(helm-find-files :which-key "find files")
   "F"  '(helm-projectile-find-file :which-key "find project files")
   "P"  '(helm-projectile-switch-project :which-key "choose project")
   ;; Window
   "l"  '(windmove-right :which-key "move right")
   "L"  '(evil-window-move-far-right :which-key "move right")
   "h"  '(windmove-left :which-key "move left")
   "H"  '(evil-window-move-far-left :which-key "move left")
   "k"  '(windmove-up :which-key "move up")
   "K"  '(evil-window-move-very-top :which-key "move up")
   "j"  '(windmove-down :which-key "move down")
   "J"  '(evil-window-move-very-bottom :which-key "move down")
   "%"  '(split-right :which-key "split right")
   "\""  '(split-down :which-key "split down")
   "d"  '(delete-window :which-key "delete window")
   "o"  '(delete-other-windows :which-key "delete other windows")
   ;; Terminal
   "`"  '(vterm :which-key "open terminal")
   ;; Editing
   "c" '(:ignore t :which-key "code actions")
   "cc" '(comment-line :which-key "comment code")
   "cf" '(my-lsp-format :which-key "format code")
   "cs" '(helm-imenu :which-key "code outline")
   ;; Workspace
   "n"  '(eyebrowse-next-window-config :which-key "next workspace")
   "p"  '(eyebrowse-prev-window-config :which-key "prev workspace")
   "x"  '(eyebrowse-close-window-config :which-key "delete workspace")
   "$"  '(eyebrowse-rename-window-config :which-key "rename workspace")
   "w" '(:ignore t :which-key "workspace actions")
   "wc" '(eyebrowse-create-window-config :which-key "create workspace")
   "wn" '(eyebrowse-next-window-config :which-key "next workspace")
   "wp" '(eyebrowse-prev-window-config :which-key "prev workspace")
   ;; Buffers
   "b"  '(helm-buffers-list :which-key "buffers list")
   "wb" '(:ignore t :which-key "buffer actions")
   "wbb"  '(helm-buffers-list :which-key "buffers list")
   "wbd" '(kill-current-buffer :which-key "kill buffer")
   "wbx" '(kill-buffer-and-window :which-key "kill buffer+window")
   "wbr" '(rename-buffer :which-key "rename buffer")
   ))

;; Theming/Appearance
;; Functions
(defun fix-doom-dark-theme ()
  (interactive)
  (setq
   doom-themes-enable-bold nil   ; if nil, bold is universally disabled
   doom-themes-enable-italic t)  ; if nil, italics is universally disabled
  (set-face-attribute 'mode-line nil
                      :background "#353535"
                      :foreground "grey"
                      :weight 'normal)
  (set-face-attribute 'mode-line-inactive nil
                      :background "#131313"
                      :weight 'normal)
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground "grey"
                      :weight 'normal))
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tomorrow-night t)
  (fix-doom-dark-theme))
;; Show colons in modeline
(column-number-mode 1)
;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Vim mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "K") 'my-lsp-help)
  (define-key evil-normal-state-map (kbd "J") 'lsp-ui-sideline-toggle-symbols-info)
  ;; Modify navigation to close help
  (define-key evil-normal-state-map (kbd "h") 'move-backward)
  (define-key evil-normal-state-map (kbd "j") 'move-down)
  (define-key evil-normal-state-map (kbd "k") 'move-up)
  (define-key evil-normal-state-map (kbd "l") 'move-forward)
  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up))

(use-package evil-escape
  :ensure t
  :init
  (setq-default evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode 1))

;; terminal insert mode cursor fix
(use-package evil-terminal-cursor-changer
  :ensure t
  :hook (after-init . (lambda()
                        (unless (display-graphic-p)
                          (evil-terminal-cursor-changer-activate))))
  :init
  (setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; ⎸
  (setq evil-emacs-state-cursor  'hbar))

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-candidate-number-list 150
        helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
  :config
  (helm-mode 1))

(add-hook 'helm-after-initialize-hook
          (lambda()
            (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
            (define-key helm-map (kbd "C-z") 'helm-select-action)))

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-wrap-around t)
  :config
  (eyebrowse-mode t))

;; VTerm
(add-to-list 'load-path "~/projects/emacs/emacs-libvterm")
(require 'vterm)
(unless (display-graphic-p)
  (define-key vterm-mode-map [up]    '(lambda () (interactive) (vterm-send-key "<up>")))
  (define-key vterm-mode-map [down]  '(lambda () (interactive) (vterm-send-key "<down>")))
  (define-key vterm-mode-map [right] '(lambda () (interactive) (vterm-send-key "<right>")))
  (define-key vterm-mode-map [left]  '(lambda () (interactive) (vterm-send-key "<left>")))
  (define-key vterm-mode-map [tab]   '(lambda () (interactive) (vterm-send-key "<tab>")))
  (define-key vterm-mode-map (kbd "DEL") '(lambda () (interactive) (vterm-send-key "<backspace>")))
  (define-key vterm-mode-map (kbd "RET") '(lambda () (interactive) (vterm-send-key "<return>"))))


;; macOS specific
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
    (global-set-key (kbd "<H-backspace>") 'delete-forward-char)
    ))

;; use 4 tab space
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'helm)
  (setq projectile-mode-line "Projectile")
  (setq projectile--mode-line "Projectile")
  :config
  (projectile-mode 1)
  (projectile-global-mode))
(use-package helm-projectile
  :ensure t)

;; Disable backup files
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

;; Programming Languages
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
  (before-save . my-lsp-format)
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-doc-enable nil)
  :commands lsp-ui-mode)
(use-package company-lsp
  :ensure t
  :commands company-lsp)
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)
(use-package dap-mode
  :ensure t)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'lsp-mode-hook 'flycheck-mode)

;; disable blinking cursor
(blink-cursor-mode 0)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; git gutter
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
        (quote
         ("7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" default)))
 '(package-selected-packages
        (quote
         (git-gutter eyebrowse yaml-mode which-key web-mode vue-mode use-package typescript-mode telephone-line spacemacs-theme spaceline smart-mode-line-powerline-theme rust-mode python-mode magit lsp-ui lsp-treemacs helm-projectile helm-lsp go-mode general flycheck exec-path-from-shell evil-terminal-cursor-changer evil-escape doom-themes doom-modeline dap-mode cycle-themes company-lsp color-theme-sanityinc-tomorrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
