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
  (if (bound-and-true-p lsp-mode)
      (lsp-format-buffer)
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
(add-to-list 'default-frame-alist '(font . "Inconsolata-17"))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))
(set-default-font "Inconsolata 17")

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

;; Vim mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package evil-escape
  :ensure t
  :init
  (setq-default evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode 1))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tomorrow-night t))
;; Global settings (defaults)
(setq
  doom-themes-enable-bold nil   ; if nil, bold is universally disabled
  doom-themes-enable-italic t)  ; if nil, italics is universally disabled
;; modeline theme
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
  :init (setq doom-modeline-height 20)
(set-face-attribute 'mode-line nil
   :background "#353535")
(set-face-attribute 'mode-line-inactive nil
   :background "#131313")

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
            (define-key helm-map (kbd "C-z") 'helm-select-action)
            (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)
            (define-key helm-buffer-map (kbd "ESC") 'helm-keyboard-quit)))
            ;; (define-key helm-M-x-map (kbd "ESC") 'helm-keyboard-quit)
            ;; (define-key helm-major-mode-map (kbd "ESC") 'helm-keyboard-quit)
            ;; (define-key helm-find-files-map (kbd "ESC") 'helm-keyboard-quit)
            ;; (define-key helm-etags-map (kbd "ESC") 'helm-keyboard-quit)
            ;; (define-key helm-imenu-map (kbd "ESC") 'helm-keyboard-quit)
            ;; (define-key helm-locate-map (kbd "ESC") 'helm-keyboard-quit)
            ;; (define-key helm-pdfgrep-map (kbd "ESC") 'helm-keyboard-quit)))

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
  :config (general-define-key
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
   ;; "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
    "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
    "SPC" '(helm-M-x :which-key "M-x")
    "ff"  '(helm-find-files :which-key "find files")
    "pf"  '(helm-projectile-find-file :which-key "find project files")
    "pp"  '(helm-projectile-switch-project :which-key "choose project")
    ;; Buffers
    "bb"  '(helm-buffers-list :which-key "buffers list")
    ;; Window
    "wl"  '(windmove-right :which-key "move right")
    "wh"  '(windmove-left :which-key "move left")
    "wk"  '(windmove-up :which-key "move up")
    "wj"  '(windmove-down :which-key "move down")
    "w/"  '(split-right :which-key "split right")
    "w-"  '(split-down :which-key "split down")
    "wx"  '(delete-window :which-key "delete window")
    ;; Others
    "at"  '(ansi-term :which-key "open terminal")
    ;; Git
    "g"   '(magit-status :which-key "magit status")
    ;; Editing
    "cc" '(comment-or-uncomment-region :which-key "comment code")
    "cf" '(my-lsp-format :which-key "format code")
    ))

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
    (setq mac-right-option-modifier 'none) ;; unbind right key for accented input

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
  :config
  (projectile-mode 1)
  (projectile-global-mode))
(use-package helm-projectile
  :ensure t)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; PATH
(use-package exec-path-from-shell
  :ensure t
  :init
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
(define-key evil-normal-state-map (kbd "K") 'my-lsp-help)
(define-key evil-normal-state-map (kbd "J") 'close-help-window)
(define-key evil-normal-state-map (kbd "C-g") 'helm-imenu)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)
;; Modify navigation to close help
(define-key evil-normal-state-map (kbd "h") 'move-backward)
(define-key evil-normal-state-map (kbd "j") 'move-down)
(define-key evil-normal-state-map (kbd "k") 'move-up)
(define-key evil-normal-state-map (kbd "l") 'move-forward)

;; git
(use-package magit
  :ensure t)

;; disable blinking cursor
(blink-cursor-mode 0)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (helm-projectile flycheck lsp-ui web-mode dap-mode lsp-treemacs helm-lsp company-lsp lsp-mode sh-mode html-mode typescript-mode vue-mode javascript-mode rust-mode python-mode go-mode magit company smart-mode-line-powerline smart-mode-line-powerline-theme exec-path-from-shell projectile general which-key helm doom-themes evil-escape evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
