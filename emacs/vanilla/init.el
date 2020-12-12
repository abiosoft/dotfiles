;;; Minimal UI
;;; Code: configuration
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
;; menubar is actually useful in GUI
(unless (display-graphic-p)
  (menu-bar-mode   -1))


;;; Functions
(defun my-lookup()
  "Lookup the symbol under cursor."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-describe-thing-at-point)
    (describe-symbol (symbol-at-point))))
;; Custom keybindings
;; Functions
(defun split-right ()
  "Split windows right and move to the new split"
  (interactive)
  (split-window-right)
  (windmove-right))
(defun split-down ()
  "Split windows down and move to the new split"
  (interactive)
  (split-window-below)
  (windmove-down))


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

;; Show colons in modeline
(column-number-mode 1)
;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)


;;; Ivy/Counsel setup
(use-package flx
  :ensure t)
(use-package counsel
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function #'ignore)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :config
  (ivy-mode 1))
;; other counsel key bindings
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
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
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)


;;; VTerm
(use-package vterm
  :ensure t)
;; (add-to-list 'load-path "~/projects/emacs/emacs-libvterm")
;; (require 'vterm)

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
  (setq projectile--mode-line "P")
  ;; we mainly want projects defined by a few markers and we always want to take the top-most marker.
    ;; Reorder so other cases are secondary
  (setq projectile-project-root-files #'( ".projectile" ))
  (setq projectile-project-root-files-functions #'(projectile-root-top-down
                                               projectile-root-top-down-recurring
                                               projectile-root-bottom-up
                                               projectile-root-local)))

;; fix to let eglot find project dir in mono repos
(defun my-projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1)
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'my-projectile-project-find-function)))
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
;; Language Server Protocol
(use-package eglot
  :ensure t)
;; eglot temporary fix
(defun project-root (project)
  (car (project-roots project)))
(add-hook 'go-mode-hook 'eglot-ensure)

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
  (setq magit-diff-refine-ignore-whitespace nil)
  :ensure t)


;;; git gutter
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))


;;; improve ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-vertically)


;;; improve dired
(setq dired-dwim-target t)

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


;;; rest client
(use-package restclient
  :ensure t)

;;; EVIL mode
;; Configs that must be set before loading evil mode.
(setq evil-lookup-func #'my-lookup)
(setq evil-toggle-key "C-z")

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

;; free C-n for personal use. my favourite tmux mapping
(define-key evil-normal-state-local-map (kbd "C-n") nil)
(define-key evil-normal-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-visual-state-map (kbd "C-n") nil)
(define-key evil-emacs-state-map (kbd "C-n") nil)
(define-key evil-motion-state-map (kbd "C-n") nil)
(global-set-key (kbd "C-n") nil)
(global-set-key (kbd "<normal-state> C-n") nil)


;; eshell is somehow notorious and refusing to release C-n
;; binding only works by using hook
(defun bind-eshell-keys()
  (evil-define-key 'normal eshell-mode-map (kbd "C-n") nil)
  (evil-collection-define-key 'normal 'eshell-mode-map (kbd "C-n") nil))
(add-hook 'eshell-mode-hook 'bind-eshell-keys)


;; key bindings
(define-key evil-normal-state-map (kbd ", SPC") 'evil-ex-nohighlight)
(define-key evil-normal-state-map (kbd ",cc") 'comment-line)
(define-key evil-visual-state-map (kbd ",cc") 'comment-line)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
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
  (setq evil-emacs-state-cursor  'bar)) ; |

;; custom keybinding for code navigation in evil modes
;; same as my vim bindings
;; (map! :n "M-g" #'lsp-ivy-workspace-symbol
;;       :nv ",cc" #'comment-line
;;       :nv ",O" #'neotree-toggle
;;       :nv ",o" #'+neotree/find-this-file
;;       :n ",f" #'lsp-format-buffer
;;       :nv "TAB" #'+workspace/switch-right
;;       :nv "<backtab>" #'+workspace/switch-left
;;       :n ", SPC" #'evil-ex-nohighlight)
;; other custom vim bindings
;; (global-set-key (kbd "M-p") '+ivy/projectile-find-file)
;; (global-set-key (kbd "M-u") 'evil-scroll-up)
;; (global-set-key (kbd "M-d") 'evil-scroll-down)
;; (global-set-key (kbd "M-v") 'evil-visual-block)


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
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map [tab] nil)
  (define-key company-active-map [return] #'company-complete-common-or-cycle))


(use-package perspective
  :ensure t
  :bind (("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*)))
(persp-mode)

;; custom keybinding for window management
;; same as my tmux bindings
(global-set-key (kbd "C-n z") 'doom/window-maximize-buffer)
(global-set-key (kbd "C-n h") 'evil-window-left)
(global-set-key (kbd "C-n H") 'evil-window-move-far-left)
(global-set-key (kbd "C-n l") 'evil-window-right)
(global-set-key (kbd "C-n L") 'evil-window-move-far-right)
(global-set-key (kbd "C-n j") 'evil-window-down)
(global-set-key (kbd "C-n J") 'evil-window-move-very-bottom)
(global-set-key (kbd "C-n k") 'evil-window-up)
(global-set-key (kbd "C-n K") 'evil-window-move-very-top)
(global-set-key (kbd "C-n %") 'split-right)
(global-set-key (kbd "C-n \"") 'split-down)
(global-set-key (kbd "C-n x") 'evil-window-delete)
(global-set-key (kbd "C-n b") 'persp-ivy-switch-buffer)
(global-set-key (kbd "C-n :") 'counsel-M-x)
(global-set-key (kbd "C-n s") 'persp-switch)
(global-set-key (kbd "C-n c") (lambda () (interactive) (persp-switch "new-unnamed")))
(global-set-key (kbd "C-n $") 'persp-rename)
(global-set-key (kbd "C-n n") 'persp-next)
(global-set-key (kbd "C-n p") 'persp-prev)
(global-set-key (kbd "C-n X") 'persp-kill)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "e72f5955ec6d8585b8ddb2accc2a4cb78d28629483ef3dcfee00ef3745e2292f" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "7b3ce93a17ce4fc6389bba8ecb9fee9a1e4e01027a5f3532cc47d160fe303d5a" "7e22a8dcf2adcd8b330eab2ed6023fa20ba3b17704d4b186fa9c53f1fab3d4d2" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "JetBrains Mono")))))
