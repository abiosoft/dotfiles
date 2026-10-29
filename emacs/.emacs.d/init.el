;;; init.el --- Minimal Emacs 31 configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Vanilla Emacs with custom colours, frame-scoped buffers, Magit and Ghostel.

;;; Code:

;;; Interface

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(which-key-mode 1)

;; Relative line numbers in text and code buffers.
(setq-default display-line-numbers-type 'relative)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Command as Control and Option as Meta for both macOS ports.
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'control
        ns-option-modifier 'meta
        mac-command-modifier 'control
        mac-option-modifier 'meta))

;;; Theme

(require-theme 'modus-themes)
(setq modus-operandi-palette-overrides
      '((bg-main "#eeeeee")
        (fg-main "#222222")
        (bg-dim "#e8e8e8")
        (bg-inactive "#dedede")
        (bg-hl-line "#dddddd")
        (bg-hover-secondary "#eaddd0"))
      modus-vivendi-palette-overrides
      '((bg-main "#121212")
        (fg-main "#dddddd")
        (fg-dim "#c0c6e0")
        (bg-dim "#232323")
        (bg-hl-line "#444444")
        (bg-prose-block-contents "#181732")
        (bg-prose-block-delimiter "#181732")
        ;; Faint syntax colours.
        (builtin maroon)
        (comment fg-dim)
        (constant blue-faint)
        (docstring fg-alt)
        (docmarkup magenta-faint)
        (fnname pink)
        (keyword indigo)
        (preprocessor rust)
        (string slate)
        (type cyan-faint)
        (variable cyan-faint)
        (rx-construct gold)
        (rx-backslash olive)))
(load-theme 'modus-vivendi t)

;;; Fonts: JetBrains Mono, 15 pt

(dolist (face '(default fixed-pitch))
  (set-face-attribute face nil :family "JetBrains Mono" :height 150
                      :weight 'normal :width 'normal))

;;; Mode line

(setq display-time-format "%I:%M %p"
      display-time-default-load-average nil)
(display-time-mode 1)

;; Identify remote buffers.
(add-to-list
 'mode-line-misc-info
 '(:eval
   (when-let* ((remote (file-remote-p default-directory)))
     (concat " (T " (substring remote 1 -1) ")"))))

;;; Indentation

(setq-default tab-width 4
              indent-tabs-mode nil)
(setq go-ts-indent-offset 4)

;; Go modes prefer tabs by default; use spaces while editing.
(dolist (hook '(go-mode-hook go-ts-mode-hook go-mod-ts-mode-hook go-work-ts-mode-hook))
  (add-hook hook (lambda () (setq-local indent-tabs-mode nil))))

;;; Languages

;; Install missing tree-sitter grammars on first use.
(require 'treesit)
(when (treesit-available-p)
  (setopt treesit-auto-install-grammar 'always
          treesit-enabled-modes t))

;; Built-in LSP client; start once per project with M-x eglot.
(require 'eglot)

(defun ab/eglot-format-on-save ()
  "Format Eglot buffers when the server supports it."
  (when (and (eglot-managed-p)
             (eglot-server-capable :documentFormattingProvider))
    (eglot-format-buffer)))

(add-hook 'before-save-hook #'ab/eglot-format-on-save)

;;; Code navigation

(require 'xref)

;; Ctrl-click follows definitions; clicking a definition shows references.
(defun ab/xref-at-mouse (event)
  "Find the clicked symbol's definition, or references when already there."
  (interactive "e")
  (mouse-set-point event)
  (let ((buffer (current-buffer))
        (bounds (bounds-of-thing-at-point 'symbol)))
    (xref-find-definitions-at-mouse event)
    (when (and bounds
               (eq buffer (current-buffer))
               (<= (car bounds) (point))
               (< (point) (cdr bounds)))
      (xref-find-references-at-mouse event))))

(keymap-set xref-mouse-mode-map "C-<mouse-1>" #'ab/xref-at-mouse)
(add-hook 'prog-mode-hook #'xref-mouse-mode)

;;; Packages

(require 'package)
;; Use MELPA for Ghostel and NonGNU ELPA for Magit.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-pinned-packages '(beframe . "gnu"))
(add-to-list 'package-pinned-packages '(corfu . "gnu"))
(add-to-list 'package-pinned-packages '(diff-hl . "gnu"))
(add-to-list 'package-pinned-packages '(magit . "nongnu"))
(add-to-list 'package-pinned-packages '(ghostel . "melpa"))
(package-initialize)

(dolist (pkg '(beframe corfu diff-hl exec-path-from-shell magit ghostel))
  (add-to-list 'package-selected-packages pkg)
  (unless (package-installed-p pkg)
    (condition-case err
        (progn
          (unless (assq pkg package-archive-contents)
            (package-refresh-contents))
          (package-install pkg))
      (error
       (display-warning
        'init
        (format "%s: %s. Refresh archives, then M-x package-install RET %s."
                pkg (error-message-string err) pkg))))))

;;; Completion

(fido-vertical-mode 1)
(setq completion-ignore-case t)
(global-completion-preview-mode -1)
(setq-default tab-always-indent 'complete)

(when (require 'corfu nil t)
  (setq corfu-auto t
        corfu-preselect 'first
        corfu-preview-current nil)
  (global-corfu-mode 1))

;;; Git

;; Language-aware Magit diffs.
(setq magit-diff-fontify-hunk 'all
      magit-diff-specify-hunk-foreground nil
      magit-diff-use-indicator-faces t)

;; Open Magit in same window
(setq magit-display-buffer-function
      #'magit-display-buffer-same-window-except-diff-v1)

;; Show horizontal splits for ediff
(setq ediff-split-window-function #'split-window-horizontally)

;; Use same frame for ediff
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; Show changed lines in the gutter, including unsaved local edits.
(when (require 'diff-hl nil t)
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;;; Shell environment

;; Import PATH and exec-path from the interactive login shell.
(when (require 'exec-path-from-shell nil t)
  (exec-path-from-shell-initialize))

;; Include the remote login PATH and common user-installed tools.
(with-eval-after-load 'tramp
  (dolist (path '(tramp-own-remote-path "~/go/bin" "~/.cargo/bin"
                 "~/.local/bin" "~/bin"))
    (add-to-list 'tramp-remote-path path t)))

;;; Frame buffers

(when (require 'beframe nil t)
  (beframe-mode 1))

;;; Terminal

;; Download Ghostel's native module on first use.
(setq ghostel-module-auto-install 'download)

;; Render Ghostel's bold text at normal weight.
(custom-set-faces
 '(ghostel-bold ((t (:inherit ansi-color-bold :weight normal)))))

;;; Key bindings

(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-c t") #'ghostel-project)
(global-set-key (kbd "C-c T") #'ghostel-project-new)

;;; init.el ends here
