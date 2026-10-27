;;; init.el --- Minimal Emacs 31 configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Vanilla Emacs with custom colours, frame-scoped buffers, Magit and Ghostel.

;;; Code:

;;; Interface

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

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

;;; Languages

;; Install missing tree-sitter grammars on first use.
(require 'treesit)
(when (treesit-available-p)
  (setopt treesit-auto-install-grammar 'always
          treesit-enabled-modes t))

;;; Packages

;; Language-aware Magit diffs.
(setq magit-diff-fontify-hunk 'all
      magit-diff-specify-hunk-foreground nil
      magit-diff-use-indicator-faces t)

(require 'package)
;; Use MELPA for Ghostel and NonGNU ELPA for Magit.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-pinned-packages '(beframe . "gnu"))
(add-to-list 'package-pinned-packages '(magit . "nongnu"))
(add-to-list 'package-pinned-packages '(ghostel . "melpa"))
(package-initialize)

;; Download Ghostel's native module on first use.
(setq ghostel-module-auto-install 'download)

(dolist (pkg '(beframe magit ghostel))
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
        (format "%s installation failed: %s. Retry with M-x package-refresh-contents, then M-x package-install RET %s."
                pkg (error-message-string err) pkg))))))

;;; Frame buffers

(when (require 'beframe nil t)
  (beframe-mode 1))

;;; Key bindings

(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-c t") #'ghostel)

;;; init.el ends here
