;;; init.el --- Minimal Emacs 31 configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Vanilla Emacs with custom colours, Org, frame/tab-local buffers, Magit and Ghostel.

;;; Code:

;;; Packages

(require 'package)
;; Prefer GNU and NonGNU ELPA; use MELPA for Ghostel and Org-roam.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-pinned-packages '(bufferlo . "gnu"))
(add-to-list 'package-pinned-packages '(corfu . "gnu"))
(add-to-list 'package-pinned-packages '(diff-hl . "gnu"))
(add-to-list 'package-pinned-packages '(htmlize . "nongnu"))
(add-to-list 'package-pinned-packages '(magit . "nongnu"))
(add-to-list 'package-pinned-packages '(multiple-cursors . "nongnu"))
(add-to-list 'package-pinned-packages '(org-superstar . "nongnu"))
(add-to-list 'package-pinned-packages '(ghostel . "melpa"))
(add-to-list 'package-pinned-packages '(org-roam . "melpa"))
(package-initialize)

(dolist (pkg '(bufferlo corfu diff-hl exec-path-from-shell ghostel htmlize
               magit multiple-cursors org-roam org-superstar with-editor))
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

;;; Shell environment

;; Import PATH and exec-path from the interactive login shell.
(when (require 'exec-path-from-shell nil t)
  (exec-path-from-shell-initialize))

;; Include the remote login PATH and common user-installed tools.
(with-eval-after-load 'tramp
  (dolist (path '(tramp-own-remote-path "~/go/bin" "~/.cargo/bin"
                 "~/.local/bin" "~/bin"))
    (add-to-list 'tramp-remote-path path t)))

;;; Interface

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Enable mouse wheel scrolls
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; Enable which key
(which-key-mode 1)

;; Relative line numbers in text and code buffers.
(setq-default display-line-numbers-type 'relative
              display-line-numbers-current-absolute t)
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
;; Keep code, tables and other spacing-sensitive text monospaced in Org.
(setq modus-themes-mixed-fonts t)
(load-theme 'modus-vivendi t)

;;; Font: JetBrains Mono, 15 pt

(dolist (face '(default fixed-pitch variable-pitch))
  (set-face-attribute face nil :family "JetBrains Mono" :height 150
                      :weight 'normal :width 'normal))

;;; Mode line

(set-face-attribute 'mode-line-buffer-id nil :weight 'normal)

(setq display-time-format " %I:%M %p"
      display-time-default-load-average nil)
(display-time-mode 1)

;; Identify remote buffers.
(add-to-list
 'mode-line-misc-info
 '(:eval
   (when-let* ((remote (file-remote-p default-directory)))
     (concat " (T " (substring remote 1 -1) ") "))))

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

;;; Completion

(fido-vertical-mode 1)
(setq completion-ignore-case t)
(global-completion-preview-mode -1)
(setq-default tab-always-indent t)

;; Reserve Tab completion for programming buffers.
(defun ab/prog-completion-setup ()
  "Allow Tab to complete after indenting in programming buffers."
  (setq-local tab-always-indent 'complete))

(add-hook 'prog-mode-hook #'ab/prog-completion-setup)

(when (require 'corfu nil t)
  (setq corfu-auto t
        corfu-preselect 'first
        corfu-preview-current nil)
  (add-hook 'prog-mode-hook #'corfu-mode))

;;; Org

(require 'org)
;; Babel uses Org's default language support (Emacs Lisp).
(require 'ob)
;; Expand <s TAB into source blocks.
(require 'org-tempo)

;; A spacious outline with circles and stars.
(setq org-startup-indented t
      org-indent-indentation-per-level 2
      org-hide-emphasis-markers nil
      org-ellipsis " ⤵"
      org-pretty-entities t
      org-fontify-whole-heading-line t
      org-fontify-quote-and-verse-blocks t
      org-src-fontify-natively t)

(when (require 'org-superstar nil t)
  (setq org-superstar-headline-bullets-list '(?◉ ?○ ?✸ ?✿ ?◆ ?▸)
        org-superstar-item-bullet-alist '((?+ . ?◦) (?- . ?•) (?* . ?✦))
        ;; Org Indent supplies the spacing before each heading's final star.
        org-superstar-leading-bullet ?\s
        org-superstar-special-todo-items nil)
  (add-hook 'org-mode-hook #'org-superstar-mode))

(defun ab/org-setup ()
  "Give Org buffers a spacious appearance using the current theme's colours."
  (display-line-numbers-mode -1)
  (visual-line-mode 1)
  ;; Use 18 pt for Org text while retaining the shared JetBrains Mono font.
  (buffer-face-set '(:height 180))
  (setq-local line-spacing 0.18)
  ;; Buffer-local typography preserves the theme's colours and other modes.
  (dolist (face '(variable-pitch fixed-pitch))
    (face-remap-set-base face 'default))
  ;; Keep headings and other Org faces at the enlarged body size.
  (dolist (face '(org-ellipsis org-document-info
                 org-level-1 org-level-2 org-level-3 org-level-4
                 org-level-5 org-level-6 org-level-7 org-level-8))
    (face-remap-add-relative face :height 1.0))
  (face-remap-add-relative 'org-document-title :height 1.3 :weight 'bold)
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                 org-level-6 org-level-7 org-level-8))
    (face-remap-add-relative face :weight 'normal))
  ;; Keep metadata, TODO keywords, timestamps and code blocks at 15 pt.
  (dolist (face '(org-document-info-keyword org-meta-line org-special-keyword
                 org-drawer org-property-value
                 org-todo org-done org-date
                 org-block org-block-begin-line org-block-end-line))
    (face-remap-add-relative face :height 150))
  (dolist (face '(org-special-keyword org-date))
    (face-remap-add-relative face :weight 'normal :underline nil :box nil)))

(add-hook 'org-mode-hook #'ab/org-setup)

;;; Org export

(require 'ox-md)

(setq org-export-with-section-numbers nil)

(with-eval-after-load 'ox-html
  ;; Inline syntax colours come from the active Emacs theme via htmlize.
  (setq org-html-htmlize-output-type 'inline-css
        org-html-doctype "html5"
        org-html-html5-fancy t
        ;; Clicking a heading updates the URL with its section anchor.
        org-html-self-link-headlines t
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        org-html-validation-link nil
        ;; Embed the stylesheet so exported documents can be shared directly.
        org-html-head-extra
        (concat "<style>\n"
                (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "org-html.css" user-emacs-directory))
                  (buffer-string))
                "\n</style>")))

;;; Org capture and agenda

(setq org-directory (expand-file-name "~/org/"))

(defvar ab/org-notes-inbox (expand-file-name "notes/inbox.org" org-directory)
  "File for captured notes.")

(defvar ab/org-tasks-inbox (expand-file-name "tasks/inbox.org" org-directory)
  "File for captured tasks.")

;; Create the inbox directories and files without changing existing contents.
(dolist (file (list ab/org-notes-inbox ab/org-tasks-inbox))
  (make-directory (file-name-directory file) t)
  (unless (file-exists-p file)
    (with-temp-file file)))

(setq org-default-notes-file ab/org-notes-inbox
      ;; Include .org files directly inside notes/ and tasks/.
      org-agenda-files (list (expand-file-name "notes/" org-directory)
                             (expand-file-name "tasks/" org-directory))
      org-capture-templates
      `(("n" "Note" entry (file ,ab/org-notes-inbox)
         "* %?\n  %U\n  %a\n")
        ("t" "Task" entry (file ,ab/org-tasks-inbox)
         "* TODO %?\n  %U\n  %a\n")))

(defun ab/org-open-notes-inbox ()
  "Open the notes inbox."
  (interactive)
  (find-file ab/org-notes-inbox))

(defun ab/org-open-tasks-inbox ()
  "Open the tasks inbox."
  (interactive)
  (find-file ab/org-tasks-inbox))

(defun ab/org-open-directory ()
  "Open the Org directory in Dired."
  (interactive)
  (dired org-directory))

;;; Org-roam

(when (require 'org-roam nil t)
  (setq org-roam-directory (expand-file-name "roam/" org-directory)
        org-roam-completion-everywhere t)
  (make-directory org-roam-directory t)
  ;; Org expects this cache on first capture; nil is a valid empty cache.
  (unless (file-exists-p org-id-locations-file)
    (with-temp-file org-id-locations-file (insert "nil\n")))
  (org-roam-db-autosync-mode 1))

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

;;; Terminal

;; Download Ghostel's native module on first use.
(setq ghostel-module-auto-install 'download)
(setq ghostel-tramp-shell-integration t)

(add-hook 'ghostel-pre-spawn-hook #'with-editor-export-editor)

(defun ab/ghostel-home ()
  "Create a new Ghostel terminal in the home directory."
  (interactive)
  (let ((default-directory (expand-file-name "~/")))
    (ghostel-new)))

(defun ab/ghostel-directory (directory)
  "Create a new Ghostel terminal in DIRECTORY."
  (interactive (list (read-directory-name "Ghostel directory: " nil nil t)))
  (let ((default-directory (file-name-as-directory
                            (expand-file-name directory))))
    (ghostel-new)))

;; Render Ghostel's bold text at normal weight.
(custom-set-faces
 '(ghostel-bold ((t (:inherit ansi-color-bold :weight normal)))))

;;; Frame and tab buffers

(setq tab-bar-new-tab-to 'rightmost)
(tab-bar-mode 1)

(when (require 'bufferlo nil t)
  (bufferlo-mode 1)
  ;; Keep a local scratch buffer, but show a new terminal at home in new tabs.
  (setq tab-bar-new-tab-choice #'bufferlo-create-local-scratch-buffer)
  (defun ab/new-tab-home-directory (_tab)
    "Start Ghostel at home after Bufferlo initializes a new tab."
    (when (eq tab-bar-new-tab-choice #'bufferlo-create-local-scratch-buffer)
      (ab/ghostel-home)))
  (add-hook 'tab-bar-tab-post-open-functions #'ab/new-tab-home-directory t)
  ;; Clean up the closing tab, preserving buffers shared with other tabs.
  (add-hook 'tab-bar-tab-pre-close-functions
            (lambda (tab only-tab-p)
              (unless (and only-tab-p (null tab-bar-close-last-tab-choice))
                (bufferlo-kill-buffers
                 nil nil (seq-position (funcall tab-bar-tabs-function) tab #'eq))))
            t)
  ;; Let the built-in switch-to-buffer use local completion too.
  (setq bufferlo-anywhere-filter
        (remq 'switch-to-buffer bufferlo-anywhere-filter))
  ;; Use local buffers by default in commands that read buffer names.
  (bufferlo-anywhere-mode 1))

;; Open a new terminal in a chosen directory from the tab line's + button.
(setq tab-line-new-tab-choice
      (lambda () (call-interactively #'ab/ghostel-directory)))
(global-tab-line-mode 1)

;;; Key bindings

;; Suggested multiple-cursors bindings; C-c C-< is reserved by Org.
(global-set-key (kbd "C->") #'mc/mark-next-like-this)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c o") #'ab/org-open-notes-inbox)
(global-set-key (kbd "C-c i") #'ab/org-open-tasks-inbox)
(global-set-key (kbd "C-c d") #'ab/org-open-directory)

(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n c") #'org-roam-capture)
(global-set-key (kbd "C-c n b") #'org-roam-buffer-toggle)

(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-c t") #'ab/ghostel-home)
(global-set-key (kbd "C-c T") #'ab/ghostel-directory)

;;; Server

(require 'server)
(unless (server-running-p)
  (server-start))

;;; init.el ends here
