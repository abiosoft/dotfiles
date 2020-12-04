;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Helvetica Neue" :size 16))
;; disable bold font
(setq doom-themes-enable-bold nil)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi)
(setq modus-vivendi-theme-syntax 'faint)
;; tweak theme background a bit
(setq modus-vivendi-theme-override-colors-alist
            '(("bg-main" . "#151515")
              ("bg-dim" . "#222222")
              ("bg-alt" . "#181732")
              ("bg-hl-line" . "#333333")))

; Set cursor color to grey when doom theme is disabled
;; (set-cursor-color "#555555")
;; (add-to-list 'default-frame-alist '(cursor-color . "#555555"))
;; (custom-set-faces! '(default :cursor-color "#555555"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type 'nil)
(setq display-line-numbers-type 'relative)
;; (setq display-line-numbers 'relative)
;; (setq display-line-numbers-width 3)
;; turn line numbers off for GUI, so slow
;; (if (display-graphic-p)
;;     (setq display-line-numbers-type 'nil)
;;   (setq display-line-numbers-type 'relative))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; key bindings for macos
(setq mac-command-modifier      'meta
      ns-command-modifier       'meta
      mac-option-modifier       'control
      ns-option-modifier        'control
      mac-right-option-modifier 'none
      ns-right-option-modifier  'none)

;; fix golang lsp
;; FIXME Fix for emacs 27
;; https://github.com/emacs-lsp/lsp-mode/issues/1778
(setq lsp-gopls-codelens nil)

;; autocomplete delay
(setq company-idle-delay 0)

;; custom keybinding for code navigation in evil modes
;; same as my vim bindings
(map! :n "M-g" #'lsp-ivy-workspace-symbol
      :nv ",cc" #'comment-line
      :nv ",O" #'neotree-toggle
      :nv ",o" #'+neotree/find-this-file
      :n ",f" #'lsp-format-buffer
      :nv "TAB" #'+workspace/switch-right
      :nv "<backtab>" #'+workspace/switch-left
      :n ", SPC" #'evil-ex-nohighlight)
;; other custom vim bindings
(map! "M-p" #'+ivy/projectile-find-file)
(map! "M-u" #'evil-scroll-up)
(map! "M-d" #'evil-scroll-down)
(map! "M-v" #'evil-visual-block)

;; custom keybinding for window management
;; same as my tmux bindings
(map! "M-n z" #'doom/window-maximize-buffer)
(map! "M-n s" #'+workspace/switch-to)
(map! "M-n h" #'evil-window-left)
(map! "M-n H" #'evil-window-move-far-left)
(map! "M-n l" #'evil-window-right)
(map! "M-n L" #'evil-window-move-far-right)
(map! "M-n j" #'evil-window-down)
(map! "M-n J" #'evil-window-move-very-bottom)
(map! "M-n k" #'evil-window-up)
(map! "M-n K" #'evil-window-move-very-top)
(map! "M-n %" #'evil-window-vsplit)
(map! "M-n \"" #'evil-window-split)
(map! "M-n x" #'evil-window-delete)
(map! "M-n $" #'+workspace/rename)
(map! "M-n n" #'+workspace/switch-right)
(map! "M-n p" #'+workspace/switch-left)
(map! "M-n c" #'+workspace/new)
(map! "M-n b b" #'+ivy/switch-workspace-buffer)
(map! "M-n b d" #'kill-current-buffer)
(map! "M-n :" #'counsel-M-x)
