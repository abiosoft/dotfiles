;;; early-init.el --- macOS native compiler compatibility -*- lexical-binding: t; -*-

;; Keep asynchronous native compiler diagnostics in the log without pop-ups.
(setq native-comp-async-report-warnings-errors 'silent)

;; Work around the bundled GCC's incorrect macOS 27 deployment target.
(when (and (eq system-type 'darwin) (native-comp-available-p))
  (require 'comp)
  (add-to-list 'native-comp-driver-options "-mmacosx-version-min=11.0"))

;;; early-init.el ends here
