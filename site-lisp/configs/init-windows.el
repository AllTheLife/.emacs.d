;;; -*- lexical-binding: t -*-

(require 'popper)
(require 'popper-echo)
(autoload 'ace-window "ace-window")
(autoload 'ace-swap-window "ace-window")


(winner-mode t)

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "除非提供 `ARG', 否则拆分此窗口并切换到新窗口."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(defun toggle-delete-other-windows ()
  "删除掉 frame 中的其他窗口(如果有), 不然恢复以前的窗口配置."
  (interactive)
  (if (and winner-mode
	   (equal (length (window-list)) 1))
      (winner-undo)
    (delete-other-windows)))

(defun my-split-window-vertically ()
  (interactive)
  (funcall 'split-window-vertically)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (select-window target-window)))

(defun my-split-window-horizontally ()
  (interactive)
  (funcall 'split-window-horizontally)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (select-window target-window)))

(defun my-split-window-right ()
  (interactive)
  (split-window-right)
  (select-window (next-window)))

(defun my-split-window-below ()
  (interactive)
  (split-window-below)
  (select-window (next-window)))

;; (global-set-key (kbd "C-x 1") 'toggle-delete-other-windows)
;; (global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
;; (global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(setq popper-group-function #'popper-group-by-directory)
(setq popper-echo-dispatch-actions t)
(setq popper-reference-buffers
        '("\\*Messages\\*$"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "^\\*eldoc.*\\*$"
          "\\*Compile-Log\\*$"
          "\\*Completions\\*$"
          "\\*Warnings\\*$"
          "\\*Async Shell Command\\*$"
          "\\*Apropos\\*$"
          "\\*Backtrace\\*$"
          "\\*Calendar\\*$"
          "\\*Fd\\*$" "\\*Find\\*$" "\\*Finder\\*$"
          "\\*Kill Ring\\*$"
          "\\*Embark \\(Collect\\|Live\\):.*\\*$"

          bookmark-bmenu-mode
          comint-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          flymake-diagnostics-buffer-mode
          flycheck-error-list-mode flycheck-verify-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*Process List\\*$" process-menu-mode
          list-environment-mode cargo-process-mode

          "^\\*.*eshell.*\\*.*$"
          "^\\*.*shell.*\\*.*$"
          "^\\*.*terminal.*\\*.*$"
          "^\\*.*vterm[inal]*.*\\*.*$"

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\**"
          "\\*diff-hl\\**"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-.+\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode))
(with-no-warnings
    (defun my-popper-fit-window-height (win)
      "Determine the height of popup window WIN by fitting it to the buffer's content."
      (fit-window-to-buffer
       win
       (floor (frame-height) 3)
       (floor (frame-height) 3)))
    (setq popper-window-height #'my-popper-fit-window-height)

    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p))
                 popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist)))
         (when (window-live-p window)
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack))

(popper-mode t)


(provide 'init-windows)
