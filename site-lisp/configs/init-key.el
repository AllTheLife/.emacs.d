(require 'lazy-load)


;;; 增强 minibuffer

(lazy-load-local-keys
 '(("C-<backspace>" . vertico-directory-up))
 minibuffer-local-map
 "vertico-directory")

(lazy-load-global-keys
 '(([remap switch-to-buffer] . consult-buffer)
   ([remap goto-line] . consult-goto-line)
   ("C-c l" . consult-line)
   ("M-y" . consult-yank-pop)
   ("C-c g" . consult-grep))
 "consult")


;;; 绑定快捷跳转
(lazy-load-global-keys
 '(("C-." . remember-init)
   ("C-," .  remember-jump))
 "init-utils")


;;; 关闭当前 buffer
(global-set-key (kbd "C-;") 'kill-this-buffer)


;;; 管理 Tab-bar
(lazy-load-set-keys
 '(("C-c t r" . tab-bar-rename-tab)
   ("C-c t i" . tab-bar-new-tab)
   ("C-c t x" . tab-bar-close-tab)
   ("C-c t n" . tab-bar-switch-to-next-tab)
   ("C-c t e" . tab-bar-switch-to-prev-tab)))


;;; 结构化编辑
(defvar fingertip-key-alist nil)
(setq fingertip-key-alist
      '(
        ;; 移动
        ("M-p" . fingertip-jump-left)
        ("M-n" . fingertip-jump-right)
        ;; 符号插入
        ("%" . fingertip-match-paren)            ;括号跳转
        ("(" . fingertip-open-round)             ;智能 (
        ("[" . fingertip-open-bracket)           ;智能 [
        ("{" . fingertip-open-curly)             ;智能 {
        (")" . fingertip-close-round)            ;智能 )
        ("]" . fingertip-close-bracket)          ;智能 ]
        ("}" . fingertip-close-curly)            ;智能 }
        ("（" . fingertip-open-chinese-round)    ;智能 （
        ("「" . fingertip-open-chinese-bracket)  ;智能 「
        ("【" . fingertip-open-chinese-curly)    ;智能 【
        ("）" . fingertip-close-chinese-round)   ;智能 ）
        ("」" . fingertip-close-chinese-bracket) ;智能 」
        ("】" . fingertip-close-chinese-curly)   ;智能 】
        ("\"" . fingertip-double-quote)          ;智能 "
        ("'" . fingertip-single-quote)           ;智能 '
        ("=" . fingertip-equal)                  ;智能 =
        ("SPC" . fingertip-space)                ;智能 space
        ("RET" . fingertip-newline)              ;智能 newline
        ;; 删除
        ("M-o" . fingertip-backward-delete) ;向后删除
        ("C-d" . fingertip-forward-delete)  ;向前删除
        ("C-k" . fingertip-kill)            ;向前kill
        ;; 包围
        ("M-\"" . fingertip-wrap-double-quote) ;用 " " 包围对象, 或跳出字符串
        ("M-'" . fingertip-wrap-single-quote) ;用 ' ' 包围对象, 或跳出字符串
        ("M-[" . fingertip-wrap-bracket)      ;用 [ ] 包围对象
        ("M-{" . fingertip-wrap-curly)        ;用 { } 包围对象
        ("M-(" . fingertip-wrap-round)        ;用 ( ) 包围对象
        ("M-)" . fingertip-unwrap)            ;去掉包围对象
        ;; 跳出并换行缩进
        ("C-j" . fingertip-jump-out-pair-and-newline)
	))
(lazy-load-set-keys fingertip-key-alist fingertip-mode-map)


;;; lsp-bridge: 自动补全
(lazy-load-global-keys
 '(("e" . lsp-bridge-popup-documentation)
   ("d" . lsp-bridge-find-def)
   ("D" . lsp-bridge-find-def-return)
   ("n" . lsp-bridge-find-references)
   ("r" . lsp-bridge-rename)
   ("a" . lsp-bridge-code-action)
   ("s" . lsp-bridge-toggle-sdcv-helper)
   ("p" . lsp-bridge-peek)
   ("v" . lsp-bridge-avy-peek)
   ("t" . lsp-bridge-peek-through)
   ("i n" . lsp-bridge-diagnostic-jump-next)
   ("i p" . lsp-bridge-diagnostic-jump-prev)
   ("i l" . lsp-bridge-diagnostic-list)
   ("i i" . lsp-bridge-diagnostic-ignore))
 "init-lsp-bridge"
 "C-c d")


;;; dirvish: 文件浏览器
(lazy-load-global-keys
 '(("d" . dirvish)
   ("s" . dirvish-side))
 "init-dired"
 "C-c r")

(with-eval-after-load 'dirvish
  (lazy-load-local-keys
   '(("h" . dired-up-directory))
   dirvish-mode-map
   "init-dired"))


;;; 保存恢复工作布局
(lazy-load-global-keys
 '(("s" . emacs-session-save)
   ("l" . emacs-session-restore)
   ("r" . recentf))
 "init-sessions"
 "C-c s")


;;; maple-run: 编译并运行
(lazy-load-global-keys
 '(("r" . compile-and-run))
 "init-compile-and-run"
 "C-c r")


;;; embark: 选中后操作
(lazy-load-global-keys
 '(("C-c e" . embark-act))
 "embark")


(provide 'init-key)
