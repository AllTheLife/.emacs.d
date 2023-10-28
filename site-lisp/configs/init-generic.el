;; 增加长行处理性能
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; 增加 IO 性能
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 1024 1024))

;; 解决font-lock的性能问题
(setq jit-lock-defer-time 0.03)
(setq jit-lock-stealth-time 16)

;; 平滑滚动
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t
      scroll-step 1
      scroll-conservatively 10000)

(defun +pixel-scroll-interpolate-down (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
    (pixel-scroll-interpolate-down)))

(defun +pixel-scroll-interpolate-up (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
  (pixel-scroll-interpolate-up))

(defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
(defalias 'scroll-down-command '+pixel-scroll-interpolate-up)

(fset 'yes-or-no-p 'y-or-n-p)                    ;; 以 y/n 代表 yes/no
(global-subword-mode 1)                          ;; Word 移动支持 FooBar 的格式
(delete-selection-mode 1)                        ;; 选择后再次输入视为替换
(global-display-line-numbers-mode 1)             ;; 显示行号
(global-hl-line-mode 1)                          ;; 高亮当前行
(auto-compression-mode 1)                        ;; 打开压缩文件时自动解压缩
(prettify-symbols-mode)                          ;; 将字符显示为奇特的符号
(save-place-mode)                                ;; 保存光标在每个文件的位置
(savehist-mode 1)                                ;; 保存命令历史
(tool-bar-mode -1)                               ;; 关闭工具栏
(menu-bar-mode -1)                               ;; 关闭菜单栏
(scroll-bar-mode -1)                             ;; 关闭滚动条
(electric-pair-mode 1)                           ;; 自动补全括号
(setq global-mark-ring-max 1024)                 ;; 设置最大的全局标记容量
(setq use-dialog-box nil)                        ;; 从不弹出对话框
(setq history-delete-duplicates t)               ;; 删除 minibuffer 的重复历史
(setq x-stretch-cursor t)                        ;; 光标在 TAB 字符上会显示为一个大方块
(setq inhibit-startup-screen t)                  ;; 关闭开始界面
(setq void-text-area-pointer nil)	         ;; 禁止显示鼠标指针
(setq initial-scratch-message
      ";; Happy hacking, Emacs ❤ you!\n\n")      ;; 设置 *scratch* 注释的内容
(setq ring-bell-function 'ignore)                ;; 关闭出错时烦人的提示声
(setq mouse-yank-at-point t)                     ;; 粘贴于光标处,而不是鼠标指针处
(setq x-select-enable-clipboard t)               ;; 支持 emacs 和外部程序的粘贴
(setq inhibit-compacting-font-caches t)          ;; 使用字体缓存，避免卡顿
(setq confirm-kill-processes nil)                ;; 退出自动杀掉进程
(setq async-bytecomp-allowed-packages nil)       ;; 避免 magit 报错
(setq custom-safe-themes t)                      ;; 启动时不检测主题安全性
(setq word-wrap-by-category t)                   ;; 按照中文折行
(setq ad-redefinition-action 'accept)            ;; 不要烦人的 redefine warning
(setq frame-resize-pixelwise t)                  ;; 设置缩放的模式,避免 Mac 平台最大化窗口以后右边和下边有空隙
(setq create-lockfiles nil)                      ;; 不生成 lockfile
(setq auto-save-default nil)                     ;; 取消自动保存
(setq make-backup-files nil)                     ;; 不生成备份文件
(setq scroll-margin 5)                           ;; 始终在屏幕上方和下方保留 5 行
(setq-default comment-style 'indent)             ;; 设定自动缩进的注释风格
(setq byte-compile-warnings
      (quote (
              ;; 显示的警告
              free-vars                 ;; 不在当前范围的引用变量
              unresolved                ;; 不知道的函数
              callargs                  ;; 函数调用的参数和定义的不匹配
              obsolete                  ;; 荒废的变量和函数
              noruntime                 ;; 函数没有定义在运行时期
              interactive-only          ;; 正常不被调用的命令
              make-local ;; 调用 `make-variable-buffer-local' 可能会不正确的
              mapcar     ;; `mapcar' 调用
              ;; 抑制的警告
              (not redefine)        ;; 重新定义的函数 (比如参数数量改变)
              (not cl-functions)    ;; `CL' 包中的运行时调用的函数
              )))

;; 在行号旁显示文件指示符
(add-hook 'prog-mode-hook
	  (lambda () (setq indicate-buffer-boundaries 'left)))

;; 高亮显示行尾空格
(setq-default show-trailing-whitespace nil)
(dolist (hook '(prog-mode-hook text-mode-hook markdown-mode-hook org-mode-hook))
  (add-hook hook (lambda ()
		   (setq-local show-trailing-whitespace t))))

;; 自动刷新文件
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)


(provide 'init-generic)
