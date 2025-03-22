(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun add-subdirs-to-load-path (search-dir depth)
  (unless (> depth default-max-load-depth)
    (let* ((dir (file-name-as-directory search-dir)))
      (dolist (subdir
	       ;; 过滤出不必要的目录，提升 Emacs 启动速度
	       (cl-remove-if
		#'(lambda (subdir)
		    (or
		     ;; 不是目录的文件都移除
		     (not (file-directory-p (concat dir subdir)))
		     ;; 父目录、 语言相关和版本控制目录都移除
		     (member subdir '("." ".."
				      "dist" "node_modules" "__pycache__"
				      "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
		(directory-files dir)))
	(let ((subdir-path (concat dir (file-name-as-directory subdir))))
	  ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升 Emacs 启动速度
	  (when (cl-some #'(lambda (subdir-file)
			     (and (file-regular-p (concat subdir-path subdir-file))
				  ;; .so .dll 文件指非 Elisp 语言编写的 Emacs 动态库
				  (member (file-name-extension subdir-file) '("el" "so" "dll"))))
			 (directory-files subdir-path))

	    ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
	    ;; 这样 Emacs 会从父目录到子目录的顺序搜索 Elisp 插件，顺序反过来会导致 Emacs 无法正常启动
	    (add-to-list 'load-path subdir-path t))

	  ;; 继续递归搜索子目录
	  (add-subdirs-to-load-path subdir-path (+ 1 depth)))))))

(defun +vc-branch-name ()
  (when vc-mode
    (propertize
     (replace-regexp-in-string
      "Git[-:]"
      ""
      (substring-no-properties vc-mode))
     'face
     'bold)))

(defun upcase-char(arg)
  "Uppercase for character."
  (interactive "P")
  (upcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

(defun downcase-char (arg)
  "Downcase for character."
  (interactive "P")
  (downcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

(defun remember-init ()
  "记住当前位置."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun remember-jump ()
  "跳转到最后一次的位置."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun copy-and-insert-newline ()
  "复制当前行并在下方插入新的一行，再进行粘贴"
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-end-position))
  (save-excursion
    (move-end-of-line 1)
    (newline)
    (yank))
  (next-line))

(defun kill-unused-buffers ()
  (interactive)
  (ignore-errors
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when (and (string-prefix-p "*" (buffer-name)) (string-suffix-p "*" (buffer-name)))
          (kill-buffer buf))))))

(defun delete-this-file ()
  "删除此文件，并抹掉对应的缓冲区"
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "重命名当前文件和缓冲区，并访问新名称"
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun add-number-under-point ()
  "把光标下的数字加一"
  (interactive)
  (let ((number (thing-at-point 'number)))
    (delete-region (match-beginning 0) (match-end 0))
    (insert (number-to-string (+ number 1)))))

(defun reduce-number-under-point ()
  "把光标下的数字减一"
  (interactive)
  (let ((number (thing-at-point 'number)))
    (delete-region (match-beginning 0) (match-end 0))
    (insert (number-to-string (- number 1)))))

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

;; Copied from DOOM Emacs
(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The hook(s) to add to.
  2. Optional properties :local, :append, and/or :depth [N].
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N] :remove :call-immediately] FUNCTIONS-OR-FORMS...)"
  (declare (indent defun))
  (let* ((hook-forms (if (listp hooks) hooks (list hooks)))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p call-immediately-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))
        (:call-immediately (setq call-immediately-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil)) next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (func (list ,@func-forms))
         (dolist (hook (nreverse ',hook-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))
         ,(if call-immediately-p `(funcall func))))))

(defmacro defadvice! (symbol arglist &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &rest [WHERE PLACES...] BODY\)"
  (declare (indent defun))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defcustom gc-cons-threshold-while-loading-configs 100000000
  "加载配置时的gc值，默认设为100M.")

(defmacro load-configs (&rest body)
  "加快加载配置时的速度."
  `(let ((file-name-handler-alist nil)
         (gc-cons-threshold gc-cons-threshold-while-loading-configs))
     (progn ,@body)))

(defmacro lazy-load-configs (arglist)
  "分块加载语句."
  (load-configs
   `(let ((load-time 0.3))
      (dolist (sentence ,arglist)
	(run-with-idle-timer
	 load-time nil
	 `(lambda () (load-configs ,sentence)))
	(setq load-time (+ load-time 0.3))))))

(defun jump-last-buffer ()
  "关闭当前buffer."
  (interactive)
  (let ((buffer (buffer-name)))
    (kill-buffer buffer)))


(provide 'init-utils)
