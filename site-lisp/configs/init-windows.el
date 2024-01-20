;;; -*- lexical-binding: t -*-


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

(global-set-key (kbd "C-x 1") 'toggle-delete-other-windows)
(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))


(provide 'init-windows)
