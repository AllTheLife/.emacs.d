(require 'maple-run)


(setq maple-run:timeout 180
      maple-run:auto-clear t)

(add-to-list 'maple-run:alist '((c++-ts-mode c++-mode) :command "./main"))
(add-to-list 'maple-run:alist '((python-ts-mode python-mode) :command "python %F"))

(defun compile-and-run-cpp ()
  (interactive)
  (let* ((file (buffer-file-name))
	 (command (concat "g++ -o main -Wall -g " file)))
    (compile command)
    (unless compilation-finish-functions
      (add-hook 'compilation-finish-functions
		(lambda (buffer _)
		  (next-window-any-frame)
		  (let ((status (save-excursion
				  (goto-char (point-min))
				  (search-forward "finished" nil t))))
		    (if status
			(progn
			  (delete-window)
			  (kill-buffer "*compilation*")
			  (maple-run)))))))))

(defun compile-and-run ()
  (interactive)
  (if (derived-mode-p 'c++-mode)
      (compile-and-run-cpp)
    (maple-run)))


(provide 'init-compile-and-run)
