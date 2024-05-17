(require 'lsp-bridge)
(require 'breadcrumb)
(autoload 'avy-goto-word-0 "avy")


(setq acm-enable-tabnine nil)
(setq acm-enable-yas t)
(setq acm-enable-citre nil)
(setq acm-enable-quick-access t)
(setq acm-enable-telega nil)
(setq acm-backend-search-file-words-candidate-min-length 2)
(setq acm-backend-yas-candidate-min-length 2)
(setq acm-backend-lsp-candidate-min-length 2)
(setq acm-backend-lsp-enable-auto-import nil)
(setq acm-candidate-match-function 'regexp-quote)
(setq lsp-bridge-enable-org-babel nil)
(setq lsp-bridge-c-lsp-server "ccls")
(setq lsp-bridge-enable-hover-diagnostic t)
(setq lsp-bridge-enable-completion-in-minibuffer t)
(setq lsp-bridge-enable-inlay-hint t)
(setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
(setq lsp-bridge-signature-show-with-frame-position "point")
(setq lsp-bridge-peek-ace-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
(setq lsp-bridge-diagnostic-fetch-idle 0.1)
(setq lsp-bridge-semantic-tokens-mode t)
(setq markdown-enable-highlighting-syntax t)
(dolist (i '(1 2 3 4 5 6 7 8 9 0))
  (add-to-list 'lsp-bridge-completion-hide-characters (number-to-string i)))
(setq-default markdown-fontify-code-blocks-natively t)

(global-lsp-bridge-mode)
(breadcrumb-mode t)

(defun lsp-bridge-avy-peek ()
  "Peek any symbol in the file by avy jump."
  (interactive)
  (setq lsp-bridge-peek-ace-list (make-list 5 nil))
  (setf (nth 1 lsp-bridge-peek-ace-list) (point))
  (setf (nth 2 lsp-bridge-peek-ace-list) (buffer-name))
  (save-excursion
    (call-interactively 'avy-goto-word-0)
    (lsp-bridge-peek)
    (setf (nth 3 lsp-bridge-peek-ace-list) (buffer-name))
    (setf (nth 4 lsp-bridge-peek-ace-list) (point))))

(progn
  (defun exec/lsp-which-function(file line column)
	(with-current-buffer
		(find-file-noselect file)
	  (goto-line line)
	  (move-to-column column)
      (which-function)))

  (defun get-function-name-and-overlay (file line column)
	"Find the function name at a specific line and column in a file and put an overlay."
	(let* ((function-name (exec/lsp-which-function file line column))
		   (ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) nil t))
		   (text (format "%30s │" (if function-name function-name "")))
		   )
	  ;; (delete-all-overlays (current-buffer))
      (overlay-put ov 'before-string
				   (propertize text 'face 'font-lock-string-face)
				   )
	  (overlay-put ov 'evaporate t)))

  (defun parse-buffer-and-overlay-function-name (&optional a b c)
	"Parse the buffer content to get file, line number, column and make an overlay of function name."
	(interactive)
	(save-excursion
      (goto-char (point-min))
      (let ((current-file nil))
		(while (not (eobp)) ; while not end of buffer
          ;; check if current line is a file path
          (if (looking-at "^/.+?$")
              ;; update the current file
			  (setq current-file (buffer-substring-no-properties
								  (line-beginning-position) (line-end-position)))
			(progn
			  ;; else check if it's a line:col
			  (when (and current-file (looking-at "^\\([0-9]+\\):\\([0-9]+\\):"))
				;; call your function with the captured groups as arguments
				(get-function-name-and-overlay
				 current-file
				 (string-to-number (match-string 1))
				 (string-to-number (match-string 2))))))
          (forward-line 1)))))

  (advice-add 'lsp-bridge-references--popup :after 'parse-buffer-and-overlay-function-name))


(provide 'init-lsp)
