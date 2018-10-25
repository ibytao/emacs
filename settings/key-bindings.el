(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)

 ("C-<down>" . sp-down-sexp)
 ("C-<up>"   . sp-up-sexp)
 ("M-<down>" . sp-backward-down-sexp)
 ("M-<up>"   . sp-backward-up-sexp)

 ("C-M-f" . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)

 ("C-M-n" . sp-next-sexp)
 ("C-M-p" . sp-previous-sexp)

 ("C-S-f" . sp-forward-symbol)
 ("C-S-b" . sp-backward-symbol)

 ("C-<right>" . sp-forward-slurp-sexp)
 ("M-<right>" . sp-forward-barf-sexp)
 ("C-<left>"  . sp-backward-slurp-sexp)
 ("M-<left>"  . sp-backward-barf-sexp)

 ("C-M-t" . sp-transpose-sexp)
 ("C-M-k" . sp-kill-sexp)
 ("C-k"   . sp-kill-hybrid-sexp)
 ("M-k"   . sp-backward-kill-sexp)
 ("C-M-w" . sp-copy-sexp)

 ("C-M-d" . delete-sexp)

 ("M-<backspace>" . backward-kill-word)
 ("C-<backspace>" . sp-backward-kill-word)
 ([remap sp-backward-kill-word] . backward-kill-word)

 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp)

 ("C-x C-t" . sp-transpose-hybrid-sexp)

 ("C-c ("  . wrap-with-parens)
 ("C-c ["  . wrap-with-brackets)
 ("C-c {"  . wrap-with-braces)
 ;; ("C-c '"  . wrap-with-single-quotes)
 ("C-c \"" . wrap-with-double-quotes)
 ("C-c _"  . wrap-with-underscores)
 ("C-c `"  . wrap-with-back-quotes))


(bind-keys
 :map prog-mode-map
 ;; :filter (org-at-item-p)
 ;; Clever newlines
 ("C-o" . open-line-and-indent)
 ("<C-return>" . open-line-below)
 ("<C-S-return>" . open-line-above)
 ("<M-return>" . new-line-dwim)
 )

(defun my-org-hook ()
  (define-key org-mode-map "C-o" nil)
  (define-key org-mode-map "<C-return>" nil)
  (define-key org-mode-map "<C-S-return>" nil)
  (define-key org-mode-map "<M-return>" nil)
  )
(add-hook 'org-mode-hook 'my-org-hook)

;;key-map
(global-set-key (kbd "C-c b") 'create-scratch-buffer)
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Buffer file functions
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; M-i for back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Indentation help
(global-set-key (kbd "M-j") (λ (join-line -1)))

;; Find file in project
(global-set-key (kbd "C-x o") 'find-file-in-project)

(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Move windows, even in org-mode
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

(provide 'key-bindings)
