(bind-keys
 :map prog-mode-map
 :filter (org-mode)
 ;; Clever newlines
 ("C-o" . open-line-and-indent)
 ("<C-return>" . open-line-below)
 ("<C-S-return>" . open-line-above)
 ("<M-return>" . new-line-dwim)
 )
;;key-map
(global-set-key (kbd "C-c b") 'create-scratch-buffer)
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Buffer file functions
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(provide 'key-bindings)
