(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)

 ("C-M-f" . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)

 ("C-M-d" . sp-down-sexp)
 ("C-M-u" . sp-backward-up-sexp)

 ("C-M-p" . sp-backward-down-sexp)
 ("C-M-n" . sp-up-sexp)

 ("M-<up>" . sp-splice-sexp-killing-backward)
 ("M-<down>" . sp-splice-sexp-killing-forward)

 ("M-s" . sp-splice-sexp)
 ("M-S" . sp-splict-sexp)
 ("M-J" . sp-join-sexp)

 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp)

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
 )

;; Clever newlines
;; (define-key input-decode-map "\C-i" [C-i])
(global-set-key (kbd "<C-o>") 'open-line-and-indent)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "<M-return>") 'new-line-dwim)
(global-set-key (kbd "<S-return>") 'new-line-dwim)

(defun my-org-hook ()
  (define-key org-mode-map (kbd "<C-o>") 'org-open-line)
  (define-key org-mode-map (kbd "<C-return>") 'org-insert-heading-respect-content)
  (define-key org-mode-map (kbd "<C-S-return>") 'org-insert-todo-heading-respect-content)
  (define-key org-mode-map (kbd "<M-return>") 'org-meta-return)
  (define-key org-mode-map (kbd "C-c /") 'org-sparse-tree)
  (define-key org-mode-map (kbd "C-c l") 'org-store-link)
  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
  (define-key org-mode-map (kbd "C-c c") 'org-capture)
  )
(add-hook 'org-mode-hook 'my-org-hook)

;;key-map
(global-set-key (kbd "C-c n") 'create-scratch-buffer)
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Buffer file functions
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Perform general cleanup.
(global-set-key (kbd "C-c c") 'cleanup-buffer)

;; M-i for back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Indentation help
(global-set-key (kbd "M-j") (λ (join-line -1)))

(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Move windows, even in org-mode
;; (global-set-key (kbd "<s-right>") 'windmove-right)
;; (global-set-key (kbd "<s-left>") 'windmove-left)
;; (global-set-key (kbd "<s-up>") 'windmove-up)
;; (global-set-key (kbd "<s-down>") 'windmove-down)

(global-unset-key (kbd "s-n"))
(global-set-key (kbd "s-n") 'next-buffer)
(global-set-key (kbd "s-p") 'previous-buffer)

(defun prev-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "s-[") 'prev-window)
(global-set-key (kbd "s-]") 'other-window)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Killing text
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-S-w") 'kill-to-beginning-of-line)
(global-unset-key (kbd "C-u"))
(global-set-key (kbd "C-u") 'kill-to-beginning-of-line)

;; Line movement
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

;; Like isearch, but adds region (if any) to history and deactivates mark
;; (global-set-key (kbd "C-s") 'isearch-forward-use-region)
;; (global-set-key (kbd "C-r") 'isearch-backward-use-region)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;; Jump from file to containing directory
(global-set-key (kbd "C-x C-j") 'dired-jump) (autoload 'dired-jump "dired")
(global-set-key (kbd "C-x M-j") '(λ (dired-jump 1)))

;; Make shell more convenient, and suspend-frame less
;; ansi-term
(global-set-key (kbd "C-z") '(lambda ()(interactive)(ansi-term "/usr/local/bin/fish")))
;; (global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x M-z") 'suspend-frame)

;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)
(global-set-key (kbd "s-0") 'kill-buffer-and-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
;; (global-set-key (kbd "s-2") 'split-window-below)
;; (global-set-key (kbd "s-3") 'split-window-horizontally)
(global-set-key (kbd "s-2") (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key (kbd "s-3") (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(global-set-key (kbd "C-;") 'marker-visit-prev)
(global-set-key (kbd "C-:") 'marker-visit-next)

;; c-h to delete
;; (global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "C-h") 'paredit-backward-delete)
;; (global-set-key (kbd "M-h") 'backward-kill-word)
;; (global-set-key (kbd "<f1>") 'help-command)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

;; (define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key (kbd "s-b") 'ido-switch-buffer)

;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

(provide 'key-bindings)
