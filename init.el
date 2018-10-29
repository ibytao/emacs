(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

;;
;; use use-package
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(use-package diminish :ensure t)
(use-package bind-key :ensure t)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;
;;basic setup
;;

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;;shell path
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Set path to dependencies
(setq settings-dir
       (expand-file-name "settings" user-emacs-directory))
;; Set up load path
 (add-to-list 'load-path settings-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Settings for currently logged in user
(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))
(add-to-list 'load-path user-settings-dir)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;;tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-width 2)
(setq lua-indent-level 2)
;;setup html indent
(add-hook 'html-mode-hook
          (lambda ()
            ;; (emmet-mode t)
            ;; default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 2)))
(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq-default js2-basic-offset 2)
(setq css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-indent-style 2)
(setq web-mode-html-offset 2)
(setq web-mode-script-offset 2)

;;Setup key bindings
(require 'key-bindings)

;;
;;setup ido
;;
(eval-after-load 'ido '(require 'setup-ido))

;;undo
(require 'undo-tree)
(global-undo-tree-mode)

;; fullscreen
(setq initial-frame-alist (quote ((fullscreen . maximized))))
;; (setq default-frame-alist
;;       '((width . 100) ; character
;;         (height . 100) ; lines
;;         ))

;;rename html tag
(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

;; auto swith branch (git)
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)

;; Highlight current line
(global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")
(rename-modeline "undo-tree-mode" clojure-mode "U")
(rename-modeline "guide-key-mode" clojure-mode "G")

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Load the config
(org-babel-load-file (concat user-emacs-directory "config.org"))

;;set cursor style
(setq-default cursor-type 'bar)
(blink-cursor-mode t)

;; (defun highlight-selected-window ()
;;   "Highlight selected window with a different background color."
;;   (walk-windows (lambda (w)
;;                   (unless (eq w (selected-window)) 
;;                     (with-current-buffer (window-buffer w)
;;                       (buffer-face-set '(:background "#111"))))))
;;   (buffer-face-set 'default)
;;   )

;; (add-hook 'buffer-list-update-hook 'highlight-selected-window)
