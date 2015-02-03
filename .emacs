;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))


;; $PATH from shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; ghc-mod
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; chez-scheme
(defun file-path-to-clipboard ()
  "copies the current buffer's file path to the clipboard"
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (kill-new filename)
      (message "Copied: '%s'" filename))))

(global-set-key "\C-c\ \C-f" 'file-path-to-clipboard)

(fset 'p423-import-lib
      (lambda (&optional arg)
	"Keyboard macro."
	(interactive "p")
	(kmacro-exec-ring-item (quote ([67108896 67108896 134217788 M-right right 11 25 24 111 40 105 109 112 111 114 116 32 25 41 return 24 111 21 67108896 21 67108896] 0 "%d")) arg)))

(add-hook 'scheme-mode-hook
          (lambda ()
            (local-set-key
             (kbd "M-RET")
             'p423-import-lib)))

(defun scheme-mode-quack-hook ()
  (require 'quack)
  (setq quack-fontify-style 'emacs))

(add-hook 'scheme-mode-hook 'scheme-mode-quack-hook)


;; paredit-mode
(defun enable-nospace-paredit-mode ()
  "no automatic space."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(add-hook 'prog-mode-hook 'enable-nospace-paredit-mode)


;; rainbow-delimiters-mode
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; racket repl
(add-hook 'racket-mode-hook
          '(lambda ()
             (define-key racket-mode-map (kbd "M-RET") 'racket-run)))


;; python repl
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "M-RET") 'python-shell-send-buffer)))


;; auto-complete-mode
(add-hook 'prog-mode-hook 'auto-complete-mode)


;; linum-mode
(global-linum-mode 1)
(setq linum-format 'dynamic)


;; helm-mode
(helm-mode 1)


;; recentf-mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'helm-recentf)


;; some weird emacs behaviors
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)


;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-H-c C-H-c") 'mc/edit-lines)


;; visual-line-mode, adaptive-wrap-prefix-mode
(global-visual-line-mode 1)
(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))

(add-hook 'minibuffer-setup-hook
	  (lambda ()
	    (visual-line-mode -1)))


;; no tool bar
(tool-bar-mode -1)


;; cursor-type
(setq-default cursor-type '(bar . 1))


;; Quick opacity toggle
(defun my-increase-opacity ()
  (interactive)
  (let ((oldv (frame-parameter (selected-frame) 'alpha)))
    (let ((v (if (null oldv) 100
	       (if (> (+ oldv 2) 100) 100 (+ oldv 2)))))
      (set-frame-parameter (selected-frame) 'alpha v))))

(defun my-decrease-opacity ()
  (interactive)
  (let ((oldv (frame-parameter (selected-frame) 'alpha)))
    (let ((v (if (null oldv) 100
	       (if (< (- oldv 2) 20) 20 (- oldv 2)))))
      (set-frame-parameter (selected-frame) 'alpha v))))

(global-set-key [(meta triple-wheel-up)] 'my-increase-opacity)
(global-set-key [(meta triple-wheel-down)] 'my-decrease-opacity)


;; pin packages
(add-to-list 'package-pinned-packages '(auto-complete . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(coffee-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(color-theme-sanityinc-solarized . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(color-theme-sanityinc-tomorrow . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(dash . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(exec-path-from-shell . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(ghc . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(haskell-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(helm . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(js2-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(markdown-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(multiple-cursors . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(paredit . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(quack . "marmalade") t)
(add-to-list 'package-pinned-packages '(racket-mode . "melpa") t)
(add-to-list 'package-pinned-packages '(rainbow-delimiters . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(slime . "melpa-stable") t)


;; scheme style comments
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)


;; mac system shortcuts
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)


;; mac option key as meta
(defun mac-switch-meta nil 
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
	(setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'hyper))
    (progn 
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta))))

(defun mac-set-option-as-meta nil 
  "set Option key as meta"
  (interactive)
  (progn
    (setq mac-option-modifier 'meta)
    (setq mac-command-modifier 'hyper)))

(mac-set-option-as-meta)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(quack-default-program "p423petite")
 '(quack-programs
   (quote
    ("guile" "mit-scheme" "petite" "racket" "racket -il typed/racket" "scheme" "p423petite")))
 '(scheme-program-name "p423petite")
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "nil" :slant normal :weight normal :height 141 :width normal)))))


