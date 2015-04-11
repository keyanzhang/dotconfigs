;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(setq pkg-ls
      '(emmet-mode
        ace-jump-mode
        adaptive-wrap
        auctex
        auto-complete
        popup
        cider
        queue
        pkg-info
        epl
        dash
        clojure-mode
        coffee-mode
        color-theme-sanityinc-solarized
        color-theme-sanityinc-tomorrow
        dash-at-point
        exec-path-from-shell
        ghc
        haskell-mode
        helm
        async
        htmlize
        js-doc
        markdown-mode
        multiple-cursors
        paredit
        quack
        racket-mode
        s
        faceup
        rainbow-delimiters
        skewer-mode
        js2-mode
        simple-httpd
        slime
        smartparens
        sml-mode
        tuareg
        caml
        web-mode))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(dolist (p pkg-ls)
  (unless (package-installed-p p)
    (package-install p)))

(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))


;; clojure
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-log-messages t)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-result-prefix ";; => ")
(setq cider-repl-display-in-current-window t)


;; ace-jump-mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key [(hyper f)] 'ace-jump-mode)


;; HTML
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (interactive)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (disable-paredit-mode)
  (smartparens-strict-mode)
  (emmet-mode))

(add-hook 'web-mode-hook 'my-web-mode-hook)


;; coffee
(eval-after-load "coffee-mode"
  '(define-key coffee-mode-map (kbd "C-c C-c") 'coffee-send-line))

(eval-after-load "coffee-mode"
  '(define-key coffee-mode-map (kbd "C-c C-l") 'coffee-send-buffer))



;; js
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'skewer-mode)
(eval-after-load "js2-mode"
  '(define-key js2-mode-map (kbd "C-c C-l") 'skewer-load-buffer))
(eval-after-load "js2-mode"
  '(define-key js2-mode-map "{" 'paredit-open-curly))
(eval-after-load "js2-mode"
  '(define-key js2-mode-map "}" 'paredit-close-curly-and-newline))

(defun skewer-reload-and-eval-defun ()
  "skewer-load-buffer & skewer-eval-defun"
  (interactive)
  (skewer-load-buffer)
  (skewer-eval-defun))

(eval-after-load "js2-mode"
  '(define-key js2-mode-map (kbd "C-c C-c") 'skewer-reload-and-eval-defun))


(require 'js-doc)
(setq js-doc-mail-address "kz5@indiana.edu"
      js-doc-author (format "Keyan Zhang <%s>" js-doc-mail-address)
      js-doc-url "http://keyanzhang.com"
      js-doc-license "WTFPL v2")
(eval-after-load "js2-mode"
  '(define-key js2-mode-map (kbd "C-c i") 'js-doc-insert-function-doc))


;; R
(require 'ess-site)


;; $PATH from shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; ghc-mod
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


;; haskell
;; (setq haskell-stylish-on-save t)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

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
	"Poor man's keyboard macro to import a lib."
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

(put 'dethm 'scheme-indent-function 'defun)
(mapc (lambda (x) (put x 'scheme-indent-function 'defun))
      '(locals ulocals spills frame-conflict locate new-frames call-live
               register-conflict import export))

;; paredit-mode
(defun enable-nospace-paredit-mode ()
  "no automatic space."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(add-hook 'prog-mode-hook 'enable-nospace-paredit-mode)
;; (add-hook 'racket-mode-hook 'enable-nospace-paredit-mode)
;; (add-hook 'scheme-mode-hook 'enable-nospace-paredit-mode)
;; (add-hook 'lisp-mode-hook 'enable-nospace-paredit-mode)

;; give smartparens a try.

;; (add-hook 'prog-mode-hook 'smartparens-mode)
;; (add-hook 'prog-mode-hook 'smartparens-strict-mode)


;; rainbow-delimiters-mode
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; racket-mode
(mapc (lambda (x) (put x 'racket-indent-function 1))
      '(pmatch pmatch-who))

(eval-after-load "racket-mode"
  '(define-key racket-mode-map (kbd "C-c C-c") 'racket-send-definition))
(eval-after-load "racket-mode"
  '(define-key racket-mode-map (kbd "M-RET") 'racket-run))



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
(setq recentf-max-menu-items 150)
(global-set-key "\C-x\ \C-r" 'helm-recentf)


;; some weird emacs behaviors
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq bkup-dir "/Users/k/.emacs.d/bkup/") ;; no more temp files in the folder

(setq backup-directory-alist
      `((".*" . ,bkup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,bkup-dir t)))

(setq-default indent-tabs-mode nil)

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

;; Meta + mousewheel-up: increase window opacity
;; Meta + mousewheel-down: decrease opacity
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
(add-to-list 'package-pinned-packages '(tuareg . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(skewer-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(web-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(ace-jump-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(htmlize . "marmalade") t)
(add-to-list 'package-pinned-packages '(smartparens . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(emmet-mode . "marmalade") t)


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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#657b83" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#fdf6e3"))
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(fci-rule-color "#eee8d5")
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-indentation
     (lambda nil
       (ghc-init)))) t)
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-indent-switch-body t)
 '(markdown-indent-on-enter nil)
 '(quack-default-program "p423petite")
 '(quack-programs
   (quote
    ("guile" "mit-scheme" "petite" "racket" "racket -il typed/racket" "scheme" "p423petite")))
 '(scheme-program-name "p423petite")
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-code-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Anonymous Pro" :foundry "nil" :slant normal :weight normal :height 140 :width normal)))))


