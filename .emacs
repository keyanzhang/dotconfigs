(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)

(package-initialize)

(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

(show-paren-mode 1)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(defun enable-nospace-paredit-mode ()
  "no automatic space."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(add-hook 'prog-mode-hook 'enable-nospace-paredit-mode)

(add-hook 'racket-mode-hook
          '(lambda ()
             (define-key racket-mode-map (kbd "M-RET") 'racket-run)))

(global-linum-mode 1)

(helm-mode 1)

(add-hook 'prog-mode-hook 'auto-complete-mode)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'helm-recentf)

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Keybonds
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)

;; personal shortcuts
(global-set-key [(hyper /)] 'comment-or-uncomment-region)

;; mac switch meta key
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
 '(ac-auto-start t)
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#343d46" "#bf616a" "#a3be8c" "#ebcb8b" "#8fa1b3" "#b48ead" "#8fa1b3" "#dfe1e8"])
 '(ansi-term-color-vector [unspecified "#343d46" "#bf616a" "#a3be8c" "#ebcb8b" "#8fa1b3" "#b48ead" "#8fa1b3" "#dfe1e8"])
 '(custom-enabled-themes (quote (base16-ocean-dark)))
 '(custom-safe-themes (quote ("e027af9a75cf334e3992d4283ae1e110596617b345f304b73ab9503a2e93c048" "48a4c6ddfec61c550e585274a8cd3d561dbaad1f7308c76fa59ee975d01c3820" "9c80ac7a9056844e850a9c518c7da91cc82e00641bcfcc90e78dc60c3a93a4ee" "0d4002022c6ecfc203b772dfa9c017aee6209cbca67fa694174b9b89e14a1ab0" "93704bcb6fd547ef60d5e92b3b7772f7ed138b2227f7a92fc0527733f434b0b9" "44350b5b6c90a880eb4d173884ea1f340320b46dc1c9cc98785f60066740030a" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "45bb2cffcb74aa1a22cb472eec6b68371766c42ec4027e7752062868bb15a38a" "ef67c2b7844e55223d15e5bfd6bc54de0752d356f89b7f02308838abf2726323" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" default)))
 '(fci-rule-color "#343d46")
 '(fringe-mode (quote (0)) nil (fringe))
 '(global-auto-complete-mode nil)
 '(indicate-buffer-boundaries (quote ((t . right) (top . left))))
 '(racket-mode-pretty-lambda nil)
 '(racket-program "/usr/local/bin/racket")
 '(raco-program "/usr/local/bin/raco")
 '(show-paren-mode t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#bf616a") (40 . "#DCA432") (60 . "#ebcb8b") (80 . "#B4EB89") (100 . "#89EBCA") (120 . "#89AAEB") (140 . "#C189EB") (160 . "#bf616a") (180 . "#DCA432") (200 . "#ebcb8b") (220 . "#B4EB89") (240 . "#89EBCA") (260 . "#89AAEB") (280 . "#C189EB") (300 . "#bf616a") (320 . "#DCA432") (340 . "#ebcb8b") (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "nil" :slant normal :weight light :height 150 :width normal)))))
