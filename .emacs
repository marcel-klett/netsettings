(setq byte-compile-warnings '(cl-functions))
(require 'package)
;add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")t)
(package-initialize)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq inhibit-startup-screen t)

; start auto-complete with emacs
;(require 'auto-complete)
; do default config for auto-complete
;(require 'auto-complete-config)
;(ac-config-default)

; install not-installed packages
(defun package-dl (p)
  (unless (package-installed-p p)
    (progn
      (package-refresh-contents)
      (package-install p))))

(package-dl 'use-package)
(package-dl 'company)
(package-dl 'company-irony)
(package-dl 'irony)
(package-dl 'lsp-mode)
(package-dl 'jedi)
(package-dl 'multiple-cursors)
(package-dl 'projectile)


(use-package company
	     :ensure t
	     :config
	     (setq company-idle-delay 0)
	     (setq company-minimum-prefix-length 3))
;
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))
;    
(use-package company-irony
	     :ensure t
	     :config
	     (require 'company)
	     (add-to-list 'company-backends 'company-irony))
;;
(use-package irony
	     :ensure t
	     :config
	     (add-hook 'c++-mode-hook 'irony-mode)
	     (add-hook 'c-mode-hook 'irony-mode)
	     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
;
(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))
	    

(use-package lsp-mode
  :hook
  ((c++-mode . lsp)
   (python-mode . lsp)
   (rust-mode . lsp))
  :config
  (setq lsp-clients-clang-args '("-j=4" "-background-index" "-log=error"))
  )


;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)    

; IGNORE BELL
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)

(use-package company
	     :ensure t
	     :config
	     (setq company-idle-delay 0)
	     (setq company-minimum-prefix-length 3))

; DEFINE SHORTCUTS
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-d") 'duplicate-line)


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

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)

(use-package which-key
  :ensure t
  :defer 2
  :config (which-key-mode))

(package-dl 'multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C-x C-m") 'mc/edit-lines)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)



(add-to-list 'custom-theme-load-path "/home/marcel/.emacs.d/themes")
(load-theme 'nimbus t)
; (package-dl 'zenburn-theme)
; (require 'zenburn-theme)
(put 'upcase-region 'disabled nil)

;; xterm
(xterm-mouse-mode t)
(setq mouse-sel-mode t)
(defun track-mouse(e))
(unless (window-system)
  (global-set-key (kbd "<mouse-4>") 'mwheel-scroll)
  (global-set-key (kbd "<mouse-5>") 'mwheel-scroll)
  (global-set-key (kbd "<C-mouse-4>") 'mouse-wheel-text-scale)
  (global-set-key (kbd "<C-mouse-5>") 'mouse-wheel-text-scale)
  (global-set-key (kbd "<S-mouse-4>") 'mwheel-scroll)
  (global-set-key (kbd "<S-mouse-5>") 'mwheel-scroll))

;; paren
(setq show-paren-delay 0)
(show-paren-mode 1)

;; vc
(setq
 vc-ignore-dir-regexp
 (format "\\(%s\\)\\|\\(%s\\)"
         vc-ignore-dir-regexp
         tramp-file-name-regexp)
 vc-git-grep-template "git --no-pager grep --recurse-submodules -n --break <C> -e <R> -- <F>")
(defun user/vc-git-grep (regexp)
  (interactive
   (progn
     (grep-compute-defaults)
     (list (grep-read-regexp))))
  (vc-git-grep regexp "" (vc-git-root default-directory)))
(global-set-key (kbd "C-x v f") 'user/vc-git-grep)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (whichkey which-key projectile multiple-cursors nimbus-theme lsp-mode jedi irony-eldoc company-irony-c-headers company-irony))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

