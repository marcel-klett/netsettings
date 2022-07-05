;; -*- lexical-binding: t; -*-

;; Speed up the startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      max-lisp-eval-depth 1600
      max-specpdl-size 2500)
(defun user/reset-startup-values ()
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))
(add-hook 'emacs-startup-hook 'user/reset-startup-values)

(setq byte-compile-warnings '(cl-functions))

(setq-default custom-file (concat user-emacs-directory "custom.el"))

;; Work around Emacs bug https://debbugs.gnu.org/cgi/bugreport.cgi?bug=36725
(when (and (gnutls-available-p)
           (>= libgnutls-version 30603)
           (version<= emacs-version "26.2"))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(require 'package)
;add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")t)
(package-initialize)

(setq inhibit-startup-screen t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package multiple-cursors
  :ensure t
  :bind (("C-x C-m" . mc/edit-lines)))
(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)
	      ("C-c p" . projectile-command-map)))

; IGNORE BELL
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)

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

(use-package nimbus-theme
  :ensure t
  :config
  (load-theme 'nimbus t))

(put 'upcase-region 'disabled nil)

;; xterm
(xterm-mouse-mode t)
(setq mouse-sel-mode t)
(defun track-mouse(e))
(unless (window-system)
  (defun user/mwheel-scroll (&rest args)
    "Wraps `mwheel-scroll' for use with <mouse-4> and <mouse-5>."
    (interactive (advice-eval-interactive-spec
                  (cadr (interactive-form 'mwheel-scroll))))
    (let ((mouse-wheel-down-event 'mouse-4)
          (mouse-wheel-up-event 'mouse-5))
      (apply 'mwheel-scroll args)))
  (defun user/mouse-wheel-text-scale (&rest args)
    "Wraps `mouse-wheel-text-scale' for use with <mouse-4> and <mouse-5>."
    (interactive (advice-eval-interactive-spec
                  (cadr (interactive-form 'mouse-wheel-text-scale))))
    (let ((mouse-wheel-down-event 'mouse-4)
          (mouse-wheel-up-event 'mouse-5))
      (apply 'mouse-wheel-text-scale args)))
  (global-set-key (kbd "<mouse-4>") 'user/mwheel-scroll)
  (global-set-key (kbd "<mouse-5>") 'user/mwheel-scroll)
  (global-set-key (kbd "<C-mouse-4>") 'user/mouse-wheel-text-scale)
  (global-set-key (kbd "<C-mouse-5>") 'user/mouse-wheel-text-scale)
  (global-set-key (kbd "<S-mouse-4>") 'user/mwheel-scroll)
  (global-set-key (kbd "<S-mouse-5>") 'user/mwheel-scroll))

;; delsel
(delete-selection-mode 1)

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

;; Tree-sitter needs dynamic module loading
(when (and (functionp 'module-load) (bound-and-true-p module-file-suffix))
  (use-package tree-sitter
    :ensure t
    :config (global-tree-sitter-mode))
  (use-package tree-sitter-langs
    :ensure t
    :after tree-sitter
    :config (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))


(use-package xclip
  :ensure t
  :config
  (condition-case err
      (xclip-mode 1)
    (file-error (message "file-error: %S" err))))

