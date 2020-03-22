;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-list
      '(helm company yasnippet yasnippet-snippets
             multiple-cursors vlf pdf-tools treemacs
             eglot markdown-mode auctex))
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; General Editor Settings
(load-theme 'tsdh-dark)
(setq make-backup-files nil)
(setq auto-save-timeout 300)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq show-paren-delay 0)
(show-paren-mode 1)
(global-set-key (kbd "s-t") 'shell)
(global-set-key [f12] 'indent-region)
(electric-pair-mode 1)

;; multiple-cursors-mode
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; view large files
(require 'vlf-setup)

;; windmove settings
(windmove-default-keybindings)

;; company settings
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 3)

;; yasnippet settings
(yas-global-mode 1)
(setq yas-triggers-in-field t)

;; helm settings
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-s o") 'helm-occur)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "M-<tab>") 'helm-lisp-completion-at-point)
(global-set-key (kbd "M-/") 'helm-dabbrev)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; python-mode + eglot
(setq python-shell-interpreter "/usr/bin/python3")
(add-hook 'python-mode-hook 'eglot-ensure)

;; c-mode + eglot
;; c++-mode + eglot
(require 'eglot)
(setq-default c-basic-offset 4)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-9"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; pdf-tools settings
(pdf-tools-install)

;; auctex settings
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(add-hook 'LaTeX-mode-hook 'eglot-ensure)
;(add-hook 'LaTeX-mode-hook (lambda () (setq-local company-idle-delay 1)))

;; Treemacs settings
(require 'treemacs)
(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (irony smartparens company-lsp meghanada treemacs auctex pdf-tools edit-indirect markdown-mode irony-eldoc impatient-mode company-irony company-irony-c-headers yasnippet-snippets emmet-mode company-web helm vlf multiple-cursors company-anaconda))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
