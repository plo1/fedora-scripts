;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-list
      '(helm company yasnippet yasnippet-snippets
             multiple-cursors vlf pdf-tools treemacs
             eglot lsp-mode company-lsp lsp-java 
             markdown-mode auctex rust-mode))
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
(setq inhibit-startup-screen t)
(setq show-paren-delay 0)
(show-paren-mode 1)
(global-set-key (kbd "M-t") 'shell)
(global-set-key [f12] 'indent-region)
(electric-pair-mode 1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; treemacs
(require 'treemacs)
(global-set-key [f8] 'treemacs)

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
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 3)
(delete 'company-clang company-backends)

;; eglot
(require 'eglot)

;; lsp + company-lsp
(require 'lsp-mode)
(require 'company-lsp)
(push 'company-lsp company-backends)

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

;; rust-mode + eglot
(require 'rust-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)

;; c-mode + eglot
;; c++-mode + eglot
(setq-default c-basic-offset 4)
(add-to-list 'eglot-server-programs '(c-mode . ("clangd" "-header-insertion=never")))
(add-to-list 'eglot-server-programs '(c++-mode . ("clangd" "-header-insertion=never")))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; java-mode + lsp-mode
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

;; pdf-tools settings
(pdf-tools-install)

;; auctex settings
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(add-hook 'LaTeX-mode-hook 'eglot-ensure)
;;(add-hook 'LaTeX-mode-hook (lambda () (setq-local company-idle-delay 1)))



