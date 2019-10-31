;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-list '(company-anaconda anaconda-mode multiple-cursors vlf helm company-web emmet-mode yasnippet-snippets yasnippet company-irony-c-headers company-irony impatient-mode irony-eldoc irony company markdown-mode edit-indirect pdf-tools auctex treemacs))
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

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
(electric-pair-mode 1)
(global-set-key (kbd "s-t") 'shell)

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

;; anaconda-mode + company-anaconda settings
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook (lambda () (setq-local company-minimum-prefix-length 1)))
(with-eval-after-load 'company
    (add-to-list 'company-backends '(company-anaconda)))

;; irony + company-irony + company-irony-c-headers settings
;; Requires clang-devel, cmake, llvm-devel
;;(setq company-clang-executable "/usr/bin/clang-8")
(with-eval-after-load 'company 
    (add-to-list 'company-backends '(company-irony-c-headers company-irony)))
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook #'irony-eldoc)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; company-web settings
(with-eval-after-load 'company
    (add-to-list 'company-backends '(company-web-html company-web-jade company-web-slim)))

;; emmet-mode Settings
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;; helm settings
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-s o") 'helm-occur)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "M-<tab>") 'helm-lisp-completion-at-point)
(global-set-key (kbd "M-/") 'helm-dabbrev)

;; Impatient-mode markdown function
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

;; pdf-tools settings
(pdf-tools-install)

;; auctex settings
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(add-hook 'LaTeX-mode-hook (lambda () (setq-local company-idle-delay 2)))

;; Treemacs settings
(require 'treemacs)
(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)

