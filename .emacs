
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(server-start)
(global-auto-revert-mode)

(add-to-list 'load-path "~/.emacs-extras")
(add-to-list 'package-archives
	                  '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(defun zenburn-init ()
  (load-theme 'zenburn)
)
(add-hook 'after-init-hook 'zenburn-init)

(autoload 'vc-git-root "vc-git")

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)


(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(electric-pair-mode)
(dumb-jump-mode)

(require 'ibuffer)
(setq ibuffer-saved-filter-groups
  (quote (("default"
            ("Projects"
              (filename . "/projects/"))
            ("Lib"
              (filename . "/lib/"))
            ("Misc Programming"
              (or
                (mode . c-mode)
                (mode . perl-mode)
                (mode . python-mode)
                (mode . emacs-lisp-mode)
                ))
            ("Notes"
              (mode . rst-mode))
           ))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))


(scroll-bar-mode -1)

(defun lo-tabs-mode ()
  (setq default-tab-width 2)
  (setq indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 2)
  (setq js-indent-level 2)  
  (setq c-default-style "bsd"
        c-basic-offset 2)
  )

(require 'volatile-highlights)
(volatile-highlights-mode t)

(setq cua-enable-cua-keys nil)
(cua-mode t)

(column-number-mode 1)




;; Document modes
(require 'rst)

(setq auto-mode-alist
      (append '(("\\.txt$" . rst-mode)
                ("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)
               ) auto-mode-alist))
(add-hook 'rst-adjust-hook 'rst-toc-update)

(require 'linum)



(setq frame-background-mode 'dark)

;; http://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; No splash screen!
(setq inhibit-splash-screen t)

;; Save session
(desktop-save-mode 1)

;; Disable tool-bar
;; (tool-bar-mode -1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(custom-safe-themes
   (quote
    ("e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default)))
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(js2-basic-offset 2)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(package-selected-packages
   (quote
    (json-mode dumb-jump ag projectile smart-tab magit magit-popup js2-mode git-commit zenburn-theme names flx exec-path-from-shell org-roam)))
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(split-height-threshold 120)
 '(tabbar-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-follow-symlinks t)
 '(warning-minimum-level :error)
 )
(setq require-final-newline t)

;; http://stackoverflow.com/questions/730751/hiding-m-in-emacs
(defun remove-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


;; Modified from Steve Yegge
(defun lop-swap-window ()
  "Swap the first and next windows' buffers" (interactive)
  (let* ((w1 (first (window-list)))
        (w2 (next-window)))
    (lop-swap-windows (w1 w2))))

(defun lop-swap-window-to-first ()
  "Swap the first and current windows' buffers" (interactive)
  (let* ((w1 (window-at 0 0))
         (w2 (first (window-list))))
    (lop-swap-windows w1 w2)
    (select-window w1)))

(defun lop-swap-windows (w1 w2)
  (let* ((b1 (window-buffer w1))
         (b2 (window-buffer w2))
         (s1 (window-start w1))
         (s2 (window-start w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)
    (set-window-start w1 s2)
    (set-window-start w2 s1)
    ))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (other-window -1))



;; http://stackoverflow.com/questions/43765/pin-emacs-buffers-to-windows-for-cscope
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))


;; Customizing colors used in diff mode
(defun custom-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute
   'diff-added nil :foreground "grey")
  (set-face-attribute
   'diff-removed nil :foreground "grey")
  (set-face-attribute
   'diff-changed nil :foreground "purple"))
;;  (set-face-attribute
;;   'magit-diff-hunk-header nil :foreground "black")
(eval-after-load "diff-mode" '(custom-diff-colors))

;; Org-mode



;; Keybindings

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key "\C-xp" 'select-previous-window)
(global-set-key "\C-xo" 'other-window)
(global-set-key [(control q)] 'kill-this-buffer)
(global-set-key [(control shift q)] 'quoted-insert)
(global-set-key [f9] 'toggle-window-dedicated)
(global-set-key "\C-m" 'indent-new-comment-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c <C-return>") 'cua-set-rectangle-mark)

(global-set-key "\M-j" 'dumb-jump-quick-look)
(global-set-key (kbd "C-M-j") 'dumb-jump-go)


;; effective emacs
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'kill-region)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)


(global-set-key (kbd "C-c s") 'magit-status)

;; org-mode
(global-set-key (kbd "C-c c") 'org-capture)


(projectile-mode +1)
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
