(server-start)
(global-auto-revert-mode)

(add-to-list 'load-path "~/.emacs-extras")

(push '(font-backend xft x) default-frame-alist)

(require 'color-theme)
(color-theme-charcoal-black)

(require 'diff-mode-)

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; (require 'ipython)
(require 'show-wspace)
(require 'textmate)
(textmate-mode)
(textmate-also-ignore "eggs|cover|daisy|.*.pyc")

(require 'nose)
(setq nose-use-verbose nil)
(add-to-list 'nose-project-names "eggs/bin/test")
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'nosetests-all)
            (local-set-key "\C-cm" 'nosetests-module)
            (local-set-key "\C-c." 'nosetests-one)))

(defun pyflakes-this-buffer ()
  (interactive)
  (compilation-start (concat "pyflakes " buffer-file-name) nil (lambda (mode) "*pyflakes*")))

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'mercurial)

; use tab for indent or complete
(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

(add-hook 'python-mode-hook (lambda () (local-set-key [tab] 'indent-or-expand)))


;; Document modes
(require 'rst)

(setq auto-mode-alist
      (append '(("\\.txt$" . rst-mode)
                ("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))
(add-hook 'rst-adjust-hook 'rst-toc-update)

;; Random

(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

(setq frame-background-mode 'dark)

;; http://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; No splash screen!
(setq inhibit-splash-screen t)

;; Disable tool-bar
(tool-bar-mode -1)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(hg-log-limit 30)
 '(hg-outgoing-repository "")
 '(hg-incoming-repository "")
 '(fringe-mode (quote (5 . 5)) nil (fringe))
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(tabbar-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "Grey15" :foreground "Grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal))))
 '(show-ws-tab ((t (:background "DarkGreen"))))
 '(tabbar-default ((((class color grayscale) (background dark)) (:inherit variable-pitch :background "gray50" :foreground "white" :height 0.9))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "white" :box (:line-width 1 :color "white" :style pressed-button))))))

;; h/t Augie
(add-hook 'before-save-hook '(lambda ()
                                (whitespace-cleanup)))
(setq require-final-newline t)

;; Keybindings
(global-set-key [(meta t)] 'textmate-goto-file)
(global-set-key [(meta z)] 'textmate-find-in-project-type)
(global-set-key [(control q)] 'kill-this-buffer)
