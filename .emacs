(server-start)
(global-auto-revert-mode)

(add-to-list 'load-path "~/.emacs-extras")

(require 'color-theme)
(color-theme-charcoal-black)

; (require 'ipython)
(require 'show-wspace)

(require 'ido)
(ido-mode t)

(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)


;; http://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)



(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(tabbar-mode t nil (tabbar))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(show-ws-tab ((t (:background "DarkGreen"))))
 '(tabbar-default ((((class color grayscale) (background dark)) (:inherit variable-pitch :background "gray50" :foreground "white" :height 0.9))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "white" :box (:line-width 1 :color "white" :style pressed-button))))))

;; h/t Augie
;;(add-hook 'before-save-hook '(lambda ()
;;                                (whitespace-cleanup)))
(setq require-final-newline t)
