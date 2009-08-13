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
(add-to-list 'nose-project-names "../bin/nosetests")
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

;; http://www.emacswiki.org/emacs/WindowNavigation
(defun ido-jump-to-window ()
  (interactive)
  (let* ((swap (lambda (l)
                 (if (cdr l)
                     (cons (cadr l) (cons (car l) (cddr l)))
                   l)))
         ;; Swaps the current buffer name with the next one along.
         (visible-buffers (swap (mapcar '(lambda (window) (buffer-name (window-buffer window))) (window-list))))
         (buffer-name (ido-completing-read "Window: " visible-buffers))
         window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have a visible window" buffer-name)
      (setq window-of-buffer
                (delq nil (mapcar '(lambda (window)
                                       (if (equal buffer-name (buffer-name (window-buffer window)))
                                           window
                                         nil))
                                  (window-list))))
      (select-window (car window-of-buffer)))))



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


;; Keybindings
(global-set-key [(meta t)] 'textmate-goto-file)
(global-set-key [(meta z)] 'textmate-find-in-project-type)
(global-set-key [(control q)] 'kill-this-buffer)
(global-set-key [(meta j)] 'lop-swap-window-to-first)
(global-set-key [(meta shift j)] 'lop-swap-window)
(global-set-key [pause] 'toggle-window-dedicated)
(global-set-key "\C-m" 'indent-new-comment-line)
