
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(server-start)
(global-auto-revert-mode)

(add-to-list 'load-path "~/.emacs-extras")

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))

(push '(font-backend xft x) default-frame-alist)


(autoload 'vc-git-root "vc-git")


(electric-pair-mode)


(scroll-bar-mode -1)
(blink-cursor-mode 0)


; Django templates
;(load "~/lib/nxhtml/autostart.el")
;(setq mumamo-background-colors nil)
;;(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(autoload 'typescript-mode "TypeScript" nil t)
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

(add-to-list 'auto-mode-alist '("\\.wsgi$" . python-mode))

(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.component$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(add-hook 'web-mode-hook
  (lambda ()
    (local-set-key (kbd "RET") 'newline-and-indent)))

(defun lo-tabs-mode ()
  (setq default-tab-width 2)
  (setq indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 2)
  (setq js-indent-level 2)  
  (setq c-default-style "bsd"
        c-basic-offset 2)
  )

(add-hook 'nxhtml-mumamo-mode-hook 'lo-tabs-mode)
(add-hook 'nxhtml-nxhtml-mode-hook 'lo-tabs-mode)
(add-hook 'php-mode-hook 'lo-tabs-mode)
(add-hook 'js-mode-hook 'lo-tabs-mode)
(add-hook 'web-mode-hook 'lo-tabs-mode)

(require 'volatile-highlights)
(volatile-highlights-mode t)


(require 'midnight)
(midnight-delay-set 'midnight-delay "9:50am")
(setq clean-buffer-list-delay-general 5)

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


(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Shell

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Random

;;(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

(setq frame-background-mode 'dark)

;; http://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; No splash screen!
(setq inhibit-splash-screen t)

;; Save session
(desktop-save-mode 1)

;; Disable tool-bar
(tool-bar-mode -1)

(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(custom-safe-themes
   (quote
    ("0c32e4f0789f567a560be625f239ee9ec651e524e46a4708eb4aba3b9cdc89c5" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" default)))
 '(fringe-mode (quote (5 . 5)) nil (fringe))
 '(hg-incoming-repository "")
 '(hg-log-limit 30)
 '(hg-outgoing-repository "")
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(js2-basic-offset 2)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(mumamo-submode-indent-offset 4)
 '(org-agenda-files (quote ("~/org/things.org" "~/org/notes.org" "~/")))
 '(org-directory "~/org" t)
 '(package-selected-packages
   (quote
    (json-mode flycheck dumb-jump ag projectile smart-tab magit magit magit-popup js2-mode git-commit zenburn-theme names flx exec-path-from-shell)))
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(split-height-threshold 120)
 '(tabbar-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-follow-symlinks t)
 '(warning-minimum-level :error))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "Grey15" :foreground "Grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal))))
 '(diff-added ((t (:foreground "DarkGreen"))))
 '(diff-changed ((t (:foreground "MediumBlue"))))
 '(diff-context ((t (:foreground "Black"))))
 '(diff-file-header ((t (:foreground "Red" :background "White"))))
 '(diff-header ((t (:foreground "Red"))))
 '(diff-hunk-header ((t (:foreground "White" :background "Salmon"))))
 '(diff-index ((t (:foreground "Green"))))
 '(diff-nonexistent ((t (:foreground "DarkBlue"))))
 '(diff-removed ((t (:foreground "DarkMagenta"))))
 '(show-trailing-whitespace ((t (:background "Gold"))))
 '(split-height-threshold 120)
 '(tabbar-default ((((class color grayscale) (background dark)) (:inherit variable-pitch :background "gray50" :foreground "white" :height 0.9))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "white" :box (:line-width 1 :color "white" :style pressed-button))))))

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

(setq org-directory "~/org/")
(setq org-default-notes-file "~/org/notes.org")
(setq org-hide-leading-stars t)
(setq org-completion-use-ido t)
(setq org-return-follows-link t)
(setq org-refile-use-outline-path t)
(setq org-log-done 'time)
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq org-remember-templates
    '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "things.org" "Tasks")
      ("Journal"   ?j "** %^{Head Line} %U %^g\n%i%?"  "journal.org")
      ("Clipboard" ?c "** %^{Head Line} %U %^g\n%c\n%?"  "journal.org")
      ("Review" ?r "** %t\n%[~/org/.daily_review.txt]\n" "journal.org")

      )
    )

(setq org-capture-templates
    '(
      ("t" "Todo" entry (file+headline "~/org/things.org" "Tasks")
         "* TODO %^{Brief Description} %^g\n%?\nAdded: %U")
      ("p" "Project" entry (file+headline "~/org/things.org" "Projects")
         "* %^{Project} %^g\n%?\nAdded: %U")
      ("a" "Agenda" entry (file+headline "~/org/things.org" "Agenda")
         "* %?\n  %i\n  %a")
      ))

;; Octave
;;(autoload 'octave-mode "octave-mod" nil t)
;;(setq auto-mode-alist
;;      (cons '("\\.m$" . octave-mode) auto-mode-alist))
;;(autoload 'run-octave "octave-inf" nil t)

;; Coffeescript
;;(
;;require 'coffee-mode)

;;(defun coffee-custom ()
;;  "coffee-mode-hook"
;; 
;;  ;; CoffeeScript uses two spaces.
;;  (set (make-local-variable 'tab-width) 2)
;;  (setq 'indent-tabs-mode nil)
;; 
;;  ;; *Messages* spam
;;  (setq coffee-debug-mode t)
;; 
;;  ;; Emacs key binding
;;  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
;; 
;;)
;; 
;;(add-hook 'coffee-mode-hook '(lambda () (coffee-custom)))


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


;; comint
(define-key comint-mode-map (kbd "M-") 'comint-next-input)
(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

(global-set-key (kbd "C-c SPC") 'er/expand-region)

(global-set-key (kbd "C-c s") 'magit-status)

;; org-mode
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c c") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "<M-return>") 'org-insert-heading)
(global-set-key (kbd "<C-return>") 'org-insert-heading-respect-content)


;(projectile-mode +1)
;(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
