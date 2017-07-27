;; Currently installed packages are:
;; hl-todo, auto-complete, tabbar, highlight-parentheses, json-mode, json-reformat, javadoc-lookup

;; Setting up the list of packages that will be automatically installed.
;; Add any new desired packages to this list.  They are white space delimitted.
(setq package-list '(hl-todo auto-complete tabbar highlight-parentheses json-mode json-reformat pug-mode))

;; Adding the melpa package archive for melpa packages.
;; Note: If there is a new archive you'll need to add it like the melpa archive was added.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Refresh the list of packages.
(unless package-archive-contents
  (package-refresh-contents))

;; Loop over the list of packages desired for installing and if they aren't
;; installed go ahead and install them.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Setting global auto complete mode.
(require 'auto-complete)
(global-auto-complete-mode t)

;; Setting global parentheses highlight mode.
(require `highlight-parentheses)
(global-highlight-parentheses-mode t)

;; Yapify to allow for formatting of Python Code to PEP standard.
;; Requires pip to be installed and yapf installed.
;;(require 'yapfify)
;;(add-hook 'python-mode-hook 'yapf-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (deeper-blue)))
 ;; Setting up minor global modes. 
 '(global-hl-line-mode t)
 '(global-hl-todo-mode t)
 '(tabbar-mode t)
 '(hl-todo-activate-in-modes (quote (java-mode emacs-lisp-mode python-mode c++-mode javascript-mode js-mode)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(ido-mode t)
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Sample function that shows how you could shell out if you wanted.
(defun custom-function ()
 	(interactive)
 	(other-window 1)
 	;; Switch to `*shell*'
 	(shell)
 	;; Goto last prompt, clear old input if any, and insert new one
 	(goto-char (point-max))
 	(comint-kill-input)
 	(insert "cd PATH/TO/GRADLE/PROEJCT")
 	;; Execute
 	(comint-send-input)
 	(insert "gradle build run")
 	(comint-send-input)
 	(other-window 1)
 	)

;; Setting up hotkeys for my customization
;; Use C-h k to describe a key so you can modify the hotkey as desired.
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-k") 'kill-this-buffer)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-f") 'right-word)
(global-set-key (kbd "C-d") 'left-word)
(global-set-key (kbd "C-n") 'forward-paragraph)
(global-set-key (kbd "C-p") 'backward-paragraph)
(global-set-key (kbd "C-/") nil)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-b") 'kill-region)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-l") 'goto-line)
(global-set-key (kbd "M-f") 'occur)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "M-m") 'custom-function)
(global-set-key (kbd "C-c m") 'null)
(global-set-key (kbd "C-x z") 'null)
(global-set-key (kbd "C-x C-z") 'null)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-SPC") 'auto-complete)
(global-set-key (kbd "M-d") 'diff)
(global-set-key (kbd "C-j") 'javadoc-lookup)
(global-set-key (kbd "M-j") 'javadoc-sort-imports)
(global-set-key (kbd "M-r") 'revert-buffer)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq scroll-step 1) ;;Scroll one line at a time
(setq backup-directory-alist `(("." . "~/.saves")))
