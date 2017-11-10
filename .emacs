;; Currently installed packages are:
;; hl-todo, auto-complete, tabbar, highlight-parentheses, json-mode, json-reformat, javadoc-lookup

;; Setting up the list of packages that will be automatically installed.
;; Add any new desired packages to this list.  They are white space delimitted.
(setq package-list '(hl-todo auto-complete tabbar highlight-parentheses json-mode json-reformat pug-mode anything-tramp php-mode batch-mode))

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

;; Declaring some major modes for custom types of files.  More of a convenience than anything else.
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode))

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
 '(global-hl-line-mode t)
 '(global-hl-todo-mode t)
 '(hl-todo-activate-in-modes
   (quote
    (java-mode emacs-lisp-mode python-mode c++-mode javascript-mode js-mode batch-mode)))
 '(ido-mode t nil (ido))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (batch-mode php-mode anything-tramp pug-mode json-mode highlight-parentheses tabbar auto-complete hl-todo)))
 '(tabbar-mode t nil (tabbar)))

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

;; Creating the key map the minor mode will use.
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-tab>") 'other-window)
     (define-key map (kbd "C-k") 'kill-this-buffer)
     (define-key map (kbd "C-.") 'next-buffer)
     (define-key map (kbd "C-,") 'previous-buffer)
     (define-key map (kbd "C-M-f") 'forward-char)
     (define-key map (kbd "C-M-d") 'backward-char)
     (define-key map (kbd "C-f") 'right-word)
     (define-key map (kbd "C-d") 'left-word)
     (define-key map (kbd "C-M-n") 'next-line)
     (define-key map (kbd "C-M-p") 'previous-line)
     (define-key map (kbd "C-n") 'forward-paragraph)
     (define-key map (kbd "C-p") 'backward-paragraph)
     (define-key map (kbd "C-/") nil)
     (define-key map (kbd "C-z") 'undo)
     (define-key map (kbd "M-b") 'kill-region)
     (define-key map (kbd "M-c") 'kill-ring-save)
     (define-key map (kbd "M-v") 'yank)
     (define-key map (kbd "M-l") 'goto-line)
     (define-key map (kbd "M-f") 'occur)
     (define-key map (kbd "C-l") 'goto-line)
     (define-key map (kbd "C-b") 'switch-to-buffer)
     (define-key map (kbd "M-m") 'custom-function)
     (define-key map (kbd "C-c m") 'null)
     (define-key map (kbd "C-x z") 'null)
     (define-key map (kbd "C-x C-z") 'null)
     (define-key map (kbd "C-r") 'query-replace)
     (define-key map (kbd "C-SPC") 'auto-complete)
     (define-key map (kbd "M-d") 'diff)
     (define-key map (kbd "C-j") 'javadoc-lookup)
     (define-key map (kbd "M-j") 'javadoc-sort-imports)
     (define-key map (kbd "M-r") 'revert-buffer)
     map)
    "my-keys-minor-mode keymap.")

;; Defining the key mode.
(define-minor-mode my-keys-minor-mode
  "A minor mode with customized key bindings to ensure they override the major modes."
  :init-value t
  :lighter " my-keys")

;; Enabling key mode.
(my-keys-minor-mode 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq scroll-step 1) ;;Scroll one line at a time
(setq backup-directory-alist `(("." . "~/.saves")))
