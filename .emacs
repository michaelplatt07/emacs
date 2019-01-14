;; If you are experiencing a package not found error then you need to run the command package-refresh-contents

;; Setting up the list of packages that will be automatically installed.
;; Add any new desired packages to this list.  They are white space delimitted.
(setq package-list '(hl-todo auto-complete tabbar highlight-parentheses json-mode json-reformat pug-mode anything-tramp php-mode batch-mode autopair flex-autopair rjsx-mode smart-mode-line flymd))

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
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
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

;; Set up smart line mode
(require 'smart-mode-line)
(sml/setup)
(setq sml/theme 'dark)
(add-to-list 'sml/replacer-regexp-list '("~/Desktop/RecipeApp/RecipeBookServer/*" ":RecipeServer:") t)
(add-to-list 'sml/replacer-regexp-list '("~/Desktop/RecipeApp/RecipeBookUi/*" ":RecipeEJS:") t)
(add-to-list 'sml/replacer-regexp-list '("~/Desktop/RecipeApp/recipe-book-react/*" ":RecipeReact:") t)
(smart-mode-line-enable t)

;; Setting global auto complete mode.
(require 'auto-complete)
(global-auto-complete-mode t)

;; Setting global flex autopair mode.
;; (require 'flex-autopair)
;; (flex-autopair-mode 1)

;; Using autopair because it deletes things nicely.
(require 'autopair)
(autopair-global-mode 1)

;; Setting global parentheses highlight mode.
(require `highlight-parentheses)
(global-highlight-parentheses-mode t)

;; Declaring some major modes for custom types of files.  More of a convenience than anything else.
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; Yapify to allow for formatting of Python Code to PEP standard.
;; Requires pip to be installed and yapf installed.
;;(require 'yapfify)
;;(add-hook 'python-mode-hook 'yapf-mode)

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
 	(insert "cd /home/michael/Desktop/VideoGame/tloll")
 	;; Execute
 	(comint-send-input)
 	(insert "gradle build run")
 	(comint-send-input)
 	(other-window 1)
 	)

;; Add TODO key binding for Java mode.
(defun insert-todo-java-mode ()
  (interactive)
  (insert "// TODO(map) : ")
  )
(defun my-java-mode-config ()
  "For use in `html-mode-hook'."
  (local-set-key (kbd "C-t") 'insert-todo-java-mode)
  )
(add-hook 'java-mode-hook 'my-java-mode-config)

;; Add TODO key binding for JavaScript mode.
(defun insert-todo-javascript-mode ()
  (interactive)
  (insert "// TODO(map) : ")
  )
(defun my-javascript-mode-config ()
  (local-set-key (kbd "C-t") 'insert-todo-javascript-mode)
  )
(add-hook 'js-mode-hook 'my-javascript-mode-config)

;; Add TODO key binding for Python mode.
(defun insert-todo-python-mode ()
  (interactive)
  (insert "# TODO(map) : ")
  )
(defun my-python-mode-config ()
  (local-set-key (kbd "C-t") 'insert-todo-python-mode)
  )
(add-hook 'python-mode-hook 'my-python-mode-config)

;; Add TODO key binding for HTML mode.
(defun insert-todo-html-mode ()
  (interactive)
  (insert "<!-- TODO(map) : -->")
  )
(defun my-html-mode-config ()
  (local-set-key (kbd "C-t") 'insert-todo-html-mode)
  )
(add-hook 'html-mode-hook 'my-html-mode-config)

;; Add TODO key binding for C++ mode.
(defun insert-todo-c++-mode ()
  (interactive)
  (insert "// TODO(map) : ")
  )
(defun my-c++-mode-config ()
  (local-set-key (kbd "C-t") 'insert-todo-c++-mode)
  )
(add-hook 'c++-mode-hook 'my-c++-mode-config)

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

;; Set python files to use 4 spaces instead of tabs.
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)))

;; Some additional settings.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(size-indication-mode 1)
(delete-selection-mode 1)
(setq org-log-done t)
(setq scroll-step 1) ;;Scroll one line at a time
(setq backup-directory-alist `(("." . "~/.saves")))
