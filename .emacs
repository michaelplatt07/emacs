;; If you are experiencing a package not found error then you need to run the command package-refresh-contents

;; Setting up the list of packages that will be automatically installed.
;; Add any new desired packages to this list.  They are white space delimitted.
(setq package-list '(hl-todo auto-complete tabbar highlight-parentheses json-mode json-reformat pug-mode php-mode autopair flex-autopair rjsx-mode smart-mode-line flymd modalka csharp-mode))

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

;; Handling requires
(require 'smart-mode-line)
(require 'auto-complete)
(require 'autopair)
(require `highlight-parentheses)

;; Define modalka mode to make this a modal editor.
(require 'modalka)

(add-to-list 'load-path "~/.emacs.d/templates/")
(load "my-templates")
(load "todo-templates")
(load "comment-block-templates")

;; Color theming.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(global-hl-line-mode t)
 '(global-hl-todo-mode t)
 '(hl-todo-activate-in-modes
   (quote
    (java-mode emacs-lisp-mode python-mode c++-mode javascript-mode js-mode ruby-mode html-mode)))
 '(ido-mode t nil (ido))
 '(package-selected-packages
   (quote
    (modalka php-mode anything-tramp pug-mode json-mode highlight-parentheses tabbar auto-complete hl-todo))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Custom Occur because I want to swap buffers so I can select it.
(defun custom-occur ()
  (interactive)
  (call-interactively #'occur)
  (other-window 1)
  )

;; Custom open window because I want to swap buffers when I split.
(defun custom-split-horizontal ()
  (interactive)
  (call-interactively #'split-window-right)
  (other-window 1)
  )

;; Custom open window because I want to swap buffers when I split.
(defun custom-split-vertical ()
  (interactive)
  (call-interactively #'split-window-below)
  (other-window 1)
  )

;; Custom open window because I want to swap buffers when I split.
(defun custom-open ()
  (interactive)
  (call-interactively #'ido-find-file)
  )

;; Custom comment a line because apparently emacs 24 doesn't have the nice (comment-line) command.
(defun custom-comment-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position))
  )

(defun insert-grep-params (regex search-dir file-type)
  (if (string-equal search-dir "")
      (setq search-dir default-directory)
    )
  (kill-whole-line)
  (insert (format "grep -rnw %s --include=\\\%s -e %s" search-dir file-type regex))  
  )

(defun custom-grep-function ()
  (interactive)
  (let (regex search-dir file-type)
    (setq regex (read-string "Regex: "))
    (setq search-dir (read-string "Directory to search (leave blank for current dir): "))
    (setq file-type (read-string "File type to search: "))
    (add-hook 'minibuffer-setup-hook (lambda () (insert-grep-params regex search-dir file-type)))
    (execute-extended-command nil "grep")
    (remove-hook 'minibuffer-setup-hook (lambda () (insert-grep-params regex search-dir file-type)))
    )
  )

;; Inserting custom templated docstrings for things we might need.
(defun custom-insert-docs ()
  (interactive)
  (let (doc_type)
    (setq doc_type (read-string "Enter Doc Type:"))
    (if (string-equal doc_type "swagger")
	(if (string-equal major-mode "rjsx-mode")
	    (get-javascript-swagger)
	  )
      )
    (when (string-equal doc_type "doc")
      (if (string-equal major-mode "java-mode")
	  (get-java-doc)
	)
      (if (string-equal major-mode "python-mode")
	(get-python-doc)
	)
      )
    )
  )

;; Setting up TODO insert calls.
(defun custom-insert-todos ()
  (interactive)
  (print major-mode)
  (if (string-equal major-mode "java-mode")
      (insert-java-todo)
    )
  (if (string-equal major-mode "js-mode")
      (insert-javascript-todo)
    )
  (if (string-equal major-mode "rjsx-mode")
      (insert-javascript-todo)
    )
  (if (string-equal major-mode "python-mode")
      (insert-python-todo)
    )
  (if (string-equal major-mode "c++-mode")
      (insert-cpp-todo)
    )
  (if (string-equal major-mode "csharp-mode")
      (insert-csharp-todo)
    )
  (if (string-equal major-mode "html-mode")
      (insert-html-todo)
    )
  (if (string-equal major-mode "php-mode")
      (insert-php-todo)
    )
  (if (string-equal major-mode "ruby-mode")
      (insert-ruby-todo)
    )
  (if (string-equal major-mode "org-mode")
      (insert-org-todo)
    ))

(defun custom-insert-comment-block ()
  (interactive)
  (if (string-equal major-mode "java-mode")
      (insert-java-comment-block)
    )
  (if (string-equal major-mode "js-mode")
      (insert-javascript-comment-block)
    )
  (if (string-equal major-mode "rjsx-mode")
      (insert-javascript-comment-block)
    )
  (if (string-equal major-mode "python-mode")
      (insert-python-comment-block)
    )
  (if (string-equal major-mode "c++-mode")
      (insert-cpp-comment-block)
    )
  (if (string-equal major-mode "csharp-mode")
      (insert-csharp-comment-block)
    )
  (if (string-equal major-mode "html-mode")
      (insert-html-comment-block)
    )
  (if (string-equal major-mode "php-mode")
      (insert-php-comment-block)
    )
  (if (string-equal major-mode "ruby-mode")
      (insert-ruby-comment-block)
    )
  )

;; Setting up auto insert for various filex.
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.java\\'" . "Java Skeleton")
     '((file-name-base (buffer-file-name))
       "/**" > \n
       "* @author: MAPlatt" > \n
       "* @date: " (format-time-string "%Y-%m-%d") > \n
       "* @license: " > \n
       "*/" > \n \n \n)))
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.py\\'" . "Python Skeleton")
     '(
       (file-name-nondirectory (buffer-file-name))
       "# @author: MAPlatt" > \n
       "# @date: " (format-time-string "%Y-%m-%d") > \n
       "# @license: " > \n \n \n)))
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.js\\'" . "JavaScript Skeleton")
     '(
       (file-name-nondirectory (buffer-file-name))
       "/**" > \n
       "* @author: MAPlatt" > \n
       "* @date: " (format-time-string "%Y-%m-%d") > \n
       "* @license: " > \n
       "*/" > \n \n \n)))
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
     '((file-name-nondirectory (buffer-file-name))
       "/**" \n
       "* @author: MAPlatt" > \n
       "* @date: " (format-time-string "%Y-%m-%d") > \n
       "* @license: " > \n
       " */" > \n \n
       "#include <iostream>" \n \n
       "using namespace std;" \n \n \n)))
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.org\\'" . "Org Mode skeleton")
     'get-org-skeleton))

;; Modal key bindings
(define-key modalka-mode-map (kbd "a") 'ignore) ; NOTE(map) : Available
(define-key modalka-mode-map (kbd "A") #'mark-whole-buffer)
(define-key modalka-mode-map (kbd "b") 'switch-to-buffer)
(define-key modalka-mode-map (kbd "B") 'list-buffers)
(define-key modalka-mode-map (kbd "c") 'custom-insert-comment-block)
(define-key modalka-mode-map (kbd "C") 'kill-ring-save)
(define-key modalka-mode-map (kbd "d") #'left-word)
(define-key modalka-mode-map (kbd "D") #'backward-char)
(define-key modalka-mode-map (kbd "e") #'end-of-buffer)
(define-key modalka-mode-map (kbd "E") #'move-end-of-line)
(define-key modalka-mode-map (kbd "f") #'right-word)
(define-key modalka-mode-map (kbd "F") #'forward-char)
(define-key modalka-mode-map (kbd "g") 'keyboard-quit)
(define-key modalka-mode-map (kbd "G") 'vc-diff)
(define-key modalka-mode-map (kbd "h") #'beginning-of-buffer)
(define-key modalka-mode-map (kbd "H") #'move-beginning-of-line)
(define-key modalka-mode-map (kbd "i") #'modalka-mode)
(define-key modalka-mode-map (kbd "I") 'custom-insert-docs)
(define-key modalka-mode-map (kbd "j") 'ignore) ; NOTE(map) : Available
(define-key modalka-mode-map (kbd "J") 'ignore) ; NOTE(map) : Available
(define-key modalka-mode-map (kbd "k") 'kill-this-buffer)
(define-key modalka-mode-map (kbd "K") 'kill-whole-line)
(define-key modalka-mode-map (kbd "l") 'goto-line)
(define-key modalka-mode-map (kbd "L") #'linum-mode)
(define-key modalka-mode-map (kbd "m") 'set-mark-command)
(define-key modalka-mode-map (kbd "M") 'flymd-flyit)
(define-key modalka-mode-map (kbd "n") #'forward-paragraph)
(define-key modalka-mode-map (kbd "N") #'next-line)
(define-key modalka-mode-map (kbd "o") 'custom-open)
(define-key modalka-mode-map (kbd "O") 'ignore)
(define-key modalka-mode-map (kbd "p") #'backward-paragraph)
(define-key modalka-mode-map (kbd "P") #'previous-line)
(define-key modalka-mode-map (kbd "q") 'ignore) ; NOTE(map) : Available
(define-key modalka-mode-map (kbd "Q") 'save-buffers-kill-terminal)
(define-key modalka-mode-map (kbd "r") 'query-replace)
(define-key modalka-mode-map (kbd "R") 'revert-buffer)
(define-key modalka-mode-map (kbd "s") 'save-buffer)
(define-key modalka-mode-map (kbd "S") 'shell)
(define-key modalka-mode-map (kbd "t") 'custom-insert-todos)
(define-key modalka-mode-map (kbd "T") #'toggle-frame-fullscreen)
(define-key modalka-mode-map (kbd "u") 'ignore) ; NOTE(map) : Available
(define-key modalka-mode-map (kbd "U") 'custom-grep-function)
(define-key modalka-mode-map (kbd "v") 'ignore) ; NOTE(map) : Available
(define-key modalka-mode-map (kbd "V") 'yank)
(define-key modalka-mode-map (kbd "w") 'isearch-forward)
(define-key modalka-mode-map (kbd "W") 'custom-occur)
(define-key modalka-mode-map (kbd "x") 'execute-extended-command)
(define-key modalka-mode-map (kbd "X") 'kill-region)
(define-key modalka-mode-map (kbd "y") 'ignore) ; NOTE(map) : Available
(define-key modalka-mode-map (kbd "Y") 'ignore) ; NOTE(map) : Available
(define-key modalka-mode-map (kbd "z") 'undo)
(define-key modalka-mode-map (kbd "Z") 'ignore) ; NOTE(map) : Available

(define-key modalka-mode-map (kbd ".") 'ignore) ; NOTE(map) : Available
(define-key modalka-mode-map (kbd ",") 'ignore) ; NOTE(map) : Available
(define-key modalka-mode-map (kbd "0") 'delete-window)
(define-key modalka-mode-map (kbd "1") 'delete-other-windows)
(define-key modalka-mode-map (kbd "2") 'custom-split-vertical)
(define-key modalka-mode-map (kbd "3") 'custom-split-horizontal)
(define-key modalka-mode-map (kbd "/") 'custom-comment-line)
(define-key modalka-mode-map (kbd "<tab>") 'other-window)
(define-key modalka-mode-map (kbd "<home>") #'beginning-of-buffer)
(define-key modalka-mode-map (kbd "<end>") #'end-of-buffer)
(define-key modalka-mode-map (kbd "<prior>") #'scroll-down-command)
(define-key modalka-mode-map (kbd "<next>") #'scroll-up-command)

;; Custom editing keybindings.
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
    (define-key map (kbd "C-c m") 'null)
    (define-key map (kbd "C-x z") 'null)
    (define-key map (kbd "C-x C-z") 'null)
    (define-key map (kbd "C-r") 'query-replace)
    (define-key map (kbd "C-SPC") 'auto-complete)
    (define-key map (kbd "M-d") 'diff)
    (define-key map (kbd "M-r") 'revert-buffer)
    (define-key map (kbd "M-i") #'modalka-mode)
    map)
  "my-keys-minor-mode keymap.")

;; Defining the key mode.
(define-minor-mode my-keys-minor-mode
  "A minor mode with customized key bindings to ensure they override the major modes."
  :init-value t
  :lighter " my-keys")

;; Set up smart line mode
(sml/setup)
(setq sml/theme 'dark)
(add-to-list 'sml/replacer-regexp-list '("~/Desktop/RecipeApp/RecipeBookServer/*" ":RecipeServer:") t)
(add-to-list 'sml/replacer-regexp-list '("~/Desktop/RecipeApp/RecipeBookUi/*" ":RecipeEJS:") t)
(add-to-list 'sml/replacer-regexp-list '("~/Desktop/RecipeApp/recipe-book-react/*" ":RecipeReact:") t)
(smart-mode-line-enable t)

;; Setting global auto complete mode.
(global-auto-complete-mode t)

;; Using autopair because it deletes things nicely.
(autopair-global-mode 1)

;; Setting global parentheses highlight mode.
(global-highlight-parentheses-mode t)

;; Declaring some major modes for custom types of files.  More of a convenience than anything else.
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))


;; NOTE(map) : Configuration settings
;; Enabling key mode.
(my-keys-minor-mode 1)

;; Setting global auto complete mode.
(global-auto-complete-mode t)

;; Using autopair because it deletes things nicely.
(autopair-global-mode 1)

;; Setting global parentheses highlight mode.
(global-highlight-parentheses-mode t)

;; Some additional settings.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(size-indication-mode 1)
(delete-selection-mode 1)

(auto-insert-mode t)
;; Default enable Modalka mode
(modalka-global-mode 1)

(setq-default cursor-type 'box)
(setq modalka-cursor-type '(bar . 1))
(setq inhibit-startup-screen t)
(setq org-log-done t)
(setq scroll-step 1) ;;Scroll one line at a time
(setq backup-directory-alist `(("." . "~/.saves")))
;; NOTE(map) : END configurations
