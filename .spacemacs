;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/dotspacemacs")
   dotspacemacs-configuration-layers
   '(
	 html
	 javascript
     helm
     emacs-lisp
     auto-completion
     git
     org
     (spell-checking :variables spell-checking-enable-by-default nil)
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     version-control
     c-c++
     jabber
     extra-langs
     ycmd
     ranger
     themes-megapack
     docker
     ess
     octave
     gnus
     python
     search-engine
     chrome
     command-log
     games
     xkcd
     emoji
     selectric
	 markdown
     )
   dotspacemacs-additional-packages '(
                                      centimacro
                                      beacon
                                      keyfreq
                                      engine-mode
                                      fireplace
                                      openwith
                                      evil-quickscope
                                      outline-magic
                                      lua-mode
									  all-the-icons
									  dash
									  doom-themes
									  exwm
									  exwm-x
                                      (accelerate :location (recipe :fetcher wiki))
                                      )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(
									smartparens
									)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 10
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(default
						 spacemacs-dark
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   ;; dotspacemacs-default-font '("Source Code Pro"
   ;;                             :size 13
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts t
   dotspacemacs-large-file-size 5
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup changed
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq exec-path-from-shell-check-startup-files nil)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (setq user-dropbox-directory (expand-file-name "~/Dropbox"))
  (evil-leader/set-key "jl" 'end-of-line)
  (evil-leader/set-key "jh" 'beginning-of-line)

  (setq-default c-basic-offset 4
				tab-width 4
				indent-tabs-mode t)

  (add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

  (beacon-mode 1)

  (set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/ycmd/ycmd")))
  (set-variable 'ycmd-global-config (expand-file-name "~/global_config.py"))

  (setq evil-escape-key-sequence "kj")

  (define-key evil-hybrid-state-map (kbd "C-h") 'delete-backward-char)

  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
	(let ((inhibit-read-only t))
	  (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  ;; make sure timestamps are in english
  (setq system-time-locale "C")

  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  (defun my-nop (&rest r) :ok)
  (advice-add 'orgtrello-controller-migrate-user-setup :override #'my-nop)
  (setq evil-move-beyond-eol 't)

  (setq my-private-config-file (concat user-dropbox-directory "config.el"))
  (setq my-private-jabber-config-file (concat user-dropbox-directory "jabber-config.el"))
  (when (file-exists-p my-private-config-file) (load-file my-private-config-file)) ;; stuff that requires absolute paths
  (when (file-exists-p my-private-jabber-config-file) (load-file my-private-jabber-config-file))

  ;; start directory in windows
  (setq default-directory (concat (getenv "HOME") "/"))

  (with-eval-after-load 'org
	(add-hook 'org-capture-mode-hook 'evil-insert-state)

	(spacemacs|disable-company org-mode)
	(spacemacs|disable-company org-capture-mode)
	(setq org-log-reschedule 'time)
	(setq org-capture-templates
		  '(("i" "Inbox" entry (file+headline "~/Dropbox/org/gtd.org" "Inbox")
			 "* %?\nEntry date: %U")
			("s" "Someday" entry (file+headline "~/Dropbox/org/gtd.org" "Someday")
			 "* %?\nEntry date: %U")
			("t" "Task" entry (file+headline "~/Dropbox/org/gtd.org" "Tasks")
			 "* TODO %?\nEntry date: %U")
			("c" "Calendar" entry (file+headline "~/Dropbox/org/gtd.org" "Calendar")
			 "* %?\nEntry date: %U")
			("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
			 "* %?\nEntry date: %U")
			("b" "Birthday" entry (file+headline "~/Dropbox/org/birthdays.org" "Birthdays")
			 "* Brithday of %^{Name: }\n%^{Date}t%?")))

	(setq org-refile-targets
		  '((nil :maxlevel . 1)
			("~/Dropbox/org/gtd.org" :tag . "proj")
			(org-agenda-files :maxlevel . 1)))

	(setq org-agenda-files (list
							"~/Dropbox/org/birthdays.org"
							"~/Dropbox/org/gtd.org"
							"~/Dropbox/org/journal.org"))

	(setq org-agenda-custom-commands
		  '(("w" "Agenda and Work-related tasks"
			 ((agenda "")
			  (tags-todo "WORK+TODO=\"NEXT\"")
			  (tags-todo "WORK+TODO=\"WAITING\"")
			  (tags-todo "WORK+TODO=\"TODO\"")
			  ))
			("p" "Agenda and private tasks"
			 ((agenda "")
			  (tags-todo "PRIVATE+TODO=\"NEXT\"")
			  (tags-todo "PRIVATE+TODO=\"WAITING\"")
			  (tags-todo "PRIVATE+TODO=\"TODO\"")
			  ))
			("o" "Agenda and private tasks"
			 ((agenda "")
			  (tags-todo "PRIVATE+TODO=\"WAITING\"")
			  (tags-todo "WORK+TODO=\"WAITING\"")
			  (tags-todo "WORK+TODO=\"NEXT\"")
			  (tags-todo "PRIVATE+TODO=\"NEXT\"")
			  ))
			))

	(setq org-todo-keyword-faces
		  '(("TODO" . "pink")
			("NEXT" . "red")
			("APPT" . "yellow")
			("STARTED" . "LightSkyBlue")
			("WAITING" . "CornflowerBlue")))
	;; allow for export=>beamer by placing
	(setq org-startup-with-inline-images nil)

	(setq org-hide-leading-stars 't)

	(spacemacs/set-leader-keys-for-major-mode 'org-mode
	  "gg" 'org-preview-latex-fragment)

	(setq org-publish-project-alist
		  '(("thesis"
			 :base-directory "~/Dropbox/org/thesis"
			 :base-extension "org"
			 :publishing-directory "~/theses/schachmann"
			 :publishing-function (org-ascii-publish-to-ascii)
			 :html-preamble nil
			 :html-postamble nil)
			("thesis-html"
			 :base-directory "~/Dropbox/org/thesis"
			 :base-extension "org"
			 :publishing-directory "/import/home/schachma/thesis"
			 :publishing-function (org-html-publish-to-html)
			 :html-preamble nil
			 :html-postamble nil)))

	(require 'ox-latex)
	(add-to-list 'org-latex-classes
				 '("dima"
				   "\\documentclass{report}"
				   ("\\chapter{%s}" . "\\chapter*{%s}")
				   ("\\section{%s}" . "\\section*{%s}")
				   ("\\subsection{%s}" . "\\subsection*{%s}")
				   ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
				 )

	)
  (setq bookmark-default-file (concat user-dropbox-directory "bookmarks"))
  (setq tramp-default-method "ssh")
  (setq tramp-default-user "schachma")
  (setq helm-buffer-max-length 50)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
	 (sh . t)
	 ))

  (setq browse-url-browser-function (quote browse-url-chromium)
		browse-url-chromium-program "google-chrome")

  (defengine leo "https://dict.leo.org/ende/index_de.html#/search=%s&searchLoc=0&resultOrder=basic&multiwordShowSingle=on")
  (add-to-list 'search-engine-alist
			   '(leo
				 :name "Leo"
				 :url "https://dict.leo.org/ende/index_de.html#/search=%s&searchLoc=0&resultOrder=basic&multiwordShowSingle=on"))

  (setq openwith-associations '(("\\.pdf\\'" "evince" (file))))
  (setq jabber-alert-presence-hooks nil)
  (openwith-mode t)
  (put 'narrow-to-page 'disabled nil)

  (require 'accelerate)
  (accelerate evil-previous-line 5)
  (accelerate evil-next-line 5)
  ;;  (accelerate backward-char 3)
  ;;  (accelerate forward-char 3)
  ;;  (accelerate dired-previous-line 2)
  ;;  (accelerate dired-next-line 2)
  ;;  (accelerate speedbar-prev 2)
  ;;  (accelerate speedbar-next 2)
  (add-to-list 'auto-mode-alist '("\\.eml\\'" . org-mode))
  (global-evil-quickscope-mode 1)
  (global-evil-mc-mode  1)
  (setq
   ess-use-auto-complete nil
   ;; ac-auto-show-menu 1
   ;; ac-candidate-limit nil
   ac-delay 5
   ;; ac-disable-faces (quote (font-lock-comment-face font-lock-doc-face))
   ;; ac-ignore-case 'smart
   ;; ac-menu-height 10
   ;; ac-quick-help-delay 1.5
   ;; ac-quick-help-prefer-pos-tip t
   ;; ac-use-quick-help nil
   )
  ;; (set-face-attribute 'default nil :height 170)
  (defun sigint-r ()
	"kill send Ctrl+C to running R process."
	(interactive)
	(signal-process "R" 2))

  (evil-leader/set-key "or" 'sigint-r)
  (setq projectile-use-git-grep t)

  ;; turn off mouse clicks!
  ;; (dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
  ;; 			   [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
  ;; 			   [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
  ;; 			   [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
  ;; 			   [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  ;; 	(global-unset-key k))
  (setq  org-confirm-babel-evaluate 'nil)
  (setq  explicit-shell-file-name "/bin/sh")
  ;; (setq org-latex-pdf-process "/usr/bib/rubber --pdf %f")
  ;; (setq org-latex-to-pdf-process (list "latexmk -pdf %f"))
  ;; "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")
  ;; (setq org-latex-pdf-process (list "pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-pdf-process (list "rubber -f --pdf %f"))

  ;; setxkbmap us -option ctrl:nocaps
  ;; setxkbmap de -option ctrl:nocaps


  (defun setxkbmap-de ()
	"Set keyboard layout to german."
	(interactive)
	(progn
	  (shell-command "setxkbmap de -option ctrl:nocaps")
	  (message "GERMAN keyboard")))

  (defun setxkbmap-us ()
	"Set keyboard layout to english."
	(interactive)
	(progn
	  (shell-command "setxkbmap us -option ctrl:nocaps")
	  (message "ENGLISH keyboard")))

  (evil-leader/set-key "od" 'setxkbmap-de)
  (evil-leader/set-key "ou" 'setxkbmap-us)
  (defun occur-non-ascii ()
	"Find any non-ascii characters in the current buffer."
	(interactive)
	(occur "[^[:ascii:]]"))

  (defun save-excursion-advice (old-function &rest arguments)
    "save excursion advice"
    (save-excursion (apply old-function arguments)))
  (advice-add #'ess-eval-buffer :around #'save-excursion-advice)
  (setq org-ascii-text-width most-positive-fixnum)
  ;; (add-hook 'org-mode-hook (lambda () (git-gutter+-mode -1)))
  (setq git-gutter+-disabled-modes '(org-mode asm-mode image-mode))
  (eval-after-load 'outline
    '(progn
       (require 'outline-magic)
       (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))

  (defun duplicate-current-line-or-region (arg)
	"Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
	(interactive "p")
	(let (beg end (origin (point)))
	  (if (and mark-active (> (point) (mark)))
		  (exchange-point-and-mark))
	  (setq beg (line-beginning-position))
	  (if mark-active
		  (exchange-point-and-mark))
	  (setq end (- (line-end-position) 1))
	  (let ((region (buffer-substring-no-properties beg end)))
		(dotimes (i arg)
		  (goto-char end)
		  (newline)
		  (insert region)
		  (setq end (point)))
		(goto-char (+ origin (* (length region) arg) arg))
		)
	  (next-line)
	  ))

  (require 'doom-themes)
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-molokai t)

  (require 'exwm)
  (require 'exwm-config)
  (require 'exwm-x)
  (require 'exwm-x-example)
  (exwm-config-default)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zonokai-theme zenburn-theme zen-and-art-theme yapfify xkcd ws-butler wolfram-mode window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme typit mmt twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org thrift tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stan-mode spacemacs-theme spaceline powerline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode seti-theme selectric-mode scss-mode scad-mode sass-mode reverse-theme restart-emacs ranger rainbow-delimiters railscasts-theme qml-mode pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox spinner pacmacs outline-magic orgit organic-green-theme org-projectile org-present org org-pomodoro alert log4e gntp org-plus-contrib org-download org-bullets openwith open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme matlab-mode material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lua-mode lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint light-soap-theme less-css-mode keyfreq js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme jabber fsm ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hydra hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gmail-message-mode ham-mode markdown-mode html-to-markdown gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md gandalf-theme flyspell-correct-helm flyspell-correct flycheck-ycmd flycheck-pos-tip pos-tip flycheck flx-ido flx flatui-theme flatland-theme fireplace firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse exwm-x start-menu dmenu switch-window config-parser exwm xelb expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-quickscope evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight ess-smart-equals ess-R-object-popup ess-R-data-view ctable ess julia-mode espresso-theme engine-mode emoji-cheat-sheet-plus emmet-mode elisp-slime-nav edit-server dumb-jump dracula-theme doom-themes dockerfile-mode docker json-mode tablist magit-popup docker-tramp json-snatcher json-reformat django-theme disaster diminish diff-hl define-word darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-ycmd ycmd pkg-info request-deferred request deferred epl company-web web-completion-data company-tern dash-functional tern company-statistics company-emoji company-c-headers company-anaconda company command-log-mode column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmake-mode clues-theme clean-aindent-mode clang-format cherry-blossom-theme centimacro busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-map bind-key beacon seq badwolf-theme auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed arduino-mode apropospriate-theme anti-zenburn-theme anaconda-mode pythonic f s ample-zen-theme ample-theme all-the-icons font-lock+ dash alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async accelerate ac-ispell auto-complete popup 2048-game quelpa package-build))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
