;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '("~/dotspacemacs")
   dotspacemacs-configuration-layers
   '(
     auto-completion
     emacs-lisp
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
     dockerfile
     ess
     octave
     trello
     unimpaired
     shell
     gnus
     python
     )
   dotspacemacs-additional-packages '(
                                      beacon
                                      )
   dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 10
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 3
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(molokai
                         spacemacs-dark
                         lush
                         monokai
                         pastels-on-dark
                         reverse
                         soothe
                         toxi)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup 't
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers 'nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
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
  (define-key evil-hybrid-state-map (kbd "C-S-n") (lambda () (interactive) (next-line 5)))
  (define-key evil-hybrid-state-map (kbd "C-S-p") (lambda () (interactive) (previous-line 5)))

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

  (when (file-exists-p "~/Dropbox/config.el") (load-file "~/Dropbox/config.el")) ;; stuff that requires absolute paths
  (when (file-exists-p "~/Dropbox/jabber-config.el") (load-file "~/Dropbox/jabber-config.el"))

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

	(setq org-hide-leading-stars 't)
	)

  (setq tramp-default-method "ssh")
  (setq tramp-default-user "schachma")
  (setq helm-buffer-max-length 50)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))

 )
