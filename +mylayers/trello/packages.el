;;; packages.el --- trello layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Dimitri Schachmann <dima@trusty>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `trello-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `trello/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `trello/pre-init-PACKAGE' and/or
;;   `trello/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(setq trello-packages
  '(org-trello))

(defun trello/init-org-trello ()
  (use-package org-trello
    ;; :defer t
	:init (progn
				 (spacemacs/set-leader-keys "ats" 'org-trello/sync-buffer)
				 (spacemacs/set-leader-keys "atc" 'org-trello/add-card-comment)
				 ;; (spacemacs/set-leader-keys-for-minor-mode 'org-trello-mode "os" 'org-trello/sync-buffer)
				 )
	))

;;; packages.el ends here
