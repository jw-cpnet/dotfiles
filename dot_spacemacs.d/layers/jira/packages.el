;;; packages.el --- jira layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: jian <jian@jian-asus-desktop>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; JIRA integration layer for Spacemacs.
;; This layer provides JIRA integration through git-commit-jira-prefix and jira.el packages.

;;; Code:

(defconst jira-packages
  '(
    (git-commit-jira-prefix :location (recipe :fetcher github
                                              :repo "chrisbarrett/git-commit-jira-prefix"
                                              :files ("*.el")))
    jira
    )
  "The list of Lisp packages required by the jira layer.")

(defun jira/init-git-commit-jira-prefix ()
  "Initialize git-commit-jira-prefix package."
  (use-package git-commit-jira-prefix
    :defer t
    :init
    (autoload 'git-commit-jira-prefix-init "git-commit-jira-prefix")
    :config
    (with-eval-after-load 'git-commit
      (git-commit-jira-prefix-init))))

(defun jira/init-jira ()
  "Initialize jira package."
  (use-package jira
    :defer t
    :init
    (spacemacs/set-leader-keys "aj" 'jira-issues)))

(defun jira/post-init-jira ()
  "Post-initialize jira package."

  ;; Set up keybindings for jira-issues-mode
  (add-hook 'jira-issues-mode-hook
            (lambda ()
              (when (boundp 'jira-issues-mode-map)
                (evilified-state-evilify-map jira-issues-mode-map
                  :mode jira-issues-mode
                  :bindings
                  (kbd "l") 'jira-issues-menu
                  (kbd "?") 'jira-issues-actions-menu)))))

;;; packages.el ends here
