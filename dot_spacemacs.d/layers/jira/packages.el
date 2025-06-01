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
    (progn
      ;; Set JIRA configuration from environment variables
      (setq jira-base-url (getenv "JIRA_URL")) ;; Jira instance URL
      (setq jira-username "jian.wang@cpnet.io") ;; Jira username (usually, an email)
      ;; API token for Jira
      ;; See https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/
      (setq jira-token (getenv "JIRA_API_TOKEN"))
      (setq jira-token-is-personal-access-token nil)
      ;; (setq jira-api-version 3) ;; Version 2 is also allowed
      )
    :config
    (progn
      ;; Customize Jira table column widths for better alignment
      (with-eval-after-load 'jira-utils
        (setq jira-issues-fields
              '((:key . ((:path . (key))
                         (:columns . 12)  ; Increased from 10
                         (:name . "Key")
                         (:formatter . jira-fmt-issue-key)))
                (:priority-name . ((:path . (fields priority name))
                                   (:columns . 10)
                                   (:name . "Priority")))
                (:priority-icon.  ((:path . (fields priority iconUrl))
                                   (:columns . 10)
                                   (:name . "Priority")))
                (:labels . ((:path . (fields labels))
                            (:columns . 15)  ; Increased from 10
                            (:name . "Labels")))
                (:original-estimate . ((:path . (fields aggregatetimeoriginalestimate))
                                       (:columns . 12)  ; Increased from 10
                                       (:name . "Estimate")
                                       (:formatter . jira-fmt-time-from-secs)))
                (:work-ratio . ((:path . (fields workratio))
                                (:columns . 8)   ; Increased from 6
                                (:name . "WR")
                                (:formatter . jira-fmt-issue-progress)))
                (:remaining-time . ((:path . (fields timeestimate))
                                    (:columns . 12)  ; Increased from 10
                                    (:name . "Remaining")
                                    (:formatter . jira-fmt-time-from-secs)))
                (:assignee-name . ((:path . (fields assignee displayName))
                                   (:columns . 18)  ; Increased from 14
                                   (:name . "Assignee")))
                (:reporter-name . ((:path . (fields reporter displayName))
                                   (:columns . 18)  ; Increased from 14
                                   (:name . "Reporter")))
                (:components . ((:path . (fields components))
                                (:columns . 15)  ; Increased from 10
                                (:name . "Components")
                                (:formatter . jira-fmt-issue-components)))
                (:fix-versions . ((:path . (fields fixVersions))
                                  (:columns . 15)  ; Increased from 10
                                  (:name . "Fix Versions")
                                  (:formatter . jira-fmt-issue-fix-versions)))
                (:status-name . ((:path . (fields status))
                                 (:columns . 18)  ; Increased from 15
                                 (:name . "Status")
                                 (:formatter . jira-fmt-issue-status)))
                (:status-category-name . ((:path . (fields status statusCategory name))
                                          (:columns . 15)  ; Increased from 10
                                          (:name . "Status Category")))
                (:creator-name . ((:path (fields creator  displayName))
                                  (:columns . 15)  ; Increased from 10
                                  (:name . "Creator")))
                (:progress-percent . ((:path . (fields progress  percent))
                                      (:columns . 12)  ; Increased from 10
                                      (:name . "Progress")
                                      (:formatter . jira-fmt-issue-progress)))
                (:issue-type-name . ((:path . (fields issuetype name))
                                     (:columns . 22)  ; Increased from 15
                                     (:name . "Type")
                                     (:formatter . jira-fmt-issue-type-name)))
                (:issue-type-icon . ((:path . (fields issuetype iconUrl))
                                     (:columns .  12)  ; Increased from 10
                                     (:name . "Type")))
                (:project-key . ((:path . (fields project key))
                                 (:columns . 12)  ; Increased from 10
                                 (:name . "Project")))
                (:project-name .  ((:path . (fields project name))
                                   (:columns . 15)  ; Increased from 10
                                   (:name . "Project")))
                (:parent-type-name . ((:path . (fields parent fields issuetype name))
                                      (:columns . 15)  ; Increased from 10
                                      (:name . "Parent Type")
                                      (:formatter . jira-fmt-issue-type-name)))
                (:parent-status . ((:path . (fields parent fields status))
                                   (:columns . 15)  ; Increased from 10
                                   (:name . "Parent Status")
                                   (:formatter . jira-fmt-issue-status)))
                (:parent-key . ((:path . (fields parent key))
                                (:columns . 12)  ; Increased from 10
                                (:name . "Parent Key")
                                (:formatter . jira-fmt-issue-key)))
                (:created . ((:path . (fields created))
                             (:columns . 12)  ; Increased from 10
                             (:name . "Created")))
                (:updated . ((:path . (fields updated))
                             (:columns . 12)  ; Increased from 10
                             (:name . "Updated")))
                (:description . ((:path . (fields description))
                                 (:columns . 20)  ; Increased from 10
                                 (:name . "Description")))
                (:summary . ((:path . (fields summary))
                             (:columns . 50)  ; Significantly increased from 10
                             (:name . "Summary")))
                (:due-date . ((:path . (fields duedate))
                              (:columns . 12)  ; Increased from 10
                              (:name . "Due Date")
                              (:formatter . jira-fmt-date)))
                (:sprints . ((:path . (fields (custom "Sprint")))
                             (:columns . 15)  ; Increased from 10
                             (:name . "Sprints")
                             (:formatter . jira-fmt-issue-sprints)))
                (:line . ((:path . (fields (custom "Business line")))
                          (:columns . 15)  ; Increased from 10
                          (:name . "Business Line")
                          (:formatter . jira-fmt-business-line)))
                (:cost-center . ((:path . (fields (custom "Cost center")))
                                 (:columns . 15)  ; Increased from 10
                                 (:name . "Const Center")
                                 (:formatter . jira-fmt-cost-center)))
                (:resolution . ((:path . (fields resolution name))
                                                                 (:columns . 15)  ; Increased from 10
                                 (:name . "Resolution")))))))))

(defun jira/post-init-jira ()
  "Post-initialize jira package."
  (add-hook 'jira-issues-mode-hook
            (lambda ()
              (when (boundp 'jira-issues-mode-map)
                (evilified-state-evilify-map jira-issues-mode-map
                  :mode jira-issues-mode
                  :bindings
                  (kbd "l") 'jira-issues-menu
                  (kbd "?") 'jira-issues-actions-menu)))))

;;; packages.el ends here 