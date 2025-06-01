;;; packages.el --- claude layer packages file for Spacemacs.
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

;; Claude AI integration layer for Spacemacs.
;; This layer provides Claude AI integration through claude-code.el package.

;;; Code:

(defconst claude-packages
  '(
    (claude-code :location (recipe :fetcher github
                                   :repo "stevemolitor/claude-code.el"
                                   :files ("*.el")))
    )
  "The list of Lisp packages required by the claude layer.")

(defun claude/init-claude-code ()
  "Initialize claude-code package."
  (use-package claude-code
    :defer t
    :init
    (progn
      ;; Set up Spacemacs keybindings
      (spacemacs/declare-prefix "A" "AI")
      (spacemacs/declare-prefix "Ac" "claude")
      (spacemacs/set-leader-keys
        "Acc" 'claude-code
        "Acd" 'claude-code-current-directory
        "Act" 'claude-code-toggle
        "Acb" 'claude-code-switch-to-buffer
        "Ack" 'claude-code-kill
        "Acs" 'claude-code-send-command
        "Acx" 'claude-code-send-command-with-context
        "Acr" 'claude-code-send-region
        "Ace" 'claude-code-fix-error-at-point
        "Ac/" 'claude-code-slash-commands
        "Acm" 'claude-code-transient
        "Acy" 'claude-code-send-return
        "Acn" 'claude-code-send-escape
        "AcR" 'claude-code-toggle-read-only-mode))
    :config
    (progn
      ;; Enable claude-code-mode globally
      (claude-code-mode 1)

      ;; Configure display buffer for Claude window
      (add-to-list 'display-buffer-alist
                   '("^\\*claude\\*"
                     (display-buffer-in-side-window)
                     (side . right)
                     (window-width . 0.33)))

      ;; Set up hooks
      (add-hook 'claude-code-start-hook
                (lambda ()
                  (message "Claude Code session started"))))))

;;; packages.el ends here
