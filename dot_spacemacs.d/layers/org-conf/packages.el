;;; packages.el --- org-conf layer packages file for Spacemacs.
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `org-conf-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-conf/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-conf/pre-init-PACKAGE' and/or
;;   `org-conf/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org-conf-packages
  '(
    (aggressive-fill-paragraph :location (recipe :fetcher github
                                                 :repo "et2010/aggressive-fill-paragraph-mode"
                                                 :upgrade nil))
    counsel
    org
    org-autolist
    org-download
    smartparens
    )
  )


(defconst org-conf-struct-templates
  '(("el"  . "src emacs-lisp")
    ("ex" . "src emacs-lisp :lexical t")
    ("py"  . "src python :results output raw")
    ("sh"  . "src shell")
    ("vb"   . "verbatim"))
  "Custom org templates.")


(defconst org-conf-keyword-templates
  '(("al"  . "attr_latex")
    ("ao"  . "attr_org")
    ("cal" . "call")
    ("cap" . "caption")
    ("lc"  . "latex_class")
    ("lh"  . "latex_header")
    ("lo"  . "latex_class_options")
    ("n"   . "name")
    ("o"   . "options")
    ("t"   . "title")
    ("tn"  . "tblname"))
  "Custom org keyword templates.")


(defun org-conf/post-init-counsel ()
  (defun org-conf//insert-selection-as-org-link (x)
    (with-ivy-window (insert (format "[[%s]]" x))
                     (goto-char (- (point)
                                   2))))
  (defun org-conf//insert-selection-as-inline-image (x)
    (with-ivy-window (let ((org-download-timestamp ""))
                       (org-download-image x))))
  ;; TODO add actions mode-locally: https://github.com/abo-abo/swiper/issues/1598
  (dolist (sym '(counsel-find-file counsel-locate))
    (ivy-add-actions sym
                     '(("k" org-conf//insert-selection-as-org-link
                        "insert as an org link")
                       ("I" org-conf//insert-selection-as-inline-image
                        "insert as an inline image")))))


(defun org-conf/init-aggressive-fill-paragraph ()
  (use-package aggressive-fill-paragraph
    :defer t
    :init (add-hook 'org-mode-hook #'aggressive-fill-paragraph-mode)
    :config (setq afp-fill-keys (append afp-fill-keys '(?， ?。)))))


(defun org-conf/init-org-autolist ()
  (use-package org-autolist
    :defer t
    :init (add-hook 'org-mode-hook
                    (lambda ()
                      (org-autolist-mode 1)))))


(defun org-conf/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    (sp-local-pair 'org-mode "\\[" "\\]" :trigger "\\[")
    (sp-local-pair '(LaTeX-mode org-mode) "（" "）")
    (sp-local-pair '(LaTeX-mode org-mode) "\\left(" "\\right)" :trigger "\\l(")
    (sp-local-pair '(LaTeX-mode org-mode) "\\left[" "\\right]" :trigger "\\l[")
    (sp-local-pair '(LaTeX-mode org-mode) "\\left{" "\\right}" :trigger "\\l{"))
  (add-hook 'org-mode-hook #'spacemacs/toggle-smartparens))


(defun org-conf/pre-init-org-download ()
  (spacemacs|use-package-add-hook org-download
    :post-init
    (progn
      ;; (setq-default org-download-backend "wget \"%s\" -O \"%s\"")
      (if (spacemacs/system-is-mswindows)
          (progn
            (setq-default org-download-screenshot-file (expand-file-name "emacs-org-download-screenshot.png"))
            (setq-default org-download-screenshot-method "i_view64 /capture=4 /convert=\"%s\""))
        (setq-default org-download-screenshot-method "flameshot gui --raw > %s"))
      (setq-default org-download-image-dir "./img"
                    org-download-image-org-width 400)

      (spacemacs/declare-prefix-for-mode 'org-mode "mod" "download")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "iDc" 'org-download-clipboard
        "iDd" 'org-download-delete
        "iDf" 'org-download-rename-last-file
        "iDr" 'org-download-rename-at-point
        "iDY" 'org-download-yank-replace))
    :post-config
    (progn
      (defun org-download-yank-no-timestamp (orig-func)
        (let ((org-download-timestamp ""))
          (funcall orig-func)))
      (advice-add #'org-download-yank :around #'org-download-yank-no-timestamp))))


(defun org-conf/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-init
    (progn
      (defun org-babel-execute:sh-dot-env (original-func body params)
        "Advice to source .env file before executing shell commands in Org-mode source blocks."
        (let* ((dir (or (cdr (assoc :dir params))
                        default-directory))
               (remote (file-remote-p dir))
               (env-file-path (if remote
                                  (tramp-file-name-localname (tramp-dissect-file-name (expand-file-name ".env" dir)))
                                (expand-file-name ".env" dir)))
               (env-file-exists (file-exists-p (if remote
                                                   (concat remote env-file-path)
                                                 env-file-path)))
               (sourced-body (if env-file-exists
                                 (format "set -a; . '%s'; set +a; %s" env-file-path
                                         body)
                               ;; If the .env file does not exist, just use the original body
                               body)))
          (funcall original-func sourced-body params)))

        (advice-add 'org-babel-execute:sh :around #'org-babel-execute:sh-dot-env)
      )
    :post-config
    (setq org-src-lang-modes (append '(("conf" . conf))
                                     org-src-lang-modes))
    )
  )


(defun org-conf/post-init-org ()
  (require 'org-tempo)
  ;; set up struct (such as src block) templates
  (dolist (templ org-conf-struct-templates)
    (add-to-list 'org-structure-template-alist templ))
  ;; set up keyword (e.g. #+options:) templates
  (dolist (templ org-conf-keyword-templates)
    (add-to-list 'org-tempo-keywords-alist templ)))
