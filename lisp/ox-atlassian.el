;;; org-atlassian.el --- Atlassian Wiki Markup Back-End for Org Export Engine -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2023 Johnny Ruiz
;; Author: Johnny Ruiz <johnny@ruiz-usa.com>
;; Keywords: org, org-export, atlassian, confluence, jira

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements an Atlassian Wiki Markup backend for Org
;; generic exporter. See Org manual for more information.

(require 'ox)

(org-export-define-derived-backend 'atlassian 'ascii
  ;; TODO: Add menu entry options
  :translate-alist '((headline . org-atlassian-headline)))

;;; User Configurable Variables

(defgroup org-export-atlassian nil
  "Options for exporting Org mode files to Atlassian."
  :tag "Org Export Atlassian"
  :group 'org-export)

(defcustom org-atlassian-max-headline-depth 6
  "Maximum depth of a headline."
  :group 'org-export-atlassian
  :type 'integer)

;;;; Headline

(defun org-atlassian-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Atlassian.
CONTENTS holds the contents of the headline. INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (min (org-export-get-relative-level headline info)
                       org-atlassian-max-headline-depth))
           (title (org-export-data (org-element-property :title headline) info)))
      (format "h%s. %s\n%s" level title (or contents "")))))

(provide 'ox-atlassian)
