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
  :translate-alist '((headline . org-atlassian-headline)
                     (plain-list . org-atlassian-plain-list)
                     (item . org-atlassian-item)))

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

;;;; Item

(defun org-atlassian-item (item contents _info)
  "Transcode a ITEM element from Org to Atlassian.
CONTENTS holds the contents of the item. INFO is a plist holding
contextual information."
  (let ((bullet (org-atlassian--list-item-bullet item)))
    (format "%s %s" bullet contents)))

;;;; Plain List

(defun org-atlassian-plain-list (_plain-list contents _info)
  "Transcode a PLAIN-LIST element from Org to Atlassian.
CONTENTS holds the contents of the list. INFO is a plist holding
contextual information."
  contents)

;;;; Utility Functions

(defun org-atlassian--list-item-bullet (item)
  "Return the bullet associated with ITEM.
Build the bullet characters by walking up the item's ancestors
and collecting the appropriate bullet character associated with
the plain list's type."
  ;; Build bullet characters by walking up the tree and pushing the
  ;; bullet appropriate for the parent plain list type.
  (let ((bullet-chars'()))
    (while (and item
                (setq item (org-export-get-parent item)))
      (when (eq (org-element-type item) 'plain-list)
        (let* ((list-type (org-element-property :type item))
               (char (if (eq list-type 'ordered) ?# ?*)))
          (push char bullet-chars))))
    (apply #'string bullet-chars)))

(provide 'ox-atlassian)
