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
  :menu-entry
  '(?a "Export to Atlassian Wiki"
       ((?a "To temporary buffer"
            (lambda (a s v b) (org-atlassian-export-as-atlassian a s v)))
        (?m "To file" (lambda (a s v b) (org-atlassian-export-to-atlassian a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-atlassian-export-to-atlassian t s v)
                (org-open-file (org-atlassian-export-to-atlassian nil s v)))))))
  :translate-alist '((bold . org-atlassian-bold)
                     (code . org-atlassian-code)
                     (headline . org-atlassian-headline)
                     (horizontal-rule . org-atlassian-horizontal-rule)
                     (italic . org-atlassian-italic)
                     (item . org-atlassian-item)
                     (paragraph . org-atlassian-paragraph)
                     (plain-list . org-atlassian-plain-list)
                     (quote-block . org-atlassian-quote-block)
                     (strike-through . org-atlassian-strike-through)
                     (section . org-atlassian-section)
                     (subscript . org-atlassian-subscript)
                     (superscript . org-atlassian-superscript)
                     (underline . org-atlassian-underline)
                     (verbatim . org-atlassian-verbatim))
  :options-alist
  '((:atlassian-max-headline-depth nil nil org-atlassian-max-headline-depth)
    (:atlassian-unfill-paragraph nil nil org-atlassian-unfill-paragraph)))

;;; User Configurable Variables

(defgroup org-export-atlassian nil
  "Options for exporting Org mode files to Atlassian."
  :tag "Org Export Atlassian"
  :group 'org-export)

(defcustom org-atlassian-max-headline-depth 6
  "Maximum depth of a headline."
  :group 'org-export-atlassian
  :type 'integer)

(defcustom org-atlassian-unfill-paragraph t
  "Non-nil means un-fill paragraph text before exporting."
  :group 'org-export-atlassian
  :type 'boolean)

;;;; Bold

(defun org-atlassian-bold (_bold contents _info)
  "Transcode BOLD from Org to Atlassian.
CONTENTS holds the text with bold markup. INFO is a plist holding
contextual information."
  (format "*%s*" contents))

;;;; Code

(defun org-atlassian-code (code _contents _info)
  "Transcode CODE from Org to Atlassian.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let ((value (org-element-property :value code)))
    (format "{{%s}}" value)))

;;;; Headline

(defun org-atlassian-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Atlassian.
CONTENTS holds the contents of the headline. INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (min (org-export-get-relative-level headline info)
                       (or (plist-get info :atlassian-max-headline-depth))))
           (title (org-export-data (org-element-property :title headline) info)))
      (format "h%s. %s\n%s" level title (or contents "")))))

;;;; Horizontal Rule

(defun org-atlassian-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode a HORIZONTAL-RULE element from Org to Atlassian.
CONTENTS is nil. INFO is a plist holding contextual information."
  "----")

;;;; Italic

(defun org-atlassian-italic (_italic contents _info)
  "Transcode ITALIC from Org to Atlassian.
CONTENTS holds the text with italic markup. INFO is a plist holding
contextual information."
  (format "_%s_" contents))

;;;; Item

(defun org-atlassian-item (item contents _info)
  "Transcode a ITEM element from Org to Atlassian.
CONTENTS holds the contents of the item. INFO is a plist holding
contextual information."
  (let ((bullet (org-atlassian--list-item-bullet item)))
    (format "%s %s" bullet contents)))

;;;; Paragraph

(defun org-atlassian-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Atlassian.
CONTENTS holds the contents of the paragraph. INFO is a plist
holding contextual information."
  (if (plist-get info :atlassian-unfill-paragraph)
      (string-fill contents (length contents))
    contents))

;;;; Plain List

(defun org-atlassian-plain-list (_plain-list contents _info)
  "Transcode a PLAIN-LIST element from Org to Atlassian.
CONTENTS holds the contents of the list. INFO is a plist holding
contextual information."
  contents)

;;;; Quote Block

(defun org-atlassian-quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element from Org to Atlassian.
CONTENTS holds the contents of the block. INFO is a plist holding
contextual information."
  (format "bq. %s" contents))

;;;; Section

(defun org-atlassian-section (_section contents _info)
  "Transcode a SECTION element from Org to Atlassian.
CONTENTS holds the contents of the section. INFO is a plist
holding contextual information."
  contents)

;;;; Strike-through

(defun org-atlassian-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH from Org to Atlassian.
CONTENTS holds the text with strike-through markup. INFO is a
plist holding contextual information."
  (format "-%s-" contents))

;;;; Subscript

(defun org-atlassian-subscript (_subscript contents _info)
  "Transcode SUBSCRIPT from Org to Atlassian.
CONTENTS holds the text with subscript markup. INFO is a plist
holding contextual information."
  (format "~%s~" contents))

;;;; Superscript

(defun org-atlassian-superscript (_superscript contents _info)
  "Transcode SUPERSCRIPT from Org to Atlassian.
CONTENTS holds the text with superscript markup. INFO is a plist
holding contextual information."
  (format "{^%s^}" contents))

;;;; Underline

(defun org-atlassian-underline (_underline contents _info)
  "Transcode UNDERLINE from Org to Atlassian.
CONTENTS holds the text with underline markup. INFO is a plist
holding contextual information."
  (format "+%s+" contents))

;;;; Verbatim

(defun org-atlassian-verbatim (verbatim _contents _info)
  "Transcode VERBOSE from Org to Atlassian.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let ((value (org-element-property :value verbatim)))
    (format "{{%s}}" value)))

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

;;; End-user functions

;;;###autoload
(defun org-atlassian-convert-region-to-atlassian ()
  "Assume region has Org syntax, and convert it to Atlassian Wiki."
  (interactive)
  (org-export-replace-region-by 'atlassian))

;;;###autoload
(defun org-atlassian-export-as-atlassian (&optional async subtreep visible-only)
  "Export current buffer to an Atlassian Wiki buffer.

If narrowing is active in the current buffer, only export its narrowed
part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously. The resulting buffer should be accessible through the
`org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree at
point, extracting information from the headline properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export contents
of hidden elements.

Export is done in a buffer named \"*Org Atlassian Wiki Export*\",
which will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'atlassian "*Org Atlassian Wiki Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-atlassian-export-to-atlassian
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an Atlassian Wiki file.

If narrowing is active in the current buffer, only export its narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously. The resulting file should be accessible through the
`org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree at point,
extracting information from the headline properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export contents of
hidden elements.

When optional argument BODY-ONLY is non-nil, strip title and table of contents
from output.

EXT-PLIST, when provided, is a property list with external parameters
overriding Org default settings, but still inferior to file-local settings.

Return output file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".atlassian" subtreep)))
    (org-export-to-file 'atlassian file
      async subtreep visible-only body-only ext-plist)))

(provide 'ox-atlassian)
