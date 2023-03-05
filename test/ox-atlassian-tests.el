;;; ox-atlassian-tests.el --- Tests for ox-atlassian -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2023 Johnny Ruiz

;;; Code

(require 'ert)
(require 'ox-atlassian)

(defun org-atlassian-export-string (string)
  (string-trim-right
   (org-export-string-as string 'atlassian t)))

(defun string-equal-p (input expected)
  (equal
   (string-trim input)
   (string-trim expected)))

;;; Tests
(ert-deftest org-atlassian-headlines-test ()
  (should
   (string-equal-p (org-atlassian-export-string "
* Level 1
** Level 2
*** Level 3
**** Level 4
***** Level 5
****** Level 6") "
h1. Level 1
h2. Level 2
h3. Level 3
h4. Level 4
h5. Level 5
h6. Level 6")))

(ert-deftest org-atlassian-lists-test ()
  (should
   (string-equal-p (org-atlassian-export-string "
- Plain unordered list level 2.0
- Plain unordered list level 2.1
  + Plain unordered list level 3.0


1) Plain ordered list level 1.0
   1) Plain ordered list level 2.0
   2) Plain ordered list level 2.1
      1) Plain ordered list level 3.0


1) Plain ordered list level 1.0
   * Plain unordered list level 2.0
   * Plain unordered list level 2.1
     1) Plain ordered list level 3.0") "
* Plain unordered list level 2.0
* Plain unordered list level 2.1
** Plain unordered list level 3.0


# Plain ordered list level 1.0
## Plain ordered list level 2.0
## Plain ordered list level 2.1
### Plain ordered list level 3.0


# Plain ordered list level 1.0
#* Plain unordered list level 2.0
#* Plain unordered list level 2.1
#*# Plain ordered list level 3.0")))

(ert-deftest org-atlassian-text-effect-tests ()
  (should (string-equal-p (org-atlassian-export-string "*bold*") "*bold*"))
  (should (string-equal-p (org-atlassian-export-string "+deleted+") "-deleted-"))
  (should (string-equal-p (org-atlassian-export-string "/italics/") "_italics_"))
  (should (string-equal-p (org-atlassian-export-string "_inserted_") "+inserted+"))
  (should (string-equal-p (org-atlassian-export-string "~monospaced~") "{{monospaced}}"))
  (should (string-equal-p (org-atlassian-export-string "~verbatim~") "{{verbatim}}"))

  (let ((org-export-with-sub-superscripts t))
    (should (string-equal-p (org-atlassian-export-string "kg/m^3") "kg/m{^3^}"))
    (should (string-equal-p (org-atlassian-export-string "Text with_subscript") "Text with~subscript~")))
  (let ((org-export-with-sub-superscripts nil))
    (should (string-equal-p (org-atlassian-export-string "kg/m^3") "kg/m^3"))
    (should (string-equal-p (org-atlassian-export-string "Text with_subscript") "Text with_subscript"))))

(ert-deftest org-atlassian-horizontal-rule-tests ()
  (should (string-equal-p (org-atlassian-export-string "-----") "----"))
  (should (string-equal-p (org-atlassian-export-string "------") "----"))
  (should (string-equal-p (org-atlassian-export-string "-------") "----"))
  (should (string-equal-p (org-atlassian-export-string "-------") "----")))

(ert-deftest org-atlassian-text-block-quote-tests ()
  (should (string-equal-p (org-atlassian-export-string "
#+begin_quote
Here's how you make a paragraph appear as a block quotation.
#+end_quote") "
bq. Here's how you make a paragraph appear as a block quotation.")))
