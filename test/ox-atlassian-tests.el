;;; ox-atlassian-tests.el --- Tests for ox-atlassian -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2023 Johnny Ruiz

;;; Code

(require 'ert)
(require 'ox-atlassian)

(defun org-atlassian-export-string (string)
  (string-trim-right
   (org-export-string-as string 'atlassian t)))

(defmacro match (input expected)
  (declare (indent 2))
  `(equal
    (string-trim (org-atlassian-export-string ,input))
    (string-trim ,expected)))

;;; Tests
(ert-deftest org-atlassian-headlines-test ()
  (should (match "
* Level 1
** Level 2
*** Level 3
**** Level 4
***** Level 5
****** Level 6" "
h1. Level 1
h2. Level 2
h3. Level 3
h4. Level 4
h5. Level 5
h6. Level 6")))

(ert-deftest org-atlassian-lists-test ()
  (should
   (match "
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
     1) Plain ordered list level 3.0" "
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
