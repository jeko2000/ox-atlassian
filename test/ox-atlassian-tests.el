;;; ox-atlassian-tests.el --- Tests for ox-atlassian -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2023 Johnny Ruiz

;;; Code

(require 'ert)
(require 'ox-atlassian)

(defun org-atlassian-export-string (string)
  (string-trim-right
   (org-export-string-as string 'atlassian t)))

;;; Tests
(ert-deftest org-atlassian-headlines-test ()
  (should
   (equal
    (org-atlassian-export-string "* Level 1\n** Level 2\n*** Level 3\n**** Level 4\n***** Level 5\n****** Level 6")
    "h1. Level 1\nh2. Level 2\nh3. Level 3\nh4. Level 4\nh5. Level 5\nh6. Level 6")))
