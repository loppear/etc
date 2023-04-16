;; Org-mode & Org-roam

(use-package org-roam
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory "~/org-roam")
  :bind (:map org-roam-mode-map
              (("C-c r l" . org-roam)
               ("C-c r f" . org-roam-find-file)
               ("C-c r i" . org-roam-insert)
               ("C-c r t" . org-roam-tag-add)
               ("C-c r c" . org-roam-capture)
               ("C-c r d" . org-roam-dailies-find-date)
               ("C-c r x" . org-roam-jump-to-index)
               ("C-c r g" . org-roam-graph))
              :map org-mode-map
              (("C-c r c" . org-roam-capture))
              (("C-c r x" . org-roam-jump-to-index)))

  :init (setq org-roam-tag-sources '(prop all-directories))
  (setq org-roam-title-sources '((title headline) alias))
  (setq org-roam-capture-templates
        '(
          ("f" "Fleeting" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "fleeting/%<%Y%m%d>---${slug}"
           :head "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t)
          ("c" "Notes" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "notes/%<%Y%m%d>---${slug}"
           :head "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t)
          ("b" "Book Notes" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "book/notes/%<%Y%m%d>---${slug}"
           :head "#+title: Notes on ${title}\n#+roam_tags:Book\n* ${title}\n\n"
           :unnarrowed t)
          ("r" "Book Review" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "book/review/%<%Y%m%d>---${slug}"
           :head "#+title: Review of ${title}\n#+roam_tags:Book, Review\n* ${title}\n\n"
           :unnarrowed t)
          ("l" "Link" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "links/%<%Y%m%d>---${slug}"
           :head "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t)
          ))
  (setq org-roam-dailies-directory "daily/")

  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
	 #'org-roam-capture--get-point
	 "* %?"
	 :file-name "daily/%<%Y-%m-%d>"
	          :head "#+title: %<%Y-%m-%d>\n\n")))
  
  )

(provide 'ldo-org.el)
