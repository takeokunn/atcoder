EMACS ?= emacs

.PHONY: org-to-html
org-to-html:
	$(EMACS) index.org -Q --batch --eval "(progn (setq org-html-htmlize-output-type nil) (org-html-export-to-html))"
