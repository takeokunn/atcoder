EMACS ?= emacs
TOP_DIR := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
HTMLIZE_FILE=$(TOP_DIR)/htmlize.el

$(HTMLIZE_FILE):
	wget https://raw.githubusercontent.com/hniksic/emacs-htmlize/master/htmlize.el

.PHONY: org-to-html
org-to-html: $(HTMLIZE_FILE)
	$(EMACS) index.org -Q --batch --eval "(progn (load \""$(HTMLIZE_FILE)"\") (setq org-html-htmlize-output-type 'css) (org-html-export-to-html))"
