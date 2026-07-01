# Makefile
# Copyright (c) 2026 Jeffrey H. Johnson <johnsonjh.dev@gmail.com>
# SPDX-License-Identifier: MIT-0
# scspell-id: 5d5ebf90-753b-11f1-b3ba-80ee73e9b8e7
# NB: MIT-0 license applies to this file only; other files licensed separately

################################################################################

all:
	@printf '\n%s\n\n' \
		"ERROR: Review the README.md file for build instructions." || :
	@exit 1

################################################################################

scc: README.md
	awk '/<!-- scc-start -->/ { \
		print; system("scc \
			--count-as-pattern *.txt:Text:\"Plain Text\" \
			--count-as-pattern *.hlp:Text:\"Plain Text\" \
			--count-as-pattern *.crt:Text:\"Plain Text\" \
			--count-as-pattern *.vdm:VEDIT:\"Plain Text\" \
			--count-as-pattern *.exc:VEDIT:\"Plain Text\" \
			--exclude-file LICENSE,README.md,README.awk \
			--exclude-dir LICENSES,.git,oldsrc,oldbin \
			--exclude-ext art,ini \
			--no-cocomo -u --no-size -s lines -f html-table; \
			printf \"\n%s\n\" \"<!-- scc-end -->\""); \
			skip=1; next } \
		skip && /<!-- scc-end -->/ { skip=0; next } \
		!skip' README.md > README.awk && \
	mv -f README.awk README.md && \
	expand README.md > README.out && \
	mv -f README.out README.md

################################################################################

.PHONY: scc

################################################################################

.NOTPARALLEL:

################################################################################

# Local Variables:
# mode: makefile
# indent-tabs-mode: t
# tab-width: 8
# whitespace-style: (tabs tab-mark)
# whitespace-display-mappings: ((tab-mark 9 [45] [45]))
# fill-column: 80
# eval: (setq-local whitespace-display-mappings
#                   '((tab-mark 9
#                               [45 45 45 45 45 45 62]
#                               [45 45 45 45 45 45 62])))
# eval: (whitespace-mode 1)
# eval: (setq-local display-fill-column-indicator-column 80)
# eval: (display-fill-column-indicator-mode 1)
# End:

################################################################################
# vim: set ft=make ts=8 ai noexpandtab list listchars=tab\:\>\- cc=80 :
################################################################################
