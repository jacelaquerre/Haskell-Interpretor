.PHONY: default
default:
	@echo "no arguments -- call 'make hwXX' for some homework number like 'hw01'"

.stack-work:
	stack setup

clean:
	rm -rf .stack-work/

##############
# RUNNING HW #
##############

.PHONY: eval
eval:
	@echo -------
	@echo RUNNING $(EVAL_PATH)
	@echo -------
	stack ghci --ghci-options '-e $(EVAL_PATH).main'

.PHONY: hw01
hw01: ; make eval EVAL_PATH=HW01

.PHONY: hw02
hw02: ; make eval EVAL_PATH=HW02

.PHONY: hw03
hw03: ; make eval EVAL_PATH=HW03

.PHONY: hw04
hw04: ; make eval EVAL_PATH=HW04

.PHONY: hw05
hw05: ; make eval EVAL_PATH=HW05

.PHONY: hw06
hw06: ; make eval EVAL_PATH=HW06

.PHONY: hw07
hw07: ; make eval EVAL_PATH=HW07

.PHONY: hw08
hw08: ; make eval EVAL_PATH=HW08

.PHONY: hw09
hw09: ; make eval EVAL_PATH=HW09

.PHONY: hw10
hw10: ; make eval EVAL_PATH=HW10

.PHONY: live
live: ; make eval EVAL_PATH=Live

.PHONY: fp
fp: ; make eval EVAL_PATH=FP


#################
# GHCI HOMEWORK #
#################

.PHONY: interact
interact:
	@echo -------
	@echo LOADING $(EVAL_PATH)
	@echo -------
	stack ghci src/$(EVAL_PATH).hs

.PHONY: hw01-i
hw01-i: ; make interact EVAL_PATH=HW01

.PHONY: hw02-i
hw02-i: ; make interact EVAL_PATH=HW02

.PHONY: hw03-i
hw03-i: ; make interact EVAL_PATH=HW03

.PHONY: hw04-i
hw04-i: ; make interact EVAL_PATH=HW04

.PHONY: hw05-i
hw05-i: ; make interact EVAL_PATH=HW05

.PHONY: hw06-i
hw06-i: ; make interact EVAL_PATH=HW06

.PHONY: hw07-i
hw07-i: ; make interact EVAL_PATH=HW07

.PHONY: hw08-i
hw08-i: ; make interact EVAL_PATH=HW08

.PHONY: hw09-i
hw09-i: ; make interact EVAL_PATH=HW09

.PHONY: hw10-i
hw10-i: ; make interact EVAL_PATH=HW10

.PHONY: live-i
live-i: ; make interact EVAL_PATH=Live

.PHONY: fp-i
fp-i: ; make interact EVAL_PATH=FP

##################
# GHCID HOMEWORK #
##################

.PHONY: dev
dev: .stack-work
	@echo -----------
	@echo INTERACTIVE $(EVAL_PATH)
	@echo -----------
	ghcid --test=$(EVAL_PATH).main --warnings

.PHONY: hw01-dev
hw01-dev: ; make dev EVAL_PATH=HW01

.PHONY: hw02-dev
hw02-dev: ; make dev EVAL_PATH=HW02

.PHONY: hw03-dev
hw03-dev: ; make dev EVAL_PATH=HW03

.PHONY: hw04-dev
hw04-dev: ; make dev EVAL_PATH=HW04

.PHONY: hw05-dev
hw05-dev: ; make dev EVAL_PATH=HW05

.PHONY: hw06-dev
hw06-dev: ; make dev EVAL_PATH=HW06

.PHONY: hw07-dev
hw07-dev: ; make dev EVAL_PATH=HW07

.PHONY: hw08-dev
hw08-dev: ; make dev EVAL_PATH=HW08

.PHONY: hw09-dev
hw09-dev: ; make dev EVAL_PATH=HW09

.PHONY: hw10-dev
hw10-dev: ; make dev EVAL_PATH=HW10

.PHONY: live-dev
live-dev: ; make dev EVAL_PATH=Live

.PHONY: fp-dev
fp-dev: ; make dev EVAL_PATH=FP

###########
# PARSING #
###########

.PHONY: pl1
pl1: ; stack ghci --ghci-options '-e "Lang.Parse.action [\"l1\",\"$E\"]"'

.PHONY: pl2
pl2: ; stack ghci --ghci-options '-e "Lang.Parse.action [\"l2\",\"$E\"]"'

##################
# STAFF USE ONLY #
##################

.PHONY: sl01
sl01: ; make eval EVAL_PATH=Solutions.SL01

.PHONY: sl02
sl02: ; make eval EVAL_PATH=Solutions.SL02

.PHONY: sl03
sl03: ; make eval EVAL_PATH=Solutions.SL03

.PHONY: sl04
sl04: ; make eval EVAL_PATH=Solutions.SL04

.PHONY: sl05
sl05: ; make eval EVAL_PATH=Solutions.SL05

.PHONY: sl06
sl06: ; make eval EVAL_PATH=Solutions.SL06

.PHONY: sl07
sl07: ; make eval EVAL_PATH=Solutions.SL07

.PHONY: sl08
sl08: ; make eval EVAL_PATH=Solutions.SL08

.PHONY: sl09
sl09: ; make eval EVAL_PATH=Solutions.SL09

.PHONY: sl10
sl10: ; make eval EVAL_PATH=Solutions.SL10

.PHONY: sl01-dev
sl01-dev: ; make dev EVAL_PATH=Solutions.SL01

.PHONY: sl02-dev
sl02-dev: ; make dev EVAL_PATH=Solutions.SL02

.PHONY: sl03-dev
sl03-dev: ; make dev EVAL_PATH=Solutions.SL03

.PHONY: sl04-dev
sl04-dev: ; make dev EVAL_PATH=Solutions.SL04

.PHONY: sl05-dev
sl05-dev: ; make dev EVAL_PATH=Solutions.SL05

.PHONY: sl06-dev
sl06-dev: ; make dev EVAL_PATH=Solutions.SL06

.PHONY: sl07-dev
sl07-dev: ; make dev EVAL_PATH=Solutions.SL07

.PHONY: sl08-dev
sl08-dev: ; make dev EVAL_PATH=Solutions.SL08

.PHONY: sl09-dev
sl09-dev: ; make dev EVAL_PATH=Solutions.SL09

.PHONY: sl10-dev
sl10-dev: ; make dev EVAL_PATH=Solutions.SL10

HW_RELEASE := 01 02 03 04 05 06 07 08 09
LANG_RELEASE := Trees L1 L1M L1MN L2 L2C L3 L4 L5 L6 L7 L8
NO_TOUCHIE := src/HW02.hs src/HW03.hs src/HW06.hs src/HW07.hs
RELEASE_FILES := \
	Makefile package.yaml README.md stack.yaml src/FP.hs \
	$(wildcard src/Util/*) \
    $(foreach n,$(HW_RELEASE),src/HW$n.hs $(wildcard tests/hw$n/**/*)) \
	$(foreach l,$(LANG_RELEASE),src/Lang/$l.hs $(wildcard src/Lang/$l/*.hs)) \

RELEASE_DIR := cs225-hw-2020-01

.PHONY: prepare
prepare:
	@echo RELEASING FILES: $(RELEASE_FILES)
	@echo CLEANING
	@cd $(RELEASE_DIR) && git clean -fd
	@echo COPYING
	@$(foreach f,$(RELEASE_FILES), \
		mkdir -p $(dir $(RELEASE_DIR)/$f) ; \
		cp $f $(RELEASE_DIR)/$f ; \
	)
	@echo REVERTING FILES: $(NO_TOUCHIE)
	@$(foreach f,$(NO_TOUCHIE), \
	  (cd $(RELEASE_DIR) && git checkout $f) ; \
	)
	@echo STATUS
	@cd $(RELEASE_DIR) && git status && git add --dry-run .

.PHONY: release
release:
	@make prepare
	@echo COMMITTING
	@cd $(RELEASE_DIR) && git add . && git commit -m "update" && git push

.PHONY: re
re:
	touch src/Live.hs
