CFLAGS=-std=c11 -g -fno-common -Wall -Wno-switch

PROJECT_VERSION="0.0"
GIT_VERSION=$(shell git describe --tags --always --dirty)

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

# Stage 1

superc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): superc.h version.h

# Make version.h update when git HEAD changes
version.h: .git/HEAD $(shell git rev-parse --git-dir)/refs/heads/$(shell git branch --show-current)
	echo "#ifndef _VERSION_H_IN" > version.h
	echo "#define _VERSION_H_IN" >> version.h
	echo "#define PROJECT_VERSION \"$(PROJECT_VERSION)\"" >> version.h
	echo "#define PROJECT_GIT_VERSION_DESCRIBE \"$(GIT_VERSION)\"" >> version.h
	echo "#endif // _VERSION_H_IN" >> version.h

test/%.ll: superc test/%.c
	-./superc -fpic -fno-constant-folding -Iinclude -Itest -S -o test/$*.ll test/$*.c

test/%.s: test/%.ll
	-llc --filetype=asm -O3 -o test/$*.s test/$*.ll

test/%.exe: superc test/%.c test/%.s
	@if ./superc -fpic -fno-constant-folding -Iinclude -Itest -c -o test/$*.o test/$*.c && \
	    $(CC) -pthread -o $@ test/$*.o -xc test/common; then \
		:; \
	else \
		rm -f "$@"; \
		echo "BUILD FAILED: $@"; \
	fi

test: $(TESTS)
	@build_failed=""; llc_failed=""; test_failed=""; test_ok="" \
	build_failed_count=0; llc_failed_count=0; \
	test_failed_count=0; test_ok_count=0; \
	driver_failed=0; \
	for i in $(TESTS); do \
		if [ ! -r "$${i%.exe}.ll" ]; then \
			build_failed="$$build_failed $${i%.exe}.c"; \
			build_failed_count=$$((build_failed_count + 1)); \
			continue; \
		fi; \
		if [ ! -r "$${i%.exe}.s" ]; then \
			llc_failed="$$llc_failed $${i%.exe}.ll"; \
			llc_failed_count=$$((llc_failed_count + 1)); \
			continue; \
		fi; \
		echo "$$i"; \
		if ! "./$$i"; then \
			test_failed="$$test_failed $$i"; \
			test_failed_count=$$((test_failed_count + 1)); \
		else \
			test_ok="$$test_ok $$i"; \
			test_ok_count=$$((test_ok_count + 1)); \
		fi; \
		echo; \
	done; \
	if ! test/driver.sh ./superc; then \
		driver_failed=1; \
	fi; \
	echo; \
	echo "\033[33m==== Test summary ====\033[0m"; \
	if [ -n "$$test_ok" ]; then \
		printf "\033[1;32mPassed ($$test_ok_count):\033[0m$$test_ok\n"; \
	fi; \
	if [ -n "$$build_failed" ]; then \
		printf "\033[1;31mCError ($$build_failed_count):\033[0m$$build_failed\n"; \
	fi; \
	if [ -n "$$llc_failed" ]; then \
		printf "\033[1;31mLError ($$llc_failed_count):\033[0m$$llc_failed\n"; \
	fi; \
	if [ -n "$$test_failed" ]; then \
		printf "\033[1;31mFailed ($$test_failed_count):\033[0m$$test_failed\n"; \
	fi; \
	if [ $$driver_failed ]; then \
		printf "\033[1;31m+ Driver test failed!\033[0m\n"; \
	fi; \
	if [ -z "$$build_failed" ] && [ -z "$$test_failed" ] && [ -z "$$driver_failed" ]; then \
		echo "All tests passed."; \
	fi; \
	test -z "$$build_failed" -a -z "$$test_failed" -a -z "$$driver_failed"

test-all: test test-stage2

# Stage 2

stage2/superc: $(OBJS:%=stage2/%)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

stage2/%.o: superc %.c
	mkdir -p stage2/test
	./superc -c -o $(@D)/$*.o $*.c

stage2/test/%.exe: stage2/superc test/%.c
	mkdir -p stage2/test
	./stage2/superc -Iinclude -Itest -c -o stage2/test/$*.o test/$*.c
	$(CC) -pthread -o $@ stage2/test/$*.o -xc test/common

test-stage2: $(TESTS:test/%=stage2/test/%)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	test/driver.sh ./stage2/superc

# Misc.

clean:
	rm -rf superc tmp* $(TESTS) test/*.ll test/*.s test/*.exe stage2
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean test-stage2
.PRECIOUS: $(TEST_SRCS:.c=.ll) $(TEST_SRCS:.c=.s)
