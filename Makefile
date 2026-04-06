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

test/%.exe: superc test/%.c
	./superc -Iinclude -Itest -c -o test/$*.o test/$*.c
	$(CC) -pthread -o $@ test/$*.o -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	test/driver.sh ./superc

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
	rm -rf superc tmp* $(TESTS) test/*.s test/*.exe stage2
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean test-stage2
