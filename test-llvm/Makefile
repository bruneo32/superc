LLCFLAGS=-O0 -mtriple=x86_64-pc-linux-gnu

all: simple.exe simple.s

%.exe: %.o
	ld.lld -o $@ -dynamic-linker /lib64/ld-linux-x86-64.so.2 \
		-L/usr/lib/x86_64-linux-gnu \
		/usr/lib/x86_64-linux-gnu/crt1.o \
		/usr/lib/x86_64-linux-gnu/crti.o \
		$< \
		-lc \
		/usr/lib/x86_64-linux-gnu/crtn.o

%.o: %.ll
	llc --filetype=obj $(LLCFLAGS) $< -o $@

%.s: %.ll
	llc --filetype=asm $(LLCFLAGS) $< -o $@

%.ll: %.c
	clang -S -emit-llvm -O0 -fcommon $< -o $@

.PHONY: all clean

clean:
	rm -rf *.o *.exe *.s
