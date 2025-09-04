LLCFLAGS=-O0 -mtriple=x86_64-pc-linux-gnu

simple.exe: simple.o
	ld.lld -o $@ -dynamic-linker /lib64/ld-linux-x86-64.so.2 \
		-L/usr/lib/x86_64-linux-gnu \
		/usr/lib/x86_64-linux-gnu/crt1.o \
		/usr/lib/x86_64-linux-gnu/crti.o \
		$< \
		-lc \
		/usr/lib/x86_64-linux-gnu/crtn.o

simple.o: simple.ll
	llc --filetype=obj $(LLCFLAGS) $< -o $@

simple.s: simple.ll
	llc --filetype=asm $(LLCFLAGS) $< -o $@

simple.ll: simple.c
	clang -O0 -g -S -emit-llvm $< -o $@

.PHONY: clean

clean:
	rm -rf *.o *.exe *.s
