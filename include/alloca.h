#ifndef	_ALLOCA_H
#define	_ALLOCA_H

#undef alloca

/* Allocate stack space */
extern void *alloca(int __size);

#define alloca(size) __builtin_alloca((size))

#endif
