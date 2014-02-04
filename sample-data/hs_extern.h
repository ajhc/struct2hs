#ifndef __HS_EXTERN_H__
#define __HS_EXTERN_H__

#include <bus.h>
#include <ac97var.h>
#include <auich.h>

extern void dummy_func(void);

static inline void
funptr_apply_p1(void (*funptr)(void), void *p1)
{
	void (*lock)(void *) = (void (*)(void *)) funptr;
	(*lock)(p1);
}

#endif /* __HS_EXTERN_H__ */
