#ifndef _SFDISC_H
#define _SFDISC_H	1

#include	<sfio.h>

#if _PACKAGE_ast
#if _BLD_ast && defined(__EXPORT__)
#define extern          __EXPORT__
#endif
#endif

/* to create a user-defined event specific to some package */
#define SFDCEVENT(a,b,n)	((((a)-'A'+1)<<11)^(((b)-'A'+1)<<6)^(n))

_BEGIN_EXTERNS_
/* functions to create disciplines */
extern int	sfdcdio _ARG_((Sfio_t*, size_t));
extern int	sfdcdos _ARG_((Sfio_t*));
extern int	sfdcfilter _ARG_((Sfio_t*, const char*));
extern int	sfdcseekable _ARG_((Sfio_t*));
extern int	sfdcslow _ARG_((Sfio_t*));
extern int	sfdcsubstream _ARG_((Sfio_t*, Sfio_t*, Sfoff_t, Sfoff_t));
extern int	sfdctee _ARG_((Sfio_t*, Sfio_t*));
extern int	sfdcunion _ARG_((Sfio_t*, Sfio_t**, int));

extern int	sfdclzw _ARG_((Sfio_t*));
extern int	sfdcgzip(Sfio_t*, int);
_END_EXTERNS_

#if _PACKAGE_ast
#undef extern
#endif

#endif
