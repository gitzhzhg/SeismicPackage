#ifndef _STDIO_S_H
#define _STDIO_S_H	1


#include	"FEATURE/sfio"

#if _typ___FILE /* Redhat7.3 requires __FILE in wchar.h */
typedef struct _sfio_s	*__FILE;
#endif

#include	"sfhdr.h"
#include	"stdio.h"

#endif /*_STDIO_S_H*/
