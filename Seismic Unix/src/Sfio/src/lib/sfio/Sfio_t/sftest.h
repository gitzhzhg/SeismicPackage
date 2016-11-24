#include	"FEATURE/sfio"

#if !_PACKAGE_ast
#ifdef __STDC__
#include	<stdarg.h>
#else
#include	<varargs.h>
#endif
#endif

#if _SFIO_H_ONLY
#include	<sfio.h>
#else
#include	<sfio_t.h>
#endif

#if _PACKAGE_ast
#include	<ast.h>
#endif

#if _hdr_unistd
#include	<unistd.h>
#endif

#include	<errno.h>

#include	"terror.h"

#ifndef EINTR
#define EINTR	4
#endif
#ifndef EBADF
#define EBADF	9
#endif
#ifndef EAGAIN
#define EAGAIN	11
#endif
#ifndef ENOMEM
#define ENOMEM	12
#endif
#ifndef ESPIPE
#define ESPIPE	29
#endif

#if _hdr_math
#include	<math.h>
#endif

#if _hdr_float
#include	<float.h>
#endif
