/* Copyright (c) Colorado School of Mines, 2007.*/
/* All rights reserved.                       */

#ifndef _VTHDR_H
#define _VTHDR_H	1

#include	<vthread.h>
#include	"FEATURE/vthread"

#if !vt_threaded /* for creating sub functions */
#undef vtopen
#undef vtclose
#undef vtkill
#undef vtwait
#undef vtrun

#undef vtset
#undef vtonce

#undef vtmtxopen
#undef vtmtxclose
#undef vtmtxlock
#undef vtmtxtrylock
#undef vtmtxunlock
#undef vtmtxclrlock

#undef vtstatus
#undef vterror
#undef vtmtxerror
#undef vtonceerror
#endif /*!vt_threaded*/

#ifndef NIL
#define NIL(t)	((t)0)
#endif

#if _num_PTHREAD_MUTEX_ERRORCHECK
#define _mtx_errorcheck	1
#endif
#if _num_PTHREAD_MUTEX_RECURSIVE
#define _mtx_recursive	1
#endif

/* Linux */
#ifdef __linux__
#if _num_PTHREAD_MUTEX_ERRORCHECK_NP && !_mtx_errorcheck
#define PTHREAD_MUTEX_ERRORCHECK	PTHREAD_MUTEX_ERRORCHECK_NP
#define _mtx_errorcheck	1
#endif
#if defined(PTHREAD_MUTEX_ERRORCHECK_NP) && !_mtx_errorcheck
#define PTHREAD_MUTEX_ERRORCHECK	PTHREAD_MUTEX_ERRORCHECK_NP
#define _mtx_errorcheck	1
#endif
#if _num_PTHREAD_MUTEX_RECURSIVE_NP && !_mtx_recursive
#define PTHREAD_MUTEX_RECURSIVE		PTHREAD_MUTEX_RECURSIVE_NP
#define _mtx_recursive	1
#endif
#if defined(PTHREAD_MUTEX_ERRORCHECK_NP) && !_mtx_recursive
#define PTHREAD_MUTEX_RECURSIVE		PTHREAD_MUTEX_RECURSIVE_NP
#define _mtx_recursive	1
#endif
#endif

/* pre-HPUX 11 strangeness */
#if !_lib_pthread_attr_init && _num_MUTEX_NONRECURSIVE_NP
#define _hpux_weirdness	1
#endif

#if _num_MUTEX_NONRECURSIVE_NP && !_mtx_errorcheck
#define PTHREAD_MUTEX_ERRORCHECK	MUTEX_NONRECURSIVE_NP
#define _mtx_errorcheck	1
#endif
#if defined(MUTEX_NONRECURSIVE_NP) && !_mtx_errorcheck
#define PTHREAD_MUTEX_ERRORCHECK	MUTEX_NONRECURSIVE_NP
#define _mtx_errorcheck	1
#endif
#if _num_MUTEX_RECURSIVE_NP && !_mtx_recursive
#define PTHREAD_MUTEX_RECURSIVE		MUTEX_RECURSIVE_NP
#define _mtx_recursive	1
#endif
#if defined(MUTEX_RECURSIVE_NP) && !_mtx_recursive
#define PTHREAD_MUTEX_RECURSIVE		MUTEX_RECURSIVE_NP
#define _mtx_recursive	1
#endif
#if !_lib_pthread_mutexattr_init && _lib_pthread_mutexattr_create
#define pthread_mutexattr_init		pthread_mutexattr_create
#endif
#if !_lib_pthread_mutexattr_destroy && _lib_pthread_mutexattr_delete
#define pthread_mutexattr_destroy	pthread_mutexattr_delete
#endif
#if !_lib_pthread_attr_init && _lib_pthread_attr_create
#define pthread_attr_init		pthread_attr_create
#endif
#if !_lib_pthread_attr_destroy && _lib_pthread_attr_delete
#define pthread_attr_destroy		pthread_attr_delete
#endif

/* define pthread_mutexattr_settype() based on detected alternatives */
#if !_lib_pthread_mutexattr_settype
#if _lib_pthread_mutexattr_setkind_np
#undef pthread_mutexattr_setkind
#undef _lib_pthread_mutexattr_setkind
#define pthread_mutexattr_setkind	pthread_mutexattr_setkind_np
#define _lib_pthread_mutexattr_setkind	1
#endif
#if _lib_pthread_mutexattr_setkind
#undef pthread_mutexattr_settype
#define pthread_mutexattr_settype	pthread_mutexattr_setkind
#define _lib_pthread_mutexattr_settype	1
#endif
#endif /*!_lib_pthread_mutexattr_settype*/

/* because of irix and hpux quirks, we'll bias toward mtx_errorcheck */
#if _mtx_errorcheck && _mtx_recursive    
#undef _mtx_recursive
#endif

#if _mtx_errorcheck
#define MTXTYPE		PTHREAD_MUTEX_ERRORCHECK
#endif
#if _mtx_recursive
#define MTXTYPE		PTHREAD_MUTEX_RECURSIVE
#endif

/* deal with pre-hpux11's lapse of judgement! */
#if _hpux_weirdness
#define MTXTRYLOCK_OK		1
#define MTXLOCK_OK		0
#define MTXUNLOCK_OK		0
#define MTXBUSY(rv)		(rv == 0)
#define ATTR(attr)		(attr)
#else
#define MTXTRYLOCK_OK		0
#define MTXLOCK_OK		0
#define MTXUNLOCK_OK		0
#define MTXBUSY(rv)		(rv == EBUSY)
#define ATTR(attr)		(&attr)
#endif

/* variables local to library */
typedef struct _vtextern_s	Vtextern_t;
typedef int			(*Vttrylock_f) _ARG_((_vtmtx_t*));
struct _vtextern_s
{
	Vtmutex_t	vt_mutex;
	Vthread_t**	vt_list;
	int		vt_nlist;
	Vttrylock_f	vt_trylockf;
	int		vt_init;
};
#define _Vtmutex	(&_Vtextern.vt_mutex)
#define _Vtlist		_Vtextern.vt_list
#define _Vtnlist	_Vtextern.vt_nlist
#define _Vttrylockf	_Vtextern.vt_trylockf
#define _Vtinit		_Vtextern.vt_init

#define VTONCE()	(_Vtinit ? 0 : vtonce(&_Vtonce, _vtonce) )

_BEGIN_EXTERNS_
extern Vtextern_t	_Vtextern;
extern void		_vtonce();
extern Vtonce_t		_Vtonce;

extern Void_t*		malloc _ARG_((size_t));
extern void		free _ARG_((Void_t*));

_END_EXTERNS_

#endif /*_VTHDR_H*/
