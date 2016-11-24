#include	"vthdr.h"

/*	Create or initialize a mutex.
**
**	Written by Kiem-Phong Vo.
*/

/* this get done once per process */
void _vtonce()
{
#if vt_threaded
#if _WIN32
	char	sys_dir[MAX_PATH];
	int	n;
	HANDLE	hm;

	/* get the win32 TryEnterCriticalSection function */
	n = GetSystemDirectory(sys_dir, sizeof(sys_dir));
	sys_dir[n++] = '\\';
	strcpy(&sys_dir[n], "kernel32");
	if((hm = GetModuleHandle(sys_dir)) )
		_Vttrylockf = (Vttrylock_f)GetProcAddress(hm,"TryEnterCriticalSection");
#endif

	_Vtinit = 1;

	/* initialize the global mutex that we use internally */
	vtmtxopen(_Vtmutex, VT_INIT);
#endif
}

#if __STD_C
Vtmutex_t* vtmtxopen(Vtmutex_t* mtx, int flags)
#else
Vtmutex_t* vtmtxopen(mtx, flags)
Vtmutex_t*	mtx;
int		flags;
#endif
{
#if !vt_threaded
	return NIL(Vtmutex_t*);
#else
	Vtmutex_t*	m;

	VTONCE();

	if(!(m = mtx))
	{	if(!(m = (Vtmutex_t*)malloc(sizeof(Vtmutex_t))) )
			return NIL(Vtmutex_t*);
		flags = VT_INIT|VT_FREE;
	}

	if(flags & VT_INIT)
	{	m->count = 0;
		m->error = 0;
		m->state = 0;

#if _WIN32
#define _did_init	1
		InitializeCriticalSection(&m->lock);
		m->owner = 0;
#endif /*_WIN32*/

#if !_did_init && defined(MTXTYPE)
#define _did_init	1
		{ pthread_mutexattr_t	attr;
		  pthread_mutexattr_init(&attr);
		  pthread_mutexattr_settype(&attr, MTXTYPE);
		  pthread_mutex_init(&m->lock, ATTR(attr));
		  pthread_mutexattr_destroy(&attr);
		  m->owner = pthread_self();
		}
#endif

#if !_did_init /*BSD or Solaris*/
		pthread_mutex_init(&m->lock, NIL(pthread_mutexattr_t*));
		m->owner = pthread_self();
#endif
	}

	m->state |= (flags&VT_FREE);

	return m;

#endif /*vt_threaded*/
}
