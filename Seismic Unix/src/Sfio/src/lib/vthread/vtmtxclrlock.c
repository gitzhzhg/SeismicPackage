#include	"vthdr.h"


/*	Clear all locks on a mutex.
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int vtmtxclrlock(Vtmutex_t* mtx)
#else
int vtmtxclrlock(mtx)
Vtmutex_t*	mtx;
#endif
{
#if !vt_threaded
	return -1;
#else
	int	rv;

	if(!mtx )
		return -1;

#if _WIN32
#define _did_clrlock	1
	EnterCriticalSection(&mtx->lock);
	for(; mtx->count > 0; mtx->count -= 1)
		LeaveCriticalSection(&mtx->lock);
	mtx->count = 0;
	LeaveCriticalSection(&mtx->lock);
	return rv=0;
#endif /*_WIN32*/

#if !_did_clrlock && _mtx_recursive
#define _did_clrlock	1
	if((rv = pthread_mutex_lock(&mtx->lock)) == MTXLOCK_OK )
	{	for(; mtx->count > 0; mtx->count -= 1)
			pthread_mutex_unlock(&mtx->lock);
		mtx->count = 0;
		pthread_mutex_unlock(&mtx->lock);
		return 0;
	}
	else
	{	mtx->error = rv;
		return -1;
	}
#endif

#if !_did_clrlock && _mtx_errorcheck && !_hpux_pthread
#define _did_clrlock	1
	if((rv = pthread_mutex_lock(&mtx->lock)) == MTXLOCK_OK )
	{	mtx->count = 0;
		mtx->owner = pthread_self();
		pthread_mutex_unlock(&mtx->lock);
		return 0;
	}
	else if(rv == EDEADLK && pthread_equal(pthread_self(), mtx->owner) )
	{	mtx->count = 0;
		pthread_mutex_unlock(&mtx->lock);
		return 0;
	}
	else
	{	mtx->error = rv;
		return -1;
	}
#endif

#if !_did_clrlock
	if((rv = pthread_mutex_trylock(&mtx->lock)) == MTXTRYLOCK_OK )
	{	mtx->count = 0;
		mtx->owner = pthread_self();
		pthread_mutex_unlock(&mtx->lock);
		return 0;
	}
	else if(MTXBUSY(rv) && pthread_equal(pthread_self(), mtx->owner) )
	{	mtx->count = 0;
		pthread_mutex_unlock(&mtx->lock);
		return 0;
	}
	else
	{	mtx->error = rv;
		return -1;
	}
#endif

#endif /*vt_threaded*/
}
