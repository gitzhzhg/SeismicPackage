#include	"vthdr.h"


/*	Unlock a mutex.
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int vtmtxunlock(Vtmutex_t* mtx)
#else
int vtmtxunlock(mtx)
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
#define _did_unlock	1
	EnterCriticalSection(&mtx->lock);
	if(mtx->count <= 0)
	{	mtx->error = EINVAL;
		rv = -1;
	}
	else
	{	rv = 0;
		mtx->count -= 1;
		LeaveCriticalSection(&mtx->lock);
	}
	LeaveCriticalSection(&mtx->lock);
	return rv;
#endif /*_WIN32*/

#if !_did_unlock && _mtx_recursive
#define _did_unlock	1
	if((rv = pthread_mutex_lock(&mtx->lock)) == MTXLOCK_OK)
	{	if(mtx->count <= 0)
		{	mtx->error = EPERM;
			rv = -1;
		}
		else
		{	mtx->count -= 1;
			pthread_mutex_unlock(&mtx->lock);
			rv = 0;
		}
		pthread_mutex_unlock(&mtx->lock);
		return rv;
	}
	else
	{	mtx->error = EPERM;
		return -1;
	}
#endif

#if !_did_unlock && _mtx_errorcheck
#define _did_unlock	1
	if((rv = pthread_mutex_lock(&mtx->lock)) == MTXLOCK_OK)
	{	mtx->count = 0;
		mtx->owner = pthread_self();
		pthread_mutex_unlock(&mtx->lock);
		return 0;
	}
	else if(rv == EDEADLK && pthread_equal(pthread_self(), mtx->owner) )
	{	if((mtx->count -= 1) > 0)
			return 0;
		else
		{	mtx->count = 0;
			pthread_mutex_unlock(&mtx->lock);
			return 0;
		}
	}
	else
	{	mtx->error = EPERM;
		return -1;
	}
#endif

#if !_did_unlock /* bsd, solaris */
	if((rv = pthread_mutex_trylock(&mtx->lock)) == MTXTRYLOCK_OK)
	{	mtx->count = 0;
		mtx->owner = pthread_self();
		pthread_mutex_unlock(&mtx->lock);
		return 0;
	}
	else if(MTXBUSY(rv) && pthread_equal(pthread_self(), mtx->owner) )
	{	if((mtx->count -= 1) > 0)
			return 0;
		else
		{	mtx->count = 0;
			pthread_mutex_unlock(&mtx->lock);
			return 0;
		}
	}
	else
	{	mtx->error = EPERM;
		return -1;
	}
#endif

#endif /*vt_threaded*/
}
