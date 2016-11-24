#include	"vthdr.h"

/*	Free a mutex.
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int vtmtxclose(Vtmutex_t* mtx)
#else
int vtmtxclose(mtx)
Vtmutex_t*	mtx;
#endif
{
#if !vt_threaded
	return -1;
#else

	if(!mtx)
		return -1;

	if(vtmtxclrlock(mtx) < 0)
		return -1;

#if _WIN32
	DeleteCriticalSection(&mtx->lock);
#else
	pthread_mutex_destroy(&mtx->lock);
#endif /*_WIN32*/

	if(mtx->state&VT_FREE)
		free(mtx);

	return 0;

#endif /*vt_threaded*/
}
