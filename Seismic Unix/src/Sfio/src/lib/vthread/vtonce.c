#include	"vthdr.h"


/*	Execute some set of actions exactly once
**
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int vtonce(Vtonce_t* once, void(*func)() )
#else
int vtonce(once, func)
Vtonce_t*	once;
void		(*func)();
#endif
{
#if !vt_threaded
	return -1;
#else
	int	rv;

	if(!func)
	{	once->error = EINVAL;
		return -1;
	}
	if(once->done)
		return 0;

#if _WIN32
	if(InterlockedIncrement(&once->once) == 1)
	{	(*func)();
		once->done = 1;
	}
	else
	{	once->once = 2;
		while(!once->done)
			Sleep(1);
	}
	rv = 0;
#else
	if((rv = pthread_once(&once->once, func)) != 0)
		once->error = rv;
	else	once->done = 1;
#endif

	return rv ? -1 : 0;

#endif /*vt_threaded*/
}
