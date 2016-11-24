#include	"vthdr.h"

/*	Set attributes of a thread
**
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int vtset(Vthread_t* vt, int type, Void_t* val)
#else
int vtset(vt, type, val)
Vthread_t*	vt;
int		type;
Void_t*		val;
#endif
{
#if !vt_threaded
	return -1;
#else
	int	rv;

	if(!vt)
		return -1;

	if(type == VT_STACK)
	{
		vt->stack = (size_t)val;
#if _WIN32
		rv = 0;
#else
		if((rv = pthread_attr_setstacksize(&vt->attrs, (size_t)val)) != 0)
			vt->error = rv;
#endif
	}
	else
	{	vt->error = EINVAL;
		rv = -1;
	}

	return rv ? -1 : 0;

#endif /*vt_threaded*/
}
