#include	"sfstdio.h"

/*	Internal printf engine to write to a string buffer
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int vsprintf(char* s, const char* form, va_list args)
#else
int vsprintf(s,form,args)
reg char*	s;
reg char*	form;
va_list		args;
#endif
{
	return (s && form) ? (int)sfvsprintf(s,SF_BUFSIZE,form,args) : -1;
}


#if __STD_C
int vasprintf(char** as, const char* form, va_list args)
#else
int vasprintf(as,form,args)
reg char**	as;
reg char*	form;
va_list		args;
#endif
{
	return (as && form) ? (int)sfvaprints(as,form,args) : -1;
}
