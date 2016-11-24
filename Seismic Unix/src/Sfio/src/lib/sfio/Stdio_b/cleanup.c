#include	"sfstdio.h"

/*
**	Cleanup actions before exiting
**	Written by Kiem-Phong Vo.
*/

#if !_PACKAGE_ast && !_lib_atexit && !_lib_on_exit && !_lib_onexit && _exit_cleanup
static int _Sfio_already_has_cleanup;
#else

#if __STD_C
void _cleanup(void)
#else
void _cleanup()
#endif
{
	if(_Sfcleanup)
		(*_Sfcleanup)();
}

#endif
