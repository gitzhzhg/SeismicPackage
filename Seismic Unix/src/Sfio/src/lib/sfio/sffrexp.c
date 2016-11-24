#include	"sfhdr.h"

#if !_PACKAGE_ast

/*
 * frexp[l]/ldexp[l] implementation
 */

/* this is the template part. We expect the following macros to be defined:
	_dbl_t:		type of a double.
	_pow2:		name of array of power of twos.
	_initpow2:	function to initialize the table.
	_dbl_max:	max for a double value
	_dbl_max_exp:	max exponent for a double value.
*/

#if defined(_frexp_) || defined(_ldexp_)
static _dbl_t	_pow2[_dbl_max_exp];
static void _initpow2()
{
	int	x;
	_dbl_t	g;

	for(g = 1, x = 0;; x++, g *= 2)
	{	_pow2[x] = g;
		if(x == sizeof(_pow2)/sizeof(_pow2[0])-1)
			break;
	}
}
#endif /* _frexp_ || _ldexp_ */

#if defined(_frexp_) /* now define the function frexp or frexpl */
#if __STD_C
extern _dbl_t _frexp_(_dbl_t f, int* p)
#else
extern _dbl_t _frexp_(f, p)
_dbl_t	f;
int*	p;
#endif
{
	int	k, x;
	_dbl_t	g;

	/* initialize power-of-2 table. the below test handles concurrency */
	if(_pow2[sizeof(_pow2)/sizeof(_pow2[0])-1] == 0)
		_initpow2();

	/*
	 * normalize
	 */

	x = k = _dbl_max_exp / 2;
	if (f < 1)
	{
		g = ((_dbl_t)1) / f;
		for (;;)
		{
			k = (k + 1) / 2;
			if (g < _pow2[x])
				x -= k;
			else if (k == 1 && g < _pow2[x+1])
				break;
			else	x += k;
		}
		if (g == _pow2[x])
			x--;
		x = -x;
	}
	else if (f > 1)
	{
		for (;;)
		{
			k = (k + 1) / 2;
			if (f > _pow2[x])
				x += k;
			else if (k == 1 && f > _pow2[x-1])
				break;
			else	x -= k;
		}
		if (f == _pow2[x])
			x++;
	}
	else	x = 1;

	*p = x;

	/*
	 * shift
	 */

	x = -x;
	if (x < 0)
		f /= _pow2[-x];
	else if (x < _dbl_max_exp)
		f *= _pow2[x];
	else	f = (f * _pow2[_dbl_max_exp - 1]) * _pow2[x - (_dbl_max_exp - 1)];

	return f;
}

#endif /* defined(_frexp_) */

#if defined(_ldexp_)
#if __STD_C
extern _dbl_t _ldexp_(_dbl_t f, int x)
#else
extern _dbl_t _ldexp_(f, x)
_dbl_t	f;
int	x;
#endif
{
	if(_pow2[0] == 0)
		_initpow2();

	if (x < 0)
		f /= _pow2[-x];
	else if (x < _dbl_max_exp)
		f *= _pow2[x];
	else	f = (f * _pow2[_dbl_max_exp - 1]) * _pow2[x - (_dbl_max_exp - 1)];
	return f;
}
#endif /* defined(_ldexp_) */

#if !_lib_frexp
#undef _lib_frexp
#define _lib_frexp	1

#undef _frexp_
#undef _ldexp_
#undef _dbl_t
#undef _initpow2
#undef _pow2
#undef _dbl_max_exp

#define _frexp_		frexp
#define _ldexp_		ldexp
#define _dbl_t		double
#define _initpow2	initdblpow2
#define _pow2		dblpow2
#define _dbl_max_exp	DBL_MAX_EXP

#include	"sffrexp.c"
#endif /*!_lib_frexp*/

/* now define frexpl and ldexpl as needed */
#if !_lib_frexpl && _ast_fltmax_double && _lib_frexp
#undef _lib_frexpl
#define _lib_frexpl	1	/* frexpl will be redefined to frexp */
#endif

#if !_lib_frexpl
#undef _lib_frexpl
#define _lib_frexpl	2

#undef _frexp_
#undef _ldexp_
#undef _dbl_t
#undef _initpow2
#undef _pow2
#undef _dbl_max_exp

#define _frexp_		frexpl
#define _ldexp_		ldexpl
#define _dbl_t		_ast_fltmax_t
#define _initpow2	initldblpow2
#define _pow2		ldblpow2
#if defined(LDBL_MAX_EXP)
#define _dbl_max_exp	LDBL_MAX_EXP
#else
#define _dbl_max_exp	DBL_MAX_EXP
#endif

#include	"sffrexp.c"
#endif /* !_lib_frexpl */

#endif /*!_PACKAGE_ast*/

#if _PACKAGE_ast || _lib_frexpl == 1
int _Sf_frexpl_in_standard_library = 1;
#endif
