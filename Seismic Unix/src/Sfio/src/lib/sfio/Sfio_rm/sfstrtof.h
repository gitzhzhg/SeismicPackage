/*
 * AT&T Labs Research
 * Glenn Fowler & Phong Vo
 *
 * common header and implementation for
 *
 *	strtof		strtod		strtold		_sfscand
 *
 * define these macros to instantiate an implementation:
 *
 *	S2F_function	the function name
 *	S2F_static	1 if S2F_function is static
 *	S2F_type	0:float 1:double 2:long.double
 *	S2F_scan	1 for alternate interface with these arguments:
 *				void* handle
 *				int (*getchar)(void* handle)
 *			exactly one extra (*getchar)() is done, i.e.,
 *			the caller must do the pushback
 */

#include "sfhdr.h"
#include "FEATURE/float"

#if S2F_type == 2 && _ast_fltmax_double
#undef	S2F_type
#define S2F_type	1
#endif

#if S2F_type == 0
#define S2F_number	float
#define S2F_ldexp	ldexp
#define S2F_pow10	_Sffpow10
#define S2F_huge	_Sffhuge
#define S2F_min		(FLT_MIN)
#define S2F_max		(FLT_MAX)
#define S2F_exp_10_min	(FLT_MIN_10_EXP)
#define S2F_exp_10_max	(FLT_MAX_10_EXP)
#define S2F_exp_2_min	(FLT_MIN_EXP)
#define S2F_exp_2_max	(FLT_MAX_EXP)
#endif
#if S2F_type == 1
#define S2F_number	double
#define S2F_ldexp	ldexp
#define S2F_pow10	_Sfdpow10
#define S2F_huge	_Sfdhuge
#define S2F_min		(DBL_MIN)
#define S2F_max		(DBL_MAX)
#define S2F_exp_10_min	(DBL_MIN_10_EXP)
#define S2F_exp_10_max	(DBL_MAX_10_EXP)
#define S2F_exp_2_min	(DBL_MIN_EXP)
#define S2F_exp_2_max	(DBL_MAX_EXP)
#endif
#if S2F_type == 2
#define S2F_number	long double
#define S2F_ldexp	ldexpl
#define S2F_pow10	_Sflpow10
#define S2F_huge	_Sflhuge
#define S2F_min		(LDBL_MIN)
#define S2F_max		(LDBL_MAX)
#define S2F_exp_10_min	(LDBL_MIN_10_EXP)
#define S2F_exp_10_max	(LDBL_MAX_10_EXP)
#define S2F_exp_2_min	(LDBL_MIN_EXP)
#define S2F_exp_2_max	(LDBL_MAX_EXP)
#endif

#if -S2F_exp_10_min < S2F_exp_10_max
#define S2F_exp_10_abs	(-S2F_exp_10_min)
#else
#define S2F_exp_10_abs	S2F_exp_10_max
#endif

#define S2F_batch	_ast_flt_unsigned_max_t

#if S2F_scan

typedef int (*S2F_get_f)_ARG_((void*));

#define ERR(e)
#define GET(p)		(*get)(p)
#define PUT(p)
#define SET(p,t)

#else

#define ERR(e)		(errno=(e))
#define GET(p)		(*p++)
#define PUT(p)		(end?(*end=(char*)p-1):(char*)0)
#define SET(p,t)	(t=p)

#endif

typedef struct S2F_part_s
{
	S2F_batch	batch;
	int		digits;
} S2F_part_t;

#ifndef ERANGE
#define ERANGE		EINVAL
#endif

#if S2F_static
static
#else
#if defined(__EXPORT__)
#define extern		__EXPORT__
#endif
extern
#undef	extern
#endif
S2F_number
#if S2F_scan
#if __STD_C
S2F_function(void* s, S2F_get_f get)
#else
S2F_function(s, get) void* s; S2F_get_f get;
#endif
#else
#if __STD_C
S2F_function(const char* str, char** end)
#else
S2F_function(str, end) char* str; char** end;
#endif
#endif
{
#if !S2F_scan
	register unsigned char*	s = (unsigned char*)str;
	unsigned char*		t;
#endif
	register S2F_batch	n;
	register int		c;
	register int		digits;
	register int		m;
	register unsigned char*	cv;
	int			negative;
	int			enegative;
	int			fraction;
	int			decimal = 0;
	int			thousand = 0;
	int			part = 0;
	S2F_number		v;
	S2F_number		p;
	S2F_part_t		parts[16];

	/*
	 * radix char and thousands separator are locale specific
	 */

	SFSETLOCALE(&decimal, &thousand);
	SFCVINIT();

	/*
	 * skip initial blanks
	 */

	do c = GET(s); while (isspace(c));
	SET(s, t);

	/*
	 * get the sign
	 */

	if ((negative = (c == '-')) || c == '+')
		c = GET(s);

	/*
	 * drop leading 0's
	 */

	digits = 0;
	fraction = -1;
	if (c == '0')
	{
		c = GET(s);
		if (c == 'x' || c == 'X')
		{
			/*
			 * hex floating point -- easy
			 */

			cv = _Sfcv36;
			v = 0;
			for (;;)
			{
				c = GET(s);
				if ((part = cv[c]) < 16)
				{
					digits++;
					v *= 16;
					v += part;
				}
				else if (c == decimal)
				{
					decimal = -1;
					fraction = digits;
				}
				else
					break;
			}
			m = 0;
			if (c == 'p' || c == 'P')
			{
				c = GET(s);
				if ((enegative = c == '-') || c == '+')
					c = GET(s);
				while (c >= '0' && c <= '9')
				{
					m = (m << 3) + (m << 1) + (c - '0');
					c = GET(s);
				}
				if (enegative)
					m = -m;
			}

			/*
			 * consume the optional suffix
			 */

			switch (c)
			{
			case 'f':
			case 'F':
			case 'l':
			case 'L':
				c = GET(s);
				break;
			}
			PUT(s);
			if (v == 0)
				return v;
			if (fraction >= 0)
				m -= 4 * (digits - fraction);
			if (m < S2F_exp_2_min)
			{
				if ((m -= S2F_exp_2_min) < S2F_exp_2_min)
				{
					ERR(ERANGE);
					return 0;
				}
				v = S2F_ldexp(v, S2F_exp_2_min);
			}
			else if (m > S2F_exp_2_max)
			{
				ERR(ERANGE);
				return negative ? -S2F_huge : S2F_huge;
			}
			v = S2F_ldexp(v, m);
			goto check;
		}
		while (c == '0')
			c = GET(s);
	}
	else if (c == decimal)
	{
		decimal = -1;
		fraction = 0;
		for (;;)
		{
			c = GET(s);
			if (c != '0')
				break;
			digits++;
		}
	}
	else if (c == 'i' || c == 'I')
	{
		if ((c = GET(s)) != 'n' && c != 'N' ||
		    (c = GET(s)) != 'f' && c != 'F')
		{
			PUT(t);
			return 0;
		}
		c = GET(s);
		SET(s, t);
		if (((c)          == 'i' || c == 'I') &&
		    ((c = GET(s)) == 'n' || c == 'N') &&
		    ((c = GET(s)) == 'i' || c == 'I') &&
		    ((c = GET(s)) == 't' || c == 'T') &&
		    ((c = GET(s)) == 'y' || c == 'Y'))
		{
			c = GET(s);
			SET(s, t);
		}
		PUT(t);
		return negative ? -S2F_huge : S2F_huge;
	}
	else if (c == 'n' || c == 'N')
	{
		if ((c = GET(s)) != 'a' && c != 'A' ||
		    (c = GET(s)) != 'n' && c != 'N')
		{
			PUT(t);
			return 0;
		}
		do c = GET(s); while (c && !isspace(c));
		PUT(s);
		return negative ? -S2F_huge : S2F_huge;
	}
	else if (c < '1' || c > '9')
	{
		PUT(t);

#if S2F_scan /* KPV: this returns an indicator that no number was specified */
		*((int*)s) = 1;
#endif
		return 0;
	}

	/*
	 * consume the integral and fractional parts
	 */

	n = 0;
	m = 0;
	for (;;)
	{
		if (c >= '0' && c <= '9')
		{
			digits++;
			n = (n << 3) + (n << 1) + (c - '0');
			if (n >= ((~((S2F_batch)0)) / 10) &&
			    part < sizeof(parts)/sizeof(parts[0]) )
			{
				parts[part].batch = n;
				n = 0;
				parts[part].digits = digits;
				part++;
			}
		}
		else if (m && (digits - m) != 3)
			break;
		else if (c == decimal)
		{
			decimal = -1;
			m = 0;
			fraction = digits;
		}
		else if (c != thousand)
			break;
		else if (!(m = digits))
			break;
		c = GET(s);
	}

	/*
	 * don't forget the last part
	 */

	if (n && part < sizeof(parts)/sizeof(parts[0]) )
	{
		parts[part].batch = n;
		parts[part].digits = digits;
		part++;
	}

	/*
	 * consume the exponent
	 */

	if (fraction >= 0)
		digits = fraction;
	if (c == 'e' || c == 'E')
	{
		c = GET(s);
		if ((enegative = (c == '-')) || c == '+')
			c = GET(s);
		n = 0;
		while (c >= '0' && c <= '9')
		{
			n = (n << 3) + (n << 1) + (c - '0');
			c = GET(s);
		}
		if (enegative)
			digits -= n;
		else
			digits += n;
	}

	/*
	 * consume the optional suffix
	 */

	switch (c)
	{
	case 'f':
	case 'F':
	case 'l':
	case 'L':
		c = GET(s);
		break;
	}
	PUT(s);

	/*
	 * adjust for at most one multiply per part
	 * and at most one divide overall
	 */

	if (!part)
		return 0;
	else if ((m = parts[part-1].digits - digits) > 0)
		digits += m;
	else
		m = 0;

	/*
	 * combine the parts
	 */

	v = 0;
	while (part--)
	{
		p = parts[part].batch;
		c = digits - parts[part].digits;
		if (c > S2F_exp_10_max)
		{
			ERR(ERANGE);
			v = S2F_huge;
			break;
		}
		if (c > 0)
			p *= S2F_pow10[c];
		v += p;
	}
	if (m)
	{
		while (m > S2F_exp_10_max)
		{
			m -= S2F_exp_10_max;
			v /= S2F_pow10[S2F_exp_10_max];
		}
		v /= S2F_pow10[m];
	}

	/*
	 * check the range
	 */

 check:
	if (v < S2F_min)
	{
		ERR(ERANGE);
		v = 0;
	}
	else if (v > S2F_max)
	{
		ERR(ERANGE);
		v = S2F_huge;
	}

	/*
	 * done
	 */

	return negative ? -v : v;
}
