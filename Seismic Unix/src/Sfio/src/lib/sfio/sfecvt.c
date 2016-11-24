#include	"sfhdr.h"

#if __STD_C
char* sfecvt(double dval, int n_digit, int* decpt, int* sign)
#else
char* sfecvt(dval,n_digit,decpt,sign)
double	dval;		/* value to convert */
int	n_digit;	/* number of digits wanted */
int*	decpt;		/* to return decimal point */
int*	sign;		/* to return sign */
#endif
{
	int		len;
	static char	buf[SF_MAXDIGITS];

	return _sfcvt((Sfdouble_t)dval,buf,sizeof(buf),n_digit,decpt,sign,&len,SFFMT_EFORMAT);
}
