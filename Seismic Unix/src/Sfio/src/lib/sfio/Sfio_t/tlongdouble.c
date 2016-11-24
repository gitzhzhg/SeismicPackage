#include	"sftest.h"

#if _typ_long_double
#include	<float.h>
#endif

MAIN()
{
#if _typ_long_double
	long double	ldval, ldmax;
	char		*s, *str;

	ldmax = LDBL_MAX;

	if(!(s = sfprints("%Le",ldmax)) )
		terror("sfprints failed1\n");
	if(!(str = malloc(strlen(s)+1)) )
		terror("Malloc failed\n");
	strcpy(str,s);

	if(sfsscanf(str,"%Le",&ldval) != 1)
		terror("sfsscanf failed\n");
	if(!(s = sfprints("%Le",ldval)) )
		terror("sfprints failed2\n");

	if(strcmp(s,str) != 0)
		terror("Bad conversion, expecting %s and getting %s\n",str,s);
#endif

	TSTEXIT(0);
}
