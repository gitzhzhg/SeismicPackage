#include	"stdtest.h"
#if _lib_locale
#include	<locale.h>
#endif

MAIN()
{
#if _lib_locale
	char		buf[128], cmp[128];
	float		d;
	int		n, decimal, thousand;
	struct lconv*	lv;

	setlocale(LC_ALL, "en");

	if(!(lv = localeconv()))
		TSTEXIT(0);

	decimal = '.';
	if(lv->decimal_point && lv->decimal_point[0])
		decimal = lv->decimal_point[0];

	thousand = 0;
	if(lv->thousands_sep && lv->thousands_sep[0])
		thousand = lv->thousands_sep[0];
		
	if(thousand)
		sprintf(cmp,"1%c000", thousand);
	else	sprintf(cmp,"1000");
	sprintf(buf, "%'d", 1000);
	if(strcmp(buf, cmp) != 0)
		terror("Bad printing");
	
	if(thousand)
		sprintf(cmp, "1%c000%c10", thousand, decimal);
	else	sprintf(cmp, "1000%c10", decimal);
	d = 0.;
	if((n = sscanf(cmp, "%'f", &d)) != 1)
		terror("Scan error %d", n);
	if(d < 1000.099 || d > 1000.101)
		terror("Bad scanning");
	sprintf(buf, "%.2f", d);
	if(strcmp(buf, "1000.10") != 0)
		terror("Deep formatting error");
#endif

	TSTEXIT(0);
}
