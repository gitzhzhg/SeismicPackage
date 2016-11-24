/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*
 * T - time and date for non-military types
 *
 * Usage: t
 *
 * Credit: Jack 
 */
/**************** end self doc ********************************/

# include "cwp.h"
# include <time.h>

char am_pm[] = " am";
static char *wday[] = {
			"  Sunday",
			"  Monday",
			"  Tuesday",
			"  Wednesday",
			"  Thursday",
			"  Friday",
			"  Saturday"
	     };
static char *mon[] = {
			" January",
			" Febuary",
			" March",
			" April",
			" May",
			" June",
			" July",
			" August",
			" September",
			" October",
			" November",
			" December"
		     };

int
main() {
	struct tm *timer;
	time_t seconds;

	seconds = time(0);		/* get the time */
	timer = localtime(&seconds);	/* extract date and time info */

	if (timer->tm_hour >= 12) am_pm[1] = 'p'; /* switch to pm */

	if (timer->tm_hour > 12) timer->tm_hour -= 12; /* 12 hour clock */

        timer->tm_year+=1900;
        printf("%d:%02d%s%s, %s %d, %d\n",
                timer->tm_hour, timer->tm_min, am_pm, wday[timer->tm_wday],
                        mon[timer->tm_mon], timer->tm_mday, timer->tm_year);

	return EXIT_SUCCESS;
}
