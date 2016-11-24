/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*************************************************************/
/*                                                           */
/*  Copyright (c) 1993                                       */
/*  Wenceslau Gouveia                                        */
/*  Center for Wave Phenomena      	                     */
/*  Colorado School of Mines                                 */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/

/*
 *  file:	genesis_monitor.c
 *
 *  author:     Wenceslau Gouveia	
 *
 *  created:	1993
 *
 *  purpose:    monitor performance of BEST member in each generated 
 *		population.
 *
 */
#include "extern.h"
#include "cwp.h"

Monitor_best()
{
        FILE *fopen(), *fp;
/*  
    The BEST member of this generation is pointed by Best_guy
*/
        fp = fopen(FlowBestfile, "a");
	fprintf(fp, "%4d  %14.7f\n", TrialTotal, ABS(New[Best_guy].Perf));
	fclose(fp);
}

/** end of file **/
