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
 *  file:	genesis_converge.c
 *
 *  author:     Wenceslau Gouveia	
 *
 *  created:	1993
 *
 *  purpose:    Sends infomation to the master to able it verify the 
 *		convergence of the algorithm. It basically send the 
 *		best member of the current population of each city
 *		to the master.
 *
 */
#include "extern.h"
#include "communication.h"
#include "pvm.h"

Check_convergence(master)
int master;
{
	int idebug;
/*
    Decoding the string to real parameter
*/
        Unpack(New[Best_guy].Gene, Bitstring, Length);
        if (Floatflag)
                FloatRep(Bitstring, Vector, Genes);

/*
    Sending best member to master processor
*/
 	pvmBeginMessage();
	pvmPutNDouble(Genes, Vector);
	pvmPutNDouble(1, &New[Best_guy].Perf);
	pvmSend(IS_IT_CONVERGING, CMASTER, master);

/*
    And waiting the answer
*/
	pvmReceive(ABOUT_THE_CONVERGENCE);
	pvmGetNInt(1, &IS_CONVERGING_FLAG);
	if (!IS_CONVERGING_FLAG)
		fprintf(stderr,"Msg==>> City %d received the RUN_OUT_OF_DIVERSITY message\n",instance);
}
/** end of file **/
