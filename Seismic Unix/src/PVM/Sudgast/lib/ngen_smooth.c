/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "extern.h"
#include "cwp.h"
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
 *  file:	genesis_smooth.c
 *
 *  author:     Wenceslau Gouveia	
 *
 *  created:	1993
 *
 *  purpose:	Smooth receiver statics 
 *
 */

void Smooth()
{
	int imember; 		/* current member 	*/
	int i, j, igene;	/* counters		*/
	int begin, end;		/* limit the operator 	*/
	static double *Vector_Aux;
	static int first=1;

	if (first)
	{
	        Vector_Aux = 
			(double *) calloc((unsigned) Genes, sizeof(double));
                if (Vector_Aux == NULL) 
				Error("Allocation failed for Vector_Aux");
		first = 0;
	}

/*
    For all the members of the current population 
*/
	if (verbose) 
		fprintf(stderr,"\nSubpopulation %d smoothing trial solutions\n", instance);

	for (imember = 0; imember < Popsize; imember++)
	{
        	Unpack(New[imember].Gene, Bitstring, Length);
                FloatRep(Bitstring, Vector, Genes);

		for (igene = 0, Vector_Aux[igene] = 0.; 
		     igene < NSOURCES + NRECEIVERS; 
		     igene++, Vector_Aux[igene] = 0.)
		{
			if (igene < NSOURCES)
			{
				/* no smooth of source statics */
				Vector_Aux[igene] = Vector[igene];
			}	
			else
			{
				i = to_filter[igene - NSOURCES][0];
				j = to_filter[igene - NSOURCES][1];

				if (to_filter[igene - NSOURCES][2] == 2)
					Vector_Aux[igene] = 
						(Vector[i] + Vector[j]) / 2.;
				else
					Vector_Aux[igene] =
						(Vector[i] + Vector[igene] + 
						 Vector[j]) / 3.;
			}
		}
/*
    Packing
*/
                StringRep(Vector_Aux, Bitstring, Genes);
                Pack(Bitstring, New[imember].Gene, Length);
		New[imember].Needs_evaluation = 1;
	}
}
/** end of file **/
