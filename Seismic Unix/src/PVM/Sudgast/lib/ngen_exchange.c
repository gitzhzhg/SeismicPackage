/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*************************************************************/
/*                                                           */
/*  Copyright (c) 1993                                       */
/*  Wenceslau Gouveia                                        */
/*  Center for Wave Phenomena				     */
/*  Colorado School of Mines                                 */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/
/*
 *  file:	genesis_exchange.c
 *
 *  author:     Wenceslau Gouveia	
 *
 *  created:	1993
 *
 *  purpose:    Manages all the exchange of information among different     
 *	        cities	
 *	
 */

#include "communication.h"
#include "extern.h"
#include "pvm.h"

/*
    Related to random generation
*/
extern int Randint();

/*
    Numerical Recipes using C
    Routine INDEXX - Chapter 8, section 3, pg. 249
    Sorting routine (Ascending order)
*/

indexx(n,arrin,indx)
int n,indx[];
double arrin[];
{
        int l,j,ir,indxt,i;
        float q;
        for (j=1;j<=n;j++) indx[j]=j;
        if (n == 1) return;
        l=(n >> 1) + 1;
        ir=n;
        for (;;) {
                if (l > 1)
                        q=arrin[(indxt=indx[--l])];
                else {
                        q=arrin[(indxt=indx[ir])];
                        indx[ir]=indx[1];
                        if (--ir == 1) {
                                indx[1]=indxt;
                                return;
                        }
                }
                i=l;
                j=l << 1;
                while (j <= ir) {
                        if (j < ir && arrin[indx[j]] < arrin[indx[j+1]]) j++;
                        if (q < arrin[indx[j]]) {
                                indx[i]=indx[j];
                                j += (i=j);
                        }
                        else j=ir+1;
                }
                indx[i]=indxt;
        }
}

Exchange_members()
{
	FILE *fopen(), *fp;

	int send;	
                        	/* define city that will receive new members */
	int the_member, imember, k;
			        /* defines to be received / transmitted   */
	int not_change;         /* signals if the member was accepted     */ 
        int jdebug, vdebug;
	int member_exchange;	/* # of exchanged members                 */
	double maxperf_city;    /* max performance in town */

/*
    Now 10% of the last evolved population will be chosen to migrate 
    to other city. This choice will be done radomly
*/

        send = (instance + 1) % Ncities;
        pvmBeginMessage();
	pvmSend(NEW_MEMBERS,CITIES,send);	/* Syncronizations purposes */

	pvmReceive(NEW_MEMBERS);

	member_exchange = NINT(.1 * Popsize);

/*
    Ordering city members
    Note that imember goes from 1 to Popsize, due NUMERICAL RECIPES
    The sort will procude a WORST MEMBER ---> BEST MEMBER list
*/ 

	for (imember = 0; imember < Popsize; imember++)
	{
		performance[imember + 1] = - New[imember].Perf;
	}
        indexx(Popsize,performance,indx_new);
/*
    Sending members some best members and some random ones
*/
	if (RandomSent) 
	{
		if (verbose)
		{
			fprintf(stderr,"Subpopulation %d will send %d members at random to subpopulation %d\n",instance, member_exchange,send);
		}
	}
	else
	{
		if (verbose)
		{
			fprintf(stderr,"Subpopulation %d will send the %d BEST and %d RANDOM members to subpopulation %d\n",instance,NINT(.3 * member_exchange) + 1,member_exchange - NINT(.3 * member_exchange) - 1,send);
		}
	}

	for (imember = 0; imember < member_exchange; imember++)
	{
		if (RandomSent) 
			the_member = Randint(0,Popsize-1);
		else
		{
			/* 30% of Best members */
			if (imember <= NINT(.3 * member_exchange)) 
				the_member = indx_new[Popsize - imember] - 1;   
			else
				the_member = Randint(0,Popsize-1);
		}
		pvmBeginMessage();
		pvmPutNByte(Bytes, (void*) New[the_member].Gene);
		pvmPutNDouble(1,&New[the_member].Perf);
		pvmPutNInt(1,&New[the_member].Needs_evaluation);
		pvmSend(FROM_THE_MEMBER,CITIES,send);
	}
/*
    Now the incoming members will be received
*/
	for (imember = 0; imember < member_exchange; imember++)
	{
		pvmBeginMessage();
		pvmReceive(FROM_THE_MEMBER);
		pvmGetNByte(Bytes, (void*) Exchange[imember].Gene);
		pvmGetNDouble(1,&Exchange[imember].Perf);
		pvmGetNInt(1,&Exchange[imember].Needs_evaluation);
	}
/*
    Now the members will be inserted in the population of THIS city if
    they have a better performance than them.
    The arrays New[] and Exchange[] will be ordered in sorted in
    descending order of performance. 

    Sorting incoming members
    Note that the performance is - evaluation of objective function
    The sort will procude a WORST MEMBER ---> BEST MEMBER list
*/ 
	for (imember = 0; imember < member_exchange; imember++)
	{
		performance[imember + 1] = (double) - Exchange[imember].Perf;
	}

        indexx(member_exchange,performance,indx_exchange);
/* 
    That's the best performance in town 
*/
	maxperf_city = New[indx_new[Popsize] - 1].Perf;
	if (Exchange[indx_exchange[member_exchange]-1].Perf > New[indx_new[1]-1].Perf)
		return;
/*
    In this situation the incoming members did not help
    That is the largest performance of the incoming members is smaller
    than the smallest performance of the city members
*/
	the_member = 1;		/* that's the WORST member in city */
	for (imember = member_exchange; imember > 0; imember--)
	{
		not_change = 1;   	/* tells if change happened */
/*
    Comparing performances
*/
		if (Exchange[indx_exchange[imember]-1].Perf < New[indx_new[the_member]-1].Perf)
		{
/*
    A good member arrived
*/
			not_change = 0;
/*
    YOU SHOULD USE THIS TRANFERENCE BETWEEN STRUCTURES OTHERWISE
    THE PROGRAM C*R*A*S*H*E*S
*/
			for (k = 0; k < Bytes; k++)
				New[indx_new[the_member]-1].Gene[k] = Exchange[indx_exchange[imember]-1].Gene[k];
			New[indx_new[the_member]-1].Perf = Exchange[indx_exchange[imember]-1].Perf;
			New[indx_new[the_member]-1].Needs_evaluation = 0;


			if (Exchange[indx_exchange[imember]-1].Perf < maxperf_city)
			{
/*
    Best performance was reached
*/
				maxperf_city = New[indx_new[the_member]-1].Perf;
				if (verbose)
				{
					fprintf(stderr,"Subpopulation %d had its performance improved by incoming members\n\n",instance);
				}
			}
		}
			if (not_change) return;
			the_member++;	/* next WORST model in city */
	}
/*
    The new members were added to the population the GA proceeds again
*/
/*  

    Sorting city members
    Note that imember goes from 1 to Popsize, due NUMERICAL RECIPES
    The sort will procude a WORST MEMBER ---> BEST MEMBER list
*/
	return;
}
/** end of file **/

