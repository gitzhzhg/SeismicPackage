/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	measure.c
 *
 *  purpose:	calculate performance measures and append them
 *		to the output file.
 *
 *  modified:	26 mar 86
 *
 *		2 dec 86: call Converge() right before output,
 *		and fake remainder of output if Bias > 0.99
 *
 *		10 sep 90: print statistics in display mode,
 *              handle max or min problems.
 */

#include "extern.h"
#include "cwp.h"

extern void Monitor_best();

#define DISPMEAS 4

Measure(current_evolution)
int current_evolution;
{
	double New_worst();
	FILE *fp, *fopen();
	register int i;
	register int w;
	register double performance;
	int j;

	Trace("Measure entered");
	Dtrace("measure");

	for (i=0; i<Popsize; i++)
	{
		/* update current statistics */
		performance = New[i].Perf;
		if (i>0)
		{
			Ave_current_perf += performance;
			if (BETTER(performance, Best_current_perf))
			{
				Best_current_perf = performance;
				Best_guy = i;
			}
			if (BETTER(Worst_current_perf, performance))
			{
				Worst_current_perf = performance;
				Worst_guy = i;
			}
		}
		else
		{
			Best_current_perf = performance;
			Worst_current_perf = performance;
			Ave_current_perf = performance;
			Best_guy = 0;
			Worst_guy = 0;
		}
	}

        Ave_current_perf /= Popsize;

	/* Building a file with the performance of the best member 
	   if FlowBestflag is active					*/

        if (FlowBestflag) Monitor_best();

	/* update Worst */
	if (Windowsize)
	{
		/* Worst = worst in last (Windowsize) generations */
		w = Gen % Windowsize;
		Window[w] = New_worst();
		Worst = Window[0];
		for (i=1; i < Windowsize; i++)
			if (BETTER(Worst, Window[i])) Worst = Window[i];
	}
	else
		if (BETTER(Worst, Worst_current_perf))
			Worst = New_worst();

	/* update overall performance measures */
	if (Trials == 0) 
	{
		Online = Onsum;
		Offline = Offsum;
	}
	else
	{
		Online = Onsum / Trials;
		Offline = Offsum / Trials;
	}

	if (Traceflag)
	{
		printf("     Gen %d     Trials %d\n",Gen,Trials);
		if (Onlnflag) printf("     Online %e\n", Online);
		if (Offlnflag) printf("     Offline %e\n", Offline);
	}

	if (Displayflag)
	{
		static firstflag = 1;
		if (firstflag)
		{
			firstflag = 0;
			move(DISPMEAS - 1,0);
			printw("Gens Trials Lost ");
			printw("Conv  Bias       Online      ");
			printw("Offline         Best      Average");
		}

		move(DISPMEAS,0);
		clrtoeol();
		Converge();
		printw("%4d %6d %4d ",
			Gen, Trials, Lost);
		printw("%4d %5.3f  %11.4f  ",
			Conv, Bias, Online);
		printw("%11.4f  %11.4f  %11.4f",
			Offline, Best, Ave_current_perf);

		move(DISPMEAS+2,0);
		clrtoeol();
		printw("Current Best Structure: %3d  ", Best_guy);
		printw("Performance: %0.4f ", New[Best_guy].Perf);
		move(DISPMEAS+3,0);
		clrtoeol();
		Unpack(New[Best_guy].Gene, Bitstring, Length);
		if (Floatflag)
		{
			FloatRep(Bitstring, Vector, Genes);
			for (j=0; j<Genes; j++)
				printw(Gene[j].format, Vector[j]);
		}
		else
		{
			printw("%s", Bitstring);
		}
		refresh();
	}
	
	if ( Interval && Collectflag && ((Trials >= Plateau) || Doneflag))
	{
		/* add measures to the output file */
		Converge();
                if (current_evolution == Numevolutions - 1 || !IS_CONVERGING_FLAG)
		{
			/*fp = fopen(Outfile, "a");
			fprintf(fp,OUT_F2, OUT_V2);*/

			if (Bias > 0.99)
			{
				int temp;
				temp = Trials;
				while (temp < Totaltrials)
				{
					temp += Interval;
					Gen += (Interval/Popsize)?(Interval/Popsize):1;
					fprintf(fp,OUT_F2,OUT_V2);
				}
				Spin = Maxspin;
			}
			/*fclose(fp);
			remove(Outfile);*/	/* no more use for that */
		}
		Plateau = (Trials/Interval)*Interval + Interval;
	}

	if (Logflag  && (Spin >= Maxspin))
	{
		fp = fopen(Logfile, "a");
		fprintf(fp, "Experiment %1d  ", Experiment);
		fprintf(fp, "SPINNING at Gen %1d, ",Gen);
		fprintf(fp, "after %1d Trials\n", Trials);
		fclose(fp);
	}

	if ( Interflag && (Spin >= Maxspin))
	{
		move(22,0);
		clrtoeol();
		printw("SPINNING at Gen %1d, ",Gen);
		printw("after %1d Trials\n", Trials);
		refresh();
	}

	Trace("Measure completed");
}


double New_worst()
{
	double delta;
	
	/* return a value a little worse than Worst_current_perf */
	
	if (Maxflag)
		delta = 1.0e-4;
	else
		delta = -1.0e-4;

	if (Worst_current_perf == 0.0) return (-delta);

	if (Worst_current_perf > 0.0)
		return (Worst_current_perf*(1.0 - delta));

	return (Worst_current_perf*(1.0 + delta));
}



static char BIT[CHARSIZE] ={ '\200', '\100', '\040', '\020',
			'\010', '\004', '\002', '\001'};


Converge()			/* measure population convergence	*/
{
	register int i,j;	/* loop control				*/
	register int ones;	/* number of ones in a given position	*/
	int focus;		/* index of current byte		*/
	int bit;		/* index of current bit			*/
	FILE *fp, *fopen();

	Trace("Converge entered");

	Bias = 0.0;
	Lost = Conv = 0;
	if (!Convflag) return;

	for (j = 0; j < Length; j++)
	{
		focus = j / CHARSIZE;
		bit = j % CHARSIZE;
		ones = 0;
		for (i=0; i < Popsize; i++)
			ones += ((New[i].Gene[focus] & BIT[bit]) != 0);
		Lost += (ones == 0) || (ones == Popsize);
		Conv += (ones <= FEW) || (ones >= Popsize - FEW);
		Bias += (ones > Popsize/2) ? ones : (Popsize - ones);
	}

	Bias /= (Popsize*Length);

	if (Logflag && (Lost==Length))
	{
		fp = fopen(Logfile, "a");
		fprintf(fp, "CONVERGED at Gen %1d, ",Gen);
		fprintf(fp, "after %1d Trials\n", Trials);
		fclose(fp);
	}

	Trace("Converge completed");
}


/** end of file **/

