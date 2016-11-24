/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	display.c
 *
 *  purpose:	manages the display for interactive use
 *
 *  modified:	10 sep 90
 */
 
#include "extern.h"

Dtrace(s)
	char *s;
{
	/* write currently executing function name to display */
	if (Displayflag)
	{
		move(1,46);
		clrtoeol();
		printw("%s", s);
		refresh();

		/* delay in order to make the action intelligible */
#if TURBOC
		/* delay(250); */ /* delay 250 ms */
#else
		/* sleep(1); */   /* delay 1 sec (too long!) */
#endif
	}
}


Interactive() {
	char cmd[40];
	char opt[40];
	register int i;
	int ncycles;
	int ok;
		
	ncycles = 1;
 	while (ok) {
		ok = 1;
		move(22,0);
		clrtoeol();
		move(22,35);
		printw("q (clear & exit), x (exit), <n> (do n gens)");
		move(22,0);
		printw("                                  ");
		refresh();
		move(22,0);
		printw("gens[%d]: ", ncycles);
		refresh();
		getstr(cmd);
		if (strcmp(cmd, "q") == 0) {
			/*if (Lastflag)
				Checkpoint(Ckptfile);
			else
				if (Savesize)
					Printbest();*/
			clear();
			die();
		}
		if (strcmp(cmd, "x") == 0) {
			/*if (Lastflag)
				Checkpoint(Ckptfile);
			else
				if (Savesize)
					Printbest();*/
			move(23,0);
			die();
		}
		if (strcmp(cmd, "") != 0) {
			if (sscanf(cmd, "%d", &ncycles) !=1)
			{
				move(23,0);
				clrtoeol();
				printw("unknown command: %s", cmd);
				ok = 0;
				refresh();
			}
		}
		if (ok)
		{
			move(23,0);
			clrtoeol();
			move(1,0);
			printw("run until Gens = %d", Gen + ncycles -1);
			move(1,35);
			printw("executing: ");
			refresh();
			for (i=0; i < ncycles; i++)
				Generate();
		}
	}
}



die(sig)
int sig;
{
	sig++;
	signal(SIGINT, SIG_IGN);
	move(23,0);
	clrtoeol();
	refresh();	
	endwin();
	exit(0);
}


#if TURBOC

/* Turbo C versions of curses functions */

move(row, col)
	int row, col;
{
	/* move to row, col coordinates of the screen */
	/* (0,0) = upper left corner; (23,79) = lower right corner */
	
	gotoxy(col+1, row+1);	
}


clear()
{
	/* clear the screen */
	clrscr();
}


getstr(s)
     char *s;
{
  getw(s);
}


initscr() {}

endwin() {}

#endif
