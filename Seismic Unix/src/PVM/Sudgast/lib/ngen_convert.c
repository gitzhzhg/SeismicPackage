/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	convert.c
 *
 *  purpose:	functions that translate between various representations
 *
 *  modified:	16 apr 86
 *		15 sep 90: translations for floating point representation
 */

#include "extern.h"

static char BIT[CHARSIZE] ={ '\200', '\100', '\040', '\020',
				'\010', '\004', '\002', '\001'};

/* Itoc and Ctoi translate ints to strings and vice versa */


unsigned long int Ctoi(instring, length)
	char *instring;		/* string representation	*/
	int length;		/* length of instring		*/
{
	register int i;		/* loop control			*/
	unsigned long n;	/* accumulator for return value	*/

	n = (unsigned long) 0;
	for (i=0; i<length; i++)
	{
		n <<= 1;
		n += (*instring++ - (int) '0');
	}
	return(n);
}

Itoc(n, outstring, length)
	unsigned long int n;	/* input int value		*/
	char *outstring;	/* string representation	*/
	int length;		/* length of outstring		*/
{
	register int i;		/* loop control			*/

	for (i=length-1; i>=0; i--)
	{
		outstring[i] = '0' + (n & 1);
		n >>= 1;
	}
}

/* Pack and Unpack translate between strings and (packed) bit arrays */

Pack(instring, outstring, length)
	char *instring;		/* string representation		*/
	char *outstring;	/* packed representation of instring	*/
	int length;		/* length of instring			*/
{
	static firstflag = 1 ;
	static full;	/* number of fully used bytes in outstring	*/
	static slop;	/* number of bits used in outstring's last byte	*/
	register i,j;	/* loop control					*/

	if (firstflag)
	{
		full = length / CHARSIZE;
		slop = length % CHARSIZE;
		firstflag = 0;
	}

	for (i=0; i<full; i++, outstring++)
	{
		*outstring = '\0';
		for (j=0; j < CHARSIZE; j++)
			if (*instring++ == '1')  *outstring |= BIT[j];
	}
	if (slop)
	{
		*outstring = '\0';
		for (j=0; j < slop; j++)
			if (*instring++ == '1')  *outstring |= BIT[j];
	}
}

Unpack(instring, outstring, length)
	char *instring;		/* packed bit representation		*/
	char *outstring;	/* string representation of instring	*/
	int length;		/* length of outstring			*/
{
	static firstflag = 1 ;
	static full;	/* number of fully used bytes in instring	*/
	static slop;	/* number of bits used in instring's last byte	*/
	register i,j;	/* loop control					*/

	if (firstflag)
	{
		full = length / CHARSIZE;
		slop = length % CHARSIZE;
		firstflag = 0;
	}

	for (i=0; i<full; i++, instring++)
	{
		for (j=0; j < CHARSIZE; j++)
			if (*instring & BIT[j])
				*outstring++ = '1';
			else
				*outstring++ = '0';
	}

	if (slop)
	{
		for (j=0; j < slop; j++)
			if (*instring & BIT[j])
				*outstring++ = '1';
			else
				*outstring++ = '0';
	}
	*outstring = '\0';
}


/* Translations between fixed point ints and reflected Gray code */


Gray(instring, outstring, length)
char *instring;		/* string representing fixed point int		*/
char *outstring;	/* string representing Gray coded value		*/
register int length;	/* length of strings				*/
{
	register int i;
	register char last;

	last = '0';
	for (i=0; i<length; i++)
	{
		outstring[i] = '0' + (instring[i] != last);
		last = instring[i];
	}
}


Degray(instring, outstring, length)
char *instring;		/* string representing Gray coded int		*/
char *outstring;	/* string representing fixed point int		*/
register int length;	/* length of strings				*/
{
	register int i;
	register int last;

	last = 0;
	for (i=0; i<length; i++)
	{
		if (instring[i] == '1')
			outstring[i] = '0' + (!last);
		else
			outstring[i] = '0' + last;
		last = outstring[i] - '0';
	}
}


/* Translations between string representation and floating point vectors */


FloatRep(instring, vect, length)
	char instring[];	/* string representation		*/
	double vect[];		/* floating point representation	*/
	int length;		/* length of vect (output array)	*/
{
	register int i;		/* loop control				*/
	unsigned long int n;	/* decoded int value			*/
	register int pos;	/* position to start decoding		*/
	char tmpstring[80];	/* used for gray code interpretation	*/

	pos = 0;
	for (i=0; i < length; i++)
	{
		if (Grayflag)
		{
			Degray(&instring[pos], tmpstring, Gene[i].bitlength);
			n = Ctoi(tmpstring, Gene[i].bitlength);
		}
		else
		{
			n = Ctoi(&instring[pos], Gene[i].bitlength);
		}
		vect[i] = Gene[i].min + n*Gene[i].incr;
		pos += Gene[i].bitlength;
	}
}


StringRep(vect, outstring, length)
	double *vect;		/* floating point representation	*/
	char *outstring;	/* string representation		*/
	int length;		/* length of vect 			*/
{
	register int i;		/* loop control				*/
	unsigned long int n;	/* index of vext[i] within legal range	*/
	register int pos;	/* next position for filling outstring	*/
	char tmpstring[80];	/* used for gray code translation	*/
	
	pos = 0;
	for (i=0; i < length; i++)
	{
		/* convert floating value to an index */
		n = (int) ((vect[i] - Gene[i].min) / Gene[i].incr + 0.5);

		/* encode n in char string */
		if (Grayflag)
		{
			/* convert to Gray code */
			Itoc(n, tmpstring, Gene[i].bitlength);
			Gray(tmpstring, &outstring[pos], Gene[i].bitlength);
		}
		else
		{
			Itoc(n, &outstring[pos], Gene[i].bitlength);
		}
		pos += Gene[i].bitlength;
	}
	outstring[pos] = '\0';
}

/*** end of file ***/
