/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
#include "cprim.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define LINELEN 80

/*
 ***************************** COPYRIGHT NOTICE *************************
 *                                                                      *
 *                 CONFIDENTIAL AND PROPRIETARY INFORMATION             *
 *                              OF CONOCO INC.                          *
 *                      PROTECTED BY THE COPYRIGHT LAW                  *
 *                          AS AN UNPUBLISHED WORK                      *
 *                                                                      *
 ***************************** COPYRIGHT NOTICE *************************
         1         2         3         4         5         6         7  |
123456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
 * --------------------------------------------------------------------
 * Name   : strut (string utilities)
 * Author : Richard Day
 * Date   : 01/19/95
 * Revised: 10/22/96  Vunderink
 *
 * Purpose: Various string utilities
 *
 * ---------------------------------------------------------------------
 * List of strut functions
 *
 * strut_flush       Write line of characters to file
 * strut_addsub      Add subscript to a filename if not present
 * strut_delsub      Deletes everything from first period
 * strut_changesub   Change subscript even if has one already
 * strut_left        Shift card to left to remove leading spaces
 * strut_right       Get rid of any spaces just before terminating null
 * strut_pad         Concatenate two strings into new allocated address
 * strut_new         Create memory for output and copy card into it
 * strut_nospaces    Remove spaces from string
 * strut_norepeat    Delete any repeated characters from string
 * strut_piece       Copy piece of string 
 * strut_rstr        Return pointer to last occurrence of string
 * ---------------------------------------------------------------------
 * Revisions:
 * Date      Who        Description
 * --------  ---------  ------------------------------------------------
 * 10/22/96  Vunderink  Inserted into CONLIB.
 * ---------------------------------------------------------------------
C\END DOC
 */

long strut_flush (long nl, long ngap, char *temp, FILE * out)
{
/* write out up to LINELEN characters of the temp array if there is
 * room;  nl should contain the number of characters written so far
 * to this line;  returns the updated value of nl;
 * ngap blank spaces will be reserved at the end of the line.
 * Initialize nl to zero on first call.
 */
  long nt;

  nt = strlen (temp);
  nt = (nt > LINELEN) ? LINELEN : nt;
  if (nt + nl + ngap > LINELEN) {
    fprintf (out, "\n");
    nl = 0;
    if (ngap < LINELEN / 2) {
      fprintf (out, " ");
      nl++;
      if (ngap < LINELEN / 4) {
	fprintf (out, " ");
	nl++;
	if (ngap <= 0) {
	  fprintf (out, " ");
	  nl++;
	}
      }
    }
  }
  fprintf (out, "%s", temp);
  nl += nt;
  return nl;
}

char *
strut_addsub (char *card, char *sub)
{
/* add a subscript to a filename if not present, returns card */
  card = strut_left (strut_right (card));	/* get rid of spaces */
  if (*sub == '.' || isspace (*sub))	/* delete period */
    sub++;
  if (strchr (card, '.'))	/* don't add subscript if has one */
    return card;
  card = strut_pad (card, ".");	/* add period */
  return strut_pad (card, sub);	/* add subscript */
}

char *
strut_delsub (char *card)
{
/* deletes everything from first period, returns card without blanks */
  char *c;

  card = strut_left (strut_right (card));	/* get rid of spaces */
  if (c = strchr (card, '.'))	/* find period and delete */
    *c = '\0';
  return card;
}

char *
strut_changesub (char *card, char *sub)
{
/* change subscript even if has one already */
  return strut_addsub (strut_delsub (card), sub);
}

char *
strut_left (char *card)
{
/* shift card to left to remove leading spaces, returns card */
  while (isspace (*card) && (*card) != '\0')
    card++;
  return card;
}

char *
strut_right (char *card)
{
/* get rid of any spaces just before terminating null, returns card */
  char *c;

/* get to end of string */
  c = card;
  while ((*c) != '\0')
    c++;
/* start with null, back up until reach a non space */
  while ((c > card) && (isspace (*(c - 1)))) {
    c--;
  }
/* c is first space, replace  with null */
  *c = '\0';
  return card;
}

char *
strut_pad (char *card, char *cadd)
{
/* concatenate two strings into new allocated address, returned */
/* if card was allocated by last pad, then will be deallocated */
  char *c,
   *cnew;
  static char *clast;

  c = (char *) malloc (strlen (card) + strlen (cadd) + 2);
  cnew = strcat (strcpy (c, card), cadd);
  if (clast == card) {
    free (card);
  }
  clast = cnew;
  return cnew;
}

char *
strut_new (char *card)
{
/* create memory for output and copy card into it */
  char *c;

  c = (char *) malloc (strlen (card) + 1);
  return strcpy (c, card);
}

long 
strut_nospaces (char *s)
{
  char *snext;

  snext = s;
  while ((*snext) != '\0') {
    if (!isspace (*snext)) {
      (*s) = (*snext);
      s++;
    }
    snext++;
  }
  (*s) = '\0';
  return 1;
}

long 
strut_norepeat (char *s)
{
/* delete any repeated characters or spaces from string */
  char *snew,
   *ss,
    slast;

  strut_nospaces (s);
  if (strlen (s)) {
    snew = ss = s;
    while ((*snew) != '\0') {
      snew++;
      if ((*snew) != (*ss)) {
	ss++;
	(*ss) = (*snew);
      }
    }
  }
  return 1;
}

char *
strut_piece (char *out, char *start, char *end, long n)
{
/* copy piece of strint to out from between start and end */
  long m;

  m = (long) (end - start + 1);
  m = ((n - 1) < m) ? (n - 1) : m;
  strncpy (out, start, m);
  out[(long) (end - start + 1)] = '\0';
  return out;
}

char *
strut_rstr (char *cs, char *ct)
{
/* return pointer to last occurrence of string ct in cs, or
 * NULL if not present */
  char *c1,
   *c2;

  c1 = strstr (cs, ct);
  if (c1) {
    while (strlen (c1 + 1) && (long) (c2 = strstr (c1 + 1, ct))) {
      c1 = c2;
    }
  }
  return c1;
}
/*  %W%     %G%  */
