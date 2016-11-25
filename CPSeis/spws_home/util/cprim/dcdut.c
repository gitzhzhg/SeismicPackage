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
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "cprim.h"

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
 * Name   : dcdut_dcode
 * Author : Richard Day
 * Date   : 01/19/95
 * Revised: 10/22/96  Vunderink
 *
 * Decodes the string in 'card', finds the 'value' for a particular
 * 'name'.  The long integer 'ric' should have the values 'r','i', or 
 * 'c' depending on whether you want a real number, an integer, or 
 * character string, 
 * (You can use the corresponding equivalents 'f', 'd', and 's'.)
 * Terminate 'card' and 'name' with a null.  Do not include
 * extra spaces in the string 'name'.
 * 
 * Declare float*value, long*value, or char*value, and pass &value.
 * 
 * If no specification is found, then this routine returns a zero.  
 * Otherwise, the return value is the number of characters in the 
 * string, or the number elements in an array (1 or more).
 * 
 * Card should contain assignments in the format 'name = value'
 * Examples of assignments are 
 * file = test.dat  title = "This is a title" nx=1, nx = 1, 
 * dt=0.004  dt = 4e-3;  indices = 2 3 4 5  values =2.3 5.1 8 9.8; 
 * 
 * A parameter specification can include spaces before and after an 
 * equals sign.  The variable name must be preceded by a space or the 
 * beginning of the string.  Spaces are defined as blanks, formfeeds, 
 * newline, tab, and vertical tabs.
 * 
 * The value of a parameter begins with the first non-space character 
 * after the equals sign.  A floating point value ends with the first 
 * non-digit (digits include 0-9, '.', '+', '-', 'e', and 'E').  
 * Integers are decoded as floating point numbers, then rounded to the 
 * nearest integer.  A character string value can begin and end with 
 * double quotes ("...").  If quotes are omitted, then the string is 
 * assumed to be bracketed by spaces.  To include spaces in the string,
 * you must use quotes; otherwise, they are optional.
 * 
 * Declare 'value' as a pointer, and pass a pointer to the pointer.
 * This routine will allocate the memory to contain the value(s).
 * For example,
 * { float *array; long i,n;
 * n = dcdut_dcode ( (void**) &array, "dxs",'r',"dxs = 2 3 4.5");
 * i=0; while(i<n) printf("%g ",array[i++]); }
 * will print out the values "2 3 4.5 "
 *
 * ---------------------------------------------------------------------
 * Revisions:
 * Date      Who        Description
 * --------  ---------  ------------------------------------------------
 * 10/22/96  Vunderink  Inserted into CONLIB.
 * 08/07/96  Day        Modified dcdut_trunc 's handling of comments
 *                      Free string copies before return.
 * ---------------------------------------------------------------------
C\END DOC
 */
long dcdut_dcode (void **value, char *name, long ric, char *card)
{

  long nd,			/* # characters, reals, or integers found */
    ndalloc,			/* number of elements allocated */
    ndinc = 10,			/* increment ndalloc by this */
    nc,				/* # characters in input card */
    nn,				/* # c's in name tag */
    nbuf = 64,			/* number of characters in buf[nbuf] */
    okay,			/* loop test */
    empty,			/* something specified */
    nsize,			/* word size */
    (*test) (char),		/* test for float or long */
   *pi;				/* temp pointer to integer */
  float *pr;			/* temp pointer to real */
  char *cc=0,			/* copy of card */
   *cn=0,			/* copy of name */
   *t,				/* temp  character index */
    buf[64],			/* character buffer for float/long */
   *s,				/* start of valid string */
   *f,				/* first of substring */
   *e;				/* end of specified string */

  /* printf ("card: %s\n", card); DEBUG */
  /* printf ("name: %s\n", name); DEBUG */
  /* printf ("ric: %c\n", ric);   DEBUG */
/* fix aliases */
  if (ric == 'f')
    ric = 'r';
  if (ric == 'd')
    ric = 'i';
  if (ric == 's')
    ric = 'c';
/* check for inconsistent lengths */
  if (!(nc = strlen (card))
      || !(nn = strlen (name))
      || (nn >= nc)
    )
    return 0;
/* convert input card to lower case for matching */
  cc = (char *) malloc (nc + 1);
  strcpy (cc, card);
  dcdut_lower (cc);
  nc = dcdut_trunc (cc);
/* convert name card to lower case for matching */
  cn = (char *) malloc (nn + 1);
  strcpy (cn, name);
  dcdut_lower (cn);
/* begin loop, s is location of last =, t is beginning of search string */
  s = cc + strlen (cc);
restart:
  *s = '\0';			/* end search at previous = */
  if (!(s = strut_rstr (cc, "=")))	/* match last  '=' */
    {if(cn) free(cn); if(cc) free(cc);
     return 0; /* only failure; return nd=0, no ='s left */
    }
  *s = '\0';			/* temporarily end string at = */
  okay = (int) (t = strut_rstr (cc, cn));	/* last name before = */
  *s = '=';			/* restore previous = */
  if (!okay)			/* last name before = */
    goto restart;
/* now t is first character of located name */
  if (t > cc && isalnum (*(t - 1)))
    goto restart;		/* part of another variable name */
  t += nn;			/* move to char after variable name */
  while (isspace (*t))
    t++;			/* skip spaces */
  if (*t == '[') {		/* skip over a pair of brackets */
    if (!(t = strchr (t, ']')))
      goto restart;		/* couldn't find the other bracket */
    t++;
    while (isspace (*t))
      t++;			/* skip spaces */
  }
  if (t != s)			/* something else between name and = */
    goto restart;
/* end loop */
  s++;				/* s is character after '=' */
/* restore original case for character string */
  if (ric == 'c') {
    strcpy (cc, card);
    nc = dcdut_trunc (cc);
  }
/* skip spaces */
  while (isspace (*s)) {
    s++;
  }
/* set e to end of useful stuff */
  /* printf ("s: %s\n", s);   DEBUG */
  e = s;
  empty = 1;
/* count floats */
  if (ric == 'i' || ric == 'r') {
    if (ric == 'i') {
      nsize = sizeof (int);

      test = (long (*)(char)) dcdut_isint;
    }
    if (ric == 'r') {
      nsize = sizeof (float);

      test = (long (*)(char)) dcdut_isfloat;
    }
    nd = ndalloc = 0;
    okay = 1;
    while (okay) {
      f = e;			/* first character of substring */
      /* printf ("f: %s\n", f); DEBUG */
      while ((*test) (*e)) {
	e++;
	empty = 0;
      }
      if (!empty) {
	nd++;			/* count number of  items */
	/* printf (" nd: %d\n", nd); DEBUG */
/* increase output array size if necessary */
	if (ndalloc == 0) {	/* first call */
	  ndalloc += 1;
	  /* printf ("allocating nsize=%d \n", nsize * ndalloc); 
	   */
	  (*value) = (void *) malloc (nsize * ndalloc);
	  /* printf ("finished allocating\n"); */
	}
	else if (nd > ndalloc) {	/* increase size */
	  /* printf ("Increasing output size \n"); DEBUG */
	  ndalloc += ndinc;
	  /* printf ("allocating\n"); */
	  (*value) = realloc
	    (*value, nsize * ndalloc);
	}
/* copy string for decoding into buffer, terminate with null  */
	strut_piece (buf, f, e, nbuf);
	if (ric == 'r') {
/* pointer to value needing to be specified */
	  pr = ((float *) *value) + nd - 1;
/* decode into proper output postion */
	  *pr = atof (buf);
	}
	else if (ric == 'i') {
	  pi = ((long *) *value) + nd - 1;
	  *pi = atoi (buf);
	}
/* skip next bunch of spaces */
	while (isspace (*e)) {
	  e++;			/* skip spaces */
	}
      }
/* repeat this loop if next character is valid for type */
/* 'e" is not really a valid begining for float */
      okay = (*test) (*e) && (*e) != 'e' && (*e) != 'E';
    }
  }
/* get character string for c type */
  else if (ric == 'c') {
    if ((*s) == '\"') {
      s++;
      e = s;
      if ((e = strchr (s, '\"')) == NULL)
	if ((e = strchr (s, '\n')) == NULL)
	  e = strchr (s, '\0');
    }
    else if ((*s) == '\'') {
      s++;
      e = s;
      if ((e = strchr (s, '\'')) == NULL)
	if ((e = strchr (s, '\n')) == NULL)
	  e = strchr (s, '\0');
    }
    else {
      while (isgraph (*e) && !isspace (*e)) {
	e++;
      }
    }
/* mark end of useful string with a null */
    if (e == s)
      empty = 1;
    else {
      (*e) = '\0';
      nd = e - s + 1;
      *value = (char *) malloc (nd + 1);
      strcpy ((char *) *value, s);
      empty = 0;
    }
  }
/* now s and e span the string used for decoding.  */
  (*e) = '\0';
  /* printf ("s to e: %s\n", s);       DEBUG */
/* empty string was specified for real or integer */
  if (empty)
    {if(cn) free(cn); if(cc) free(cc);
     return 0; /* only failure; return nd=0, no ='s left */
    }
  if(cn) free(cn); if(cc) free(cc);
  return nd;
}

long 
dcdut_dcode_file (void **value,
		char *name, long ric, char *file, long nmax)
{
/* decode and return pointers to values from file, at most nmax lines ,
 * return nd */
  long nbuf = 120,
    nd,
    nlines,
    n;
  char buf[120];
  FILE *f;

  nd = 0;
  if (f = fopen (file, "r")) {
    nlines = 0;
    while (fgets (buf, nbuf, f) && nlines < nmax) {
      nlines++;
      if ((n = dcdut_dcode (value, name, ric, buf)))
	nd = n;
    }
  }
  fclose (f);
  return nd;
}

long 
dcdut_count_ascii_words (long maxlen, char *fname)
{
/* return number uninterrupted ascii words, max dimension s[maxlen+1] */
  FILE *f;
  long n;
  char *s;

  n = 0;
  s = (char *) malloc (maxlen + 1);
  if (f = fopen (fname, "r")) {
    while (dcdut_get_ascii_word (s, maxlen, f))
      n++;
    fclose (f);
  }
  free (s);
  return n;
}

long 
dcdut_get_ascii_word (char *s, long maxlen, FILE * f)
{
/* get next uninterrupted ascii word, max dimension s[maxlen+1] */
/* returns 0 when file is empty */
  char c;
  long n;

  n = 0;
  while (!isalpha (c = fgetc (f))) {
    if (c == EOF)
      return 0;
  }
  s[n] = c;
  n++;
  while (isalpha (c = fgetc (f)) && n < maxlen) {
    s[n] = c;
    n++;
  }
  s[n] = '\0';
  return 1;
}

void 
dcdut_lower (char *s)
{
  while ((*s) != '\0') {
    if ((*s) >= 'A' && (*s) <= 'Z') {
      (*s) += 'a' - 'A';
    }
    s++;
  }
}

void 
dcdut_upper (char *s)
{
  while ((*s) != '\0') {
    if ((*s) >= 'a' && (*s) <= 'z') {
      (*s) += 'A' - 'a';
    }
    s++;
  }
}


long 
dcdut_isfloat (char c)
{
  return (isdigit (c) || (c) == 'e' || (c) == 'E'
	  || (c) == '+' || (c) == '-' || (c) == '.');
}

long 
dcdut_isint (char c)
{
  return (isdigit (c) || (c) == '+' || (c) == '-');
}

long 
dcdut_count (char *s)
{
/* count number of floats on line */
  long ncount;

  if (s[0] == '\0' || s[0] == '#')
    return 0;
  ncount = 0;
  while (*s != '\0') {
    while (!dcdut_isfloat (*s) && *s != '\0')
      s++;
    if (dcdut_isfloat (*s))
      ncount++;
    while (dcdut_isfloat (*s))
      s++;
  }
  return ncount;
}

long 
dcdut_float (float *r, long n, char *s)
{
/* decode nth float from input string, free format, return 1 if successful */
  char *e;
  long nbuf = 20;
  char buf[20];
  long m;

  m = 0;
  if (n <= 0 || s[0] == '\0' || s[0] == '#')
    return 0;
  while (*s != '\0') {
    while (!dcdut_isfloat (*s) && *s != '\0')
      s++;
    if (dcdut_isfloat (*s)) {
      e = s;
      while (dcdut_isfloat (*e))
	e++;
      e--;
      strut_piece (buf, s, e, nbuf);
      m++;
      if (m == n) {
	*r = atof (buf);
	return 1;
      }
      s = e + 1;
    }
  }
  return 0;
}

char *
dcdut_field (char *out, char *in, long n)
{
/* copy the nth non-space field from in to out, return out */
  char *s,
   *e;

  s = in;
  while (n) {
    while (isspace (*s) && *s != '\0')
      s++;
    n--;
    e = s;
    while (!isspace (*e) && *e != '\0')
      e++;
    e--;
  }
  strut_piece (out, s, e, strlen (in));
  return out;
}

long 
dcdut_trans (float **r1, float **r2, char *coord, char *trans_file,
	   long max_lines)
{
/* get coordinate values r1 and r2 (allocating memory) for values
 * of coord1 and coord2 from file trans_file, returns 1 if successful,
 * look at maximum of max_lines in trans_file */
  char *c1,
   *c2;

  c1 = strut_pad (coord, "1");
  c2 = strut_pad (coord, "2");
  if (
       dcdut_dcode_file ((void **) r1, c1, 'r', trans_file, max_lines)
       *
       dcdut_dcode_file ((void **) r2, c2, 'r', trans_file, max_lines)
    )
    return 1;
  else
    return 0;
}

long 
dcdut_trunc (char *card)
{
/* collapse string between each # and \n
   collapse if * is first character on a line
*/
  long comment,
    end,
    off,
    on;
  char *cout, *lb,
   *cin;

  if (*card == '\0')
    return 0;
  end = 0;
  comment = 0;
  cin = card;
  cout= card;
  lb  = cin;
  while (!end) {
    off = (*cin == '\n') || (end = (*cin == '\0'));
    on = (*cin == '#') && (*(cin + 1) != '#');
/*    on = on || (*card == '*') || (*(card + 1) == '*'); */
    if (end) on = on || (*lb == '*');
    else     on = on || (*lb == '*') || (*(lb + 1) == '*');
    if(off) lb = cin+1;
    comment = (comment || on) && !off;
    if (!comment) {
      *cout = (*cin);
      cout++;
    }
    cin++;
  }
  return strlen (card);
}
/*  %W%     %G%  */

