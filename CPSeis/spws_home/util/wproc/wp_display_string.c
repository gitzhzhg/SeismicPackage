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
/*
 * wpDisplayString is like DisplayString except it mallocs so the caller
 * must free.  wpDisplayString is necessary because VMS's DisplayString
 * returns a VMS device name.  Putting wpDisplayString in wproc isolates
 * the developer from this VMS quirk, allowing code to be identical for
 * VMS and UNIX.
 * ehs --- 13May94
 */

#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <assert.h>
#include <X11/Xlib.h>

#ifdef VMS

#include <iodef.h>
#include <ssdef.h>
#include <descrip.h>

#define DECW$C_WS_DSP_NODE 1
#define DECW$C_WS_DSP_TRANSPORT 2
#define DECW$C_WS_DSP_SERVER 3
#define DECW$C_WS_DSP_SCREEN 4
#define DECW$C_WS_DSP_MODE 5

#define IO$M_WS_DISPLAY 1<<6

#endif /* VMS */

static char *expandDisplayName(char *in);

#ifdef VMS
static char *getVAXDisplayName(Display *display);
static int show_display_item(char *devnam, unsigned long item, char *retstr,
	int *retlen);
#endif

char *wpDisplayString(Display *display)
{
	char *s= "";   /* init to satisify to compiler - trey 1/23/97 */

	if (DisplayString(display)[0] == '_')
	{
#ifdef VMS
		s = getVAXDisplayName(display);
#else
		assert(False);
#endif
	}
	else
	{
		s = expandDisplayName(DisplayString(display));
	}

	return(s);
}

static char *expandDisplayName(char *in)
{
	char *lastColon  = strrchr(in, (int) ':');
	char *lastPeriod = strrchr(in, (int) '.');
	size_t length    = strlen(in);
	char *lastChar   = in + length - 1;
	char *out;

	assert(lastColon != (char *) NULL);

	if (lastColon == lastChar)
	{
		assert(out = (char *) malloc(length + 4));
		strcpy(out, in);
		strcat(out, "0.0");
	}
	else if (lastPeriod < lastColon)
	{
		assert(out = (char *) malloc(length + 3));
		strcpy(out, in);
		strcat(out, ".0");
	}
	else if (lastPeriod == lastChar)
	{
		assert(out = (char *) malloc(length + 2));
		strcpy(out, in);
		strcat(out, "0");
	}
	else
	{
		out = (char *) malloc(length + 1);
		strcpy(out, in);
	}

	return(out);
}

#ifdef VMS

static char *getVAXDisplayName(Display *display)
{
	char *name;
	char retstr[30];
	int retlen;
	int i;

	memset(retstr, (int) '\0', sizeof(retstr));
	retlen = (int) sizeof(retstr);

	assert(show_display_item(DisplayString(display),
		DECW$C_WS_DSP_NODE     , retstr, &retlen) == SS$_NORMAL);

	assert(name = (char *) malloc(strlen(retstr) + 6));

	strcpy(name, retstr);

	memset(retstr, (int) '\0', sizeof(retstr));
	retlen = (int) sizeof(retstr);

	assert(show_display_item(DisplayString(display),
		DECW$C_WS_DSP_TRANSPORT, retstr, &retlen) == SS$_NORMAL);

	if (!strcmp(retstr, "DECNET") || !strcmp(retstr, "LOCAL"))
		strcat(name, "::");
	else
		strcat(name, ":");

	memset(retstr, (int) '\0', sizeof(retstr));
	retlen = (int) sizeof(retstr);

	assert(show_display_item(DisplayString(display),
		DECW$C_WS_DSP_SERVER   , retstr, &retlen) == SS$_NORMAL);

	strcat(name, retstr);

	strcat(name, ".");

	memset(retstr, (int) '\0', sizeof(retstr));
	retlen = (int) sizeof(retstr);

	assert(show_display_item(DisplayString(display),
		DECW$C_WS_DSP_SCREEN   , retstr, &retlen) == SS$_NORMAL);

	strcat(name, retstr);

	return(name);
}

static int show_display_item(char *devnam, unsigned long item, char *retstr,
	int *retlen)
{
/* from Chris Knorr, DEC, via Randy Hays */

	unsigned long chan;
	unsigned long iosb[2];
	struct dsc$descriptor itemval;
	struct dsc$descriptor device;
	unsigned long status;

	/* Assign a channel to the device */
	device.dsc$w_length = strlen(devnam);
	device.dsc$a_pointer = &devnam[0];
	status = sys$assign (&device, &chan, 0, 0);
	if (status != SS$_NORMAL) return(status);

	/* Setup the return buffer for the QIO */
	itemval.dsc$w_length = *retlen;
	itemval.dsc$b_dtype = 0;
	itemval.dsc$b_class = 0;
	itemval.dsc$a_pointer = &retstr[0];

	/* Get the information */
	status = sys$qiow (0, chan, IO$_SENSEMODE|IO$M_WS_DISPLAY, &iosb, 0, 0,
		   itemval.dsc$a_pointer,
	    	   itemval.dsc$w_length,
	    	   item, 0, 0, 0);

	/* Return the length of the string returned by the QIO */
	*retlen = iosb[1];

	if (status != SS$_NORMAL) return(status);
	if (iosb[0] != SS$_NORMAL) return(iosb[0]);

	/* Deassign the device channel */
	status = sys$dassgn ( &chan );
/*	if (status != SS$_NORMAL) return(status); */
/*	return(status); */
	return(SS$_NORMAL);	/* this is screwy */
}

#endif
