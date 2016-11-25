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
#include "ipc/cm_slave.hh"

#ifndef STDLIB_H
#include <stdlib.h>
#define STDLIB_H
#endif

#ifndef STDIO_H
#include <stdio.h>
#define STDIO_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#ifndef STRING_H
#include <string.h>
#define STRING_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef LABEL_H
#include <Xm/Label.h>
#define LABEL_H
#endif

#ifndef WPROC_H
#include "wproc.h"
#define WPROC_H
#endif

ClientMessageSlave::ClientMessageSlave(Widget w, XtPointer client,
	int *argc, char **argv) : ClientMessageBase(client)
{
	char	*displayString, *slaveDisplayString, *otherDisplayString;
	Widget	parent;

	assert(parseCommandLine(argc, argv, &displayString, &_outWindow));
	_error.add(_outWindow, this);

	slaveDisplayString = wpDisplayString(XtDisplay(w));

	if (strcmpCaseInsensitive(slaveDisplayString, displayString))
	{
		Display	**ptr;
		int count;
		for (appContextToDpyList(XtWidgetToApplicationContext(w),
			&ptr, &count), _display = (Display *) False;
			count;
			ptr++, count--)
		{
			otherDisplayString = wpDisplayString(*ptr);

			if (!strcmpCaseInsensitive(otherDisplayString,
				displayString))
			{
				_display = *ptr;
				free(otherDisplayString);
				break;
			}

			free(otherDisplayString);
		}

		if (!_display)
		{
			_display = XtOpenDisplay(
				XtWidgetToApplicationContext(w),
				displayString, NULL, "spws_junk", NULL, 0,
#ifndef XtSpecificationRelease
				(Cardinal *) argc,
#else
#if XtSpecificationRelease == 4
				(Cardinal *) argc,
#else
				argc,
#endif
#endif
				argv);

			_newDisplay = True;
		}
 
		parent = XtVaAppCreateShell(NULL, "spws_junk",
			applicationShellWidgetClass, _display,
			XmNmappedWhenManaged, False,
                        NULL);
		_newTopLevel = True;
	}
	else
	{
		_display = XtDisplay(w);
		parent = w;
	}

	free(slaveDisplayString);

	for (; !XtIsComposite(parent); parent = XtParent(parent));

	_inWidget = XtVaCreateManagedWidget("spws_junk", xmLabelWidgetClass,
		parent,
		XmNmappedWhenManaged, False,
		XmNheight           , 1    ,
		XmNwidth            , 1    ,
		NULL);

	if (_newTopLevel)
		XtRealizeWidget(parent);

	initClientMessage();

	send(XtWindow(_inWidget));	// tell master
}

ClientMessageSlave::ClientMessageSlave()
{
	assert(False);
}

ClientMessageSlave::~ClientMessageSlave()
{
	// do nothing
}

Bool ClientMessageSlave::parseCommandLine(int *argc, char **argv,
	char **displayString, Window *window)
{
	int i, j;

	for (i = 1, argv++; i < *argc; i++, argv++)
	{
		if (!strcmpCaseInsensitive(*argv, _commandLineOption))
			if (*argc - i >= 3)
			{
				*displayString = *(argv + 1);
#ifndef VMS
				sscanf(*(argv + 2), "%u", window);
#else
/* VMS sscanf screws-up	*/	char *ptr;
/* with unsigned.	*/	for (*window = 0, ptr = argv[2]; *ptr; ptr++)
/* I'll write my own.	*/		*window = *window * 10
						+ (Window) (*ptr - '0');
#endif /* VMS */
				for (j = 0; j < *argc - i - 3; j++)
					*(argv + j) = *(argv + j + 3);
				*argc -= 3;

				return(True);
			}
			else
			{
				return(False);
			}
	}

	return(False);
}

int ClientMessageSlave::strcmpCaseInsensitive(const char *s1, const char *s2)
{
	char *locS1 = new char[strlen(s1) + 1];
	char *locS2 = new char[strlen(s2) + 1];

	int length, i;

// I would like to use toupper in the loops, but it is screwed up in VMS .

	for (length = (int) strlen(s1), i = 0; i < length; i++)
		if (s1[i] >= 'a' && s1[i] <= 'z')
			locS1[i] = s1[i] - ('a' - 'A');
		else
			locS1[i] = s1[i];

	locS1[length] = '\0';

	for (length = (int) strlen(s2), i = 0; i < length; i++)
		if (s2[i] >= 'a' && s2[i] <= 'z')
			locS2[i] = s2[i] - ('a' - 'A');
		else
			locS2[i] = s2[i];

	locS2[length] = '\0';

	int retval = strcmp(locS1, locS2);

	delete [] locS1;
	delete [] locS2;

	return retval;
}
