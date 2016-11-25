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
#include "ipc/cm_master.hh"

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#ifndef STDIO_H
#include <stdio.h>
#define STDIO_H
#endif

#ifndef STDLIB_H
#include <stdlib.h>
#define STDLIB_H
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

ClientMessageMaster::ClientMessageMaster(Widget w, XtPointer client,
	char * string) : ClientMessageBase(client)
{
	const size_t BUFF_SIZE = 256;
	char buff[BUFF_SIZE];

/*
	for (; !XtIsComposite(w); w = XtParent(w));
*/
	/*
	 * Always put in separate shell in case w get deleted.
	 */
	Widget parent = XtVaAppCreateShell(NULL, "spws_junk",
		applicationShellWidgetClass, XtDisplay(w),
		XmNmappedWhenManaged, False,
		NULL);
	XtRealizeWidget(parent);
	_newTopLevel = True;

	_inWidget = XtVaCreateManagedWidget("spws_junk",
		xmLabelWidgetClass, parent,
		XmNmappedWhenManaged, False,
		XmNheight           , 1    ,
		XmNwidth            , 1    ,
		NULL);

	_display = XtDisplay(_inWidget);

	initClientMessage();

	char *displayString = wpDisplayString(_display);
	sprintf(buff, "%s %s %s %u &", string, _commandLineOption,
		displayString, XtWindow(_inWidget));
	assert(strlen(buff) < BUFF_SIZE);
	free(displayString);

	wpSystem(buff);
}

ClientMessageMaster::ClientMessageMaster()
{
	assert(False);
}

ClientMessageMaster::~ClientMessageMaster()
{
	// do nothing
}
