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
#include "ipc/sd_slave.hh"
#include "ipc/sd_scrwin.hh"
#include "sl/sl_app.hh"
#include "sl/sl_pull_pop.hh"
#include "sl/color_info_set.hh"
#include <unistd.h>

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef MAINW_H
#include <Xm/MainW.h>
#define MAINW_H
#endif

#ifndef DRAWINGA_H
#include <Xm/DrawingA.h>
#define DRAWINGA_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

static char *fallback_resources[] = {
#     include "Slave_dpy.h"
      NULL
};

Bool parseCommandLine(const char *const, int *, char **);
void buttonPushed(void *,  long);
void closingFunc(void *);
void quit(SlaveDisplaySlave *);

enum { QUIT, XOR_CURSOR, BORDERS };

#define NSEC 0

int main(int argc, char **argv)
{
        //printf ("slave_dpy Process ID = %ld\n", getpid());
        if (NSEC>0) sleep (NSEC); // chance to attach debugger bre 46
	SLApp *application;

	if (parseCommandLine(SlaveDisplayBase::_privateColormap, &argc, argv))
		application = new SLApp("slave_dpy", "Slave_dpy",
			argc, argv, (XrmOptionDescRec *) NULL, 0, True );
	else
		application = new SLApp("slave_dpy", "Slave_dpy",
			argc, argv, (XrmOptionDescRec *) NULL, 0, False);

        application->setFallbacks(fallback_resources);
	SLPullPop *file_pull0 = new SLPullPop("file", NULL, application);
	file_pull0->addPush("quit", QUIT);

	SLPullPop *file_pull1 = new SLPullPop("controls", NULL, application);
	file_pull1->addTog("xor_cursor", XOR_CURSOR);
	file_pull1->addTog("borders"   , BORDERS   );

	SlaveDisplayScrWin *sdScrWin = new SlaveDisplayScrWin
		(application->mainWindow(), "RemoteDisplay");

	Widget da = XtVaCreateManagedWidget("da", xmDrawingAreaWidgetClass,
		sdScrWin->scrollParent(),
		NULL);

	sdScrWin->setWorkArea(da);
	  
	application->setWorkArea(sdScrWin->W());
	application->showStatline(True);

	application->realize();		// SlaveDisplaySlave needs window id.

	SlaveDisplaySlave *theSlave = new SlaveDisplaySlave(da, &argc, argv,
		application, sdScrWin);

	theSlave->setXorCursor((Bool) file_pull1->toggleValue(XOR_CURSOR));
	theSlave->setBorders  ((Bool) file_pull1->toggleValue(BORDERS   ));

	sdScrWin->setSlaveDisplay(theSlave);

	file_pull0 ->setAltPushAction   (buttonPushed, (void *) theSlave);
	file_pull1 ->setAltPushAction   (buttonPushed, (void *) theSlave);
	application->setAltClosingAction(closingFunc , (void *) theSlave);

        //application->registerWorkProcFunction (
        //  (WorkProcFunction)ColorInfoCollection::loopUpdate);

	application->loop();

	return 0;
}

Bool parseCommandLine(const char *const string, int *argc, char **argv)
{
	int i;
	for (i = 1, argv++; i < *argc; i++, argv++)
		if (!strcmp(*argv, string))
		{
			for (int j = 0; j < *argc - i - 1; j++)
				*(argv + j) = *(argv + j + 1);

			(*argc)--;
			return(True);
		}

	return(False);
}

void buttonPushed(void *data,  long ident)
{
	SlaveDisplaySlave *theSlave = (SlaveDisplaySlave *) data;
	SLPullPop *pushed = SLPullPop::lastPushedObj();

	switch (ident)
	{
		case QUIT:
			quit(theSlave);
			break;
		case XOR_CURSOR:
			theSlave->setXorCursor(
				(Bool) pushed->toggleValue(XOR_CURSOR));
			break;
		case BORDERS:
			theSlave->setBorders  (
				(Bool) pushed->toggleValue(BORDERS   ));
			break;
		default:
			assert(False);
	}
}

void closingFunc(void *data)
{
	quit((SlaveDisplaySlave *) data);
}

void quit(SlaveDisplaySlave *theSlave)
{
	delete theSlave;
}
