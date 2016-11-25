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
#include "plot/ll_pick_base.hh"
#include "plot/pick_base.hh"
#include "plot/plot_base.hh"
#include "wproc.h"

#include <Xm/Xm.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

void PickBaseSamePlotBaseLinkedList::add(PickBase *pickBase)
{
	PickBaseSamePlotBaseElement *theElement =
		new PickBaseSamePlotBaseElement(pickBase);

	BaseLinkedList::add((Element *) theElement);
}

PickBase *PickBaseSamePlotBaseLinkedList::find(PickBase *pickBase, void **p)
{
	PickBaseSamePlotBaseElement *ptr = (PickBaseSamePlotBaseElement *)
		BaseLinkedList::find((void *) pickBase, p);

	return (ptr ? ptr->_pickBase : NULL);
}

PickBase *PickBaseSamePlotBaseLinkedList::top(void **p)
{
	PickBaseSamePlotBaseElement *ptr = (PickBaseSamePlotBaseElement *)
		BaseLinkedList::top    (p);

	return (ptr ? ptr->_pickBase : NULL);
}

PickBase *PickBaseSamePlotBaseLinkedList::bottom(void **p)
{
	PickBaseSamePlotBaseElement *ptr = (PickBaseSamePlotBaseElement *)
		BaseLinkedList::bottom (p);

	return (ptr ? ptr->_pickBase : NULL);
}

PickBase *PickBaseSamePlotBaseLinkedList::next(void **p)
{
	PickBaseSamePlotBaseElement *ptr = (PickBaseSamePlotBaseElement *)
		BaseLinkedList::next   (p);

	return (ptr ? ptr->_pickBase : NULL);
}

PickBase *PickBaseSamePlotBaseLinkedList::prev(void **p)
{
	PickBaseSamePlotBaseElement *ptr = (PickBaseSamePlotBaseElement *)
		BaseLinkedList::prev   (p);

	return (ptr ? ptr->_pickBase : NULL);
}

PickBase *PickBaseSamePlotBaseLinkedList::current(void **p)
{
	PickBaseSamePlotBaseElement *ptr = (PickBaseSamePlotBaseElement *)
		BaseLinkedList::current(p);

	return (ptr ? ptr->_pickBase : NULL);
}

PickBaseElement::PickBaseElement(PlotBase *plot, XtTranslations translations)
	: _plot(plot)
{
	_pickBases = new PickBaseSamePlotBaseLinkedList();

	Widget widget = _plot->getWidget();
	assert(widget);

	XtVaGetValues(widget,
		XmNtranslations, &_oldTranslations,
		NULL);

	XtUninstallTranslations(widget);
	XtOverrideTranslations (widget, translations);
}

PickBaseElement::~PickBaseElement()
{
	Widget widget = _plot->getWidget();
	if (widget)
	{
		XtUninstallTranslations(widget);
		XtOverrideTranslations (widget, _oldTranslations);
	}

	delete _pickBases;
}

PickBaseLinkedList::PickBaseLinkedList(Widget widget)
{
	static XtActionsRec actionTable[] =
	{
		{ "staticPressAction"  ,
			(XtActionProc) PickBaseLinkedList::staticPressAction  },
		{ "staticMotionAction" ,
			(XtActionProc) PickBaseLinkedList::staticMotionAction },
		{ "staticReleaseAction",
			(XtActionProc) PickBaseLinkedList::staticReleaseAction},
	};

	XtAppAddActions(XtWidgetToApplicationContext(widget),
		actionTable, XtNumber(actionTable));

	static char actionsFormat[] =
		"Any <BtnDown>    : staticPressAction(%u)  \n\
		 Any <Btn1Motion> : staticMotionAction(%u) \n\
		 Any <Btn2Motion> : staticMotionAction(%u) \n\
		 Any <Btn3Motion> : staticMotionAction(%u) \n\
		 Any <BtnUp>      : staticReleaseAction(%u)\n";
	/*
	 * Up to 10 digits (4e9) replace each %u plus terminating \0.
	 * 5 * (10 - 2) + 1 = 41
	 */
	char *actions = new char[strlen(actionsFormat) + 41];
	
	sprintf(actions, actionsFormat, this, this, this, this, this);
	
	_translations = XtParseTranslationTable(actions);
	
	delete [] actions;
}

void PickBaseLinkedList::staticPressAction(Widget /*w*/, XEvent *event,
	char **params, Cardinal * /*num_params*/)
{
	PickBaseLinkedList *obj = (PickBaseLinkedList *) parseInt(params[0]);

	PickBase *pickBase = obj->getPickBase(event->xbutton.window);
	assert(pickBase);

	pickBase->pressAction(event->xbutton.x, event->xbutton.y,
		event->xbutton.button, event->xbutton.state);
}

void PickBaseLinkedList::staticMotionAction(Widget w, XEvent *event,
	char **params, Cardinal * /*num_params*/)
{
	const unsigned buttonDown = Button1Mask | Button2Mask | Button3Mask;

	if ((event->xbutton.state & buttonDown) != 0)
	{
		if (wpMoreEvents(XtDisplay(w), XtWindow(w), MotionNotify))
			return;

		PickBaseLinkedList *obj =
			(PickBaseLinkedList *) parseInt(params[0]);

		PickBase *pickBase = obj->getPickBase(event->xbutton.window);
		assert(pickBase);

		pickBase->motionAction(event->xbutton.x, event->xbutton.y,
			event->xbutton.state);
	}
	else
	{
		/*
		 * Replaced Any <BtnMotion> with Any <Btn[123]Motion>s
		 * in translation table.  Should never get here now.
		 * ehs 29oct97
		 */
		assert(0);
	}
}

void PickBaseLinkedList::staticReleaseAction(Widget /*w*/, XEvent *event,
	char **params, Cardinal * /*num_params*/)
{
	PickBaseLinkedList *obj = (PickBaseLinkedList *) parseInt(params[0]);

	PickBase *pickBase = obj->getPickBase(event->xbutton.window);
	assert(pickBase);

	pickBase->releaseAction(event->xbutton.x, event->xbutton.y,
		event->xbutton.button, event->xbutton.state);
}

int PickBaseLinkedList::parseInt(char *string)
{
	int retval = 0;
	
	for (; *string != '\0';)
		retval = 10 * retval + (int) (*string++ - '0');
	
	return retval;
}

PickBase *PickBaseLinkedList::getPickBase(Window window)
{
	PickBase *retval;

	PickBaseElement *ptr;
	void *p;
	for (	ptr = (PickBaseElement *) BaseLinkedList::top (&p);
		ptr;
		ptr = (PickBaseElement *) BaseLinkedList::next(&p))
	{
		if (ptr->_plot->getDrawable() == window)
			break;
	}

	/*
	 * If you assert on the second part of this,
	 * you probably forgot to call the static function
	 * PickBase::changePlotBase when you changed PlotBases
	 * in this window.
	 */
	assert(ptr && ptr->_plot->isCurrentInWindow());

	retval = ptr->_pickBases->bottom();

	return retval;
}

void PickBaseLinkedList::add(PickBase *pickBase)
{
	PlotBase *plotBase = pickBase->getPlot();

	PickBaseElement *ptr =
		(PickBaseElement *) BaseLinkedList::find((void *) plotBase);

	if (ptr == (PickBaseElement *) NULL)
	{
		ptr = new PickBaseElement(plotBase, _translations);

		BaseLinkedList::add((Element *) ptr);
	}

	ptr->_pickBases->add(pickBase);
}

void PickBaseLinkedList::remove(PickBase *pickBase)
{
	PlotBase *plotBase = pickBase->getPlot();

	PickBaseElement *ptr =
		(PickBaseElement *) BaseLinkedList::find((void *) plotBase);

	assert(ptr != (PickBaseElement *) NULL);

	ptr->_pickBases->remove(pickBase);

	if ((ptr->_pickBases->count() == 0))
		removeByPtr(ptr);
}

PickBase *PickBaseLinkedList::find(PickBase *pickBase, void **p)
{
	PlotBase *plotBase = pickBase->getPlot();

	PickBaseElement *ptr =
		(PickBaseElement *) BaseLinkedList::find((void *) plotBase);

	if (ptr == (PickBaseElement *) NULL)
		return ((PickBase *) NULL);
	else
		return (ptr->_pickBases->find(pickBase, p));
}

PickBase *PickBaseLinkedList::top(PlotBase *plotBase, void **p)
{
	return position(plotBase, &PickBaseSamePlotBaseLinkedList::top, p);
}

PickBase *PickBaseLinkedList::bottom(PlotBase *plotBase, void **p)
{
	return position(plotBase, &PickBaseSamePlotBaseLinkedList::bottom, p);
}

PickBase *PickBaseLinkedList::next(PlotBase *plotBase, void **p)
{
	return position(plotBase, &PickBaseSamePlotBaseLinkedList::next, p);
}

PickBase *PickBaseLinkedList::prev(PlotBase *plotBase, void **p)
{
	return position(plotBase, &PickBaseSamePlotBaseLinkedList::prev, p);
}

PickBase *PickBaseLinkedList::current(PlotBase *plotBase, void **p)
{
	return position(plotBase, &PickBaseSamePlotBaseLinkedList::current, p);
}

int PickBaseLinkedList::getPlots(PlotBase ***plotBases)
{
	int retval = count();

	if (retval > 0)
	{
		/*
		 * Client only deletes if retval > 0.
		 */
		PlotBase **plotBasesPtr = *plotBases = new PlotBase *[retval];

		void *p;
		for (PickBaseElement *ptr =
			(PickBaseElement *) BaseLinkedList::top(&p);
			ptr;
			ptr = (PickBaseElement *) BaseLinkedList::next(&p))
		{
			*plotBasesPtr++ = ptr->_plot;
		}
	}

	return retval;
}

PickBase *PickBaseLinkedList::position(PlotBase *plotBase, posFunc func,
	void **p)
{
	PickBaseElement *ptr =
		(PickBaseElement *) BaseLinkedList::find((void *) plotBase);

	if (ptr == (PickBaseElement *) NULL)
		return ((PickBase *) NULL);
	else
		return ((ptr->_pickBases->*func)(p));
}

void PickBaseLinkedList::changePlotBase(PlotBase *from, PlotBase *to)
{
	assert( from->getDrawable() &&
	       (from->getDrawable() == to->getDrawable()));

	PickBaseElement *ptr;

	/*
	 * Make sure that to PlotBase is does not already have
	 * some PickBases on it.
	 */
	ptr = (PickBaseElement *) BaseLinkedList::find((void *) to);
	assert(!ptr);

	ptr = (PickBaseElement *) BaseLinkedList::find((void *) from);
	if (ptr)
	{
		ptr->_plot = to;

		void *p;
		for (PickBase *pb = ptr->_pickBases->top(&p);
			pb;
			pb = ptr->_pickBases->next(&p))
		{
			pb->_plot = to;
		}
	}
}

void SimplePickBaseLinkedList::add(PickBase *pickBase)
{
	SimplePickBaseElement *theElement =
		new SimplePickBaseElement(pickBase);

	BaseLinkedList::add((Element *) theElement);
}

PickBase *SimplePickBaseLinkedList::find(PickBase *pickBase, void **p)
{
	SimplePickBaseElement *ptr = (SimplePickBaseElement *)
		BaseLinkedList::find((void *) pickBase, p);

	return (ptr ? ptr->_pickBase : NULL);
}

PickBase *SimplePickBaseLinkedList::top(void **p)
{
	SimplePickBaseElement *ptr = (SimplePickBaseElement *)
		BaseLinkedList::top    (p);

	return (ptr ? ptr->_pickBase : NULL);
}

PickBase *SimplePickBaseLinkedList::bottom(void **p)
{
	SimplePickBaseElement *ptr = (SimplePickBaseElement *)
		BaseLinkedList::bottom (p);

	return (ptr ? ptr->_pickBase : NULL);
}

PickBase *SimplePickBaseLinkedList::next(void **p)
{
	SimplePickBaseElement *ptr = (SimplePickBaseElement *)
		BaseLinkedList::next   (p);

	return (ptr ? ptr->_pickBase : NULL);
}

PickBase *SimplePickBaseLinkedList::prev(void **p)
{
	SimplePickBaseElement *ptr = (SimplePickBaseElement *)
		BaseLinkedList::prev   (p);

	return (ptr ? ptr->_pickBase : NULL);
}

PickBase *SimplePickBaseLinkedList::current(void **p)
{
	SimplePickBaseElement *ptr = (SimplePickBaseElement *)
		BaseLinkedList::current(p);

	return (ptr ? ptr->_pickBase : NULL);
}
