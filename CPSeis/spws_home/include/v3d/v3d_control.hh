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
#ifndef _V3D_CONTROL_HH
#define _V3D_CONTROL_HH

#include "v3d/v3d_pick.hh"
#include "sl/sl_form_pop.hh"
#include "oprim/element.hh"
#include "oprim/ll_base.hh"

#include <Xm/Xm.h>

#include <assert.h>
#include <iostream.h>

class V3dContWidElement : public Element
{
	friend class V3dContWidLinkedList;

	private:

		Widget _widget;
		int _ident;

		V3dContWidElement()
			{ assert(0); }
		V3dContWidElement(Widget widget, int ident)
			: _widget(widget), _ident(ident)
			{ /* do nothing */ }
		~V3dContWidElement()
			{ /* do nothing */ }
		int operator ==(void * const widget) const
			{ return((Widget) widget == _widget); }
		void print() const
			{ cout << " " << _widget; }
};

class V3dContWidLinkedList : public BaseLinkedList
{
	friend class V3dControl;

	public:		/* Destructor must be public for class to be static. */

		V3dContWidLinkedList()
			{ /* do nothing */ }
		~V3dContWidLinkedList()
			{ /* do nothing */ }

	private:

		int top    (void **p = (void **) NULL)
		{
			V3dContWidElement *ptr = (V3dContWidElement *)
				BaseLinkedList::top    (p);
			return (ptr ? ptr->_ident : (int) NULL);
		}
		int bottom (void **p = (void **) NULL)
		{
			V3dContWidElement *ptr = (V3dContWidElement *)
				BaseLinkedList::bottom (p);
			return (ptr ? ptr->_ident : (int) NULL);
		}
		int next   (void **p = (void **) NULL)
		{
			V3dContWidElement *ptr = (V3dContWidElement *)
				BaseLinkedList::next   (p);
			return (ptr ? ptr->_ident : (int) NULL);
		}
		int prev   (void **p = (void **) NULL)
		{
			V3dContWidElement *ptr = (V3dContWidElement *)
				BaseLinkedList::prev   (p);
			return (ptr ? ptr->_ident : (int) NULL);
		}
		int current(void **p = (void **) NULL)
		{
			V3dContWidElement *ptr = (V3dContWidElement *)
				BaseLinkedList::current(p);
			return (ptr ? ptr->_ident : (int) NULL);
		}
		void add(Widget widget, int ident)
		{
			V3dContWidElement *theElement = new V3dContWidElement(
				widget, ident);
			BaseLinkedList::add((Element *) theElement);
		}
		void remove(Widget widget)
			{ BaseLinkedList::remove((void *) widget); }
		int find(Widget widget)
		{
			V3dContWidElement *ptr = (V3dContWidElement *)
				BaseLinkedList::find((void *) widget);
			return (ptr ? ptr->_ident : (int) NULL);
		}
		V3dContWidLinkedList(V3dContWidLinkedList &)
			{ assert(False); }
		V3dContWidLinkedList& operator=(V3dContWidLinkedList &p)
			{ assert(False);  return p; }
};

class V3dControl : public SLFPopSep
{
	public:

		V3dControl(SLDelay *contain, char *name,
			class V3dReadout *readout,
			class V3dDataLinkedList *data,
			class VectorLinkedList *vectors,
			class V3dCage *cage,
			Bool editable = False,
			V3dPickEdit::Constraint constraint=V3dPickEdit::XandY,
			Bool visible = True);
		~V3dControl();
		void addPlot(class PlotBase *plot);
		void removePlot(class PlotBase *plot);
		void updateAngles(float degreesZ, float degreesY);

	private:

		enum CallbackIdent
		{
			/*
			 * Using 0 instead of NULL because gcc version
			 * egcs-2.91.66 19990314/Linux (egcs-1.1.2 release)
			 * will not allow NULL in enum definition.
			 * ehs --- 23aug01
			 */
			ROTATE_Z = 0 + 1,	/* NULL is not found */
			ROTATE_Y,
			VISIBLE,
			EXP_X,
			EXP_Y,
			EXP_Z,
			EDIT,
			CONSTRAIN_X,
			CONSTRAIN_Y,
			CONSTRAIN_Z
		};

		enum NotifyIdent
		{
			INIT_SKIP = FP_NONE + 1,
			DO,
			SKIP,
			NPLOT,
			SCAN_LEFT,
			SCAN_RIGHT
		};

		static void staticCallback(Widget widget, XtPointer client,
			XtPointer call);
		void objectCallback(CallbackIdent ident, Widget widget,
			XtPointer call);
		void rotate(CallbackIdent ident, float degrees);
		void expand(CallbackIdent ident, float exp);
		void visible  (               Bool set);
		void edit     (Widget widget, Bool set);
		void constrain(Widget widget, Bool set);
		Bool isVisible();
		Bool isEditable();
		V3dPickEdit::Constraint getConstraint();
		virtual Boolean notify(SLPrim *obj);
		void getScanParams(int *initSkip, int *ido, int *skip, int *inc,
			int *count);
		void updateDisplay(int initSkip, int ido, int skip, int inc,
			int count);
		void scan(int dir, int initSkip, int ido, int skip, int inc,
			int count);

		V3dControl()
			: SLFPopSep((SLDelay *) NULL, (char *) NULL,
			(unsigned long) 0, (HelpCtx) NULL)
			{ /* private, no access to default constructor */ }
		V3dControl(V3dControl &)
			: SLFPopSep((SLDelay *) NULL, (char *) NULL,
			(unsigned long) 0, (HelpCtx) NULL)
			{ /* private, no access to copy constructor */ }
		V3dControl& operator=(V3dControl &p)
			{ /* private, no access to = */ return p; }

		class V3dReadout *_readout;
		class V3dDataLinkedList *_data;
		class VectorLinkedList *_vectors;
		class V3dCage *_cage;
		class PlotBaseLinkedList *_plots;
		class SimplePickBaseLinkedList *_pickers;
		class SL2Text *_initSkip, *_do, *_skip, *_nplot;
		class SLpArrow *_scanLeft, *_scanRight;
		static V3dContWidLinkedList _widgets;
		Widget _visible;
		Widget _edit, _xConstrained, _yConstrained, _zConstrained;
		Widget _rotateZScale, _rotateYScale;
		Widget _scanMainForm;
};

#endif /* _V3D_CONTROL_HH */
