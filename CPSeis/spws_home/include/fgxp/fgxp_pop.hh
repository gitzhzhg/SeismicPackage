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
#ifndef _FGXP_POP_HH
#define _FGXP_POP_HH

#include "sl/sl_form_help_pop.hh"
#include "fgxp/fgxp_constants.hh"

#include <Xm/Xm.h>

class FgXpPop : public SLFormHelpPop
{
	public:

		FgXpPop(SLDelay *contain, char *name, HelpCtx hctx,
			class FieldGeometry *fg, class FgSeisPlotList *spList,
			float size = 6.0,
			class SLApp *app = (class SLApp *) NULL);
		virtual ~FgXpPop();
		virtual Widget make(Widget p) = 0;
		void removePlot(class FgXpPlot *fgXpDPlot);
		void clearPlots();
		int numPlots();
		class FgSeisPlot *getSp()
			{ return _sp; }
		class FgXpPlotLinkedList *getFgXpPlotLinkedList()
			{ return _plots; }
		void setDisplayMode(DisplayMode displayMode);
		void freeze();
		void thaw  ();
		class SLPullPop *pulldown();
		virtual void scale() = 0;

	protected:

		class FgSeisPlot         *_sp    ;
		class FgSeisPlotList     *_spList;
		class FieldGeometry      *_fg    ;
		class FgXpPlotLinkedList *_plots ;
		class FgXpSelectPop      *_select;
		class FgXpPick           *_pick  ;

		float _size;
		char *_selectName;
		Bool _frozen;
		class SLPullPop *_pulldown, *_displayModeCascade;

		virtual void managing() = 0;
		virtual Boolean notifyComplex(SLDelay *obj, int ident);
		void extraMouseButtonThreeStuff();

	private:

		virtual void extraButton(int ident);

		FgXpPop()
			: SLFormHelpPop((SLDelay *) NULL, (char *) NULL,
				(unsigned long) 0, (HelpCtx) NULL)
			{ /* private, no access to default constructor */ }
		FgXpPop(FgXpPop &)
			: SLFormHelpPop((SLDelay *) NULL, (char *) NULL,
				(unsigned long) 0, (HelpCtx) NULL)
			{ /* private, no access to copy constructor */ }
		FgXpPop& operator=(FgXpPop &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FGXP_POP_HH */
