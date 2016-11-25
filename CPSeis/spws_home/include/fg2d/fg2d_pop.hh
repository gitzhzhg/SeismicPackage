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
#ifndef _FG2D_POP_HH
#define _FG2D_POP_HH

#include "sl/sl_form_help_pop.hh"
#include "fg2d/fg2d_chart_data.hh"

#include <Xm/Xm.h>

class Fg2DPop : public SLFormHelpPop
{
	public:

		enum Type
		{
			GndPosChart  ,
			StackingChart,
			Fold         ,
			Statics      ,
			Nothing
		};

		Fg2DPop(SLDelay *contain, char *name, HelpCtx hctx,
			class FieldGeometry *fg, class FgSeisPlotList *spList,
			float size = 6.0,
			class SLApp *app = (class SLApp *) NULL);
		virtual ~Fg2DPop();
		virtual Widget make(Widget p = (Widget) NULL);
		class SLPullPop *pulldown();

	protected:

		virtual void managing();
		virtual Boolean notifyComplex(SLDelay *obj, int ident);

	private:

		class FieldGeometry      *_fg    ;
		class FgSeisPlot         *_sp    ;
		class FgSeisPlotList     *_spList;
		class Fg2DPlot           *_plot  ;
		class Fg2DPick           *_pick  ;
		class Fg2DLocOut         *_locOut;
		class SLApp              *_app   ;
		class FgXpPlotLinkedList *_plots ;
		class StaticsReadPop     *_pop   ;

		float _size;
		class SLPullPop *_pulldown;
		class SLPullPop *_pullTypeCas   , *_popTypeCas   ;
		class SLPullPop *_pullActiveCas , *_popActiveCas ;
		class SLPullPop *_pullSelectCas , *_popSelectCas ;
		class SLOptionMenu *_chartType;
		Type _type;
		Fg2DChartData::ActiveDriver _activeDriver;
		Fg2DChartData::SelectDriver _selectDriver;

		void addRadios(class SLPullPop *typeCas,
			class SLPullPop *activeCas, class SLPullPop *selectCas);
		void setType(Type type);
		void setActiveDriver(Fg2DChartData::ActiveDriver activeDriver);
		void setSelectDriver(Fg2DChartData::SelectDriver selectDriver);
		void setSensitivities();
		void setTitle(char *title);

		Fg2DPop()
			: SLFormHelpPop((SLDelay *) NULL, (char *) NULL,
				(unsigned long) 0, (HelpCtx) NULL)
			{ /* private, no access to default constructor */ }
		Fg2DPop(Fg2DPop &)
			: SLFormHelpPop((SLDelay *) NULL, (char *) NULL,
				(unsigned long) 0, (HelpCtx) NULL)
			{ /* private, no access to copy constructor */ }
		Fg2DPop& operator=(Fg2DPop &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_POP_HH */
