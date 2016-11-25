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
#ifndef _FG2D_PICK_HH
#define _FG2D_PICK_HH

#include "plot/pick_base.hh"

/*
 * Do all the work on button release because the work often adds a
 * PickWatch which will confuse ignoreActions from button press.
 */

class Fg2DPick : public PickBase
{
	public:

		Fg2DPick(class PlotBase *plot);
		virtual ~Fg2DPick();
		void setPlot(class Fg2DPlot *fg2DPlot);

	protected:

		class VectorLinkedList *_rbnList  ;
		class VectData         *_rbnData  ;
		class Vector           *_rbnVector;
		class Fg2DPlot         *_fg2DPlot ;

	private:

		int  _xPress, _yPress;

		virtual void noModButtonOnePress  (int x , int y);
		virtual void noModButtonOneMotion (int x1, int x2,
			int y1, int y2);
		virtual void noModButtonOneRelease(int x1, int x2,
			int y1, int y2);

		virtual void cntlButtonOnePress  (int x , int y);
		virtual void cntlButtonOneMotion (int x1, int x2,
			int y1, int y2);
		virtual void cntlButtonOneRelease(int x1, int x2,
			int y1, int y2);

		virtual void cntlButtonTwoPress  (int x , int y);
		virtual void cntlButtonTwoMotion (int x1, int x2,
			int y1, int y2);
		virtual void cntlButtonTwoRelease(int x1, int x2,
			int y1, int y2);

		virtual void buttonAny           (int x1, int x2,
			int y1, int y2,
			int button, Action action, Modifier modifier);

		void cntlButtonRelease(int x1, int x2, int y1, int y2, char c);

		Fg2DPick() : PickBase((class PlotBase *) NULL)
			{ /* private, no access to default constructor */ }
		Fg2DPick(Fg2DPick &) : PickBase((class PlotBase *) NULL)
			{ /* private, no access to copy constructor */ }
		Fg2DPick& operator=(Fg2DPick &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_PICK_HH */
