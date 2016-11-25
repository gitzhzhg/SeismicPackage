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
#ifndef _FGXP_PICK_HH
#define _FGXP_PICK_HH

#include "plot/pick_base.hh"

enum AreaSelectMode
{
	selectFlags     ,
	interpolateFlags,
	selectLine
};

/*
 * Do all the work on button release because the work often adds a
 * PickWatch which will confuse ignoreActions from button press.
 */

class FgXpPick : public PickBase
{
	public:

		FgXpPick(class PlotBase *plot, char *mode,
			const char * const helpToken,
			const char * const helpFallback,
			class FgXpPlotLinkedList *plots, Bool canDelete,
			AreaSelectMode areaSelectMode);
		virtual ~FgXpPick();

	protected:

		class FgXpPlotLinkedList *_plots         ;
		class BaseData           *_edtData       ;
		class VectorLinkedList   *_rbnList       ;
		class VectData           *_rbnData       ;
		class Vector             *_rbnVector     ;
		      AreaSelectMode      _areaSelectMode;

		Bool _canDelete;
		int _edtIndex;

		void areaSelectPress (int x , int y);
		void areaSelectMotion(int x1, int x2, int y1, int y2);
		Bool getIndices(int x, int y, long *ixl_ret,
			class FieldGeometry **fg_ret,
			long *ixf_ret  = (long *) NULL,
			long *ixs2_ret = (long *) NULL,
			Bool multi_lines = False,
			int *num_lines = (int *) NULL);

	private:

		int  _xPress, _yPress;

		virtual void noModButtonOnePress  (int x , int y);
		virtual void noModButtonOneMotion (int x1, int x2,
			int y1, int y2);
		virtual void noModButtonOneRelease(int x1, int x2,
			int y1, int y2);

		virtual void noModButtonTwoPress  (int x , int y);
		virtual void noModButtonTwoMotion (int x1, int x2,
			int y1, int y2);
		virtual void noModButtonTwoRelease(int x1, int x2,
			int y1, int y2);

		virtual void shiftButtonTwoPress  (int x , int y);
		virtual void shiftButtonTwoMotion (int x1, int x2,
			int y1, int y2);
		virtual void shiftButtonTwoRelease(int x1, int x2,
			int y1, int y2);

		/*
		 * cntlButtonOnePress & cntlButtonOneMotion graphically
		 * select area.  cntlButtonOneRelease is in derived
		 * class to do something with selected area.
		 */
		virtual void cntlButtonOnePress  (int x , int y);
		virtual void cntlButtonOneMotion (int x1, int x2,
			int y1, int y2);

		virtual void cntlButtonTwoPress  (int x , int y);
		virtual void cntlButtonTwoMotion (int x1, int x2,
			int y1, int y2);
		virtual void cntlButtonTwoRelease(int x1, int x2,
			int y1, int y2);

		virtual void buttonAny           (int x1, int x2,
			int y1, int y2,
			int button, Action action, Modifier modifier);

		FgXpPick() : PickBase((class PlotBase *) NULL)
			{ /* private, no access to default constructor */ }
		FgXpPick(FgXpPick &) : PickBase((class PlotBase *) NULL)
			{ /* private, no access to copy constructor */ }
		FgXpPick& operator=(FgXpPick &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FGXP_PICK_HH */
