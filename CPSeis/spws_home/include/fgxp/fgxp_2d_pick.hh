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
#ifndef _FGXP_2D_PICK_HH
#define _FGXP_2D_PICK_HH

#include "fgxp/fgxp_pick.hh"

enum PickConstraint
{
	noEditing   ,
	constrainX  ,
	constrainY  ,
	noConstraint
};

class FgXp2DPick : public FgXpPick
{
	public:

		FgXp2DPick(class PlotBase *plot,
			class FgXpPlotLinkedList *plots,
			PickConstraint pickConstraint = noConstraint,
			Bool canDelete = False,
			AreaSelectMode areaSelectMode = selectFlags,
			class FgLocOut *fgLocOut = (class FgLocOut *) NULL);
		virtual ~FgXp2DPick()
			{ /* do nothing */ }
		void setConstraint(PickConstraint pickConstraint)
			{ _pickConstraint = pickConstraint; }
		void setAreaSelectMode(AreaSelectMode areaSelectMode);

	private:

		PickConstraint _pickConstraint;
		class FgLocOut *_fgLocOut;
		float _constrainedX, _constrainedY;
		Bool _attr1, _attr2;
		float _xPrev, _yPrev;

		virtual void shiftButtonOnePress  (int x , int y);
		virtual void shiftButtonOneMotion (int x1, int x2,
			int y1, int y2);
		virtual void shiftButtonOneRelease(int x1, int x2, int y1,
			int y2);

		virtual void cntlButtonOneRelease(int x1, int x2,
			int y1, int y2);

		FgXp2DPick() : FgXpPick((class PlotBase *) NULL,
			(char *) NULL, (const char *) NULL, (const char *) NULL,
			(class FgXpPlotLinkedList *) NULL, False, selectFlags)
			{ /* private, no access to default constructor */ }
		FgXp2DPick(FgXp2DPick &) : FgXpPick((class PlotBase *) NULL,
			(char *) NULL, (const char *) NULL, (const char *) NULL,
			(class FgXpPlotLinkedList *) NULL, False, selectFlags)
			{ /* private, no access to copy constructor */ }
		FgXp2DPick& operator=(FgXp2DPick &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FGXP_2D_PICK_HH */
