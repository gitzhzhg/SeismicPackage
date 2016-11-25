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
#ifndef _FGXP_3D_PICK_HH
#define _FGXP_3D_PICK_HH

#include "fgxp/fgxp_pick.hh"

class FgXp3DPick : public FgXpPick
{
	public:

		FgXp3DPick(class PlotBase *plot,
			class FgXpPlotLinkedList *plots, class V3dCage *cage,
			Bool canDelete = False,
			AreaSelectMode areaSelectMode = selectFlags,
			int interpolateX = 0, int interpolateY = 0,
			int interpolateZ = 1);
		virtual ~FgXp3DPick()
			{ /* do nothing */ }
		void setAreaSelectMode(AreaSelectMode areaSelectMode);

	private:

		class V3dCage  *_cage    ;
		class Rotate3D *_rotate3D;
		int _interpolate[3];

		virtual void shiftButtonOnePress  (int x , int y);
		virtual void shiftButtonOneMotion (int x1, int x2,
			int y1, int y2);
		virtual void shiftButtonOneRelease(int x1, int x2, int y1,
			int y2);

		virtual void cntlButtonOneRelease(int x1, int x2,
			int y1, int y2);

		FgXp3DPick() : FgXpPick((class PlotBase *) NULL,
			(char *) NULL, (const char *) NULL, (const char *) NULL,
			(class FgXpPlotLinkedList *) NULL, False, selectFlags)
			{ /* private, no access to default constructor */ }
		FgXp3DPick(FgXp3DPick &) : FgXpPick((class PlotBase *) NULL,
			(char *) NULL, (const char *) NULL, (const char *) NULL,
			(class FgXpPlotLinkedList *) NULL, False, selectFlags)
			{ /* private, no access to copy constructor */ }
		FgXp3DPick& operator=(FgXp3DPick &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FGXP_3D_PICK_HH */
