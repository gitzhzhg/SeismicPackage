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
#ifndef _FG2D_LOC_OUT_HH
#define _FG2D_LOC_OUT_HH

#include "fgmap/fg_loc_out.hh"

class Fg2DLocOut : public FgLocOut
{
	public:

		Fg2DLocOut(Widget p, char *name, HelpCtx hctx,
			FieldGeometry *fg, SeisPlot *sp);
		virtual ~Fg2DLocOut();

		void setPlot(class Fg2DPlot *fg2DPlot);

	private:

		class Fg2DPlot *_fg2DPlot;

		virtual Boolean outputByOther(SeisPlot *sp, int x, int y,
			long *line_index, long *flag_index, long *shot_index,
			FgMapToFlag::SkidType *skid_type);

		Fg2DLocOut()
			: FgLocOut((Widget) NULL, (char *) NULL,
				(HelpCtx) NULL, (FieldGeometry *) NULL,
				(SeisPlot *) NULL)
			{ /* private, no access to default constructor */ }
		Fg2DLocOut(Fg2DLocOut &)
			: FgLocOut((Widget) NULL, (char *) NULL,
				(HelpCtx) NULL, (FieldGeometry *) NULL,
				(SeisPlot *) NULL)
			{ /* private, no access to copy constructor */ }
		Fg2DLocOut& operator=(Fg2DLocOut &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FG2D_LOC_OUT_HH */
