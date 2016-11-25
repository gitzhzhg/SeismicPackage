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
#ifndef _FGXP_3D_GRID_POP_HH
#define _FGXP_3D_GRID_POP_HH

/*
 * Modeled after Mike Sherrill's SeisGridPop.
 */

#include "sl/sl_form_pop.hh"

class FgXp3DGridPop : public SLFPopSep
{
	public:

		FgXp3DGridPop(SLDelay *contain, char *name, HelpCtx hctx,
			class SeisPlot *sp, class FgXp3DPop *fgXp3DPop);
		virtual ~FgXp3DGridPop();
		virtual Widget make(Widget p);
		virtual void managing();

	protected:

		/*
		 * SLFPopSep virtual function.
		 */
		virtual void DoAction();

	private:

		class SeisPlot  *_sp;
		class FgXp3DPop *_fgXp3DPop;
		class SLTextBox *_textBox;
		float _size;
		float _xMin, _xMax, _yMin, _yMax, _zMin, _zMax;
};

#endif	/* _FGXP_3D_GRID_POP_HH */
