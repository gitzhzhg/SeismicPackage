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
#ifndef _COLOR_POP_HH
#define _COLOR_POP_HH

#include "sl/sl_form_pop.hh"

#include <Xm/Xm.h>

class ColorPop : public SLFPopSep
{
	public:

		ColorPop(SLDelay *contain, char *name, HelpCtx hctx,
			class VaVectColors *colors);
		virtual ~ColorPop();
		virtual Widget make(Widget p);

	protected:

		virtual Boolean notifyComplex(SLDelay *obj, int ident);

	private:

		class VaVectColors *_colors ;
		class SLTogBox     *_toggles, *_tog;
		class SLRadioBox   *_radio, *_rad, *_over_mark,
			*_over_pick, *_doppler;
		class SLSep        *_top_sep, *_mid_sep1, *_mid_sep2,
			*_mid_sep3, *_mid_sep4, *_bot_sep;
};

#endif /* _COLOR_POP_HH */
