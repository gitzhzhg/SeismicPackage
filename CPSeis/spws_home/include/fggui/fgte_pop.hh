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
#ifndef _FGTE_POP_HH
#define _FGTE_POP_HH

#include "sl/sl_form_pop.hh"

class FgTePop : public SLFPopSep
{
	public:
	
		FgTePop(SLDelay *contain, char *name, HelpCtx Hctx,
			class FieldGeometry *fg, SLDelay *dcp,
			class ContainerList *all_pops);
		virtual ~FgTePop();
		Widget make(Widget p = (Widget) NULL);
		void setValidity(int flag);
		void updateFile();
	
	private:

		virtual void extraButton(int ident);
		static char *staticLabelUpdate(void *data);

		class FieldGeometry *_fg      ;
		      SLDelay       *_dcp     ;
		class ContainerList *_all_pops;
		class FgTeData      *_data    ;
		class FgMessageGui  *_message ;
		class FgStatusGui   *_status  ;
		class FgTePair      *_pair    ;
		class SLpLabel      *_label   ;
		class FgTeTable     *_table   ;
};
	
#endif /* _FGTE_POP_HH */
