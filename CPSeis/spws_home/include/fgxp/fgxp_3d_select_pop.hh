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
#ifndef _FGXP_3D_SELECT_POP_HH
#define _FGXP_3D_SELECT_POP_HH

#include "fgxp/fgxp_select_pop.hh"

#include <Xm/Xm.h>
#include <assert.h>

class FgXp3DSelectPop : public FgXpSelectPop
{
	public:

		FgXp3DSelectPop(SLDelay *contain, char *name, HelpCtx hctx,
			class FieldGeometry *fg, class FgXp3DPop *fgXp3DPop);
		virtual ~FgXp3DSelectPop()
			{ /* do nothing */ }
		virtual Widget make(Widget p);

	protected:

		/*
		 * From SL
		 */
		virtual void DoAction();

		/*
		 * From FgXpSelectPop
		 */
		virtual char *checkIndices();
		virtual char *newActiveLine();
		virtual char *textInput(WhichText whichText);
		virtual void  resetText(WhichText whichText);
		virtual void  setSelectMode (SelectMode  selectMode );

		/*
		 * From FgInform
		 */
		virtual void freezingDependentUpdates  (FieldGeometry *fg);
		virtual void postResumeDependentUpdates(FieldGeometry *fg);

	private:

		class FgXp3DPop *_fgXp3DPop;
		class SLOptionMenu *_xDataType, *_yDataType, *_zDataType;
		class SLpText *_xIndex1, *_yIndex1, *_zIndex1,
			*_xIndex2, *_yIndex2, *_zIndex2;

		Widget _indexLabel1, _indexLabel2;

		int _textInput;

		FgXp3DSelectPop()
			: FgXpSelectPop((SLDelay *) NULL, (char *) NULL,
				(HelpCtx) NULL, (class FieldGeometry *) NULL)
			{ /* private, no access to default constructor */ }
		FgXp3DSelectPop(FgXp3DSelectPop &)
			: FgXpSelectPop((SLDelay *) NULL, (char *) NULL,
				(HelpCtx) NULL, (class FieldGeometry *) NULL)
			{ /* private, no access to copy constructor */ }
		FgXp3DSelectPop& operator=(FgXp3DSelectPop &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FGXP_3D_SELECT_POP_HH */
