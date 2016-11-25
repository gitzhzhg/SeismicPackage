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
#ifndef _FGXP_SELECT_POP_HH
#define _FGXP_SELECT_POP_HH

#include "sl/sl_form_pop.hh"
#include "geom/fg_inform.hh"
#include "fgxp/fgxp_constants.hh"

#include <Xm/Xm.h>

enum WhichToggle
{
	CONSTANT_LINE,
	REPLACE
};

class FgXpSelectPop : public SLFPopSep, public FgInform
{
	public:

		FgXpSelectPop(SLDelay *contain, char *name, HelpCtx hctx,
			class FieldGeometry *fg);
		virtual ~FgXpSelectPop()
			{ /* do nothing */ }
		virtual Widget make(Widget p);

	protected:

		class SLRadioBox *_indexMode;
		class SLTogBox   *_toggles  ;
		class SLpText    *_message  ;
		Widget _rc;

		enum WhichText
		{
			X_INDEX1 = FP_REMOVE + 1,
			Y_INDEX1,
			Z_INDEX1,
			X_INDEX2,
			Y_INDEX2,
			Z_INDEX2
		};


		/*
		 * From SL
		 */
		virtual void DoAction() = 0;
		virtual Boolean notify(SLPrim *obj);

		/*
		 * From FgInform
		 */
		virtual void  preRemoveInsertLines(FieldGeometry *fg,
			long index, long nrem, long nins);
		virtual void postRemoveInsertLines(FieldGeometry *fg,
			long index, long nrem, long nins);
		virtual void  preNewActiveLine(FieldGeometry *fg);
		virtual void postNewActiveLine(FieldGeometry *fg);
		virtual void startingChanges(FieldGeometry *fg);
		virtual void finishedChanges(FieldGeometry *fg);
		virtual void freezingDependentUpdates  (FieldGeometry *fg) = 0;
		virtual void postResumeDependentUpdates(FieldGeometry *fg) = 0;

		/*
		 * For derived classes
		 */
		virtual char *checkIndices() = 0;
		virtual char *newActiveLine() = 0;
		virtual char *textInput(WhichText whichText) = 0;
		virtual void  resetText(WhichText whichText) = 0;
		virtual void  setSelectMode (SelectMode  selectMode ) = 0;

		/*
		 * Called at end of derived make.
		 */
		void makeAttachments(Widget widget);

		void freeze();
		void thaw  ();

	private:

		Bool _removedLines, _insertedLines, _newActiveLine, _justThawed;
		Widget _indexForm, _topSep;

		Boolean notifyComplex(SLDelay *obj, int ident);

		FgXpSelectPop()
			: SLFPopSep((SLDelay *) NULL, (char *) NULL,
				(unsigned long) 0, (HelpCtx) NULL),
			FgInform((class FieldGeometry *) NULL)
			{ /* private, no access to default constructor */ }
		FgXpSelectPop(FgXpSelectPop &)
			: SLFPopSep((SLDelay *) NULL, (char *) NULL,
				(unsigned long) 0, (HelpCtx) NULL),
			FgInform((class FieldGeometry *) NULL)
			{ /* private, no access to copy constructor */ }
		FgXpSelectPop& operator=(FgXpSelectPop &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FGXP_SELECT_POP_HH */
