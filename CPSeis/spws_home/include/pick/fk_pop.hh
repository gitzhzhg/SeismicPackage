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
#ifndef _FK_POP_HH
#define _FK_POP_HH

#include "sl/sl_form_pop.hh"
#include "sp/seis_inform.hh"
#include "sp/sp_transform.hh"
#include "pick/fk_data.hh"

class FkSPTransform : public SPTransform
{
	public:

		FkSPTransform(class FkDataLinkedList *data);

		virtual float convDataToXOffset(SeisPlot *sp, float data);
		virtual float convXOffsetToData(SeisPlot *sp, float data);
		virtual float convDataToYOffset(SeisPlot *sp, float data);
		virtual float convYOffsetToData(SeisPlot *sp, float data);

	private:

		class FkDataLinkedList *_data;
};

#define NUM_COLORS 5

class FkPop : public SLFPopSep, public SeisInform
{
	public:

		FkPop(SLDelay *contain, char *name, HelpCtx hctx, SeisPlot *sp);
		virtual ~FkPop();
		virtual Widget make(Widget p = (Widget) NULL);
		virtual void newPlot (SeisPlot *sp);
		virtual void  preScan(SeisPlot *, SeisPlot::ScanDir dir);
		virtual void postScan(SeisPlot *, SeisPlot::ScanDir dir);
		int getDisplayedHeaders(float *hw1, float *hw2);
		char *nextColor();
		class FkPair             *getFilePair();
		      SeisPlot           *getSeisPlot();
		class FkDataLinkedList   *getData    ();
		class SeisVectLinkedList *getVectors ();
		virtual Boolean notifyComplex(SLDelay *obj, int ident);
		CutPass getCutPass();
		virtual void stopActivity();

	protected:

		virtual void DoAction();
		virtual void extraButton(int ident);
		virtual void managing();

	private:

		class FkPair             *_fkPair  ;
		      SeisPlot           *_sp      ;
		class FkDataLinkedList   *_data    ;
		class SeisVectLinkedList *_vectors ;
		FkSPTransform            *_trans   ;
		  SPTransform            *_oldTrans;
		class SLpText            *_whw1    ;
		class SLpText            *_whw2    ;
		class SLOptionMenu       *_cutPass ;
		class FkPick             *_pick    ;

		float _hw1, _hw2;
		int _firstDisplay;
		float _oldHw1, _oldHw2;
		int _scannedDisplay;
		static char *_colors[NUM_COLORS];
		int _colorIndex;

		void updateDisplay();
		void start();
		void stop();
		void copyPrev();
		void clear();

		FkPop()
			: SLFPopSep((SLDelay *) NULL, (char *) NULL,
				(unsigned long) 0, (HelpCtx) NULL)
			{ /* private, no access to default constructor */ }
		FkPop(FkPop &)
			: SLFPopSep((SLDelay *) NULL, (char *) NULL,
				(unsigned long) 0, (HelpCtx) NULL)
			{ /* private, no access to copy constructor */ }
		FkPop& operator=(FkPop &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FK_POP_HH */
