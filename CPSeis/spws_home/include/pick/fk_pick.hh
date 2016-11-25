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
#ifndef _FK_PICK_HH
#define _FK_PICK_HH

#include "plot/pick_base.hh"

class FkPick : public PickBase
{
	public:

		FkPick(class FkPop *fkPop);
		virtual ~FkPick();

	protected:

		virtual void noModButtonOnePress  (int x, int y);
		virtual void shiftButtonOnePress  (int x, int y);
		virtual void cntlButtonOnePress   (int x, int y);

		virtual void noModButtonOneMotion (int x1, int x2,
			int y1, int y2);
		virtual void shiftButtonOneMotion (int x1, int x2,
			int y1, int y2);
		virtual void cntlButtonOneMotion  (int x1, int x2,
			int y1, int y2);

		virtual void noModButtonOneRelease(int x1, int x2,
			int y1, int y2);
		virtual void shiftButtonOneRelease(int x1, int x2,
			int y1, int y2);
		virtual void cntlButtonOneRelease (int x1, int x2,
			int y1, int y2);

		virtual void noModButtonTwoPress  (int x, int y);
		virtual void shiftButtonTwoPress  (int x, int y);
		virtual void cntlButtonTwoPress   (int x, int y);

		virtual void noModButtonTwoMotion (int x1, int x2,
			int y1, int y2);
		virtual void shiftButtonTwoMotion (int x1, int x2,
			int y1, int y2);
		virtual void cntlButtonTwoMotion  (int x1, int x2,
			int y1, int y2);

		virtual void noModButtonTwoRelease(int x1, int x2,
			int y1, int y2);
		virtual void shiftButtonTwoRelease(int x1, int x2,
			int y1, int y2);
		virtual void cntlButtonTwoRelease (int x1, int x2,
			int y1, int y2);

	private:

		enum PickMode { Normal, InsertFan, InsertFanSym, InsertPoly,
			EditFan, EditPoly };

		PickMode _pickMode;
		class FkPop *_fkPop;
		class SeisVectLinkedList *_vectors;
		class VectData *_rbnData, *_insData;
		class Vector   *_rbnVect, *_insVect, *_edtVect;
		class FkData   *_edtData;
		int _edtIndex;
		char *_vectColor;

		int canInsertFan();
		int canInsertPoly();
		int canInsertPolyPt();
		int insertedPolyDone();

		void getFanValues(int xDc, int yDc, float *xFan, float *yFan);

		FkPick(        ) : PickBase((class PlotBase *) NULL)
			{ /* private, no access to default constructor */ }
		FkPick(FkPick &) : PickBase((class PlotBase *) NULL)
			{ /* private, no access to copy constructor */ }
		FkPick& operator=(FkPick &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _FK_PICK_HH */
