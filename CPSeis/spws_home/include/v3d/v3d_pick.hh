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
#ifndef _V3D_PICK_HH
#define _V3D_PICK_HH

#include "plot/pick_base.hh"

class V3dPick : public PickBase
{
	public:

		V3dPick(class VectorLinkedList *vectors,
			class PlotBase *plot, class V3dReadout *readout,
			class V3dDataLinkedList *data, class V3dCage *cage,
			class V3dControl *control, char *mode = "3D viewer",
			const char * const helpToken = "V3D_PICK",
			const char * const helpFallback = 
"mouse*plot*V3D_PICK: BTN#1: get location, BTN#2: none\\nShift-BTN#1 on R: rotate, Shift-BTN#2: none, BTN#3: none")
			: PickBase(plot, mode, helpToken, helpFallback),
			_vectors(vectors), _readout(readout), _data(data),
			_cage(cage), _control(control)
			{ /* do nothing */ }
		virtual ~V3dPick()
			{ /* do nothing */ }

	protected:

		class VectorLinkedList  *_vectors;
		class V3dReadout        *_readout;
		class V3dDataLinkedList *_data   ;

		virtual void noModButtonOnePress  (int x , int y);
		virtual void buttonAny(int x1, int x2, int y1, int y2,
			int button, Action action, Modifier modifier);

		virtual void shiftButtonOnePress  (int x , int y);
		virtual void shiftButtonOneMotion (int x1, int x2,
			int y1, int y2);
		virtual void shiftButtonOneRelease(int x1, int x2,
			int y1, int y2);

	private:

		class V3dCage    *_cage    ;
		class V3dControl *_control ;
		class Rotate3D   *_rotate3D;

		V3dPick() : PickBase((class PlotBase *) NULL)
			{ /* private, no access to default constructor */ }
		V3dPick(V3dPick &) : PickBase((class PlotBase *) NULL)
			{ /* private, no access to copy constructor */ }
		V3dPick& operator=(V3dPick &p)
			{ /* private, no access to = */ return p; }
};

class V3dPickEdit : public V3dPick
{
	public:

		enum Constraint
		{
			X    ,
			Y    ,
			Z    ,
			XandY,
			XandZ,
			YandZ
		};

		V3dPickEdit::V3dPickEdit(class VectorLinkedList *vectors,
			class PlotBase *plot, class V3dReadout *readout,
			class V3dDataLinkedList *data, class V3dCage *cage,
			class V3dControl *control,
			Constraint constraint = XandY, float nil = -1e-30,
			char *mode = "3D editor",
			const char * const helpToken = "V3D_PICK_EDIT",
			const char * const helpFallback = 
"mouse*plot*V3D_PICK_EDIT: BTN#1: get location, BTN#2: edit\\nShift-BTN#1 on R: rotate, Shift-BTN#2: nil, BTN#3: none");
		virtual ~V3dPickEdit()
			{ /* do nothing */ }
		void setConstraint(Constraint constaint)
			{ _constraint = constaint; }
		Constraint getConstraint()
			{ return _constraint; }

	private:

		Constraint _constraint;
		float _nil;

		int _numVectorsToEdit;
		int *_edtIndex;
		class Vect3DData **_edtData;
		class Vect3DData **_rbnData;
		class Trans3Dto2D  *_edtTrans;
		class Trans3Dto2D **_rbnTrans;
		class Vector **_rbnVector;
		float _xFixed3D, _yFixed3D, _zFixed3D;

		virtual void noModButtonTwoPress  (int x , int y);
		virtual void noModButtonTwoMotion (int x1, int x2,
			int y1, int y2);
		virtual void noModButtonTwoRelease(int x1, int x2, int y1,
			int y2);
		virtual void shiftButtonTwoPress  (int x , int y);

		V3dPickEdit()
			: V3dPick((class VectorLinkedList *) NULL,
			(class PlotBase *) NULL, (class V3dReadout *) NULL,
			(class V3dDataLinkedList *) NULL,
			(class V3dCage *) NULL, (class V3dControl *) NULL)
			{ /* private, no access to default constructor */ }
		V3dPickEdit(V3dPickEdit &)
			: V3dPick((class VectorLinkedList *) NULL,
			(class PlotBase *) NULL, (class V3dReadout *) NULL,
			(class V3dDataLinkedList *) NULL,
			(class V3dCage *) NULL, (class V3dControl *) NULL)
			{ /* private, no access to copy constructor */ }
		V3dPickEdit& operator=(V3dPickEdit &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _V3D_PICK_HH */
