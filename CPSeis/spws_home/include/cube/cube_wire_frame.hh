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
#ifndef _CUBE_WIRE_FRAME_HH
#define _CUBE_WIRE_FRAME_HH

#include "plot/pick_base.hh"
#include "cube/cube_inform.hh"

class CubeWireFramePick : public PickBase
{
	public:

		CubeWireFramePick(class PlotBase *plot,
			class CubeWireFrame *wire);
		~CubeWireFramePick();

	private:

		class CubeWireFrame *_wire;
		class Rotate3D      *_rotate3D;

		/*
		 * PickBase virtual functions.
		 */
		virtual void buttonOnePress       (int x , int y ,
			Modifier modifier);
		virtual void buttonOneMotion      (int x1, int x2,
			int y1, int y2, Modifier modifier);
		virtual void buttonOneRelease     (int x1, int x2,
			int y1, int y2, Modifier modifier);
		virtual void shiftButtonOnePress  (int x , int y );
		virtual void shiftButtonOneMotion (int x1, int x2,
			int y1, int y2);
		virtual void shiftButtonOneRelease(int x1, int x2,
			int y1, int y2);
		virtual void buttonTwoPress       (int x , int y ,
			Modifier modifier);
		virtual void buttonTwoMotion      (int x1, int x2,
			int y1, int y2, Modifier modifier);
		virtual void buttonTwoRelease     (int x1, int x2,
			int y1, int y2, Modifier modifier);

		CubeWireFramePick()
			: PickBase((class PlotBase *) NULL)
			{ /* private, no access to default constructor */ }
		CubeWireFramePick(CubeWireFramePick &)
			: PickBase((class PlotBase *) NULL)
			{ /* private, no access to copy constructor */ }
		CubeWireFramePick& operator=(CubeWireFramePick &p)
			{ /* private, no access to = */ return p; }
};

class CubeWireFrame : public CubeInform
{
	public:

		CubeWireFrame(class CubeDisplay *cd, class SeisPlot *sp);
		~CubeWireFrame();
		class V3dCage               *getCage();
		class Trans3Dto2DLinkedList *getData();
		Bool  startMoveSlice(int xDc, int yDc, float x2D, float y2D);
		void   dragMoveSlice(                  float x2D, float y2D);
		void finishMoveSlice(                  float x2D, float y2D);

		class SeisPlot *SP();

		void rbn(float xl, float il, float ts);
		void clearRbn();

	private:

		enum View { _INLINE, _CROSSLINE, _TIMESLICE, _NUM_VIEWS };
		enum Face { _TOP, _BOTTOM, _FRONT, _BACK, _LEFT, _RIGHT };

		class CubeDisplay           *_cd                ;
		class SeisPlot              *_sp                ;
		class Trans3Dto2DLinkedList *_data2D            ;
		class SeisVectLinkedList    *_vectors           ;
		class Vector                *_vector[_NUM_VIEWS];
		class Vect3DData            *_square[_NUM_VIEWS];
		class Trans3Dto2D           *_trans [_NUM_VIEWS];
		class Vector                *_rbnVector         ;
		class Vect3DData            *_rbnSquare         ;
		class Trans3Dto2D           *_rbnTrans          ;
		class V3dCage               *_cage              ;
		      CubeWireFramePick     *_picker            ;

		Bool _cubeInited;
		View _movingView;
		Face _movingFace;

		int _xMin, _xCur, _xMax;	/* x is crossline  */
		int _yMin, _yCur, _yMax;	/* y is inline     */
		int _zMin, _zCur, _zMax;	/* z is time slice */
		int _zMin_seis, _zMax_seis;

		/*
		 * Stuff for rbn crosshair
		 */
		int _xh_vis;
		class Vector      *_xh_vect[_NUM_VIEWS];
		class Vect3DData  *_xh_data[_NUM_VIEWS];
		class Trans3Dto2D *_xh_tran[_NUM_VIEWS];

		/*
		 * CubeInform virtual functions.
		 */
		virtual void postPlot       (Cube *cube, int in_line,
			int cross_line, int time_slice);
		virtual void noPlotDisplayed(Cube *cube);
		virtual void cubeIsCurrent  (Cube *cube);
		virtual void cubeMovie      (Cube *cube,
			Cube::WhichPlot face,
			Cube::MovieDir  mdir,
			int             slice);
		virtual void newCubeCreated (Cube *cube);
		/*
		 * A deleted cube is automatically removed from
		 * all CubeInform lists.  Therefore, I do not need destroyed.
		virtual void destroyed      (Cube *cube);
		 */

		Bool checkCube(Cube *cube, int in_line, int cross_line,
			int time_slice);
		void initCube();
		void loadCube();
		Bool startInline    (float x2D, float y2D,
			int index1, int index2);
		Bool startCrossline (float x2D, float y2D,
			int index1, int index2);
		Bool startTimeslice (float x2D, float y2D,
			int index1, int index2);
		void dragInline     (float x2D, float y2D);
		void dragCrossline  (float x2D, float y2D);
		void dragTimeslice  (float x2D, float y2D);
		void finishInline   (float x2D, float y2D);
		void finishCrossline(float x2D, float y2D);
		void finishTimeslice(float x2D, float y2D);

		CubeWireFrame()
			{ /* private, no access to default constructor */ }
		CubeWireFrame(CubeWireFrame &)
			{ /* private, no access to copy constructor */ }
		CubeWireFrame& operator=(CubeWireFrame &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _CUBE_WIRE_FRAME_HH */
