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
#ifndef _VECTOR_INFORM_HH
#define _VECTOR_INFORM_HH

#include "sp/seis_inform.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/hard_vect.hh"

class VectorInform : public SeisInform
{
	public:

		VectorInform(SeisVectLinkedList *vectors) : _vectors(vectors)
			{ /* do nothing */ }
		~VectorInform()
			{ /* do nothing */ }
		void expose(SeisPlot *sp, int x, int y, int width, int height)
			{ _vectors->repair(sp, x, y, width, height); }
		void newPlot(SeisPlot *sp)
			{ _vectors->deferredRepairAll(sp); }
		void postZoomSeparateWindow(SeisPlot *sp, SeisPlot *zoomsp)
			{
				zoomsp->setTransform(sp->transform());
				_vectors->addPlot(zoomsp, True);
			}
		void destroyed(SeisPlot *sp)
			{ _vectors->removePlot(sp, False); }
		void postScan (SeisPlot *sp, SeisPlot::ScanDir  /*dir*/)
			{ _vectors->deferredRepairAll(sp); }
		void postZoom (SeisPlot *sp, SeisPlot::ZoomDir    dir  )
			{
				if ((dir != SeisPlot::UpSeparateWin)
				 && (dir != SeisPlot::Abort        ))
				{
					_vectors->deferredRepairAll(sp);
				}
			}
		void postMovie(SeisPlot *sp, SeisPlot::MovieDir /*dir*/)
			{ _vectors->deferredRepairAll(sp); }
		void visableAreaChange(SeisPlot *sp, int x, int y,
			int width, int height)
			{ _vectors->visableAreaChange(sp, x, y, width, height);}
		void preWriteHardCopy(SeisPlot * /*sp*/)
			{ _vectors->setDoDefer(0); }
		void writeToHardCopy(SeisPlot *sp, HardCopyPlot *hard,
			float scale)
			/* junk is only to suppress compiler warning. */
			{ HardVect junk(_vectors, sp, hard, scale); }
		void postWriteHardCopy(SeisPlot * /*sp*/)
			{ _vectors->setDoDefer(1); }

	private:

		SeisVectLinkedList *_vectors;
};

#endif
