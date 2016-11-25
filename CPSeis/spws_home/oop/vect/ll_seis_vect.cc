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
#include <string.h>
#include "vect/ll_seis_vect.hh"
#include "vect/vector_inform.hh"

SeisVectLinkedList::SeisVectLinkedList()
{
	_inform = new VectorInform(this); // VectorInform defined in 
									  // .../include/vect/vector_inform.hh

}

SeisVectLinkedList::~SeisVectLinkedList()
{
	delete _inform;
}

void SeisVectLinkedList::addPlot(class SeisPlot *plot, Bool dummy)
{
	VectorLinkedList::addPlot(plot, dummy); // call base class addPlot()
											// to add plot to list of plots
											// this SeisVectLinkedList affects

	_inform->addSeisPlot(plot);  // class VectorInform::addSeisPlot() comes
								 // from base class SeisInform. This 
								 // accomplishes 3 things: 1) this plot is
								 // added to list of plots this 
								 // SeisVectLinkedList knows about and affects;`
								 // 2) this plot is set to know about this
								 // VectorInform _inform; 3) this plot is put
								 // in a list of plots for this VectorInform
								 // informer.
}

void SeisVectLinkedList::removePlot(class SeisPlot *plot, Bool delFromInform)
{
	/*
	 * If delFromInform is False, this came from VectorInform::destroyed.
	 * In that case, VectorLinkedList::removePlot should not try
	 * to erase the vectors because the SeisPlot and its drawable
	 * are gone.
	 */
	VectorLinkedList::removePlot(plot, delFromInform);

	if (delFromInform)
		_inform->delSeisPlot(plot);
}
