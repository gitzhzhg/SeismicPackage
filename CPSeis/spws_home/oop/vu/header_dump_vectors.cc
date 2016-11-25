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

//------------------- header_dump_vectors.cc ---------------------//
//------------------- header_dump_vectors.cc ---------------------//
//------------------- header_dump_vectors.cc ---------------------//

//        implementation file for the HeaderDumpVectors class
//             derived from the SeisVectLinkedList class
//                         subdirectory pick


#include <string.h>
#include "vu/header_dump_vectors.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "sp/seis_plot.hh"
#include "plot_image.hh"
#include "sl/sl_table_view.hh"
#include <iostream.h>
#include <assert.h>

#define MARKER_LENGTH    11
#define LOCATION_COLOR   "red"
#define SELECTION_COLOR  "orange"


//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//
//--------------------- constructor and destructor ---------------//

HeaderDumpVectors::HeaderDumpVectors(SeisPlot *sp,
                                     SLTableView *view)
          : SeisVectLinkedList(),
                    _view        (view),
                    _sel_vector  (NULL),
                    _loc_vector  (NULL),
                    _sel_data    (NULL),
                    _loc_data    (NULL),
                    _visible     (FALSE),
                    _ymin        (0.0),
                    _yperpix     (0.0),
                    _sp          (sp)
{
  assert(sp && view);
  addPlot(sp);
  _view->setSelectTrap(selectTrap, this);
  _view->setLocateTrap(locateTrap, this);

  _sel_data = new VectData(0, NULL, NULL);
  _loc_data = new VectData(0, NULL, NULL);
  _sel_vector = add(_sel_data,
                        SELECTION_COLOR, 2, False, Vector::NoLine,
                        Vector::VerticalLineMarker, 10000, 2);
  _loc_vector = add(_loc_data,
                        LOCATION_COLOR, 2, False, Vector::NoLine,
                        Vector::FilledDiamondMarker, MARKER_LENGTH, 1);
/*_loc_vector->setXYOffsets(FALSE, TRUE); */
  _loc_vector->setXYOffsets(Vector::IsNotOffset, Vector::IsOffset);
  makeInvisible();
}


HeaderDumpVectors::~HeaderDumpVectors()
{
  remove(_sel_vector);
  remove(_loc_vector);
  delete _sel_data;
  delete _loc_data;
}



//------------------- make visible or invisible ----------------//
//------------------- make visible or invisible ----------------//
//------------------- make visible or invisible ----------------//

void HeaderDumpVectors::makeVisible()
{
  _sel_vector->makeVisible();
  _loc_vector->makeVisible();
  _visible = TRUE;
}


void HeaderDumpVectors::makeInvisible()
{
  _sel_vector->makeInvisible();
  _loc_vector->makeInvisible();
  _visible = FALSE;
}



//-------------------- update sel vector -----------------------//
//-------------------- update sel vector -----------------------//
//-------------------- update sel vector -----------------------//

void HeaderDumpVectors::updateSelVector()
{
float trace_value;


  if(!_visible) return;
  long ncolumns   = _view->numberOfColumns();
  long nsel       = _view->numberOfAllSelectedColumns();
  int old_points = _sel_data->getNumPts();
  if(nsel == 0 || ncolumns == 0)
     {
     _sel_data->remove(0, old_points);
     return;
     }
  float *xdata = new float[nsel];
  float *ydata = new float[nsel];
  int column, n;
  for(column = 1, n = 0; column <= ncolumns && n < nsel; column++)
     {
     Boolean selected = _view->columnIsSelected(column);
     if(selected)
        {
        long truecol = _view->trueColumnFromCurrentColumn(column);

        if(_sp->plotType() == PlotImage::PlotSEMB ||
           _sp->plotType() == PlotImage::PlotARRAY   )
          {
          long index = 
            _sp->currentFrame() * _sp->originalTraces() * _sp->numHeaders()
            + _sp->matchHeader() - 1 + ((truecol - 1) * _sp->numHeaders());
          const float *hd = _sp->headers();
          trace_value = hd[index];
          }
        else
          {
          trace_value = (float)truecol;
          }
        xdata[n] = trace_value;
        ydata[n] = 0.0;
        n++;
        }
     }
  if(n != nsel) cout << "HeaderDumpVectors::updateSelData: " <<
                        "inconsistent number of selections" << endl;
  _sel_data->replace(0, old_points, n, xdata, ydata);
  delete [] xdata;
  delete [] ydata;
}



//-------------------- update loc vector -----------------------//
//-------------------- update loc vector -----------------------//
//-------------------- update loc vector -----------------------//

void HeaderDumpVectors::updateLocVector()
{
float trace_value;

  if(!_visible) return;
  long first = _view->firstVisibleColumn();
  long last  = _view->lastVisibleColumn();
  int old_points = _loc_data->getNumPts();

  if(last < first)
     {
     _loc_data->remove(0, old_points);
     return;
     }

  long nshow = last - first + 1;
  float *xdata = new float[nshow+1];
  float *ydata = new float[nshow+1];
  long column, n;

  for(column = first, n = 0; column <= last && n < nshow; column++)
     {
     long truecol = _view->trueColumnFromCurrentColumn(column);
     if(truecol > 0)
          {
          if(_sp->plotType() == PlotImage::PlotSEMB ||
           _sp->plotType() == PlotImage::PlotARRAY   )
            {
            long index = 
              _sp->currentFrame() * _sp->originalTraces() * _sp->numHeaders()
              + _sp->matchHeader() - 1 + ((truecol - 1) * _sp->numHeaders());
            const float *hd = _sp->headers();
            trace_value = hd[index];
            }
          else
            {
            trace_value = (float)truecol;
            }
          xdata[n] = trace_value;
          ydata[n] = 25;
          n++;
          }
     }

  long active  = _view->activeColumn();
  long truecol = _view->trueColumnFromCurrentColumn(active);
  if(truecol > 0)
     {
     if(_sp->plotType() == PlotImage::PlotSEMB ||
        _sp->plotType() == PlotImage::PlotARRAY   )
       {
       long index = 
          _sp->currentFrame() * _sp->originalTraces() * _sp->numHeaders()
          + _sp->matchHeader() - 1 + ((truecol - 1) * _sp->numHeaders());
       const float *hd = _sp->headers();
       trace_value = hd[index];
       }
     else
       {
       trace_value = (float)truecol;
       }
     xdata[n] = trace_value;
     ydata[n] = 15;
     n++;
     }

  _loc_data->replace(0, old_points, (int)n, xdata, ydata);
  delete [] xdata;
  delete [] ydata;
}



//--------------- traps called from SLTableView --------------//
//--------------- traps called from SLTableView --------------//
//--------------- traps called from SLTableView --------------//


void HeaderDumpVectors::selectTrap(void    *data,
                                   long    /*column*/,
                                   Boolean /*selected*/)
{
  assert(data);
  HeaderDumpVectors *headvectors = (HeaderDumpVectors*)data;
  headvectors->updateSelVector();
}


void HeaderDumpVectors::locateTrap(void *data,
                                   long /*first*/, 
                                   long /*last*/, 
                                   long /*active*/)
{
  assert(data);
  HeaderDumpVectors *headvectors = (HeaderDumpVectors*)data;
  headvectors->updateLocVector();
}



//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
//--------------------------- end -------------------------------//
