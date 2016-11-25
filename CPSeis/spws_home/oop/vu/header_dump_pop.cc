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

//---------------------- header_dump_pop.cc -----------------------//
//---------------------- header_dump_pop.cc -----------------------//
//---------------------- header_dump_pop.cc -----------------------//

//         implementation file for the HeaderDumpPop class
//                 derived from the SLDialog class
//                       subdirectory pick

#include "vu/header_dump_pop.hh"
#include "vu/header_dump_inform.hh"
#include "vu/header_dump_vectors.hh"
#include "vu/header_dump_pick.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_table_view.hh"
#include "sl/slp_push.hh"
#include <iostream.h>
#include <assert.h>

#define START_PICKING  "Begin Picking"
#define STOP_PICKING   " End Picking "

//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//


HeaderDumpPop::HeaderDumpPop(SLDelay *slparent, char *name,
                             SeisPlot *sp, HelpCtx hctx,
                             long nchar, long ndec,
                             long nrowmax, long ncolmax)
            : SLDialog(slparent, name, hctx),
                  _sp           (sp),
                  _headinform   (NULL),
                  _headvectors  (NULL),
                  _headpick     (NULL),
                  _view         (NULL),
                  _pickb        (NULL)
{
  constructorHelper(nchar, ndec, nrowmax, ncolmax);
}


HeaderDumpPop::HeaderDumpPop(Widget wparent, char *name,
                             SeisPlot *sp, HelpCtx hctx,
                             long nchar, long ndec,
                             long nrowmax, long ncolmax)
            : SLDialog(wparent, name, hctx),
                  _sp           (sp),
                  _headinform   (NULL),
                  _headvectors  (NULL),
                  _headpick     (NULL),
                  _view         (NULL),
                  _pickb        (NULL)
{
  constructorHelper(nchar, ndec, nrowmax, ncolmax);
}


HeaderDumpPop::~HeaderDumpPop()
{
  if(_headpick) delete _headpick;
  delete _headvectors;
  delete _headinform;
  delete _view;
}


//--------------------- constructor helper --------------------//
//--------------------- constructor helper --------------------//
//--------------------- constructor helper --------------------//

void HeaderDumpPop::constructorHelper(long nchar, long ndec,
                                      long nrowmax, long ncolmax)
{
  assert(_sp);
  _view = new SLTableView(this, "view", NULL,
                                   nchar, ndec, nrowmax, ncolmax);
  setWorkArea(_view);
           _pickb = addBottomPush("pickb", 0, pickTrap, this);
                    addBottomRemove();
                    addBottomHelp();
  _pickb->setLabel(START_PICKING);
  _headinform  = new HeaderDumpInform (_sp, this);
  //_headvectors = new HeaderDumpVectors(_sp, _view);
}



//------------------------- traps -----------------------------//
//------------------------- traps -----------------------------//
//------------------------- traps -----------------------------//

void HeaderDumpPop::pickTrap(void *data, long /*ident*/)
{
  HeaderDumpPop *headpop = (HeaderDumpPop*)data;
  char *labl = headpop->_pickb->label();
  if(strings_equal(labl, START_PICKING)) headpop->startPicking();
  else                                   headpop->stopPicking();
}



//--------------------- new and end display -------------------//
//--------------------- new and end display -------------------//
//--------------------- new and end display -------------------//

void HeaderDumpPop::newDisplay(SeisPlot *sp)
{
  if(sp && sp->imageIsDisplayed())
       {
       _sp = sp;
       if(!isManaged()) return;
       long nrows         = sp->numHeaders();
       long ncolumns      = sp->displayedTraces(sp->currentFrame());
       long offset        = sp->currentFrame() * sp->originalTraces();
       const float *array = sp->headers() + nrows * offset;
       long istart        = sp->firstTrace();
       long istop         = istart + sp->displayedTraces() - 1;
       _view->newTable(array, nrows, ncolumns, nrows, istart, istop);
       _headvectors->updateSelVector();
       _headvectors->updateLocVector();
       }
  else
       {
       if(!isManaged()) return;
       _view->emptyTable();
       }
}


void HeaderDumpPop::endDisplay(SeisPlot *)
{
  if(!isManaged()) return;
  _view->emptyTable();
}

void HeaderDumpPop::restartDisplay(SeisPlot *sp)
{
  _sp= sp;
  if(!isManaged()) return;

  Boolean i_am_picking= False;
  if (_headpick) i_am_picking= True;
  stopPicking();
  _headvectors->makeInvisible();
  if(_headvectors)  delete _headvectors;
  endDisplay(_sp);
  _headvectors = new HeaderDumpVectors(_sp, _view);
  _headvectors->makeVisible();
  newDisplay(_sp);
  if (i_am_picking) startPicking();
}


//------------------ start and stop picking ----------------//
//------------------ start and stop picking ----------------//
//------------------ start and stop picking ----------------//

void HeaderDumpPop::startPicking()
{
  // cout << "HeaderDumpPop::startPicking" << endl;
  if(_headpick) return;
  _headpick = new HeaderDumpPick(_sp, _headvectors, _view);
  _pickb->setLabel(STOP_PICKING);
}


void HeaderDumpPop::stopPicking()
{
  // cout << "HeaderDumpPop::stopPicking" << endl;
  if(!_headpick) return;
  delete _headpick;
  _headpick = NULL;
  _pickb->setLabel(START_PICKING);
}



//----------- post manage and unmanage notify ---------------//
//----------- post manage and unmanage notify ---------------//
//----------- post manage and unmanage notify ---------------//

void HeaderDumpPop::postManageNotify()
{
  if (!_headvectors) {
       _headvectors = new HeaderDumpVectors(_sp, _view);
       _headvectors->makeVisible();
       newDisplay(_sp);
  }
}


void HeaderDumpPop::postUnmanageNotify()
{
  stopPicking();
  assert(_headvectors);
  _headvectors->makeInvisible();
  delete _headvectors;
  _headvectors= NULL;
  endDisplay(_sp);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
