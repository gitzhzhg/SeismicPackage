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

//------------------------ header_dump_pop.hh ---------------------//
//------------------------ header_dump_pop.hh ---------------------//
//------------------------ header_dump_pop.hh ---------------------//

//              header file for the HeaderDumpPop class
//                  derived from the SLDialog class
//                         subdirectory pick


#ifndef _SL_HEADER_DUMP_POP_HH_
#define _SL_HEADER_DUMP_POP_HH_

#include "sl/sl_dialog.hh"


class HeaderDumpPop : public SLDialog
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class SeisPlot           *_sp;
  class HeaderDumpInform   *_headinform;
  class HeaderDumpVectors  *_headvectors;
  class HeaderDumpPick     *_headpick;
  class SLTableView        *_view;
  class SLpPush            *_pickb;

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor

  HeaderDumpPop (SLDelay *slparent, char *name,
               class SeisPlot *sp, HelpCtx hctx = NULL,
               long nchar = 14, long ndec = 14,
               long nrowmax = 64, long ncolmax = 6);
  HeaderDumpPop (Widget    wparent, char *name,
               class SeisPlot *sp, HelpCtx hctx = NULL,
               long nchar = 14, long ndec = 14,
               long nrowmax = 64, long ncolmax = 6);
  virtual ~HeaderDumpPop();

private:

  void constructorHelper (long nchar, long ndec,
                          long nrowmax, long ncolmax);
 
  static void pickTrap(void *data, long ident);
  void  postManageNotify   ();     // overrides SLDialog
  void  postUnmanageNotify ();     // overrides SLDialog

public:

  void  newDisplay     (SeisPlot *sp);
  void  endDisplay     (SeisPlot *sp);
  void  restartDisplay (SeisPlot *sp);
  void  startPicking ();
  void  stopPicking  ();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
