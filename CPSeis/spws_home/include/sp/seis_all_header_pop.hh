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
//************************ COPYRIGHT NOTICE ****************************
//      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
//       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
//************************ COPYRIGHT NOTICE ****************************



//************************************************************************
//***    Supports a popup that displays all of the header words.       ***
//***    This is a replacement for an old class that was too hardwired ***
//***    to support only 64 header words (need > 64 per new CPS now).  ***
//***    Author:Michael L. Sherrill 08/2001                            ***
//************************************************************************


#ifndef SEISALLHEADERPOP_HH
#define SEISALLHEADERPOP_HH

#include "sl/sl_dialog.hh"
#include "sl/sl_databox.hh"
#include "sp/seis_inform.hh"


class SeisPlot;


class SeisAllHeaderPop : public SLDialog, public SeisInform {

    public:
       SeisAllHeaderPop( Widget    p,
                         char      *name,
                         HelpCtx   hctx,
                         SeisPlot  *sp);
       virtual ~SeisAllHeaderPop();
       void addControl(SeisPlot *sp);
       void removeControl(SeisPlot *sp);
       virtual void notCurrentInWindow(SeisPlot *sp);
       virtual void mouseOutputUpdate(SeisPlot*, float /*x*/, float /*y*/);
       virtual void    managing();
       virtual  long getNumHeaders(){return _num_headers;}

    private:
       class SeisHeaderTable *_table;
       long _num_headers;
       SLpPush            *_cancel_button;

    
};


class SeisHeaderTable  :  public SLDatabox
{

  public:
    SeisHeaderTable( SLDelay      *slparent,
                   char         *name,
                   SeisAllHeaderPop *header_pop);
    virtual  ~SeisHeaderTable();
    virtual  void clearHeaders();
    virtual  void setHeader(int index, float val);
    SeisAllHeaderPop *_header_pop;
    float    *_header_values;
    virtual  int reallocateHeaders(long num_headers);
    virtual  const long getCurrentNumHeaders(){return _current_num_headers;}

  private:
    void     makeHelper();
    long     _current_num_headers;
    
};


#endif
