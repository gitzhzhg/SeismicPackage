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


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include "sp/seis_all_header_pop.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_smart_form.hh"


enum {COLUMN1  =  1, COLUMN2 , COLUMN3 , COLUMN4  };
enum {COLUMN1A = 11, COLUMN2A, COLUMN3A, COLUMN4A };

SeisAllHeaderPop::SeisAllHeaderPop ( Widget    p,
                                     char      *name,
                                     HelpCtx   hctx,
                                     SeisPlot  *sp)
             : SLDialog(p, name, hctx, FALSE),
               SeisInform(sp), _num_headers(64)

{

  SLSmartForm *work   = workArea();
  _table = new SeisHeaderTable(work, name, this);

  work->attach(_table, work, work, work, work,  0,   0,  10,  10);

  _cancel_button     = addBottomCancel(1, NULL, this);

  setTitle("All Headers Readout");

}

SeisAllHeaderPop::~SeisAllHeaderPop()
{
  delete _table;
}


void SeisAllHeaderPop::addControl(SeisPlot *sp)
{
   addSeisPlot(sp);
   _num_headers = sp->currentSPInWindow()->numHeaders();
}


void SeisAllHeaderPop::removeControl(SeisPlot *sp)
{
   delSeisPlot(sp);
   _num_headers = sp->currentSPInWindow()->numHeaders();
}


void SeisAllHeaderPop::notCurrentInWindow(SeisPlot *sp)
{
  addSeisPlot(sp->currentSPInWindow());
  _num_headers = sp->currentSPInWindow()->numHeaders();
}


void SeisAllHeaderPop::managing()
{
}


void SeisAllHeaderPop::mouseOutputUpdate(SeisPlot *sp, float x, float)
{
  float value;


  _num_headers = sp->numHeaders();

  if (made() && XtIsManaged(W()) ) 
    {
    int trace_no= (int)sp->getTraceNumberFromXpixel((int)x) +
                  ( sp->currentFrame() * sp->originalTraces() );
    if ((x>-1.0) && (trace_no > 0)) 
      {
      for(int i= 0; (i<_num_headers); i++) 
        {
        value= sp->getHeaderFromTrace(trace_no, i+1);
        _table->setHeader(i, value);
        }   
      }
    else
      { //Use the following if we want to show 0 when mouse is off of image
        //for(int i= 0; (i<_num_headers); i++)
        // _table->setHeader(i, 0.0F); 
      }
    PrimSupport::updateEverything();
    }
}





//=============================================================================
//====  The SLDataBox that creates the table of header values               ===
//=============================================================================
SeisHeaderTable::SeisHeaderTable( SLDelay             *slparent,
                                  char                *name,
                                  SeisAllHeaderPop    *header_pop)
                : SLDatabox(slparent,name, NULL, 4, 1),
                  _header_pop(header_pop), _current_num_headers(64)
{
  _header_values = (float *)calloc( 1, 
                                 (int)(_current_num_headers * sizeof(float)));
}


SeisHeaderTable::~SeisHeaderTable()
{
  free(_header_values);
}


void SeisHeaderTable::clearHeaders()
{
  //Could use this to zero out the table values if requested at a later date.
}

void SeisHeaderTable::setHeader(int index, float val)
{
  if(_header_pop->getNumHeaders() != _current_num_headers ||
     index >= _current_num_headers                          )
        assert(reallocateHeaders(_header_pop->getNumHeaders()));
  
  _header_values[index] = val;
}

int SeisHeaderTable::reallocateHeaders(long num_headers)
{

  if(_current_num_headers != num_headers)
    _header_values = (float *)realloc(_header_values, 
                                    (int)num_headers * sizeof(float)); 

  if(_header_values != NULL)
    {
    _current_num_headers = num_headers;
    return 1;
    }
  else
    {
    printf("Error allocating header arrays in seis_all_header_pop.cc\n");
    _current_num_headers = 0;
    return 0;
    }
}



//=============================================================================
//======== Notify table of how many headers to make room for. =================
//=============================================================================
static long num_headers_update(void *data)
{
SeisHeaderTable *st = (SeisHeaderTable *)data;
long num_headers = st->_header_pop->getNumHeaders();


  if(st->reallocateHeaders(num_headers))
    return  (long)(((float)st->getCurrentNumHeaders() + 3.0F)/ 4.0F);
  else
    return 0;
}


static long iswitch_update(void *data, long ident, long index)
{
SeisHeaderTable *st = (SeisHeaderTable *)data;
long num_rows =  num_headers_update(data);
long index_offset = (ident - COLUMN1A) * num_rows + index; 
  
  if(index_offset < st->getCurrentNumHeaders())
    return -45;
  else
    return -77; //Blank out the field
}


static long dswitch_update(void *data, long ident, long index)
{
SeisHeaderTable *st = (SeisHeaderTable *)data;
long num_rows =  num_headers_update(data);
long index_offset = (ident - COLUMN1) * num_rows + index; 

 if(index >= num_rows)
    return -77;  //Blank out the field
 else if(index_offset < st->getCurrentNumHeaders())
    return 1;    //Bright non-enterable indented text appearance
    //return  5; //Bright non-enterable flat     text appearance
 else
    return -77;  //Blank out the field
}


static long ivalue_update(void *data, long ident, long index)
{
SeisHeaderTable *st = (SeisHeaderTable *)data;
long num_rows =  num_headers_update(data);
long index_offset = (ident - COLUMN1A) * num_rows + index; 
  
  if(index_offset < st->getCurrentNumHeaders())
       return index_offset + 1;
  else
       return 0;
}


static double dvalue_update(void *data, long ident, long index)
{
SeisHeaderTable *st = (SeisHeaderTable *)data;
long num_rows =  num_headers_update(data);
long index_offset = (ident - COLUMN1) * num_rows + index; 
  
  if(index_offset < st->getCurrentNumHeaders())
       return  st->_header_values[index_offset];
  else
       return 0.0;
}


void SeisHeaderTable::makeHelper()
{
static long zero =   0;
static long five =   5;
static long m45  = -45;



  //         NFunc              NMAXFunc           ROW COL NCHAR MAXROWS
  regArrays(num_headers_update, num_headers_update, 0,  0,   0,    46, 18);

  regIarray (COLUMN1A,  " ",  &zero,   &m45 ,  0,    3);
  regDarray (COLUMN1 ,  " ",  &zero,   &five,  0,   12, 99);
  regIarray (COLUMN2A,  " ",  &zero,   &m45 ,  0,    3);
  regDarray (COLUMN2 ,  " ",  &zero,   &five,  0,   12, 99);
  regIarray (COLUMN3A,  " ",  &zero,   &m45 ,  0,    3);
  regDarray (COLUMN3 ,  " ",  &zero,   &five,  0,   12, 99);
  regIarray (COLUMN4A,  " ",  &zero,   &m45 ,  0,    3);
  regDarray (COLUMN4 ,  " ",  &zero,   &five,  0,   12, 99);

  funIvar   (COLUMN1A, NULL, ivalue_update, iswitch_update);
  funIvar   (COLUMN2A, NULL, ivalue_update, iswitch_update);
  funIvar   (COLUMN3A, NULL, ivalue_update, iswitch_update);
  funIvar   (COLUMN4A, NULL, ivalue_update, iswitch_update);

  funDvar   (COLUMN1 , NULL, dvalue_update, dswitch_update);
  funDvar   (COLUMN2 , NULL, dvalue_update, dswitch_update);
  funDvar   (COLUMN3 , NULL, dvalue_update, dswitch_update);
  funDvar   (COLUMN4 , NULL, dvalue_update, dswitch_update);
}
