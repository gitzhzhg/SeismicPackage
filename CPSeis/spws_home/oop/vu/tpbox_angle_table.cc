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

//---------------------- tpbox_angle_table.cc -----------------------//
//---------------------- tpbox_angle_table.cc -----------------------//
//---------------------- tpbox_angle_table.cc -----------------------//

//          implementation file for the TpboxAngleTable class
//                   derived from the SLDatabox class
//                        subdirectory pick


#include "vu/tpbox_angle_table.hh"
#include "vu/seis_avast.hh"
#include "sl/sl_prim.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


enum { SELECT = 1, ANGLE };


#define TABLE  TpboxAngleTable *table = (TpboxAngleTable*)data;


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


TpboxAngleTable::TpboxAngleTable(SLDelay *slparent, SeisAvast *so)
           : SLDatabox(slparent, "tpbox_angle_table", NULL, 4, TRUE),
                     _so        (so),
                     _nangles   (0),
                     _angles    (NULL),
                     _select    (NULL)
{
  assert(_so);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//


TpboxAngleTable::~TpboxAngleTable()
{
  if(_angles) delete [] _angles;
  if(_select) delete [] _select;
}



//--------------------- get values --------------------------------//
//--------------------- get values --------------------------------//
//--------------------- get values --------------------------------//

     // public.

float TpboxAngleTable::getAngle(int index)  const
{
  assert(index >= 0 && index < _nangles);
  return _angles[index];
}


int TpboxAngleTable::isSelected(int index)  const
{
  assert(index >= 0 && index < _nangles);
  return _select[index];
}



//--------------------- set values --------------------------------//
//--------------------- set values --------------------------------//
//--------------------- set values --------------------------------//

     // public.

void TpboxAngleTable::setAngles(int nangles, float *angles)
{
  assert(nangles >= 0 && angles);
  if(_angles) delete [] _angles;
  if(_select) delete [] _select;
  _nangles = nangles;
  if(_nangles == 0) return;
  _angles = new float [_nangles];
  _select = new int   [_nangles];
  for(int index = 0; index < _nangles; index++)
      {
      _angles[index] = angles[index];
      _select[index] = TRUE;
      }
  _so->supplyInfo(_nangles, _angles, _select);
}


void TpboxAngleTable::setSelection(int index, int select)
{
  assert(index >= 0 && index < _nangles);
  _select[index] = select ;
  _so->supplyInfo(_nangles, _angles, _select);
}


void TpboxAngleTable::toggleSelection(int index)
{
  assert(index >= 0 && index < _nangles);
  _select[index] = !_select[index];
  _so->supplyInfo(_nangles, _angles, _select);
}


void TpboxAngleTable::toggleSelections()
{
  if(_nangles == 0) return;
  int select = !_select[0];
  for(int index = 0; index < _nangles; index++)
      {
      _select[index] = select;
      }
  _so->supplyInfo(_nangles, _angles, _select);
}



//---------------------- trap functions --------------------------//
//---------------------- trap functions --------------------------//
//---------------------- trap functions --------------------------//


static void prompt_trap(void *data, long /*ident*/, long /*index*/,
                            char* /*value*/, long /*nread*/, char* endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  TABLE
  table->toggleSelections();
}


static void select_trap(void *data, long /*ident*/, long index,
                            long /*value*/, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  table->toggleSelection((int)index);
}



//------------------------- update functions -------------------------//
//------------------------- update functions -------------------------//
//------------------------- update functions -------------------------//


static long select_update(void *data, long /*ident*/, long index)
{
  TABLE
  return table->isSelected((int)index);
}


static float angle_update(void *data, long /*ident*/, long index)
{
  TABLE
  return table->getAngle((int)index);
}


static long n_update(void *data)
{
  TABLE
  return table->numAngles();
}



//---------------------- switch functions --------------------------//
//---------------------- switch functions --------------------------//
//---------------------- switch functions --------------------------//


//--------------------------- make helper -----------------------------//
//--------------------------- make helper -----------------------------//
//--------------------------- make helper -----------------------------//

void TpboxAngleTable::makeHelper()
{
  static long zero  =   0; 
  static long two   =   2; 
  static long three =   3; 
  static long five  =   5; 

    //        N        NMAX    ROW COL NCHAR MAXROWS
  regArrays(n_update, n_update, 0,  0,   2,    20);

    //        ID      PROMPT          JSW      ISW     COL NCHAR NDEC
  regIarray (SELECT, "show"        , &two  ,  &three,   0,    2);
  regFarray (ANGLE , "angle"       , &zero ,  &five ,   0,    5,  2);

  funCvar  (-SELECT,      prompt_trap);
  funIvar  ( SELECT,      select_trap, select_update);
  funFvar  ( ANGLE ,             NULL,  angle_update);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
