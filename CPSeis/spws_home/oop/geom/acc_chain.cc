
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
//---------------------- acc_chain.cc -----------------------//
//---------------------- acc_chain.cc -----------------------//
//---------------------- acc_chain.cc -----------------------//

//         implementation file for the AccChain class
//               derived from the AccBase class
//                     subdirectory geom

   /////////// note: DERR changed to DNIL everywhere 10/31/2001.


#include "geom/acc_chain.hh"
#include "geom/seis_line.hh"
#include "geom/field_flag.hh"
#include "geom/fg_constants.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>


   // This class resets values in array elements in the SeisLine
   // class.  The values referred to are the following:
   //
   //     incremental distance   (ident FG_DIST)
   //     X location             (ident FG_XLOC)
   //     Y location             (ident FG_YLOC)
   //     elevation              (ident FG_ELEV)
   //
   // The values changed are either the incremental distance or
   // the X location, depending on the chaining parameter.  The
   // changed values are flagged as dependent values.


   // This class calls the following functions in SmartArray (base
   // class for SeisLine):
   //
   //    void   valuesWillChange  (int ident, long index, long nrem, long nins)
   //    void   valuesHaveChanged (int ident, long index, long nrem, long nins)

   // This class does its own explicit calls to valuesWillChange and
   // valuesHaveChanged, rather than letting the base class do it,
   // because the ident has to be changed depending on the chaining
   // parameter.


   // This class calls the following functions in SeisLine (derived
   // from SmartArray):
   //
   //      long numFlagsOnLine  ()
   //    double getIncrDistance (long index)
   //    double getXloc         (long index)
   //    double getYloc         (long index)
   //    double getElevation    (long index)


   // The following functions must be called before and after a
   // value is changed, or after an array element is inserted/removed,
   // or after the direction of the array has been reversed, or
   // when the chaining parameter is changed, respectively:
   //
   //    void preChange              (long index, long nrem, long nins)
   //    void postChange             ()
   //    void notifyReverseDirection ()
   //    void setChaining            (int chaining)
   //
   // The pre/postChange functions must be called when values change
   // for any of the following idents:
   //    FG_DIST                          after _acc_dist is called.
   //    FG_XLOC                          after _acc_xloc is called.
   //    FG_YLOC                          after _acc_yloc is called.
   //    FG_ELEV (only if SLOPE_CHAINING) after _acc_elev is called.
   //



static const double DZERO       = 0.0;
static const int    DUMMY_IDENT = 0;


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


AccChain::AccChain(SeisLine *seis_line, int chaining)
           :  AccBase(seis_line, DUMMY_IDENT, FALSE),
                   _seis_line   (seis_line),
                   _chaining    (chaining)
{
  assert(_seis_line);
  assert(_chaining == HORIZONTAL_CHAINING ||
         _chaining ==      SLOPE_CHAINING ||
         _chaining ==         NO_CHAINING);
}


//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

AccChain::~AccChain()
{
}



//------------------ needs values --------------------------//
//------------------ needs values --------------------------//
//------------------ needs values --------------------------//

    // public
    // returns TRUE if the values corresponding to the specified
    //   ident are needed to calculate chaining.
    // otherwise returns FALSE.

int AccChain::needsValues(int ident)  const
{
  switch(ident)
      {
      case FG_DIST: if(_chaining != NO_CHAINING)    return TRUE; break;
      case FG_XLOC: if(_chaining == NO_CHAINING)    return TRUE; break;
      case FG_YLOC:                                 return TRUE;
      case FG_ELEV: if(_chaining == SLOPE_CHAINING) return TRUE; break;
      default: break;
      }
  return FALSE;
}



//------------------ changes values ------------------------//
//------------------ changes values ------------------------//
//------------------ changes values ------------------------//

    // public
    // returns TRUE if the values corresponding to the specified
    //   ident are changed by the chaining calculation.
    // otherwise returns FALSE.

int AccChain::changesValues(int ident)  const
{
  switch(ident)
      {
      case FG_DIST: if(_chaining == NO_CHAINING) return TRUE; break;
      case FG_XLOC: if(_chaining != NO_CHAINING) return TRUE; break;
      default: break;
      }
  return FALSE;
}



//--------------- notify reverse direction ----------------//
//--------------- notify reverse direction ----------------//
//--------------- notify reverse direction ----------------//

    //  public.
    //  This function must be called whenever the direction
    //  of the array elements has been reversed.

    //  Regardless of the _chaining parameter, the FG_DIST values
    //  are modified so that they are consistent with the
    //  corresponding FG_XLOC, FG_YLOC, and FG_ELEV values, which
    //  have been reversed.
    //  
    //  If FG_XLOC is the independent variable, then there is no
    //  problem if updates are frozen, since FG_DIST will be
    //  recalculated when updates are resumed.

    //  If FG_DIST is the independent variable, its first value
    //  (after reversal) may become inaccurate if updates are frozen,
    //  since that value is obtained from the first value of FG_XLOC
    //  (after reversal), which could be out of date.

    //  Therefore it would be best not to allow reversing the
    //  direction of the line while updates are frozen.  For
    //  this reason, this routine asserts if updates are frozen.

    //  This routine sets all dependency flags TRUE for the FG_DIST
    //  variables.  If FG_DIST is the independent variable, then the
    //  dependency flags must be reset to smarter TRUE/FALSE values
    //  after this routine does its work.


void AccChain::notifyReverseDirection()
{
  assert(!updatesFrozen());
  long n = _seis_line->numFlagsOnLine();
  _seis_line->valuesWillChange(FG_DIST, 0, n, n);
  for(long index = n-1; index >= 1; index--)
      {
      FieldFlag *fflag = _seis_line->unsafeFieldFlag(index);
      FieldFlag *fprev = _seis_line->unsafeFieldFlag(index-1);
      double value = fprev->getIncrDistance();
      fflag->setDependentIncrDistance(-value);
      }
  FieldFlag *fflag = _seis_line->unsafeFieldFlag(0);
  double value = fflag->getXloc();
  fflag->setDependentIncrDistance(value);
  _seis_line->valuesHaveChanged(FG_DIST, 0, n, n);
}
  


//--------------------- set chaining -----------------------//
//--------------------- set chaining -----------------------//
//--------------------- set chaining -----------------------//

    //  public.
    //  This function must be called whenever the chaining
    //     parameter is changed.
    //  This function asserts if updates are frozen.
    //  This function returns the new independent ident (FG_DIST
    //     or FG_XLOC) if the independent variable has changed;
    //     otherwise this function returns FG_NONE.

int AccChain::setChaining(int chaining)
{
  assert(chaining == HORIZONTAL_CHAINING ||
         chaining ==      SLOPE_CHAINING ||
         chaining ==         NO_CHAINING);

  assert(!updatesFrozen());

  int new_indep_ident = FG_NONE;
  if(_chaining == chaining) return new_indep_ident;
  if      ( chaining == NO_CHAINING) new_indep_ident = FG_XLOC;
  else if (_chaining == NO_CHAINING) new_indep_ident = FG_DIST;
  long n = _array->numElements();
  if(_chaining == SLOPE_CHAINING && chaining == NO_CHAINING)
      {
      _chaining = HORIZONTAL_CHAINING;
      preChange (0, n, n);
      postChange();
      }
  else if(_chaining == NO_CHAINING && chaining == SLOPE_CHAINING)
      {
      _chaining = HORIZONTAL_CHAINING;
      preChange (0, n, n);
      postChange();
      }
  _chaining = chaining;
  preChange (0, n, n);
  postChange();
  return new_indep_ident;
}



//--------------------- pre and post change2 ----------------------//
//--------------------- pre and post change2 ----------------------//
//--------------------- pre and post change2 ----------------------//

  // public.
  // needed since this class told its base class not to send
  //   value-changed messages.
  // these functions are to be called before and after values are
  //   changed or remove/insert happens.
  // only the dependent values are reported.
  // the independent values are reported by their respective classes.

void AccChain::preChange2(long /*index*/, long nrem, long nins)
{
  long n = _array->numElements();
  long index2 = 0;
  long nrem2 = n;
  long nins2 = n - nrem + nins;
  if(_chaining == NO_CHAINING)
      {
      _seis_line->valuesWillChange(FG_DIST  , index2, nrem2, nins2);
      }
  else
      {
      _seis_line->valuesWillChange(FG_COORDS, index2, nrem2, nins2);
      _seis_line->valuesWillChange(FG_XLOC  , index2, nrem2, nins2);
      }
}



void AccChain::postChange2(long /*index*/, long nrem, long nins)
{
  long n = _array->numElements();
  long index2 = 0;
  long nrem2 = n - nins + nrem;
  long nins2 = n;
  if(_chaining == NO_CHAINING)
      {
      _seis_line->valuesHaveChanged(FG_DIST  , index2, nrem2, nins2);
      }
  else
      {
      _seis_line->valuesHaveChanged(FG_COORDS, index2, nrem2, nins2);
      _seis_line->valuesHaveChanged(FG_XLOC  , index2, nrem2, nins2);
      }
}



//---------------------- get range ----------------------------//
//---------------------- get range ----------------------------//
//---------------------- get range ----------------------------//

    // private virtual function.

void AccChain::getRange(long n, long *index1, long *index2)
{
  *index1 = 0;
  *index2 = n - 1;
}



//---------------------- fix range ----------------------------//
//---------------------- fix range ----------------------------//
//---------------------- fix range ----------------------------//

    // private virtual function.
    // Updates the appropriate value in each array element,
    //   by replacing it with a calculated value.

void AccChain::fixRange(long n, long /*index1*/, long /*index2*/)
{
  if(n == 0) return;
  switch(_chaining)
      {
      case HORIZONTAL_CHAINING: horizontalChainingUpdate(n); break;
      case      SLOPE_CHAINING:      slopeChainingUpdate(n); break;
      case         NO_CHAINING:         noChainingUpdate(n); break;
      default:  assert(FALSE);
      }
}



//------------------------ horizontal chaining update ------------------//
//------------------------ horizontal chaining update ------------------//
//------------------------ horizontal chaining update ------------------//

            // private.
            // sets xloc from dist and yloc.
            // n is guaranteed to be greater than zero.

void AccChain::horizontalChainingUpdate(long n)
{
  FieldFlag *fflag = _seis_line->unsafeFieldFlag(0);
  double dist = fflag->getIncrDistance();
  fflag->setDependentXloc(dist);
  for(long index = 1; index < n; index++)
      {
      FieldFlag *fflag = _seis_line->unsafeFieldFlag(index);
      FieldFlag *fprev = _seis_line->unsafeFieldFlag(index-1);
      double xloc;
      double dist      = fflag->getIncrDistance();
      double yloc      = fflag->getYloc        ();
      double xloc_prev = fprev->getXloc        ();
      double yloc_prev = fprev->getYloc        ();
/*
      if(dist      == DNIL || yloc      == DNIL || 
         xloc_prev == DNIL || yloc_prev == DNIL ||
         dist      == DERR || yloc      == DERR || 
         xloc_prev == DERR || yloc_prev == DERR)
            {
            xloc = DERR;
            }
*/
      if(dist      == DNIL || yloc      == DNIL || 
         xloc_prev == DNIL || yloc_prev == DNIL)
            {
            xloc = DNIL;
            }
      else
            {
            double ydif = yloc - yloc_prev;
            double xdif2 = dist * dist - ydif * ydif;
            if(xdif2 < DZERO)
                {
/*
                xloc = DERR;
*/
                xloc = DNIL;
                }
            else
                {
                double xdif = sqrt(xdif2);
                if(dist < DZERO) xdif = -xdif;
                xloc = xloc_prev + xdif;
                }
            }
      fflag->setDependentXloc(xloc);
      }
}



//------------------------------ slope chaining update ------------------//
//------------------------------ slope chaining update ------------------//
//------------------------------ slope chaining update ------------------//

            // private.
            // sets xloc from dist and yloc and elev.
            // n is guaranteed to be greater than zero.

void AccChain::slopeChainingUpdate(long n)
{
  FieldFlag *fflag = _seis_line->unsafeFieldFlag(0);
  double dist = fflag->getIncrDistance();
  fflag->setDependentXloc(dist);
  for(long index = 1; index < n; index++)
      {
      FieldFlag *fflag = _seis_line->unsafeFieldFlag(index);
      FieldFlag *fprev = _seis_line->unsafeFieldFlag(index-1);
      double xloc;
      double dist      = fflag->getIncrDistance();
      double yloc      = fflag->getYloc        ();
      double elev      = fflag->getElevation   ();
      double xloc_prev = fprev->getXloc        ();
      double yloc_prev = fprev->getYloc        ();
      double elev_prev = fprev->getElevation   ();
      if(dist      == DNIL || yloc      == DNIL || 
         elev      == DNIL || xloc_prev == DNIL ||
         yloc_prev == DNIL || elev_prev == DNIL)
            {
            xloc = DNIL;
            }
/*
      if(dist      == DNIL || yloc      == DNIL || 
         elev      == DNIL || xloc_prev == DNIL ||
         yloc_prev == DNIL || elev_prev == DNIL ||
         dist      == DERR || yloc      == DERR || 
         elev      == DERR || xloc_prev == DERR ||
         yloc_prev == DERR || elev_prev == DERR)
            {
            xloc = DERR;
            }
*/
      else
            {
            double ydif = yloc - yloc_prev;
            double edif = elev - elev_prev;
            double xdif2 = dist * dist - ydif * ydif - edif * edif;
            if(xdif2 < DZERO)
                {
/*
                xloc = DERR;
*/
                xloc = DNIL;
                }
            else
                {
                double xdif = sqrt(xdif2);
                if(dist < DZERO) xdif = -xdif;  // testing the idea
                xloc = xloc_prev + xdif;
                }
            }
      fflag->setDependentXloc(xloc);
      }
}



//------------------------------ no chaining update ------------------//
//------------------------------ no chaining update ------------------//
//------------------------------ no chaining update ------------------//

            // private.
            // sets distance from xloc and yloc.
            // n is guaranteed to be greater than zero.

void AccChain::noChainingUpdate(long n)
{
  FieldFlag *fflag = _seis_line->unsafeFieldFlag(0);
  double xloc = fflag->getXloc();
  fflag->setDependentIncrDistance(xloc);
  if(n > 1)
      {
      double dist;
      for(long index = 1; index < n; index++)
          {
          FieldFlag *fflag = _seis_line->unsafeFieldFlag(index);
          FieldFlag *fprev = _seis_line->unsafeFieldFlag(index-1);
          double xloc      = fflag->getXloc();
          double yloc      = fflag->getYloc();
          double xloc_prev = fprev->getXloc();
          double yloc_prev = fprev->getYloc();
/*
          if(xloc      == DNIL || yloc      == DNIL || 
             xloc_prev == DNIL || yloc_prev == DNIL ||
             xloc      == DERR || yloc      == DERR || 
             xloc_prev == DERR || yloc_prev == DERR)
                {
                dist = DERR;
                }
*/
          if(xloc      == DNIL || yloc      == DNIL || 
             xloc_prev == DNIL || yloc_prev == DNIL)
                {
                dist = DNIL;
                }
          else
                {
                double xdist = xloc - xloc_prev;
                double ydist = yloc - yloc_prev;
                dist = sqrt(xdist * xdist + ydist * ydist);
                if(xdist < DZERO) dist = -dist;  // testing the idea
                }
          fflag->setDependentIncrDistance(dist);
          }
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

