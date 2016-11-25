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


//----------------------- dfp_base.hh ------------------------------//
//----------------------- dfp_base.hh ------------------------------//
//----------------------- dfp_base.hh ------------------------------//

//               header file for the DFpBase class
//                 derived from the SLPrim class
//                      subdirectory sl

//    This class serves as a base class for all data fields
//       which are children of a SLDatabox.
//    These data fields are drawn on, and managed by, the SLDatabox.
//    These data fields are not widgets.
//    The parent SLDatabox must already have _hctx set.
//    Currently, the parent SLDatabox, and these data fields, are
//      wrappers around the windowbox utility.


#ifndef _DFP_BASE_HH_
#define _DFP_BASE_HH_

#include "sl/sl_prim.hh"


#define TRAP_ARGUMENTS   void *box, long *ident, long * /* index */,  \
                         char * /* text */, long *nread, char *endkey

class SLDatabox;

class DFpBase : public SLPrim
{

//---------------------- data -------------------------//
//---------------------- data -------------------------//
//---------------------- data -------------------------//

protected:

  long     _isw;       // windowbox             switch variable.
  long     _iswtrue;   // windowbox  sensitive  switch variable.
  long     _iswfalse;  // windowbox insensitive switch variable.
  long     _nchar;     // number of characters to display.
  long     _ndec;      // number of decimals to display.
  union
    {
    long     _ivalue;    // copy of   long variable.
    float    _fvalue;    // copy of  float variable.
    double   _dvalue;    // copy of double variable.
    char    *_cvalue;    // copy of   char variable (allocated).
    } _v;
  long     _nalloc;    // allocated length of _v._cvalue (not incl null).
  long     _irow;      // row number of data field.
  long     _icol;      // column number of start of data field.

//-------- functions needing replacement in derived classes --------//
//-------- functions needing replacement in derived classes --------//
//-------- functions needing replacement in derived classes --------//

protected:

  DFpBase (SLDatabox *databox, char *name, long ident, long type,
             long iswtrue, long iswfalse, long irow, long icol,
             long nchar, long ndec);

  virtual WidgetClass  topClass()         { return NULL; }
  virtual Boolean isWidgetBased()         { return FALSE; }
  virtual long   ivarResource(void) const { return _v._ivalue; }
  virtual float  fvarResource(void) const { return _v._fvalue; }
  virtual double dvarResource(void) const { return _v._dvalue; }
  virtual char  *cvarResource(void) const { return _v._cvalue; }

public:

  virtual ~DFpBase (void);

//------------------- other functions ------------------------//
//------------------- other functions ------------------------//
//------------------- other functions ------------------------//

public:   // some are inline

  virtual void    manageHelper  (void) {}
  virtual void    unmanageHelper(void) {}
  virtual Widget  make (Widget p = NULL);

private:   // set resources

  static void dfcallback (TRAP_ARGUMENTS);

  virtual void  setSenseResource (void);
  virtual void  setIvarResource  (void);
  virtual void  setFvarResource  (void);
  virtual void  setDvarResource  (void);
  virtual void  setCvarResource  (void);

//--------------------- end of functions ------------------------//
//--------------------- end of functions ------------------------//
//--------------------- end of functions ------------------------//

} ;

#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
