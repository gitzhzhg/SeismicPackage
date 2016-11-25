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

//------------------------ fg_miss_pop.hh ----------------------------//
//------------------------ fg_miss_pop.hh ----------------------------//
//------------------------ fg_miss_pop.hh ----------------------------//

//                header file for the FgMissPop class
//                  derived from the SLDialog class
//                  derived from the FgInform class
//                         subdirectory fggui


#ifndef _FG_MISS_POP_HH_
#define _FG_MISS_POP_HH_

#include "sl/sl_dialog.hh"
#include "geom/fg_inform.hh"
#include <X11/Intrinsic.h>


class FgMissPop : public SLDialog, public FgInform
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:           // also protected _fg in FgInform

  class FgMissTableGui *_table;
  int                   _updated;

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  FgMissPop(SLDelay *slparent, char *name, HelpCtx hctx,
               class FieldGeometry *fg,
               class FgControlPop  *dcp,
               class ContainerList *clist = NULL);
  virtual ~FgMissPop();

  FgMissTableGui *getFgMissTableGui ()            const { return _table; }
  int             getUpdateFlag     ()            const { return _updated; }
  void            setUpdateFlag     (int updated)       { _updated = updated; }

protected:     // overrides FgInform.

  virtual void postRemoveInsertFlags      (FieldGeometry*,
                             long ixl, long index, long nrem, long nins);

  virtual void postRemoveInsertLines      (FieldGeometry*,
                                        long index, long nrem, long nins);

  virtual void postRemoveInsertRpCards    (FieldGeometry*,
                                       long index, long nrem, long nins);

  virtual void postRemoveInsertPpCards    (FieldGeometry*,
                                       long index, long nrem, long nins);

  virtual void postRemoveInsertZt1Cards   (FieldGeometry*,
                                       long index, long nrem, long nins);

  virtual void postRemoveInsertZt2Cards   (FieldGeometry*,
                                       long index, long nrem, long nins);

  virtual void postRemoveInsertZt3Cards   (FieldGeometry*,
                                       long index, long nrem, long nins);

  virtual void postRemoveInsertZt4Cards   (FieldGeometry*,
                                       long index, long nrem, long nins);



  virtual void postResumeDependentUpdates (FieldGeometry*);

  virtual void dependentValuesOutOfDate   (FieldGeometry*);

  virtual void postReverseLineDirection   (FieldGeometry*, long ixl);

  virtual void postSortReceiverPatterns   (FieldGeometry*);



  virtual void postFlagValuesChanged      (FieldGeometry*,
                  long ixl, int ident, long index, long nrem, long nins);

  virtual void postLineNumbersChanged     (FieldGeometry*,
                                       long index, long nrem, long nins);

  virtual void postPpValuesChanged        (FieldGeometry*,
                      int ident, long index, long nrem, long nins);

  virtual void postRpValuesChanged        (FieldGeometry*,
                      int ident, long index, long nrem, long nins);

  virtual void postZt1ValuesChanged       (FieldGeometry*,
                      int ident, long index, long nrem, long nins);

  virtual void postZt2ValuesChanged       (FieldGeometry*,
                      int ident, long index, long nrem, long nins);

  virtual void postZt3ValuesChanged       (FieldGeometry*,
                      int ident, long index, long nrem, long nins);

  virtual void postZt4ValuesChanged       (FieldGeometry*,
                      int ident, long index, long nrem, long nins);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
