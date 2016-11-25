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

//------------------------ vfpop_pick.hh ----------------------------//
//------------------------ vfpop_pick.hh ----------------------------//
//------------------------ vfpop_pick.hh ----------------------------//

//                header file for the VfpopPick class
//                  derived from the SLDialog class
//                  derived from the VfInform class
//                        subdirectory vfgui


#ifndef _VFPOP_PICK_HH_
#define _VFPOP_PICK_HH_

#include "sl/sl_dialog.hh"
#include "vf/vf_inform.hh"
#include <X11/Intrinsic.h>


class VfDataset;


class VfpopPick : public SLDialog, public VfInform
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:     // also protected manager() in VfInform.

  class VfFunction *_velfun;    // copy of velocity function for resetting.
  int               _modified;  // whether active velocity function modified.


//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

           VfpopPick (SLDelay *slparent, char *name,
                      class VfManager     *manager,
                      class ContainerList *clist);
  virtual ~VfpopPick ();

  int  resetIsUseful()  const  { return (_modified != 0); }

public:   // overriding SLDialog.

  void  postManageNotify();
  void  postUnmanageNotify();
  void  resetNotify();


public:   // overriding VfInform.

  virtual void postNewActiveDataset ();
  virtual void postTotalChanges     (VfDataset *dataset);
  virtual void postRemoveInsertVelocityFunctions
                       (VfDataset *dataset, long ifun, long nrem, long nins);
  virtual void postNewActiveVelocityFunction (VfDataset *dataset);
  virtual void postChangeCoords     (VfDataset *dataset, long ifun, long nchng);
  virtual void postNewDefaultTypes  (VfDataset *dataset, long ifun, long nchng);
  virtual void postModifyStrings    (VfDataset *dataset, long ifun, long nchng);
  virtual void postModifyPicks
       (VfDataset *dataset, long ifun, int type, long ipick, long nrem,
                                                             long nins);

private:

  void  saveCopy();
  void  resetCopy();
  void  deleteCopy();
  

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
