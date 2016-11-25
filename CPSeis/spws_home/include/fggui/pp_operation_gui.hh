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

//------------------------ pp_operation_gui.hh ----------------------------//
//------------------------ pp_operation_gui.hh ----------------------------//
//------------------------ pp_operation_gui.hh ----------------------------//

//              header file for the PpOperationGui class
//                  derived from the SLSmartForm class
//                         subdirectory fggui

             // This class is used to choose an operation
             // to be performed on PP cards.

#ifndef _PP_OPERATION_GUI_HH_
#define _PP_OPERATION_GUI_HH_

#include "sl/sl_smart_form.hh"
#include <X11/Intrinsic.h>


class PpOperationGui : public SLSmartForm
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

public:

enum { DELETE_PP_CARDS = 1,  ADD_PP_CARDS };

private:

  class FieldGeometry *_fg; // pointer to field geometry.

  long   _operation;      // which operation to perform (enum).
  long   _delete_card1;   // card number of first card to delete.
  long   _delete_card2;   // card number of last  card to delete.
  long   _num_last_set;   // number of cards falling into last set.
  long   _num_new_sets;   // number of sets of cards to build.

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  PpOperationGui(SLDelay *slparent, char *name, HelpCtx hctx,
                                       class FieldGeometry *fg);
  virtual ~PpOperationGui();

  FieldGeometry *getFieldGeometry ()  const  { return _fg; }

  long  getOperation   ()  const  { return _operation; }

  long getDeleteCard1  ()  const  { return _delete_card1; }
  long getDeleteCard2  ()  const  { return _delete_card2; }
  long getNumLastSet   ()  const  { return _num_last_set; }
  long getNumNewSets   ()  const  { return _num_new_sets; }

  long numCardsToDelete ()  const;
  long numCardsToAdd    ()  const;

  void setDeleteCard1  (long delete_card1);
  void setDeleteCard2  (long delete_card2);
  void setNumLastSet   (long num_last_set);
  void setNumNewSets   (long num_new_sets);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
