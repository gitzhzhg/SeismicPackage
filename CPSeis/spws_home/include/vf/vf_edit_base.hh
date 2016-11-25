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

//------------------------ vf_edit_base.hh ----------------------------//
//------------------------ vf_edit_base.hh ----------------------------//
//------------------------ vf_edit_base.hh ----------------------------//

//               header file for the VfEditBase class
//                   not derived from any class
//                         subdirectory vf

//    This is a base class for editing velocity data.

//    The derived classes must be instantiated by a user (outside of
//    the VfManager and VfDataset classes).  The derived classes must
//    contain all parameters and algorithms for editing VfKernal (the
//    guts of the velocity dataset).

//    A pointer to the derived class should be passed to the editDataset
//    function in VfDataset.  VfDataset will then perform all necessary
//    informs (as specified by the "informStyle" virtual function), will
//    display the appropriate working message (as specified by the
//    "workingMessage" virtual function), perform necessary undo and
//    backup procedures, and will call the "edit" virtual function to
//    actually perform the edit of VfKernal.


#ifndef _VF_EDIT_BASE_HH_
#define _VF_EDIT_BASE_HH_

#include <assert.h>

class VfEditBase
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

public:

  enum { INFORM_TOTAL,       // call pre/postTotalChanges.
         INFORM_STRINGS,     // call pre/postModifyStrings.
         INFORM_TYPE         // call pre/postNewDefaultTypes.
       };

  enum { CHOICE_SELECTED = 1,  // process selected velocity functions.
         CHOICE_ALL      = 2,  // process all velocity functions.
         CHOICE_ACTIVE   = 3,  // process active velocity function.
         CHOICE_BLANK    = 4,  // process velocity functions with blank names.
         CHOICE_EMPTY    = 5   // process empty velocity functions.
       };

private:   // (_choice might not be used by all derived classes)
           // (CHOICE_BLANK is not used by most derived classes)
           // (CHOICE_EMPTY is not used by most derived classes)

  const int _style;      // which style to use for informing    (enum).
  int       _choice;     // which velocity functions to process (enum).
  char     *_verb;       // verb which describes the process.


//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//

public:

           VfEditBase (int style, const char *verb = "process");
  virtual ~VfEditBase ();

  int         informStyle   ()  const    { return _style; }
  int         getChoice     ()  const    { return _choice; }
  const char *getVerb       ()  const    { return _verb; }
  void        setChoice     (int choice);

protected: // to be called by derived classes.
           // returns TRUE if a function should be rejected for processing.
           // also shows message of progress unless gerund is NULL.
           // uses the _choice variable.

  int  reject (class VfKernal *kernal, long ifun,
                                         const char *gerund = "processing");

public:   // to be called from VfDataset.
          // the VfKernal argument keeps users from calling the edit function,
          //   since users do not have access to the VfKernal object.
          // checkForErrors:
          //   called by VfDataset prior to calling editKernal.
          //   returns TRUE and an error message if errors are found.
          //   returns FALSE and a working message otherwise.
          // editKernal:
          //   called by VfDataset only if checkForErrors returns FALSE.
          //   returns TRUE and an error message if errors are found.
          //   returns FALSE and a success message otherwise.

  int  checkForErrors (class VfKernal *kernal, char *msg);
  int  editKernal     (class VfKernal *kernal, char *msg);

protected: // virtual functions to be overriden by derived class.
           // called by checkForErrors and editKernal respectively.
           // checkForErrors returns FALSE and "working..." if not overridden.
           // editKernal returns TRUE and "not implemented" if not overridden.

  virtual int virtualCheck (class VfKernal *kernal, char *msg);
  virtual int virtualEdit  (class VfKernal *kernal, char *msg);


//  Note1: Before calling virtualCheck, checkForErrors returns TRUE if
//  there are no velocity functions to process.

//  Note2: If virtualCheck and/or virtualEdit do not reset msg, an
//  appropriate message is applied based on the returned error flag.


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
