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


//------------------------ sl_file_selection_pop.hh ---------------------//
//------------------------ sl_file_selection_pop.hh ---------------------//
//------------------------ sl_file_selection_pop.hh ---------------------//

//              header file for the SLFileSelectionPop class
//                derived from the SLShellContainer class
//                            subdirectory sl

   //  This class is a question dialog box.  When popped up, it asks a
   //  question which takes a YES or NO answer.  When the YES or NO button
   //  is pressed, the dialog box automatically pops down.
   //  The specified name becomes the title of the popup.
   //  The virtual function answerYes is called if the YES button is pressed.
   //  The virtual function answerNo  is called if the NO  button is pressed.

   //      The popup is created ahead of time and can
   //      be used repeatedly.  It is popped up or down
   //      whenever makeAndManage() is called.
   //
   //      The popup can be registered with a
   //      pushbutton to be automatically managed or unmanaged
   //      when the pushbutton is pressed.
   //      The pushbutton does not have to have a
   //      trap registered with it.


#ifndef _SL_FILE_SELECTION_POP_HH_
#define _SL_FILE_SELECTION_POP_HH_

#include "sl/sl_shell_container.hh"

typedef void  SLFileSelectionAnswer (void *user_data, const char *filename);


class SLFileSelectionPop : public SLShellContainer
{

//------------------------ data --------------------------------//
//------------------------ data --------------------------------//
//------------------------ data --------------------------------//

private:

  char                   *_pattern;
  SLFileSelectionAnswer  *_yes_fun;
  void                   *_user_data;
  Widget                 _filebox;

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

public:  // constructor and destructor.

  SLFileSelectionPop (SLDelay *slparent, char *name,
                      const char            *pattern,
                      SLFileSelectionAnswer *yes_fun,
                      void                  *user_data);

  virtual ~SLFileSelectionPop();

  char *directory ();
  void setDirectory (char *directory);
  void setPattern (char *pattern = NULL);

private:

  virtual void   closing       () {}
  virtual Widget make          (Widget p = NULL);
  virtual void   manage        ();
  static void    yesCallback   (Widget w, XtPointer data, XtPointer call);
  static void    applyCallback (Widget w, XtPointer data, XtPointer call);
  static void    noCallback    (Widget w, XtPointer data, XtPointer call);

//------------------------ end of functions -------------------------//
//------------------------ end of functions -------------------------//
//------------------------ end of functions -------------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
