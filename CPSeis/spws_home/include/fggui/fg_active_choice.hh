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

//------------------------ fg_active_choice.hh ---------------------//
//------------------------ fg_active_choice.hh ---------------------//
//------------------------ fg_active_choice.hh ---------------------//

//              header file for the FgActiveChoice class
//                 derived from the SL2Arrows class
//                         subdirectory fggui

     // Allows user to choose an active item in FieldGeometry.

 
#ifndef _FG_ACTIVE_CHOICE_HH_
#define _FG_ACTIVE_CHOICE_HH_

#include "sl/sl2_arrows.hh"


class FgActiveChoice  :  public SL2Arrows
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

public:

  enum ActiveChoice
           { TRACE_PLUS_S = 1, TRACE_PLUS_R,
             TRACE_PLUS_PP, TRACE,
             GROUP_PLUS_PP, GROUP,
             FLAG, LINE, RPCARD, PPCARD, CMP };

private:

  class FieldGeometry  *_fg;
  ActiveChoice          _choice;
  char                 *_label;

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor.

  FgActiveChoice (SLDelay *slparent, char *name,
                  class FieldGeometry *fg,
                  ActiveChoice choice = TRACE,
                  const char  *label  = NULL);
  virtual ~FgActiveChoice();

  FieldGeometry *getFieldGeometry  ()    const  { return _fg; }
  ActiveChoice   getActiveChoice   ()    const  { return _choice; }

  void      setActiveChoice (ActiveChoice choice,
                             const char *label = NULL);

private:

  static void  num1Trap    (void *data, long newvar);
  static long  num1Update  (void *data);
  static long  num2Update  (void *data);
  static char *labelUpdate (void *data);
//static long  senseUpdate (void *data);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
