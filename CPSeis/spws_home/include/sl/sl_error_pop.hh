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


//------------------------ sl_error_pop.hh ---------------------//
//------------------------ sl_error_pop.hh ---------------------//
//------------------------ sl_error_pop.hh ---------------------//

//              header file for the SLErrorPop class
//            derived from the SLShellContainer class
//                        subdirectory sl

     //  This error box should be created just when it
     //  is needed to pop up an error message.
     //  It is made and popped up by the constructor.
     //  It deletes itself when it is popped down.
     //  The specified name becomes the title of the popup.
     //  The specified message cannot be NULL.

     //  Care should be taken to make sure that this popup
     //  does not disappear from the display before it is
     //  popped down by pressing its OK button.  See the
     //  implementation file for details.


#ifndef _SL_ERROR_POP_HH_
#define _SL_ERROR_POP_HH_

#include "sl/sl_shell_container.hh"


class SLErrorPop : public SLShellContainer
{

//------------------------ data --------------------------------//
//------------------------ data --------------------------------//
//------------------------ data --------------------------------//

private:       // no data

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

public:

  SLErrorPop (SLDelay *slparent, char *name, const char *msg);
  SLErrorPop (Widget    wparent, char *name, const char *msg);
  virtual ~SLErrorPop ();

private:

  void constructorHelper(const char *msg);
  virtual void closing     () {}
  Widget make(Widget p = NULL);

//------------------------ end of functions -------------------------//
//------------------------ end of functions -------------------------//
//------------------------ end of functions -------------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
