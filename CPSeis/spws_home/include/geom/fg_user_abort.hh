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

//---------------------- fg_user_abort.hh ---------------------------//
//---------------------- fg_user_abort.hh ---------------------------//
//---------------------- fg_user_abort.hh ---------------------------//

//              header file for the FgUserAbort class
//                   not derived from any class
//                       subdirectory geom

     // there is no implementation file for this class.


#ifndef _FG_USER_ABORT_HH_
#define _FG_USER_ABORT_HH_

#include "geom/fg_informer.hh"
#include <assert.h>


class FgUserAbort
{

  typedef int AbortFun (void *abort_data);

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class FgInformer *_informer;
  AbortFun         *_abort_fun;
  void             *_abort_data;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  FgUserAbort(FgInformer *informer)
           : 
                _informer (informer)
    {
    assert(_informer);
    }


  void registerAbortFun(AbortFun *abort_fun, void *abort_data)
    {
    _abort_fun  = abort_fun;
    _abort_data = abort_data;
    }


  void startAbortOption()
    {
    if(_abort_fun) _informer->sendMessage("start abort button", 0,0,0,0,0);
    }


  void stopAbortOption()
    {
    if(_abort_fun) _informer->sendMessage("stop abort button", 0,0,0,0,0);
    }


  int aborted()
    {
    return (_abort_fun && _abort_fun(_abort_data));
    }


//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
