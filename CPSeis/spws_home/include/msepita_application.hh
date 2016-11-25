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

//-------------------------- msepita_application.hh --------------------------//
//-------------------------- msepita_application.hh --------------------------//
//-------------------------- msepita_application.hh --------------------------//

//            header file for the MsepitaApplication class
//                  derived from the SLApp class
//                   subdirectory ~spws/msepita
//                   subdirectory ~tom/msepita


#ifndef _MSEPITA_APPLICATION_HH_
#define _MSEPITA_APPLICATION_HH_

#include "sl/sl_app.hh"


class MsepitaApplication : public SLApp
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class StaticManager         *_manager;
  class ContainerList         *_clist;
  class SLPullPop             *_file;
  class SLPullPop             *_option;
  char                        *_helpfile;  // custom resource.
  class StatguiQuit           *_quit;
  class RegulateStatgui       *_regulate;

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:  // constructor and destructor.

           MsepitaApplication (int argc, char **argv);
  virtual ~MsepitaApplication ();

public:  // these override SLApp or SLDelay.

  virtual void    closing       ();
  virtual Boolean notifyComplex (SLDelay *sender, int ident);

private:

  void  customResources ();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
