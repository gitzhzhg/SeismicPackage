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

//------------------------ sl_how_read_write.hh ---------------------//
//------------------------ sl_how_read_write.hh ---------------------//
//------------------------ sl_how_read_write.hh ---------------------//

//            header file for the SLHowReadWrite class
//                derived from the SLDatabox class
//                         subdirectory sl

 
      // This class displays and updates various parameters needed
      // for reading and writing self defining non trace disk files.
      // Only those parameters known to the FIO primitive are displayed.


#ifndef _SL_HOW_READ_WRITE_HH_
#define _SL_HOW_READ_WRITE_HH_

#include "sl/sl_databox.hh"


class SLHowReadWrite  :  public SLDatabox
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class FileSupportInterface *_interface;   // pointer to external object.


//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:

           SLHowReadWrite(SLDelay *slparent,
                          FileSupportInterface *interface,
                          int nrows_init = -1);
  virtual ~SLHowReadWrite();

  FileSupportInterface *getInterface()  const  { return _interface; }

private:

  void makeHelper();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
