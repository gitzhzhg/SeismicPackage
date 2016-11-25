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

//---------------------- statics_read_gui.hh ---------------------------//
//---------------------- statics_read_gui.hh ---------------------------//
//---------------------- statics_read_gui.hh ---------------------------//

//             header file for the StaticsReadGui class
//               derived from the SLSmartForm class
//                       subdirectory fgqc


      //  Chooses the action to take when a statics file is read.


#ifndef STATICS_READ_GUI_HH
#define STATICS_READ_GUI_HH

#include "sl/sl_smart_form.hh"

class StaticsFile;

class StaticsReadGui : public SLSmartForm
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  StaticsFile *_file;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:
  StaticsReadGui				// Constructor
    (SLDelay *slparent,				//   SLDelay parent pointer
     char *name,				//   name to use for GUI
     StaticsFile *file);			//   statics file pointer

  virtual ~StaticsReadGui ();			// Destructor

  StaticsFile *getFile () const			// return the statics file ptr
    { return _file; }

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
