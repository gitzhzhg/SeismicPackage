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

//---------------------- fg_summary_gui.hh ---------------------------//
//---------------------- fg_summary_gui.hh ---------------------------//
//---------------------- fg_summary_gui.hh ---------------------------//

//             header file for the FgSummaryGui class
//               derived from the SLSmartForm class
//                       subdirectory fggui


      //  Displays a summary of the contents of a FieldGeometry
      //    object or the contents of a JD file.
      //  If _fg is not NULL, the information is automatically
      //    updated from FieldGeometry.
      //  If _jd is not NULL, the information is automatically
      //    updated from JdFile.
      //  The constructor asserts if  both   _fg and _jd are NULL.
      //  The constructor asserts if neither _fg nor _jd are NULL.


#ifndef _FG_SUMMARY_GUI_HH_
#define _FG_SUMMARY_GUI_HH_

#include "sl/sl_smart_form.hh"


class FgSummaryGui : public SLSmartForm
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class FieldGeometry *_fg;    // field geometry
  class JdFile        *_jdi;   // JD file for input
  class JdFile        *_jdo;   // JD file for output

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  FgSummaryGui(SLDelay *slparent, char *name, char *label,
                       FieldGeometry *fg, JdFile *jdi, JdFile *jdo);

  virtual ~FgSummaryGui();

  FieldGeometry *getFg       ()  const  { return _fg; }
  JdFile        *getJdInput  ()  const  { return _jdi; }
  JdFile        *getJdOutput ()  const  { return _jdo; }

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
