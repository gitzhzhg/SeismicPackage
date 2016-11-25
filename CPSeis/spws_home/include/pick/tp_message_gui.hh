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

//----------------------- tp_message_gui.hh -------------------------//
//----------------------- tp_message_gui.hh -------------------------//
//----------------------- tp_message_gui.hh -------------------------//

//              header file for the TpMessageGui class
//                 derived from the SLpLabel class
//                        subdirectory pick

#ifndef _TP_MESSAGE_GUI_HH_
#define _TP_MESSAGE_GUI_HH_

#include "sl/slp_label.hh"


class TpMessageGui : public SLpLabel
{

//----------------------- data --------------------------------//
//----------------------- data --------------------------------//
//----------------------- data --------------------------------//

private:

  int _show;

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

public:    // constructor and destructor

  TpMessageGui(SLDelay *slparent, char *name);
  virtual ~TpMessageGui();

public:

  void showBlank   ();
  void showMessage (const char *msg);
  void showError   (const char *msg);

  void showActionMessage (long action, 
           long pickmode, long automode, long snap, char *post);

private:

  void           setColor           (int show);
  virtual Widget make               (Widget p);
  int            getPickModePrefix  (long pickmode, char *msg);
  int            addAutoModePortion (long automode, char *msg);
  int            addSnapPortion     (long snap    , char *msg);

//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//

};

#endif

//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
