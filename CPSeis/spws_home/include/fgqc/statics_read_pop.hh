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

//------------------------ statics_read_pop.hh ----------------------------//
//------------------------ statics_read_pop.hh ----------------------------//
//------------------------ statics_read_pop.hh ----------------------------//

//                header file for the StaticsReadPop class
//                  derived from the SLDialog class
//                         subdirectory fgqc


#ifndef STATICS_READ_POP_HH
#define STATICS_READ_POP_HH

#include "sl/sl_dialog.hh"
#include "oprim/file_base.hh"
#include <X11/Intrinsic.h>


class StaticsReadPop : public SLDialog
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class FieldGeometry *_fg;			// pointer to FieldGeometry
  class StaticsFile   *_file;			// owned by this class.
  class SLFileChoice  *_choice;			// owned by this class.
  Boolean             _use_srg;			// TRUE for smooth/insert gui
     
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  StaticsReadPop				// Constructor
    (SLDelay *slparent,				//   pointer to SLDelay parent
     char *name,				//   name of dialog box
     HelpCtx hctx,				//   context sensitive help obj
     class FieldGeometry *fg,			//   FieldGeometry ptr
     Boolean use_srg = FALSE);			//   TRUE for smooth/insert gui

  StaticsReadPop				// Constructor
    (Widget wparent,				//   pointer to widget parent
     char *name,				//   name of dialog box
     HelpCtx hctx,				//   context sensitive help obj
     class FieldGeometry *fg,			//   FieldGeometry ptr
     Boolean use_srg = FALSE);			//   TRUE for smooth/insert gui

  void constructorHelper ();			// Constructor helper

  virtual ~StaticsReadPop();

  virtual Widget make (Widget p = 0);		// make

  StaticsFile *getFile () const			// return the statics file ptr
    { return _file; }

  SLFileChoice *getFileChoice () const		// return file choice pointer
    { return _choice; }

private:     					// these override SLDialog.

  virtual Boolean preManageNotify ();

  virtual Boolean okNotify ();

  virtual void    applyNotify ();

  virtual Boolean cancelNotify ();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
