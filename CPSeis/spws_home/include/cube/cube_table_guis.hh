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
#ifndef CUBE_TABLE_GUIS_HH
#define CUBE_TABLE_GUIS_HH

#include "sl/sl_form_pop.hh"

class CubeTableShow;

class CubeTableShowGui : public SLFPopSep {

public:
  CubeTableShowGui				// constructor
    (Widget p,					//   Gui widget parent
     char *name,				//   name of table
     HelpCtx hctx);				//   context sensitive help

  CubeTableShowGui				// constructor
    (SLDelay *slp,				//   Gui widget parent
     char *name,				//   name of table
     HelpCtx hctx);				//   context sensitive help

  ~CubeTableShowGui ();				// destructor

  virtual Widget make				// make function
    (Widget p);					//   parent Widget

private:
  CubeTableShow
    *_table;					// table object

};






class CubeTableSelect;
class SLpRadio;
class RadioList;
class SLTextBox;
class Cube;

class CubeTableSelectGui : public SLFPopSep {

public:
  CubeTableSelectGui				// constructor
    (Widget p,					//   Gui widget parent
     char *name,				//   name of table
     HelpCtx hctx);				//   context sensitive help

  CubeTableSelectGui				// constructor
    (SLDelay *slp,				//   Gui widget parent
     char *name,				//   name of table
     HelpCtx hctx);				//   context sensitive help

  ~CubeTableSelectGui ();			// destructor

  virtual Widget make				// make function
    (Widget p);					//   parent Widget

  void displaySectionNumbers			// writes section #s
    (Cube *cube = 0);				//   given cube

protected:
  virtual void DoAction ();			// do after OK or APPLY

private:
  void constructorHelper ();			// called from constructor

  CubeTableSelect
    *_table;					// table object

  RadioList
    *_section_list;				// section type rlist

  SLpRadio
    *_line_rb,					// select a line section
    *_crossline_rb,				// select a crossline section
    *_horizon_slice_rb;				// select a horizon slice sctn

  Widget
    _line,					// current line section
    _crossline,					// current crossline section
    _horizon_slice;				// current horizon slice sectn

  long
    _section_state;				// section type state

};

#endif
