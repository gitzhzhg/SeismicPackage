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
// header_word_menu.hh

#ifndef HEADER_WORD_MENU_HH
#define HEADER_WORD_MENU_HH

#include "fgqc/fgqc_pop_sub_menu.hh"
#include "fgqc/fgqc_plot_constants.hh"

enum HeaderWord {HEADER_WORD_SELECTED};

class HeaderWordMenu : public FgQcPopSubMenu {

  public:
    HeaderWordMenu				// constructor
      (Widget p,				//   top level Widget parent
       char *name,				//   name of pop up
       HelpCtx hctx,				//   context sensitive help
       FgQcPop *fgqc_pop);			//   FgQc pop up menu

    virtual ~HeaderWordMenu ();			// destructor

    virtual void manage ();			// SL manage function

    virtual Widget make				// SL make function
      (Widget p);				//   top level Widget parent

    char *questionText ();			// return question Text

  protected:
    static void HeaderWLosingFocusAction	// static trap when focus leave
      (void *data,				//   object passed in
       long which);				//   selection not used

    static void HeaderAtAction			// header at on radio action
      (void *data,				//   object passed in
       long which);				//   selection not used

    int
      _header_word;				// selected header word number
    
    DataLocation
      _header_value_loc;			// where to plot header value

    class FgQcPop
      *_pop;					// parent popup for this menu

  private:
    class SLTextBox
      *_header_box;				// text box to select hdr wrd #

    class SLRadioBox
      *_headeratrbox;				// radio box to select hdr posn

};

#endif
