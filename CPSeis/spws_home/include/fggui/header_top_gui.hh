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

//---------------------- header_top_gui.hh ---------------------------//
//---------------------- header_top_gui.hh ---------------------------//
//---------------------- header_top_gui.hh ---------------------------//

//             header file for the HeaderTopGui class
//               derived from the SLSmartForm class
//                       subdirectory fggui

#ifndef _HEADER_TOP_GUI_HH_
#define _HEADER_TOP_GUI_HH_

#include "sl/sl_smart_form.hh"


class HeaderTopGui : public SLSmartForm
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class SLRangeSelect   *_range;
  class SLpOption       *_show;
  class SLpOption       *_scroll;
  class SLpOption       *_more;
  class SLpToggle       *_round;
  class SLpPush         *_sort;
  class SL2Arrows       *_source;
  class FgActiveChoice  *_gather;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  HeaderTopGui(SLDelay *slparent, char *name, class FieldGeometry *fg);

  virtual ~HeaderTopGui();

  SLRangeSelect  *getRangeSelect    ()  const  { return _range; }
  SLpOption      *getShowOption     ()  const  { return _show; }
  SLpOption      *getScrollOption   ()  const  { return _scroll; }
  SLpOption      *getMoreOption     ()  const  { return _more; }
  SLpToggle      *getRoundToggle    ()  const  { return _round; }
  SLpPush        *getSortPush       ()  const  { return _sort; }
  SL2Arrows      *getSourceArrows   ()  const  { return _source; }
  FgActiveChoice *getGatherArrows   ()  const  { return _gather; }

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
