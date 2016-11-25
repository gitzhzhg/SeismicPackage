
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
//---------------------- rp_top_gui.cc ------------------------//
//---------------------- rp_top_gui.cc ------------------------//
//---------------------- rp_top_gui.cc ------------------------//

//           implementation file for the RpTopGui class
//               derived from the SLSmartForm class
//                        subdirectory fggui


#include "fggui/rp_top_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_text.hh"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//

static void find_trap(void *data, long /*ident*/, long /*oldvar*/,
                                                            long newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long ixrp = fg->findReceiverPattern(newvar);
  if(ixrp >= 0) fg->setActiveRpCardIndex(ixrp);
  else          fg->ringBell();
}


static void end_trap(void *data, long /*ident*/, long /*oldvar*/,
                                                            long newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long ixrp = fg->findEndOfReceiverPattern(newvar);
  if(ixrp >= 0) fg->setActiveRpCardIndex(ixrp);
  else          fg->ringBell();
}


static void chan_trap(void *data, long /*ident*/, long /*oldvar*/,
                                                            long newvar)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long ixrp = fg->getActiveRpCardIndex();
  if(ixrp == -1) { fg->ringBell(); return; }
  long pattern = fg->getRpPatternNumber(ixrp);
  ixrp = fg->findRpCardWithDesiredChannel(pattern, newvar);
  if(ixrp >= 0) fg->setActiveRpCardIndex(ixrp);
  else          fg->ringBell();
}



//------------- update functions ----------------------------//
//------------- update functions ----------------------------//
//------------- update functions ----------------------------//

static long find_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  long ixrp = fg->getActiveRpCardIndex();
  if(ixrp == -1) return 0;
  return fg->getRpPatternNumber(ixrp);
}

 
static char *sort_update(void *data)
{
  static char blank[] = " ";
  static char msg  [] = "RECEIVER PATTERNS ARE NOT SORTED";
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->receiverPatternsAreSorted()) return blank;
  return msg;
}

 
 

//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


RpTopGui::RpTopGui(SLDelay *slparent, char *name, FieldGeometry *fg)
       : SLSmartForm(slparent, name, NULL, TRUE)
{
  assert(fg);

  SL2Text   *find = new SL2Text (this, "find", 0,
                   "find receiver pattern:",        SLpText::_LONG, 6);
  SL2Text   *end  = new SL2Text (this, "end" , 0,
                   "find end of receiver pattern:", SLpText::_LONG, 6);
  SL2Text   *chan = new SL2Text (this, "chan", 0,
                   "find channel number:",          SLpText::_LONG, 6);

  SLpText   *sort = new SLpText (this, "sort");

//             left  right  top   bottom

  attach(find, NULL, this,  this, NULL);
  attach(end , NULL, this,  find, NULL);
  attach(chan, NULL, this,  end , this);
  attach(sort, this, find,  this, NULL);

  sort->showLabelAppearance();
  sort->setupCvarFun (sort_update, fg);
  find->setupIvarFun (find_update, fg);
  end ->setupIvarFun (find_update, fg);
  find->setItrap     (find_trap,   fg);
  end ->setItrap     ( end_trap,   fg);
  chan->setItrap     (chan_trap,   fg);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


RpTopGui::~RpTopGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

