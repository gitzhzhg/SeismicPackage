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
#include "plot/pick_base.hh"
#include "vu/pick_label.hh"

#include "vu/seis_label.hh"
#include "sp/seis_plot.hh"

static char *mode = "Mode: Add Label";
static const char * const help_token = "LABELPICK";
static const char * const help_fallback = 
 "mouse*LABELPICK:  BTN#1: Insert/Move Label,  BTN#2: Delete Label,\
  BTN#3: Popup Menu \\nShift BTN#1: Modify Label";


PickLabel::PickLabel( PlotBase   *plot, SeisLabel  *label) :
          PickBase(plot, mode, help_token, help_fallback), _label(label)
{
}

PickLabel::~PickLabel()
{
}

void PickLabel::noModButtonOnePress(int x, int y)
{
  _label->doInsertLabel(this,x,y,False);
}

void PickLabel::noModButtonOneMotion(int, int x2, int, int y2)
{
  _label->doMoveLabel(this,x2,y2);
}

void PickLabel::noModButtonOneRelease(int, int x2, int, int y2)
{
  _label->doFinishLabel(this,x2,y2);
}

void PickLabel::shiftButtonOnePress(int x, int y)
{
  _label->doInsertLabel(this,x,y,True);
}

void PickLabel::shiftButtonOneMotion(int, int x2, int, int y2)
{
  _label->doMoveLabel(this,x2,y2);
}

void PickLabel::shiftButtonOneRelease(int, int x2, int, int y2)
{
  _label->doFinishLabel(this,x2,y2);
}

void PickLabel::noModButtonTwoPress(int x, int y)
{
  _label->doDeleteLabel(this,x,y);
}

void PickLabel::noModButtonTwoRelease(int, int, int, int)
{
  puts("noModButtonTwoRelease");
}
