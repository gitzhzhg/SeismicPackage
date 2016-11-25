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

//---------------------- simple_pick.cc -----------------------//
//---------------------- simple_pick.cc -----------------------//
//---------------------- simple_pick.cc -----------------------//

//         implementation file for the SimplePick class
//              derived from the PickBase class
//                      subdirectory pick


#include "pick/simple_pick.hh"


//------------------- constructor and destructor -----------------//
//------------------- constructor and destructor -----------------//
//------------------- constructor and destructor -----------------//

SimplePick::SimplePick(PlotBase *plot, SimplePickTrap *pick_trap,
                             void           *pick_data,
                             SecondPress     secondPress,
                             Bool            stickyModifier)
        : PickBase(plot,
		"simple picking", NULL, NULL,	/* ehs 27jun94 */
		XC_tcross, secondPress, stickyModifier),
                     _plot       (plot),
                     _pick_trap  (pick_trap),
                     _pick_data  (pick_data)
{
}


SimplePick::~SimplePick(void)
{
}


//---------------------- button -----------------------------//
//---------------------- button -----------------------------//
//---------------------- button -----------------------------//

void SimplePick::buttonAny (int x1, int x2, int y1, int y2,
                 int button, Action action, Modifier modifier)
{
  if(_pick_trap) _pick_trap(_pick_data, _plot,
                             x1, x2, y1, y2,
                             button, action, modifier);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
