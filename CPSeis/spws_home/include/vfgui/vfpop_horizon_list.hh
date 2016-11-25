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

//--------------------- vfpop_horizon_list.hh ---------------------//
//--------------------- vfpop_horizon_list.hh ---------------------//
//--------------------- vfpop_horizon_list.hh ---------------------//

//              header file for the VfpopHorizonList class
//                    derived from the SLDialog class
//                          subdirectory vfgui


#ifndef _VFPOP_HORIZON_LIST_HH_
#define _VFPOP_HORIZON_LIST_HH_

#include "sl/sl_dialog.hh"


class VfpopHorizonList : public SLDialog
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class VfManager  *_manager;
  class VfHorizons *_horizons;


//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

           VfpopHorizonList (SLDelay *slparent, char *name,
                             class VfManager        *manager,
                             class VfHorizons       *horizons,
                             class SLShellContainer *trans,
                             class ContainerList    *clist);
  virtual ~VfpopHorizonList ();

  class VfManager  *manager ()  const  { return _manager; }
  class VfHorizons *horizons()  const  { return _horizons; }


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
