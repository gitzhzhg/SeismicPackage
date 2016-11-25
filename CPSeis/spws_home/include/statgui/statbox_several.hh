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

//------------------------ statbox_several.hh ---------------------//
//------------------------ statbox_several.hh ---------------------//
//------------------------ statbox_several.hh ---------------------//

//            header file for the StatboxSeveral class
//                derived from the SLDatabox class
//                      subdirectory statgui

 
#ifndef _STATBOX_SEVERAL_HH_
#define _STATBOX_SEVERAL_HH_

#include "sl/sl_databox.hh"


class StatboxSeveral  :  public SLDatabox
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class StaticManager   *_manager;
  class StatguiSeveral  *_gui;
  long                  *_dindex;   // pointer to list of dataset indices.

  long  _interp;  // one of the StaticDataset::INTERP enums.
  long  _extrap;  // one of the StaticDataset::EXTRAP enums.

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor

  StatboxSeveral (SLDelay *slparent, StaticManager *manager);
  virtual ~StatboxSeveral();

  void    scrollTable();

  StaticManager   *manager             ()  const  { return _manager; }
  StatguiSeveral  *getStatguiSeveral   ()  const  { return _gui; }

  long     getDatasetIndex (long index)  const  { return _dindex[index]; }
  long     getXindex       (long index)  const;
  long     getYindex       (long index)  const;

  void setStatguiSeveral (StatguiSeveral *gui)     { _gui = gui; }
  void setDatasetIndex   (long index, long dindex) { _dindex[index] = dindex; }
  void stepDatasetIndex  (long index, int step)    { _dindex[index] += step; }

  long getInterp  ()          const  { return  _interp; }
  long getExtrap  ()          const  { return  _extrap; }

  void stepInterp ();
  void stepExtrap ();

private:

  virtual void makeHelper();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
