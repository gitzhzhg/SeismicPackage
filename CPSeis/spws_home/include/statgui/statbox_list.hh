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

//------------------------ statbox_list.hh ---------------------//
//------------------------ statbox_list.hh ---------------------//
//------------------------ statbox_list.hh ---------------------//

//            header file for the StatboxList class
//                derived from the SLDatabox class
//                      subdirectory statgui

 
#ifndef _STATBOX_LIST_HH_
#define _STATBOX_LIST_HH_

#include "sl/sl_databox.hh"


class StatboxList  :  public SLDatabox
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

public:

  enum { SHOW_READ = 55, SHOW_SAVE, SHOW_BACKUP, SHOW_STATUS, SHOW_NILS,
         SHOW_BINS, SHOW_MINMAX };

private:

  class StaticManager *_manager;
  int                  _show;

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor

  StatboxList (SLDelay *slparent, StaticManager *manager);
  virtual ~StatboxList();

  class StaticManager *manager        ()  const  { return _manager; }
  int                  getShowOption  ()  const  { return _show; }
  void                 setShowOption  (int show);
  void                 stepShowOption (int step);  // step is +- integer.

private:

  void makeHelper();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
