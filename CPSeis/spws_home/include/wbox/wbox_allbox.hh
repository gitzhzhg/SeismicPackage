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

//----------------------- wbox_allbox.hh -------------------------//
//----------------------- wbox_allbox.hh -------------------------//
//----------------------- wbox_allbox.hh -------------------------//

//             header file for the WboxAllbox class
//                  not derived from any class
//                       subdirectory wbox


#ifndef _WBOX_ALLBOX_HH_
#define _WBOX_ALLBOX_HH_


#include "wproc.h"


class WboxScreen;
class WboxBox;


class WboxAllbox
{

//--------------------------- typedefs ----------------------------//
//--------------------------- typedefs ----------------------------//
//--------------------------- typedefs ----------------------------//

public:

  typedef void WboxGenericTrap (void *box, long *ident, long *index,
                                  char *text, long *nread, char *endkey);

  typedef void WboxUpdate      (void *data);

  typedef void WboxFortranTrapHandler (WboxGenericTrap *trap, int traptype,
                                       int ibox, int *ident, int *index,
                                       char *text, int nread, char *endkey);

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  int                 _startup;
  int                 _totnumbox;
  int                 _totnumscreens;
  int                 _debug;
  int                 _tinysize;
  int                 _keymode;
  int                 _scrollflag;
  int                 _scrollwidth;
  int                 _maxrows;
  class WboxBox      *_box_being_created;  // NULL if not being created.
  int                 _trap_box_number;    // 0 if not in a trap.
  int                 _entering_text;      // TRUE or FALSE.
  class ArrayBucket  *_box_list;
  class ArrayBucket  *_screen_list;
  WboxUpdate         *_updatename;
  void               *_updatedata;
  WboxFortranTrapHandler *_fortran_trap_handler;


//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  WboxAllbox();

  virtual ~WboxAllbox();

  WboxBox    *create1 (WboxGenericTrap *default_trap, int default_traptype,
                       int omit = 0, int nrows_init = -1);
  Widget      create2 (const char *boxname, Widget toplevel, Widget parent,
                       Widget w, HelpCtx hctx,
                       const char *helpfile, const char *helptitle);

  int           totNumBox          ()  const  { return _totnumbox; }
  int           totNumScreens      ()  const  { return _totnumscreens; }
  int           getDebug           ()  const  { return _debug; }
  int           getTinySize        ()  const  { return _tinysize; }
  int           getKeymode         ()  const  { return _keymode; }
  int           getScrollbarFlag   ()  const  { return _scrollflag; }
  int           getScrollbarWidth  ()  const  { return _scrollwidth; }
  int           getMaxRows         ()  const  { return _maxrows; }
  WboxBox      *getBoxBeingCreated ()  const  { return _box_being_created; }
  int           getTrapBoxNumber   ()  const  { return _trap_box_number; }

  WboxScreen *getScreenPointer   (int iscreen)          const;
  WboxBox    *getBoxPointer      (int ibox)             const;
  WboxBox    *findBoxPointer     (const char *boxname)  const;

  void  registerFortranTrapHandler(WboxFortranTrapHandler *handler)
                                     { _fortran_trap_handler = handler; }
  WboxFortranTrapHandler *fortranTrapHandler()  const 
                                        { return _fortran_trap_handler; }

  void        setMaxRows               (int maxrows);
  void        setDebug                 (int debug);
  void        setKeymode               (int keymode);
  void        toggleKeymode            ();
  void        setTrapBoxNumber         (int ibox);
  void        textBeingEntered         ();
  void        textEntryCompleted       ();
  void        informWindowboxDestroyed (WboxBox *box);

  void        registerAdditionalUpdate (WboxUpdate *updatename,
                                        void       *updatedata);
  void        doAdditionalUpdate ();

  void        flushBuffer        ();
  void        ringBell           ();
  void        wasteTime          (int n)  const;

  void        getResources(Widget widget);
  void        updateScrollbars();
  void        update();
  void        updateFields(WboxBox *active_box, const char *endkey,
                           int irow, int icol, int irow2, int icol2);

private:

  void        updateArrayVisibilities();
  void        updateSensitivities();
  void        privateUpdate(WboxBox *active_box, const char *endkey,
                            int irow, int icol, int irow2, int icol2);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


