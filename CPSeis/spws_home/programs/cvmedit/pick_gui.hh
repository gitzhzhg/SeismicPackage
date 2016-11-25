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
//------------------- pick_gui.hh -------------------------//
//------------------- pick_gui.hh -------------------------//

//            header file for the PickGui
//                 derived from the SLSmartForm class

#ifndef _PICK_GUI_HH
#define _PICK_GUI_HH

#include <stdio.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include "sl/sl_smart_form.hh"


class Vector;
class SeisVectLinkedList;
class VectorListData;
class SLRowColumn;
class SLpOption;
class SLpPush;
class RadioList;
class PickVector;


class PickGui : public SLSmartForm {

//------------------ beginning of class--------------------------//
//------------------ beginning of class--------------------------//


  private:
     SLRowColumn *_slrc1;
     SLRowColumn *_slrc2;
     RadioList   *_radio1;
     RadioList   *_radio2;
     SLpPush     *_pickpb;
     SLpOption   *_snap_menu;
     int          _picking_active;

     VectorListData     *_vldata;
     PickVector  *_pick_class;
     void        *_cvmapp;

     void    setup(void *data);
     VectorListData *getListData();

  protected:
     PickVector *pickE();
     PickVector *pickB();
     PickVector *pickI();
     PickVector *pickVLD();

  public:
     PickGui(Widget    parent,
             char     *name,
             void     *data,
             HelpCtx   Hctx=NULL,
             Boolean   doframe = False,
             Boolean   make_now = True,
             Boolean   manage_now = True);
     PickGui(SLDelay  *contain,
             char     *name,
             void     *data,
             HelpCtx   Hctx=NULL,
             Boolean   doframe = False,
             Boolean   make_if_can = True,
             Boolean   manage_now = True);
     virtual ~PickGui();
     PickVector *startPicking();
     void endPicking();
     static void radio1Trap(void *pickgui,long ident,long,long);
     static void radio2Trap(void *pickgui,long ident,long,long);
     static void pickTrap  (void *pickgui,long ident);
     static void snapTrap  (void *pickgui,long ident,long,long);
     int isActive();

    // make widget tree
    Widget make(Widget p =NULL);

    virtual WidgetClass topClass() {return xmFormWidgetClass;}
    virtual Boolean isDialog()     { return False; }
    virtual Boolean isTopLevel()   { return False; }

//--------------------- end of class ----------------------------//
//--------------------- end of class ----------------------------//

 };

#endif


