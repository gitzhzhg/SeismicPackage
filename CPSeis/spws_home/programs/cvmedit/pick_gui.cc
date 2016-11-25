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
//                      pick_gui.cc 
//         implementation file for the PickGui class
//              derived from the SLSmartForm class
#include <stdio.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include "wproc.h"

#include "pick_gui.hh"
#include "pick_vectori.hh"
#include "pick_vectorb.hh"
#include "pick_vectore.hh"
#include "pick_vector.hh"
#include "pick_vldata.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_row_column.hh"
#include "sl/slp_push.hh"
#include "sl/radio_list.hh"
#include "sl/slp_option.hh"
#include "vect/vector.hh"
#include "vect/ll_seis_vect.hh"
#include "vl_data.hh"
#include "sl_cvm_app.hh"


/* PROTOTYPES FOR METHODS */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif
#ifdef __cplusplus
}                   // for C++
#endif

PickGui::PickGui(SLDelay *contain,char *pname,void *data,
         HelpCtx Hctx, Boolean   doframe,
         Boolean   make_if_can, Boolean   manage_now ):
    SLSmartForm(contain,pname,Hctx,doframe,False,manage_now)
{
 setup(data);

 if (contain->made() && make_if_can) make();
}

PickGui::PickGui(Widget parent,char *pname,void *data,
         HelpCtx Hctx, Boolean   doframe,
         Boolean   make_now, Boolean   manage_now ):
    SLSmartForm(parent,pname,Hctx,doframe,False,manage_now)
{
 setup(data);

 if (make_now) make();
}

PickGui::~PickGui(void)
{
 if(_radio1) delete _radio1;
 if(_radio2) delete _radio2;
 if(_slrc1)    delete _slrc1;
 if(_slrc2)    delete _slrc2;
 if(_snap_menu) delete _snap_menu;
 _slrc1=NULL;
 _slrc2=NULL;
 _cvmapp = NULL;
 if(_picking_active) endPicking();
 _vldata = NULL;
 if(_pick_class) delete _pick_class;
}

void PickGui::setup(void *data)
{CvmAppBase *cvmapp;
 _slrc1 = NULL;
 _slrc2 = NULL;
 _radio1 = NULL;
 _radio2 = NULL;
 _cvmapp = data;
 cvmapp  = (CvmAppBase *) _cvmapp;
 _vldata = (VectorListData *) cvmapp->cvm_get_vlprd(cvmapp);
 _picking_active = 0;
 _pick_class = NULL;

}

Widget PickGui::make(Widget parent)
{

 char *olabel[5]={"NoSnap","Peak","Trough","+ to -","- to +"};
 char *onames[5] ={"snap1","snap2","snap3","snap4","snap5"};
 if(!made())
  {
   Widget wid = SLSmartForm::make(parent);
 
   /************************************************
    * Add push button to start/stop picking       */
    _pickpb = new SLpPush( this, "pickpb",0,"Begin Picking");
    _pickpb->setAtrap(pickTrap,(void *) this); // use static member function
    attach(_pickpb, this, this, NULL, this,10,10,0,4);

   /*************************************************
    * Add an option menu to the form               */
    _snap_menu = new SLpOption(this,"snapmenu",0);
    _snap_menu->addOption(onames[0],PickVector::SNAP_NONE,olabel[0]);
    _snap_menu->addOption(onames[1],PickVector::SNAP_PEAK,olabel[1]);
    _snap_menu->addOption(onames[2],PickVector::SNAP_TROUGH,olabel[2]);
    _snap_menu->addOption(onames[3],PickVector::SNAP_PM,olabel[3]);
    _snap_menu->addOption(onames[4],PickVector::SNAP_MP,olabel[4]);
   _snap_menu->setOptionValue(PickVector::SNAP_NONE);
    _snap_menu->setItrap(snapTrap,(void *) this); // use static member function
    attach(_snap_menu , this, this, NULL , _pickpb ,10,10,0,4);

   /************************************************
    * Add a rowcolumn to the form                 */
    _slrc1 = new SLRowColumn( this ,"rowcol1",NULL,True,True,True);
   //               left  right top   bottom
    attach(_slrc1  , this, NULL, NULL, _snap_menu,10,0,0,10);

   /************************************************
    * Create radio list for picking modes         */
    _radio1 = new RadioList();
    _radio1->addRadio(_slrc1, "rad1", 1,"Pick Structure");
    _radio1->addRadio(_slrc1, "rad2", 2,"Pick Materials");
    _radio1->addRadio(_slrc1, "rad3", 3,"Pick Pointers?");
    _radio1->setIvar(1);
    _radio1->setItrap(radio1Trap, (void *) this);

   /*************************************************
    * Add a rowcolumn to the form                  */
    _slrc2 = new SLRowColumn( this,"rowcol2",NULL,True,True,True);
    attach(_slrc2  , this, NULL, this  , _slrc1 ,10,0,10,10);

   /************************************************
    * Create radio list for pick class selection  */
    _radio2 = new RadioList();
    _radio2->addRadio(_slrc2, "rad1", 1,"Insert At End");
    _radio2->addRadio(_slrc2, "rad2", 2,"Insert At Start");
    _radio2->addRadio(_slrc2, "rad3", 3,"Insert In List");
    _radio2->setIvar(1);
    _radio2->setItrap(radio2Trap, (void *) this);

  }
 makeChildren();
 return topWidget();
}

PickVector *PickGui::pickE()
{void   *seisp;
 Vector *vector = NULL;
 CvmAppBase *cvmapp  = (CvmAppBase *) _cvmapp;
 _vldata = getListData();
 if(_pick_class)
  {vector = _vldata->getEditObject();
   delete _pick_class; _pick_class=NULL;
  }
 if(_vldata)
  {
   seisp = cvmapp->cvm_get_seisplot(cvmapp);
   _pick_class = (PickVector *) new PickVectorE(_vldata,(PlotBase *) seisp);
  }
 if(_pick_class)
  _radio2->setIvar(1);
 else
  _radio2->setIvar(0);
 return _pick_class;
}

PickVector *PickGui::pickB()
{void   *seisp;
 Vector *vector = NULL;
 CvmAppBase *cvmapp  = (CvmAppBase *) _cvmapp;
 _vldata = getListData();
 if(_pick_class)
  {vector = _vldata->getEditObject();
   delete _pick_class; _pick_class=NULL;
  }
 if(_vldata)
  {
   seisp = cvmapp->cvm_get_seisplot(cvmapp);
   _pick_class = (PickVector *) new PickVectorB(_vldata,(PlotBase *) seisp);
  }
 if(_pick_class)
  _radio2->setIvar(2);
 else
  _radio2->setIvar(0);
 return _pick_class;
}

PickVector *PickGui::pickI()
{void   *seisp;
 Vector *vector = NULL;
 CvmAppBase *cvmapp  = (CvmAppBase *) _cvmapp;
 _vldata = getListData();
 if(_pick_class)
  {vector = _vldata->getEditObject();
   delete _pick_class; _pick_class=NULL;
  }
 if(_vldata)
  {
   seisp = cvmapp->cvm_get_seisplot(cvmapp);
   _pick_class = (PickVector *) new PickVectorI(_vldata,(PlotBase *) seisp);
  }
 if(_pick_class)
  _radio2->setIvar(3);
 else
  _radio2->setIvar(0);
 return _pick_class;
}

PickVector *PickGui::pickVLD()
{void   *seisp;
 Vector *vector = NULL;
 CvmAppBase *cvmapp  = (CvmAppBase *) _cvmapp;
 _vldata = getListData();
 if(_pick_class)
  {vector = _vldata->getEditObject();
   delete _pick_class; _pick_class=NULL;
  }
 if(_vldata)
  {
   seisp = cvmapp->cvm_get_seisplot(cvmapp);
   _pick_class = (PickVector *) new PickVLData(_vldata,(PlotBase *) seisp);
  }
 _radio2->setIvar(0);
 return _pick_class;
}

PickVector *PickGui::startPicking()
{long mode2;
 if(_cvmapp==NULL) return NULL;
 _vldata = getListData();

 if(_radio1->ivar()==3)
   {
    pickVLD();
   }
 else
   {
    mode2 = _radio2->ivar();
    if(mode2==1) pickE();
    if(mode2==2) pickB();
    if(mode2==3) pickI();
    _pick_class->setSnap( _snap_menu->ivar());
   }
  _pickpb->setLabel("End Picking");
  _picking_active = 1;
 if(_pick_class) 
  {
   _pick_class->changeHelpToken("PICK");
  }
 return _pick_class;
}

void PickGui::endPicking()
{
 if(_pick_class)
  { _pick_class->setEditVector((Vector *) NULL);
    _pick_class->changeHelpToken("NOPICK");
    delete _pick_class;
    _pick_class = NULL;
  }
 _pickpb->setLabel("Begin Picking");
 _picking_active = 0;
}

VectorListData *PickGui::getListData()
{long mode = _radio1->ivar();
 VectorListData *vldata = NULL;
 CvmAppBase *cvmapp  = (CvmAppBase *) _cvmapp;
 if(_cvmapp==NULL) return NULL;

 if(mode == 1) vldata = cvmapp->cvm_get_vldata(CvmAppBase::Structure);
 if(mode == 2) vldata = cvmapp->cvm_get_vldata(CvmAppBase::Materials);
 if(mode == 3) vldata = cvmapp->cvm_get_vldata(CvmAppBase::Cpointers);
 return vldata;
}

// data, ident, oldv, newv
void PickGui::radio1Trap(void *data,long ,long oldv,long newv)
{ PickGui *pickgui = (PickGui *) data;

  CvmAppBase *cvmapp;
  if(pickgui==NULL) return;
  if(newv==oldv) return;
  cvmapp= (CvmApp *) pickgui->_cvmapp;
  if(cvmapp == NULL) { pickgui->_vldata=NULL;  return; }
  if(pickgui->_picking_active) pickgui->_vldata->unSelect();
  pickgui->_radio1->setIvar(newv);
  if(newv == 1)
   {pickgui->_vldata = cvmapp->cvm_get_vldata(CvmAppBase::Structure);}
  else if(newv ==2)
   {pickgui->_vldata = cvmapp->cvm_get_vldata(CvmAppBase::Materials);}
  else if(newv ==3)
   {pickgui->_vldata = cvmapp->cvm_get_vldata(CvmAppBase::Cpointers);
   }
  else pickgui->_vldata = NULL;
  if(pickgui->_picking_active) pickgui->startPicking();

}

void PickGui::radio2Trap(void *data,long ,long oldv,long newv)
{ PickGui *pickgui = (PickGui *) data;
  if(oldv==newv) return;
  pickgui->_radio2->setIvar(newv);
  if(pickgui->_picking_active) pickgui->startPicking();

}

void PickGui::pickTrap(void *data,long )
{ PickGui *pickgui = (PickGui *) data;
  if(pickgui->_pick_class)
   pickgui->endPicking();
  else
   pickgui->startPicking();

}

void PickGui::snapTrap(void *data,long ,long , long newv)
{ PickGui *pickgui = (PickGui *) data;
  if(pickgui->_pick_class) pickgui->_pick_class->setSnap(newv);
}

int PickGui::isActive()
{ return _picking_active; }
