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
#include <string.h>
#include <stdio.h>

#include "mlimits_edit.hh"
#include "sl/sl_dialog.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_push.hh"
#include "sl/radio_list.hh"
#include "sl/sl_row_column.hh"
#include "sl_cvm_app.hh"


enum { LOK, LCAN };


Mlimits::Mlimits(Widget parent, CvmApp *cvmapp, HelpCtx Hctx)
{/* Dialog to control model limits  */
 SLSmartForm  *work;
 SLpPush      *ok, *cancel;

  _cvmapp= NULL;
  _shell = NULL;
  _form  = NULL;
  _xmin  = 0.0;
  _xmax  = 1.0;
  _ymin  = 0.0;
  _ymax  = 1.0;
  _zmin  = 0.0;
  _zmax  = 1.0;
/***************************************************
 * Do some sanity checking                        */
 if(!parent) return;
 if(!cvmapp) return;
 
/*************************************************
 * Create a SLDialog and SLSmartForm            */
  _shell = new SLDialog(parent,"mlimshell", Hctx,True);
  work = _shell->workArea();
  ok   = _shell->addBottomOK(LOK,ControlTrap, (void *) this);
  cancel = _shell->addBottomCancel(LCAN,ControlTrap, (void *) this);
  _shell->addBottomHelp();
  _form  = work;
  _Hctx  = Hctx;
  _cvmapp= (CvmApp *) cvmapp;

  _shell->setTitle("Layer Model Limits");

  SLRowColumn *rc1;
  rc1 = new SLRowColumn(work,"limits",NULL,True, True, True);
  _x1 = new SL2Text(rc1, "xmin",0," Minimun X:", SLpText::_FLOAT,12);
  _x1->setupFvarPoint(&_xmin);
  _x2 = new SL2Text(rc1, "xmax",0," Maximum X:", SLpText::_FLOAT,12);
  _x2->setupFvarPoint(&_xmax);
  _y1 = new SL2Text(rc1, "ymin",0," Minimun Y:", SLpText::_FLOAT,12);
  _y1->setupFvarPoint(&_ymin);
  _y2 = new SL2Text(rc1, "ymax",0," Maximum Y:", SLpText::_FLOAT,12);
  _y2->setupFvarPoint(&_ymax);
  _z1 = new SL2Text(rc1, "zmin",0," Minimun Z:", SLpText::_FLOAT,12);
  _z1->setupFvarPoint(&_zmin);
  _z2 = new SL2Text(rc1, "zmax",0," Maximum Z:", SLpText::_FLOAT,12);
  _z2->setupFvarPoint(&_zmax);

  work->attach(rc1,work,work,work,NULL,4,4,4,0);

 _shell->makeAndManage();
// transfer ModLimits data to the GUI
 mlimitsInit();

}

Mlimits::~Mlimits()
{
}

void Mlimits::ControlTrap(void *data, long ident)
{ /* OK = LOK, CANCEL = LCAN  */
 Mlimits *udat = (Mlimits *) data;

 if(!udat) return;
 if(!udat->_cvmapp) return;

 if(ident == LCAN)  // CANCEL
  { 
    goto jump;
  }

 if(ident == LOK ) // OK
  { 
    udat->mlimitsTrap( );
  }
 jump:
  udat->_shell->unmanage();
  return;


}

void Mlimits::mlimitsGuiUpdate(ModLimits *mlim)
{// Update the GUI with the passed values
 ErsTransform *tx, *ty, *tz;
 if(!mlim) return;
 mlimits_get(mlim,&_xmin,&_xmax, &_zmin, &_zmax, &_ymin, &_ymax,
       &tx, &ty, &tz );
 mlimitsGuiUpdate();
}

void Mlimits::mlimitsGuiUpdate()
{//Force an update of the display in case the
 // min and max values have changed.

 _x1->update();
 _x2->update();
 _y1->update();
 _y2->update();
 _z1->update();
 _z2->update();

}

void Mlimits::mlimitsTrap()
{/* Transfer GUI parameters to the ModLimits structure */
 ModLimits     *mlimits;
 ErsTransforms *tdata;
 ErsTransform  *tx,*ty,*tz;

// float        l,r,t,b;
 if(_cvmapp == NULL) return;
 mlimits = _cvmapp->cvmGetMLimits();
 if(mlimits==NULL) return;
 mlimits_get_trans(mlimits,&tx,&ty,&tz);
 tdata   = _cvmapp->cvmGetTransforms();
 if(tdata==NULL) return;

// Transfer values from GUI to model limits of model structure.
 mlimits_set(mlimits,&_xmin,&_xmax, &_zmin, &_zmax, &_ymin, &_ymax,
       tx, ty, tz );

 return;
}

void Mlimits::mlimitsInit()
{/* Transfer Model Limits data to the GUI display */
 ErsTransforms *tdata;
 ModLimits     *mlimits;
 ErsTransform  *tx,*ty,*tz;

 tdata   = _cvmapp->cvmGetTransforms();
 mlimits = _cvmapp->cvmGetMLimits();
 if(mlimits == NULL) return;
 mlimits_get(mlimits,&_xmin,&_xmax,
     &_zmin, &_zmax, &_ymin, &_ymax,
     &tx, &ty, &tz );
 if(_xmin==_xmax) _xmax = _xmin + 1.;
 if(_ymin==_ymax) _ymax = _ymin + 1.;
 if(_zmin==_zmax) _zmax = _zmin + 1.;
/***********************************
 * Update the GUI for model limits**
 **********************************/
 mlimitsGuiUpdate();
}

Widget Mlimits::Form()
{if(!_form) return NULL;
 return _form->W();
}

SLDialog *Mlimits::Dial()
{return _shell;
}
