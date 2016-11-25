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

#include "sl/sl_dialog.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_option.hh"
#include "sl/slp_label.hh"
#include "sl/slp_push.hh"
#include "sl/sl_row_column.hh"
#include "sl_cvm_app.hh"
#include "seis_transf.hh"
#include "coord_change.hh"
#include "coord_data.hh"
#include "model_desc.hh"

/*
  Mediate model changes due to a coordinate system change.
  Also control labeling of the vertical seismic axis.
  Change:ModLimits
         GridLimits
         VectorListData
           - for structural horizons
           - for cells(boundaries and pointers)
           - for velocity horizons
         SeisPlot transform
  This is the only place that should initiate a change to
  the coordinate system.

  Select from a palette of names
 */

enum { LOK, LCAN };

/* PROTOTYPES FOR PRIVATE METHODS */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

       void CVMMsgBox(Widget , char *);

#ifdef __cplusplus
}                   // for C++
#endif


CoordChange::CoordChange(Widget parent, CvmApp *cvmapp, HelpCtx Hctx)
  : DataUser()
{/* Dialog to control coordinate changes */
 SLSmartForm  *work;
 SLpPush      *ok, *cancel;


  _cvmapp= NULL;
  _cddata= NULL;
  _shell = NULL;
  _form  = NULL;
/***************************************************
 * Do some sanity checking                        */
 if(!parent) return;
 if(!cvmapp) return;

/*************************************************
 * Create a SLDialog and SLSmartForm            */
  _shell = new SLDialog(parent,"coordchangeshell", Hctx,True);
  work   = _shell->workArea();
  ok     = _shell->addBottomOK(LOK,ControlTrap, (void *) this);
  cancel = _shell->addBottomCancel(LCAN,ControlTrap,
                    (void *) this);
  _shell->addBottomHelp();
  _form  = work;
  _Hctx  = Hctx;
  _cvmapp= (CvmApp *) cvmapp;

  _shell->setTitle("Select The Coordinate System");

  SLRowColumn *rc1,*rc2;
  rc1 = new SLRowColumn(work,"oldcoords",NULL,True, True, True);
  rc2 = new SLRowColumn(work,"newcoords",NULL,True, True, True);
  work->attach(rc1,work,work,work,NULL,4,4,4,0);
  work->attach(rc2,work,work,rc1,NULL,4,4,4,0);

  _oldx = new SLpLabel(rc1,"oldx",0,NULL);
  _oldy = new SLpLabel(rc1,"oldy",1,NULL);
  _oldz = new SLpLabel(rc1,"oldz",2,NULL);
  _xcop = new SLpOption(rc2, "xcoord",0," XCOORDINATE:");
  _ycop = new SLpOption(rc2, "ycoord",1," YCOORDINATE:");
  _zcop = new SLpOption(rc2, "zcoord",2," ZCOORDINATE:");
  _scop = new SLpOption(rc2, "scoord",3," SEIS-ZCOORD:");

// Create the blank options menus
  build_opt();
// Set the initial labels and option settings
  Init();
  _shell->makeAndManage();
}

CoordChange::~CoordChange()
{// updates before destroy???
 delete _shell;
}

void CoordChange::build_opt()
{
 ErsTransforms *tdata;
 ErsTransform  *tr;
 long           i,n;
 char          *trname,str[16];

// Scan CoordData and define the menu options
 _cddata = _cvmapp->cvmGetCDdata();
 if(_cddata == NULL) return;
 addData(_cddata);
 tdata = (ErsTransforms *) _cddata->getDataObject();
 if(tdata == NULL) return;
 n      = ErsTrans_count(tdata);
 for(i=0;i<n;i++)
   { tr = transforms_getnth(tdata,(int) i);
     trname= transform_getname(tr);
     if(trname)
      {//           res ident label
       sprintf(str,"tr_%d",i); 
       _xcop->addOption(str,i,trname);
       _ycop->addOption(str,i,trname);
       _zcop->addOption(str,i,trname);
       _scop->addOption(str,i,trname);
      }
   }
 _oldx->setLabel("OLD XCOORD:");
 _oldy->setLabel("OLD YCOORD:");
 _oldz->setLabel("OLD ZCOORD:");
}


void CoordChange::ControlTrap(void *udat, long ident)
{ /* ident for OK = LOK, ident for CANCEL = LCAN  */
 CoordChange   *data = (CoordChange *) udat;
 CoordData     *cddata;
 CvmApp        *cvmapp;
 ErsTransforms *tdata;
 ErsTransform  *tx=NULL,*ty=NULL,*tz=NULL,*ts=NULL;
 char           msg[240];
 int            pos;

 if(!data) return;
 cddata = data->_cddata;
 cvmapp = data->_cvmapp;
 if(!cddata) return;

 if(ident == LCAN)  // CANCEL
  { 
    goto jump;
  }

 if(ident == LOK ) // OK
  { 
   tdata = (ErsTransforms *) cddata->getDataObject();
   if(!tdata) return;
   pos = data->_xcop->optionValue();
   tx  = transforms_getnth(tdata, pos);
   pos = data->_ycop->optionValue();
   ty  = transforms_getnth(tdata, pos);
   pos = data->_zcop->optionValue();
   tz  = transforms_getnth(tdata, pos);
   pos = data->_scop->optionValue();
   ts  = transforms_getnth(tdata, pos);
   if(!tx)
    { strcpy(msg,"xaxes value is invalid"); goto error; }
   if(!ty)
    { strcpy(msg,"yaxes value is invalid"); goto error; }
   if(!tz)
    { strcpy(msg,"zaxes value is invalid"); goto error; }
   if(!ts)
    { strcpy(msg,"seismic axes value is invalid"); goto error; }


/* Change to the coordsys indicated by tx,ty,tz) */
   cvmapp->cvmTransform(transform_getname(tx),
                        transform_getname(ty),
                        transform_getname(tz),
                        transform_getname(ts));


  }
 jump:
  data->_shell->unmanage();
  return;

 error:
  CVMMsgBox(data->_form->W(),msg);
  return;

}

void CoordChange::modDone(BaseData *bdata, long )
{ CoordData *cddata = (CoordData *) bdata;
  printf("CoordChange: modDone\n");
  if(cddata != _cddata)
    {
     printf("CoordChange:: modDone - wrong data?\n");
     return;
    }
  GuiUpdate();
}

void CoordChange::dataDeleted( BaseData *bdata)
{ CoordData *cddata = (CoordData *) bdata;
  if(cddata != _cddata)
    {printf("CoordEdit:: dataDestroyed- wrong data?\n");
     return;
    }
  _cddata = NULL;
  delete this;
}

void CoordChange::GuiUpdate()
{//Set the menu option buttons consistent with ErsTransforms
 ModelDesc     *mod=NULL;
 ErsTransforms *tdata;
 ErsTransform  *tr;
 char          *trname,str[16];
 int            i,n,menu_cnt=0;
 if(!_shell)  return;
 if(!_cddata) return;
 mod = _cvmapp->getModelDesc();
 if(!mod) return;
 tdata =  mod->transforms();
 if(!tdata)  return;

 SLpPush *opt;
 for( SLDelay *gui = _xcop->topChild(); gui; gui = _xcop->nextChild() )
  {opt = (SLpPush*)gui;
   menu_cnt++;
  }

// if(opt) opt->setCvar(value);
 n = ErsTrans_count(tdata);
 for(i=0;i<n;i++)
   { tr = transforms_getnth(tdata,(int) i);
     trname= transform_getname(tr);
     if(trname)
      {
       opt = (SLpPush *) _xcop->findOptionByIdent((long) i);
       if(i<menu_cnt)
        {
          _xcop->setOptionLabel(i,trname);
          _ycop->setOptionLabel(i,trname);
          _zcop->setOptionLabel(i,trname);
          _scop->setOptionLabel(i,trname);
        }
       else
        {
          sprintf(str,"tr_%d",i); 
          _xcop->addOption(trname,i,trname);
          _ycop->addOption(trname,i,trname);
          _zcop->addOption(trname,i,trname);
          _scop->addOption(trname,i,trname);
        }
      }
   }

 for(i=n;i<menu_cnt;i++)
   {// get rid of unused menu buttons
   _xcop->removeOption(i);
   _ycop->removeOption(i);
   _zcop->removeOption(i);
   _scop->removeOption(i);
  }
 Init();
}

void CoordChange::Init()
{/* Make GUI consistent with the ModLimits structure */
 ModelDesc     *mod=NULL;
 SeisTransf    *sptran;
 ErsTransforms *tdata;
 ErsTransform  *tx=NULL,*ty=NULL,*tz=NULL,*ts=NULL;
 char          *trname,str[40];
 int            pos;

 if(!_cvmapp) return;
 if(!_shell)  return;
 mod = _cvmapp->getModelDesc();
 if(!mod)  return;
 mod->gettrans(&tx,&ty,&tz);
 tdata =  mod->transforms();
 if(tdata==NULL) return;

 // the ident corresponds to the position in tdata
 // the counting starts from 1
 pos =  transforms_pntrtopos(tdata,tx) - 1;
 _xcop->setOptionValue((long) pos);
 pos =  transforms_pntrtopos(tdata,ty) - 1;
 _ycop->setOptionValue((long) pos);
 pos =  transforms_pntrtopos(tdata,tz) - 1;
 _zcop->setOptionValue((long) pos);

// Set the seismic label
 sptran = _cvmapp->cvmGetSeisTransf();
 if(sptran) {
  ts = (ErsTransform *) sptran->getTZsplot();
  if(ts) pos = transforms_pntrtopos(tdata,ts) - 1;
 }
 _scop->setOptionValue((long) pos); 

 trname = transform_getname(tx);
 if((trname=transform_getname(tx)))
  {sprintf(str,"OLD XCOORD:%s",trname);
   if(trname) _oldx->setLabel(str);
  }
 trname = transform_getname(ty);
 if(trname)
  {sprintf(str,"OLD YCOORD:%s",trname);
   if(trname) _oldy->setLabel(str);
  }
 trname = transform_getname(tz);
 if(trname)
  {sprintf(str,"OLD ZCOORD:%s",trname);
   if(trname) _oldz->setLabel(str);
  }

}

Widget CoordChange::Form()
{if(_form) return _form->W();
 return NULL;
}

SLDialog *CoordChange::Dial()
{if(_shell) return _shell;
 return NULL;
}
