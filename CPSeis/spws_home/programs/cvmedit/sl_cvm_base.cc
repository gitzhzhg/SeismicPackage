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
#include <stdio.h>
#include <math.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>

#include "wproc.h"
#include "sp/seis_plot.hh"
#include "sp/seis_plot_under.hh"
#include "sp/seis_select_pop.hh"
#include "sp/seis_grid_pop.hh"
#include "sp/seis_loc_out.hh"
#include "sp/seis_color_pop.hh"
#include "sp/seis_cbar_pop.hh"
#include "vect/ll_seis_vect.hh"
#include "oprim/modbase_data.hh"
#include "sl/sl_pull_pop.hh"
#include "sl/sl_form.hh"
#include "sl/shell_mouse_help.hh"
#include "sl_cvm_base.hh"
#include "savecvm.h"
#include "seis_transf.hh"
#include "pick_gui_pop.hh"
#include "vl_data.hh"
#include "vl_data_user.hh"
#include "vec_list_util.hh"
#include "model_desc.hh"
#include "vu/seis_label.hh"


static char *fallbacks[] = { 
#include "cvm_res.h"
         NULL
};

static char *plothelpn = "mouse*plot.NOPICK: BTN#1 none, BTN#2 none, \
BTN#3 none";
static char *plothelp = "mouse*plot.PICK: BTN#1 pick, Cntrl BTN#1 select,\\n\
Shift BTN#1 move nearest, BTN#2 delete, BTN#3 none";


cvm_app_res   CvmAppBase::_resources;

class PlotBase;

#define CLASS_NAME "CVMEdit"
#define APP_NAME   "CVMEDIT"


enum { NEWCVM, OPENCVM, SAVECVM, SP, SG, ADDDISP, KILL};
enum { ZUP, ZDOWN, ZSEP, ZNONE };
enum { COLOR1,CBAR1,ADDLAB,DELLAB };
enum { CTX,OVERVIEW,VERSION,MOUSEHELP};

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

void   CVMMsgBox(Widget , char *);

#ifdef __cplusplus
}                   // for C++
#endif

typedef struct CVM_RES { char *help_path; } cvm_res;
static XtResource resources[]=
 { "cvmhelpPath", "CvmHelpPath", XtRString, sizeof(char *),
    XtOffsetOf( cvm_res,help_path),
    XtRString,
    (void*)"/u/pospsve/spws/vbld"
 };

static XrmOptionDescRec options[] = {
      { "-wo",          "*wiggleOnly",      XrmoptionNoArg,  "True" },
      { "-framebuffer", "*frameBuffer",     XrmoptionNoArg,  "True" },
      { "-private",     "*privateCmap",     XrmoptionNoArg,  "True" },
      { "-help",        "*showHelp",        XrmoptionNoArg,  "True" },
      { "-nobs",        "*doBackingStore",  XrmoptionNoArg,  "False"},
      { "-bs",          "*doBackingStore",  XrmoptionNoArg,  "True" },
      { "-cbar",        "*cbarColors",      XrmoptionSepArg, NULL   },
      { "-helpfile",    "*helpFile",        XrmoptionSepArg, NULL   },
     };

CvmAppBase::CvmAppBase(char *display_name, XtAppContext app_context):
            SLApp( APP_NAME, CLASS_NAME, display_name, app_context)
{
  if(!display()) return;
// Set the title
  setTitle("CONOCO Velocity Model Editor: Remote",True);
 _sp     = NULL;
 _spund  = NULL;
 _select = NULL;
 _sgpop  = NULL;
 _shmouse= NULL;
 _moddesc= NULL;
 _owns_model=0;
//  call setup() after the constructor
}

CvmAppBase::CvmAppBase(Display *dpy,  char *app_name, char *class_name):
            SLApp( dpy, app_name, class_name)
{
  if(!dpy) return;
// Set the title
  setTitle("CONOCO CVMEdit - Converted",True);
 _sp     = NULL;
 _spund  = NULL;
 _select = NULL;
 _sgpop  = NULL;
 _shmouse= NULL;
 _moddesc= NULL;
 _owns_model=0;
//  call setup() after the constructor
}

CvmAppBase::CvmAppBase(int    &argc, char   **argv):
            SLApp( APP_NAME, CLASS_NAME, argc, argv,
               options, XtNumber(options) )
               
{

  if(!display()) return;
// Set the title
  setTitle("CONOCO Velocity Model Editor: Remote",True);
 _sp     = NULL;
 _spund  = NULL;
 _select = NULL;
 _sgpop  = NULL;
 _shmouse= NULL;
 _moddesc= NULL;
 _owns_model=0;
//  call setup() after the constructor
}

CvmAppBase::~CvmAppBase()
{
 if(_shmouse) { delete _shmouse; _shmouse= NULL; }
 if(_spund)   { delete (SeisPlotUnder *) _spund ; _spund=NULL; }
 if(_sp)      { delete (SeisPlot *)  _sp;  _sp=NULL; }
 if(_select)  { delete (SeisSelectPop *) _select; _select=NULL; }
 if(_sgpop)   { delete (SeisGridPop *)   _sgpop ; _sgpop=NULL; }
 if(_cb_pop)   { delete (SeisCbarPop *)   _cb_pop ; _cb_pop=NULL; }
 if(_moddesc && _owns_model==1) {delete _moddesc;  _moddesc= 0; }
}

void CvmAppBase::setup()
{
 HelpCtx Hctx;

 char help_title[40];
 char help_file[120];
 char CVMPATH[80];
 cvm_res locres; // Custom resource indicating where cvm_edit help occurs.

 _main_form = NULL;
 _slout  = NULL;
 _pickdata=NULL;
 _data_user_pr = NULL;
 _data_user_mats = NULL;
 _help_file[0]='\0';
 _help_data=NULL;
 _notify_quit =NULL;
 _notify_data =NULL;

/**********************************************
 * Set Fallback Resources.                   **
 *********************************************/
 setFallbacks(fallbacks);

/**********************************************
 * Determine the availability of colors      **
 *********************************************/
 _resources.wigonly = False;
 _resources.num_col_colors = 34;
 doColor();

 setMessage("");

// Get resource for the help path name
  getResources(&locres,resources,XtNumber(resources));
  strcpy(CVMPATH,locres.help_path);

// Create a new help session
 strcpy(help_title,"Help for CVMEdit");
 strcpy(help_file,CVMPATH);
 strcat(help_file,"CVMEdit_help");
// strcpy(help_file,"/u/pospsve/spws/vbld/CVMEdit_help");
 strcpy(_help_file,help_file);
 //Hctx = setup_help(mainWindow(),(XrmDatabase *) &_help_data, help_file, help_title);
 //setHctx(Hctx);
 initHelp( help_file, help_title);
 Hctx= getHelpCtx();

 _shmouse = new ShellMouseHelp(mainWindow(), "NOPICK",plothelpn);
 _shmouse->setFallback(plothelp);

 showStatline(True);
 
/**********************************************
 * Fill out the menu bar                     **
 *********************************************/
 buildMenuBar();

/**********************************************
 * Create form to contain other things.      **
 * Create SeisPlot and SeisLocOut objects    **
 * Create SeisPlotUnder for grid plots       **
 *********************************************/
 buildPlotArea();

/**********************************************
 * Create some of the popups at startup time***
 *********************************************/
 buildPopups();

 realize();
 InitSeisPlot();
}

void CvmAppBase::buildMenuBar()
{HelpCtx Hctx = getHelpCtx();               // from SLBase
 SLPullPop     *zoompd,*custpd,*pikpd,*helppd;
 static String fnames[7] = {"fil1","fil2","fil3",
                            "fil4","fil5","fil6","fil7"};
 static String onames[8] = {"opt1","opt2","opt3","opt4",
                            "opt5","opt6","opt7","opt8"};
 static String znames[4] = {"zoom1","zoom2","zoom3","zoom4"};
 static String pnames[1] = {"pikmod"};
 static String cnames[4] = {"customize","cbar","addlab","dellab"};
 static String hnames[2] = {"Context","Overview"};

// Add Pull Downs to the menu Bar
 _filepd = new SLPullPop("FILE",   Hctx, this);
 zoompd = new SLPullPop("ZOOM",   Hctx, this);
 pikpd  = new SLPullPop("PICK",   Hctx, this);
 custpd = new SLPullPop("CUSTOMIZE",   Hctx, this);
 helppd = new SLPullPop("HELP",   Hctx, this);


// Push Buttons for the File pulldown
 _filepd->addPush(fnames[2], SaveCVM ,(void *) this,SAVECVM);
 _filepd->addPush(fnames[3], SelectSeis ,(void *) this,SP);
 _filepd->addPush(fnames[4], SelectSeis ,(void *) this,SG);
 _filepd->addSep();
 _filepd->addPush(fnames[5], KillCVM,(void *) this,KILL);

// Push Buttons for the Zoom options pulldown
 zoompd->addPush(znames[0],ZoomCB   ,(void *) this, ZUP);
 zoompd->addPush(znames[1],ZoomCB   ,(void *) this, ZDOWN);
 zoompd->addPush(znames[2],ZoomCB   ,(void *) this, ZSEP);
 zoompd->addPush(znames[3],ZoomCB   ,(void *) this, ZNONE);

// Push Buttons for the Customize pulldown
 custpd->addPush(cnames[0],ColorCB   ,(void *) this, COLOR1);
 custpd->addPush(cnames[1],ColorCB   ,(void *) this, CBAR1);
 custpd->addPush(cnames[2],ColorCB   ,(void *) this, ADDLAB);
 custpd->addPush(cnames[3],ColorCB   ,(void *) this, DELLAB);

// Push Buttons for the Help pulldown
 helppd->addPush(hnames[CTX],HelpCVM ,(void *) this,CTX);
 helppd->addPush(hnames[OVERVIEW],HelpCVM ,(void *) this,OVERVIEW);
  add_cshelpcb(helppd->getWidget(CTX),Hctx);

// Push Buttons for the Picking pulldown
 pikpd->addPush(pnames[0],InitPikCB ,(CvmAppBase *) this,1);


}

void CvmAppBase::buildPlotArea()
{/**********************************************
 * Create SeisPlot and SeisLocOut objects    **
 * Create form to contain other things.      **
 *********************************************/
 Arg  arglist[25];
 int n;

 _main_form       =  new SLForm(mainWindow(),"main_form");
 setWorkArea(_main_form);
 _sp     = new SeisPlot(_main_form->W(),"plot");
 _sp->setHelpCtx(getHelpCtx());
 _sp->setMessageWidget(statusWidget() );
 _sp->setModeWidget(modeWidget(),"Mode: Plot");
 _sp->setTransform(NULL);
 _slout = new SeisLocOut(_main_form->W(),"xyout",_sp,getHelpCtx());
 n= 0;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_FORM) ; n++;
 XtSetValues(_slout->W(),arglist,n);
 n=0;
 XtSetArg (arglist[n], XmNtopAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNleftAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNrightAttachment, XmATTACH_FORM) ; n++;
 XtSetArg (arglist[n], XmNbottomAttachment, XmATTACH_WIDGET) ; n++;
 XtSetArg (arglist[n], XmNbottomWidget, _slout->W()) ; n++;
 XtSetValues(_sp->W(),arglist,n);

 if(_cando_underlay)
  {_spund  = new SeisPlotUnder(_sp);
   _spund->setHelpCtx(getHelpCtx());
   _spund->setSeisAnnotation(0);
  }
}

void CvmAppBase::buildPopups()
{
 _select = NULL;
 _sgpop  = NULL;
 _cb_pop = NULL;
 _color_pop=NULL;
 _select = new SeisSelectPop(mainWindow(),"Select",getHelpCtx(),_sp);
 _select->make(mainWindow());
 _sgpop = new SeisGridPop(mainWindow(),"GridPop",_sp,getHelpCtx());
 _sgpop->make(mainWindow());

 _sp->setLoadableColors((int) _resources.num_col_colors);
 if (_cando_colors)
  {_color_pop = new SeisColorPop(mainWindow(),"color_pop",
                _sp, getHelpCtx());
   _color_pop->make();
   _cb_pop    = new SeisCbarPop(mainWindow(),"cbpop",_sp,getHelpCtx());
   _cb_pop->make(NULL);
   if (_spund && _cb_pop) _color_pop->addSP( _spund);
  }

}

void CvmAppBase::HelpCVM(void *data, long ident)
{CvmAppBase *cvmapp = (CvmAppBase *) data;
 if(cvmapp == NULL) return;
 switch (ident)
  { case CTX:
             break;
    case OVERVIEW:
             overview_help("help2", cvmapp->getHelpCtx());
             break;
  }

}

void CvmAppBase::InitSeisPlot()
{// Sets up reasonable defaults when SeisPlot is created
 // must come after a realize call.
 ErsCoordInf *T;
 float   w,h,uwidth=500,uheight=500,fwidth=8.0,fheight=6.0;
 float   uc[4],dc[4],nc[4],ptl;

 _sp->setPlotType(PlotImage::PlotGRID);
 _sp->setPlotSize(fwidth,fheight);
 _sp->setTI(20.0);
 _sp->setIS(1.0);
 if(_spund)
  { _spund->setPlotType(PlotImage::PlotGRID);
    _spund->setPlotSize(fwidth,fheight);
    _spund->setTI(20.0);
    _spund->setIS(1.0);
  }

 /* set normal coordinates */
 T = new_coord();
 nc[0] = 0.;
 nc[2] = 1.; /* upper left  */
 nc[1] = 1.;
 nc[3] = 0.; /* lower right */
 Set_nc(T,nc);

 /* set user coordinates */
 uc[0] = 0.0;
 uc[2] = 0.0;
 uc[1] = uwidth;
 uc[3] = uheight;
 Set_uc(T,uc);
// Set the new limits, sets corners and tmin,tmax
 _sp->setGridXYS(uc[0],uc[1],uc[2],uc[3]);
 _sp->setTmin(uc[2]);
 _sp->setTmax(uc[3]);
 if(_spund)
  { _spund->setGridXYS(uc[0],uc[1],uc[2],uc[3]);
    _spund->setTmin(uc[2]);
    _spund->setTmax(uc[3]);
  }

 ptl = (uc[3] - uc[2])/5.0;
 _sp->setTimingLines((double) ptl,(double) ptl);


 _sp->plot();
 /* get device coordinates from SeisPlot object */
 /* plot must be called before the following    */
 /* set device coordinates */
 dc[0] = (float) _sp->xPixel(uc[0]);
 dc[1] = (float) _sp->xPixel(uc[1]);
 dc[2] = (float) _sp->yPixel(uc[2]);
 dc[3] = (float) _sp->yPixel(uc[3]);
 Set_dc(T,dc);
 w = _sp->plottedWidth();
 h = _sp->plottedHeight();

 destroy_coord(T);
}

void CvmAppBase::SelectSeis(void * data, long ident)
{CvmAppBase *cvmapp = (CvmAppBase *) data;
 /* Select the Seismic for Display */
 if(cvmapp == NULL) return;
 if(ident == SG)
  { 
   if(cvmapp->_sgpop != NULL) cvmapp->_sgpop->manage();
   cvmapp->_sgpop->setPlotType(PlotImage::PlotISO);
   if(cvmapp->_sp->plotType()==PlotImage::PlotGRID) 
     cvmapp->_sgpop->setPlotType(PlotImage::PlotGRID);
  }
 else
  { if(cvmapp->_select != NULL) cvmapp->_select->manage(); }
 return;
}

void CvmAppBase::SaveCVM(void *data, long ident )
{CvmAppBase *cvmapp = (CvmAppBase *) data;
 Widget form;
 int  option;
 if(cvmapp == NULL || ident != SAVECVM) return;

 option = 0; /* returns form in a dialog */
 form = SaveGUI(option,(void *) cvmapp,cvmapp->getHelpCtx());
 if(form == NULL) return;
 XtManageChild(form);
 XtAddCallback(form,XmNdestroyCallback,(XtCallbackProc)  SaveDes,cvmapp);
 if(form != NULL)
  { cvmapp->_filepd->sensitive(False,SAVECVM,-1); }
 return;
}
void CvmAppBase::SaveDes(Widget W, CvmAppBase *cvmapp, caddr_t b)
{ cvmapp->_filepd->sensitive(True,SAVECVM,-1);
  if(b || W) return; // kluge to suppress warning
}

void CvmAppBase::KillCVM(void *data, long )
{ /* Close down everything */
 CvmAppBase *cvmapp = (CvmAppBase *) data;
 if(!cvmapp) return;
 cvmapp->Shutdown();
 return;
}
void CvmAppBase::Shutdown()
{
 if(_notify_quit) (*_notify_quit)( (void *) this, _notify_data);
 delete this;
}

void CvmAppBase::setModelDesc(ModelDesc *mod)
{// set pointer to Model Description
 SeisVectLinkedList *vll=NULL;
 _moddesc = mod;
 if(!_sp) return;
 if(!mod) return;
 vll = (SeisVectLinkedList *) mod->VLLComponent(MOD_STRUC);
 if(vll) vll->addPlot(_sp);
 VectorListSetVis(vll,1);
 vll = (SeisVectLinkedList *) mod->VLLComponent(MOD_MDATA);
 if(vll) vll->addPlot(_sp);
 VectorListSetVis(vll,1);
 vll = (SeisVectLinkedList *) mod->VLLComponent(MOD_CELLB);
 if(vll) vll->addPlot(_sp);
 vll = (SeisVectLinkedList *) mod->VLLComponent(MOD_CELLP);
 if(vll) vll->addPlot(_sp);
 VectorListSetVis(vll,1);
}


/*******************************************************
 * Create or remanage popup to control Color           *
 ******************************************************/
void CvmAppBase::ColorCB(void *data, long ident )
{CvmAppBase *cvmapp = (CvmAppBase *) data;
 /* Create and/or manage the color popup */
 if(cvmapp == NULL) return;

 if(ident == COLOR1)
   { if(!cvmapp->_color_pop)
      {CVMMsgBox(cvmapp->mainWindow(),"Color Popup does not exist");
       return;
      }
    cvmapp->_color_pop->makeAndManage();
   }
 if(ident == CBAR1)
   { if(!cvmapp->_cb_pop)
      {CVMMsgBox(cvmapp->mainWindow(),"Color Bar Popup does not exist");
       return;
      }
    cvmapp->_cb_pop->makeAndManage();
  }
 if(ident == ADDLAB) {
   cvmapp->_label->insertMoveOneLabel();
 }
 if(ident == DELLAB) {
   cvmapp->_label->deleteAllLabels();
 }
 return;
}

/*******************************************************
 * Call the appropriate zoom method of SeisPlot        *
 ******************************************************/
void CvmAppBase::ZoomCB(void *data, long ident )
{CvmAppBase *cvmapp = (CvmAppBase *) data;
 SeisPlot *sp;
 if(cvmapp == NULL) return;
 sp = (SeisPlot *) cvmapp->_sp;
 if(sp == NULL) return;

 if(ident == ZUP) sp->zoomUp();
 if(ident == ZDOWN) sp->zoomDown();
 if(ident == ZSEP) sp->zoomUpSeparateWin();
 if(ident == ZNONE) sp->originalSize();

 return;
}

/*******************************************************
 * Create or remanage popup to control model picking   *
 ******************************************************/
void CvmAppBase::InitPikCB(void *data, long /*ident*/ )
{CvmAppBase *cvmapp = (CvmAppBase *) data;

 if(cvmapp == NULL) return;
 if(cvmapp->cvm_get_vldata(CvmAppBase::Structure) == NULL || 
    cvmapp->cvm_get_vldata(CvmAppBase::Materials) == NULL)
  {
   CVMMsgBox(cvmapp->mainWindow(),"No data for picking ");
   return;
  }

 if(cvmapp->_pickdata == NULL)
   cvmapp->_pickdata  =  new PickGuiPop(cvmapp->mainWindow(),
           "pickmod", cvmapp->getHelpCtx(), cvmapp);
 if(cvmapp->_pickdata != NULL)
  { cvmapp->_pickdata->makeAndManage();
    cvmapp->_pickdata->setTitle("Model Picking"); }

 return;
}

int CvmAppBase::PickingActive()
{if(_pickdata == NULL) return 0;
 return _pickdata->isActive();
}

void CvmAppBase::StopPicking()
{
 if(_pickdata == NULL) return;
 delete _pickdata;
}

void CvmAppBase::doColor()
{ // Check availability of colors
  unsigned long cflags;
  long visclass;
  Display *dpy= XtDisplay(topWidget());
  long    total_colors;

   // --------- color ------------
   cflags= test_vis(dpy, &visclass, &total_colors );

   if (_resources.wigonly) {
        _cando_colors  = False;
        _cando_underlay= False;
   } // end if
   else {
      if ( cflags & MAY_COL ) {
          _total_to_alloc=  _resources.num_col_colors * 2 +2;
          if (_total_to_alloc < (total_colors -25))
               _cando_underlay= True;
          else {
               _cando_underlay= False;
               _total_to_alloc=  _resources.num_col_colors;
          } // End else
          if (canIAlloc(_total_to_alloc)) {
               _cando_colors= True;
          }
          else {
               tryAgainWithPrivate();
               _cando_colors= isPrivateCmap();
           } // End else
      } // End if
      else {
           _cando_colors  = False;
           _cando_underlay= False;
      } // end if
   } // end else
}

void *CvmAppBase::cvm_get_seisplot(CvmAppBase *cvmapp)
{ // SeisPlot object
  if(cvmapp==NULL) return NULL;
  return (void *) cvmapp->_sp;
}

void *CvmAppBase::cvm_get_seisplotu(CvmAppBase *cvmapp)
{ // SeisPlotUnder object
  if(cvmapp==NULL) return NULL;
  return (void *) cvmapp->_spund;
}

void *CvmAppBase::cvm_get_seispop(CvmAppBase *cvmapp)
{ if(cvmapp==NULL) return NULL;
  return (void *) cvmapp->_select;
}
void *CvmAppBase::cvm_get_seisgpop(CvmAppBase *cvmapp)
{ if(cvmapp==NULL) return NULL;
  return (void *) cvmapp->_sgpop;
}

void *CvmAppBase::cvm_get_vlpr(CvmAppBase *cvmapp)
{ if(cvmapp==NULL) return NULL;
  VectorListData *data=cvmapp->cvm_get_vldata(CvmAppBase::Structure);
  if(data==NULL) return NULL;
  return (void *) data->getDataObject();
}
void *CvmAppBase::cvm_get_vlmats(CvmAppBase *cvmapp)
{ if(cvmapp==NULL) return NULL;
  VectorListData *data=cvmapp->cvm_get_vldata(CvmAppBase::Materials);
  if(data==NULL) return NULL;
  return (void *) data->getDataObject();
}
void *CvmAppBase::cvm_get_vlcells(CvmAppBase *cvmapp)
{ if(cvmapp==NULL) return NULL;
  VectorListData *data=cvmapp->cvm_get_vldata(CvmAppBase::Boundarys);
  if(data==NULL) return NULL;
  return (void *) data->getDataObject();
}
void *CvmAppBase::cvm_get_vlclabs(CvmAppBase *cvmapp)
{ if(cvmapp==NULL) return NULL;
  VectorListData *data=cvmapp->cvm_get_vldata(CvmAppBase::Cpointers);
  if(data==NULL) return NULL;
  return (void *) data->getDataObject();
}

void *CvmAppBase::cvm_get_vlprd(CvmAppBase *cvmapp)
{ if(cvmapp==NULL) return NULL;
  if(!cvmapp->_data_user_pr) return NULL;
  return (void *) cvmapp->_data_user_pr->getData();
}
void *CvmAppBase::cvm_get_vlmatsd(CvmAppBase *cvmapp)
{ if(cvmapp==NULL) return NULL;
  if(!cvmapp->_data_user_mats) return NULL;
  return (void *) cvmapp->_data_user_mats->getData();
}

void CvmAppBase::cvmSetCDdata(CoordData *data)
{ if(_moddesc) _moddesc->setCoordData(data); }

CoordData *CvmAppBase::cvmGetCDdata()
{ if(_moddesc) return _moddesc->coord_data();
  else return NULL;
}

ModLimits *CvmAppBase::cvmGetMLimits()
{ if(_moddesc) return _moddesc->MLimits();
  else return NULL;
}


GridLimits *CvmAppBase::cvmGetGLimits()
{ if(_moddesc) return _moddesc->GLimits();
  else return NULL;
}

ErsTransforms *CvmAppBase::cvmGetTransforms()
{ if(_moddesc) return _moddesc->transforms();
  else return NULL;
}

void CvmAppBase::cvm_gettrans(ErsTransform **tx, ErsTransform **ty,
                    ErsTransform **tz)
{ *tx=NULL; *ty=NULL; *ty=NULL;
  if(_moddesc) _moddesc->gettrans(tx,ty,tz);
}

gridData *CvmAppBase::cvmGetGridData()
{ if(_moddesc) return _moddesc->GridData();
  else return NULL;
}

void CvmAppBase::cvmSetGridData(gridData *gd)
{ if(_moddesc) _moddesc->setGridData(gd);
}

VectorLinkedList *CvmAppBase::cvm_get_vll(int list)
{ // for getting the VectorListData pointer(derived from BaseData).
 VectorListData *data=NULL;
 switch (list)
  { case Structure:
             data=cvm_get_vldata(Structure);
             if(!data) return NULL;
             return data->getDataObject();
    case Materials:
             data=cvm_get_vldata(Materials);
             if(!data) return NULL;
             return data->getDataObject();
    case Boundarys:
             data=cvm_get_vldata(Boundarys);
             if(!data) return NULL;
             return data->getDataObject();
    case Cpointers:
             data=cvm_get_vldata(Cpointers);
             if(!data) return NULL;
             return data->getDataObject();
  }
 return NULL;
}

VectorListData *CvmAppBase::cvm_get_vldata(int list)
{ // for getting the VectorListData pointer(derived from BaseData).
 VectorListData *data=NULL;
 switch (list)
  { case Structure:
             data=_moddesc->VLDComponent(MOD_STRUC);
             break;
    case Materials:
             data=_moddesc->VLDComponent(MOD_MDATA);
             break;
    case Boundarys:
             data=_moddesc->VLDComponent(MOD_CELLB);
             break;
    case Cpointers:
             data=_moddesc->VLDComponent(MOD_CELLP);
             break;
  }
 return data;
}

void CvmAppBase::cvm_set_vldata(int list,VectorListData *data)
{ // for setting the VectorListData pointer(derived from BaseData).
 switch (list)
  { case Structure:
             _moddesc->setVLDComponent(MOD_STRUC, data);
             break;
    case Materials:
             _moddesc->setVLDComponent(MOD_MDATA, data);
             break;
    case Boundarys:
             _moddesc->setVLDComponent(MOD_CELLB, data);
             break;
    case Cpointers:
             _moddesc->setVLDComponent(MOD_CELLP, data);
             break;
  }
 return;
}

VLDataUser *CvmAppBase::cvm_get_datauser(int list)
{ // for getting data user pointers.
 switch (list)
  { case Structure:
             return _data_user_pr;
    case Materials:
             return _data_user_mats;
  }
 return NULL;
}

void CvmAppBase::cvm_set_datauser(int list,VLDataUser *data)
{ // for setting data user pointers.
 switch (list)
  { case Structure:
             _data_user_pr = data;
             break;
    case Materials:
             _data_user_mats = data;
             break;
  }
 return;
}

void CvmAppBase::cvmSetSeisTransf(SeisTransf *t)
{ if(_sp->transform()) delete _sp->transform();
  if(_sp) _sp->setTransform(t);
}

// Set User Window:               left     right   bottom        top
void CvmAppBase::set_user_win(float x1, float x2, float y1, float y2)
{
 double p,s;
 float  top,bot;
 float  z_scale,zsize;
 if(!_sp) return;
//  Reset coordinates for PlotGRID or PlotISO plots.
//  Remap the timing lines.

// Retrieve old timing lines and limits
   p  = _sp->primTimingLine();
   s  = _sp->secTimingLine();
   top= _sp->tmin();
   bot= _sp->tmax();
   zsize = _sp->is(); /* is actually a size */
   if(_sp->plotType() == PlotImage::PlotISO) zsize = zsize*(bot-top);
   if(zsize<0) zsize = -zsize;

// Set the new limits, sets corners and tmin,tmax
   _sp->setGridXYS(x1,x2,y2,y1);
   _sp->setTmin(y2);
   _sp->setTmax(y1);

// Rescale the old timing lines to the new system
   z_scale = 1.0;
   if(top != bot) z_scale = (y2 - y1)/(top-bot);
   _sp->setTimingLines(p*z_scale, s*z_scale);

}

