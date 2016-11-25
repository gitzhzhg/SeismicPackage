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

#include <Xm/MainW.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeB.h>

#include "wproc.h"
#include "file_choice.h"
#include "coord_change.hh"
#include "coord_data.hh"
#include "coord_edit_pop.hh"
#include "mlimits_edit.hh"
#include "glimits1.h"
#include "gridcvm.h"
#include "cell.h"
#include "cell_edit.hh"
#include "new_cvm.h"
#include "opencvm.h"
#include "savecvm.h"
#include "remote_disp.h"
#include "dispopt.h"
#include "transform.h"
#include "sp/seis_plot.hh"
#include "sp/seis_plot_under.hh"
#include "sp/seis_select_pop.hh"
#include "sp/seis_grid_pop.hh"
#include "sp/seis_loc_out.hh"
#include "sp/seis_color_pop.hh"
#include "sp/seis_cbar_pop.hh"
#include "pick_gui_pop.hh"
//#include "gplot.h"
#include "vect/ll_seis_vect.hh"
#include "oprim/modbase_data.hh"
#include "horiz_edit_pop.hh"
#include "horiz_edit.hh"
#include "sl/sl_pull_pop.hh"
#include "sl/sl_form.hh"
#include "vec_list_util.hh"
#include "seis_transf.hh"
#include "vl_data.hh"
#include "vl_data_user.hh"
#include "sl/shell_mouse_help.hh"
#include "sl_cvm_base.hh"
#include "sl_cvm_app.hh"
#include "sl_cvm_inform.hh"
#include "draw_mod.h"
#include "model_desc.hh"
#include "ray_edit.hh"
#include "time_depth.hh"
#include "hardcopy/hardcopy_seis_pop.hh"
#include "vu/seis_label.hh"

static char *fallbacks[] = {
#include "cvm_res.h"
         NULL
};

static char *plothelpn = "mouse*plot.NOPICK: BTN#1 none, BTN#2 none, \
BTN#3 none";
static char *plothelp = "mouse*plot.PICK: BTN#1 pick, Cntrl BTN#1 select,\\n\
Shift BTN#1 move nearest, BTN#2 delete, BTN#3 none";


class PlotBase;

#define CLASS_NAME "CVMEdit"
#define APP_NAME   "CVMEDIT"


enum { NEWCVM, OPENCVM, SAVECVM, SP, SG, ADDDISP, HCPY, KILL}; 
enum { RAYMOD, TDMOD };
enum { MLIM1, GLIM1, TRANS1, DISP1, CELL1, VELO1, STRUCT1, GRID1, COORD1,TEST1 };
enum { ZUP, ZDOWN, ZSEP, ZNONE };
enum { COLOR1,CBAR1,ADDLAB,DELLAB };
enum { CTX,OVERVIEW,VERSION,MOUSEHELP};

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* PROTOTYPES FOR PRIVATE METHODS */

void  *cellGui(Widget,int,HelpCtx,void *);
Widget cellForm(void *);
void   cell_init(void *);

void   CVMMsgBox(Widget , char *);

#ifdef __cplusplus
}                   // for C++
#endif


ErsModel *CvmApp::cvm_getmod(CvmApp *cvmapp)
{ if(cvmapp==NULL) return NULL;
  return cvmapp->_ersmod;
}
void CvmApp::cvm_setmod(CvmApp *cvmapp, ErsModel *model)
{ if(cvmapp==NULL) return;
  if(cvmapp->_ersmod && (model != cvmapp->_ersmod)) 
    destroy_model(cvmapp->_ersmod);
  cvmapp->_ersmod = model;
}
 
void *CvmApp::cvm_get_hzedit(CvmApp *cvmapp)
{ if(cvmapp==NULL) return NULL;
  return cvmapp->_hzedit;
}
void *CvmApp::cvm_get_struc(CvmApp *cvmapp)
{ if(cvmapp==NULL) return NULL;
  return cvmapp->_struc;
}



 
/*
 * Create empty VectorLinkedList's for a new model
 */
int CvmApp::cvm_bld_vlists(int phd, int shd, int mzeit, int szeit)
{
 ModelDesc          *mod=NULL;
 SeisPlot           *splot;
 SeisVectLinkedList *vlist,*vl1,*vl2,*vl3,*vl4;
 VectorListData     *vld;
 Vector             *vector;
 ModBaseData        *dobj;
 ErsTransforms      *tdata=NULL;
 ErsTransform       *tx=NULL,*ty=NULL,*tzm=NULL,*tzs=NULL;
 float               x=0.0,z=0.0,s=0.0,t=0.0,v=1500.0;
 int                 j,num=0,thd=NILKEY;

 mod = getModelDesc();
 if(!mod) return 1;
 if(phd<1) phd = XBASEHDR_;

 vl1 = vlist = new SeisVectLinkedList();
 dobj = new ModBaseData(num,phd,&x,shd,&s,thd,&t,mzeit,&z,NULL);
 dobj->setid(1);
 vector = vlist->add("HORIZ1",dobj,"red",3,False,
          Vector::SolidLine, Vector::FilledSquareMarker,9,1);
 vld = new VectorListData(vlist,NULL);
 cvm_set_vldata(Structure,vld);
 _data_user_pr = new VLDataUser(vld);

 vl2 = vlist = new SeisVectLinkedList;
 dobj = new ModBaseData(num,phd,&x,shd,&s,thd,&t,mzeit,&z,&v);
 dobj->setid(1);
 vector = vlist->add("MAT1",dobj,"red",3,False,
          Vector::SolidLine, Vector::FilledTriangleMarker,9,1);
 vld = new VectorListData(vlist,NULL);
 cvm_set_vldata(Materials,vld);
 _data_user_mats = new VLDataUser(vld);

 vl3 = vlist = new SeisVectLinkedList;
 dobj = new ModBaseData(num,phd,&x,shd,&s,thd,&t,mzeit,&z,NULL);
 dobj->setid(1);
 vector = vlist->add("CELL1",dobj,"green",3,False,
          Vector::SolidLine, Vector::NoMarker,9,1);
 vld = new VectorListData(vlist,NULL);
 cvm_set_vldata(Boundarys,vld);

 vl4 = vlist = new SeisVectLinkedList;
 dobj = new ModBaseData(num,phd,&x,shd,&s,thd,&t,mzeit,&z,NULL);
 dobj->setid(1);
 vector = vlist->add("CLAB1",dobj,"green",3,False,
          Vector::SolidLine, Vector::NoMarker,9,1);
 vld = new VectorListData(vlist,NULL);
 cvm_set_vldata(Cpointers,vld);


// Reset SeisTransf on all instances of SeisPlot
 if(_moddesc)
  {  tdata = _moddesc->transforms();
     _moddesc->gettrans(&tx,&ty,&tzm);
  }
 tzm   = ErsTransByID(tdata,mzeit);
 tzs   = ErsTransByID(tdata,szeit);
 cvmSPTransform(tx,ty,tzm,tzs);

// Connect VectorLinkedList's to root SeisPlot
 splot = (SeisPlot *) getSeisPlot();
 if(!splot) return 1;
 if(vl1) vl1->addPlot(splot);
 if(vl2) vl2->addPlot(splot);
 if(vl3) vl3->addPlot(splot);
 if(vl4) vl4->addPlot(splot);

// Now connect initial lists to all the remote displays
 j = connectLists();

 return 0;
}

typedef struct CVM_RES { char *help_path; } cvm_res;
static XtResource resources[]= {
        "cvmhelpPath",
        "CvmHelpPath",
        XtRString, sizeof(char *),
        XtOffsetOf( cvm_res,help_path),
        XtRString,
        (void*)"/u/pospsve/spws/vbld" };

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


CvmApp::CvmApp(char *display_name, XtAppContext app_context):
        CvmAppBase(display_name, app_context)
{
// Set the title
  setTitle("CVMEdit: Root",True);

  _new_pop= NULL;
  _remdisp= NULL;
  _hrdcpy = NULL;
  _mlimdata=NULL;
  _glimdata=NULL;
  _csysdata=NULL;
  _transpop=NULL;
  _openform=NULL;
  _struc  =NULL;
  _hzedit =NULL;
  _celldata=NULL;
  _griddata=NULL;
  _raypop  =NULL;
  _tdpop   =NULL;
  _display =NULL;
  _ersmod  =NULL;
  _label  =NULL;
//  call setup() after the constructor
}

CvmApp::CvmApp(int    &argc, char   **argv):
        CvmAppBase( argc, argv)
               
{

// initialize plot count in this constructor.
 _plots  = NULL;
 _num_plots = 0;

// Initialize model pointer in this constructor.
 _moddesc= new ModelDesc();
 if(!_moddesc) return;

// Set the title
  setTitle("CVMEdit: Root",True);

  _new_pop= NULL;
  _remdisp= NULL;
  _hrdcpy = NULL;
  _mlimdata=NULL;
  _glimdata=NULL;
  _csysdata=NULL;
  _transpop=NULL;
  _openform=NULL;
  _struc  =NULL;
  _hzedit =NULL;
  _celldata=NULL;
  _griddata=NULL;
  _raypop  =NULL;
  _tdpop   =NULL;
  _display =NULL;
  _ersmod  =NULL;
  _label  =NULL;
//  call setup() after the constructor
 return;
}

CvmApp::~CvmApp()
{int i;
 VectorLinkedList *list;
 void *splot;
 for(i=0;i<_num_plots;i++)
  { if(_plots[i]) delete _plots[i];}
 if(_plots) { free(_plots); _plots=NULL; }
 if(_shmouse) {delete _shmouse; _shmouse = NULL; }
 if(_mlimdata) {delete _mlimdata; _mlimdata = NULL; }
 if(_glimdata) {delete _glimdata; _glimdata = NULL; }
 if(_csysdata) {delete (CoordChange *) _csysdata; }
 if(_transpop)  { delete (CoordEditPop *) _transpop; }
 if(_ersmod) { destroy_model(_ersmod); _ersmod = NULL; }
 delete _label;

 splot = getSeisPlot();
 list =  cvm_get_vll(CvmAppBase::Structure);
 KillVectorList((void *) list, splot);
 list =  cvm_get_vll(CvmAppBase::Materials);
 KillVectorList((void *) list, splot);
 list =  cvm_get_vll(CvmAppBase::Boundarys);
 KillVectorList((void *) list, splot);
 list =  cvm_get_vll(CvmAppBase::Cpointers);
 KillVectorList((void *) list, splot);

 if(_moddesc) { delete _moddesc; _moddesc = NULL; }
}

void CvmApp::setup()
{
 HelpCtx Hctx;
 long ierr;
 char help_title[40];
 char help_file[120];
 char CVMPATH[80];
 cvm_res locres; // Custom resource indicating where cvm_edit help occurs.

 _main_form = NULL;
 _sp     = NULL;
 _spund  = NULL;
 _slout  = NULL;
 _select = NULL;
 _sgpop  = NULL;
 _hrdcpy = NULL;
 _pickdata=NULL;
 _data_user_pr = NULL;
 _data_user_mats = NULL;
 _help_file[0]='\0';
 _help_data=NULL;

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
 _help_file[0]='\0';
 strcpy(help_title,"Help for CVMEdit");
 strcpy(help_file,CVMPATH);
 strcat(help_file,"CVMEdit_help");
// strcpy(help_file,"/u/pospsve/spws/vbld/CVMEdit_help");
 strcpy(_help_file,help_file);
 //Hctx = setup_help(mainWindow(), (XrmDatabase *)&_help_data, 
 //                  help_file, help_title);
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
 _label=  new SeisLabel(_sp, Hctx);

/*************************************************
 * Set up a plot window - uses SeisPlot         */
// if(bypass!=1)
//  {
//   _cvmgui->wkid = 1;
//   i = xopws((void **) &_sp,&_cvmgui->wkid,800,500,
//       _main_form->W(),getHelpCtx());
//  }
/*************************************************
 * Create SeisVectLinkedList objects for display**
 * of vectors on the SeisPlot Object            */
 ErsTransform *tx=NULL,*ty=NULL,*tz=NULL;
 int phd=XBASEHDR_,shd=YBASEHDR_,zeit=TIMEHDR_,szeit=TIMEHDR_;
 if(_moddesc)
   {_moddesc->gettrans(&tx,&ty,&tz);
    phd = (int ) transform_gethdr(tx);
    shd = (int ) transform_gethdr(ty);
    zeit= (int ) transform_gethdr(tz);
   }
 ierr = cvm_bld_vlists(phd,shd,zeit,szeit);
}

void CvmApp::buildMenuBar()
{HelpCtx Hctx = getHelpCtx();               // from SLBase
 SLPullPop  *optpd,*zoompd,*custpd,*pikpd,*modelpd,*helppd;
 static String fnames[8] = {"fil1","fil2","fil3",
                            "fil4","fil5","fil6","fil7","fil8"};
 static String onames[10]= {"opt1","opt2","opt3","opt4",
                            "opt5","opt6","opt7","opt8","opt9","opt10"};
 static String znames[4] = {"zoom1","zoom2","zoom3","zoom4"};
 static String pnames[1] = {"pikmod"};
 static String cnames[4] = {"customize","cbar","addlab","dellab"};
 static String mnames[2] = {"mod1","mod2"};
 static String hnames[2] = {"Context","Overview"};

// Add Pull Downs to the menu Bar
 _filepd= new SLPullPop("FILE",   Hctx, this);
 optpd  = new SLPullPop("OPTION", Hctx, this);
 zoompd = new SLPullPop("ZOOM",   Hctx, this);
 pikpd  = new SLPullPop("PICK",   Hctx, this);
 custpd = new SLPullPop("CUSTOMIZE",   Hctx, this);
 modelpd= new SLPullPop("MODELING",   Hctx, this);
 helppd = new SLPullPop("HELP",   Hctx, this);

// Push Buttons for the File pulldown
 _filepd->addPush(fnames[0], NewCVM  ,(void *) this,NEWCVM);
 _filepd->addPush(fnames[1], OpenCVM ,(void *) this,OPENCVM);
 _filepd->addPush(fnames[2], SaveCVM ,(void *) this,SAVECVM);
 _filepd->addPush(fnames[3], SelectSeis ,(void *) this,SP);
 _filepd->addPush(fnames[4], SelectSeis ,(void *) this,SG);
 _filepd->addSep();
 _filepd->addPush(fnames[6], AddDispCB,(void *) this,ADDDISP);
 _filepd->addPush(fnames[7], HardCopy,(void *) this,HCPY);
 _filepd->addPush(fnames[5], KillCVM ,(void *) this,KILL);

// Push Buttons for the option pulldown
 optpd->addPush(onames[0],MlimitsCB   ,(void *) this,MLIM1);
 optpd->addPush(onames[1],GlimitsCB   ,(void *) this,GLIM1);
 optpd->addPush(onames[2],TransfCB    ,(void *) this,TRANS1);
 optpd->addPush(onames[8],CoordCB     ,(void *) this,COORD1);
 optpd->addPush(onames[3],DisplayCB   ,(void *) this,DISP1);
 optpd->addPush(onames[4],CellCB      ,(void *) this,CELL1);
 optpd->addPush(onames[5],VelocityCB  ,(void *) this,VELO1);
 optpd->addPush(onames[6],StructCB    ,(void *) this,STRUCT1);
 optpd->addPush(onames[7],GridCVM     ,(void *) this,GRID1);
 optpd->addPush(onames[9], NULL    ,(void *) this,TEST1);

// Push Buttons for the Zoom options pulldown
 zoompd->addPush(znames[0],ZoomCB   ,(void *) this, ZUP);
 zoompd->addPush(znames[1],ZoomCB   ,(void *) this, ZDOWN);
 zoompd->addPush(znames[2],ZoomCB   ,(void *) this, ZSEP);
 zoompd->addPush(znames[3],ZoomCB   ,(void *) this, ZNONE);

// Push Buttons for the Picking pulldown
 pikpd->addPush(pnames[0],InitPikCB ,(CvmAppBase *) this,1);

// Push Buttons for the Customize pulldown
 custpd->addPush(cnames[0],ColorCB   ,(void *) this, COLOR1);
 custpd->addPush(cnames[1],ColorCB   ,(void *) this, CBAR1);
 custpd->addPush(cnames[2],ColorCB   ,(void *) this, ADDLAB);
 custpd->addPush(cnames[3],ColorCB   ,(void *) this, DELLAB);

// Push Buttons for the Modeling pulldown
 modelpd->addPush(mnames[0],RayCB    ,(void *) this, RAYMOD);
 modelpd->addPush(mnames[1],TimeDepthCB ,(void *) this, TDMOD);

// Push Buttons for the Help pulldown
 helppd->addPush(hnames[CTX],HelpCVM ,(void *) this,CTX);
 helppd->addPush(hnames[OVERVIEW],HelpCVM ,(void *) this,OVERVIEW);
 add_cshelpcb(helppd->getWidget(CTX),Hctx);

}

void CvmApp::buildPlotArea()
{/**********************************************
 * Create SeisPlot and SeisLocOut objects    **
 * Create form to contain other things.      **
 *********************************************/
 SLCvmInform *cvminform;
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

 cvminform = new SLCvmInform(this,_sp); 
 if(_cando_underlay)
  {_spund  = new SeisPlotUnder(_sp);
   _spund->setHelpCtx(getHelpCtx());
   _spund->setSeisAnnotation(0);
  }

}

void CvmApp::buildPopups()
{
 _select = NULL;
 _sgpop  = NULL;
 _cb_pop = NULL;
 _color_pop=NULL;
 _hrdcpy = NULL;
 _select = new SeisSelectPop(mainWindow(),"Select",getHelpCtx(),_sp);
 _select->make(mainWindow());
 _sgpop = new SeisGridPop(mainWindow(),"GridPop",_sp,getHelpCtx());
 _sgpop->make(mainWindow());
// _hrdcpy = new HardCopySeisPop(mainWindow(),"HardCopy",getHelpCtx(),_sp,
//   1.0,True);

 _sp->setLoadableColors((int) _resources.num_col_colors );
 if (_cando_colors)
  {_color_pop = new SeisColorPop(mainWindow(),"color_pop",
                _sp, getHelpCtx());
   _color_pop->make();
   _cb_pop    = new SeisCbarPop(mainWindow(),"cbpop",_sp,getHelpCtx());
   _cb_pop->make(NULL);
   if (_spund && _color_pop) _color_pop->addSP( _spund);
//   if (_spund && _cb_pop) _cb_pop->addSP( _spund);
  }

}

void CvmApp::cvmGuiUpdate()
{
 if(_mlimdata) _mlimdata->mlimitsGuiUpdate(cvmGetMLimits());
 if(_glimdata) glimitsInit(_glimdata);
}

void CvmApp::NewCVM(void *data, long ident)
{CvmApp *cvmapp = (CvmApp *) data;
 if(cvmapp == NULL || ident != NEWCVM) return;
 if(cvmapp->_new_pop)
  { cvmapp->_new_pop->makeAndManage();
    return;
  }

 cvmapp->_new_pop = (SLDialog *) NewGUI(cvmapp->mainWindow(),
                    cvmapp, cvmapp->getHelpCtx()); 
 if(!cvmapp->_new_pop)
  { 
   CVMMsgBox(cvmapp->mainWindow(),"NewCVM: no dialog to start new");
   return;
  }
  
}

void CvmApp::startNewModel(ErsModel *model)
{/* Start a new model from an exisiting model which
  * was probably read in from a disk file
  */
 SeisVectLinkedList *svll;
 SeisTransf   *sptrans;
 gridData     *gdold=NULL;

 ErsTransform  *tx=NULL,*ty=NULL,*tz=NULL,*tzs=NULL;
 int            j;
// Default Coordinates - see transform.h for definitions
 int           phd=XBASEHDR_,shd=YBASEHDR_,zeit=TIMEHDR_;
 int           szeit=TIMEHDR_;
 float         uc[4],ymin,ymax;
 if(!model) { startNewModel("NONE"); return; }

// save seismic z-coord before killing old model
 sptrans = (SeisTransf *) _sp->transform();
 if(sptrans) tzs = (ErsTransform *) sptrans->getTZsplot();
 if(tzs) szeit = (int) transform_gethdr(tzs);
 gdold = cvmGetGridData();

 killOldModel(0);

 if(_sp->plotType() >= PlotImage::PlotCOLOR)
  {/* Honor preexisting seismic coordinates. */
   /* Reset coordinate system for Grid plots */
   mlimits_get(model_getmlim(model),&uc[0],&uc[1],
     &uc[2], &uc[3], &ymin, &ymax,
     &tx, &ty, &tz );
   set_user_win(uc[0],uc[1],uc[3],uc[2]);
   _sp->plot();
  }

 _moddesc = new ModelDesc(model);
 _moddesc->setGridData(gdold);
 _moddesc->gettrans(&tx,&ty,&tz);
 if(tx) phd = (int ) transform_gethdr(tx);
 if(ty) shd = (int ) transform_gethdr(ty);
 if(tz) zeit= (int ) transform_gethdr(tz);

 _data_user_pr = new VLDataUser(cvm_get_vldata(Structure));
 _data_user_mats = new VLDataUser(cvm_get_vldata(Materials));
// Set SeisTransf for all instances of SeisPlot
 cvmSPTransform(tx,ty,tz,tzs);
 if(gdold) gdold->gridDataTrans(_moddesc->transforms(),
           transform_getname(tx),
           transform_getname(ty),
           transform_getname(tz));

// Connect lists for root instance of SeisPlot
 svll = (SeisVectLinkedList *) cvm_get_vll(CvmAppBase::Structure);
 if(svll  ) svll->addPlot((SeisPlot *) _sp);
 svll = (SeisVectLinkedList *) cvm_get_vll(CvmAppBase::Materials);
 if(svll  ) svll->addPlot((SeisPlot *) _sp);
 svll = (SeisVectLinkedList *) cvm_get_vll(CvmAppBase::Boundarys);
 if(svll  ) svll->addPlot((SeisPlot *) _sp);
 svll = (SeisVectLinkedList *) cvm_get_vll(CvmAppBase::Cpointers);
 if(svll  ) svll->addPlot((SeisPlot *) _sp);

 cvmGuiUpdate();
// Connect list for remote instances of SeisPlot
 j = connectLists();
}

void CvmApp::startNewModel(char *name)
{// Start new model from scratch
 ModelDesc     *mod=NULL;
 gridData      *gdold=NULL;
 SeisTransf    *sptrans=NULL;
 ErsTransforms *tdata=NULL;
 ErsTransform  *tx=NULL,*ty=NULL,*tz=NULL,*tzs=NULL;
// Default Coordinates - see transform.h for definitions
 int phd=XBASEHDR_,shd=YBASEHDR_,mzeit=TIMEHDR_,szeit=TIMEHDR_,i;

// Preserve coordinate & grid info from old model
 sptrans = (SeisTransf *) _sp->transform();
 if(sptrans) tzs = (ErsTransform *) sptrans->getTZsplot();
 if(tzs) szeit= (int ) transform_gethdr(tzs);
 if(_moddesc)
   {_moddesc->gettrans(&tx,&ty,&tz);
    if(tx) phd = (int ) transform_gethdr(tx);
    if(ty) shd = (int ) transform_gethdr(ty);
    if(tz) mzeit= (int ) transform_gethdr(tz);
    gdold = _moddesc->GridData();
   }

// Start a New Model
  mod = new ModelDesc();
  if(!mod) return;
  transforms_copy(_moddesc->transforms(), mod->transforms());
  mod->setName(name);
  mod->setGridData(gdold);

// Destroy old model if there is one
  killOldModel(1);

// Get ErsTransforms and set ModLimits and Gridlimits 
  _moddesc = mod;
  tdata = _moddesc->transforms();
  tx = ErsTransByID(tdata,phd);
  ty = ErsTransByID(tdata,shd);
  tz = ErsTransByID(tdata,mzeit);
  mlimits_set_trans(_moddesc->MLimits(),tx,ty,tz);
  glimits_set_trans(_moddesc->GLimits(),tx,ty,tz);
  if(gdold) gdold->gridDataTrans(_moddesc->transforms(),
           transform_getname(tx),
           transform_getname(ty),
           transform_getname(tz));

// Set some new dummy lists to get started
  i = cvm_bld_vlists(phd,shd,mzeit,szeit);
  CVMMsgBox(this->mainWindow(),"Old model destroyed - New is started");
 return;
}

void CvmApp::killOldModel(int rm)
{
 void *list;
 /****************************************************
 * Destroy the current popups with wbox tables or  ***
 * that are connected to vector data.              ***
 ****************************************************/
 if(_hzedit != NULL)
  { delete (HorizEditPop *) _hzedit;
    _hzedit = NULL;
  }
 if(_struc != NULL)
  { delete (HorizEditPop *) _struc;
    _struc = NULL;
  }
 if(_celldata)
  {delete (CellEditPop *) _celldata;
   _celldata = NULL;
  }
 if(_pickdata)
  {delete _pickdata;
   _pickdata = NULL;
  }

 /********************************************
 * Destroy old SeisVectLinkedList and      ***
 * VectorListData data objects.            ***
 ********************************************/
  list =  cvm_get_vll(CvmAppBase::Structure);
  KillVectorList((void *) list, _sp);
  list =  cvm_get_vll(CvmAppBase::Materials);
  KillVectorList((void *) list, _sp);
  list =  cvm_get_vll(CvmAppBase::Boundarys);
  KillVectorList((void *) list, _sp);
  list =  cvm_get_vll(CvmAppBase::Cpointers);
  KillVectorList((void *) list, _sp);
  if(_moddesc) delete _moddesc;
  _moddesc = NULL;

  delete _data_user_pr;
  delete _data_user_mats;
  _data_user_pr = NULL;
  _data_user_mats = NULL;

  if(_transpop)
  { delete (CoordEditPop *) _transpop;
    _transpop = NULL;
  }
 /****************************************************
 * Blow away the old model                         ***
 ****************************************************/
 if(rm > 0)
  {
   destroy_model(_ersmod);
   _ersmod = NULL;
  }
}

void CvmApp::OpenCVM(void *data, long ident )
{CvmApp *cvmapp = (CvmApp *) data;
 /* Open an Old Model */
 Widget form;
 int option;
 if(cvmapp == NULL || ident != OPENCVM) return;
 option = 0; /* returns form in a dialog */
 if(cvmapp->_openform) 
  { XtManageChild( cvmapp->_openform);
    return;
  }
 form = OpenGUI(option,cvmapp->mainWindow(),cvmapp->getHelpCtx(),cvmapp);
 if(form == NULL) return;
 XtAddCallback(form,XmNdestroyCallback,(XtCallbackProc) OpenDes,cvmapp);
 XtManageChild(form);
 cvmapp->_openform = form;
 return;
}
void CvmApp::OpenDes(Widget/*W*/, CvmApp *cvmapp, caddr_t /*b*/)
{ //cvmapp->_filepd->sensitive(True,OPENCVM,-1);
  cvmapp->_openform = 0;
}

void CvmApp::Shutdown()
{
 delete this;
 exit(0);
}

void CvmApp::GridCVM(void *data , long ident)
{CvmApp *cvmapp = (CvmApp *) data;
 SLSmartForm  *form;
 SLDialog *gshell=0;
 void   *gdata;
 int     option;
 if(cvmapp == NULL || ident != GRID1) return;

 option = 0; /* returns form in a dialog */
 if(cvmapp->_griddata == NULL)
  {gdata = gridGUI(option,cvmapp->mainWindow(),cvmapp->getHelpCtx(),
           (void *) cvmapp);
   if(gdata != NULL)
    {form = gridForm(gdata);
     XtAddCallback(form->W(),XmNdestroyCallback,(XtCallbackProc) GridDes,cvmapp);
     cvmapp->_griddata = gdata;
    }
  }
 else
  form = gridForm(cvmapp->_griddata);
 if(form == NULL) return;
 gshell = gridDial(cvmapp->_griddata);
 if(gshell) gshell->makeAndManage();
 gridUpdate(cvmapp->_griddata);
 return;
}
void CvmApp::GridDes(Widget , CvmApp *cvmapp, caddr_t )
{
 cvmapp->_griddata = NULL;
}

/*******************************************************
 * Create or remanage popup to control Transforms      *
 ******************************************************/
void CvmApp::TransfCB(void *data, long ident )
{CvmApp *cvmapp = (CvmApp *) data;
 CoordEditPop *pop;
 if(cvmapp == NULL || ident != TRANS1) return;

 if(!cvmapp->cvmGetCDdata()) return;

 if(cvmapp->_transpop==NULL)
   {cvmapp->_transpop = 
      (void *) new CoordEditPop(cvmapp->mainWindow(),
                   "TransEdit",cvmapp->getHelpCtx(),cvmapp->cvmGetCDdata());
   }
 pop = (CoordEditPop *) cvmapp->_transpop;
 if(pop) pop->makeAndManage();
 return;
}
void CvmApp::TransfDes()
{// delete the transforms popup
 if(!_transpop) return;
 delete (CoordEditPop *) _transpop;
 _transpop = NULL;
}

/*******************************************************
 * Create or remanage popup to control ModelLimits     *
 * Control sensitivity of the pushbutton.              *
 ******************************************************/
void CvmApp::MlimitsCB(void *data, long ident )
{CvmApp *cvmapp = (CvmApp *) data;
 SLDialog *dial;
 Widget   form;
 Mlimits  *mdata;
 if(cvmapp == NULL) return;

 if(cvmapp->_mlimdata == NULL || ident != MLIM1)
  {
   mdata = new Mlimits(cvmapp->mainWindow(), cvmapp,cvmapp->getHelpCtx());
   cvmapp->_mlimdata = mdata;
   form = mdata->Form();
   if(form != NULL) 
    XtAddCallback(form,XmNdestroyCallback,(XtCallbackProc) MlimitsDes,cvmapp);
  }
 else
  {mdata = cvmapp->_mlimdata;
   mdata->mlimitsInit();
  }

 dial = mdata->Dial();
 if(dial)
  {
   dial->makeAndManage();
//   mlimitsReset(cvmapp->_mlimdata);
  }

 return;
}
void CvmApp::MlimitsDes(Widget , CvmApp *cvmapp, caddr_t )
{cvmapp->_mlimdata = NULL;
}

/*******************************************************
 * Create/remanage popup to control Coordinate system  *
 ******************************************************/
void CvmApp::CoordCB(void *data, long ident )
{CvmApp *cvmapp = (CvmApp *) data;
 CoordChange *cdata;
 SLDialog *dial;
 Widget   form;
 if(cvmapp == NULL || ident != COORD1) return;
 if(!cvmapp->_moddesc->coord_data()) return;

 if(cvmapp->_csysdata == NULL)
  {
   cdata = new CoordChange(cvmapp->mainWindow(), cvmapp,cvmapp->getHelpCtx());
   cvmapp->_csysdata = cdata;
   if(cdata) form = cdata->Form();
   if(form != NULL) 
    XtAddCallback(form,XmNdestroyCallback,(XtCallbackProc) CoordDes,cvmapp);
  }
 else
  {cdata = (CoordChange *) cvmapp->_csysdata;
   cdata->Init();
  }

 if(cdata) dial = cdata->Dial();
 if(dial)
  {
   dial->makeAndManage();
  }

 return;
}
void CvmApp::CoordDes(Widget , CvmApp *cvmapp, caddr_t )
{cvmapp->_csysdata = NULL;
}

/*******************************************************
 * Create or remanage popup to control GridLimits      *
 ******************************************************/
void CvmApp::GlimitsCB(void *data, long ident )
{CvmApp      *cvmapp = (CvmApp *) data;
 GridLimits  *globj=NULL;
 SLDialog    *dial;
 SLSmartForm *sform;
 Glimits  *gdata;
 if(cvmapp == NULL || ident != GLIM1) return;

 if(cvmapp->_glimdata == NULL)
  {
   gdata = glimitsGui(cvmapp, 0,cvmapp, cvmapp->getHelpCtx());
   cvmapp->_glimdata = gdata;
   sform = glimitsForm(gdata);
   if(sform) 
    XtAddCallback(sform->W(),XmNdestroyCallback,(XtCallbackProc)  GlimitsDes,cvmapp);
  }
 else
  gdata = (Glimits *) cvmapp->_glimdata;
 
 if(glimitsDial((void *) gdata))
  {
   dial = glimitsDial((void *) gdata);
   if(dial) dial->makeAndManage();
   if(cvmapp->_moddesc) globj = cvmapp->_moddesc->GLimits();
   glimitsGuiUpdate(cvmapp->_glimdata,globj);
  }
 return;
}
void CvmApp::GlimitsDes(Widget W, CvmApp *cvmapp, caddr_t b)
{cvmapp->_glimdata = NULL;
  if(b || W) return; // kluge to suppress warning
}

/*******************************************************
 * Create or remanage popup to control Plotting        *
 * Control sensitivity of the pushbutton.              *
 ******************************************************/
void CvmApp::DisplayCB(void *data, long ident )
{CvmApp *cvmapp = (CvmApp *) data;
 Widget   form;
 if(cvmapp == NULL || ident != DISP1) return;

 form = cvmapp->_display;
 if(form == NULL) 
  {form = DispGUI(0, cvmapp->mainWindow(), cvmapp->getHelpCtx(), cvmapp);
   cvmapp->_display = form;
   XtAddCallback(form,XmNdestroyCallback,(XtCallbackProc)  DisplayDes,cvmapp);
  }
 if(form != NULL)
  {
   XMapWindow(XtDisplay(XtParent(form)),XtWindow(XtParent(form)));
   XtManageChild(form);
  }
 return;
}
void CvmApp::DisplayDes(Widget , CvmApp *cvmapp, caddr_t )
{ cvmapp->_display = NULL;
}


/*******************************************************
 * Create or remanage popup to control Cells           *
 * Control sensitivity of the pushbutton.              *
 ******************************************************/
void CvmApp::TestCVM(void *data , long ident)
{CvmApp *cvmapp = (CvmApp *) data;
 TimeDepthPop *tdpop=NULL;
 if(cvmapp == NULL || ident != TEST1) return;
 if(!cvmapp->_test)
  {
   tdpop = new TimeDepthPop(cvmapp->mainWindow(),"tdcalc",
         cvmapp->getHelpCtx(),cvmapp);
   cvmapp->_test = (void *) tdpop;
  }
 else
   tdpop= (TimeDepthPop *) cvmapp->_test;
 if(tdpop) tdpop->makeAndManage();
}

void CvmApp::RayCB(void *data , long ident)
{CvmApp *cvmapp = (CvmApp *) data;
 if(cvmapp == NULL || ident != RAYMOD) return;
 if(!cvmapp->_raypop)
  {
   cvmapp->_raypop = new RayEditPop(cvmapp->mainWindow(),"ryedit",
           cvmapp->getHelpCtx(),cvmapp);
  }
 if(cvmapp->_raypop) cvmapp->_raypop->makeAndManage();
}

void CvmApp::TimeDepthCB(void *data , long ident)
{CvmApp *cvmapp = (CvmApp *) data;
 if(cvmapp == NULL || ident != TDMOD) return;
 if(!cvmapp->_tdpop)
  {
   cvmapp->_tdpop = new TimeDepthPop(cvmapp->mainWindow(),"tdcalc",
         cvmapp->getHelpCtx(),cvmapp);
  }
 if(cvmapp->_tdpop) cvmapp->_tdpop->makeAndManage();
}


void CvmApp::CellCB(void *data, long ident )
{CvmApp *cvmapp = (CvmApp *) data;
 CellEditPop *cepop;
 Widget   form;
 if(cvmapp == NULL || ident != CELL1) return;

 form = NULL;
 if(cvmapp->_celldata == NULL)
  {cepop = new CellEditPop(cvmapp->mainWindow(),"cpedit",
         cvmapp->getHelpCtx(),cvmapp);
   form = cepop->Form();
   if(form != NULL)
    XtAddCallback(form,XmNdestroyCallback,(XtCallbackProc)  CellDes,cvmapp);
   cvmapp->_celldata = (void *) cepop;
  }
 else
  {cepop= (CellEditPop *) cvmapp->_celldata;
  }

 if(cepop) cepop->makeAndManage();
 return;
}
void CvmApp::CellDes(Widget , CvmApp *cvmapp, caddr_t )
{ 
  cvmapp->_celldata = NULL;
}

/*******************************************************
 * Create or remanage popup to control Velocitys       *
 ******************************************************/
void CvmApp::VelocityCB(void *data, long ident )
{CvmApp *cvmapp = (CvmApp *) data;
 VectorListData *vld=NULL;
 /* Create and/or manage the Display popup */
 HorizEditPop *hzeditpop;
 if(cvmapp == NULL || ident != VELO1) return;

 vld = cvmapp->cvm_get_vldata(Materials);
 if(cvmapp->_hzedit == NULL)
  cvmapp->_hzedit = (void *) new HorizEditPop(cvmapp->mainWindow(),"HzEdit",
                   cvmapp->getHelpCtx(),(void *) vld);
 hzeditpop = (HorizEditPop *) cvmapp->_hzedit;
 if(hzeditpop != NULL)
  { hzeditpop->makeAndManage();
    hzeditpop->_hzedit->PassRepFunc( VectorListResetLabel);
    hzeditpop->_hzedit->PassRepData((void *) cvmapp->cvm_get_vll(CvmAppBase::Cpointers));
  }
 return;
}
/*******************************************************
 * Create or remanage popup to control Structure       *
 ******************************************************/
void CvmApp::StructCB(void *data, long ident )
{CvmApp *cvmapp = (CvmApp *) data;
 VectorListData *vld=NULL;
 /* Create and/or manage the Display popup */
 HorizEditPop *hzeditpop;

 if(cvmapp == NULL || ident != STRUCT1) return;

 vld = cvmapp->cvm_get_vldata(Structure);
 if(cvmapp->cvm_get_struc(cvmapp) == NULL)
  cvmapp->_struc = (void *) new HorizEditPop(cvmapp->mainWindow(),"StrucEdit",
                   cvmapp->getHelpCtx(),(void *) vld);
 hzeditpop = (HorizEditPop *) cvmapp->_struc;
 if(hzeditpop != NULL)
  { hzeditpop->makeAndManage();
  }
 return;
}

/*******************************************************
 * Create or remanage popup to control remote displays *
 ******************************************************/
void CvmApp::AddDispCB(void *data, long ident )
{CvmApp *cvmapp = (CvmApp *) data;
 /* Create and/or manage the remote display popup */
//CvmAppBase    *app;
//char     dname[48];
//int i;
 if(cvmapp == NULL || ident != ADDDISP) return;

 if(cvmapp->_remdisp)
  { cvmapp->_remdisp->makeAndManage();
    return;
  }

 cvmapp->_remdisp = (SLDialog *) RemoteGUI(cvmapp->mainWindow(),
                    cvmapp, cvmapp->getHelpCtx()); 
 if(!cvmapp->_remdisp)
  { 
   CVMMsgBox(cvmapp->mainWindow(),"AddDispCB: no dialog to add remote");
   return;
  }
  
 return;
}

void CvmApp::HardCopy(void * data, long ident)
{CvmApp *cvmapp = (CvmApp *) data;
 /* Hardcopy of the display */
 if(cvmapp == NULL) return;
 if(ident == HCPY)
  { 
   if(cvmapp->_hrdcpy != NULL) cvmapp->_hrdcpy->makeAndManage();
  }
}


CvmAppBase *CvmApp::addDisp(char *dname)
{
 /* Create and/or manage the remote display popup */
 CvmAppBase    *app;
 SeisPlot *sp_boss, *sp_rem;
 SeisPlotUnder *spund_boss, *spund_rem;
 int j, stat;

 if(dname == NULL) return NULL;
 if(_num_plots > 2) return NULL;
 if(strcmp(dname,"NONE")==0)  return NULL;
 app = (CvmAppBase *)  new CvmAppBase(dname,
                  XtWidgetToApplicationContext( this->W()));
 if(app)
  {
   if(app->display()==NULL)
    { delete app;
     CVMMsgBox(mainWindow(),"addDisp: display was NULL");
     return NULL;
    }
   app->setup();
   app->set_notify( RemDes, (void *) this);
  }
 else
   return NULL;
// Update the count of plots.
 if(_num_plots==0)
  {_plots = (CvmAppBase **) calloc(1,sizeof(CvmAppBase *));
  }
 else
  {_plots = (CvmAppBase **) realloc(_plots,
                       (size_t) (_num_plots+1) * sizeof(CvmAppBase *));
  }
 _plots[_num_plots] = app;
 _num_plots += 1;
 
 if(app)
   {//VectorListData *vldata;
   sp_boss    = (SeisPlot *) getSeisPlot();
   sp_rem     =  (SeisPlot *) app->getSeisPlot();
   spund_boss = (SeisPlotUnder *) getSeisPlotUnder();
   spund_rem  =  (SeisPlotUnder *) app->getSeisPlotUnder();
   *sp_rem    = *sp_boss; // overloaded = operator
   app->setMessage("plotting overlay data on remote");
   sp_rem->plot();  // plot the seismic data
   app->setMessage("");
   j= connectLists(app);  // attach vectors to plot 
   if(spund_rem) 
    { app->setMessage("plotting underlay data on remote");
      stat = updateOneRemoteUnd(app);
      app->setMessage("");
    }
  }

 return (CvmAppBase *) app;
}

void CvmApp::updateRemoteUnders()
{int       i,stat;
 CvmApp    *app;
 if(!_num_plots) return;

 for(i=0;i<_num_plots;i++)
  {
   app = (CvmApp *) _plots[i];
   if(!app) break;
   stat = updateOneRemoteUnd(app);
  }
}

int CvmApp::updateOneRemoteUnd(CvmAppBase *cvm_remote)
{int j,matchHdr;
 SeisPlotUnder *spund_rem;
 SeisPlot      *sp_rem;
 gridcvm       *gcvm;
 gridData      *gdat;
 void          *garr=NULL;
 ErsTransform  *t1,*t2,*t3;
 float         d1=1.0,d2=1.0,d3=1.0,o1=0.0,o2=0.0,o3=0.0;
 int           n1=1,n2=1,n3=1;
 int           bypass=0;

 if(!cvm_remote) return 0;
 spund_rem  =  (SeisPlotUnder *) cvm_remote->getSeisPlotUnder();
 sp_rem     =  (SeisPlot *) cvm_remote->getSeisPlot();

 if(!bypass) *spund_rem = *_spund;     // copy from root underlay
// spund_rem->initArrayTypePlot();
 spund_rem->setSeisAnnotation(0);
 gcvm = (gridcvm *) _griddata; // data of boss app
 gdat = gridDataPntr(gcvm);
 if(gdat && spund_rem)
  {float X1,X2,Z1,Z2;
   int   p1,p2;

   garr    = gdat->getGridData();
   gdat->getGridVals(&n1,&o1,&d1,&n2,&o2,&d2,&n3,&o3,&d3);
   gdat->getGridCoords(&t1,&t2,&t3);
   X1 = o2;
   X2 = X1 + (n2 - 1)*d2;
   Z1 = o1;
   Z2 = Z1 + (n1 - 1)*d1;
   p1 = (int) sp_rem->xPixel(X1);
   p2 = (int) sp_rem->xPixel(X2);

   matchHdr = (int) transform_gethdr(t2);
   spund_rem->setMatchHeader(matchHdr);
   sp_rem->setMatchHeader(matchHdr);

   if(matchHdr > 0) matchHdr -= 1;
   if(p1>p2)
    {spund_rem->setGridXYS(X2,X1,Z1,Z2);
     spund_rem->setTmin(Z1);
     spund_rem->setTmax(Z2);
//     spund_rem->initArrayTypeData(1,1,n2,n1,(float *) garr);
     for(j=0;j<n2;j++)
      spund_rem->setHeader(j*spund_rem->numHeaders()+matchHdr,X2-j*d2);
    }
   else
    {
     spund_rem->setGridXYS(X1,X2,Z1,Z2);
     spund_rem->setTmin(Z1);
     spund_rem->setTmax(Z2);
//     spund_rem->initArrayTypeData(1,1,n2,n1,(float *) garr);
     for(j=0;j<n2;j++)
      spund_rem->setHeader(j*spund_rem->numHeaders()+matchHdr,X1+j*d2);
    }

   cvm_remote->setMessage("plotting underlay");
   spund_rem->plotArrayTypeToSeismic(spund_rem->matchHeader());
   cvm_remote->setMessage("");
   return 1;
  }
 else return 0;
}

int  CvmApp::connectLists()
{/* Connect Structure and Material lists to remote displays.
  * Assuming cvmSPTransform was called earlier to set SeisTransf
  */
 CvmApp        *app;
 int            i,j=0;
 if(!_num_plots) return 0;
 for(i=0;i<_num_plots;i++)
  {
   app = (CvmApp *) _plots[i];
   if(!app) break;
   connectLists(app);
   j++;
  }
 return j;
}

int  CvmApp::connectLists(CvmAppBase *app)
{/* Connect Structure and Material lists to a remote display.
  * Assuming cvmSPTransform was called earlier to set SeisTransf
  */
 SeisPlot       *sp_rem;
 VectorListData *vldata;
 SeisVectLinkedList *vlist;
 ErsTransform   *tx=NULL,*ty=NULL,*tz=NULL,*tzs=NULL;
 int             phd=XBASEHDR_,shd=YBASEHDR_,thd=NILKEY;
 SeisTransf     *sptrans=NULL;

 if(!app) return 0;
 if(!_num_plots) return 0;
 sptrans = (SeisTransf *) _sp->transform();
 if(sptrans) tzs = (ErsTransform *) sptrans->getTZsplot();
 if(_moddesc) _moddesc->gettrans(&tx,&ty,&tz);
 if(tx) phd = (int ) transform_gethdr(tx);
 if(ty) shd = (int ) transform_gethdr(ty);
 if(phd<1) phd = XBASEHDR_;

 if(app->PickingActive())
   {//halt active picking on remote display
     app->StopPicking();
   }

 sp_rem =  (SeisPlot *) app->getSeisPlot();
 sptrans = (SeisTransf *) SeisTransfXToSP((void *) sp_rem,phd,shd,thd,
             (void *) tzs,(void *) tz);

 vldata = (VectorListData *) cvm_get_vldata(CvmAppBase::Structure);
 if(vldata)
   {vlist  = (SeisVectLinkedList *) vldata->getDataObject();
    vlist->addPlot((SeisPlot *) sp_rem);
   }

 vldata = (VectorListData *) cvm_get_vldata(CvmAppBase::Materials);
 if(vldata)
   {vlist  = (SeisVectLinkedList *) vldata->getDataObject();
    vlist->addPlot((SeisPlot *) sp_rem);
   }

 app->setModelDescPntr(_moddesc);
 app->setOwnsModel(0);
 ((CvmApp *) app)->_data_user_pr =  _data_user_pr;
 ((CvmApp *) app)->_data_user_mats = _data_user_mats;
   
 return 1;
}

void CvmApp::RemDes(void *appbase,void *app)
{int i,j=0;
 CvmApp *cvmapp = (CvmApp *) app;
 CvmAppBase **bplt,*appx;
 if(cvmapp->_num_plots < 1) return;
 bplt = NULL;
 if(cvmapp->_num_plots > 1)
  bplt = (CvmAppBase **) calloc(cvmapp->_num_plots - 1,sizeof(CvmAppBase *));

 for(i=0;i<cvmapp->_num_plots;i++)
  { appx = cvmapp->_plots[i];
    if (appx != appbase)
     {bplt[j] = appx;
      j++;
     }
  }
 if(cvmapp->_plots) free(cvmapp->_plots);
 cvmapp->_plots = bplt;
 cvmapp->_num_plots -= 1;
}

int CvmApp::cvmTransform(char *xlab,char *ylab, char *zlab, char *slab)
{/* Transform data objects to the target coord. systems.
  xlab ... horizontal coordinate name for layer model.
  ylab ... normal coordinate name for layer model.
  zlab ... vertical coordinate name for layer model.
  slab ... vertical coordinate name for seismic data.
 */
 ErsTransforms  *tdata;
 ErsTransform   *tx=NULL,*ty=NULL,*tz=NULL,*tzs=NULL;

 tdata   = cvmGetTransforms();
 if(tdata==NULL) return 0;

 if(!cvmTransform(xlab,ylab,zlab)) return 0;

// Now reset the transform function for SeisPlot for
// root and remote displays.
 tx  = ErsTransGetTran(tdata,xlab);
 ty  = ErsTransGetTran(tdata,ylab);
 tz  = ErsTransGetTran(tdata,zlab);
 tzs = ErsTransGetTran(tdata,slab);
 cvmSPTransform(tx, ty, tz, tzs);

// Update GUIs that are not DataUsers.
 cvmGuiUpdate();
 return 1;
}

int CvmApp::cvmTransform(char *xlab,char *ylab, char *zlab)
{// Transform the model to the target coordinate systems.
 // New system labeled by xlab,ylab,zlab.
 // No transforms made unless all are legitimate.
 ErsTransforms  *tdata;
 ErsTransform   *txo=NULL,*tyo=NULL,*tzo=NULL;
 VectorListData *list1,*list2,*list3,*list4;
 int             ierr;

 tdata   = cvmGetTransforms();
 if(tdata==NULL) return 0;
 txo = ErsTransGetTran(tdata,xlab);
 tyo = ErsTransGetTran(tdata,ylab);
 tzo = ErsTransGetTran(tdata,zlab);
 if(txo==NULL || tyo==NULL || tzo==NULL) return 0;

// Transform the GridLimits
// Rescales the data and sets pointers to the new transforms
 ierr = glimits_trans(cvmGetGLimits(),tdata,xlab,ylab,zlab);

// Transform the VectorListData objects.
// These contain the SeisVectLinkedList's
 list1= cvm_get_vldata(CvmAppBase::Structure);
 list2= cvm_get_vldata(CvmAppBase::Materials);
 list3= cvm_get_vldata(CvmAppBase::Boundarys);
 list4= cvm_get_vldata(CvmAppBase::Cpointers);
 if(list1) list1->transform(tdata,xlab,ylab,zlab);
 if(list2) list2->transform(tdata,xlab,ylab,zlab);
 if(list3) list3->transform(tdata,xlab,ylab,zlab);
 if(list4) list4->transform(tdata,xlab,ylab,zlab);

// Transform the ModLimits
// This is the last step since the rest of the Model
// is supposed to be consistent with its coordinate sys.
 ierr = mlimits_trans(cvmGetMLimits(),tdata,xlab,ylab,zlab);

 return 1;
}

int  CvmApp::cvmSPTransform(SeisPlot *sp, ErsTransform *tx, ErsTransform *ty,
             ErsTransform*tz,ErsTransform *tzs)
{
 SeisTransf   *sptrans;
// Default Coordinates - see transform.h for definitions
 int           phd=XBASEHDR_,shd=YBASEHDR_,thd=NILKEY;

 if(!sp) return 0;
 if(tx) phd = (int) transform_gethdr(tx);
 if(ty) shd = (int) transform_gethdr(ty);
 if(phd<1) phd = 17;
 if(shd<1) shd = 18;
 sptrans = (SeisTransf *) SeisTransfXToSP((void *) sp,phd,shd,thd,
             (void *) tzs,(void *) tz);
 if(!sptrans) return 0;
 return 1;
}

int  CvmApp::cvmSPTransform(ErsTransform *tx, ErsTransform *ty,
             ErsTransform*tz,ErsTransform *tzs)
{
// Now set the transform functions for all SeisPlot 
// displays to be consistent with the argument list
 CvmApp       *app;
 SeisPlot     *sp;
 int           i,j=0;

// root SeisPlot
 sp = _sp;
 this->cvmSPTransform(sp,tx,ty,tzs,tz);

// Now for remote SeisPlots's
 for(i=0;i<_num_plots;i++)
  {app = (CvmApp *) _plots[i];
   if(!app) break;
   sp =  (SeisPlot *) app->getSeisPlot();
   j += app->cvmSPTransform(sp,tx,ty,tzs,tz);
  }
 return j;
}

void CvmApp::cvmSetVis(int vis)
{// set visibility of the vector lists used by CvmApp.
 SeisVectLinkedList *list1,*list2,*list3,*list4;
 list1= (SeisVectLinkedList *) cvm_get_vll(CvmAppBase::Structure);
 list2= (SeisVectLinkedList *) cvm_get_vll(CvmAppBase::Materials);
 list3= (SeisVectLinkedList *) cvm_get_vll(CvmAppBase::Boundarys);
 list4= (SeisVectLinkedList *) cvm_get_vll(CvmAppBase::Cpointers);
 if(vis)
  {
   if(list1) list1->makeVisible();
   if(list2) list2->makeVisible();
   if(list3) list3->makeVisible();
   if(list4) list4->makeVisible();
  }
 else
  {if(list1) list1->makeInvisible();
   if(list2) list2->makeInvisible();
   if(list3) list3->makeInvisible();
   if(list4) list4->makeInvisible();
  }
}

int CvmApp::cvmUpdateModel()
{
 ErsModel     *model=NULL;

 if(_moddesc) model = _moddesc->toErsModel();
 if(model) cvm_setmod(this,model);

 return 1;
}
 
