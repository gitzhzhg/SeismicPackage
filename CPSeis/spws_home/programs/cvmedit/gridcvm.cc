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
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/Separator.h>
#include "sl/shell_watch.hh"
#include "sl_cvm_app.hh"
#include "sp/seis_plot_under.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_dialog.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_row_column.hh"
#include "sl/radio_list.hh"
#include "sl/slp_push.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_file.hh"
#include "seis_transf.hh"
#include "wproc.h"
#include "file_choice.h"
#include "tf_global.h"
#include "tfdefs.h"
#include "netw.h"
#include "rmodc.h"
#include "wrdcnvrt.h"
#include "glimits1.h"
#include "grid_save.h"
#include "gridcvm.h"
#include "model_trans.h"
#include "modgrid_frou.h"

#define READ 0
#define LTOG 2
#define GTOL 3
#define NOGR 4

enum {GAPPLY, GCAN};
enum {HIDE, SHOW};

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

int  grid_comp(ErsModel *model,
     int  *nx, float *xorg, float *dx,
     int  *nz, float *zorg, float *dz, float *garr);
 
int  read_data_fromdisk(int fd, IO_request *ioreq, TF_Global *g,
     char *hd, char *tr);
int read_data_translate(IO_request *ioreq, TF_Global *g,
          char *hd, char *tr, float *TASF);

void rmodopen_w_(int  *, char *,int  *,int  *,int  *,int *,int  *,int  *);

/* PROTOTYPES FOR PRIVATE METHODS */
void saveTrap(void *data, long ident);
void gridTrap(void *data, long ident);
void modeTrap(void *data, long, long oldv, long newv );
void visTrap(void *data, long, long oldv, long newv );
static void KillCB(Widget W, gridcvm *, caddr_t b);
void   CVMMsgBox(Widget , char *);

#ifdef __cplusplus
}                   // for C++
#endif
 
void *gridGUI(int option, Widget parent, HelpCtx Hctx, void * data)
{/* set option = 0 for return widget to be a form in a dialog */
 /* set option != 0 for return widget to be a form */
 CvmApp *cvmapp = (CvmApp *) data;
 gridcvm *gdata;
 int   i;

 static long   tbident[4]= {READ,LTOG,GTOL,NOGR};
 static String tbnames[4]= {"tb1","tb2","tb3","tb4"};
 static String tblabs[4] = {"Read&Display","LayerToGrid",
                            "GridToLayer","Delete Grid"};

 static long   tbident3[2]= {HIDE,SHOW};
 static String tbnames3[2]= {"hide","show"};
 static String tblabs3[2] = {"HideGrid","ShowGrid"};

 SLDialog     *shell;
 SLSmartForm  *work,*glim_form;
 SLpPush      *pbsav;
 SLpPush      *cancel,*apply;
 
/***************************************************
 * Do some sanity checking                        */
 if(parent  == NULL) return NULL;
 if(cvmapp  == NULL) return NULL;

/*************************************************
 * Create a popup shell                         */
 gdata = (gridcvm *) calloc(1,sizeof(gridcvm));
 gdata->shell = NULL;
 gdata->sav_shell = NULL;
 gdata->cvmapp= cvmapp;
 gdata->infcw= NULL;

 if(option == 0)
  {
   shell = new SLDialog(parent,"gshell",Hctx,True);
   if(shell) shell->setTitle("CVM Grid Control");
   gdata->shell = shell;
   work   = shell->workArea();
   apply  = shell->addBottomApply(GAPPLY,gridTrap, (void *) gdata);
   cancel = shell->addBottomCancel(GCAN,gridTrap, (void *) gdata);
   shell->addBottomHelp();
  }
 else return NULL;
 strcpy(gdata->filei,"NONE");
 shell->make();
 gdata->form = work;
/*
 n= 0;
 XtSetArg (arglist[n], XmNautoUnmanage, False); n++;
 XtSetValues(gdata->form,arglist,n);
 */

/*************************************************
 * Add a RowColumn                              */
 SLRowColumn *savrc;
 savrc = new SLRowColumn(work,"savrc",NULL,True, True, True);
 work->attach(savrc,work,work,NULL,work,10,10,0,10);
/************************************************
 * Create radio list for major options         */
 RadioList *radio1 = new RadioList();
/************************************************
 * Create toggle buttons for major options     */
 for(i=0;i<XtNumber(tbnames);i++)
  {radio1->addRadio(savrc, tbnames[i], tbident[i],tblabs[i]);
  }
 radio1->setIvar(LTOG);
 radio1->setItrap(modeTrap, (void *) gdata);
 gdata->mode=LTOG;
 gdata->modRad = radio1;

/*************************************************
 * Add a grid limits display                    */
 gdata->glim = (void *) glimitsGui(work,1,cvmapp,Hctx);
 work->attach(glimitsForm(gdata->glim),work,work,NULL,savrc,10,10,0,10);
 glim_form = glimitsForm(gdata->glim);
/**************************************************
 * Add a control for slice selection.            */
 gdata->xslice = new SL2Text(work, "xslice",0,"Slice Number:",
                 SLpText::_LONG,6);
 work->attach(gdata->xslice,work,NULL,NULL,glimitsForm(gdata->glim),
       10,0,0,10);
 gdata->xslice->setupIvarPoint((long *) &gdata->start);
 gdata->start = 1;
 gdata->slices= 1;

/*************************************************
 * Add a RowColumn                              */
 SLRowColumn *rc2;
 rc2 = new SLRowColumn(work,"visrc",NULL,True, True, True);
 work->attach(rc2,work,NULL,NULL,gdata->xslice,10,0,0,10);
/************************************************
 * Create radio list for visibility options    */
 RadioList *radio2 = new RadioList();
 for(i=0;i<XtNumber(tblabs3);i++)
  {radio2->addRadio(rc2, tbnames3[i], tbident3[i],tblabs3[i]);
  }
 radio2->setIvar(SHOW);
 radio2->setItrap(visTrap, (void *) gdata);
 gdata->visRad = radio2;

/*************************************************
 * PushButton to invoke Save As popup for grid  */
 pbsav = new SLpPush(work->W(),"save",0,"Save Grid As...");
 pbsav->setAtrap(saveTrap,(void *) gdata);
 work->attach(pbsav,work,work,NULL,rc2,10,10,0,10);

/*************************************************
 * Add a FileChoice widget                      */
 gdata->infcw = new SLpFile(work,"FCWIN", 0, "Grid Input...",NULL,
                "hgrid",SLpFile::_INPUT);
 work->attach(gdata->infcw,work,work,NULL,pbsav,10,10,0,10);
 shell->makeAndManage();

 XtAddCallback(work->W(),XmNdestroyCallback,(XtCallbackProc) KillCB,gdata);

 if(gdata->infcw) gdata->infcw->setFilename("NONE");
 gridInit((void *) gdata);


 return (void *) gdata;
}

void gridTrap(void *data, long ident)
{ gridcvm *gcvm = (gridcvm *) data;
 /* Unmanage the graphics window */
 CvmApp        *cvmapp;
 ErsModel      *model;
 ErsTransforms *tdata;
 ErsTransform  *tx,*ty,*tz;
 ErsTransform  *mtx,*mty,*mtz;

 GridLimits *glimitsm;
 gridData   *grid_dobj,*go,*gn;
 float       dx,dy,dz,ox,oy,oz;
 int         nx,ny,nz;

  int      stdo;
  char     text[108],ftype[16];
  char     asrep[6404];
  int      rank,ng[4];
  int      hd[4];
  float    og[4],dg[4];
  char     label1[16];
  char     label2[16];
  char     label3[16];
  char     unit1[16];
  char     unit2[16];
  char     unit3[16];
  char     xyz_order[8];


 void       *garr=NULL;
 Glimits *glim = (Glimits *) gcvm->glim;
 if(gcvm == NULL) return;
 cvmapp = (CvmApp *) gcvm->cvmapp;

 if(ident == GCAN)
  {/* end this popup session */
   gcvm->shell->unmanage();
   /* delete gcvm->shell; */
   return;
  }

 /* Transfer grid limits to model structure. This will be
  * done again when glimits form is destroyed. However we
  * need to update the model before that will happen */
 glimitsTrap(NULL, (Glimits *) glim , NULL);

 glimitsm= cvmapp->cvmGetGLimits();
 tdata = cvmapp->cvmGetTransforms();
 cvmapp->cvm_gettrans(&mtx,&mty,&mtz);

 if(gcvm->slices < 1) gcvm->slices = 1;
 if(gcvm->slices > glim->nyg) gcvm->slices = glim->nyg;
 if(gcvm->start < 1) gcvm->start = 1;
 if(gcvm->start > glim->nyg) gcvm->start = glim->nyg;

 if(ident == GAPPLY )
  {
   ShellWatch sw;
   int   ierr;
   char  hfil[120],msg[160];
   char  dfil[120];
   char  ftype[16];

   if(gcvm->mode == READ)
     {// destroy old grid and read in new grid
      printf("DBG-1\n");
      if(gcvm->infcw) strcpy(gcvm->filei,gcvm->infcw->filename());
      printf("DBG-2\n");
      cvmapp->setMessage("Wait: Reading Grid File");
      printf("DBG-3\n");
      strcpy(hfil,gcvm->filei);
      printf("DBG-4\n");


     stdo = 6;
     modgrid_dfile_c(hfil,&stdo,ftype, dfil);

     // addext_rep_(hfil,"hgrid",&ierr);
     // addext_rep_(hfil,"vodat",&ierr);
      go = gridDataPntr(gcvm);
      if(go) // destroy old data first
       { garr = go->getGridData();
         if(garr) free(garr);
         garr = NULL;
         gridSetDataPntr(gcvm, NULL); // calls gridData destructor
         go=NULL;
       }

      gn = gridRead(hfil,gcvm);
      printf("DBG0\n");
      if(gn)
       gridSetDataPntr(gcvm, gn);
      else 
       { strcpy(msg,"gridcvm: READ OF GRIDDED MODEL FAILED");
         CVMMsgBox(gcvm->form->W(),msg);
         printf("dfil=%s\n",dfil);
         printf("hfil=%s\n",hfil);
         goto error;
       }
      printf("DBG1\n");
      gridPlotUnd(gcvm);
      printf("DBG2\n");
      gn->getGridVals(&nz,&oz,&dz,&nx,&ox,&dx,&ny,&oy,&dy);
      gn->getGridCoords(&tz,&tx,&ty);
      // Update the GUI and the model grid limits
      glimitsSvals(glim,nx,dx,ox,ny,dy,oy,nz,dz,oz);
      glimits_set(glimitsm,&nz,&oz,&dz,&nx,&ox,&dx,&ny,&oy,&dy,
       tz, tx, ty );
      gridUpdate((void *) gcvm); //update grid limits part of display
     }
   else if(gcvm->mode == LTOG)
     {// Given a layer model compute a grid model
      cvmapp->setMessage("Wait: Computing Grid from layer velocities");

      grid_dobj = gridDataPntr(gcvm);
      if(grid_dobj)
       {// Clean up old gridData object
        garr = grid_dobj->getGridData();
        if(garr) free(garr);
        garr = NULL;
        gridSetDataPntr(gcvm, NULL); // calls gridData destructor
        grid_dobj=NULL;
       }
      // Honor grid limits from the GUI.
      glimitsGvals(glim,  &nx, &dx, &ox, &ny, &dy, &oy, &nz, &dz, &oz);
      garr = (void  *) calloc(1,(unsigned int) nx*nz*sizeof(float));
      grid_dobj = new gridData((float *) garr,
          nz, oz, dz, nx, ox, dx, ny, oy, dy, mtz, mtx, mty );
      grid_dobj->setSlice((int) gcvm->start);
      cvmapp->cvmUpdateModel(); //Update model from Vectors
      model = cvmapp->cvm_getmod(cvmapp);
      model_sethfile(model,NULL); // forces no writing
      ierr = grid_comp(model,&nx,&ox,&dx, &nz,&oz,&dz, (float *) garr);
      if(ierr != 0)
       { strcpy(msg,"GRID: MODEL GENERATION FAILED");
         if(grid_dobj)
          { garr = grid_dobj->getGridData();
            if(garr) free(garr);
            garr = NULL;
            gridSetDataPntr(gcvm, NULL); // calls gridData destructor
            grid_dobj=NULL;
          }
         CVMMsgBox(gcvm->form->W(),msg);
         goto error;
       }
      gridSetDataPntr(gcvm, grid_dobj);
      gridPlotUnd(gcvm);
      grid_dobj->getGridVals(&nz,&oz,&dz,&nx,&ox,&dx,&ny,&oy,&dy);
      grid_dobj->getGridCoords(&tz,&tx,&ty);
      // Update the GUI and the model grid limits
      glimitsSvals(glim,nx,dx,ox,ny,dy,oy,nz,dz,oz);
      glimits_set(glimitsm,&nz,&oz,&dz,&nx,&ox,&dx,&ny,&oy,&dy,
       tz, tx, ty );
     }
   else if(gcvm->mode == GTOL)
     {// convert grid data to layer velocities
      VectorLinkedList *mlist,*clist;
      garr = NULL;
      go = gridDataPntr(gcvm);
      if(go) garr = go->getGridData();
      if(!garr)
       { strcpy(msg,"GRID: No grid to use as input");
         CVMMsgBox(gcvm->form->W(),msg);
         goto error;
       }
      cvmapp->setMessage("Wait: Inverting Grid For Layer Velocities");
      mlist = cvmapp->cvm_get_vll(CvmAppBase::Materials);
      clist = cvmapp->cvm_get_vll(CvmAppBase::Boundarys);
      go->getGridVals(&nz, &oz, &dz, &nx, &ox, &dx, &ny, &oy, &dy);
      ierr = grid_to_layr(mlist, clist, &nx,&ox,&dx,
                   &nz,&oz,&dz, (float *) garr);
      if(ierr) goto error;
     }
   else if(gcvm->mode == NOGR)
     {// eliminate the grid data and replot.
      gridKillData(gcvm);
     }
   else
     { CVMMsgBox(gcvm->form->W(),"GRID: Option not supported yet");
       goto error;
     }

  }

error:
 cvmapp->setMessage("");
 return;
}

gridData *gridRead(char *hfil, gridcvm *gcvm)
{/*
  * Read in slice(s) and create a gridData object
  */
 CvmApp        *cvmapp;
 TF_Global     *g=NULL;
 Grid3DDesc    *h=NULL;
 IO_request    ioreq;
 Pt3Di         *N=0,*C=0,*c=0;
 Pt3Df         *O=0,*D=0;
 Pt3Ds         *axis=0;
 ErsTransforms *tdata;
 ErsTransform  *tx,*ty,*tz,*mtx,*mty,*mtz;
 gridData      *grid_dobj=0;
 float          dx=1.0,dy=1.0,dz=1.0,ox=0.,oy=0.,oz=0.;
 int            nx=1,ny=1,nz=1;
 int            i,fd, type, wdtypi;
 int            oflag,iotype=DSKIO_CSTR,i_err;
 char           labx[16],laby[16],labz[16];
 char          *garr=0, *hd=0, *dfil;
 float         *tasf=0;

// Determine the globals and the file type
 g = get_global_info(hfil);
 if(!g) return grid_dobj;
 type = getgl_str_to_ftype(tf_global_ftype(g));
 if(type == UNKWN_TYPE || type == BADFN_TYPE ) {
   printf("gridRead: failed get_global_info for %s\n",hfil);
   return grid_dobj;
 }
 wdtypi = tf_global_get_wdtyp(g);
 h = (Grid3DDesc *) g->h;
 if(h) {
  N = tf3d_getN(h);
  O = tf3d_getO(h);
  D = tf3d_getD(h);
  C = tf3d_getBlock(h);
  c = tf3d_getBrick(h);
  axis = tf3d_getAxisNames(h);
  nz = N->v[0];
  nx = N->v[1];
  ny = N->v[2];
  dz = D->v[0];
  dx = D->v[1];
  dy = D->v[2];
  strcpy(labz,axis->v[0]);
  strcpy(labx,axis->v[1]);
  strcpy(laby,axis->v[2]);
  oz = O->v[2]; 
  ox = O->v[0];
  oy = O->v[1];
 }
 else {
  free(g);
  return grid_dobj;
 }

 dfil = tf_global_data_file(g);
 printf("gridRead: read for %s\n",dfil);
 oflag = dskio_ordo_(); 
 dskio_xop_(&iotype,&fd,dfil,&oflag, &i_err);
 /* fd =open(dfil,O_RDONLY,0); */
 if(fd  < 0 ) return grid_dobj;
 ioreq.axis = 3;
 ioreq.index= gcvm->start-1;
 ioreq.nsamp= nz;
 ioreq.sdec = 1;
 ioreq.samp1= 1;
 printf("gridRead: type=%d\n",type);
 switch(type) {

 case HGRID_TYPE:
 case VOXET_TYPE:
 case TF3D_TYPE:
 case TF3DF_TYPE:
   garr = (char *) malloc(nz*nx*tf_global_get_nbydp(g)*sizeof(char));
   hd = (char *) malloc(64*nx*sizeof(float));
   tasf= (float *) malloc((nx+2)*sizeof(float));
   i =  read_data_fromdisk(fd, &ioreq, g, hd, garr);
   i =  read_data_translate(&ioreq, g, hd, garr, tasf);
   if(hd) free(hd);
   if(tasf) free(tasf);
   break;

 case BRICK_TYPE:
   /* create a CubeIO and read in a slice
   CubeIO *cc = new CubeIO<char>(fd,C.v,c.v);
   CubeSlice<char> *clsi = cc->slice_rd(ioreq->axis,ioreq->index);
   garr = csli->data();
  */
   return 0;  
 default:
   close(fd);
   if(g->h) {free(g->h); g->h=0; }
   if(g) {free(g); g=0; }
   return grid_dobj;
 }

 close(fd);
 if(g->h) {free(g->h); g->h=0; }
 if(g) free(g);
 if(!garr) return grid_dobj;
 // create object and transform to model coords.
 cvmapp = (CvmApp *) gcvm->cvmapp;
 tdata  = cvmapp->cvmGetTransforms();
 mtz = tz = ErsTransGetTran(tdata,labz);
 mtx = tx = ErsTransGetTran(tdata,labx);
 mty = ty = ErsTransGetTran(tdata,laby);
 cvmapp->cvm_gettrans(&mtx,&mty,&mtz);
 printf("gridRead nx=%d ny=%d, ox=%f oy=%f dx=%f dy=%f nz=%d dz=%f\n",
  nx,ny,ox,oy,dx,dy,nz,dz);
 grid_dobj = new gridData((float *) garr,
              nz, oz, dz, nx, ox, dx, ny, oy, dy,
              tz,tx,ty);
 grid_dobj->setSlice((int) gcvm->start);
 grid_dobj->gridDataTrans(tdata,labz,labx,laby);
 printf("gridRead DBGE\n");
 return grid_dobj;
}
/*
 C dfile.......... The name of the data file.
 C word_string.... The data type in the input file.
 C n1,n2,n3....... Dimensions of the data cube.
 C slice.......... Plane to retrieve along axis n3.
 */
char *gridGetSlice(char *dfile, char *word_string,
       int  n1, int  n2, int  n3, int slice)
{ char *wbuf=0;
  int  local_word;
  int  word_in,key,lun=0;
  int  nc, rec, reclen;
  char hostname[32],os[32],msg[120];

  if(!dfile) return wbuf;
  if(slice < 1 || slice > n3) return wbuf;
 /***************************************
  * Open an old data file             ***
  **************************************/
  local_word = (int) netw_lnode(hostname, os);
  key    =-1;
  word_in= tf_global_iword_type(word_string);
  reclen = n1;
  if(word_in != WBYTE) reclen = 4*n1;
  if(word_in != WCRAY) reclen = 8*n1;
  key = rmodopc_(&lun, dfile, dfile, "STREAM","OLD","BINARY",
        &reclen,&word_in);
  if( key < 1) {return wbuf;} 
 /****************************************
  * Read in a slice from a data  file    *
  ***************************************/
  if(strcmp(word_string,"BYTE")==0) 
   wbuf= (char *) calloc(1,n1*n2*sizeof(char));
  else
   wbuf= (char *) calloc(1,n1*n2*sizeof(float));
  if(wbuf == 0) return wbuf;
  if(key > 0)
   {int   ierr,num;
    num = n1*n2;
    rec = (slice-1)*n2 + 1;
    rmodrdc_(&key,&rec, &n2, &n1, (char *) wbuf,
        &word_in,&ierr, msg);
    if(ierr) { free(wbuf); return 0; }
    if(local_word == WIEEE)
      nc = bswap_(&num,(unsigned char *) wbuf);

   }

 return wbuf;
}


void gridPlotUnd(gridcvm *gcvm)
{// Build a plot based upon the data in a gridData class.
 // Update the gui after this call since the coord sys may change.
 CvmApp        *cvmapp;
 SeisPlotUnder *spund;
 SeisPlot      *sp;
 gridData      *grid_dobj;
 ErsTransform  *t1=NULL,*t2=NULL,*t3=NULL;
 SeisTransf    *sptrans=NULL;
 float          lo=1500, hi=15000;
 int            matchHdr=0;
 char           msg[120];

 if(!gcvm) return;
 cvmapp = (CvmApp *) gcvm->cvmapp;
 grid_dobj = gridDataPntr(gcvm);
 if(!cvmapp ) return;
 spund = (SeisPlotUnder *) cvmapp->getSeisPlotUnder();
 sp    = (SeisPlot *) cvmapp->getSeisPlot();
 if(!spund || !sp) return;

 printf("gridPlotUnd: DBG1\n");
 sptrans = (SeisTransf *) sp->transform();
 printf("gridPlotUnd: DBG2\n");
 cvmapp->setMessage("Wait: Plotting Grid Data");
 printf("gridPlotUnd: DBG3\n");
 if(gridMatchSeis(sptrans, grid_dobj) == 0)
  {  sprintf(msg,"gridPlotUnd: seismic and grid data do not match");
     CVMMsgBox(gcvm->form->W(),msg);
  }

 printf("gridPlotUnd: DBG4\n");
 spund->setSeisAnnotation(0); // dont annotate SeisPlotUnder
//  Grid data has to be stored in left to right display order 

 if(grid_dobj)
  {
 printf("gridPlotUnd: DBG5\n");
   grid_dobj->getGridCoords(&t1,&t2,&t3);
   matchHdr = (int) transform_gethdr(t2);
   sp->setMatchHeader(matchHdr);
   spund->setMatchHeader(matchHdr);
   grid_dobj->getGridHiLo(&lo, &hi);
   if(hi==lo) { lo=.90*lo; hi = 1.1*lo; }
  }

/*************************************************
 * Initialize for array plotting and set scaling *
 * Before any data is plotted the plot type is PlotGRID
 * Note: initArrayTypePlot changes the plot type*/
 if(sp->plotType() < PlotImage::PlotCOLOR && grid_dobj)
  {
   spund->setMinColorAmp(lo);
   spund->setMaxColorAmp(hi);
   spund->plotArrayTypeToSeismic(spund->matchHeader());
  }
 else
  {
   sp->setMinColorAmp(lo);
   sp->setMaxColorAmp(hi);
   sp->plot();
  }

 printf("gridPlotUnd: DBG6\n");
 gcvm->visRad->setIvar(SHOW);
// Update the underlay plot of the remote displays
 cvmapp->setMessage("Updateing remote underlay plots");
 printf("gridPlotUnd: DBG7\n");
 cvmapp->updateRemoteUnders();
 cvmapp->setMessage("");
}

int gridMatchSeis(SeisTransf *sptrans, gridData *gobj)
{ErsTransform *ts=NULL,*t1=NULL,*t2=NULL,*t3=NULL;
// Check whether seismic and grid data are defined
// and match in their vertical coordinate systems.
  if(sptrans) ts = (ErsTransform *) sptrans->getTZsplot();
  if(gobj) gobj->getGridCoords(&t1,&t2,&t3);
 printf("gridMatchSeis: DBG1\n");
  if(t1==NULL || ts==NULL) return -1;
  if( ts != t1) return 0;
  return 1;
}

void saveTrap(void *data, long /*ident*/)
{ gridcvm *grid = (gridcvm *) data;
 /* Build the Save As popup for the grid output*/
 CvmApp *cvmapp;
 if(grid == NULL) return;
 if(!grid->sav_shell)
  {cvmapp = (CvmApp *) grid->cvmapp;
   grid->sav_shell = (SLDialog *) GridSaveGUI(grid->form, 
                     grid, cvmapp->getHelpCtx());
  }
 if(grid->sav_shell) grid->sav_shell->makeAndManage();
 return;
}

void modeTrap(void *data, long, long oldv, long newv )
{ /* major Options */
 gridcvm *grid = (gridcvm *) data;
 if(grid == NULL) return;
 if(newv==oldv) return;
 grid->mode=LTOG;
 if(newv==LTOG) grid->mode= LTOG;
 if(newv==READ) grid->mode= READ;
 if(newv==GTOL) grid->mode= GTOL;
 if(newv==NOGR) grid->mode= NOGR;
 if(grid->modRad) grid->modRad->setIvar(newv);
 return;
}

void visTrap(void *data, long, long oldv, long newv )
{ /* Control the visibility of underlay */
 gridcvm *grid = (gridcvm *) data;
 CvmApp *cvmapp;
 SeisPlotUnder *spund;
 if(newv==oldv) return;
 cvmapp = (CvmApp *) grid->cvmapp;
 if(grid == NULL || cvmapp == NULL) return;
 spund = (SeisPlotUnder *) cvmapp->getSeisPlotUnder();
 if(spund == NULL) return;
 if(newv==HIDE) spund->hide();
 if(newv==SHOW) spund->show();
 if(grid->visRad) grid->visRad->setIvar(newv);
 return;
}
 



static void KillCB(Widget , gridcvm *gcvm, caddr_t )
{ /* Close down everything */
 gridData *gd;
 void     *garr;
 if(gcvm == NULL) return;
 gd = gridDataPntr(gcvm);
 if(gd)
  { garr = gd->getGridData();
    if(garr) free(garr);
    gd->setGridData((char *) 0);
    garr = NULL;
  }
 gridSetDataPntr(gcvm, NULL); // calls gridData destructor
 gridPlotUnd(gcvm);
 free(gcvm);
 return;
}

/* Call only after glimits display is built */
void gridInit(void *data)
{
 CvmApp *cvmapp;
 ModLimits *mlimits;
 ErsTransform *tx,*ty,*tz;
 float mxmin,mxmax,mymin,mymax,mzmin,mzmax;
 Glimits *glim;
 gridcvm *grid = (gridcvm *) data;
 if(grid==NULL) return;
 cvmapp = (CvmApp *) grid->cvmapp;
 mlimits = cvmapp->cvmGetMLimits();
 glim    = (Glimits *) grid->glim;
 if(mlimits == NULL ) return;

 mlimits_get(mlimits,&mxmin,&mxmax,&mzmin,&mzmax,&mymin,&mymax,&tx,&ty,&tz);

 /* provide some default grid values */
 if(mxmax <= mxmin) mxmax = mxmin+1.0;
 if(mymax <  mymin) mymax = mymin;
 if(mzmax <= mzmin) mzmax = mzmin+1.0;
 if(glim->nxg <= 1) glim->nxg = 2;
 if(glim->nyg <  1) glim->nyg = 1;
 if(glim->nzg <= 1) glim->nzg = 2;
 if(glim->dx== 0.) 
   glim->dx= (mxmax-mxmin)/(glim->nxg - 1);
 if(glim->dy== 0.) 
   glim->dy= (mymax-mxmin)/(glim->nyg - 1);
 if(glim->dz== 0.)
   glim->dz= (mzmax-mzmin)/(glim->nzg - 1);

 return;
}

void gridUpdate(void *data)
{/* make display consistent with the ModelDesc structure */
 CvmApp     *cvmapp;
 GridLimits *globj;
 gridcvm    *gcvm = (gridcvm *) data;
 if(!gcvm) return;
 cvmapp = (CvmApp *) gcvm->cvmapp;
 globj   = cvmapp->cvmGetGLimits();
 glimitsGuiUpdate((void *) gcvm->glim,globj);
 return;
}

SLSmartForm *gridForm(void *data)
{/* return form */
 gridcvm *grid;
 grid = (gridcvm *) data;
 if(grid==NULL) return NULL;
 return grid->form;
}

SLDialog *gridDial(void *data)
{/* return form */
 gridcvm *grid;
 grid = (gridcvm *) data;
 if(grid==NULL) return NULL;
 return grid->shell;
}

gridData *gridDataPntr(gridcvm *gcvm)
{CvmApp   *cvmapp=NULL;
 if(gcvm) cvmapp = gridCvmAppPntr(gcvm);
 if(cvmapp) return cvmapp->cvmGetGridData();
 return NULL;
}

void gridSetDataPntr(gridcvm *gcvm, gridData *pntr)
{// update the gridcvm and SeisPlot* objects
 CvmApp        *cvmapp;
 SeisPlotUnder *spund;
 SeisPlot      *sp;
 SeisTransf    *sptrans=NULL;
 gridData      *gdold;
 ErsTransform  *t1=NULL,*t2=NULL,*t3=NULL;
 void          *garr=NULL;
 float         d1=1.0,d2=1.0,d3=1.0,o1=0.0,o2=0.0,o3=0.0;
 float         X1,X2,Z1,Z2;
 int           n1=1,n2=1,n3=1;
 int           i,matchHdr=0;
 static int    isosp,isospu;

 if(!gcvm) return;
 cvmapp  = gridCvmAppPntr(gcvm);
 if(!cvmapp ) return;
 gdold = gridDataPntr(gcvm);
 if(pntr == gdold) return;
 spund = (SeisPlotUnder *) cvmapp->getSeisPlotUnder();
 sp    = (SeisPlot *)      cvmapp->getSeisPlot();
 sptrans = (SeisTransf *) sp->transform();
 if(gridMatchSeis(sptrans,pntr) == 0)
  {// proceed only if coords match or pntr=null
   printf("gridSetDataPntr: mismatch in seismic and grid z coordinates\n");
   return;
  }

 if(gdold)  // get old values
  { garr    = gdold->getGridData();
    gdold->getGridVals(&n1,&o1,&d1,&n2,&o2,&d2,&n3,&o3,&d3);
    gdold->getGridCoords(&t1,&t2,&t3);
    delete gdold;
  }
 if(pntr)   // reset for new data
  { garr    = pntr->getGridData();
    pntr->getGridVals(&n1,&o1,&d1,&n2,&o2,&d2,&n3,&o3,&d3);
    pntr->getGridCoords(&t1,&t2,&t3);
  }
 cvmapp->cvmSetGridData(pntr);  // sets ModelDesc::_grid_data
 X1 = o2;
 X2 = X1 + (n2 - 1)*d2;
 Z1 = o1;
 Z2 = Z1 + (n1 - 1)*d1;

 matchHdr = (int) transform_gethdr(t2);
 sp->setMatchHeader(matchHdr);
 spund->setMatchHeader(matchHdr);
 if(matchHdr > 0) matchHdr -= 1;
// call initArrayTypePlot prior to initArrayTypeData
// Note: initArrayTypePlot changes the plot type*/
 if(sp->plotType() < PlotImage::PlotCOLOR)
  {if(spund->plotType() != PlotImage::PlotISO && isospu==0) 
    {  spund->initArrayTypePlot(); isospu++;}
   spund->setGridXYS(X1,X2,Z1,Z2);
   spund->setTmin(Z1);
   spund->setTmax(Z2);
   if(garr)
    {spund->setPlotType(PlotImage::PlotISO);
     spund->initArrayTypeData(1,1,n2,n1,(float *) garr); //after setGridXYS
    }
   for(i=0;i<n2;i++)
    spund->setHeader(i*spund->numHeaders()+matchHdr,X1+i*d2);
  }
 else
  {
   if(sp->plotType() == PlotImage::PlotGRID && isosp==0)
     { sp->initArrayTypePlot(); isosp++; }
   sp->setGridXYS(X1,X2,Z1,Z2);
   spund->setTmin(Z1);
   spund->setTmax(Z2);
   if(garr) 
    {sp->setPlotType(PlotImage::PlotISO);
     sp->initArrayTypeData(1,1,n2,n1,(float *) garr);  //after setGridXYS
    }
   for(i=0;i<n2;i++)
    sp->setHeader(i*sp->numHeaders()+matchHdr,X1+i*d2);
  }
 if(!garr)
   { // Can not be a PlotISO if the data vanishes
     if(sp->plotType() == PlotImage::PlotISO)
      {sp->cancelArrayTypeData();
       sp->setPlotType(PlotImage::PlotGRID);
      }
     if(spund->plotType() == PlotImage::PlotISO)
      {spund->cancelArrayTypeData();
       spund->setPlotType(PlotImage::PlotGRID);
      }
   }

}

CvmApp *gridCvmAppPntr(gridcvm *grid)
{if(grid) return grid->cvmapp;
 return NULL;
}

void gridKillData(gridcvm *gcvm)
{ CvmApp   *cvmapp;
  gridData *grid_dobj;
  float    *garr;
  // eliminate the grid data and replot.
  cvmapp  = gridCvmAppPntr(gcvm);
  grid_dobj = gridDataPntr(gcvm);
  if(grid_dobj)
   { garr = (float *) grid_dobj->getGridData();
     if(garr && grid_dobj->ownsData()==0) free(garr);
     grid_dobj->setGridData((char *) 0);
   }
  gridSetDataPntr(gcvm, NULL); // calls gridData destructor
  gridPlotUnd(gcvm);
}
