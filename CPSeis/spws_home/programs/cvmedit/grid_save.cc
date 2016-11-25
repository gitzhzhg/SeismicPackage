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
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>

#include "wproc.h"
#include "wrdcnvrt.h"
#include "model_io.h"
#include "model_desc.hh"
#include "tfdefs.h"   /* for defined file types */

#include "sl/sl_dialog.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_row_column.hh"
#include "sl/slp_push.hh"
#include "sl/radio_list.hh"
#include "sl/slp_file.hh"
#include "sl/slp_option.hh"
#include "sl_cvm_app.hh"

#include "grid_data.hh"
#include "grid_save.h"

#define NETCDF 20
enum { IEEEWD=1,VMSWD,BYTEWD };
enum { GOK, GCAN };

/* PROTOTYPES FOR PRIVATE METHODS */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

int  grid_wr(ModelDesc *mod, char *file,
     int  word_out, int  file_type, gridData *gdata);

void   CVMMsgBox(Widget , char *);

static void KillCB(Widget W, gridsave *, caddr_t b);
static void ControlTrap(void *data, long ident);
static void wordTrap(void *data,long ident,long oldv,long newv);
static void styleTrap(void *data,long ident,long oldv,long newv);

#ifdef __cplusplus
}                   // for C++
#endif


SLDelay *GridSaveGUI(SLDelay *slparent, gridcvm *grid, HelpCtx Hctx)
{/* Dialog to control saving of grid file  */
 SLDialog    *dial;
 SLSmartForm *work;
 SLpPush     *ok, *cancel;
 SLRowColumn *rc_style, *rc_word;
 RadioList   *radio1,*radio2;
 CvmApp   *cvmapp;
 gridsave *gs;

 
/***************************************************
 * Do some sanity checking                        */
 cvmapp = (CvmApp *) gridCvmAppPntr(grid);
 if(!grid)   return NULL;
 if(!slparent) return NULL;
 if(!cvmapp) return NULL;

 gs = (gridsave *) calloc(1,sizeof(gridsave));
 strcpy(gs->file,"NONE");
 gs->file_type = HGRID_TYPE;
 gs->word_type = WIEEE;
 gs->start_slice = 1;
 gs->slices = 1;
 gs->grid_cvm= grid;
 gs->cvmapp = (void *) cvmapp;
/*************************************************
 * Create a SLDialog and SLSmartForm            */
  dial = new SLDialog(slparent,"GridSave", Hctx,True);
  work = dial->workArea();
  ok   = dial->addBottomOK(GOK,ControlTrap,
                    (void *) gs);
  cancel = dial->addBottomCancel(GCAN,ControlTrap,
                    (void *) gs);
  dial->addBottomHelp();
  gs->shell = dial;
  gs->work  = work;

  dial->setTitle("Save Gridded Model");
  dial->make();
  XtAddCallback(work->W(),XmNdestroyCallback,(XtCallbackProc) KillCB,gs);

/************************************************
 * Add a rowcolumn to the form                 */
    rc_style = new SLRowColumn( work ,"rc1",NULL,True,True,True);
   //                        left  right top   bottom  
   work->attach(rc_style  , work, work, NULL, work,20,20,0,8);

/************************************************
 * Create radio list for file style            */
    radio1 = new RadioList();
    radio1->addRadio(rc_style, "rad1", HGRID_TYPE,"HGRID File");
    radio1->addRadio(rc_style, "rad2", VOXET_TYPE,"VOXET File");
    radio1->addRadio(rc_style, "rad3", BRICK_TYPE,"BRICK File");
    radio1->addRadio(rc_style, "rad4", NETCDF,"netCDF File");
    radio1->setIvar(CVMTYP);
    radio1->setItrap(styleTrap, (void *) gs);
    gs->radio1 = radio1;

/*************************************************
 * Add a rowcolumn to the form                  */
    rc_word  = new SLRowColumn( work,"rc2",NULL,True,True,True);
    work->attach(rc_word  , work, work, NULL  , rc_style ,20,20,0,8);

/************************************************
 * Create radio list for word type selection.  */
    radio2 = new RadioList();
    radio2->addRadio(rc_word, "rad1", IEEEWD,"IEEE floats");
    radio2->addRadio(rc_word, "rad2", VMSWD, "VMS floats");
    radio2->addRadio(rc_word, "rad3", BYTEWD,"Byte Values");
    radio2->setIvar(IEEEWD);
    radio2->setItrap(wordTrap, (void *) gs);
    gs->radio2 = radio2;

/*************************************************
 * Add a FileChoice widget under the form       */
 gs->ofcw = new SLpFile(work,"gridFC", 0,"Grid File Name",NULL,
        "hgrid",SLpFile::_OUTPUT);
 work->attach(gs->ofcw,work,work,NULL,rc_word,10,10,0,8);
 if(gs->ofcw) gs->ofcw->setFilename("NONE");

 dial->makeAndManage();
 return dial;
}

static void ControlTrap(void *data, long ident)
{ /* OK = GOK, CANCEL = GCAN  */
 gridsave *gsav = (gridsave *) data;
 gridcvm  *gcvm;
 gridData *gdata;
 CvmApp   *cvmapp;
 ModelDesc *mod=NULL;
 char *fn=0;

 char msg[240];
 int  ierr;
 if(!data) return;

 if(ident == GCAN)  // CANCEL
  { 
    goto jump;
  }

 if(ident == GOK ) // OK
  { 
    if(gsav->ofcw) fn = gsav->ofcw->filename();
    if(fn) strcpy(gsav->file,fn);
    else strcpy(gsav->file,"NONE");
    if(strcmp(gsav->file,"NONE")==0) goto jump;
    cvmapp= (CvmApp *) gsav->cvmapp;
    gcvm  = (gridcvm *) gsav->grid_cvm;
    gdata = gridDataPntr(gcvm);
    if(!gdata)
     { strcpy(msg,"grid_save: no gridData object exists");
       goto error;
     }

    mod = cvmapp->getModelDesc();
    ierr = grid_wr(mod, gsav->file, gsav->word_type, gsav->file_type,
           gdata);
    if(!ierr)
     { strcpy(msg,"grid_save: Failed to save a grid model");
       goto error;
     }

  }
 jump:
  gsav->shell->unmanage();
  return;
 error:
  CVMMsgBox(gsav->work->W(),msg);
}

static void styleTrap(void *data,long ,long oldv,long newv)
{// record the preference for output file format.
 char str[120],*fn;
 int  ierr;
 gridsave *grid = (gridsave *) data;
 if(data == NULL) return;
 if(newv==oldv) return;
 grid->radio1->setIvar(newv);
 grid->file_type= HGRID_TYPE;
 fn = grid->ofcw->filename();
 if(fn) strcpy(str,fn);
 else strcpy(str,"NONE");
 if(newv == HGRID_TYPE)
  { grid->file_type= HGRID_TYPE;
    if(strcmp(str,"NONE") != 0) 
     addext_rep_(str,"hgrid",&ierr);
  }
 if(newv == VOXET_TYPE)
  { grid->file_type= VOXET_TYPE;
    if(strcmp(str,"NONE") != 0) 
     addext_rep_(str,"vo",&ierr);
  }
 if(newv == BRICK_TYPE)
  { grid->file_type= BRICK_TYPE;
    if(strcmp(str,"NONE") != 0) 
     addext_rep_(str,"br",&ierr);
  }
 if(newv == NETCDF)
  { grid->file_type= VOXET_TYPE;
    if(strcmp(str,"NONE") != 0) 
     addext_rep_(str,"vo",&ierr);
    CVMMsgBox(grid->work->W(),"Not supported yet, For SpyGlass");
  }
 grid->ofcw->setFilename(str);

 return;
}

static void wordTrap(void *data,long ,long oldv,long newv)
{// record the preference for output word format.
 gridsave *grid = (gridsave *) data;
 if(data == NULL) return;
 if(newv==oldv) return;
 grid->radio2->setIvar(newv);
 grid->word_type= WIEEE;
 if(newv == IEEEWD)
  {grid->word_type= WIEEE;
  }
 if(newv == VMSWD)
  {grid->word_type = WVMS;
  }
 if(newv == BYTEWD)
  {grid->word_type = WBYTE;
  }

 return;
}

static void KillCB(Widget , gridsave *dat, caddr_t )
{ /* Close down everything */
 if(dat == NULL) return;
 free(dat);
 return;
}
