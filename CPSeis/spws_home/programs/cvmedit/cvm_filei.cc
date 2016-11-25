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
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include "file_choice.h"
#include "model.h"
#include "model_io.h"
#include "tfdefs.h"

#include "mlimits.h"
#include "vec_list_util.hh"
#include "sl_cvm_app.hh"
#include "pick_gui_pop.hh"
//#include "gplot.h"
#include "draw_mod.h"
#include "seis_transf.hh"
#include "vect/ll_seis_vect.hh"
#include "oprim/modbase_data.hh"
#include "sl/shell_watch.hh"
#include "sp/seis_plot.hh"
#include "horiz_edit_pop.hh"
#include "vl_data.hh"
#include "vl_data_user.hh"


#ifdef __cplusplus
extern "C" {                 // for C++
#endif
void   CVMMsgBox(Widget , char *);

ErsModel *cvm_get(char *infile, char *msg, int *ftype, char *linename);
void cvm_filei(Widget W, void *data,caddr_t b);
#ifdef __cplusplus
}                   // for C++
#endif


void cvm_filei(Widget W, void *data,caddr_t b)
{CvmApp       *cvmapp = (CvmApp *) data;
 ErsModel     *model,*model_temp;
 ErsTransform *tx,*ty,*tz;
 char xname[18],yname[18],zname[18];

 int   ftype;
 char  filei[96],linename[64],title[96];
 static char msg[88];
 wprocFileChoiceSuccCallbackStruct *fccb;

 fccb = (wprocFileChoiceSuccCallbackStruct *) b;
 if(!wprocFileChoiceGetFileByStr(W,filei) ) return;
 if(cvmapp == NULL) return;
 cvmapp->cvm_set_file(filei);
 if(strncmp(filei,"NONE",4)==0 || strncmp(filei,"none",4)==0)
  { return; }
 if(filei[0]==' ') return;

 if(cvmapp->PickingActive()) 
  {/* active picking detected , do not overwrite old model */
     CVMMsgBox(cvmapp->mainWindow(),
     "End Picking before reading in new model");
      fccb->doit = False;
      return;
  }

 ftype = UNKWN_TYPE;
 ShellWatch sw;
 cvmapp->setMessage("Wait: reading input");
 model_temp = cvm_get(filei, msg, &ftype,linename);
 cvmapp->setMessage("");
 if(!model_temp)
   {
    CVMMsgBox(cvmapp->mainWindow(),msg);
    return;
   }
  
 sprintf(title,"CVMEdit: Root, File=%s",filei);
 cvmapp->setTitle(title);
 model = cvmapp->cvm_getmod(cvmapp);
 /**********************************
  * We want to save some of the old model 
  * when the file read was GWS_TYPE
  *********************************/
 if(ftype == GWS_TYPE)
  {/* Preserve limits and transforms */
    memcpy(model_getmlim(model_temp), model_getmlim(model), sizeof(ModLimits));
    memcpy(model_getglim(model_temp), model_getglim(model),sizeof(GridLimits));
    transforms_copy(model_gettdata(model), model_gettdata(model_temp));
    model_gettrans(model,&tx,&ty,&tz);
    if(transform_getname(tx)) strcpy(xname,transform_getname(tx));
    else strcpy(xname,"XBASEMENT");
    if(transform_getname(ty)) strcpy(yname,transform_getname(ty));
    else strcpy(xname,"YBASEMENT");
    if(transform_getname(tz)) strcpy(zname,transform_getname(tz));
    else strcpy(xname,"TIME");
    tx = ErsTransGetTran(model_gettdata(model_temp),xname);
    ty = ErsTransGetTran(model_gettdata(model_temp),yname);
    tz = ErsTransGetTran(model_gettdata(model_temp),zname);
    mlimits_set_trans(model_getmlim(model_temp),tx,ty,tz);
    // pr_new = (PR_ *) model_getpdata(model_temp);
    // model_rep_pdata(*model, pr_new);
    //destroy_model(model_temp);
  }

 cvmapp->startNewModel(model_temp);

 return;
}

ErsModel *cvm_get(char *infile, char *msg, int *ftype, char *linename)
{
 ErsModel      *model_temp;
 PR_           *pr_new;
 ModLimits     *mlimits;
 UserInfo      *us;
 ErsTransform  *tx,*ty,*tz;
 float          xmin,xmax,zmin,zmax,ymin,ymax;
 int            ierr,tru_ftype,in_type;
 float          x_small,x_big,z_small,z_big;

 msg[0]='\0';
 /**********************************
  * Check the input file name     **
  *********************************/
 ierr = 3;
 if(infile == NULL) return NULL;
 if(strcmp(infile,"none") == 0) return NULL;
 if(strcmp(infile,"NONE") == 0) return NULL;
 if(strlen(infile)==0) return NULL;
 if(strncmp(infile," ",1) == 0) return NULL;
 strcpy(msg,"cvm_get: bad file name");

 /**********************************
  * Check that the input type=LAYER*
  * or GWS_TYPE. Network??         *
  *********************************/
 in_type = getgl_ftype(infile);
 if(in_type != LAYER_TYPE && in_type != GWS_TYPE)
  { *ftype = in_type;
    return NULL;
  }
  
 /**********************************
  * Read in the data               *
  *********************************/
 model_temp = new_model();
 if(pcardrd(model_temp, infile, &tru_ftype))
  { strcpy(msg,"cvm_get: Model file read in");
  }
 else
  { strcpy(msg,"cvm_get: Model file not read in");
    destroy_model(model_temp);
    ierr = 1;
    return NULL;
  }

 /**********************************
  * Retrieve line name, if any    **
  *********************************/
 ierr=0;
 us =model_getudata(model_temp);
 userinfo_get_lname(us,linename); /* retrieve line name */

 /**********************************
  * Check the file type           **
  *********************************/
 *ftype = tru_ftype;

/*********************************************
 * Scan picks for the min & max limits.     **
 * SPMIN,SPMAX,ZMIN,ZMAX = Actual data limits*
 * XM1,XM2,ZM1,ZM2       = File model limits**
 ********************************************/
 pr_new = (PR_ *) model_getpdata(model_temp);
 ErsPRMinMax(pr_new,&x_small,&x_big, &z_small, &z_big);
 mlimits = model_getmlim(model_temp);
 mlimits_get(mlimits, &xmin, &xmax,
     &zmin, &zmax, &ymin, &ymax,&tx,&ty,&tz );
 if(xmin == xmax)
  { xmin = x_small; xmax = x_big;}
 if(zmin == zmax)
  { zmin = z_small; zmax = z_big;}
 mlimits_set(mlimits, &xmin, &xmax,
     &zmin, &zmax, &ymin, &ymax, tx, ty, tz );
 msg[0]='\0';

return model_temp;
}

