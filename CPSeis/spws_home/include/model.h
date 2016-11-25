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
#ifndef _ERS_CVM
#define _ERS_CVM

#include "transform.h"
#include "mlimits.h"
#include "glimits.h"
#include "model_cell.h"
#include "model_mat.h"

 
/************************************************
 * Macros for ErsModel Structure members       **
#define MGet_hf(model)     ( (model)->headfile )
#define MSet_hf(model,str) ( (model)->headfile = (str) )
#define MGet_tf(model)     ( (model)->transfile )
#define MSet_tf(model,str) ( (model)->transfile = (str) )
#define MGet_df(model)     ( (model)->datafile )
#define MSet_df(model,str) ( (model)->datafile = (str) )
 ***********************************************/
#define MGet_type(model)   ( (model)->type )
#define MGet_tdat(model)   ( (model)->TransData )
#define MGet_cdat(model)   ( (model)->CellData )
#define MGet_mdat(model)   ( (model)->MatData )
#define MGet_pikdat(model) ( (model)->PikRec )
#define MGet_ui(model)     ( (model)->user_info )
#define MGet_mlim(model)   ( &(model)->modlim )
#define MGet_glim(model)   ( &(model)->gridlim )

typedef struct _UserInfo
 { char *linename;
   int  linetype;
   char *comment;
 } UserInfo;



/********** Depth Model definitions  ***********/
typedef struct _ErsModel
 { char          *name;     /* optional name for this model*/
   ModLimits     modlim;    /* model limits */
   GridLimits    gridlim;   /* grid  limits */
   ErsTransforms *TransData;/* defined coordinate systems  */
   ErsCells      *CellData; /* Cell data for a model       */
   ErsMaterials  *MatData;  /* Material data               */
   void          *PikRec;   /* PR_ pointer                 */
   void          (*PikRecDestroy)();/* PR_ destroy method  */
   UserInfo      *user_info;/* user information            */
   char          *headfile; /* File with header information*/
   char          *datafile; /* Layer or Grid File          */
   char          *transfile;/* File with Transform data    */
   char          type[16];  /* Data file type              */
   int           grid_word; /* Float word type of grid file*/
   void          *SeisTrans; /* coordinate mapper function for SeisPlot */
 } ErsModel;

/* Prototypes of Model methods */
#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* UserInfo methods */
UserInfo *new_userinfo();
void      destroy_userinfo(UserInfo *);
void      userinfo_get_lname(UserInfo *, char *);
void      userinfo_set_lname(UserInfo *, char *);

/* ErsModel methods */
ErsModel *new_model();
void destroy_model(ErsModel *model);
UserInfo     *model_getudata(ErsModel *);
ErsTransforms *model_gettdata(ErsModel *);
void         *model_getpdata(ErsModel *);
void          model_setpdata(ErsModel *, void *);
ErsCells     *model_getcdata(ErsModel *);
ErsMaterials *model_getmdata(ErsModel *);
ModLimits    *model_getmlim(ErsModel *);
GridLimits   *model_getglim(ErsModel *);
void model_rep_pdata(ErsModel *model, void *pnew);
void model_rep_cdata(ErsModel *model, ErsCells *cnew);
void model_rep_mdata(ErsModel *model, ErsMaterials *mnew);
char *model_gethfile(ErsModel *model);
char *model_getdfile(ErsModel *model);
char *model_gettfile(ErsModel *model);
char *model_getname(ErsModel *model);
void model_sethfile(ErsModel *model, char *hfile);
void model_setdfile(ErsModel *model, char *dfile);
void model_settfile(ErsModel *model, char *tfile);
void model_setname(ErsModel *, char *);
char *model_gettype(ErsModel *model);
void model_settype(ErsModel *model, char *type);
int  model_getwtype(ErsModel *model);
void model_setwtype(ErsModel *model,int  *wt);
void *model_getmodgui(void *Model);
void model_gettrans(ErsModel *mod,
     ErsTransform **x, ErsTransform **y, ErsTransform **z);

/* following methods are in model_SPtrans.cc */
void *model_getSeisTrans(void *model);
void model_setSeisTrans(void *model, void *sptrans);

#ifdef __cplusplus
}                   // for C++
#endif
 
#endif
