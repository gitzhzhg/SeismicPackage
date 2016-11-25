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
#include <stdlib.h>
#include <string.h>
#include "pick.h"
#include "model.h"


void *model_getmodgui(void *Model);

 

/*********************************************************
 * Methods that act upon an UserInfo structure.        ***
 ********************************************************/
UserInfo *new_userinfo()
{ UserInfo *u;
 u = ( UserInfo *) calloc(1,sizeof(UserInfo));
 return u;
}

void destroy_userinfo(UserInfo *us)
{
 if(us == NULL) return;
 if(us->linename != NULL) free(us->linename);
 if(us->comment  != NULL) free(us->comment);
 free(us);
}

void userinfo_get_lname(UserInfo *us, char *linename)
{
 if(us == NULL || linename == NULL) return;
 linename[0]='\0';
 if(us->linename != NULL) strcpy(linename,us->linename);
 return;
}

void userinfo_set_lname(UserInfo *us, char *linename)
{
 if(us == NULL || linename == NULL) return;
 if(us->linename != NULL)
  { free(us->linename); }
 us->linename = (char *) malloc(strlen(linename)+1);
 strcpy(us->linename,linename);
 return;
}

/*********************************************************
 * Methods that act upon an ErsModel structure.        ***
 ********************************************************/
ErsModel *new_model()
{ErsModel *model;
 ErsTransforms *tdata;
 model = (ErsModel *) calloc(1,sizeof(ErsModel));
 model->TransData = (ErsTransforms *) new_transforms();
 transforms_adddef(model->TransData);
 mlimits_setdef(&model->modlim,model->TransData);
 glimits_setdef(&model->gridlim,&model->modlim);
 
 model->MatData   = (ErsMaterials *) new_materials();
 model->CellData  = (ErsCells *) new_cells();
 model->PikRec    = (void *) PR_Create();
 model->PikRecDestroy = (void (*)()) PR_Free;
 PR_SetPhdr((PR_ *) model->PikRec, 17);
 PR_SetShdr((PR_ *) model->PikRec, UNDEFINED_KEY);
 PR_SetThdr((PR_ *) model->PikRec, UNDEFINED_KEY);
 model->user_info = (UserInfo *) new_userinfo();
 model_setname((ErsModel *) model,(char *) "NONE");
 return model;
}

void destroy_model(ErsModel *model)
{PR_ *pikrec;
 int debug;
 debug = 0;
 if(model == NULL) return;
 if(debug != 1)
  { destroy_transforms(model->TransData);
   model->TransData = NULL;
  }
 if(debug != 2)
  { destroy_cells(model->CellData);
    model->CellData = NULL;
  }
 if(debug != 3)
  { destroy_materials(model->MatData);
    model->MatData = NULL;
  }
 if(debug != 4)
  { destroy_userinfo(model->user_info);
    pikrec = (PR_ *) model->PikRec;
  }
 if(model->PikRecDestroy != NULL) (*model->PikRecDestroy)(pikrec);
 /*PR_Destroy(pikrec);  links to x & motif */
 model->PikRec = NULL;
 if(model->headfile != NULL) free(model->headfile);
 model->headfile = NULL;
 if(model->transfile != NULL) free(model->transfile);
 model->transfile = NULL;
 if(model->datafile != NULL) free(model->datafile);
 model->datafile = NULL;
 if(model->name != NULL) free(model->name);
 model->name = NULL;
}

void free_model(ErsModel *model)
{PR_ *pikrec;
 if(model == NULL) return;
 destroy_transforms(model->TransData);
 model->TransData = NULL;
 destroy_cells(model->CellData);
 model->CellData = NULL;
 destroy_materials(model->MatData);
 model->MatData = NULL;
 destroy_userinfo(model->user_info);
 pikrec = (PR_ *) model->PikRec;
 if(model->PikRecDestroy != NULL) (*model->PikRecDestroy)(pikrec);
 /* PR_Free(pikrec);  No links to x or motif */
 model->PikRec = NULL;
 if(model->headfile != NULL) free(model->headfile);
 model->headfile = NULL;
 if(model->transfile != NULL) free(model->transfile);
 model->transfile = NULL;
 if(model->datafile != NULL) free(model->datafile);
 model->datafile = NULL;
 if(model->name != NULL) free(model->name);
 model->name = NULL;
}

void model_setname(ErsModel *model, char *name)
{int n;
 if(model == NULL || name== NULL) return;
 n = strlen(name);
 if(model->name) free(model->name);
 model->name = (char *) malloc(n+1);
 strcpy(model->name,name);
}

char *model_getname(ErsModel *model)
{if(model == NULL ) return NULL;
 return model->name;
}

ErsTransforms *model_gettdata(ErsModel *model)
{
 if(model == NULL) return NULL;
 return (ErsTransforms *) model->TransData;
}

void model_gettrans(ErsModel *mod,
     ErsTransform **x, ErsTransform **y, ErsTransform **z)
{ModLimits *mlimits;
 *x = NULL;
 *y = NULL;
 *z = NULL;
 if(!mod) return;
 mlimits= &mod->modlim;
 if(!mlimits) return;
 mlimits_get_trans(mlimits,x,y,z);

}

UserInfo *model_getudata(ErsModel *model)
{
 if(model == NULL) return NULL;
 return (UserInfo *) model->user_info;
}

void *model_getpdata(ErsModel *model)
{
 if(model == NULL) return NULL;
 return (void *) model->PikRec;
}
void model_setpdata(ErsModel *model, void *pikrec)
{ if(model == NULL) return;
  model->PikRec = (PR_ *) pikrec;
  return;
}

ErsCells *model_getcdata(ErsModel *model)
{
 if(model == NULL) return NULL;
 return model->CellData;
}

ErsMaterials     *model_getmdata(ErsModel *model)
{
 if(model == NULL) return NULL;
 return model->MatData;
}

ModLimits        *model_getmlim(ErsModel *model)
{
 if(model == NULL) return NULL;
 return &model->modlim;
}

GridLimits       *model_getglim(ErsModel *model)
{
 if(model == NULL) return NULL;
 return &model->gridlim;
}

/* Physically copy the input data to the model structure */
/* Do not use this call if the PickingRecord is connected
   to the widget */
void model_rep_pdata(ErsModel *model, void *pnew)
{ PR_ *pikrec, *pold;
 if(model == NULL) return;
 pikrec = (PR_ *) pnew;
 pold   = (PR_ *) model->PikRec;
 PR_Free(pold);
 model->PikRec = (void *) pnew;
 
 return;
}

void model_rep_cdata(ErsModel *model, ErsCells *cnew)
{ ErsCells  *cold;
 if(model == NULL) return;
 destroy_cells(model->CellData);
 model->CellData = cnew;
 
 return;
}

void model_rep_mdata(ErsModel *model, ErsMaterials *mnew)
{ ErsMaterials  *mold;
 if(model == NULL) return;
 destroy_materials(model->MatData);
 model->MatData = mnew;
 
 return;
}

char *model_gethfile(ErsModel *model)
{if(model == NULL) return NULL;
 return model->headfile;
}
char *model_getdfile(ErsModel *model)
{if(model == NULL) return NULL;
 return model->datafile;
}
char *model_gettfile(ErsModel *model)
{if(model == NULL) return NULL;
 return model->transfile;
}
void model_sethfile(ErsModel *model, char *hfile)
{char *str;
 if(model == NULL) return;
 str = NULL;
 if(hfile != NULL)
  {str = (char *) calloc(1,strlen(hfile)+1);
   strcpy(str,hfile);
  }
 if(model->headfile != NULL) free(model->headfile);
 model->headfile = str;
 return;
}
void model_setdfile(ErsModel *model, char *dfile)
{char *str;
 if(model == NULL) return;
 str = NULL;
 if(dfile != NULL)
  {str = (char *) calloc(1,strlen(dfile)+1);
   strcpy(str,dfile);
  }
 if(model->datafile != NULL) free(model->datafile);
 model->datafile = str;
 return;
}
void model_settfile(ErsModel *model, char *tfile)
{char *str;
 if(model == NULL) return;
 str = NULL;
 if(tfile != NULL)
  {str = (char *) calloc(1,strlen(tfile)+1);
   strcpy(str,tfile);
  }
 if(model->transfile != NULL) free(model->transfile);
 model->transfile = str;
 return;
}
char *model_gettype(ErsModel *model)
{if(model == NULL) return NULL;
 return model->type;
}
void model_settype(ErsModel *model, char *type)
{if(model == NULL) return;
 if(type == NULL)
  { model->type[0] = '\0'; return; }
 strcpy(model->type,type);
 return;
}

int model_getwtype(ErsModel *model)
{if(model == NULL) return -1;
 return model->grid_word;
}
void model_setwtype(ErsModel *model, int *wtype)
{if(model == NULL) return;
 if(wtype == NULL)  return;
 model->grid_word= *wtype;
 return;
}

void *model_getmodgui(void *Model)
{ ErsModel *model;
  PR_ *pikrec;
  extern void *PR_getgui();
  if(Model == NULL) return NULL;
  model = (ErsModel *) Model;
  pikrec = (PR_ *) model->PikRec;
  return PR_getgui(pikrec);
}
