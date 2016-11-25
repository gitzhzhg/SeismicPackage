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
#include <string.h>
#include <X11/Xlib.h>
#include <X11/IntrinsicP.h>
#include <X11/Shell.h>
#include <Xm/MessageB.h>
#include <Xm/XmP.h>
#include "pcard.h"


/*************************************************
 * PR_ *mod_init()                ***
 * Function to initiate picking                ***
 * 1. Check if pick record is attached already ***
 * 2. Allocate a model structure if needed.    ***
 * 3. Install translations & connect to Widget ***
 ************************************************/
PR_ *mod_init(ModInfo *mod, XtTranslations dummytrans,
                 void *model, struct PlotImage *image)
{
  struct PlotImage *limage;
  Widget widget,form;
  ModInfo *modgui;
  static PR_ *pikrec;
  int Phdr, Shdr, Thdr, wcount;
  char   msg[120];

  if(mod==NULL)
   {printf("mod_init error: model picking GUI * = NULL\n");
    return NULL; }
  form = mod->ctl_form;
  limage = image;
  if(image == NULL)
   { limage = mod->image; }
  if(limage == NULL)
   {strcpy(msg,"mod_init error: image = NULL");
    MODMsgBox(form,msg);
    return NULL; }
  widget = I_gphWidget(limage); /* see image.h for macro definition */
  if(widget == NULL) 
   {strcpy(msg,"mod_init error: graphic widget = NULL");
    MODMsgBox(form,msg);
    return NULL; }
  pikrec = NULL;
  pikrec = ErsGetPickingRecord(widget);
  if(pikrec != NULL)
   {strcpy(msg,"mod_init warn: Cannot connect picking twice on same widget");
    /* MODMsgBox(form,msg); */
    return pikrec; }

/******************************************
 * Retrieve or Allocate a model structure *
 * Check for existing model               *
 * model->PikRec->user_data->mod          *
 * mod->pikrec                            *
 *****************************************/
/******************************************
 * 1. install translations.             ***
 * 2. connect to widget.                ***
 * 3. saves PlotImage as part of record ***
 *****************************************/
 if(model == NULL && mod->pikrec == NULL)
  {/* very 1st connection, GUI will own model */
   mod->model      = (ErsModel *) new_model();
   mod->can_destroy_model = True;
   mod->Spik_or_Vpik= MODPICK;
   pikrec = (PR_ *) model_getpdata(mod->model);
   PR_SetTranslations(pikrec, dummytrans,
                      ErsOVERRIDE_TRANSLATIONS);
   PR_ConnectWidget(pikrec, widget, (caddr_t) limage);
  }

 else if(model == NULL && mod->pikrec != NULL)
  {/* 2nd or later connection, GUI will own model */
   modgui = (ModInfo *) PR_getgui((void *) mod->pikrec);
   if(modgui == NULL)
    {strcpy(msg,"mod_init error: old pikrec not connected to GUI");
     MODMsgBox(form,msg);
     return NULL;
    }
   if(modgui != mod)
    {strcpy(msg,"mod_init error: old modgui != new modgui"); 
     MODMsgBox(form,msg);
     return NULL;
    }
   if(mod->pikrec != model_getpdata(mod->model))
    {if(mod->Spik_or_Vpik == MODPICK)
      {strcpy(msg,"mod_init error: gui pikrec != model pikrec");
       MODMsgBox(form,msg);
       return NULL;
      }
    }
   mod->can_destroy_model = True;
   pikrec = mod->pikrec;
   PR_SetTranslations(pikrec, dummytrans,
                      ErsOVERRIDE_TRANSLATIONS);
   PR_ConnectWidget(pikrec, widget,(caddr_t)  limage);
  }

 else if(model != NULL && mod->pikrec == NULL)
  {/* First connection , GUI will not own model */
   pikrec = (PR_ *) model_getpdata((ErsModel *) model);
   if(pikrec == NULL)
    {strcpy(msg,"mod_init error: old pikrec = NULL\n");
     MODMsgBox(form,msg);
     return NULL;
    }
   
   wcount = PR_WidgetCount(pikrec);
   if(wcount == 0)
    {mod->model    = model;
     mod->pikrec   = model_getpdata((ErsModel *) model);
     mod->can_destroy_model = False;
     mod->Spik_or_Vpik= MODPICK;
    }
   else
    {strcpy(msg,"mod_init error:mod->pikrec == NULL, but widget count\n");
     strcat(msg,"               is not zero");
     MODMsgBox(form,msg);
     return NULL;
    }
    
   PR_SetTranslations(pikrec, dummytrans,
                      ErsOVERRIDE_TRANSLATIONS);
   PR_ConnectWidget(pikrec, widget,(caddr_t)  limage);
  }
 
 else if(model != NULL && mod->pikrec != NULL)
  {/* 2nd or later connection , GUI will not own model */
   pikrec = (PR_ *) model_getpdata((ErsModel *) model);
   if(pikrec == NULL)
    {strcpy(msg,"mod_init error: model pikrec = NULL");
     MODMsgBox(form,msg);
     return NULL;
    }
   if(model != mod->model)
    {strcpy(msg,"mod_init error: inconsistent model pointers");
     MODMsgBox(form,msg);
     return NULL;
    }
   modgui = (ModInfo *) PR_getgui((void *) mod->pikrec);
   if(modgui == NULL)
    {strcpy(msg,"mod_init error: pikrec not connected to GUI\n");
     strcat(msg,"                and this is not 1st connection");
     MODMsgBox(form,msg);
     return NULL;
    }
   if(modgui != mod)
    {strcpy(msg,"mod_init error: old model gui != new model gui"); 
     MODMsgBox(form,msg);
     return NULL;
    }
   wcount = PR_WidgetCount(mod->pikrec);
   if(wcount == 0)
    {strcpy(msg,"mod_init error: count = 0 & mod->pikrec != NULL");
     MODMsgBox(form,msg);
     return NULL;
    }
   if(mod->pikrec != model_getpdata(mod->model))
    {if(mod->Spik_or_Vpik == MODPICK)
      {strcpy(msg,"mod_init error: gui pikrec != model pikrec");
       MODMsgBox(form,msg);
       return NULL;
      }
    }
    
   mod->model    = model;
   mod->can_destroy_model = False;
   PR_SetTranslations(pikrec, dummytrans,
                      ErsOVERRIDE_TRANSLATIONS);
   PR_ConnectWidget(pikrec, widget,(caddr_t) limage);
  }
 else
  {strcpy(msg,"mod_init error: invalid call\n");
   MODMsgBox(form,msg);
   return NULL;
  }

/******************************************
 * If this is the first connection between*
 * widget and picking record, then...     *
 *1 Set header keys for the pick record   *
 *2 Connect picking record to gui         *
 *****************************************/
 if(PR_WidgetCount(pikrec) <= 1 )
  {
   pikrec->user_data = (caddr_t) mod;
   if(model == NULL)
    {
     Phdr = mod->phead;
     Shdr = mod->shead;
     Thdr = mod->thead;
     PR_SetPhdr(pikrec, Phdr);
     PR_SetShdr(pikrec, Shdr);
     PR_SetThdr(pikrec, Thdr);
    }
   else 
    {mod->phead = PR_GetPhdr(pikrec);
     mod->shead = PR_GetShdr(pikrec);
     mod->thead = PR_GetThdr(pikrec);
    }
  }

 mod->model->PikRecDestroy = (void (*)()) PR_Destroy;
 return pikrec;
}

/*****************************************************************/
/*                                                               */
/*  DisconnectGUI                                                */
/*  1. Disconnects picking record from all connected widgets     */
/*  2. Does a conditional destroy of the model structure.        */
/*  3. Will always destroy temporary velocity pick record        */
/*  4. Resets some of the ModInfo pointers.                      */
/*                                                               */
/*****************************************************************/
void DisconnectGUI(ModInfo *modgui)
{
  Widget      widget;
  ErsModel    *model;
  PR_ *pikrec;
  char msg[120];
  int count;
  register int i;

  if(modgui == NULL) return;
  model = modgui->model;
  pikrec= (PR_ *) modgui->pikrec;
  if(pikrec == NULL) return;

/*
 * Disconnect picking record from all widgets
 */
 while (PR_WidgetCount(pikrec)!= 0)
  {widget = PR_GetWidget(pikrec, 1);
   PR_DisconnectWidget(pikrec, widget);
  }
/*
 * Save Material data back to model structure.
 * Destroy the temporary picking record.
 */
 if(modgui->Spik_or_Vpik == VELPICK)
  {
   if(modgui->pikrec == model_getpdata(modgui->model))
    { strcpy(msg,"DisconnectGUI: inconsistent data");
     MODMsgBox(modgui->ctl_form,msg);
    }
   StoreVelPicks((void *) modgui);
   if(model_getpdata(model) != modgui->pikrec);
     PR_Destroy(modgui->pikrec);
  }
/*
 * Reinitialize pointers when all connections are cut.
 */
 count = PR_WidgetCount(pikrec);
 if(count <= 0)
  {modgui->pikrec = NULL;
   modgui->model  = NULL;
   pikrec->user_data = (caddr_t) NULL;
  }
/*
 * Destroy model if it is owned by the GUI
 */
 if(modgui->can_destroy_model) destroy_model(model);

}

void StoreVelPicks(void *data)
{
 ModInfo *modgui;
 PR_ *vpikrec;
 ErsMaterials *matsnew;
 extern ErsMaterials *pikrec_to_mats();
 modgui = (ModInfo *) data;
 if(modgui == NULL) return;

/*
 * If we are model picking then no action is needed
 */
 if(modgui->Spik_or_Vpik == MODPICK) return;
/*
 * We were Velocity Picking. Swap velocity and picking data.
 * Convert PickingRecord to Material data
 */
 vpikrec = modgui->pikrec;
 matsnew = pikrec_to_mats(vpikrec);
/*
 * Place modified material data back in the model structure
 * Will destroy the old material data.
 */
 if(matsnew != NULL) model_rep_mdata(modgui->model,matsnew);

}

void ModOrVelPick(void *data, int type)
{
 Widget    wlist[10];
 struct  PlotImage  *pilist[10];
 ErsCoordInf *T[10];
 ErsTransform *tx,*tz;
 ModInfo   *modgui;
 ErsModel  *model;
 PR_ *pikrec;
 ErsMaterials *matsnew,*mats;
 char       msg[120];
 int        count=0,n;
 Boolean    tempbool;
 extern ErsMaterials *pikrec_to_mats();
 extern PR_ *mats_to_pikrec();
 extern void ptr_sel_hor();
 extern void PointInfoShow();
 
 modgui = (ModInfo *) data;
 if(modgui==NULL)
  {printf("PcardPickChoice: NO GUI. Call me after GUI is built!\n");
   return;
  }
 model = modgui->model;
 if(model == NULL)
  {strcpy(msg,"ModOrVelPick: No model data available!");
   MODMsgBox(modgui->ctl_form,msg);
   return;
  }
 if(type != MODPICK && type != VELPICK)
  {strcpy(msg,"ModOrVelPick: Bad type requested!");
   MODMsgBox(modgui->ctl_form,msg);
   return;
  }


/* type = desired picking type */
 if(type == MODPICK)
  {/* return if we are already picking structure */
   if(modgui->pikrec == model_getpdata(modgui->model)) return;

   /*Close down the active velocity picking */
   StoreVelPicks((void *) modgui);
   count = PR_WidgetCount(modgui->pikrec);
   for(n=0;n<count;n++)
   {wlist[n] = PR_GetWidget(modgui->pikrec, n+1);
    pilist[n]= (struct PlotImage *) ErsGetPlotInf(wlist[n]);
    T[n] = ErsGetCoordInf(wlist[n]);
   }
   PR_Destroy(modgui->pikrec);
   pikrec = model_getpdata(modgui->model);
   for(n=0;n<count;n++)
    { PR_ConnectWidget(pikrec,wlist[n],(caddr_t) pilist[n]);
      ErsSetCoordInf0(wlist[n],T[n]);
    }
   modgui->pikrec= pikrec;
   modgui->Spik_or_Vpik = MODPICK;
   pikrec->user_data = (caddr_t) modgui;
   tempbool = modgui->can_destroy_model;
   modgui->can_destroy_model = False;
   sync_picking(modgui);
   modgui->can_destroy_model = tempbool;

  }

 if(type == VELPICK)
  {/* return if we are already picking velocity*/
   if(modgui->pikrec != model_getpdata(modgui->model)) return;

   /*1.  get existing materials data */
   /*2.  Create a Picking Record to hold material data */
   /*3.  Save pointer to the old structural Picking Record */
   /*4.  Connect new Picking Record to modgui */
   /*5.  reset Picking Record pointer in model */
   mats   = model_getmdata(modgui->model);
   if(mats == NULL)
    {MODMsgBox(modgui->mwig[MODPOP].w,
     "PcardPickChoice: No model material data available!");
     return;
    }
   pikrec = mats_to_pikrec(mats);
   if(pikrec == NULL)
    {strcpy(msg,"ModOrVelPick: Failed to convet velocity data\n");
     strcat(msg,"                 to a picking record");
     MODMsgBox(modgui->mwig[MODPOP].w,msg);
     return;
    }
   PR_HorizonCallback(pikrec, ptr_sel_hor, modgui,
                                   NULL, NULL, NULL, NULL);
   PR_PointCallback(pikrec, PointInfoShow, pikrec,
                                 NULL, NULL, NULL, NULL);

   pikrec->user_data = (caddr_t) modgui;
   count = 0;
   while (PR_WidgetCount(modgui->pikrec)!= 0)
   {wlist[count] = PR_GetWidget(modgui->pikrec, 1);
    pilist[count]= (struct PlotImage *) ErsGetPlotInf(wlist[count]);
    T[count] = ErsGetCoordInf(wlist[count]);
    PR_DisconnectWidget(modgui->pikrec, wlist[count]);
    count++;
   }
   for(n=0;n<count;n++)
    { PR_ConnectWidget(pikrec,wlist[n],(caddr_t) pilist[n]);
      ErsSetCoordInf0(wlist[n],T[n]);
    }
   modgui->pikrec = pikrec;
   modgui->Spik_or_Vpik = VELPICK;
   tempbool = modgui->can_destroy_model;
   modgui->can_destroy_model = False;
   sync_picking(modgui);
   modgui->can_destroy_model = tempbool;
  }

 return;
}
