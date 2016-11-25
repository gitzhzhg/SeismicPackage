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
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/Text.h>
#include "mod_pop.h"
#include "image.h"
#include "pcard.h"
#include "pick.h"



#define WRITESTR "Writting model file, please wait..."


Bool mod_stop( Widget     modpop  )

{
  Widget              graphic;
  ModInfo             *mod;
  PR_                 *pikrec;
  char                filename[80];
  int                 l,i,j,istat;
  Arg arglist[2];
  Boolean             stat;
  Widget              shell, pshell;
  char                tmpstr[200],msg[160];
  char               *tempstr;
  long                wdtyp=1,nrecl=0;


  XtVaGetValues( modpop,  XmNuserData, &mod, NULL );
  if(mod == NULL)    return False;
  pikrec = mod->pikrec;
  if(pikrec == NULL) return False;

  l = strlen(mod->outfile);
  if(l > 79) l = 79;
  strncpy(filename,mod->outfile,l);
  filename[0] = '\0';
  i=0; j=0;
  while(j <79 && i < 100)
   { if(mod->outfile[i] == ' ' || mod->outfile[i]=='\t')
       { i++; }
     else
       { filename[j] = mod->outfile[i];
         if(filename[j] == '\0') break; 
         i++;
         j++;
       }
   }
  filename[j] = '\0';
  
  if( j == 0)
    { strcpy(filename,"untitled.pck"); }
  
  j = exist_(filename);
  if(j == 0)
   {sprintf(msg,"Warning: File %s being overwritten",filename);
    strcat(msg,filename);
    MODMsgBox(mod->mwig[MODPOP].w,msg);
   }
  strcpy(tmpstr, mod->infile);
  graphic = I_gphWidget(mod->image);
  pcardSetDefaults(graphic, mod->model);
  shell = (Widget)XutilGetShell(modpop);
  pshell = (Widget)XutilGetShell(XtParent(shell));
  wprocCursorSet( shell,  mod->watch_cur);
  tempstr= wprocPushMsg(  mod->help_text, WRITESTR);
  XSync(XtDisplay(shell), False);

/* Update materials info before saveing to disk*/
  if(mod->Spik_or_Vpik == VELPICK) StoreVelPicks((void *) mod);

  if(pcardwr( mod->model, mod->outfile,mod->outtyp) == True)
    {int   lun1,lun2;
     int   iform=0;   /*formatted*/
     int   opstat=1;  /*old       */
     int   iaccess=0; /*sequential*/

      j = strlen(mod->infile);
      if(j > 0)
       {Boolean open;
        open = True;
        if(strncmp(mod->infile,"NONE",4)==0) open=False;
        if(strncmp(mod->infile,"none",4)==0) open=False;
        if(mod->infile[0] == ' ') open=False;
        if(open == True)
         {rmodopen_w_(&lun1, mod->infile,&iform,&opstat,&iaccess,
          &nrecl,&wdtyp, &istat);
          rmodopen_w_(&lun2, mod->outfile,&iform,&opstat,&iaccess,
          &nrecl,&wdtyp, &istat);
          rmodapen_(&lun1,&lun2); /* appends infile to outfile */
          rmodclos_w_(&lun1);
          rmodclos_w_(&lun2);
         }
       }

      mod_clean(mod, True);
      XtUnmanageChild( mod->mwig[MODPOP].w );
      wproc_setsen(mod->mwig,True,4, PHEAD,SHEAD,THEAD,IFIL);
      strcpy(mod->outfile,"untitled.pck");
      strcpy(mod->infile ,"NONE");
      stat=  True;
    }
  else
    { 
      XtManageChild( mod->mwig[MODPOP].w );
      strcpy(msg,"mod_stop: MODEL NOT WRITTEN!\n");
      strcat(msg,"       file name was, ");
      strcat(msg,filename);
      wprocShowMsg( mod->errbox, msg);
      stat=  False;
    }

  wprocPopMsg(  mod->help_text, tempstr);
  wprocCursorNone(shell);
  wprocCursorNone(pshell);

  return stat;

}

