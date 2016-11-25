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
#include "draw_mod.h"
#include "vect/ll_seis_vect.hh"
#include "vect/vector.hh"
#include "data/modbase_data.hh"


/********
C\USER DOC
C-----------------------------------------------------------------------
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C       written in c -- designed to be called from c
C
C     Utility Name:  draw_pdat, draw_hdat, draw_sdat
C          Written:  93/01/01  by:  R.S.Day
C     Last revised:  95/11/30  by:  
C
C  Purpose:       Plot Picking Record, Horizon , or Segment data
C
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C
C  machine            source code directory       library
C  -------            ---------------------       -------
C  ultrix             ~spws/util/graphics         graphlib.a
C
C  c files          c++ files       fortran files      other files
C  -------          ---------       -------------      -----------
C  draw_pdat.c      none                               
C----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C
C  Libraries:    picklib.a
C  Header files: Pick.h
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2. 95/11/30  R.S.Day    Corrected bug with insert call
C  1. 93/01/01  R.S.Day    Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                CALLING SEQUENCE        (I/O: IN, OUT, BOTH)
C
C int draw_vpdat(void *vl,PR_ *pr,int *lorm,  int *labels,
C            long *iwk, float *ul, float *ur, float *ut, float *ub);
C int draw_vhdat(void *vl, PR_ *pr, ErsHorizon *horizon);
C Vector   *draw_vsdat(void *vl, ErsHorizon *horizon );
C ModBaseData *SegmentToModBaseData(ErsHorizon *horizon);
C
C  Type    Name    I/O     Valid     Description  
C  ----    ----    ---     -----     -----------
C  int *   labels  IN      0,1       Data and axes, or data only
C  int *   lorm    IN      0,1,2,3   Invisible,marks,lines,or both 
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. draw_hdat will plot the segment "horizon" plus all segments with
C     the same name as "horizon". draw_sdat plots only the single
C     segment 'horizon'
C  2. See Pick.h for definition of PR_
C  3. See modelio.c & model_io.h for methods to input data
C  4. call xopws, gsvp, and gswn prior to these calls
C-----------------------------------------------------------------------
C\END DOC
****/

int draw_vhdat(void *vl, PR_ *pr, ErsHorizon *horizon,int *lorm, int zeit)
{ErsHorizon *hlist[99];
 Vector *vect;
 int   phd,shd,thd;
 int   i,count,N;
 if(horizon == NULL || pr == NULL) return 0;
 
 phd = PR_GetPhdr(pr);
 shd = PR_GetShdr(pr);
 thd = PR_GetThdr(pr);
 ErsHorizonMemberList(pr, horizon, &count, &N, hlist);
 for(i=0;i<count;i++)
  {hlist[i]->horizon_active = True; 
   if(*lorm == 0) hlist[i]->horizon_active = False; 
   vect = draw_vsdat(vl, hlist[i],phd,shd,thd, zeit);
   if(vect == NULL) return i;
  }
 return count;
}

Vector *draw_vsdat(void *VL, ErsHorizon *horizon ,
       int phd,int shd, int thd, int zeit)
{ModBaseData *dobj;
 SeisVectLinkedList *vl;
 Vector   *vect;
 int   lstyle,mstyle;
 char  *hname,*cname;
 unsigned int   lwidth,mwidth,msize;
 vl = (SeisVectLinkedList *) VL;
 if(horizon == NULL || vl == NULL) return NULL;
 hname = horizon->horizon_name;
 cname = horizon->color_name;
 lwidth= (unsigned int) horizon->line_width;
 lstyle= Vector::SolidLine;
 mstyle= Vector::FilledSquareMarker;
 msize = 2*lwidth + 3;
 mwidth= 1;
 vect  = (Vector *) ErsHorizonGetVect(horizon);

 if(vect == NULL)
  { dobj = NULL;
    dobj = SegmentToModBaseData(horizon,phd,shd,thd,zeit);
    if(dobj == NULL) return NULL;
    vect = vl->add(hname,dobj,cname,lwidth,False,(Vector::VectorStyle) lstyle,
           (Vector::VectorMarker) mstyle,msize,mwidth);
    ErsHorizonSetVect(horizon,(void *) vect);
    if(vect == NULL) return vect;
    vect->makeVisible();
    return vect;
  }
 else 
  { vect->makeInvisible();
    vect->setColor(cname);
    vect->setWidth(lwidth);
    vect->setStyle((Vector::VectorStyle) lstyle);
    vect->setMarker((Vector::VectorMarker) mstyle,msize,mwidth);
    if(horizon->horizon_active) vect->makeVisible();
    else vect->makeInvisible();
  }

 return vect;
}

void SegmentSetVisible(ErsHorizon *hz,int visible)
{Vector *vect;
 vect  = (Vector *) ErsHorizonGetVect(hz);
 if(vect == NULL) return;
 if(visible != 0) { vect->makeVisible(); hz->horizon_active=True; }
 else { vect->makeInvisible(); hz->horizon_active=False; }
 
}

void SegmentSetAttributes(ErsHorizon *hz,int lstyle, unsigned int lwidth,
     int mstyle, unsigned int msize, unsigned int mwidth,char *color,
     int /* visible*/)
{Vector *vect;
 vect  = (Vector *) ErsHorizonGetVect(hz);
 if(vect == NULL) return;
 vect->makeInvisible();
 vect->setColor(color);
 vect->setWidth(lwidth);
 if(lstyle >= 0) vect->setStyle((Vector::VectorStyle) lstyle);
 if(mstyle >= 0) vect->setMarker((Vector::VectorMarker) mstyle,msize,mwidth);
 if(hz->horizon_active) vect->makeVisible();
    else vect->makeInvisible();

 hz->line_width = (int) lwidth;
}

ModBaseData *SegmentToModBaseData(ErsHorizon *horizon,
             int phd,int shd,int thd, int zeit)
{ErsPoint *point;
 ModBaseData *dobj;
 float x[100],z[100],s[100],t[100];
 int   i,id,num,count;
 if(horizon == NULL) return NULL;

 dobj = NULL;
 count= 0;
 num  = 0;
 point= ErsHorizonGetFirstPoint(horizon);
 while(point != NULL)
  {for(i=0;i<=point->npicks;i++)
    {if(i == point->npicks)
      { x[num]= point->pkey; z[num] = point->time;
        s[num]= point->skey; t[num]= point->tkey; 
      }
     else
      { x[num] = point->pick_list[i].pkey;
        s[num] = point->pick_list[i].skey;
        t[num] = point->pick_list[i].tkey;
        z[num] = point->pick_list[i].time;
      }
     num++;
     count++;
     if(num >= 100)
      {if(dobj == NULL)
        dobj = new ModBaseData(num,phd,x,shd,s,thd,t,zeit, z,NULL);
       else
        dobj->insert(count-num,num,x,z,s,t,NULL);
       num = 0;
      }
    }
   point = ErsPointNext(point);
  }
 if(num > 0)
  {if(dobj == NULL)
    dobj = new ModBaseData(num,phd,x,shd,s,thd,t,zeit,z,NULL);
   else
    dobj->insert(count-num,num,x,z,s,t,NULL);
  }
 id = (int) horizon->hid;
 dobj->setid(id);

 return dobj;
}

int ModBaseDataToHorizon(ModBaseData *dobj,ErsHorizon *hz)
{
 ErsPoint *point;
 float user_data;
 long  trace;
 int   i,count;
 if(hz == NULL || dobj == NULL) return 0;
 hz->hid = dobj->getid();
 count= (int) dobj->getNumPts();
 for(i=0;i<count;i++)
  {user_data = dobj->getUser(i);
   trace = 0;
   point = ErsPointCreate(dobj->getY(i), trace, &user_data,
           dobj->getX(i), dobj->getS(i), dobj->getT(i) );
   ErsPointAdd(NULL,hz,point,ErsSILENT);
  }

 return count;
}

PR_ *VectorListToPR_(void *vls)
{SeisVectLinkedList *vlist;
 Vector *vect,**vectarr;
 ModBaseData *dobj;
 PR_ *pr;
 ErsHorizon *horizon;
 int i,m,n,num,nsegs,npts;
 int lwidth,marker,msize,mwidth;

 char  name[96],color[64];
 char *names[199];
 if(vls == NULL) return NULL;
 vlist = (SeisVectLinkedList *) vls;
 i = UniqueVectNameList(vls ,&num ,199, names);
 if(i <= 0) return NULL;

 pr = PR_Create();
 n= 0;
 while(names[n] != NULL)
  {nsegs = vlist->find(names[n],&vectarr);
   m = 0;
   while(vectarr[m] != NULL && m < nsegs)
     {vect = vectarr[m];
      dobj = (ModBaseData *) vect->getData();
      strcpy(name,"dummy");
      if(vect->getName() != NULL) strcpy(name,vect->getName());
      strcpy(color,"red");
      if(vect->getColor() != NULL) strcpy(color,vect->getColor());
      lwidth  = (int) vect->getWidth();
      vect->getMarker((Vector::VectorMarker *) &marker,
       (unsigned int*) &msize,(unsigned int *) &mwidth);

      horizon = ErsHorizonCreate(pr,name, color, lwidth,False);
      horizon->symbol_size= msize;
      npts = ModBaseDataToHorizon(dobj,horizon);
      ErsHorizonSetVect(horizon,(void *) vect);
      
      m++;
     }
    delete vectarr;
   n++;
  }
 if(dobj != 0)
  {PR_SetPhdr(pr,dobj->getphdr());
   PR_SetShdr(pr,dobj->getshdr());
   PR_SetThdr(pr,dobj->getthdr());
  }
 UniqueVectNameFree(names);
 return pr;
}

SeisVectLinkedList *PR_ToVectorList(PR_ *pr,int phd,int shd, int thd,int zeit)
{ModBaseData *dobj;
 SeisVectLinkedList *vl;
 Vector   *vect;
 ErsHorizon *horizon;
 int   i,n,lstyle,mstyle,count,id;
 char  hname[32],cname[32];
 unsigned int   lwidth,mwidth,msize;

 if( PR_HorizonCount(pr) < 1) return NULL;
 vl = new SeisVectLinkedList();
 if(vl == NULL) return NULL;
 count = 0;
 n = 0;
 for(i=0;i< PR_HorizonCount(pr);i++)
  {
    horizon = pr->horizon_list[i];
    if(horizon == NULL) goto error;
    strcpy(hname , horizon->horizon_name);
    strcpy(cname , horizon->color_name);
    id = (int) horizon->hid;
    lwidth= (unsigned int) horizon->line_width;
    lstyle= Vector::SolidLine;
    mstyle= Vector::FilledSquareMarker;
    msize = 2*lwidth + 3;
    mwidth= 1;

    dobj = NULL;
    dobj = SegmentToModBaseData(horizon,phd,shd,thd,zeit);
    if(dobj == NULL) goto error;
    vect = vl->add(hname,dobj,cname,lwidth,False,(Vector::VectorStyle) lstyle,
           (Vector::VectorMarker) mstyle,msize,mwidth);
    ErsHorizonSetVect(horizon,(void *) vect);
    if(vect != NULL) vect->makeVisible();
    else goto error;
    count += 1;
 error:
   n++;
  }

 return vl;
}

