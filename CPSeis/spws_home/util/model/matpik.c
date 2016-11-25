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
#include <string.h>
#include <pick.h>
#include <model.h>

PR_ *mats_to_pikrec(ErsMaterials *mats);
ErsMaterials *pikrec_to_mats(PR_ *matpr);

PR_ *mats_to_pikrec(ErsMaterials *mats)
{ErsMaterial   *mat;
 ErsTransform  *tx,*ty,*tz;
 PR_ *matpr;
 ErsHorizon    *horizon;
 ErsPoint      *point;
/* char   hname[16],color[24]; */
 char   *hname,*color,name[24];
 long   count;
 int    npts;
 long   nmats;
 int    i,n;

 matpr = NULL;
 if(mats == NULL) return NULL;
 nmats = materials_count(mats);

 matpr = PR_Create();
 if(matpr == NULL) return NULL;
 tx = mats_get_transx(mats);
 tz = mats_get_transz(mats);
 PR_setx_transf(matpr,tx);
 PR_setz_transf(matpr,tz);
 if(nmats <= 0) return matpr;

 for(i=0;i<nmats;i++)
  {
   mat = mats_get_nth(mats,i);
   if(mat != NULL)
    {int *stype,ndof,mid;
     float *x,*y,*pv;

     material_get(mat,&ndof,&mid,&npts,&stype,&x,&y,&pv);
     sprintf(name,"mid%d",mid);
     hname = name;
     if(npts > 0)
      {
       color = material_getcolor(mat);
       hname = material_getname(mat);
       horizon = ErsHorizonCreate(matpr, hname, color, 2 ,False);
       horizon->hid = mid;
       point = ErsPointCreate(y[0], UNDEFINED_TN,
               &pv[0], x[0], 0, 0);
       ErsPointAdd(matpr, horizon, point, ErsSILENT);
      }
     for(n=1;n<npts;n++)
      {if(stype[n] != stype[n-1])
        {horizon = ErsHorizonCreate(matpr, hname, color, 2 ,False);
        }
       point = ErsPointCreate(y[n], UNDEFINED_TN,
               &pv[n*ndof], x[n], 0, 0);
       ErsPointAdd(matpr, horizon, point, ErsSILENT);
      }
    }
  }

 return matpr;
}

ErsMaterials *pikrec_to_mats(PR_ *matpr)
{ErsMaterials  *mats;
 ErsMaterial   *mat;
 ErsHorizon    *uhlist[99],*hlist[99];
 ErsPoint      *point;
 ErsTransform *tx,*ty,*tz;
 long   ndof;
 float  x[500],y[500],pv[500];
 long   npts;
 int    type[500];
 long   num;
 int    i,j,n,mid,N,nmats,count;

 mats = NULL;
 if(matpr == NULL) return NULL;
 tx = PR_x_transf(matpr);
 tz = PR_z_transf(matpr);
 ErsHorizonGetHList(matpr,&nmats, uhlist);

 mats = new_materials();
 if(mats == NULL) return mats;
 mats_set_transx(mats,tx);
 mats_set_transz(mats,tz);
 if(nmats <= 0) return mats;

 ndof = 1;
 for(i=0;i<nmats;i++)
  { mid = uhlist[i]->hid;
    ErsHorizonMemberList(matpr, uhlist[i], &count, &N, hlist);
    mat = new_material();
    material_setname(mat,uhlist[i]->horizon_name);
    material_setcolor(mat,uhlist[i]->color_name);
    num = 0;
    for(n=0;n<count;n++)
     { 
       point =hlist[n]->first_point;
       while(point != NULL)
        {for(j=0;j<=point->npicks;j++)
          {if(j == point->npicks)
            { x[num] = point->pkey;
              y[num] = point->time;
              pv[num]= (float ) point->user_data;
            }
           else
            { x[num] = point->pick_list[j].pkey;
              y[num] = point->pick_list[j].time;
              pv[num]= (float ) point->pick_list[j].user_data;
            }
            type[num]=n;
           num++;
          }
         point = point->next;
        }
     }
    material_set(mat,ndof,mid,num,type,x, y, pv);
    j = materials_add(mats,mat);
  }

 return mats;
}
