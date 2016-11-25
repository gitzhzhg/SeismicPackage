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
#include <math.h>
#include "pick.h"

/********************************************************************
 * Following functions operate upon ErsPoint records              ***
 * X= links to X, G= links to a GUI, N=no X or G links.           ***
 * ErsPointDelete       XG    Kill a Point and redraw the horizon.***
 * ErsPointInsert       XG    'Insert' point into Horizon & redraw***
 * ErsPontDraw          XG    Redraw a single point in a Horizon  ***
 * ErsPointDrawAll      XG    Redraw all horizon points           ***
 * ErsRedrawBoundingBox XG    Update a subset of the image.       ***
 * ErsPointCreate       N     Create a new point structure.       ***
 * ErsPointDestroy      N     Call ErsPointRemove & ErsPointFree. ***
 * ErsPointRemove       N     Remove Point from horizon list      ***
 * ErsPointFree         N     Free memory for point structure.    ***
 * ErsPointCount        N     Return number of points in point    ***
 * ErsPointFind         N     Find closest Point (and Horizon)    ***
 * FindNNP              N     Find Near Neighbor Points to a point***
 * ErsPointAdd          N     'Insert point into horizon          ***
 * ErsPointAddPickList  N     Add a whole series of picks.        ***
 *
 * ErsPointMinMax       N     Compute min & max time and pkey     ***
 * ErsPointInside       N     Test if a point lies in a rectangle.***
 * ErsPointTimeMinMax   N     Compute Point min & max times.      ***
 *******************************************************************/

ErsPoint *ErsPointCreate(float pick_time, long tn, float *user_data,
float pkey, float skey, float tkey )
{ static long seqno = 1;
  ErsPoint *point;

  point           = (ErsPoint *) calloc(1,sizeof(ErsPoint));
  point->time     = pick_time;
  point->tn       = tn;
  point->pkey     = pkey;
  point->skey     = skey;
  point->tkey     = tkey;
  point->previous = NULL;
  point->next     = NULL;
  point->seqno    = seqno++;

  point->npicks   = 0;
  point->pick_list= NULL;
  if(user_data != NULL) point->user_data= *user_data;

  return(point);
}

Bool    ErsPointDestroy(ErsHorizon *horizon, ErsPoint *point)
{
  ErsPointRemove(horizon, point);
  ErsPointFree(point);
}

Bool    ErsPointRemove( ErsHorizon *horizon, ErsPoint *point)
{ if (point == NULL) return;
  /* we should check if point belongs to horizon !! */

  horizon->number_of_points--;
  if (point == horizon->first_point) horizon->first_point = point->next;
  if (point == horizon->last_point) horizon->last_point = point->previous;

  if (point->previous != NULL) point->previous->next = point->next;
  if (point->next != NULL) point->next->previous = point->previous;

  return(True);
}

void ErsPointFree( ErsPoint *point)
{
  if (point->npicks != 0) { free(point->pick_list); }
  free(point);
}

ErsPoint *ErsPointFind(PR_ *picking_record, ErsHorizon *horizon,
float pkey, float time, float delta_key, float delta_time)
{
  ErsPoint *point, *selected_point;
  int       horizon_count;
  float     pkey_distance,keytol;
  float     time_distance;
  register int i;


  time_distance = 0;
  selected_point= NULL;
  keytol = delta_key;

  if (horizon == NULL)
    { /* search all the horizons */
     horizon_count = picking_record->horizon_count;
     if (horizon_count == 0) return(NULL);
     horizon = picking_record->horizon_list[0];
    }
  else
    {horizon_count = 1; }

  for (i=0; i<horizon_count; i++)
    {point = horizon->first_point;
     while (point != NULL)
       { pkey_distance = (float) fabs((double)(point->pkey - pkey));
        if (pkey_distance < keytol)
          { time_distance = (float) fabs((double) (time - point->time));
            if (time_distance <= delta_time)
              {selected_point = point;
               keytol = (float) fabs((double)(point->pkey - pkey));
              }
          }
        point = point->next;
       }
     if (i+1 < horizon_count) horizon = picking_record->horizon_list[i+1];
    }

  if (selected_point != NULL)
    {time_distance = (float) fabs((double) (time - selected_point->time));
     if (keytol <= delta_key && time_distance <= delta_time)
      return(selected_point);
    }

  return(NULL);
}

int ErsPointCount(ErsPoint *point)
{ if(!point) return 0;
  return point->npicks+1;
}

void ErsPointAdd(picking_record, horizon, point, insertion_mode)
PR_ *picking_record;
ErsHorizon *horizon;
ErsPoint *point;
int insertion_mode;
{
 ErsPoint *wpoint,*ppt,*npt;
 Widget   widget;
 int      x, y;
 register int i;

 horizon->number_of_points++;

  if (horizon->number_of_points == 1)
    {if(point->tn != UNDEFINED_TN)
       {horizon->max_tn = point->tn;
        horizon->min_tn = point->tn; }
      horizon->max_time = point->time;
      horizon->min_time = point->time;
      horizon->max_pkey = point->pkey;
      horizon->min_pkey = point->pkey;
    }
   else
    {if(point->tn != UNDEFINED_TN)
       {if (point->tn > horizon->max_tn) horizon->max_tn = point->tn;
        if (point->tn < horizon->min_tn) horizon->min_tn = point->tn;}
      if (point->time > horizon->max_time)
       horizon->max_time = point->time;
      if (point->time < horizon->min_time)
       horizon->min_time = point->time;
      if (point->pkey > horizon->max_pkey)
       horizon->max_pkey = point->pkey;
      if (point->pkey < horizon->min_pkey)
       horizon->min_pkey = point->pkey;
    }

  if (horizon->first_point == NULL)
    {/* This is the 1st point */
     horizon->first_point = point;
     horizon->last_point = point;
     return;
    }

  if (insertion_mode == ErsAPPEND || insertion_mode == ErsAUTO_PICK ||
      insertion_mode == ErsSILENT)
    {horizon->last_point->next = point;
     point->previous = horizon->last_point;
     horizon->last_point = point;
    }
  else if (insertion_mode == ErsFIRST)
    {point->next = horizon->first_point;
     horizon->first_point->previous = point;
     horizon->first_point = point;
    }
  else if (insertion_mode == ErsINSERT)
    { int nearn;
      nearn = FindNNP(horizon,point,&ppt, &npt);
    }
  else if (insertion_mode == ErsINSERT_TIME)
    {
     wpoint = horizon->first_point;
     while (wpoint->time < point->time && wpoint->next != NULL)
       wpoint = wpoint->next;
     if (wpoint->time> point->time)
       {/* insert before */
        point->previous = wpoint->previous;
        if (wpoint->previous != NULL) wpoint->previous->next = point;
        else horizon->first_point = point;
        point->next = wpoint;
        wpoint->previous = point;
       }
     else
       {/* insert after */
        point->previous = wpoint;
        point->next = wpoint->next;
        if (wpoint->next != NULL) wpoint->next->previous = point;
        else horizon->last_point = point;
        wpoint->next = point;
       }
    }
 return;
}

int FindNNP(ErsHorizon *horizon, ErsPoint *point,ErsPoint **np1, ErsPoint **np2)
{/* Find the nearest neighbor points to point in horizon */
 /* Link point to the 0 to 2 neighboring points          */
 /* Return the number of neighbors surrounding point.    */
  ErsPoint *wpoint,*nearpt, *Pt1;
  float    time_dist,time_dist1,time_dist2,key_dist,key_dist1,key_dist2;
  float    distc,distp,distn,tgap;

 /* wpoint = working point
  * npoint = nearest point
  * Pt1    = previous point
  */
  wpoint = horizon->first_point;
  nearpt = horizon->first_point;
  Pt1    = NULL;
  *np1   = NULL;
  *np2   = NULL;
  distn  = 0.;
  distp  = 0.;
  distc  = (wpoint->pkey - point->pkey);
  if(wpoint == NULL)  return 0; /* Only 1 point in horizon? */
  while ( wpoint->next != NULL) 
    { key_dist  = (wpoint->next->pkey - point->pkey);
      if(fabs((double) key_dist) < fabs((double) distc))
       { distc = key_dist;
         nearpt = wpoint->next;
         if(nearpt->next != NULL)
                distn = (nearpt->next->pkey - point->pkey);
         if(nearpt->previous != NULL)
                distp = (nearpt->previous->pkey - point->pkey);
       }
      key_dist2 = (wpoint->pkey - point->pkey);
      if(key_dist*key_dist2 <= 0. )
       {
        time_dist2= (wpoint->time - point->time);
        key_dist1 = (wpoint->next->pkey - wpoint->pkey);
        time_dist1= (wpoint->next->time - wpoint->time);
        time_dist = time_dist2;
        if(key_dist1 != 0.)
        time_dist = time_dist1*(key_dist2/key_dist1) - time_dist2;
        if(Pt1 == NULL)
         {tgap = time_dist;
          Pt1 = wpoint;
         }
        else
         {if(fabs((double) time_dist) <= fabs((double) tgap))
          { tgap = time_dist; Pt1 = wpoint; }
         }
       }
       wpoint = wpoint->next;
    }

  if(Pt1 != NULL) /* insert between delimiting points */
   {point->previous = Pt1;
    point->next     = Pt1->next;
    *np1 = point->previous;
    *np2 = point->next;
    Pt1->next->previous= point;
    Pt1->next    = point;
    return 2;
   }

  if(nearpt->next == NULL) /* nearpt is the last_point */
   {if( distp*distc >= 0.) /* insert after nearpt */ 
     { point->previous = nearpt;
       point->next     = NULL;
       *np1 = point->previous;
       *np2 = point->next;
       nearpt->next    = point;
       horizon->last_point= point;
       return 1;
     }
    else                  /* add before nearpt */
     { point->previous = nearpt->previous;
       point->next     = nearpt;
       *np1 = point->previous;
       *np2 = point->next;
       nearpt->previous->next= point;
       nearpt->previous= point;
       return 2;
     }
   }
  else if(nearpt->previous == NULL) /* nearpt is the first_point */
   {if( distn*distc >= 0.) /* insert before nearpt*/
     { point->next     = nearpt;
       point->previous = NULL;
       *np1 = point->previous;
       *np2 = point->next;
       nearpt->previous= point;
       horizon->first_point= point;
       return 1;
     }
    else                  /* add after nearpt */
     { point->previous = nearpt;
       point->next     = nearpt->next;
       *np1 = point->previous;
       *np2 = point->next;
       nearpt->next->previous= point;
       nearpt->next    = point;
       return 2;
     }
   }
  else
   {if( distn*distc <= 0.) /* insert after nearpt*/
     { point->previous = nearpt;
       point->next     = nearpt->next;
       *np1 = point->previous;
       *np2 = point->next;
       nearpt->next->previous= point;
       nearpt->next    = point;
       return 2;
     }
    else if(distp*distc <=0) /* add before nearpt */
     { point->previous = nearpt->previous;
       point->next     = nearpt;
       *np1 = point->previous;
       *np2 = point->next;
       nearpt->previous->next= point;
       nearpt->previous= point;
       return 2;
     }
    else                   /* insert after nearpt */
     { point->previous = nearpt;
       point->next     = nearpt->next;
       *np1 = point->previous;
       *np2 = point->next;
       nearpt->next->previous= point;
       nearpt->next    = point;
       return 2;
     }
   }
}

void ErsPointAddPickList(ErsHorizon *horizon, ErsPoint *point,
                         ErsPointList *pick_list, int npicks)
{ register int i;
  if (npicks == 0) return;

  if (point->pick_list != NULL) free(point->pick_list);

  point->pick_list = (ErsPointList *) calloc(1,npicks*sizeof(ErsPointList));
  point->npicks = npicks;

  for (i=0; i<npicks; i++)
   { point->pick_list[i].time = pick_list[i].time;
     point->pick_list[i].traceno = pick_list[i].traceno;
     point->pick_list[i].pkey = pick_list[i].pkey;
     point->pick_list[i].skey = pick_list[i].skey;
     point->pick_list[i].tkey = pick_list[i].tkey;
     if (point->pick_list[i].time > horizon->max_time)
      horizon->max_time = point->pick_list[i].time;
     if (point->pick_list[i].pkey > horizon->max_pkey)
      horizon->max_pkey = point->pick_list[i].pkey;
     if (point->pick_list[i].pkey < horizon->min_pkey)
      horizon->min_pkey = point->pick_list[i].pkey;
     else if (point->pick_list[i].time < horizon->min_time)
      horizon->min_time = point->pick_list[i].time;
   }
}

void ErsPointMinMax(point,xmin,xmax,tmin,tmax)
ErsPoint *point;
float    *tmin,*tmax,*xmin,*xmax;
{
  register float Min_t, Max_t, Min_x, Max_x;
  register int i;

  if(point == NULL) return;
  *tmin = point->time;
  *tmax = point->time;
  *xmin = point->pkey;
  *xmax = point->pkey;
  if (point->npicks == 0) return;

  Min_t = *tmin;
  Max_t = *tmax;
  Min_x = *xmin;
  Max_x = *xmax;
  for (i=0; i<point->npicks; i++)
    { if (point->pick_list[i].time < Min_t)
         Min_t = point->pick_list[i].time;
      else if (point->pick_list[i].time > Max_t)
         Max_t = point->pick_list[i].time;
      if (point->pick_list[i].pkey < Min_x)
         Min_x = point->pick_list[i].pkey;
      else if (point->pick_list[i].pkey > Max_x)
         Max_x = point->pick_list[i].pkey;
    }
  *tmin = Min_t;
  *tmax = Max_t;
  *xmin = Min_x;
  *xmax = Max_x;
}

