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


void ErsHorizonSegmentFree( ErsHorizon *horizon);
/********************************************************************
 * Following functions operate upon horizon records                ***
 * X= links to X, G= links to a GUI, N=no X or G links.            ***
 * ErsHorizonNew             N Create a new horizon                ***
 * ErsHorizonCreate          N Create a new horizon & add to PR    ***
 * ErsHorizonFree            N Free all segments in a horizon      ***
 * ErsHorizonSegmentFree     N Free 1 horizon-segment              ***
 * ErsHorizonSetColor        X Allocates a named color & sets pixel***
 * ErsHorizonCheckAttributes N Looks for name match, copys attrib. ***
 * ErsHorizonDestroy        GX Eliminate 1 or more horizon-segments***
 * ErsHorizonSegmentDestroy GX Eliminate a horizon-segment.        ***
 * ErsHorizonGetSegments     N Get segments by name                ***
 * ErsHorizonGetSegmentN     N Returns the N segment of a horizon  ***
 * ErsHorizonGetSegmentCount N Return segment count in a horizon   ***
 * ErsHorizonMemberList      N Get segment list given a horizon *  ***
 * ErsHorizonGetHList        N Get list of unique horizons-names   ***
 * ErsHorizonChangeAttributes  Alter a horizons attributes.        ***
 * ErsHorizonGet             N Returns the n'th horizon from a list***
 * ErsHorizonSetGC           X Set the  GC for the horizon.        ***
 * ErsHorizonUpdateMinMax    N update min and max trace and time. ***
 * ErsHorizonMinMax          N return min & max pkey and time      ***
 * ErsHorizonGetFromPoint    N Return horizon containing point struct*
 * ErsHorizonGetByName       N Return horizon by name.             ***
 * ErsHorizonFind              Return horizon closest to a point.  ***
 * ErsHorizonGetPointCount     Count the number of horizon points. ***
 * ErsHorizonGetNumPicks     N Count the picks in a horizon        ***
 * ErsHorizonGetVect         N get void pointer to Vector          ***
 * ErsHorizonSetVect         N set void pointer to Vector          ***
 * See Pick.h for prototypes                                       ***
 *******************************************************************/

ErsHorizon *ErsHorizonNew( char *horizon_name, char *color_name,
            int line_width,Bool    select_horizon_flag)
{ ErsHorizon *horizon;
  int    i,j,k,status;
  char   s[8],str[32];
  static long ncall;

  /* allocate memory for horizon */
  horizon = (ErsHorizon *) calloc(1,sizeof(ErsHorizon));

  /* initialize */
  horizon->number_of_points = 0;
  horizon->line_width       = line_width;
  horizon->first_point      = NULL;
  horizon->last_point       = NULL;
  horizon->hid              = UNDEFINED_HID;
  horizon->user_int         = UNDEFINED_HID;
  horizon->symbol_size      = 0;
  horizon->horizon_active   = True;

  /* do some checking */
  if (color_name != NULL)
   {horizon->color_name = (char *) malloc(strlen(color_name)+1);
    strcpy(horizon->color_name,color_name);
   }
  else
   {horizon->color_name = (char *) malloc(4);
    strcpy(horizon->color_name,"red");
   }

  if (horizon_name != NULL)
   {horizon->horizon_name= (char *) malloc(strlen(horizon_name)+1);
    strcpy(horizon->horizon_name,horizon_name);
   }
  else
   {sprintf(str,"HORIZ%-3d",ncall);
    ncall++;
    horizon->horizon_name= (char *) malloc(strlen(str)+1);
    strcpy(horizon->horizon_name,str);
   }
 return horizon;
}
 
ErsHorizon *ErsHorizonCreate(PR_ *picking_record,
            char *horizon_name, char *color_name,
            int line_width,Bool    select_horizon_flag)
{ ErsHorizon *horizon;
  int    i,j,k,status;
  char   s[8],str[32];

  /* allocate memory for horizon */
  horizon = ErsHorizonNew( horizon_name, color_name,
                           line_width,   select_horizon_flag);

  /* Check for name match and set initial value for hid flag */
  /* will also be called by ErsHorizonSetColor */
  ErsHorizonCheckAttributes(picking_record, horizon);

  /* add horizon to picking_record list */
  if (picking_record->horizon_count == 0)
   {picking_record->horizon_count = 1;
    picking_record->horizon_list = (ErsHorizon **) 
                                   calloc(1,sizeof(ErsHorizon *));
    if(horizon->horizon_name != NULL) horizon->hid = 0;
   }
  else
   {picking_record->horizon_count++;
    picking_record->horizon_list = (ErsHorizon **) realloc(
                    picking_record->horizon_list, 
                    sizeof(ErsHorizon *)*picking_record->horizon_count);
   }

  picking_record->horizon_list[picking_record->horizon_count-1] = horizon;

  return(horizon);
}

void ErsHorizonFree( ErsHorizon **hlist)
{ register int i;
  if(*hlist == NULL) return;
  i = 0;
  while(hlist[i] != NULL)
   {ErsHorizonSegmentFree(hlist[i]);
    i ++;
   }
}

void ErsHorizonSegmentFree( ErsHorizon *horizon)
{ErsPoint *npoint,*point;
 if(horizon == NULL) return;

 /* free all points in the horizon */
 point = horizon->first_point;
 while (point != NULL)
  {npoint = point->next;
   ErsPointFree(point);
   point = npoint;
  }
 horizon->first_point = NULL;
 free(horizon->color_name);
 free(horizon->horizon_name);
 horizon->number_of_points=0;
 free(horizon);
}

/*******************************************************************
 * ErsHorizonCheckAttributes() initializes the attributes for a new
 * horizon. It will only work if a name has been assigned to the
 * horizon. Checks to see if an horizon by the same name already
 * exists. Copys its attributes to the new horizon if it does.
 *******************************************************************/
void ErsHorizonCheckAttributes(PR_ *pikrec,
                      ErsHorizon *horizon)
{ int  i,hid,newhid;
  char *cname;
/* Set hid = UNDEFINED_HID if horizon or name is NULL */
  if(horizon == NULL) return;
  
  if(horizon->horizon_name == NULL)
   {horizon->hid = 0;
    horizon->user_int= 0;
    return;
   }
  if(pikrec == NULL ) return;
  if (pikrec->horizon_count <= 0)
   {horizon->hid = 1;
    horizon->user_int= 1;
    return;
   }
/*
 * Search for a name match with existing horizons. **
 * All horizons by same name have same horizon id  */
 for(i=0;i<pikrec->horizon_count;i++)
  { if(strcmp(horizon->horizon_name,pikrec->horizon_list[i]->horizon_name)==0)
     { horizon->user_int   = pikrec->horizon_list[i]->user_int;
       horizon->line_width = pikrec->horizon_list[i]->line_width;
       horizon->pixel      = pikrec->horizon_list[i]->pixel;
       horizon->hid        = pikrec->horizon_list[i]->hid;
       cname               = pikrec->horizon_list[i]->color_name;
       horizon->color_name = (char *) malloc(strlen(cname)+1);
       strcpy(horizon->color_name,cname);
       return;
     }
  }
/*
 * No existing name match,find a unique horizon id number to assign.
 * Does not have to be unique.
 */
 for(newhid=1;newhid<501;newhid++)
  {for(i=0;i<pikrec->horizon_count;i++)
    { if(newhid == pikrec->horizon_list[i]->hid) break;}
   if(i == pikrec->horizon_count) break;
  }
 horizon->hid = newhid;
 horizon->user_int = newhid;
}


void ErsHorizonChangeName(PR_ *picking_record,
      ErsHorizon *horizon, char *new_name)
{ ErsHorizon  *slist[99],*duplist[99];
  int i, n,  nseg, j, jseg;
  char old_name[32];
 
  if(picking_record == NULL) return;
  if(horizon == NULL) return;
  if(new_name == NULL) return;

/*
 * Are old and new names different?
 */
  old_name[0]='\0';
  if (horizon->horizon_name != NULL)
   {strcpy(old_name,horizon->horizon_name);
    if(strcmp(new_name,old_name) == 0) return;
   }
/*
 * Yes, old and new names are different.
 * Find all horizons with old name.
 */
  ErsHorizonGetSegments(picking_record,old_name, &nseg, slist);

  for(i=0;i<nseg;i++)
   {
    if (slist[i]->horizon_name != NULL) free(slist[i]->horizon_name);
    slist[i]->horizon_name = (char *) malloc(strlen(new_name)+1);
    strcpy(slist[i]->horizon_name,new_name);
   }

 return;
}

void ErsHorizonChangeHID(PR_ *pikrec,
      ErsHorizon *horizon, int newhid)
{ ErsHorizon  *slist[99];
  int i, nseg, oldhid;
  if(pikrec == NULL || horizon == NULL) return;
/*
 * Is there a change?
 */
  if(horizon->hid == newhid) return;
/*
 * Yes, find all related segments and change the hid
 */
  ErsHorizonMemberList(pikrec, horizon, &nseg, &i, slist);
  for(i=0;i<nseg;i++)
    {slist[i]->hid = newhid; }

 return;
}

ErsHorizon *ErsHorizonGet(PR_ *picking_record, int n)
{ if (n <= 0 || n > picking_record->horizon_count) return(NULL);
  return(picking_record->horizon_list[n-1]);
}

void ErsHorizonMinMax(ErsHorizon *horizon,float *xmin, float *xmax,
     float *tmin, float *tmax)
{ ErsPoint *point;
  float Min_t,Max_t,Min_x,Max_x;

  if(horizon == NULL) return;
  if((point = horizon->first_point) == NULL) return;
  ErsPointMinMax(point,&Min_x,&Max_x,&Min_t,&Max_t);
  *tmin = Min_t;
  *tmax = Max_t;
  *xmin = Min_x;
  *xmax = Max_x;
  point = point->next;
  while (point != NULL)
   { ErsPointMinMax(point,&Min_x,&Max_x,&Min_t,&Max_t);
     if (Min_t < *tmin) *tmin = Min_t;
     else if (Max_t > *tmax) *tmax = Max_t;
     if (Min_x < *xmin) *xmin = Min_x;
     else if (Max_x > *xmax) *xmax = Max_x;
     point = point->next;
   }
}



ErsHorizon *ErsHorizonGetFromPoint(PR_ *picking_record,
           ErsPoint *point)
{ register int i;

  if (point == NULL) return(NULL);

  while (point->next != NULL) point = point->next;

  for (i=0; i<picking_record->horizon_count; i++)
    if (point == picking_record->horizon_list[i]->last_point) break;

  if (i == picking_record->horizon_count)
    return(NULL);
  else
    return(picking_record->horizon_list[i]);
}

ErsHorizon **ErsHorizonGetByName(PR_ *picking_record,
            char *horizon_name)
{ register int i,nseg;
  ErsHorizon *list[99];

  if (horizon_name == NULL) return(NULL);

  nseg = 0;
  list[nseg] = NULL;
  for (i=0; i<picking_record->horizon_count; i++)
   {if (!strcmp(horizon_name, picking_record->horizon_list[i]->horizon_name))
      { list[nseg] = picking_record->horizon_list[i];
        nseg++;
      }
   }

  if(nseg>0)  return list;
  else return(NULL);
}
/* SEGMENTS STUFF */
void ErsHorizonGetSegments(PR_ *pikrec,char *horizon_name,
    int *count, ErsHorizon **slist)
{ register int i;

  *count = 0;
  slist[*count] = NULL;
  if (pikrec == NULL)       return;
  if (horizon_name == NULL) 
    {for (i=0; i<pikrec->horizon_count; i++)
        { if (pikrec->horizon_list[i]->horizon_name == NULL)
            { slist[*count] = pikrec->horizon_list[i];
              *count += 1;
            }
        }
    }
  else
    { for (i=0; i<pikrec->horizon_count; i++)
        {if (!strcmp(horizon_name, pikrec->horizon_list[i]->horizon_name))
           { slist[*count] = pikrec->horizon_list[i];
             *count += 1;
           }
        }
    }

  slist[*count] = NULL;
  return;
}
ErsHorizon *ErsHorizonGetSegmentN(PR_ *pikrec,char *horizon_name,
    int N)
{ ErsHorizon *slist[99];
  int count;
  register int i;
 
  ErsHorizonGetSegments(pikrec,horizon_name, &count, slist);
  if(count <= 0 ) return NULL;
  if(count > 99 )
    { count = 99;
      printf("ErsHorizonGetSegmentN: segment count > 99\n");
    }
  if( N < 1)     return NULL;
  if( N > count) return NULL;
  return slist[N - 1];
}

int ErsHorizonGetSegmentCount(PR_ *pikrec, ErsHorizon *horizon)
{ 
  int count;
  register int i;
  char *hname;

  count = 0;
  if(pikrec == NULL) return count;
  if(horizon== NULL) return count;
  if(pikrec->horizon_count<=0) return count;

  for (i=0; i<pikrec->horizon_count; i++)
    {hname = pikrec->horizon_list[i]->horizon_name;
     if (strcmp(horizon->horizon_name, hname)==0) { count += 1; }
    }

 return count;
}

void ErsHorizonMemberList(PR_ *pikrec, ErsHorizon *horizon,
     int *count, int *N, ErsHorizon **list)
{ 
  register int i;
  *count = 0;
  *N= -1;
  list[*count] = NULL;
  if(horizon == NULL) return;
  ErsHorizonGetSegments(pikrec,horizon->horizon_name, count, list);
  list[*count] = NULL;
  i=0;
  while(list[i] != NULL)
   {if(list[i] == horizon) break;
    i++;
   }
  if(i >= *count) return;
  *N = i+1;
  return;
}

void ErsHorizonGetHList(PR_ *pikrec,int *nhor ,
     ErsHorizon **uhlist)
{
 int  i,j;
 char *name;

 *nhor    = 0;
 uhlist[*nhor]=NULL;
 if(pikrec->horizon_count == 0) return;

/* Find all the unique names */
 uhlist[*nhor]= pikrec->horizon_list[0];
 *nhor    += 1;
 for(i=1;i<pikrec->horizon_count;i++)
   { name = pikrec->horizon_list[i]->horizon_name;
     for(j=0;j<*nhor;j++)
       {if(strcmp(name, uhlist[j]->horizon_name)==0) break; }
     if(j==*nhor)
       { uhlist[*nhor]=pikrec->horizon_list[i];
         *nhor += 1;
       }
   }

 uhlist[*nhor] = NULL;
 return;
}
 
/* END SEGMENTS STUFF */
 
ErsHorizon *ErsHorizonFind(PR_ *picking_record,
float pkey, float time, float delta_key, float delta_time)
{ ErsHorizon *horizon;
  ErsPoint *point;
  float pkey1, pkey2;
  float time1, time2, ctime;
  float a, b;
  register int i;

  /* first find if we are close to a point */
  point = ErsPointFind(picking_record, NULL, pkey, time, delta_key,
                       delta_time);
  if (point != NULL) return(ErsHorizonGetFromPoint(picking_record, point));

  /* we are not close to a point, so we have to go the hard way */
  for (i=0; i<picking_record->horizon_count; i++) {
    horizon = picking_record->horizon_list[i];
    if (horizon->first_point != NULL) {
      pkey1 = horizon->first_point->pkey;
      time1 = horizon->first_point->time;
      point = horizon->first_point->next;
      while (point != NULL) {
        pkey2 = point->pkey;
        time2 = point->time;
        if ((pkey >= pkey1 && pkey <= pkey2) ||
            (pkey >= pkey2 && pkey <= pkey1)) {
          if ((time >= time1 && time <= time2) || 
              (time >= time2 && time <= time1)) {
            /* point is inside rectangle */
            if (pkey1 == pkey2) return(horizon);
            a = (time2 - time1) / (pkey2 - pkey1);
            b = (time1 * pkey2 - time2 * pkey1) / (pkey2 - pkey1);
            ctime = a*pkey + b;
            if (fabs((double) (time - ctime)) <= delta_time) return(horizon);
          }
        }
        time1 = time2;
        pkey1 = pkey2;
        point = point->next;
      }
    }
  }
  return(NULL);
}

ErsPoint *ErsHorizonGetPoint(ErsHorizon *horizon, int point_number)
{ register ErsPoint *point = horizon->first_point;

  while (point_number != 1) {
    if (point == NULL) return(NULL);
    point_number--;
    point = point->next;
  }

  return(point);
}


int ErsHorizonGetPointCount( ErsHorizon *horizon)
{ ErsPoint *point;
  int count = 0;

  point = horizon->first_point;

  while (point != NULL) {
   count++;
   point = point->next;
  }

   return(count);
}

int  ErsHorizonGetNumPicks( ErsHorizon *horizon)
{ ErsPoint *point;
  int count = 0;

  if(!horizon) return count;
  point = horizon->first_point;

  while (point != NULL) {
   count += ErsPointCount(point);
   point = point->next;
  }

   return(count);
}

/* Get and set the Vector that is matched to ErsHorizon */
void *ErsHorizonGetVect( ErsHorizon *horizon)
{ if(horizon == NULL) return NULL;
  return horizon->vector;
}
void ErsHorizonSetVect( ErsHorizon *horizon, void *vector)
{ if(horizon == NULL) return;
  horizon->vector = vector;
}

