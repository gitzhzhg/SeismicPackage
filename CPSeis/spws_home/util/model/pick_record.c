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

/**********************************************/
/*  Functions that operate on picking record  */
/**********************************************/

/*********************************************************/
/*                                                       */
/*  PR_ PR_Create()                                      */
/*  This is the routine to allocate and initialize a     */
/*  picking record.                                      */
/*  To connect to a display call the following functions */
/*  1. PR_SetTranslations()                 */
/*  2. PR_ConnectWidget()                   */
/*********************************************************/
PR_ *PR_Create()
{ PR_ *picking_record;

  /* allocate structure */
  picking_record = (PR_ *) calloc(1, sizeof(PR_));

  /* initialize fields */
  picking_record->horizon_count = 0;
  picking_record->widget_count  = 0;
  picking_record->current_horizon = NULL;
  picking_record->default_symbol_size = 0;
  picking_record->default_line_width  = DEFAULT_LINE_WIDTH;
  picking_record->selected_line_width = SELECTED_LINE_WIDTH;
  picking_record->selected_symbol_size= ErsSYMBOL_AUTO_SIZED;
  picking_record->selected_pixel      = ErsHORIZON_PIXEL;

  picking_record->track_mode = ErsTRACK_MAXIMUM;
  picking_record->projection_mode = ErsLINEAR_PROJECTION;
  picking_record->max_dip_in_samples = 8;
  picking_record->snap_mode = ErsNO_SNAP;

  picking_record->point_select_callback  = NULL;
  picking_record->point_add_callback     = NULL;
  picking_record->point_delete_callback  = NULL;
  picking_record->horizon_select_callback= NULL;
  picking_record->horizon_add_callback   = NULL;
  picking_record->horizon_delete_callback= NULL;
  picking_record->horizon_move_callback  = NULL;
  picking_record->point_move_callback    = NULL;
  picking_record->snap_callback          = NULL;
  picking_record->snap_callback_data     = (caddr_t) picking_record;
  picking_record->tracker_callback       = NULL;
  picking_record->tracker_callback_data  = (caddr_t) picking_record;
  picking_record->Phdr = UNDEFINED_HDR;
  picking_record->Shdr = UNDEFINED_HDR;
  picking_record->Thdr = UNDEFINED_HDR;
  picking_record->user_data = NULL;

  return(picking_record);
}

void PR_Free(PR_ *picking_record)
{ ErsHorizon *slist[299];
  int count;
  register int i;

  if(picking_record == NULL) return;
  /* remove link with all connected widgets */
  if(picking_record->widget_count != 0)
  { printf("PR_Free: widget count is non-zero\n");
    printf("PR_Free: use PR_Destroy in this case\n");
  }

  /* destroy all the horizons */
  count = picking_record->horizon_count;
  for (i=count-1; i>=0; i--) slist[i] = picking_record->horizon_list[i];
  slist[count] = NULL;
  ErsHorizonFree(picking_record, slist);

  /* free structure */
  free(picking_record);
}

/* Pass in a PickingRecord pointer - will return a ModInfo pointer */
void *PR_getgui(void *pikrec)
{PR_ *pr;
 if(pikrec == NULL) return NULL;
 pr = (PR_ *) pikrec;
/*  (ModInfo *) pr->user_data; */
 return (void *) pr->user_data;
}

void PR_setx_transf(PR_ *pr, ErsTransform *t)
{if(pr == NULL) return;
 pr->transx = t;
 return; }
void PR_setz_transf(PR_ *pr, ErsTransform *t)
{if(pr == NULL) return;
 pr->transz = t;
 return; }
ErsTransform *PR_x_transf(PR_ *pr)
{ if(pr == NULL) return NULL;
 return pr->transx; }
ErsTransform *PR_z_transf(PR_ *pr)
{ if(pr == NULL) return NULL;
 return pr->transz; }

/***************************************************************************
 * - PR_SetDefaultLineWidth -
 *  Function used to set the default line width for horizons.
 ***************************************************************************/
void PR_SetDefaultLineWidth(picking_record, line_width)
PR_ *picking_record;
int line_width;
{ picking_record->default_line_width = line_width; }

/***************************************************************************
 * - PR_SetDefaultSymbolSize -
 *  Function used to set the default symbol size.
 ***************************************************************************/
void PR_SetDefaultSymbolSize(picking_record, symbol_size)
PR_ *picking_record;
int symbol_size;
{ picking_record->default_symbol_size = symbol_size; }


/***************************************************************************
 * - PR_SetCurrentHorizonSymbolPixel -
 *  Function used to set which horizon is currently selected.
 ***************************************************************************/
void PR_SetCurrentHorizonSymbolPixel(picking_record, pixel)
PR_ *picking_record;
Pixel pixel;
{ picking_record->selected_pixel = pixel; }

void PR_SetSnapMode(picking_record, mode)
PR_ *picking_record;
int mode;
{ picking_record->snap_mode = mode; }

void PR_SetTrackMode(picking_record, track_mode, projection_mode, 
                             max_dip_in_samples)
PR_ *picking_record;
int track_mode;
int projection_mode;
int max_dip_in_samples;
{
  picking_record->track_mode = track_mode;
  picking_record->projection_mode = projection_mode;
  if (max_dip_in_samples != 0)
    picking_record->max_dip_in_samples = max_dip_in_samples;
  else
    picking_record->max_dip_in_samples = 8;
}

Bool    PR_Compare(PR_ *picking_record1, PR_ *picking_record2)
{
  ErsPoint *point1, *point2;
  register int i;

  if (picking_record1->horizon_count != picking_record2->horizon_count)
    return(False);

  for (i=0; i<picking_record1->horizon_count; i++) {
    point1 = picking_record1->horizon_list[i]->first_point;
    point2 = picking_record2->horizon_list[i]->first_point;
    while (point1 != NULL) {
      if (point2 == NULL) return(False);
      if (point1->time != point2->time) return(False);
      if (point1->tn != point2->tn) return(False);
/* Changed order 08/21/92
      if (point2 == NULL) return(False);
*/
      point1 = point1->next;
      point2 = point2->next;
    }
    if (point2 != NULL) return(False);
  }

  return(True);
}
/***********************************************************
 ** Return the horizon count and the total number of picks**
 ** for the entire picking record                         **
 **********************************************************/   
void PRCounts(PR_ *pr,long * nseg, long *count )
{ErsHorizon *horizon;
 ErsPoint   *point; 
 register int i;
 if(pr == NULL) return;
 *nseg = 0;
 *count= 0;
 *nseg = PR_HorizonCount(pr);
 for(i=0;i<*nseg;i++)
  { point = pr->horizon_list[i]->first_point;
    while(point != NULL)
     { *count += P_Getnpicks(point) + 1;
       point = ErsPointNext(point); }
  }
 return;
}

void ErsPRMinMax(PR_ *pr,float *xmin, float *xmax,
     float *tmin, float *tmax)
{ErsHorizon *horizon;
 float Min_t,Max_t,Min_x,Max_x;
 int i,nseg;

 if(pr == NULL) return;
 nseg = PR_HorizonCount(pr);
 if(nseg == 0) return;
 horizon = pr->horizon_list[0];
 ErsHorizonMinMax(horizon,&Min_x,&Max_x,&Min_t,&Max_t);
 *tmin = Min_t;
 *tmax = Max_t;
 *xmin = Min_x;
 *xmax = Max_x;
 for(i=1;i<nseg;i++)
  { horizon = pr->horizon_list[i];
    ErsHorizonMinMax(horizon,&Min_x,&Max_x,&Min_t,&Max_t);
    if (Min_t < *tmin) *tmin = Min_t;
    else if (Max_t > *tmax) *tmax = Max_t;
    if (Min_x < *xmin) *xmin = Min_x;
    else if (Max_x > *xmax) *xmax = Max_x;
  }
 return;
}

