/****
!<CPS_v1 type=AUXILIARY_FILE"/>
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S  A U X I L I A R Y  F I L E
!
! Name       : set_defaults
! Category   : stand-alone
! Written    : 2001-04-16   by: Donna K. Vunderink
! Revised    : 2001-04-16   by: Donna K. Vunderink
! Maturity   : beta
! Purpose    : CGM library.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2001-04-16  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

char set_defaults_ident[100] = 
"$Id: set_defaults.c,v 1.1 2008/02/15 20:58:09 mengewm Exp $";


#define MAIN_1 1
#include "cgm_common.h"

char desc[256];
int ndesc;

void cgmSetDefaults()
{
  int i;
  int  max_element_list = 59;
  int class_list[90] = {
      0, 0, 0, 0, 0,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, 2, 2, 2, 2, 2, 2,
      3, 3,
      4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
      5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
      5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  int id_list[90] = {
      1, 2, 3, 4, 5,
      1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,13,
      1, 2, 3, 4, 5, 6, 7,
      5, 6,
      1, 2, 3, 4, 5, 7, 9,12,15,16,
      2, 3, 4, 6, 7, 8,10,11,12,13,14,15,16,17,18,22,23,24,25,31,32,
     33,34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

/* Begin Metafile */
  cgmPutString(desc,ndesc);
  cgmPutData(0,1);

/* Metafile Version Number */
  cgmPutInt(1);
  cgmPutData(1,1);

/* Metafile Description */
  cgmPutString(desc,ndesc);
  cgmPutData(1,2);

/* VDC Type */
  if (vdc_type == 0) {
     cgmPutEnum(0);      /* Type is Integer */
     cgmPutData(1,3);
  }
  else {
     cgmPutEnum(1);      /* Type is real */
     cgmPutData(1,3);
  }

/* Integer Precision */
  cgmPutInt(16);
  cgmPutData(1,4);

/* Real Precision */
  cgmPutEnum(1);         /* Fixed Point Format */
  cgmPutInt(16);         /* Whole part is 16 bits */
  cgmPutInt(16);         /* Fractional Part is 16 bits */
  cgmPutData(1,5);

/* Index Precison */
  cgmPutInt(16);
  cgmPutData(1,6);

/* Color Precision */
  cgmPutInt(16);
  cgmPutData(1,7);

/* Color Index Precision */
  cgmPutInt(16);
  cgmPutData(1,8);

/* Maximum Color Index */
  cgmPutInt(512);
  cgmPutData(1,9);

/* Color Value Extent */
  cgmPutColor(0.0,0.0,0.0);
  cgmPutColor(1.0,1.0,1.0);
  cgmPutData(1,10);

/* Metafile Elements List */
  cgmPutInt(max_element_list);
  for (i=0;i<max_element_list;i++) {
      cgmPutInt(class_list[i]);
      cgmPutInt(id_list[i]);
  }
  cgmPutData(1,11);

/* Font List */
  cgmTextInit();

/* Begin Picture */
  cgmPutString("First Picture",13);
  cgmPutData(0,3);

/* Scaling Mode */
  cgmPutEnum(1);
  cgmPutFloat(25.4/scalefactor);
  cgmPutData(2,1);

/* Color Selection Mode */
  cgmPutEnum(0);         /* Indexed Color Selection */
  cgmPutData(2,2);

/* Line Width Specification Mode */
  cgmPutEnum(1);         /* Line width specification is scaled */
  cgmPutData(2,3);

/* Marker Size SPecification Mode */
  cgmPutEnum(marker_scale_mode);  /* Marker size spec = 1 is scaled   */
  cgmPutData(2,4);                /*                    0 is absolute */

/* Edge Width Specification Mode */
  cgmPutEnum(1);                  /* Edge width specification is scaled */
  cgmPutData(2,5);

/* VDC Extent */
/*      set in trans.c */

/* Background Color */
  cgmPutColor(back_red,back_green,back_blue);
  cgmPutData(2,7);
}

void cgmInitBackground(float r,float g,float b)
{
  back_red = r;
  back_green = g;
  back_blue = b;
}

void cgmInitMarkerScalingMode(int marker_tmp)
{
  marker_scale_mode = marker_tmp;
}

void cgmInitDesc(char *desc_tmp,int ndesc_tmp)
{
  strcpy(desc,desc_tmp);
  ndesc = ndesc_tmp;
}

void cgmPipInit()
{
  int iyear, imonth, iday;
  char date[12];

  cgmGetDate(&imonth,&iday,&iyear);
  sprintf(date,"%4.4d-%2.2d-%2.2d\"",iyear,imonth,iday);
  strcpy(desc,"\"ProfileId: PIP/I/2\" \"ColourClass: Colour\" \"Extention: Extended/Seismic\" \"Source: Conoco Inc.\" \"Date: ");
  strcat(desc,date);
  ndesc = strlen(desc);
  scalefactor = 1.0;
  vdc_type = 1;
  cgmPipFonts();
  linefilter = 0;
  back_red = 1.0;
  back_green = 1.0;
  back_blue = 1.0;
  clip_on = 0;
}

void cgmInitAttributes()
{

/* Line Attributes */
  linetype = -1;
  linecolor = -1;
  linewidth = -1.0;

/* Fill Attributes */
  filltype = 0;
  cgmPutInt(filltype);
  cgmPutData(5,22);

/* Marker Attributes */
  markerfont = 0;

/* Edge Attributes */

/* Dashline parmeter flag */
  dshflg = FALSE;
}

