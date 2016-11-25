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
/*-------------------------------------------------------------------------
 *USER DOC
 * File        : custom_pop.h
 * Author      : Michael L. Sherrill
 * Date        : 03/92
 *
 * Purpose     Header file for cbyt customize color popup
 *
 *NOTES:
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *
 *END DOC
 *
-------------------------------------------------------------------------*/


#include "wproc.h"
#include "image.h"

/*custom popup defines*/
#define FILEB 0
#define INFIL 1
#define OUTFIL 2
#define VMIN 3 
#define VMAX 4 
#define PNC 5 
#define PPC 6 
#define CUSTFORM  7 
#define CUSTOM_Q  8
#define DESTROY 9
#define MEDIAN 10
#define SGRAY 11
#define COLOROPT 12
#define GRADEV 13
#define GRADEH 14
#define DOCBAR 15
#define DOPRE 16
#define PSCALE 17
#define PNCSCALE 18
#define PPCSCALE 19
#define DOCAMP 20
#define DOVAMP 21
#define DOPAMP 22
#define REDRAW 23
#define OVERLAY 24
#define MAX_WIG 25

/*color bar popup defines*/
#define CBARPOP 0 
#define REDRAWCBAR 1
#define INTENSITY 2
#define COMPRESS 3

#define MAX_CBARWIG 5


 

struct custom_popinfo 
       {
       char                cbar_filein[100];    /*cps color file in */ 
       char                cbar_fileout[100];   /*cps color file out*/
       wunion              custwig[MAX_WIG];    /*widget union for customize*/
       wunion              cbarwig[MAX_CBARWIG];/*widget union for color bar*/
       struct CURRFLD_INFO fld_info;            /*file input call back info*/
       struct CB           custcb[MAX_WIG];     /*customize widget cb array*/
       struct CB           cbarcb[MAX_CBARWIG]; /*color bar widget cb array*/
       struct FILE_INFO    infile_info;         /*file input info*/
       struct FILE_INFO    outfile_info;        /*file output info*/
       long                good_infile;         /*input file boolean*/
       long                good_outfile;        /*output file boolean*/
       Window              shell_win;           /*shell window*/
       Widget              qbox;                /*question box popup*/
       Widget              ebox;                /*error box popup*/
       Widget              custcolor;           /*customize drawing area*/
       Widget              cbarcolor;           /*color bar drawing area*/
       Widget              pnclabel;            /*neg percent label*/
       Widget              ppclabel;            /*pos percent label*/
       Widget              prelabel;            /*predefined color label*/
       Widget              title1;              /*form label*/
       Widget              title2;              /*form label*/
       char                tmpfile[100];        /*temporary file holder*/
       long                color_type;          /*gray, median etc*/
       long                do_color;            /*do cps type color processing*/
       long                do_median;        /*rescale to median of data*/
       long                do_gray;          /*standard gray scale*/
       long                do_build;         /*build a color file*/
       long                do_cbar;          /*read existing color file*/
       long                do_pre;           /*use predefined color file*/
       long                do_save;          /*save color file*/
       long                do_camp;          /*use colorbar amplitudes*/
       long                do_vamp;          /*use use min and max amplitudes*/
       long                do_pamp;          /*calculate amp percentages*/
       int                 predef;           /*predefined color number*/
       int                 pnc;              /*percent of negative to color*/
       int                 ppc;              /*percent of positive to color*/
       long                gradeh;           /*grade image horizontally*/
       long                gradev;           /*grade image vertically*/
       long                overlay;          /*overlay wiggle traces*/
       float               vmin;             /*min amplitude to color*/
       float               vmax;             /*max amplitude to color*/
       void                *ok_data;         /*used in ok call back*/
       GC                  customgc;         /*for color bar drawing*/
       long                change_color;     /*user has changed color*/
       long                intensity;        /*rescale color brightness*/
       long                compress;         /*compress or expand colors*/
       float               rgb[1024];        /*color file rgb values*/
       long                numcolors;        /*number of colors to load*/
       };

