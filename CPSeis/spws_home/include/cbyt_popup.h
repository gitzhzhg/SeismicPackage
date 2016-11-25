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
/*
 *  Header File for cbyte popup primitive
 */

#include "cenv.h"
#include "wproc.h"
#include "cbprim.h"
#include "image.h"
#include <X11/Intrinsic.h>
#include <Xm/Text.h>



#define NUM_TEXT 13              /* number of text widgets in array */
#define NUM_WIDGET  31           /* number of push button widgets in array */

/* text widget offsets */
#define FNAME     0
#define NTOT      1
#define ISKP      2
#define NDO       3
#define NSKP      4
#define SCAN      5
#define TMIN      6
#define TMAX      7
#define TDEC      8
#define TI        9
#define IS       10
#define HELPOUT  11
#define CT       12
/* push button offsets */
#define OK        13
#define CANCEL    14
#define HELP      15
#define SHOWF     16
#define ANNO      17
#define SCALE     18
/* toggle button offsets */
#define GS        19
#define WONLY     20
#define WFILL     21
#define RP        22
#define NORM      23
#define RD        24
#define INVERT_Y  25
#define MAXIM     26
#define MAX_QUEST 27

#define MOVIE     28 
#define MFRAMES   29 

#define FORM_MAP   30 

struct Anno { double ptl;
              double stl;
              int   first;
              int tinc;
              int hdrwrd;
              int hdrwrdTwo;
              char label[80]; };


struct POPUP_INFO {
            wunion             wary[NUM_WIDGET];  /* array of push Widgets */
            struct CB          wcb[NUM_WIDGET];
            struct CB          helpcb[2];
            Widget             f1;
            Widget             f2;
            Widget             f3;
            Widget             rc1;
            Widget             rc2;
            Widget             rc3;
            Widget             rc4;
            Widget             rc5;
            Widget             rc6;
            Widget             Popform;
            Widget             maxq;
            Window             shell_wid;
            Widget             fsbox;       /* file selection box Widget */
            Widget             errbox;
            Widget             helpbox;
            Widget             anno_bb;
            Widget             defaults;
            void               *anno_ctx;
            unsigned long      field_flags;
            struct Anno        anno;
            struct ImageInput  tempory;   /* struct holds data until OK */
            struct ImageInput  *user;     /* struct holds data after OK */
            struct PlotImage   *image;      /* struct holds data after OK */
            struct FILE_INFO   finfo;     /* file information for call back */
            long               good_file;   /* true if valid byt file */
            Widget             currtextw;   /* Widget ID of current field */
            long               currfld;     /* constant of current field */
            void               (*ok_func)(); /* routine to call on OK */
            void               *ok_data;     /* data to pass on OK */
            char               tr_rng[70];
            char               time_rng[70];
            char               tmin_rng[70];
            HelpCtx            helpctx;
            char               over_str[100];
            Boolean            defdef;
          };

#define CKF(flg)      ((flg)&ctx->field_flags)


