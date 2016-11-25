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
#ifndef cmutedef

#define cmutedef

/*
 *    cmute.h - Definition of parameters for parameters in mute program.
 */
/*                                 */
/*                                 */
#include <wproc.h>



#define MAX_CM_WIG 12
#define MAX_PICKS  200
#define FBUT       0
#define IFIL       1
#define OFIL       2
#define USERPOINT  3
#define MPCK       4
#define HWD        5
#define HWD2       6
#define CMFORM     7
#define MINOFF     8
#define MAXOFF     9 
#define MUTE_Q     10
#define WDESTROY   11


#define OFF_INFO  "INFORMATION:\n\nMin Offset: %7.2f\nMax Offset: %7.2f"

      struct mars {
             float  xoff;
             float  xtim;
             float  xtrn;
             int    xpix;
             int    ypix; 
         } ; 

      struct mags {
             int    igan;
             float  xgat;
         } ; 

      struct mans {
             float  odis;
             float  otrn;
         } ; 

/*
 *   typedef  struct {
 *           float  xoff;
 *           float  xtim;
 *           float  xpix;
 *           float  ypix; 
 *       } mars ; 
 */
 
/* eliminate multiple defines */
#if (ultrix || hpux)
extern
#endif
     struct  mute_decl     {
             int  noff;            /*  Max num offsets per gather  */
             int  mpick;           /*  Max Num of Mute Picks       */
             int  npick;           /*  Num of Mute pick locations  */
             int  nrec;            /*  Number of records written   */ 
                                   /*      - - - - - - - - -       */ 
             int  ntrc;            /*  Number of  traces in File   */
             int  ngat;            /*  Number of gathers in File   */
             int  igat;            /*  Current gather number       */
             long picked_files;    /*  Number of files picked      */
            float xgat;            /*  Current ID of the gather    */
             int  mark;            /*  Type of marker  (-7 to +7)  */
             int  icol;            /*  Pick color , 1,2 = blue,red */ 
             int  isw;             /*  Picking Switch, 0,1 or 2    */
             int  isav;            /*  # of entries into pick_save */
             int  meth;            /*  Method 1,2 = reg,irreg offs */
             int  dbug;            /*  Print number for debugging  */

             int   dtype;          /*  Data type  */
             int   units;
             float xoff_min;
             float xoff_max;
             float xoff1;          /*  Extreme  left offset         */
             float xoff2;          /*  Extreme right offset         */
             float xoff_inc;
             float troff1 ;         /*  Offset of  left trace in display */
             float troff2 ;         /*  Offset of right trace in display */
             float xpix_min;
             float xpix_max;
             float slope;
             float startim;

             float    *pam;              /*  Pointer to array of mutes  */
             int       mem;              /*  Size of   pam   array      */
/*           int      pa1,pa2,pa3;           3 pointers in the array    */

             FILE     *pif;              /*  Pointer to   Input  File   */
             int      pi1,pi2,pi3;       /*  3 pointers for Input  File */

             FILE     *pfm;              /*  Pointer to  File of mutes  */
             int      pt1,pt2,pt3;       /*  3 pointers within the File */

             char     in_file[100];      /* input file name */
             char     out_file[100];     /* input file name */
             int      hwd;               /* primary header word */
             int      hwd2;              /* secondary header word */

             int   iad1, iad2;     
            float  xad1, xad2;  
/*
 *           int   xpix[15];
 *           int   ypix[15]; 
 *           float xoff[15];
 *           float xtim[15];
 */
            struct  mars   *p_mars;         
/*            mars         *p_mars;           */
            struct  mags   *p_mags;         
            struct  mans   *p_mans;         
       }   pmute ;
/* */             

/* eliminate multiple defines */
#if (ultrix || hpux)
extern
#endif
     struct  mpik_decl     {
             
             int      nhx;         /*  Header Word for Offset Location  */
             int      nhy;         /*  Header Word for Other Direction  */
             int      nhx2;
             int      nhy2;
             float    x1;          /*  Mimnimum Offset   */
             float    y1;
             float    xinc;        /*  Nominal Offset Increment  */
             float    yinc; 
             int      nx;          /*  Number of picks per gather  */
             int      ny;          /*  Number of gathers           */
             long     lpac;        /*  File pointer after comment cards */

              int    nsw;
              int    ix1;     /*    X Pixel value for button  press     */
              int    iy1;     /*    Y Pixel value for button  press     */
              int    ip1;     /*  Pick Bin Value closest to first pick  */

              int    ix2;     /*    X Pixel value for button release    */
              int    iy2;     /*    Y Pixel value for button release    */
              int    ip2;     /*  Pick Bin Value closest to  2-nd pick  */
              int    idir;

       }     mpik;  



struct cmute_popinfo {
                      struct mute_decl pmutetmp;
                      struct mute_decl *pmute;
                      char       savin_file[100];      /* input file name */
                      wunion     cmwig[MAX_CM_WIG];
                      struct CB  cmcb[MAX_CM_WIG];
                      Widget     offlab;
                      Widget     qbox;
                      Widget     errbox;
                      Widget     mute_infobox;
                      struct CURRFLD_INFO fld_info;
                      Window shell_win;
                      void       (*map_func)();
                      void       (*ok_func)();
                      void       (*can_func)();
                      void       *ok_data;
                      long       good_infile;
                      long       good_outfile;
                      long       wigmax;
/*
                      char       *bytfile;
                      long       *ntot;
*/
                      struct FILE_INFO infile_stuff;
                      struct FILE_INFO outfile_stuff;
                      Widget           outary[2];
                      Widget      mute_info;         /*informational label*/
                      long        userpoint;
                     };

#endif
