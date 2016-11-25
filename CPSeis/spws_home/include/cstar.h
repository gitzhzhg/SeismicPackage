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
#ifndef cstardef

#define cstardef

/*
 *    cstar.h - Definition of parameters for parameters in 
 *                                        refraction statics picking.
 */


#include <wproc.h>

#define MAX_CS_WIG 10
#define N_MHOR    0
#define N_IFIL    1
#define N_OFIL    2
#define N_FBUT    3
#define N_MSEX    4
#define N_FORM    5
#define N_HWD1    6
#define N_HWD2    7
#define N_SQUE    8
#define N_DEST    9


#define OFF_INFO  "INFORMATION:\n\nMin Offset: %7.2f\nMax Offset: %7.2f"


      struct pigs {
             int    igan;
             float  xgat;
         } ; 

      struct pins {
             float  xtrn;
             float  time;
             float  xpix;
             float  ypix;
         } ; 

 
/* eliminate multiple defines */
#if (ultrix || hpux)
extern
#endif
     struct  star_decl     {
             int  noff;            /*  Max num offsets per gather  */
             int  ntrc;            /*  Number of  traces in File   */
             int  ngat;            /*  Number of gathers in File   */
             int  igat;            /*  Current gather number       */
            float xgat;            /*  Current ID of the gather    */
             int  mark;            /*  Type of marker  (-7 to +7)  */
             int  isw;             /*  Picking Switch, 0,1 or 2    */
             int  isav;            /*  # of entries into pick_save */
             int  meth;            /*  Method 1,2 = reg,irreg offs */
             int  dbug;            /*  Print number for debugging  */
             int   dtype;          /*  Data type                   */
             int   jtype;          /*  Prog Type - 1=SCRS, 2=AVO   */
             int   units;

/*
 *           float    *pam; 
 *           int       mem; 
 */
             char     in_file[100];      /* input file name */
             char     out_file[100];     /* input file name */

             int      attr  ;
             int      ievn  ;
             int      iseg  ;
             int      nterp ;
             int      mhor  ;     /*  Max num of horizons allowed */
             int      msex  ;     /*  Max num of sections allowed */
             int      hwd1  ;
             int      hwd2  ;
             int   istat_in ;        /*  0,1  status for  in_file  */
             int   istat_out ;       /*  0,1  status for out_file  */
            float    start  ;       /*  Starting time of byte file */
            float  xad1, xad2;

/*          struct  pigs   *p_pigs;         */

       }   pstar ;


/* eliminate multiple defines */
#if (ultrix || hpux)
extern
#endif
     struct  apik_decl     {

             int      ntrd ;   /*     Number of traces to read     */
             int      ntsr ;   /*     Num of time samples read     */
             int      ievn ;   /*      Current  Event  Number      */
             int      iseg ;   /*      Current Segment Number      */
             int      attr ;   /*      Attribute to search on      */
             int      idir ;   /*       Direction of search        */
             int      mode ;
             int      rang ;   /*  Max # of points for ver search  */
             int      type ;
             int      mark ;
             int      zpik ;
             int      shade ;
             int      icnt ;   /*  Counter for calling amp_events  */
             int      index ;  /*   Index for zoom, starts at 10   */
             int      icol;    /*   Pick color , 1, 2 = blue,red   */ 

             int      nterp ;  /*  Interpol btw samples (0,1=n,y)  */
             float    wfact ;  /*   Weighting Factor to Bias Fit   */
             float    srin;    /*    Inverse sample rate of data   */

             float   qx1,    qx2 ;  /*  Pixel corner points        */ 
             float   qy1,    qy2 ;  /*   for orig trace display    */ 
             float   smin,  smax ;  /*  Trace and time corner pts  */ 
             float   tmin,  tmax ;  /*   for orig trace display    */ 

             float   px1,    px2 ;  /*  Pixel corner points        */ 
             float   py1,    py2 ;  /*   for curr trace display    */
             float   xmin,  xmax ;  /*  Trace and time corner pts  */ 
             float   ymin,  ymax ;  /*   for curr trace display    */

             float  zoomx, zoomy ;  /*   Zoom factors - Not used   */
             float  xscal, yscal ;  /*     Translation factors     */
             float  xinsc, yinsc ;  /*   Inv translation factors   */

             int      nsw;
             int      ix1;     /*    X Pixel for button  press     */
             int      iy1;     /*    Y Pixel for button  press     */
             int      ip1;     /*  Pick Bin closest to first pick  */

             int      ix2;     /*    X Pixel for button release    */
             int      iy2;     /*    Y Pixel for button release    */
             int      ip2;     /*  Pick Bin closest to  2-nd pick  */
             float  slope;     /*  slope in Y-pixels per X-pixels  */

            struct  pins   *p_pins;         

       }     papik;


/* eliminate multiple defines */
#if (ultrix || hpux)
extern
#endif
     struct ampa_decl {    /*     AMPGEN common block in ICP   */

         int     ilun ;    /*    Status and number of file     */
         int     lrec ;    /*      Record length of file       */
         int     nrec ;    /*    Number of records in file ?   */
         int    imeth ;    /*   Meth of file format ( 1, 2 )   */

         int    nelem ;
         int    jtype ;
         int    jprog ;
         int    junit ;

         int    npanb ;    /*    Num of panels in byte file    */
         int    npana ;    /*  Num panels allowed in amp file  */
         int    npan1 ;    /*   1-st relevant panel in file    */
         int    npan2 ;    /*   Last relevant panel in file    */
         int    naoff ;    /*  Number of traces in each panel  */

         int    ianum ;    /*   Number of panels  with picks   */
         int    kevnt ;    /*   Max Number of events allowed   */
         int    karec ;    /*   Num of add. recs per event.    */
         int    kattr ;    /*   Max Num of attributes / record */
         int    khead ;    /*   Num of header records  >= 2    */ 
         int    nrep  ;    /*  Repeat number for offset locs   */
         int    irep1 ;    /*   Offset number within repeat #  */

       float   cmpinc ;
       float    cmp1  ;
       float   spinc  ;
       float     sp1  ;
       float    xinc  ;
       float   xmin1  ;
       float   xmax1  ;
         int    memx  ;    /*  size of ampa->xbuf array       */
       float   *xbuf  ;    /* buffer to read or write records */

         int  *napnt  ;    /*   File pointers for each panel  */
       float   *xpan  ;    /*    Panel location identifier    */

               }     pampa ;


/* eliminate multiple defines */
#if (ultrix || hpux)
extern
#endif
     struct ampi_decl {    /*     AMPGAT common block in ICP   */

         int     cpan ;    /*  Current panel number           */
         int     cpik ;    /*  Current panel num with picks   */
         int     iask ;    /*  Switch for asking a question   */
         int     begx ;    /*  Begin trace in panel on screen */
         int     endx ;    /*   End  trace in panel on screen */
         int     evnt ;    /*   Number of current event        */
         int    mevnt ;    /*  Maximum number of events allowed */
         int    nattr ;    /*  Number of current attribute     */
         int    ntrac ;    /*  Number of traces in gather      */
         int    iread ;    /*      Initial event to read       */   
         int    nread ;    /*     Number of events to read     */ 
         int     mem  ;    /*     Size of  ampi->pam  array    */
        float   *pam  ;    /*    Array of picks, amplitudes    */
        int    *istat ;    /*       Status of events           */
        int    *istar ;    /*     Status of starting values    */
        int    *isear ;    /*      Search type of events       */

/*     float amattr[9000] ;  **   Buffer for picks & amps      */

               }    pampi ;


struct cstar_popinfo {
                      struct star_decl pstartmp;
                      struct star_decl *pstar;
                      char       savin_file[100];      /* input file */
/*
 *                    wunion     cmwig[MAX_CS_WIG];
 *                    struct CB  cmcb[MAX_CS_WIG];
 */
                      Widget     offlab;
                      Widget     qbox;
                      struct CURRFLD_INFO fld_info;
                      Window shell_win;
                      void       (*map_funcs)();
                      void       (*ok_funcs)();
                      void       (*can_funcs)();
                      void       *ok_datas;
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
                      struct CB  cmcb[MAX_CS_WIG];
                      wunion     cmwig[MAX_CS_WIG];
                     };

#endif
