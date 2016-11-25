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
#ifndef cattrdef

#define cattrdef

/*
 *    cstar.h - Definition of parameters for parameters in 
 *                                        refraction statics picking.
 */


#include <wproc.h>



#define MAX_CA_WIG 10
#define NJPICK    0
#define NJPEAK    1
#define NJTROF    2
#define NJ_ZC1    3
#define NJ_ZC2    4
/* #define NJNONE    5  */

#define NA_EVNT    5

#define NA_FORM    6
#define NA_FOCU    7
#define NA_SQUE    8
#define NA_DEST    9



 
/* eliminate multiple defines */
#if (ultrix || hpux)
extern
#endif
     struct  attr_decl     {

             int  pro_num;     /*  Program number                   */
             int  evn_min;     /*  Minimum event number allowed     */
             int  evn_max;     /*  Maximum event number allowed     */
             int  evn_old;     /*  Previous value for event number  */
             int  evn_new;     /*  Current  value for event number  */
             int  jpick ;      /*  On-off Switch for Picking        */ 
             int  j_old ;      /*  Previous Pick Attr Value         */ 
             int  j_cur ;      /*  Current  Pick Attr Value         */ 
             int  jpeak;       /*  On-Off Switch for Peak           */
             int  jtrof;       /*  On-Off Switch for Trough         */
             int  j_zc1;       /*  On-Off Switch for Pos Zero Cross */
             int  j_zc2;       /*  On-Off Switch for Neg Zero Cross */ 
             int  jnone;       /*  On-Off Switch for None           */ 

       }     pattr;


struct cattr_popinfo {
                      struct attr_decl pattrtmp;
                      struct attr_decl *pattr;
                      char       savin_file[100];      /* input file */
/*
 *                    wunion     cmwig[MAX_CA_WIG];
 *                    struct CB  cmcb[MAX_CA_WIG];
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
                      Widget           outary[2];
                      struct CB  cmcb[MAX_CA_WIG];
                      wunion     cmwig[MAX_CA_WIG];
                     };

#endif
