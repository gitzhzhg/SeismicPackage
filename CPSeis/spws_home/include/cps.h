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
#ifndef CPSH
#define CPSH

#include <X11/Intrinsic.h>
#include "cpsglob.h"
#include "cpscust.h"
/****************************************************
 * Assign a process ID number to each CPS process ***
 ***************************************************/
#define MAXJOBS 10
#define MAXPROC 50
#define MAXDCRDS 50
#define PNULL  -1
#define XP_    0
#define MVXP_  1
#define TPOW_  2
#define SHFT_  3
#define INIT_  4
#define TSVF_  5

/****************************************************
 * Define a structure for describing a CPS process***
 ***************************************************/
typedef struct CPS_PROCESS
         { void *jobptr;     /*Job identifier                */
           long proc_num;
           char *proc_nam;
           struct CPS_PROCESS *prev;
           struct CPS_PROCESS *next;
           long save;
           long ltr;
           CpsGlobals cpsg;
           long ntrc;
           long scratch;
           long storage;
           long nparm;
           long need_tap;
           long cart;
           long alternate;
           long label;
           long need2;
           char *call_crd;
           long ndat;
           char *dat_crds[MAXDCRDS]; } CpsProcess;
/****************************************************
 * Define a structure for batch job information   ***
 ***************************************************/
typedef struct CPS_BATCH
         { char job_name[8];
           char userid[16];
           char account[16];
           char password[16];
           char project[16];
           char line[16];
           long priority;
           long sub_priority;
           long time;
           CpsCust custom;
           char history[8];
           char library[8];
           char route_name[16];
           char route_addr[16];
           char phone_addr[16];
           char metric_sys[8];
           char print_type[8];
           char pdnuid[16];  } CpsBatch;
/****************************************************
 * Some important Widgets used in an XCFE session ***
 ***************************************************/
struct CFE_WBAG
         { void *parent;   /*Container Widget for XCFE     */ 
           void *list1;    /*List widget 1-proc menu       */
           void *list2;    /*List widget 2-job stream      */
           void *tbadd;    /*Wid of build mode toggles.    */
           void *tbins;
           void *tbrep;
           void *tbmod;
           void *tbdel;
           void *setbat;
           void *setcus;
           void *submit; };
/****************************************************
 * Define a structure for describing a CFE-CPS job***
 ***************************************************/
typedef struct CPS_JOB
         { long jobid;       /*Job identifier                */
           long build_mode;  /*Job modification mode         */
           Boolean doproc;   /*Do or Dont apply procs        */
           struct CFE_WBAG wbag; /*wid's that are needed     */
           void (*pfunc)();  /*Address of process_data()     */
           Boolean dobatch;  /*true=batch, false=interactive */
           CpsBatch batch;
           int tapecnt_rnd;
           int tapecnt_cart;
           long scratch;
           long storag;
           int  nloops;
           long ltr1;
           long ltr2;
           long nwih;        /*No. of CPS headers per trace  */
           void *ses;        /*pointer to cfe session?       */
           long nproc;       /*No. of processes in job stream*/
           CpsGlobals *cpsg;
           CpsProcess *first;
           CpsProcess *last; } CpsJob;

/****************************************************
 * Data structure needed for some of our callbacks***
 ***************************************************/
struct xcfe_data
       { CpsJob *cpsjob;
         void   *cfepop; };

/****************************************************
 * Define a structure for maintaining concurrent  ***
 * CPS jobs.                                      ***
 ***************************************************/
struct CPSCFE_LIST
         { long   jobcnt;
           CpsJob *cpscfe[MAXJOBS]; };


#endif
