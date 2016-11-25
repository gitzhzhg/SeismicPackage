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
#ifndef _XCFE_
#define _XCFE_

#include "c2f_interface.h"
#include <stdio.h>
#include <Xm/Xm.h>
#include "cps.h"

/* Aliases for routines on the Cray */
#ifdef NEED_CAPITALS
#define ncodew_            NCODEW
#define dcoded_            DCODED
#define dcodes_            DCODES
#define ncodes_            NCODES
#define dcodebw_           DCODEBW
#define ncodebw_           NCODEBW
#define get_globals_addr_  GET_GLOBALS_ADDR
#define get_global_vals_   GET_GLOBAL_VALS
#define set_global_vals_   SET_GLOBAL_VALS
#define init_old_datai_    INIT_OLD_DATAI
#define get_old_datai_     GET_OLD_DATAI
#define get_procnames_     GET_PROCNAMES
#define save_proc_cisu_    SAVE_PROC_CISU
#define get_proc_num_      GET_PROC_NUM
#define save_proc_cip_     SAVE_PROC_CIP
#define get_proc_cip_      GET_PROC_CIP
#define save_proc_cid_     SAVE_PROC_CID
#define get_dcode_card_    GET_DCODE_CARD
#define get_cip_parms_     GET_CIP_PARMS
#define get_procnames_     GET_PROCNAMES
#define save_batch_dat_    SAVE_BATCH_DAT
#define get_batch_dat_     GET_BATCH_DAT
#define batch_set_         BATCH_SET
#define init_update_       INIT_UPDATE
#define get_nameoccur_     GET_NAMEOCCUR

#endif

/* Aliases for routines on VMS */
#if (VMS || _AIX || __hpux)
#define ncodew_            ncodew
#define dcoded_            dcoded
#define dcodes_            dcodes
#define ncodes_            ncodes
#define dcodebw_           dcodebw
#define ncodebw_           ncodebw
#define get_globals_addr_  get_globals_addr
#define get_global_vals_   get_global_vals
#define set_global_vals_   set_global_vals
#define init_old_datai_    init_old_datai
#define get_old_datai_     get_old_datai
#define get_procnames_     get_procnames
#define save_proc_cisu_    save_proc_cisu
#define get_proc_num_      get_proc_num
#define save_proc_cip_     save_proc_cip
#define get_proc_cip_      get_proc_cip
#define save_proc_cid_     save_proc_cid
#define get_dcode_card_    get_dcode_card
#define get_cip_parms_     get_cip_parms
#define get_procnames_     get_procnames
#define save_batch_dat_    save_batch_dat
#define get_batch_dat_     get_batch_dat
#define batch_set_         batch_set
#define init_update_       init_update
#define get_nameoccur_     get_nameoccur
#endif

struct Choice
 { Widget listwid;
   long   listnum;
 };

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

/* Prototypes of methods defined in xcfe_pop.c **
 *************************************************/
/* Function that creates the popup for CFE job building */
void *xcfe(Widget, char *, struct HELPCTX **, long *);

/* Callbacks for the 2 list widgets that are used. */
void cfe_choose       ( char * ,  void *);
void cfe_modify_stream( Widget ,  struct xcfe_data *, XtPointer);

/* Callback on the do processing button(true or false) */
void cfe_setup(  Widget ,  CpsJob *, XtPointer);
void xcfe_setbat( Widget ,  CpsJob *, XtPointer );

long cfe_manage_proc(Widget, struct xcfe_data *, char *, Widget *);
void cfe_unmanage_proc(char *);
void xcfe_destroy( Widget , long * , XtPointer );
void cfe_destroy ( Widget ,  struct xcfe_data *, XtPointer );
long cfe_pass_procs(  struct xcfe_data *data, long *ipn0);
long cfe_pass_ses(void *init, void *end, CpsProcess *curr_proc);

/* Toggle button callbacks for setting the build_mode */
void cfe_add_mode( Widget ,  CpsJob *, XtPointer );
void cfe_rep_mode( Widget ,  CpsJob *, XtPointer );
void cfe_del_mode( Widget ,  CpsJob *, XtPointer );
void cfe_mod_mode( Widget ,  CpsJob *, XtPointer );
void cfe_ins_mode( Widget ,  CpsJob *, XtPointer );
void xcfe_unmap( Widget ,  struct xcfe_data *, XEvent *);
void cfe_ok      ( Widget ,  struct xcfe_data *, XtPointer);
void cfe_help    ( Widget ,  struct HELPCTX *, XtPointer );

/* Callback and utilities for setting chices in 1st list widget */
void cfe_set_choices( Widget ,  struct Choice * , XtPointer );
void set_all_items( char *Items[], long *i);
void set_io_items ( char *s[], long *);
void set_amp_items( char *s[], long *);
void set_filter_items(char *s[], long *);
void set_cbyt_items(char *s[], long *);
void set_synthetics_items(char *s[], long *);
void set_migrations_items(char *s[], long *);


/* Prototypes of methods defined in xcfe_utils.c  **
 **************************************************/
void create_xcfe_(long *, CpsJob **, long *);
void destroy_xcfe_(long *);
void init_job_count();


void save_batch_dat_(char *jobn, char *uid, char *acc, char *pwd,
     char *proect,char *line, char *hist, char *libs,
     char *metric, char *phone, char *rname, char *raddr,
     char *print, char *pdnuid, long *prio, long *subpr, long *time);
void get_batch_dat_(char *jobn, char *uid, char *acc, char *pwd,
     char *proect,char *line, char *hist, char *libs,
     char *metric, char *phone, char *rname, char *raddr,
     char *print, char *pdnuid, long *prio, long *subpr, long *time);
void save_proc_cisu_(long *, char pnam[],long *,long *,long *,
     float *,float *,float *,float *,float dx[],long *);
void save_proc_cip_( long *, char card[]);
void get_proc_cip_ ( long *, char card[] );
void save_proc_cid_( long *, char card[]);

void get_procnames_(long *,char *);
void get_proclist_(long *, long *, char *plist[]);
long get_nameoccur_(char *name, long *count);
void get_proc_num_(long *ipn);
void get_jobid_(long *);
void get_job_struct(long *, CpsJob **);
CpsJob *get_cjob();
void get_proc_dat( long *, CpsProcess *, long *);
void set_proc_dat( long *, CpsProcess *, long *);
void get_cproc_dat(CpsProcess **);
void set_iproc_globals_(long *, long *, float *,
     float *,float *,float *,float dx[], long *);
void make_proc_cproc( CpsProcess *);
void make_iproc_cproc( long *, long *);
void make_job_current( CpsJob *);
void modify_job_(struct xcfe_data *, long *, long *);
void change_job(long *, CpsProcess *, long *, long *);
void set_build_mode_(long *, long *);
void get_build_mode_(long *, long *);

/* Public Methods for manuipulating a CpsProcess structure */
void        proc_destroy( CpsProcess *proc);
CpsProcess *proc_new( );
void        proc_remove(CpsProcess *proc);
CpsProcess *proc_next(CpsProcess *proc);
CpsProcess *proc_prev(CpsProcess *proc);
CpsJob     *proc_getjob(CpsProcess *proc);
void        proc_setjob(CpsProcess *proc, CpsJob *job);
long        proc_getnum(CpsProcess *proc);
void        proc_setnum(CpsProcess *proc,long number);
long        proc_getltr(CpsProcess *proc);
void        proc_setltr(CpsProcess *proc,long ltr);
void        proc_setlab(CpsProcess *proc,long label);
long        proc_getlab(CpsProcess *proc);
void        proc_setsav(CpsProcess *proc,long sav);
long        proc_getsav(CpsProcess *proc);
char       *proc_getnam(CpsProcess *proc);
void        proc_setnam(CpsProcess *proc, char *name);
void        proc_setndat(CpsProcess *proc, long ndat);
long        proc_getndat(CpsProcess *proc);
char        *proc_getcardi(CpsProcess *proc, long i);
void        proc_setcardi(CpsProcess *proc, long i,char *card);
int         proc_loopstrt(CpsProcess *proc);
char       *proc_getcall(CpsProcess *proc);
void        proc_setcall(CpsProcess *proc, char *card);
CpsGlobals *proc_getglob(CpsProcess *proc);
void        proc_setglob(CpsProcess *proc, CpsGlobals *G);
/* End of Methods */

void free_proc_ ( CpsProcess *);
void free_all_proc(CpsJob *);
void xfr_globals_addr(CpsGlobals *);
void get_dcode_card_(char *,long *, long *, long *, long *);

/* Prototypes of methods defined in xcfe_job.c **
 *************************************************/
CpsJob *job_new();
void    job_destroy(CpsJob *job);
long    job_get_nproc(CpsJob *job);
void    job_addproc(CpsJob *job, CpsProcess *proc);
CpsBatch *job_getbat(CpsJob *job);
CpsProcess *job_getfirst(CpsJob *job);
void    job_setfirst(CpsJob *job, CpsProcess *first);
void    job_setjid(CpsJob *job, long jobid);
long    job_getjid(CpsJob *job);
void    job_setfunc(CpsJob *job, void *pfunc);
void   *job_getfunc(CpsJob *job);
void    job_setbatch(CpsJob *job,
        char *jobn, char *uid, char *acc, char *pwd,
        char *proect,char *line, char *hist, char *libs,
        char *metric, char *phone, char *rname, char *raddr,
        char *print,char *pdnuid,  long prio, long subpr, long time);
void    job_getbatch(CpsJob *job,
        char *jobn, char *uid, char *acc, char *pwd,
        char *project,char *line, char *hist, char *libs,
        char *metric, char *phone, char *rname, char *raddr,
        char *print, char *pdnuid,long *prio,long *subpr,long *time);


/* Prototypes of methods defined in xcfe_to_cinf.c **
 ***************************************************/
void    xcfe_cinf_open( FILE **ifile, char *fname, long *io);
void    xcfe_to_cinf( char *fname, long *io, struct CPS_JOB *cpsjob);
CpsJob *xcfe_from_cinf( char *fname);

/* Prototypes of methods defined in xcfe_to_job.c **
 **************************************************/
void xcfe_job_open( FILE **ifile, char *fname, long *io);
void xcfe_to_job( char *fname, long *io, CpsJob *cpsjob);
void xcfe_bld_fort(FILE *ifile, CpsJob *job, long ,long ,long);
void xcfe_parse_job(CpsJob *job, long *maxiop, long *trtot, long
     *hdtot);
void xcfe_proc_prt(FILE *ifile, CpsProcess *proc, int *niop);

/* Prototypes of methods defined in xcfe_sub.c **
 *************************************************/
void xcfe_sub(CpsJob *job, long *istat, char *reqid);

#ifdef __cplusplus
}                   // for C++
#endif

#endif
