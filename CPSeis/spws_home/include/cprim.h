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


/*------------------------- cprim.h -------------------------------------*/
/*------------------------- cprim.h -------------------------------------*/
/*------------------------- cprim.h -------------------------------------*/
/*------------------------- cprim.h -------------------------------------*/

/*
    This file contains function prototypes and spelling adjustments
    for all utilities on the library cprim.a.  The spelling adjustments
    are needed only for functions which might be called from Fortran.

    These utilities have the following requirements:

     -- They all reside in library cprim.a .
     -- They all reside in source files in ~spws/util/cprim .
     -- They are all written in C.
     -- Each utility may contain one or more functions.
     -- Each utility must reside on a single source file, with
            documentation at the beginning of the file.
     -- They do not reference any routines on any libraries
            other than their own library (cprim.a) and the
            standard C libraries.
     -- None of them require any header files other than this
            one (cprim.h) and the standard C header files.
     -- None of them have any knowledge of Xlib, Xt, or Motif.
            (If they do, they should be put into wproc.a.)
*/


#ifndef _CPRIM_H_
#define _CPRIM_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "c2f_interface.h"

#ifdef sun
#ifndef SOLARIS
#include <spws_sunos.h>
#endif
#endif
#ifdef VMS
#include <spws_vms.h>
#endif
#ifdef __hpux
#include <spws_hpux.h>
#endif

#ifdef __cplusplus
extern "C" {              /* for C++ */
#endif


/*-------------- beginning of constants and macros ----------------------*/
/*-------------- beginning of constants and macros ----------------------*/
/*-------------- beginning of constants and macros ----------------------*/
/*-------------- beginning of constants and macros ----------------------*/


/*---------- misc constants not associated with any source file ---------*/
             /* or associated with more than one source file */

/* later the constants True, TRUE, False, and FALSE should be
   eliminated here since they are in named_constants.h */


#ifndef True
#define True 1
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef False
#define False 0
#endif

#ifndef FALSE
#define FALSE 0
#endif


#define UNKNOWN      0      /* unknown direction */
#define FORWARD      1      /* forward direction */
#define REVERSE     -1      /* reverse or backward direction */
#define BACKWARD    -1      /* reverse or backward direction */
#define UP           2      /* upward   direction */
#define DOWN        -2      /* downward direction */
#define RIGHT        3      /* right direction */
#define LEFT        -3      /* left  direction */
#define HORIZONTAL   4      /* horizontal  direction */
#define VERTICAL    -4      /* vertical    direction */



/*------------------- CPS header word constants -------------------------*/

  /*We are going to leave the following define. It is used in CFG for
    internal header generation work so it should not be a problem even
    though CPS can generate greater than 64 header words. If for some
    reason we need to generate more headers in CFG at a later date we
    can re-visit this issue.
  */

#define CPS_MAX_HEADERS       64 

/* later these constants should be eliminated and the similar ones
   in named_constants.h should be used instead */

// The following CPS_ constants are now in named_constants.h but with
// values 0-63 instead of 1-64.
// These are not referred to anywhere in the following locations:
//                 spws/include/*     spws/include/*/*
//                 spws/util/*        spws/util/*/*
//                 spws/oop/*         spws/oop/*/*
//                 spws/programs/*    spws/programs/*/*

/*
#define CPS_SEQUENCE           1
#define CPS_TOP_MUTE           2
#define CPS_CURR_GROUP         3
#define CPS_CURR_GROUP_CHAN    4
#define CPS_FOLD               5
#define CPS_OFFSET             6
#define CPS_CMP_X_GRID         7
#define CPS_CMP_Y_GRID         8
#define CPS_ORIG_GROUP         9
#define CPS_ORIG_GROUP_CHAN   10
#define CPS_SOURCE_X_DIST     11
#define CPS_SOURCE_Y_DIST     12
#define CPS_SOURCE_ELEV       13
#define CPS_RECEIVER_X_DIST   14
#define CPS_RECEIVER_Y_DIST   15
#define CPS_RECEIVER_ELEV     16
#define CPS_CMP_X_DIST        17
#define CPS_CMP_Y_DIST        18
#define CPS_CMP_ELEV          19
#define CPS_SOURCE_DEPTH      20
#define CPS_RECEIVER_DEPTH    21
#define CPS_SOURCE_COMPON     22
#define CPS_RECEIVER_COMPON   23
#define CPS_PANEL             24
#define CPS_LAV               25
#define CPS_SCRATCH_26        26
#define CPS_SCRATCH_27        27
#define CPS_SCRATCH_28        28
#define CPS_SCRATCH_29        29
#define CPS_SCRATCH_30        30
#define CPS_SCRATCH_31        31
#define CPS_SCRATCH_32        32
#define CPS_SOURCE_X_GRID     33
#define CPS_SOURCE_Y_GRID     34
#define CPS_RECEIVER_X_GRID   35
#define CPS_RECEIVER_Y_GRID   36
#define CPS_X_ANNOTATION      37
#define CPS_Y_ANNOTATION      38
#define CPS_PRE               39
#define CPS_POST              40
#define CPS_CUM_DATUM_STATIC  41
#define CPS_CUM_REFR_STATIC   42
#define CPS_CUM_RESID_STATIC  43
#define CPS_SOURCE_UPTIME     44
#define CPS_RECEIVER_UPTIME   45
#define CPS_SOURCE_GP         46
#define CPS_RECEIVER_GP       47
#define CPS_USER_48           48
#define CPS_USER_49           49
#define CPS_USER_50           50
#define CPS_USER_51           51
#define CPS_USER_52           52
#define CPS_USER_53           53
#define CPS_USER_54           54
#define CPS_USER_55           55
#define CPS_RPRE              56
#define CPS_RPOST             57
#define CPS_UNASSIGNED_58     58
#define CPS_UNASSIGNED_59     59
#define CPS_UNASSIGNED_60     60
#define CPS_UNASSIGNED_61     61
#define CPS_UNASSIGNED_62     62
#define CPS_UNASSIGNED_63     63
#define CPS_BOTTOM_MUTE       64
*/

/*---------- misc macros not associated with any source file ------------*/
            /* or associated with more than one source file */

/* this section is now in named_constants.h */

/*
#define MinimumValue(a,b)   (  (a) < (b) ? (a) :  (b)  )
#define MaximumValue(a,b)   (  (a) > (b) ? (a) :  (b)  )
#define AbsoluteValue(a)    (  (a) >  0  ? (a) : -(a)  )
#define AbsValue(a)         (  (a) >  0  ? (a) : -(a)  )
#define NearestInteger(a)   (  (a) >= 0  ? (long)((a) + 0.5)   \
                                         : (long)((a) - 0.5)  )

#define ConstrainValue(a,minimum,maximum)                                \
   (  (minimum) < (maximum)                                              \
     ? (  MinimumValue( (maximum), MaximumValue( (minimum), (a) ) )  )   \
     : (  MinimumValue( (minimum), MaximumValue( (maximum), (a) ) )  )  )

#define BinNumber(xbin,xcenter,xwidth)   \
   ( NearestInteger( ( (xbin) - (xcenter) ) / (xwidth) ) )

#define BinCenter(xbin,xcenter,xwidth)   \
   ( (xcenter) + (float)BinNumber(xbin,xcenter,xwidth) * (xwidth) )

#define BinCenter2(xbin_number,xcenter,xwidth)   \
   ( (xcenter) + (float)(xbin_number) * (xwidth) )
*/


/* the following is probably of little use and has been commented out */

/*
#define CopyArray(i,index1,index2,input,output)   \
  for(i = (index1); i <= (index2); i++) { output[i] = input[i]; }

#define FillArray(i,index1,index2,array,value)   \
  for(i = (index1); i <= (index2); i++) { array[i] = (value); }

#define AddArray(i,index1,index2,array,value)   \
  for(i = (index1); i <= (index2); i++) { array[i] += (value); }

#define MultArray(i,index1,index2,array,value)   \
  for(i = (index1); i <= (index2); i++) { array[i] *= (value); }
*/



/*------------------- CpOffset and CpOffsetOf macros --------------------*/
    /* this code is taken from Intrinsic.h */
    /* same as XtOffset and XtOffsetOf */
    /* placed here so the user does not need Intrinsic.h */

/* this section is now in named_constants.h */

/*
#if defined(CRAY) || defined(__arm)
#ifdef CRAY2

#define CpOffset(p_type,field) \
        (sizeof(int)*((unsigned int)&(((p_type)NULL)->field)))

#else   / * !CRAY2 * /

#define CpOffset(p_type,field) ((unsigned int)&(((p_type)NULL)->field))

#endif  / * !CRAY2 * /
#else   / * ! (CRAY || __arm) * /

#define CpOffset(p_type,field) \
     ((unsigned int) (((char *) (&(((p_type)NULL)->field))) - ((char *) NULL)))

#endif / * !CRAY * /

#define CpOffsetOf(s_type,field) CpOffset(s_type*,field)

#ifdef notdef
/ * this doesn't work on picky compilers * /
#define CpOffset(p_type,field)  ((unsigned int)&(((p_type)NULL)->field))
#endif

*/




/*------------- beginning of sections for each source file --------------*/
/*------------- beginning of sections for each source file --------------*/
/*------------- beginning of sections for each source file --------------*/
/*------------- beginning of sections for each source file --------------*/


/*-------------------------- pack16.c & pack32.c ------------------------*/
#if (VMS || _AIX || __hpux)
#define unpack16_ unpack16
#define pack16_   pack16
#endif
#ifdef NEED_CAPITALS
#define unpack16_ UNPACK16
#define pack16_   PACK16
#endif

void pack16_(long *in, long *istride, long *out, long *num);
void unpack16_(long *in, long *out, long *ostride, long *num);

#define PAKLIM 2500

#ifdef NEED_CAPITALS
#define upack32_ UPACK32
#define pack32_  PACK32
#endif
#if (VMS || _AIX || __hpux)
#define upack32_ UPACK32
#define pack32_  PACK32
#endif

void pack32_(long *in,long *istr,long *out,long *ostr,long *n,long *hl);
void upack32_(long *in, long *istr, long *out,  long *n, long *hl);

/*-------------------------- parse_file.c -------------------------------*/
#if (VMS || _AIX || __hpux)
#define parse_file_ parse_file
#define parse_ext_  parse_ext
#define parse_file_cppath_ parse_file_cppath
#define parse_fnet_ parse_fnet
#define parse_fbld_ parse_fbld
#endif
#ifdef NEED_CAPITALS
#define parse_file_ PARSE_FILE
#define parse_ext_  PARSE_EXT
#define parse_file_cppath_ PARSE_FILE_CPPATH
#define parse_fnet_ PARSE_FNET
#define parse_fbld_ PARSE_FBLD
#endif

char *parse_file_( char *file, char *name, char *path );
char *parse_ext_(char *name,char *root, char *ext);
void parse_file_cppath_(char *hfile, char *file);
long parse_fnet_( char *netnam, char *node, char *user, char *file,
      char *msg);
long parse_fbld_( char *netnam, char *node, char *user, char *file);


/*-------------------------- str.c -------------------------------*/

/* the file name str.c should be changed to strut.c (or similar) to
   avoid conflicting with the file str.c in sps */

  long strut_flush (long nl, long ngap, char *temp, FILE * out);
  char *strut_addsub (char *card, char *sub);
  char *strut_delsub (char *card);
  char *strut_changesub (char *card, char *sub);
  char *strut_left (char *card);
  char *strut_right (char *card);
  char *strut_pad (char *card, char *cadd);
  char *strut_new (char *card);
  long strut_nospaces (char *s);
  long strut_norepeat (char *s);
  char *strut_piece (char *out, char *start, char *end, long n);
  char *strut_rstr (char *cs, char *ct);

/*-------------------------- dcd.c -------------------------------*/

  long dcdut_dcode_file (void **value,
		       char *name, long ric, char *file, long nmax);
  long dcdut_dcode (void **value,
		  char *name, long ric, char *card);
  long dcdut_count_ascii_words (long maxlen, char *fname);
  long dcdut_get_ascii_word (char *s, long maxlen, FILE * f);
  void dcdut_lower (char *s);
  void dcdut_upper (char *s);
  long dcdut_isfloat (char c);
  long dcdut_isint (char c);
  long dcdut_count (char *s);
  long dcdut_float (float *r, long n, char *s);
  char *dcdut_field (char *out, char *in, long n);
  long dcdut_trans (float **r1, float **r2, char *coord, char *trans_file,
		  long max_lines);
  long dcdut_trunc (char *card);

/*-------------------------- get_inil.c ---------------------------------*/

/*
#ifdef NEED_UNDERSCORE
#define get_inil        get_inil_
#define get_fnil        get_fnil_
#define get_dnil        get_dnil_
#define get_ierr        get_ierr_
#define get_ferr        get_ferr_
#define get_derr        get_derr_
#define set_inil        set_inil_
#define set_fnil        set_fnil_
#define set_dnil        set_dnil_
#define set_ierr        set_ierr_
#define set_ferr        set_ferr_
#define set_derr        set_derr_
#endif

#ifdef REMOVE_UNDERSCORE
#define get_inil_       get_inil 
#define get_fnil_       get_fnil 
#define get_dnil_       get_dnil 
#define get_ierr_       get_ierr 
#define get_ferr_       get_ferr 
#define get_derr_       get_derr 
#define set_inil_       set_inil 
#define set_fnil_       set_fnil 
#define set_dnil_       set_dnil 
#define set_ierr_       set_ierr 
#define set_ferr_       set_ferr 
#define set_derr_       set_derr 
#endif

#ifdef NEED_CAPITALS
#define get_inil        GET_INIL 
#define get_fnil        GET_FNIL 
#define get_dnil        GET_DNIL 
#define get_ierr        GET_IERR 
#define get_ferr        GET_FERR 
#define get_derr        GET_DERR 
#define set_inil        SET_INIL 
#define set_fnil        SET_FNIL 
#define set_dnil        SET_DNIL 
#define set_ierr        SET_IERR 
#define set_ferr        SET_FERR 
#define set_derr        SET_DERR 
#endif

#ifdef NEED_CAPITALS
#define get_inil_       GET_INIL 
#define get_fnil_       GET_FNIL 
#define get_dnil_       GET_DNIL 
#define get_ierr_       GET_IERR 
#define get_ferr_       GET_FERR 
#define get_derr_       GET_DERR 
#define set_inil_       SET_INIL 
#define set_fnil_       SET_FNIL 
#define set_dnil_       SET_DNIL 
#define set_ierr_       SET_IERR 
#define set_ferr_       SET_FERR 
#define set_derr_       SET_DERR 
#endif

void get_inil (int   *inil);   / * for C or C++ or fortran * /
void get_fnil (float *fnil);   / * for C or C++ or fortran * /
void get_dnil (double*dnil);   / * for C or C++ or fortran * /
void get_ierr (int   *ierr);   / * for C or C++ or fortran * /
void get_ferr (float *ferr);   / * for C or C++ or fortran * /
void get_derr (double*derr);   / * for C or C++ or fortran * /

void set_inil (int   *inil);   / * for C or C++ or fortran * /
void set_fnil (float *fnil);   / * for C or C++ or fortran * /
void set_dnil (double*dnil);   / * for C or C++ or fortran * /
void set_ierr (int   *ierr);   / * for C or C++ or fortran * /
void set_ferr (float *ferr);   / * for C or C++ or fortran * /
void set_derr (double*derr);   / * for C or C++ or fortran * /

int    INIL(void);    / * alternative to get_inil for C or C++ * /
float  FNIL(void);    / * alternative to get_fnil for C or C++ * /
double DNIL(void);    / * alternative to get_dnil for C or C++ * /
int    IERR(void);    / * alternative to get_ierr for C or C++ * /
float  FERR(void);    / * alternative to get_ferr for C or C++ * /
double DERR(void);    / * alternative to get_derr for C or C++ * /
*/


/*------------------------ convert_ii2ss.c ------------------------------*/

/*
#if (ultrix || sun || __sgi)
#define convert_ii2ss              convert_ii2ss_
#define convert_ff2ss              convert_ff2ss_
#define convert_dd2ss              convert_dd2ss_
#define convert_ss2ii              convert_ss2ii_
#define convert_ss2ff              convert_ss2ff_
#define convert_ss2dd              convert_ss2dd_
#define convert_ii2ss_simple       convert_ii2ss_simple_
#define convert_ff2ss_simple       convert_ff2ss_simple_
#define convert_dd2ss_simple       convert_dd2ss_simple_
#define convert_ss2ii_simple       convert_ss2ii_simple_
#define convert_ss2ff_simple       convert_ss2ff_simple_
#define convert_ss2dd_simple       convert_ss2dd_simple_
#endif

#ifdef NEED_CAPITALS
#define convert_ii2ss              CONVERT_II2SS
#define convert_ff2ss              CONVERT_FF2SS
#define convert_dd2ss              CONVERT_DD2SS
#define convert_ss2ii              CONVERT_SS2II
#define convert_ss2ff              CONVERT_SS2FF
#define convert_ss2dd              CONVERT_SS2DD
#define convert_ii2ss_simple       CONVERT_II2SS_SIMPLE
#define convert_ff2ss_simple       CONVERT_FF2SS_SIMPLE
#define convert_dd2ss_simple       CONVERT_DD2SS_SIMPLE
#define convert_ss2ii_simple       CONVERT_SS2II_SIMPLE
#define convert_ss2ff_simple       CONVERT_SS2FF_SIMPLE
#define convert_ss2dd_simple       CONVERT_SS2DD_SIMPLE
#endif

void convert_ii2ss       (int  *ivar, char  *hvar, int *nchar);
void convert_ff2ss       (float *fvar, char  *hvar, int *nchar, int *ndec);
void convert_dd2ss       (double*dvar, char  *hvar, int *nchar, int *ndec);
void convert_ss2ii       (char  *hvar, int  *ivar, int *istat);
void convert_ss2ff       (char  *hvar, float *fvar, int *istat);
void convert_ss2dd       (char  *hvar, double*dvar, int *istat);
 
void convert_ii2ss_simple(int  *ivar, char  *hvar, int *nchar);
void convert_ff2ss_simple(float *fvar, char  *hvar, int *nchar, int *ndec);
void convert_dd2ss_simple(double*dvar, char  *hvar, int *nchar, int *ndec);
void convert_ss2ii_simple(char  *hvar, int  *ivar, int *istat);
void convert_ss2ff_simple(char  *hvar, float *fvar, int *istat);
void convert_ss2dd_simple(char  *hvar, double*dvar, int *istat);
*/
 

 
/*----------------------- string_copy.c ---------------------------------*/

/* this file should eventually be eliminated since it was originally
   written to deal with Fortran strings as well as C strings, plus some
   other stupid stuff. */

int   safe_strcmp     (char *cs, char *ct);
int   safe_strncmp    (char *cs, char *ct, size_t n);
char *safe_strcpy     (char  *s, char *ct);
char *safe_strncpy    (char  *s, char *ct, size_t n);
char *safe_strcat     (char  *s, char *ct);
char *safe_strncat    (char  *s, char *ct, size_t n);
char *string_copy     (char  *s, long ns,  char *ct, long nct);
int   string_length   (char *cs, long ncs);
int   string_compare  (char *cs, long ncs, char *ct, long nct);
int   strings_equal   (char *cs,           char *ct          );
char *string_alloc    (char  *s,           char *ct, long nct);
char *string_alloc_cat(char  *s,           char *ct, long nct);
char *string_free     (char  *s);



/*--------------------- remove_blanks.c ---------------------------------*/

/*
long remove_trailing_blanks (char *new_string, char *old_string);
long remove_all_blanks      (char *new_string, char *old_string);
*/



/*--------------------- convert_case.c ----------------------------------*/

/*
long convert_case_to_upper  (char *new_string, char *old_string);
long convert_case_to_lower  (char *new_string, char *old_string);
*/



/*-------------------------- make_family.c ------------------------------*/

typedef struct _FamilyStruct FamilyStruct;

typedef void FamilyFun (void *data);

FamilyStruct *make_family  (void);
int           join_family  (FamilyStruct *family, FamilyFun *fun, void *data);
int           unjoin_family(FamilyStruct *family, FamilyFun *fun, void *data);
void          call_family  (FamilyStruct *family);
FamilyStruct *free_family  (FamilyStruct *family);



/*-------------------------- make_chain.c -------------------------------*/

typedef struct _ChainStruct ChainStruct;

typedef struct _LinkStruct
{
    void        *prev;    /* pointer to previous structure in chain */
    void        *next;    /* pointer to   next   structure in chain */
    ChainStruct *chain;   /* pointer to structure defining this chain */
} LinkStruct;

ChainStruct *make_chain   (unsigned int offset);
ChainStruct *get_chain    (LinkStruct  link);
void         zero_link    (LinkStruct* link);
void         insert_link  (ChainStruct *chain, void *ss);
void insert_link_before   (ChainStruct *chain, void* ss, void *ss_in_chain);
void         remove_link  (ChainStruct *chain, void *ss);
void        *first_link   (ChainStruct *chain);
void        *last_link    (ChainStruct *chain);
int          number_links (ChainStruct *chain);
void        *prev_link    (ChainStruct *chain, void *ss);
void        *next_link    (ChainStruct *chain, void *ss);
ChainStruct *free_chain   (ChainStruct *chain);
ChainStruct *destroy_chain(ChainStruct *chain);

#define MakeChain(UserStruct, offset)    \
           make_chain(CpOffsetOf(UserStruct, offset))



/*-------------------------- snap_picks.c -------------------------------*/

#define PEAK       1      /* pickmode */
#define TROUGH    -1      /* pickmode */
#define POSITIVE   2      /* pickmode */
#define NEGATIVE  -2      /* pickmode */

#define MANUAL     1      /* action */
#define AUTOMATIC  2      /* action */
#define ZERO       3      /* action */
#define SNAP       4      /* action */

#define FOLLOW_LINE           1   /* automode */
#define FOLLOW_SLOPE          2   /* automode */
#define FOLLOW_CURVE          3   /* automode */
#define FIRST_BREAK           4   /* automode */
#define FIRST_BREAK_NO_SNAP   5   /* automode */
#define FIRST_BREAK_CORR      6   /* automode */
#define HURST_BREAK           7   /* automode */
#define HURST_BREAK_NO_SNAP   8   /* automode */
#define HURST_CORR            9   /* automode */
#define COMBO                10   /* automode */
#define COMBO_CORR           11   /* automode */
#define PICK_CORR            12   /* automode */
#define CORRELATE            13   /* automode */

void derive_picks_spws(long action, long pickmode, long automode, 
        long direction,
        float threshhold, float minSigDiff,
        long first_trace, long last_trace, long num_traces,
        float first_time, float last_time,
        float tmincur, float tmaxcur, float picks[],
        float missing_pick, float zero_pick, unsigned char traces[],
        long nsamp, float tmin, float dt, float *amplitudes,
        const float head[], long nwords, unsigned char all_traces[],
	long tot_num_traces, float all_picks[], const float all_head[],
	long index_into_all, float orf,
        float shortMin, float shortMax, float longMin, float longMax,
        float poto, float outlier);

void slope_picks(long first_trace, long last_trace, long num_traces,
                   float first_time, float last_time,
                   float tmincur, float tmaxcur,
                   float picks[], float missing_pick);

void snap_picks (long first_trace, long last_trace, long num_traces,
                   float tmincur, float tmaxcur,
                   float picks[], float missing_pick, float zero_pick,
                   unsigned char traces[],
                   long nsamp, float tmin, float dt, long pickmode,
                   float *amplitudes);

void snap_pick  (float *pick, unsigned char trace[],
                   long nsamp, float tmin, float dt, long pickmode,
                   float *amplitude);

void stat_sear  (int ipol, int irang, int interp, int nrecl,
       float xsam, float *smax, float *datmax, unsigned char jdat[]);

void set_hurst_func(void (*hurst_func)	/* 11oct96   ehs */
	(long first_trace, long last_trace, long num_traces,
	 float tmincur, float tmaxcur, float picks[], float missing_pick,
	 float zero_pick, unsigned char traces[], long nsamp, float tmin,
	 float dt, float minSigDiff, float *minPicks, float *maxPicks));

/*
 * break_pick used to be a static function.
 * It is now included in cprim.h so the hurst_func can call it.
 * 18oct96   ehs
 */
void break_pick(float *pick, unsigned char trace[],
	float tmincur, float tmaxcur,
	long nsamp, float tmin, float dt, long pickmode, float threshhold);

 
/*------------------------ math_util.c ---------------------------------*/

void copy_iarray(long  *array1, long  *array2, long n);
void copy_farray(float *array1, float *array2, long n);

long find_iarray_direction(long  array[], long n);
long find_farray_direction(float array[], long n);

void find_iarray_spacings(long  array[], long n, long  *mn, long  *mx);
void find_farray_spacings(float array[], long n, float *mn, float *mx);

void find_iarray_brackets(long  want, long  array[], long n,
                            long *ia, long *ib, float *wa, float *wb);
void find_farray_brackets(float want, float array[], long n,
                            long *ia, long *ib, float *wa, float *wb);

long find_iarray_nearest_match(long xwant, long x[], long n, long direction,
                                                 long adjustment);
long find_iarray_match      (long xwant, long x[], long n, long direction);

void switch_iarray_direction(long  *array, long n);
void switch_farray_direction(float *array, long n);

void remove_duplicate_iarray_values(long  *array, long *n);
void remove_duplicate_farray_values(float *array, long *n);
 
void convert_byte_to_float  (long n, unsigned char b[], float f[]);
void return_float_to_byte   (long n, float f[], unsigned char b[]);

float interpolated_ordinate(float xwant,
                       float xa, float xb, float ya, float yb);

float binary_terp1(float xwant, float *x, float *y, long n);

void regularize_sampling(float *x, float *y, long n,
                  float xmin, float xmax, long nbins, float *array);


/*------------------------- sort_util.c ---------------------------------*/

void generic_sort(long nsort,
                  int (*fun)(void *data, long lo, long up), void *data);

void ffl_sort(long nsort, float *x, float *y, long *r);



/*-------------------------- stat_data.c --------------------------------*/

/* this file will be eliminated since it has been superceded
   by static_kernal.cc  and static_kernal.hh */

#define ZNIL      -1.0e-30  /* value representing a   nil   time pick */
#define MISSING   -9.0e22   /* value representing a missing time pick */
                                         /* or a missing xbin or ybin */
  
#define XMATCH    1   /* attribute for getting prev, next, or sel picks */
#define XYDIFF    2   /* attribute for getting prev, next, or sel picks */

 /* NOTE: indices ix    range from 1 thru nx    (fortran style) */
 /* NOTE: indices iy    range from 1 thru ny    (fortran style) */
 /* NOTE: indices icard range from 1 thru ncard (fortran style) */

typedef struct _StatStruct StatStruct;

StatStruct *stat_create      (void);
void        stat_clear_arrays(StatStruct *ss);
void        stat_clear       (StatStruct *ss);
StatStruct *stat_destroy     (StatStruct *ss);

void         stat_bad_validation      (StatStruct *ss);
const char  *stat_get_time_string     ();
const char  *stat_get_program_pointer (StatStruct *ss);
const char  *stat_get_type_pointer    (StatStruct *ss);
const char  *stat_get_card_pointer    (StatStruct *ss, long icard);

void   stat_set_nhx       (StatStruct *ss, long nhx);
void   stat_set_nhy       (StatStruct *ss, long nhy);
void   stat_set_nhx2      (StatStruct *ss, long nhx2);
void   stat_set_nhy2      (StatStruct *ss, long nhy2);
void   stat_set_x1        (StatStruct *ss, float x1);
void   stat_set_y1        (StatStruct *ss, float y1);
void   stat_set_xinc      (StatStruct *ss, float xinc);
void   stat_set_yinc      (StatStruct *ss, float yinc);
void   stat_set_nx        (StatStruct *ss, long nx);
void   stat_set_ny        (StatStruct *ss, long ny);
void   stat_set_xend      (StatStruct *ss, float xend);
void   stat_set_yend      (StatStruct *ss, float yend);

void   stat_get_type      (StatStruct *ss, char *type);
long   stat_get_nhx       (StatStruct *ss);
long   stat_get_nhy       (StatStruct *ss);
long   stat_get_nhx2      (StatStruct *ss);
long   stat_get_nhy2      (StatStruct *ss);
float  stat_get_x1        (StatStruct *ss);
float  stat_get_y1        (StatStruct *ss);
float  stat_get_xinc      (StatStruct *ss);
float  stat_get_yinc      (StatStruct *ss);
long   stat_get_nx        (StatStruct *ss);
long   stat_get_ny        (StatStruct *ss);
long   stat_get_ncards    (StatStruct *ss);
float  stat_get_xend      (StatStruct *ss);
float  stat_get_yend      (StatStruct *ss);
int    stat_get_card      (StatStruct *ss, char *card, long icard);
float  stat_get_value     (StatStruct *ss, long ix, long iy);
int    stat_get_values    (StatStruct *ss, float *values);
float  stat_get_terp_value(StatStruct *ss, float xbin, float ybin);
float *stat_get_pointer   (StatStruct *ss);

int stat_get_ivar (StatStruct *ss, long   *ivar, char *ident);
int stat_get_fvar (StatStruct *ss, float  *fvar, char *ident);
int stat_get_dvar (StatStruct *ss, double *dvar, char *ident);
int stat_get_cvar (StatStruct *ss, char   *cvar, char *ident);
int stat_get_ivar2(StatStruct *ss, long   *ivar, long   *ivar2, char *ident);
int stat_get_fvar2(StatStruct *ss, float  *fvar, float  *fvar2, char *ident);
int stat_get_dvar2(StatStruct *ss, double *dvar, double *dvar2, char *ident);
int stat_get_cvar2(StatStruct *ss, char   *cvar, char   *cvar2, char *ident);

void   stat_set_program   (StatStruct *ss, char *program);
void   stat_set_type      (StatStruct *ss, char *type);
void   stat_set_xheads    (StatStruct *ss, long nhx, long nhx2);
void   stat_set_yheads    (StatStruct *ss, long nhy, long nhy2);
void   stat_set_xbins     (StatStruct *ss, float x1, float xinc, long nx);
void   stat_set_ybins     (StatStruct *ss, float y1, float yinc, long ny);
void   stat_set_xrange    (StatStruct *ss, float x1, float xinc, float xend);
void   stat_set_yrange    (StatStruct *ss, float y1, float yinc, float yend);
int    stat_set_xbin_array(StatStruct *ss, float *xbins, long nx);
int    stat_set_ybin_array(StatStruct *ss, float *ybins, long ny);
void   stat_remove_cards  (StatStruct *ss);
int    stat_replace_card  (StatStruct *ss, char *card, long icard);
int    stat_add_card      (StatStruct *ss, char *card);
int    stat_set_value     (StatStruct *ss, long ix, long iy, float value);
int    stat_set_values    (StatStruct *ss, float *values);
int    stat_clear_values  (StatStruct *ss);
void   stat_free_pointer  (StatStruct *ss);
void   stat_set_pointer   (StatStruct *ss, float *pointer);

int stat_set_ivar (StatStruct *ss, long   ivar, char *ident, char *comment);
int stat_set_fvar (StatStruct *ss, float  fvar, char *ident, char *comment);
int stat_set_dvar (StatStruct *ss, double dvar, char *ident, char *comment);
int stat_set_cvar (StatStruct *ss, char  *cvar, char *ident, char *comment);
int stat_set_ivar2(StatStruct *ss, long   ivar, long   ivar2, char *ident,
                                                              char *comment);
int stat_set_fvar2(StatStruct *ss, float  fvar, float  fvar2, char *ident,
                                                              char *comment);
int stat_set_dvar2(StatStruct *ss, double dvar, double dvar2, char *ident,
                                                              char *comment);
int stat_set_cvar2(StatStruct *ss, char  *cvar, char  *cvar2, char *ident,
                                                              char *comment);

int   stat_set_matching_value (StatStruct *ss, float xbin, float ybin,
                               float value, float xwidth, float ywidth);

long  stat_get_nearest_ix  (StatStruct *ss, float xbin, long adjustment);
long  stat_get_nearest_iy  (StatStruct *ss, float ybin, long adjustment);
float stat_get_nearest_xbin(StatStruct *ss, float xbin, long adjustment);
float stat_get_nearest_ybin(StatStruct *ss, float ybin, long adjustment);
float stat_get_nearby_xbin (StatStruct *ss, long ix);
float stat_get_nearby_ybin (StatStruct *ss, long iy);

int  stat_get_picks_spws(StatStruct *ss, float head[], long nwords,
         long ntraces, float picks[]);

int  stat_put_picks_spws(StatStruct *ss, float head[], long nwords,
         long ntraces, float picks[]);

int  stat_get_prev_picks_spws(StatStruct *ss, float head[], long nwords,
         long ntraces, float picks[], long att, long xy_switch);

int  stat_get_next_picks_spws(StatStruct *ss, float head[], long nwords,
         long ntraces, float picks[], long att, long xy_switch);

int  stat_get_sel_picks_spws (StatStruct *ss, float head[], long nwords,
         long ntraces, float picks[], long att, long xy_switch, float ysel);

  /* the following typedefs are used by stat_data.c and scrs_data.c */

typedef void  MsgFun       (void *msgdata, char *msg);
typedef void *TFOpen       (char *filename, long *nwords, long *ntraces);
typedef long  TFReadHeader (void *glbl, float fhead[], int open_file);
typedef long  TFReadHdrTr  (void *glbl, float fhead[]);
typedef long  TFRewind     (void *glbl);
typedef long  TFClose      (void *glbl);

/*
int  stat_create_from_tracefile(StatStruct *ss, void (*msgfun)(),
      void *msgdata, char *msg, char *tracefile,
      void *(*tracefile_open)(), long (*tracefile_read_header)(),
      long (*tracefile_rewind)(), long (*tracefile_close)());
*/
int  stat_create_from_tracefile(StatStruct *ss, MsgFun *msgfun,
      void *msgdata, char *msg, char *tracefile,
      TFOpen   *tracefile_open  , TFReadHeader *tracefile_read_header,
      TFRewind *tracefile_rewind, TFClose      *tracefile_close);

/*
int  stat_prepare(StatStruct *ss, char *filename1, char *filename2,
       void (*msgfun)(), void *msgdata, long status,
       char *msg, char *tracefile,
       void *(*tracefile_open)(), long (*tracefile_read_header)(),
       long (*tracefile_rewind)(), long (*tracefile_close)());
*/
int  stat_prepare(StatStruct *ss, char *filename1, char *filename2,
       MsgFun *msgfun, void *msgdata, long status,
       char *msg, char *tracefile,
      TFOpen   *tracefile_open  , TFReadHeader *tracefile_read_header,
      TFRewind *tracefile_rewind, TFClose      *tracefile_close);

int  stat_save_cps_file(StatStruct *ss, char *filename, char *msg);
int  stat_read_cps_file(StatStruct *ss, char *filename, char *msg);

long stat_validate      (StatStruct *ss, char *filename, char *info);
long stat_check_validity(StatStruct *ss, char *filename, char *info);

void stat_check_validities(StatStruct *ss, char *filename1, char *filename2,
     long *valid1, long *valid2, char *info1, char *info2,
     long *same_datasets);

long stat_inquire(StatStruct *ss, char *filename1, char *filename2,
     long required1, long required2,
     char *msg1, char *msg2, char *msg3,
     long *status1, long *status2);


/*-------------------------- scrs_data.c --------------------------------*/

#define SEQU     1    /* sequential ground positions (46, 0,47, 0) */
#define GRID     2    /* grid       ground positions (33,34,35,36) */

#define CHANNEL   1   /* attribute for getting prev, next, or sel picks */
#define OFFSET    2   /* attribute for getting prev, next, or sel picks */
#define RECEIVER  3   /* attribute for getting prev, next, or sel picks */

#define SCRS_MISSING -999.0   /* value representing a missing time pick */
#define SCRS_ZERO       0.0   /* value representing a zero-ed time pick */

typedef struct _ScrsStruct ScrsStruct;

long  scrs_get_first_profile  (ScrsStruct *ss);
long  scrs_get_last_profile   (ScrsStruct *ss);
long  scrs_get_nearest_profile(ScrsStruct *ss, long igroup, long adjustment);

long scrs_check_validity(char *filename, char *info,
                                            long *ngrp, long *nch);

void scrs_check_validities(char *filename1, char *filename2,
     long *valid1, long *valid2, char *info1, char *info2,
     long *same_datasets);

long scrs_inquire(char *filename1, char *filename2,
     long required1, long required2,
     char *msg1, char *msg2, char *msg3,
     long *status1, long *status2);

/*
ScrsStruct *scrs_open(char *filename1, char *filename2, void (*msgfun)(),
      void *msgdata, long typegp, long status,
      char *msg, char *tracefile,
      void *(*tracefile_open)(), long (*tracefile_read_header)(),
      long (*tracefile_rewind)(), long (*tracefile_close)());
*/
ScrsStruct *scrs_open(char *filename1, char *filename2, MsgFun *msgfun,
      void *msgdata, long typegp, long status,
      char *msg, char *tracefile,
      TFOpen   *tracefile_open  , TFReadHeader *tracefile_read_header,
      TFRewind *tracefile_rewind, TFClose      *tracefile_close,
      int       tracefile_pickhead);

long  scrs_get_ngrp  (ScrsStruct *ss);
long  scrs_get_nch   (ScrsStruct *ss);
long  scrs_get_latest(ScrsStruct *ss);
int   scrs_close     (ScrsStruct *ss);
void  scrs_abort     (ScrsStruct *ss);

int   scrs_get_picks_spws (ScrsStruct *ss, float head[], long nwords,
                                            long ntraces, float picks[]);
int   scrs_put_picks_spws (ScrsStruct *ss, float head[], long nwords,
                                            long ntraces, float picks[]);

int scrs_get_prev_picks_spws(ScrsStruct *ss, float head[], long nwords,
                      long ntraces, float picks[], long att);
int scrs_get_next_picks_spws(ScrsStruct *ss, float head[], long nwords,
                      long ntraces, float picks[], long att);
int scrs_get_sel_picks_spws (ScrsStruct *ss, float head[], long nwords,
                      long ntraces, float picks[], long att, long grp);

int   scrs_head_get  (ScrsStruct *ss);
int   scrs_head_put  (ScrsStruct *ss);
int   scrs_shot_get  (ScrsStruct *ss);
int   scrs_shot_put  (ScrsStruct *ss);
int   scrs_shot_getd (ScrsStruct *ss, long igroup);
int   scrs_shot_putd (ScrsStruct *ss, long igroup);
int   scrs_tail_get  (ScrsStruct *ss);
int   scrs_tail_put  (ScrsStruct *ss);

int   scrs_head_grab (ScrsStruct *ss, long *ngrp, long *incgp, float *rinc,
                                   long *nch, float *trsh, char izc[]);

int   scrs_head_fill (ScrsStruct *ss, long ngrp, long incgp, float rinc,
                                   long nch, float trsh, char izc[]);

int   scrs_head_grab2(ScrsStruct *ss, long *nxpw, long *nypw, long *npwoff,
                             long *ipkwhx, long *ipkwhy,
                             float xpw[], float ypw[], float pkwn[]);

int   scrs_head_fill2(ScrsStruct *ss, long nxpw, long nypw, long npwoff,
                             long ipkwhx, long ipkwhy,
                             float xpw[], float ypw[], float pkwn[]);

int   scrs_shot_grab (ScrsStruct *ss, float shot[], float picks[],
                     long ixgp[], long iygp[], long ioff[], long elev[]);

int   scrs_shot_fill (ScrsStruct *ss, float shot[], float picks[],
                     long ixgp[], long iygp[], long ioff[], long elev[]);

int   scrs_shot_fillh_spws(ScrsStruct *ss, float head[], long nwords, 
                           float picks[]);

int   scrs_shot_fill1_spws(ScrsStruct *ss, float head[], long nwords,
                           float pick, long ich);

int   scrs_head_grab3(ScrsStruct *ss, long *itmax, long *mxxgp, long *mxygp,
                                   long *mnxgp, long *mnygp, long *mxo);

int   scrs_head_fill3(ScrsStruct *ss, long itmax, long mxxgp, long mxygp,
                                   long mnxgp, long mnygp, long mxo);



/*-------------------------- i_exist.c ------------------------------*/
void i_exist( int argc, char *argv[], char *check_str );


/*-------------------------- newstr.c ------------------------------*/

/* this file should eventually be eliminated since its efforts are
   duplicated in the sps file str.c and str.h */

char *newstr( const char *s);
char *newstrcat( char *s, ...);
char *strToUpper( char *s);
char *strToLower( char *s);


/*---------- File name same as the function name ------------*/

/* exp_tilde and time_date_string should be eliminated since their
   functionality is included in the sps files exptilde.c and str.c
   respectively. */


int strcmpnull( char *s1, char *s2 );
int strlennull( char *s1 );
void  exp_tilde( char *expstr, char *instr );
char *strip_file( char *lfile, char *retfile );
char *time_date_string(void);

/*-------------------------- time_date_string.c ------------------------*/

/* now obsolete (replaced by functionality in str.c) */


#ifdef NEED_CAPITALS
#define time_date_stringf_   TIME_DATE_STRINGF
#endif
#if (VMS || _AIX || __hpux)
#define time_date_stringf_   time_date_stringf
#endif
void time_date_stringf_(char *timeordate, int *flag);


/*-------------------------- read_card.c --------------------------------*/

/*
char *read_card(FILE *stream);
char *read_cards(FILE *stream, int num_cards);
*/


/*-------------------------- exp_file.c --------------------------------*/
long exp_file( char *fullfile, char *infile);
char *getdir();


/*-------------------------- tracefile.c --------------------------------*/

typedef struct _TracefileStruct TracefileStruct;

TracefileStruct *tracefile_open(char *filename, long *nwords, long *ntraces);
long            tracefile_read_header(TracefileStruct *glbl, float fhead[],
                                      int open_file);
long            tracefile_rewind     (TracefileStruct *glbl);
long            tracefile_close      (TracefileStruct *glbl);


/*------------------------ memory_cache.c -----------------------*/

typedef struct _MemoryCache MemoryCache;

MemoryCache *create_memory_cache  (long size, long max_number);
MemoryCache *destroy_memory_cache (MemoryCache *cache);
void         add_to_memory_cache  (MemoryCache *cache, void *data, long ident);
void        *get_from_memory_cache(MemoryCache *cache, void *data, long ident);


/*----------------------- memory_util.c -----------------------*/

/*
void *memory_alloc_generic  (void *array, long n, long size, int *error);
void memory_copy_generic    (void *array2, long index2,
                             void *array1, long index1, long ncopy, long size);
long memory_insert_generic  (void *array, long n, long size, long index,
                             long nins, void *values);
long memory_remove_generic  (void *array, long n, long size, long index,
                             long nrem);
long memory_replace_generic (void *array, long n, long size, long index,
                             long nrep, void *values);
long memory_rem_ins_generic (void *array, long n, long size, long index,
                             long nrem, long nins, void *values);
void memory_fetch_generic   (void *array, long n, long size, long index,
                             long nget, void *values);

float *memory_alloc_floats  (float *array, long n, int *error);
void memory_copy_floats     (float *array2, long index2,
                             float *array1, long index1, long ncopy);
long memory_insert_floats   (float *array, long n, long index,
                             long nins, float *values);
long memory_remove_floats   (float *array, long n, long index,
                             long nrem);
long memory_replace_floats  (float *array, long n, long index,
                             long nrep, float *values);
long memory_rem_ins_floats  (float *array, long n, long index,
                             long nrem, long nins, float *values);
void memory_fetch_floats    (float *array, long n, long index,
                             long nget, float *values);

char *memory_alloc_chars    (char *array, long n, int *error);
void memory_copy_chars      (char *array2, long index2,
                             char *array1, long index1, long ncopy);
long memory_insert_chars    (char *array, long n, long index,
                             long nins, char *values);
long memory_remove_chars    (char *array, long n, long index,
                             long nrem);
long memory_replace_chars   (char *array, long n, long index,
                             long nrep, char *values);
long memory_rem_ins_chars   (char *array, long n, long index,
                             long nrem, long nins, char *values);
void memory_fetch_chars     (char *array, long n, long index,
                             long nget, char *values);
*/


/*----------------------- binary_search.c -----------------------*/

/*
#define EXTRAP_DOWN  1
#define EXTRAP_UP    2
#define EXACT_MATCH  3
#define INTERPOLATE  4
#define NO_VALUES    5

typedef int BinarySearchFun (void *data, long i);

int  binary_search_generic (BinarySearchFun *fun, void *data,
                                  long n, long *ia, long *ib);
int  binary_search_floats  (float *array, float want, float tolerance,
                                  long n, long *ia, long *ib);
*/


/*---------------------- qsearch_object.c -----------------------*/

/*
typedef struct _QsearchStruct QsearchStruct;

typedef float QsearchFun (void *data, long i);

QsearchStruct *qsearch_create  (void);
QsearchStruct *qsearch_destroy (QsearchStruct *qsst);

void qsearch_set_tolerance     (QsearchStruct *qsst, float tolerance);
void qsearch_set_ascending     (QsearchStruct *qsst);
void qsearch_set_descending    (QsearchStruct *qsst);
void qsearch_register_function (QsearchStruct *qsst,
                                        QsearchFun *fun, void *data);
void qsearch_register_array    (QsearchStruct *qsst, float *array);
int  qsearch_perform           (QsearchStruct *qsst, float want, long n);

int  qsearch_flag              (QsearchStruct *qsst);
long qsearch_ia                (QsearchStruct *qsst);
long qsearch_ib                (QsearchStruct *qsst);

long qsearch_matching_index    (QsearchStruct *qsst);
long qsearch_nearest_index     (QsearchStruct *qsst, int adjustment);
long qsearch_prev_index        (QsearchStruct *qsst);
long qsearch_next_index        (QsearchStruct *qsst);
long qsearch_insertion_index   (QsearchStruct *qsst);
long qsearch_insertion2_index  (QsearchStruct *qsst);

int  qsearch_extrap_down       (QsearchStruct *qsst);
int  qsearch_extrap_up         (QsearchStruct *qsst);
int  qsearch_exact_match       (QsearchStruct *qsst);
int  qsearch_interpolate       (QsearchStruct *qsst);
int  qsearch_no_values         (QsearchStruct *qsst);
*/



/*------------------------- interp_util.c ---------------------*/

float interp_two_point (float x, float xa, float va,
                                 float xb, float vb);
float interp_floats    (float x, float *xarray, long n, float *varray);


/*------------------------- xdim_object.c ---------------------*/

/*
typedef struct _XdimStruct XdimStruct;

XdimStruct *xdim_create     (float y, float z, float xtol);
void        xdim_clear      (XdimStruct *xdim);
XdimStruct *xdim_destroy    (XdimStruct *xdim);

float xdim_get_nil          (void);
long  xdim_get_nx           (XdimStruct *xdim);

float xdim_get_z            (XdimStruct *xdim);
float xdim_get_y            (XdimStruct *xdim);
float xdim_get_x            (XdimStruct *xdim, long ix);
float xdim_get_v            (XdimStruct *xdim, long ix);

float xdim_get_value        (XdimStruct *xdim, float x);
float xdim_get_extrap_value (XdimStruct *xdim, float x);
int   xdim_insert_point     (XdimStruct *xdim, float x, float v);
int   xdim_insert_range     (XdimStruct *xdim, float xa, float va,
                                               float xb, float vb);

int   xdim_write_to_file    (XdimStruct *xdim, FILE *stream, int blank_line);
*/


/*------------------------- xydim_object.c ---------------------*/

/*
typedef struct _XYdimStruct XYdimStruct;

XYdimStruct *xydim_create       (float z, float xtol, float ytol);
void         xydim_clear        (XYdimStruct *xydim);
XYdimStruct *xydim_destroy      (XYdimStruct *xydim);

float xydim_get_nil        (void);
long  xydim_get_ny         (XYdimStruct *xydim);
long  xydim_get_nx         (XYdimStruct *xydim, long iy);

float xydim_get_z          (XYdimStruct *xydim);
float xydim_get_y          (XYdimStruct *xydim, long iy);
float xydim_get_x          (XYdimStruct *xydim, long ix, long iy);
float xydim_get_v          (XYdimStruct *xydim, long ix, long iy);

float xydim_get_terp_value (XYdimStruct *xydim, float x, float y);
float xydim_get_value      (XYdimStruct *xydim, float x, float y);
int   xydim_insert_point   (XYdimStruct *xydim, float x, float y, float v);
int   xydim_insert_range   (XYdimStruct *xydim, float xa, float va,
                                                float xb, float vb,
                                                float y);

float xydim_get_matching_y(XYdimStruct *xydim, float y);
float xydim_get_nearest_y (XYdimStruct *xydim, float y, long adjustment);
float xydim_get_prev_y    (XYdimStruct *xydim, float y);
float xydim_get_next_y    (XYdimStruct *xydim, float y);

int   xydim_write_to_file (XYdimStruct *xydim, FILE *stream, int blank_line);
*/


/*------------------------- xyzdim_object.c ---------------------*/

/*
typedef struct _XYZdimStruct XYZdimStruct;

XYZdimStruct *xyzdim_create       (float xtol, float ytol, float ztol);
void          xyzdim_clear        (XYZdimStruct *xyzdim);
XYZdimStruct *xyzdim_destroy      (XYZdimStruct *xyzdim);

float xyzdim_get_nil      (void);
long  xyzdim_get_nz       (XYZdimStruct *xyzdim);
long  xyzdim_get_ny       (XYZdimStruct *xyzdim, long iz);
long  xyzdim_get_nx       (XYZdimStruct *xyzdim, long iy, long iz);

float xyzdim_get_z        (XYZdimStruct *xyzdim, long iz);
float xyzdim_get_y        (XYZdimStruct *xyzdim, long iy, long iz);
float xyzdim_get_x        (XYZdimStruct *xyzdim, long ix, long iy, long iz);
float xyzdim_get_v        (XYZdimStruct *xyzdim, long ix, long iy, long iz);

float xyzdim_get_xmin     (XYZdimStruct *xyzdim);
float xyzdim_get_ymin     (XYZdimStruct *xyzdim);
float xyzdim_get_zmin     (XYZdimStruct *xyzdim);
float xyzdim_get_xmax     (XYZdimStruct *xyzdim);
float xyzdim_get_ymax     (XYZdimStruct *xyzdim);
float xyzdim_get_zmax     (XYZdimStruct *xyzdim);

float xyzdim_get_terp_value (XYZdimStruct *xyzdim, float x, float y, float z);
float xyzdim_get_value      (XYZdimStruct *xyzdim, float x, float y, float z);
int   xyzdim_insert_point   (XYZdimStruct *xyzdim, float x, float y, float z,
                                                   float v);
int   xyzdim_insert_range   (XYZdimStruct *xyzdim, float xa, float va,
                                                   float xb, float vb,
                                                   float y, float z);

float xyzdim_get_matching_y (XYZdimStruct *xyzdim, float y, float z);
float xyzdim_get_matching_z (XYZdimStruct *xyzdim, float y, float z);

float xyzdim_get_nearest_y  (XYZdimStruct *xyzdim, float y, float z, long adj);
float xyzdim_get_nearest_z  (XYZdimStruct *xyzdim, float y, float z, long adj);

float xyzdim_get_prev_y     (XYZdimStruct *xyzdim, float y, float z);
float xyzdim_get_next_y     (XYZdimStruct *xyzdim, float y, float z);
float xyzdim_get_prev_z     (XYZdimStruct *xyzdim, float y, float z);
float xyzdim_get_next_z     (XYZdimStruct *xyzdim, float y, float z);

int   xyzdim_write_to_file  (XYZdimStruct *xyzdim, FILE *stream);
int   xyzdim_read_from_file (XYZdimStruct *xyzdim, FILE *stream);
*/


/*------------------------- mutefile_object.c ---------------------*/

/*
typedef struct _MuteStruct MuteStruct;

MuteStruct *mutefile_create_spws       (void);
void        mutefile_clear_spws        (MuteStruct *ss);
MuteStruct *mutefile_destroy_spws      (MuteStruct *ss);

void  mutefile_make_grid_spws (MuteStruct *ss,
                          float xmin, float ymin, float zmin,
                          float xmax, float ymax, float zmax,
                          float xinc, float yinc, float zinc,
                          MuteStruct *source);

void  mutefile_replace_with_grid_spws (MuteStruct *ss,
                                  float xmin, float ymin, float zmin,
                                  float xmax, float ymax, float zmax,
                                  float xinc, float yinc, float zinc);

float mutefile_get_nil_spws      (void);
long  mutefile_get_nz_spws       (MuteStruct *ss);
long  mutefile_get_ny_spws       (MuteStruct *ss, long iz);
long  mutefile_get_nx_spws       (MuteStruct *ss, long iy, long iz);

float mutefile_get_z_spws        (MuteStruct *ss, long iz);
float mutefile_get_y_spws        (MuteStruct *ss, long iy, long iz);
float mutefile_get_x_spws        (MuteStruct *ss, long ix, long iy, long iz);
float mutefile_get_v_spws        (MuteStruct *ss, long ix, long iy, long iz);

long  mutefile_get_nhx_spws      (MuteStruct *ss);
long  mutefile_get_nhy_spws      (MuteStruct *ss);
long  mutefile_get_nhz_spws      (MuteStruct *ss);

float mutefile_get_ylatest_spws  (MuteStruct *ss);
float mutefile_get_zlatest_spws  (MuteStruct *ss);
float mutefile_get_ysel_spws     (MuteStruct *ss);
float mutefile_get_zsel_spws     (MuteStruct *ss);
int   mutefile_get_yz_switch_spws(MuteStruct *ss);
int   mutefile_get_interp_spws   (MuteStruct *ss);

float mutefile_get_xmin_spws     (MuteStruct *ss);
float mutefile_get_ymin_spws     (MuteStruct *ss);
float mutefile_get_zmin_spws     (MuteStruct *ss);
float mutefile_get_xmax_spws     (MuteStruct *ss);
float mutefile_get_ymax_spws     (MuteStruct *ss);
float mutefile_get_zmax_spws     (MuteStruct *ss);

int   mutefile_set_nhx_spws      (MuteStruct *ss, long nhx);
int   mutefile_set_nhy_spws      (MuteStruct *ss, long nhy);
int   mutefile_set_nhz_spws      (MuteStruct *ss, long nhz);
int   mutefile_set_ysel_spws     (MuteStruct *ss, float ysel, int directional);
int   mutefile_set_zsel_spws     (MuteStruct *ss, float zsel, int directional);
int   mutefile_set_yz_switch_spws(MuteStruct *ss, int yz_switch);
int   mutefile_set_interp_spws   (MuteStruct *ss, int interp);

float mutefile_get_terp_value_spws (MuteStruct *ss, float x, float y, float z);
float mutefile_get_value_spws      (MuteStruct *ss, float x, float y, float z);
int   mutefile_insert_point_spws   (MuteStruct *ss, float x, float y, float z,
                                               float v);
int   mutefile_insert_range_spws   (MuteStruct *ss, float xa, float va,
                                               float xb, float vb,
                                               float y, float z);

float mutefile_get_x_from_header_spws (MuteStruct *ss, const float *head);
float mutefile_get_y_from_header_spws (MuteStruct *ss, const float *head);
float mutefile_get_z_from_header_spws (MuteStruct *ss, const float *head);

void mutefile_get_picks_spws (MuteStruct *ss, const float head[], long nwords,
                              long ntraces, float picks[]);
void mutefile_put_picks_spws (MuteStruct *ss, const float head[], long nwords,
                              long ntraces, float picks[]);
void mutefile_get_prev_picks_spws (MuteStruct *ss, const float head[], 
                              long nwords,
                              long ntraces, float picks[]);
void mutefile_get_next_picks_spws (MuteStruct *ss, const float head[],
                              long nwords,
                              long ntraces, float picks[]);
void mutefile_get_sel_picks_spws  (MuteStruct *ss, const float head[], 
                              long nwords,
                              long ntraces, float picks[]);

int  mutefile_prepare_spws (MuteStruct *ss, char *filename1, char *filename2,
                       void (*msgfun)(void *data, char *msg),
                       void *msgdata, long status, char *msg);

int  mutefile_save_file_spws   (MuteStruct *ss, char *filename, char *msg);
int  mutefile_read_file_spws   (MuteStruct *ss, char *filename, char *msg); 

long mutefile_check_validity_spws  (MuteStruct *ss, char *filename, char *info);

void mutefile_check_validities_spws (MuteStruct *ss,
                                char *filename1, char *filename2,
                                long    *valid1, long    *valid2,
                                char     *info1, char     *info2,
                                long *same_datasets);

long mutefile_inquire_spws (MuteStruct *ss,
                       char *filename1, char *filename2,
                       long  required1, long  required2,
                       char      *msg1, char      *msg2, char *msg3,
                       long   *status1, long   *status2);
*/


/*----------------------- cards_object.c --------------------------*/

typedef struct _CardsStruct CardsStruct;

CardsStruct *cards_create       (char *program);
void         cards_clear        (CardsStruct *cc);
CardsStruct *cards_destroy      (CardsStruct *cc);

long      cards_get_ncards      (CardsStruct *cc);
char     *cards_get_card        (CardsStruct *cc, long icard);
int       cards_replace_card    (CardsStruct *cc, char *card, long icard);
int       cards_add_card        (CardsStruct *cc, char *card);

int       cards_match           (char *card, char *string);
char     *cards_read_card       (FILE *stream);
int       cards_write_card      (FILE *stream, char *card);

int       cards_write_time_card (CardsStruct *cc, FILE *stream);
int       cards_write_all_cards (CardsStruct *cc, FILE *stream);

int cards_set_ivar1 (CardsStruct *cc, char *ident, long   );
int cards_set_fvar1 (CardsStruct *cc, char *ident, float  );
int cards_set_dvar1 (CardsStruct *cc, char *ident, double );
int cards_set_cvar1 (CardsStruct *cc, char *ident, char  *);

int cards_set_ivar2 (CardsStruct *cc, char *ident, long   , long   );
int cards_set_fvar2 (CardsStruct *cc, char *ident, float  , float  );
int cards_set_dvar2 (CardsStruct *cc, char *ident, double , double );
int cards_set_cvar2 (CardsStruct *cc, char *ident, char  *, char  *);

int cards_set_ivar3 (CardsStruct *cc, char *ident, long   , long   , long   );
int cards_set_fvar3 (CardsStruct *cc, char *ident, float  , float  , float  );
int cards_set_dvar3 (CardsStruct *cc, char *ident, double , double , double );
int cards_set_cvar3 (CardsStruct *cc, char *ident, char  *, char  *, char  *);

int cards_get_ivar1 (CardsStruct *cc, char *ident, long  *);
int cards_get_fvar1 (CardsStruct *cc, char *ident, float *);
int cards_get_dvar1 (CardsStruct *cc, char *ident, double*);
int cards_get_cvar1 (CardsStruct *cc, char *ident, char  *);

int cards_get_ivar2 (CardsStruct *cc, char *ident, long  *, long  *);
int cards_get_fvar2 (CardsStruct *cc, char *ident, float *, float *);
int cards_get_dvar2 (CardsStruct *cc, char *ident, double*, double*);
int cards_get_cvar2 (CardsStruct *cc, char *ident, char  *, char  *);

int cards_get_ivar3 (CardsStruct *cc, char *ident, long  *, long  *, long  *);
int cards_get_fvar3 (CardsStruct *cc, char *ident, float *, float *, float *);
int cards_get_dvar3 (CardsStruct *cc, char *ident, double*, double*, double*);
int cards_get_cvar3 (CardsStruct *cc, char *ident, char  *, char  *, char  *);


/*------------------- asgn_code_to_hdr.c -----------------------*/

int asgn_code_to_hdr_spws (const float *a_header, long *num_header_words,
  long *table_size, char **dos,
  char **codes, long *hdr_wrd_1, float *strt_vlu_1, float *end_vlu_1,
  long *hdr_wrd_2, float *strt_vlu_2, float *end_vlu_2, long *hdr_wrd_3,
  float *strt_vlu_3, float *end_vlu_3, long *del_tr, long *kill_tr,
  long *rev_tr, long *flag_tr);


/*--------------------- tredfile_object.c -------------------------*/

typedef struct _TredFile TredFile;

TredFile *tredfile_create  (void);
int       tredfile_get     (const char *filename, TredFile *tf, char *info);
int       tredfile_put     (const char *filename, TredFile *tf, char *info);
int       tredfile_clear   (TredFile *tf);
TredFile *tredfile_destroy (TredFile *tf);


/*--------------------- get_values.c -------------------------*/

int   cprim_getI  (char *buff, char *prompt, int   *value);
int   cprim_getF  (char *buff, char *prompt, float *value);
int   cprim_getStr(char *buff, char *prompt, char **str  );
int   cprim_nextI (char *buff,               int   *value);
int   cprim_nextF (char *buff,               float *value);
char *cprim_prompt(char *buff, char *prompt              );


/*------------ end of sections for each source file ---------------------*/
/*------------ end of sections for each source file ---------------------*/
/*------------ end of sections for each source file ---------------------*/
/*------------ end of sections for each source file ---------------------*/

#ifdef __cplusplus
}                      /* for C++ */
#endif


#endif              /*  _CPRIM_H_  */

/*---------------------------- end --------------------------------------*/
/*---------------------------- end --------------------------------------*/
/*---------------------------- end --------------------------------------*/
/*---------------------------- end --------------------------------------*/
