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
/*<CPS_v1 type="HEADER_FILE",pretag="!"/>
! other files are:  hdfmod.f90  hdfmod_crou.c 
!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E        
!
! Name       : hdfmod_crou.h
! Category   : io
! Written    : 2004-05-03   by: Bill Done
! Revised    : 2004-05-03   by: Bill Done
! Maturity   : beta
! Purpose    : Functions to read and write hdf model files
!</brief_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2004-05-03  Bill Done    Initial version.
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!--------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! None.
!
!--------------------------------------------------------------------------
!</portability_doc>
*/
/*
 * $Id: hdfmod_crou.h,v 1.3 2004/08/03 13:03:34 spws Exp $
 * $Name:  $
 */

#ifndef _HDFMOD_CROU_H
#define _HDFMOD_CROU_H

#include "c2f_interface.h"

#if NEED_UNDERSCORE
/*
 * defines for C functions callable from fortran, using fortran name that is
 * lower case with trailing underscore
 */
#define hdfmod_write_info_def hdfmod_write_info_
#define hdfmod_qksort1_def    hdfmod_qksort1_
#define hdfmod_qksort2_def    hdfmod_qksort2_
#elif NEED_CAPITALS
/*
 * defines for C functions callable from fortran, normally using fortran name
 * that is upper case with no trailing underscore. but in this library, the
 * absoft compilers are being forced to create fortran subroutine names
 * using lower case with a following underscore.
 */
#define hdfmod_write_info_def hdfmod_write_info_
#define hdfmod_qksort1_def    hdfmod_qksort1_
#define hdfmod_qksort2_def    hdfmod_qksort2_
#endif

#define UNKNOWN_SPHEROID   0
#define UNKNOWN_PROJECTION 0
#define UNKNOWN_STATEPLANE 0
#define DEFAULT_NULLCOORD 1e20
#define VSET_INTERFACE

/*
 * the following unit entities came from unit_name.h on directory dataBldr
 */
/* depunit_code: Depth/Vertical Unit Types
   logunit_code: Log Sample Unit Types 
*/
#ifndef UNIT_TYPES
#    define METERS            0
#    define FEET              1
#    define MSEC              2
#    define INCHES            3
#    define CYCLES_FOOT       4
#    define CYCLES_METER      5
#    define CYCLES_MSEC       6
#    define uSEC_PER_FOOT     7
#    define uSEC_PER_METER    8
#    define API_UNITS         9
#    define OHM_M            10
#    define MMHO_M           11
#    define MV               12
#    define PERCENT          13 
#    define G_PER_CC         14
#    define DEGF             15
#    define DEGC             16
#    define UNKNOWN_UNIT     20
#    define MAX_UNIT_TYPES   21
#endif

/*
 * Array of unit names 
 */
static char unit_name[MAX_UNIT_TYPES][15] = {
    "METERS", "FEET", "MSEC", "INCHES",
    "CYCLES/M", "CYCLES/FT", "CYCLES/MS",
    "uSEC/F", "uSEC/M",
    "API UNITS", "OHM-M", "MMHO-M", "MV",
    "%", "G/CC", "", "", "", "", "",
    "UNKNOWN_UNIT"
};

/*
 * the following came from geodeticINFO.h on directory dataBldr and was modified
 */
typedef struct {
    double    gi_xporig,gi_yporig;    /* Projected x,y of AOI (Area Of Interest) */
    double    gi_xrange,gi_yrange;    /* Width, Height of AOI */
    float     gi_leflon,gi_botlat;    /* Lower left corner  of AOI */
    float     gi_riglon,gi_toplat;    /* Upper right corner of AOI */
    float     gi_datum;               /* Reference Datum - from Sea Level */
    int       gi_iproj;               /* Projection Code - 1-11, 101-5010 */
    int       gi_stateplane;          /* State Plane 101-5010 */
    int       gi_isph;                /* Spheroid 1-24 */
    int       gi_units;               /* 1=feet, 0=meters */
    float     gi_sclfac;              /* Scale actor */
    float     gi_lonc;                /* Central Meridian */
    float     gi_lato;                /* Base (Origin) Latitude */
    float     gi_lats,gi_latn;        /* Southern,Northern standard latitudes */
    float     gi_falseN,gi_falseE;    /* False northing, False easting */
} hdfmod_geodetic_info;

/*
 * the following came from modelINFO.h on directory dataBldr and was modified
 */
typedef struct {
    char                 *mi_filename;     /* File containing model info */
    char                 *mi_descrip;      /* Text description of model*/
    float                 mi_rotang;       /* Rotation Angle of model */
    int                   mi_dimsflag;     /* Dimension of Model Data */
    int                   mi_iunits;       /* Model internal coordinate units */
    int                   mi_nregion;      /* Number of region VGROUPS */
    hdfmod_geodetic_info *mi_geodetic;     /* Model geodetic info struct. ptr */
    char                **mi_region_names; /* Region Names */
} hdfmod_model_info;

/* the following came from ginfoutil.c and was modified */
#define MAXGEODETICS	25
static int ngeoptr=0;
struct geoptrLIST_structure
{	int gref;
	hdfmod_geodetic_info *geoptr;
}geoptrLIST[MAXGEODETICS];

/*
 * the following came from srth.c on .../grid2tetra/sol28g32.
 * it is used in the quick sort routines.
 */
static volatile int sorting;  /* Flag to indicate all processes to return */
struct qs_t { /* Interprocess communications */
	int left;  /* Left index (start of sort region) */
	int right; /* right index (end of sort region) */
	int inuse; /* Flag indicating processor busy */
	int spin;  /* DEBUG accumulated spin time */
	int entrycnt; /* Indicator of recursive calls */
};


/* function prototypes */
#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

char *hdfmod_fstr2cstr(char *strin, int nchar);
char *hdfmod_strnmake(char *strin, int nchar);
char *hdfmod_strip_right(char *strin);
int32 hdfmod_write_model_info(char *filename, hdfmod_model_info *minfo);
int32 hdfmod_write_vset_minfo(HFILEID hdfp, hdfmod_model_info *minfo, 
                              char *vgname, char *vgclass, char *vsclass);
int32 hdfmod_write_vset_ginfo(HFILEID hdfp, hdfmod_geodetic_info *ginfo,
                              char *vgname, char *vgclass, char *vsclass);


/*--------------------------- sort routines  ------------------------------*/

static void hdfmod_qksort1(int set[], int *sndx, int nprocs,
                           volatile struct qs_t *call);
static void hdfmod_qksort2(int set[][3], int *sndx, int nprocs, 
                           volatile struct qs_t *call);


/*--------------------------- vset utilities ------------------------------*/

char  hdfmod_getVSET_char(HFILEID hdfp, int32 pgref, char *field,
                          char *vsname, char *vsclass, char nullval);
char *hdfmod_getVSET_string(HFILEID hdfp, int32 pgref, char *field,
                            char *vsname, char *vsclass);
char **hdfmod_getVSET_string_array(HFILEID hdfp, int32 pgref, char *field,
                                   char *vsname, char *vsclass, int ncnt);
float32 hdfmod_getVSET_float32(HFILEID hdfp, int32 pgref, char *field,
                               char *vsname, char *vsclass, float32 nullval);
float32 *hdfmod_getVSET_float32_array(HFILEID hdfp, int32 pgref, char *field,
                                      char *vsname, char *vsclass, int ncnt);
float64 hdfmod_getVSET_float64(HFILEID hdfp, int32 pgref, char *field,
                               char *vsname, char *vsclass, float64 nullval);
float64 *hdfmod_getVSET_float64_array(HFILEID hdfp, int32 pgref, char *field,
                                      char *vsname, char *vsclass, int ncnt);
int32 hdfmod_getVSET_int32(HFILEID hdfp, int32 pgref, char *field,
                           char *vsname, char *vsclass, int32 nullval);
int32 *hdfmod_getVSET_int32_array(HFILEID hdfp, int32 pgref, char *field,
                                  char *vsname, char *vsclass, int ncnt);
int8 hdfmod_getVSET_int8(HFILEID hdfp, int32 pgref, char *field,
                         char *vsname, char *vsclass, int8 nullval);
int8 *hdfmod_getVSET_int8_array(HFILEID hdfp, int32 pgref, char *field,
                                char *vsname, char *vsclass, int ncnt);
int32 hdfmod_putVSET_char(HFILEID hdfp, char *field, char *vsname,
                          char *vsclass, char charval, char nullval);
int32 hdfmod_putVSET_string(HFILEID hdfp, char *field, char *vsname,
                            char *vsclass,char *strval);
int32 hdfmod_putVSET_string_array(HFILEID hdfp, char *field, char *vsname,
                                  char *vsclass, int ncnt, char **strarr);
int32 hdfmod_putVSET_float32(HFILEID hdfp, char *field, char *vsname,
                             char *vsclass, float32 dval, float32 nullval);
int32 hdfmod_putVSET_float32_array(HFILEID hdfp, char *field, char *vsname,
                                   char *vsclass, int ncnt, float32 *fltarr);
int32 hdfmod_putVSET_float64(HFILEID hdfp, char *field, char *vsname,
                             char *vsclass, float64 dval, float64 nullval);
int32 hdfmod_putVSET_float64_array(HFILEID hdfp, char *field, char *vsname,
                                   char *vsclass, int ncnt, float64 *fltarr);
int32 hdfmod_putVSET_int32(HFILEID hdfp, char *field, char *vsname,
                           char *vsclass, int32 intval, int32 nullval);
int32 hdfmod_putVSET_int32_array(HFILEID hdfp, char *field, char *vsname,
                                 char *vsclass, int ncnt, int32 *intarr);
int32 hdfmod_putVSET_int8(HFILEID hdfp, char *field, char *vsname,
                          char *vsclass, int8 intval, int8 nullval);
int32 hdfmod_putVSET_int8_array(HFILEID hdfp, char *field, char *vsname,
                                char *vsclass, int ncnt, int8 *intarr);


/*------------------------- vhiread utilities -----------------------------*/

int32 hdfmod_VHgetgroup(HFILEID fn, int32 tagarray[], int32 refarray[],
                        int32 *ncnt, char *vgname, char *vgclass);
int32 hdfmod_VHgetdata(HFILEID fn, int32 vgref, char *field, char buf[], 
                       int32 *ncnt, int32 *datatype, char *vsname, 
                       char *vsclass);
int32 hdfmod_VHgetdatam(HFILEID fn, int32 vgref, char *field, char buf[],
                        int32 *ncnt, int32 *datatype, char *vsname,
                        char *vsclass, int32 *order);
int32 hdfmod_VHgetgroupByName(HFILEID fn, int32 vgref, int32 tagarray[],
                              int32 refarray[], int32 *ncnt, char  *vgname,
                              char *vgclass);
int32 hdfmod_VHgetsubgroupByName(HFILEID fn, int32 pvgref, int32 vgref,
                                 int32 tagarray[], int32 refarray[], 
                                 int32 *ncnt, char *vgname, char *vgclass);
int32 hdfmod_VHgetgroupByID(HFILEID fn, int32 vgref, int32 tagarray[],
                            int32 refarray[], int32 *ncnt, char *vgname,
                            char *vgclass);
int32 hdfmod_VHgetdataByName(HFILEID fn, int32 vgref, int32 vsid,
                             char *field, char buf[], int32 *ncnt,
                             int32 *datatype, char *vsname, char *vsclass);
int32 hdfmod_VHgetdataByID(HFILEID fn, int32 vsid, char *field, char buf[], 
                           int32 *ncnt, int32 *datatype, char *vsname,
                           char *vsclass);
int32 hdfmod_VHgetdatamByName(HFILEID fn, int32 vgref, int32 vsid, 
                              char *field, char buf[], int32 *ncnt, 
                              int32 *datatype, char *vsname, char *vsclass, 
                              int32 *order);
int32 hdfmod_VHgetdatamByID(HFILEID fn, int32 vsid, char *field, char buf[], 
                            int32 *ncnt, int32 *datatype, char *vsname,
                            char *vsclass, int32 *order);
int32 hdfmod_VHgetgroupnames(HFILEID fn, int32 pgref, int32 tagarray[],
                             int32 refarray[], int32 *ncnt, char **vgname,
                             char *vgclass);
int32 hdfmod_VHgetgroupclasses(HFILEID fn, int32 pgref, int32 tagarray[],
                               int32 refarray[], int32 *ncnt, char *vgname,
                               char **vgclass);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
