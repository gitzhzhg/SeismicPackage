/*<CPS_v1 type="HEADER_FILE",pretag="!"/>
! other files are:  segy.f90  segy_crou.c 
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
!                        C P S   P R I M I T I V E        
!
! Name       : segy_crou.h
! Category   : io
! Written    : 1999-12-09   by: Bill Menger
! Revised    : 2004-08-23   by: Randy Selzler
! Maturity   : production
! Purpose    : primitive to read/write segy file headers and to convert segy
!              trace headers to/from cps header format. 
!</brief_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 10. 2004-08-23  R. Selzler   Improved segy recognition of format variations.
!  9. 2003-08-18  Bill Done    Insert items into tapehdr array to represent
!                              the 14 unused 4-byte words in the segy tape
!                              header (bytes 181-236).
!  8. 2001-12-10  Ed Schmauch  Removed all Conoco extensions to segy format.
!                              Removed unnecessary structure definitions.
!  7. 2000-11-17  Bill Menger  Moved includes into here from .c file.
!  6. 2000-11-09  R.S.Day      Added prototypes segy_map_cps_to_segy and
!                              segy_map_segy_to_cps
!  5. 2000-09-21  Bill Menger  Modified documentation, commented out unused
!                              code,removed ident string,added tabplot routine.
!  4. 2000-01-21  Bill Menger  Fleshed out the conoco specifics in the tapesegy.
!  3. 2000-01-18  Bill Menger  Added nbits to binary header, added 
!                              gain_for_recovery in trace header (word 59)
!  2. 1999-12-14  Bill Menger  Modified 64bit fields to 8-chars from longlong.
!  1. 1999-12-13  Bill Menger  Initial version modified from cwp.
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!--------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
! Portions of this code depend upon the ASCII collating sequence and/or ASCII
! variable definitions as defined in ANSI C Lexical conventions, EBCDIC 
! collating sequence, and the SEG-Y standard.
!--------------------------------------------------------------------------
!</portability_doc>
*/

#ifndef _SEGY_CROU_H_
#define _SEGY_CROU_H_

/*"$Id: segy_crou.h,v 1.10 2004/08/23 13:16:02 Selzler prod sps $"*/

#include "swap.h"
#include "c2f_interface.h"
#include "wrdc_crou.h"
#include <math.h>
#include <stdio.h>

#ifdef NEED_CAPITALS

#define segy_is_file_segy_c SEGY_IS_FILE_SEGY_C

#define segy_map_segy_to_cps_c SEGY_MAP_SEGY_TO_CPS_C
#define segy_map_cps_to_segy_c SEGY_MAP_CPS_TO_SEGY_C
#define segy_gettapebhval_b SEGY_GETTAPEBHVAL_B
#define segy_gettapebhval_u SEGY_GETTAPEBHVAL_U
#define segy_gettapebhval_p SEGY_GETTAPEBHVAL_P
#define segy_gettapebhval_f SEGY_GETTAPEBHVAL_F
#define segy_gettapebhval_d SEGY_GETTAPEBHVAL_D
#define segy_gettapebhval_c SEGY_GETTAPEBHVAL_C

#define segy_puttapebhval_b SEGY_PUTTAPEBHVAL_B
#define segy_puttapebhval_u SEGY_PUTTAPEBHVAL_U
#define segy_puttapebhval_p SEGY_PUTTAPEBHVAL_P
#define segy_puttapebhval_f SEGY_PUTTAPEBHVAL_F
#define segy_puttapebhval_d SEGY_PUTTAPEBHVAL_D
#define segy_puttapebhval_c SEGY_PUTTAPEBHVAL_C

#define segy_gettapehval_u SEGY_GETTAPEHVAL_U
#define segy_gettapehval_p SEGY_GETTAPEHVAL_P
#define segy_gettapehval_f SEGY_GETTAPEHVAL_F
#define segy_gettapehval_d SEGY_GETTAPEHVAL_D
#define segy_gettapehval_c SEGY_GETTAPEHVAL_C

#define segy_puttapehval_u SEGY_PUTTAPEHVAL_U
#define segy_puttapehval_p SEGY_PUTTAPEHVAL_P
#define segy_puttapehval_f SEGY_PUTTAPEHVAL_F
#define segy_puttapehval_d SEGY_PUTTAPEHVAL_D
#define segy_puttapehval_c SEGY_PUTTAPEHVAL_C

#define segy_swaptapehval SEGY_SWAPTAPEHVAL
#define segy_swaptapebhval SEGY_SWAPTAPEBHVAL
#define segy_tabplot SEGY_TABPLOT

#elif defined NEED_UNDERSCORE

#define segy_is_file_segy_c segy_is_file_segy_c_

#define segy_map_segy_to_cps_c segy_map_segy_to_cps_c_
#define segy_map_cps_to_segy_c segy_map_cps_to_segy_c_
#define segy_gettapebhval_b segy_gettapebhval_b_
#define segy_gettapebhval_u segy_gettapebhval_u_
#define segy_gettapebhval_p segy_gettapebhval_p_
#define segy_gettapebhval_f segy_gettapebhval_f_
#define segy_gettapebhval_d segy_gettapebhval_d_
#define segy_gettapebhval_c segy_gettapebhval_c_

#define segy_puttapebhval_b segy_puttapebhval_b_
#define segy_puttapebhval_u segy_puttapebhval_u_
#define segy_puttapebhval_p segy_puttapebhval_p_
#define segy_puttapebhval_f segy_puttapebhval_f_
#define segy_puttapebhval_d segy_puttapebhval_d_
#define segy_puttapebhval_c segy_puttapebhval_c_

#define segy_gettapehval_u segy_gettapehval_u_
#define segy_gettapehval_p segy_gettapehval_p_
#define segy_gettapehval_f segy_gettapehval_f_
#define segy_gettapehval_d segy_gettapehval_d_
#define segy_gettapehval_c segy_gettapehval_c_

#define segy_puttapehval_u segy_puttapehval_u_
#define segy_puttapehval_p segy_puttapehval_p_
#define segy_puttapehval_f segy_puttapehval_f_
#define segy_puttapehval_d segy_puttapehval_d_
#define segy_puttapehval_c segy_puttapehval_c_

#define segy_swaptapehval segy_swaptapehval_
#define segy_swaptapebhval segy_swaptapebhval_
#define segy_tabplot segy_tabplot_

#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Definitions */
#define SEGY_NKEYS  79
#define SEGY_NBKEYS 36
#define max(A,B) ((A) > (B) ? (A) : (B) )
#define SCREENFUL    19
#define PLOTWIDTH    29

static char *str[] = {
    "-----------------------------|",
    " ----------------------------|",
    "  ---------------------------|",
    "   --------------------------|",
    "    -------------------------|",
    "     ------------------------|",
    "      -----------------------|",
    "       ----------------------|",
    "        ---------------------|",
    "         --------------------|",
    "          -------------------|",
    "           ------------------|",
    "            -----------------|",
    "             ----------------|",
    "              ---------------|",
    "               --------------|",
    "                -------------|",
    "                 ------------|",
    "                  -----------|",
    "                   ----------|",
    "                    ---------|",
    "                     --------|",
    "                      -------|",
    "                       ------|",
    "                        -----|",
    "                         ----|",
    "                          ---|",
    "                           --|",
    "                            -|",
          "                             *",
          "                             |+",
          "                             |++",
          "                             |+++",
          "                             |++++",
          "                             |+++++",
          "                             |++++++",
          "                             |+++++++",
          "                             |++++++++",
          "                             |+++++++++",
          "                             |++++++++++",
          "                             |+++++++++++",
          "                             |++++++++++++",
          "                             |+++++++++++++",
          "                             |++++++++++++++",
          "                             |+++++++++++++++",
          "                             |++++++++++++++++",
          "                             |+++++++++++++++++",
          "                             |++++++++++++++++++",
          "                             |+++++++++++++++++++",
          "                             |++++++++++++++++++++",
          "                             |+++++++++++++++++++++",
          "                             |++++++++++++++++++++++",
          "                             |+++++++++++++++++++++++",
          "                             |++++++++++++++++++++++++",
          "                             |+++++++++++++++++++++++++",
          "                             |++++++++++++++++++++++++++",
          "                             |+++++++++++++++++++++++++++",
          "                             |++++++++++++++++++++++++++++",
          "                             |+++++++++++++++++++++++++++++",
};


/* TYPEDEFS */

typedef union { /* storage for arbitrary type */
    char               s[8];
    short              h;
    unsigned short     u;
    long               l;
    unsigned long      v;
    int                i;
    unsigned int       p;
    float              f;
    double             d;
    char               C[8];
    unsigned int       U:16;
    unsigned int       P:32;
    unsigned int       F:32;
    unsigned char      D[8];
} Value;

/* Copyright (c) Colorado School of Mines, 1998.*/
/* All rights reserved.                       */

/* tapesegy.h - include file for SEGY traces as bytes (only for segyread,write)
 *
 * declarations for:
 *      typedef struct {} segytape - the trace identification header
 *          --- eliminated 05nov01 ehs
 *      typedef struct {} bhedtape - binary header
 *          --- eliminated 05nov01 ehs
 *      static struct tapehdr [] - type and offsets for trace header
 *      static struct tapebhdr[] - type and offsets for binary header
 *
 * Reference:
 *      K. M. Barry, D. A. Cavers and C. W. Kneale, "Special Report:
 *              Recommended Standards for Digital Tape Formats",
 *              Geophysics, vol. 40, no. 2 (April 1975), P. 344-352.
 *      
 */ 

/* Copyright (c) Colorado School of Mines, 1998.*/
/* All rights reserved.                       */

/* tapehdr.h - include file for SEGY traces as bytes (only for segyread,write)
 *
 * Reference:
 *      K. M. Barry, D. A. Cavers and C. W. Kneale, "Special Report:
 *              Recommended Standards for Digital Tape Formats",
 *              Geophysics, vol. 40, no. 2 (April 1975), P. 344-352.
 *      
 */ 

struct _tapehdr
{
  char *key;
  char *type;
  int offs;
};

static struct _tapehdr tapehdr[] = {
           {"tracl",             "P",            0},
           {"tracr",             "P",            4},
            {"fldr",             "P",            8},
           {"tracf",             "P",            12},
              {"ep",             "P",            16},
             {"cdp",             "P",            20},
            {"cdpt",             "P",            24},
            {"trid",             "U",            28},
             {"nvs",             "U",            30},
             {"nhs",             "U",            32},
            {"duse",             "U",            34},
          {"offset",             "P",            36},
           {"gelev",             "P",            40},
           {"selev",             "P",            44},
          {"sdepth",             "P",            48},
            {"gdel",             "P",            52},
            {"sdel",             "P",            56},
           {"swdep",             "P",            60},
           {"gwdep",             "P",            64},
          {"scalel",             "U",            68},
          {"scalco",             "U",            70},
              {"sx",             "P",            72},
              {"sy",             "P",            76},
              {"gx",             "P",            80},
              {"gy",             "P",            84},
          {"counit",             "U",            88},
           {"wevel",             "U",            90},
          {"swevel",             "U",            92},
             {"sut",             "U",            94},
             {"gut",             "U",            96},
           {"sstat",             "U",            98},
           {"gstat",             "U",            100},
           {"tstat",             "U",            102},
            {"laga",             "U",            104},
            {"lagb",             "U",            106},
           {"delrt",             "U",            108},
            {"muts",             "U",            110},
            {"mute",             "U",            112},
              {"ns",             "U",            114},
              {"dt",             "U",            116},
            {"gain",             "U",            118},
             {"igc",             "U",            120},
             {"igi",             "U",            122},
            {"corr",             "U",            124},
             {"sfs",             "U",            126},
             {"sfe",             "U",            128},
            {"slen",             "U",            130},
            {"styp",             "U",            132},
            {"stas",             "U",            134},
            {"stae",             "U",            136},
           {"tatyp",             "U",            138},
           {"afilf",             "U",            140},
           {"afils",             "U",            142},
          {"nofilf",             "U",            144},
          {"nofils",             "U",            146},
             {"lcf",             "U",            148},
             {"hcf",             "U",            150},
             {"lcs",             "U",            152},
             {"hcs",             "U",            154},
            {"year",             "U",            156},
             {"day",             "U",            158},
            {"hour",             "U",            160},
          {"minute",             "U",            162},
             {"sec",             "U",            164},
          {"timbas",             "U",            166},
            {"trwf",             "U",            168},
          {"grnors",             "U",            170},
          {"grnofr",             "U",            172},
          {"grnlof",             "U",            174},
            {"gaps",             "U",            176},
           {"otrav",             "U",            178},

/*
 * Introduce array of 4 byte integers for bytes 181-236
 *   wjd: 2003/8/11
 */
        { "unusedA",             "P",            180},
        { "unusedB",             "P",            184},
        { "unusedC",             "P",            188},
        { "unusedD",             "P",            192},
        { "unusedE",             "P",            196},
        { "unusedF",             "P",            200},
        { "unusedG",             "P",            204},
        { "unusedH",             "P",            208},
        { "unusedI",             "P",            212},
        { "unusedJ",             "P",            216},
        { "unusedK",             "P",            220},
        { "unusedL",             "P",            224},
        { "unusedM",             "P",            228},
        { "unusedN",             "P",            232},

/*
 * No more conoco segy format.
 * ehs 02nov01
 *      {   "conid",             "C",             180},
 *      {     "lav",             "F",             188},
 *      { "mp_xloc",             "D",             192},
 *      { "mp_yloc",             "D",             200},
 *      { "mp_elev",             "D",             208},
 *      {"mp_xgrid",             "D",             216},
 *      {"mp_ygrid",             "D",             224},
 *      { "gain_for_recovery",   "F",             232},
 */

        { "unused1",             "P",             236}
};

static int tapehdr_size = (int) (sizeof(tapehdr) / sizeof(struct _tapehdr));

struct _tapebhdr
{
  char *key;
  char *type;
  int offs;
};

static struct _tapebhdr tapebhdr[] = {
           {"jobid",             "P",            0},
           {"lino",              "P",            4},
           {"reno",              "P",            8},
           {"ntrpr",             "U",            12},
           {"nart",              "U",            14},
           {"hdt",               "U",            16},
           {"dto",               "U",            18},
           {"hns",               "U",            20},
           {"nso",               "U",            22},
           {"format",            "U",            24},
           {"fold",              "U",            26},
           {"tsort",             "U",            28},
           {"vscode",            "U",            30},
           {"hsfs",              "U",            32},
           {"hsfe",              "U",            34},
           {"hslen",             "U",            36},
           {"hstyp",             "U",            38},
           {"schn",              "U",            40},
           {"hstas",             "U",            42},
           {"hstae",             "U",            44},
           {"htatyp",            "U",            46},
           {"hcorr",             "U",            48},
           {"bgrcv",             "U",            50},
           {"rcvm",              "U",            52},
           {"mfeet",             "U",            54},
           {"polyt",             "U",            56},
           {"vpol",              "U",            58}

/*
 * No more conoco segy format.
 * ehs 02nov01
 *         {"conid",             "C",            60},
 *         {"tstart",            "F",            68},
 *         {"xorigin",           "D",            72},
 *         {"yorigin",           "D",            80},
 *         {"dx11",              "D",            88},
 *         {"dx12",              "D",            96},
 *         {"dx21",              "D",           104},
 *         {"dx22",              "D",           112},
 *         {"endian",            "P",           120},
 *         {"nbits",             "P",           124}
 */
};

static int tapebhdr_size = (int) (sizeof(tapebhdr) / sizeof(struct _tapebhdr));

/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/
/*------------------------ start of prototypes ---------------------------*/

/*
 * I don't see these functions.
 * ehs 02nov01
 * void segy_swaphval(segyhdr *tr, int *index);
 * void segy_swapbhval(bhed *bh, int *index);
 */

void segy_gettapehval(void *tapetr, int *index, Value *valp); 
void segy_puttapehval(void *tapetr, int *index, Value *valp); 
void segy_gettapebhval(void *tapetr, int *index, Value *valp); 
void segy_puttapebhval(void *tapetr, int *index, Value *valp); 

void segy_tabplot(float *tp, int *itmin, int *itmax);
void segy_map_segy_to_cps_c(char *segy, int *nummap, int *nbytes,
     int *sbyte, int *cpsmap, int *mtype, DOUBLE *cpshd);
void segy_map_cps_to_segy_c(char *segy, int *nummap, int *nbytes,
     int *sbyte, int *cpsmap, int *mtype, DOUBLE *cpshd);

/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/
/*------------------------- end of prototypes ---------------------------*/


#ifdef __cplusplus
}
#endif


#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/


