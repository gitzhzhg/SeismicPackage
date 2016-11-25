/*<CPS_v1 type="HEADER_FILE" PRETAG="!" /> 
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
! SEE wrdc.f90 for documentation, see also wrdc_crou.c
!<brief_doc>
!-------------------------------------------------------------------------------
! Name       : wrdc_crou (.h)
! Category   : io
! Revised    : 2001-03-23   by: Bill Menger
! Maturity   : production   2001-04-26  
! Purpose    : Convert from one word format to another.
! Portability: possibly 4-byte word length only.
!-------------------------------------------------------------------------------
!</brief_doc>
!<history_doc>
!--------------------------------------------------------------------------
!                          REVISION HISTORY                              
!     Date        Author       Description
!     ----        ------       -----------
!  3. 2001-04-26  Bill Menger  Added signed-to-unsigned codes.
!  2. 2000-09-20  R.S. Day     Eliminated static ident variable
!  1. 2000-06-27  Bill Menger  Initial version.(split off of wrdc_crou.c)
!--------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!--------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS                         
!--------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS                      
! The module "wrdc.f90" must be included as part of this module.
!--------------------------------------------------------------------------
!</compile_doc>
!--------------------------"module" start ----------------------------------
*/
#ifndef _WRDC_CROU_H_
#define _WRDC_CROU_H_

#include <stdio.h>
#include "named_constants.h"
#include "c2f_interface.h"

/*
static char wrdc_crou_ident_h[100] = 
"$Id: wrdc_crou.h,v 1.3 2001/04/25 19:21:57 sps prod sps $";
*/

#ifdef __cplusplus
extern "C" {
#endif

#ifdef CRAYMPP
    typedef short fourbyte;
#else
    typedef int fourbyte;
#endif

#ifndef ebcdic_
#define ebcdic_

#define BYTE_SIZE 256
/* EBCDIC values of increasing ASCII values.*/
/* Plug in the ascii to get the ebcdic.     */
static unsigned char ascii_to_ebcdic[BYTE_SIZE] = {
    0,    /* ASCII octal   0 */
    1,    /* ASCII octal   1 */
    2,    /* ASCII octal   2 */
    3,    /* ASCII octal   3 */
   55,    /* ASCII octal   4 */
   45,    /* ASCII octal   5 */
   46,    /* ASCII octal   6 */
   47,    /* ASCII octal   7 */
   22,    /* ASCII octal  10 */
    5,    /* ASCII octal  11 */
   37,    /* ASCII octal  12 */
   11,    /* ASCII octal  13 */
   12,    /* ASCII octal  14 */
   13,    /* ASCII octal  15 */
   14,    /* ASCII octal  16 */
   15,    /* ASCII octal  17 */
   16,    /* ASCII octal  20 */
   17,    /* ASCII octal  21 */
   18,    /* ASCII octal  22 */
   19,    /* ASCII octal  23 */
   60,    /* ASCII octal  24 */
   61,    /* ASCII octal  25 */
   50,    /* ASCII octal  26 */
   38,    /* ASCII octal  27 */
   24,    /* ASCII octal  30 */
   25,    /* ASCII octal  31 */
   63,    /* ASCII octal  32 */
   39,    /* ASCII octal  33 */
   28,    /* ASCII octal  34 */
   29,    /* ASCII octal  35 */
   30,    /* ASCII octal  36 */
   31,    /* ASCII octal  37 */
   64,    /* ASCII octal  40 */
   79,    /* ASCII octal  41 */
  127,    /* ASCII octal  42 */
  123,    /* ASCII octal  43 */
   91,    /* ASCII octal  44 */
  108,    /* ASCII octal  45 */
   80,    /* ASCII octal  46 */
  125,    /* ASCII octal  47 */
   77,    /* ASCII octal  50 */
   93,    /* ASCII octal  51 */
   92,    /* ASCII octal  52 */
   78,    /* ASCII octal  53 */
  107,    /* ASCII octal  54 */
   96,    /* ASCII octal  55 */
   75,    /* ASCII octal  56 */
   97,    /* ASCII octal  57 */
  240,    /* ASCII octal  60 */
  241,    /* ASCII octal  61 */
  242,    /* ASCII octal  62 */
  243,    /* ASCII octal  63 */
  244,    /* ASCII octal  64 */
  245,    /* ASCII octal  65 */
  246,    /* ASCII octal  66 */
  247,    /* ASCII octal  67 */
  248,    /* ASCII octal  70 */
  249,    /* ASCII octal  71 */
  122,    /* ASCII octal  72 */
   94,    /* ASCII octal  73 */
   76,    /* ASCII octal  74 */
  126,    /* ASCII octal  75 */
  110,    /* ASCII octal  76 */
  111,    /* ASCII octal  77 */
  124,    /* ASCII octal 100 */
  193,    /* ASCII octal 101 */
  194,    /* ASCII octal 102 */
  195,    /* ASCII octal 103 */
  196,    /* ASCII octal 104 */
  197,    /* ASCII octal 105 */
  198,    /* ASCII octal 106 */
  199,    /* ASCII octal 107 */
  200,    /* ASCII octal 110 */
  201,    /* ASCII octal 111 */
  209,    /* ASCII octal 112 */
  210,    /* ASCII octal 113 */
  211,    /* ASCII octal 114 */
  212,    /* ASCII octal 115 */
  213,    /* ASCII octal 116 */
  214,    /* ASCII octal 117 */
  215,    /* ASCII octal 120 */
  216,    /* ASCII octal 121 */
  217,    /* ASCII octal 122 */
  226,    /* ASCII octal 123 */
  227,    /* ASCII octal 124 */
  228,    /* ASCII octal 125 */
  229,    /* ASCII octal 126 */
  230,    /* ASCII octal 127 */
  231,    /* ASCII octal 130 */
  232,    /* ASCII octal 131 */
  233,    /* ASCII octal 132 */
  173,    /*cASCII octal 133 */
  224,    /* ASCII octal 134 */
  189,    /*cASCII octal 135 */
   95,    /* ASCII octal 136 */
  109,    /* ASCII octal 137 */
  121,    /* ASCII octal 140 */
  129,    /* ASCII octal 141 */
  130,    /* ASCII octal 142 */
  131,    /* ASCII octal 143 */
  132,    /* ASCII octal 144 */
  133,    /* ASCII octal 145 */
  134,    /* ASCII octal 146 */
  135,    /* ASCII octal 147 */
  136,    /* ASCII octal 150 */
  137,    /* ASCII octal 151 */
  145,    /* ASCII octal 152 */
  146,    /* ASCII octal 153 */
  147,    /* ASCII octal 154 */
  148,    /* ASCII octal 155 */
  149,    /* ASCII octal 156 */
  150,    /* ASCII octal 157 */
  151,    /* ASCII octal 160 */
  152,    /* ASCII octal 161 */
  153,    /* ASCII octal 162 */
  162,    /* ASCII octal 163 */
  163,    /* ASCII octal 164 */
  164,    /* ASCII octal 165 */
  165,    /* ASCII octal 166 */
  166,    /* ASCII octal 167 */
  167,    /* ASCII octal 170 */
  168,    /* ASCII octal 171 */
  169,    /* ASCII octal 172 */
  192,    /* ASCII octal 173 */
  106,    /* ASCII octal 174 */
  208,    /* ASCII octal 175 */
  161,    /* ASCII octal 176 */
    7,    /* ASCII octal 177 */
   32,    /* ASCII octal 200 */
   33,    /* ASCII octal 201 */
   34,    /* ASCII octal 202 */
   35,    /* ASCII octal 203 */
   36,    /* ASCII octal 204 */
   21,    /* ASCII octal 205 */
    6,    /* ASCII octal 206 */
   23,    /* ASCII octal 207 */
   40,    /* ASCII octal 210 */
   41,    /* ASCII octal 211 */
   42,    /* ASCII octal 212 */
   43,    /* ASCII octal 213 */
   44,    /* ASCII octal 214 */
    9,    /* ASCII octal 215 */
   10,    /* ASCII octal 216 */
   27,    /* ASCII octal 217 */
   48,    /* ASCII octal 220 */
   49,    /* ASCII octal 221 */
   26,    /* ASCII octal 222 */
   51,    /* ASCII octal 223 */
   52,    /* ASCII octal 224 */
   53,    /* ASCII octal 225 */
   54,    /* ASCII octal 226 */
    8,    /* ASCII octal 227 */
   56,    /* ASCII octal 230 */
   57,    /* ASCII octal 231 */
   58,    /* ASCII octal 232 */
   59,    /* ASCII octal 233 */
    4,    /* ASCII octal 234 */
   20,    /* ASCII octal 235 */
   62,    /* ASCII octal 236 */
  225,    /* ASCII octal 237 */
   65,    /* ASCII octal 240 */
   66,    /* ASCII octal 241 */
   67,    /* ASCII octal 242 */
   68,    /* ASCII octal 243 */
   69,    /* ASCII octal 244 */
   70,    /* ASCII octal 245 */
   71,    /* ASCII octal 246 */
   72,    /* ASCII octal 247 */
   73,    /* ASCII octal 250 */
   81,    /* ASCII octal 251 */
   82,    /* ASCII octal 252 */
   83,    /* ASCII octal 253 */
   84,    /* ASCII octal 254 */
   85,    /* ASCII octal 255 */
   86,    /* ASCII octal 256 */
   87,    /* ASCII octal 257 */
   88,    /* ASCII octal 260 */
   89,    /* ASCII octal 261 */
   98,    /* ASCII octal 262 */
   99,    /* ASCII octal 263 */
  100,    /* ASCII octal 264 */
  101,    /* ASCII octal 265 */
  102,    /* ASCII octal 266 */
  103,    /* ASCII octal 267 */
  104,    /* ASCII octal 270 */
  105,    /* ASCII octal 271 */
  112,    /* ASCII octal 272 */
  113,    /* ASCII octal 273 */
  114,    /* ASCII octal 274 */
  115,    /* ASCII octal 275 */
  116,    /* ASCII octal 276 */
  117,    /* ASCII octal 277 */
  118,    /* ASCII octal 300 */
  119,    /* ASCII octal 301 */
  120,    /* ASCII octal 302 */
  128,    /* ASCII octal 303 */
  138,    /* ASCII octal 304 */
  139,    /* ASCII octal 305 */
  140,    /* ASCII octal 306 */
  141,    /* ASCII octal 307 */
  142,    /* ASCII octal 310 */
  143,    /* ASCII octal 311 */
  144,    /* ASCII octal 312 */
  154,    /* ASCII octal 313 */
  155,    /* ASCII octal 314 */
  156,    /* ASCII octal 315 */
  157,    /* ASCII octal 316 */
  158,    /* ASCII octal 317 */
  159,    /* ASCII octal 320 */
  160,    /* ASCII octal 321 */
  170,    /* ASCII octal 322 */
  171,    /* ASCII octal 323 */
  172,    /* ASCII octal 324 */
  173,    /* ASCII octal 325 */
  174,    /* ASCII octal 326 */
  175,    /* ASCII octal 327 */
  176,    /* ASCII octal 330 */
  177,    /* ASCII octal 331 */
  178,    /* ASCII octal 332 */
  179,    /* ASCII octal 333 */
  180,    /* ASCII octal 334 */
  181,    /* ASCII octal 335 */
  182,    /* ASCII octal 336 */
  183,    /* ASCII octal 337 */
  184,    /* ASCII octal 340 */
  185,    /* ASCII octal 341 */
  186,    /* ASCII octal 342 */
  187,    /* ASCII octal 343 */
  188,    /* ASCII octal 344 */
  189,    /* ASCII octal 345 */
  190,    /* ASCII octal 346 */
  191,    /* ASCII octal 347 */
  202,    /* ASCII octal 350 */
  203,    /* ASCII octal 351 */
  204,    /* ASCII octal 352 */
  205,    /* ASCII octal 353 */
  206,    /* ASCII octal 354 */
  207,    /* ASCII octal 355 */
  218,    /* ASCII octal 356 */
  219,    /* ASCII octal 357 */
  220,    /* ASCII octal 360 */
  221,    /* ASCII octal 361 */
  222,    /* ASCII octal 362 */
  223,    /* ASCII octal 363 */
  234,    /* ASCII octal 364 */
  235,    /* ASCII octal 365 */
  236,    /* ASCII octal 366 */
  237,    /* ASCII octal 367 */
  238,    /* ASCII octal 370 */
  239,    /* ASCII octal 371 */
  250,    /* ASCII octal 372 */
  251,    /* ASCII octal 373 */
  252,    /* ASCII octal 374 */
  253,    /* ASCII octal 375 */
  254,    /* ASCII octal 376 */
  255,    /* ASCII octal 377 */
};
/* ASCII values sorted in increasing EBCDIC values.*/
/* Plug in the ebcdic to get the ascii.     */
static unsigned char ebcdic_to_ascii[BYTE_SIZE] = {
    0,    /* EBCDIC octal   0 */
    1,    /* EBCDIC octal   1 */
    2,    /* EBCDIC octal   2 */
    3,    /* EBCDIC octal   3 */
  156,    /* EBCDIC octal   4 */
    9,    /* EBCDIC octal   5 */
  134,    /* EBCDIC octal   6 */
  127,    /* EBCDIC octal   7 */
  151,    /* EBCDIC octal  10 */
  141,    /* EBCDIC octal  11 */
  142,    /* EBCDIC octal  12 */
   11,    /* EBCDIC octal  13 */
   12,    /* EBCDIC octal  14 */
   13,    /* EBCDIC octal  15 */
   14,    /* EBCDIC octal  16 */
   15,    /* EBCDIC octal  17 */
   16,    /* EBCDIC octal  20 */
   17,    /* EBCDIC octal  21 */
   18,    /* EBCDIC octal  22 */
   19,    /* EBCDIC octal  23 */
  157,    /* EBCDIC octal  24 */
  133,    /* EBCDIC octal  25 */
    8,    /* EBCDIC octal  26 */
  135,    /* EBCDIC octal  27 */
   24,    /* EBCDIC octal  30 */
   25,    /* EBCDIC octal  31 */
  146,    /* EBCDIC octal  32 */
  143,    /* EBCDIC octal  33 */
   28,    /* EBCDIC octal  34 */
   29,    /* EBCDIC octal  35 */
   30,    /* EBCDIC octal  36 */
   31,    /* EBCDIC octal  37 */
  128,    /* EBCDIC octal  40 */
  129,    /* EBCDIC octal  41 */
  130,    /* EBCDIC octal  42 */
  131,    /* EBCDIC octal  43 */
  132,    /* EBCDIC octal  44 */
   10,    /* EBCDIC octal  45 */
   23,    /* EBCDIC octal  46 */
   27,    /* EBCDIC octal  47 */
  136,    /* EBCDIC octal  50 */
  137,    /* EBCDIC octal  51 */
  138,    /* EBCDIC octal  52 */
  139,    /* EBCDIC octal  53 */
  140,    /* EBCDIC octal  54 */
    5,    /* EBCDIC octal  55 */
    6,    /* EBCDIC octal  56 */
    7,    /* EBCDIC octal  57 */
  144,    /* EBCDIC octal  60 */
  145,    /* EBCDIC octal  61 */
   22,    /* EBCDIC octal  62 */
  147,    /* EBCDIC octal  63 */
  148,    /* EBCDIC octal  64 */
  149,    /* EBCDIC octal  65 */
  150,    /* EBCDIC octal  66 */
    4,    /* EBCDIC octal  67 */
  152,    /* EBCDIC octal  70 */
  153,    /* EBCDIC octal  71 */
  154,    /* EBCDIC octal  72 */
  155,    /* EBCDIC octal  73 */
   20,    /* EBCDIC octal  74 */
   21,    /* EBCDIC octal  75 */
  158,    /* EBCDIC octal  76 */
   26,    /* EBCDIC octal  77 */
   32,    /* EBCDIC octal 100 */
  160,    /* EBCDIC octal 101 */
  161,    /* EBCDIC octal 102 */
  162,    /* EBCDIC octal 103 */
  163,    /* EBCDIC octal 104 */
  164,    /* EBCDIC octal 105 */
  165,    /* EBCDIC octal 106 */
  166,    /* EBCDIC octal 107 */
  167,    /* EBCDIC octal 110 */
  168,    /* EBCDIC octal 111 */
   91,    /* EBCDIC octal 112 */
   46,    /* EBCDIC octal 113 */
   60,    /* EBCDIC octal 114 */
   40,    /* EBCDIC octal 115 */
   43,    /* EBCDIC octal 116 */
   33,    /* EBCDIC octal 117 */
   38,    /* EBCDIC octal 120 */
  169,    /* EBCDIC octal 121 */
  170,    /* EBCDIC octal 122 */
  171,    /* EBCDIC octal 123 */
  172,    /* EBCDIC octal 124 */
  173,    /* EBCDIC octal 125 */
  174,    /* EBCDIC octal 126 */
  175,    /* EBCDIC octal 127 */
  176,    /* EBCDIC octal 130 */
  177,    /* EBCDIC octal 131 */
   93,    /* EBCDIC octal 132 */
   36,    /* EBCDIC octal 133 */
   42,    /* EBCDIC octal 134 */
   41,    /* EBCDIC octal 135 */
   59,    /* EBCDIC octal 136 */
   94,    /* EBCDIC octal 137 */
   45,    /* EBCDIC octal 140 */
   47,    /* EBCDIC octal 141 */
  178,    /* EBCDIC octal 142 */
  179,    /* EBCDIC octal 143 */
  180,    /* EBCDIC octal 144 */
  181,    /* EBCDIC octal 145 */
  182,    /* EBCDIC octal 146 */
  183,    /* EBCDIC octal 147 */
  184,    /* EBCDIC octal 150 */
  185,    /* EBCDIC octal 151 */
  124,    /* EBCDIC octal 152 */
   44,    /* EBCDIC octal 153 */
   37,    /* EBCDIC octal 154 */
   95,    /* EBCDIC octal 155 */
   62,    /* EBCDIC octal 156 */
   63,    /* EBCDIC octal 157 */
  186,    /* EBCDIC octal 160 */
  187,    /* EBCDIC octal 161 */
  188,    /* EBCDIC octal 162 */
  189,    /* EBCDIC octal 163 */
  190,    /* EBCDIC octal 164 */
  191,    /* EBCDIC octal 165 */
  192,    /* EBCDIC octal 166 */
  193,    /* EBCDIC octal 167 */
  194,    /* EBCDIC octal 170 */
   96,    /* EBCDIC octal 171 */
   58,    /* EBCDIC octal 172 */
   35,    /* EBCDIC octal 173 */
   64,    /* EBCDIC octal 174 */
   39,    /* EBCDIC octal 175 */
   61,    /* EBCDIC octal 176 */
   34,    /* EBCDIC octal 177 */
  195,    /* EBCDIC octal 200 */
   97,    /* EBCDIC octal 201 */
   98,    /* EBCDIC octal 202 */
   99,    /* EBCDIC octal 203 */
  100,    /* EBCDIC octal 204 */
  101,    /* EBCDIC octal 205 */
  102,    /* EBCDIC octal 206 */
  103,    /* EBCDIC octal 207 */
  104,    /* EBCDIC octal 210 */
  105,    /* EBCDIC octal 211 */
  196,    /* EBCDIC octal 212 */
  197,    /* EBCDIC octal 213 */
  198,    /* EBCDIC octal 214 */
  199,    /* EBCDIC octal 215 */
  200,    /* EBCDIC octal 216 */
  201,    /* EBCDIC octal 217 */
  202,    /* EBCDIC octal 220 */
  106,    /* EBCDIC octal 221 */
  107,    /* EBCDIC octal 222 */
  108,    /* EBCDIC octal 223 */
  109,    /* EBCDIC octal 224 */
  110,    /* EBCDIC octal 225 */
  111,    /* EBCDIC octal 226 */
  112,    /* EBCDIC octal 227 */
  113,    /* EBCDIC octal 230 */
  114,    /* EBCDIC octal 231 */
  203,    /* EBCDIC octal 232 */
  204,    /* EBCDIC octal 233 */
  205,    /* EBCDIC octal 234 */
  206,    /* EBCDIC octal 235 */
  207,    /* EBCDIC octal 236 */
  208,    /* EBCDIC octal 237 */
  209,    /* EBCDIC octal 240 */
  126,    /* EBCDIC octal 241 */
  115,    /* EBCDIC octal 242 */
  116,    /* EBCDIC octal 243 */
  117,    /* EBCDIC octal 244 */
  118,    /* EBCDIC octal 245 */
  119,    /* EBCDIC octal 246 */
  120,    /* EBCDIC octal 247 */
  121,    /* EBCDIC octal 250 */
  122,    /* EBCDIC octal 251 */
  210,    /* EBCDIC octal 252 */
  211,    /* EBCDIC octal 253 */
  212,    /* EBCDIC octal 254 */
   91,    /*cEBCDIC octal 255 */
  214,    /* EBCDIC octal 256 */
  215,    /* EBCDIC octal 257 */
  216,    /* EBCDIC octal 260 */
  217,    /* EBCDIC octal 261 */
  218,    /* EBCDIC octal 262 */
  219,    /* EBCDIC octal 263 */
  220,    /* EBCDIC octal 264 */
  221,    /* EBCDIC octal 265 */
  222,    /* EBCDIC octal 266 */
  223,    /* EBCDIC octal 267 */
  224,    /* EBCDIC octal 270 */
  225,    /* EBCDIC octal 271 */
  226,    /* EBCDIC octal 272 */
  227,    /* EBCDIC octal 273 */
  228,    /* EBCDIC octal 274 */
   93,    /*cEBCDIC octal 275 */
  230,    /* EBCDIC octal 276 */
  231,    /* EBCDIC octal 277 */
  123,    /* EBCDIC octal 300 */
   65,    /* EBCDIC octal 301 */
   66,    /* EBCDIC octal 302 */
   67,    /* EBCDIC octal 303 */
   68,    /* EBCDIC octal 304 */
   69,    /* EBCDIC octal 305 */
   70,    /* EBCDIC octal 306 */
   71,    /* EBCDIC octal 307 */
   72,    /* EBCDIC octal 310 */
   73,    /* EBCDIC octal 311 */
  232,    /* EBCDIC octal 312 */
  233,    /* EBCDIC octal 313 */
  234,    /* EBCDIC octal 314 */
  235,    /* EBCDIC octal 315 */
  236,    /* EBCDIC octal 316 */
  237,    /* EBCDIC octal 317 */
  125,    /* EBCDIC octal 320 */
   74,    /* EBCDIC octal 321 */
   75,    /* EBCDIC octal 322 */
   76,    /* EBCDIC octal 323 */
   77,    /* EBCDIC octal 324 */
   78,    /* EBCDIC octal 325 */
   79,    /* EBCDIC octal 326 */
   80,    /* EBCDIC octal 327 */
   81,    /* EBCDIC octal 330 */
   82,    /* EBCDIC octal 331 */
  238,    /* EBCDIC octal 332 */
  239,    /* EBCDIC octal 333 */
  240,    /* EBCDIC octal 334 */
  241,    /* EBCDIC octal 335 */
  242,    /* EBCDIC octal 336 */
  243,    /* EBCDIC octal 337 */
   92,    /* EBCDIC octal 340 */
  159,    /* EBCDIC octal 341 */
   83,    /* EBCDIC octal 342 */
   84,    /* EBCDIC octal 343 */
   85,    /* EBCDIC octal 344 */
   86,    /* EBCDIC octal 345 */
   87,    /* EBCDIC octal 346 */
   88,    /* EBCDIC octal 347 */
   89,    /* EBCDIC octal 350 */
   90,    /* EBCDIC octal 351 */
  244,    /* EBCDIC octal 352 */
  245,    /* EBCDIC octal 353 */
  246,    /* EBCDIC octal 354 */
  247,    /* EBCDIC octal 355 */
  248,    /* EBCDIC octal 356 */
  249,    /* EBCDIC octal 357 */
   48,    /* EBCDIC octal 360 */
   49,    /* EBCDIC octal 361 */
   50,    /* EBCDIC octal 362 */
   51,    /* EBCDIC octal 363 */
   52,    /* EBCDIC octal 364 */
   53,    /* EBCDIC octal 365 */
   54,    /* EBCDIC octal 366 */
   55,    /* EBCDIC octal 367 */
   56,    /* EBCDIC octal 370 */
   57,    /* EBCDIC octal 371 */
  250,    /* EBCDIC octal 372 */
  251,    /* EBCDIC octal 373 */
  252,    /* EBCDIC octal 374 */
  253,    /* EBCDIC octal 375 */
  254,    /* EBCDIC octal 376 */
  255,    /* EBCDIC octal 377 */
};

#endif

#ifdef NEED_CAPITALS

#define wrdc_ibm_to_float_c WRDC_IBM_TO_FLOAT_C
#define wrdc_float_to_ibm_c WRDC_FLOAT_TO_IBM_C
#define wrdc_ibm_to_float_sbc WRDC_IBM_TO_FLOAT_SBC
#define wrdc_float_to_ibm_sbc WRDC_FLOAT_TO_IBM_SBC
#define wrdc_asc_ebc_c WRDC_ASC_EBC_C
#define wrdc_ebc_asc_c WRDC_EBC_ASC_C
#define wrdc_asc_ebc_sbc WRDC_ASC_EBC_SBC
#define wrdc_ebc_asc_sbc WRDC_EBC_ASC_SBC
#define wrdc_pack8_4 WRDC_PACK8_4
#define wrdc_unpack4_8 WRDC_UNPACK4_8
#define wrdc_packf_dble WRDC_PACKF_DBLE
#define wrdc_packf_real WRDC_PACKF_REAL
#define wrdc_unpackf_dble WRDC_UNPACKF_DBLE
#define wrdc_unpackf_real WRDC_UNPACKF_REAL
#define wrdc_packi_dble WRDC_PACKI_DBLE
#define wrdc_packi_real WRDC_PACKI_REAL
#define wrdc_packi_short WRDC_PACKI_SHORT
#define wrdc_unpacki_dble WRDC_UNPACKI_DBLE
#define wrdc_unpacki_real WRDC_UNPACKI_REAL
#define wrdc_unpacki_short WRDC_UNPACKI_SHORT
#define wrdc_sbtousb_ca WRDC_SBTOUSB_CA
#define wrdc_usbtosb_ca WRDC_USBTOSB_CA
#define wrdc_sbtousb_ci WRDC_SBTOUSB_CI
#define wrdc_usbtosb_ci WRDC_USBTOSB_CI
#define wrdc_sbtousb_cr WRDC_SBTOUSB_CR
#define wrdc_usbtosb_cr WRDC_USBTOSB_CR

#endif

#ifdef NEED_UNDERSCORE

#define wrdc_ibm_to_float_c wrdc_ibm_to_float_c_
#define wrdc_float_to_ibm_c wrdc_float_to_ibm_c_
#define wrdc_ibm_to_float_sbc wrdc_ibm_to_float_sbc_
#define wrdc_float_to_ibm_sbc wrdc_float_to_ibm_sbc_
#define wrdc_asc_ebc_c wrdc_asc_ebc_c_
#define wrdc_ebc_asc_c wrdc_ebc_asc_c_
#define wrdc_asc_ebc_sbc wrdc_asc_ebc_sbc_
#define wrdc_ebc_asc_sbc wrdc_ebc_asc_sbc_
#define wrdc_pack8_4 wrdc_pack8_4_
#define wrdc_packf_dble wrdc_packf_dble_
#define wrdc_packf_real wrdc_packf_real_
#define wrdc_unpackf_dble wrdc_unpackf_dble_
#define wrdc_unpackf_real wrdc_unpackf_real_
#define wrdc_packi_dble wrdc_packi_dble_
#define wrdc_packi_real wrdc_packi_real_
#define wrdc_packi_short wrdc_packi_short_
#define wrdc_unpacki_dble wrdc_unpacki_dble_
#define wrdc_unpacki_real wrdc_unpacki_real_
#define wrdc_unpacki_short wrdc_unpacki_short_
#define wrdc_sbtousb_ca wrdc_sbtousb_ca_
#define wrdc_usbtosb_ca wrdc_usbtosb_ca_
#define wrdc_sbtousb_ci wrdc_sbtousb_ci_
#define wrdc_usbtosb_ci wrdc_usbtosb_ci_
#define wrdc_sbtousb_cr wrdc_sbtousb_cr_
#define wrdc_usbtosb_cr wrdc_usbtosb_cr_

#endif

/* function prototypes */

void wrdc_float_to_ibm_sbc(fourbyte *from, fourbyte *to, int *n, int *endian);
void wrdc_ibm_to_float_sbc(fourbyte *from, fourbyte *to, int *n, int *endian);
int wrdc_packf_dble    (void * vec, int * len, int * from, int * to);
int  wrdc_packf_real   (void * vec, int * len, int * from, int * to);
int  wrdc_unpackf_dble  (void * vec, int * len, int * from, int * to);
int  wrdc_unpackf_real  (void * vec, int * len, int * from, int * to);
int  wrdc_packi_dble (void * vec, int * len, int * from, int * to);
int  wrdc_packi_real (void * vec, int * len, int * from, int * to);
int  wrdc_packi_short (void * vec, int * len, int * from, int * to);
int  wrdc_unpacki_dble  (void * vec, int * len, int * from, int * to);
int  wrdc_unpacki_real  (void * vec, int * len, int * from, int * to);
int  wrdc_unpacki_short  (void * vec, int * len, int * from, int * to);
void wrdc_float_to_ibm_c(fourbyte *from, int *n, int *endian);
void wrdc_float_to_ibm_sbc(fourbyte *from, fourbyte *to, int *n, int *endian);
void wrdc_ibm_to_float_c(fourbyte *from, int *n, int *endian);
void wrdc_ibm_to_float_sbc(fourbyte *from, fourbyte *to, int *n, int *endian);
void wrdc_ebc_asc_sbc(int *nchars, int *off,unsigned  char *eb,
                      unsigned  char *as);
void wrdc_asc_to_ebc_sbc(int *nchars,unsigned  char *as,unsigned  char *eb);
void wrdc_asc_ebc_c(unsigned char *io, int *nchars);
void wrdc_ebc_asc_c(unsigned char *io, int *nchars);
void wrdc_sbtousb_c(int *n, char *sb, unsigned char *usb);
void wrdc_usbtosb_c(int *n, unsigned char *usb, char *sb);

#ifdef __cplusplus
}
#endif

#endif
