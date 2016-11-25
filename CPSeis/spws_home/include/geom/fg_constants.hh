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

//------------------------ fg_constants.hh ---------------------//
//------------------------ fg_constants.hh ---------------------//
//------------------------ fg_constants.hh ---------------------//

//                    subdirectory geom

//  This header file contains constants which are needed by several
//  classes related to field geometry.  The constants are isolated
//  here, rather than being part of a class, in order to minimize
//  the necessity to include class header files when the only
//  requirement would be to gain access to these constants.

//  This header file is included in field_geometry.hh for convenience.

//  Most of these constants are normally referred to as "idents".

//  The "ident" constants are convenient for accessing variables in
//  the FieldFlag objects through functions common to all variables,
//  which is convenient when used with virtual functions in
//  classes derived from ArrayBase.

//  The "ident" constants are also needed for sending messages to
//  data users.

//  This header file is incomplete.  Additional constants will be
//  added as needed.


#ifndef _FG_CONSTANTS_HH_
#define _FG_CONSTANTS_HH_


//------------------- chaining constants --------------------------//
//------------------- chaining constants --------------------------//
//------------------- chaining constants --------------------------//

enum { HORIZONTAL_CHAINING = 1, SLOPE_CHAINING, NO_CHAINING };


//------------------- select constants --------------------------//
//------------------- select constants --------------------------//
//------------------- select constants --------------------------//

enum { SELECT_MAYBE  = ' ',  // unspecified selection.
       SELECT_YES    = 'Y',  // yes definitely selected.
       SELECT_NO     = 'n',  // no definitely not selected.
       SELECT_TOP    = 'T',  // top of selected range.
       SELECT_BOTTOM = 'B'   // bottom of selected range.
     };


//------------------- lock constants --------------------------//
//------------------- lock constants --------------------------//
//------------------- lock constants --------------------------//

    // These constants, set and referenced by setDataLock and
    // getDataLock, are provided to allow the user to protect
    // himself from accidentally making changes when she/he
    // prefers not to.  Any data changes attempted while those
    // changes are locked will simply send the ringBell message
    // to data users.  (Changing an "active" item, or selecting
    // and unselecting items, will always be allowed.)

    // This data lock is set appropriately when source, receiver,
    // or CMP gathers are calculated (sometimes a lengthy process),
    // to protect the user from accidentally changing something which
    // would render the calculations out-of-date.  The user can
    // reset this flag at any time.

    // The higher the value of the locking constant, the more
    // restrictive the lock is.  Therefore, to test whether
    // a particular level of locking is present, one can test
    // the value returned by getDataLock using inequalities
    // as well as equalities.  For example:
    //
    // (1) The user is permitted to delete data if
    //             getDataLock() < LOCK_DEL,
    //       but not permitted to delete data if
    //             getDataLock() >= LOCK_DEL.
    //
    // (2) A new JD file can be read in if
    //             getDataLock() <= LOCK_DEL,
    //       but cannot be read in if
    //             getDataLock() > LOCK_DEL.
    //
    // (3) Receiver pattern (RP) cards can be deleted if
    //             getDataLock() < LOCK_DEL,
    //       or added or modified if
    //             getDataLock() < LOCK_S_R,
    //       but not touched otherwise.


enum { LOCK_NONE    = 0,  // allow changing any data.
                          //
       LOCK_DEL     = 1,  // do not allow DELETING any lines or
                          //   flags or cards.
                          //
       LOCK_S       = 2,  // also do not allow changing any data
                          //   which will cause the locations and
                          //   makeups of SOURCE GATHERS to change
                          //   (become out-of-date).  This includes
                          //   changes mentioned above, plus
                          //   most changes to LD, PP, ZT1, ZT3,
                          //   and ZT4 cards.
                          //
       LOCK_S_R     = 3,  // also do not allow changing any data
                          //   which will cause the locations and
                          //   makeups of RECEIVER GATHERS to change
                          //   (become out-of-date).  This includes
                          //   changes mentioned above, plus
                          //   changes to the RP and ZT2 cards.
                          //
       LOCK_S_R_CMP = 4,  // also do not allow changing any data
                          //   which will cause the locations and
                          //   makeups of CMP GATHERS to change
                          //   (become out-of-date).  This includes
                          //   changes mentioned above, plus
                          //   changes to the grid transform,
                          //
       LOCK_ALL     = 5   // do not allow changing any data.
     };


//--------------------- field flag constants -----------------------//
//--------------------- field flag constants -----------------------//
//--------------------- field flag constants -----------------------//

enum { FG_NONE  =  0, // unknown, or anything else, or none of these.
       FG_SHOT  =  1, // shotpoint of field flag.
       FG_DIST  =  2, // incremental distance of field flag from previous flag.
       FG_XLOC  =  3, // X distance location of field flag.
       FG_YLOC  =  4, // Y distance location of field flag.
       FG_ELEV  =  5, // elevation of field flag.
       FG_XGRID =  6, // X grid location of field flag.
       FG_YGRID =  7, // Y grid location of field flag.
       FG_HD    =  8, // default hole depth of a source at this field flag.
       FG_TUH   =  9, // default uphole time of a source at this field flag.
       FG_RSTAT = 10, // receiver static at this field flag.
       FG_SSTAT = 11, // source static at this field flag.
       FG_XSKID = 12, // inline skid of receiver at this field flag.
       FG_YSKID = 13, // crossline skid of receiver at this field flag.
       FG_ESKID = 14, // elevation skid of receiver at this field flag.
       FG_SEL   = 15, // selection flag.
       FG_CUM   = 16, // cumulative horiz dist of field flag from start of line.
       FG_AZIM  = 17  // azimuth of field flag from previous field flag.
     };

enum { FIRST_FIELD_FLAG_VARIABLE  = FG_SHOT };
enum {  LAST_FIELD_FLAG_VARIABLE  = FG_AZIM };
enum {   NUM_FIELD_FLAG_VARIABLES =  LAST_FIELD_FLAG_VARIABLE -
                                    FIRST_FIELD_FLAG_VARIABLE + 1 };

enum { FG_COORDS = 99 };  // used in FgInform to signify
                          // changes to X and/or Y coordinates.

//---------------------- RP card constants -------------------------//
//---------------------- RP card constants -------------------------//
//---------------------- RP card constants -------------------------//

enum { RP_FLAG_X = 1, RP_FLAG_Y, RP_FLAG_SKIP, RP_FLAG_DUP };

enum { RP_NONE    =  0, // unknown, or anything else, or none of these.
       RP_PAT     =  1, // receiver pattern number.
       RP_NCHAN   =  2, // number of channels (from ALL CARDS of pattern).
       RP_CUMCHAN =  3, // cumulative number of channels (up thru this card).
       RP_FLAG    =  4, // receiver pattern flag (enum).
       RP_SHOT    =  5, // shotpoint   of first receiver in pattern.
       RP_LINE    =  6, // line number of first receiver in pattern.
       RP_NX      =  7, // number of       receivers in    inline direction.
       RP_XINC    =  8, // spacing between receivers in    inline direction.
       RP_NY      =  9, // number of       receivers in crossline direction.
       RP_YINC    = 10, // spacing between receivers in crossline direction.
       RP_XSKID   = 11, // pattern skid in    inline direction.
       RP_YSKID   = 12, // pattern skid in crossline direction.
       RP_ESKID   = 13  // pattern skid in elevation.
     };

enum { FIRST_RP_VARIABLE  = RP_PAT   };
enum {  LAST_RP_VARIABLE  = RP_ESKID };
enum {   NUM_RP_VARIABLES =  LAST_RP_VARIABLE -
                            FIRST_RP_VARIABLE + 1 };


//---------------------- PP card constants -------------------------//
//---------------------- PP card constants -------------------------//
//---------------------- PP card constants -------------------------//

enum { PP_NONE    =  0, // unknown, or anything else, or none of these.
       PP_FILE    =  1, // original file number from the field.
       PP_SSHOT   =  2, // shotpoint   of first source on card.
       PP_SLINE   =  3, // line number of first source on card.
       PP_RSHOT   =  4, // shotpoint   of first receiver in pattern.
       PP_RLINE   =  5, // line number of first receiver in pattern.
       PP_PAT     =  6, // receiver pattern number.
       PP_XSKID   =  7, // source inline skid.
       PP_YSKID   =  8, // source crossline skid.
       PP_HOLD    =  9, // how many sources to hold skid.
       PP_ELEV    = 10, // new source elevation.
       PP_HD      = 11, // new source hole depth.
       PP_TUH     = 12, // new source uphole time.
       PP_SMOVE   = 13, // source   moveup in FieldFlag indices.
       PP_RMOVE   = 14, // receiver moveup in FieldFlag indices.
       PP_NGROUPS = 15, // number of groups described by this card.
       PP_NTRACES = 16, // number of traces described by this card.
       PP_THRU_GR = 17, // through group number for this card.
       PP_THRU_TR = 18  // through trace number for this card.
     };

enum { FIRST_PP_VARIABLE  = PP_FILE    };
enum {  LAST_PP_VARIABLE  = PP_THRU_TR };
enum {   NUM_PP_VARIABLES =  LAST_PP_VARIABLE -
                            FIRST_PP_VARIABLE + 1 };


//---------------------- ZT code values -------------------------//
//---------------------- ZT code values -------------------------//
//---------------------- ZT code values -------------------------//

enum { ZT_CODE_ZERO = 1,  // killed.
       ZT_CODE_REV  = 2,  // reverse polarity.
       ZT_CODE_MISS = 3,  // missing on input reels.
       ZT_CODE_LIVE = 4,  // live    (not permitted on ZT card).
       ZT_CODE_NONE = 5   // unknown (not permitted on ZT card).
     };


//---------------------- ZT1 card constants -------------------------//
//---------------------- ZT1 card constants -------------------------//
//---------------------- ZT1 card constants -------------------------//

   // note that the idents for all four types of ZT cards are unique.

enum { ZT1_NONE  = 10, // unknown, or anything else, or none of these.
       ZT1_CODE  = 11, // code value (enum).
       ZT1_SHOT1 = 12, // from source shotpoint.
       ZT1_SHOT2 = 13, // to   source shotpoint.
       ZT1_LINE  = 14  // source line number.
     };

enum { FIRST_ZT1_VARIABLE  = ZT1_CODE  };
enum {  LAST_ZT1_VARIABLE  = ZT1_LINE  };
enum {   NUM_ZT1_VARIABLES =  LAST_ZT1_VARIABLE -
                             FIRST_ZT1_VARIABLE + 1 };


//---------------------- ZT2 card constants -------------------------//
//---------------------- ZT2 card constants -------------------------//
//---------------------- ZT2 card constants -------------------------//

   // note that the idents for all four types of ZT cards are unique.

enum { ZT2_NONE  = 20, // unknown, or anything else, or none of these.
       ZT2_CODE  = 21, // code value (enum).
       ZT2_SHOT1 = 22, // from receiver shotpoint.
       ZT2_SHOT2 = 23, // to   receiver shotpoint.
       ZT2_LINE  = 24  // receiver line number.
     };

enum { FIRST_ZT2_VARIABLE  = ZT2_CODE  };
enum {  LAST_ZT2_VARIABLE  = ZT2_LINE  };
enum {   NUM_ZT2_VARIABLES =  LAST_ZT2_VARIABLE -
                             FIRST_ZT2_VARIABLE + 1 };


//---------------------- ZT3 card constants -------------------------//
//---------------------- ZT3 card constants -------------------------//
//---------------------- ZT3 card constants -------------------------//

   // note that the idents for all four types of ZT cards are unique.

enum { ZT3_NONE   = 30, // unknown, or anything else, or none of these.
       ZT3_CODE   = 31, // code value (enum).
       ZT3_GROUP1 = 32, // from group.
       ZT3_GROUP2 = 33, // to   group.
       ZT3_TRACE1 = 34, // from trace.
       ZT3_TRACE2 = 35  // to   trace.
     };

enum { FIRST_ZT3_VARIABLE  = ZT3_CODE   };
enum {  LAST_ZT3_VARIABLE  = ZT3_TRACE2 };
enum {   NUM_ZT3_VARIABLES =  LAST_ZT3_VARIABLE -
                             FIRST_ZT3_VARIABLE + 1 };


//---------------------- ZT4 card constants -------------------------//
//---------------------- ZT4 card constants -------------------------//
//---------------------- ZT4 card constants -------------------------//

   // note that the idents for all four types of ZT cards are unique.

enum { ZT4_NONE   = 40, // unknown, or anything else, or none of these.
       ZT4_CODE   = 41, // code value (enum).
       ZT4_SSHOT1 = 42, // from source shotpoint.
       ZT4_SSHOT2 = 43, // to   source shotpoint.
       ZT4_SLINE  = 44, // source line number.
       ZT4_RSHOT1 = 45, // from receiver shotpoint.
       ZT4_RSHOT2 = 46, // to   receiver shotpoint.
       ZT4_RLINE  = 47  // receiver line number.
     };

enum { FIRST_ZT4_VARIABLE  = ZT4_CODE   };
enum {  LAST_ZT4_VARIABLE  = ZT4_RLINE  };
enum {   NUM_ZT4_VARIABLES =  LAST_ZT4_VARIABLE -
                             FIRST_ZT4_VARIABLE + 1 };


//---------------------- end of constants --------------------------//
//---------------------- end of constants --------------------------//
//---------------------- end of constants --------------------------//

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
