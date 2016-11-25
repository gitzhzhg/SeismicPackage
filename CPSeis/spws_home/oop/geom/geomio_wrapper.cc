
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
//-------------------------- geomio_wrapper.cc ------------------------------//
//-------------------------- geomio_wrapper.cc ------------------------------//
//-------------------------- geomio_wrapper.cc ------------------------------//

//             implementation file for the GeomioWrapper class
//                 derived from the PjarFileSupport class
//                          subdirectory geom


#include "geom/geomio_wrapper.hh"
#include "str.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


static const int   nfields_ld  = 13;
static const char *fields_ld[] =
                       { "sp", "dist", "xloc", "yloc",
                         "elev", "depth", "tuh", "tr",
                         "ts", "xsd", "ysd", "elsd", "line" };

static const int   nrequired_ld  = 5;
static const char *required_ld[] =
                       { "sp", "dist", "xloc", "yloc", "line" };



static const int   nfields_rp  = 11;
static const char *fields_rp[] =
                       { "ipat1", "flag", "sp1", "line1",
                         "nx", "ixinc", "ny", "iyinc",
                         "xsd1", "ysd1", "elsd1" };

static const int   nrequired_rp  = 8;
static const char *required_rp[] =
                       { "ipat1", "flag", "sp1", "line1",
                         "nx", "ixinc", "ny", "iyinc" };



static const int   nfields_pp  = 14;
static const char *fields_pp[] =
                       { "sp2", "line2", "sp3", "line3",
                         "ipat2", "xsd2", "ysd2", "hold",
                         "elev2", "depth2", "tuh2", "is", "ir", "ig" };

static const int   nrequired_pp  = 8;
static const char *required_pp[] =
                       { "sp2", "line2", "sp3", "line3",
                         "ipat2",
                         "is", "ir", "ig" };



static const int   nfields_zt1  = 4;
static const char *fields_zt1[] =
                       { "ccc1", "sss1", "sss1a", "lll1" }; 

static const int   nrequired_zt1  = 4;
static const char *required_zt1[] =
                       { "ccc1", "sss1", "sss1a", "lll1" };



static const int   nfields_zt2  = 4;
static const char *fields_zt2[] =
                       { "ccc2", "rrr2", "rrr2a", "lll2" };

static const int   nrequired_zt2  = 4;
static const char *required_zt2[] =
                       { "ccc2", "rrr2", "rrr2a", "lll2" };



static const int   nfields_zt3  = 5;
static const char *fields_zt3[] =
                       { "ccc3", "iggg3", "iggg3a", "ittt3", "ittt3a" };

static const int   nrequired_zt3  = 5;
static const char *required_zt3[] =
                       { "ccc3", "iggg3", "iggg3a", "ittt3", "ittt3a" };



static const int   nfields_zt4  = 7;
static const char *fields_zt4[] =
                       { "ccc4", "sss4", "sss4a",
                         "lll4", "rrr4", "rrr4a", "lll4a" };

static const int   nrequired_zt4  = 7;
static const char *required_zt4[] =
                       { "ccc4", "sss4", "sss4a",
                         "lll4", "rrr4", "rrr4a", "lll4a" };



enum { SITUATION_GEOMETRY, SITUATION_FOREIGN,
       SITUATION_GENERIC, SITUATION_NONE };


//------------------ constructor and destructor -----------------------//
//------------------ constructor and destructor -----------------------//
//------------------ constructor and destructor -----------------------//


GeomioWrapper::GeomioWrapper(int io)
         : PjarFileSupport (io, "geometry"),
              _situation  (SITUATION_NONE)
{
}



GeomioWrapper::~GeomioWrapper()
{
  closeFile();
}



//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//


#if NEED_UNDERSCORE
#define geomio_frou_verify                  geomio_frou_verify_
#define geomio_frou_augment                 geomio_frou_augment_
#define geomio_frou_open_read               geomio_frou_open_read_
#define geomio_frou_open_write              geomio_frou_open_write_
#define geomio_frou_open_foreign            geomio_frou_open_foreign_
#define geomio_frou_close                   geomio_frou_close_
#define geomio_frou_read_ld_card            geomio_frou_read_ld_card_
#define geomio_frou_write_ld_card           geomio_frou_write_ld_card_
#define geomio_frou_read_rp_card            geomio_frou_read_rp_card_
#define geomio_frou_write_rp_card           geomio_frou_write_rp_card_
#define geomio_frou_read_pp_card            geomio_frou_read_pp_card_
#define geomio_frou_write_pp_card           geomio_frou_write_pp_card_
#define geomio_frou_read_zt1_card           geomio_frou_read_zt1_card_
#define geomio_frou_write_zt1_card          geomio_frou_write_zt1_card_
#define geomio_frou_read_zt2_card           geomio_frou_read_zt2_card_
#define geomio_frou_write_zt2_card          geomio_frou_write_zt2_card_
#define geomio_frou_read_zt3_card           geomio_frou_read_zt3_card_
#define geomio_frou_write_zt3_card          geomio_frou_write_zt3_card_
#define geomio_frou_read_zt4_card           geomio_frou_read_zt4_card_
#define geomio_frou_write_zt4_card          geomio_frou_write_zt4_card_
#elif NEED_CAPITALS
#define geomio_frou_verify                  GEOMIO_FROU_VERIFY
#define geomio_frou_augment                 GEOMIO_FROU_AUGMENT
#define geomio_frou_open_read               GEOMIO_FROU_OPEN_READ  
#define geomio_frou_open_write              GEOMIO_FROU_OPEN_WRITE 
#define geomio_frou_open_foreign            GEOMIO_FROU_OPEN_FOREIGN
#define geomio_frou_close                   GEOMIO_FROU_CLOSE
#define geomio_frou_read_ld_card            GEOMIO_FROU_READ_LD_CARD
#define geomio_frou_write_ld_card           GEOMIO_FROU_WRITE_LD_CARD
#define geomio_frou_read_rp_card            GEOMIO_FROU_READ_RP_CARD
#define geomio_frou_write_rp_card           GEOMIO_FROU_WRITE_RP_CARD
#define geomio_frou_read_pp_card            GEOMIO_FROU_READ_PP_CARD
#define geomio_frou_write_pp_card           GEOMIO_FROU_WRITE_PP_CARD
#define geomio_frou_read_zt1_card           GEOMIO_FROU_READ_ZT1_CARD
#define geomio_frou_write_zt1_card          GEOMIO_FROU_WRITE_ZT1_CARD
#define geomio_frou_read_zt2_card           GEOMIO_FROU_READ_ZT2_CARD
#define geomio_frou_write_zt2_card          GEOMIO_FROU_WRITE_ZT2_CARD
#define geomio_frou_read_zt3_card           GEOMIO_FROU_READ_ZT3_CARD
#define geomio_frou_write_zt3_card          GEOMIO_FROU_WRITE_ZT3_CARD
#define geomio_frou_read_zt4_card           GEOMIO_FROU_READ_ZT4_CARD
#define geomio_frou_write_zt4_card          GEOMIO_FROU_WRITE_ZT4_CARD
#endif


//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//

extern "C" {

void geomio_frou_verify       (F90Pointer *pjar  , const char *secname,
                               int        *err   , char       *msg);

void geomio_frou_augment      (F90Pointer *pjar  , const char *secname);

void geomio_frou_open_read    (F90Pointer *fpoint, const char *filename,
                               F90Pointer *pjar  , const char *secname,
                               int        *err   , char       *msg);

void geomio_frou_open_write   (F90Pointer *fpoint, const char *filename,
                               F90Pointer *pjar  , const char *secname,
                               int        *err   , char       *msg);

void geomio_frou_open_foreign (F90Pointer *fpoint, const char *filename,
                               F90Pointer *pjar  , const char *secname,
                               int        *err   , char       *msg);

void geomio_frou_close        (F90Pointer *fpoint);

void geomio_frou_read_ld_card
        (F90Pointer  *fpoint,
         const int   *ild   , int   *err  , char  *msg ,
         float       *sp    , float *dist , float *xloc, float *yloc,
         float       *elev  , float *depth, float *tuh ,
         float       *tr    , float *ts   , float *xsd , float *ysd,
         float       *elsd  , int   *line);

void geomio_frou_write_ld_card
        (F90Pointer  *fpoint,
         const int   *ild   , int         *err  , char        *msg ,
         const float *sp    , const float *dist ,
         const float *xloc  , const float *yloc ,
         const float *elev  , const float *depth, const float *tuh ,
         const float *tr    , const float *ts   ,
         const float *xsd   , const float *ysd  ,
         const float *elsd  , const int   *line);

void geomio_frou_read_rp_card
        (F90Pointer  *fpoint,
         const int   *irp  , int        *err  , char  *msg,
         int         *ipat1, char       *flag , float *sp1, int *line1,
         int         *nx   , int        *ixinc, int   *ny , int *iyinc,
         float       *xsdi , float      *ysdi , float *elsd1);

void geomio_frou_write_rp_card
        (F90Pointer  *fpoint,
         const int   *irp  , int         *err  , char  *msg,
         const int   *ipat1, const char  *flag ,
         const float *sp1  , const int   *line1,
         const int   *nx   , const int   *ixinc,
         const int   *ny   , const int   *iyinc,
         const float *xsdi , const float *ysdi , const float *elsd1);

void geomio_frou_read_pp_card
        (F90Pointer  *fpoint,
         const int   *ipp  , int   *err  , char  *msg   ,
         float       *sp2  , int   *line2, float *sp3   , int   *line3,
         int         *ipat2, float *xsd2 , float *ysd2  ,
         int         *hold , float *elev2, float *depth2, float *tuh2,
         int         *is   , int   *ir   , int   *ig);

void geomio_frou_write_pp_card
        (F90Pointer  *fpoint,
         const int   *ipp   , int         *err  , char        *msg,
         const float *sp2   , const int   *line2,
         const float *sp3   , const int   *line3,
         const int   *ipat2 , const float *xsd2 , const float *ysd2,
         const int   *hold  , const float *elev2,
         const float *depth2, const float *tuh2,
         const int   *is    , const int   *ir   , const int   *ig);

void geomio_frou_read_zt1_card
        (F90Pointer *fpoint,
         const int  *izt1, int   *err  , char  *msg,
         char       *ccc1, float *sss1 , float *sss1a, int *lll1);

void geomio_frou_write_zt1_card
        (F90Pointer *fpoint,
         const int  *izt1, int         *err  , char        *msg,
         const char *ccc1, const float *sss1 , const float *sss1a,
         const int  *lll1);

void geomio_frou_read_zt2_card
        (F90Pointer *fpoint,
         const int  *izt2, int   *err  , char  *msg,
         char       *ccc2, float *rrr2 , float *rrr2a, int *lll2);

void geomio_frou_write_zt2_card
        (F90Pointer *fpoint,
         const int  *izt2, int         *err  , char        *msg,
         const char *ccc2, const float *rrr2 , const float *rrr2a,
         const int  *lll2);

void geomio_frou_read_zt3_card
        (F90Pointer *fpoint,
         const int  *izt3 , int *err  , char *msg,
         char       *ccc3 , int *iggg3, int  *iggg3a,
         int        *ittt3, int *ittt3a);

void geomio_frou_write_zt3_card
        (F90Pointer *fpoint,
         const int  *izt3 , int       *err  , char      *msg,
         const char *ccc3 , const int *iggg3, const int *iggg3a,
         const int  *ittt3, const int *ittt3a);

void geomio_frou_read_zt4_card
        (F90Pointer *fpoint,
         const int  *izt4, int   *err , char  *msg,
         char       *ccc4, float *sss4, float *sss4a,
         int        *lll4, float *rrr4, float *rrr4a, int *lll4a);

void geomio_frou_write_zt4_card
        (F90Pointer *fpoint,
         const int  *izt4, int         *err , char        *msg,
         const char *ccc4, const float *sss4, const float *sss4a,
         const int  *lll4, const float *rrr4, const float *rrr4a,
         const int  *lll4a);

}   // end extern "C"



//---------------- virtual functions overriding PjarFileSupport -----------//
//---------------- virtual functions overriding PjarFileSupport -----------//
//---------------- virtual functions overriding PjarFileSupport -----------//

  //  


int GeomioWrapper::virtualValidate (const char *filename, char *msg)
{
  int err;
  geomio_frou_open_read(fpoint(), filename, pjar(), defname(), &err, msg);
  geomio_frou_close    (fpoint());

  fieldsArePresent();   // needed to call chooseActiveSection(),
                        // which calls chooseMostLikelySection().

  if     (strcmp(secname(), "GEOMETRY") == 0) _situation = SITUATION_GEOMETRY;
  else if(strcmp(secname(), "FOREIGN" ) == 0) _situation = SITUATION_FOREIGN;
  else if(strcmp(secname(), ""        ) == 0) _situation = SITUATION_NONE;
  else                                        _situation = SITUATION_GENERIC;

  if(_situation == SITUATION_FOREIGN && isInput())
      {
      setScalarInteger("nld" , -1);
      setScalarInteger("nrp" , -1);
      setScalarInteger("npp" , -1);
      setScalarInteger("nzt1", -1);
      setScalarInteger("nzt2", -1);
      setScalarInteger("nzt3", -1);
      setScalarInteger("nzt4", -1);

      geomio_frou_augment (pjar(), secname());

      helperAugmentParameters("ld_secname" , fields_ld , nfields_ld );
      helperAugmentParameters("rp_secname" , fields_rp , nfields_rp );
      helperAugmentParameters("pp_secname" , fields_pp , nfields_pp );
      helperAugmentParameters("zt1_secname", fields_zt1, nfields_zt1);
      helperAugmentParameters("zt2_secname", fields_zt2, nfields_zt2);
      helperAugmentParameters("zt3_secname", fields_zt3, nfields_zt3);
      helperAugmentParameters("zt4_secname", fields_zt4, nfields_zt4);
      }

  if(_situation == SITUATION_GENERIC && isInput())
      {
      setActiveHeaderSection("GENERIC");   // new section created.

      setScalarInteger("nld" , -1);
      setScalarInteger("nrp" , -1);
      setScalarInteger("npp" , -1);
      setScalarInteger("nzt1", -1);
      setScalarInteger("nzt2", -1);
      setScalarInteger("nzt3", -1);
      setScalarInteger("nzt4", -1);

      setScalarString("ld_secname" , "");
      setScalarString("rp_secname" , "");
      setScalarString("pp_secname" , "");
      setScalarString("zt1_secname", "");
      setScalarString("zt2_secname", "");
      setScalarString("zt3_secname", "");
      setScalarString("zt4_secname", "");

      for(int indx = 0; indx < numHeaderSections(); indx++)
          {
          setActiveHeaderSection(indx);
          for(int icol = 0; icol < numFields(); icol++)
              {
              setElementLogical("field_is_active", icol, FALSE);
              setElementLogical("skip"           , icol, TRUE);
              }
          }
/*
      helperDeactivateFields("ld_secname" );
      helperDeactivateFields("rp_secname" );
      helperDeactivateFields("pp_secname" );
      helperDeactivateFields("zt1_secname");
      helperDeactivateFields("zt2_secname");
      helperDeactivateFields("zt3_secname");
      helperDeactivateFields("zt4_secname");
*/
      setActiveHeaderSection("GENERIC");
      }
  else
      {
      helperMarkActiveFields("ld_secname" , fields_ld , nfields_ld );
      helperMarkActiveFields("rp_secname" , fields_rp , nfields_rp );
      helperMarkActiveFields("pp_secname" , fields_pp , nfields_pp );
      helperMarkActiveFields("zt1_secname", fields_zt1, nfields_zt1);
      helperMarkActiveFields("zt2_secname", fields_zt2, nfields_zt2);
      helperMarkActiveFields("zt3_secname", fields_zt3, nfields_zt3);
      helperMarkActiveFields("zt4_secname", fields_zt4, nfields_zt4);
      }

  if(err == STATUS_OK)
      {
      int   nld     = getScalarInteger("nld"    );
      int   nrp     = getScalarInteger("nrp"    );
      int   npp     = getScalarInteger("npp"    );
      int   nzt1    = getScalarInteger("nzt1"   );
      int   nzt2    = getScalarInteger("nzt2"   );
      int   nzt3    = getScalarInteger("nzt3"   );
      int   nzt4    = getScalarInteger("nzt4"   );

      sprintf(msg, "LD=%d  RP=%d  PP=%d  ZT1=%d  ZT2=%d  ZT3=%d  ZT4=%d",
                                nld, nrp, npp, nzt1, nzt2, nzt3, nzt4);
      }
  return (err != STATUS_OK);
}



void GeomioWrapper::virtualAugment ()
{
  if(isInput()) return;

  if(!keywordPresent("nld" )) setScalarInteger("nld" , -1);
  if(!keywordPresent("nrp" )) setScalarInteger("nrp" , -1);
  if(!keywordPresent("npp" )) setScalarInteger("npp" , -1);
  if(!keywordPresent("nzt1")) setScalarInteger("nzt1", -1);
  if(!keywordPresent("nzt2")) setScalarInteger("nzt2", -1);
  if(!keywordPresent("nzt3")) setScalarInteger("nzt3", -1);
  if(!keywordPresent("nzt4")) setScalarInteger("nzt4", -1);

  geomio_frou_augment (pjar(), secname());

  helperAugmentParameters("ld_secname" , fields_ld , nfields_ld );
  helperAugmentParameters("rp_secname" , fields_rp , nfields_rp );
  helperAugmentParameters("pp_secname" , fields_pp , nfields_pp );
  helperAugmentParameters("zt1_secname", fields_zt1, nfields_zt1);
  helperAugmentParameters("zt2_secname", fields_zt2, nfields_zt2);
  helperAugmentParameters("zt3_secname", fields_zt3, nfields_zt3);
  helperAugmentParameters("zt4_secname", fields_zt4, nfields_zt4);

  helperMarkActiveFields("ld_secname" , fields_ld , nfields_ld );
  helperMarkActiveFields("rp_secname" , fields_rp , nfields_rp );
  helperMarkActiveFields("pp_secname" , fields_pp , nfields_pp );
  helperMarkActiveFields("zt1_secname", fields_zt1, nfields_zt1);
  helperMarkActiveFields("zt2_secname", fields_zt2, nfields_zt2);
  helperMarkActiveFields("zt3_secname", fields_zt3, nfields_zt3);
  helperMarkActiveFields("zt4_secname", fields_zt4, nfields_zt4);
}



int GeomioWrapper::virtualVerify (char *msg)
{
  int error = (strcmp(secname(), "GEOMETRY") != 0 &&
               strcmp(secname(), "GENERIC" ) != 0 &&
               strcmp(secname(), "FOREIGN" ) != 0);
  if(error)
      {
      switch(_situation)
          {
          case SITUATION_GEOMETRY:
                   strcpy(msg, "you must choose section GEOMETRY"); break;
          case SITUATION_GENERIC:
                   strcpy(msg, "you must choose section GENERIC"); break;
          case SITUATION_FOREIGN:
                   strcpy(msg, "you must choose section FOREIGN"); break;
          default:
                   strcpy(msg, "invalid section chosen"); break;
          }
      return error;
      }

  int err;
  geomio_frou_verify (pjar(), secname(), &err, msg);
  error = (err != STATUS_OK);

  if(!error) error = helperRequired ("ld_secname" , "nld" , "skip_ld",
                                     required_ld , nrequired_ld , msg);
  if(!error) error = helperRequired ("rp_secname" , "nrp" , "skip_rp",
                                     required_rp , nrequired_rp , msg);
  if(!error) error = helperRequired ("pp_secname" , "npp" , "skip_pp",
                                     required_pp , nrequired_pp , msg);
  if(!error) error = helperRequired ("zt1_secname", "nzt1", "skip_zt1",
                                     required_zt1, nrequired_zt1, msg);
  if(!error) error = helperRequired ("zt2_secname", "nzt2", "skip_zt2",
                                     required_zt2, nrequired_zt2, msg);
  if(!error) error = helperRequired ("zt3_secname", "nzt3", "skip_zt3",
                                     required_zt3, nrequired_zt3, msg);
  if(!error) error = helperRequired ("zt4_secname", "nzt4", "skip_zt4",
                                     required_zt4, nrequired_zt4, msg);
  return error;
}



//------------------------ get values ---------------------------------//
//------------------------ get values ---------------------------------//
//------------------------ get values ---------------------------------//


int GeomioWrapper::getGenericInputChoice ()
{
  if(isOutput()) return CHOICE_NONE;
  if(_situation != SITUATION_GENERIC) return CHOICE_NONE;
  if(strcmp(secname(), "GENERIC") == 0) return CHOICE_NONE;
  int choice = getScalarInteger("choice");
  if(choice == INIL || choice < CHOICE_LD || choice > CHOICE_ZT4)
                                                         choice = CHOICE_NONE;
  return choice;
}


int GeomioWrapper::needGenericInputChoice ()
{
  if(isOutput()) return FALSE;
  if(_situation != SITUATION_GENERIC) return FALSE;
  if(strcmp(secname(), "GENERIC") == 0) return FALSE;
  if(strcmp(getEncoding(), "") == 0) return FALSE;
  return TRUE;
}


int GeomioWrapper::needGenericInputParameters ()
{
  if(isOutput()) return FALSE;
  if(_situation == SITUATION_NONE) return FALSE;
  if(strcmp(secname(), "GENERIC" ) == 0) return TRUE;
  if(strcmp(secname(), "FOREIGN" ) == 0) return TRUE;
  if(strcmp(secname(), "GEOMETRY") == 0) return TRUE;
  return FALSE;
}



//------------------------ set values ---------------------------------//
//------------------------ set values ---------------------------------//
//------------------------ set values ---------------------------------//


void GeomioWrapper::stepGenericInputChoice (int step)
{
  if(step == 0) return;
  if(isOutput()) return;
  if(_situation != SITUATION_GENERIC) return;
  if(strcmp(secname(), "GENERIC") == 0) return;

////////// now we are in a card header section.
////////// remove previous fields in this section:

  int nfields  = numFields();
  int remember = getScalarInteger("remember_nfields");
  if(remember == INIL)
      {
      setScalarInteger("remember_nfields", nfields);
      }
  else
      {
      for(int icol = nfields-1; icol >= remember; icol--)
          {
          removeField(icol);
          }
      int ncolumns = getNcolumns();
      if(ncolumns > remember) setNcolumns(remember);
      }

////////// get the current (previous) choice and go to the main section:

  int choice = getScalarInteger("choice");
  if(choice == INIL || choice < CHOICE_LD || choice > CHOICE_ZT4)
                                                        choice = CHOICE_NONE;
  char keep[55];
  strcpy(keep, secname());
  setActiveHeaderSection("GENERIC");

////////// delete previous references to the card header section:

  if(strcmp(getScalarString("ld_secname"), keep) == 0)
            setScalarString("ld_secname", "");
  if(strcmp(getScalarString("rp_secname"), keep) == 0)
            setScalarString("rp_secname", "");
  if(strcmp(getScalarString("pp_secname"), keep) == 0)
            setScalarString("pp_secname", "");
  if(strcmp(getScalarString("zt1_secname"), keep) == 0)
            setScalarString("zt1_secname", "");
  if(strcmp(getScalarString("zt2_secname"), keep) == 0)
            setScalarString("zt2_secname", "");
  if(strcmp(getScalarString("zt3_secname"), keep) == 0)
            setScalarString("zt3_secname", "");
  if(strcmp(getScalarString("zt4_secname"), keep) == 0)
            setScalarString("zt4_secname", "");

////////// step to the next valid card type to use:

  while(TRUE)
      {
      choice += step;
      if(choice < CHOICE_NONE) choice = CHOICE_ZT4;
      if(choice > CHOICE_ZT4 ) choice = CHOICE_NONE;
      switch(choice)
          {
          case CHOICE_LD :
             if(strcmp(getScalarString("ld_secname"), "") != 0) continue;
             setScalarString("ld_secname", keep);
             break;
          case CHOICE_RP :
             if(strcmp(getScalarString("rp_secname"), "") != 0) continue;
             setScalarString("rp_secname", keep);
             break;
          case CHOICE_PP :
             if(strcmp(getScalarString("pp_secname"), "") != 0) continue;
             setScalarString("pp_secname", keep);
             break;
          case CHOICE_ZT1:
             if(strcmp(getScalarString("zt1_secname"), "") != 0) continue;
             setScalarString("zt1_secname", keep);
             break;
          case CHOICE_ZT2:
             if(strcmp(getScalarString("zt2_secname"), "") != 0) continue;
             setScalarString("zt2_secname", keep);
             break;
          case CHOICE_ZT3:
             if(strcmp(getScalarString("zt3_secname"), "") != 0) continue;
             setScalarString("zt3_secname", keep);
             break;
          case CHOICE_ZT4:
             if(strcmp(getScalarString("zt4_secname"), "") != 0) continue;
             setScalarString("zt4_secname", keep);
             break;
          default:
             choice = CHOICE_NONE;
             break;
          }
      break;
      }

////////// augment this card header section (done from the main section):

  switch(choice)
      {
      case CHOICE_LD :
         helperAugmentParameters("ld_secname" , fields_ld , nfields_ld );
         helperMarkActiveFields ("ld_secname" , fields_ld , nfields_ld );
         break;
      case CHOICE_RP :
         helperAugmentParameters("rp_secname" , fields_rp , nfields_rp );
         helperMarkActiveFields ("rp_secname" , fields_rp , nfields_rp );
         break;
      case CHOICE_PP :
         helperAugmentParameters("pp_secname" , fields_pp , nfields_pp );
         helperMarkActiveFields ("pp_secname" , fields_pp , nfields_pp );
         break;
      case CHOICE_ZT1:
         helperAugmentParameters("zt1_secname", fields_zt1, nfields_zt1);
         helperMarkActiveFields ("zt1_secname", fields_zt1, nfields_zt1);
         break;
      case CHOICE_ZT2:
         helperAugmentParameters("zt2_secname", fields_zt2, nfields_zt2);
         helperMarkActiveFields ("zt2_secname", fields_zt2, nfields_zt2);
         break;
      case CHOICE_ZT3:
         helperAugmentParameters("zt3_secname", fields_zt3, nfields_zt3);
         helperMarkActiveFields ("zt3_secname", fields_zt3, nfields_zt3);
         break;
      case CHOICE_ZT4:
         helperAugmentParameters("zt4_secname", fields_zt4, nfields_zt4);
         helperMarkActiveFields ("zt4_secname", fields_zt4, nfields_zt4);
         break;
      }

////////// return to the card header section and set the new choice:

  setActiveHeaderSection(keep);
  setScalarInteger("choice", choice);
}



//----------------------- read and write disk files -----------------------//
//----------------------- read and write disk files -----------------------//
//----------------------- read and write disk files -----------------------//


int   GeomioWrapper::openInputFile       (const char *filename, char *msg)
{
  int err;
  geomio_frou_open_foreign(fpoint(), filename, pjar(), secname(), &err, msg);

  if(err != STATUS_OK) closeFile();
  return (err != STATUS_OK);
}


///// Note: encoding will be removed from pjar by grid.f90 (if not oldcps)
///// and must be restored:

int   GeomioWrapper::openOutputFile       (const char *filename, char *msg)
{
  char encoding[44];
  strcpy(encoding, getEncoding());
  int err;
  geomio_frou_open_write(fpoint(), filename, pjar(), secname(), &err, msg);
  setEncoding(encoding);

  if(err != STATUS_OK) closeFile();
  return (err != STATUS_OK);
}



void  GeomioWrapper::closeFile()
{
  geomio_frou_close (fpoint());
}



//------------------- read and write cards --------------------------------//
//------------------- read and write cards --------------------------------//
//------------------- read and write cards --------------------------------//


void GeomioWrapper::readLdCard
                    (int    ild , int   *err  , char  *msg ,
                     float *sp  , float *dist , float *xloc, float *yloc,
                     float *elev, float *depth, float *tuh ,
                     float *tr  , float *ts   , float *xsd , float *ysd,
                     float *elsd, int   *line)
{
  geomio_frou_read_ld_card (fpoint(), &ild, err, msg,
                            sp  , dist , xloc, yloc,
                            elev, depth, tuh ,
                            tr  , ts   , xsd , ysd,
                            elsd, line);
}



void GeomioWrapper::writeLdCard
                    (int    ild , int   *err  , char  *msg ,
                     float  sp  , float  dist , float  xloc, float  yloc,
                     float  elev, float  depth, float  tuh ,
                     float  tr  , float  ts   , float  xsd , float  ysd,
                     float  elsd, int    line)
{
  geomio_frou_write_ld_card (fpoint(), &ild, err, msg,
                             &sp  , &dist , &xloc, &yloc,
                             &elev, &depth, &tuh ,
                             &tr  , &ts   , &xsd , &ysd,
                             &elsd, &line);
}



void GeomioWrapper::readRpCard
                    (int    irp  , int        *err  , char  *msg,
                     int   *ipat1, char       *flag , float *sp1, int *line1,
                     int   *nx   , int        *ixinc, int   *ny , int *iyinc,
                     float *xsdi , float      *ysdi , float *elsd1)
{
  geomio_frou_read_rp_card (fpoint(), &irp, err, msg,
                            ipat1, flag , sp1, line1,
                            nx   , ixinc, ny , iyinc,
                            xsdi , ysdi , elsd1);
}



void GeomioWrapper::writeRpCard
                    (int    irp  , int        *err  , char  *msg,
                     int    ipat1, const char *flag , float  sp1, int  line1,
                     int    nx   , int         ixinc, int    ny , int  iyinc,
                     float  xsdi , float       ysdi , float  elsd1)
{
  geomio_frou_write_rp_card (fpoint(), &irp, err, msg,
                             &ipat1,  flag , &sp1, &line1,
                             &nx   , &ixinc, &ny , &iyinc,
                             &xsdi , &ysdi , &elsd1);
}



void GeomioWrapper::readPpCard
                    (int    ipp  , int   *err  , char  *msg   ,
                     float *sp2  , int   *line2, float *sp3   , int   *line3,
                     int   *ipat2, float *xsd2 , float *ysd2  ,
                     int   *hold , float *elev2, float *depth2, float *tuh2,
                     int   *is   , int   *ir   , int   *ig)
{
  geomio_frou_read_pp_card (fpoint(), &ipp, err, msg,
                            sp2  , line2, sp3   , line3,
                            ipat2, xsd2 , ysd2  ,
                            hold , elev2, depth2, tuh2,
                            is   , ir   , ig);
}



void GeomioWrapper::writePpCard
                    (int    ipp  , int   *err  , char  *msg   ,
                     float  sp2  , int    line2, float  sp3   , int    line3,
                     int    ipat2, float  xsd2 , float  ysd2  ,
                     int    hold , float  elev2, float  depth2, float  tuh2,
                     int    is   , int    ir   , int    ig)
{
  geomio_frou_write_pp_card (fpoint(), &ipp, err, msg,
                             &sp2  , &line2, &sp3   , &line3,
                             &ipat2, &xsd2 , &ysd2  ,
                             &hold , &elev2, &depth2, &tuh2,
                             &is   , &ir   , &ig);
}



void GeomioWrapper::readZt1Card
                    (int         izt1, int   *err  , char  *msg,
                     char       *ccc1, float *sss1 , float *sss1a, int *lll1)
{
  geomio_frou_read_zt1_card (fpoint(), &izt1, err, msg,
                             ccc1, sss1 , sss1a, lll1);
}



void GeomioWrapper::writeZt1Card
                    (int         izt1, int   *err  , char  *msg,
                     const char *ccc1, float  sss1 , float  sss1a, int  lll1)
{
  geomio_frou_write_zt1_card (fpoint(), &izt1, err, msg,
                              ccc1, &sss1 , &sss1a, &lll1);
}



void GeomioWrapper::readZt2Card
                    (int         izt2, int   *err  , char  *msg,
                     char       *ccc2, float *rrr2 , float *rrr2a, int *lll2)
{
  geomio_frou_read_zt2_card (fpoint(), &izt2, err, msg,
                             ccc2, rrr2 , rrr2a, lll2);
}



void GeomioWrapper::writeZt2Card
                    (int         izt2, int   *err  , char  *msg,
                     const char *ccc2, float  rrr2 , float  rrr2a, int  lll2)
{
  geomio_frou_write_zt2_card (fpoint(), &izt2, err, msg,
                              ccc2, &rrr2 , &rrr2a, &lll2);
}



void GeomioWrapper::readZt3Card
                    (int         izt3 , int *err  , char *msg,
                     char       *ccc3 , int *iggg3, int  *iggg3a,
                     int        *ittt3, int *ittt3a)
{
  geomio_frou_read_zt3_card (fpoint(), &izt3, err, msg,
                             ccc3 , iggg3, iggg3a,
                             ittt3, ittt3a);
}



void GeomioWrapper::writeZt3Card
                    (int         izt3 , int *err  , char *msg,
                     const char *ccc3 , int  iggg3, int   iggg3a,
                     int         ittt3, int  ittt3a)
{
  geomio_frou_write_zt3_card (fpoint(), &izt3, err, msg,
                               ccc3 , &iggg3, &iggg3a,
                              &ittt3, &ittt3a);
}



void GeomioWrapper::readZt4Card
                    (int         izt4, int   *err , char  *msg,
                     char       *ccc4, float *sss4, float *sss4a,
                     int        *lll4, float *rrr4, float *rrr4a, int *lll4a)
{
  geomio_frou_read_zt4_card (fpoint(), &izt4, err, msg,
                             ccc4, sss4, sss4a,
                             lll4, rrr4, rrr4a, lll4a);
}



void GeomioWrapper::writeZt4Card
                    (int         izt4, int   *err , char  *msg,
                     const char *ccc4, float  sss4, float  sss4a,
                     int         lll4, float  rrr4, float  rrr4a, int  lll4a)
{
  geomio_frou_write_zt4_card (fpoint(), &izt4, err, msg,
                               ccc4, &sss4, &sss4a,
                              &lll4, &rrr4, &rrr4a, &lll4a);
}



//------------------ helper augment parameters ----------------------------//
//------------------ helper augment parameters ----------------------------//
//------------------ helper augment parameters ----------------------------//


void GeomioWrapper::helperAugmentParameters (const char *keyword,
                                             const char **fields,
                                             int nfields)
{
  assert(numHeaderSections() > 0);
  char keep[55];
  char section[55];
  strcpy(keep, secname());
  strcpy(section, getScalarString(keyword));
  setActiveHeaderSection(section);

  for(int icol = 0; icol < nfields; icol++)
      {
      int colindex = findAddField(fields[icol]);
      }
  if(isInput())
      {
      if(strcmp(keep, "FOREIGN") == 0)
          {
          setNcolumns (0);
          setNilstring("n");
          setEncoding ("ascii");
          setFirstline(1);
          }
      }
  else    // if(isOutput())
      {
      setNcolumns (numFields());
      setNilstring("n");
      }

  setActiveHeaderSection(keep);
}



void GeomioWrapper::helperDeactivateFields  (const char *keyword)
{
  if(numHeaderSections() == 0) return;
  if(isOutput()) return;
  char keep[55];
  char section[55];
  strcpy(keep, secname());
  strcpy(section, getScalarString(keyword));
  if(strcmp(section, "") == 0) return;
  setActiveHeaderSection(section);

  for(int indx = 0; indx < numFields(); indx++)
      {
      setElementLogical("field_is_active", indx, FALSE);
      setElementLogical("skip"           , indx, TRUE);
      }
  setActiveHeaderSection(keep);
}



void GeomioWrapper::helperMarkActiveFields  (const char *keyword,
                                             const char **fields,
                                             int nfields)
{
  if(numHeaderSections() == 0) return;
  if(isOutput()) return;
  char keep[55];
  char section[55];
  strcpy(keep, secname());
  strcpy(section, getScalarString(keyword));
  if(strcmp(section, "") == 0) return;
  setActiveHeaderSection(section);

  int ncolumns = getNcolumns();

  for(int indx = 0; indx < numFields(); indx++)
      {
      const char *field = getField(indx);
      int active = (nfields == 0);
      for(int indx2 = 0; indx2 < nfields; indx2++)
          {
          if(strcmp(field, fields[indx2]) == 0)
              {
              active = TRUE;
              break;
              }
          }
      int skip = (!active || indx >= ncolumns);
      setElementLogical("field_is_active", indx, active);
      setElementLogical("skip"           , indx, skip);
      }
  setActiveHeaderSection(keep);
}



int  GeomioWrapper::helperRequired (const char *keyword,
                                    const char *nkeyword,
                                    const char *skeyword,
                                    const char **required,
                                    int nrequired, char *msg)
{
  assert(numHeaderSections() > 0);
  if(strcmp(getScalarString("encoding"), "oldcps") == 0) return FALSE;
  char keep[55];
  char section[55];
  strcpy(keep, secname());
  strcpy(section, getScalarString(keyword));
  int ncards = getScalarInteger(nkeyword);
  int skip   = getScalarLogical(skeyword);
  if(ncards == 0 || ncards == INIL) return FALSE;
  if(skip)                          return FALSE;
  setActiveHeaderSection(section);

  int ncolumns = getNcolumns();
  int error = FALSE;
  for(int indx = 0; indx < nrequired; indx++)
      {
      int colindx = findField(required[indx]);
      if(colindx < 0 || colindx >= ncolumns || inputFieldIsSkipped(colindx))
          {
          char required2[44];
          str_to_upper(required2, (char*)required[indx]);
          strcpy(msg, "required field ");
          strcat(msg, required2);
          if(isInput()) strcat(msg, " not specified for input ");
          else          strcat(msg, " not specified for output ");
          strcat(msg, keyword);
          error = TRUE;
          break;
          }
      }

  setActiveHeaderSection(keep);
  return error;
}



void GeomioWrapper::helperSkip(const char *skeyword, int skip)
{
  if(numHeaderSections() == 0) return;
  char keep[55];
  char section[55];
  strcpy(keep, secname());
  switch(_situation)
      {
      case SITUATION_GEOMETRY: strcpy(section, "GEOMETRY"); break;
      case SITUATION_GENERIC : strcpy(section, "GENERIC" ); break;
      case SITUATION_FOREIGN : strcpy(section, "FOREIGN" ); break;
      default:                 assert(FALSE);
      }
  setActiveHeaderSection (section);
  setScalarLogical       (skeyword, skip);
  setActiveHeaderSection (keep);
}



//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//

