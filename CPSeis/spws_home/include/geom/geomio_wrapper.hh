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

//-------------------------- geomio_wrapper.hh ------------------------------//
//-------------------------- geomio_wrapper.hh ------------------------------//
//-------------------------- geomio_wrapper.hh ------------------------------//

//              header file for the GeomioWrapper class
//              derived from the PjarFileSupport class
//                        subdirectory geom


#ifndef _GEOMIO_WRAPPER_HH_
#define _GEOMIO_WRAPPER_HH_

#include "oprim/pjar_file_support.hh"


class GeomioWrapper : public PjarFileSupport
{

//------------------------------- data ----------------------------------//
//------------------------------- data ----------------------------------//
//------------------------------- data ----------------------------------//

public:

  enum { CHOICE_NONE = 0, CHOICE_LD, CHOICE_RP, CHOICE_PP,
         CHOICE_ZT1, CHOICE_ZT2, CHOICE_ZT3, CHOICE_ZT4 };

private:

  int _situation;


//----------------------------- functions -------------------------------//
//----------------------------- functions -------------------------------//
//----------------------------- functions -------------------------------//

public:  // io must be 0 for input and 1 for output.

           GeomioWrapper (int io);
  virtual ~GeomioWrapper ();

private:  // keyword is the keyword for the section name for ld, etc. cards.

  void helperAugmentParameters (const char *keyword,
                                const char **fields, int nfields);

  void helperMarkActiveFields  (const char *keyword,
                                const char **fields, int nfields);

  void helperDeactivateFields  (const char *keyword);

  int  helperRequired          (const char *keyword,
                                const char *nkeyword,
                                const char *skeyword,
                                const char **required, int nrequired,
                                char *msg);

  void helperSkip              (const char *skeyword, int skip);


//---------------- virtual functions overriding PjarFileSupport -----------//
//---------------- virtual functions overriding PjarFileSupport -----------//
//---------------- virtual functions overriding PjarFileSupport -----------//

protected:

  virtual int   virtualValidate (const char *filename, char *msg);
  virtual void  virtualAugment  ();
  virtual int   virtualVerify   (char *msg);

public:

  virtual int   allowOldcps ()  const  { return TRUE; }
  virtual int   allowAscii  ()  const  { return TRUE; }
  virtual int   allowBinary ()  const  { return FALSE; }
  virtual int   allowHybrid ()  const  { return FALSE; }

  virtual int   showFieldtypes      ()  const  { return FALSE; }
  virtual int   showUnits           ()  const  { return FALSE; }
  virtual int   showHdrs            ()  const  { return FALSE; }
  virtual int   showDefaults        ()  const  { return TRUE ; }
  virtual int   showComments        ()  const  { return FALSE; }
  virtual int   showWidths          ()  const  { return TRUE ; }
  virtual int   showConverters      ()  const  { return TRUE ; }
  virtual int   showMaxchars        ()  const  { return TRUE ; }

  virtual int   switchFields        (int indx)  { return   2; }
  virtual int   switchColumnNumbers (int indx)  { return   1; }
  virtual int   switchFieldtypes    (int indx)  { return   1; }
  virtual int   switchUnits         (int indx)  { return   1; }
  virtual int   switchHdrs          (int indx)  { return   1; }
  virtual int   switchDefaults      (int indx)  { return   1; }

/****
  ////// not needed:
  virtual void  stepFieldtype       (int indx, int step);
  virtual void  stepUnits           (int indx, int step);
  virtual void  stepHdr             (int indx, int step);
  virtual void  stepDefault         (int indx, int step);
****/


//-------------- new functions accessing the pickle jar -----------------//
//-------------- new functions accessing the pickle jar -----------------//
//-------------- new functions accessing the pickle jar -----------------//

public:

  int  getGenericInputChoice      ();
  void stepGenericInputChoice     (int step);
  int  needGenericInputChoice     ();
  int  needGenericInputParameters ();

  void skipLdCards  (int skip)  { helperSkip("skip_ld" , skip); }
  void skipRpCards  (int skip)  { helperSkip("skip_rp" , skip); }
  void skipPpCards  (int skip)  { helperSkip("skip_pp" , skip); }
  void skipZt1Cards (int skip)  { helperSkip("skip_zt1", skip); }
  void skipZt2Cards (int skip)  { helperSkip("skip_zt2", skip); }
  void skipZt3Cards (int skip)  { helperSkip("skip_zt3", skip); }
  void skipZt4Cards (int skip)  { helperSkip("skip_zt4", skip); }


//------------------------ read and write disk files --------------------//
//------------------------ read and write disk files --------------------//
//------------------------ read and write disk files --------------------//

public:   // read and write disk file.
          // openInputFile reads file using parameters from pickle jar.
          // openOutputFile uses and writes parameters from pickle jar.
          // these set msg.
          // these return error = TRUE or FALSE.

  int   openInputFile  (const char *filename, char *msg);
  int   openOutputFile (const char *filename, char *msg);
  void  closeFile      ();

public:   // read or write a card.
          // these set msg.
          // these set err = STATUS_OK, STATUS_ERROR, STATUS_EOF.

  void readLdCard   (int    ild , int   *err  , char  *msg ,
                     float *sp  , float *dist , float *xloc, float *yloc,
                     float *elev, float *depth, float *tuh ,
                     float *tr  , float *ts   , float *xsd , float *ysd,
                     float *elsd, int   *line);

  void writeLdCard  (int    ild , int   *err  , char  *msg ,
                     float  sp  , float  dist , float  xloc, float  yloc,
                     float  elev, float  depth, float  tuh ,
                     float  tr  , float  ts   , float  xsd , float  ysd,
                     float  elsd, int    line);

  void readRpCard   (int    irp  , int        *err  , char  *msg,
                     int   *ipat1, char       *flag , float *sp1, int *line1,
                     int   *nx   , int        *ixinc, int   *ny , int *iyinc,
                     float *xsdi , float      *ysdi , float *elsd1);

  void writeRpCard  (int    irp  , int        *err  , char  *msg,
                     int    ipat1, const char *flag , float  sp1, int  line1,
                     int    nx   , int         ixinc, int    ny , int  iyinc,
                     float  xsdi , float       ysdi , float  elsd1);

  void readPpCard   (int    ipp  , int   *err  , char  *msg   ,
                     float *sp2  , int   *line2, float *sp3   , int   *line3,
                     int   *ipat2, float *xsd2 , float *ysd2  ,
                     int   *hold , float *elev2, float *depth2, float *tuh2,
                     int   *is   , int   *ir   , int   *ig);

  void writePpCard  (int    ipp  , int   *err  , char  *msg   ,
                     float  sp2  , int    line2, float  sp3   , int    line3,
                     int    ipat2, float  xsd2 , float  ysd2  ,
                     int    hold , float  elev2, float  depth2, float  tuh2,
                     int    is   , int    ir   , int    ig);

  void readZt1Card  (int         izt1, int   *err  , char  *msg,
                     char       *ccc1, float *sss1 , float *sss1a, int *lll1);

  void writeZt1Card (int         izt1, int   *err  , char  *msg,
                     const char *ccc1, float  sss1 , float  sss1a, int  lll1);

  void readZt2Card  (int         izt2, int   *err  , char  *msg,
                     char       *ccc2, float *rrr2 , float *rrr2a, int *lll2);

  void writeZt2Card (int         izt2, int   *err  , char  *msg,
                     const char *ccc2, float  rrr2 , float  rrr2a, int  lll2);

  void readZt3Card  (int         izt3 , int *err  , char *msg,
                     char       *ccc3 , int *iggg3, int  *iggg3a,
                     int        *ittt3, int *ittt3a);

  void writeZt3Card (int         izt3 , int *err  , char *msg,
                     const char *ccc3 , int  iggg3, int   iggg3a,
                     int         ittt3, int  ittt3a);

  void readZt4Card  (int         izt4, int   *err , char  *msg,
                     char       *ccc4, float *sss4, float *sss4a,
                     int        *lll4, float *rrr4, float *rrr4a, int *lll4a);

  void writeZt4Card (int         izt4, int   *err , char  *msg,
                     const char *ccc4, float  sss4, float  sss4a,
                     int         lll4, float  rrr4, float  rrr4a, int  lll4a);


//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//

};

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
