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

//-------------------------- velio_wrapper.cc ------------------------------//
//-------------------------- velio_wrapper.cc ------------------------------//
//-------------------------- velio_wrapper.cc ------------------------------//

//             implementation file for the VelioWrapper class
//                 derived from the PjarFileSupport class
//                          subdirectory vf


#include "vf/velio_wrapper.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_utilities.hh"
#include "str.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


static const int   nfields  = 12;
static const char *fields[] =
                       { "abscissa", "ordinate", "xcoord", "ycoord",
                         "veltype", "velname", "project", "line",
                         "rdate", "pdate", "userid", "comment" };

static const int   nrequired  = 2;
static const char *required[] = { "abscissa", "ordinate" };


//------------------ constructor and destructor -----------------------//
//------------------ constructor and destructor -----------------------//
//------------------ constructor and destructor -----------------------//


VelioWrapper::VelioWrapper(int io, const char *defname)
         : PjarFileSupport (io, defname, fields, nfields, required, nrequired)
{
}



VelioWrapper::~VelioWrapper()
{
  closeFile();
}



//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//
//-------------------- fortran spelling adjustments -----------------------//


#if NEED_UNDERSCORE
#define velio_frou_verify                  velio_frou_verify_
#define velio_frou_augment                 velio_frou_augment_
#define velio_frou_open_read               velio_frou_open_read_
#define velio_frou_open_write              velio_frou_open_write_
#define velio_frou_open_foreign            velio_frou_open_foreign_
#define velio_frou_close                   velio_frou_close_
#define velio_frou_read_velfun             velio_frou_read_velfun_
#define velio_frou_write_velfun            velio_frou_write_velfun_
#elif NEED_CAPITALS
#define velio_frou_verify                  VELIO_FROU_VERIFY
#define velio_frou_augment                 VELIO_FROU_AUGMENT
#define velio_frou_open_read               VELIO_FROU_OPEN_READ  
#define velio_frou_open_write              VELIO_FROU_OPEN_WRITE 
#define velio_frou_open_foreign            VELIO_FROU_OPEN_FOREIGN
#define velio_frou_close                   VELIO_FROU_CLOSE
#define velio_frou_read_velfun             VELIO_FROU_READ_VELFUN
#define velio_frou_write_velfun            VELIO_FROU_WRITE_VELFUN
#endif


//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//
//-------------------------- fortran prototypes ---------------------------//

extern "C" {

void velio_frou_verify       (F90Pointer *pjar  , const char *secname,
                              int        *err   , char       *msg);

void velio_frou_augment      (F90Pointer *pjar  , const char *secname);

void velio_frou_open_read    (F90Pointer *fpoint, const char *filename,
                              F90Pointer *pjar  , const char *secname,
                              int        *err   , char       *msg);

void velio_frou_open_write   (F90Pointer *fpoint, const char *filename,
                              F90Pointer *pjar  , const char *secname,
                              int        *err   , char       *msg);

void velio_frou_open_foreign (F90Pointer *fpoint, const char *filename,
                              F90Pointer *pjar  , const char *secname,
                              int        *err   , char       *msg);

void velio_frou_close        (F90Pointer *fpoint);

void velio_frou_read_velfun  (F90Pointer  *fpoint ,
                              float       *xcoord , float       *ycoord,
                              int         *npicks , const int   *maxpicks,
                              float       *tpicks , float       *vpicks,
                              int         *err    , char        *msg,
                              char        *velname, char        *veltype,
                              char        *project, char        *line,
                              char        *rdate  , char        *pdate,
                              char        *userid , char        *comment);

void velio_frou_write_velfun (F90Pointer  *fpoint ,
                              const float *xcoord , const float *ycoord,
                              const int   *npicks ,
                              const float *tpicks , const float *vpicks,
                              const int   *err    , const char  *msg,
                              const char  *velname, const char  *veltype,
                              const char  *project, const char  *line,
                              const char  *rdate  , const char  *pdate,
                              const char  *userid , const char  *comment);

}   // end extern "C"



//---------------- virtual functions overriding PjarFileSupport -----------//
//---------------- virtual functions overriding PjarFileSupport -----------//
//---------------- virtual functions overriding PjarFileSupport -----------//


const char *VelioWrapper::getDescription (int indx)
{
  static const char time    [] = "time";
  static const char depth   [] = "depth";
  static const char thick   [] = "layer thickness";
  static const char vnmo    [] = "NMO velocity";
  static const char vrms    [] = "RMS velocity";
  static const char vav     [] = "average velocity";
  static const char vint    [] = "interval velocity";
  static const char abscissa[] = "time (or depth)";
  static const char ordinate[] = "velocity (or depth)";
  static const char xcoord  [] = "X coordinate";
  static const char ycoord  [] = "Y coordinate";
  static const char veltype [] = "type of function";
  static const char velname [] = "name of function";
  static const char comment [] = "generic comment field";

  const char *field = getField(indx);

  if(strcmp(field, "abscissa") == 0)
      {
      if(strcmp(defname(), "ATTRIBUTE") == 0)
          {
          return abscissa;
          }
      else
          {
          int indx2 = findField("veltype");
          if(indx2 < getNcolumns() && !inputFieldIsSkipped(indx2))
                                                          return abscissa;
          int type = getType();
          if(type < 0) return abscissa;
          if(VfUtilities::abscissaIsTime     (type)) return time;
          if(VfUtilities::abscissaIsDepth    (type)) return depth;
          if(VfUtilities::abscissaIsThickness(type)) return thick;
          return abscissa;
          }
      }

  if(strcmp(field, "ordinate") == 0)
      {
      if(strcmp(defname(), "ATTRIBUTE") == 0)
          {
          return getAttributeName();
          }
      else
          {
          int indx2 = findField("veltype");
          if(indx2 < getNcolumns() && !inputFieldIsSkipped(indx2))
                                                          return ordinate;
          int type = getType();
          if(type < 0) return ordinate;
          if(VfUtilities::ordinateIsVNMO (type)) return vnmo;
          if(VfUtilities::ordinateIsVRMS (type)) return vrms;
          if(VfUtilities::ordinateIsVAV  (type)) return vav;
          if(VfUtilities::ordinateIsVINT (type)) return vint;
          if(VfUtilities::ordinateIsDepth(type)) return depth;
          return ordinate;
          }
      }

  if(strcmp(field, "xcoord" ) == 0) return xcoord;
  if(strcmp(field, "ycoord" ) == 0) return ycoord;
  if(strcmp(field, "veltype") == 0) return veltype;
  if(strcmp(field, "velname") == 0) return velname;
  if(strcmp(field, "project") == 0) return comment;
  if(strcmp(field, "line"   ) == 0) return comment;
  if(strcmp(field, "rdate"  ) == 0) return comment;
  if(strcmp(field, "pdate"  ) == 0) return comment;
  if(strcmp(field, "userid" ) == 0) return comment;
  if(strcmp(field, "comment") == 0) return comment;
  return CNIL;
}



int VelioWrapper::virtualValidate (const char *filename, char *msg)
{
  int err;
  velio_frou_open_read(fpoint(), filename, pjar(), defname(), &err, msg);
  velio_frou_close    (fpoint());

  if(err == STATUS_OK)
      {
      int   nhx     = getScalarInteger("nhx"    );
      int   nhy     = getScalarInteger("nhy"    );
      int   nfun    = getScalarInteger("nfun"   );
      float nmosign = getScalarFloat  ("nmosign");
      float nmoexp  = getScalarFloat  ("nmoexp" );
      int   order   = VfUtilities::deriveMoveoutOrder(nmosign, nmoexp);

      sprintf(msg, "#velfuns=%d  nhx=%d  nhy=%d  order=%d",
                                nfun, nhx, nhy, order);
      }
  return (err != STATUS_OK);
}



void VelioWrapper::virtualAugment ()
{
  velio_frou_augment (pjar(), secname());
}



int VelioWrapper::virtualVerify (char *msg)
{
  int err;
  velio_frou_verify (pjar(), secname(), &err, msg);
  return (err != STATUS_OK);
}



void VelioWrapper::setDefault (int indx, const char *value)
{
  const char *field = getField(indx);
  if(strcmp(field, "veltype") == 0)
      {
      setTypeFromSymbol(value);
      }
  else if(strcmp(field, "xcoord") == 0)
      {
      float xcoord;
      int istat;
      str_ss2ff((char*)value, &xcoord, &istat);
      if(istat == 1) setDefaultXcoord(xcoord);
      }
  else if(strcmp(field, "ycoord") == 0)
      {
      float ycoord;
      int istat;
      str_ss2ff((char*)value, &ycoord, &istat);
      if(istat == 1) setDefaultYcoord(ycoord);
      }
  else
      {
      PjarFileSupport::setDefault(indx, value);
      }
}



void VelioWrapper::setFieldtype (int indx, const char *value)
{
  const char *field = getField(indx);
  if(strcmp(field, "ordinate") == 0)
      {
      setAttributeName(value);
      }
  else
      {
      PjarFileSupport::setFieldtype(indx, value);
      }
}



void VelioWrapper::setUnits (int indx, const char *value)
{
  const char *field = getField(indx);
  if(strcmp(field, "ordinate") == 0)
      {
      setAttributeUnits(value);
      }
  else if(strcmp(field, "abscissa") == 0)
      {
      setTimeDepthUnits(value);
      }
  else
      {
      PjarFileSupport::setUnits(indx, value);
      }
}



void VelioWrapper::setHdr (int indx, int value)
{
  const char *field = getField(indx);
  if(strcmp(field, "xcoord") == 0)
      {
      setNhx(value);
      }
  else if(strcmp(field, "ycoord") == 0)
      {
      setNhy(value);
      }
  else
      {
      PjarFileSupport::setHdr(indx, value);
      }
}



//------------------------ get values ---------------------------------//
//------------------------ get values ---------------------------------//
//------------------------ get values ---------------------------------//



const char *VelioWrapper::getName()
{
  return getScalarString("name");
}


float VelioWrapper::getDefaultXcoord()
{
  return getComboDefaultFloat("default_xcoord", "xcoord");
}


float VelioWrapper::getDefaultYcoord()
{
  return getComboDefaultFloat("default_ycoord", "ycoord");
}


int VelioWrapper::getType()
{
  return VfUtilities::getTypeFromSymbol(getTypeSymbol());
}


const char *VelioWrapper::getTypeSymbol()
{
  return getComboDefaultString("default_veltype", "veltype");
}


int VelioWrapper::getNhx()
{
  return getComboHdr("nhx", "xcoord");
}


int VelioWrapper::getNhy()
{
  return getComboHdr("nhy", "ycoord");
}


int VelioWrapper::getMoveoutOrder()
{
  return VfUtilities::deriveMoveoutOrder(getNhosign(), getNhoexp());
}


float VelioWrapper::getNhosign()
{
  return getScalarFloat("nmosign");
}


float VelioWrapper::getNhoexp()
{
  return getScalarFloat("nmoexp");
}


int VelioWrapper::getDistanceUnits()
{
  return VfUtilities::getUnitsFromSymbol(getUnitsSymbol());
}


const char *VelioWrapper::getUnitsSymbol()
{
  return getScalarString("dunits");
}


const char *VelioWrapper::getAttributeName()
{
  return getComboFieldtype("attname", "ordinate");
}


const char *VelioWrapper::getAttributeUnits()
{
  return getComboUnits("attunits", "ordinate");
}


const char *VelioWrapper::getTimeDepthUnits()
{
  return getComboUnits("tdunits", "abscissa");
}



//------------------------ set values ---------------------------------//
//------------------------ set values ---------------------------------//
//------------------------ set values ---------------------------------//


void VelioWrapper::setName (const char *value)
{
  setScalarString("name", value);
}



void VelioWrapper::setDefaultXcoord (float value)
{
  setComboDefaultFloat("default_xcoord", "xcoord", value);
}


void VelioWrapper::setDefaultYcoord (float value)
{
  setComboDefaultFloat("default_ycoord", "ycoord", value);
}



void VelioWrapper::setType (int value)
{
  if(value == -1)
      {
      setComboDefaultString("default_veltype", "veltype", "none");
      }
  else if(value >= FIRSTTYPE && value <= LASTTYPE)
      {
      setComboDefaultString
             ("default_veltype", "veltype", VfUtilities::typeSymbol(value));
      }
  else
      {
      assert(FALSE);
      }
}



void VelioWrapper::setTypeFromSymbol (const char *value)
{
  setType(VfUtilities::getTypeFromSymbol(value));
}



void VelioWrapper::setNhx (int value)
{
  setComboHdr("nhx", "xcoord", value);
}



void VelioWrapper::setNhy (int value)
{
  setComboHdr("nhy", "ycoord", value);
}



void VelioWrapper::setMoveoutOrder (int value)
{
  setScalarFloat("nmosign", VfUtilities::deriveNhosign(value));
  setScalarFloat("nmoexp" , VfUtilities::deriveNhoexp (value));
}



void VelioWrapper::setDistanceUnits (int value)
{
  setScalarString("dunits", VfUtilities::getSymbolFromUnits(value));
}



void VelioWrapper::setUnitsFromSymbol (const char *value)
{
  setScalarString("dunits", value);
}



void VelioWrapper::setAttributeName (const char *value)
{
  setComboFieldtype("attname", "ordinate", value);
}



void VelioWrapper::setAttributeUnits (const char *value)
{
  setComboUnits("attunits", "ordinate", value);
}



void VelioWrapper::setTimeDepthUnits (const char *value)
{
  setComboUnits("tdunits", "abscissa", value);
}



//----------------------- read and write disk files -----------------------//
//----------------------- read and write disk files -----------------------//
//----------------------- read and write disk files -----------------------//


int   VelioWrapper::openInputFile       (const char *filename, char *msg)
{
  int err;
  velio_frou_open_foreign(fpoint(), filename, pjar(), secname(), &err, msg);

  if(err != STATUS_OK) closeFile();
  return (err != STATUS_OK);
}



int   VelioWrapper::openOutputFile       (const char *filename, char *msg)
{
  int err;
  velio_frou_open_write(fpoint(), filename, pjar(), secname(), &err, msg);

  if(err != STATUS_OK) closeFile();
  return (err != STATUS_OK);
}



void  VelioWrapper::closeFile()
{
  velio_frou_close (fpoint());
}



//------------------- read and write velocity function --------------------//
//------------------- read and write velocity function --------------------//
//------------------- read and write velocity function --------------------//


void VelioWrapper::readVelfun (float *xcoord , float *ycoord,
                               int   *npicks , int    maxpicks,
                               float *tpicks , float *vpicks,
                               int   *err    , char  *msg,
                               char  *velname, char  *veltype,
                               char  *project, char  *line,
                               char  *rdate  , char  *pdate,
                               char  *userid , char  *comment)
{
  velio_frou_read_velfun (fpoint(), xcoord, ycoord, npicks, &maxpicks,
                          tpicks, vpicks,
                          err, msg, velname, veltype, project, line,
                          rdate, pdate, userid, comment);
}



void VelioWrapper::writeVelfun (      float  xcoord ,       float  ycoord,
                                      int    npicks ,
                                const float *tpicks , const float *vpicks,
                                      int   *err    ,       char  *msg,
                                const char  *velname, const char  *veltype,
                                const char  *project, const char  *line,
                                const char  *rdate  , const char  *pdate,
                                const char  *userid , const char  *comment)
{
  velio_frou_write_velfun (fpoint(), &xcoord, &ycoord, &npicks,
                           tpicks, vpicks,
                           err, msg, velname, veltype, project, line,
                           rdate, pdate, userid, comment);
}



//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//

