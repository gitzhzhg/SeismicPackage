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

//------------------------- pjar_file_support.cc ---------------------------//
//------------------------- pjar_file_support.cc ---------------------------//
//------------------------- pjar_file_support.cc ---------------------------//

//           implementation file for the PjarFileSupport class
//              derived from the FileSupportInterface class
//                          subdirectory oprim


#include "oprim/pjar_file_support.hh"
#include "oprim/pjar_wrapper.hh"
#include "oprim/history_cards.hh"
#include "oprim/generic_cards.hh"
#include "geom/grid_transform.hh"
#include "named_constants.h"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//----------------------- constructor and destructor -------------------//
//----------------------- constructor and destructor -------------------//
//----------------------- constructor and destructor -------------------//


PjarFileSupport::PjarFileSupport(int io, const char *defname,
                                 const char **fields, int nfields,
                                 const char **required, int nrequired)
          :
             FileSupportInterface(),
               _io          (io),
               _fields      (fields),
               _nfields     (nfields),
               _required    (required),
               _nrequired   (nrequired),
               _pjar        (NULL),
               _cards       (NULL)
{
  assert(sizeof(INTEGER) == sizeof(int   ));
  assert(sizeof(REAL   ) == sizeof(float ));
  assert(sizeof(DOUBLE ) == sizeof(double));
  memset(&_fpoint, 0, sizeof(_fpoint));

  assert(defname);
  assert(defname[0] != '\0' );
  assert(defname[0] != ' '  );
  assert(_io == 0 || _io == 1);
  str_to_upper(_defname, (char*)defname);
  strcpy      (_secname, "");

  assert((_fields   && _nfields   > 0) || (!_fields   && _nfields   == 0));
  assert((_required && _nrequired > 0) || (!_required && _nrequired == 0));

  _pjar  = new PjarWrapper();
  _cards = new GenericCards();
}



PjarFileSupport::~PjarFileSupport()
{
  delete _pjar;
  delete _cards;
}



//------------------------- print and clear -------------------------------//
//------------------------- print and clear -------------------------------//
//------------------------- print and clear -------------------------------//


void PjarFileSupport::print()
{
  _pjar->print();
}


void PjarFileSupport::clear()
{
  _pjar->clear();
  strcpy(_secname, "");
}


//---------------------- get f90 pointer to pjar --------------------------//
//---------------------- get f90 pointer to pjar --------------------------//
//---------------------- get f90 pointer to pjar --------------------------//

        // protected.


F90Pointer *PjarFileSupport::pjar()
{
  return _pjar->getFpoint();
}



//---------------------- get and put history cards ---------------------//
//---------------------- get and put history cards ---------------------//
//---------------------- get and put history cards ---------------------//


int         PjarFileSupport::numHistoryCards   ()
{
  if(_pjar->numSections() == 0) return 0;
  int indx = _pjar->findSection("history");
  if(indx < 0) return 0;
  _pjar->chooseSection("history");
  return _pjar->numCards();
}



const char *PjarFileSupport::getHistoryCard    (int indx)
{
  static char card[111];
  if(_pjar->numSections() == 0) return CNIL;
  int indx2 = _pjar->findSection("history");
  if(indx2 < 0) return CNIL;
  _pjar->chooseSection("history");
  _pjar->getCard(indx, card);
  return card;
}



void PjarFileSupport::getHistoryCardsFromPjar (HistoryCards *history)
{
  history->deleteHistoryCards();
  if(_pjar->numSections() == 0) return;
  int indx = _pjar->findSection("history");
  if(indx < 0) return;
  history->getHistoryCardsFromPjar(_pjar);
}



void PjarFileSupport::putHistoryCardsIntoPjar (const HistoryCards *history,
                                               int skiphist)
{
  history->putHistoryCardsIntoPjar(_pjar, skiphist);
}



//---------------------------- filetype ---------------------------------//
//---------------------------- filetype ---------------------------------//
//---------------------------- filetype ---------------------------------//


#define OLABEL  "old style CPS "
#define ALABEL  "self defining ascii "
#define BLABEL  "self defining binary "
#define HLABEL  "self defining hybrid "


const char *PjarFileSupport::filetype()
{
  static char filetype[77];
  const char *encoding = getEncoding();
  if     (strcmp(encoding, "oldcps") == 0) strcpy(filetype, OLABEL);
  else if(strcmp(encoding, "ascii" ) == 0) strcpy(filetype, ALABEL);
  else if(strcmp(encoding, "binary") == 0) strcpy(filetype, BLABEL);
  else if(strcmp(encoding, "hybrid") == 0) strcpy(filetype, HLABEL);
  else                                     strcpy(filetype, "");
  if(strcmp(_secname, _defname) == 0)
      {
      char defname[33];
      str_to_lower(defname, _defname);
      int length = strlen(defname);
      for(int i = 0; i < length; i++)
          {
          if(defname[i] == '_') defname[i] = ' ';
          }
      strcat(filetype, defname);
      strcat(filetype, " file");
      }
  else if(strcmp(_secname, "FOREIGN") == 0)
      {
      strcpy(filetype, "file of questionable type");
      }
  else
      {
      strcat(filetype, "file");
      }
  return filetype;
}



//------------------------- validate file ----------------------------------//
//------------------------- validate file ----------------------------------//
//------------------------- validate file ----------------------------------//

     // public.
     // should be called right after validating an input file.
     // uses _fields and _nfields.
     // this function is legitimate when the overriding class needs
     //  only one section (other than history), and any of the sections
     //  in the validated file might be the desired one.

     // if _FIELDS and _NFIELDS are specified, these fields will be flagged
     //  as active (if they are found) and to be skipped (if they are not
     //  found).  if they are not specified, all fields will be considered
     //  active and initialized not to be skipped.  columns > ncolumns are
     //  always initialized to be skipped.

     // if retain is true, the previous pickle jar contents are retained
     // if the previous file and current file are both foreign files.


int PjarFileSupport::validateFile (const char *filename, char *msg, int retain)
{
  _cards->readCardsFromFile(filename,300);
  int error;
  if(isInput())
      {
      if(strcmp(_secname, "FOREIGN") != 0) retain = FALSE;
      PjarWrapper *keep = NULL;
      if(retain)
          {
          keep = new PjarWrapper();
          keep->copy(_pjar);
          }

      strcpy(_secname, "");
      error = virtualValidate(filename, msg);
      int nsections = _pjar->numSections();

      if(retain)
          {
          if(nsections == 1)
              {
              char secname[44];
              _pjar->chooseSection(0);
              _pjar->getSecname(secname);
              if(strcmp(secname, "FOREIGN") == 0) _pjar->copy(keep);
              nsections = _pjar->numSections();
              }
          delete keep;
          }

      for(int indx = 0; indx < nsections; indx++)
          {
          _pjar->chooseSection(indx);
          _pjar->getSecname(_secname);
          const char *encoding = getEncoding();

          if(strcmp(encoding, "") == 0) continue;

          int ncolumns = getNcolumns();

          int icol;
          for(icol = 0; icol < _nfields; icol++)
              {
              findAddField(_fields[icol]);
              }
/*****
          int address1 = getElementInteger("address", 0);
          int address2 = getElementInteger("address", 1);
          if(address1 == INIL) address1 = 0;
          if(address2 == INIL) address2 = 0;
*****/
          virtualAugment();
/*****
          setElementInteger("address", 0, address1);
          setElementInteger("address", 1, address2);

          // Note: The above code to restore the address is needed
          // because virtualAugment (in fioutil) resets the address
          // to zero.  The reason is that the augment function which
          // calls fioutil is intended only while preparing to write
          // the file, whereas in this case we may also be preparing
          // to read the file.  A fix has been put into fioutil; when
          // the new version of fioutil is put into prodlib the above
          // code to restore the address can be removed.
*****/

          setNcolumns(ncolumns);     

          if(_pjar->keywordPresent("field_is_active")) continue;
          for(icol = 0; icol < numFields(); icol++)
              {
              const char *field = getField(icol);
              int active = (_nfields == 0);
              for(int icol2 = 0; icol2 < _nfields; icol2++)
                  {
                  if(strcmp(field, _fields[icol2]) == 0)
                      {
                      active = TRUE;
                      break;
                      }
                  }
              int skip = (!active || icol >= ncolumns);
              setElementLogical("field_is_active", icol, active);
              setElementLogical("skip"           , icol, skip);
              }
          }
      }
  else
      {
      strcpy(_secname, "");
      error = virtualValidate(filename, msg);
      }
  chooseMostLikelySection();
  return error;
}



//---------------------- choose most likely section ------------------------//
//---------------------- choose most likely section ------------------------//
//---------------------- choose most likely section ------------------------//

     // private.
     // should be called right after validating an input file.
     // chooses the most likely correct pickle jar section.
     // sets _secname to blank if there are no sections.
     // there will be no sections if file not found.
     // there will always be at least one section if file is found.
     // if the found file has no sections, one will have been added.


void PjarFileSupport::chooseMostLikelySection()
{
  int nsections = _pjar->numSections();
  if(nsections == 0)
      {
      strcpy(_secname, "");
      return;
      }
  int indx;
  for(indx = 0; indx < nsections; indx++)
      {
      _pjar->chooseSection(indx);
      _pjar->getSecname(_secname);
      if(strcmp(_secname, _defname) == 0) return;
      }
  for(indx = 0; indx < nsections; indx++)
      {
      _pjar->chooseSection(indx);
      _pjar->getSecname(_secname);
      if(strcmp(_secname, "GENERIC") == 0) return;
      }
  for(indx = 0; indx < nsections; indx++)
      {
      _pjar->chooseSection(indx);
      _pjar->getSecname(_secname);
      if(strcmp(_secname, "HISTORY") != 0) return;
      }
  ////// now there are only history sections (unlikely).
  _pjar->chooseSection("foreign");    // also in fio.f90
  _pjar->getSecname(_secname);        // also in fio.f90
  setEncoding("ascii");               // also in fio.f90
  setFirstline(1);                    // also in fio.f90
}

   // Note: fio.f90 will create a "foreign" header section if there
   // are no sections.  Therefore an existing input file will always
   // have at least one header section.  If the file is not found,
   // or the file name is blank, then fio.f90 will NOT create a
   // header section.  Therefore, if there are no header sections when
   // this routine is called, this means that no file was read.



//------------------- initialize output parameters -------------------------//
//------------------- initialize output parameters -------------------------//
//------------------- initialize output parameters -------------------------//

     // public.


void PjarFileSupport::initializeOutputParameters ()
{
  assert(_io == 1);
  _pjar->clear();
  _pjar->chooseSection(_defname);
  strcpy(_secname, _defname);
  setEncoding("ascii");
  for(int icol = 0; icol < _nfields; icol++)
      {
      findAddField(_fields[icol]);
      }
  virtualAugment();
  setNcolumns(numFields());
}



//------------------- augment output parameters -------------------------//
//------------------- augment output parameters -------------------------//
//------------------- augment output parameters -------------------------//

     // public.


void PjarFileSupport::augmentOutputParameters ()
{
  assert(_io == 1);
  assert(numHeaderSections() > 0);
  assert(_secname[0] != '\0');
  assert(_secname[0] != ' ');
  virtualAugment();
}



//------------------------- choose active section ------------------------//
//------------------------- choose active section ------------------------//
//------------------------- choose active section ------------------------//

     // private.
     // chooses the pickle jar section specified by _secname.
     // does nothing if there are no sections.


void PjarFileSupport::chooseActiveSection()
{
  if(_pjar->numSections() == 0) return;
  if(strcmp(_secname, "") == 0)
      {
      chooseMostLikelySection();
      }
  _pjar->chooseSection(_secname);
}



//--------------------- miscellaneous virtual functions -------------------//
//--------------------- miscellaneous virtual functions -------------------//
//--------------------- miscellaneous virtual functions -------------------//


int         PjarFileSupport::inputFieldIsSkipped  (int indx)
{
  if(isOutput()) return FALSE;
  return getElementLogical("skip", indx);
}



int         PjarFileSupport::numDataCards   ()
{
  if(_pjar->numSections() == 0) return 0;
  chooseActiveSection();
  return _pjar->numCards();
}


       
const char *PjarFileSupport::getDataCard    (int indx)
{
  if(_pjar->numSections() == 0) return CNIL;
  static char card[111];
  chooseActiveSection();
  _pjar->getCard(indx, card);
  return card;
}



int         PjarFileSupport::numStartingCards   ()
{
  return _cards->numCardsRead();
}


       
const char *PjarFileSupport::getStartingCard    (int indx)
{
  return _cards->getCard(indx);
}



/////////// not a virtual function:

void PjarFileSupport::replaceStartingCard (int indx, const char *card)
{
  _cards->replaceCard(indx, card);
}



//------------------------- verify parameters -----------------------------//
//------------------------- verify parameters -----------------------------//
//------------------------- verify parameters -----------------------------//

                 // uses _required and _nrequired.

int  PjarFileSupport::verifyParameters (char *msg)
{
  if(numHeaderSections() == 0)
      {
      strcpy(msg, "no header sections -- probably a programmer error");
      return TRUE;
      }

  if(strcmp(getActiveHeaderSection(), "HISTORY") == 0)
      {
      strcpy(msg, "the primary header section cannot be HISTORY");
      return TRUE;
      }

  const char *encoding = getEncoding();
  if(strcmp(encoding, "ascii" ) == 0 ||
     strcmp(encoding, "binary") == 0 ||
     strcmp(encoding, "hybrid") == 0)
      {
      int nfields = numFields();
      if(nfields == 0)
          {
          if(isInput()) strcpy(msg, "no fields specified for input");
          else          strcpy(msg, "no fields specified for output");
          return TRUE;
          }

      int ncolumns = getNcolumns();
      if(ncolumns == 0)
          {
          if(isInput()) strcpy(msg, "no columns specified for input");
          else          strcpy(msg, "no columns specified for output");
          return TRUE;
          }

      for(int indx = 0; indx < _nrequired; indx++)
          {
          int colindx = findField(_required[indx]);
          if(colindx < 0 || colindx >= ncolumns || inputFieldIsSkipped(colindx))
              {
              char required[44];
              str_to_upper(required, (char*)_required[indx]);
              strcpy(msg, "required field ");
              strcat(msg, required);
              if(isInput()) strcat(msg, " not specified for input");
              else          strcat(msg, " not specified for output");
              return TRUE;
              }
          }
      }
  return virtualVerify(msg);
}



//------------------------ get and set header sections -----------------//
//------------------------ get and set header sections -----------------//
//------------------------ get and set header sections -----------------//


int         PjarFileSupport::numHeaderSections   ()
{
  return _pjar->numSections();
}



const char *PjarFileSupport::getHeaderSection    (int indx)
{
  if(_pjar->numSections() == 0) return CNIL;
  static char buffer[55];
  _pjar->chooseSection(indx);
  _pjar->getSecname(buffer);
  return buffer;
}



void        PjarFileSupport::setActiveHeaderSection (int indx)
{
  _pjar->chooseSection(indx);
  _pjar->getSecname(_secname);
}



void        PjarFileSupport::setActiveHeaderSection (const char *secname)
{
  _pjar->chooseSection(secname);
  _pjar->getSecname(_secname);
}



//-------------------- get and set scalar parameters ----------------------//
//-------------------- get and set scalar parameters ----------------------//
//-------------------- get and set scalar parameters ----------------------//


const char *PjarFileSupport::getEncoding  ()
{
  return getScalarString("encoding");
}



const char *PjarFileSupport::getNilstring ()
{
  return getScalarString("nilstring");
}



int         PjarFileSupport::getWrap      ()
{
  return getScalarInteger("wrap");
}



int         PjarFileSupport::getNcolumns  ()
{
  int ncolumns = getScalarInteger("ncolumns");
  if(ncolumns == INIL || ncolumns < 0) ncolumns = 0;
  return ncolumns;
}



int         PjarFileSupport::getFirstline ()
{
  return getScalarInteger("firstline");
}



int         PjarFileSupport::getFillout   ()
{
  return getScalarLogical("fillout");
}



int         PjarFileSupport::getNoheaders ()
{
  return getScalarLogical("noheaders");
}



const char *PjarFileSupport::getTemplate  ()
{
  return getScalarString("template");
}

                           /////////////////


void        PjarFileSupport::setNilstring (const char *value)
{
  if(value[0] == ' ' || value[0] == '\0') setScalarString("nilstring", "nil");
  else                                    setScalarString("nilstring", value);
}



void        PjarFileSupport::setWrap      (int         value)
{
  if(value <= 0 || value == INIL) value = 1;
  setScalarInteger("wrap", value);
}



void        PjarFileSupport::setNcolumns  (int         value)
{
  if(value < 0 || value == INIL)
      {
      value = 0;
      }
  else if(value > numFields())
      {
      if(isInput())
          {
          for(int indx = numFields(); indx < value; indx++)
              {
              setField(indx, "----");
              }
          }
      value = numFields();
      }
  setScalarInteger("ncolumns", value);
}



void        PjarFileSupport::setFirstline (int         value)
{
  if(value <= 0) value = INIL;
  setScalarInteger("firstline", value);
}



void        PjarFileSupport::setFillout   (int         value)
{
  setScalarLogical("fillout", value);
}



void        PjarFileSupport::setNoheaders (int         value)
{
  setScalarLogical("noheaders", value);
}



void        PjarFileSupport::setTemplate  (const char *value)
{
  setScalarString("template", value);
}



void        PjarFileSupport::setEncoding (const char *value)
{
  setScalarString("encoding", value);
  privateValidateEncoding();
}


                           /////////////////


void        PjarFileSupport::stepEncoding (int step)
{
  if(step <= 0) return;
  if(_pjar->numSections() == 0) return;
  char value[22];
  chooseActiveSection();
  _pjar->getScalar("encoding" ,value);
  if(isInput())
      {
      if     (strcmp(value,"ascii" ) == 0) strcpy(value, "");
      else                                 strcpy(value, "ascii" );
      }
  else
      {
      if     (strcmp(value,"ascii" ) == 0) strcpy(value, "binary");
      else if(strcmp(value,"binary") == 0) strcpy(value, "hybrid");
      else if(strcmp(value,"hybrid") == 0) strcpy(value, ""      );
      else                                 strcpy(value, "ascii" );
      }
  _pjar->putScalar("encoding", value);
  privateValidateEncoding();
}



//---------------------- private validate encoding -------------------------//
//---------------------- private validate encoding -------------------------//
//---------------------- private validate encoding -------------------------//

       // chooseActiveSection() should already have been called.


void PjarFileSupport::privateValidateEncoding()
{
  if(_pjar->numSections() == 0) return;
  char value[22];
  _pjar->getScalar("encoding", value);
  while(TRUE)
       {
       if(strcmp(value,"oldcps") == 0)
            {
            if(allowOldcps()) break; strcpy(value,"ascii");
            }
       else if(strcmp(value,"ascii") == 0)
            {
            if(allowAscii()) break; strcpy(value,"binary");
            }
       else if(strcmp(value,"binary") == 0)
            {
            if(allowBinary()) break; strcpy(value,"hybrid");
            }
       else if(strcmp(value,"hybrid") == 0)
            {
            if(allowHybrid()) break; strcpy(value,"");
            }
       else if(isOutput())
            {
            strcpy(value,"ascii");   // changed from "oldcps" 2003-09-23
            }
       else
            {
            _pjar->removeKeyword("encoding");
            return;
            }
       }
  _pjar->putScalar("encoding", value);
}



//--------------------------- find field ----------------------------//
//--------------------------- find field ----------------------------//
//--------------------------- find field ----------------------------//


int PjarFileSupport::findField (const char *field)
{
  if(_pjar->numSections() == 0) return 0;
  chooseActiveSection();
  return _pjar->find("fields", field);
}



//--------------------------- find add field ----------------------------//
//--------------------------- find add field ----------------------------//
//--------------------------- find add field ----------------------------//


int PjarFileSupport::findAddField (const char *field)
{
  if(_pjar->numSections() == 0) return 0;
  chooseActiveSection();
  return _pjar->findAdd("fields", field);
}



//--------------------------- fields are present -------------------------//
//--------------------------- fields are present -------------------------//
//--------------------------- fields are present -------------------------//


int PjarFileSupport::fieldsArePresent()
{
  if(_pjar->numSections() == 0) return FALSE;
  chooseActiveSection();
  return _pjar->keywordPresent("fields");
}



//--------------------------- field is active ------------------------//
//--------------------------- field is active ------------------------//
//--------------------------- field is active ------------------------//


int PjarFileSupport::fieldIsActive(int indx)
{
  if(_pjar->numSections() == 0) return FALSE;
  chooseActiveSection();
  if(!_pjar->keywordPresent("field_is_active")) return TRUE;
  if(indx >= _pjar->numElements("field_is_active")) return FALSE;
  return getElementLogical("field_is_active", indx);
}



//--------------------------- remove field ----------------------------//
//--------------------------- remove field ----------------------------//
//--------------------------- remove field ----------------------------//


void PjarFileSupport::removeField(int indx)
{
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();
  privateRemove(indx);
}



//--------------------------- get generic values --------------------------//
//--------------------------- get generic values --------------------------//
//--------------------------- get generic values --------------------------//



int PjarFileSupport::keywordPresent (const char *keyword)
{
  if(_pjar->numSections() == 0) return FALSE;
  chooseActiveSection();
  return _pjar->keywordPresent(keyword);
}


void PjarFileSupport::getGridTransform (GridTransform *grid)
{
  if(_pjar->numSections() == 0)
      {
      grid->initialize();
      return;
      }
  chooseActiveSection();
  _pjar->getScalar("grid", grid);
}


const char *PjarFileSupport::getScalarString (const char *keyword)
{
  if(_pjar->numSections() == 0) return CNIL;
  static char value[99];
  chooseActiveSection();
  _pjar->getScalar(keyword, value);
  return value;
}


int PjarFileSupport::getScalarInteger (const char *keyword)
{
  if(_pjar->numSections() == 0) return INIL;
  int value;
  chooseActiveSection();
  _pjar->getScalar(keyword, &value);
  return value;
}


float PjarFileSupport::getScalarFloat (const char *keyword)
{
  if(_pjar->numSections() == 0) return FNIL;
  float value;
  chooseActiveSection();
  _pjar->getScalar(keyword, &value);
  return value;
}


int PjarFileSupport::getScalarLogical (const char *keyword)
{
  if(_pjar->numSections() == 0) return LNIL;
  int value;
  chooseActiveSection();
  _pjar->getLogical(keyword, &value);
  return value;
}

                            /////////////////


const char *PjarFileSupport::getElementString (const char *keyword, int indx)
{
  if(_pjar->numSections() == 0) return CNIL;
  static char value[99];
  chooseActiveSection();
  _pjar->getElement(keyword, indx, value);
  return value;
}


int PjarFileSupport::getElementInteger (const char *keyword, int indx)
{
  if(_pjar->numSections() == 0) return INIL;
  int value;
  chooseActiveSection();
  _pjar->getElement(keyword, indx, &value);
  return value;
}



float PjarFileSupport::getElementFloat (const char *keyword, int indx)
{
  if(_pjar->numSections() == 0) return FNIL;
  float value;
  chooseActiveSection();
  _pjar->getElement(keyword, indx, &value);
  return value;
}



int PjarFileSupport::getElementLogical (const char *keyword, int indx)
{
  if(_pjar->numSections() == 0) return FALSE;
  int value;
  chooseActiveSection();
  _pjar->getLogical(keyword, indx, &value);
  return value;
}

                            /////////////////


const char *PjarFileSupport::getElementString 
                                  (const char *keyword, const char *field)
{
  if(_pjar->numSections() == 0) return CNIL;
  chooseActiveSection();
  int index = findField(field);
  return getElementString(keyword, index);
}



int         PjarFileSupport::getElementInteger
                                  (const char *keyword, const char *field)
{
  if(_pjar->numSections() == 0) return INIL;
  chooseActiveSection();
  int index = findField(field);
  return getElementInteger(keyword, index);
}



float       PjarFileSupport::getElementFloat  
                                  (const char *keyword, const char *field)
{
  if(_pjar->numSections() == 0) return FNIL;
  chooseActiveSection();
  int index = findField(field);
  return getElementFloat(keyword, index);
}



int         PjarFileSupport::getElementLogical
                                  (const char *keyword, const char *field)
{
  if(_pjar->numSections() == 0) return FALSE;
  chooseActiveSection();
  int index = findField(field);
  return getElementLogical(keyword, index);
}


                            /////////////////


const char *PjarFileSupport::getElementFieldtype (const char *field)
{
  return getElementString("fieldtypes", field);
}



const char *PjarFileSupport::getElementUnits (const char *field)
{
  return getElementString("units", field);
}



int         PjarFileSupport::getElementHdr (const char *field)
{
  return getElementInteger("hdrs", field);
}



const char *PjarFileSupport::getElementDefaultString (const char *field)
{
  return getElementString("defaults", field);
}



int         PjarFileSupport::getElementDefaultInteger (const char *field)
{
  return getElementInteger("defaults", field);
}



float       PjarFileSupport::getElementDefaultFloat   (const char *field)
{
  return getElementFloat("defaults", field);
}


                            /////////////////


const char *PjarFileSupport::getComboFieldtype 
                                  (const char *keyword, const char *field)
{
  const char                  *value = getScalarString     (keyword);
  if(strcmp(value, CNIL) == 0) value = getElementFieldtype (field);
  return value;
}



const char *PjarFileSupport::getComboUnits 
                                  (const char *keyword, const char *field)
{
  const char                  *value = getScalarString (keyword);
  if(strcmp(value, CNIL) == 0) value = getElementUnits (field);
  return value;
}



int         PjarFileSupport::getComboHdr 
                                  (const char *keyword, const char *field)
{
  int               value = getScalarInteger (keyword);
  if(value == INIL) value = getElementHdr    (field);
  return value;
}



const char *PjarFileSupport::getComboDefaultString 
                                  (const char *keyword, const char *field)
{
  const char                  *value = getScalarString         (keyword);
  if(strcmp(value, CNIL) == 0) value = getElementDefaultString (field);
  return value;
}



int         PjarFileSupport::getComboDefaultInteger 
                                  (const char *keyword, const char *field)
{
  int               value = getScalarInteger         (keyword);
  if(value == INIL) value = getElementDefaultInteger (field);
  return value;
}



float       PjarFileSupport::getComboDefaultFloat 
                                  (const char *keyword, const char *field)
{
  float             value = getScalarFloat         (keyword);
  if(value == FNIL) value = getElementDefaultFloat (field);
  return value;
}



//--------------------------- set generic values --------------------------//
//--------------------------- set generic values --------------------------//
//--------------------------- set generic values --------------------------//


void PjarFileSupport::removeKeyword (const char *keyword)
{
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();
  _pjar->removeKeyword(keyword);
}


void PjarFileSupport::setGridTransform (const GridTransform *grid)
{
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();
  _pjar->putScalar("grid", grid);
}


void PjarFileSupport::setScalarString (const char *keyword, const char *value)
{
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();
  _pjar->putScalar(keyword, value);
}


void PjarFileSupport::setScalarInteger (const char *keyword, int value)
{
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();
  _pjar->putScalar(keyword, value);
}


void PjarFileSupport::setScalarFloat (const char *keyword, float value)
{
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();
  _pjar->putScalar(keyword, value);
}


void PjarFileSupport::setScalarLogical (const char *keyword, int value)
{
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();
  _pjar->putLogical(keyword, value);
}

                            /////////////////


void PjarFileSupport::setElementString
                          (const char *keyword, int indx, const char *value)
{
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();
  _pjar->putElement(keyword, indx, value);
}


void PjarFileSupport::setElementInteger
                          (const char *keyword, int indx, int value)
{
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();
  _pjar->putElement(keyword, indx, value);
}


void PjarFileSupport::setElementFloat
                          (const char *keyword, int indx, float value)
{
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();
  _pjar->putElement(keyword, indx, value);
}


void PjarFileSupport::setElementLogical
                          (const char *keyword, int indx, int value)
{
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();
  _pjar->putLogical(keyword, indx, value);
}


                            /////////////////


void PjarFileSupport::setElementString 
             (const char *keyword, const char *field, const char *value)
{
  int index = findAddField(field);
  setElementString(keyword, index, value);
}


void PjarFileSupport::setElementInteger
             (const char *keyword, const char *field, int         value)
{
  int index = findAddField(field);
  setElementInteger(keyword, index, value);
}


void PjarFileSupport::setElementFloat  
             (const char *keyword, const char *field, float       value)
{
  int index = findAddField(field);
  setElementFloat(keyword, index, value);
}


void PjarFileSupport::setElementLogical
             (const char *keyword, const char *field, int         value)
{
  int index = findAddField(field);
  setElementLogical(keyword, index, value);
}

                            /////////////////


void PjarFileSupport::setElementFieldtype
                                (const char *field, const char *value)
{
  setElementString("fieldtypes", field, value);
}


void PjarFileSupport::setElementUnits
                                (const char *field, const char *value)
{
  setElementString("units", field, value);
}


void PjarFileSupport::setElementHdr
                                (const char *field, int         value)
{
  setElementInteger("hdrs", field, value);
}


void PjarFileSupport::setElementDefaultString
                                (const char *field, const char *value)
{
  setElementString("defaults", field, value);
}


void PjarFileSupport::setElementDefaultInteger
                                (const char *field, int         value)
{
  setElementInteger("defaults", field, value);
}


void PjarFileSupport::setElementDefaultFloat
                                (const char *field, float       value)
{
  setElementFloat("defaults", field, value);
}

                            /////////////////


void PjarFileSupport::setComboFieldtype
                (const char *keyword, const char *field, const char *value)
{
  setScalarString     (keyword, value);
  setElementFieldtype (field  , value);
}


void PjarFileSupport::setComboUnits
                (const char *keyword, const char *field, const char *value)
{
  setScalarString (keyword, value);
  setElementUnits (field  , value);
}


void PjarFileSupport::setComboHdr
                (const char *keyword, const char *field, int         value)
{
  setScalarInteger (keyword, value);
  setElementHdr    (field  , value);
}


void PjarFileSupport::setComboDefaultString
                (const char *keyword, const char *field, const char *value)
{
  setScalarString         (keyword, value);
  setElementDefaultString (field  , value);
}


void PjarFileSupport::setComboDefaultInteger
                (const char *keyword, const char *field, int         value)
{
  setScalarInteger         (keyword, value);
  setElementDefaultInteger (field  , value);
}


void PjarFileSupport::setComboDefaultFloat
                (const char *keyword, const char *field, float       value)
{
  setScalarFloat         (keyword, value);
  setElementDefaultFloat (field  , value);
}



//----------------------- do insert or remove ------------------------------//
//----------------------- do insert or remove ------------------------------//
//----------------------- do insert or remove ------------------------------//


void PjarFileSupport::privateInsert (int index)
{
  if(_pjar->numSections() == 0) return;
               _pjar->insertElement("fields"    , index);
               _pjar->insertElement("fieldtypes", index);
               _pjar->insertElement("units"     , index);
               _pjar->insertElement("hdrs"      , index);
               _pjar->insertElement("defaults"  , index);
  if(_io == 0) _pjar->insertElement("converters", index);
  if(_io == 0) _pjar->insertElement("maxchars"  , index);
  if(_io == 1) _pjar->insertElement("widths"    , index);
               _pjar->insertElement("vartypes"  , index);
               _pjar->insertElement("delimiters", index);
  if(_io == 0) _pjar->insertElement("skip"      , index);
  if(_pjar->keywordPresent("field_is_active"))
               _pjar->insertElement("field_is_active", index);
}


void PjarFileSupport::privateRemove (int index)
{
  if(_pjar->numSections() == 0) return;
               _pjar->removeElement("fields"    , index);
               _pjar->removeElement("fieldtypes", index);
               _pjar->removeElement("units"     , index);
               _pjar->removeElement("hdrs"      , index);
               _pjar->removeElement("defaults"  , index);
  if(_io == 0) _pjar->removeElement("converters", index);
  if(_io == 0) _pjar->removeElement("maxchars"  , index);
  if(_io == 1) _pjar->removeElement("widths"    , index);
               _pjar->removeElement("vartypes"  , index);
               _pjar->removeElement("delimiters", index);
  if(_io == 0) _pjar->removeElement("skip"      , index);
  if(_pjar->keywordPresent("field_is_active"))
               _pjar->removeElement("field_is_active", index);
}



//-------------------- get and set array parameters ----------------------//
//-------------------- get and set array parameters ----------------------//
//-------------------- get and set array parameters ----------------------//


int         PjarFileSupport::numFields        ()
{
  if(_pjar->numSections() == 0) return 0;
  chooseActiveSection();
  return _pjar->numElements("fields");
}



const char *PjarFileSupport::getField         (int indx)
{
  return getElementString("fields", indx);
}



const char *PjarFileSupport::getFieldtype     (int indx)
{
  return getElementString("fieldtypes", indx);
}



const char *PjarFileSupport::getUnits         (int indx)
{
  return getElementString("units", indx);
}



int         PjarFileSupport::getHdr           (int indx)
{
  return getElementInteger("hdrs", indx);
}



const char *PjarFileSupport::getDefault       (int indx)
{
  return getElementString("defaults", indx);
}



const char *PjarFileSupport::getDescription       (int indx)
{
  return getElementString("description", indx);
}



const char *PjarFileSupport::getConverter     (int indx)
{
  return getElementString("converters", indx);
}



int         PjarFileSupport::getMaxchars      (int indx)
{
  int maxchars = getElementInteger("maxchars", indx);
  if(maxchars == 0) maxchars = INIL;
  return maxchars;
}



int         PjarFileSupport::getWidth         (int indx)
{
  int width = getElementInteger("widths", indx);
  if(width == 0) width = INIL;
  return width;
}


                           /////////////////


void        PjarFileSupport::setField         (int indx, const char *value)
{
  setElementString("fields", indx, value);
  if(isInput())
      {
      if(value[0] == '-') setElementLogical("skip", indx, TRUE);
      }
}



void        PjarFileSupport::setColumnNumber  (int indx, int column)
{
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();
  int nfields = _pjar->numElements("fields");

  if(column == INIL   ) column = nfields;
  if(column <  1      ) column = 1;
  if(column >  nfields)
      {
      if(isInput()) setNcolumns(column);
      else          column = nfields;
      }

  int index2 = indx;                            // index to move from.
  int index3 = column - 1;                      // index to move to.

  if(index2 < index3)
      {
      privateRemove(index2    );
      privateInsert(index3 - 1);
      privateRemove(index3    );
      privateInsert(index2    );
      }
  else if(index2 > index3)
      {
      privateRemove(index2    );
      privateInsert(index3    );
      privateRemove(index3 + 1);
      privateInsert(index2    );
      }
}



void        PjarFileSupport::setFieldtype     (int indx, const char *value)
{
  setElementString("fieldtypes", indx, value);
}



void        PjarFileSupport::setUnits         (int indx, const char *value)
{
  setElementString("units", indx, value);
}



void        PjarFileSupport::setHdr           (int indx, int         value)
{
  setElementInteger("hdrs", indx, value);
}



void        PjarFileSupport::setDefault       (int indx, const char *value)
{
  setElementString("defaults", indx, value);
}



void        PjarFileSupport::setDescription   (int indx, const char *value)
{
  setElementString("description", indx, value);
}



void        PjarFileSupport::setConverter     (int indx, const char *value)
{
  setElementString("converters", indx, value);
}


                           /////////////////


void        PjarFileSupport::stepField        (int indx, int step)
{
  if(step <= 0) return;
  if(_pjar->numSections() == 0) return;
  chooseActiveSection();

  assert(indx >= 0);
  int ncolumns = getNcolumns();

  if(_io == 1)
      {
      int index2 = indx;                          // index to move from.
      if(index2 < ncolumns)
          {
          int index3 = ncolumns - 1;              // index to move to.
          privateRemove(index2);
          privateInsert(index3);
          setNcolumns(ncolumns-1);
          }
      else
          {
          int index3 = ncolumns;                  // index to move to.
          privateRemove(index2);
          privateInsert(index3);
          setNcolumns(ncolumns+1);
          }
      }
  else if(getField(indx)[0] == '-')
      {
      }
  else
      {
      int skip = getElementLogical("skip", indx);
      if(indx >= ncolumns)
          {
          for(int i = ncolumns; i < indx; i++)
              {
              setElementLogical("skip", i, TRUE);
              }
          setNcolumns(indx+1);
          skip = FALSE;
          }
      else
          {
          skip = !skip;
          }
      setElementLogical("skip", indx, skip);
      }
}



void        PjarFileSupport::stepColumnNumber (int indx, int step)
{
  setColumnNumber(indx, indx + 1 + step);
}



void        PjarFileSupport::stepConverter    (int indx, int step)
{
  if(step <= 0) return;
  const char *value = getElementString("converters", indx);
  if     (!strcmp(value,"1000" )) setElementString("converters",indx,"0.001");
  else if(!strcmp(value,"0.001")) setElementString("converters",indx,"recip");
  else if(!strcmp(value,"recip")) setElementString("converters",indx," "    );
  else                            setElementString("converters",indx,"1000" );
}



void        PjarFileSupport::stepMaxchars     (int indx, int step)
{
  int value = getElementInteger("maxchars", indx);
  if(value == INIL) value = 0;
  value += step;
  if(value == 0) value = INIL;
  setElementInteger("maxchars", indx, value);
}



void        PjarFileSupport::stepWidth        (int indx, int step)
{
  int value = getElementInteger("widths", indx);
  if(value == INIL) value = 0;
  if(value > 0 || step > 0) value += step;
  if(value == 0) value = INIL;
  setElementInteger("widths", indx, value);
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
