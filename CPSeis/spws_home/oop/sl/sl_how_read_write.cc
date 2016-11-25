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

//-------------------------- sl_how_read_write.cc ---------------------------//
//-------------------------- sl_how_read_write.cc ---------------------------//
//-------------------------- sl_how_read_write.cc ---------------------------//

//            implementation file for the SLHowReadWrite class
//                     derived from the SLDatabox class
//                             subdirectory sl


#include "sl/sl_how_read_write.hh"
#include "oprim/file_support_interface.hh"
#include "named_constants.h"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

  // This class understands the following information about the
  // ENCODING parameter:
  // (1) the only encoding values recognized are ASCII, BINARY, and HYBRID.
  // (2) all other parameters are relevant only for ASCII, BINARY, and HYBRID.
  // (3) only ASCII, BINARY, HYBRID, and blank can be changed for input files.
  // (4) any encoding parameter can be changed for output files.
  // (5) parameters NILSTRING, WRAP, FIRSTLINE are relevant only for ASCII.
  // (6) parameter FILLOUT is relevant only for ASCII and HYBRID.
  // (7) parameter NOHEADERS is relevant only for ASCII.


enum { ENCODING = 1, NILSTRING, WRAP, NCOLUMNS,                  // scalars
       FILLOUT, NOHEADERS, FIRSTLINE,                            // scalars
       FIELDS, DEFAULTS, MAXCHARS, WIDTHS, CONVERTERS,           // arrays
       SECNAMES, FIELDTYPES, UNITS, HDRS, COLUMNS, DESCRIPTION   // arrays
     };


#define STEP       int step;                                          \
                   if     (strcmp(endkey,"RETURN" ) == 0) step =  1;  \
                   else if(strcmp(endkey,"BUTTON2") == 0) step = -1;  \
                   else return;                                       \

#define TABLE      SLHowReadWrite       *table     = (SLHowReadWrite*)data;
#define INTERFACE  FileSupportInterface *interface = table->getInterface();

#define HIDDEN     int hidden = TRUE;                                    \
                   const char *encoding = interface->getEncoding();      \
                   if (strcmp(encoding, "ascii" ) == 0) hidden = FALSE;  \
                   if (strcmp(encoding, "binary") == 0) hidden = FALSE;  \
                   if (strcmp(encoding, "hybrid") == 0) hidden = FALSE;


//------------------------------ constructor -------------------------------//
//------------------------------ constructor -------------------------------//
//------------------------------ constructor -------------------------------//


SLHowReadWrite::SLHowReadWrite (SLDelay *slparent,
                                FileSupportInterface *interface,
                                int nrows_init)
       : SLDatabox(slparent, "sl_how_read_write", NULL, 4, TRUE, nrows_init),
                _interface  (interface)
{
  assert(_interface);
}



//---------------------------- destructor ----------------------------------//
//---------------------------- destructor ----------------------------------//
//---------------------------- destructor ----------------------------------//

SLHowReadWrite::~SLHowReadWrite()
{
}



//------------------------------- traps ------------------------------------//
//------------------------------- traps ------------------------------------//
//------------------------------- traps ------------------------------------//


static void pwidths_trap(void *data, long /*ident*/, long /*index*/,
                         char* /*value*/, long /*nread*/, char* endkey)
{
  STEP
  TABLE
  INTERFACE
  for(int indx = 0; indx < interface->numFields(); indx++)
      {
      interface->stepWidth(indx, step);
      }
}

                         ////////////


static void itext_trap(void *data, long ident, long index,
                       long value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  INTERFACE
  switch(ident)
      {
      case WRAP     : interface->setWrap               ((int)value); break;
      case NCOLUMNS : interface->setNcolumns           ((int)value); break;
      case FIRSTLINE: interface->setFirstline          ((int)value); break;
      case FILLOUT  : interface->setFillout            ((int)value); break;
      case NOHEADERS: interface->setNoheaders          ((int)value); break;
      case COLUMNS  : interface->setColumnNumber(index, (int)value); break;
      case HDRS     : interface->setHdr         (index, (int)value); break;
      default       : assert(FALSE);
      }
}



static void ctext_trap(void *data, long ident, long index,
                       char *value, long nread, char* /*endkey*/)
{
  if(nread == 0) return;
  TABLE
  INTERFACE
  switch(ident)
      {
      case NILSTRING : interface->setNilstring       (value); break;
      case FIELDS    : interface->setField    (index, value); break;
      case FIELDTYPES: interface->setFieldtype(index, value); break;
      case UNITS     : interface->setUnits    (index, value); break;
      case DEFAULTS  : interface->setDefault  (index, value); break;
      default        : assert(FALSE);
      }
}

                         ////////////


static void ibutton_trap(void *data, long ident, long index,
                         long /*value*/, long /*nread*/, char* endkey)
{
  STEP
  TABLE
  INTERFACE
  switch(ident)
      {
      case COLUMNS   : interface->stepColumnNumber(index, step); break;
      case HDRS      : interface->stepHdr         (index, step); break;
      case MAXCHARS  : interface->stepMaxchars    (index, step); break;
      case WIDTHS    : interface->stepWidth       (index, step); break;
      default        : assert(FALSE);
      }
}



static void cbutton_trap(void *data, long ident, long index,
                         char* /*value*/, long /*nread*/, char* endkey)
{
  STEP
  TABLE
  INTERFACE
  switch(ident)
      {
      case ENCODING  : interface->stepEncoding        (step); break;
      case FIELDS    : interface->stepField    (index, step); break;
      case FIELDTYPES: interface->stepFieldtype(index, step); break;
      case UNITS     : interface->stepUnits    (index, step); break;
      case DEFAULTS  : interface->stepDefault  (index, step); break;
      case CONVERTERS: interface->stepConverter(index, step); break;
      default        : assert(FALSE);
      }
}


                         ////////////


static void secnames_trap(void *data, long /*ident*/, long index,
                          char* /*value*/, long /*nread*/, char* endkey)
{
  STEP
  TABLE
  INTERFACE
  if(step == 1) interface->setActiveHeaderSection(index);
}



//---------------------------- update functions ---------------------------//
//---------------------------- update functions ---------------------------//
//---------------------------- update functions ---------------------------//


static long ivar_update(void *data, long ident, long index)
{
  int value;
  TABLE
  INTERFACE
  if(interface->numHeaderSections() == 0) return INIL;
  switch(ident)
      {
      case WRAP     : value = interface->getWrap             (); break;
      case NCOLUMNS : value = interface->getNcolumns         (); break;
      case FILLOUT  : value = interface->getFillout          (); break;
      case NOHEADERS: value = interface->getNoheaders        (); break;
      case FIRSTLINE: value = interface->getFirstline        (); break;
      case COLUMNS  : if(interface->getNcolumns() > index) value = index + 1;
                      else value = INIL;                         break;
      case HDRS     : value = interface->getHdr         (index); break;
      case MAXCHARS : value = interface->getMaxchars    (index); break;
      case WIDTHS   : value = interface->getWidth       (index); break;
      default       : assert(FALSE);
      }
  return (long)value;
}



static char *cvar_update(void *data, long ident, long index)
{
  static const char *value;
  TABLE
  INTERFACE
  if(interface->numHeaderSections() == 0) return NULL;
  switch(ident)
      {
      case ENCODING    : value = interface->getEncoding        (); break;
      case NILSTRING   : value = interface->getNilstring       (); break;
      case FIELDS      : value = interface->getField      (index); break;
      case FIELDTYPES  : value = interface->getFieldtype  (index); break;
      case UNITS       : value = interface->getUnits      (index); break;
      case DEFAULTS    : value = interface->getDefault    (index); break;
      case DESCRIPTION : value = interface->getDescription(index); break;
      case CONVERTERS  : value = interface->getConverter  (index); break;
      default          : assert(FALSE);
      }
  return (char*)value;
}

                         ////////////


static char *secnames_update(void *data, long /*ident*/, long index)
{
  TABLE
  INTERFACE
  return (char*)interface->getHeaderSection(index);
}


                         ////////////


static long nmax_update(void *data)
{
  TABLE
  INTERFACE
  if(interface->numHeaderSections() == 0) return 0;
  HIDDEN
  if(hidden) return interface->numFields();
  return interface->numFields();
}



static long n_update(void *data)
{
  TABLE
  INTERFACE
  if(interface->numHeaderSections() == 0) return 0;
  return interface->numFields();
}



static long nsec_update(void *data)
{
  TABLE
  INTERFACE
  return interface->numHeaderSections();
}



//-------------------- switch update functions ----------------------------//
//-------------------- switch update functions ----------------------------//
//-------------------- switch update functions ----------------------------//


static long prompt_switch(void *data, long ident, long /*index*/)
{
  if(ident == -WIDTHS)
      {
      TABLE
      INTERFACE
      if(interface->numHeaderSections() == 0) return -5;
      if(strcmp(interface->getEncoding(), "ascii") != 0) return -57;
      return 2;
      }
  return 0;
}



static long encoding_switch(void *data, long ident, long index)
{
  TABLE
  INTERFACE
  if(interface->numHeaderSections() == 0) return -5;
  if(interface->isOutput()) return 2;
  if(strcmp(interface->getEncoding(), "ascii" ) == 0) return 2;
  if(strcmp(interface->getEncoding(), ""      ) == 0) return 2;
  return -57;
}



static long scalar_switch(void *data, long ident, long /*index*/)
{
  TABLE
  INTERFACE
  int sw1 = 1;
  int sw2 = 1;
  int sw3 = 3;
  if(interface->numHeaderSections() == 0)
      {
      sw1 = -5;
      sw2 = -5;
      sw3 = -3;
      }
  if (strcmp(interface->getEncoding(), "ascii" ) != 0) sw1 = -5;
  if (strcmp(interface->getEncoding(), "ascii" ) != 0 &&
      strcmp(interface->getEncoding(), "hybrid") != 0 &&
      strcmp(interface->getEncoding(), "binary") != 0) sw2 = -5;
  if (strcmp(interface->getEncoding(), "ascii" ) != 0 &&
      strcmp(interface->getEncoding(), "hybrid") != 0) sw3 = -3;
  if(interface->isInput())
      {
      if (strcmp(interface->getEncoding(), "ascii" ) != 0) sw2 = -5;
      }
  switch(ident)
      {
      case NILSTRING  : return sw1;
      case WRAP       : return sw1;
      case NCOLUMNS   : return sw2;
      case FILLOUT    : return sw3;
      case NOHEADERS  : return sw3;
      case FIRSTLINE  : return sw1;
      }
  return -5;
}

                         ////////////


static long fields_switch(void *data, long /*ident*/, long index)
{
  TABLE
  INTERFACE
  if(interface->numHeaderSections() == 0) return -5;
  HIDDEN
  int nfields  = interface->numFields      ();
  int ncolumns = interface->getNcolumns    ();
  int sw       = interface->switchFields   (index);
  int column   = index + 1;
  if(interface->isInput() &&
     interface->inputFieldIsSkipped(index)) ncolumns = 0;
  if(index >= nfields) return -5;
  if(sw == 2)
      {
      if(hidden)                           return -57;
      if(!interface->fieldIsActive(index)) return -57;
      if(column <= ncolumns)               return   6;
      }
  else if(sw == 1)
      {
      if(hidden)                           return -5;
      if(!interface->fieldIsActive(index)) return -5;
      if(column <= ncolumns)               return  7;
      }
  return sw;
}



static long array_switch(void *data, long ident, long index)
{
  TABLE
  INTERFACE
  if(interface->numHeaderSections() == 0) return -5;
  if(index >= interface->numFields()) return -5;
  int sw = -5;
  switch(ident)
      {
      case COLUMNS     : sw = interface->switchColumnNumbers(index); break;
      case FIELDTYPES  : sw = interface->switchFieldtypes   (index); break;
      case UNITS       : sw = interface->switchUnits        (index); break;
      case HDRS        : sw = interface->switchHdrs         (index); break;
      case DEFAULTS    : sw = interface->switchDefaults     (index); break;
      case DESCRIPTION : sw = -5;                                    break;
      case MAXCHARS    : sw =  2;                                    break;
      case WIDTHS      : sw =  2;                                    break;
      case CONVERTERS  : sw =  2;                                    break;
      }
  if(!interface->fieldIsActive(index))
      {
      switch(ident)
          {
          case COLUMNS     :                                        break;
          case FIELDTYPES  : sw = (sw == 2 ? -57 : -5);             break;
          case UNITS       : sw = (sw == 2 ? -57 : -5);             break;
          case HDRS        : sw = (sw == 2 ? -57 : -5);             break;
          case DEFAULTS    : sw = (sw == 2 ? -57 : -5);             break;
          case DESCRIPTION : sw = -5;                               break;
          case MAXCHARS    : sw =  2;                               break;
          case WIDTHS      : sw =  2;                               break;
          case CONVERTERS  : sw = -5;                               break;
          }
      }
  if(sw <= 0) return sw;
  HIDDEN
  if(hidden)
      {
      return (sw == 2 ? -57 : -5);
      }
  if(ident == MAXCHARS || ident == WIDTHS)
      {
      if(strcmp(encoding, "ascii") != 0) return -57;
      }
  return sw;
}

                         ////////////


static long secnames_switch(void *data, long /*ident*/, long index)
{
  TABLE
  INTERFACE
  int nsections = interface->numHeaderSections();
  if(index >= nsections                                       ) return  -5;
  const char *secname  = interface->getHeaderSection(index);
  const char *encoding = interface->getEncoding();
  if(strcmp(encoding, "oldcps") == 0                          ) return -57;
  if(strcmp(secname, "HISTORY") == 0                          ) return -57;
  if(strcmp(secname, "FOREIGN") == 0 && nsections == 1        ) return -57;
  if(strcmp(secname, interface->getActiveHeaderSection()) == 0) return   6;
  return 2;
}



//------------------------------ make helper --------------------------------//
//------------------------------ make helper --------------------------------//
//------------------------------ make helper --------------------------------//

void SLHowReadWrite::makeHelper()
{
  static long zero  =   0; 
  static long one   =   1; 
  static long two   =   2; 
  static long three =   3; 
  static long five  =   5; 
  static long m44   = -44; 

  int input  = _interface->isInput();
  int output = _interface->isOutput();

/////////////////////// register the title:

              //      ID          PROMPT         SWITCH ROW COL
  if(input) regString(0, "How to Read the File" , &m44,  1, 34);
  else      regString(0, "How to Write the File", &m44,  1, 34);

/////////////////////// register scalar parameters:

               //       ID         PROMPT    SWITCH SWITCH ROW COL NCHAR
             regCvar2(ENCODING   ," encoding"  ,&zero,&two  ,2,1,8);
             regCvar2(NILSTRING  ,"nilstring"  ,&zero,&one  ,0,1,10);
             regIvar2(WRAP       ,"     wrap"  ,&zero,&one  ,0,1,8);
             regIvar2(NCOLUMNS   ," ncolumns"  ,&zero,&one  ,0,1,8);
  if(output) regIvar2(FILLOUT    ,"  fillout"  ,&zero,&three,0,1,2);
  if(output) regIvar2(NOHEADERS  ,"noheaders"  ,&zero,&three,0,1,2);
  if(input)  regIvar2(FIRSTLINE  ,"firstline"  ,&zero,&one  ,0,1,8);

  funCvar ( ENCODING   ,   cbutton_trap,  cvar_update, encoding_switch);
  funCvar ( NILSTRING  ,     ctext_trap,  cvar_update,   scalar_switch);
  funIvar ( WRAP       ,     itext_trap,  ivar_update,   scalar_switch);
  funIvar ( NCOLUMNS   ,     itext_trap,  ivar_update,   scalar_switch);
  funIvar ( FILLOUT    ,     itext_trap,  ivar_update,   scalar_switch);
  funIvar ( NOHEADERS  ,     itext_trap,  ivar_update,   scalar_switch);
  funIvar ( FIRSTLINE  ,     itext_trap,  ivar_update,   scalar_switch);

  funCvar (-ENCODING   ,           NULL,         NULL,   NULL         );
  funCvar (-NILSTRING  ,           NULL,         NULL,   prompt_switch);
  funCvar (-WRAP       ,           NULL,         NULL,   prompt_switch);
  funCvar (-NCOLUMNS   ,           NULL,         NULL,   prompt_switch);
  funCvar (-FILLOUT    ,           NULL,         NULL,   prompt_switch);
  funCvar (-NOHEADERS  ,           NULL,         NULL,   prompt_switch);
  funCvar (-FIRSTLINE  ,           NULL,         NULL,   prompt_switch);

/////////////////////// register array of section names:

  int nscalars =  5;          // number of scalars registered above.
  int secrows  =  5;          // minimum number of sections to show.
  int maxrows  = 30;          // maximum number of rows to show.
  if(output) nscalars++;

  secrows = MaximumValue(secrows, maxrows - nscalars);

    //           N         NMAX          ROW      COL NCHAR MAXROWS
  regArrays(nsec_update, nsec_update, nscalars+2,  1,   2,  secrows, 3);

    //       ID          PROMPT       SWITCH   SWITCH  COL NCHAR
  regCarray(SECNAMES  , "  sections", &zero,   &five,   0,  12);

  funCvar ( SECNAMES, secnames_trap, secnames_update, secnames_switch);
  funCvar (-SECNAMES,          NULL,            NULL,   prompt_switch);

/////////////////////// register array parameters:

  int do_fieldtypes   = _interface->showFieldtypes  ();
  int do_units        = _interface->showUnits       ();
  int do_hdrs         = _interface->showHdrs        ();
  int do_defaults     = _interface->showDefaults    ();
  int do_descriptions = _interface->showDescriptions();
  int do_widths       = _interface->showWidths      () && output;
  int do_converters   = _interface->showConverters  () && input;
  int do_maxchars     = _interface->showMaxchars    () && input;

  int txt_fields     = (_interface->switchFields       (-1) != 2);
  int txt_columns    = (_interface->switchColumnNumbers(-1) != 2);
  int txt_fieldtypes = (_interface->switchFieldtypes   (-1) != 2);
  int txt_units      = (_interface->switchUnits        (-1) != 2);
  int txt_hdrs       = (_interface->switchHdrs         (-1) != 2);
  int txt_defaults   = (_interface->switchDefaults     (-1) != 2);

    //         N         NMAX     ROW COL NCHAR  MAXROWS  MAXROWS_INIT
  regArrays(n_update, nmax_update, 2, 23,   0,   maxrows,  10);

                         //      ID           PROMPT    SWITCH SWITCH COL NCHAR
                      regIarray(COLUMNS    , "col"        , &zero,&one ,23, 3);
                      regCarray(FIELDS     , "  fields  " , &zero,&one , 0,10);
  if(do_fieldtypes  ) regCarray(FIELDTYPES , "fieldtypes" , &zero,&one , 0,10);
  if(do_units       ) regCarray(UNITS      , "  units"    , &zero,&one , 0, 9);
  if(do_hdrs        ) regIarray(HDRS       , "hdrs"       , &zero,&one , 0, 4);
  if(do_defaults    ) regCarray(DEFAULTS   , "defaults"   , &zero,&one , 0, 8);
  if(do_descriptions) regCarray(DESCRIPTION, "        description",
                                                            &zero,&one , 0,27);
  if(do_widths      ) regIarray(WIDTHS     , "widths"     , &zero,&one , 0, 6);
  if(do_converters  ) regCarray(CONVERTERS , "converters" , &zero,&two , 0,10);
  if(do_maxchars    ) regIarray(MAXCHARS   , "maxchars"   , &zero,&one , 0, 8);

  funIvar ( COLUMNS    ,     ibutton_trap,   ivar_update,   array_switch);
  funCvar ( FIELDS     ,     cbutton_trap,   cvar_update,  fields_switch);
  funCvar ( FIELDTYPES ,     cbutton_trap,   cvar_update,   array_switch);
  funCvar ( UNITS      ,     cbutton_trap,   cvar_update,   array_switch);
  funIvar ( HDRS       ,     ibutton_trap,   ivar_update,   array_switch);
  funCvar ( DEFAULTS   ,     cbutton_trap,   cvar_update,   array_switch);
  funCvar ( DESCRIPTION,       ctext_trap,   cvar_update,   array_switch);
  funIvar ( WIDTHS     ,     ibutton_trap,   ivar_update,   array_switch);
  funCvar ( CONVERTERS ,     cbutton_trap,   cvar_update,   array_switch);
  funIvar ( MAXCHARS   ,     ibutton_trap,   ivar_update,   array_switch);

if(txt_columns   ) funIvar(COLUMNS   , itext_trap, ivar_update, array_switch);
if(txt_fields    ) funCvar(FIELDS    , ctext_trap, cvar_update,fields_switch);
if(txt_fieldtypes) funCvar(FIELDTYPES, ctext_trap, cvar_update, array_switch);
if(txt_units     ) funCvar(UNITS     , ctext_trap, cvar_update, array_switch);
if(txt_hdrs      ) funIvar(HDRS      , itext_trap, ivar_update, array_switch);
if(txt_defaults  ) funCvar(DEFAULTS  , ctext_trap, cvar_update, array_switch);

  funCvar (-COLUMNS    ,            NULL,            NULL,  prompt_switch);
  funCvar (-FIELDS     ,            NULL,            NULL,  prompt_switch);
  funCvar (-FIELDTYPES ,            NULL,            NULL,  prompt_switch);
  funCvar (-UNITS      ,            NULL,            NULL,  prompt_switch);
  funIvar (-HDRS       ,            NULL,            NULL,  prompt_switch);
  funCvar (-DEFAULTS   ,            NULL,            NULL,  prompt_switch);
  funCvar (-WIDTHS     ,    pwidths_trap,            NULL,  prompt_switch);
  funCvar (-CONVERTERS ,            NULL,            NULL,  prompt_switch);
  funCvar (-MAXCHARS   ,            NULL,            NULL,  prompt_switch);
}


//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//
//---------------------------------- end ----------------------------------//

