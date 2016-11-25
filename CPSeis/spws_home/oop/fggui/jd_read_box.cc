
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
//---------------------------- jd_read_box.cc --------------------------//
//---------------------------- jd_read_box.cc --------------------------//
//---------------------------- jd_read_box.cc --------------------------//

//           implementation file for the JdReadBox class
//                 derived from the SLDatabox class
//                       subdirectory fggui


#include "fggui/jd_read_box.hh"
#include "geom/jd_file.hh"
#include "geom/fg_constants.hh"
#include "geom/geomio_wrapper.hh"
#include "named_constants.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>


  enum { VE = 1, REF, FIXDIST, CHAINING, AUGMENT };


//------------------------- constructor and destructor ---------------------//
//------------------------- constructor and destructor ---------------------//
//------------------------- constructor and destructor ---------------------//


JdReadBox::JdReadBox (SLDelay *slparent, JdFile *file)
     : SLDatabox (slparent, "jd_read_box", NULL, 4, TRUE),
            _file  (file)
{
}



JdReadBox::~JdReadBox ()
{
}



//------------------------------- traps ------------------------------------//
//------------------------------- traps ------------------------------------//
//------------------------------- traps ------------------------------------//



static void chaining_trap (void *data, long /*ident*/, long /*index*/,
  char * /*value*/, long /*nread*/, char *endkey)
{
  if(strcmp(endkey, "RETURN") != 0) return;
  JdReadBox *box = (JdReadBox *)data;
  JdFile   *file = box->getJdFile();
  int chaining = file->getInputChaining();
  if     (chaining == HORIZONTAL_CHAINING) chaining = SLOPE_CHAINING;
  else if(chaining == SLOPE_CHAINING)      chaining = NO_CHAINING;
  else                                     chaining = HORIZONTAL_CHAINING;
  file->setInputChaining(chaining);
}



static void ve_trap (void *data, long /*ident*/, long /*index*/,
                     float value, long nread, char * /*endkey*/)
{
  if (nread == 0) return;
  JdReadBox *box = (JdReadBox *)data;
  JdFile   *file = box->getJdFile();
  file->setInputVe(value);
}


static void ref_trap (void *data, long /*ident*/, long /*index*/,
                      float value, long nread, char * /*endkey*/)
{
  if (nread == 0) return;
  JdReadBox *box = (JdReadBox *)data;
  JdFile   *file = box->getJdFile();
  file->setInputRef(value);
}


static void fixdist_trap (void *data, long /*ident*/, long /*index*/,
                          float value, long nread, char * /*endkey*/)
{
  if (nread == 0) return;
  JdReadBox *box = (JdReadBox *)data;
  JdFile   *file = box->getJdFile();
  file->setInputFixdist(value);
}



static void augment_trap (void *data, long /*ident*/, long /*index*/,
  char * /*value*/, long /*nread*/, char *endkey)
{
  int step = 0;
  if     (strcmp(endkey, "RETURN" ) == 0) step =  1;
  else if(strcmp(endkey, "BUTTON2") == 0) step = -1;
  if(step == 0) return;
  JdReadBox     *box    = (JdReadBox *)data;
  JdFile        *file   = box->getJdFile();
  GeomioWrapper *geomio = file->geomioInput();
  geomio->stepGenericInputChoice(step);
}



//------------------------- update functions -----------------------------//
//------------------------- update functions -----------------------------//
//------------------------- update functions -----------------------------//


static char *chaining_update (void *data, long /*ident*/, long /*index*/)
{
  static const char horizontal [] = "horizontal";
  static const char slope      [] = "slope";
  static const char none       [] = "none";
  static const char unspecified[] = "unspecified";
  JdReadBox *box = (JdReadBox *)data;
  JdFile   *file = box->getJdFile();
  int   chaining = file->getInputChaining();
  if(chaining == HORIZONTAL_CHAINING) return (char*)horizontal;
  if(chaining == SLOPE_CHAINING)      return (char*)slope;
  if(chaining == NO_CHAINING)         return (char*)none;
  return (char*)unspecified;
}



static float ve_update (void *data, long /*ident*/, long /*index*/)
{
  JdReadBox *box = (JdReadBox *)data;
  JdFile   *file = box->getJdFile();
  return file->getInputVe();
}



static float ref_update (void *data, long /*ident*/, long /*index*/)
{
  JdReadBox *box = (JdReadBox *)data;
  JdFile   *file = box->getJdFile();
  return file->getInputRef();
}



static float fixdist_update (void *data, long /*ident*/, long /*index*/)
{
  JdReadBox *box = (JdReadBox *)data;
  JdFile   *file = box->getJdFile();
  return file->getInputFixdist();
}



static char *augment_update (void *data, long /*ident*/, long /*index*/)
{
  static const char read_ld [] = "LD cards";
  static const char read_rp [] = "RP cards";
  static const char read_pp [] = "PP cards";
  static const char read_zt1[] = "ZT1 cards";
  static const char read_zt2[] = "ZT2 cards";
  static const char read_zt3[] = "ZT3 cards";
  static const char read_zt4[] = "ZT4 cards";
  static const char none    [] = "none";
  JdReadBox     *box    = (JdReadBox *)data;
  JdFile        *file   = box->getJdFile();
  GeomioWrapper *geomio = file->geomioInput();
  int            choice = geomio->getGenericInputChoice();
  switch(choice)
      {
      case GeomioWrapper::CHOICE_LD  : return (char*)read_ld;
      case GeomioWrapper::CHOICE_RP  : return (char*)read_rp;
      case GeomioWrapper::CHOICE_PP  : return (char*)read_pp;
      case GeomioWrapper::CHOICE_ZT1 : return (char*)read_zt1;
      case GeomioWrapper::CHOICE_ZT2 : return (char*)read_zt2;
      case GeomioWrapper::CHOICE_ZT3 : return (char*)read_zt3;
      case GeomioWrapper::CHOICE_ZT4 : return (char*)read_zt4;
      case GeomioWrapper::CHOICE_NONE: return (char*)none;
      }
  return (char*)none;
}



//---------------------------- switch functions ---------------------------//
//---------------------------- switch functions ---------------------------//
//---------------------------- switch functions ---------------------------//


static long text_switch (void *data, long /*ident*/, long /*index*/)
{
  JdReadBox     *box    = (JdReadBox *)data;
  JdFile        *file   = box->getJdFile();
  GeomioWrapper *geomio = file->geomioInput();
  if(geomio->needGenericInputParameters()) return 1;
  return -77;
}


static long chaining_switch (void *data, long /*ident*/, long /*index*/)
{
  JdReadBox     *box    = (JdReadBox *)data;
  JdFile        *file   = box->getJdFile();
  GeomioWrapper *geomio = file->geomioInput();
  if(geomio->needGenericInputParameters()) return 2;
  return -77;
}


static long augment_switch (void *data, long /*ident*/, long /*index*/)
{
  JdReadBox     *box    = (JdReadBox *)data;
  JdFile        *file   = box->getJdFile();
  GeomioWrapper *geomio = file->geomioInput();
  if(geomio->needGenericInputChoice()) return 2;
  return -77;
}


static long prompt_switch (void *data, long /*ident*/, long /*index*/)
{
  JdReadBox     *box    = (JdReadBox *)data;
  JdFile        *file   = box->getJdFile();
  GeomioWrapper *geomio = file->geomioInput();
  if(geomio->needGenericInputParameters()) return 0;
  return -77;
}


static long aprompt_switch (void *data, long /*ident*/, long /*index*/)
{
  JdReadBox     *box    = (JdReadBox *)data;
  JdFile        *file   = box->getJdFile();
  GeomioWrapper *geomio = file->geomioInput();
  if(geomio->needGenericInputChoice()) return 0;
  return -77;
}


//------------------------------ make helper ----------------------------//
//------------------------------ make helper ----------------------------//
//------------------------------ make helper ----------------------------//


void JdReadBox::makeHelper ()
{
  static long zero   = 0;
  static long one    = 1;
  static long two    = 2;
  static long m44   = -44;

    //      ID          PROMPT          SWITCH ROW COL
  regString(0, "Input File Parameters" , &m44,  1, 39);

    //       ID         PROMPT               SWITCH SWITCH ROW COL NCHAR
  regFvar2(VE      ,"    Ve:"               ,&zero, &one  , 2,  7,   8, 0);
  regFvar2(REF     ,"    Ref:"              ,&zero, &one  , 2, -1,  10, 0);
  regFvar2(FIXDIST ,"    Fixdist:"          ,&zero, &one  , 2, -1,   8, 3);
  regCvar2(CHAINING,"    Chaining:"         ,&zero, &two  , 2, -1,  12);
  regCvar2(AUGMENT ,"Type of cards to read from this section:"
                                            ,&zero, &two  , 3, 25,  10);

  funFvar ( VE      ,       ve_trap,       ve_update,     text_switch);
  funFvar ( REF     ,      ref_trap,      ref_update,     text_switch);
  funFvar ( FIXDIST ,  fixdist_trap,  fixdist_update,     text_switch);
  funCvar ( CHAINING, chaining_trap, chaining_update, chaining_switch);
  funCvar ( AUGMENT ,  augment_trap,  augment_update,  augment_switch);
  funCvar (-VE      ,          NULL,            NULL,   prompt_switch);
  funCvar (-REF     ,          NULL,            NULL,   prompt_switch);
  funCvar (-FIXDIST ,          NULL,            NULL,   prompt_switch);
  funCvar (-CHAINING,          NULL,            NULL,   prompt_switch);
  funCvar (-AUGMENT ,          NULL,            NULL,  aprompt_switch);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
