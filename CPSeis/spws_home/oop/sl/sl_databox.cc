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

//---------------------- sl_databox.cc ---------------------------------//
//---------------------- sl_databox.cc ---------------------------------//
//---------------------- sl_databox.cc ---------------------------------//

//         implementation file for the SLDatabox derived class
//                      subdirectory sl


#include "sl/sl_databox.hh"
#include "sl/sl_databox_hardcopy.hh"
#include "sl/sl_prim.hh"
#include "wbox.h"
#include "named_constants.h"


//---------------------- update stuff ---------------------//
//---------------------- update stuff ---------------------//
//---------------------- update stuff ---------------------//


static void update_guis_from_wbox(void * /* data */)    // registered with wbox
{
  ////  cout << "am updating guis from wbox" << endl;
  SLDelay::updateAll();
}


static void update_wbox_from_guis(void * /* data */)    // registered with guis
{
  ////  cout << "am updating wbox from guis (SLDelays and DFs)" << endl;
  wbox_update();
}


static void prepare_updates(void)
{
  static long already_done = FALSE;

  if(already_done) return;
  wbox_additional_update(update_guis_from_wbox, NULL);
  PrimSupport::addUpdateFunction(update_wbox_from_guis, NULL);
  already_done = TRUE;
}



//---------------- constructors -----------------------------------//
//---------------- constructors -----------------------------------//
//---------------- constructors -----------------------------------//


SLDatabox::SLDatabox(SLDelay *slparent, char *name,
                     void *user_data, long traptype, int omit, int nrows_init)
        : SLDelay(slparent, name),
          _box        (NULL),
          _user_data  (user_data),
          _traptype   (traptype),
          _omit       (omit),
          _nrows_init (nrows_init),
          _make_trap  (NULL),
          _make_data  (NULL)
{
  _dflist = new DatafieldList();
}


SLDatabox::SLDatabox(Widget wparent, char *name,
                     void *user_data, long traptype, int omit, int nrows_init)
        : SLDelay(wparent, name),
          _box       (NULL),
          _user_data (user_data),
          _traptype  (traptype),
          _omit      (omit),
          _nrows_init (nrows_init),
          _make_trap (NULL),
          _make_data (NULL)
{
  _dflist = new DatafieldList();
}


SLDatabox::SLDatabox(Widget w,
                     void *user_data, long traptype, int omit, int nrows_init)
        : SLDelay(XtParent(w), XtName(w)),
          _box        (NULL),
          _user_data  (user_data),
          _traptype   (traptype),
          _omit       (omit),
          _nrows_init (nrows_init),
          _make_trap  (NULL),
          _make_data  (NULL)
{
  setTopWidget(w);
  _dflist = new DatafieldList();
}


//------------------ destructor -------------------------//
//------------------ destructor -------------------------//
//------------------ destructor -------------------------//

SLDatabox::~SLDatabox(void)
{
  if(made()) wbox_destroy_box(_box);          // added 5/15/00.
}


//------------------ static get user data --------------------//
//------------------ static get user data --------------------//
//------------------ static get user data --------------------//

void *SLDatabox::getUserData(void *box)
{
  SLDatabox *databox = (SLDatabox*)wbox_get_userdata(box);
  return databox->getUserData();
}



//--------------------- set make trap -----------------------//
//--------------------- set make trap -----------------------//
//--------------------- set make trap -----------------------//

//   The registered trap is called from make(), just after calling
//     makeChildren().
//   The function _make_trap(_make_data) is to be used for old-style
//     registrations using wbox_... routines.

void SLDatabox::setMakeTrap(DataboxMakeTrap *trap, void *data)
{
  _make_trap = trap;
  _make_data = data;
}


//----------------------- show message -------------------------//
//----------------------- show message -------------------------//
//----------------------- show message -------------------------//

void SLDatabox::showMessage(const char *msg)
{
  wbox_messageline(_box, msg);
}



//----------------------- set focus ----------------------------//
//----------------------- set focus ----------------------------//
//----------------------- set focus ----------------------------//

void SLDatabox::setFocus(int ident, int index)
{
  wbox_set_focus(_box, ident, index + 1);
}



//----------------------- save table ---------------------------//
//----------------------- save table ---------------------------//
//----------------------- save table ---------------------------//

void SLDatabox::saveTable(const char *filename)  const
{
  wbox_save_table(_box, filename);
}



/*
//------------------------- hardcopy trap ---------------------------//
//------------------------- hardcopy trap ---------------------------//
//------------------------- hardcopy trap ---------------------------//

static char default_filename_buffer[222];

static void hardcopy_yes(void *data)
{
  SLDatabox *databox = (SLDatabox*)data;
  databox->saveTable(default_filename_buffer);
}


static const char *hardcopy_trap
                     (void *data, int numlines, const char *default_filename)
{
  SLDatabox *databox = (SLDatabox*)data;
  char question[900];
  sprintf(question, "Hardcopy file will contain about %d lines\n", numlines);
  strcat(question, "----\n");
  strcat(question, "The filename will be\n");
  strcat(question, default_filename);
  strcat(question, "\n----\n");
  strcat(question, "Do you want to save this hardcopy file now?");
  strcpy(default_filename_buffer, default_filename);
  new SLQuestPop
          (databox, "Question", question, TRUE, hardcopy_yes, NULL, databox);
  return NULL;
}
*/


static const char *hardcopy_trap
                     (void *data, int numlines, const char *default_filename)
{
  SLDatabox *databox = (SLDatabox*)data;
  new SLDataboxHardcopy(databox, databox, numlines, default_filename);
  return NULL;
}



//---------------------- make ---------------------------------//
//---------------------- make ---------------------------------//
//---------------------- make ---------------------------------//

Widget SLDatabox::make(Widget p)
{
  if(!made())
       {
/*
       if(_omit) _box = wbox_create1_omit (NULL, (int)_traptype);
       else      _box = wbox_create1      (NULL, (int)_traptype);
*/
       _box = wbox_create1_more (NULL, (int)_traptype, _omit, _nrows_init);
       wbox_put_userdata(_box, (void*)this);
       wbox_reg_hardcopy(_box, hardcopy_trap);
       prepare_updates();
       makeHelper();
       Widget w = SLDelay::make(p);
       w = wbox_create2((char*)instanceName(), NULL, wParent(), w,
                              getHelpCtx(), NULL, NULL);
       if(!w) return NULL;
       setTopWidget(w);
       XtManageChild(w);
       XtVaSetValues(w, XmNuserData, _box, NULL);
       SLPrim::updateEverything();
       }
  return topWidget();
}


//-------------------- re-register pointers, etc. ----------------------//
//-------------------- re-register pointers, etc. ----------------------//
//-------------------- re-register pointers, etc. ----------------------//

Boolean SLDatabox::isVisible(long ident)
{ return wbox_is_visible(_box, (int)ident); }


void SLDatabox::nnewreg(long ident, long *point)
{ wbox_nnewreg(_box, (int)ident, point); }

void SLDatabox::inewreg(long ident, long *point)
{ wbox_inewreg(_box, (int)ident, point); }

void SLDatabox::fnewreg(long ident, float *point)
{ wbox_fnewreg(_box, (int)ident, point); }

void SLDatabox::dnewreg(long ident, double *point)
{ wbox_dnewreg(_box, (int)ident, point); }

void SLDatabox::cnewreg(long ident, char *point)
{ wbox_cnewreg(_box, (int)ident, point); }



//---------------------- create linked array set ---------------------//
//---------------------- create linked array set ---------------------//
//---------------------- create linked array set ---------------------//


void SLDatabox::regArrays(DataboxNupdate *nupdate, DataboxNupdate *nmaxupdate,
                          int irow, int icol, int nchar, int numrow,
                          int numrow_init)
{
  dbox_rega2(nupdate, nmaxupdate, irow, icol, nchar, numrow, numrow_init);
}



//---------------------- create scalar datafields ------------------------//
//---------------------- create scalar datafields ------------------------//
//---------------------- create scalar datafields ------------------------//


void SLDatabox::regMsg   (const char *msg, int irow, int icol)
{
  wbox_mreg((char*)msg, irow, icol);
}


void SLDatabox::regBlank                  (int irow, int icol)
{
  regMsg(" ", irow, icol);
}


void SLDatabox::regString
                 (int ident, const char *msg, long *isw, int irow, int icol)
{
  int nchar = strlen(msg);
  int length = nchar;
  bbox_creg(ident, irow, icol, nchar, length);
  dbox_set_cpoint(ident, (char*)msg);
  dbox_set_spoint(ident, isw);
}


void SLDatabox::regIvar (int ident, long *isw, int irow, int icol, int nchar)
{
  dbox_ireg(ident, isw, irow, icol, nchar, 0);
}


void SLDatabox::regFvar (int ident, long *isw, int irow, int icol, int nchar,
                                                                int ndec)
{
  dbox_freg(ident, isw, irow, icol, nchar, ndec);
}


void SLDatabox::regDvar (int ident, long *isw, int irow, int icol, int nchar,
                                                                int ndec)
{
  dbox_dreg(ident, isw, irow, icol, nchar, ndec);
}


void SLDatabox::regCvar (int ident, long *isw, int irow, int icol, int nchar)
{
  dbox_creg(ident, isw, irow, icol, nchar, 0);
}



//------------------- create double scalar datafields --------------------//
//------------------- create double scalar datafields --------------------//
//------------------- create double scalar datafields --------------------//


void SLDatabox::regIvar2 (int ident, const char *prompt, long *jsw,
                        long *isw, int irow, int icol, int nchar)
{
  dbox_ireg2(ident, (char*)prompt, jsw, isw, irow, icol, nchar, 0);
}


void SLDatabox::regFvar2 (int ident, const char *prompt, long *jsw,
                        long *isw, int irow, int icol, int nchar, int ndec)
{
  dbox_freg2(ident, (char*)prompt, jsw, isw, irow, icol, nchar, ndec);
}


void SLDatabox::regDvar2 (int ident, const char *prompt, long *jsw,
                        long *isw, int irow, int icol, int nchar, int ndec)
{
  dbox_dreg2(ident, (char*)prompt, jsw, isw, irow, icol, nchar, ndec);
}


void SLDatabox::regCvar2 (int ident, const char *prompt, long *jsw,
                        long *isw, int irow, int icol, int nchar)
{
  dbox_creg2(ident, (char*)prompt, jsw, isw, irow, icol, nchar, 0);
}


void SLDatabox::regIvar3 (int ident, const char *prompt, long *jsw,
                        long *isw, int irow, int icol, int nchar)
{
  dbox_ireg3(ident, (char*)prompt, jsw, isw, irow, icol, nchar, 0);
}



//------------------------- create datafield arrays ----------------------//
//------------------------- create datafield arrays ----------------------//
//------------------------- create datafield arrays ----------------------//


void SLDatabox::regIarray (int ident, const char *prompt, long *jsw,
                        long *isw,           int icol, int nchar)
{
  int lengthp = nchar;
  if(prompt) lengthp = strlen(prompt);
  int ncharp = lengthp;      // different from bbox routine.

  bbox_irega(ident, ncharp, lengthp, icol, nchar);
  dbox_set_cpoint      (-ident, (char*)prompt);
  dbox_set_spoint      (-ident, jsw);
  dbox_set_spoint      ( ident, isw);
/*
  dbox_irega(ident, (char*)prompt, jsw, isw, icol, nchar, 0);
*/
}


void SLDatabox::regFarray (int ident, const char *prompt, long *jsw,
                        long *isw,           int icol, int nchar, int ndec)
{
  int lengthp = nchar;
  if(prompt) lengthp = strlen(prompt);
  int ncharp = lengthp;      // different from bbox routine.

  bbox_frega(ident, ncharp, lengthp, icol, nchar, ndec);
  dbox_set_cpoint      (-ident, (char*)prompt);
  dbox_set_spoint      (-ident, jsw);
  dbox_set_spoint      ( ident, isw);
/*
  dbox_frega(ident, (char*)prompt, jsw, isw, icol, nchar, ndec);
*/
}


void SLDatabox::regDarray (int ident, const char *prompt, long *jsw,
                        long *isw,           int icol, int nchar, int ndec)
{
  int lengthp = nchar;
  if(prompt) lengthp = strlen(prompt);
  int ncharp = lengthp;      // different from bbox routine.

  bbox_drega(ident, ncharp, lengthp, icol, nchar, ndec);
  dbox_set_cpoint      (-ident, (char*)prompt);
  dbox_set_spoint      (-ident, jsw);
  dbox_set_spoint      ( ident, isw);
/*
  dbox_drega(ident, (char*)prompt, jsw, isw, icol, nchar, ndec);
*/
}


void SLDatabox::regCarray (int ident, const char *prompt, long *jsw,
                        long *isw,           int icol, int nchar)
{
  int lengthp = nchar;
  if(prompt) lengthp = strlen(prompt);
  int ncharp = lengthp;      // different from bbox routine.

  int length = nchar;

  bbox_crega(ident, ncharp, lengthp, icol, nchar, length);
  dbox_set_cpoint      (-ident, (char*)prompt);
  dbox_set_spoint      (-ident, jsw);
  dbox_set_spoint      ( ident, isw);
/*
  dbox_crega(ident, (char*)prompt, jsw, isw, icol, nchar, 0);
*/
}



//------------------- register traps and update functions ---------------//
//------------------- register traps and update functions ---------------//
//------------------- register traps and update functions ---------------//


void SLDatabox::trapIvar    (int ident, DataboxItrap *trap)
{
  dbox_set_itrap(ident, trap);
}


void SLDatabox::trapFvar    (int ident, DataboxFtrap *trap)
{
  dbox_set_ftrap(ident, trap);
}


void SLDatabox::trapDvar    (int ident, DataboxDtrap *trap)
{
  dbox_set_dtrap(ident, trap);
}


void SLDatabox::trapCvar    (int ident, DataboxCtrap *trap)
{
  dbox_set_ctrap(ident, trap);
}



void SLDatabox::updateIvar  (int ident, DataboxIupdate *update)
{
  dbox_set_ifun(ident, update);
}


void SLDatabox::updateFvar  (int ident, DataboxFupdate *update)
{
  dbox_set_ffun(ident, update);
}


void SLDatabox::updateDvar  (int ident, DataboxDupdate *update)
{
  dbox_set_dfun(ident, update);
}


void SLDatabox::updateCvar  (int ident, DataboxCupdate *update)
{
  dbox_set_cfun(ident, update);
}


void SLDatabox::updateSwitch  (int ident, DataboxIupdate *sw)
{
  dbox_set_sfun(ident, sw);
}



void SLDatabox::funIvar    (int ident, DataboxItrap   *trap,
                                       DataboxIupdate *update,
                                       DataboxIupdate *sw)
{
  if(trap)   dbox_set_itrap(ident, trap);
  if(update) dbox_set_ifun (ident, update);
  if(sw)     dbox_set_sfun (ident, sw);
}


void SLDatabox::funFvar    (int ident, DataboxFtrap   *trap,
                                       DataboxFupdate *update,
                                       DataboxIupdate *sw)
{
  if(trap)   dbox_set_ftrap(ident, trap);
  if(update) dbox_set_ffun (ident, update);
  if(sw)     dbox_set_sfun (ident, sw);
}


void SLDatabox::funDvar    (int ident, DataboxDtrap   *trap,
                                       DataboxDupdate *update,
                                       DataboxIupdate *sw)
{
  if(trap)   dbox_set_dtrap(ident, trap);
  if(update) dbox_set_dfun (ident, update);
  if(sw)     dbox_set_sfun (ident, sw);
}


void SLDatabox::funCvar    (int ident, DataboxCtrap   *trap,
                                       DataboxCupdate *update,
                                       DataboxIupdate *sw)
{
  if(trap)   dbox_set_ctrap(ident, trap);
  if(update) dbox_set_cfun (ident, update);
  if(sw)     dbox_set_sfun (ident, sw);
}



//------------------------ histogram routines --------------------------//
//------------------------ histogram routines --------------------------//
//------------------------ histogram routines --------------------------//

      // public.

///// these static variables work as long as
///// histogramHeader is called before histogramBar (or Bar2):

static float VMIN  = -1.0;
static float VMAX  =  1.0;
static int   NCHAR =    1;
static int   IZERO =    0;
static char  BUFFER[222];


static int get_index(float value)
{
  if(value == FNIL) return -1;
  float f = (NCHAR - 1) * (value - VMIN) / (VMAX - VMIN);
  long  i = NearestInteger(f);
  i = ConstrainValue(i, 0, NCHAR-1);
  return (int)i;
}


char *SLDatabox::histogramBar(float value)
{
  memset(BUFFER, ' ', NCHAR);
  BUFFER[NCHAR] = '\0';
  int i = get_index(value);
  if(i != -1)
      {
      if     (value > 0.0) BUFFER[i] = 'P';
      else if(value < 0.0) BUFFER[i] = 'M';
      }
  if(IZERO >= 0 && IZERO < NCHAR) BUFFER[IZERO] = '|';
  return BUFFER;
}


char *SLDatabox::histogramBar2(float value1, float value2)
{
  memset(BUFFER, ' ', NCHAR);
  BUFFER[NCHAR] = '\0';
  int i1 = get_index(value1);
  int i2 = get_index(value2);
  if(i1 == i2 && i1 != -1)
      {
      if     (value1 > 0.0) BUFFER[i1] = '+';
      else if(value1 < 0.0) BUFFER[i1] = '-';
      }
  else
      {
      if(i1 != -1)
          {
          if     (value1 > 0.0) BUFFER[i1] = 'P';
          else if(value1 < 0.0) BUFFER[i1] = 'M';
          }
      if(i2 != -1)
          {
          if     (value2 > 0.0) BUFFER[i2] = 'p';
          else if(value2 < 0.0) BUFFER[i2] = 'm';
          }
      }
  if(IZERO >= 0 && IZERO < NCHAR) BUFFER[IZERO] = '|';
  return BUFFER;
}


char *SLDatabox::histogramHeader(float vmin, float vmax, int nchar)
{
  NCHAR = nchar;
  int ivmin = NearestInteger(vmin - 0.49);
  int ivmax = NearestInteger(vmax + 0.49);
  if(ivmin == ivmax) { ivmin--; ivmax++; }
  VMIN = ivmin;
  VMAX = ivmax;
  IZERO = get_index(0.0);
  memset(BUFFER, ' ', NCHAR);
  BUFFER[NCHAR] = '\0';
  char lower[10], upper[10];
  sprintf(lower, "%d", ivmin);
  sprintf(upper, "%d", ivmax);
  int n1 = strlen(lower);
  int n2 = strlen(upper);
  memcpy( BUFFER            , lower, n1);
  memcpy(&BUFFER[NCHAR - n2], upper, n2);
  if(IZERO > 0 && IZERO < NCHAR-1 && BUFFER[IZERO] == ' ' &&
     BUFFER[IZERO-1] == ' ' && BUFFER[IZERO+1] == ' ') BUFFER[IZERO] = '0';
  return BUFFER;
}



//----------------------------- end ----------------------------------//
//----------------------------- end ----------------------------------//
//----------------------------- end ----------------------------------//
