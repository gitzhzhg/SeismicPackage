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

//---------------------- sl_matrix_view_box.cc -----------------------//
//---------------------- sl_matrix_view_box.cc -----------------------//
//---------------------- sl_matrix_view_box.cc -----------------------//

//         implementation file for the SLMatrixViewBox class
//                derived from the SLDatabox class
//                       subdirectory sl


     // Displays a matrix of double-precision values of any kind
     // in a windowbox.  The matrix has a variable number of rows
     // and columns.

     // Virtual functions to override:
     //
     //
     // newFirstVisibleColumn      (OPTIONAL)
     //     If you want to be notified when the first displayed column
     //     has been changed by the user manipulating this gui.
     //
     // newNumVisibleColumns       (OPTIONAL)
     //     If you want to be notified when the number of displayed
     //     columns has been changed by the user manipulating this gui.
     //
     // numRowsUpdate              (REQUIRED)
     //     To return the number of rows in the matrix.
     //
     // numColumnsUpdate           (REQUIRED)
     //     To return the number of columns in the matrix.
     //
     // readyToUpdate              (OPTIONAL)
     //     Called when an update is ready to commence.  Needs to be
     //     overridden if the derived class needs to take special
     //     action before the matrixUpdate and promptUpdate functions
     //     are called.
     //
     //
     //
     // matrixUpdate               (REQUIRED)
     //     To return the value of the matrix element to be displayed.
     //
     // promptUpdate               (OPTIONAL)
     //     To return the prompt at the top of each column.  If not
     //     overridden, the prompt will be simply the matrix column number.
     //
     // integerUpdate              (OPTIONAL)
     //     To return the integer at the right of each row.  If not
     //     overridden, the integer will be simply the matrix row number.
     //
     // messageUpdate              (OPTIONAL)
     //     To return the message at the right of each row.  If not
     //     overridden, the message will be blank.
     //
     //
     //
     // matrixSwitchUpdate         (OPTIONAL)
     //     To return the switch value for the matrix element.  If not
     //     overridden, the switch value will be 5 (number on white
     //     background in a box, enterable but not editable).
     //
     // promptSwitchUpdate         (OPTIONAL)
     //     To return the switch value for the prompt at the top of
     //     each column.  If not overridden, the switch value will
     //     be -44 (white letters).
     //
     // integerSwitchUpdate        (OPTIONAL)
     //     To return the switch value for the integer at the right of
     //     each row.  If not overridden, the switch value will
     //     be -44 (white letters).
     //
     // messageSwitchUpdate        (OPTIONAL)
     //     To return the switch value for the message at the right of
     //     each row.  If not overridden, the switch value will
     //     be -44 (white letters).
     //
     //
     //
     // matrixTrap                 (MAYBE)
     //     Needed if the matrix switch value allows user operation
     //     on the matrix datafields.
     //
     // promptTrap                 (MAYBE)
     //     Needed if the prompt switch value allows user operation
     //     on the prompt datafields.
     //
     // integerTrap                (MAYBE)
     //     Needed if the integer switch value allows user operation
     //     on the integer datafields.
     //
     // messageTrap                (MAYBE)
     //     Needed if the message switch value allows user operation
     //     on the message datafields.



#include "sl/sl_matrix_view_box.hh"
#include "sl/sl_prim.hh"
#include "wbox.h"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>



//---------------------- constructors -----------------------//
//---------------------- constructors -----------------------//
//---------------------- constructors -----------------------//


SLMatrixViewBox::SLMatrixViewBox(Widget wparent, char *name, HelpCtx hctx,
               long nchar, long ndec, long nrowmax, long ncolmax,
               long width1, long width2, long width3)
           : SLDatabox(wparent, name, hctx, 4),
                 _NCHAR      (nchar),
                 _NDEC       (ndec),
                 _NROWMAX    (nrowmax),
                 _NCOLMAX    (ncolmax),
                 _WIDTH1     (width1),
                 _WIDTH2     (width2),
                 _WIDTH3     (width3),
                 _first      (1),
                 _numvis     (ncolmax)
{
  constructorHelper();
}


SLMatrixViewBox::SLMatrixViewBox(SLDelay *slparent, char *name, HelpCtx hctx,
               long nchar, long ndec, long nrowmax, long ncolmax,
               long width1, long width2, long width3)
           : SLDatabox(slparent, name, hctx, 4),
                 _NCHAR      (nchar),
                 _NDEC       (ndec),
                 _NROWMAX    (nrowmax),
                 _NCOLMAX    (ncolmax),
                 _WIDTH1     (width1),
                 _WIDTH2     (width2),
                 _WIDTH3     (width3),
                 _first      (1),
                 _numvis     (ncolmax)
{
  constructorHelper();
}



//---------------------- constructor helper --------------------//
//---------------------- constructor helper --------------------//
//---------------------- constructor helper --------------------//

void SLMatrixViewBox::constructorHelper()
{
  assert(_NCHAR   >  0 && _NCHAR   <= 20);
  assert(_NDEC    >= 0 && _NDEC    <= 20);
  assert(_NROWMAX >  0 && _NROWMAX <= 40);
  assert(_NCOLMAX >  0 && _NCOLMAX <= 10);
  assert(_WIDTH1  >  0 && _WIDTH1  <= 10);
  assert(_WIDTH2  >= 0 && _WIDTH2  <= 10);
  assert(_WIDTH3  >= 0 && _WIDTH2  <= 80);
  SLPrim::updateEverything();
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

SLMatrixViewBox::~SLMatrixViewBox()
{
}



//------------------ methods to set values --------------------------//
//------------------ methods to set values --------------------------//
//------------------ methods to set values --------------------------//

       // public.

int SLMatrixViewBox::resetFirstVisibleColumn()
{
  return setFirstVisibleColumn(_first);
}


int SLMatrixViewBox::setFirstVisibleColumn(long first)
{
  Boolean changed = setFirstVisibleColumn(first, FALSE);
  if(changed) SLPrim::updateEverything();
  return changed;
}


void SLMatrixViewBox::makeColumnVisible(long icol)
{
  long first = _first;
  long last = _first + _numvis - 1;
  if(icol >= _first && icol <= last) return;
  long ncol = numColumnsUpdate();
  if(icol < 1 || icol > ncol) return;
  if     (icol == _first - 1 && icol >=    1) first--;
  else if(icol ==   last + 1 && icol <= ncol) first++;
  else first = icol - (_numvis - 1) / 2;
  Boolean changed = setFirstVisibleColumn(first, FALSE);
  if(changed) SLPrim::updateEverything();
}



void SLMatrixViewBox::makeRowVisible(long irow)
{
  long ident = 1;
  void *box = getBox();
  wbox_set_focus(box, (int)ident, (int)irow);
}



void SLMatrixViewBox::showMessage(char *msg)
{
  wbox_show_message(msg);
}


void SLMatrixViewBox::maybeShowMessage(char *msg)
{
  wbox_maybe_show_message(msg);
}



//-------------- internal methods to set values ----------------------//
//-------------- internal methods to set values ----------------------//
//-------------- internal methods to set values ----------------------//

  // private.
  // The argument 'internal' is FALSE when called by user,
  //   and TRUE when called internally from a trap in this class.
  // When 'internal' is TRUE, the appropriate virtual function is called.


int SLMatrixViewBox::setFirstVisibleColumn(long first, int internal)
{
  long ncol = numColumnsUpdate();          // virtual function
  if(first > ncol) first = ncol;
  if(first <    1) first = 1;
//  first = ConstrainValue(first, 1, ncol);
  if(first == _first) return FALSE;
  _first = first;
  if(internal) newFirstVisibleColumn(first);    // virtual function
  return TRUE;
}



//------------------ find num visible columns ---------------//
//------------------ find num visible columns ---------------//
//------------------ find num visible columns ---------------//

       // private.

void SLMatrixViewBox::findNumVisibleColumns()
{
  long numvis = 0;
  for(long ident = 1; ident <= _NCOLMAX; ident++)
      {
      if(isVisible(ident)) numvis++;
      }
  if(numvis == _numvis) return;
  _numvis = numvis;
  newNumVisibleColumns(numvis);      // virtual function
}



//----------------------- trap helper -----------------------//
//----------------------- trap helper -----------------------//
//----------------------- trap helper -----------------------//

       // private.
       // ident must be absolute value of the ident.

void SLMatrixViewBox::trapHelper(long ident, char *endkey)
{
  if(strings_equal("REDRAW", endkey))
            {
            readyToUpdate();              // virtual function
            return;
            }
  long ncol = numColumnsUpdate();         // virtual function
  findNumVisibleColumns();                // search
  long last_first = ncol - _numvis + 1;
  if(strings_equal("LEFT", endkey) && ident == 1)
            {
            setFirstVisibleColumn(_first - 1, TRUE);
            strcpy(endkey, "junk");
            }
  else if(strings_equal("RIGHT", endkey) && ident == _numvis)
            {
            setFirstVisibleColumn
                     (MinimumValue(_first + 1, last_first), TRUE);
            strcpy(endkey, "junk");
            }
  else if(strings_equal("^LEFT", endkey) && ident == 1)
            {
            setFirstVisibleColumn(_first - _numvis, TRUE);
            strcpy(endkey, "junk");
            }
  else if(strings_equal("^RIGHT", endkey) && ident == _numvis)
            {
            setFirstVisibleColumn
                     (MinimumValue(_first + _numvis, last_first), TRUE);
            strcpy(endkey, "junk");
            }
  else if(strings_equal("@LEFT", endkey) && ident == 1)
            {
            setFirstVisibleColumn(1, TRUE);
            strcpy(endkey, "junk");
            }
  else if(strings_equal("@RIGHT", endkey) && ident == _numvis)
            {
            setFirstVisibleColumn(last_first, TRUE);
            strcpy(endkey, "junk");
            }
}



//---------------- virtual functions to override -----------------//
//---------------- virtual functions to override -----------------//
//---------------- virtual functions to override -----------------//

       // protected.

char  *SLMatrixViewBox::promptUpdate (long icol)
{
  static char prompt[25];
  sprintf(prompt, "%d", icol);
  return prompt;
}


long   SLMatrixViewBox::integerUpdate (long irow)
{
  return irow;
}


char  *SLMatrixViewBox::messageUpdate (long /*irow*/)
{
  static char *blank = " ";
  return blank;
/*
  static char message[25];
  sprintf(message, "%d", irow);
  return message;
*/
}



//-------------------- static member functions --------------------//
//-------------------- static member functions --------------------//
//-------------------- static member functions --------------------//

       // private.
       // registered with windowbox routines.

long   SLMatrixViewBox::staticNumRowsUpdate    (void *data)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  return THIS->numRowsUpdate();
}



double SLMatrixViewBox::staticMatrixUpdate
                         (void *data, long ident, long index)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  long irow = index + 1;
  long icol = THIS->_first + ident - 1;
  return THIS->matrixUpdate(irow, icol);
}


char  *SLMatrixViewBox::staticPromptUpdate
                         (void *data, long ident, long /*index*/)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  long icol = THIS->_first - ident - 1;      // ident is negative
  return THIS->promptUpdate(icol);
}


long   SLMatrixViewBox::staticIntegerUpdate
                         (void *data, long /*ident*/, long index)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  long irow = index + 1;
  return THIS->integerUpdate(irow);
}


char  *SLMatrixViewBox::staticMessageUpdate
                         (void *data, long /*ident*/, long index)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  long irow = index + 1;
  return THIS->messageUpdate(irow);
}



long   SLMatrixViewBox::staticMatrixSwitchUpdate
                        (void *data, long ident, long index)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  long irow = index + 1;
  long icol = THIS->_first + ident - 1;
  return THIS->matrixSwitchUpdate(irow, icol);
}


long   SLMatrixViewBox::staticPromptSwitchUpdate
                        (void *data, long ident, long /*index*/)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  long icol = THIS->_first - ident - 1;      // ident is negative
  return THIS->promptSwitchUpdate(icol);
}


long   SLMatrixViewBox::staticIntegerSwitchUpdate
                        (void *data, long /*ident*/, long index)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  long irow = index + 1;
  return THIS->integerSwitchUpdate(irow);
}


long   SLMatrixViewBox::staticMessageSwitchUpdate
                        (void *data, long /*ident*/, long index)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  long irow = index + 1;
  return THIS->messageSwitchUpdate(irow);
}



void   SLMatrixViewBox::staticMatrixTrap
                         (void *data, long ident, long index,
                          double dvar, long nread, char *endkey)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  long irow = index + 1;
  long icol = THIS->_first + ident - 1;
  THIS->matrixTrap(irow, icol, dvar, nread, endkey);
  THIS->trapHelper(ident, endkey);
}


void   SLMatrixViewBox::staticPromptTrap
                         (void *data, long ident, long /*index*/,
                          char  *cvar, long nread, char *endkey)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  long icol = THIS->_first - ident - 1;      // ident is negative
  THIS->promptTrap(icol, cvar, nread, endkey);
  THIS->trapHelper(-ident, endkey);
}


void   SLMatrixViewBox::staticIntegerTrap
                         (void *data, long /*ident*/, long index,
                          long   ivar, long nread, char *endkey)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  long irow = index + 1;
  THIS->integerTrap(irow, ivar, nread, endkey);
  if(strings_equal("REDRAW", endkey))
            {
            THIS->readyToUpdate();              // virtual function
            }
}


void   SLMatrixViewBox::staticMessageTrap
                         (void *data, long /*ident*/, long index,
                          char  *cvar, long nread, char *endkey)
{
  SLMatrixViewBox *THIS = (SLMatrixViewBox*)data;
  long irow = index + 1;
  THIS->messageTrap(irow, cvar, nread, endkey);
  if(strings_equal("REDRAW", endkey))
            {
            THIS->readyToUpdate();              // virtual function
            }
}



//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//
//---------------------- make helper ------------------------//

         // protected.
         // additional datafields can be added above these tables
         //   by overriding this function, and calling this function
         //   from the overriding function after adding the desired
         //   datafields.
         // additional columns can be added to the right of these tables
         //   by overriding this function, and calling this function
         //   from the overriding function before adding the desired
         //   columns.

void SLMatrixViewBox::makeHelper()
{
  static char prompt[] = "12345678901234567890abc";
  static long junk = 0;
  prompt[_NCHAR] = '\0';

         //        N                   NMAX          ROW COL  NCHAR    MAXROWS
  dbox_rega(staticNumRowsUpdate, staticNumRowsUpdate, 0,  0,
                                                   (int)_WIDTH1, (int)_NROWMAX);

  for(int ident = 1; ident <= _NCOLMAX; ident++)
      {
             //  ID     PROMPT SWITCH SWITCH COL       NCHAR        NDEC
      dbox_drega(ident, prompt, &junk, &junk, 0, (int)_NCHAR, (int)_NDEC);

      dbox_set_ctrap(-ident, staticPromptTrap);
      dbox_set_dtrap( ident, staticMatrixTrap);
      dbox_set_cfun (-ident, staticPromptUpdate);
      dbox_set_dfun ( ident, staticMatrixUpdate);
      dbox_set_sfun (-ident, staticPromptSwitchUpdate);
      dbox_set_sfun ( ident, staticMatrixSwitchUpdate);
      }
  if(_WIDTH2 > 0)
      {
      int ident = (int)_NCOLMAX + 1;
             //   ID     PROMPT   SWITCH   SWITCH  COL       NCHAR   NDEC
      dbox_irega(ident,  " "   ,   &junk,  &junk,   0, (int)_WIDTH2, 0);

      dbox_set_itrap( ident, staticIntegerTrap);
      dbox_set_ifun ( ident, staticIntegerUpdate);
      dbox_set_sfun ( ident, staticIntegerSwitchUpdate);
      }
  if(_WIDTH3 > 0)
      {
      int ident = (int)_NCOLMAX + 2;
             //   ID     PROMPT   SWITCH   SWITCH  COL       NCHAR   NDEC
      dbox_crega(ident,  " "   ,   &junk,  &junk,   0, (int)_WIDTH3, 0);

      dbox_set_ctrap( ident, staticMessageTrap);
      dbox_set_cfun ( ident, staticMessageUpdate);
      dbox_set_sfun ( ident, staticMessageSwitchUpdate);
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
