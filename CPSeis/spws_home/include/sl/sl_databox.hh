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

//------------------------- sl_databox.hh ----------------------------//
//------------------------- sl_databox.hh ----------------------------//
//------------------------- sl_databox.hh ----------------------------//

//              header file for the SLDatabox class
//                 derived from the SLDelay class
//                      subdirectory sl

//  This SLDatabox is to contain drawn table and scalar data fields.
//  Currently this is a wrapper around the windowbox utility.

//  The simplest (and recommended) way to use this databox is this:
//   (1) Call the constructor from the derived class using
//         user_data = NULL and traptype = 4 (not the default 3).
//   (2) Override the virtual makeHelper() function.
//   (3) Call registration functions from the makeHelper() function.
//   (4) Write traps and update functions and put them in the
//         implementation file of the derived databox.  These traps
//         and update functions can be either static member functions
//         or static C-style non-member functions.

#ifndef _SL_DATABOX_HH_
#define _SL_DATABOX_HH_

#include "sl/sl_delay.hh"
#include "sl/datafield_list.hh"
#include <Xm/Form.h>

typedef void DataboxMakeTrap (void *data);

class SLDatabox : public SLDelay
{

//------------------------- typedefs for traps --------------------------//
//------------------------- typedefs for traps --------------------------//
//------------------------- typedefs for traps --------------------------//

 // The following traps are called when a new value has been entered.
 //
 // data   = pointer to this databox (must be cast to derived class).
 // ident  = ident of datafield (positive for variables, negative for labels).
 // index  = index of array (>=0) (always 0 for labels and scalars).
 // ivar   = new value for integer variable.
 // fvar   = new value for floating point variable.
 // dvar   = new value for double precision variable.
 // cvar   = new value for character variable or label.
 // nread  = number of characters input (zero if value not changed).
 // endkey = string indicating which key was pressed to terminate input.
 //
 // A few typical values of endkey (there are many more):
 //   "RETURN"  = return key has been pressed or button1 has been released
 //                 (important only for taking action when a pushbutton
 //                 has been pressed).
 //   "INSERT"  = user wants to insert a row at this index.
 //   "REMOVE"  = user wants to remove a row at this index.
 //                 (these are important only for arrays)
 //   "ARRIVED" = focus has moved to this datafield (important only
 //                 if you want to take special action at this point).
 //   "REDRAW"  = we are about to begin calling update functions for
 //                 this databox (important only if special stuff must
 //                 be pre-calculated before the updates are performed)
 //                 (usually not needed if update functions are called).
 //
 // Rules of thumb for traps:
 // (1) For pushbuttons, take action only when endkey = "RETURN".
 // (2) For variables whose values can change, set the new value only
 //       when nread is greater than zero.
 // (3) For arrays, insert or remove a row when endkey is "INSERT" or "REMOVE".
 // (4) For arrays, append a new row if nread is greater than zero and
 //       index is equal to the current length of the array.
 // (5) For radio buttons or toggle buttons, change the associated variable
 //       value either when nread is greater than zero or when endkey
 //       = "RETURN" (your choice).

  typedef   void  DataboxItrap (void *data, long ident, long index,
                                   long   ivar, long nread, char *endkey);
  typedef   void  DataboxFtrap (void *data, long ident, long index,
                                   float  fvar, long nread, char *endkey);
  typedef   void  DataboxDtrap (void *data, long ident, long index,
                                   double dvar, long nread, char *endkey);
  typedef   void  DataboxCtrap (void *data, long ident, long index,
                                   char  *cvar, long nread, char *endkey);

//-------------------- typedefs for update functions ----------------------//
//-------------------- typedefs for update functions ----------------------//
//-------------------- typedefs for update functions ----------------------//

 // The following update functions are called when the value to be
 //   displayed is needed to update the datafield.  The value to be
 //   displayed should be returned.
 //
 // data   = pointer to this databox (must be cast to derived class).
 // ident  = ident of datafield (positive for variables, negative for labels).
 // index  = index of array (>=0) (always 0 for labels and scalars).
 //
 // Warnings:
 //  (1) Functions which update the switch values can be called when
 //        the index exceeds the length of the array.
 //  (2) The pointer returned by DataboxCupdate must point to a character
 //        string in static or allocated memory (not on the stack).

  typedef   long  DataboxIupdate (void *data, long ident, long index);
  typedef  float  DataboxFupdate (void *data, long ident, long index);
  typedef double  DataboxDupdate (void *data, long ident, long index);
  typedef   char *DataboxCupdate (void *data, long ident, long index);
  typedef   long  DataboxNupdate (void *data);

//----------------------------- data ----------------------------------//
//----------------------------- data ----------------------------------//
//----------------------------- data ----------------------------------//

protected:

  void            *_box;        // windowbox structure.
  void            *_user_data;  // user data available in wbox traps.
  long             _traptype;   // 3 for oldstyle, 4 for newstyle.
  int              _omit;       // whether to omit the bottom message lines.
  int              _nrows_init; // restrict initial number of rows if >= 0.
  DataboxMakeTrap *_make_trap;  // make old-style wbox calls in this.
  void            *_make_data;  // data passed to _make_trap.

private:

  DatafieldList   *_dflist;     // linked list of data field children.

//---------------------- constructor and destructor -----------------//
//---------------------- constructor and destructor -----------------//
//---------------------- constructor and destructor -----------------//

public:

  SLDatabox (SLDelay *slparent, char *name, void *user_data = NULL,
                                            long  traptype  = 3,
                                            int   omit      = 0,
                                            int nrows_init  = -1);
  SLDatabox (Widget    wparent, char *name, void *user_data = NULL,
                                            long  traptype  = 3,
                                            int   omit      = 0,
                                            int nrows_init  = -1);
  SLDatabox (Widget    w,                   void *user_data = NULL,
                                            long  traptype  = 3,
                                            int   omit      = 0,
                                            int nrows_init  = -1);

  virtual ~SLDatabox (void);
  virtual WidgetClass topClass() { return xmFormWidgetClass; }
  virtual Boolean isContainer () { return TRUE; }
  void setMakeTrap (DataboxMakeTrap *make_trap, void *make_data = NULL);

  void showMessage (const char *msg);   // show message at bottom of databox.
  void setFocus    (int ident, int index);
  void saveTable   (const char *filename = NULL)  const;

//-------------------- miscellaneous functions ------------------------//
//-------------------- miscellaneous functions ------------------------//
//-------------------- miscellaneous functions ------------------------//

public:

  void *getBox (void)  { return _box; }

  virtual Widget make (Widget p = NULL);       // overrides SLDelay

  void  setUserData (void *data) { _user_data = data; }
  void *getUserData ()   const   { return _user_data; }

  static void *getUserData(void *box);  // to use in wbox traps

public:    // re-register pointers, etc.

  Boolean isVisible (long ident);
  void    nnewreg   (long ident, long   *point);
  void    inewreg   (long ident, long   *point);
  void    fnewreg   (long ident, float  *point);
  void    dnewreg   (long ident, double *point);
  void    cnewreg   (long ident, char   *point);

//-------------- method to override when subclassing --------------------//
//-------------- method to override when subclassing --------------------//
//-------------- method to override when subclassing --------------------//

// Adding wbox children:
//
// - Wbox children can be created only from within the registered
//   _make_trap() function, and/or from within the overriding 
//   makeHelper() method.
//
// - Each wbox child is added at the time it is created.

// If no registered _make_trap() function will be used, then the call
// to that function can be omitted from the overriding method.

protected:

  virtual void makeHelper()
      { makeChildren();
        if(_make_trap) _make_trap(_make_data); }

//----------- registration routines to be called from makeHelper ---------//
//----------- registration routines to be called from makeHelper ---------//
//----------- registration routines to be called from makeHelper ---------//

    // These registration routines are provided as a convenience, and
    // are recommended.  There are equivalent routines prototyped in
    // wbox.h which can be used instead, plus a lot of additional
    // routines which are retained for backward compatibility, or are
    // rarely needed.

    // ident = identification integer            (greater than zero).
    // irow  = row number                        (greater than zero).
    // icol  = column number                     (greater than zero).
    // nchar = number of characters in datafield (greater than zero).
    // ndec  = maximum number of decimal digits to display (>= zero).
    // if irow is  0, it will be the      row    after the previous one.
    // if irow is -1, it will be the same row       as the previous one.
    // if icol is  0, it will be the      column after the previous one.
    // if icol is -1, it will be the same column    as the previous one.
    // numrow = maximum number of rows to display at any one time
    //              (typically 10-35).
    // jsw = pointer to switch value for label    (commonly: 0 2 -2).
    // isw = pointer to switch value for variable (commonly: 0 1 3 4 5
    //                                                       -1 -3 -4 -5).
    // label = pointer to label (can be a hard-coded string).
    //   The number of characters in the label is the length of the
    //   string pointed to by the label argument at the time the
    //   registration routine is called.
    //
    // Permitted values of switches (jsw and isw):
    //   22   enterable editable dimmed text field (bright background).
    //   14   enterable non-editable radio button.
    //   13   enterable non-editable toggle button.
    //   12   enterable non-editable text field    (label look-alike).
    //   11   enterable non-editable text field.
    //    7   enterable editable text field        (gold background).
    //    6   enterable editable gold pushbutton   (gold background).
    //    5   enterable non-editable text field    (bright background).
    //    4   enterable editable radio button      (bright or gold).
    //    3   enterable editable toggle button     (bright or gold).
    //    2   enterable editable pushbutton        (bright background).
    //    1   enterable editable text field        (bright background).
    //    0   bypassed label or other message or information.
    //   -1   bypassed text field.
    //   -2   bypassed pushbutton.
    //   -3   bypassed toggle button.
    //   -4   bypassed radio button.
    //   -5   bypassed non-editable text field.
    //   -6   bypassed pushbutton.
    //   -7   bypassed text field.
    //  -11   bypassed non-editable text field.
    //  -12   bypassed non-editable text field  (label look-alike).
    //  -13   bypassed non-editable toggle button.
    //  -14   bypassed non-editable radio button.
    //  -22   bypassed dimmed text field.
    //  -25   histogram plot.
    //  -33   special  normal   message in a box (resource wboxmessagetext).
    //  -34   special important message in a box (resource wboxmessageloud).
    //  -44   special  normal   message          (resource wboxmessagetext).
    //  -45   special  normal   message right justified.
    //  -55   dimmed label.
    //  -56   centered label.
    //  -57   centered bypassed non-editable text field.
    //  -66   vertical line in middle of field width given by nchar.
    //  -77   blank field displayed.
    //  -99   nothing displayed at all (nothing is drawn).
    // Other switch values have undefined or reserved behavior and
    //   should not be used.
    // See file Wbox_help for actual foreground and background colors used.

public:    // create linked array set.
           // nupdate returns the current length of the array.
           // nmaxupdate returns the maximum allowed length of the array.
           // if the array should not be allowed to increase, nmaxupdate
           //   should return the current length.
           // if the array should always be allowed to increase, nmaxupdate
           //   should return the current length plus at least one.
           // to omit the column of indices on the left, set nchar to zero.
           // to override the wboxmaxrows resource, set numrow_init to the
           //   initial number of rows to display (effective only if this
           //   linked array set is at the bottom of the windowbox).

  void regArrays (DataboxNupdate *nupdate, DataboxNupdate *nmaxupdate,
                    int irow, int icol, int nchar, int numrow,
                    int numrow_init = 0);

public:    // create scalar datafields.
           // isw and msg cannot be NULL, even if an update function will
           //   be used (update function cannot be used for regMsg).

  void regMsg    (const char *msg, int irow = 0, int icol = 1);
  void regBlank                   (int irow = 0, int icol = 1);
  void regString (int ident, const char *msg, long *isw, int irow, int icol);

  void regIvar (int ident, long *isw, int irow, int icol, int nchar);
  void regFvar (int ident, long *isw, int irow, int icol, int nchar, int ndec);
  void regDvar (int ident, long *isw, int irow, int icol, int nchar, int ndec);
  void regCvar (int ident, long *isw, int irow, int icol, int nchar);

public:    // create double scalar datafields (label + scalar datafield).
           // use ivar3 for toggle and radio buttons (button precedes label).
           // isw, jsw, and label cannot be NULL, even if an update
           //   function will be used.
           // the length of label is strlen(label).

  void regIvar2 (int ident, const char *label, long *jsw,
                        long *isw, int irow, int icol, int nchar);
  void regFvar2 (int ident, const char *label, long *jsw,
                        long *isw, int irow, int icol, int nchar, int ndec);
  void regDvar2 (int ident, const char *label, long *jsw,
                        long *isw, int irow, int icol, int nchar, int ndec);
  void regCvar2 (int ident, const char *label, long *jsw,
                        long *isw, int irow, int icol, int nchar);
  void regIvar3 (int ident, const char *label, long *jsw,
                        long *isw, int irow, int icol, int nchar);

public:    // create datafield arrays (label + array datafield column).
           // isw and jsw cannot be NULL, even if an update function will
           //   be used.
           // if label is not NULL, its length is strlen(label).
           // if label is NULL, its length is nchar, and an update function
           //   should be used.

  void regIarray (int ident, const char *label, long *jsw,
                        long *isw,           int icol, int nchar);
  void regFarray (int ident, const char *label, long *jsw,
                        long *isw,           int icol, int nchar, int ndec);
  void regDarray (int ident, const char *label, long *jsw,
                        long *isw,           int icol, int nchar, int ndec);
  void regCarray (int ident, const char *label, long *jsw,
                        long *isw,           int icol, int nchar);

public:  // register traps and update functions.
         // use the negative of the ident for labels (trapCvar and updateCvar).
         // traps are not needed if switch value is always negative, or if
         //   user cannot change the value or activate a button and you
         //   do not care about the value of endkey.
         // update functions are not needed for switches if jsw or isw will
         //   never change.
         // update functions are not needed for labels if the string will
         //   never change.
         // if an update function is not provided for a switch, the switch
         //   value will be the current value pointed to by the jsw or
         //   isw pointer.
         // if an update function is not provided for a label, the label
         //   value will be the current value pointed to by the label
         //   pointer (or the hard-coded string if provided).

  void trapIvar     (int ident, DataboxItrap *trap);
  void trapFvar     (int ident, DataboxFtrap *trap);
  void trapDvar     (int ident, DataboxDtrap *trap);
  void trapCvar     (int ident, DataboxCtrap *trap);

  void updateIvar   (int ident, DataboxIupdate *update);
  void updateFvar   (int ident, DataboxFupdate *update);
  void updateDvar   (int ident, DataboxDupdate *update);
  void updateCvar   (int ident, DataboxCupdate *update);
  void updateSwitch (int ident, DataboxIupdate *sw);     // switch isw or jsw.

         // the following are combination convenience routines.
         // one or more of the arguments can be NULL.
         // the first  argument is the same as for trapIvar etcetera.
         // the second argument is the same as for updateIvar etcetera.
         // the third  argument is the same as for updateSwitch.

  void funIvar (int ident, DataboxItrap   *trap,
                           DataboxIupdate *update = 0,
                           DataboxIupdate *sw     = 0);

  void funFvar (int ident, DataboxFtrap   *trap,
                           DataboxFupdate *update = 0,
                           DataboxIupdate *sw     = 0);

  void funDvar (int ident, DataboxDtrap   *trap,
                           DataboxDupdate *update = 0,
                           DataboxIupdate *sw     = 0);

  void funCvar (int ident, DataboxCtrap   *trap,
                           DataboxCupdate *update = 0,
                           DataboxIupdate *sw     = 0);

public:  // create text for histogram and associated header.
         // to be used with switch value -25.
         // convenience functions which do not use any member variables.
         // the returned value of histogramHeader should be returned
         //   by the user's DataboxCupdate function for a label at the
         //   top of an array column.
         // the returned value of histogramBar (or histogramBar2) should
         //   be returned by the user's DataboxCupdate function for an
         //   array element.
         // the arguments for histogramHeader are saved for use by
         //   histogramBar and histogramBar2.  this works because the
         //   update function for a label is always called before the
         //   update function for the associated array.

  static char *histogramHeader (float vmin, float vmax, int nchar);
  static char *histogramBar    (float value);
  static char *histogramBar2   (float value1, float value2);

//--------------------------- end functions --------------------------//
//--------------------------- end functions --------------------------//
//--------------------------- end functions --------------------------//

} ;

#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
