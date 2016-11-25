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

//------------------------ sl_matrix_view_box.hh ---------------------//
//------------------------ sl_matrix_view_box.hh ---------------------//
//------------------------ sl_matrix_view_box.hh ---------------------//

//              header file for the SLMatrixViewBox class
//                derived from the SLDatabox class
//                         subdirectory sl

     // Displays a matrix of double-precision values of any kind
     // in a windowbox.
     // See the implementation file for additional documentation.

 
#ifndef _SL_MATRIX_VIEW_BOX_HH_
#define _SL_MATRIX_VIEW_BOX_HH_

#include "sl/sl_databox.hh"


class SLMatrixViewBox  :  public SLDatabox
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  const long _NCHAR;     // number of displayed characters.
  const long _NDEC;      // number of displayed decimals.
  const long _NROWMAX;   // maximum number of displayed rows.
  const long _NCOLMAX;   // maximum number of displayed columns.
  const long _WIDTH1;    // width of index   field at left  of each row.
  const long _WIDTH2;    // width of integer field at right of each row.
  const long _WIDTH3;    // width of message field at right of each row.
  long       _first;     // first displayed column.
  long       _numvis;    // number of displayed columns.

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor

  SLMatrixViewBox (SLDelay *slparent, char *name, HelpCtx hctx = NULL,
               long nchar = 10, long ndec = 10,
               long nrowmax = 30, long ncolmax = 6,
               long width1 = 5, long width2 = 0, long width3 = 0);
  SLMatrixViewBox (Widget    wparent, char *name, HelpCtx hctx = NULL,
               long nchar = 10, long ndec = 10,
               long nrowmax = 30, long ncolmax = 6,
               long width1 = 5, long width2 = 0, long width3 = 0);
  virtual ~SLMatrixViewBox();

protected:     //  virtual functions to override

  virtual void   newFirstVisibleColumn (long /*first*/) {}
  virtual void   newNumVisibleColumns  (long /*numvis*/) {}

  virtual long   numRowsUpdate      () = 0;
  virtual long   numColumnsUpdate   () = 0;
  virtual void   readyToUpdate      () {}

  virtual double matrixUpdate       (long /*irow*/, long /*icol*/) = 0;
  virtual char  *promptUpdate                      (long /*icol*/);
  virtual long   integerUpdate      (long /*irow*/); 
  virtual char  *messageUpdate      (long /*irow*/); 

  virtual long matrixSwitchUpdate (long /*irow*/, long /*icol*/) { return 5; }
  virtual long promptSwitchUpdate                (long /*icol*/) { return -44; }
  virtual long integerSwitchUpdate(long /*irow*/)                { return -44; }
  virtual long messageSwitchUpdate(long /*irow*/)                { return -44; }

  virtual void   matrixTrap (long /*irow*/, long /*icol*/, double /*dvar*/,
                                          long /*nread*/, char* /*endkey*/) {}
  virtual void   promptTrap                (long /*icol*/, char*  /*cvar*/,
                                          long /*nread*/, char* /*endkey*/) {}
  virtual void   integerTrap(long /*irow*/,                long   /*ivar*/,
                                          long /*nread*/, char* /*endkey*/) {}
  virtual void   messageTrap(long /*irow*/,                char*  /*cvar*/,
                                          long /*nread*/, char* /*endkey*/) {}

private:       // static member functions

  static long   staticNumRowsUpdate    (void *data);

  static double staticMatrixUpdate  (void *data, long ident, long index);
  static char  *staticPromptUpdate  (void *data, long ident, long index);
  static long   staticIntegerUpdate (void *data, long ident, long index);
  static char  *staticMessageUpdate (void *data, long ident, long index);

  static long   staticMatrixSwitchUpdate (void *data, long ident, long index);
  static long   staticPromptSwitchUpdate (void *data, long ident, long index);
  static long   staticIntegerSwitchUpdate(void *data, long ident, long index);
  static long   staticMessageSwitchUpdate(void *data, long ident, long index);

  static void   staticMatrixTrap  (void *data, long ident, long index,
                                   double dvar, long nread, char *endkey);
  static void   staticPromptTrap  (void *data, long ident, long index,
                                   char  *cvar, long nread, char *endkey);
  static void   staticIntegerTrap (void *data, long ident, long index,
                                   long   ivar, long nread, char *endkey);
  static void   staticMessageTrap (void *data, long ident, long index,
                                   char  *cvar, long nread, char *endkey);


public:        // methods to get values

  long    getFirstVisibleColumn()  const  { return _first; }
  long    getNumVisibleColumns ()  const  { return _numvis; }

public:        // methods to set values
               // int is returned TRUE if change has been made.

  int     setFirstVisibleColumn   (long first);
  int     resetFirstVisibleColumn ();
  void    makeColumnVisible       (long icol);   // scrolls if necessary.
  void    makeRowVisible          (long irow);   // scrolls if necessary.
  void    showMessage             (char *msg);   // in message box.
  void    maybeShowMessage        (char *msg);   // in message box.

private:       // miscellaneous functions.
               // int is returned TRUE if change has been made.

  int     setFirstVisibleColumn (long first, int internal);
  void    findNumVisibleColumns ();
  void    constructorHelper     ();
  void    trapHelper            (long ident, char *endkey);

protected:

  void    makeHelper            ();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
