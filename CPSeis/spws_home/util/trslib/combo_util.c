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
/*
C      combo_util.c
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
c        1         2         3         4         5         6         7  |
c23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C              written in c -- designed to be called from c
C                               (and some from fortran)
C
C     Utility Name:  combo_util       (combination utility routines)
C          Written:  93/03/31  by:  Tom Stoeckley
C     Last revised:  93/08/20  by:  Tom Stoeckley
C
C  Purpose:       Routines which call functions in wboxlib.a and
C                 in trslib.a (i.e. routines which link the
C                 simultaneous use of windowbox routines and
C                 make_widgets routines).
C
C  Rationale:     It is not desireable for windowbox routines and
C                 make_widget routines to call each other, since
C                 in many cases only one or the other set is being
C                 used in the same application, and we do not want
C                 to link to both sets of routines unless we are
C                 using both sets.
C
C  Related Documentation:  wbox   make_widgets
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C
C  machine:                ultrix
C  source code directory:  ~spws/util/trslib
C  library:                trslib.a
C  source files:           combo_util.c    trslib.h 
C----------------------------------------------------------------------
C             DIFFERENCES BETWEEN CODE ON DIFFERENT MACHINES
C                                n/a
C-----------------------------------------------------------------------
C                ROUTINES ON SOURCE FILE combo_util.c
C
C            Documented routines are listed later.
C            Undocumented static routines are listed here.
C
C              update_from_wbox     update_from_make
C-----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C     (C, X, Xt, and Motif libraries and header files not included)
C
C  Libraries:     trslib.a   wboxlib.a   wproc.a    cpsprim.a
C  Header files:  trslib.h   wbox.h     wproc.h 
C-----------------------------------------------------------------------
C                 EXTERNALS REFERENCED BY THIS UTILITY
C          (C, X, Xt, and Motif library routines not included)
C
C            wbox_additional_update            wbox_update
C            add_update_function               update_widgets
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2. 93/08/20  Stoeckley  Change to use the trslib.h header file.
C  1. 93/03/31  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  To set up coordinated updating and register a user function:
C  To force updates:
C
C            void register_update_function(fun, data)
C            void force_updates(void)
C
C  void (*fun)(void *data) = user-written function to call.
C  void *data              = pointer to user data.
C
C  The first function sets up the update procedures in both the
C    windowbox routines and the make_widget routines so that both
C    sets of user interface data fields will be automatically updated 
C    whenever either set of routines does the updating.  The user-written
C    function will be called prior to the rest of the updating.  If you 
C    do not need a user-written function, you can use two NULL arguments.
C
C  The first function should normally be called only once, normally
C    before entering the event loop.  If called an additional time,
C    the new function and data will replace the old function and data.
C
C  The second function can be called whenever the program wants to
C    force the updating.  The user-written function will be called
C    first, followed by the windowbox and make_widgets updates.
C
C  Either or both of data and fun can be NULL if desired.
C  The second function can also be called from Fortran.
C-----------------------------------------------------------------------
C                                NOTES
C
C 1.
C-----------------------------------------------------------------------
C\END DOC
*/



/*----------------------- header files ---------------------------------*/

#include <stdio.h>
#include <Xm/DialogS.h>
#include <X11/StringDefs.h>
#include "trslib.h"
#include "wproc.h"
#include "wbox.h"



/*---------- automatic update routines ---------------------------------*/

static void  (*update_fun)(void *update_data) = NULL;
static void   *update_data = NULL;
static Boolean update_start = True;

static void update_from_wbox(void *null)
{
  if(update_fun) update_fun(update_data);
  update_widgets();
}

static void update_from_make(void *null)
{
  if(update_fun) update_fun(update_data);
  wbox_update();
}

void register_update_function(void (*fun)(void *data), void *data)
{
  if(update_start)
       {
       wbox_additional_update(update_from_wbox, NULL);
       add_update_function   (update_from_make, NULL);
       update_start = False;
       }
  update_fun  = fun;
  update_data = data;
}

void force_updates(void)
{
  if(update_fun) update_fun(update_data);
  wbox_update();
  update_widgets();
}


/*--------------------------- end --------------------------------------*/
