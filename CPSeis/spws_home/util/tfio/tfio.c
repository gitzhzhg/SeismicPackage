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
C***************************** COPYRIGHT NOTICE *************************
C*                                                                      *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION             *
C*                              OF CONOCO INC.                          *
C*                      PROTECTED BY THE COPYRIGHT LAW                  *
C*                          AS AN UNPUBLISHED WORK                      *
C*                                                                      *
C***************************** COPYRIGHT NOTICE *************************
C        1         2         3         4         5         6         7  |
C23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                       CONOCO PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C
C  Primitive name:  TFIO
C         Library:  CONLIB
C          Author:  Richard Day
C    Date Written:  91/03/01
C    Last revised:  2000/02/21   Day
C
C  Purpose:         (Trace file I/O routines ).
C
C_______________________________________________________________________
C                                NOTES
C    LIST OF TFIO  FUNCTIONS IN ALPHABETICAL ORDER
C
C  tf_arr_rd       -Read pattern of traces form trace file
C  tf_arr_wr       -Write traces to trace file
C  tf_bmov         -???
C  tf_check_cntrl  -???
C  tf_Chedk_Glbl   -???
C  tf_close        -Close a trace file if it is open
C  tf_close_by_name-Close a named file if it is open
C  tf_glbl_add     -Save a trace file data base entry
C  tf_glbl_add1    _Save a trace file data base entry
C  tf_glbl_del     -Delete entry in trace file data base
C  tf_glbl_get     -Return entry in trace file data base
C  tf_glbl_name_to_fd Convert file name to its file descriptor
C  tf_set_trmaxg   -Set max trace amplitude for file in the data base
C  tf_get_glblpntr -returns pointer to a data base entry.
C  tf_glbl_get1    -Return entry in trace file data base
C  tf_glbl_getg    -Retrieve a global variable
C  tf_glbl_getn    -Return entry in trace file data base
C  tf_glbl-putg    -Put global variable in memory
C  tf_glbl_rd      -Read global from trace file
C  tf_glbl_wr      -Write global record to trace file
C  tf_glbls_rd     -Read global from trace file
C  tf_glbls_wr     -Write global to trace file
C  tf_hc_rd        -Read history cards from trace file
C  tf_hc_wr        -Write history cards to trace file
C  tf_open         -Open a trace file
C  tf_print_glbls  -???
C  tf_tr_rd        -Read traces sequentially from trace file
C  tf_tr_wr        -Write traces sequetially to trace file
C  tf_set_gdefs    -initialize decode stuff
C_______________________________________________________________________
C Revision History:
C19. Sherrill 00/10/26  Added trcio calls to create and write trcio files.
C18. Day  2000/02/21    Modified tf_tr_rd and tf_arr_rd so that the
C                       reyturn trace count is set to zero for io errors.
C17. Day    99/11/17    Fixed logic error when opening new file
C                       when an old version existed. tf_xopen_
C                       when op_stat=-4.
C16. Day    99/09/21    Logic fixed in tf_xopen for CBYT files
C15. Day    99/08/23    Fixed buffer allocation problem in tf_xopen that
C                       occurred for old file opens in some situations
C14. Day    99/07/24    Removed lseek,read,write calls for history cards
C                       Removed restrictions on grecsiz global.
C                       Added an argument(grecsiz) to the 
C                       tfio_xopenf_ function.
C13. Day    99/06/18    Deleted extern statements and added static
C                       declaration to some global variables. Cures a
C                       problem with the Portland Group xompilers.
C12. Day    99/04/29    Added fortran acces call tfio_xopenf_.
C                       Replaced exist call with dskio_exist_.
C                       Replaced dskio_xop_ with dskio_xopc_.
C                       Now a failed open returns lun as -1.
C11. Day    99/03/08    All offsets changed from int to longs.
C                       Converted to dskio_x* routines for all
C                       IO operations. Added tf_xopen call to allow
C                       explicit specification of IO type.
C10. Day    99/02/03    Updating in Conlib.
C 9. Day    98/08/06    Corrected logic in tf_arr_rd for do-skip
C                       when ntot != integer multiple of ndo.
C 8. Day    98/06/30    Modified tf_tr_rd,tf_tr_wr,tf_arr_rd
C                       tf_arr_wr functions. Replaced dskiocrd
C                       & dskiocwr with dskiord & dskiowr.
C                       Added tf_glbl_name_to_fd, and
C                       tf_close_by_name
C 7. Day    97/12/02    Added new open option. minor fixes.
C 6. Day    97/01/16    Update for compatibility with SPWS
C 5. Day    96/02/14    Revised history card read and write
C 4. Day    95/01/19    Made glbl static array
C 3. Day    94/04/08    Added two functions to this file.
C 2. Day    93/11/08    Corrected bug in tf_hc_wr_(). Eliminated MAXSCR.
C 1. Day    93/09/15    Dynamical allocation of IO buffer "buff".
C_______________________________________________________________________
C  Documentation for the above listed functions
C
C Name   : tf_arr_rd_
C Purpose: Read a regular pattern of traces from a CPS trace file.
C
C Function Definition:        ( Language = C )
C   int  tf_arr_rd_(int *lun,IO_request *Cl, char hd[],char arr[], char
C  *msg)
C  *lun   input     io channel number of an open trace file.
C  *Cl    in&out    Controls which traces and samples will be read.
C                   See tfio.h for structure definition.
C  hd[]   output    array to receive header data.
C  arr[]  output    array to receive trace data.
C  *msg   output    msg='OK' if no errors occur.
C
C NOTES:
C  1. lav's not returned anymore
C  2. 1st and last traces of the m'th group are:
C     n1 = iskp+1 + (m-1)*( (abs(ndo)-1)*dir + nskp + 1)
C     n2 = n1 + (abs(ndo)-1)*dir
C     where 1 <= m <= (ntot-1)/ndo+1
C  3. Keeps reading until either:
C     - number of traces read in = ntot.
C     - starting trace,n1<1 or n1>ntrfil (outside of file limits)
C     - n2 was outside file limits on previous pass.
C-----------------------------------------------------------------------
C Name   : tf_arr_wr_
C Purpose: Write header and trace data to an output trace file.
C
C Function Definition:        ( Language = C )
C   int  tf_arr_wr_( int  *lun,struct Cntrl *Cl, char hd[],char arr[],char
C  *msg)
C  *lun   input     io channel number of an open trace file.
C  *Cl    in&out    Controls which traces and samples will be written.
C                   See tfio.h for structure definition.
C  hd[]   output    array containing header data.
C  arr[]  output    array containing trace data.
C  *msg   output    msg='OK' if no errors occur.
C
C NOTES:
C  1. Writes are constrained more than the tf_arr_rd calls.
C     CL->ndo,Cl->ntot, and Cl->nskp must be greater than zero.
C  2. Data in hd, and arr are packed head to tail. The trace
C     size = ndptr*nbydp bytes.
C-----------------------------------------------------------------------
C Name   : tf_close_
C Purpose: Close a trace file.
C
C Function Definition:        ( Language = C )
C   int  tf_close_( int  *lun,  int  *cl_stat, char *msg )
C  *lun   input     io channel number of an open trace file.
C  *cl_stat in      Set to 0 to keep file, otherwise delete.
C  *msg   output    msg='OK' if no errors occur.(pass by reference)
C
C NOTES:
C  1. When a file is closed: retrieve globals for lun from the memory
C     resident data base, update the global record on disk(Only if
C     ftyp="TFILE"), close file, then eliminate the lun entry in the
C     trace file globals data base.
C  2. msg="OK" if no errors occur.
C
C-----------------------------------------------------------------------
C Name   : tf_glbl_add_, and tf_glbl_add1_
C Purpose: Saves or updates a Trace File data base entry.
C
C Function Definition:        ( Language = C )
C   int  tf_glbl_add_(  int  *istat,  int  *lun, TF_Global *gtmp )
C   int  tf_glbl_add1_( char name[], int  *num,  char val[],
C        int  *istat,  int  *lun)
C  *istat    output     0 if no errors occur.
C  *lun      input      active io channel number.
C  *gtmp     input      structure to be added or updated.
C
C NOTES:
C  1. *lun is the key to the data base entrys. The trace file
C      data base resides in memory.
C  2.  Will add or update an existing entry. Maximum of 10 entrys .
C      After 10 the function just returns.
C  3.  GLBL is the input trace file global. If calling from fortran
C      GLBL is first element of the global common block.
C  4.  See tfio.h for definition of GLBL.
C
C-----------------------------------------------------------------------
C Name   : tf_glbl_del_
C Purpose: Deletes an entry in the trace file data base.
C
C Function Definition:        ( Language = C )
C   int  tf_glbl_del_(  int  *istat,  int  *lun )
C  *istat    output     0 if no errors occur.
C  *lun      input      active io channel number.
C
C NOTES:
C  1. *lun is the key to the data base entrys.
C  2.  Will delete an existing entry. Maximum of 10 entrys allowed.
C-----------------------------------------------------------------------
C Name   : tf_glbl_get_, tf_glbl_getn_, tf_glbl_get1
C Purpose: Returns an entry(s) in the trace file data base. Match by
C          lun or file name
C
C Function Definition:        ( Language = C )
C   int  tf_glbl_get_(  int  *istat,  int  *lun, TF_Global *gtmp )
C   int  tf_glbl_getn_(  int  *istat, char *namen, TF_Global *gtmp )
C   int  tf_glbl_get1_( char name[], int  *num,  char val[],
C        int  *istat,  int  *lun)
C  *istat    output     0 if no errors occur.
C  *lun      input      active io channel number.
C  *gtmp     output     structure to be added or updated.
C  namen     input      Search in the data base for this file name.
C
C NOTES:
C  1. *lun is the key to the data base entrys.
C  2.  Will return an existing entry. Maximum of 10 entrys allowed.
C      After 10 the function just returns.
C  3.  GLBL is the input trace file global. If calling from fortran
C      GLBL is first element of the global common block.
C  4.  See tfio.h for definition of GLBL.
C-----------------------------------------------------------------------
C Name   : tf_glbl_getg_
C Purpose: Retrieve a named global variable from a global structure.
C
C Function Definition:        ( Language = C )
C   int  tf_glbl_getg_(char name[], int  *num,  char val[],  int  *j,
C       char mem[])
C  name[]     in     Name of the global variable to retrive.
C  num        in&out Number of values to transfer. Will be clipped to
C                    the size of the corresponding structure member.
C  val        in     Will receive the int, float, or string.
C  j          out    Number of the structure member obtained. Will be
C                    less than zero for failure.
C  mem        out    Base memory address of a Trace File global
C                    structure.
C
C NOTES:
C  1. Data is placed in val if a match is found between
C     name and an internal list of global-structure member names.
C  2. tf_glbl_getg lets users retrieve a global structure 1 piece at
C     a time.
C  3. Most structure members are a single float, or int so num=1 in
C     these cases. When the structure member is an array, one can set
C     num=sizeof(array) to transfer the whole array in one call.
C     (see tfio.h for definition of the global structure)
C-----------------------------------------------------------------------
C Name   : tf_glbl_putg_
C Purpose: Put global variable in correct relative location in memory
C
C Function Definition:        ( Language = C )
C   int  tf_glbl_putg_(char name[], int  *num,  char val[],  int  *j,
C       char mem[])
C  name[]     in     Name of the variable in the global structure.
C  num        in&out Number of values to transfer. Will be clipped to
C                    the size of the corresponding structure member.
C  val        in     Contains the  int , float, or string to store.
C  *j         out    Number of the structure member replaced. Will be
C                    less than zero for failure.
C  mem        out    Starting memory address for a Trace File global
C                    structure.
C
C NOTES:
C  1. The data contained in val will be placed at the correct relative
C     offset from the base address mem, if a match is found between
C     name and a permanent list of global structure member names.
C  2. tf_glbl_putg lets users build a global structure 1 piece at
C     a time. tf_glbl_putg is used before calling tf_open_.
C  3. Most structure members are a single float, or int so num=1 in
C     these cases. When the structure member is an array, one can set
C     num=sizeof(array) to transfer the whole array in one call.
C     (see tfio.h for definition of the global structure)
C
C-----------------------------------------------------------------------
C Name   : tf_glbl_rd_
C Purpose: Read a global record from a CPS Trace File.
C
C Function Definition:        ( Language = C )
C   int  tf_glbl_rd_(  int  *lun, TF_Global *gtmp, char *msg )
C  *lun   input     io channel number of an open trace file.
C  GLBL   output    C-structure or Fortran common to receive the
C                   global record.
C  *msg   output    msg='OK' if no errors occur.
C
C NOTES:
C  1. The global record size is read from the file and returned
C     in gtmp->grecsiz . A size of 0 is returned if the file does
C     not conform to the trace file definition.
C  2. See  the file tfio.h for the exact definition of the global
C     record structure.
C-----------------------------------------------------------------------
C Name   : tf_glbl_wr_
C Purpose: Convert global structure to ascii and write as a global
C          record to a CPS Trace File.
C
C Function Definition:        ( Language = C )
C   int  tf_glbl_wr_(  int  *lun, TF_Global *gtmp, char *msg )
C  *lun   input     io channel number of an open trace file.
C  GLBL   output    C-structure or Fortran common containing the
C                   global record.
C  *msg   output    msg='OK' if no errors occur.
C
C NOTES:
C  1. The *gtmp argument is first encoded as an ascii decode string.
C     then it is written to the file pointed tyo by lun
C     ( Trace file marker="*global" is prepended to the string)
C  2. Number of bytes wrtitten will be lesser of gtmp->grecsize
C     or GRECSIZ(from tfio.h) or the string length. A null will
C     be placed at the end of the string
C-----------------------------------------------------------------------
C Name   : tf_glbls_rd_
C Purpose: Read a global record from a CPS Trace File.
C
C Function Definition:        ( Language = C )
C   int  tf_glbls_rd_(  int  *lun, char str[], char *msg )
C  *lun   input     io channel number of an open Trace File.
C   str   output    Char. variable to receive the Trace File global
C                   record which is ascii data. Is left untouched
C                   if file is determined to be a non Trace File.
C  *msg   output    msg='OK' if no errors occur.
C
C NOTES:
C  1. The global record size is read from the file and returned
C     in str. The size of str must be GRECSIZ characters which is
C     defined in tfio.h . Global records  int er than GRECSIZ bytes
C     will require GRECSIZ to be increased.
C  2. The global record is assumed to start on a 512 byte boundary
C     given by GRECPOS/RECSIZE + 1 . ( see tfio.h )
C  3. returns an error status. no errors returns 0.
C-----------------------------------------------------------------------
C Name   : tf_glbls_wr_
C Purpose: Write an ascii global record to a CPS Trace File.
C
C Function Definition:        ( Language = C )
C   int  tf_glbls_wr_(  int  *lun,  int  *len, char str[], char *msg )
C  *lun   input     io channel number of an open trace file.
C  *len   in&out    Number of bytes from str to transfer.
C                   Must be less than the file global record size
C   str   input     Character variable to write to the Trace File
C                   as a global record.str must be null terminated.
C  *msg   output    msg='OK' if no errors occur.
C
C*NOTES:
C  1. See  the file tfio.h for the exact definition of the global
C     record structure.
C  2. The number of bytes taken from str will be the lesser of len
C     or the string length of str.
C     ( Trace file marker="*global" is prepended to the string str)
C  3. returns the number of bytes written to the global record area.
C-----------------------------------------------------------------------
C Name   : tf_hc_rd_
C  urpose: Read history card images from a Trace File.
C
C Function Definition:        ( Language = C )
C   int  tf_hc_rd_( int  *lun, int  *ns, int  *num,char *hc,char *msg);
C  *lun   input     io channel number of an open trace file.
C  *ns    in&out    Initial card number to read.
C                   *ns > 0
C  *num   in&out    Number of sequential card images to read.
C                   *num > 0
C  hc     input     Array containing *num card images where a card
C                   image contains 80 bytes.
C  *msg   output    msg='OK' if no errors occur.
C
C NOTES:
C  1.
C-----------------------------------------------------------------------
C Name   : tf_hc_wr_
C Purpose: Write history card images to a Trace File.
C
C Function Definition:        ( Language = C )
C   int  tf_hc_wr_( int  *lun, int  *ns, int  *num,char *hc,char *msg);
C  *lun   input     io channel number of an open trace file.
C  *ns    in&out    Initial card number to write.
C                   *ns > 0
C  *num   in&out    Number of sequential card images to write.
C                   *num > 0
C  hc     input     Array containing *num card images where a card
C                   image contains BYPERHC bytes(see tfio.h).
C  *msg   output    msg='OK' if no errors occur.
C
C NOTES:
C  1.
C-----------------------------------------------------------------------
C Name   : tf_open_
C Purpose: Open a trace file.
C
C Function Definition:        ( Language = C )
C  int  tf_open_(int  *lun, char *namen,TF_Global *glbu,
C                     int  *op_stat, char *msg )
C  *lun   output    io channel number that was opened.
C                   Is set to 0 if the open fails.
C  namen  input     Name of the file to open
C  glbu   in&out    Pointer to a C-structure or the 1st element of
C                   a fortran common. Returned for old or update files.
C                   User must provide *glbu for new files.
C  op_statinput     =0 for read only.
C                   =1 for write only.
C                   =2 for read and write. Update!
C                   =3 for new file with write only.
C                   =4 for new file with read write.
C                   =-4 for update file with read write.
C  *msg   output    msg='OK' if no errors occur.(pass by reference)
C                   needs to be null terminated!!
C
C NOTES:
C  1. Will check for existence of file, open the file, and update
C     the memory resident global record data base.
C  2. See tfio.h for definition of GLBL.
C  3. See tf_glbl_putg, and tf_glbl_getg for an easy way to construct
C     an area in memory that is of type TF_Global .
C-----------------------------------------------------------------------
C Author : R. Day
C Date   : 3/1/91
C
C Function Definition:        ( Language = C )
C   int  tf_tr_rd_( int  *lun, int  *ns, int  *num,  int  *nsamp,  int  *samp1,
C       int  *sdec,  int  *trnsps, char hd[],char tr[],char *msg);
C  *lun   input     io channel number of an open trace file.
C  *ns    in&out    Initial trace number to read.
C                   0 < *ns <= ntrfil
C  *num   in&out    Number of sequential traces to read.
C                   *num can be negative.
C  *nsamp in        No. of samples to return
C  *samp1 in        1st sample to return
C  *sdec  in        Sample decimation factor
C  *trnspsin        Transpose tr if *trnsps!=0
C  hd[]   output    Header array to receive data.
C  tr[]   output    Trace array to receive data.
C  *msg   output    msg='OK' if no errors occur.
C
C NOTES:
C  1. *num will be reset if the call trys to read beyond the data.
C--------------------------------------------------------------------
C Name   : tf_tr_wr_
C Purpose: Write headers and traces in sequential order to a trace
C          file. May begin at an arbitrary point in the file.
C
C Function Definition:        ( Language = C )
C   int  tf_tr_wr_( int  *lun, int  *ns, int  *num,char hd[],char tr[],char
C  *msg);
C  *lun   input     io channel number of an open trace file.
C  *ns    in&out    Initial trace number to write.
C                   0 < *ns <= Traces in file
C  *num   in&out    Number of sequential traces to write.
C                   *num > 0
C  hd[]   input     Header array containing data.
C  tr[]   input     Trace array containing data.
C  *msg   output    msg='OK' if no errors occur.
C
C NOTES:
C  1.
C--------------------------------------------------------------------
balls
C--------------------------------------------------------------------
C                       REVISION HISTORY
C     Date     Author   Description
C     ----     ------   -----------
C 14. 93/08/09 Day      tf_tr_rd, tr_arr_rd - Increased MAXNUM
C 13. 92/07/15 Day      tf_tr_rd  - Added some features from tf_arr_rd
C 12. 92/07/15 Day      tr_arr_rd - Removed computation of amplitude
C                                   factors
C 11. 91/10/23 Day      tf_open   - Problem with path name corrected
C 10. 91/10/22 Day      tf_open   - Store op_stat in Glbl structure
C                                   (for close)
C 9.  91/10/22 Day      tf_close - check opstat member of Glbl
C                                  structure
C 8.  91/10/05 Day      tf_tr_wr  - Changed loginc of overlap reading.
C 7.  91/09/01 Day      tr-arr-rd - MAXINUM clip. More careful
C                                   checking for file limits.
C 6.  91/05/10 Day      tf_open   - Enabled op_stat=4
C 5.  91/05/08 Day      tf_glbl_wr -gtmp->lun set = to *lun
C                                   before encode & write.
C 4.  91/05/08 Day      tf_glbl_rd -Ignore gtmp->lun from file.
C                                   Replace by *lun
C 3.  91/04/26 Day      Added tf_glbls_rd   function  and
C                       tf_glbls_wr    function
C 2.  91/04/25 Day      Added tf_glbl_putg  function  and
C                       tf-glbl-putg  function
C 1.  91/03/01 Day      Initial Version
C-----------------------------------------------------------------------
C   SUBROUTINE, FUNCTION, ENTRY, AND COMMON BLOCK NAMES IN THIS MODULE
C
C  Subroutines:     None
C  Functions:       tf_arr-rd      tf_arr_wr      tf_bmov
C                   tf_check_cntrl tf_Check_Glbl  tf_close
C                   tf_glbl_add    tf_glbl_add1   tf_glbl_del
C                   tf-glbl_get    tf_glbl_get1   tf_glbl_getg
C                   tf_glbl_getn   tf_glbl_putg   tf_glbl_rd
C                   tf_glbl_wr     tf_glbls_rd    tf_glbls_wr
C                   tf_hc_rd       tf_hc_wr       tf_open
C                   tf_print_glbls tf_tr_rd       tf_tr_wr
C                   tf_set_gdefs
C  Entry points:
C  Common blocks:
C_______________________________________________________________________
C                 EXTERNALS REFERENCED BY THIS MODULE
C
C                              none
C-----------------------------------------------------------------------
C                       MEMORY REQUIREMENTS
C
C  Storage         -
C  Heap (dynamic)  -
C-----------------------------------------------------------------------
C\END DOC
*/
#include <string.h>
#include <stdlib.h>

#include "tf_global.h"
#include "tfdefs.h"
#include "jsfiles.h"

char *tf_set_bsiz( int  newsiz);

static int nglbl;
static TF_Global glbl[MAXENT];
static char *buff;           /* io-buffer */
static  int  BSIZE;      /* current IO-buffer size */
static  long  *trace_byt;
static  int  ntrbyt;
static  float *lav = NULL;
static  int  length_of_lav = 0;

/*------------------------------------------------------------------
 *Name   : tf_glbl_add_, and tf_glbl_add1_
 *Purpose: Saves or updates a Trace File data base entry.
 *------------------------------------------------------------------*/
 int  tf_glbl_add_(  int  *istat,  int  *lun, TF_Global *gtmp )
{
  TF_Global *gl;
  int         i, match;
  *istat= 0;
  match= -1;
  if(*lun<=0) { *istat=1; return *istat; }

  
  for (i=0;i<=nglbl-1;i++)
   { if( *lun==glbl[i].lun )
       { match=i;
         memcpy( &glbl[match],gtmp,sizeof(TF_Global));
         return *istat;
       }
   }
/*
 * No match found, attempt to add to end of list */
  if(nglbl<MAXENT)
      { memcpy( &glbl[nglbl],gtmp,sizeof(TF_Global));
        nglbl += 1;       /* add globals at end of list */
      }
     else
      {  *istat = 2;
         nglbl=MAXENT;
       }

  return *istat;
}
  int  tf_glbl_add1_( char name[], int  *num,  char val[],
       int  *ierr,  int  *lun)
 {
   int     match,  i, j;
  *ierr=0;
  j = -1;
  match = -1;
  if(*lun<=0) { *ierr=1; return *ierr; } /* use after open */

  for (i=0;i<=nglbl-1;i++)
    if( *lun==glbl[i].lun )
       { match=i; break; }
/*
 * If no match found, attempt to add to end of list */
  if(match<0 )
    { if(nglbl<MAXENT)
        { nglbl += 1;       /* add globals at end of list */
          match = nglbl-1;
        }
      else { *ierr=1; return 1; }
    }

/* j is the structure member replaced */
  tf_glbl_putg_( name,num,val,&j,(char *) &glbl[match] );
  if(j<0 || match<0) { *ierr=1; return 1; }

  return 0;
 }

 int  tf_set_trmaxg_( int  *lun, float *trmaxg)
{TF_Global *G;
 G =  tf_get_glblpntr_(lun );
 if(G==NULL) return 1;
 G->trmaxg = *trmaxg;
 return 0;
}

int tf_glbl_name_to_fd(char *name)
{int i;
 if(!name) return -1;
  if(nglbl<=0)  return -1;
  for (i=0;i<nglbl;i++) {
    if( strcmp(name,glbl[i].path )==0) {
      return glbl[i].lun;
    }
  }
 return -1;
}

TF_Global *tf_get_glblpntr_( int  *lun )
{  int         i,  match;
  if(*lun<=0)   return NULL;
  if(nglbl<=0)  return NULL;

  match = -1;
  for (i=0;i<=nglbl-1;i++)
   { if( *lun==glbl[i].lun )
       { match=i;
         return &glbl[match];
       }
   }

   if(match<0) return NULL;  /* Has not been saved yet? */
   return &glbl[match];
}

/*------------------------------------------------------------------
 *Name   : tf_glbl_get_, tf_glbl_getn_, tf_glbl_get1
 *Purpose: Returns an entry(s) in the trace file data base. Match by
 *         lun or file name
 *------------------------------------------------------------------*/
 int  tf_glbl_get_(  int  *istat,  int  *lun, TF_Global *gtmp )

{
   int         i,  match;
  *istat = 0;
  match = -1;
  if(*lun<=0) { *istat=1; return *istat; }
  if(nglbl<=0){ *istat=2; return *istat; }

  for (i=0;i<=nglbl-1;i++)
   { if( *lun==glbl[i].lun )
       { match=i;
         memcpy( gtmp,&glbl[match],sizeof(TF_Global));
         return *istat;
       }
   }

   if(match<0) *istat=3;  /* Has not been saved yet? */

 return *istat;
}

 int  tf_glbl_getn_(  int  *istat, char *namen, TF_Global *gtmp )
{/* check header and data file names for a match */
  int     i,  match;
  char    *header_file=0,*data_file=0;
  *istat = 0;
  match = -1;
  if(nglbl<=0){ *istat=2; return *istat; }

  for (i=0;i<=nglbl-1;i++)
    { header_file = tf_global_header_file(&glbl[i]);
      data_file   = tf_global_data_file(&glbl[i]);
     if( (strcmp(namen,data_file) == 0) || (strcmp(namen,header_file) == 0) )
       { match=i;
         memcpy( gtmp,&glbl[match],sizeof(TF_Global));
         return *istat;
       }
   }

   if(match<0) *istat=3;  /* Has not been saved yet? */

 return *istat;
}

int  tf_glbl_get1_( char name[], int  *num,  char val[],
       int  *ierr,  int  *lun)
 {
   int     match,  i, j;
  *ierr=0;
  j = -1;
  match = -1;
  if(*lun<=0) { *ierr=1; return *ierr; } /* use after open */

  for (i=0;i<=nglbl-1;i++)
    if( *lun==glbl[i].lun )
       { match=i; break; }
/*
 * If no match found, attempt to add to end of list */
  if(match<0 )
    { if(nglbl<MAXENT)
        { nglbl += 1;       /* add globals at end of list */
          match = nglbl-1;
        }
      else { *ierr=1; return 1; }
    }

/* j is the structure member replaced */
  tf_glbl_getg_( name,num,val,&j,(char *)&glbl[match] );
  if(j<0 || match<0) { *ierr=1; return 1; }

  return 0;
 }
/*------------------------------------------------------------------
 *Name   : tf_glbl_del_
 *Purpose: Deletes an entry in the trace file data base.
 *------------------------------------------------------------------*/
 int  tf_glbl_del_(  int  *istat,  int  *lun )
 {
   int         i,  match;
  *istat = 0;
  match = -1;
  if(nglbl<1) return *istat;

  for (i=0;i<=nglbl-1;i++)
    if( *lun==glbl[i].lun ) { match=i; break; }

  if(match<0)   /* Cant erase whats not there */
   { *istat=1;
     return *istat;
   }

  for (i=match;i<=nglbl-2;i++)
    memcpy( &glbl[i],&glbl[i+1],sizeof(TF_Global));

  nglbl=nglbl-1;
  return *istat;
 }

/*------------------------------------------------------------------
 *Name   : tf_glbl_putg_
 *Purpose: Put global variable in correct relative location in memory
 *------------------------------------------------------------------*/
  int  tf_glbl_putg_(char name[], int  *num,  char val[],  int  *j,
   char mem[])
 {


   int     sizl, sizf, i, n, lens;
  GlobalFmt *gfmt=0;
  char *jp, tmp[120];
/*
 * Make sure the GlobalFmt structure GFmt is defined */
  gfmt = tf_set_gdefs_();
/*
 * Find which member of the structure is to be replaced */
  *j= -1;
  for(i=0;i<gfmt->nkey;i++)
    if( strcmp(name,gfmt->names[i])==0) { *j=i; break; }
/*
 * No match found for existing list? */
  if(*j<0) goto error;

  sizf=sizeof(float);
  sizl=sizeof(int);
  if((jp=strstr(gfmt->fmt[*j],"%f")) !=NULL)
        {n=1;
         if(jp!=gfmt->fmt[*j]) n = atol(gfmt->fmt[*j]);
         if(*num > n) *num = n;
         memcpy(mem+gfmt->goff[*j],val,(*num)*sizf);
         }
  else if((jp=strstr(gfmt->fmt[*j],"%g")) !=NULL)
        {n=1;
         if(jp!=gfmt->fmt[*j]) n = atol(gfmt->fmt[*j]);
         if(*num > n) *num = n;
         memcpy(mem+gfmt->goff[*j],val,(*num)*sizf);
         }
  else if((jp=strstr(gfmt->fmt[*j],"%d")) !=NULL)
        {n=1;
         if(jp!=gfmt->fmt[*j]) n = atol(gfmt->fmt[*j]);
         if(*num > n) *num = n;
         memcpy(mem+gfmt->goff[*j],val,(*num)*sizl);
         }
  else if( (jp = strstr(gfmt->fmt[*j],"s")) !=NULL)
        {lens=1;
         if(jp-gfmt->fmt[*j] >1 )
            lens = atol(gfmt->fmt[*j]+1);
         if(lens<1) lens=1;
         if(lens>128) lens=128;
         n=strlen(val);
         if(n<lens) strcpy(tmp,val);
          else strncpy(tmp,val,lens-1);
         tmp[lens-1]='\0';
         memcpy(mem+gfmt->goff[*j],tmp,lens); }
  else
        {*j= -2;
         goto error; }

  if(gfmt) free(gfmt);
  return *j;
  error:
  if(gfmt) free(gfmt);
  return *j;
 }

/*------------------------------------------------------------------
 *Name   : tf_glbl_getg_
 *Purpose: Retrieve a named global variable from a global structure.
 *------------------------------------------------------------------*/
  int  tf_glbl_getg_(char name[], int  *num,  char val[],  int  *j,
   char mem[])
 {

   int     sizl, sizf, i, n, lens;
  GlobalFmt *gfmt=0;
  char *jp;
/*
 * Make sure the GlobalFmt structure GFmt is defined */
  gfmt = tf_set_gdefs_();
/*
 * Find which member of the structure is to be obtained*/
  *j= -1;
  if(*num<=0) goto error;
  for(i=0;i<gfmt->nkey;i++)
    if( strcmp(name,gfmt->names[i])==0) { *j=i; break; }
/*
 * No match found for existing list? */
  if(*j<0) goto error;

  sizf=sizeof(float);
  sizl=sizeof(int);
  if((jp=strstr(gfmt->fmt[*j],"%f")) !=NULL)
        {n=1;
         if(jp!=gfmt->fmt[*j]) n = atol(gfmt->fmt[*j]);
         if(*num > n) *num = n;
         memcpy(val, mem+gfmt->goff[*j],(*num)*sizf);
         /* rw  = *(float *) (val);*/ }
  else if((jp=strstr(gfmt->fmt[*j],"%g")) !=NULL)
        {n=1;
         if(jp!=gfmt->fmt[*j]) n = atol(gfmt->fmt[*j]);
         if(*num > n) *num = n;
         memcpy(val, mem+gfmt->goff[*j],(*num)*sizf);
         /* rw  = *(float *) (val);*/ }
  else if((jp=strstr(gfmt->fmt[*j],"%d")) !=NULL)
        {n=1;
         if(jp!=gfmt->fmt[*j]) n = atol(gfmt->fmt[*j]);
         if(*num > n) *num = n;
         memcpy(val, mem+gfmt->goff[*j],(*num)*sizl);
         /* lw = *( int  *) (val);*/ }
  else if( (jp = strstr(gfmt->fmt[*j],"s")) !=NULL)
        {lens=1;
         if(jp-gfmt->fmt[*j] >1 )
            lens = atol(gfmt->fmt[*j]+1);
         if(lens<1) lens=1;
         if(lens>128) lens=128;
         strncpy(val, mem + gfmt->goff[*j], lens-1);
         val[lens-1]='\0'; }
  else
        {*j= -2;
         goto error; }

  if(gfmt) free(gfmt);
  return *j;
  error:
  if(gfmt) free(gfmt);
  return *j;
 }

/*------------------------------------------------------------------
 *Name   : tf_open_
 *Purpose: Open a trace file.
 *------------------------------------------------------------------*/
int  tf_open_(int  *lun,char *namen,TF_Global *glbu, int  *op_stat,char *msg)
{ int iotype=DSKIO_CSTR;
  return tf_xopen(&iotype, lun, namen, glbu, op_stat, msg);
}

int  tf_xopenf_(int *ufi, int *iotype, char *namen, int * op_stat,
     char *ftyp, int *grecsiz, char *wdtyp, int *nhdwd,
     int *n1, WREAL *o1, WREAL *d1, char *units1)
{TF_Global G;
 int i,dsize=1, iftyp,iwdtyp;
 char msg[96];

 iftyp = getgl_str_to_ftype(ftyp);
 iwdtyp = wrdc_iword_type(wdtyp);
 if(iwdtyp==WIEEE || iwdtyp==WIBM || iwdtyp==WVMS) dsize=4;
 if(iwdtyp==WIBM2) dsize=2;
 if(iwdtyp==WCRAY) dsize=8;
 if(iwdtyp==WSBYT) dsize=1;

 if(*op_stat >=3 | *op_stat== -4) { /* new file */
   strcpy(G.path,namen);
   strcpy(G.ftyp,ftyp);
   strcpy(G.srun, units1);
   G.ntrfil = 0;
   G.ntrcll = 1;          /* number of traces per cell    */
   G.grecsiz= *grecsiz;
   G.hdtyp  = 0;
   G.lun    = -1;
   G.wdtyp  = iwdtyp;    /* see wrdcnvrt.h for types     */
   G.ntb    = 0;
   G.numhc  = 0;
   G.trmaxg = 0.;
   G.nhdwd  = *nhdwd;     /* header words per trace       */
   G.nbyhd  = 4;          /* header words per trace       */
   G.hdtyp  = 0;
   G.nbydp  = dsize;      /* number of bytes per trace word */
   G.ndptr  = *n1;
   G.srval  = *d1;
   G.tstrt  = *o1;
   G.xorg   = 0.;
   G.yorg   = 0.;
   G.dx0[0]=1.0; G.dx0[1]=0.0;
   G.dx0[2]=0.0; G.dx0[3]=1.0;
   G.dn0[0]=1.0; G.dn0[1]=0.0;
   G.dn0[2]=0.0; G.dn0[3]=1.0;
   G.h=0;

   if(iftyp==TFILE_TYPE || iftyp==TF3D_TYPE || iftyp==TF3DF_TYPE) {
     G.nbyhd=4;    /* will use WIEEE for headers */
     G.grecsiz= (*grecsiz > GRECSIZ) ? *grecsiz : GRECSIZ; 
     G.hdtyp  = 0;
     strcpy(G.ftyp,"TFILE");
   }
   if(iftyp==SEGY_TYPE) {
     G.nhdwd=240;
     G.nbyhd=1;
     G.grecsiz=3600;
     G.hdtyp  =2;
     G.wdtyp = WIBM;
     if(iwdtyp==WIBM2) G.wdtyp=WIBM2;
     if(iwdtyp==WBYTE) G.wdtyp=WBYTE;
     if(iwdtyp==WSBYT) G.wdtyp=WSBYT;
   }
   G.nbycll = G.ntrcll*(G.nbyhd*G.nhdwd + G.ndptr*G.nbydp);
 }
 i = tf_xopen(iotype,ufi, namen, &G, op_stat, msg);
 if(*ufi <=0) { *ufi= -1; return *ufi; }
 if(*op_stat <3 && *op_stat!= -4) { /* old file */
  strcpy(ftyp,G.ftyp);
  *n1 = G.ndptr; 
  *o1 = G.tstrt;
  *d1 = G.srval;
  strcpy(units1,G.srun);
  *nhdwd = G.nhdwd;
  iwdtyp = G.wdtyp;
  strcpy(wdtyp,wrdc_cword_type(iwdtyp));
 }
/* recl= dskio_chain_recl_(ufi); */

 return *ufi;
}

/* user passes in glbu */
int  tf_xopen(int *iotype,int *lun, char *namen, 
     TF_Global *glbu, int *op_stat, char *msg)
{  int  j, k, nch, ifl, opf,istat,i_err;
   int  cl_stat,recl,wrd=0,npage=1,byperpage=262144;
   TF_Global G;

  *lun= -1;
  strcpy(msg,"OK");
  if(strncmp(namen,"NONE",4)==0 ) return 0;
  if(strncmp(namen,"none",4)==0 ) return 0;
  if(strncmp(namen," ",1)==0 )    return 0;
  nch = strlen(namen);
  if(nch > sizeof(glbu->path) )
    {  /*sprintf(msg,"01:tf_open: file name is >%.3s characters",
         sizeof(glbu->path) );*/
       printf(msg,"01:tf_open: file name is > %d characters",
              sizeof(glbu->path) );
       goto error;}

 ifl = *op_stat;
 if(*op_stat>4 ) ifl=0;

 j=dskio_exist_(namen);
 if( j != 0 )           /* file doesnt exist */
    {if(abs(ifl) < 3 )  /* old or upd requested */
      { strcpy(msg,"01:tf_open: no old file=");
        strcat(msg,namen);
        goto error;
      }
     else               /* new requested */
      { istat=0;
        istat = tf_Check_Glbl(glbu, msg);
        glbu->lun = -1;
        strcpy(glbu->path,namen);
        if(istat != 0 ) goto error;
      }
    }
 else                  /* file does exist */
     if(ifl >= 3)      /* but we requested new? */
      { strcpy(msg,"03:tf_open:old file & new, file=");
        strcat(msg,namen);
        goto error;
      }
     else              /* is this file already in db? */
      { if(ifl != -4) { 
        tf_glbl_getn_(&istat,namen,&G); /* DAY 10/1/91 use name */
        if(istat == 0 ) {
          memcpy( glbu,&G,sizeof(TF_Global));
          return 0;
        }
        k = get_global_data_(namen,glbu,&i_err);

        /*The following will allow trot file types to be read 
          before they are complete*/
        if(strstr(glbu->ftyp,"TROT")!=0)
          {
          if(i_err == MAYBE_INCOMPLETE_FILE)
            {
            k = i_err = 0;
            }
          }

        if(i_err > 0 ) { /* failed to get globals */
          strcpy(msg,"05:tf_open: failed getting globals");
          goto error;
        }
        }
      }

 opf = dskio_convert_flag(*op_stat);
/* set a natural record length in case fortran IO */
 recl = glbu->nbycll;
 dskio_xopc_(iotype,&glbu->lun,namen,&opf,&i_err,&byperpage,&npage,
       &recl,&wrd);

 glbu->opstat=opf;
 *lun      = (int) glbu->lun;

 /*Cannot do this any more since users want trot files named with
   a .byt extension. M.L.Sherrill 12/00
 if(strstr(namen,".byt") != NULL ) strcpy(glbu->ftyp,"CBYTE");
 if(strstr(namen,".BYT") != NULL ) strcpy(glbu->ftyp,"CBYTE");
 */

 if(glbu->lun<=0) {
    sprintf(msg,
    "04:tf_open: error in dskio_xopc, file=%s",namen);
    goto error;
 }
 strcpy(glbu->path,namen);
 /* make sure io buffer is allocated before 1st request */
 if(BSIZE < 20480)
  { buff = (char *) tf_set_bsiz(20480); }
 if(buff == NULL) { strcpy(msg,"09: buff error"); goto error; }

 if( abs(ifl) >= 3 )        /* start global record for new file */
  {tf_glbl_add_(&istat, lun, glbu );
   if(istat!=0)
    { strcpy(msg,"05:tf_open: failed adding global to db");
      cl_stat=1;
      tf_close_(lun,&cl_stat,msg); *lun=0; glbu->lun =0;
      goto error; }
  }
 else                  /* get global from old file */
  {tf_glbl_add_(&istat, lun, glbu );
   if(istat!=0)
    { strcpy(msg,"05:tf_open: failed adding global to db");
      cl_stat=0;
      tf_close_(lun,&cl_stat,msg);  *lun=0; glbu->lun =0;
      goto error; }
  }

 return 0;
 error:
 return msg[1]-'0';
}

/*------------------------------------------------------------------
 *Name   : tf_close_, tf_close_by_name
 *Purpose: Close a trace file.
 *------------------------------------------------------------------*/
int  tf_close_by_name( char *name,  int  *cl_stat, char *msg )
{ int  lun= -1;
  lun = tf_glbl_name_to_fd(name);
  return tf_close_( &lun, cl_stat,msg);
}

int  tf_close_( int  *lun,  int  *cl_stat, char *msg )
{ 
  int   tmp_lun= *lun, istat,i_err, len;
  char *tstr=0,path[256];
  TF_Global G;
  char *ftype;
  int tstat;
  int ftyp;

  strcpy(msg,"OK");
  if(*lun<=0) {
    strcpy(msg,"01:tf_close: lun<= 0"); goto error;
  }

  /*Prevent a unitialized warning. In the case we have trcio data
    the tf_glbl_get_ call will not find a database entry. The database
    entries are for the old byte file type only
  */
  strcpy(G.ftyp,"");

  tf_glbl_get_(&istat,lun,&G);

   /*Check for trcio type and close*/
  ftyp = getgl_str_to_ftype (tf_global_ftype(&G));
  if(!ftyp)
    {
    /*printf ("tf_close_: calling jsfiles_close\n");*/
    if (jsfiles_close(*lun) == 1) {
      /* there is a JavaSeis File with this logical unit number */
      /* WARNING: this logic is not robust */
      /* there is an ugly possiblity that a trcio file and a JSEIS file */
      /* have the same logical unit number! BEWARE */
      return 0;
    }
    trciof77wrapper_close_file_(lun, &tstat);
    if(tstat != 0)
      {
      strcpy(msg,"Error closing a trcio type file in tf_close_");
      goto error;
      }
    else
      {
      return 0;
      }
    }

  /*The following will be true if it is a trcio type file*/
  if(istat != 0) {
    strcpy(msg,"03:tf_close: no data base entry!"); goto error;
  }

/*
  if(G.opstat != dskio_ordo_() ) {
    if(put_global_to_filet_(&G,ofile) != 1) {
      strcpy(msg,"02:tf_close: no global written!");
    }
  }
*/



  tstr = tf_glbl_to_asc(&G);
  if(tstr) {
    len = (G.grecsiz>0) ? G.grecsiz : 0;
    if(G.opstat != dskio_ordo_() )
      tf_glbls_wr_(lun,&len,tstr,msg);
    if(tstr) free(tstr);
    if(strncmp(msg,"OK",2) != 0 ) goto error;
  }
/*
 *--- Delete the data base entry corresponding to lun */
  strcpy(path,G.path);
  if(G.h) { free(G.h); G.h=0; }
  tf_glbl_del_(&istat,lun);
  dskio_xxcl_(lun,&i_err);
  if(istat!=0 ) {
    strcpy(msg,"01:tf_close:tf_glbl_del problem "); goto error;
  }
  if(*cl_stat!=0) {
    if(remove(path) != 0)
        {strcpy(msg,"02:tf_close: remove of file failed "); goto error; }
  }

  return 0;
  error:
  return msg[1]-'0';
}
/*------------------------------------------------------------------
 *Name   : tf_glbls_rd_
 *Purpose: Read a global record from a CPS Trace File.
 *------------------------------------------------------------------*/
 int  tf_glbls_rd_(int *lun, char str[] , char *msg )
{
  int   lenr,nr,ierr;
  long   grecstrt;
  char   gmarker[GMARKSIZ];
  TF_Global G;
  strcpy(msg,"OK");
  str[0]='\0';
  if(*lun<=0) {
    strcpy(msg,"01:tf_glbls_rd: lun<= 0"); goto error;
  }

  tf_glbl_get_(&ierr,lun,&G);
  if(ierr !=0) {
   strcpy(msg,"01:tf_glbls_rd: no data base entry"); goto error;
  }
  grecstrt=GRECPOS;
  lenr = (G.grecsiz>0) ? G.grecsiz : 0;
  if(lenr==0) return 0;
  dskio_xxskrd_(&G.lun,buff,&grecstrt,&lenr,&ierr);
  if(ierr!=0) {
    strcpy(msg,"02:tf_glbls_rd: read error");
    goto error;
  }
  if(strncmp(buff,"*global",7)!=0) {
    strcpy(msg,"03:tf_glbls_rd: not standard trace file format");
    goto error;
  }
  nr = lenr - sizeof(gmarker);
  memcpy(str,buff+sizeof(gmarker),nr);
  str[nr-1]='\0';

  return 0;
  error:
  return msg[1]-'0';
}
/*------------------------------------------------------------------
 *Name   : tf_glbls_wr_
 *Purpose: Write an ascii global record to a CPS Trace File.
 *    - len Writes the lesser of len or the string length.
 *    - str the buffer to write to the file.
 * Note: Not suitable for binary headers which may have nulls
 *       embedded in the header!!
 *------------------------------------------------------------------*/
 int  tf_glbls_wr_(  int  *lun,  int  *len , char str[] , char *msg )
{
  long  grecstrt=GRECPOS;
  int   i,lenw,lens,ierr;
  TF_Global G;
  strcpy(msg,"OK");
  if(*len <= 0 ) return 0;
  if(*lun<=0) {
    strcpy(msg,"01:tf_glbls_wr: lun<= 0"); goto error;
  }
  tf_glbl_get_(&ierr,lun,&G);
  if(ierr !=0) {
   strcpy(msg,"02:tf_glbls_wr: no data base entry"); goto error;
  }
  lens = strlen(str);
  lenw = (lens < *len) ? lens + 1 : *len + 1;
  if(lenw> G.grecsiz) {
    strcpy(msg,"03:tf_glbls_wr: illegal to write more than grecsiz");
    goto error;
  }

  memcpy(buff,str,lenw);
  buff[lenw-1]='\0';
  dskio_xxskwr_(&G.lun,buff,&grecstrt,&lenw,&ierr);
  if(ierr != 0) {
    strcpy(msg,"04:tf_glbls_wr: write error");
    goto error;
  }

  return lenw;
  error:
  return 0;
}

/*------------------------------------------------------------------
 *Name   : tf_glbl_rd_
 *Purpose: Read a global record from a CPS Trace File.
 *------------------------------------------------------------------*/
 int  tf_glbl_rd_(  int  *lun, TF_Global *gtmp, char *msg )
{
  GlobalFmt *gfmt=0;
  char fname[104];
  long nn;
  int  lenr,ierr,l;
  long grecstrt=GRECPOS;
  TF_Global G;
  strcpy(msg,"OK");
  if(*lun <=0) {
    strcpy(msg,"01:tf_glbl_rd: UFI<=0"); goto error;
  }

  dskio_chain_fname_(lun,fname);
  l = get_global_data_(fname,gtmp,&ierr);

  /*The following will allow trot file types to be read 
    before they are complete*/
  if(strstr(gtmp->ftyp,"TROT")!=0)
    {
    if(ierr == MAYBE_INCOMPLETE_FILE)
      {
      l = ierr = 0;
      }
    }

  if(ierr ==0 || ierr < 0) {
    return 0;
  }
  lenr = GRECSIZ;
  dskio_xxskrd_(lun,buff,&grecstrt,&lenr,&ierr);
  if(ierr!=0) {
    strcpy(msg,"02:tf_glbl_rd: read error");
    goto error;
  }
  if(strncmp(buff,"*global",7)!=0) {
    strcpy(msg,"03:tf_glbl_rd: not standard trace file format");
    goto error;
  }

  gfmt = tf_set_gdefs_();
  strcpy(gtmp->ftyp,"TFILE");
  if(ccode_(gfmt->fmt,gfmt->names,gfmt->goff,gfmt->nkey,
     (char *) gtmp,buff,msg) != gfmt->nkey ) {
    printf("tf_glbl_rd: missing decode targets\n");
  }

  if(gfmt) free(gfmt);
  gtmp->lun = *lun;
  return 0;
  error:
  if(gfmt) free(gfmt);
  return msg[1]-'0';
}

/*------------------------------------------------------------------
 *Name   : tf_glbl_wr_
 *Purpose: Convert global structure to ascii and write as a global
 *         record to a CPS Trace File.
 *------------------------------------------------------------------*/
 int  tf_glbl_wr_(  int  *lun, TF_Global *gtmp, char *msg )
{
  GlobalFmt *gfmt=0;
  long       grecstrt=GRECPOS;
  int        i,lenw,lens,ierr;
  char      *tstr=0;
  TF_Global G;
  strcpy(msg,"OK");
  if( gtmp->grecsiz <= 0 ) return 0;
  if(*lun<=0) {
    strcpy(msg,"01:tf_glbl_wr: UFI<= 0"); goto error;
  }
  tf_glbl_get_(&ierr,lun,&G);
  if(ierr !=0) {
   strcpy(msg,"02:tf_glbl_wr: no data base entry"); goto error;
  }

  gtmp->lun = *lun;
 /* if(gtmp->grecsiz<GRECSIZ) gtmp->grecsiz=GRECSIZ; */
  tstr = tf_glbl_to_asc(gtmp);
  if(!tstr) {
    strcat(msg," tf_glbl_wr: failed to build global string");
    goto error;
  }
  for(i=0;i<=gtmp->grecsiz-1;i++) buff[i]=' ';
  lens=strlen(tstr);
  memcpy(buff,tstr,lens+1);
  buff[gtmp->grecsiz-1]='\0';
  if(tstr) free(tstr);

  /* update the data base entry as well as the file */
  lenw = gtmp->grecsiz;
  if(lens+1>lenw) {
    strcat(msg," tf_glbl_wr: warning-string longer than grecsiz");
  } 
  dskio_xxskwr_(lun,buff,&grecstrt,&lenw,&ierr);
  if(ierr != 0) {
    strcpy(msg,"03:tf_glbl_wr: write error");
    goto error;
  }
  tf_glbl_add_(&ierr, lun, gtmp );

  return 0;
  error:
  if(tstr) free(tstr);
  return msg[1]-'0';
}

char *tf_glbl_to_asc(TF_Global *g)
{ /* convert a binary global record to an ascii global record */
  /* allocates memory for the returned string. */
  Grid3DDesc   *h=0;
  GlobalFmt *gfmt=0;
  int        i=0,lens;
  float      ftes=0;
  char      *str=0,msg[88];

  if(!g) return str;
  if(g->grecsiz <=0) return str;
/* allocate and Initialize the receiving string */
  if(strcmp(g->ftyp,getgl_ftype_to_str(TFILE_TYPE))==0) i=1;
  if(strcmp(g->ftyp,getgl_ftype_to_str(TF3D_TYPE))==0)  i=2;
  if(strcmp(g->ftyp,getgl_ftype_to_str(TF3DF_TYPE))==0) i=3;
  if(i==0) return str;
  lens = (g->grecsiz> GRECSIZ) ? g->grecsiz : GRECSIZ;
  str = (char *) calloc(1,lens);
  if(!str) return str;
  strcpy(str,"*global ");

/* Take care of encoding conventional 2D global */
  gfmt = tf_set_gdefs_();
  cncode_(gfmt->fmt,gfmt->names,gfmt->goff,gfmt->nkey,
          g,str+8, msg );
  if(gfmt) free(gfmt);
  if(strncmp(msg,"OK",2)!=0) {
    strcat(msg," tf_glbl_to_asc: no encoding of global");
    free(str);
    return NULL;
  }
  str[g->grecsiz-1]='\0'; /* ensure that there is a null */
  if(!g->h || i<2) return str;

/* Encode 3D parameters if there are any*/
  h = (Grid3DDesc *) g->h;
  sprintf(msg," %s=%10d %s=%10d %s=%10d \n", h->keys[0],h->N.v[0],
   h->keys[1],h->N.v[1], h->keys[2],h->N.v[2]);
  strcat(str,msg);
  sprintf(msg," %s=%10g %s=%10g %s=%10g \n", h->keys[3],h->O.v[0],
   h->keys[4],h->O.v[1], h->keys[5],h->O.v[2]);
  strcat(str,msg);
  sprintf(msg," %s=%10g %s=%10g %s=%10g \n", h->keys[6],h->D.v[0],
   h->keys[7],h->D.v[1], h->keys[8],h->D.v[2]);
  strcat(str,msg);
  sprintf(msg," %s=%-s %s=%-s %s=%-s \n", h->keys[9],h->axis.v[0],
   h->keys[10],h->axis.v[1], h->keys[11],h->axis.v[2]);
  strcat(str,msg);
  sprintf(msg," %s=%d %s=%d %s=%d \n", h->keys[18],h->hd.v[0],
   h->keys[19],h->hd.v[1], h->keys[20],h->hd.v[2]);
  strcat(str,msg);
  sprintf(msg," %s=%s\n", h->keys[13],h->P.name);
  strcat(str,msg);
  ftes = h->U.v[0]+h->U.v[1]+h->U.v[2];
  if(ftes !=0)
   {sprintf(msg,"%s= %10f %10f %10f\n",
    h->keys[15],h->U.v[0],h->U.v[1],h->U.v[2]);
    strcat(str,msg);
   }
  ftes = h->V.v[0]+h->V.v[1]+h->V.v[2];
  if(ftes !=0)
   {sprintf(msg,"%s= %10f %10f %10f\n",
    h->keys[16],h->V.v[0],h->V.v[1],h->V.v[2]);
    strcat(str,msg);
   }
  ftes = h->W.v[0]+h->W.v[1]+h->W.v[2];
  if(ftes !=0)
   {sprintf(msg,"%s= %10f %10f %10f\n",
    h->keys[17],h->W.v[0],h->W.v[1],h->W.v[2]);
     strcat(str,msg);
   }

  return str;
}

/*------------------------------------------------------------------
 *Name   : tf_tr_rd_
 *Purpose: Read headers and traces sequentially from a trace file.
 *------------------------------------------------------------------*/
 int  tf_tr_rd_( int  *lun, int  *ns, int  *num,  int  *nsamp,  int  *samp1,
     int  *sdec,  int  *trnsps, char hd[],char tr[],char *msg)
{
   int  i,j,k, igath, iscat, iby, i1,i2,j1,j2, dir;
   int  nbycll,ntrcll,grecsiz,maxcll,maxtr;
   int  trace_count,lasttr,outtr,hit,iput,iget;
   int  nbydp, nbyhd, nhdwd, ndptr;
   int  istat, ierr;
   int  trace_no, cell_no, pos_in_cell, byt_per_tr, hoffset, toffset;
   int  nrd, buffin, l1;
   TF_Global G;
  strcpy(msg,"OK");
  if(*num == 0) *num = 1;
  if(*lun <= 0) {
    strcpy(msg,"01:tf_tr_rd: lun<= 0");
    *num = 0;
    goto error;
  }
  tf_glbl_get_( &istat, lun, &G);
  if(istat!=0) {
    strcpy(msg,"02:tf_tr_rd:tf_glbl_get:  get failure");
    *num = 0;
    goto error;
  }
  if(G.ntrfil<=0) {
    strcpy(msg,"02:tf_tr_rd: no traces in file");
    *num = 0;
    goto error;
  }

  if(*ns <= 0) {
    strcpy(msg,"03:tf_tr_rd: negative starting trace?");
    *num = 0;
    goto error;
  }
  if(*ns > G.ntrfil)    {
    strcpy(msg,"04:tf_tr_rd: requested beyond last trace of file");
    *num=0;
    goto error;
  }

  if(ntrbyt < sizeof(float)*abs(*num) )
     { if(trace_byt != NULL ) free(trace_byt);
       ntrbyt = abs(*num);
       if(ntrbyt< MAXNUM) ntrbyt = MAXNUM;
       trace_byt = ( long  *) malloc(ntrbyt*sizeof(long));
     }
  if(trace_byt == NULL) {
    strcpy(msg,"05:tf_tr_rd: error allocating trace_byt");
    *num=0;
    goto error;
  }


  if(*ns + *num -1> G.ntrfil) *num=G.ntrfil - *ns       + 1;
  if(*ns + *num < 1) *num = -*ns;

  nbycll=G.nbycll;
  ntrcll=G.ntrcll;
  grecsiz=G.grecsiz;
  nbydp =G.nbydp;
  nbyhd =G.nbyhd;
  nhdwd =G.nhdwd;
  ndptr =G.ndptr;
  byt_per_tr = tf_global_byinht(&G);
/*
 * Compute the byte positions of the requested traces */
  hit = 0;
  lasttr=0;
  trace_count=0;
  dir = (*num>0) ? 1 : -1;
  for( i=0; i<=abs(*num)-1; i++ ) {
     trace_no   = *ns + dir*i;
     if(trace_no> G.ntrfil) trace_no=G.ntrfil;
     else if(trace_no< 1)      trace_no=1;
     else lasttr++;
     cell_no    = (trace_no-1)/ntrcll;
     pos_in_cell= trace_no  - cell_no*ntrcll -1;
     trace_byt[i]= grecsiz + cell_no*nbycll + pos_in_cell*byt_per_tr;
  }
/*
 * Read traces into buffer in byte ascending order. */
   if(BSIZE < nbycll) buff = (char *) tf_set_bsiz(nbycll + 1024);
   if(buff == NULL) { strcpy(msg,"09: buff error"); goto error; }
   maxcll=(BSIZE-511)/nbycll;     /* safe estimate */
   if(maxcll < 1) maxtr = 1;
   else maxtr = maxcll*ntrcll;

   j1 = (*num>0) ? 0 : lasttr-1;
   for(k=0;k<=(lasttr-1)/maxtr;k++) {
        j2 = j1 + dir*(maxtr-1);
        if(j2>abs(*num)-1) j2 = abs(*num)-1;
        if(j2>lasttr-1) j2 = lasttr-1;
        if(j2<0) j2 = 0;
        buffin = trace_byt[j2] - trace_byt[j1] +  byt_per_tr;
/*
      nrd = dskiord_(lun,&trace_byt[j1],&buffin,buff);
      if(nrd!=buffin)
*/
        dskio_xxskrd_(&G.lun,buff,&trace_byt[j1],&buffin,&ierr);
        if(ierr!=0)
        { strcpy(msg,"07:tf_tr_rd:dskiord read problem");
          *num=0;   goto error; }
/*
 * Transfer data from buffer to hd and tr arrays
 * Traces will be in order requested( *num < 0 is allowed ) */
        igath = *sdec*nbydp;
        iscat = (*trnsps ==0 ) ? nbydp : nbydp*(*num) ;
        i1=k*maxtr;
        i2=(i1+maxtr-1<=lasttr-1) ? i1+maxtr-1 : lasttr-1;
        for( i=i1; i<=i2; i++ ) {
          hoffset=trace_byt[i] - trace_byt[j1];
          toffset=hoffset + nbyhd*nhdwd + nbydp*(*samp1-1);
          memcpy(hd + trace_count*nbyhd*nhdwd,buff+hoffset,nbyhd*nhdwd);
          outtr = (*trnsps ==0 ) ? trace_count*nbydp*(*nsamp) :
                  trace_count*nbydp;
          if(igath==nbydp && iscat==nbydp)
           memcpy(tr + outtr,buff+toffset,*nsamp*nbydp);
          else {
             for(iby=0;iby<nbydp;iby++)
             tf_bmov_(buff+toffset+iby,&igath,tr+outtr+iby,
             &iscat,nsamp);
          }
          trace_count += 1;
          if(trace_count >= *num) goto done;
        }
        j1+= dir*maxtr;
   }

 done:
  if( trace_count!= *num && *trnsps!=0 ) {
    for(j=1;j<= *nsamp;j++) {
      iput = (j-1)*trace_count*nbydp;
      iget = (j-1)*(*num)*nbydp;
      memcpy(tr+iput,tr+iget,trace_count*nbydp);
    }
  }
  *num=trace_count;

  return 0;
  error:
  return msg[1]-'0';
}

/*------------------------------------------------------------------
 *Name   : tf_tr_wr_
 *Purpose: Write headers and traces in sequential order to a trace
 *         file. May begin at an arbitrary point in the file.
 *------------------------------------------------------------------*/
 int  tf_tr_wr_( int  *lun, int  *ns, int  *num,char hd[],char tr[],char *msg)
{
  TF_Global G;
   int  i, k, j1, j2, nwr, maxcll, maxtr, ierr;
   int  istat, ntrcll, nbycll, grecsiz, ndptr, nbydp, nbyhd, nhdwd;
   int  trace_no, cell_no, pos_in_cell, byt_per_tr, hoffset, toffset;
   int  buffin, buffo;

  strcpy(msg,"OK");
  if(*lun <= 0)
    { strcpy(msg,"01:tf_tr_wr: lun<= 0");
      goto error; }
  tf_glbl_get_( &istat, lun, &G);
  if(istat!=0)
    { strcpy(msg,"02:tf_tr_wr:tf_glbl_get:  get failure");
      goto error; }

  if(*ns <= 0)
    { strcpy(msg,"03:tf_tr_wr: negative starting trace?");
      goto error; }
  if(*num < 1)
    { strcpy(msg,"04:tf_tr_wr: no. to write<1?");
      goto error; }

  if(ntrbyt < sizeof(float)*abs(*num) )
     { if(trace_byt != NULL ) free(trace_byt);
       ntrbyt = abs(*num);
       if(ntrbyt< MAXNUM) ntrbyt = MAXNUM;
       trace_byt = (long *) malloc(ntrbyt*sizeof(long));
     }
  if(trace_byt == NULL)
    { strcpy(msg,"05:tf_tr_wr: error allocating trace_byt");
       goto error; }

  nbycll=G.nbycll;
  ntrcll=G.ntrcll;
  grecsiz=G.grecsiz;
  nbydp =G.nbydp;
  nbyhd =G.nbyhd;
  nhdwd =G.nhdwd;
  ndptr =G.ndptr;
  byt_per_tr = tf_global_byinht(&G);

/*
 * Compute the byte positions of the traces in the file*/
  for( i=0; i<= *num-1; i++ ) {
     trace_no   = *ns + i;
     cell_no    = (trace_no-1)/ntrcll;
     pos_in_cell= trace_no  - cell_no*ntrcll -1;
     trace_byt[i]= grecsiz + cell_no*nbycll + pos_in_cell*byt_per_tr;
  }

/* Adjust buffer size to fit at least 1 cell.        **
 * Fill the IO buffer with traces and output to disk */
   if(BSIZE < nbycll) buff = (char *) tf_set_bsiz(nbycll + 1024);
   if(buff == NULL) { strcpy(msg,"09: buff error"); goto error; }
   maxcll=(BSIZE-511)/nbycll;     /* safe estimate */
   if(maxcll < 1) maxtr = 1;
   else maxtr = maxcll*ntrcll;
   for(k=0;k<=(*num-1)/maxtr;k++)  {      /* cycle for buffer size*/
       j1=k*maxtr;
       j2=j1 + (maxtr-1);
       if(j2>abs(*num)-1) j2 = *num-1;
/*
 * Place the traces from hd and tr into an io buffer */
       buffo = trace_byt[j2] - trace_byt[j1] +  byt_per_tr;
       if(buffo>BSIZE) {
          strcpy(msg,"06:tf_tr_wr: buffer overflow");
          goto error;
       }
       for( i=j1; i<=j2; i++ ) {
          hoffset=trace_byt[i] - trace_byt[j1];
          toffset=hoffset+nbyhd*nhdwd;
          memcpy(buff+hoffset,hd + i*nbyhd*nhdwd,nbyhd*nhdwd);
          memcpy(buff+toffset,tr + i*ndptr*nbydp,ndptr*nbydp);
       }
/*
 * Output the io buffer to disk */
/*
       nwr=dskiowr_(lun,&trace_byt[j1],&buffo,buff);
       if(nwr!=buffo) {
*/
       dskio_xxskwr_(&G.lun,buff,&trace_byt[j1],&buffo,&ierr);
       if(ierr !=0) {
          strcpy(msg,"07:tf_tr_wr:dskiowr write problem");
          *num=0;
          goto error;
       }
/*
 * Update the trace count for the globals data base in memory */
        if(G.ntrfil < *ns + j2 ) G.ntrfil       = *ns + j2 ;

   }

  tf_glbl_add_( &istat, lun, &G);
  if(istat!=0) {
    strcpy(msg,"08:tf_tr_wr:tf_glbl_add: failed to update");
    goto error;
  }

  return 0;
  error:
  return msg[1]-'0';
}
/*------------------------------------------------------------------
 *Name   : tf_hc_wr_
 *Purpose: Write history card images to a Trace File.
 *------------------------------------------------------------------*/
 int  tf_hc_wr_( int  *lun, int  *ns, int  *num,char *hc, char *msg)
{
  TF_Global G;
  int   istat;
  long  first_hc_byt;
  int   l1,nw;

  strcpy(msg,"OK");
  if(*lun <= 0) {
    strcpy(msg,"01:tf_hc_wr: lun<= 0");
    goto error;
  }
  if(hc == NULL) {
    strcpy(msg,"01:tf_hc_rd: hc buffer = 0");
    goto error;
  }
  if(*ns <= 0) {
    strcpy(msg,"02:tf_hc_wr: negative starting card?");
    goto error;
  }
  if(*num <= 0) {
    strcpy(msg,"03:tf_hc_wr: no. to write<1?");
    goto error;
  }

  tf_glbl_get_( &istat, lun, &G);
  if(istat!=0) {
    strcpy(msg,"04:tf_hc_wr:tf_glbl_get:  get failure");
    goto error;
  }

  first_hc_byt= (long) (tf_global_first_hcbyt(&G) + (*ns-1)*BYPERHC);
/*
 * Output the io buffer to disk */
   l1 = *num * BYPERHC;
   dskio_xxskwr_(lun,hc,&first_hc_byt,&l1,&istat);
   if(istat != 0) {
     strcpy(msg,"06:tf_hc_wr: write problem");
     *num= 0; goto error;
   }
/*
 * Update the history count for the globals data base in memory */
   if(G.numhc  < *ns + *num -1 ) G.numhc = *ns + *num -1 ;
   tf_glbl_add_( &istat, lun, &G);
   if(istat!=0) {
     strcpy(msg,"07:tf_hc_wr:tf_glbl_add: failed to update");
     goto error;
   }
  return 0;
  error:
  return msg[1]-'0';
}

/*------------------------------------------------------------------
 *Name   : tf_hc_rd_
 *Purpose: Read history card images from a Trace File.
 *------------------------------------------------------------------*/
 int  tf_hc_rd_( int  *lun, int  *ns, int  *num,char *hc, char *msg)
{
  TF_Global G;
   int  istat;
   long  first_hc_byt;
  int  l1,nr;

  strcpy(msg,"OK");
  if(*lun <= 0){
    strcpy(msg,"01:tf_hc_rd: lun<= 0");
    goto error;
  }
  if(hc == NULL) {
    strcpy(msg,"01:tf_hc_rd: hc buffer = 0");
    goto error;
  }
  if(*ns <= 0) {
    strcpy(msg,"02:tf_hc_rd: negative starting card?");
    goto error;
  }
  if(*num <= 0) {
    strcpy(msg,"03:tf_hc_rd: no. to write<1?");
    goto error;
  }

  tf_glbl_get_( &istat, lun, &G);
  if(istat!=0) {
    strcpy(msg,"04:tf_hc_rd:tf_glbl_get:  get failure");
    goto error;
  }

  first_hc_byt= (long) (tf_global_first_hcbyt(&G) + (*ns-1)*BYPERHC);
  l1 = *num *BYPERHC;
/*
 * Read the requested history cards */
  dskio_xxskrd_(lun,hc,&first_hc_byt,&l1,&istat);
  if(istat != 0) {
    strcpy(msg,"06:tf_hc_rd: read problem");
    *num= 0; goto error;
  }

  return 0;
  error:
  return msg[1]-'0';
}

/*------------------------------------------------------------------
 *Name   : tf_arr_rd_
 *Purpose: Read a regular pattern of traces from a CPS trace file.
 *------------------------------------------------------------------*/
 int  tf_arr_rd_( int  *lun,IO_request *Cl, char hd[],char arr[],char *msg)
{
  TF_Global G;
   char *hdoffset, *troffset;
   int  igath, iscat, iput, iget, l1, iby, buffin;
   int  num,i1,i2,j1,j2, i,j,k,m,hit;
   int  ngrps, dir, outtr, lasttr;
   int  trace_count, n1, n2, istat;
   int  nbycll,ntrcll,maxcll,maxtr;
   int  nbydp, nbyhd, nhdwd, ndptr;
   int  nsamp,samp1,sdec,trnsps;
   int   sizf, pmax, shift;

  strcpy(msg,"OK");
  if(*lun <= 0) {
    strcpy(msg,"01:tf_arr_rd: lun<= 0");
    Cl->ntot = 0;
    goto error;
  }
  tf_glbl_get_( &istat, lun, &G);
  if(istat!=0) {
     strcpy(msg,"02:tf_arr_rd:tf_glbl_get:  get failure");
     Cl->ntot = 0;
     goto error;
  }
  if(G.ntrfil<=0) {
     strcpy(msg,"02:tf_arr_rd: no traces in file");
     Cl->ntot = 0;
     goto error;
  }


  if(Cl->iskp < 0 || Cl->iskp >G.ntrfil-1) {
     strcpy(msg,"03:tf_arr_rd: ISKP<0 or >ntrfil-1?");
     Cl->ntot = 0;
     goto error;
  }
  if(Cl->ntot < 1) {
     strcpy(msg,"04:tf_arr_rd: NTOT<1:NO traces requested?");
     goto error;
  }
  dir = (Cl->ndo>=0) ? 1 : -1;
  if(Cl->nskp == 0 && Cl->ndo < 10 )  Cl->ndo = dir*MAXNUM;
  if(Cl->ndo == 0) {
     strcpy(msg,"05:tf_arr_rd: NDO=0 NO traces requested?");
     Cl->ntot = 0;
     goto error;
  }

  if(ntrbyt < sizeof(float)*abs(Cl->ndo) ) {
     if(trace_byt != NULL ) free(trace_byt);
     ntrbyt = abs(Cl->ndo);
     if(ntrbyt< MAXNUM) ntrbyt = MAXNUM;
     trace_byt = ( long *) malloc(ntrbyt*sizeof(long));
  }
  if(trace_byt == NULL) {
     strcpy(msg,"05:tf_arr_rd: error allocating trace_byt");
     goto error;
  }

  if(abs(Cl->ndo)> Cl->ntot ) Cl->ndo = dir*Cl->ntot;

/*
 *Allocate memory for lav factors if needed */
   sizf = sizeof(float);
   if(length_of_lav < sizf*(Cl->ntot+1) ) {
       if(lav != NULL ) free(lav);
       lav = (float *) malloc( (1+ Cl->ntot)*sizf );
       length_of_lav = sizf*(Cl->ntot+1);
   }

  nbycll=G.nbycll;
  ntrcll=G.ntrcll;
  nbydp =G.nbydp;
  nbyhd =G.nbyhd;
  nhdwd =G.nhdwd;
  ndptr =G.ndptr;
  nsamp = Cl->nsamp;
  samp1 = Cl->samp1;
  sdec  = Cl->sdec;
  trnsps= Cl->trnsps;

  if(BSIZE < nbycll) buff = (char *) tf_set_bsiz(nbycll + 1024);
  if(buff == NULL) { strcpy(msg,"09: buff error"); goto error; }
  maxcll=(BSIZE-511)/nbycll;     /* safe estimate */
  if(maxcll < 1) maxtr = 1;
  else maxtr = maxcll*ntrcll;
/*
 * Loop over the trace groups 1 <= m <= ngrps .*/
  trace_count=0;
  n1    = Cl->iskp+1;   /* n1 is starting trace of a group */
  n2    = n1 + dir*(abs(Cl->ndo)-1);
  hit   = 0;
  ngrps = (Cl->ntot - 1)/abs(Cl->ndo) + 1;
  for(m=1;m<=ngrps;m++) {
    if(n2 < 1 || n2 > G.ntrfil) hit=1;
    hdoffset=hd +  (trace_count*G.nhdwd*G.nbyhd);
    troffset=arr + (trace_count*nsamp*G.nbydp);
    /*troffset=arr + (trace_count*G.ndptr*G.nbydp); */
    num = abs(Cl->ndo);
    num = (Cl->ntot-(m-1)*num < num) ? Cl->ntot - (m-1)*num : num;
    istat = tf_tr_rd_(lun,&n1,&num,&nsamp,&samp1,
     &sdec,  &Cl->trnsps, hdoffset,troffset,msg);
    if(istat != 0) {
     strcpy(msg,"01:tf_arr_rd: istat=%d from tf_tr_rd");
     goto error;
    }
    trace_count += num;
    n1 +=  dir*(abs(Cl->ndo) - 1) + Cl->nskp + 1;
    if(n1<1 || n1 > G.ntrfil) goto done;
    n2  =  n1 + dir*(abs(Cl->ndo)-1);
    if(hit>0) goto done;
  }

 done:
  if( (trace_count!=Cl->ntot) && trnsps!=0 ) {
     for(j=1;j<=nsamp;j++) {
         iput = (j-1)*trace_count*nbydp;
         iget = (j-1)*Cl->ntot*nbydp;
         memcpy(arr+iput,arr+iget,trace_count*nbydp);
     }
  }
  Cl->ntot=trace_count;
/*  Place scalee factors at endd of array
 * place scale factors on word boundary
  lav[Cl->ntot]= bigbig;
  shift=2;
  if(sizeof(float)==8) shift=3;
  pmax = Cl->ntot * Cl->nsamp * G.nbydp;
  pmax = pmax << shift;
  pmax = ( pmax >> shift ) + sizf;
  memcpy(arr+pmax,lav,(trace_count+1)*sizf);
 */

  return 0;
  error:
  return msg[1]-'0';
}

/*------------------------------------------------------------------
 *Name   : tf_arr_wr_
 *Purpose: Write header and trace data to an output trace file.
 *------------------------------------------------------------------*/
 int  tf_arr_wr_( int  *lun,IO_request *Cl, char hd[],char arr[],char *msg)
{
  TF_Global G;
   int    ngrps, num, nwr;
   int    istat, i, trace_count, n1;
  char   *hdoffset, *troffset;
  strcpy(msg,"OK");
  if(*lun <= 0)
    { strcpy(msg,"01:tf_arr_wr: lun<= 0");
      goto error; }
  tf_glbl_get_( &istat, lun, &G);
  if(istat!=0)
    { strcpy(msg,"02:tf_arr_wr:tf_glbl_get:  get failure");
      goto error; }

  if(Cl->iskp < 0 )
    { strcpy(msg,"03:tf_arr_wr: ISKP<0 ?");
      goto error; }
  if(Cl->ntot < 1)
    { strcpy(msg,"04:tf_arr_wr: NTOT<1:NO traces to write?");
      goto error; }
  if(Cl->ndo < 1)
    { strcpy(msg,"05:tf_arr_wr: NDO<1:NO traces to write?");
      goto error; }
  if(Cl->nskp <= 0 ) Cl->ndo = Cl->ntot;
  if(abs(Cl->ndo)> Cl->ntot ) Cl->ndo = Cl->ntot;

  ngrps = (Cl->ntot - 1)/abs(Cl->ndo) + 1;
  n1 = Cl->iskp+1;   /* n1 is starting trace of a group */
  trace_count=0;
  for(i=1;i<=ngrps;i++)
   {hdoffset=hd +  (trace_count*G.nhdwd*G.nbyhd);
    troffset=arr + (trace_count*G.ndptr*G.nbydp);
    num = Cl->ndo;
    istat = tf_tr_wr_(lun,&n1,&num,hdoffset,troffset,msg);
    if(istat != 0)
     { strcpy(msg,"06:tf_arr_wr: Error from tf_tr_wr_ could not write trace");
      goto error; }
    trace_count +=  num;
    if(n1+Cl->ndo-1>G.ntrfil) G.ntrfil=n1+Cl->ndo-1;
    n1 += Cl->ndo + Cl->nskp;
   }

  Cl->ntot=trace_count;
  tf_glbl_add_( &istat, lun, &G);
  return 0;
  error:
  return msg[1]-'0';

}


 int  tf_check_cntrl(TF_Global *G,IO_request *C)
{ int  dir;

/*
  if(C->iskp > G->ntrfil-1) C->iskp = G->ntrfil-1;
*/
  if(C->iskp > G->ntrfil) C->iskp = G->ntrfil;
  if(C->iskp < 0 ) C->iskp = 0;
  if(C->ntot > G->ntrfil) C->ntot = G->ntrfil;
  if(C->ntot < 1 ) C->ntot = 1;
  if(C->ndo == 0) C->ndo = 1;
  dir = (C->ndo>=0) ? 1 : -1;
  if(abs(C->ndo)> C->ntot ) C->ndo = dir*C->ntot;
  if(C->nskp == 0 ) C->ndo = dir*C->ntot;

  if( C->sdec  < 1 ) C->sdec = 1;
  if( C->sdec  > G->ndptr ) C->sdec = G->ndptr;
  if( C->samp1 < 1 ) C->samp1 = 1;
  if( C->samp1 > G->ndptr ) C->samp1 = G->ndptr;
  if( C->samp1 + (C->nsamp-1)*C->sdec > G->ndptr)
     C->nsamp = (G->ndptr - C->samp1)/C->sdec + 1;
/* zero samples is legitimate if only headers are wanted */
  if( C->nsamp < 1 ) C->nsamp = 0;

  if(C->axis>3) C->axis=0;
 return 0;
}

 int  tf_Check_Glbl(TF_Global *G, char *msg)
{
   int  byt_per_tr;
  strcpy(msg,"OK");

  byt_per_tr = tf_global_byinht(G);
  if(G->nbycll<=0)
   { strcpy(msg,"01:bad global, nbycll<=0"); goto error; }
  if(G->nbycll<byt_per_tr)
   { strcpy(msg,"01:bad global, nbycll<=bytes per trace"); goto error; }
  if(G->ntrcll<1)
   { strcpy(msg,"01:bad global, ntrcll<=0"); goto error; }
  if(G->nbydp<1 || G->nbyhd<1)
   { strcpy(msg,"01:bad global, nbyhd<=0 or nbydp<=0"); goto error; }
  if(G->nhdwd+G->ndptr<0)
   { strcpy(msg,"01:bad global,  nhdwd+ndptr<0"); goto error; }
  if(G->ntrfil<0)
   { strcpy(msg,"01:bad global,  ntrfil<0"); goto error; }

  if(strcmp(G->ftyp,"TFILE")==0 && G->grecsiz < GRECSIZ)
   { strcpy(msg,"01:bad global,   grecsiz <GRECSIZ"); goto error; }

 return 0;
 error:
 return msg[1]-'0';
}

 int  tf_print_glbls_(char name[], TF_Global *glbl)
{
 printf("\nGlobal parameters for file=%s\n\n",name);
 printf("ntrfil\tnbycll\tntrcll\t nhdwd\t ndptr\t  ftyp\n");
 printf("%6d\t%6d\t%6d\t%6d\t%6d\t%s\n",glbl->ntrfil,glbl->nbycll,
 glbl->ntrcll,glbl->nhdwd,glbl->ndptr,glbl->ftyp);
 printf("\n nbyhd\t nbydp\tgrecsiz\t numhc\t srval\t tstrt\n");
 printf("%6d\t%6d\t%6d\t%6d\t%6.4f\t%6.2f\t\n",glbl->nbyhd,glbl->nbydp,
 glbl->grecsiz,glbl->numhc,glbl->srval,glbl->tstrt);
 return 0;
}

void tf_bmov_(char *ap,  int  *iap, char *bp,  int  *ibp,  int  *np)
{
  char *a, *b;
   int  ia, ib, n;
  a=ap;b=bp;n=(*np);ia=(*iap);ib=(*ibp);
  for (;n>0;n--) { (*b)=(*a); b+=ib; a+=ia;}
}


GlobalFmt *tf_set_gdefs_( )
{
  GlobalFmt *gf=0;
  TF_Global *g;
  int     i;

/*
 * Keyword names in the global record
 * Formats of global parameters.
 * names = List of GLBL member decode names
 * fmt   = Format type of GLBL members
 * goff  = Memory offset from base address
 * nn in nn%d and nn%f indicate the member size for int and float.
 * Assumed to be 1 if nn is missing */
  g = (TF_Global *) malloc(sizeof(TF_Global));
  gf= (GlobalFmt *) calloc(1, sizeof(GlobalFmt));
  i=0;
  gf->goff[ i] = 0;
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"ntrfil"); i++;
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"nbycll"); i++; /* Number of bytes per disk cell     R*/
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"ntrcll"); i++; /* Number of traces per disk cell    R*/
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"grecsiz"); i++;/* Size of the global record in bytesR*/
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"ntb"); i++;   /* No. of trailer bytes before history*/
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"numhc"); i++; /* Number of history cards in the file*/
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"nbydp"); i++; /* Number of bytes/data point >0     R*/
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"nbyhd"); i++; /* Number of bytes/header word  >0   R*/
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"hdtyp"); i++; /* Header type.0-CPS,1-ISP...         */
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"wdtyp"); i++; /* Float word type 0-Native,1-IEEE    */
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"nhdwd"); i++; /* Number of header words/trace      R*/
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"ndptr"); i++; /* Number of data points/trace       R*/
  strcpy(gf->fmt[i],"%f");
  strcpy(gf->names[i],"srval"); i++; /* Sample rate value of the data points*/
  strcpy(gf->fmt[i],"%f");
  strcpy(gf->names[i],"tstrt"); i++; /* Trace start time(or depth)         */
  strcpy(gf->fmt[i],"%f");
  strcpy(gf->names[i],"xorg"); i++;  /* CPS global                         */
  strcpy(gf->fmt[i],"%f");
  strcpy(gf->names[i],"yorg"); i++;  /* CPS global                         */
  strcpy(gf->fmt[i],"4%f");
  strcpy(gf->names[i],"dx0"); i++;   /* CPS global                         */
  strcpy(gf->fmt[i],"4%f");
  strcpy(gf->names[i],"dn0"); i++;   /* CPS global                         */
  strcpy(gf->fmt[i],"%g");
  strcpy(gf->names[i],"trmaxg"); i++;/* Largest amplitude in all traces    */
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"dunits"); i++;  /* distance units, 1=meters,2=feet  */
  strcpy(gf->fmt[i],"%d");
  strcpy(gf->names[i],"lun"); i++;   /* File descriptor - Channel number   */
  strcpy(gf->fmt[i],"%8s");
  strcpy(gf->names[i],"ftyp"); i++;  /* File type.TFILE,CBYTE,STROT,DTROT  */
  strcpy(gf->fmt[i],"%88s");
  strcpy(gf->names[i],"path"); i++;  /* Name of the file for these globals */
  strcpy(gf->fmt[i],"%8s");
  strcpy(gf->names[i],"srun"); i++;  /* Sample rate units(ME,FE,SE,MS)     */
  gf->nkey = i;

  i = 0;
  gf->goff[i] = 0; i++;
  gf->goff[i] = (char *)(&g->nbycll) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->ntrcll)- (char *)g; i++;
  gf->goff[i] = (char *)(&g->grecsiz) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->ntb) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->numhc) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->nbydp) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->nbyhd) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->hdtyp) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->wdtyp) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->nhdwd) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->ndptr) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->srval) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->tstrt) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->xorg) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->yorg) - (char *)g; i++;
  gf->goff[i] = (char *)g->dx0  - (char *)g; i++;
  gf->goff[i] = (char *)g->dn0  - (char *)g; i++;
  gf->goff[i] = (char *)(&g->trmaxg)- (char *)g; i++;
  gf->goff[i] =  (char *)(&g->dunits) - (char *)g; i++;
  gf->goff[i] = (char *)(&g->lun) - (char *)g; i++;
  gf->goff[i] =  g->ftyp  - (char *)g; i++;
  gf->goff[i] =  g->path  - (char *)g; i++;
  gf->goff[i] =  g->srun  - (char *)g; i++;
  if(gf->nkey != i)
   {printf("tf_set_gdefs: bug\n");
    exit(1);
   }

 free(g);
 return (GlobalFmt *) gf;
}

/*
 * Dynamically allocate an io buffer */
char *tf_set_bsiz( int  newsiz)
{static  int  bsize;
 static char *iobuff;
  if(newsiz < BUFFSIZ) newsiz = BUFFSIZ;
  if(newsiz > bsize)
     { if(iobuff != NULL ) free(iobuff);
       iobuff = (char *) malloc(newsiz);
     }
  if(iobuff == NULL)
    { printf("tf_set_bsiz: error allocating buffer %d\n",newsiz);
      bsize = 0;
    }
  else  bsize = newsiz;
  BSIZE = bsize;
 return (char *) iobuff;
}

/*************************************************************************
**                 New CPS trcio file writing                           **
**                 Michael L. Sherrill 10/2000                          **
*************************************************************************/

int tf_trcio_create_file_(char *filename, char *mode, int scratch, int nwih,
                         int nsamp, int nbits_trace, int nbits_header, 
                         float dt, float tmin, float tmax, float trmaxg,
                         int *lun)
{
int error = 1;
int stat;

  trciof77wrapper_create_file_(filename, mode, &scratch, &nwih, &nsamp, 
                               &nbits_trace, &nbits_header, &dt, 
                               &tmin, &tmax, &trmaxg, lun, &stat);

  if(stat != 0) 
    return error;
  else
    return (error = 0);

}


int tf_trcio_write_trace_(int lun, float *hd, float *tr, int hd_size,
                          int tr_size )
{
int i, stat;
int error   = 1;
double *dhd;

  dhd = (double *) calloc(1,hd_size*sizeof(double));
  for(i = 0; i < hd_size; i++)
     dhd[i] = (double)hd[i];

  trciof77wrapper_write_trace_(&lun, dhd, tr, &hd_size, &tr_size, &stat);

  free(dhd);

  if(stat != 0) 
    return error;
  else
    return (error = 0);
  
}


void tf_trcio_close_file_(int lun, int *istat)
{

  trciof77wrapper_close_file_(&lun, istat);
}
