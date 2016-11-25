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
C      stat_data.c
c        1         2         3         4         5         6         7  |
c23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C             written in C -- designed to be called from C
C
C     Utility Name:  stat_data         (static file utilities)
C          Written:  93/09/08  by:  Tom Stoeckley
C     Last revised:  97/12/03  by:  Tom Stoeckley
C
C  Purpose:       To maintain static file information in a hidden
C                 structure accessable through public functions.
C                 Also to read and write CPS static files to and
C                 from the hidden structure.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/cprim   (shared)
C  library:                cprim.a            (shared)
C  header file:            cprim.h            (shared)
C  source file:            stat_data.c
C
C  documented functions:
C     stat_open               stat_head_get         stat_shot_get
C     stat_close              stat_head_put         stat_shot_put
C     stat_get_picks_spws          stat_head_grab3       stat_shot_getd
C     stat_put_picks_spws          stat_head_fill3       stat_shot_putd
C     stat_inquire            stat_head_fill        stat_shot_fill
C     stat_abort              stat_head_fill2       stat_shot_fillh
C                             stat_head_grab        stat_shot_grab
C                             stat_head_grab2       stat_shot_fill1
C
C     stat_get_prev_picks_spws     stat_get_next_picks_spws   stat_get_sel_picks_spws
C     stat_get_nhx    stat_get_nhy   stat_get_nhx2   stat_get_nhy2
C     stat_get_x1     stat_get_y1    stat_get_xinc   stat_get_yinc
C     stat_get_nx     stat_get_ny    stat_get_xend   stat_get_yend
C     stat_tail_get           stat_tail_put         stat_get_latest
C     stat_check_validity     
C     stat_check_validities   
C     stat_get_nearest_xbin
C     stat_get_nearest_ybin
C
C  documented functions added 12/3/97:
C
C     stat_bad_validation        stat_get_time_string
C     stat_validate     
C     stat_get_program_pointer   stat_get_type_pointer
C     stat_get_card_pointer      stat_replace_card
C     stat_set_nhx   stat_set_nhx2   stat_set_x1   stat_set_xinc
C     stat_set_nhy   stat_set_nhy2   stat_set_y1   stat_set_yinc
C     stat_set_nx    stat_set_xend
C     stat_set_ny    stat_set_yend
C     stat_get_nearest_ix        stat_get_nearest_iy
C     stat_get_nearby_xbin       stat_get_nearby_ybin
C
C  static functions:
C     get_prev_ybin        copy_statfile         create_statfile
C     get_next_ybin        read_statfile
C     get_position         set_position          working
C     move_picks           find_match            check_headsize
C     head_check           search_position       update_limits
C     unconstrained_ix     unconstrained_iy
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES
C         (this utility does not reference X, Xt, and Motif)
C              (standard C references not listed here)
C
C  libraries:     cprim.a
C  header files:  cprim.h
C
C  functions:
C          find_iarray_direction          find_iarray_match
C          inquire_files_combo
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  4. 97/12/03  Stoeckley  Added several new functions (listed above).
C  3. 95/11/08  Stoeckley  Changed to check for error on
C                           stat_clear_values.
C  2. 94/10/25  Stoeckley  Changed all checks on returned value from
C                           fprintf to test for <0 (rather than <=0)
C                           for error.  POSIX says the number of chars
C                           written should be returned, whereas now
C                           (using gcc, rather than cc -YPOSIX), zero
C                           is returned upon success.
C  1. 93/09/08  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C    Note that to make this stuff work most easily in cbyt,
C    the following variables should appear externally like longs
C    instead of floats:
C         x1  y1  xinc  yinc  xend  yend
C    If the above are longs, the following should be too:
C         xbin  ybin  xgp  ygp  xgps  ygps
C    If the above are longs, the following are not needed:
C         xbin  ybin
C-----------------------------------------------------------------------
C                ARGUMENTS IN FUNCTION CALLS
C
C  StatStruct  *ss = pointer to opaque static file structure.
C  char *filename or *filename1 = name of input static file.
C  char *filename or *filename2 = name of output static file.
C  long  required1 = TRUE if input file is required, FALSE otherwise.
C  long  required2 = TRUE if output file is required, FALSE otherwise.
C
C  char *type = type of static file (dimensioned 9 including null).
c  long   nhx = header word used to define the static X ground position.
c  long   nhy = header word used to define the static Y ground position.
c  long  nhx2 = second X header word for source=receiver file.
c  long  nhy2 = second Y header word for source=receiver file.
c  float   x1 = X ground position of first static in the file.
c  float   y1 = Y ground position of first static in the file.
c  float xinc = increment between X ground positions (non-zero).
c  float yinc = increment between Y ground positions (non-zero).
c  float xend = X ground position of last static in the file.
c  float yend = Y ground position of last static in the file.
C
C
c  float xwidth = width of bin for determining X ground position match.
c  float ywidth = width of bin for determining Y ground position match.
C         (these need not be the same as xinc and yinc)
C
c  float  xbin = a single X ground position.
c  float  ybin = a single Y ground position.
C
c  long    nx  = number of ground positions in X direction.
c  long    ny  = number of ground positions in Y direction (=one for 2-D).
c  long ncards = number of comment cards.
C
C  float *xbins = array of X ground positions (dimensioned nx).
C  float *ybins = array of Y ground positions (dimensioned ny).
C
C  float    value = a single static value.
C  float  *values = array of static values (dimensioned nx*ny).
C  float *pointer = pointer to array of static values.
C  char     *card = a comment card (dimensioned 81 including null).
C
C  long     ivar = integer variable saved on a comment card.
C  float    fvar = float   variable saved on a comment card.
C  double   dvar = double  variable saved on a comment card.
C  char    *cvar = string  variable saved on a comment card.
C  char   *ident = identification for variable saved on comment card.
C  char *comment = comment        for variable saved on comment card.
C
C  long    ix = desired X location (1 thru nx).
C  long    iy = desired Y location (1 thru ny).
C  float   fx = desired X location (exact 1 thru nx).
C  float   fy = desired Y location (exact 1 thru ny).
C  long icard = desired comment card number (1 thru ncard).
C
C  int   error = returned error (zero if no error, non-zero if error).
C  long status = returned status (see specific documentation below).
C  char   *msg = returned message corresponding with returned status.
C
C  All char* types must be null-terminated.
C  The filename must include the extension (if it has one), and may
C    optionally include a path.
C  The X-coordinate changes fastest in the array of static values.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each argument is flagged as
C  follows:  i = value required upon INPUT to the routine.
C            o = value set by the routine upon OUTPUT.
C            b = value BOTH required upon input and changed upon output.
C
C  For pointers, the flag (i,o,b) refers to the contents pointed to 
C  by the pointer, not to the value of the pointer itself.  The pointer
C  value is required upon INPUT in all cases.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  To create, clear, or destroy the data structure:
C
C                  ss = stat_create ()
C                       stat_clear  (ss)
C                  ss = stat_destroy(ss)
C                                    i
C
C  The entire structure is set to zero or blank by stat_create and
C    by stat_clear (except for type, which is set to "MISC").
C  A NULL is returned from stat_create if unsuccessful.
C  A NULL is always returned from stat_destroy.
C  Memory pointed to by pointers in the structure is freed by stat_clear
C    and stat_destroy.  But if the user supplied a pointer to static
C    values (using the function stat_set_pointer), that pointer will
C    not be freed, but this utility will no longer have access to
C    the pointer.
C-----------------------------------------------------------------------
C  To get various items from the data structure:
C
C                                       i    o
C                       stat_get_type  (ss, type)
C               ncard = stat_get_ncard (ss)
C
C               nhx   = stat_get_nhx   (ss)
C               nhy   = stat_get_nhy   (ss)
C               nhx2  = stat_get_nhx2  (ss)
C               nhy2  = stat_get_nhy2  (ss)
C               x1    = stat_get_x1    (ss)
C               y1    = stat_get_y1    (ss)
C               xinc  = stat_get_xinc  (ss)
C               yinc  = stat_get_yinc  (ss)
C               nx    = stat_get_nx    (ss)
C               ny    = stat_get_ny    (ss)
C
C    ?? the above and/or the following ??
C
C                                   i     o     o     o     o     o
C       error = stat_get_xbins     (ss, &nhx, &nhx2, &x1, &xinc, &nx)
C       error = stat_get_ybins     (ss, &nhy, &nhy2, &y1, &yinc, &ny)
C
C                                   i     o     o      o     o
C       error = stat_get_xbin_array(ss, &nhx, &nhx2, xbins, &nx)
C       error = stat_get_ybin_array(ss, &nhy, &nhy2, ybins, &ny)
C
C    ?? the above or the following ??
C                                           i     o
C               error = stat_get_xbin_array(ss, xbins)
C               error = stat_get_ybin_array(ss, ybins)
C
C                                       i   i     o
C               error = stat_get_xbin  (ss, ix, &xbin)
C               error = stat_get_ybin  (ss, iy, &ybin)
C
C                                       i     o     i
C               error = stat_get_card  (ss, card, icard)
C
C                                       i     i      o      o
C               error = stat_get_ivar  (ss, ident, &ivar)
C               error = stat_get_fvar  (ss, ident, &fvar)
C               error = stat_get_dvar  (ss, ident, &dvar)
C               error = stat_get_cvar  (ss, ident,  cvar)
C               error = stat_get_ivar2 (ss, ident, &ivar, &ivar2)
C               error = stat_get_fvar2 (ss, ident, &fvar, &fvar2)
C               error = stat_get_dvar2 (ss, ident, &dvar, &dvar2)
C               error = stat_get_cvar2 (ss, ident,  cvar,  cvar2)
C
C                                    i    i     i      i       i
C   value = stat_get_value          (ss, ix,   iy)
C   value = stat_get_nearest_value  (ss, xbin, ybin)
C   value = stat_get_terp_value     (ss, xbin, ybin)
C   value = stat_get_matching_value (ss, xbin, ybin, xwidth, ywidth)
C
C                                        i     o
C               error = stat_get_values (ss, values)
C             pointer = stat_get_pointer(ss)
C
C                                       i    i
C         ix = stat_get_nearest_xindex (ss, xbin)
C         iy = stat_get_nearest_yindex (ss, ybin)
C         fx = stat_get_terp_xindex    (ss, xbin)
C         fy = stat_get_terp_yindex    (ss, ybin)
C         ix = stat_get_matching_xindex(ss, xbin)
C         iy = stat_get_matching_yindex(ss, ybin)
C
C  The routines which return xinc or yinc return zero if the
C    ground position spacing is uneven.  Otherwise, zero is never
C    a permitted value for xinc or yinc.
C  The routines which get xbin or ybin arrays will copy existing arrays
C    if the ground position spacing is uneven, or fill them from x1,
C    xinc, etc.
C  The routines which return a static value return a nil value (the
C    constant FNIL) if:
C    - ix,iy is out of range.
C    - nx or ny = 0.
C    - there is no match within xwidth and ywidth.
C    - the values array is NULL.
C    - xinc or yinc is zero, and there are no ground position arrays.
C  The routines which return ix,iy,fx,fy return zero if an error occurs.
C  NULL pointers in the argument lists of stat_get_xbins, stat_get_ybins,
C    stat_get_xbin_array, and stat_get_ybin_array are not set.
C-----------------------------------------------------------------------
C  To put various items into the data structure:
C
C                                    i    i
C    error = stat_set_type          (ss, type)
C
C                                    i    i    i
C    error = stat_set_xheads        (ss, nhx, nhx2)
C    error = stat_set_yheads        (ss, nhy, nhy2)
C
C                                    i   i    i    i
C    error = stat_set_xbins         (ss, x1, xinc, nx)
C    error = stat_set_ybins         (ss, y1, yinc, ny)
C
C                                    i     i    i
C    error = stat_set_xbin_array    (ss, xbins, nx)
C    error = stat_set_ybin_array    (ss, ybins, ny)
C
C                                    i    i
C    error = stat_add_card          (ss, card)
C    error = stat_remove_cards      (ss)
C
C                                    i     i     i      i      i
C    error = stat_set_ivar          (ss, ident, ivar, comment)
C    error = stat_set_fvar          (ss, ident, fvar, comment)
C    error = stat_set_dvar          (ss, ident, dvar, comment)
C    error = stat_set_cvar          (ss, ident, cvar, comment)
C    error = stat_set_ivar2         (ss, ident, ivar, ivar2, comment)
C    error = stat_set_fvar2         (ss, ident, fvar, fvar2, comment)
C    error = stat_set_dvar2         (ss, ident, dvar, dvar2, comment)
C    error = stat_set_cvar2         (ss, ident, cvar, cvar2, comment)
C
C                                    i     i
C    error = stat_set_values        (ss, values)
C    error = stat_set_pointer       (ss, pointer)
C
C                                    i    i     i      i      i       i
C    error = stat_set_value         (ss, ix,   iy,   value)
C    error = stat_set_matching_value(ss, xbin, ybin, value, xwidth, ywidth)
C
C  The xbins,ybins arrays must be monotonic.
C  When setting the xbins,ybins arrays:
C   - the corresponding xinc or yinc value is set to zero.
C   - the corresponding x1   or   y1 value is set to the first element.
C   - the corresponding xend or yend value is set to the last element.
C  The values nx,ny cannot be reset after the values array has been
C    allocated.
C-----------------------------------------------------------------------
C  To check the validity of a CPS static file:
C
C                                      i       o  
C      valid = stat_check_validity (filename, info,
C
C   &nhx, &nhy, &nhx2, &nhy2, &x1, &y1, &xinc, &yinc, &nx, &ny, &xend, &yend)
C     o     o     o      o     o    o     o      o     o    o     o      o
C
C  char *filename = name of CPS static file.
C  char     *info = information describing the file (blank unless valid = YES).
C  long     valid = whether the file is a valid file (YES or NO or MAYBE).
C
C  Returns valid = YES   if the file is a valid CPS static file.
C  Returns valid = NO    if the file is NOT a valid CPS static file.
C  Returns valid = MAYBE if the file cannot be opened for read.
C  Sets the header variables if YES is returned; otherwise sets them to zero.
C  The info argument can be NULL to prohibit return of the information.
C  See the documentation in inquire_file.c for more details.
C-----------------------------------------------------------------------
C  To check the validity of a pair of CPS static files:
C
C                                        i           i
C      void    stat_check_validities (filename1, filename2,
C
C         &valid1, &valid2, info1, info2, &same_datasets,
C            o        o       o      o          o
C
C   &nhx, &nhy, &nhx2, &nhy2, &x1, &y1, &xinc, &yinc, &nx, &ny, &xend, &yend)
C     o     o     o      o     o    o     o      o     o    o     o      o
C
C  char *filename1 = name of input  CPS static file.
C  char *filename2 = name of output CPS static file.
C  long     valid1 = whether the input  file is valid (YES or NO or MAYBE).
C  long     valid2 = whether the output file is valid (YES or NO or MAYBE).
C  char     *info1 = information describing the input  file.
C  char     *info2 = information describing the output file.
C  long same_datasets = TRUE if the two files appear to belong to the
C                         same datasets (i.e. ngrp and nch match).
C
C  Returns valid = YES   if the file is a valid CPS static file.
C  Returns valid = NO    if the file is NOT a valid CPS static file.
C  Returns valid = MAYBE if the file cannot be opened for read.
C  Sets the header variables if YES is returned; otherwise sets them to zero.
C  The info arguments can be NULL to prohibit return of the information.
C  See the documentation in inquire_file.c for more details.
C-----------------------------------------------------------------------
C  To inquire about a pair of CPS static files:
C
C                               i          i          i          i
C    status = stat_inquire (filename1, filename2, required1, required2,
C
C        msg1, msg2, msg3, &status1, &status2)
C         o     o     o       o         o
C
C  char *filename1 = name of input CPS static file.
C  char *filename2 = name of output CPS static file.
C  long  required1 = TRUE if the input file is required.
C  long  required2 = TRUE if the output file is required.
C  char *msg1 = message referring to the status of the input file.
C  char *msg2 = message referring to the status of the output file.
C  char *msg3 = message referring to the two files together.
C  long status1 = the status of the input file (see below).
C  long status2 = the status of the output file (see below).
C  long  status = the status of the two files together (see below).
C
C  status1 = FILE_BLANK, FILE_VALID, FILE_ERROR.
C  status2 = FILE_BLANK, FILE_VALID, FILE_ERROR, FILE_NOT_FOUND.
C  status  = FILE_CREATE, FILE_READ_ONLY, FILE_COPY, FILE_UPDATE, FILE_ERROR.
C
C  See the documentation in inquire_file.c for more details.
C-----------------------------------------------------------------------
C  To open a CPS static file:
C
C                       i          i 
C   ss = stat_open (filename1, filename2,
C
C        i    i    i     i    i   i    i     i    i   i
C       nhx, nhy, nhx2, nhy2, x1, y1, xinc, yinc, nx, ny,
C
C        i         i    
C       msgfun, msgdata,
C
C       status, msg, tf, tf_open, tf_read_header, tf_rewind, tf_close)
C         i      o   i      i           i             i         i
C
C  char  *filename1 = name of input CPS static file.
C  char  *filename2 = name of output CPS static file.
C  MsgFun   *msgfun = function to call for displaying working message.
C  void    *msgdata = pointer to data for passing to msgfun.
C  long      status = the status of the two files together (see below).
C  char        *msg = message referring to the two files together.
C  StatStruct   *ss = pointer to opaque static file data structure.
C  char         *tf = trace file name.
C  TFOpen       *tf_open        = function to open the trace file.
C  TFReadHeader *tf_read_header = function to read one trace header.
C  TFRewind     *tf_rewind      = function to rewind the trace file.
C  TFClose      *tf_close       = function to close the trace file.
C
C  The status must have been previously returned by a call to stat_inquire,
C    inquire_files, inquire_files_combo, or any other routine which
C    returns status = FILE_CREATE, FILE_READ_ONLY, FILE_COPY, FILE_UPDATE,
C    or FILE_ERROR.
C
C  The last five arguments are needed only if status = FILE_CREATE, and
C    may be NULL otherwise.  This routine will then read through the
C    specified trace file, using the specified routines, and create 
C    a static file to match (filled with zero-ed picks).  The prototypes
C    of the four functions which access the trace file must be:
C      void *tf_open       (char *tf, long *nwords, long *ntraces);
C      long  tf_read_header(void *glbl, float head[]);
C      long  tf_rewind     (void *glbl);
C      long  tf_close      (void *glbl);
C    Here, glbl = an opaque pointer returned by tf_open (NULL if open fails).
C       *nwords = number of words in a header (returned).
C      *ntraces = number of traces on a file (returned).
C        head[] = array of header words for one trace.
C    Each time tf_read_header is called, the next trace on the file is read.
C
C  msgfun has this prototype: void msgfun(void *msgdata, char *msg),
C    where msg is the message to display.  msgfun is called repeatedly
C    during time-consuming operations, and is called with a blank
C    message before returning.  msgfun and/or msgdata can be NULL.
C
C  NULL is returned if an error occurs.  An error message will be placed
C    into msg.
C-----------------------------------------------------------------------
C  To close a CPS static file:
C                              i
C          error = stat_close (ss)
C          void    stat_abort (ss)
C
C  StatStruct *ss = pointer to opaque static file data structure.
C  int      error = error return (zero if no error).
C
C  The file is closed and the opaque structure is freed.
C  If the file is read/write, stat_close will write the header record
C    before closing.
C-----------------------------------------------------------------------
C  To read or write the tail record in the CPS static file:
C                                 b  
C          error = stat_tail_get (ss)
C          error = stat_tail_put (ss)
C                                 i 
C
C  StatStruct *ss = pointer to opaque static file data structure.
C  int      error = error return (zero if no error).
C
C  The file is positioned at the end of normal data before read or write.
C  The latest group updated is moved into or out of the opaque structure.
C  The file is flushed after a write.
C-----------------------------------------------------------------------
C  To read or write picks associated with arbitrary traces:
C                               b    i      i        i       o
C        error = stat_get_picks_spws(ss, head, nwords, ntraces, picks)
C        error = stat_put_picks_spws(ss, head, nwords, ntraces, picks)
C                               b    i      i        i       i
C
C  StatStruct *ss = pointer to opaque static file data structure.
C  float   head[] = CPS trace headers (dimensioned nwords * ntraces).
C  long    nwords = number of words in each trace header.
C  long   ntraces = number of trace headers.
C  float  picks[] = pick times in seconds (dimensioned ntraces).
C  int      error = number of picks not transferred (zero if no error).
C
C  The headers can be any random mix of traces.
C  The file is written and flushed after each write.
C  When getting picks, a pick that is not in the file is set to
C    the constant MISSING, which is defined in cprim.h.
C-----------------------------------------------------------------------
C  To read previous picks associated with arbitrary traces:
C  To read   next   picks associated with arbitrary traces:
C  To read selected picks associated with arbitrary traces:
C
C  error = stat_get_prev_picks_spws(ss, head, nwords, ntraces, picks, att)
C  error = stat_get_next_picks_spws(ss, head, nwords, ntraces, picks, att)
C  error = stat_get_sel_picks_spws (ss, head, nwords, ntraces, picks, att, ybin)
C                              b    i      i        i       i     o    o
C
C  StatStruct *ss = pointer to opaque static file data structure.
C  float   head[] = CPS trace headers (dimensioned nwords * ntraces).
C  long    nwords = number of words in each trace header.
C  long   ntraces = number of trace headers.
C  float  picks[] = pick times in seconds (dimensioned ntraces).
C  long       att = attribute (CHANNEL, OFFSET, RECEIVER).
C  long      ybin = selected ybin.
C  int      error = number of picks not transferred (zero if no error).
C
C  These functions work like stat_get_picks_spws, except for the following
C    differences:
C  The returned picks correspond to the ybin:
C     prior to that in the trace headers for stat_get_prev_picks_spws.
C     after    that in the trace headers for stat_get_next_picks_spws.
C     given by ybin                      for stat_get_sel_picks_spws.
C  The returned picks are matched up by channel number, offset, or
C     receiver ground position, according to the value of att.
C-----------------------------------------------------------------------
C  To get various static file header values:
C  To get the latest  ybin which has been updated:
C  To get the nearest ybin in the file:
C
C       nhx     = stat_get_nhx         (ss)
C       nhy     = stat_get_nhy         (ss)
C       nhx2    = stat_get_nhx2        (ss)
C       nhy2    = stat_get_nhy2        (ss)
C       x1      = stat_get_x1          (ss)
C       y1      = stat_get_y1          (ss)
C       xinc    = stat_get_xinc        (ss)
C       yinc    = stat_get_yinc        (ss)
C       nx      = stat_get_nx          (ss)
C       ny      = stat_get_ny          (ss)
C       xend    = stat_get_xend        (ss)
C       yend    = stat_get_yend        (ss)
C       latest  = stat_get_latest      (ss)
C       nearest = stat_get_nearest_ybin(ss, ybin, adjustment)
C       nearest = stat_get_nearest_xbin(ss, xbin, adjustment)
C                                       i    i        i
C
C  StatStruct  *ss = pointer to opaque static file data structure.
C  long       ybin = ybin to find.
C  long adjustment = which nearest ybin to get (see below).
C  long     latest = the latest ybin which has been updated.
C  long    nearest = the nearest ybin in the file to ybin.
C
C  adjustment = zero returns nearest profile to igroup (>, =, or <).
C  adjustment = positive returns nearest profile >= igroup.
C  adjustment = negative returns nearest profile <= igroup.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                                NOTES
C
C  1. If the static file is a one-dimensional file, then only one header
C     word (arguments NHX, NHX2, NX, X1, and XINC) is to be used.  In
C     this case, you should set NHY=0, NHY2=0, NY=1, Y1=anything, and
C     YINC=nonzero.
C
C  2. The second set of header words (NHX2 and NHY2) is used only for
C     source=receiver static files.  Otherwise they must be set to zero.
C
C  3. The more commonly used values for TYPE are:
C                "DATUM"    = datum static file
C                "REFR"     = refraction static file
C                "RESID"    = residual static file
C
C  4. The one-dimensional index for  S(IX,IY)  is  S(IX+(NX-1)*IY).
C
C  5. A special static value is a nil value (-1.0E-30), given by the
C     constant FNIL (defined in named_constants.h).
C
C-----------------------------------------------------------------------
C                 NOTES COPIED FROM FORTRAN DOCUMENTATION
C
C  6. Static files are VAX-resident text files with the following
C     format:
C     First record:        (A8,4I4/4G14.6,2I7)  TYPE,NHX,NHY,NHX2,NHY2,
C                                               X1,Y1,XINC,YINC,NX,NY
C     Each comment card:        (A80)           CARD
C     Special card after all comment cards: (A80)  CARD (='+++END+++')
C     Static values written:    (I8,5G14.6)     S (NX*NY values)
C     Static values read:       (8X,5G14.6)     S (NX*NY values)
C     The field designated I8 contains the 1-dimensional index of the
C     first value of array S on that line.  This field is skipped while
C     reading the file.  It is intended as help to the user if he is
C     viewing or editing the static file with a general-purpose editor
C     such as EVV.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C\END DOC

C-----------------------------------------------------------------------
              ORIGINAL FORTRAN CODE (FOR REFERENCE)
C-----------------------------------------------------------------------

      SUBROUTINE STATRII (LFN,TYPE,NHX,NHY,NHX2,NHY2,
     $                        X1,Y1,XINC,YINC,NX,NY,*,NCARD)
C     THIS IS THE VAX VERSION OF THE SAME PRIMITIVE ON THE CRAY.
C     IT IS IDENTICAL TO THE CRAY VERSION EXCEPT FOR THE OMISSION OF
C        PRINT STATEMENTS.
C----------DIMENSION STATEMENTS.
      DIMENSION S(NX,NY),T(*)
      CHARACTER*8 TYPE
      CHARACTER*80 CARD(*),MSG,ENDFLAG
      DATA ENDFLAG/'+++END+++'/
C----------READ INITIAL INFORMATION.
      NCARD=-1
5     REWIND LFN
      READ (LFN,4000,ERR=992) TYPE,NHX,NHY,NHX2,NHY2,
     $                        X1,Y1,XINC,YINC,NX,NY
      IF (NCARD.GE.0) RETURN
C----------READ AND PRINT HISTORY CARDS.
10    NCARD=NCARD+1
      READ (LFN,5000) MSG
      IF (MSG.EQ.ENDFLAG) GO TO 5
      GO TO 10
C----------READ COMMENT CARDS.
      ENTRY STATRCC (LFN,CARD,NUMBER,*)
      DO 19 I=1,NUMBER
19    CARD(I)=' '
      DO 20 I=1,NUMBER
      READ (LFN,5000,ERR=993) CARD(I)
      IF (CARD(I).EQ.ENDFLAG) THEN
           BACKSPACE LFN
           RETURN 1
      END IF
20    CONTINUE
      RETURN
C----------READ STATIC VALUES.
      ENTRY STATREAD (LFN,NX,NY,S,*)
50    READ (LFN,5000,ERR=994) MSG
      IF (MSG.NE.ENDFLAG) GO TO 50
      READ (LFN,2000,ERR=995) S
      RETURN
C----------WRITE INITIAL INFORMATION.
      ENTRY STATWII (LFN,TYPE,NHX,NHY,NHX2,NHY2,
     $                   X1,Y1,XINC,YINC,NX,NY,*)
C----------WRITE FIRST CARD.
      REWIND LFN
      WRITE (LFN,4000,ERR=996) TYPE,NHX,NHY,NHX2,NHY2,
     $                         X1,Y1,XINC,YINC,NX,NY
      RETURN
C----------WRITE COMMENT CARDS.
      ENTRY STATWCC (LFN,CARD,NUMBER,*)
      DO 60 I=1,NUMBER
      IF (CARD(I).EQ.' ') GO TO 60
      WRITE (LFN,5000,ERR=997) CARD(I)
60    CONTINUE
      RETURN
C----------WRITE STATIC VALUES.
      ENTRY STATWRIT (LFN,NX,NY,T,*)
      WRITE (LFN,5000,ERR=998) ENDFLAG
      N=NX*NY
      DO 40 I=1,N,5
      I2=MIN0(I+4,N)
40    WRITE (LFN,1000,ERR=999) I,(T(J),J=I,I2)
      RETURN
C----------ERROR RETURN.
992   PRINT *, 'STATRII:  ERROR IN STATIC FILE READ 992 '
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
993   PRINT *, 'STATRII:  ERROR IN STATIC FILE READ 993'
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
994   PRINT *, 'STATREAD:  ERROR IN STATIC FILE READ 994'
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
995   PRINT *, 'STATREAD:  ERROR IN STATIC FILE READ 995'
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
996   PRINT *, 'STATWII:  ERROR IN STATIC FILE WRITE 996'
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
997   PRINT *, 'STATWCC:  ERROR IN STATIC FILE WRITE 997'
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
998   PRINT *, 'STATWRIT:  ERROR IN STATIC FILE WRITE 998'
      print 5002,lfn,lfn
      RETURN 1
C----------ERROR RETURN.
999   PRINT *, 'STATRII:  ERROR IN STATIC FILE READ OR WRITE'
      RETURN 1
C----------FORMAT STATEMENTS.
1000  FORMAT (I8,5G14.6)
2000  FORMAT (8X,5G14.0)
4000  FORMAT (A8,4I4/4G14.6,2I7)
5000  FORMAT (A80)
5002  format (' lfn=',i3,' or lfn=',a8)
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
*/



#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include "cprim.h"
#include "str.h"
#include "inquire.h"
#include "named_constants.h"


#define DEBUG FALSE
/*
#define DEBUG TRUE
*/



/*-------------- opaque data structure ---------------------------------*/
/*-------------- opaque data structure ---------------------------------*/
/*-------------- opaque data structure ---------------------------------*/
/*-------------- opaque data structure ---------------------------------*/
/*-------------- opaque data structure ---------------------------------*/
        /* do not keep read_only and latest in data structure */

struct _StatStruct
    {
    float *xbins;          /* pointer to array of xbins.           */
    float *ybins;          /* pointer to array of ybins.           */
    float *values;         /* pointer to array of static values.   */
    char  *cards;          /* pointer to array of comment cards.   */
    long   ncards;         /* actual number of comment cards       */
    long   ncards_alloc;   /* number of allocated comment cards    */
    long   user_pointer;   /* TRUE if values belongs to user       */
    char   program[60];    /* program name using this utility      */

    char   type[9];        /* type of static file (default "MISC") */
    long   nhx , nhy ;     /* x and y header word numbers          */
    long   nhx2, nhy2;     /* second x and y header word numbers   */
    float  x1  , y1  ;     /* first x and y bin centers            */
    float  xinc, yinc;     /* x and y bin increments               */
    long   nx  , ny  ;     /* number of x and y bins               */
    } ;



/*--------------------------- macros ----------------------------------*/
/*--------------------------- macros ----------------------------------*/
/*--------------------------- macros ----------------------------------*/
/*--------------------------- macros ----------------------------------*/
/*--------------------------- macros ----------------------------------*/
         /* icard must be within the range 1 thru ncards */
         /* ix    must be within the range 1 thru nx     */
         /* iy    must be within the range 1 thru ny     */
         /* ichar must be within the range 1 thru 81     */

/*
#define CINDEX(icard)      (  ((icard) - 1) * 81 )
#define SINDEX(ix,iy)      (  ((iy) - 1) * ss->nx + ((ix) - 1)  )
#define CARD(icard)        (  &ss->cards[CINDEX(icard)]  )
#define VALUE(ix,iy)       (  ss->values[SINDEX(ix,iy)]  )
#define CHAR(icard,ichar)  (  ss->cards[CINDEX(icard) + ichar - 1]  )
*/


/*------------- set program name -------------------*/
/*------------- set program name -------------------*/
/*------------- set program name -------------------*/
/*------------- set program name -------------------*/
/*------------- set program name -------------------*/
    /* this is not in the cprim.h header file */

void stat_set_program(StatStruct *ss, char *program2)
{
  string_copy(ss->program, 60, program2, 0);
}



/*----------- create or clear or destroy data structure ---------------*/
/*----------- create or clear or destroy data structure ---------------*/
/*----------- create or clear or destroy data structure ---------------*/
/*----------- create or clear or destroy data structure ---------------*/
/*----------- create or clear or destroy data structure ---------------*/

StatStruct *stat_create(void)
{
  StatStruct *ss;

  ss = (StatStruct*)calloc(1, sizeof(StatStruct));
  if(!ss) return NULL;
  stat_clear(ss);
  stat_set_program(ss, "program");
  return ss;
}


void stat_clear_arrays(StatStruct *ss)
{
  stat_remove_cards(ss);
  stat_free_pointer(ss);
  if(ss->xbins) { ss->x1 = 0.0; ss->xinc = 1.0; ss->nx = 1; }
  if(ss->ybins) { ss->y1 = 0.0; ss->yinc = 1.0; ss->ny = 1; }
  if(ss->xbins) free(ss->xbins);
  if(ss->ybins) free(ss->ybins);
  ss->xbins = NULL;
  ss->ybins = NULL;
  ss->user_pointer = FALSE;
}



void stat_clear(StatStruct *ss)
{
  strcpy(ss->type, "MISC");
  ss->nhx  = 7;
  ss->nhy  = 8;
  ss->nhx2 = 0;
  ss->nhy2 = 0;
  ss->x1   = 0.0;
  ss->y1   = 0.0;
  ss->xinc = 1.0;
  ss->yinc = 1.0;
  ss->nx   = 1;
  ss->ny   = 1;
  stat_clear_arrays(ss);
}



void stat_bad_validation(StatStruct *ss)
{
  stat_clear_arrays(ss);
  strcpy(ss->type, " ");
  ss->nhx    = INIL;
  ss->nhy    = INIL;
  ss->nhx2   = INIL;
  ss->nhy2   = INIL;
  ss->x1     = FNIL;
  ss->y1     = FNIL;
  ss->xinc   = FNIL;
  ss->yinc   = FNIL;
  ss->nx     = INIL;
  ss->ny     = INIL;
  ss->ncards = INIL;
}



StatStruct *stat_destroy(StatStruct *ss)
{
  if(!ss) return NULL;
  stat_clear_arrays(ss);
  free(ss);
  return NULL;
}



/*------------------- static functions --------------------------------*/
/*------------------- static functions --------------------------------*/
/*------------------- static functions --------------------------------*/
/*------------------- static functions --------------------------------*/
/*------------------- static functions --------------------------------*/


/*-------------- check integrity of static file values ----------------*/
      /* needs to be called right after reading header from file,
           before xinc or yinc are reset to zero when uneven arrays
           are used */
      /* needs to be called right before writing header to file,
           and after xinc or yinc are reset from zero when uneven arrays
           are used */

static int check_integrity(StatStruct *ss)
{
  int e = 0;
  if(ss->nhx  <=    0                                  ) e = 91;
  if(ss->nhy  <     0                                  ) e = 92;
  if(ss->nhx2 <     0                                  ) e = 93;
  if(ss->nhy2 <     0                                  ) e = 94;
  if(ss->nhy2 !=    0     && ss->nhy  ==       0       ) e = 95;
  if(ss->nhx  == ss->nhx2 || ss->nhx  ==    ss->nhy    ) e = 96;
  if(ss->nhy  == ss->nhy2 && ss->nhy  !=       0       ) e = 96;
  if(ss->nx   <=    0     || ss->ny   <=       0       ) e = 96;
  if(ss->xinc ==   0.0    || ss->yinc ==      0.0      ) e = 97;
  if(DEBUG && e) printf("check_integrity error %d\n", e);
  return e;
}




/*-------------- reallocate the values array --------------------------*/

static int reallocate_values(StatStruct *ss)
{
  float *new;
  long n;

  n = ss->nx * ss->ny * sizeof(float);
  if(ss->user_pointer) ss->values = NULL;
  ss->user_pointer = FALSE;
  if(ss->values) new = (float*)realloc(ss->values, n);
  else           new = (float*) malloc(            n);
  if(!new) return 1;
  ss->values = new;
  return 0;
}



/*
static int maybe_allocate_values_array(StatStruct *ss, float *values)
{
  int i, n;

  if(!ss->values)
       {
       n = ss->nx * ss->ny;
       ss->values = (float*)malloc(n * sizeof(float));
       if(!ss->values) return 1;
       for(i = 0; i < n; i++)
            {
            if(values) ss->values[i] = values[i];
            else       ss->values[i] = FNIL;
            }
       }
  return 0;
}
*/




/*---------- get various items from data structure --------------------*/
/*---------- get various items from data structure --------------------*/
/*---------- get various items from data structure --------------------*/
/*---------- get various items from data structure --------------------*/
/*---------- get various items from data structure --------------------*/

const char *stat_get_program_pointer  (StatStruct *ss) { return ss->program; }
const char *stat_get_type_pointer     (StatStruct *ss) { return ss->type; }

const char *stat_get_card_pointer (StatStruct *ss, long icard)
{
  long index = (icard - 1) * 81;

  if(icard < 1 || icard > ss->ncards) { return NULL; }
  return &ss->cards[index];
}


void  stat_get_type  (StatStruct *ss, char *type)
                                      { strcpy(type, ss->type); }

long  stat_get_nhx   (StatStruct *ss) { return ss->nhx;    }
long  stat_get_nhy   (StatStruct *ss) { return ss->nhy;    }
long  stat_get_nhx2  (StatStruct *ss) { return ss->nhx2;   }
long  stat_get_nhy2  (StatStruct *ss) { return ss->nhy2;   }
float stat_get_x1    (StatStruct *ss) { return ss->x1;     }
float stat_get_y1    (StatStruct *ss) { return ss->y1;     }
float stat_get_xinc  (StatStruct *ss) { return ss->xinc;   }
float stat_get_yinc  (StatStruct *ss) { return ss->yinc;   }
long  stat_get_nx    (StatStruct *ss) { return ss->nx;     }
long  stat_get_ny    (StatStruct *ss) { return ss->ny;     }
long  stat_get_ncards(StatStruct *ss) { return ss->ncards; }

float stat_get_xend  (StatStruct *ss)
                { if(ss->x1   == FNIL) return FNIL;
                  if(ss->nx   == INIL) return FNIL;
                  if(ss->xinc == FNIL) return FNIL;
                  return (ss->x1 + (ss->nx - 1) * ss->xinc); }

float stat_get_yend  (StatStruct *ss)
                { if(ss->y1   == FNIL) return FNIL;
                  if(ss->ny   == INIL) return FNIL;
                  if(ss->yinc == FNIL) return FNIL;
                  return (ss->y1 + (ss->ny - 1) * ss->yinc); }


int stat_get_card (StatStruct *ss, char *card, long icard)
{
  long index = (icard - 1) * 81;

  if(icard < 1 || icard > ss->ncards) { card[0] = '\0'; return 1; }
  strcpy(card, &ss->cards[index]);
  return 0;
}


float stat_get_value (StatStruct *ss, long ix, long iy)
{
  long index = (iy - 1) * ss->nx + (ix - 1);

  if(ss->values == NULL)    return FNIL;
  if(ix < 1 || ix > ss->nx) return FNIL;
  if(iy < 1 || iy > ss->ny) return FNIL;
  return ss->values[index];
}



float stat_get_terp_value (StatStruct *ss, float xbin, float ybin)
{
  float v1, v2, v3, v4, sky, sly, value;
  float almost_zero = 0.5;
  float nearly_one  = 1.0 - almost_zero;
  float x = (xbin - ss->x1) / ss->xinc;
  float y = (ybin - ss->y1) / ss->yinc;
  long kx = x;
  long ky = y;
  long lx = kx + 1;
  long ly = ky + 1;
  if(ss->values == NULL)    return FNIL;
  kx = ConstrainValue(kx, 0, ss->nx - 1);
  ky = ConstrainValue(ky, 0, ss->ny - 1);
  lx = ConstrainValue(lx, 0, ss->nx - 1);
  ly = ConstrainValue(ly, 0, ss->ny - 1);
  v1 = ss->values[ky * ss->nx + kx];
  v2 = ss->values[ky * ss->nx + lx];
  v3 = ss->values[ly * ss->nx + kx];
  v4 = ss->values[ly * ss->nx + lx];
  x = ConstrainValue(x, kx, lx);
  y = ConstrainValue(y, ky, ly);
  if(v2 == FNIL && x - kx < almost_zero) v2 = v1;
  if(v1 == FNIL && x - kx > nearly_one ) v1 = v2;
  if(v4 == FNIL && x - kx < almost_zero) v4 = v3;
  if(v3 == FNIL && x - kx > nearly_one ) v3 = v4;
  if(v3 == FNIL && y - ky < almost_zero) v3 = v1;
  if(v1 == FNIL && y - ky > nearly_one ) v1 = v3;
  if(v4 == FNIL && y - ky < almost_zero) v4 = v2;
  if(v2 == FNIL && y - ky > nearly_one ) v2 = v4;
  if(v1 == FNIL || v2 == FNIL ||
     v3 == FNIL || v4 == FNIL) return FNIL;
  sky = v1 + (x - kx) * (v2 - v1);
  sly = v3 + (x - kx) * (v4 - v3);
  value = sky + (y - ky) * (sly - sky);
  return value;
}


int stat_get_values(StatStruct *ss, float *values)
{
  int i, n;

  if(ss->values == NULL) return 1;
  if(values == NULL) return 1;
  n = ss->nx * ss->ny;
  for(i = 0; i < n; i++) { values[i] = ss->values[i]; }
  return 0;
}


float *stat_get_pointer(StatStruct *ss)
{
  return ss->values;
}



/*---------- put various items into data structure --------------------*/
/*---------- put various items into data structure --------------------*/
/*---------- put various items into data structure --------------------*/
/*---------- put various items into data structure --------------------*/
/*---------- put various items into data structure --------------------*/

void stat_set_type (StatStruct *ss, char *type)
{
  char temp[9];
  int len;

  string_copy(temp, 8, type, 8);
  len = str_remove_all_blanks(temp, temp);
  if(len == 0) strcpy      (ss->type, "MISC");
  else         str_to_upper(ss->type, temp);
}



void stat_set_nhx(StatStruct *ss, long nhx)
{
  if(nhx  >= 1) ss->nhx  = nhx ;
}


void stat_set_nhy(StatStruct *ss, long nhy)
{
  if(nhy  >= 0) ss->nhy  = nhy ;
  if(ss->nhy == 0) ss->nhy2 = 0;
}


void stat_set_nhx2(StatStruct *ss, long nhx2)
{
  if(nhx2 >= 0) ss->nhx2 = nhx2;
}


void stat_set_nhy2(StatStruct *ss, long nhy2)
{
  if(nhy2 >= 0) ss->nhy2 = nhy2;
  if(ss->nhy == 0) ss->nhy2 = 0;
}


void stat_set_x1(StatStruct *ss, float x1)
{
  if(ss->xbins) free(ss->xbins);
  ss->x1     = x1;
}


void stat_set_y1(StatStruct *ss, float y1)
{
  if(ss->ybins) free(ss->ybins);
  ss->y1     = y1;
}


void stat_set_xinc(StatStruct *ss, float xinc)
{
  if(ss->xbins) free(ss->xbins);
  ss->xinc   = AbsoluteValue(xinc);
  if(ss->xinc == 0.0) ss->xinc = 1.0;
}


void stat_set_yinc(StatStruct *ss, float yinc)
{
  if(ss->ybins) free(ss->ybins);
  ss->yinc   = AbsoluteValue(yinc);
  if(ss->yinc == 0.0) ss->yinc = 1.0;
}


void stat_set_nx(StatStruct *ss, long nx)
{
  if(ss->xbins) free(ss->xbins);
  if(!ss->values) ss->nx = MaximumValue(nx, 1);
}


void stat_set_ny(StatStruct *ss, long ny)
{
  if(ss->ybins) free(ss->ybins);
  if(!ss->values) ss->ny = MaximumValue(ny, 1);
}


void stat_set_xend(StatStruct *ss, float xend)
{
  long nx = 1.5 + (xend - ss->x1) / ss->xinc;
  stat_set_nx(ss, nx);
}


void stat_set_yend(StatStruct *ss, float yend)
{
  long ny = 1.5 + (yend - ss->y1) / ss->yinc;
  stat_set_ny(ss, ny);
}



void stat_set_xheads(StatStruct *ss, long nhx, long nhx2)
{
  if(nhx  >= 1) ss->nhx  = nhx ;
  if(nhx2 >= 0) ss->nhx2 = nhx2;
}



void stat_set_yheads(StatStruct *ss, long nhy, long nhy2)
{
  if(nhy  >= 0) ss->nhy  = nhy ;
  if(nhy2 >= 0) ss->nhy2 = nhy2;
  if(ss->nhy == 0) ss->nhy2 = 0;
}



void stat_set_xbins(StatStruct *ss, float x1, float xinc, long nx)
{
  if(ss->xbins) free(ss->xbins);
  ss->x1     = x1;
  ss->xinc   = AbsoluteValue(xinc);
  if(ss->xinc == 0.0) ss->xinc = 1.0;
  if(!ss->values) ss->nx = MaximumValue(nx, 1);
}



void stat_set_ybins(StatStruct *ss, float y1, float yinc, long ny)
{
  if(ss->ybins) free(ss->ybins);
  ss->y1     = y1;
  ss->yinc   = AbsoluteValue(yinc);
  if(ss->yinc == 0.0) ss->yinc = 1.0;
  if(!ss->values) ss->ny = MaximumValue(ny, 1);
}



void stat_set_xrange(StatStruct *ss, float x1, float xinc, float xend)
{
  long nx;

  xinc = AbsoluteValue(xinc);
  if(xinc == 0.0) xinc = 1.0;
  nx = 1.5 + (xend - x1) / xinc;
  stat_set_xbins(ss, x1, xinc, nx);
}
  


void stat_set_yrange(StatStruct *ss, float y1, float yinc, float yend)
{
  long ny;

  yinc = AbsoluteValue(yinc);
  if(yinc == 0.0) yinc = 1.0;
  ny = 1.5 + (yend - y1) / yinc;
  stat_set_ybins(ss, y1, yinc, ny);
}
  


int stat_set_xbin_array(StatStruct *ss, float *xbins, long nx)
{
  int i, xdir;
  float *new;

  if(ss->values && nx != ss->nx) return 1;
  if(nx <= 0)                    return 1;
  if(xbins[0] > xbins[nx-1]) xdir = -1;
  else                       xdir =  1;
  for(i = 1; i < nx; i++)
       {  if(xdir * xbins[i] <= xdir * xbins[i-1]) return 1; }
  if(ss->xbins) new = (float*)realloc(ss->xbins, nx * sizeof(float));
  else          new = (float*) malloc(           nx * sizeof(float));
  if(!new) return 1;
  ss->xbins = new;
  for(i = 0; i < nx; i++) { if(xdir == 1) ss->xbins[i] = xbins[i];
                            else          ss->xbins[i] = xbins[nx - i - 1]; }
  ss->x1     = 0.0;
  ss->xinc   = 1.0;
  ss->nx     = nx;
  return 0;
}



int stat_set_ybin_array(StatStruct *ss, float *ybins, long ny)
{
  int i, ydir;
  float *new;

  if(ss->values && ny != ss->ny) return 1;
  if(ny <= 0)                    return 1;
  if(ybins[0] > ybins[ny-1]) ydir = -1;
  else                       ydir =  1;
  for(i = 1; i < ny; i++)
       {  if(ydir * ybins[i] <= ydir * ybins[i-1]) return 1; }
  if(ss->ybins) new = (float*)realloc(ss->ybins, ny * sizeof(float));
  else          new = (float*) malloc(           ny * sizeof(float));
  if(!new) return 1;
  ss->ybins = new;
  for(i = 0; i < ny; i++) { if(ydir == 1) ss->ybins[i] = ybins[i];
                            else          ss->ybins[i] = ybins[ny - i - 1]; }
  ss->y1     = 0.0;
  ss->yinc   = 0.0;
  ss->ny     = ny;
  return 0;
}


void stat_remove_cards(StatStruct *ss)
{
  if(ss->cards) free(ss->cards);
  ss->cards  = NULL;
  ss->ncards = 0;
  ss->ncards_alloc = 0;
}


int stat_replace_card(StatStruct *ss, char *card, long icard)
{
  long index = (icard - 1) * 81;

  if(icard < 1 || icard > ss->ncards) return 1;
  string_copy(&ss->cards[index], 80, card, 80);
  return 0;
}


int stat_add_card(StatStruct *ss, char *card)
{
  char *new;
  long ncards_alloc, n, index;

  if(ss->ncards >= ss->ncards_alloc)
       {
       ncards_alloc = ss->ncards + 100;
       n = ncards_alloc * 81 * sizeof(char);
       if(ss->cards) new = (char*)realloc(ss->cards, n);
       else          new = (char*) malloc(           n);
       if(!new) return 1;
       ss->cards = new;
       ss->ncards_alloc = ncards_alloc;
       }
  ss->ncards++;
  index = (ss->ncards - 1) * 81;
  string_copy(&ss->cards[index], 80, card, 80);
  return 0;
}


static long search_for_ident(StatStruct *ss, char *start)
{
  long icard, len, index;

  len = strlen(start);
  for(icard = 1; icard <= ss->ncards; icard++)
       {
       index = (icard - 1) * 81;
       if(!memcmp(&ss->cards[index], start, len)) return icard;
       }
  return 0;
}


static int search_to_set_ident(StatStruct *ss, char *start, char *card)
{
  int e;
  long icard;

  icard = search_for_ident(ss, start);
  if(icard >= 1) e = stat_replace_card(ss, card, icard);
  else           e = stat_add_card    (ss, card);
  return e;
}


static int search_to_get_ident(StatStruct *ss, char *start, char *card)
{
  int e;
  long icard;

  icard = search_for_ident(ss, start);
  if(icard == 0) return 1;
  e = stat_get_card(ss, card, icard);
  return e;
}



int stat_set_ivar(StatStruct *ss, long ivar, char *ident, char *comment)
{
  int e;
  char start[100], card[200];

  sprintf(start, "###ivar %s:", ident);
  sprintf(card , "###ivar %s: %d   %s", ident, ivar, comment);
  e = search_to_set_ident(ss, start, card);
  return e;
}


int stat_set_fvar(StatStruct *ss, float fvar, char *ident, char *comment)
{
  int e;
  char start[100], card[200];

  sprintf(start, "###fvar %s:", ident);
  sprintf(card , "###fvar %s: %g   %s", ident, fvar, comment);
  e = search_to_set_ident(ss, start, card);
  return e;
}


int stat_set_dvar(StatStruct *ss, double dvar, char *ident, char *comment)
{
  int e;
  char start[100], card[200];

  sprintf(start, "###dvar %s:", ident);
  sprintf(card , "###dvar %s: %g   %s", ident, dvar, comment);
  e = search_to_set_ident(ss, start, card);
  return e;
}


int stat_set_cvar(StatStruct *ss, char *cvar, char *ident, char *comment)
{
  int e;
  char start[100], card[200];

  sprintf(start, "###cvar %s:", ident);
  sprintf(card , "###cvar %s: %s   %s", ident, cvar, comment);
  e = search_to_set_ident(ss, start, card);
  return e;
}


int stat_set_ivar2(StatStruct *ss, long ivar, long ivar2,
                                       char *ident, char *comment)
{
  int e;
  char start[100], card[200];

  sprintf(start, "###ivar2 %s:", ident);
  sprintf(card , "###ivar2 %s: %d %d   %s", ident, ivar, ivar2, comment);
  e = search_to_set_ident(ss, start, card);
  return e;
}


int stat_set_fvar2(StatStruct *ss, float fvar, float fvar2,
                                       char *ident, char *comment)
{
  int e;
  char start[100], card[200];

  sprintf(start, "###fvar2 %s:", ident);
  sprintf(card , "###fvar2 %s: %g %g   %s", ident, fvar, fvar2, comment);
  e = search_to_set_ident(ss, start, card);
  return e;
}


int stat_set_dvar2(StatStruct *ss, double dvar, double dvar2,
                                       char *ident, char *comment)
{
  int e;
  char start[100], card[200];

  sprintf(start, "###dvar2 %s:", ident);
  sprintf(card , "###dvar2 %s: %g %g   %s", ident, dvar, dvar2, comment);
  e = search_to_set_ident(ss, start, card);
  return e;
}


int stat_set_cvar2(StatStruct *ss, char *cvar, char *cvar2,
                                       char *ident, char *comment)
{
  int e;
  char start[100], card[200];

  sprintf(start, "###cvar2 %s:", ident);
  sprintf(card , "###cvar2 %s: %s %s   %s", ident, cvar, cvar2, comment);
  e = search_to_set_ident(ss, start, card);
  return e;
}


int stat_get_ivar(StatStruct *ss, long *ivar, char *ident)
{
  int e;
  char start[100], card[200];

  *ivar = 0;
  sprintf(start, "###ivar %s:", ident);
  e = search_to_get_ident(ss, start, card);
  if(e) return e;
  e = sscanf(card, "###ivar %*s %ld", ivar);
  if(e != 1) return 3;
  return 0;
}



int stat_get_fvar(StatStruct *ss, float *fvar, char *ident)
{
  int e;
  char start[100], card[200];

  *fvar = 0.0;
  sprintf(start, "###fvar %s:", ident);
  e = search_to_get_ident(ss, start, card);
  if(e) return e;
  e = sscanf(card, "###fvar %*s %g", fvar);
  if(e != 1) return 3;
  return 0;
}



int stat_get_dvar(StatStruct *ss, double *dvar, char *ident)
{
  int e;
  char start[100], card[200];

  *dvar = 0.0;
  sprintf(start, "###fvar %s:", ident);
  e = search_to_get_ident(ss, start, card);
  if(e) return e;
  e = sscanf(card, "###fvar %*s %lg", dvar);
  if(e != 1) return 3;
  return 0;
}



int stat_get_cvar(StatStruct *ss, char *cvar, char *ident)
{
  int e;
  char start[100], card[200];

  cvar[0] = '\0';
  sprintf(start, "###cvar %s:", ident);
  e = search_to_get_ident(ss, start, card);
  if(e) return e;
  e = sscanf(card, "###cvar %*s %s", cvar);
  if(e != 1) return 3;
  return 0;
}



int stat_get_ivar2(StatStruct *ss, long *ivar, long *ivar2, char *ident)
{
  int e;
  char start[100], card[200];

  *ivar  = 0;
  *ivar2 = 0;
  sprintf(start, "###ivar2 %s:", ident);
  e = search_to_get_ident(ss, start, card);
  if(e) return e;
  e = sscanf(card, "###ivar2 %*s %ld %ld", ivar, ivar2);
  if(e != 1) return 3;
  return 0;
}



int stat_get_fvar2(StatStruct *ss, float *fvar, float *fvar2, char *ident)
{
  int e;
  char start[100], card[200];

  *fvar  = 0.0;
  *fvar2 = 0.0;
  sprintf(start, "###fvar2 %s:", ident);
  e = search_to_get_ident(ss, start, card);
  if(e) return e;
  e = sscanf(card, "###fvar2 %*s %g %g", fvar, fvar2);
  if(e != 1) return 3;
  return 0;
}



int stat_get_dvar2(StatStruct *ss, double *dvar, double *dvar2, char *ident)
{
  int e;
  char start[100], card[200];

  *dvar  = 0.0;
  *dvar2 = 0.0;
  sprintf(start, "###dvar2 %s:", ident);
  e = search_to_get_ident(ss, start, card);
  if(e) return e;
  e = sscanf(card, "###dvar2 %*s %lg %lg", dvar, dvar2);
  if(e != 1) return 3;
  return 0;
}



int stat_get_cvar2(StatStruct *ss, char *cvar, char *cvar2, char *ident)
{
  int e;
  char start[100], card[200];

  cvar [0] = '\0';
  cvar2[0] = '\0';
  sprintf(start, "###cvar2 %s:", ident);
  e = search_to_get_ident(ss, start, card);
  if(e) return e;
  e = sscanf(card, "###cvar2 %*s %s %s", cvar, cvar2);
  if(e != 1) return 3;
  return 0;
}


int stat_set_nil_values(StatStruct *ss, long iy,
                                     float xbin1, float xbin2)
{
  int e;
  long ix;
  long ix1 = NearestInteger((xbin1 - ss->x1) / ss->xinc) + 1;
  long ix2 = NearestInteger((xbin2 - ss->x1) / ss->xinc) + 1;

  if(ix1 > ix2)
      {
      ix = ix1; ix1 = ix2; ix2 = ix;
      }
  for(ix = ix1; ix <= ix2; ix++)
      {
      e = stat_set_value(ss, ix, iy, FNIL);
      if(e) return e;
      }
  return 0;
}



int stat_set_graded_values(StatStruct *ss, long iy,
                  float xbin1, float xbin2, float value1, float value2)
{
  int e;
  float value, slope;
  long ix;
  long ix1 = NearestInteger((xbin1 - ss->x1) / ss->xinc) + 1;
  long ix2 = NearestInteger((xbin2 - ss->x1) / ss->xinc) + 1;

  if(value1 == FNIL || value2 == FNIL) return 1;
  if(ix1 == ix2)
      {
      value = 0.5 * (value1 + value2);
      e = stat_set_value(ss, ix1, iy, value);
      return e;
      }
  if(ix1 > ix2)
      {
         ix =    ix1;    ix1 =    ix2;    ix2 =    ix;
      value =  xbin1;  xbin1 =  xbin2;  xbin2 = value;
      value = value1; value1 = value2; value2 = value;
      }
/*
  value1 = modify this to bin center...
  value2 = modify this to bin center...
*/
  slope = (value2 - value1) / (xbin2 - xbin1);
  for(ix = ix1; ix <= ix2; ix++)
      {
      float xcenter = ss->x1 + (ix - 1) * ss->xinc;
      value = value1 + (xcenter - xbin1) * slope;
      e = stat_set_value(ss, ix, iy, value);
      if(e) return e;
      }
  return 0;
}



int stat_set_value(StatStruct *ss, long ix, long iy, float value)
{
  long index = (iy - 1) * ss->nx + (ix - 1);

  if(ix < 1 || ix > ss->nx) return 1;
  if(iy < 1 || iy > ss->ny) return 1;
  if(!ss->values)
      {
      int e = stat_clear_values(ss);
      if(e) return 1;
      }
  ss->values[index] = value;
  return 0;
}



int stat_set_matching_value (StatStruct *ss, float xbin, float ybin,
                               float value, float xwidth, float ywidth)
{
  long ix = 1, iy = 1, index;

/*
  error = get_match(ss->x1, ss->xinc, ss->nx, ss->xbins); if(error) return 1;
  error = get_match(ss->y1, ss->yinc, ss->ny, ss->ybins); if(error) return 1;
  put this stuff into the above static routine (get_match):
  if(ss->xbins)
     {
          here must find nearest value to xbin in array xbins,
          then test for match within bin xwidth to get ix (=1 thru nx),
          or return 0 if no match.
     }
  else

  if(ix < 1 || ix > ss->nx) return 1;
  if(iy < 1 || iy > ss->ny) return 1;
  error = maybe_allocate_values_array(ss, NULL);  if(error) return 1;
*/
  index = (iy - 1) * ss->nx + (ix - 1);
  ss->values[index] = value;
  return 0;
}




int stat_set_values(StatStruct *ss, float *values)
{
  int i, n, e;

  e = reallocate_values(ss);            if(e) return e;
  n = ss->nx * ss->ny;
  for(i = 0; i < n; i++)
       {
       if(values) ss->values[i] = values[i];
       else       ss->values[i] = FNIL;
       }
  return 0;
}



int stat_clear_values(StatStruct *ss)
{
  int e = stat_set_values(ss, NULL);
  return e;
}



void stat_free_pointer(StatStruct *ss)
{
  stat_set_pointer(ss, NULL);
}


void stat_set_pointer(StatStruct *ss, float *pointer)
{
  if(!ss->user_pointer && ss->values) free(ss->values);
  ss->values = pointer;
  ss->user_pointer = (pointer != NULL);
}




/*------------------- static functions --------------------------------*/
/*------------------- static functions --------------------------------*/
/*------------------- static functions --------------------------------*/
/*------------------- static functions --------------------------------*/
/*------------------- static functions --------------------------------*/


static long unconstrained_ix(StatStruct *ss, float xbin, long adjustment)
{
  float f;
  long i;

  f = (xbin - ss->x1) / ss->xinc;
  i = NearestInteger(f);
  if     (adjustment < 0 && f < i) i--;
  else if(adjustment > 0 && f > i) i++;
  return (i + 1);
}


static long unconstrained_iy(StatStruct *ss, float ybin, long adjustment)
{
  float f;
  long i;

  f = (ybin - ss->y1) / ss->yinc;
  i = NearestInteger(f);
  if     (adjustment < 0 && f < i) i--;
  else if(adjustment > 0 && f > i) i++;
  return (i + 1);
}




/*-------------------- convenience routines ---------------------------*/
/*-------------------- convenience routines ---------------------------*/
/*-------------------- convenience routines ---------------------------*/
/*-------------------- convenience routines ---------------------------*/
/*-------------------- convenience routines ---------------------------*/



long stat_get_nearest_ix(StatStruct *ss, float xbin, long adjustment)
{
  long ix = unconstrained_ix(ss, xbin, adjustment);
  ix = ConstrainValue(ix, 1, ss->nx);
  return ix;
}


long stat_get_nearest_iy(StatStruct *ss, float ybin, long adjustment)
{
  long iy = unconstrained_iy(ss, ybin, adjustment);
  iy = ConstrainValue(iy, 1, ss->ny);
  return iy;
}



float stat_get_nearest_xbin(StatStruct *ss, float xbin, long adjustment)
{
  float f;
  long i;

  f = (xbin - ss->x1) / ss->xinc;
  i = NearestInteger(f);
  if     (adjustment < 0 && f < i) i--;
  else if(adjustment > 0 && f > i) i++;
  i = ConstrainValue(i, 0, ss->nx - 1);
  return (ss->x1 + i * ss->xinc);
}


float stat_get_nearest_ybin(StatStruct *ss, float ybin, long adjustment)
{
  float f;
  long i;

  f = (ybin - ss->y1) / ss->yinc;
  i = NearestInteger(f);
  if     (adjustment < 0 && f < i) i--;
  else if(adjustment > 0 && f > i) i++;
  i = ConstrainValue(i, 0, ss->ny - 1);
  return (ss->y1 + i * ss->yinc);
}


float stat_get_nearby_xbin(StatStruct *ss, long ix)
{
  if(ss->nx == 0) return ss->x1;
  ix = ConstrainValue(ix, 1, ss->nx);
  return (ss->x1 + (ix - 1) * ss->xinc);
}


float stat_get_nearby_ybin(StatStruct *ss, long iy)
{
  if(ss->ny == 0) return ss->y1;
  iy = ConstrainValue(iy, 1, ss->ny);
  return (ss->y1 + (iy - 1) * ss->yinc);
}



#define MISSING_YBIN -999.9e22

/*
static float get_prev_ybin(StatStruct *ss, float ybin)
{
  long i;

  if(ss->ny == 0) return MISSING_YBIN;
  i = find_farray_nearest_match(ybin, ss->ybin, ss->ny,
                               ss->direction, 1);
  if(i == 0) return MISSING_YBIN;
  return ss->ybin[i - 1];
}


static float get_next_ybin(StatStruct *ss, float ybin)
{
  long i;

  if(ss->ny == 0) return MISSING_YBIN;
  i = find_farray_nearest_match(ybin, ss->ybin, ss->ny,
                               ss->direction, -1);
  if(i == ss->ny - 1) return MISSING_YBIN;
  return ss->ybin[i + 1];
}
*/


/*----------------- get xbin and ybin from header ----------------*/

      /*
          If type is "MUTE", and nhx = 6, and nhy = 46:
             - forces xbin negative if header 47 <  header 46.
             - forces xbin positive if header 47 >= header 46.
      */

static float get_xbin_from_header(StatStruct *ss, float *head)
{
  float xbin = head[ss->nhx - 1];
/*
  if(ss->nhx == 6 && ss->nhy == 46 && strings_equal(ss->type, "MUTE"))
       {
       if (head[46] < head[45]) xbin = -AbsoluteValue(xbin);
       else                     xbin =  AbsoluteValue(xbin);
       }
*/
  return xbin;
}


static float get_ybin_from_header(StatStruct *ss, float *head)
{
  if(ss->nhy != 0) return head[ss->nhy - 1];
  return 0.0;
}



/*----------------- get or put picks for cbyt -------------------------*/
       /* later change this to use access functions only */

#define CURR  1  /* get  current    picks */
#define PREV  2  /* get  previous   picks */
#define NEXT  3  /* get  next       picks */
#define SEL   4  /* get  selected   picks */
#define SAVE  5  /* save current    picks */

static int move_picks(StatStruct *ss, float head[], long nwords,
                     long ntraces, float picks[],
                     long att, int option, float ysel, long xy_switch)
{
  int i, error = ntraces;
  float xbin, ybin, xbin2, ybin2;
  long index, xindex, yindex;

  for(i = 0; i < ntraces; i++)
       {
       xbin = get_xbin_from_header(ss, &head[nwords * i]);
       ybin = get_ybin_from_header(ss, &head[nwords * i]);
/*
       xbin = head[nwords * i + ss->nhx - 1];
       if(ss->nhy != 0) ybin = head[nwords * i + ss->nhy - 1];
       else             ybin = 0.0;
*/
       if(xy_switch)
            {
            if     (option == PREV) xbin2 = xbin - ss->xinc;
            else if(option == NEXT) xbin2 = xbin + ss->xinc;
            else if(option == SEL ) xbin2 = ysel;
            else                    xbin2 = xbin;
            if     (att == XMATCH)   ybin2 = ybin;
            else                     ybin2 = ybin - xbin + xbin2;
            }
       else
            {
            if     (option == PREV) ybin2 = ybin - ss->yinc;
            else if(option == NEXT) ybin2 = ybin + ss->yinc;
            else if(option == SEL ) ybin2 = ysel;
            else                    ybin2 = ybin;
            if     (att == XMATCH)   xbin2 = xbin;
            else                     xbin2 = xbin - ybin + ybin2;
            }
/*
    maybe eventually:
       xindex = stat_get_matching_xindex(ss, xbin2, xwidth);
       yindex = stat_get_matching_yindex(ss, ybin2, ywidth);
*/
       xindex = NearestInteger( (xbin2 - ss->x1) / ss->xinc );
       yindex = NearestInteger( (ybin2 - ss->y1) / ss->yinc );
       if(ss->nhy == 0)
            {
            if(option == CURR || option == SAVE) yindex =  0;
            else                                 yindex = -1;
            }
       if(xindex >= 0 && xindex < ss->nx &&
          yindex >= 0 && yindex < ss->ny)
            {
            index = xindex + ss->nx * yindex;
            if(option == SAVE)
                 {
                 if(picks[i] == FNIL) ss->values[index] =          picks[i];
                 else                 ss->values[index] = 1000.0 * picks[i];
                 }
/*
            else if(strings_equal(ss->type, "MUTE"))
                 {
                 float value = stat_get_terp_value(ss, xbin2, ybin2);
                 if(value == FNIL) picks[i] =         value;
                 else              picks[i] = 0.001 * value;
                 }
*/
            else
                 {
                 if(ss->values[index] == FNIL)
                                       picks[i] =         ss->values[index];
                 else                  picks[i] = 0.001 * ss->values[index];
                 }
            error--;
            }
       else if(option != SAVE) picks[i] = MISSING;
       }
  return error;
}


/*----------------- get or put picks for cbyt -------------------------*/

/*****
  Note: The following mysterious compiler warnings are printed for
  the lines "return move_picks...":
                line 1825: warning: 0
                line 1834: warning: 0
                line 1843: warning: 0
                line 1852: warning: 0
*****/


int stat_get_picks_spws(StatStruct *ss, float head[], long nwords,
        long ntraces, float picks[])
{
  return
    move_picks(ss, head, nwords, ntraces, picks, XMATCH, CURR, 0.0, FALSE);
}



int stat_put_picks_spws(StatStruct *ss, float head[], long nwords,
        long ntraces, float picks[])
{
  return
    move_picks(ss, head, nwords, ntraces, picks, XMATCH, SAVE, 0.0, FALSE);
}



int stat_get_prev_picks_spws(StatStruct *ss, float head[], long nwords,
        long ntraces, float picks[], long att, long xy_switch)
{
  return
    move_picks(ss, head, nwords, ntraces, picks, att, PREV, 0.0, xy_switch);
}



int stat_get_next_picks_spws(StatStruct *ss, float head[], long nwords,
        long ntraces, float picks[], long att, long xy_switch)
{
  return
    move_picks(ss, head, nwords, ntraces, picks, att, NEXT, 0.0, xy_switch);
}



int stat_get_sel_picks_spws (StatStruct *ss, float head[], long nwords,
        long ntraces, float picks[], long att, long xy_switch, float ysel)
{
  return
    move_picks(ss, head, nwords, ntraces, picks, att, SEL, ysel, xy_switch);
}




/*------------- helper for working message ---------------------------*/

static void working(MsgFun *msgfun, void *msgdata,
                    long i, long n, char *phrase1, char *phrase2)
{
  char msg[200];

  if(msgfun && i == 10*(i/10))
       {
       sprintf(msg, "%s record %d of %d %s", phrase1, i+1, n, phrase2);
       msgfun(msgdata, msg);
       }
}




/*--------- fill in data structure from tracefile info ---------------*/

#define MAXW      65
#define ERROR(n)                                                       \
    {                                                                  \
    if(msg) sprintf(msg,                                               \
           "error %d trying to\ncreate static file\nfrom traces", n);  \
    if(glbl) tracefile_close(glbl);                                    \
    return (n);                                                        \
    }


int stat_create_from_tracefile(StatStruct *ss, MsgFun *msgfun,
      void *msgdata, char *msg, char *tracefile,
      TFOpen   *tracefile_open  , TFReadHeader *tracefile_read_header,
      TFRewind *tracefile_rewind, TFClose      *tracefile_close)
{
  long ntraces, nwords, e, i, j;
  float head[MAXW], xend=0.0, yend=0.0, headx, heady;
  void *glbl = NULL;
  int open_file = 1;

  if(!tracefile || !tracefile_open || !tracefile_read_header ||
     !tracefile_rewind || !tracefile_close)                       ERROR(401)
  glbl = tracefile_open(tracefile, &nwords, &ntraces);  if(!glbl) ERROR(402)
  if(ntraces <= 0)                                                ERROR(403)
  if(ss->nhx > nwords)                                   ERROR(451);
  if(ss->nhy > nwords)                                   ERROR(452);

/*
  heady = 0.0;
*/
  i = ss->nx = 0;
  while(i < ntraces && ss->nx == 0)
       {
       e = tracefile_read_header(glbl, head, open_file);if(e == -1) ERROR(405)
              /* e == -1 means a bad error has occurred */
              /* e == -2 means the first trace does not have hdr1 = 1 */
              /* e == nwords means all is ok */
       open_file = 0;
       headx = get_xbin_from_header(ss, head);
       heady = get_ybin_from_header(ss, head);
/*
                   headx = head[ss->nhx - 1];
       if(ss->nhy) heady = head[ss->nhy - 1];
*/
       if(i == 0)
            {
            ss->x1 = xend = headx;
            ss->y1 = yend = heady;
            }
       else if(heady != ss->y1)
            {
            ss->nx = i;
            }
       i++;
       }

  if(ss->nx == 0) ss->nx = ntraces;
  if(ss->nx <= 2)                                                  ERROR(406)
  ss->ny = ntraces / ss->nx;        if(ntraces != ss->nx * ss->ny) ERROR(407)
  e = tracefile_rewind(glbl);                                if(e) ERROR(412)
  for(i = 0; i < ss->ny; i++)
       {
       working(msgfun, msgdata, i, ss->ny, "creating", "on output file");
       for(j = 0; j < ss->nx; j++)
            {
            e = tracefile_read_header(glbl, head, open_file);if(e == -1) ERROR(413)
            headx = get_xbin_from_header(ss, head);
            heady = get_ybin_from_header(ss, head);
/*
                        headx = head[ss->nhx - 1];
            if(ss->nhy) heady = head[ss->nhy - 1];
*/
            ss->x1 = MinimumValue(ss->x1, headx);
            ss->y1 = MinimumValue(ss->y1, heady);
            xend   = MaximumValue(xend  , headx);
            yend   = MaximumValue(yend  , heady);
            }
       }
  ss->nx = 1.5 + (xend - ss->x1) / ss->xinc;
  ss->ny = 1.5 + (yend - ss->y1) / ss->yinc;
  tracefile_close(glbl);
  if(msg) strcpy(msg, " ");
  return 0;
}




/*-------------- prepare data for cbyt --------------------------------*/
/*-------------- prepare data for cbyt --------------------------------*/
/*-------------- prepare data for cbyt --------------------------------*/
/*-------------- prepare data for cbyt --------------------------------*/
/*-------------- prepare data for cbyt --------------------------------*/

int stat_prepare(StatStruct *ss, char *filename1, char *filename2,
       MsgFun *msgfun, void *msgdata, long status,
       char *msg, char *tracefile,
/*
       void *(*tracefile_open)(), long (*tracefile_read_header)(),
       long (*tracefile_rewind)(), long (*tracefile_close)())
*/
       TFOpen   *tracefile_open  , TFReadHeader *tracefile_read_header,
       TFRewind *tracefile_rewind, TFClose      *tracefile_close)
{
  long e;

  if(status == FILE_CREATE)
       {
       if(msgfun) msgfun(msgdata, "creating static file...");
       if(stat_get_nx(ss) == 1)
            {
            e = stat_create_from_tracefile(ss,
                 msgfun, msgdata, msg, tracefile,
                 tracefile_open, tracefile_read_header,
                 tracefile_rewind, tracefile_close);  if(e) goto error;
            }
       e = stat_clear_values(ss);
       if(e) { if(msgfun) msgfun(msgdata, "cannot allocate enough space");
               goto error; }
       e = stat_save_cps_file(ss, filename2, msg);    if(e) goto error;
       }
  else if(status == FILE_COPY)
       {
       if(msgfun) msgfun(msgdata, "copying static file...");
       e = stat_read_cps_file(ss, filename1, msg);    if(e) goto error;
       e = stat_save_cps_file(ss, filename2, msg);    if(e) goto error;
       }
  else if(status == FILE_READ_ONLY)
       {
       if(msgfun) msgfun(msgdata, "reading static file...");
       e = stat_read_cps_file(ss, filename1, msg);    if(e) goto error;
       }
  else if(status == FILE_UPDATE)
       {
       if(msgfun) msgfun(msgdata, "reading static file...");
       e = stat_read_cps_file(ss, filename1, msg);    if(e) goto error;
       }
  else
       {
       e = 777;  if(msg) strcpy(msg, "illegal status");     goto error;
       }
  if(msgfun) msgfun(msgdata, " ");
  return 0;

  error:
     if(msgfun) msgfun(msgdata, " ");
     stat_clear(ss);
     return e;         
}





/*--------------- lowest level static I/O routines --------------------*/
/*--------------- lowest level static I/O routines --------------------*/
/*--------------- lowest level static I/O routines --------------------*/
/*--------------- lowest level static I/O routines --------------------*/
/*--------------- lowest level static I/O routines --------------------*/



/*------------ read header from CPS static file -----------------------*/
               /* FORMAT (A8,4I4/4G14.6,2I7) */

static int cps_read_header(FILE *stream, StatStruct *ss)
{
  int e;

  e = fscanf(stream, "%s%ld%ld%ld%ld%f%f%f%f%ld%ld\n",
        ss->type, &ss->nhx, &ss->nhy, &ss->nhx2, &ss->nhy2,
        &ss->x1, &ss->y1, &ss->xinc, &ss->yinc, &ss->nx, &ss->ny);
  if(e ==  0) return -777;
  if(e != 11) return    e;
  return 0;
}




/*--------- read one comment card from CPS static file ----------------*/
                     /* FORMAT (A80) */

static int cps_read_card(FILE *stream, char *card)
{
  int e;

  e = fscanf(stream, "%[^\n]%*c", card);
  if(e == 0)
       {
       e = fscanf(stream, "%*c");
       card[0] = '\0';
       }
  if(e == EOF) return 1;
  return 0;
}




/*------ read one line of static values into CPS static file ----------*/
                   /* FORMAT (8X,5G14.6) */

static int cps_read_values(FILE *stream, long i, long n, float *values)
{
  long j;
  int e;

  e = fscanf(stream, "%*8[^\n]");     if(e != 0) return (i+1);
  if(i + 4 < n)
       {
       e = fscanf(stream, "%14f%14f%14f%14f%14f",
                    &values[i  ], &values[i+1], &values[i+2],
                    &values[i+3], &values[i+4]);
       if(e != 5) return (i+2);
       }
  else
       {
       for(j = i; j < n; j++)
            {
            e = fscanf(stream, "%14f", &values[j]);
            if(e != 1) return (i+3);
            }
       }
  e = fscanf(stream, "%*[^\n]");     if(e != 0) return (i+4);
  e = fscanf(stream, "%*c");         if(e != 0) return (i+5);
  return 0;
}




/*------------ save header into CPS static file -----------------------*/
               /* FORMAT (A8,4I4/4G14.6,2I7) */

static int cps_save_header(FILE *stream, StatStruct *ss)
{
  int e;

  e = fprintf(stream,
        "%-8s%4d%4d%4d%4d\n%#14.6g%#14.6g%#14.6g%#14.6g%7d%7d\n",
        ss->type, ss->nhx, ss->nhy, ss->nhx2, ss->nhy2,
        ss->x1, ss->y1, ss->xinc, ss->yinc, ss->nx, ss->ny);
  if(e < 0) return 1;
  return 0;
}





/*--------- save one comment card into CPS static file ----------------*/
                      /* FORMAT (A80) */

static int cps_save_card(FILE *stream, char *card)
{
  int e;

  e = fprintf(stream, "%s\n", card);       if(e < 0) return 1;
  return 0;
}




/*-------- save one line of static values into CPS static file --------*/
                   /* FORMAT (I8,5G14.6) */

static int cps_save_values(FILE *stream,
                                  long i, long n, long nx, float *values)
{
  long j, ix, iy;
  int e;

  iy = 1 + i / nx;
  ix = 1 + i - (iy - 1) * nx;
  if     (ix <= 9999 && iy <= 999) e = fprintf(stream, "%4d%4d", ix, iy);
  else if(ix <= 999 && iy <= 9999) e = fprintf(stream, "%3d%5d", ix, iy);
  else if(ix <= 99999 && iy <= 99) e = fprintf(stream, "%5d%3d", ix, iy);
  else if(ix <= 999999 && iy <= 9) e = fprintf(stream, "%6d%2d", ix, iy);
  else if(ix <= 99 && iy <= 99999) e = fprintf(stream, "%2d%6d", ix, iy);
  else if(ix <= 9 && iy <= 999999) e = fprintf(stream, "%1d%7d", ix, iy);
  else                             e = fprintf(stream, "%8d", i + 1);
  if(e < 0) return (i+1);
  if(i + 4 < n)
       {
       e = fprintf(stream, "%#14.6g%#14.6g%#14.6g%#14.6g%#14.6g\n",
                    values[i  ], values[i+1], values[i+2],
                    values[i+3], values[i+4]);
       if(e < 0) return (i+2);
       }
  else
       {
       for(j = i; j < n; j++)
            {
            e = fprintf(stream, "%#14.6g", values[j]);
            if(e < 0) return (i+3);
            }
       e = fprintf(stream, "\n");         if(e < 0) return (i+4);
       }
  return 0;
}


/*---------------- helper to get time string ---------------*/
    /* The returned pointer from ctime points to an area in
       static memory managed by ctime and other time-related
       functions referred to in the time.h header file.
       This pointer must not be deallocated. */

const char *stat_get_time_string(void)
{
  time_t tp;
  char *time_string;
  int length;

  tp = time(NULL);
  time_string = ctime(&tp);
  length = strlen(time_string);
  if(length > 0 && time_string[length - 1] == '\n')
                   time_string[length - 1] = '\0';
  return time_string;
}



/*------- documented routines which access CPS static files ----------*/
/*------- documented routines which access CPS static files ----------*/
/*------- documented routines which access CPS static files ----------*/
/*------- documented routines which access CPS static files ----------*/
/*------- documented routines which access CPS static files ----------*/


/*------------- save static file from data structure -----------------*/

#define SERR(EEE,SSS)                                              \
    {                                                              \
    if(msg) sprintf(msg,                                           \
           "error %d\ntrying to save\nstatic file %s", EEE, SSS);  \
    if(stream) fclose(stream);                                     \
    return 1;                                                      \
    }
                  

int stat_save_cps_file(StatStruct *ss, char *filename, char *msg)
{
  FILE *stream = NULL;
  int e;
  long i, n, nx, ny, ncards;
  char card[81];
  float *pointer;

  e = check_integrity(ss);                      if(e) SERR(e,"(bad header)")
  stream = fopen(filename, "w");   if(stream == NULL) SERR(1,"(bad open)")
  e = cps_save_header(stream, ss);              if(e) SERR(e,"header")
  ncards  = stat_get_ncards (ss);
  nx      = stat_get_nx     (ss);
  ny      = stat_get_ny     (ss);
  pointer = stat_get_pointer(ss);
  for(i = 1; i <= ncards; i++)
       {
       e = stat_get_card(ss, card, i);          if(e) SERR(-i,"cards")
       e = cps_save_card(stream, card);         if(e) SERR( i,"cards")
       }
  sprintf(card, "File saved by %s:  %s", ss->program, stat_get_time_string());
  e = cps_save_card(stream, card);              if(e) SERR(66,"cards")
  e = cps_save_card(stream, "+++END+++");       if(e) SERR(77,"cards")
  n = nx * ny;
  for(i = 0; i < n; i += 5)
       {
       e = cps_save_values(stream, i, n, nx, pointer); if(e) SERR(e,"values")
       }
  fclose(stream);
  if(msg) strcpy(msg, " ");
  return 0;
}



/*------------- read static file into data structure -----------------*/

#define RERR(EEE,SSS)                                            \
    {                                                            \
    if(msg) sprintf(msg,                                         \
         "error %d\ntrying to read\nstatic file %s", EEE, SSS);  \
    stat_clear(ss);                                              \
    if(stream) fclose(stream);                                   \
    return 1;                                                    \
    }
                  

int stat_read_cps_file(StatStruct *ss, char *filename, char *msg)
{
  FILE *stream = NULL;
  int e;
  long i = 0, n, nx, ny;
  float *pointer;
  char card[200];

  stream = fopen(filename, "r");   if(stream == NULL) RERR(1, "(bad open)")
  stat_clear(ss);
  e = cps_read_header  (stream, ss);    if(e) RERR(e,"header")
  e = check_integrity  (ss);            if(e) RERR(e,"(bad header)")
  e = reallocate_values(ss);            if(e) RERR(e,"(bad reallocate)")
  while(1)
       {
       i++;
       e = cps_read_card(stream, card);  if(e) RERR(i,"cards")
       str_remove_trailing_blanks(card, card);
       if(!strcmp(card, "+++END+++")) break;
       if(card[0])
            {
            e = stat_add_card(ss, card);     if(e) RERR(-i,"cards")
            }
       }
  sprintf(card, "File read  by %s:  %s", ss->program, stat_get_time_string());
  e = stat_add_card(ss, card);              if(e) RERR(66,"cards")
  nx      = stat_get_nx     (ss);
  ny      = stat_get_ny     (ss);
  pointer = stat_get_pointer(ss);
  n = nx * ny;
  for(i = 0; i < n; i +=5)
       {
       e = cps_read_values(stream, i, n, pointer);  if(e) RERR(e,"values")
       }
  fclose(stream);
  if(msg) strcpy(msg, " ");
  return 0;
}




/*------------------------- validate file ----------------------------*/

       /* stat_validate differs from stat_check_validity in
          these ways:  (1) the history cards are read;
          (2) stat_bad_validation is called. */

#define VERR2                           \
    {                                   \
    stat_bad_validation(ss);            \
    if(stream) fclose(stream);          \
    return INQUIRE_VALID_NO;            \
    }
         

long stat_validate(StatStruct *ss, char *filename, char *info)
{
  FILE *stream = NULL;
  int e;

  stat_clear(ss);
  if(filename[0] == '\0')                                   VERR2;
  stream = fopen(filename, "r");         if(stream == NULL) VERR2;
  e = cps_read_header(stream, ss);                    if(e) VERR2;
  e = check_integrity(ss);                            if(e) VERR2;
/******  the following is new 1/24/97 ******/
  while(1)
       {
       char card[200];
       e = cps_read_card(stream, card);               if(e) VERR2;
       str_remove_trailing_blanks(card, card);
       if(!strcmp(card, "+++END+++")) break;
       if(card[0])
            {
            e = stat_add_card(ss, card);              if(e) VERR2;
            }
       }
/******  the above is new 1/24/97 ******/
  if(info)
       {
       sprintf(info, "x and y headers %d %d  --  x and y bins %d %d",
                stat_get_nhx(ss), stat_get_nhy(ss),
                stat_get_nx (ss), stat_get_ny (ss));
       }
  fclose(stream);
  return INQUIRE_VALID_YES;
}



/*----------------- check validity of file ----------------------------*/

#define VERR                 \
    {                        \
    stat_clear(ss);          \
    fclose(stream);          \
    return INQUIRE_VALID_NO; \
    }
                  
                  
long stat_check_validity(StatStruct *ss, char *filename, char *info)
{
  FILE *stream;
  int e;

  stat_clear(ss);
  stream = fopen(filename, "r");   if(stream == NULL) return INQUIRE_VALID_NO;
  e = cps_read_header(stream, ss);                    if(e) VERR;
  e = check_integrity(ss);                            if(e) VERR;
  if(info)
       {
       sprintf(info, "x and y headers %d %d  --  x and y bins %d %d",
                stat_get_nhx(ss), stat_get_nhy(ss),
                stat_get_nx (ss), stat_get_ny (ss));
       }
  fclose(stream);
  return INQUIRE_VALID_YES;
}




/*----------------- check validity of both files ----------------------*/

void stat_check_validities(StatStruct *ss, char *filename1, char *filename2,
     long *valid1, long *valid2, char *info1, char *info2,
     long *same_datasets)
{
  long nhx, nhy, nx, ny;
  float x1, y1, xinc, yinc;

  *valid2 = stat_check_validity(ss, filename2, info2);
  nhx  = stat_get_nhx (ss);
  nhy  = stat_get_nhy (ss);
  nx   = stat_get_nx  (ss);
  ny   = stat_get_ny  (ss);
  x1   = stat_get_x1  (ss);
  y1   = stat_get_y1  (ss);
  xinc = stat_get_xinc(ss);
  yinc = stat_get_yinc(ss);
  *valid1 = stat_check_validity(ss, filename1, info1);
  *same_datasets = (  nhx  == stat_get_nhx (ss) &&
                      nhy  == stat_get_nhy (ss) &&
                      nx   == stat_get_nx  (ss) &&
                      ny   == stat_get_ny  (ss) &&
                      x1   == stat_get_x1  (ss) &&
                      y1   == stat_get_y1  (ss) &&
                      xinc == stat_get_xinc(ss) &&
                      yinc == stat_get_yinc(ss)   );
}



/*----------------- inquire -------------------------------------------*/

long stat_inquire(StatStruct *ss, char *filename1, char *filename2,
     long required1, long required2,
     char *msg1, char *msg2, char *msg3,
     long *status1, long *status2)
{
  long same_datasets, status, valid1, valid2;
  char info1[100], info2[100];
  static char filetype[] = "CPS static file";

  stat_check_validities(ss, filename1, filename2,
          &valid1, &valid2, info1, info2, &same_datasets);
  status = inquire_files_combo(filename1, filename2,
          filetype, required1, required2, valid1, valid2,
          info1, info2, same_datasets, status1, status2,
          msg1, msg2, msg3);
  return status;
}



/*------------- convenience routines which do not need structure -------*/
/*------------- convenience routines which do not need structure -------*/
/*------------- convenience routines which do not need structure -------*/
/*------------- convenience routines which do not need structure -------*/
/*------------- convenience routines which do not need structure -------*/
/*------------- convenience routines which do not need structure -------*/

/*
  increments[n] (input)  = static increments.
  values    [n] (output) = integrated static values with nils preserved.
*/

void stat_integrate(float *increments, long n, float *values)
{
  long i;
  values[0] = 0.0;
  for(i = 1; i < n; i++)
     {
     values[i] = values[i-1] + increments[i-1];
     if(increments[i-1] == FNIL) values[i-1] = FNIL;
     }
}



void stat_replace_nils(float *values, long n)
{
  long i, ia=0, ib, j;
/*----------------- find first non-nil value ------------------*/
  for(i = 0; i < n && values[i] == FNIL; i++) {}
  if(i == n) return;
  values[0] = values[i];
/*----------------- find last non-nil value ------------------*/
  for(i = n-1; i >= 0 && values[i] == FNIL; i--) {}
  if(i < 0) return;          /* <-- this return should not happen */
  values[n-1] = values[i];
/*----------------- do the work ------------------*/
  for(i = 1; i < n; i++)
       {
       if(values[i-1] != FNIL && values[i] == FNIL)
           {
           ia = i - 1;
           }
       else if(values[i-1] == FNIL && values[i] != FNIL)
           {
           ib = i;
           for(j = ia+1; j <= ib-1; j++)
               {
               values[j] = values[ia] + 
                         (j-ia) * (values[ib] - values[ia]) / (ib-ia);
               }
           }
       }
}


/*
    there must not be any nils in the values array:
*/

void stat_smooth(long nrun, float *values, long n, float *smooth)
{
  long i;
  for(i = 0; i < n; i++)
      {
      long mrun = nrun / 2;
      long j, ja, jb;
      float sum = 0.0;
      mrun = MinimumValue(mrun, n-i-1);
      mrun = MinimumValue(mrun, i);
      ja = MaximumValue  (i-mrun, 0);
      jb = ConstrainValue(i+mrun, ja, n-1);
      for(j = ja; j <= jb; j++)
          {
          sum += values[j];
          }
      smooth[i] = sum / (jb-ja+1);
      }
}





/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/

