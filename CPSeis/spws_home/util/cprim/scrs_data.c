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
C      scrs_data.c
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
C             written in C -- designed to be called from C
C
C     Utility Name:  scrs_data    (SCRS pickfile I/O utilities)
C          Written:  93/04/16  by:  Tom Stoeckley
C     Last revised:  99/11/30  by:  Ed Schmauch
C
C  Purpose:       To read and write SCRS pickfiles using C.
C                 Can use both sequential and random access.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C
C  machine            source code directory       library
C  -------            ---------------------       -------
C  pospsv (ultrix)    ~spws/util/cprim (shared)   cprim.a (shared)
C
C  source files       header files
C  ------------       ------------
C  scrs_data.c        cprim.h (shared)
C----------------------------------------------------------------------
C             DIFFERENCES BETWEEN CODE ON DIFFERENT MACHINES
C                                 n/a
C-----------------------------------------------------------------------
C                  ROUTINES ON SOURCE FILE scrs_data.c
C
C  Documented routines:
C     scrs_open               scrs_head_get         scrs_shot_get
C     scrs_close              scrs_head_put         scrs_shot_put
C     scrs_get_picks_spws          scrs_head_grab3       scrs_shot_getd
C     scrs_put_picks_spws          scrs_head_fill3       scrs_shot_putd
C     scrs_inquire            scrs_head_fill        scrs_shot_fill
C     scrs_abort              scrs_head_fill2       scrs_shot_fillh_spws
C                             scrs_head_grab        scrs_shot_grab
C                             scrs_head_grab2       scrs_shot_fill1_spws
C     scrs_get_prev_picks_spws     scrs_get_next_picks_spws   scrs_get_sel_picks_spws
C                             scrs_get_ngrp         scrs_get_nch
C     scrs_tail_get           scrs_tail_put         scrs_get_latest
C     scrs_check_validity     
C     scrs_check_validities   
C     scrs_get_first_profile
C     scrs_get_last_profile
C     scrs_get_nearest_profile
C
C  Undocumented static routines:
C     get_prev_profile     copy_pickfile         create_pickfile
C     create_pickfile_unequal
C     get_next_profile     read_pickfile         find_terp_match
C     get_position         set_position          working
C     move_picks           find_match            check_headsize
C     head_check           search_position       update_limits
C-----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C
C  Libraries:     cprim.a
C  Header files:  cprim.h  unistd.h
C-----------------------------------------------------------------------
C                 EXTERNALS REFERENCED BY THIS UTILITY
C
C   access       find_iarray_direction          find_iarray_match
C   inquire_files_combo
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  7. 02/06/19  Stoeckley  Add scrs_copy_from_foreign.
C  6. 99/11/30  Schmauch   Eliminate requirement that sequential trace number
C                            in trace file headers starts at one.  This allows
C                            a seismic survey to be broken into manageable
C                            pieces.
C  5. 99/11/30  Schmauch   When creating pickfile, always scan for maximum
C                            channels in a group.  This was requested after
C                            confusion caused by a trace file where the total
C                            traces was an exact multiple of the number of
C                            channels in the first group, but the number of
C                            channels in a group was not constant so the
C                            number of channels in the first group was not
C                            the maximum.
C  4. 97/02/11  Schmauch   Add static routine create_pickfile_unequal to
C                            create an SCRS pickfile from scratch when
C                            the groups have differing numbers of traces.
C  3. 94/11/04  Stoeckley  Add static routine find_terp_match, and add
C                            memory caching feature.
C  2. 94/02/17  Stoeckley  Add tests for slight non-matching in
C                            find_match.
C  1. 93/04/16  Stoeckley  Initial version.
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
C  To check the validity of an SCRS pickfile:
C
C                                      i       o     o     o
C      valid = scrs_check_validity (filename, info, ngrp, nch)
C
C  char *filename = name of SCRS pickfile.
C  char     *info = information describing the file (blank unless valid = YES).
C  long     *ngrp = number of groups on file.
C  long      *nch = number of channels in a group.
C  long     valid = whether the file is a valid file (YES or NO or MAYBE).
C
C  Returns valid = YES   if the file is a valid SCRS pickfile.
C  Returns valid = NO    if the file is NOT a valid SCRS pickfile.
C  Returns valid = MAYBE if the file cannot be opened for read.
C  Sets ngrp and nch if YES is returned; otherwise sets them to zero.
C  The info argument can be NULL to prohibit return of the information.
C  See the documentation in inquire_file.c for more details.
C-----------------------------------------------------------------------
C  To check the validity of a pair of SCRS pickfiles:
C
C                                        i           i
C      void    scrs_check_validities (filename1, filename2,
C
C         &valid1, &valid2, info1, info2, &same_datasets)
C            o        o       o      o          o
C
C  char *filename1 = name of input  SCRS pickfile.
C  char *filename2 = name of output SCRS pickfile.
C  long     valid1 = whether the input  file is valid (YES or NO or MAYBE).
C  long     valid2 = whether the output file is valid (YES or NO or MAYBE).
C  char     *info1 = information describing the input  file.
C  char     *info2 = information describing the output file.
C  long same_datasets = TRUE if the two files appear to belong to the
C                         same datasets (i.e. ngrp and nch match).
C
C  Returns valid = YES   if the file is a valid SCRS pickfile.
C  Returns valid = NO    if the file is NOT a valid SCRS pickfile.
C  Returns valid = MAYBE if the file cannot be opened for read.
C  The info arguments can be NULL to prohibit return of the information.
C  See the documentation in inquire_file.c for more details.
C-----------------------------------------------------------------------
C  To inquire about a pair of scrs pickfiles:
C
C                               i          i          i          i
C    status = scrs_inquire (filename1, filename2, required1, required2,
C
C        msg1, msg2, msg3, &status1, &status2)
C         o     o     o       o         o
C
C  char *filename1 = name of input scrs pickfile.
C  char *filename2 = name of output scrs pickfile.
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
C  To open an scrs pickfile:
C
C                       i          i        i        i       i       i
C   ss = scrs_open (filename1, filename2, msgfun, msgdata, typegp, status,
C
C       msg, tf, tf_open, tf_read_header, tf_rewind, tf_close, tf_pickhead)
C        o   i      i           i             i         i          i
C
C  char  *filename1 = name of input scrs pickfile.
C  char  *filename2 = name of output scrs pickfile.
C  MsgFun   *msgfun = function to call for displaying working message.
C  void    *msgdata = pointer to data for passing to msgfun.
C  long      typegp = type of ground positions (sequential or grid).
C  long      status = the status of the two files together (see below).
C  char        *msg = message referring to the two files.
C  ScrsStruct   *ss = pointer to opaque pickfile data structure.
C  char         *tf = trace file name.
C  TFOpen       *tf_open        = function to open the trace file.
C  TFReadHeader *tf_read_header = function to read one trace header.
C  TFRewind     *tf_rewind      = function to rewind the trace file.
C  TFClose      *tf_close       = function to close the trace file.
C  int           tf_pickhead    = header word containing refr statics picks.
C
C  The status must have been previously returned by a call to scrs_inquire,
C    inquire_files, inquire_files_combo, or any other routine which
C    returns status = FILE_CREATE, FILE_READ_ONLY, FILE_COPY, FILE_UPDATE,
C    or FILE_ERROR.
C
C  The last six arguments are needed only if status = FILE_CREATE, and
C    may be NULL otherwise.  This routine will then read through the
C    specified trace file, using the specified routines, and create 
C    a pickfile to match (filled with zero-ed picks unless tf_pickhead
C    is from 1 thru number of trace headers as a fortran-style index).
C    The prototypes of the four functions which access the trace file must be:
C      void *tf_open       (char *tf, long *nwords, long *ntraces);
C      long  tf_read_header(void *glbl, float head[]);
C      long  tf_rewind     (void *glbl);
C      long  tf_close      (void *glbl);
C    Here, glbl = an opaque pointer returned by tf_open (NULL if open fails).
C       *nwords = number of words in a header (returned).
C      *ntraces = number of traces on a file (returned).
C        head[] = array of header words for one trace.
C    Each time tf_read_header is called, the next trace on the file is read.
C    It is assumed that the trace file contains shot profiles, each with
C      the same number of traces.
C
C  msgfun has this prototype: void msgfun(void *msgdata, char *msg),
C    where msg is the message to display.  msgfun is called repeatedly
C    during time-consuming operations, and is called with a blank
C    message before returning.  msgfun and/or msgdata can be NULL.
C
C  Set typegp = SEQU for sequential ground positions (46,0 ,47,0 ).
C  Set typegp = GRID for   grid     ground positions (33,34,35,36).
C  SEQU and GRID are defined constants in cprim.h.
C
C  NULL is returned if an error occurs.  An error message will be placed
C    into msg.
C-----------------------------------------------------------------------
C  To copy a foreign columnar file to an scrs pickfile:
C
C     o                                 i          i       o
C   error = scrs_copy_from_foreign (filename1, filename2, msg)
C
C  const char *filename1 = name of foreign input file.
C  const char *filename2 = name of output scrs pickfile.
C  char       *msg       = error message.
C  int         error     = error flag (TRUE or FALSE).
C
C  This routine is not prototyped in cprim.h because it is not a generalized
C  routine.  This routine was written to convert a specific foreign file
C  for Mike Gurch, and therefore contains the following hard-wired assumptions:
C
C   (1) The foreign file contains these three columns:
C         source X coord      receiver X coord      pick time in seconds
C   (2) The elevations, hole depths, and uphole times are zero.
C   (2) The Y coordinates are zero.
C   (3) The  source  X and Y coordinate header words are 11 and 12.
C   (4) The receiver X and Y coordinate header words are 14 and 15.
C   (5) The data are arranged in sequential shot profile order.
C
C-----------------------------------------------------------------------
C  To close an scrs pickfile:
C                              i
C          error = scrs_close (ss)
C          void    scrs_abort (ss)
C
C  ScrsStruct *ss = pointer to opaque pickfile data structure.
C  int      error = error return (zero if no error).
C
C  The file is closed and the opaque structure is freed.
C  If the file is read/write, scrs_close will write the header record
C    before closing.
C-----------------------------------------------------------------------
C  To fill data into an scrs header record in the data structure:
C  To grab data from an scrs header record in the data structure:
C                            b    i      i     i     i    i     i
C    error = scrs_head_fill (ss, ngrp, incgp, rinc, nch, trsh, izc)
C    error = scrs_head_grab (ss,&ngrp,&incgp,&rinc,&nch,&trsh, izc)
C                            b    o      o     o     o    o     o
C
C  ScrsStruct *ss = pointer to opaque pickfile data structure.
C  long      ngrp = number of groups (shot profiles).
C  long     incgp = ground position increment.
C  float     rinc = receiver interval (feet or meters).
C  long       nch = number of channels in a shot profile.
C  float     trsh = threshhold amplitude for picking.
C  char    izc[8] = flag for zero-crossing search ("YES" or "NO").
C  int      error = error return (zero if no error).
C
C  The first routine defaults the following array-related variables:
C    nxpw,nypw,npwoff,ipkwhx,ipkwhy,xpw,ypw,pkwn,npkwn.
C    To specify these variables directly, call scrs_head_fill2 right
C    after calling scrs_head_fill.
C  The first routine initializes these limits:
C    itmax,mxxgp,mxygp,mnxgp,mnygp,mxo.
C-----------------------------------------------------------------------
C  To fill array data into an scrs header record in the data structure:
C  To grab array data from an scrs header record in the data structure:
C                             b    i      i     i       i       i
C    error = scrs_head_fill2 (ss, nxpw, nypw, npwoff, ipkwhx, ipkwhy,
C                                     xpw, ypw, pkwn)
C                                      i    i    i
C
C                             b    o      o     o        o      o
C    error = scrs_head_grab2 (ss,&nxpw,&nypw,&npwoff,&ipkwhx,&ipkwhy,
C                                     xpw, ypw, pkwn)
C                                      o    o    o
C
C  ScrsStruct *ss = pointer to opaque pickfile data structure.
C  long      nxpw = number of xpw values (1 thru 100).
C  long      nypw = number of ypw values (1 thru 100).
C  long    npwoff = number of pick window offsets (2 thru 6).
C  long    ipkwhx = header word # for pick window X locs (default 9).
C  long    ipkwhy = header word # for pick window Y locs (default 0).
C  float    xpw[] = pick window X locations.
C  float    ypw[] = pick window Y locations.
C  float   pkwn[] = pick window functions for each location (xpw,ypw).
C                     Each function consists of (offset,twin,bwin) for
C                     npwoff offsets at each location.  This product
C                     cannot exceed 1800: npwoff * 3 * nxpw * nypw.
C-----------------------------------------------------------------------
C  To fill limits data into an scrs header record in the data structure:
C  To grab limits data from an scrs header record in the data structure:
C                           b     i      i      i      i      i     i
C  error = scrs_head_fill3 (ss, itmax, mxxgp, mxygp, mnxgp, mnygp, mxo)
C  error = scrs_head_grab3 (ss,&itmax,&mxxgp,&mxygp,&mnxgp,&mnygp,&mxo)
C                           b     o      o      o      o      o     o
C
C  ScrsStruct *ss = pointer to opaque pickfile data structure.
C  long     itmax = maximum pick time on shot profile (milliseconds).
C  long     mxxgp = maximum X ground position.
C  long     mxygp = maximum Y ground position.
C  long     mnxgp = minimum X ground position.
C  long     mnygp = minimum Y ground position.
C  long       mxo = maximum offset (ground position units).
C
C  Normally, you do not need to call the first routine because these
C    variables are updated automatically when calling scrs_shot_fill 
C    or scrs_shot_fillh_spws.
C-----------------------------------------------------------------------
C  To fill data into an scrs shot record in the data structure:
C  To grab data from an scrs shot record in the data structure:
C                            b    i      i     i     i     i     i
C    error = scrs_shot_fill (ss, shot, picks, ixgp, iygp, ioff, elev)
C    error = scrs_shot_grab (ss, shot, picks, ixgp, iygp, ioff, elev)
C                            b    o      o     o     o     o     o
C
C  ScrsStruct   *ss = pointer to opaque pickfile data structure.
C  float   shot[10] = miscellaneous shot information (see header file).
C  float picks[nch] = pick times in seconds.
C  long   ixgp[nch] = receiver X ground positions (hwd 35 or 47).
C  long   iygp[nch] = receiver Y ground positions (hwd 36 or  0).
C  long   ioff[nch] = offset of trace (hwd 6) (feet or meters).
C  long   elev[nch] = receiver elevation (hwd 16).
C  int        error = error return (zero if no error).
C
C  The dimension of each of the last five arrays must be nch, which is
C    the number of channels in a shot profile.
C  Any of the arrays can be set to NULL; this prohibits movement of
C    data into or out of the array.
C  This routine also updates these limits in the header record in the
C    data structure: itmax,mxxgp,mxygp,mnxgp,mnygp,mxo.
C-----------------------------------------------------------------------
C  To fill an scrs shot record in the data structure using trace headers:
C                                    b    i      i       i  
C       error = scrs_shot_fillh_spws(ss, head, nwords, picks)
C
C  ScrsStruct         *ss = pointer to opaque pickfile data structure.
C  float head[nwords*nch] = CPS trace headers for all channels.
C  long            nwords = number of words in each trace header.
C  float       picks[nch] = pick times in seconds for all channels.
C  int              error = error return (zero if no error).
C
C  The headers must be a complete set from one shot profile, exactly
C    matching the shot record in the data structure.
C  The dimension of the two arrays must be controlled by nch, which is
C    the number of channels in a shot profile.
C  The picks[] array can be NULL, in which case zeroes are used.
C  This routine also updates these limits in the header record in the
C    data structure: itmax,mxxgp,mxygp,mnxgp,mnygp,mxo.
C-----------------------------------------------------------------------
C  To fill one channel of an scrs shot record in the data structure
C    using one trace header:
C                                    b    i      i      i     i 
C       error = scrs_shot_fill1_spws(ss, head, nwords, pick, ich)
C
C  ScrsStruct     *ss = pointer to opaque pickfile data structure.
C  float head[nwords] = CPS trace headers for one channel.
C  long        nwords = number of words in trace header.
C  float         pick = pick times in seconds for this channel.
C  long           ich = channel number (between 1 and number of channels).
C  int          error = error return (zero if no error).
C
C  This routine must be called sequentially, beginning with ich = 1
C    and ending with ich = nch (the total number of channels).
C  If ich = 1, this routine also fills the shot[] array.
C  If ich = nch, this routine also updates these limits in the header
C    record in the data structure: itmax,mxxgp,mxygp,mnxgp,mnygp,mxo.
C-----------------------------------------------------------------------
C  To read or write the header record in the scrs pickfile:
C                                 b  
C          error = scrs_head_get (ss)
C          error = scrs_head_put (ss)
C                                 i 
C
C  ScrsStruct *ss = pointer to opaque pickfile data structure.
C  int      error = error return (zero if no error).
C
C  A rewind is done prior to reading or writing the header record.
C  The header record is moved onto or out of the opaque structure.
C  The file is flushed after a write.
C-----------------------------------------------------------------------
C  To read or write the NEXT shot record in the scrs pickfile:
C                                 b  
C          error = scrs_shot_get (ss)
C          error = scrs_shot_put (ss)
C                                 i 
C
C  ScrsStruct *ss = pointer to opaque pickfile data structure.
C  int      error = error return (zero if no error).
C
C  No file positioning is done when reading or writing a shot record.
C  The shot record is moved into or out of the opaque structure.
C  No file flushing is done.
C-----------------------------------------------------------------------
C  To read or write the tail record in the scrs pickfile:
C                                 b  
C          error = scrs_tail_get (ss)
C          error = scrs_tail_put (ss)
C                                 i 
C
C  ScrsStruct *ss = pointer to opaque pickfile data structure.
C  int      error = error return (zero if no error).
C
C  The file is positioned at the end of normal data before read or write.
C  The latest group updated is moved into or out of the opaque structure.
C  The file is flushed after a write.
C-----------------------------------------------------------------------
C  To read or write a DESIRED shot record in the scrs pickfile:
C                                 b     i
C         error = scrs_shot_getd (ss, igroup)
C         error = scrs_shot_putd (ss, igroup)
C                                 i     i
C
C  ScrsStruct *ss = pointer to opaque pickfile data structure.
C  long    igroup = group number to read or write.
C  int      error = error return (zero if no error).
C
C  The file is positioned at the desired shot record prior to reading
C    or writing the shot record.
C  The shot record is moved onto or out of the opaque structure.
C  scrs_shot_putd calls scrs_tail_put.
C  The file is flushed after a write.
C-----------------------------------------------------------------------
C  To read or write picks associated with arbitrary traces:
C                               b    i      i        i       o
C        error = scrs_get_picks_spws(ss, head, nwords, ntraces, picks)
C        error = scrs_put_picks_spws(ss, head, nwords, ntraces, picks)
C                               b    i      i        i       i
C
C  ScrsStruct *ss = pointer to opaque pickfile data structure.
C  float   head[] = CPS trace headers (dimensioned nwords * ntraces).
C  long    nwords = number of words in each trace header.
C  long   ntraces = number of trace headers.
C  float  picks[] = pick times in seconds (dimensioned ntraces).
C  int      error = number of picks not transferred (zero if no error).
C
C  The headers can be any random mix of traces from portions of one
C    or more shot profiles, but the routines are more efficient if
C    traces from a single shot profile are adjacent.
C  The file is positioned at each of the required shot records prior
C    to reading or writing each shot record.
C  The shot record is moved into and out of the opaque structure as 
C    needed.
C  The file is flushed after each write.
C  When getting picks, a pick that is not in the file is set to
C    the constant SCRS_MISSING, which is defined in cprim.h.
C-----------------------------------------------------------------------
C  To read previous picks associated with arbitrary traces:
C  To read   next   picks associated with arbitrary traces:
C  To read selected picks associated with arbitrary traces:
C
C  error = scrs_get_prev_picks_spws(ss, head, nwords, ntraces, picks, att)
C  error = scrs_get_next_picks_spws(ss, head, nwords, ntraces, picks, att)
C  error = scrs_get_sel_picks_spws (ss, head, nwords, ntraces, picks, att, grp)
C                              b    i      i        i       i     o    o
C
C  ScrsStruct *ss = pointer to opaque pickfile data structure.
C  float   head[] = CPS trace headers (dimensioned nwords * ntraces).
C  long    nwords = number of words in each trace header.
C  long   ntraces = number of trace headers.
C  float  picks[] = pick times in seconds (dimensioned ntraces).
C  long       att = attribute (CHANNEL, OFFSET, RECEIVER).
C  long       grp = selected group (shot record).
C  int      error = number of picks not transferred (zero if no error).
C
C  These functions work like scrs_get_picks_spws, except for the following
C    differences:
C  The returned picks correspond to the group number:
C     prior to that in the trace headers for scrs_get_prev_picks_spws.
C     after    that in the trace headers for scrs_get_next_picks_spws.
C     given by grp                       for scrs_get_sel_picks_spws.
C  The returned picks are matched up by channel number, offset, or
C     receiver ground position, according to the value of att.
C-----------------------------------------------------------------------
C  To get the number of groups (shot profiles) in the file:
C  To get the number of channels in each group:
C  To get the latest  group (shot profile) which has been updated:
C  To get the first   group (shot profile) in the file:
C  To get the last    group (shot profile) in the file:
C  To get the nearest group (shot profile) in the file:
C
C       ngrp    = scrs_get_ngrp           (ss)
C       nch     = scrs_get_nch            (ss)
C       latest  = scrs_get_latest         (ss)
C       first   = scrs_get_first_profile  (ss)
C       last    = scrs_get_last_profile   (ss)
C       nearest = scrs_get_nearest_profile(ss, igroup, adjustment)
C                                          i     i         i
C
C  ScrsStruct  *ss = pointer to opaque pickfile data structure.
C  long     igroup = group number to find.
C  long adjustment = which nearest profile to get (see below).
C  long       ngrp = number of groups in the file.
C  long        nch = number of channels in each group.
C  long     latest = the latest group which has been updated.
C  long      first = the first   group in the file.
C  long       last = the last    group in the file.
C  long    nearest = the nearest group in the file to igroup.
C
C  adjustment = zero returns nearest profile to igroup (>, =, or <).
C  adjustment = positive returns nearest profile >= igroup.
C  adjustment = negative returns nearest profile <= igroup.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C\END DOC
*/


/*----------------------- header files --------------------------------*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <assert.h>
#include <errno.h>

#include "cprim.h"
#include "readcard.h"
#include "inquire.h"
#include "str.h"
#include "named_constants.h"


/*-------------- defined constants -------------------------------------*/

#define MAX_CACHE  400    /* maximum number of shot records in cache */
#define VERIFY     FALSE  /* whether to verify proper operation of cache */
#define CVPRINT    FALSE  /* whether to print whether found in cache */

#define LARGE     199999  /* largest value for ss->mnxgp and ss->mnygp */

#define DEBUG      FALSE  /* whether to print debug information */

#define NCHMAX    12000   /* maximum allowed value of nch  */
#define NGRPMAX   19999   /* maximum allowed value of ngrp */


/*-------------- opaque data structure ---------------------------------*/


struct _ScrsStruct
    {
    MemoryCache *cache;
    int  cache_update_flag;  /* whether to update memory cache */
    FILE *stream;
    int  read_only;         /* 1 if read only, 0 if read/write    */
    long igroup[NGRPMAX];   /* array of group numbers.            */
/*
#ifndef sun  (also VMS does not understand)
    fpos_t  pos[NGRPMAX+1]; / * corresponding position indicators. * /
#endif
*/
    long offset[NGRPMAX+1]; /* corresponding position indicators. */
    long direction;         /* direction of igroup[] (1, -1, 0).  */
    long typegp;       /* type of ground positions (SEQU or GRID) */
    long sxgp;         /* hwd # for  source  X gnd pos (33 or 46) */
    long sygp;         /* hwd # for  source  Y gnd pos (34 or  0) */
    long rxgp;         /* hwd # for receiver X gnd pos (35 or 47) */
    long rygp;         /* hwd # for receiver Y gnd pos (36 or  0) */
    long latest;  /* latest group updated (number in tail record) */

/*---header record---*/

    long  ngrp;      /* number of groups (shot profiles)              */
    long  itmax;     /* maximum pick time on shot profile (millisec)  */
    long  mxxgp;     /* maximum x ground position                     */
    long  mxygp;     /* maximum y ground position                     */
    long  mnxgp;     /* minimum x ground position                     */
    long  mnygp;     /* minimum y ground position                     */
    long  mxo;       /* maximum offset (ground position units)        */
    long  incgp;     /* ground position increment                     */
    float rinc;      /* receiver interval (feet or meters)            */
    long  nch;       /* number of channels in a shot profile          */
    float trsh;      /* threshhold amplitude for picking              */
    char  izc[8];    /* flag for zero-crossing search ("YES" or "NO") */
    long  nxpw;      /* number of xpw values (cannot exceed 100)      */
    long  nypw;      /* number of ypw values (cannot exceed 100)      */
    long  npwoff;    /* number of pick window offsets (2 thru 6)      */
    long  ipkwhx;    /* hwd # for pick window x locs (default 9)      */
    long  ipkwhy;    /* hwd # for pick window y locs (default 0)      */

    float xpw  [100];    /* pick window x locations                   */
    float ypw  [100];    /* pick window y locations                   */
    float pkwn [1800];   /* pick window functions for each location   */
                         /* (XPW,YPW).  each function consists of     */
                         /* (offset,twin,bwin) for NPWOFF offsets     */
                         /* at each location.                         */

    long  npkwn;     /* number of pkwn values (cannot exceed 1800)    */
                     /* npkwn = npwoff * 3 * nxpw *nypw               */
                     /* npkwn is not in the header record itself      */

/*---shot record---*/

    float shot[10];   /* values relating to shot profile:              */
                      /*   shot[0] = source X gnd pos (hwd 33 or 46).  */
                      /*   shot[1] = hwd 20 (source depth).            */
                      /*   shot[2] = hwd 44 (source uphole time).      */
                      /*   shot[3] = hwd 13 (source elev) - hwd 20.    */
                      /*   shot[4] = apparently not used.              */
                      /*   shot[5] = apparently not used.              */
                      /*   shot[6] = apparently not used.              */
                      /*   shot[7] = source Y gnd pos (hwd 34 or 0).   */
                      /*   shot[8] = hwd 9 (shot profile number).      */
                      /*   shot[9] = apparently not used.              */
    long iarriv[NCHMAX]; /* pick time on trace (millisec).             */
    long ixgp  [NCHMAX]; /* receiver X gnd pos (hwd 35 or 47).         */
    long iygp  [NCHMAX]; /* receiver Y gnd pos (hwd 36 or  0).         */
    long ioff  [NCHMAX]; /* offset of trace (hwd 6) (feet or meters).  */
    long elev  [NCHMAX]; /* receiver elevation (hwd 16).               */

    } ;


/*---------------- opaque shot record structure --------------------*/


typedef struct _ShotRecord
    {
    float shot[10];
    long iarriv[NCHMAX];
    long ixgp  [NCHMAX];
    long iygp  [NCHMAX];
    long ioff  [NCHMAX];
    long elev  [NCHMAX];
    } ShotRecord;


/*-------------------- convenience routines ---------------------------*/

long scrs_get_ngrp  (ScrsStruct *ss) { return ss->ngrp; }
long scrs_get_nch   (ScrsStruct *ss) { return ss->nch; }
long scrs_get_latest(ScrsStruct *ss) { return ss->latest; }


long scrs_get_first_profile(ScrsStruct *ss)
{
  if(ss->ngrp == 0) return 0;
  return ss->igroup[0];
}


long scrs_get_last_profile(ScrsStruct *ss)
{
  if(ss->ngrp == 0) return 0;
  return ss->igroup[ss->ngrp-1];
}


long scrs_get_nearest_profile(ScrsStruct *ss, long igroup, long adjustment)
{
  long i;

  if(ss->ngrp == 0) return 0;
  i = find_iarray_nearest_match(igroup, ss->igroup, ss->ngrp,
                               ss->direction, adjustment);
  return ss->igroup[i];
}


#define MISSING_PROFILE -999

static long get_prev_profile(ScrsStruct *ss, long igroup)
{
  long i;

  if(ss->ngrp == 0) return MISSING_PROFILE;
  i = find_iarray_nearest_match(igroup, ss->igroup, ss->ngrp,
                               ss->direction, 1);
  if(i == 0) return MISSING_PROFILE;
  return ss->igroup[i - 1];
}


static long get_next_profile(ScrsStruct *ss, long igroup)
{
  long i;

  if(ss->ngrp == 0) return MISSING_PROFILE;
  i = find_iarray_nearest_match(igroup, ss->igroup, ss->ngrp,
                               ss->direction, -1);
  if(i == ss->ngrp - 1) return MISSING_PROFILE;
  return ss->igroup[i + 1];
}


/*-------------------- get current position in file -------------------*/
                          /* uses two methods */

static int get_position(ScrsStruct *ss, int i)
{
  int e;

  if(i < 0 || i > ss->ngrp)                            return 700;
/*
#ifndef sun  (also VMS does not understand)
  e = fgetpos(ss->stream, &ss->pos[i]);   if(e != 0  ) return 701;
#endif
*/
  e = ss->offset[i] = ftell(ss->stream);  if(e == -1L) return 702;
  if(DEBUG) printf("am in get_position i=%d offset=%d\n",
                                 i, ss->offset[i]);
  return 0;
}



/*------------------- set position of file ----------------------------*/
        /* uses one of two methods (the other is commented out) */

static int set_position(ScrsStruct *ss, int i)
{
  int e;

  if(i < 0 || i > ss->ngrp)                                    return 710;
/*
  e = fsetpos(ss->stream, &ss->pos[i]);             if(e != 0) return 711;
*/
  e = fseek  (ss->stream, ss->offset[i], SEEK_SET); if(e != 0) return 712;
  if(DEBUG) printf("am in set_position i=%d offset=%d\n",
                                 i, ss->offset[i]);
  return 0;
}



/*------------------- set position of file for desired shot -----------*/

static int search_position(ScrsStruct *ss, long igroup)
{
  int e, i;

  i = find_iarray_match(igroup, ss->igroup, ss->ngrp, ss->direction);
  if(i >= 0)
       {
       e = set_position(ss, i);
       return e;
       }
  return 722;
}



/*-------------------- check header values ----------------------------*/

static int head_check(ScrsStruct *ss)
{
  int e = 0;
  if(ss->ngrp <= 0 || ss->ngrp > NGRPMAX)         e = 92;
  if(ss->mxxgp < ss->mnxgp && ss->mnxgp != LARGE) e = 93;
  if(ss->mxygp < ss->mnygp && ss->mnygp != LARGE) e = 94;
  if(ss->nch <= 0 || ss->nch > NCHMAX)            e = 95;
  if(ss->nxpw < 0 || ss->nxpw > 100)              e = 96;
  if(ss->nypw < 0 || ss->nypw > 100)              e = 97;
  if(ss->npwoff < 2 || ss->npwoff > 6)            e = 98;
  if(ss->npkwn > 1800)                            e = 99;
  if(DEBUG && e) printf("head_check error %d\n", e);
  return e;
}



/*----------- read or write scrs tail record --------------------------*/

int scrs_tail_get(ScrsStruct *ss)
{
  int e;

  if(DEBUG) printf("am in scrs_tail_get\n");
  e = set_position(ss, ss->ngrp);                if(e) return e;
  e = fscanf(ss->stream, "%ld", &ss->latest);
  if(e == EOF || e == 0) ss->latest = 0;
  return 0;
}



int scrs_tail_put(ScrsStruct *ss)
{
  int e;

  if(DEBUG) printf("am in scrs_tail_put\n");
  e = set_position(ss, ss->ngrp);                  if(e) return e;
  e = fprintf(ss->stream, "%ld\n", ss->latest);    if(e < 0) return e;
  e = fflush(ss->stream);                          if(e != 0) return 683;
  return 0;
}



/*-------------------- read scrs header record ------------------------*/

int scrs_head_get(ScrsStruct *ss)
{
  char *card;          /* added april 12, 2002 */
  char  tkn[22];       /* added april 12, 2002 */

  int e, i;

  if(DEBUG) printf("am in scrs_head_get\n");
  rewind(ss->stream);

/**************
  original code:
  e = fscanf(ss->stream, "%ld %ld %ld %ld %ld %ld %ld %ld",
                       &ss->ngrp , &ss->itmax, &ss->mxxgp, &ss->mxygp,
                       &ss->mnxgp, &ss->mnygp, &ss->mxo  , &ss->incgp);
  if(e != 8) return 301;
**************/

/**************
  new in march 2002:
  e = fscanf(ss->stream, "%6ld%6ld%6ld%6ld%6ld%6ld%6ld%6ld",
                       &ss->ngrp , &ss->itmax, &ss->mxxgp, &ss->mxygp,
                       &ss->mnxgp, &ss->mnygp, &ss->mxo  , &ss->incgp);
  if(e != 8) return 301;
**************/

/**************
  new april 12, 2002:
**************/
  card = readcard(ss->stream);
  if(!card) return 301;
  strncpy(tkn,&card[ 0],6); tkn[6]='\0'; sscanf(tkn, "%ld", &ss->ngrp );
  strncpy(tkn,&card[ 6],6); tkn[6]='\0'; sscanf(tkn, "%ld", &ss->itmax);
  strncpy(tkn,&card[12],6); tkn[6]='\0'; sscanf(tkn, "%ld", &ss->mxxgp);
  strncpy(tkn,&card[18],6); tkn[6]='\0'; sscanf(tkn, "%ld", &ss->mxygp);
  strncpy(tkn,&card[24],6); tkn[6]='\0'; sscanf(tkn, "%ld", &ss->mnxgp);
  strncpy(tkn,&card[30],6); tkn[6]='\0'; sscanf(tkn, "%ld", &ss->mnygp);
  strncpy(tkn,&card[36],6); tkn[6]='\0'; sscanf(tkn, "%ld", &ss->mxo  );
  strncpy(tkn,&card[42],6); tkn[6]='\0'; sscanf(tkn, "%ld", &ss->incgp);
/**************
  end new april 12, 2002.
**************/

  e = fscanf(ss->stream, "%f %ld %f %s %ld %ld %ld %ld %ld",
                       &ss->rinc  , &ss->nch , &ss->trsh  , ss->izc,
                       &ss->nxpw  , &ss->nypw, &ss->npwoff,
                       &ss->ipkwhx, &ss->ipkwhy);
  if(e != 9) return 302;
  ss->npkwn = ss->npwoff * 3 * ss->nxpw * ss->nypw;
  e = head_check(ss);
  if(e) return (300 + e);
  for(i = 0; i < ss->nxpw; i++)
       {  e = fscanf(ss->stream, "%f", &ss->xpw [i]); if(e != 1) return 303; }
  for(i = 0; i < ss->nypw; i++)
       {  e = fscanf(ss->stream, "%f", &ss->ypw [i]); if(e != 1) return 304; }
  for(i = 0; i < ss->npkwn; i++)
       {  e = fscanf(ss->stream, "%f", &ss->pkwn[i]); if(e != 1) return 305; }
  e = fscanf(ss->stream, "%*[^\n]%*c");              if(e == EOF) return 306;
  if(e == 0) e = fscanf(ss->stream, "%*c");          if(e == EOF) return 307;
  return 0;
}


/*------------------- write scrs header record ------------------------*/

int scrs_head_put(ScrsStruct *ss)
{
  int e, i, k;

  if(DEBUG) printf("am in scrs_head_put\n");
  if(ss->read_only) return 0;
  e = head_check(ss);                      if(e) return (400 + e);
  rewind(ss->stream);
  e = fprintf(ss->stream, "%6d%6d%6d%6d%6d%6d%6d%6d\n",
                       ss->ngrp , ss->itmax, ss->mxxgp, ss->mxygp,
                       ss->mnxgp, ss->mnygp, ss->mxo  , ss->incgp);
  if(e < 0) return 401;
  e = fprintf(ss->stream, "%6.1f%6d%6.3f   %-3.3s%4d%4d%4d%4d%4d\n",
                       ss->rinc  , ss->nch , ss->trsh  , ss->izc,
                       ss->nxpw  , ss->nypw, ss->npwoff,
                       ss->ipkwhx, ss->ipkwhy);
  if(e < 0) return 402;
  for(i = 0, k = 0; i < ss->nxpw; i++)
       {
       e = fprintf(ss->stream, " %#9.0f", ss->xpw [i]); if(e < 0) return 403;
       if(++k == 5 || i == ss->nxpw - 1)
              { e = fprintf(ss->stream, "\n"); k = 0; if(e < 0) return 404; }
       }
  for(i = 0, k = 0; i < ss->nypw; i++)
       {
       e = fprintf(ss->stream, " %#9.0f", ss->ypw [i]); if(e < 0) return 405;
       if(++k == 5 || i == ss->nypw - 1)
              { e = fprintf(ss->stream, "\n"); k = 0; if(e < 0) return 406; }
       }
  for(i = 0, k = 0; i < ss->npkwn; i++)
       {
       e = fprintf(ss->stream, " %9.3f", ss->pkwn[i]); if(e < 0) return 407;
       if(++k == 3 || i == ss->npkwn - 1)
              { e = fprintf(ss->stream, "\n"); k = 0; if(e < 0) return 408; }
       }
  e = fflush(ss->stream);                             if(e != 0) return 409;
  return 0;
}


/*-------------------- read next scrs shot record ---------------------*/

int scrs_shot_get(ScrsStruct *ss)
{
  char *card;           /* added april 12, 2002 */
  char  tkn[22];        /* added april 12, 2002 */

  int e, i, half;

  if(DEBUG) printf("am in scrs_shot_get\n");

/*  ADDED 1/22/97 BELOW */
  for(half = 0; half < 2; half++)
      {
      int len;
      char *card = readcard(ss->stream);         
      if(card == NULL) return 501;           
      len = strlen(card);                     
      for(i = 0; i < 5; i++)
          {
          char piece[12];                       
          int index = 10 * i;                    
          int nnn = MinimumValue(len - index, 10);
          if(nnn <= 0) return 501;        
          memcpy(piece, &card[index], nnn);
          piece[nnn] = '\0';
          e = sscanf(piece, "%f", &ss->shot[i + 5*half]);
          if(e != 1) return 501;
          }
      }
/*  ADDED 1/22/97 ABOVE */

/*
    REMOVED 1/22/97:
       { e = fscanf(ss->stream, "%f", &ss->shot[i]); if(e != 1) return 501; }
*/

  for(i = 0; i < ss->nch; i++)
       {
/**************
original code:
       e = fscanf(ss->stream, "%ld %ld %ld %ld %ld",
                    &ss->iarriv[i], &ss->ixgp[i], &ss->iygp[i],
                    &ss->ioff  [i], &ss->elev[i]);
       if(e != 5) return 502;
**************/

/**************
  new in march 2002:
       e = fscanf(ss->stream, "%6ld%6ld%6ld%6ld%6ld",
                    &ss->iarriv[i], &ss->ixgp[i], &ss->iygp[i],
                    &ss->ioff  [i], &ss->elev[i]);
       if(e != 5) return 502;
**************/

/**************
  new april 12, 2002:
**************/
       card = readcard(ss->stream);
       if(!card) return 502;
       strncpy(tkn,&card[ 0],6); tkn[6]='\0'; sscanf(tkn,"%ld",&ss->iarriv[i]);
       strncpy(tkn,&card[ 6],6); tkn[6]='\0'; sscanf(tkn,"%ld",&ss->ixgp  [i]);
       strncpy(tkn,&card[12],6); tkn[6]='\0'; sscanf(tkn,"%ld",&ss->iygp  [i]);
       strncpy(tkn,&card[18],6); tkn[6]='\0'; sscanf(tkn,"%ld",&ss->ioff  [i]);
       strncpy(tkn,&card[24],6); tkn[6]='\0'; sscanf(tkn,"%ld",&ss->elev  [i]);
/**************
  end new april 12, 2002.
**************/
       }

/**************
  removed april 12, 2002:
  e = fscanf(ss->stream, "%*[^\n]%*c");              if(e == EOF) return 503;
  if(e == 0) e = fscanf(ss->stream, "%*c");          if(e == EOF) return 504;
**************/
  return 0;
}



/*------------------- write next scrs shot record ---------------------*/

int scrs_shot_put(ScrsStruct *ss)
{
  int e, i;

  if(DEBUG) printf("am in scrs_shot_put\n");
  if(ss->read_only) return 0;
  for(i = 0; i < 5; i++)
       {
       if(ss->shot[i] > 99999.9 || ss->shot[i] < -9999.9)
       e = fprintf(ss->stream, "%10.2f", ss->shot[i]);
       else
       e = fprintf(ss->stream, "%10.3f", ss->shot[i]);
       if(e < 0) return 601;
       }
/*
       { e = fprintf(ss->stream, "%10.3f", ss->shot[i]);
                                                     if(e < 0) return 601; }
*/
  e = fprintf(ss->stream, "\n");                     if(e < 0) return 602;

  for(i = 5; i < 10; i++)
       {
       if(ss->shot[i] > 99999.9 || ss->shot[i] < -9999.9)
       e = fprintf(ss->stream, "%10.2f", ss->shot[i]);
       else
       e = fprintf(ss->stream, "%10.3f", ss->shot[i]);
       if(e < 0) return 601;
       }
/*
       { e = fprintf(ss->stream, "%10.3f", ss->shot[i]);
                                                     if(e < 0) return 603; }
*/
  e = fprintf(ss->stream, "\n");                     if(e < 0) return 604;

  for(i = 0; i < ss->nch; i++)
       {
       e = fprintf(ss->stream, "%6d%6d%6d%6d%6d\n",
                    ss->iarriv[i], ss->ixgp[i], ss->iygp[i],
                    ss->ioff  [i], ss->elev[i]);
       if(e < 0) return 605;
       }
  ss->latest = NearestInteger(ss->shot[8]);
  return 0;
}



/*---------------------- compare verify ----------------------------*/

     /* to verify that the cache contains the same information
        as the file */

static void compare_verify(ScrsStruct *ss, ShotRecord *sr, long igroup)
{
  int i, e;
  if(!sr)
      {
      if(CVPRINT) printf("compare_verify group %d not found\n", igroup);
      return;
      }
  if(CVPRINT) printf("compare_verify group %d found\n", igroup);
  e = search_position(ss, igroup);
  if(e != 0)
      {
      printf("compare_verify group %d search_position error\n", igroup);
      assert(FALSE);
      }
  e = scrs_shot_get(ss);
  if(e != 0)
      {
      printf("compare_verify group %d scrs_shot_get error\n", igroup);
      assert(FALSE);
      }
  for(i = 0; i < 10; i++)
      {
      assert(ss->shot[i] == sr->shot[i]);
      }
  for(i = 0; i < ss->nch; i++)
      {
      assert(ss->iarriv[i] == sr->iarriv[i]);
      assert(ss->ixgp  [i] == sr->ixgp  [i]);
      assert(ss->iygp  [i] == sr->iygp  [i]);
      assert(ss->ioff  [i] == sr->ioff  [i]);
      assert(ss->elev  [i] == sr->elev  [i]);
      }
}



/*------------------ read desired scrs shot record --------------------*/

int scrs_shot_getd(ScrsStruct *ss, long igroup)
{
  int e;
  ShotRecord *sr;

  if(DEBUG) printf("am in scrs_shot_getd\n");
  sr = (ShotRecord*)get_from_memory_cache(ss->cache,
                                       (void*)ss->shot, igroup);
  if(VERIFY)
      {
      compare_verify(ss, sr, igroup);
      }
  if(!sr)
      {
      e = search_position(ss, igroup);  if(e != 0) return 801;
      e = scrs_shot_get  (ss        );  if(e != 0) return 802;
      if(ss->cache_update_flag)
                add_to_memory_cache(ss->cache, (void*)ss->shot, igroup);
      }
  return 0;
}



/*----------------- write desired scrs shot record --------------------*/

int scrs_shot_putd(ScrsStruct *ss, long igroup)
{
  int e;

  if(DEBUG) printf("am in scrs_shot_putd\n");
  if(ss->read_only) return 0;
  e = search_position(ss, igroup);             if(e) return 901;
  e = scrs_shot_put  (ss        );             if(e) return 902;
  e = scrs_tail_put  (ss        );             if(e) return 903;
  e = fflush         (ss->stream);             if(e) return 904;
  add_to_memory_cache(ss->cache, (void*)ss->shot, igroup);
  return 0;
}



/*----------------- check header size ---------------------------------*/

static int check_headsize(ScrsStruct *ss, long nwords)
{
  int e = 0;
  if(nwords < 44      ) e = 2002;
  if(nwords < ss->sxgp) e = 2003;
  if(nwords < ss->sygp) e = 2004;
  if(nwords < ss->rxgp) e = 2005;
  if(nwords < ss->rygp) e = 2006;
  if(DEBUG && e) printf("check_headsize error %d\n", e);
  return e;
}


/*--------------- find slow 2D array match ----------------------*/

static int find_2d_iarray_match(long gpx, long gpy,
                                  long *ixgp, long *iygp, int nch)
{
  int k;
  for(k = 0; k < nch; k++)
       {
       if(gpx == ixgp[k] && gpy == iygp[k]) return k;
       }
  return -1;
}


/*----------------- find match for one or two header words ------------*/
                         /* not efficient for two */

static int find_match(ScrsStruct *ss, int i,
                  float head[], long nwords, long direction, long att)
{
  int k;
  long gpx, gpy, source_gpx, source_gpx2;
  long source_gpy, source_gpy2, offset, offset2;
  long xoff, yoff;
  float head_gpx, head_gpy;

  if(att == CHANNEL)
       {
       k = NearestInteger(head[nwords * i + 9]);       /* hwd 10 */
       if(k > 0 && k <= ss->nch) return k - 1;
       return -1;
       }
  head_gpx = head[nwords * i + ss->rxgp - 1];
  gpx = NearestInteger(head_gpx);
/*
  gpx = NearestInteger(head[nwords * i + ss->rxgp - 1]);
*/
  if(ss->rygp == 0)
       {
       if(att == OFFSET)
            {
            source_gpx = NearestInteger(head[nwords * i + ss->sxgp - 1]);
            source_gpx2 = NearestInteger(ss->shot[0]);
            gpx = gpx - source_gpx + source_gpx2;
            }
       k = find_iarray_match(gpx, ss->ixgp, ss->nch, direction);
/* the following 5 lines are new 2/17/94 */
       if(k >= 0) return k;
       if(head_gpx > gpx + 0.4)
                k = find_iarray_match(gpx+1, ss->ixgp, ss->nch, direction);
       else if(head_gpx < gpx - 0.4)
                k = find_iarray_match(gpx-1, ss->ixgp, ss->nch, direction);
       return k;
       }
  head_gpy = head[nwords * i + ss->rygp - 1];
  gpy = NearestInteger(head_gpy);
/*
  gpy = NearestInteger(head[nwords * i + ss->rygp - 1]);
*/
  if(att == OFFSET)
       {
       source_gpx = NearestInteger(head[nwords * i + ss->sxgp - 1]);
       source_gpy = NearestInteger(head[nwords * i + ss->sygp - 1]);
       source_gpx2 = NearestInteger(ss->shot[0]);
       source_gpy2 = NearestInteger(ss->shot[7]);
       xoff = gpx - source_gpx;
       yoff = gpy - source_gpy;
       offset = NearestInteger(sqrt((float)(xoff * xoff + yoff * yoff)));
       for(k = 0; k < ss->nch; k++)
            {
            xoff = ss->ixgp[k] - source_gpx2;
            yoff = ss->iygp[k] - source_gpy2;
            offset2 = NearestInteger(sqrt((float)(xoff * xoff + yoff * yoff)));
            if(offset2 == offset) return k;
            }
       }
  else
       {
/*
       for(k = 0; k < ss->nch; k++)
            {
            if(gpx == ss->ixgp[k] && gpy == ss->iygp[k]) return k;
            }
*/
/* the following several lines are new 2/17/94 */
       k = find_2d_iarray_match(gpx, gpy, ss->ixgp, ss->iygp, ss->nch);
       if(k >= 0) return k;
       if(head_gpy > gpy + 0.4)
         {
         if(head_gpx > gpx + 0.4)
           k = find_2d_iarray_match(gpx+1, gpy+1, ss->ixgp, ss->iygp, ss->nch);
         else if(head_gpx < gpx - 0.4)
           k = find_2d_iarray_match(gpx-1, gpy+1, ss->ixgp, ss->iygp, ss->nch);
         else
           k = find_2d_iarray_match(gpx  , gpy+1, ss->ixgp, ss->iygp, ss->nch);
         }
       else if(head_gpy < gpy - 0.4)
         {
         if(head_gpx > gpx + 0.4)
           k = find_2d_iarray_match(gpx+1, gpy-1, ss->ixgp, ss->iygp, ss->nch);
         else if(head_gpx < gpx - 0.4)
           k = find_2d_iarray_match(gpx-1, gpy-1, ss->ixgp, ss->iygp, ss->nch);
         else
           k = find_2d_iarray_match(gpx  , gpy-1, ss->ixgp, ss->iygp, ss->nch);
         }
       else
         {
         if(head_gpx > gpx + 0.4)
           k = find_2d_iarray_match(gpx+1, gpy  , ss->ixgp, ss->iygp, ss->nch);
         else if(head_gpx < gpx - 0.4)
           k = find_2d_iarray_match(gpx-1, gpy  , ss->ixgp, ss->iygp, ss->nch);
         }
       return k;

       }
  return -1;
}



/*------ find interpolated match for one or two header words ----------*/


static long find_terp_match(ScrsStruct *ss, int i,
                  float head[], long nwords, long direction, long att)
{
/*
  int k;
*/
  long gpx, /*gpy,*/ source_gpx, source_gpx2;
/*
  long source_gpy, source_gpy2, offset, offset2;
  long xoff, yoff;
*/
  long iarriv;
  float head_gpx /* , head_gpy*/;
  long ia, ib;
  float wa, wb;

  head_gpx = head[nwords * i + ss->rxgp - 1];
  gpx = NearestInteger(head_gpx);
/*
  gpx = NearestInteger(head[nwords * i + ss->rxgp - 1]);
*/
  if(ss->rygp == 0)
       {
       if(att == OFFSET)
            {
            source_gpx = NearestInteger(head[nwords * i + ss->sxgp - 1]);
            source_gpx2 = NearestInteger(ss->shot[0]);
            gpx = gpx - source_gpx + source_gpx2;
            }
       find_iarray_brackets(gpx, ss->ixgp, ss->nch, &ia, &ib, &wa, &wb);
       if(ia == ib) iarriv = SCRS_MISSING;
       else         iarriv = NearestInteger(ss->iarriv[ia] * wa +
                                            ss->iarriv[ib] * wb);
       return iarriv;
       }

/*********** finished down to here ***********/

  return SCRS_MISSING;      /* temporary */

/******************
  head_gpy = head[nwords * i + ss->rygp - 1];
  gpy = NearestInteger(head_gpy);
/ *
  gpy = NearestInteger(head[nwords * i + ss->rygp - 1]);
* /
  if(att == OFFSET)
       {
       source_gpx = NearestInteger(head[nwords * i + ss->sxgp - 1]);
       source_gpy = NearestInteger(head[nwords * i + ss->sygp - 1]);
       source_gpx2 = NearestInteger(ss->shot[0]);
       source_gpy2 = NearestInteger(ss->shot[7]);
       xoff = gpx - source_gpx;
       yoff = gpy - source_gpy;
       offset = NearestInteger(sqrt((float)(xoff * xoff + yoff * yoff)));
       for(k = 0; k < ss->nch; k++)
            {
            xoff = ss->ixgp[k] - source_gpx2;
            yoff = ss->iygp[k] - source_gpy2;
            offset2 = NearestInteger(sqrt((float)(xoff * xoff + yoff * yoff)));
            if(offset2 == offset) return k;
            }
       }
  else
       {
/ *
       for(k = 0; k < ss->nch; k++)
            {
            if(gpx == ss->ixgp[k] && gpy == ss->iygp[k]) return k;
            }
* /
/ * the following several lines are new 2/17/94 * /
       k = find_2d_iarray_match(gpx, gpy, ss->ixgp, ss->iygp, ss->nch);
       if(k >= 0) return k;
       if(head_gpy > gpy + 0.4)
         {
         if(head_gpx > gpx + 0.4)
           k = find_2d_iarray_match(gpx+1, gpy+1, ss->ixgp, ss->iygp, ss->nch);
         else if(head_gpx < gpx - 0.4)
           k = find_2d_iarray_match(gpx-1, gpy+1, ss->ixgp, ss->iygp, ss->nch);
         else
           k = find_2d_iarray_match(gpx  , gpy+1, ss->ixgp, ss->iygp, ss->nch);
         }
       else if(head_gpy < gpy - 0.4)
         {
         if(head_gpx > gpx + 0.4)
           k = find_2d_iarray_match(gpx+1, gpy-1, ss->ixgp, ss->iygp, ss->nch);
         else if(head_gpx < gpx - 0.4)
           k = find_2d_iarray_match(gpx-1, gpy-1, ss->ixgp, ss->iygp, ss->nch);
         else
           k = find_2d_iarray_match(gpx  , gpy-1, ss->ixgp, ss->iygp, ss->nch);
         }
       else
         {
         if(head_gpx > gpx + 0.4)
           k = find_2d_iarray_match(gpx+1, gpy  , ss->ixgp, ss->iygp, ss->nch);
         else if(head_gpx < gpx - 0.4)
           k = find_2d_iarray_match(gpx-1, gpy  , ss->ixgp, ss->iygp, ss->nch);
         }
       return k;

       }

  return -1;
*********************/
}



/*----------------- get or put picks for cbyt -------------------------*/

#define CURR 1  /* get  current  picks */
#define PREV 2  /* get  previous picks */
#define NEXT 3  /* get  next     picks */
#define SEL  4  /* get  selected picks */
#define SAVE 5  /* save current  picks */

static int move_picks(ScrsStruct *ss, float head[], long nwords,
                     long ntraces, float picks[],
                     long att, int option, long grp)
{
  int i = 0, k, e, error = ntraces;
  long igroup, jgroup, igroup2, direction=0, iarriv;

  e = check_headsize(ss, nwords);             if(e) return e;
  if(nwords < ss->rxgp || nwords < ss->rygp)  return (-100 * option - 102);
  if(ntraces <   1)                           return (-100 * option - 103);
  while(i < ntraces)
       {
       igroup = NearestInteger(head[nwords * i + 8]);
       if     (option == PREV) igroup2 = get_prev_profile(ss, igroup);
       else if(option == NEXT) igroup2 = get_next_profile(ss, igroup);
       else if(option == SEL ) igroup2 = grp;
       else                    igroup2 = igroup;
       e = scrs_shot_getd(ss, igroup2);
       if(e == 0) direction = find_iarray_direction(ss->ixgp, ss->nch);
       jgroup = igroup;
       while(i < ntraces && jgroup == igroup)
            {
            if(option != SAVE) picks[i] = SCRS_MISSING;
            if(e == 0)
                 {
                 k = find_match(ss, i, head, nwords, direction, att);
                 if(k >= 0)
                    {
                    if(option == SAVE) ss->iarriv[k] = 1000.0 * picks[i] + 0.5;
                    else               picks[i]      = 0.001  * ss->iarriv[k];
                    error--;
                    }
                 else if(att != CHANNEL &&
                    (option == PREV || option == NEXT || option == SEL))
                    {
                    iarriv = find_terp_match
                               (ss, i, head, nwords, direction, att);
                    if(iarriv != SCRS_MISSING)
                        {
                        picks[i] = 0.001 * iarriv;
                        error--;
                        }
                    }
                 }
            i++;
            if(i < ntraces) jgroup = NearestInteger(head[nwords * i + 8]);
            }
       if(option == SAVE) e = scrs_shot_putd(ss, igroup2);
       }
  return error;
}



/*----------------- get or put picks for cbyt -------------------------*/

int scrs_get_picks_spws(ScrsStruct *ss, float head[], long nwords,
                                             long ntraces, float picks[])
{
  int error;
  ss->cache_update_flag = TRUE;
  error = move_picks(ss, head, nwords, ntraces, picks, RECEIVER, CURR, 0);
  return error;
}



int scrs_put_picks_spws(ScrsStruct *ss, float head[], long nwords,
                                             long ntraces, float picks[])
{
  int error;
  ss->cache_update_flag = FALSE;
  error = move_picks(ss, head, nwords, ntraces, picks, RECEIVER, SAVE, 0);
  ss->cache_update_flag = TRUE;
  return error;
}



int scrs_get_prev_picks_spws(ScrsStruct *ss, float head[], long nwords,
                      long ntraces, float picks[], long att)
{
  int error;
  ss->cache_update_flag = FALSE;
  error = move_picks(ss, head, nwords, ntraces, picks, att, PREV, 0);
  ss->cache_update_flag = TRUE;
  return error;
}



int scrs_get_next_picks_spws(ScrsStruct *ss, float head[], long nwords,
                      long ntraces, float picks[], long att)
{
  int error;
  ss->cache_update_flag = FALSE;
  error = move_picks(ss, head, nwords, ntraces, picks, att, NEXT, 0);
  ss->cache_update_flag = TRUE;
  return error;
}



int scrs_get_sel_picks_spws (ScrsStruct *ss, float head[], long nwords,
                      long ntraces, float picks[], long att, long grp)
{
  int error;
  ss->cache_update_flag = TRUE;
  error = move_picks(ss, head, nwords, ntraces, picks, att, SEL, grp);
  return error;
}




/*----------------- grab header record --------------------------------*/

int scrs_head_grab(ScrsStruct *ss, long *ngrp, long *incgp, float *rinc,
                                   long *nch, float *trsh, char izc[])
{
  *ngrp   = ss->ngrp;
  *incgp  = ss->incgp;
  *rinc   = ss->rinc;
  *nch    = ss->nch ;
  *trsh   = ss->trsh;
  strcpy(izc, ss->izc);
  return 0;
}



/*----------------- fill header record --------------------------------*/

int scrs_head_fill(ScrsStruct *ss, long ngrp, long incgp, float rinc,
                                   long nch, float trsh, char izc[])
{
  int i, j, k, index;
  float offset, twin, bwin;

               /* set variables from arguments */

  ss->ngrp   = ngrp;
  ss->incgp  = incgp;
  ss->rinc   = rinc;
  ss->nch    = nch ;
  ss->trsh   = trsh;
  if(izc[0] == 'Y' || izc[0] == 'y') strcpy(ss->izc, "YES");
  else                               strcpy(ss->izc, "NO" );

               /* set defaulted variables */

  ss->nxpw   = 2;
  ss->nypw   = 2;
  ss->npwoff = 3;
  ss->ipkwhx = 9;
  ss->ipkwhy = 0;
  for(i = 0; i < ss->nxpw ; i++) { ss->xpw [i] =  1.0 +        i; }
  for(i = 0; i < ss->nypw ; i++) { ss->ypw [i] = 10.0 + 10.0 * i; }
  ss->npkwn  = ss->npwoff * 3 * ss->nxpw * ss->nypw;
  k          =                  ss->nxpw * ss->nypw;
  index = -1;
  for(i = 0; i < k; i++)
       {
       for(j = 0; j < ss->npwoff; j++)
            {
            offset = i +             5000.0 * j;
            twin   =                    0.2 * j;
            bwin   = 2.0 + 0.1 * i +    0.2 * j;
            index++; ss->pkwn[index] = offset;
            index++; ss->pkwn[index] = twin  ;
            index++; ss->pkwn[index] = bwin  ;
            }
       }

               /* initialize limits */

  ss->mxo   = 0;
  ss->mxxgp = 0;
  ss->mxygp = 0;
  ss->mnxgp = LARGE;
  ss->mnygp = LARGE;
  ss->itmax = 0;
  return head_check(ss);
}



/*----------------- fill arrays in header record ----------------------*/

int scrs_head_fill2(ScrsStruct *ss, long nxpw, long nypw, long npwoff,
                             long ipkwhx, long ipkwhy,
                             float xpw[], float ypw[], float pkwn[])
{
  int i;

  ss->nxpw   = nxpw;
  ss->nypw   = nypw;
  ss->npwoff = npwoff;
  ss->npkwn  = ss->npwoff * 3 * ss->nxpw * ss->nypw;
  ss->ipkwhx = ipkwhx;
  ss->ipkwhy = ipkwhy;
  for(i = 0; i < ss->nxpw ; i++) { ss->xpw [i] = xpw [i]; }
  for(i = 0; i < ss->nypw ; i++) { ss->ypw [i] = ypw [i]; }
  for(i = 0; i < ss->npkwn; i++) { ss->pkwn[i] = pkwn[i]; }
  return head_check(ss);
}



/*----------------- grab arrays in header record ----------------------*/

int scrs_head_grab2(ScrsStruct *ss, long *nxpw, long *nypw, long *npwoff,
                             long *ipkwhx, long *ipkwhy,
                             float xpw[], float ypw[], float pkwn[])
{
  int i;

  *nxpw   = ss->nxpw;
  *nypw   = ss->nypw;
  *npwoff = ss->npwoff;
  *ipkwhx = ss->ipkwhx;
  *ipkwhy = ss->ipkwhy;
  for(i = 0; i < ss->nxpw ; i++) { xpw [i] = ss->xpw [i]; }
  for(i = 0; i < ss->nypw ; i++) { ypw [i] = ss->ypw [i]; }
  for(i = 0; i < ss->npkwn; i++) { pkwn[i] = ss->pkwn[i]; }
  return head_check(ss);
}



/*----------------- fill limits data in header record -----------------*/

int scrs_head_fill3(ScrsStruct *ss, long itmax, long mxxgp, long mxygp,
                                    long mnxgp, long mnygp, long mxo)
{
  ss->itmax = itmax;
  ss->mxxgp = mxxgp;
  ss->mxygp = mxygp;
  ss->mnxgp = mnxgp;
  ss->mnygp = mnygp;
  ss->mxo   = mxo  ;
  return 0;
}



/*----------------- grab limits data in header record -----------------*/

int scrs_head_grab3(ScrsStruct *ss, long *itmax, long *mxxgp, long *mxygp,
                                    long *mnxgp, long *mnygp, long *mxo)
{
  *itmax = ss->itmax;
  *mxxgp = ss->mxxgp;
  *mxygp = ss->mxygp;
  *mnxgp = ss->mnxgp;
  *mnygp = ss->mnygp;
  *mxo   = ss->mxo  ;
  return 0;
}




/*----------------- update header limits ------------------------------*/

static void update_limits(ScrsStruct *ss)
{
  int i, j;

  if(ss->shot[0] > ss->mxxgp) ss->mxxgp = ss->shot[0];
  if(ss->shot[0] < ss->mnxgp) ss->mnxgp = ss->shot[0];
  if(ss->shot[7] > ss->mxygp) ss->mxygp = ss->shot[7];
  if(ss->shot[7] < ss->mnygp) ss->mnygp = ss->shot[7];
  for(i = 0; i < ss->nch; i++)
       {
       j = ss->ioff[i] / ss->rinc;
       if(j              > ss->mxo  ) ss->mxo   = j;
       if(ss->ixgp  [i] > ss->mxxgp) ss->mxxgp = ss->ixgp  [i];
       if(ss->ixgp  [i] < ss->mnxgp) ss->mnxgp = ss->ixgp  [i];
       if(ss->iygp  [i] > ss->mxygp) ss->mxygp = ss->iygp  [i];
       if(ss->iygp  [i] < ss->mnygp) ss->mnygp = ss->iygp  [i];
       if(ss->iarriv[i] > ss->itmax) ss->itmax = ss->iarriv[i];
       }
}



/*----------------- grab shot record ----------------------------------*/

int scrs_shot_grab(ScrsStruct *ss, float shot[], float picks[],
                     long ixgp[], long iygp[], long ioff[], long elev[])
{
  int i;

  if(shot)
       {
       for(i = 0; i < 10; i++)  { shot[i] = ss->shot[i]; }
       }
  for(i = 0; i < ss->nch; i++)
       {
       if(ixgp ) ixgp [i] =         ss->ixgp  [i];
       if(iygp ) iygp [i] =         ss->iygp  [i];
       if(ioff ) ioff [i] =         ss->ioff  [i];
       if(elev ) elev [i] =         ss->elev  [i];
       if(picks) picks[i] = 0.001 * ss->iarriv[i];
       }
  return 0;
}



/*----------------- fill shot record ----------------------------------*/

int scrs_shot_fill(ScrsStruct *ss, float shot[], float picks[],
                     long ixgp[], long iygp[], long ioff[], long elev[])
{
  int i;

  if(shot)
       {
       for(i = 0; i < 10; i++)  { ss->shot[i] = shot[i]; }
       }
  for(i = 0; i < ss->nch; i++)
       {
       if(ixgp ) ss->ixgp  [i] =          ixgp [i];
       if(iygp ) ss->iygp  [i] =          iygp [i];
       if(ioff ) ss->ioff  [i] =          ioff [i];
       if(elev ) ss->elev  [i] =          elev [i];
       if(picks) ss->iarriv[i] = 1000.0 * picks[i] + 0.5;
       }
  update_limits(ss);
  return 0;
}



/*----------------- fill shot record using trace headers --------------*/

int scrs_shot_fill1_spws(ScrsStruct *ss, float head[], long nwords, float pick,
                                                     long ich)
{
  int e, i;

  if(ich <= 0 || ich > ss->nch) return 7842;
  if(ich == 1)
       {
       e = check_headsize(ss, nwords);  if(e) return e;
       ss->shot[0] = NearestInteger(head[ss->sxgp - 1]);
       ss->shot[1] = head[20       - 1];
       ss->shot[2] = head[44       - 1];
       ss->shot[3] = head[13       - 1] - head[20 - 1];
       ss->shot[4] = 0;
       ss->shot[5] = 0;
       ss->shot[6] = 0;
       if(ss->sygp) ss->shot[7] = NearestInteger(head[ss->sygp - 1]);
       else         ss->shot[7] = 0;
       ss->shot[8] = NearestInteger(head[9 - 1]);
       ss->shot[9] = 0;
       }
  i = ich - 1;
       ss->iarriv[i] = 1000.0 * MaximumValue(pick,0.0) + 0.5;
       ss->ixgp[i] = NearestInteger(head[ss->rxgp - 1]);
       if(ss->rygp) ss->iygp[i] = NearestInteger(head[ss->rygp - 1]);
       else         ss->iygp[i] = 0;
       ss->ioff[i] = NearestInteger(head[6  - 1]);
       ss->elev[i] = NearestInteger(head[16 - 1]);
  if(ich == ss->nch) update_limits(ss);
  return 0;
}



int scrs_shot_fillh_spws(ScrsStruct *ss, float head[], long nwords,
                         float picks[])
{
  int e, i;
  float pick;

  for(i = 0; i < ss->nch; i++)
       {
       if(picks) pick = picks[i];
       else      pick = 0;
       e = scrs_shot_fill1_spws(ss, &head[i * nwords], nwords, pick, i + 1);
       if(e) return e;
       }
  return 0;
}



/*----------------- check validity of file ----------------------------*/

long scrs_check_validity(char *filename, char *info,
                                            long *ngrp, long *nch)
{
  ScrsStruct ss;
  int error;

  *ngrp = 0;
  *nch  = 0;
  ss.stream = fopen(filename, "r");
  if(ss.stream == NULL) return INQUIRE_VALID_NO;
  error = scrs_head_get(&ss);
  fclose(ss.stream);
  if(error) return INQUIRE_VALID_NO;
  *ngrp = ss.ngrp;
  *nch  = ss.nch ;
  if(info)
       sprintf(info, "%d shot profiles -- %d channels", *ngrp, *nch);
  return INQUIRE_VALID_YES;
}



/*----------------- check validity of both files ----------------------*/

void scrs_check_validities(char *filename1, char *filename2,
     long *valid1, long *valid2, char *info1, char *info2,
     long *same_datasets)
{
  long ngrp1, nch1;
  long ngrp2, nch2;

  *valid1 = scrs_check_validity(filename1, info1, &ngrp1, &nch1);
  *valid2 = scrs_check_validity(filename2, info2, &ngrp2, &nch2);
  *same_datasets = (ngrp1 == ngrp2 && nch1 == nch2);
}



/*----------------- inquire -------------------------------------------*/

long scrs_inquire(char *filename1, char *filename2,
     long required1, long required2,
     char *msg1, char *msg2, char *msg3,
     long *status1, long *status2)
{
  long same_datasets, status;
  long /* ngrp1, nch1, */ valid1;
  long /* ngrp2, nch2, */ valid2;
  char info1[100], info2[100];
  static char filetype[] = "SCRS pickfile";

  scrs_check_validities(filename1, filename2,
              &valid1, &valid2, info1, info2, &same_datasets);
  status = inquire_files_combo(filename1, filename2,
          filetype, required1, required2, valid1, valid2,
          info1, info2, same_datasets, status1, status2,
          msg1, msg2, msg3);
  return status;
}




/*------------- helper for working message ---------------------------*/

static void working(MsgFun *msgfun, void *msgdata,
                    long i, long ngrp, char *phrase1, char *phrase2)
{
  char msg[200];

  if(msgfun && i == 10*(i/10))
       {
       sprintf(msg, "%s record %d of %d %s", phrase1, i+1, ngrp, phrase2);
       msgfun(msgdata, msg);
       }
}



/*-------------------- copy pickfile ---------------------------------*/
            /* the pickfiles are already opened */

static long copy_pickfile(ScrsStruct *ss, MsgFun *msgfun,
                         void *msgdata, FILE *stream1, FILE *stream2)
{
  long i, e;

  ss->stream = stream1; e = scrs_head_get(ss);          if(e) return 201;
  ss->stream = stream2; e = scrs_head_put(ss);          if(e) return 202;
  for(i = 0; i < ss->ngrp; i++)
     {
     working(msgfun, msgdata, i, ss->ngrp, "copying", "to output file");
     ss->stream = stream1; e = scrs_shot_get(ss);       if(e) return 203;
     ss->stream = stream2; e = get_position(ss, i);     if(e) return 204;
     ss->stream = stream2; e = scrs_shot_put(ss);       if(e) return 205;
     ss->igroup[i] = NearestInteger(ss->shot[8]);
     if(i < MAX_CACHE) add_to_memory_cache(ss->cache,
                                   (void*)ss->shot, ss->igroup[i]);
     }
  ss->stream = stream1; e = get_position(ss, ss->ngrp); if(e) return 206;
  ss->stream = stream1; e = scrs_tail_get(ss);          if(e) return 207;
  ss->stream = stream2; e = get_position(ss, ss->ngrp); if(e) return 208;
  ss->stream = stream2; e = scrs_tail_put(ss);          if(e) return 209;
  ss->stream = stream2; e = scrs_head_put(ss);          if(e) return 210;
  ss->stream = stream2; rewind(ss->stream);
  return 0;
}



/*-------------------- read pickfile ---------------------------------*/
            /* the pickfile is already opened */

static long read_pickfile(ScrsStruct *ss, MsgFun *msgfun, void *msgdata)
{
  long i, e;

  e = scrs_head_get(ss);                                if(e) return 301;
  for(i = 0; i < ss->ngrp; i++)
     {
     working(msgfun, msgdata, i, ss->ngrp, "reading", " ");
     e = get_position(ss, i);                           if(e) return 302;
     e = scrs_shot_get(ss);                             if(e) return 303;
     ss->igroup[i] = NearestInteger(ss->shot[8]);
     if(i < MAX_CACHE) add_to_memory_cache(ss->cache,
                                   (void*)ss->shot, ss->igroup[i]);
     }
  e = get_position(ss, ss->ngrp);                       if(e) return 304;
  e = scrs_tail_get(ss);                                if(e) return 305;
  rewind(ss->stream);
  return 0;
}



/*
 * We now always scan the entire trace file to determine the maximum channels.
 * We do this because of experience with files where the total number of traces
 * is an exact multiple of the number of channels in the first group, but the
 * number of channels in the first group is not the maximum number of channels.
 * Instead of rewriting the application to always just call
 * create_pickfile_unequal, create_pickfile is still called and always returns
 * 407 telling the calling routine to call create_pickfile_unequal.  This
 * is to minimize code-meddling and testing since create_pickfile_unequal
 * depends on some error checking in create_pickfile being done prior to
 * create_pickfile_unequal being called.
 * ehs   30nov99
 */

/*------------- create pickfile from scratch -------------------------*/
            /* the pickfile is already opened */

#define PICK        0.0
#define TRSH        0.0
#define IZC         "YES     "
#define MAXW        6500
#define ERROR(nn, message)                                        \
                 { if(glbl) tracefile_close(glbl);                \
                   sprintf(msg, "Error %d\n%s", (nn), (message)); \
                   return (nn); }

/******
  There is a compiler warning on the above macro ERROR:
    "Value of escape sequence exceeds maximum value"
  I do not know what this message means.
  The n in the \n in the sprintf was being replaced by the n is the 
  macro argument list.  Replaced n in the macro argument list with
  nn and the warning disappears.
******/


static long create_pickfile(ScrsStruct *ss, MsgFun *msgfun,
      void *msgdata, char *tracefile,
/*
      void *(*tracefile_open)(), long (*tracefile_read_header)(),
      long (*tracefile_rewind)(), long (*tracefile_close)(), char *msg)
*/
      TFOpen   *tracefile_open    , TFReadHeader *tracefile_read_header,
      TFRewind *tracefile_rewind  , TFClose      *tracefile_close,
      int       tracefile_pickhead, char *msg)
{
  long ngrp, nch, ntraces, nwords, incgp, e, i, j, ich /*, len*/ ;
  float head[MAXW], head9=0.0, head14=0.0, head15=0.0;
  float head35=0.0, head47=0.0, xincgp=0.0, rinc=0.0;
  void *glbl = NULL;
  int open_file = 1;
  float pick = PICK;

  if(!tracefile || !tracefile_open || !tracefile_read_header ||
     !tracefile_rewind || !tracefile_close)
                      ERROR(401,"Trace file I/O routines not available")
  glbl = tracefile_open(tracefile, &nwords, &ntraces);
  if(!glbl)           ERROR(402,"Trace file cannot be opened")
  if(ntraces <= 0)    ERROR(403,"No traces on trace file")
  if(nwords  < 47)    ERROR(404,"Not enough words in trace headers")

  i = nch = 0;
  while(i < ntraces && nch == 0)
       {
       e = tracefile_read_header(glbl, head, open_file);
       open_file = 0;
       /*
        * If trace file, return of -2 means sequential trace number did not
        * start at one.  That's ok.
        * ehs   30nov99
        */
    /* if(e != nwords) ERROR(405,"Trace header has wrong number of words") */
       if ((e != nwords)
        && ((tracefile_read_header != (TFReadHeader *) &tracefile_read_header)
         || (e != -2)))
       {
         ERROR(405,"Trace header has wrong number of words")
       }
       if(i == 0)
            {
            head9  = head[8];      /* group number */
            head14 = head[13];     /* receiver x distance coord */
            head15 = head[14];     /* receiver y distance coord */
            head35 = head[34];     /* receiver x grid ground pos */
            head47 = head[46];     /* receiver sequential ground pos */
            }
       else if(head[8] != head9)
            {
            nch = i;
            }
       else if(i == 1)
            {
            if(ss->typegp == SEQU) xincgp = head[46] - head47;
            else                   xincgp = head[34] - head35;
            rinc = sqrt( (head[13] - head14) * (head[13] - head14) +
                         (head[14] - head15) * (head[14] - head15) );
/*
            rinc = head[13] - head14;
*/
            }
       i++;
       }

  if(nch == 0) nch = ntraces;
  if(nch <= 2)      ERROR(406,"number of channels less than 3")
  ngrp = ntraces / nch;

  /*
   * Force call of create_pickfile_unequal.
   * ehs   30nov99
   */
/*if(ntraces != ngrp * nch) */
/*       ERROR(407,"Number of traces differs\n in different shot profiles") */
 /*
  * tracefile_read_header will always be non-zero here.
  * If is only to avoid compiler warning.
  */
  if (tracefile_read_header)
         ERROR(407,"")
  if(xincgp < 0.0) xincgp = -xincgp;
  if(rinc   < 0.0) rinc   = -rinc  ;
  incgp = NearestInteger(xincgp);
  if(incgp == 0)    ERROR(408,"X ground position increment is zero")
  rinc = rinc / incgp;                           
  if(rinc  == 0.0)  ERROR(409,"Receiver increment is zero")
  e = scrs_head_fill(ss, ngrp, incgp, rinc, nch, TRSH, IZC);
  if(e)             ERROR(410,"trying to fill SCRS pickfile header")
  e = scrs_head_put (ss);                                  
  if(e)             ERROR(411,"trying to put SCRS pickfile header")
  e = tracefile_rewind(glbl);                              
  if(e)             ERROR(412,"trying to rewind trace file")
  for(i = 0; i < ngrp; i++)
       {
       working(msgfun, msgdata, i, ngrp, "creating", "on output file");
       for(j = 0; j < nch; j++)
            {
            e = tracefile_read_header(glbl, head, open_file);
            /*
             * If trace file, return of -2 means sequential trace number did not
             * start at one.  That's ok.
             * ehs   30nov99
             */
          /*
           *if(e != nwords) ERROR(413,"Trace header has wrong number of words")
           */
            if ((e != nwords)
             && ((tracefile_read_header !=
                  (TFReadHeader *) &tracefile_read_header)
              || (e != -2)))
            {
              ERROR(413,"Trace header has wrong number of words")
            }
            ich = j + 1;
            if (tracefile_pickhead >= 1 && tracefile_pickhead <= nwords)
                            pick = 0.001 * head[tracefile_pickhead - 1];
            e = scrs_shot_fill1_spws(ss, head, nwords, pick, ich);
            if(e) ERROR(414,"trying to fill SCRS pickfile shot record")
            }
       e = get_position(ss, i);                         
       if(e) ERROR(415,"trying to find position in SCRS pickfile")
       e = scrs_shot_put(ss);                           
       if(e) ERROR(416,"trying to write shot record")
       ss->igroup[i] = NearestInteger(ss->shot[8]);
       if(i < MAX_CACHE) add_to_memory_cache(ss->cache,
                                   (void*)ss->shot, ss->igroup[i]);
       }
  ss->latest = 0;
  e = get_position(ss, ss->ngrp);                           
  if(e) ERROR(417,"trying to get position\n of end of SCRS pickfile")
  e = scrs_tail_put(ss);                                    
  if(e) ERROR(418,"trying to put tail record\n onto SCRS pickfile")
  e = scrs_head_put(ss);                                    
  if(e) ERROR(419,"trying to write SCRS pickfile\n trace header when finished")
  rewind(ss->stream);
  tracefile_close(glbl);
  strcpy(msg, " ");
  return 0;
}




/*------------- create pickfile from scratch -------------------------*/
/*------------- number of traces differs in different shot profiles --*/
            /* the pickfile is already opened */

#define NGRP_ALLOC_INC 100

static long create_pickfile_unequal(ScrsStruct *ss, MsgFun *msgfun,
      void *msgdata, char *tracefile,
      TFOpen   *tracefile_open    , TFReadHeader *tracefile_read_header,
      TFRewind *tracefile_rewind  , TFClose      *tracefile_close,
      int       tracefile_pickhead, char *msg)
{
  long ngrp, ngrp_alloc, nch, max_nch, ntraces, nwords, incgp, e, i, j, ich;
  long *chs_in_grp;
  float head[MAXW], head9=0.0, head14=0.0, head15=0.0;
  float head35=0.0, head47=0.0, xincgp=0.0, rinc=0.0;
  void *glbl = NULL;
  int open_file = 1;
  float pick = PICK;
  static float zero_head[MAXW] =
       { 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 
         0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 
         0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 
         0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 
         0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 
         0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 
         0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 
         0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F };

  msgfun(msgdata, "Scanning data file for largest shot");

  if(!tracefile || !tracefile_open || !tracefile_read_header ||
     !tracefile_rewind || !tracefile_close)
                      ERROR(401,"Trace file I/O routines not available")
  glbl = tracefile_open(tracefile, &nwords, &ntraces);
  if(!glbl)           ERROR(402,"Trace file cannot be opened")
  if(ntraces <= 0)    ERROR(403,"No traces on trace file")
  if(nwords  < 47)    ERROR(404,"Not enough words in trace headers")

  ngrp_alloc = NGRP_ALLOC_INC;
  assert(chs_in_grp = (long *) malloc((size_t) ngrp_alloc * sizeof(long)));

  i = nch = max_nch = ngrp = 0;
  while(i < ntraces)
       {
       e = tracefile_read_header(glbl, head, open_file);
       open_file = 0;
       /*
        * If trace file, return of -2 means sequential trace number did not
        * start at one.  That's ok.
        * ehs   30nov99
        */
    /* if(e != nwords) ERROR(405,"Trace header has wrong number of words") */
       if ((e != nwords)
        && ((tracefile_read_header != (TFReadHeader *) &tracefile_read_header)
         || (e != -2)))
       {
         ERROR(405,"Trace header has wrong number of words")
       }
       if(max_nch == 0 && nch == 0)
            {
            head9  = head[8];      /* group number */
            head14 = head[13];     /* receiver x distance coord */
            head15 = head[14];     /* receiver y distance coord */
            head35 = head[34];     /* receiver x grid ground pos */
            head47 = head[46];     /* receiver sequential ground pos */
            }
       else if(head[8] != head9)
            {
            assert(max_nch > 0 || nch > 2);
            head9 = head[8];
            if (nch > max_nch)
                 max_nch = nch;
            if (ngrp == ngrp_alloc)
                 {
                 ngrp_alloc += NGRP_ALLOC_INC;
                 assert(chs_in_grp = (long *) realloc((void *) chs_in_grp,
                        (size_t) ngrp_alloc * sizeof(long)));
                 }
            chs_in_grp[ngrp] = nch;
            ngrp++;
            nch = 0;	/* will be incremented at bottom of while loop */
            }
       else if(max_nch == 0 && nch == 1)
            {
            if(ss->typegp == SEQU) xincgp = head[46] - head47;
            else                   xincgp = head[34] - head35;
            rinc = sqrt( (head[13] - head14) * (head[13] - head14) +
                         (head[14] - head15) * (head[14] - head15) );
            }
       i++;
       nch++;
       }

  if (nch > max_nch)
       max_nch = nch;
  if (ngrp == ngrp_alloc)
       {
       ngrp_alloc += NGRP_ALLOC_INC;
       assert(chs_in_grp = (long *) realloc((void *) chs_in_grp,
              (size_t) ngrp_alloc * sizeof(long)));
       }
  chs_in_grp[ngrp] = nch;
  ngrp++;

  assert(max_nch > 2);
  /*
   * Since create_pickfile_unequal is now always called, this assert
   * is no longer valid.
   * ehs   30nov99
   */
/*assert(ntraces != ngrp * max_nch); */
  if(xincgp < 0.0) xincgp = -xincgp;
  if(rinc   < 0.0) rinc   = -rinc  ;
  incgp = NearestInteger(xincgp);
  if(incgp == 0)    incgp = 1;
  rinc = rinc / incgp;                           
  if(rinc  == 0.0)  ERROR(409,"Receiver increment is zero")
  e = scrs_head_fill(ss, ngrp, incgp, rinc, max_nch, TRSH, IZC);
  if(e)             ERROR(410,"trying to fill SCRS pickfile header")
  e = scrs_head_put (ss);                                  
  if(e)             ERROR(411,"trying to put SCRS pickfile header")
  e = tracefile_rewind(glbl);                              
  if(e)             ERROR(412,"trying to rewind trace file")
  for(i = 0; i < ngrp; i++)
       {
       working(msgfun, msgdata, i, ngrp, "creating", "on output file");
       for(j = 0; j < chs_in_grp[i]; j++)
            {
            e = tracefile_read_header(glbl, head, open_file);
            /*
             * If trace file, return of -2 means sequential trace number did not
             * start at one.  That's ok.
             * ehs   30nov99
             */
          /*
           *if(e != nwords) ERROR(413,"Trace header has wrong number of words")
           */
            if ((e != nwords)
             && ((tracefile_read_header !=
                  (TFReadHeader *) &tracefile_read_header)
              || (e != -2)))
            {
              ERROR(413,"Trace header has wrong number of words")
            }
            ich = j + 1;
            if (tracefile_pickhead >= 1 && tracefile_pickhead <= nwords)
                            pick = 0.001 * head[tracefile_pickhead - 1];
            e = scrs_shot_fill1_spws(ss,    head, nwords, pick, ich);
            if(e) ERROR(414,"trying to fill SCRS pickfile shot record")
            }
       for(     ; j < max_nch      ; j++)
            {
            ich = j + 1;
            e = scrs_shot_fill1_spws(ss,zero_head,nwords, PICK, ich);
            if(e) ERROR(414,"trying to fill SCRS pickfile shot record")
            }
       e = get_position(ss, i);                         
       if(e) ERROR(415,"trying to find position in SCRS pickfile")
       e = scrs_shot_put(ss);                           
       if(e) ERROR(416,"trying to write shot record")
       ss->igroup[i] = NearestInteger(ss->shot[8]);
       if(i < MAX_CACHE) add_to_memory_cache(ss->cache,
                                   (void*)ss->shot, ss->igroup[i]);
       }

  free((void *) chs_in_grp);
  ss->latest = 0;
  e = get_position(ss, ss->ngrp);                           
  if(e) ERROR(417,"trying to get position\n of end of SCRS pickfile")
  e = scrs_tail_put(ss);                                    
  if(e) ERROR(418,"trying to put tail record\n onto SCRS pickfile")
  e = scrs_head_put(ss);                                    
  if(e) ERROR(419,"trying to write SCRS pickfile\n trace header when finished")
  rewind(ss->stream);
  tracefile_close(glbl);
  strcpy(msg, " ");
  return 0;
}




/*----------------- open ----------------------------------------------*/

#undef ERROR
#define ERROR(n)                                             \
     {                                                       \
     if(n != 9999) sprintf(msg,                              \
       "Error %d\ntrying to access\nSCRS pickfile", (n));    \
     if(stream1) fclose(stream1);                            \
     if(stream2) fclose(stream2);                            \
     if(ss && ss->stream) fclose(ss->stream);                \
     if(ss) free(ss);                                        \
     if(msgfun) msgfun(msgdata, " ");                        \
     return NULL;                                            \
     }


ScrsStruct *scrs_open(char *filename1, char *filename2, MsgFun *msgfun,
      void *msgdata, long typegp, long status,
      char *msg, char *tracefile,
/*
      void *(*tracefile_open)(), long (*tracefile_read_header)(),
      long (*tracefile_rewind)(), long (*tracefile_close)())
*/
      TFOpen   *tracefile_open  , TFReadHeader *tracefile_read_header,
      TFRewind *tracefile_rewind, TFClose      *tracefile_close,
      int tracefile_pickhead)
{
  ScrsStruct *ss = NULL;
  FILE *stream1 = NULL, *stream2 = NULL;
  long e;


          /*---allocate and initialize the data structure---*/

  ss = (ScrsStruct*)calloc(1,sizeof(ScrsStruct));          if(!ss) ERROR(1)
  ss->cache = create_memory_cache(sizeof(ShotRecord), MAX_CACHE);
                                                           if(!ss) ERROR(111)
  ss->cache_update_flag = TRUE;
  ss->typegp = typegp;
  if(typegp == SEQU)
       {
       ss->sxgp = 46; ss->sygp =  0; ss->rxgp = 47; ss->rygp =  0;
       }
  else if(typegp == GRID)
       {
       ss->sxgp = 33; ss->sygp = 34; ss->rxgp = 35; ss->rygp = 36;
       }
  else
       {
                                                                   ERROR(2)
       }

            /*---read the file(s)---*/

  if(status == FILE_CREATE)         /* create second file from scratch */
       {
       ss->read_only = FALSE;
       ss->stream = fopen(filename2, "w+");        if(!ss->stream) ERROR(3)
       e = create_pickfile(ss, msgfun, msgdata, tracefile,
                 tracefile_open, tracefile_read_header,
                 tracefile_rewind, tracefile_close, tracefile_pickhead, msg);
       if (e == 407)
            {
            e = create_pickfile_unequal(ss, msgfun, msgdata, tracefile,
                   tracefile_open, tracefile_read_header,
                   tracefile_rewind, tracefile_close, tracefile_pickhead, msg);
            if (e) ERROR(9999)
            }
       else if (e)
            {
            ERROR(9999)
            }
       }
  else if(status == FILE_COPY)                /* copy to second file*/
       {
       ss->read_only = FALSE;
       stream1 = fopen(filename1, "r" );              if(!stream1) ERROR(4)
       stream2 = fopen(filename2, "w+");              if(!stream2) ERROR(5)
       e = copy_pickfile(ss, msgfun, msgdata, stream1, stream2);
                                                             if(e) ERROR(e)
       e = fclose(stream1);                           if(e == EOF) ERROR(6)
       ss->stream = stream2;
       }
  else if(status == FILE_READ_ONLY)   /* use first file read-only */
       {
       ss->read_only = TRUE;
       ss->stream = fopen(filename1, "r");         if(!ss->stream) ERROR(7)
       e = read_pickfile(ss, msgfun, msgdata);               if(e) ERROR(e)
       }
  else if(status == FILE_UPDATE)            /* update first file */
       {
       ss->read_only = FALSE;
       ss->stream = fopen(filename1, "r+");        if(!ss->stream) ERROR(8)
       e = read_pickfile(ss, msgfun, msgdata);               if(e) ERROR(e)
       }
  else
       {
                                                                   ERROR(9)
       }
  ss->direction = find_iarray_direction(ss->igroup, ss->ngrp);
  if(msgfun) msgfun(msgdata, " ");
  strcpy(msg, " ");
  return ss;
}




/*------------------------- close -------------------------------------*/

int scrs_close(ScrsStruct *ss)
{
  int e;

  if(!ss) return 0;
  e = scrs_head_put(ss);     if(e != 0  ) return e;
  e = fclose(ss->stream);    if(e == EOF) return 1;
  destroy_memory_cache(ss->cache);
  free(ss);
  return 0;
}


void scrs_abort(ScrsStruct *ss)
{
  if(!ss) return;
  fclose(ss->stream);
  destroy_memory_cache(ss->cache);
  free(ss);
}


/*--------------------- added 6-18-2002 for mike gurch --------------------*/
/*--------------------- added 6-18-2002 for mike gurch --------------------*/
/*--------------------- added 6-18-2002 for mike gurch --------------------*/


/*--------------------------------- getline -------------------------------*/
/*--------------------------------- getline -------------------------------*/
/*--------------------------------- getline -------------------------------*/

#define SCRS1_OK      0
#define SCRS1_EOF    -1
#define SCRS1_ERROR  -7


static int getline (FILE *stream, char *string, int length, char *errmsg)
{
  char *card = readcard(stream);
  if(card)
      {
      if(strlen(card) >= length)
           {
           string[0] = '\0';
           sprintf(errmsg, "card image is longer than max length %d", length);
           return SCRS1_ERROR;
           }
      else
           {
           strcpy(string, card);
           errmsg[0] = '\0';
           return SCRS1_OK;
           }
      }
  else if(feof(stream))
      {
      string[0] = '\0';
      errmsg[0] = '\0';
      return SCRS1_EOF;
      }
  else if(ferror(stream))
      {
      string[0] = '\0';
      sprintf(errmsg, "error %d: %s", errno, strerror(errno));
      return SCRS1_ERROR;
      }
  string[0] = '\0';
  strcpy(errmsg, "line too long or contains null (possible binary data)");
  return SCRS1_ERROR;
}


/*------------------------- scrs_copy_from_foreign ------------------------*/
/*------------------------- scrs_copy_from_foreign ------------------------*/
/*------------------------- scrs_copy_from_foreign ------------------------*/


/*****
   this routine currently reads a foreign columnar file with these columns:
              source coord      receiver coord      pick time in secs
   and creates an equivalent scrs pickfile.
*****/


#define WHOOPS(message)                            \
               { error = TRUE;                     \
                 strcpy(msg, (message));           \
                 goto finishing; }


int scrs_copy_from_foreign
             (const char *filename1, const char *filename2, char *msg)
{

  ScrsStruct *ss = NULL;
  FILE *stream1 = NULL;
  int error = FALSE;
  char string[222];
  char errmsg[222];
  long e, ngrp, nch, incgp, kount, ich;
  float scoord, rcoord, pick, skeep, rkeep, rinc;
  int i;

  float   shot[10];
  float *picks = NULL;
  long   *ixgp = NULL;
  long   *iygp = NULL;
  long   *ioff = NULL;
  long   *elev = NULL;

  strcpy(msg, "no errors");

/*------open foreign file and read thru to get several values------*/

  stream1 = fopen(filename1, "r");
  if(!stream1) WHOOPS("input file open error")

  ngrp  = 0;
  ich   = 0;
  nch   = 0;
  rinc  = 0.0;
  incgp = 1;
  kount = 0;

  while(TRUE)
      {
      e = getline(stream1, string, 222, errmsg);
      if(e == SCRS1_ERROR) WHOOPS(errmsg);
      if(e == SCRS1_EOF) break;
      sscanf(string, "%f%f%f", &scoord, &rcoord, &pick);
      if(ngrp == 0)
          {
          skeep = scoord;
          rkeep = rcoord;
          ich   = 1;
          ngrp  = 1;
          }
      else if(scoord != skeep)
          {
          skeep = scoord;
          rkeep = rcoord;
          ich   = 1;
          ngrp++;
          }
      else
          {
          ich++;
          if(rinc == 0) rinc = rcoord - rkeep;
          }
      nch = MaximumValue(nch, ich);
      kount++;
      }

/*------allocate and initialize scrs data structure------*/

  ss = (ScrsStruct*)calloc(1,sizeof(ScrsStruct));
  if(!ss) WHOOPS("calloc error")

  ss->cache = NULL;
  ss->cache_update_flag = FALSE;
  ss->typegp = GRID;
  ss->sxgp = 11;
  ss->sygp = 12;
  ss->rxgp = 14;
  ss->rygp = 15;
  ss->read_only = FALSE;

/*------open new scrs output file file and write header------*/

  ss->stream = fopen(filename2, "w+");
  if(!ss->stream) WHOOPS("output file open error")

  e = scrs_head_fill(ss, ngrp, incgp, rinc, nch, TRSH, IZC);
  if(e) WHOOPS("trying to fill SCRS pickfile header")

  e = scrs_head_put (ss);
  if(e) WHOOPS("trying to put SCRS pickfile header")

/*------allocate needed arrays------*/

  picks = (float*)malloc(nch * sizeof(float));
  ixgp  = (long *)malloc(nch * sizeof(long ));
  iygp  = (long *)malloc(nch * sizeof(long ));
  ioff  = (long *)malloc(nch * sizeof(long ));
  elev  = (long *)malloc(nch * sizeof(long ));

                      /*   shot[0] = source X gnd pos (hwd 33 or 46).  */
                      /*   shot[1] = hwd 20 (source depth).            */
                      /*   shot[2] = hwd 44 (source uphole time).      */
                      /*   shot[3] = hwd 13 (source elev) - hwd 20.    */
                      /*   shot[4] = apparently not used.              */
                      /*   shot[5] = apparently not used.              */
                      /*   shot[6] = apparently not used.              */
                      /*   shot[7] = source Y gnd pos (hwd 34 or 0).   */
                      /*   shot[8] = hwd 9 (shot profile number).      */
                      /*   shot[9] = apparently not used.              */

/*------read thru foreign file and write new scrs output file------*/

  rewind(stream1);

  ngrp  = 0;
  ich   = 0;

  while(TRUE)
      {
      e = getline(stream1, string, 222, errmsg);
      if(e == SCRS1_ERROR) WHOOPS(errmsg)
      if(e == SCRS1_EOF) break;
      if(str_trimmed_length(string) == 0) continue;
      sscanf(string, "%f%f%f", &scoord, &rcoord, &pick);
      if(ngrp == 0)
          {
          skeep = scoord;
          ich   = 1;
          ngrp  = 1;
          shot[0] = scoord;  /* source X gnd pos (hwd 33 or 46).  */
          shot[1] = 0.0;     /* hwd 20 (source depth).            */
          shot[2] = 0.0;     /* hwd 44 (source uphole time).      */
          shot[3] = 0.0;     /* hwd 13 (source elev) - hwd 20.    */
          shot[4] = 0.0;     /* apparently not used.              */
          shot[5] = 0.0;     /* apparently not used.              */
          shot[6] = 0.0;     /* apparently not used.              */
          shot[7] = 0.0;     /* source Y gnd pos (hwd 34 or 0).   */
          shot[8] = ngrp;    /* hwd 9 (shot profile number).      */
          shot[9] = 0.0;     /* apparently not used.              */
          memset(picks, 0, nch * sizeof(float));
          memset(ixgp , 0, nch * sizeof(long ));
          memset(iygp , 0, nch * sizeof(long ));
          memset(ioff , 0, nch * sizeof(long ));
          memset(elev , 0, nch * sizeof(long ));
          if(ngrp < 50)
              {
              for(i = 0; i < nch; i++) { elev[i] = 1; }
              }
          else
              {
              for(i = 0; i < nch; i++) { elev[i] = 2; }
              }
          picks[ich-1] = pick;
          ixgp [ich-1] = NearestInteger(rcoord);
          ioff [ich-1] = NearestInteger(rcoord - scoord);
          ioff [ich-1] = AbsoluteValue(ioff[ich-1]);
          }
      else if(scoord != skeep)
          {
          e = scrs_shot_fill(ss, shot, picks, ixgp, iygp, ioff, elev);
          if(e) WHOOPS("error in scrs_shot_fill")

          e = get_position(ss, ngrp-1);
          if(e) WHOOPS("trying to find position in SCRS pickfile")

          e = scrs_shot_put(ss);
          if(e) WHOOPS("trying to write shot record")

          ss->igroup[ngrp-1] = NearestInteger(ss->shot[8]);

          skeep = scoord;
          ich   = 1;
          ngrp++;
          shot[0] = scoord;  /* source X gnd pos (hwd 33 or 46).  */
          shot[1] = 0.0;     /* hwd 20 (source depth).            */
          shot[2] = 0.0;     /* hwd 44 (source uphole time).      */
          shot[3] = 0.0;     /* hwd 13 (source elev) - hwd 20.    */
          shot[4] = 0.0;     /* apparently not used.              */
          shot[5] = 0.0;     /* apparently not used.              */
          shot[6] = 0.0;     /* apparently not used.              */
          shot[7] = 0.0;     /* source Y gnd pos (hwd 34 or 0).   */
          shot[8] = ngrp;    /* hwd 9 (shot profile number).      */
          shot[9] = 0.0;     /* apparently not used.              */
          memset(picks, 0, nch * sizeof(float));
          memset(ixgp , 0, nch * sizeof(long ));
          memset(iygp , 0, nch * sizeof(long ));
          memset(ioff , 0, nch * sizeof(long ));
          memset(elev , 0, nch * sizeof(long ));
          if(ngrp < 50)
              {
              for(i = 0; i < nch; i++) { elev[i] = 1; }
              }
          else
              {
              for(i = 0; i < nch; i++) { elev[i] = 2; }
              }
          picks[ich-1] = pick;
          ixgp [ich-1] = NearestInteger(rcoord);
          ioff [ich-1] = NearestInteger(rcoord - scoord);
          ioff [ich-1] = AbsoluteValue(ioff[ich-1]);
          }
      else
          {
          ich++;
          picks[ich-1] = pick;
          ixgp [ich-1] = NearestInteger(rcoord);
          ioff [ich-1] = NearestInteger(rcoord - scoord);
          ioff [ich-1] = AbsoluteValue(ioff[ich-1]);
          }
      }

  e = scrs_shot_fill(ss, shot, picks, ixgp, iygp, ioff, elev);
  if(e) WHOOPS("error in last scrs_shot_fill")

  e = get_position(ss, ngrp-1);
  if(e) WHOOPS("trying to find last position in SCRS pickfile")

  e = scrs_shot_put(ss);
  if(e) WHOOPS("trying to write last shot record")

  ss->igroup[ngrp-1] = NearestInteger(ss->shot[8]);

/*------finish up and return------*/

  ss->latest = 0;

  e = get_position(ss, ss->ngrp);    
  if(e) WHOOPS("trying to get position\n of end of SCRS pickfile")

  e = scrs_tail_put(ss);
  if(e) WHOOPS("trying to put tail record\n onto SCRS pickfile")

  e = scrs_head_put(ss);
  if(e) WHOOPS("trying to write SCRS pickfile\n trace header when finished")

finishing:

  if(picks) free(picks);
  if(ixgp)  free(ixgp);
  if(iygp)  free(iygp);
  if(ioff)  free(ioff);
  if(elev)  free(elev);

  if(ss)
      {
      if(ss->stream)
          {
          e = fclose(ss->stream);
          if(e == EOF) return TRUE;
          }
      free(ss);
      }

  if(stream1)
      {
      e = fclose(stream1);
      if(e == EOF) return TRUE;
      }

  return error;
}



/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/
/*------------------------- end ---------------------------------------*/

