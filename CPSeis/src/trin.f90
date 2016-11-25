!<CPS_v1 type="PROCESS"/>


!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>


!<brief_doc>
!-------------------------------------------------------------------------------
!                         C P S   P R O C E S S
!
! Name       : TRIN       (TRace INput from disk)
! Category   : io
! Written    : 1999-08-26   by: Bill Menger
! Revised    : 2007-11-29   by: Bill Menger
! Maturity   : beta
! Purpose    : Read seismic traces from one or more disk files.
! Portability: No known limitations.
! Parallel   : Yes (in SS)
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! [Note that not all the parameters shown here will have been implemented when
! testing begins for TRIN.  Some may prove to be undesirable or unnecessary.]
!
!
! TRIN reads traces from a disk file, either randomly or sequentially. The
! PATHNAMES is the only input parameter that must be entered; defaults can be
! used for all other parameters.  TROT is the corresponding process for writing
! disk files.
!
! File Identification
!
! Use PATHNAMES = [path/subpath/]filename[.ext] (The "[]" are not typed in,
!                                               they indicate optional parts of
!                                               the pathname.)
!
! To check the PATHNAMES and load special globals that were written to the file
! from its creating job, use the CHECKFILE parameter.  This checks the file
! from CFE by opening it, reading the header(s), and loading special globals
! for the subsequent processing steps.
!
!   To read native CWP files, (SU file format), you must run the SU program:
!   $segyhdrs <native_file.su
!   $segywrite tape=myfile.segy convert=0 <native_file.su
!   Then run trin with PATHNAMES=myfile.segy
!
! Abort Conditions
!
! ABORT options are: STRICT, NORMAL and LENIENT conditions for aborting if file
! globals do not match globals in job.  TRIN prints a summary of the globals
! for each file it reads and also prints warning or error messages if file
! globals do not match job globals.
!
! History Selection
!
! HISTORY options to control transfer of histories from file are YES or NO.
!
! Input Merging
!
! ORDER_LOCAL and ORDER_PREV
!   ORDER_LOCAL tells TRIN how to sequence traces from within its group of files
!   from the input file list (within this particular instance of TRIN).
!
!   ORDER_PREV tells TRIN how to sequence traces that it will bring into the job
!   with respect to the other processes in the job. ORDER_PREV is not restricted
!   unless on the first process, where it must be NEW. Any other process (IPN>3)
!   it can be either MERGE or APPEND.  IF Merge, it will MERGE 1 trace onto the
!   end of the group of traces coming to it.  Ex:
!   2 traces come from previous process.  TRIN-MERGE adds one trace, passing out
!   3 traces:  the first two have been processed by previous modules, the third
!   is being added from this TRIN's input file(s).
!   IF APPEND, it will wait until the processing loop has received all of its
!   traces, then will add all of the traces (one at a time) from its files at
!   the end of the processing stream. Ex:
!   2 traces come from previous process, then a NO_MORE_TRACES flag is sent.
!   Now, this TRIN-APPEND will add 1 trace, and on the return loop, will add a
!   second trace, etc. until all traces are input.
!   You can intersperse APPEND and MERGE and even put processing steps in-
!   between TRINs to do comparison processing.  Ex:
!   TRIN-NEW XP1 TRIN-APPEND XP2.  Your output will be XP1 + XP2 applied to the
!   first set of traces, but only XP2 applied to the second set.
!   *** In MERGE mode where you have two files of different numbers of traces,
!   the traces are MERGEd until one file runs out of traces, then the remaining
!   traces from the other file are APPENDED at the end.  Example:
!   File1 (10 traces), denoted by a-1, a-2, a-3...a-10
!   File2 (5 traces), denoted by  b-1, b-2, b-3, b-4, b-5
!   file3 (3 traces), denoted by  c-1, c-2, c-3
!   Job 1
!   (TRIN1(order_prev=new), TRIN2 (order_prev=MERGE), TRIN3(order_prev=MERGE)
!   Traces will be:
!   a-1,b-1,c-1,a-2,b-2,c-2,a-3,b-3,c-3,a-4,b-4,a-5,b-5,a-6,a-7,a-8,a-9,a-10
!
!   Job 2
!   (TRIN1(order_prev=new), TRIN2 (order_prev=APPEND), TRIN3(order_prev=APPEND)
!   Traces will be:
!   a-1,a-2,a-3,a-4,a-5...a-10,b-1,b-2,...b-5,c-1,c-2,c-3
!
!   Job 3
!   (TRIN1(order_prev=new), TRIN2 (order_prev=MERGE), TRIN3(order_prev=APPEND)
!   Traces will be:
!   a-1,b-1,a-2,b-2,a-3,b-3,a-4,b-4,a-5,b-5,a-6,a-7,a-8,a-9,a-10,c-1,c-2,c-3
!
! Trace Selection
!
! TR_MAX is the maximum number of traces to read.  Set TR_MAX to 0 to read no
! traces, set TR_MAX to a very large number to read all traces.
!
! DO_SKIP parameters are SKIP_INIT, NUM_DO and NUM_SKIP.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!                             trace lengths
!
!  If you read a file whose traces are shorter than your job's traces, they
!  will be padded with 0.0's
!
!  If you read a file whose traces are longer than your job's traces, they
!  will be truncated.
!
!                             start time (tstrt)
!
!  If you read a file whose tim_beg or tstrt is not = your job's tstrt, then:
!
!  If your job tstrt > file tim_beg, you get only those data values that are
!  within the range of your job's tstrt to your job's end-of-trace (which is
!  tstrt + (ndpt-1)/dt).
!
!  If your job tstrt < file tim_beg, you will get 0.0's in the first part of
!  your trace, until your samples fall within the values that are recorded on
!  disk.
!
!  If you are trying to RESET a tstart, you should read the traces using a tstrt
!  that is == to the file's tstrt, THEN do a SHIFT operation, THEN process the
!  data.  Otherwise you will simply have zero values for all samples that were
!  not recorded on the file.  The file header contains a start and end time for
!  the traces.
!
!                             Merging Traces
!
! ORDER_LOCAL and ORDER_PREV
!
!                            SEGY Header Words
! When TRIN reads a SEGY file, it maps SEGY headers to CPS headers using the
! following mapping.  For byte offsets of the SEGY header words, either see
! below (where the description of SEGY->CPS mapping is found, or look in the
! SEGY documentation, or look in the Seismic Unix documentation from the CWP at
! Colorado School of mines.  Their naming convention for segy headers is used.
!
! If CPS creates the SEGY file with TROT, conoco job globals are written to
! unused portions of the binary header, and are detected by TRIN.  The LAV is
! also saved for each trace, as is the MIDPOINT x,y,elev, etc.  (see midpoint
! information below.)
!
! ----------- MAPPING CPS <==> SEGY  sort by segy_byte#--(sort by CPS# is below)
! SEGY BYTES CPS HDR CPS                   SEGY
! BEG:END    NUMBER  NAME                  NAME
! -------     --     -----------------     -------
! 001:004     01     sequence              tracl
! 005:008     01     sequence              tracr
! 009:012     03     current_group         fldr
! 013:016     04     current_channel       tracf
! 017:020     29     source_shotpoint      ep
! 021:024     07     midpoint_xgrid        cdp
! 021:024     37     midpoint_shotpoint    cdp
! 025:028     38     midpoint_line         cdpt
! 029:030                                  trid
! 031:032                                  nvs
! 033:034     05     fold                  nhs
! 035:036                                  duse
! 037:040     06     offset                offset
! 041:044     16     receiver_elev         gelev
! 045:048     13     source_elev           selev
! 049:052     20     source_depth          sdepth
! 053:056                                  gdel
! 057:060                                  sdel
! 061:064                                  swdep
! 065:068                                  gwdep
! 069:070                                  scalel
! 071:072                                  scalco
! 073:076     11     source_xloc           sx
! 077:080     12     source_yloc           sy
! 081:084     14     receiver_xloc         gx
! 085:088     15     receiver_yloc         gy
! 089:090                                  counit
! 091:092                                  wevel
! 093:094                                  swevel
! 095:096     44     source_uptime         sut
! 097:098     45     receiver_uptime       gut
! 099:100     39     pre                   sstat
! 101:102     40     post                  gstat
! 103:104     43     cum_resid_static      tstat
! 105:106                                  laga
! 107:108                                  lagb
! 109:110                                  delrt
! 111:112     02     top_mute              muts
! 113:114     64     bottom_mute           mute
! 115:116                                  ns
! 117:118                                  dt
! 119:120                                  gain
! 121:122                                  igc
! 123:124                                  igi
! 125:126                                  corr
! 127:128                                  sfs
! 129:130                                  sfe
! 131:132                                  slen
! 133:134                                  styp
! 135:136                                  stas
! 137:138                                  stae
! 139:140                                  tatyp
! 141:142                                  afilf
! 143:144                                  afils
! 145:146                                  nofilf
! 147:148                                  nofils
! 149:150                                  lcf
! 151:152                                  hcf
! 153:154                                  lcs
! 155:156                                  hcs
! 157:158                                  year
! 159:160                                  day
! 161:162                                  hour
! 163:164                                  minute
! 165:166                                  sec
! 167:168                                  timbas
! 169:170     30     scratch_30            trwf
! 171:172                                  grnors
! 173:174                                  grnofr
! 175:176                                  grnlof
! 177:178                                  gaps
! 179:180                                  otrav
! 237:240     08     midpoint_ygrid        unused1
!
!
!        Documentation on User Defined Header Mapping can be found in
!                         Primitive MAPSEGY
!
! ----------- MAPPING CPS <==> SEGY sort by CPS hdr#-------------
! SEGY BYTES CPS HDR CPS                   SEGY
! BEG:END    NUMBER  NAME                  NAME
! -------     --     -----------------     -------
! 001:004     01     sequence              tracl
! 005:008     01     sequence              tracr
! 111:112     02     top_mute              muts
! 009:012     03     current_group         fldr
! 013:016     04     current_channel       tracf
! 033:034     05     fold                  nhs
! 037:040     06     offset                offset
! 021:024     07     midpoint_xgrid        cdp
! 237:240     08     midpoint_ygrid        unused1
! 073:076     11     source_xloc           sx
! 077:080     12     source_yloc           sy
! 045:048     13     source_elev           selev
! 081:084     14     receiver_xloc         gx
! 085:088     15     receiver_yloc         gy
! 041:044     16     receiver_elev         gelev
! 193:200     17     midpoint_xloc         mp_xloc
! 201:208     18     midpoint_yloc         mp_yloc
! 209:216     19     midpoint_elev         mp_elev
! 049:052     20     source_depth          sdepth
! 189:192     25     lav                   lav (dbl-precision)
! 233:236     25     lav                   lav (dbl-precision)
! 017:020     29     source_shotpoint      ep
! 169:170     30     scratch_30            trwf
! 021:024     37     midpoint_shotpoint    cdp
! 025:028     38     midpoint_line         cdpt
! 099:100     39     pre                   sstat
! 101:102     40     post                  gstat
! 103:104     43     cum_resid_static      tstat
! 095:096     44     source_uptime         sut
! 097:098     45     receiver_uptime       gut
! 117:118                                  dt
! 029:030                                  trid
! 035:036                                  duse
! 115:116                                  ns
! 061:064                                  swdep
! 065:068                                  gwdep
! 113:114     64     bottom_mute           mute
!
! Not all CPS header words have a corresponding SEGY header.  If you create a
! file with TROT in SEGY format, and read that file with TRIN, you will lose
! some header words.  Also, SEGY header words are integer, so you may lose
! precision making the round trip.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! None.  Process is a trace supplying process.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This is a trace-supplying process.  Traces may be merged with or appended to
! sets of traces from previous processes.
!
! This process does not alter input traces except for the tail mute.
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! IPN      process number
! MAXTR    max number of traces input/output
! NWIH     number of words in trace header
! NDPT     number of sample values in trace
! TSTRT    starting time on trace
! DT       trace sample interval
! GRID     grid transformation structure
! GATHERED Are traces gathered?             If order_prev /= MERGE, set false.
!                                           otherwise, leave alone.
!
! TRIN will check agreement of some or all globals in the file with those in
! the job and, depending on value of ABORT, may abort if they disagree.
! TRIN will load into the job optional globals if they are present in the file
! in a "jobglobals" section and the file is a "trcio" type file.
!
! !!!---These globals MAY EFFECT subsequent processes---!!!
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
! 1       Sequential Trace Count     set to sequential trace number PASSED out
!                                    of TRIN.
! 2       Head Mute                  Set >= 1 && <= ndpt
! 3       Current gather             Set by SORT option
! 4       Number within gather       Set by SORT option
! 25      HDR_LAV                    reset.
! 31      scratch                    Non zero if first trace in a file.
! 64      Tail mute                  Set >= headmute && <= ndpt
! HDR_GROUP Input file index         if OPT_GROUP=YES header word HDR_GROUP
!                                    is set to the input file index 
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date       Author        Description
!     ----       ------        -----------
! 84. 2007-11-29 Bill Menger   Modified print statement on resetting maxtrc.
! 83. 2007-11-20 Stoeckley     Remove INCR from the row "START END INCR" to
!                               eliminate scrollbars from the SeisSpace menu.
! 82. 2007-11-08 Stoeckley     Changed two options from APP to APPEND before
!                               calling pc_put so the parameter cache would
!                               not complain when it encounters the older
!                               spelling APP.
! 81. 2007-07-17 Bill Menger   Modified the GUI (with Tom's help.)
! 80. 2007-07-10 Bill Menger   Added parallel modes and SeisSpace parameters, 
!                              fixed bug in SS that disallowed parallel or
!                              multiple jobs running simultaneously.
! 79. 2006-08-29 Stoeckley     Add tab groups to HelpSection.
! 78. 2006-08-24 D. Glover     Added NULLIFY statements for Intel compiler.
! 77. 2006-07-13 Bill Menger   Modified the order_prev method so that a file
!                              will merge with others then if it has more traces
!                              it will continue to supply traces as if it were
!                              in the append mode after the other trace
!                              supplying processes are finished.
! 76. 2006-05-16 SMCook        Moved 'use trcio_module' statement to the
!                               beginning of the 'use list' to force the
!                               compiler to honor new optional argument
!                               lbo_version in trcio_open function.
! 75. 2006-01-10 B. Menger     Removed unused variables.
! 74. 2005-07-11 Bill Menger   Added code to allow merge to keep reading traces
!                              until all files are empty, instead of stopping
!                              when the first file is at EOF
! 73. 2004-12-16 Bill Menger   Modified NTR output so that a TRIN will not set
!                              NTR to NO_MORE_TRACES when it is out of data 
!                              IF any previous process has passed traces to it.
!                              This allows TRIN to run dry before a previous
!                              trace-generating process.
! 72. 2004-12-15 Hanson        Add len_group.
! 71. 2004-09-21 Hanson        Add opt_group, hdr_group.
! 70. 2004-08-23 Bill Menger   Merged parm screens, added error msg on bad trc.
! 69. 2004-04-06 SMCook        Changed case statement to include LBO when
!                               handling jobglobals.
! 68. 2004-02-05 Menger        Remove (line 1625) the call to set lav.  Trcio
!                              handles this, and puts negative numbers in it
!                              when data is corrupted, so we don't want to
!                              override that feature.
!                              Added code to return on pc_error instead of
!                              ignoring errors.
! 67. 2004-01-21 Menger        Change tr-max setting to reset in the odd case
!                              where a do-skip pattern causes the expected file
!                              size needed to exceed 2^31.  This happens when a
!                              person uses TRMAX=999999999 and SKIP=999.  When
!                              multiplied, they give the "size" of file that
!                              must be traversed in order to hit that TR-MAX.
!                              If that size > 2^31 then maxthru turned out to
!                              be < 0 because of integer rollover and I didn't
!                              set the tr-max because maxthru was "smaller" than
!                              the file size.
! 66. 2003-10-31 Menger        Force order_prev=NEW if ipn <= 3.
! 65. 2003-10-29 Goodger       Fix problem creating %trin_filenames file which
!                              is used in velocity_model permsaves.  The
!                              write was moved from the "number of files"
!                              loop in revision 63 to a history card loop
!                              which was too large, and thus wrote junk to
!                              %trin_filenames file.
! 64. 2003-10-03 Bill Menger   Added code to abort if EOF prematurely hit.
! 63. 2003-07-29 SMCook        Fixed array violation at point where potentially
!                              long filenames were written to history (replaced
!                              logic where trin was building its own cards with
!                              logic that uses the cardset primitive).
! 62. 2003-01-23 Ed Schmauch   Improved error message for initial skip too big.
! 61. 2002-10-25 Goodger       Add use getlun_module.
! 60. 2002-08-26 K. Goodger    Write input file names to the history file.
! 59. 2002-07-30 K. Goodger    Flag scratch header 31 to indicate the first
!                              trace in a file if velocity model files are
!                              being read.
!                              TTROT will use this for the
!                              PS_VMOD option, to know when to write an EOF
!                              when multiple files are saved.
!                              Output a file called %trin_filenames.  This
!                              file will be saved along with processing
!                              records by TTROT if a velocity model permsave.
! 58. 2002-05-28 Ed Schmauch   Added multifile, keeping downward compatibility.
! 57. 2002-04-09 Ed Schmauch   No direct calls to cmprio_module, only indirct
!                              access through trcio_module.
! 56. 2002-02-04 Ed Schmauch   Removed old multifile options.
! 55. 2001-11-08 Stoeckley     Add file selection dialog and status line for
!                               PATHNAME.
! 54. 2001-11-06 Ed Schmauch   Removed unnecessary use segy_module.  Updated
!                              documentation to reflect changes in segy headers.
! 53. 2001-10-18 Bill Menger   Fix number of files that are open on CFE by
!                              saving the checkfile status as a logical.
! 52. 2001-08-27 K. Goodger    Replace call to trcio_read_history with
!                              trcio_read_history_cards.
! 51. 2001-06-05 Ed Schmauch   Added nint to first_sample calculation
!                              in trin_read_trace.
! 50. 2001-04-30 K. Goodger    Move segy header mapping to primitive
!                              mapsegy.
! 49. 2001-03-15 Bill Menger   Fixed bug in the "APP" mode.
! 48. 2001-02-28 Bill Menger   Removed unused object elements.
! 47. 2001-02-21 Bill Menger   Added trace compression.
! 46. 2001-01-11 Bill Menger   Added dumpheader call on file open, added hist-
!                              ory calls.
! 45. 2000-12-13 Bill Menger   Removed pc_info diagnostic calls.
! 44. 2000-12-12 Bill Menger   Added skip_wrapup flag, remove wrapped_up flag.
! 43. 2000-11-13 Bill Menger   Added KLUDGE file naming support for cluster
!                              system's method of 2gig limit.
! 42. 2000-11-09 R.S.Day       Added optional SEGY custom header mapping
! 41. 2000-10-24 Bill Menger   *Added secondary sort option on pri-sec multiple
!                               file option.
! 40. 2000-10-04 Bill Menger   *changed documentation only.
! 39. 2000-10-03 Bill Menger   Added mode=multiple option,fixed do-skip.
! 38. 2000-09-28 Bill Menger   Modified FE with checkfile option.
! 37. 2000-09-26 Bill Menger   Added jobglobals to file open portion, modified
!                              SEGY documentation.
! 36. 2000-09-05 Bill Menger   Added put-control to update function, removed
!                              from the trap for obj%merge.
! 35. 2000-08-24 Bill Menger   Added ability to modify tr_max based on file
!                              size.
! 34. 2000-08-17 Bill Menger   Added history capability.
! 33. 2000-07-18 Bill Menger   Removed requirement for extension to file name.
! 32. 2000-07-07 Bill Menger   Added "QTROT format" file support.
! 31. 2000-06-27 Stoeckley     Added test for keyword present before getting
!                              globals NUMTR and GATHERED to circumvent a
!                              problem with a new capability in PC.
! 30. 2000-05-24 Bill Menger   Fixed print statement to give correct number of
!                              traces upon hitting EOF.
! 29. 2000-05-08 Bill Menger   Fixed update function, modified print* statement
! 28. 2000-05-04 Bill Menger   Added dooskip code and string_compress_blanks.
! 27. 2000-04-19 Bill Menger   Added gui code.
! 26. 2000-02-24 Bill Menger   Fixed pc-update_state so message stops printing.
! 25. 2000-02-21 Bill Menger   Tried to get set sensitive flags to work right.
! 24. 2000-02-18 Bill Menger   Fixed bug in size of wtype combo box variable.
! 23. 2000-02-17 Bill Menger   Modified EOF message, Added documentation,
!                              changed variable names, disabled some fields.
! 22. 2000-02-14 Bill Menger   Modified inactive fields, etc.
! 21. 2000-02-04 Bill Menger   Added gathered flag.
! 20. 2000-02-03 Bill Menger   Added segy documentation
! 19. 2000-02-03 Bill Menger   Modified traps to use kw arg, removed pathname
!                              pieces from front end and parm list and help.
! 18. 2000-01-27 Bill Menger   Modified to allow tstart /= 0.0,fixed a bug in
!                              numtr for non-merge jobs, and added some doc.
!                              Added sequential trace count.
! 17. 2000-01-21 Bill Menger   Added num_opt_ftype,num_opt_wtype,num_opt_force.
!                              and associated parameters for FE.
! 16. 2000-01-21 Bill Menger   Modified need_request for APP mode.
! 15. 2000-01-18 Bill Menger   Changed the need_request argument.
! 14. 2000-01-12 Bill Menger   Added pathname primitive calls.
! 13. 2000-01-11 Bill Menger   Added code for front-end display to work right.
! 12. 2000-01-07 Bill Menger   Removed internal calls to trin_delete!!!
!                              Upgraded header reading facility for tfils.
! 11. 2000-01-04 Bill Menger   Modified create/update to allow FE calls.
! 10. 1999-12-30 Bill Menger   Changed to comply with new documentation.
!  9. 1999-12-08 Bill Menger   Modified delete function,added ident.
!  8. 1999-12-08 Bill Menger   set obj%wrapped_up=true on wrapup.
!  7. 1999-12-01 Bill Menger   set obj%numtr = 0 on initialization.
!  6. 1999-10-18 Bill Menger   replaced all pc_info calls to not include calls
!                              to the "trim" function (it dies on solaris.) &
!                              added obj%numtr = obj%numtr + 1 before pc_put.
!  5. 1999-10-14 Bill Menger   fixed wrapup bug.
!  4. 1999-10-07 Bill Menger   Finished off functionality.
!  3. 1999-10-01 Bill Menger   Modified to use trcio.
!  2. 1999-08-26 Bill Menger   Converted from old system.
!  1. 1999-08-01 Bill Menger   Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
! (File pathname or sections thereof may be platform dependent.)
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!    NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!    NTR == NEED_TRACES    if this process needs more traces.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                    ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!                            SEGY Header Words
! When TRIN reads a SEGY file, it maps SEGY headers to CPS headers using the
! following mapping. (See also TROT, SEGY, or look in the Seismic Unix
! documentation from the CWP at Colorado School of mines.  Their naming
! convention for segy headers is used.
!
!               CPS HEADERS from SEGY headers (used in TRIN)
!
!    top_mute    = 1 + nint((segyhd%muts*1d-3 - tstart)/(segyhd%dt*1D-6))
!    bottom_mute = 1 + nint((segyhd%mute*1d-3 - tstart)/(segyhd%dt*1D-6))
!    ! calculate the scalers
!    fescale = 1.0
!    if (segyhd%scalel /= 0 .and. &
!        segyhd%scalel > -10001 .and. &
!        segyhd%scalel <  10001) then
!      if(segyhd%scalel > 0 ) then
!        fescale = segyhd%scalel*fescale
!      else
!        fescale = -fescale/segyhd%scalel
!      endif
!    endif
!    fxscale = 1.0
!    if (segyhd%scalco /= 0 .and. &
!        segyhd%scalco > -10001 .and. &
!        segyhd%scalco <  10001) then
!      if(segyhd%scalco > 0 ) then
!        fxscale = segyhd%scalco*fxscale
!      else
!        fxscale = -fxscale/segyhd%scalco
!      endif
!    endif
!    !                                                cps <- SEGY bytes
!    !---                                             head#  stb:enb
!    cpshd(HDR_SEQUENCE) = segyhd%tracl               ! 1 <- 001:004
!    cpshd(HDR_TOP_MUTE) = top_mute                   ! 2 <- 111:112 derived
!    cpshd(HDR_CURRENT_GROUP) = segyhd%fldr           ! 3 <- 009:012
!    cpshd(HDR_CURRENT_CHANNEL) = segyhd%tracf        ! 4 <- 013:016
!    cpshd(HDR_FOLD) = segyhd%nhs                     ! 5 <- 033:034
!    cpshd(HDR_OFFSET) = segyhd%offset                ! 6 <- 037:040
!    cpshd(HDR_MIDPOINT_XGRID    ) = segyhd%mp_xgrid  ! 7 <- 217:224
!    cpshd(HDR_ORIGINAL_GROUP) = segyhd%fldr          ! 9 <- 009:012
!    cpshd(HDR_ORIGINAL_CHANNEL) = segyhd%tracf       !10 <- 013:016
!    cpshd(HDR_SOURCE_XLOC) = fxscale*segyhd%sx       !11 <- 073:076
!    cpshd(HDR_SOURCE_YLOC) = fxscale*segyhd%sy       !12 <- 077:080
!    cpshd(HDR_SOURCE_ELEV) = fescale*segyhd%selev    !13 <- 045:048
!    cpshd(HDR_RECEIVER_XLOC) = fxscale*segyhd%gx     !14 <- 081:084
!    cpshd(HDR_RECEIVER_YLOC) = fxscale*segyhd%gy     !15 <- 085:088
!    cpshd(HDR_RECEIVER_ELEV) = fescale*segyhd%gelev  !16 <- 041:044
!    cpshd(HDR_MIDPOINT_XLOC) = .5*  &
!                     ( cpshd(HDR_SOURCE_XLOC) + &
!                   cpshd(HDR_RECEIVER_XLOC) )        !17 <-         derived
!    cpshd(HDR_MIDPOINT_YLOC) = .5*  &
!                     ( cpshd(HDR_SOURCE_YLOC) + &
!                   cpshd(HDR_RECEIVER_YLOC) )        !18 <-         derived
!    cpshd(HDR_MIDPOINT_ELEV) = .5*  &
!                     ( cpshd(HDR_SOURCE_ELEV) + &
!                   cpshd(HDR_RECEIVER_ELEV) )        !19 <-         derived
!    cpshd(HDR_SOURCE_DEPTH) = fescale*segyhd%sdepth  !20 <- 049:052
!    cpshd(HDR_SOURCE_SHOTPOINT) = segyhd%ep          !29 <- 017:020
!    cpshd(HDR_MIDPOINT_SHOTPOINT) =  segyhd%cdp      !37 <- 021:024
!    cpshd(HDR_MIDPOINT_LINE     ) =  segyhd%cdpt     !38 <- 025:028
!    cpshd(HDR_PRE) =  segyhd%sstat                   !39 <- 099:100
!    cpshd(HDR_POST) = segyhd%gstat                   !40 <- 101:102
!    cpshd(HDR_CUM_RESID_STATIC) = segyhd%tstat       !43 <- 103:104
!    cpshd(HDR_SOURCE_UPTIME) = segyhd%sut            !44 <- 095:096
!    cpshd(HDR_RECEIVER_UPTIME) = segyhd%gut          !45 <- 097:098
!!   calculate trace scaling.
!    if (segyhd%trwf > 0 ) then
!      cpshd(HDR_SCRATCH_30) = 2**(-segyhd%trwf)      !30 <- 169:170 derived
!    else
!      cpshd(HDR_SCRATCH_30) = 1.0
!    endif
!    cpshd(HDR_BOTTOM_MUTE) = bottom_mute             !64 <- 113:114 derived
!
!    **************** When TROT writes to SEGY, the following mapping is done:
!    fescale = 1.0
!    fxscale = 1.0
!    segyhd%tracl               = cpshd(HDR_SEQUENCE)
!    segyhd%tracr               = cpshd(HDR_SEQUENCE)
!    segyhd%fldr                = cpshd(HDR_CURRENT_GROUP)
!    segyhd%tracf               = cpshd(HDR_CURRENT_CHANNEL)
!    segyhd%ep                  = cpshd(HDR_SOURCE_SHOTPOINT)
!    segyhd%cdp                 = cpshd(HDR_MIDPOINT_SHOTPOINT)
!    segyhd%cdpt                = cpshd(HDR_MIDPOINT_LINE)
!    segyhd%trid                = 1
!    segyhd%nvs                 = 0
!    segyhd%nhs                 = cpshd(HDR_FOLD)
!    segyhd%duse                = 0
!    segyhd%offset              = cpshd(HDR_OFFSET)
!    segyhd%gelev               = cpshd(HDR_RECEIVER_ELEV)/fescale
!    segyhd%selev               = cpshd(HDR_SOURCE_ELEV)/fescale
!    segyhd%sdepth              = cpshd(HDR_SOURCE_DEPTH)/fescale
!    segyhd%gdel                = 0
!    segyhd%sdel                = 0
!    segyhd%swdep               = 0
!    segyhd%gwdep               = 0
!    if(fescale >= 1.0 ) then
!      segyhd%scalel            = nint(fescale)
!    else
!      segyhd%scalel            = -nint(1./fescale)
!    endif
!    if(fxscale >= 1.0 ) then
!      segyhd%scalco            = nint(fxscale)
!    else
!      segyhd%scalco            = -nint(1./fxscale)
!    endif
!    segyhd%sx                  = cpshd(HDR_SOURCE_XLOC)/fxscale
!    segyhd%sy                  = cpshd(HDR_SOURCE_YLOC)/fxscale
!    segyhd%gx                  = cpshd(HDR_RECEIVER_XLOC)/fxscale
!    segyhd%gy                  = cpshd(HDR_RECEIVER_YLOC)/fxscale
!    segyhd%counit              = 0
!    segyhd%wevel               = 0
!    segyhd%swevel              = 0
!    segyhd%sut                 = cpshd(HDR_SOURCE_UPTIME)
!    segyhd%gut                 = cpshd(HDR_RECEIVER_UPTIME)
!    segyhd%sstat               = cpshd(HDR_PRE)
!    segyhd%gstat               = cpshd(HDR_POST)
!    segyhd%tstat               = cpshd(HDR_CUM_RESID_STATIC)
!    segyhd%laga                = 0
!    segyhd%lagb                = 0
!    segyhd%delrt               = 0
!    segyhd%muts                = 1e3*(tstart + (cpshd(HDR_TOP_MUTE)-1)*dt)
!    segyhd%mute                = 1e3*(tstart + (cpshd(HDR_BOTTOM_MUTE)-1)*dt)
!    segyhd%ns                  = ns
!    segyhd%dt                  = dt*1.e6
!    segyhd%gain                = 0
!    segyhd%igc                 = 0
!    segyhd%igi                 = 0
!    segyhd%corr                = 0
!    segyhd%sfs                 = 0
!    segyhd%sfe                 = 0
!    segyhd%slen                = 0
!    segyhd%styp                = 0
!    segyhd%stas                = 0
!    segyhd%stae                = 0
!    segyhd%tatyp               = 0
!    segyhd%afilf               = 0
!    segyhd%afils               = 0
!    segyhd%nofilf              = 0
!    segyhd%nofils              = 0
!    segyhd%lcf                 = 0
!    segyhd%hcf                 = 0
!    segyhd%lcs                 = 0
!    segyhd%hcs                 = 0
!    segyhd%year                = 0
!    segyhd%day                 = 0
!    segyhd%hour                = 0
!    segyhd%minute              = 0
!    segyhd%sec                 = 0
!    segyhd%timbas              = 0
!    segyhd%trwf                = 0
!    segyhd%grnors              = 0
!    segyhd%grnofr              = 0
!    segyhd%grnlof              = 0
!    segyhd%gaps                = 0
!    segyhd%otrav               = 0
!    segyhd%unused1             = unused1                  !    -> 237:240
!
! Not all CPS header words have a corresponding SEGY header.  If you create a
! file with TROT in SEGY format, and read that file with TRIN, you will lose
! some header words.  Also, SEGY header words are integer, so you may lose
! precision making the round trip.
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
! PATHNAMES
!
! USERID is used to obtain permission to read the file on the local or remote
! node.  If a relative directory path is specified, the user name is also used
! to find the appropriate directory (specified by the DIR parameter) on
! the local or remote node, which is used as the origin of the relative
! directory path.
!
!
! Automatic Parameters
!
! The following TROT parameters can be determined automatically from the
! file(s) by TRIN:
!
!   TYPE, NUM_BITS, NUM_BITS_HDR, TIM_BEG, TIM_END.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS TRIN/NC=80>
!          ABORT=`CCCCCCC             HISTORY=`CC     
!    ORDER_LOCAL=`CCCCC          ORDER_PREV=`CCCCC    
!  PARALLEL_MODE=`CCCCCCCCC
!  Select_on_header_word=`II Start=`FFFFFF End=`FFFFFF Incr=`FFFFF
!<include dooskip.f90>
!<include mfile.f90>
!
!<NS Map SEGY Headers/NC=80>
!<include mapsegy.f90>
!
!<NS Groups For Kmig/NC=80>
!    OPT_GROUP~~=`CC             HDR_GROUP~~=`I      LEN_GROUP~~=`I
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!           rowgroup = ABORT HISTORY
!           rowgroup = ORDER_LOCAL ORDER_PREV
!
!           tabgroup = TraceInput(TRIN)
!
!<Help KEYWORD="ABORT">
!<Tip> How different the input file must be from the data context before TRIN will abort. </Tip>
! Default = NORMAL
! Allowed = STRICT, NORMAL, LENIENT
! If ABORT = STRICT, then abort if Sample Rate(DT), Start Time (TIM_BEG), 
! or Ending Time (TIM_END) on any file does not match the job globals. (Start time and end time
! could be depth values or frequencies instead...)
!
! If ABORT = NORMAL, then abort if a DT mismatch occurs, or if TIM_END on any
! file is LESS than TIM_END in the job.  If a TIM_BEG mismatch occurs,
! truncate or extend the top of the trace with zeroes and adjust the head mute
! index.  If TIM_END on any file is MORE than TIM_END in the job, truncate
! the bottom of the trace, and adjusts the tail mute index.
!
! If ABORT = LENIENT, then if a DT mismatch occurs, ignore DT on the file.  If
! a TIM_BEG or TIM_END mismatch occurs, truncate or extend the top or bottom of
! the trace with zeroes and adjust the mute indices.
!</Help>
!
!<Help KEYWORD="ORDER_LOCAL">
!<Tip> Whether to merge traces with those from other files. 
!      (Use this if you list more than one file below, and want data to be 
!      either appended or interleaved.)
!</Tip>
! Default = APPEND
! Allowed = APPEND(Append traces after those from other files.)
! Allowed = MERGE (Merge traces one at a time with those from other files.)
!
! The do-skip pattern is applied after the ORDER_LOCAL; so the do-skip
! is for the composite of the input files, not for each individual input file.
!</Help>
!
!<Help KEYWORD="ORDER_PREV">
!<Tip> Whether to merge traces with those from previous processes. (by adding them to the end of the ensemble passed in to this process.)</Tip>
! Default = NEW
! Allowed = NEW   (Do not merge traces.)
! Allowed = APPEND(Append traces after those from previous processes.)
! Allowed = MERGE (Merge traces one at a time with those from previous
!                  processes.)
!
! The first process in a loop cannot be a TRIN with
! ORDER_PREV = APPEND or ORDER_PREV = MERGE.
!</Help>
!
!             rowgroup = START END
!
!<Help KEYWORD="Select_on_header_word">
!<Tip> Use this to restrict input to traces with certain header word numbers (using CPS HEADERS).  See "Header Mapping" below. </Tip>
! Default = 0
! Allowed = 0 - (the number of words in trace header)
! Default of 0 means to ignore the "start, end, incr" and allow all traces
! to be passed on to the processing sequence.
!</Help>
!
!<Help KEYWORD="Start">
!<Tip> Starting value for the "select on header word" header word. </Tip>
! Default = 0.0
! Allowed = 
! All traces which have the selected header word of this value will be 
! passed through to the processing stream.  All others (except if they
! are within the "incr, end" values) will be rejected.
!</Help>
!
!<Help KEYWORD="End">
!<Tip> Ending value for the "select on header word" header word. </Tip>
! Default = 0.0
! Allowed = 
! All traces from "start" to this "end" value with increment of "incr"
! will be passed to the processing stream.
!</Help>
!
!<Help KEYWORD="Incr">
!<Tip> Increment with which to stride through the values in the selected header word.</Tip>
! Default = 0.0
! Allowed = 
! For the "select_on_header_word" option, you will allow traces from the 
! "start" value to the "end" value through using the "incr" value to 
! incrementally stride through traces.  If 0.0, then values are passed that
! fall anywhere between start and end.
!</Help>
!
!<Help KEYWORD="Parallel_mode">
!<Tip> Select which traces are read by each process.</Tip>
! Default = NONE
! Allowed = NONE
! Allowed = BLOCK
! Allowed = CIRCULAR
! If you use this, you MUST set the max traces for TRIN to read in the TR_MAX field.
! If you set some arbitrary number, then each "CPU" in your parallel job will try to read
! (that number)/(the number of CPU's).  BLOCK mode makes CPU #1 do the
! first TR_MAX/(Number of CPU's in the job) traces, CPU #2 does the next group, etc.  CIRCULAR
! makes CPU #1 do trace 1 CPU #2 does trace 2, etc going back to CPU #1 doing
! trace (Number of CPU's) + 1...    All modes will honor your DO-SKIP patterns. (The DO-SKIP
! patterns are those created by the fields TR_MAX, SKIP_INIT, NUM_DO, and NUM_SKIP.)
!</Help>
!
!           tabgroup = Groups For KMIG
!
!<Help KEYWORD="OPT_GROUP">
!<Tip> Define image groups by header word specified in the HDR_GROUP field. </Tip>
! Default = NO
! Allowed = NO  TRIN will not set header word HDR_GROUP to the file index.
! Allowed = YES KMIG will     set header word HDR_GROUP to the file index.
!</Help>
!
!<Help KEYWORD="HDR_GROUP">
!<Tip> Header word # (from CPS) to define groups. (see Header Mapping to find the correct header word number.)</Tip>
! Default = 52
! Allowed = 1-64 
! If OPT_GROUP = YES TRIN will set header word specified in the HDR_GROUP field to the file index.
!</Help>
!
!<Help KEYWORD="LEN_GROUP">
!<Tip> Number of files per group. </Tip>
! Default = 1
! Allowed = any positive integer
!</Help>
!
!           tabgroup = TraceInput(TRIN)
!
!<Help KEYWORD="HISTORY">
!<Tip> (For CPS data sets that include history) How to control transfer of processing history. </Tip>
! Default = YES
! Allowed = YES   (Read all old histories.)
! Allowed = NO    (Do not read any history.)
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

module trin_module

  use trcio_module
  use ameq_module
  use cardset_module
  use cpsio_module
  use getsys_module
  use dooskip_module
  use getlun_module
  use hist_module
  use lav_module
  use mapsegy_module
  use mfile_module
  use mutehw_module
  use named_constants_module
  use pc_module
  use string_module
  use pcps_module
  use pcpsx_module


  implicit none

  private

  !--- PROCEDURES
  public :: trin_create     ! uses the parameter cache.
  public :: trin_initialize
  public :: trin_update     ! uses the parameter cache.
  public :: trin_delete
  !--- DATA STRUCTURES
  public :: trin_struct

!<execute_only>
  public :: trin            ! main execution (trace processing) routine.
  public :: trin_wrapup
!</execute_only>

  !--- Parameters I might need.
  logical                      :: dbg  = .false. ! debug print statements.
  integer,parameter            :: trin_error = trcio_error - 1

  character(len=100),public,save :: trin_ident = &
  '$Id: trin.f90,v 1.84 2007/11/30 13:55:19 Menger beta sps $'

  ! Structure file_ptr to enable pointer to pointer.
  !
  type :: file_ptr
    type(trcio_struct), pointer :: p
  end type file_ptr

  type :: trin_struct

    private

    !--Globals--
    real                       :: tstrt
    real                       :: tend
    real                       :: dt
    integer                    :: ndpt
    integer                    :: nwih
    type(grid_struct)          :: grid
    integer                    :: numtr
    logical                    :: gathered
    logical                    :: opt_group
    integer                    :: hdr_group
    integer                    :: len_group

    !--Parms  --

    integer                    :: ipn
    type(dooskip_struct),pointer :: DS
    integer                    :: ndone
    integer                    :: trace_pointer
    integer                    :: next_trace
    integer                    :: tot_num_traces
    character(len=7)           :: abort    !NORMAL | STRICT | LENIENT
    character(len=3)           :: history  !YES | NO
    character(len=6)           :: order_local   !APPEND | MERGE
    character(len=6)           :: order_prev    !NEW | APPEND | MERGE

    integer                    :: select_on_header_word
    double precision           :: start,end,incr

    integer                    :: parallel_num_wrkr,parallel_wrkr_num
    !                                             1234 12345 1234567890
    character(len=10)          :: parallel_mode ! NONE,BLOCK,CIRCULAR
    logical                    :: first_time_in
    logical                    :: pcpsx_initialized
    character(len=9)           :: calling_program

    type(mapsegy_struct),pointer :: mapsegy

    !--Local saved vars --
    integer                    :: next_sequential_trace_num
    integer                    :: stdout
    logical                    :: skip_wrapup

    type(file_ptr)                , dimension(:), pointer :: files
    character(len=FILENAME_LENGTH), dimension(:), pointer :: filenames
    integer                                               :: num_files
    integer                                               :: init_num_files
    integer                                               :: merge_offset
    logical                                               :: vmod

    type(mfile_struct),pointer :: mfile

  end type trin_struct

  type(trin_struct),pointer,save :: trp ! need this for traps

  integer, parameter           :: n_opt_yes_no = 2
  character(len=3),parameter   :: c_opt_yes_no (n_opt_yes_no) &
    = (/'YES', 'NO '/)
  integer, parameter           :: num_opt_abort = 3
  character(len=7),parameter   :: opt_abort (num_opt_abort) &
    = (/'NORMAL ','STRICT ','LENIENT'/)
  integer, parameter           :: num_opt_history = 2
  character(len=3),parameter   :: opt_history (num_opt_history) &
    = (/'YES','NO '/)
  integer,parameter            :: num_opt_order_local = 2
  character(len=6),parameter   :: opt_order_local (num_opt_order_local) &
    = (/'APPEND','MERGE '/)
  integer,parameter            :: num_opt_order_prev = 3
  character(len=6),parameter   :: opt_order_prev (num_opt_order_prev) &
    = (/'NEW   ','APPEND','MERGE '/)

  integer,parameter            :: num_opt_parallel_mode = 3
  character(len=10),parameter  :: opt_parallel_mode(num_opt_parallel_mode) &
    = (/'NONE      ','BLOCK     ','CIRCULAR  '/)

contains

  subroutine trin_create (obj)
    type(trin_struct),pointer :: obj       ! arguments

    allocate (obj)

    nullify  (obj%DS)
    nullify  (obj%files)
    nullify  (obj%filenames)
    nullify  (obj%mapsegy) !jpa
    nullify  (obj%mfile)  !jpa

    call  mapsegy_create          (obj%mapsegy)
    call  mfile_create            (obj%mfile  )
    call  mfile_set_pathnames_ext (obj%mfile,'*')
    call  mfile_set_type          (obj%mfile, MFILE_READ_TRC_FILE)

    call trin_initialize   (obj)

  end subroutine trin_create

  subroutine trin_delete (obj)
    type(trin_struct),pointer :: obj       ! arguments
!<execute_only>
    if(associated(obj)) call trin_wrapup (obj)
!</execute_only>
    call mapsegy_delete    (obj%mapsegy)
    call   mfile_delete    (obj%mfile  )

    if(associated(obj%files    )) deallocate(obj%files    )
    if(associated(obj%filenames)) deallocate(obj%filenames)

    if(associated(obj)) deallocate(obj)
  end subroutine trin_delete

  subroutine trin_initialize (obj)
  type(trin_struct),pointer :: obj       ! arguments


    !----------Here we initialize all data that will be subsequently updated.
    !--- this data comes from the parm-cache globals


    if(.not. associated(obj) ) return

    obj%ipn        = pc_get_ipn()
    obj%numtr      = 0
    obj%tstrt      = 0.0
    obj%gathered   = .false.
    obj%tend       = 0.0
    obj%dt         = 0.0
    obj%ndpt       = 0
    obj%nwih       = 0
    !--- this data comes from the parm-cache data cards for this process
    if(dooskip_initialize(obj%DS) /= dooskip_ok) continue
    obj%abort       = 'NORMAL'
    obj%history     = 'YES'
    obj%order_local = 'APPEND'
    obj%order_prev  = 'NEW'
    obj%opt_group   = .false.     ! if opt_group set hdr_group to file index
    obj%hdr_group   = 55          ! if opt_group set hdr_group to file index
    obj%len_group   = 1           ! if opt_group set hdr_group to file index

    obj%select_on_header_word = 0
    obj%start                 = -.999999d17
    obj%end                   =  .999999d17
    obj%incr                  = 0d0

    obj%parallel_mode         = 'NONE'
    obj%parallel_num_wrkr     = 1
    obj%parallel_wrkr_num     = 1 ! first worker == 1
    obj%first_time_in         =.true.
    obj%pcpsx_initialized     =.false.
    obj%calling_program       ='CPS'

    obj%num_files        = 0
    obj%init_num_files   = 0
    obj%merge_offset= 0
    obj%vmod        = .true.

    !--- this data comes from this process itself
    obj%next_sequential_trace_num = 0

    call mapsegy_initialize(obj%mapsegy)
    call pc_put_sensitive_field_flag('select_on_header_word', .true.)


    call trin_update (obj)

  end subroutine trin_initialize

  subroutine trin_update (obj)

    type(trin_struct),target              :: obj

    !--- local

    integer                               :: status,trmax,maxthru
    character(len=cardset_datacard_length):: card
    character(len=cardset_length), pointer ,dimension(:) :: global_cards
    integer                               :: num_global_cards,nkeys
    integer                               :: idummy
    real                                  :: rdummy,dt,tend,tstrt

    type(cardset_struct),pointer          :: jobglobals, cardset_filenames
    character(len=cardset_length), pointer ,dimension(:) :: global_keywords
    character(len=cardset_length)         :: cdummy, errmsg
    character(len=132)                    :: filenames_file_for_ttrot
    character(len=64)                     :: host

    integer                               :: min_num_traces
    integer                               :: i
    integer                               :: j,k
    integer                               :: istat,lun_fn   ,ncards 

    nullify (jobglobals) ! jpa
    nullify (cardset_filenames) ! jpa

    !--- point trp for trap routines.
    trp => obj
    if(.not. associated(trp) ) return

    obj%skip_wrapup = .true.    ! needed for the wrapup routine.

    !--- get stdout unit number from parm cache.
    obj%stdout = pc_get_lun()         ! if needed.

    !--- get globals from parm cache.
    call pc_get_global ('tstrt', obj%tstrt) ! time of 1st trace sample (sec).
    call pc_get_global ('dt'   , obj%dt)    ! trace sample interval (sec).
    call pc_get_global ('ndpt' , obj%ndpt)  ! number of trace samples.
    obj%tend = obj%tstrt + (obj%ndpt-1)*obj%dt
    call pc_get_global ('nwih' , obj%nwih)  ! number of header words.
    call pc_get_global ('grid' , obj%grid)  ! grid transform data structure.
    if (pc_global_keyword_present('numtr')) &
     call pc_get_global ('numtr', obj%numtr) ! maximum number of traces.
    if (pc_global_keyword_present('gathered')) &
     call pc_get_global ('gathered', obj%gathered) ! is data gathered coming in?
    !--- I set gathered to false unless order_prev=merge, then I don't know.
    if(obj%order_prev /= 'MERGE') obj%gathered = .false.
    !--- get parameters for this instance of my process from parm cache.

    !--- get job data needed (calling_program = cps or seisspace)
    call pc_get_jdata('CALLING_PROGRAM',obj%calling_program)

    !--- initialize screen ... go to a particular field.

    call pc_get ('abort'      , obj%abort      , trin_abort_trap      )
    call pc_get ('history'    , obj%history    , trin_history_trap    )
    call pc_get ('order_local', obj%order_local, trin_order_local_trap)
    call pc_get ('order_prev' , obj%order_prev , trin_order_prev_trap )

    call pc_put ('abort'      , obj%abort      )
    call pc_put ('history'    , obj%history    )
    call pc_put ('order_local', obj%order_local)
    call pc_put ('order_prev' , obj%order_prev )

    if(obj%calling_program /= 'CPS') then
      call pc_put_sensitive_field_flag('PARALLEL_MODE', .TRUE.)
      call pc_put_visible_flag ('PARALLEL_MODE',.TRUE.)
      call pc_get ('parallel_mode',obj%parallel_mode)
      call pc_put_options_field('PARALLEL_MODE',opt_parallel_mode,&
                                 num_opt_parallel_mode )
      !---------------------------------------------------------------------
      call pc_put ('parallel_mode', obj%parallel_mode)
    else
      call pc_put_sensitive_field_flag('PARALLEL_MODE', .FALSE.)
      call pc_put_visible_flag ('PARALLEL_MODE',.FALSE.)
    endif
      call pc_put_visible_flag ('select_on_header_word',.TRUE.)
      call pc_get ('select_on_header_word',obj%select_on_header_word, &
                    trin_select_on_header_word_trap )
      call pc_put ('select_on_header_word',obj%select_on_header_word)

      if(obj%select_on_header_word == 0 ) then
        call pc_put_visible_flag ('start',.FALSE.)
        call pc_put_visible_flag ('end',.FALSE.)
        call pc_put_visible_flag ('incr',.FALSE.)
      else
        call pc_put_visible_flag ('start',.TRUE.)
        call pc_put_visible_flag ('end',.TRUE.)
        call pc_put_visible_flag ('incr',.TRUE.)
      endif

      call pc_get ('start',obj%start,trin_start_trap)
      call pc_get ('end',obj%end,trin_end_trap)
      call pc_get ('incr',obj%incr,trin_incr_trap)
      call pc_put ('start',obj%start)
      call pc_put ('end',obj%end)
      call pc_put ('incr',obj%incr)

    call pc_get ( 'opt_group',  obj%opt_group  )
    call pc_get ( 'hdr_group',  obj%hdr_group )
    call pc_get ( 'len_group',  obj%len_group )
    obj%hdr_group = max ( 1 , min ( obj%nwih, obj%hdr_group ) )
    call trin_make_downward_compatible(obj)


    call pc_put_options_field('opt_group'  , c_opt_yes_no   , n_opt_yes_no)
    call pc_put_options_field('ABORT'      , opt_abort      , num_opt_abort)
    call pc_put_options_field('HISTORY'    , opt_history    , num_opt_history)
    call pc_put_options_field('ORDER_LOCAL', opt_order_local, &
                                 num_opt_order_local)
    call pc_put_options_field('ORDER_PREV' , opt_order_prev , &
                                 num_opt_order_prev )

    if(dooskip_update(obj%DS) /= dooskip_ok) &
      call pc_error('TRIN: error applying the do_skip_update.')

    call   mfile_update(obj%mfile  )
    call mapsegy_update(obj%mapsegy)

!<execute_only>

!</execute_only>
    if(obj%order_prev == 'MERGE' ) obj%numtr = obj%numtr + 1
    obj%numtr = max(obj%numtr,1)
    call pc_put_global  ('numtr'       , obj%numtr)        ! if changed.
    call pc_put_global  ('nwih'        , obj%nwih)         ! if changed.
    call pc_put_global  ('ndpt'        , obj%ndpt)         ! if changed.
    call pc_put_global  ('dt'          , obj%dt)           ! if changed.
    call pc_put_global  ('tstrt'       , obj%tstrt)        ! if changed.
    call pc_put_global  ('grid'        , obj%grid)         ! if changed.
    call pc_put_global  ('gathered'    , obj%gathered)     ! if changed.

    call pc_put_control ('twosets'     , .false.)       ! default false
    call pc_put_control ('iftd'        , .false.)       ! default false
    call pc_put_control ('ndisk'       , 0 )
    call pc_put_control ('setup_only'  , .false.)       ! default .false.
    select case(obj%order_prev)
      case('NEW')
        call pc_put_control ('need_request', .false.   )
        call pc_put_control ('need_label'  , .true.    )
      case('APP','APPEND')
        call pc_put_control ('need_request', .true.    )
        call pc_put_control ('need_label'  , .true.    )
      case('MERGE')
        call pc_put_control ('need_request', .true.    )
        call pc_put_control ('need_label'  , .true.   )
      case default
    end select

    call pc_put ( 'opt_group',  obj%opt_group  )
    call pc_put ( 'hdr_group',  obj%hdr_group  )
    call pc_put ( 'len_group',  obj%len_group  )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


    !!!!----- see if we can do this at CFE time.
    !!!! if (pc_do_not_process_traces()) return

    if(pc_get_update_state() .eq. PC_BACKEND) then

      obj%skip_wrapup = .false.
!<execute_only>
      if (.not. mfile_get_filenames(obj%mfile, &
                              obj%filenames, obj%num_files)) then
        call pc_error('TRIN:  bad filenames in list.')
        return
      endif

      obj%init_num_files = obj%num_files ! use this if we change num_files.
      allocate(obj%files(obj%num_files), stat=status)
      if (status /= 0) then
        call pc_error('TRIN:  out of memory [in trin_update routine.]')
        return
      endif

      do i = 1, obj%num_files
        nullify(obj%files(i)%p)
      enddo

      obj%tot_num_traces = 0
      !---Save file names in temporary directory.  It will be used by TTROT if saving
      !---velocity models
      call getlun(lun_fn,istat)
      if(istat.ne.0)then
        call pc_error('Unable to get unit number for filenames[in trin_update].')
        return !-- fatal error
      endif 
      call getsys_hostname(host)
      write(filenames_file_for_ttrot,'(3A,I8.8)')'%trin_filenames_',trim(host),'_',getsys_pid()
      !print*,'Filenames File=',filenames_file_for_ttrot
      open(lun_fn,file=filenames_file_for_ttrot,status='replace')

      do i = 1, obj%num_files
        obj%files(i)%p => trcio_open(obj%filenames(i),'r')
        if(.not. associated (obj%files(i)%p) ) then
          call pc_error ('TRIN: Error: Unable to open file ' &
                         //trim(obj%filenames(i))//'.  In [trin_update].')
          return ! --- fatal error
        endif
      end do

!---- history logic
      call cardset_create(cardset_filenames)
      call cardset_put_array( &
        cardset_filenames, 'TRIN_INPUT_FILES', obj%filenames, obj%num_files)
      ncards = cardset_num_cards(cardset_filenames)

      do i = 1, ncards
        call cardset_get_card(cardset_filenames, i, card, errmsg)
        if(i==ncards) card = trim(card) // ','
        istat=hist_write(obj%ipn,card)
        if(istat.ne.0)write(obj%stdout,*)'TRIN-->hist_write error B'
      end do
      call cardset_delete(cardset_filenames)


!---- loop per file
      do i = 1, obj%num_files
        write(lun_fn,'(A)')trim(obj%filenames(i))

!------ velocity model files will have the string "_vmod.trc"

        k=index(obj%filenames(i),'_vmod.trc')
        if(k.eq.0)obj%vmod=.false.

        dt    = obj%files(i)%p%dt
        tstrt = obj%files(i)%p%tmin
        tend  = obj%files(i)%p%tmax
        status = trcio_set_ipn(obj%files(i)%p,obj%ipn)
        status = trcio_set_stdout(obj%files(i)%p,obj%stdout)
        call trcio_headerdump(obj%files(i)%p)

        ! Logic here for ABORT option.  Check DT, TMIN, TMAX

        select case (obj%abort)
        case('STRICT')
          if (.not. ameq(dt,obj%dt,obj%dt*.001) .or.       &
              .not. ameq(tstrt,obj%tstrt,obj%tstrt*.001) .or.   &
              .not. ameq(tend,obj%tend,obj%tend*.001) ) then
            call pc_error('TRIN: Either the Sample Rate (dt),'//&
            ' Trace Start Time (tmin), or Trace Maximum time (tmax) do not match with file.')
            return ! --- fatal error
          endif
        case('NORMAL')
          if (.not. ameq(dt,obj%dt,obj%dt*.001) ) then
            call pc_error('TRIN: Sample Rate (dt) does not match with file.')
            return ! --- fatal error
          endif
        case('LENIENT')
          if (.not. ameq(dt,obj%dt,obj%dt*.001) .or.       &
              .not. ameq(tstrt,obj%tstrt,obj%tstrt*.001) .or.   &
              .not. ameq(tend,obj%tend,obj%tend*.001) ) then
            call pc_warning('TRIN: Either the Sample Rate (dt),'//&
            ' Trace Start Time (tmin), or Trace Maximum time (tmax) do not match with file.')
          endif
        case default
        end select

        !--- Read jobglobals from trace file if present ---
        !---
        select case (trim(obj%files(i)%p%ftype))
          case('TRCIO', 'LBO', 'CMPR')
            num_global_cards = trcio_num_global_cards(obj%files(i)%p)
            if(num_global_cards > 0 ) then
             allocate(global_cards(num_global_cards))
             status = trcio_read_globals( &
                       obj%files(i)%p,num_global_cards,global_cards)
             if(status /= trcio_ok) then
               call pc_error('trin: error reading job globals from the file.')
               return ! --- fatal error
             endif
             call cardset_create(jobglobals)
             call cardset_set_name(jobglobals,'jobglobals')
             call cardset_put_cards(jobglobals,global_cards,num_global_cards)
             deallocate (global_cards)

             nkeys = cardset_num_keywords(jobglobals)
             allocate(global_keywords(nkeys))
             !--- we will load job global keyword names from file into array.
             !--- we will only look at scalars (nelem == 1)
             do j = 1, nkeys
               global_keywords(j) = cardset_get_keyword(jobglobals,j)
               if(cardset_nature(jobglobals,global_keywords(j)) &
                   == CARDSET_SCALAR) then
                 !--- compare with this job ---
                 select case(trim(global_keywords(j)))
                   case('NDPT')
                     call cardset_get_scalar(jobglobals,'NDPT',idummy,errmsg)
                     if(idummy /= obj%ndpt) &
                       call pc_info&
                       ('trin: file job global NDPT differs from job.')
                   case('DT')
                     call cardset_get_scalar(jobglobals,'DT',rdummy,errmsg)
                     if(rdummy /= obj%dt) &
                       call pc_info( &
                             'trin: file job global DT differs from job.')
                   case('TSTRT')
                     call cardset_get_scalar(jobglobals,'TSTRT',rdummy,errmsg)
                     if(rdummy /= obj%tstrt) &
                       call pc_info&
                       ('trin: file job global TSTRT differs from job.')
                   case('NUMTR','NWIH','GATHERED')
                   case default
                     !--- add the global ---
                     call cardset_get_scalar(jobglobals,global_keywords(j),&
                       cdummy,errmsg)
                     call pc_put_global(global_keywords(j),cdummy)
                 end select
               endif
             end do
             deallocate(global_keywords)
             call cardset_delete(jobglobals)
            endif
          case default
        end select

        select case (obj%files(i)%p%ftype)
        case default
          !--- Read applicable history ---
          if(obj%history == 'YES')then
             call trcio_read_history_cards(obj%files(i)%p,status)
             if(status.ne.0)then
               call pc_error('Abort - history problem')
               return ! -- fatal error
             endif
          endif
        end select

        obj%tot_num_traces = obj%tot_num_traces + obj%files(i)%p%num_traces

        if (i == 1) then
          min_num_traces = obj%files(i)%p%num_traces
        else if (obj%files(i)%p%num_traces < min_num_traces) then
          min_num_traces = obj%files(i)%p%num_traces
        endif

      enddo
      close(lun_fn,status='keep')

      if ((obj%order_local == 'MERGE') &
        .and. (min_num_traces * obj%num_files < obj%tot_num_traces)) then
        call pc_info( &
         'TRIN:  ORDER_LOCAL == MERGE and different file sizes, so the last '//&
         'traces may all come from only some of the files.')
        !-- obj%tot_num_traces = min_num_traces * obj%num_files
      endif

      !--- Need to determine whether or not trace file header has number of
      !--- traces in it and if user is trying to read past EOF
      maxthru = dooskip_thru_item(obj%DS) ! last item position requested

      if(obj%tot_num_traces > 0 .and. (maxthru > obj%tot_num_traces &
       .or. maxthru < 0) ) then ! maxthru can be less than 0 if rollover at 2gig
        !--- we need to reset tr_max.
        status = &
        dooskip_put_tr_max(obj%DS,dooskip_calc_trmax(obj%DS,obj%tot_num_traces))
        if(status /= dooskip_ok) then
          call pc_error(&
          'TRIN: Error resetting maximum traces to read,'//&
          ' because initial skip is too big.')
          return !-- fatal error
        endif
        trmax = dooskip_get_tr_max(obj%DS)
        write(card,'(A,I11,A)')&
        'TRIN will output ',trmax,' traces based on the file size(s) and skip pattern.'
        call pc_info(card)
      end if

      obj%trace_pointer = dooskip_next_item(obj%DS)
      obj%ndone         = 0

!</execute_only>

    endif ! PC_UPDATE_STATE.

  end subroutine trin_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


subroutine trin_abort_trap(abort)
  character(len=*),intent(in)     :: abort
  call string_to_upper(trp%abort)
  select case (trp%abort)
    case ('NORMAL','STRICT','LENIENT')
    case default
      call pc_error('TRIN: abort mode must be either NORMAL, STRICT, or LENIENT.')
      call pc_jump_field(abort)
  end select
end subroutine trin_abort_trap



subroutine trin_history_trap(history)
  character(len=*),intent(in) :: history

  call string_to_upper(trp%history)
  select case (trp%history)
    case ('YES','NO')
    case default
      call pc_info('TRIN: history must be YES or NO.')
      call pc_jump_field(history)
  end select
end subroutine trin_history_trap



subroutine trin_order_local_trap(order_local)
  character(len=*),intent(in) :: order_local
  call string_to_upper(trp%order_local)
  if (trp%order_local == 'APP') trp%order_local = 'APPEND'
        ! the above is to keep pc_put from generating an error when it compares to the options.
  select case(trp%order_local)
    case('APP','APPEND','MERGE')
    case default
      call pc_error('TRIN: order_local must be APPEND or MERGE (interleave).')
      call pc_jump_field(order_local)
  end select
end subroutine trin_order_local_trap



subroutine trin_order_prev_trap(order_prev)
  character(len=*),intent(in) :: order_prev
  call string_to_upper(trp%order_prev)
  if (trp%order_prev == 'APP') trp%order_prev = 'APPEND'
        ! the above is to keep pc_put from generating an error when it compares to the options.
  select case(trp%order_prev)
    case('NEW')
      return
    case('APP','APPEND','MERGE')
      ! --- this isn't sufficient but will catch top of loop errors for the
      ! --- beginning of program, at least.  Still could mess up at top of loop
      ! --- if in a subsequent loop.'Bill Menger 10/20/03
      if(trp%ipn <= 3) then
        call pc_error('TRIN: order_prev (order to read with respect to previous TRIN'//&
         ' processes within the job) must be "NEW" for the first process.'//&
         '  Changing it to NEW from '//trp%order_prev//'.')
        trp%order_prev = 'NEW'
        call pc_put ('order_prev',trp%order_prev )
        call pc_jump_field(order_prev)
      endif
      return
    case default
      call pc_error('TRIN: order_prev (order to read with respect to previous TRIN'//&
      ' processes within the job) must be NEW, APPEND, or MERGE.')
      call pc_jump_field(order_prev)
      return
  end select
end subroutine trin_order_prev_trap

subroutine trin_select_on_header_word_trap(select_on_header_word)
  character(len=*),intent(in) :: select_on_header_word
  if(trp%select_on_header_word <0 .OR.trp%select_on_header_word > trp%nwih)then
    call pc_error('TRIN: Cannot select on a non-existent header word.')
    call pc_jump_field(select_on_header_word)
  elseif (trp%select_on_header_word == 0 ) then
    call pc_put_visible_flag ('start',.FALSE.)
    call pc_put_visible_flag ('end',.FALSE.)
    call pc_put_visible_flag ('incr',.FALSE.)
    call pc_put_sensitive_field_flag('start', .false.)
    call pc_put_sensitive_field_flag('end', .false.)
    call pc_put_sensitive_field_flag('incr', .false.)
  else
    call pc_put_visible_flag ('start',.TRUE.)
    call pc_put_visible_flag ('end',.TRUE.)
    call pc_put_visible_flag ('incr',.TRUE.)
    call pc_put_sensitive_field_flag('start', .true.)
    call pc_put_sensitive_field_flag('end', .true.)
    call pc_put_sensitive_field_flag('incr', .true.)
    call pc_jump_field('start')
  endif
end subroutine trin_select_on_header_word_trap

subroutine trin_start_trap(start)
  character(len=*),intent(in) :: start
  double precision :: temp
  if(trp%start > trp%end) then
    temp = trp%end
    trp%end = trp%start
    trp%start = temp
  endif
  call pc_jump_field('end')
end subroutine trin_start_trap

subroutine trin_end_trap(end)
  character(len=*),intent(in) :: end
  double precision :: temp
  if(trp%start > trp%end) then
    temp = trp%end
    trp%end = trp%start
    trp%start = temp
  endif
  call pc_jump_field('incr')
end subroutine trin_end_trap

subroutine trin_incr_trap(incr)
  character(len=*),intent(in) :: incr
  double precision :: temp
  if(trp%end < trp%start ) then
    temp = trp%end
    trp%end = trp%start
    trp%start = temp
  endif
  if((trp%end - trp%start) < trp%incr ) then
    trp%incr = 0d0
    call pc_info(&
     'TRIN: increment reset to 0 since it is greater than (end - start).')
    call pc_info(&
     'If increment = 0 then all values between start and end allowed.')
    call pc_jump_field(incr)
  endif
end subroutine trin_incr_trap



!<execute_only>

subroutine trin (obj,ntr,hd,tr)
  !----------- PASSED PARAMETERS ----------------------------------------
  type(trin_struct),pointer       :: obj                    ! parm block
  integer         ,intent(inout)  :: ntr                    ! num trc
  double precision,intent(inout)  :: hd(:,:)                ! headers
  real            ,intent(inout)  :: tr(:,:)                ! traces
  !----------- LOCAL VARIABLES ------------------------------------------
  integer                         :: i , hw, ntr_coming_in
  double precision                :: hs,he,hi,dd
  integer                         ::    status 

  character(len=256)              :: string

  integer                         :: trmax
  integer                         :: nmax,ds_skip_init,ds_tr_max
  integer                         :: ds_num_do,ds_num_skip


  !--- executable code starts -------------------------------------------


  if(obj%first_time_in) then

    if(obj%calling_program /= 'CPS') then
      write(obj%stdout,'(A)')'Diagnostic == i am in seisspace'
    endif
    if(obj%parallel_mode .ne. 'NONE') then
      if(.not.obj%pcpsx_initialized) call pcpsx_init_processing()
      obj%pcpsx_initialized=.true.
      obj%parallel_num_wrkr = pcps_get_num_procs()
      obj%parallel_wrkr_num = pcps_get_worker_num()+1
      write(obj%stdout,'(A,I4.4,A,I4.4)') &
         'Number in Processing group=',obj%parallel_num_wrkr,&
         ' This is worker # ',obj%parallel_wrkr_num
      print*,'diagnostic_trin: num_wrkr = ',obj%parallel_num_wrkr
      print*,'diagnostic_trin: wrkr_num = ',obj%parallel_wrkr_num
      print*,'diagnostic_trin: par_mode = ',obj%parallel_mode
    endif
    ds_tr_max    = dooskip_get_tr_max(obj%DS)
    ds_num_do    = dooskip_get_num_do(obj%DS)
    ds_num_skip  = dooskip_get_num_skip(obj%DS)
    ds_skip_init = dooskip_get_skip_init(obj%DS)
    if(obj%parallel_mode .ne. 'NONE') then
      print*,'diagnostic_trin: ds_tr_max      = ',ds_tr_max
      print*,'diagnostic_trin: ds_tr_do       = ',ds_num_do
      print*,'diagnostic_trin: ds_num_skip    =',ds_num_skip
      print*,'diagnostic_trin: ds_skip_init   = ',ds_skip_init
    endif

    ! parcel traces out based on my worker number
    nmax = ds_tr_max/obj%parallel_num_wrkr
    if(ds_tr_max .lt.  obj%parallel_num_wrkr*nmax ) then  
      nmax = 1 + nmax
    endif
    if(obj%parallel_mode == 'BLOCK') then
      ds_skip_init = ds_skip_init + nmax*(obj%parallel_wrkr_num-1)
      if(obj%parallel_num_wrkr == obj%parallel_wrkr_num ) then !i am last worker
        if(ds_tr_max .lt.  obj%parallel_num_wrkr*nmax) then
          nmax = obj%parallel_num_wrkr*nmax - ds_tr_max
        endif
      endif
      !ds_num_skip  = 0
      !ds_num_do    = 1
      ds_tr_max    = nmax
    elseif(obj%parallel_mode == 'CIRCULAR' ) then
      ds_skip_init = ds_skip_init + &
                    (ds_num_do+ds_num_skip)*(obj%parallel_wrkr_num-1)
      ds_num_skip  = (obj%parallel_num_wrkr-1)*(ds_num_skip+ds_num_do)+&
                     ds_num_skip
      ds_num_do    = ds_num_do
      if(obj%parallel_num_wrkr == obj%parallel_wrkr_num ) then !i am last worker
        if(ds_tr_max .lt.  obj%parallel_num_wrkr*nmax) then
          nmax = obj%parallel_num_wrkr*nmax - ds_tr_max
        endif
      endif
      ds_tr_max    = nmax
    endif
    status = dooskip_put_tr_max(obj%DS,ds_tr_max)
    status = dooskip_put_num_do(obj%DS,ds_num_do)
    status = dooskip_put_num_skip(obj%DS,ds_num_skip)
    status = dooskip_put_skip_init(obj%DS,ds_skip_init)
    
    if(obj%parallel_mode .ne. 'NONE') call dooskip_state(obj%DS)
    obj%first_time_in = .false.
  endif

  ntr_coming_in = ntr

  !--- go away if error coming in.
  if (ntr == FATAL_ERROR) then
    return
  endif

  !--- go away if memory problem in job.
  if(.not. associated(obj) ) then
    string = 'TRIN: FATAL ERROR ==> file object was lost.'
    call pc_info(trim(string))
    ntr = FATAL_ERROR
    return
  endif

  hw = obj%select_on_header_word
  hs = obj%start
  he = obj%end
  hi = obj%incr

selectdo:  do 

    !--- If I have exceeded my maximum, I need to quit.
    if(dooskip_hit_tr_max(obj%DS)) then
       !-- We can't set to NO_MORE_TRACES because someone might have passed 
       !-- us a trace.
       !-- wmm 12/16/04 SO we don't touch NTR. Just return!
      return
    endif
  
    !--- figure out what mode I am in and act accordingly.
    select case (obj%order_prev)
      case ('NEW')
        !--- simple case where I just put out one trace.
        ntr = 1
      case ('APP','APPEND')
        if( ntr == NEED_TRACES .or. ntr >= 1 ) then
          !--- let main job either loop back for more or send traces on down.
          if(ntr >= 1 ) then
            obj%next_sequential_trace_num = obj%next_sequential_trace_num + ntr
          endif
          return
        else ! No more traces, so now I can add mine!
          !--- APPEND mode only kicks in when all other traces are done.
          !--- I now can act like I would have in 'NEW' mode, so set flag.
          obj%order_prev = 'APNEW'
          ntr = 1
        endif
      case('MERGE')
        !--- If someone below needs traces, I send message upstairs to supplier.
        if( ntr == NEED_TRACES ) then
          return
        elseif ( ntr >= 1 ) then
          !--- I am going to add a trace to the input group, and send them on.
          ntr = ntr + 1
        else ! no more traces
          !--- Sender has no more, so I must move to append mode.
          !--- old behavior   return
          !--- new behavior, switch to append mode since previous source process
          !--- is out of traces.
          obj%order_prev = 'APNEW'
          ntr = 1
        endif
      case('APNEW')
        !--- simple case where I put one trace, I used to be append mode.
        ntr = 1
    end select

    if(.not.dooskip(obj%DS,&
          this_item=(obj%trace_pointer), next_item=obj%next_trace))&
    then
     !-- We can't set to NO_MORE_TRACES because someone might have passed 
     !-- us a trace. BuT!!! we added 1 to ntr above, so we must subtract it
     !-- here.  Since we don't want to send an erroneous signal below, we 
     !-- won't go below the NO_MORE_TRACES signal (which == 0 ). Wmm 12/16/04
     if(dooskip_hit_tr_max(obj%DS)) then
       ntr = max(NO_MORE_TRACES,ntr - 1)
       return
     endif
     obj%trace_pointer = obj%next_trace
     ntr=ntr_coming_in
    cycle selectdo
    else
    endif

    !-- If I am here, I need to read a trace and load it into the NTR'th element
    !--- of the tr,hd arrays.
    if(obj%tot_num_traces>0 .and. obj%trace_pointer > obj%tot_num_traces ) then
     !--- test here to see if we are going to try to read past EOF (this is
     !    only a problem with QTROT files for now, but gotta handle exceptions.)
     !------------- Don't try to read, assume EOF -----
     status = TRCIO_EOF
    else !--------------- go ahead and read -----------------
      status = trin_read_trace(obj,hd(:obj%nwih,ntr),tr(:,ntr) )
    endif ! tot_num_traces > trace pointer
   
    ! === here we will decide if we should keep this trace or toss it and go
    ! === back for more.  If more, continue the do-while, if not, exit loop.

    if(hw == 0 ) exit selectdo ! do not do any testing of header word selection
    dd = hd(hw,ntr)
    if(hd(hw,ntr) < hs .OR. hd(hw,ntr) > he ) then
       ntr=ntr_coming_in
       cycle selectdo ! remove traces out of bnds
    endif
    ! now look for traces that fall into the increment ranges only.
    ! if incr == 0d0 then exit (use the trace)
    if(hi == 0d0 ) exit selectdo ! (and fall through to use this trace)
    ! otherwise, normalize the header word value and check for nearness to an
    ! increment value. dd = dummy double precision variable) di = dummy integer
    dd = ( (dd - hs)/hi )
    if(nint(dd) /= 0 ) then
      dd = dd/nint(dd)
      if(ameq(dd,1d0,.001)) exit selectdo
        ! then the increment matches the header word.
    else
      if(ameq(hd(hw,ntr) - hs,0d0,.001) ) exit selectdo ! we are at hs.
    endif 
    ! go back and re-increment the doo-skip patterns, read a trace, etc.
  end do selectdo

  select case (status)
    case (trin_error,trcio_error) !--- any type of read error.
      !--- Error handling code here
      write(string,'(A,I16,A)')'TRIN: Error reading trace ',&
        obj%trace_pointer-1
      hd(:,ntr) = 0d0
      tr(:,ntr) = 0e0
      call string_compress_blanks(string,i)
      call pc_info(string(1:i))
      ntr = FATAL_ERROR
      return
    case (trcio_eof) !--- Reached end of file
      write(string,'(A,I16,A)')'TRIN: End-Of-File after trace ',&
        obj%trace_pointer-1
      call string_compress_blanks(string,i)
      call pc_info( string(:i))
      trmax = dooskip_get_tr_max(obj%DS)
      if(trmax > obj%trace_pointer-1 ) then
        write(string,'(2A,I16,A)')'TRIN: EOF was premature -- ',&
          'Should have gotten to trace ',trmax,'.  (Aborting job.)'
        call string_compress_blanks(string,i)
        call pc_info(string(:i))
        ntr = FATAL_ERROR
        return
      endif

      !--- set tr-max to actual number of traces read so that in wrapup
      !--- we can print out the correct number.  if we simply print out
      !--- the "dooskip_nbr_done" we will include the last unread trace and
      !--- the count will be too high.
      if(0 == dooskip_put_tr_max(obj%DS,dooskip_nbr_done(obj%DS)-1))continue
      !--- Info -endofdata code here
      write(string,'(A,I16,A)')'TRIN: End of Data after reading ',&
      dooskip_get_tr_max(obj%DS),' traces from all files.'
      call string_compress_blanks(string,i)
      call pc_info(string(1:i))
      ntr = NO_MORE_TRACES
      return
    case(trcio_ok) !--- The read status was OK
      ! Set sequential trace number on every trace passed out.
      do i = 1,ntr
        obj%next_sequential_trace_num = obj%next_sequential_trace_num + 1
        hd(HDR_SEQUENCE,i) = obj%next_sequential_trace_num
      end do

      if(dooskip_hit_tr_max(obj%DS)) return
      obj%trace_pointer = obj%next_trace
  end select

end subroutine trin
!</execute_only>

subroutine trin_wrapup (obj)
  character (len=132)        :: string
  type(trin_struct),pointer  :: obj       ! arguments
  integer                    :: status
  integer                    :: i

  if(.not. associated(obj) ) return

  if (obj%skip_wrapup) return
  obj%skip_wrapup = .true.


!<execute_only>
  if(pc_get_update_state() /= PC_BACKEND .and. &
     pc_get_update_state() /= PC_GUI) then
    !--- the actual number done will be either dooskip_tr_max OR
    !--- dooskip_nbr_done, whichever is smaller.  If we hit an EOF above,
    !--- then dooskip_nbr_done is 1 greater than actual number read, which
    !--- was placed in tr_max after hitting the EOF.  Therefore, we must use
    !--- the "min" function in the print statement.
    write(string,*)&
    'TRIN:  Read ',&
    min(dooskip_nbr_done(obj%DS),dooskip_get_tr_max(obj%DS)), ' traces'
    call string_compress_blanks(string,status)
    call pc_info(string(:status))
    do i = 1, obj%init_num_files
      if(associated(obj%files(i)%p)) status = trcio_close(obj%files(i)%p)
    enddo
  endif
  status = dooskip_wrapup(obj%DS)
!</execute_only>

end subroutine trin_wrapup

function trin_read_trace(obj,hd,tr) result (status)
  !----------- PASSED PARAMETERS ----------------------------------------
  type(trin_struct),pointer       :: obj                    ! parm block
  double precision,intent(out)  :: hd(:)                    ! headers
  real            ,intent(out)  :: tr(:)                    ! traces
  !----------- Return value
  integer                         :: status
  !----------- LOCAL VARIABLES ------------------------------------------
  integer                         :: first_sample,last_sample,num_samples,i
  real,allocatable,dimension(:)   :: tr_temp
  character(len=256)              :: string

  integer                         :: ifile
  integer                         :: itrace
  integer,save                    :: lastfile=0

  call trin_which_file_and_trace(obj, ifile, itrace)
  if (ifile == -1) then
    call pc_error('TRIN error with trin_which_file_and_trace'//&
    ' [called from "trin_read_trace"].  Call the programmer.')
    status = trcio_error
    return
  elseif (ifile == 0) then ! all files are at EOF
    status = trcio_eof
    go to 8 ! eof... 
  endif

  !--- If tstart > 0.0, I need to calculate the starting sample to load in.
  !--- If tstart < 0.0, I need to fill first part of trace with zeroes.

  if(obj%files(ifile)%p%ftype== 'SEGY') then
    call mapsegy(obj%mapsegy,obj%files(ifile)%p)
  endif

  if(obj%tstrt ==  obj%files(ifile)%p%tmin ) then
    !--- read the trace normally

    status = trcio_read_trace(obj%files(ifile)%p,hd,tr(:obj%ndpt),itrace)
    if ( obj%opt_group ) hd(obj%hdr_group) = (ifile-1)/obj%len_group + 1
    if(status == trcio_error) then
      write(string,'(A,I16,A)')'TRIN: NonFatal: trcio-error reading trace ',&
      itrace,' from file '//trim(obj%filenames(ifile))
      go to 9
    elseif(status == trcio_eof) then
      go to 8
    endif
    !--- If last_sample > trace length on disk, it is zero filled.

  elseif(obj%tstrt > obj%files(ifile)%p%tmin ) then
    if(obj%files(ifile)%p%dt == 0.0 ) then
      write(string,'(A,I16,A)')'TRIN: FATAL: dt=0.0 while at trc ',&
        itrace,' from file '//trim(obj%filenames(ifile))
      status = trin_error
      go to 9
    endif
    first_sample = 1 + nint((obj%tstrt - &
      obj%files(ifile)%p%tmin)/obj%files(ifile)%p%dt)
    num_samples  = obj%ndpt
    last_sample  = num_samples + first_sample - 1
    allocate(tr_temp(last_sample))
    status = trcio_read_trace(obj%files(ifile)%p,hd, tr_temp,itrace)
    if ( obj%opt_group ) hd(obj%hdr_group) = (ifile-1)/obj%len_group + 1
    if(status == trcio_error) then
      write(string,'(A,I16,A)')'TRIN: NonFatal: trcio-error reading trace ',&
      itrace,' from file '//trim(obj%filenames(ifile))
      go to 9
    elseif(status == trcio_eof) then
      go to 8
    endif
    !--- Trace in job gets the swath of requested data from file.
    !--- If last_sample > trace length on disk, it is zero filled.
    tr(1:obj%ndpt) =  tr_temp(first_sample:last_sample)
  elseif(obj%tstrt < obj%files(ifile)%p%tmin ) then
    if(obj%files(ifile)%p%dt == 0.0 ) then
      write(string,'(A,I16,A)')'TRIN: FATAL: dt=0.0 while at trc ',&
        itrace,' from file '//trim(obj%filenames(ifile))
      status = trin_error
      go to 9
    endif
    first_sample = nint((obj%tstrt - &
      obj%files(ifile)%p%tmin)/obj%files(ifile)%p%dt)
    num_samples  = obj%ndpt
    last_sample  = num_samples + first_sample
    allocate(tr_temp(last_sample))
    status = trcio_read_trace(obj%files(ifile)%p,hd, tr_temp,itrace)
    if ( obj%opt_group ) hd(obj%hdr_group) = (ifile-1)/obj%len_group + 1
    if(status == trcio_error) then
      write(string,'(A,I16,A)')'TRIN: NonFatal: trcio-error reading trace ',&
      itrace,' from file '//trim(obj%filenames(ifile))
      go to 9
    elseif(status == trcio_eof) then
      go to 8
    endif
    !--- First part of trace is zeros
    tr(1:-first_sample) =  0.0
    !--- Last part of trace gets values from the file.
    tr(1-first_sample:obj%ndpt) = tr_temp(1:last_sample)
    !--- If last_sample > trace length on disk, it is zero filled.
  endif
!            Flag the first trace of each file.  TTROT will use this to
!              write an eof between velocity model permsaves
    if((ifile.ne.lastfile).and.obj%vmod)then
      hd(31)=dble(1.0)
      lastfile=ifile
    endif

  if(allocated(tr_temp) ) deallocate(tr_temp)
  !--- status == trcio_ok

  !--- change these headers on the trace I just read in.

  call mutehw      (hd,tr,obj%ndpt,0.0,0)
  !--- removed, this is set in trcio.call lav_set_hdr (hd,tr,obj%ndpt)

  return

8 continue
  !--- status == [trcio_eof]
  if(allocated(tr_temp) ) deallocate(tr_temp)
  return

9 continue
  !--- status == [trcio_error || trin_error]
  tr(:) = 0e0
  hd(:) = 0d0
  if(allocated(tr_temp) ) deallocate(tr_temp)
  call string_compress_blanks(string,i)
  call pc_info(string(1:i))

end function trin_read_trace

! Returns ifile = itrace = 0 if error.
!
subroutine trin_which_file_and_trace(obj, ifile, itrace)
  type(trin_struct), pointer :: obj
  integer                    :: ifile
  integer                    :: itrace
  !-- local vars
  integer                    :: i,itrcpt

  logical                    :: traces_left(obj%num_files)



  select case (obj%order_local)

    case ('APP','APPEND')

      itrace = obj%trace_pointer
      do ifile = 1, obj%num_files
        if (itrace <= obj%files(ifile)%p%num_traces) return
        itrace = itrace - obj%files(ifile)%p%num_traces
      enddo

      ifile  = 0
      itrace = 0

    case ('MERGE')
      traces_left(:) = .true.

      itrcpt = obj%trace_pointer + obj%merge_offset
      itrace =    (itrcpt - 1) / obj%num_files  + 1
      do i = 1, obj%num_files
        if(itrace > obj%files(i)%p%num_traces) traces_left(i)=.false.
      end do 

      ifile  = mod(itrcpt - 1  , obj%num_files) + 1

      do i = ifile, obj%num_files
        if(traces_left(i)) then
          itrace =    (itrcpt - 1) / obj%num_files  + 1
          ifile = i
          return
        endif
        obj%merge_offset = obj%merge_offset + 1
      end do

      ifile  = mod(itrcpt - 1  , obj%num_files) + 1
      itrcpt = obj%trace_pointer + obj%merge_offset

      do i = 1, ifile-1
        if(traces_left(i)) then
          itrace =    (itrcpt - 1) / obj%num_files  + 1
          ifile = i
          return
        endif
        obj%merge_offset = obj%merge_offset + 1
      end do

      ifile  = 0
      itrace = 0

    case default

      call pc_error( &
        'TRIN:  bad value for order_local (how to order traces '//&
        'from within the list of files included with this instance'//&
        ' of TRIN). (inside of trin_which_file_and_trace subroutine)')
      ifile  = -1
      itrace = 0

  end select

end subroutine trin_which_file_and_trace

subroutine trin_make_downward_compatible(obj)
  type (trin_struct), intent(inout) :: obj

  character(len=3)                  :: merge

  ! Check for old keyword.
  !
  if (pc_process_keyword_present('merge')) then

    ! Shouldn't have both old and new.
    !
    if (pc_process_keyword_present('order_local') &
   .or. pc_process_keyword_present('order_prev' )) then

      call pc_error('TRIN:  error converting an old work file')

    else

      call pc_get('merge', merge)

      select case (merge)
        case ('YES')
          obj%order_prev = 'MERGE'
        case ('NO')
          obj%order_prev = 'NEW'
        case ('APP','APPEND')
          obj%order_prev = 'APPEND'
        case default
          call pc_error('TRIN:  error converting an old work file')
          return
      end select

      call pc_remove_process_keyword('merge')
      call pc_warning('TRIN:  converting old merge')

    endif

  endif

end subroutine trin_make_downward_compatible

end module trin_module
