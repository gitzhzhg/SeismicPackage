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
! Name       : TROT    (TRace OuTput to disk)
! Category   : io
! Written    : 2000-01-07   by: Bill Menger
! Revised    : 2008-05-13   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Write seismic traces to one or more disk files.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! [Note that not all the parameters shown here will have been implemented when
! testing begins for TROT.  Some may prove to be undesirable or unnecessary.]
!
!
! TROT writes traces to disk file(s). The PATHNAMES is the only input parameter
! that must be entered; defaults can be used for all other parameters.  TRIN is
! the corresponding process for reading disk files.
!
! Multi-Trace Compression (disabled)
!
! The CMPR compression mode within TROT allows the user to store seismic
! trace datasets on disk in a compressed form, taking fewer bytes of storage
! than would be required for the original volume before compression.
! Compressed datasets on disk are automatically recognized and decompressed by
! TRIN.
!
! Single-Trace Compression
!
! Details on the LBO and LBO2 compression modes are found in the documentation
! for primitive 'lbo.f90'.  LBO uses a single-trace compression approach.
!
! File Identification
!
! The user enters PATHNAMES of the form user@node:dir/subdir/file.ext
! Because the CBYT program uses file extensions to determine file type, do
! not use ".byt" ... also, when writing SEGY files, CBYT expects the file
! name to end in ".sgy".
!
! File Access
!
! PERMISSION is any valid unix chmod specification for future access
! of the file in ascii format: 3 parts
! the first part determines OWNER permission, the second part
! GROUP permission, and the third WORLD permission.  The characters mean:
!            - or blank = no access
!            x = execute permission (The file can be run as a program)
!            w = Write permission (The file can be written to)
!            r = Read permission (The file can be read)
! To provide OWNER=read/write, GROUP=r, WORLD=x you would use:
! PERMISSION = 'rw-r----x'
! To test out how permissions on a UNIX file work, type :
! $ touch tmp
! $ chmod 644 tmp
! $ ls -l tmp
! and see:
! -rw-r--r--   1 mengewm  man             0 Feb  3 07:42 tmp
! ^---Special bit (d=directory,l=symbolic link, - = regular file)
!  ^^^ --- OWNER permissions (rwx either set or unset (unset = "-")
!     ^^^ --- GROUP permissions (rwx)
!        ^^^ --- WORLD permissions (rwx)
!              ^ --- how many links to this file
! -rw-r--r--   1 mengewm  man             0 Feb  3 07:42 tmp
!                ^^^^^^^ --- OWNER's name
!                         ^^^^^^^ --- GROUP's name
!                                 ^^^^^^^^^ --- file size in bytes
! -rw-r--r--   1 mengewm  man             0 Feb  3 07:42 tmp
!                                           ^^^^^^^^^^^^ -- date last modified
!                                                        ^^^^ -- filename
! File Type
!
! Options for file TYPE are:
! (TYPE=TRCIO)   the preferred default,
! (TYPE=SEGY)    industry standard SEGY,
! (TYPE=LBO)     localized bit optimization
! (TYPE=LBO2)    localized bit optimization with NIL support

! DISABLED (TYPE=CMPR)    the new compressed trace format (also preferred),
! DISABLED (TYPE=QTROT)   an archaic CPS file format. (DISABLED 4 Mar 2004)
! [not implemented] (TYPE=VOXET), a special GOCAD file type,
!
! (SEGY and VOXET files have an externally defined
! format and do not contain all the information contained in the internal
! files.)  SEGYHEADER takes the name of a textfile used to supply the 3200 byte
! SEGY header.
!
! [The TRCIO file type replaces the following file types formerly used in CPS:
! 8-bit bytefile, TFILE (DINT and DOUT), traditional Cray STROT and DTROT and
! related file types.]
!
! File Structure
!
! The file structure for a disk file always contains trace and global sections
! and may optionally contain header and history sections.
! SEGY and VOXET files have an externally defined format
! and do not contain all the information contained in the internal files.  They
! are intended for communication with non-Conoco applications.
! QTROT format does not contain the full file header of a TRCIO file.
! It also has 32 bit trace headers instead of 64 bit headers.
! This format is deprecated and should not be used except to support temporary
! bridge systems in place while CPS is being built.
!
! (disabled) --
!  CMPR format contains sections describing what is in the file, similar to the
!  CPS standard file format, (TRCIO format).
!
! Retention [NOT IMPLEMENTED]
! RETENTION is the number of days the file will be retained after it was last
! accessed. There is no provision in the UNIX operating system to automatically
! remove a file based on this RETENTION.  It is not saved by the OS, but is
! found in the file's ASCII header (if the file is of TRCIO type.)
!
! File size
!
! Files consist (internally to CPS) of up to 9999 segments.  The base segment
! is always the user-supplied pathname of the file, and the extents 
! are named by adding ".nnnn" to the base file name.  This method allows CPS to
! see a file of very large size.  A parameter "EXTENT_SIZE_MB"
! determines how much space is in the base file and its extents.
! To guarantee file space, a parameter "RESERVE_DISK" may be used which will
! always write "0"s to an entire file segment prior to performing any other I/O.
! This takes longer with large extent sizes, thus the extent size is user-
! defined. (default is 250Megabytes).
!
! Trace Attributes
!
! NUM_BITS is the number of bits used for each trace sample on disk.  Whether
! to compress the dynamic range of samples is specified by
! (disabled) dyn_range_cmpr = YES/NO.
! This attribute is independent of WAVE_DR (wavelet dynamic range compression)
! (also disabled).
!
! History Selection

! HISTORY options to control transfer of histories to the file are ALL, MODEL,
! CURRENT or NONE.
!
!
! File Writing Options [APPEND OVERWRITE NEW]
!
! If WRITE=APPEND, then append traces to the existing file with the same name
! or create it if it does not exist.  If WRITE=OVERWRITE, then overwrite a file
! with the same name or create a new file if one does not already exist with
! the same name. If WRITE=NEW, then create a new file or abort if there is a
! file with that name.
!
! TROT will abort if there is a conflict between globals in an existing file and
! TROT process globals.
!
! TROT will print a summary of globals of any pre-existing files with the same
! name as specified in TROT.
!
!
! Trace Selection and Sorting
!
! TR_MAX is the maximum number of traces this process will write (total of all
! files).  Set TR_MAX to 0 to write no traces, set TR_MAX to a very large
! number to write all traces.
!
! SKIP_INIT will skip this number of traces that come in to TROT before
! TROT will begin writing any traces out. SKIP_INIT is honored EVEN if the
! METHOD is SEQ.
!
! NUM_DO will cause TROT to write NUM_DO traces (in sequence as they come)
! NUM_SKIP will cause TROT to ignore NUM_SKIP traces after each NUM_DO group
! before writing the next group of NUM_DO traces.
!
! EXAMPLE:  To read 10,000 traces, write out every 100th starting with trace
!           100, one would:
!           call TRIN with TR_MAX=10000
!           call TROT with TR_MAX=100,SKIP_INIT=99,NUM_DO=1,NUM_SKIP=99
!  Output will be a file with 100 traces on it, numbered 1-100, but they will
!  be the 100th, 200th, 300th, ... 10000th trace from the input file.
!
! HDR_FLAG is the flagword parameter.  If HDR_FLAG /= 0, only traces with a
! flag in HDR_FLAG will be written to a file.
!
! Global Summary
! This process prints, at setup time, a summary of the globals on each
! previously-existing file which will be appended to or might be overwritten.
! It also prints warning or error messages for any mis-matches between the
! file(s) and the CPS globals when appending traces to an existing file.
!
!
! File Verification [NOT IMPLEMENTED]
! The front end for this process provides the user with the option to verify
! (at job creation time) whether the output file(s) already exist, are readable
! and writeable, and (for APPEND=YES) contain incompatible globals which will
! cause the job to abort.  The front end will check the files across the
! network if necessary.
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!                            SEGY Header Words
! When TROT writes a SEGY file, it maps CPS headers to SEGY headers using the
! following mapping.  For byte offsets of the segyhd%xxxx words, either see
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
! 021:024     37     midpoint_xgrid        cdp
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
! 169:170                                  trwf
! 171:172                                  grnors
! 173:174                                  grnofr
! 175:176                                  grnlof
! 177:178                                  gaps
! 179:180                                  otrav
! 181:188
! 189:192
! 193:200
! 201:208
! 209:216
! 217:224
! 218:232
! 233:236
! 237:240                                  unused1
!
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
! 017:020     29     source_shotpoint      ep
! 021:024     37     midpoint_xgrid        cdp
! 025:028     38     midpoint_line         cdpt
! 099:100     39     pre                   sstat
! 101:102     40     post                  gstat
! 103:104     43     cum_resid_static      tstat
! 095:096     44     source_uptime         sut
! 097:098     45     receiver_uptime       gut
! 113:114     64     bottom_mute           mute
!
! Not all CPS header words have a corresponding SEGY header.  If you create a
! file with TROT in SEGY format, and read that file with TRIN, you will lose
! some header words.  Also, SEGY header words are integer, so you may lose
! precision making the round trip.
!
!                       TRACE COMPRESSION (disabled)
!
! The trace compression utility within TROT allows the user to store seismic
! trace datasets on disk in a compressed form, taking fewer bytes of storage
! than would be required for the original volume before compression.
! Compressed datasets on disk are automatically recognized and decompressed by
! TRIN.
!
! Trace Compression Algorithm
!
! The compression algorithm involves performing a wavelet transform and
! retaining only the the largest coefficients.  The largest coefficients are
! associated with the highest amplitude events on the trace and the smallest
! coefficients are associated with lowest amplitude events on the trace which
! are presumed to be noise.  Trace compression always involves some loss of
! data integrity.
!
! Trace compression is often characterized by its signal to noise ratio (S/N).
! In this context, "signal" refers to the original data before compression and
! "noise" refers to the difference between the uncompressed data after
! compression and the original data before compression.

! Dynamic range is the ratio of the maximum reading to the minimum reading
! (the minimum often being the noise level) which can be recorded by and
! read from an instrument without change of scale. (Encyclopedic Dictionary
! of Applied Geophysics, Fourth Edition).
!
! The actual number of coefficients retained (and the amount of compression) is
! determined by the user specified value for SN_RATIO, C_RATIO, or WAVE_DR.
! (Note: C_RATIO and WAVE_DR were disabled on 4 Mar 2004).
! Only one may be non-zero.  It determines which compression goal is honored.
! Header word
! values are compressed using a lossless compression algorithm.  The actual
! compression is done with vendor software provided by GeoEnergy.
!
!
! Trace Compression Options
!
!     If COMPRESS = SN_RATIO, then traces are compressed to maintain a
!     specified S/N ratio before being written to disk.  SN_RATIO is the desired
!     signal to noise ratio to be preserved in trace compression, in dB.
!     Larger values of SN_RATIO correspond to smaller values of the compression
!     ratio used in the compression.
!
!     If COMPRESS = C_RATIO, then traces are compressed by a specified
!     compression ratio before being written to disk.  The size of the original
!     data volume, in bytes, is C_RATIO times the compressed data volume, in
!     bytes.  Larger values of C_RATIO correspond to smaller values of signal
!     to noise ratio preserved by the compression.  (Disabled 4 Mar 2004)
!
!     If COMPRESS = WAVE_DR, then traces are compressed such that the specified
!     wavelet dynamic range (ratio) is preserved.  The ratio of the byte size
!     for compressed and uncompressed data will vary, depending upon the actual
!     minimum and maximum values.  (Disabled 4 Mar 2004)
!
! Normally SN_RATIO is the preferred option because it runs faster than the
! C_RATIO option.  It may also be the safer option as it allows the user to
! specify directly the signal to noise ratio to be preserved by the compression.
!
!
! Gathers and Trace Compression
!
! The trace compression algorithm expects that consecutive traces, within a
! gather, will have some similarity.  To avoid modeling across edges of gathers,
! header word HDR_GATH is used to define gathers.  HDR_GATH should be set so
! that its value changes at natural gather edges or at junctures between groups
! of similar traces.  This is accomplished by a call to GATHER before calling
! TROT with the COMPRESSION option.  HDR_GATH should be set to the same value
! used in the GATHER process.  For example:
!
!      Set HDR_GATH to 3 for input data in common shot, common receiver or CMP
!      order.
!
!      Set HDR_GATH to 6 for 2D pre-stack data in common offset order.
!
!      Set HDR_GATH to 8 for 3D pre-stack data in common offset order (sorted
!      from slow to fast: hdr 6, hdr 8, hdr 7).
!
!      Set HDR_GATH to 1 for a collection of traces that are independent of
!      each other.
!
! Amplitude Balance and Trace Compression
!
! Traces being compressed should be well balanced in amplitude with either a
! windowed or deterministic amplitude balance applied before compression.
! Because the compression algorithm retains the largest wavelet coefficients,
! unbalanced traces will not compress well.  The lower amplitude parts of
! unbalanced traces will suffer a loss of data integrity if C_RATIO is
! specified.  And unbalanced traces will have a poor compression ratio if
! SN_RATIO is specified.
!
! The effects of decaying data vertically and horizontally need to be studied
! further when the data is compressed using various techniques.
!
! Trace amplitude should also be balanced laterally, but this is less important
! than amplitude balance with each trace.
!
!
! Process Sequence and Trace Compression
!
! Trace compression before integrating or smoothing processes (such as DMO or
! migration) should not be problematic.  Trace compression before
! differentiating processes (such as decon) may be problematic because
! compression will reduce the noise level and decon is sensitive to noise
! levels.
!
! It is safest not to do trace compression before decon; if trace compression
! is done before decon it should be very mild.  Generally, the effects of any
! trace compression done before decon become progressively less noticable as
! more processes are added to the sequence after decon.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! Input traces pass through the process unchanged.
! File(s) are written in the format specified.
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of words in trace header       used but not changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
! GRID     grid transformation structure         used but not changed
!
! TROT will check agreement of some or all globals in existing files with those
! in the job and, may abort if disagreement is found.
! ALL globals will be written to the "jobglobals" section of the trcio file
! header.
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
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 84. 2008-10-23  Bill Menger  Added javaseis output option.
! 83. 2008-05-13  Stoeckley    Modify so that the segy header file input will
!                               behave properly in SeisSpace.
! 82. 2007-12-06  Bill Menger  Add extra digit to the largest extent size.
! 81. 2006-08-29  Stoeckley    Replace pc_register_tab_group w HelpSection line.
! 80. 2006-08-24  D. Glover    Added NULLIFY statements for Intel compiler.
! 79. 2006-06-20  Stoeckley    Add pc_register_tab_group for SeisSpace.
! 78. 2006-05-25  Cook/Menger  Removed trace compression (wavelet and dynamic
!                              range compression). Added SU output format.
!                              Modified MAPSEGY to default to LANDMARK.
!                              Added LBO2 file type.  LBO2 supports FNILs,
!                              which are used to flag salt in some internal
!                              programs).  User must explicitly choose LBO2 --
!                              it is NOT the TROT default because it requires
!                              an extra bit per sample and thereby would be
!                              wasteful in most seismic data cases.  LBO2
!                              uses file extension '.lbo'.  Also needed to add
!                              code to prevent TROT from redefining wtype in
!                              the LBO/LBO2 cases.
! 77. 2006-01-10  B. Menger    Removed Unused Variables.
! 76. 2005-07-11  Bill Menger  Modified trace count when error occurs to not
!                              count last trace (the one that failed).
! 75. 2005-04-12  Goodger      Added a test for pointer association on filename
!                              in end session trap.
! 74. 2005-04-06  Goodger      Issure a warning if data type is LBO, but user
!                              puts a .trc extension on the filename.
! 73. 2004-12-15  Goodger      Insure CREATE_CSV is set to NO if not trcio
!                              format.
! 72. 2004-09-23  Bill Menger  Made LBO the default format.
! 71. 2004-08-26  Bill Menger  Fixed FrontEnd to look for segy header files.
! 70. 2004-06-15  Bill Menger  Moved main parameters into main screen.
! 69. 2004-04-27  SMCook       Fixed problem for LBO mode wherein NUM_BITS was
!                               jumping back to the default value of 16 after
!                               leaving TROT screen and then coming back to it
!                               (introduced new variable update_count).
! 68. 2004-04-06  SMCook       Added LBO format support.
! 67. 2004-03-29  R Selzler    Corrected benign read outside array bounds,
!                              detected by compiler run time checks.
! 66. 2004-03-04  R Selzler    Disable C_RATIO, WAVE_DR and QTROT features.
! 65. 2004-03-03  Bill Menger  Allow non-extent files if desired, by setting
!                              extsize to large value (large enough to not split
!                              the file.)  If > 20480 mb, disable reserve space.
! 64. 2004-02-04  R Selzler    Removed QUIT_ON_ERROR parameter.
!                              TROT will now always quit (previous default
!                              was to continue writing, without meaningful
!                              recovery proceedures).
! 63. 2004-01-27  R Selzler    Added support for GeoEnergy compression
!                              using wavelet dynamic range (WAVE_DR).
! 62. 2003-12-15  Goodger      Change default permissions to rw-rw-r--.
! 61. 2003-11-18  SMCook       User now warned that SEGY TR_LAV mode with SEGY
!                               8-bit and SEGY 16-bit output results in relative
!                               amplitudes being lost due to unrecoverable trace
!                               normalization.
! 60. 2003-10-07  Bill Menger  Added ability to append data to a file.
! 59. 2002-10-07  Ed Schmauch  Added cio_chmod to trot_wrapup.
! 58. 2002-09-20  K. Goodger   Add process number to write_history_cards call.
! 57. 2002-08-26  K. Goodger   Write output files names to history.
! 56. 2002-07-17  Ed Schmauch  Optional arguments extsize and alloc_disk are
!                              removed from trcio_open; I must call
!                              cio_set_file_ext_size and
!                              cio_set_file_space_commit myself.
! 55. 2002-07-01  Ed Schmauch  Added multifile, keeping downward compatibility.
! 54. 2002-05-20  M.L.Sherrill Added option to create a workstation CSV type
!                              file.
! 53. 2002-04-18  Ed Schmauch  Close files before truncating them.
!                              No direct calls to cmprio_module, only indirct
!                              access through trcio_module.
! 52. 2002-02-04  Ed Schmauch  Removed old multifile options.  Removed TIM_BEG
!                              and TIM_END; TROT will output according to
!                              globals TSTRT and TEND.  Changed trot_struct
!                              members hdi, tri, and ntr to global variables and
!                              renamed accordingly; renamed trot_alloc_hdi_tri
!                              to trot_alloc_global_hdi_tri.  This helps
!                              reduce the memory requirements for compression.
!                              Removed use of mutehw with MUTEHW_SET before
!                              writing since without TIM_BEG and TIM_END there
!                              is no need to shift the mutes.
!                              Added some nullifies to make work with intel
!                              compiler.
! 51. 2001-11-12  Ed Schmauch  Fixed bug in trot_cmpr.
! 50. 2001-11-08  Stoeckley    Add file selection dialog, status line, and
!                               default extensions for PATHNAME.
! 49. 2001-11-07  Ed Schmauch  Fixed bug, compressed output could ignore do-skip
!                              pattern.  Fixed bug in trot_alloc_hdi_tri.
! 48. 2001-11-06  Ed Schmauch  Removed all Conoco extensions to segy format.
! 47. 2001-10-29  Stoeckley    Fix problem where mute headers were not reset
!                               when writing traces with TIM_BEG and TIM_END
!                               different from globals.
! 46. 2001-10-24  Bill Menger  Required gather code fixed, re-modified gui.
! 45. 2001-10-22  Bill Menger  Modified memory usage for gathered traces.
! 44. 2001-10-18  Ed Schmauch  Put in nint to remove roundoff error in segy
!                              binary header dt.
! 43. 2001-10-18  Bill Menger  Required GATHER before compression,modified gui.
! 42. 2001-10-09  Ed Schmauch  Changed header equality check in gather code
!                              to use nint.
! 41. 2001-08-27  K. Goodger   Replace trcio and cmprio calls with new manhist
!                              routines.
! 40. 2001-07-03  Bill Menger  Added internal gather code.
! 39. 2001-06-18  Ed Schmauch  Allowed 8- and 16-bit SEGY to use OPT_NORM
!                              and AMPL_MAX. PRODUCTION.
! 38. 2001-05-14  Goodger      Move segy header mapping to primitive mapsegy.
! 37. 2001-04-30  Ed Schmauch  Added parameters OPT_NORM and AMPL_MAX.
!                              These parameters are only used for 8 and 16 bit
!                              TRCIO files.
!                              Made CURRENT the default HISTORY for 8 and 16 bit
!                              TRCIO files.
! 36. 2001-03-21  Bill Menger  Removed unused trcio structure elements.
! 35. 2001-02-21  Bill Menger  Added trace compression.
!                              Changed default extent size back to 2048
!                              Changed default disk allocation to "NO"
!                              Open file only on first call to trot proper.
!                              Added pc_warning vs pc_info on time range error.
! 34. 2001-01-25  Bill Menger  Modified documentation for TIM_BEG,TIM_END,
!                              added history write before file opens to include
!                              current TROT in the file's history, and fixed
!                              bug on TIM_BEG, TIM_END where negative tstrt
!                              is present.
!                              Forced a bhists, bhist call even when not a
!                              trcio file type.
! 33. 2001-01-12  Bill Menger  Changed default extent size to 256 Mbytes.
! 32. 2001-01-11  Bill Menger  Fixed tim-beg, tim-end problem,added output of
!                              EBCDIC header when writing SEGY files, added
!                              warnings when writing time range different from
!                              input trace range, changed PRI_INIT,INC,LAST and
!                              SEC_INIT,INC,LAST to allow Floating pt. input.
! 31. 2000-12-13  Bill Menger  Fixed bug about extent size.
! 30. 2000-12-12  Bill Menger  Fixed bug that called pc_get_lun() after update
!                              which caused history to not work correctly.
!                              Added skip_wrapup flag, removed wrapped_up flag.
!                              Added variable extent size option.
!                              Added reserve_disk option.
!                              Added file_size and truncate feature.
! 29. 2000-12-01  Bill Menger  Modified open statement for segyheader and
!                              allowed it to read files without line feeds
!                              in them if desired.
! 28. 2000-11-29  Bill Menger  Reversed quit-on-error flag. (oops)
! 27. 2000-11-27  Bill Menger  Fixed bug where QTROT files sometimes did not
!                              reopen correctly. Added quit_on_error flag.
! 26. 2000-11-20  Bill Menger  Fixed bug in file closer for case when more than
!                              30 files are opened simultaneously.
! 25. 2000-11-20  Bill Menger  Added better killed message and set ntr-fatal on
!                              i/o error... also moved some segy-mapping codes
!                              into if-blocks so they don't execute always.
! 24. 2000-11-13  Bill Menger  Modified mode = multiple KLUDGE file naming for
!                              old cluster support.
! 23. 2000-11-09  R.S.Day      Added optional SEGY custom header mapping
! 22. 2000-10-24  Bill Menger  Modified to handle situations where file array
!                              tries to open a file outside of the boundaries
!                              of the primary and secondary bins.
! 21. 2000-10-17  Bill Menger  Added mode=multiple option.
! 20. 2000-09-26  Bill Menger  Reworked doc and gui, added compression parms,
!                              added write of jobglobals section to file.
! 19. 2000-08-17  Bill Menger  Add history capability.
! 18. 2000-07-18  Bill Menger  Added hdr_flag option.
! 17. 2000-07-11  Bill Menger  Modified so that SEGY is always written
!                              big-endian
! 16. 2000-07-07  Bill Menger  Added "QTROT file" support.
! 15. 2000-05-15  Bill Menger  Fixed segyheader field to not go grey after inp.
! 14. 2000-05-08  Bill Menger  Added the do_skip primitive.
! 13. 2000-04-19  Bill Menger  Changed byte position to "pos" vector position
!                              to allow for very large files.
! 12. 2000-04-11  Bill Menger  Modified help for NUM_BITS_HD to NUM_BITS_HDR
! 11. 2000-04-03  Bill Menger  Added a return to segyheader trap to quit if
!                              not type SEGY.
! 10. 2000-02-24  Bill Menger  Modified pc_get_update_state() call to include
!                              PC_GUI to keep bad printout from showing up.
!  9. 2000-02-21  Bill Menger  Added num_bits_hdr information to FE.
!  8. 2000-02-17  Bill Menger  Changed error message on segy header file open.
!  7. 2000-02-14  Bill Menger  Modified permission stuff
!  6. 2000-02-03  Bill Menger  Added segy doc, fixed traps to use keyword arg,
!                              grayed out unsupported parameters.
!  5. 2000-01-27  Bill Menger  Made traps go to uppercase except for filenames.
!  4. 2000-01-21  Bill Menger  Added different word types for INT and CMPI
!                              Minor changes to support compression, fixed
!                              trap for permission (still not used!)
!  3. 2000-01-18  Bill Menger  Added pathname module calls,nbits to segybhed.
!  2. 2000-01-11  Bill Menger  minor code revision around the trcio_write call.
!                              Added front-end calls to clean up CFE.
!  1. 2000-01-07  Bill Menger  Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
! File pathname or sections thereof may be platform dependent.
! RETENTION and PERMISSION may not apply uniformly across all nodes.
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
! No special requirements.
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
!  None provided.
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! File Global Format
!
!    Globals corresponding to user-specified parameters:
!
!  RETENTION   = 0 to 9999 days.
!  PERMISSION  = valid unix chmod specification 'rwx format'
!  TYPE        = TRCIO, LBO, LBO2, SU, or SEGY (VOXET not available).
!  NUM_BITS    = 1 to 64 bits for each trace value.
!  NUM_BITS_HDR= 1 to 64 bits for each header value.
!  DT          = sample interval on trace in seconds.
!  (disabled below) --
!  DYN_RANGE_CMPR = YES or NO (dynamic range compression) (if yes, samples are
!                   marked as CMPI type.)
!
! Other globals may be added to this list and some others may also be
! calculated from those listed.
!
!
! Word Type
!
! The word type for trace values is IBM for SEGY files and IEEE for all other
! file types.  The 32-bit words are floating point and the shorter words are
! integer.  Except for SEGY and VOXET, a scale factor is included with each
! trace with integer words to allow restoration of the original values (with
! some loss of precision).
! Word type can be set to INT or CMPI (integer or compressed integer) if desired
!
! The word type for trace header words is Integer Full and half words,
! ASCII and IEEE floating point for SEGY files (ASCII and IEEE only for
! optional conoco-specific header words),
! none for VOXET files (no header words), IEEE (64-bit) floating point for
! TRCIO file types, and IEEE (32 bit) for QTROT files.
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS TROT/NC=80>
!EXTENT_SIZE_MB=`IIIIII  RESERVE_DISK=`CC  TYPE=`CCCCC  PERMISSION=`SSSSSSSS
!
!HISTORY=`CCCCCCC WRITE=`CCCCCCCCC NUM_BITS=`II
!
!HDR_FLAG=`II OPT_NORM=`CCCCCCCC AMPL_MAX=`FFFFFFFF  CREATE_CSV=`CC
!
!<INCLUDE dooskip.f90>
!
!<include mfile.f90>
!
!<NS Multi-File/NC=80>
!  `----------------------------------------
!  |OPT_SWITCH=`CCCC  FILE_MAX= `IIIIIIIIII
!  |
!  |HDR_BIN = `II
!  |BIN_INIT= `FFFFFFFFF
!  |BIN_INC = `FFFFFFFFF
!  |BIN_LAST= `FFFFFFFFF
!  |BIN_TOT = `IIIIIIIIII
!  `----------------------------------------
!<NS SEGY/NC=80>
!  `----------------------------------------------------------------------------
!  |                  (ebcdic)
!  |FOUND=`XXXXXXXX  SEGYHEADER=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!  |
!  `----------------------------------------------------------------------------
!<INCLUDE mapsegy.f90>
!<PARMS SEGYHEADER[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="EXTENT_SIZE_MB">
!<Tip> This is the file extent size in Mbytes. The default is 250.</Tip>
! Default = 250
! Allowed = 0 - 2000000
! Your disk file can be comprised of up to 9999 segments.  A base segment and
! up to 9999 extents will be seen by the system as one file.  If you need a lot
! of disk space for your file, it can be as large as 2 Terabytes if you were
! to use 2000000 for each of 255 extent sizes. If you are asking the system to 
! initialize your files (with RESERVE_DISK=YES) before writing to disk (in
! order to guarantee you have adequate disk space) then you might consider a
! smaller extent size (if you don't need 512 Gbytes).  You could use 100 Mbytes
! for instance, and the initialization would proceed in 100 Mbyte chunks.  If
! you used 2Gbytes then initialization will proceed by initializing
! one entire 2Gbyte chunk at a time.  Your tradeoff is maximum file size vs.
! speed of initialization of each extent.  If you choose > 20480 then the
! file will not be pre-initialized and your file space is not guaranteed.
! By using a large extent size > 20480 (extent size > 20Gbytes ) you can write
! your data to a "monolithic" file in  CPSDATA or CPSTEMP but your
! disk space will not be preallocated.
!-------------------------------------------------
!   EXTENT SIZE              MAX FILE SIZE
!     128 Mbytes               32   Gbytes
!     256 Mbytes               64   Gbytes
!     512 Mbytes              128   Gbytes
!    1024 Gbyte               256   Gbytes
!    2048 Gbyte               512   Gbytes
!   20480 Gbyte     up to 4.98 Terabytes reserve capability allowed.
!  >20480 Gbyte     up to size of file system capacity not reserved.
!--------------------------------------------------
!</Help>
!
!<Help KEYWORD="RESERVE_DISK">
!<Tip> YES reserves disk space, each file extent is zero-filled.</Tip>
! Default = NO
! Allowed = YES/NO
! If YES, then when the file or a file extent is opened, the disk will first
! be filled with 0s to ensure the file space is reserved.  This takes longer
! up front but guarantees you have the space needed.  If EXTENT_SIZE_MB >20480
! this is turned to NO and the system will NOT check for availability of space.
!
!</Help>
!
!<Help KEYWORD="TYPE">
!<Tip> TYPE of file to be written. </Tip>
! Default = LBO  
! Allowed = JSEIS  (javaseis format)
! Allowed = TRCIO  (32-bit IEEE format data samples)
! Allowed = LBO    (Local Bit Optimization format)
! Allowed = LBO2   (LBO with support for NILs)
! Allowed = SEGY   (An industry standard SEGY file.)
! Allowed = SU     (A CWP "segy-like" format)
! Note that SEGY files have an externally defined format and do not
! contain all the information preserved in the other formats.
!</Help>
!
!<Help KEYWORD="PERMISSION">
!<Tip> Permission for future access of the file. </Tip>
! Default = rw-rw-r--
! Allowed = valid chmod specification
! The default value is the permission that is usually set for your account.
!</Help>
!
!<Help KEYWORD="HISTORY">
!<Tip> How to control transfer of processing history. </Tip>
! Default = MODEL for  >= 32-bit TRCIO files.
! Default = CURRENT for <= 16-bit TRCIO files.
! Allowed = ALL     (Write all old histories.)
! Allowed = MODEL   (Save a set of histories that model the processing.)
! Allowed = CURRENT (Write current history only.)
! Allowed = NONE    (Do not write any history.)
! History can only be written to file types of "TRCIO and LBO/LBO2".
!</Help>
!
!<Help KEYWORD="WRITE">
!<Tip> File writing options. </Tip>
! Default = OVERWRITE
! Allowed = OVERWRITE
! Allowed = NEW
! Allowed = APPEND
! If WRITE=OVERWRITE, then overwrite a file with the same name or create a new
! file if one does not already exist with the same name.
!
! If WRITE=NEW, then create a new file or abort if there is already a file with
! that name.
!
! If WRITE=APPEND, then append traces to the existing file with the same name
! or create it if it does not already exist.  Abort if there is a conflict
! between file globals and process globals.
!
!</Help>
!
!          tabgroup = SEGY
!
!<Help KEYWORD="FOUND">
!<Tip> This tells the status of the EBCDIC header file you specify. </Tip>
!</Help>
!<Help KEYWORD="SEGYHEADER">
!<Tip> Pathame of text file used to supply the 3200 byte SEGY header. </Tip>
! Default = blank
! Allowed = char
! This file must consist of exactly 40 lines with no more than 80 characters
! each.  If no file is specified, a default empty SEGY header will be supplied.
! SEGYHEADER is active only if TYPE = SEGY.
!</Help>
!
!          tabgroup = Main Tab
!
!<Help KEYWORD="NUM_BITS">
!<Tip> Number of bits used for each trace sample in the file. </Tip>
! Default = 32  (16 for LBO, LBO2 formats)
! Allowed = 1 - 64  (6 - 20 for LBO, LBO2 formats)
! NUM_BITS must be 16 or 32 for SEGY files and 8 or 32 for VOXET files.
! QTROT requires 32 bits.
! Normally it will be 8, 16, or 32.
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! If HDR_FLAG = 0, then all traces are written.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are written. Flag is set == .true. IF the
! value in header(hdr_flag) is > 0.0; otherwise if header(hdr_flag) <= 0.0,
! flag is UNSET (flag == .false.)
!</Help>
!
!<Help KEYWORD="OPT_NORM">
!<Tip> Option to scale values by trace LAV or by a specified value. </Tip>
! Default = TR_LAV
! Allowed = TR_LAV
! Allowed = AMPL_MAX
!
! If OPT_NORM = TR_LAV, integer samples are normalized by individual trace so
! that the largest possible integer absolute amplitude for a trace is that
! trace's LAV.  If OPT_NORM = AMPL_MAX, all integer samples are scaled so the
! largest possible integer absolute amplitude is the user-specified value
! AMPL_MAX.
!</Help>
!
!<Help KEYWORD="AMPL_MAX">
!<Tip> Amplitude for the largest possible integer sample value. </Tip>
! Default = 10.0
! Allowed = real>0.0
!
! If OPT_NORM = AMPL_MAX, all integer samples are scaled so that the largest
! possible integer absolute amplitude is the user-specified value AMPL_MAX.
!</Help>
!
!      tabgroup = Multi-File
!
!<Help KEYWORD="OPT_SWITCH">
!<Tip> Write traces into multiple files by header or trace count. </Tip>
! Default = COUNT
! Allowed = COUNT  Write FILE_MAX traces into each file.
! Allowed = HDR    Write into multiple files based on header HDR_BIN.
! If compressing output or writing a csv file, must use COUNT to a single
! file.
!</Help>
!
!<Help KEYWORD="HDR_BIN">
!<Tip> Header word to use to bin into multiple files. </Tip>
! Default = 3
! Allowed = Any header word.
! Only active if OPT_SWITCH = HDR.
!</Help>
!
!<Help KEYWORD="BIN_INIT">
!<Tip> Bin center of first bin. </Tip>
! Default = 1.0
! Allowed = Any real value.
! Only active if OPT_SWITCH = HDR.
!</Help>
!
!<Help KEYWORD="BIN_INC">
!<Tip> Distance between bin centers.  Also the width of each bin. </Tip>
! Default = 1.0
! Allowed = Any non-zero, real value.
! Only active if OPT_SWITCH = HDR.
!</Help>
!
!<Help KEYWORD="BIN_LAST">
!<Tip> Bin center of last bin. </Tip>
! Default = 1.0
! Allowed = Any real value.
! Only active if OPT_SWITCH = HDR.
!</Help>
!
!<Help KEYWORD="BIN_TOT">
!<Tip> Total number of bins. </Tip>
! Default = 1
! Allowed = Any positive number.
! Only active if OPT_SWITCH = HDR.
!</Help>
!
!<Help KEYWORD="FILE_MAX">
!<Tip> Maximum number of traces written to each file. </Tip>
! Default = 99999999
! Allowed = Any positive number.
! Only active if OPT_SWITCH = COUNT.
!</Help>
!
!       tabgroup = Main Tab
!
!<Help KEYWORD="CREATE_CSV">
!<Tip> Create a CSV file from your trot file? </Tip>
! Default = NO
! Allowed = YES/NO
! Create a workstation CSV file after your trot file is written.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

module trot_module

  use wrdc_module
  use pc_module
  use named_constants_module
  use trcio_module
  use cpsio_module
  use swap_module
  use segy_module
  use cio_module
  use dooskip_module
  use string_module
  use hist_module
  use manhist_module
  use mapsegy_module
  use mfile_module
  use finquire_module
  use pathcheck_module
  use mth_module
  use mem_module

  implicit none

  private

  !--- PROCEDURES
  public :: trot_create     ! uses the parameter cache.
  public :: trot_initialize
  public :: trot_update     ! uses the parameter cache.
  public :: trot_delete
  !--- DATA STRUCTURES
  public :: trot_struct

!<execute_only>
  public :: trot            ! main execution (trace processing) routine.
  public :: trot_wrapup
!</execute_only>

  character(len=100),save,public :: trot_ident = &
  '$Id: trot.f90,v 1.84 2008/10/24 21:03:11 mengewm Exp $'

  logical,save                     :: called_bhist

  !--- global variables within the module -----------------------

  double precision,dimension(:,:),pointer,save  :: global_hdi
  real            ,dimension(:,:),pointer,save  :: global_tri
  integer                                ,save  :: global_ntr  = 0
  integer                                ,save  :: global_nwih
  integer                                ,save  :: global_ndpt
  integer                                ,save  :: global_num_trots = 0

  integer,parameter  :: LBO_MIN = 6
  integer,parameter  :: LBO_MAX = 20

  ! Structure file_ptr to enable pointer to pointer.
  !
  type :: file_ptr
    type(trcio_struct), pointer :: p
  end type file_ptr

  type :: trot_struct
    private

    !--Globals--

    integer                    :: numtr
    integer                    :: nwih
    integer                    :: ndpt
    real                       :: tstrt
    real                       :: dt
    type(grid_struct)          :: grid

    !--PARAMETERS --

    type(dooskip_struct),pointer :: DS
    type(mapsegy_struct),pointer :: mapsegy
    integer                    :: retention  ! (9999)
    character(len=16)          :: permission ! rw,rw,r or rwxrwxrwx style
    character(len=6)           :: type ! file type TRCIO,LBO,LBO2,SEGY,SU,QTROT,JSEIS
    character(len=9)           :: found ! status on segy ebcdic header.
    character(len=FILENAME_LENGTH) :: segyheader ! file name for ebcdic hdr.
    integer                    :: num_bits ! 8-64 allowed.
    integer                    :: num_bits_hdr ! 8-64 allowed.
!    character(len=3)           :: dyn_range_cmpr ! yes or no.
    character(len=8)           :: history  !NONE | ALL | MODEL | CURRENT
    character(len=9)           :: write    !OVERWRITE | NEW | APPEND
    integer                    :: hdr_flag ! 0 - 64
    character(len=3)           :: reserve_disk  ! yes/no
    character(len=3)           :: create_csv    ! yes/no
    integer                    :: extent_size_mb    ! extent size in mbytes.
!    character(len=8)           :: compress ! type of compression
    logical                    :: gathered ! is the data gathered?


    ! opt_norm and ampl_max only apply to integer data, 8-bit and 16-bit.
    character(len=8)           :: opt_norm    ! TR_LAV || AMPL_MAX
    real                       :: ampl_max    ! process parameter

    !---- File information from file header ---

    character(len=4)           :: wtype    ! word type (IEEE, IBM)
!    integer                    :: c_ratio
!    integer                    :: sn_ratio
!    integer                    :: wave_dr

    !--Local saved vars --

    integer                    :: stdout
    integer                    :: ipn
    logical                    :: skip_wrapup
    character(len=pc_datacard_length), pointer     ,dimension(:) :: global_cards
    integer                    :: num_global_cards

    type(file_ptr)                , dimension(:), pointer :: files
    character(len=FILENAME_LENGTH), dimension(:), pointer :: filenames
    integer                                               :: num_files
    integer                       , dimension(:), pointer :: traces_written
    integer                                               :: tot_traces_written
    integer                                               :: orphan_traces

    type(mfile_struct), pointer :: mfile

    ! Multifile parameters
    !
    character(len=5)            :: opt_switch
    integer                     :: hdr_bin
    double precision            :: bin_init
    double precision            :: bin_inc
    double precision            :: bin_last
    integer                     :: bin_tot
    integer                     :: file_max

    integer                     :: update_count

  end type trot_struct

  type(trot_struct),pointer,save :: trp ! need this for traps

    integer,parameter          :: num_opt_type = 6
    character(len=5),parameter :: opt_type(num_opt_type) &
      = (/'TRCIO','SEGY ','LBO  ','LBO2 ','SU   ','JSEIS'/)
    !integer,parameter          :: num_opt_dyn_range_cmpr = 2
    !character(len=3),parameter :: opt_dyn_range_cmpr(num_opt_dyn_range_cmpr) &
    !  = (/'YES','NO '/)
    integer,parameter          :: num_opt_history = 4
    character(len=8),parameter :: opt_history(num_opt_history) &
      = (/'MODEL   ','ALL     ','NONE    ','CURRENT '/)
    integer,parameter          :: num_opt_write = 3
    character(len=9),parameter :: opt_write(num_opt_write) &
      = (/'OVERWRITE','NEW      ','APPEND   '/)
    integer,parameter          :: num_opt_reserve_disk = 2
    character(len=3),parameter :: opt_reserve_disk(num_opt_reserve_disk) &
      = (/'YES','NO '/)
    integer,parameter          :: num_num_bits = 4
    character(len=2),parameter :: opt_num_bits(num_num_bits) &
      =(/' 8','16','32','64'/)
! 4 Mar 2004, R Selzler.
! Disable all compression options except NONE and SN_RATIO.
! Retain the code, just in case its needed in the future.
!   integer,parameter          :: num_compress = 4
!   character(len=8),parameter :: opt_compress(num_compress) &
!     =(/'NONE    ','SN_RATIO','C_RATIO ','WAVE_DR '/)
!    integer,parameter          :: num_compress = 2
!    character(len=8),parameter :: opt_compress(num_compress) &
!      =(/'NONE    ','SN_RATIO'/)
    integer,parameter          :: opt_norm_noptions=2
    character(len=8),parameter :: opt_norm_options(opt_norm_noptions) &
      =(/'TR_LAV  ','AMPL_MAX'/)
    integer,parameter          :: num_opt_csv = 2
    character(len=3),parameter :: opt_csv(num_opt_csv) &
      = (/'YES','NO '/)
    integer,parameter          :: num_opt_opt_switch = 2
    character(len=5),parameter :: opt_opt_switch(num_opt_opt_switch) &
      = (/'HDR  ','COUNT'/)

contains

  subroutine trot_create (obj)
    type(trot_struct),pointer :: obj       ! arguments

    allocate (obj)

    nullify  (obj%DS)
    nullify  (obj%mapsegy)
    nullify  (obj%global_cards)
    nullify  (obj%files)
    nullify  (obj%filenames)
    nullify  (obj%traces_written)
    nullify  (obj%mfile) ! jpa

    call mapsegy_create    (obj%mapsegy)

    call mfile_create  (obj%mfile)
    call mfile_set_type(obj%mfile, MFILE_WRITE_ANY_FILE) !  =2

    call trot_initialize   (obj)

    global_num_trots = global_num_trots + 1

  end subroutine trot_create

  subroutine trot_delete (obj)
    type(trot_struct),pointer :: obj       ! arguments
    integer                   :: ierr

    if(associated(obj) ) call trot_wrapup (obj)
    call mapsegy_delete    (obj%mapsegy)
    call   mfile_delete    (obj%mfile  )

    if (associated(obj%files         )) deallocate(obj%files         )
    if (associated(obj%filenames     )) deallocate(obj%filenames     )
    if (associated(obj%traces_written)) deallocate(obj%traces_written)

    if (associated(obj)) deallocate(obj)

    global_num_trots = global_num_trots - 1

    if ((global_num_trots == 0) .and. (global_ntr > 0)) then

      global_ntr = 0

      deallocate(global_hdi, STAT=ierr)
      if (ierr /= 0) goto 9

      deallocate(global_tri, STAT=ierr)
      if (ierr /= 0) goto 9

    endif

    return

9   write(obj%stdout, '(a)') 'deallocate error in trot_delete'

    return

  end subroutine trot_delete

  subroutine trot_initialize (obj)
  type(trot_struct),pointer :: obj       ! arguments

  if(.not. associated(obj) ) return
    !----------Here we initialize all data that will be subsequently updated.

    if(dooskip_initialize(obj%DS) /= dooskip_ok) continue
    call dooskip_neg_skip_ok(obj%DS,.false.)
    !--- this data comes from the parm-cache globals
    call pc_get_global ('numtr', obj%numtr) ! maximum number of traces.
    call pc_get_global ('ndpt' , obj%ndpt)  ! number of trace samples.
    call pc_get_global ('nwih' , obj%nwih)  ! number of header words.
    call pc_get_global ('tstrt', obj%tstrt) ! time of 1st trace sample (sec).
    call pc_get_global ('dt'   , obj%dt)    ! trace sample interval (sec).
    call pc_get_global ('grid' , obj%grid)  ! grid transform data structure.
    !--- this data comes from the parm-cache data cards for this process

    obj%ipn = pc_get_ipn()
    obj%retention  = 9999
    obj%permission = 'rw-rw-r--'
    obj%type       = 'SEGY'
    obj%segyheader = PATHCHECK_EMPTY
    obj%found      = '-NONE-'
    obj%num_bits   = 32 
    obj%num_bits_hdr = 64
    !obj%dyn_range_cmpr   = 'NO'
    obj%history    = 'MODEL'
    obj%write      = 'OVERWRITE'
    obj%hdr_flag   = 0
    obj%extent_size_mb   = 25000
    obj%reserve_disk  ='NO'
    obj%create_csv = 'NO'

    obj%update_count = 0   ! helps with GUI decisions

    obj%opt_norm     = 'TR_LAV  '
    obj%ampl_max     = 10.0

    !--- This data should come from the file
!    obj%sn_ratio   = 50
!    obj%compress   = 'SN_RATIO'
!    obj%c_ratio    = 5
!    obj%wave_dr    = 50

    !--- this data comes from this process itself

    call pc_put_sensitive_array_flag('sbyte_arrayset', .false.)
    obj%num_global_cards = 0
    call pc_put_sensitive_field_flag ('found' ,  .false.)

    call mapsegy_initialize(obj%mapsegy)
    call trot_multifile_initialize(obj)
    call trot_update (obj)

  end subroutine trot_initialize

  subroutine trot_update (obj)
    type(trot_struct),target              :: obj

    integer                               :: i,istat,nc,status
    character (len=80)                    :: card
    character (len=132)                   :: errmsg

    ! old_ampl_max is used to keep AMPL_MAX >= zero.   ehs 25apr01
    real                                  :: old_ampl_max

    ! old_type and old_num_bits are used to default HISTORY to CURRENT
    ! for 8- and 16-bit TRCIO files.   ehs 26apr01
    !
    character (len=6)                     :: old_type
    integer                               :: old_num_bits

    !--- point trp for trap routines.
    trp => obj
    if(.not. associated (trp) ) return

    !--- add skip_wrapup flag.
    obj%skip_wrapup = .true.

    !--- get stdout unit number from parm cache.

    obj%stdout = pc_get_lun()         ! if needed.

    !--- get globals from parm cache.
    call pc_get_global ('numtr', obj%numtr) ! maximum number of traces.
    call pc_get_global ('gathered', obj%gathered) ! is data gathered?

    !--- get parameters for this instance of my process from parm cache.

    call pc_put_sensitive_field_flag ('type'     ,  .true.)
    call pc_put_sensitive_field_flag ('permission', .true.)
    call pc_put_sensitive_field_flag ('history'  ,  .true.)
    call pc_put_sensitive_field_flag ('write'    ,  .true.)
    call pc_put_sensitive_field_flag ('num_bits' ,  .true.)

    call pc_put_sensitive_field_flag ('tr_max'   ,  .true.)
    call pc_put_sensitive_field_flag ('num_do'   ,  .true.)
    call pc_put_sensitive_field_flag ('skip_init',  .true.)
    call pc_put_sensitive_field_flag ('num_skip' ,  .true.)

    call pc_put_sensitive_field_flag ('hdr_flag' ,  .true.)

    call pc_put_sensitive_field_flag ('create_csv' ,  .true.)

    ! To default HISTORY for 8- and 16-bit TRCIO files to CURRENT,
    ! we check if the user changed the file type to TRCIO or
    ! changed the num_bits from >= 32 to <= 16.  If yes, we set
    ! HISTORY to CURRENT unless HISTORY is NONE, in which case we
    ! leave it NONE.  Note that this is not reversible, if
    ! the user changes from TRCIO to something else, HISTORY does
    ! not change.   ehs 26apr01
    !
    old_type     = obj%type
    old_num_bits = obj%num_bits

    if(dooskip_update(obj%DS) /= dooskip_ok) &
      call pc_error('TROT: error applying dooskip_update.')

    call pc_get ('type'     , obj%type     , trot_type_trap)

    call pc_put_sensitive_screen_flag('SEGY',.false.)
    call pc_put_sensitive_field_flag ('segyheader' ,  .false.)
    call pc_put_sensitive_field_flag ('num_bits' ,  .false.)
    call pc_put_sensitive_field_flag ('history'  ,  .true.)
!   call pc_put_sensitive_field_flag ('compress' ,  .true.)

    ! The following call was added because it was not always
    ! being called from this update routine.  SeisSpace requires all
    ! parameters to be put into the parameter cache by the update routine.
    ! This is because SeisSpace has no other way to find out what
    ! parameters are to be displayed in the gui.

    call pc_put_gui_only ('found    ', '-NONE-')          ! changed by stoeckley 2008-04-22

    select case (obj%type)

      case ('SEGY')
!       obj%compress = 'NONE'
        obj%history  = 'NONE'
        obj%wtype='IBM'
        obj%num_bits_hdr = 32
        call pc_put_sensitive_screen_flag('SEGY',.true.)
        call pc_put_sensitive_field_flag ('segyheader' ,  .true.)
        call pc_put_sensitive_field_flag ('num_bits' ,  .true.)
        call pc_put_sensitive_field_flag ('history'  ,  .false.)
!        call pc_put_sensitive_field_flag ('compress' ,  .false.)
!        call pc_put_sensitive_field_flag ('sn_ratio' ,  .false.)
!        call pc_put_sensitive_field_flag ('c_ratio'  ,  .false.)
!        call pc_put_sensitive_field_flag ('wave_dr'  ,  .false.)
        call pc_get ('segyheader',obj%segyheader, trot_segyheader_trap)
        call pc_get ('num_bits' , obj%num_bits , trot_num_bits_trap)

        call pc_get ('num_bits' , obj%num_bits , trot_num_bits_trap)
        if (obj%num_bits < 32) then
          call pc_put_sensitive_field_flag ('opt_norm' ,  .true. )

          call pc_get ('opt_norm', obj%opt_norm)

          if (obj%opt_norm == 'AMPL_MAX') then
              call pc_put_sensitive_field_flag ('ampl_max' ,  .true. )

              old_ampl_max = obj%ampl_max
              call pc_get ('ampl_max', obj%ampl_max)
              if (obj%ampl_max <= 0.0) then
                  obj%ampl_max = old_ampl_max
              endif
          else
              call pc_put_sensitive_field_flag ('ampl_max' ,  .false.)
          endif

        else
          call pc_put_sensitive_field_flag ('opt_norm' ,  .false.)
          call pc_put_sensitive_field_flag ('ampl_max' ,  .false.)
        endif
      case ('SU')
        obj%history      = 'NONE'
        obj%wtype        ='IBM'
        obj%num_bits_hdr = 32
        obj%num_bits     = 32
        call pc_put_sensitive_field_flag ('opt_norm' ,  .false.)
        call pc_put_sensitive_field_flag ('ampl_max' ,  .false.)
        call pc_put_sensitive_field_flag ('history'  ,  .false.)
        call pc_put_sensitive_screen_flag('SEGY'     ,  .true. )
        call pc_put_sensitive_field_flag ('segyheader' ,  .true.)
        call pc_get ('segyheader',obj%segyheader, trot_segyheader_trap)
      case ('LBO', 'LBO2')
!        obj%compress = 'NONE'
        obj%create_csv='NO'
        call pc_put_sensitive_field_flag ('segyheader', .false.)
        call pc_put_sensitive_field_flag ('history'  ,  .false.)
        call pc_put_sensitive_field_flag ('num_bits' ,  .true.)
!        call pc_put_sensitive_field_flag ('compress' ,  .false.)
!        call pc_put_sensitive_field_flag ('sn_ratio' ,  .false.)
!        call pc_put_sensitive_field_flag ('c_ratio'  ,  .false.)
!        call pc_put_sensitive_field_flag ('wave_dr'  ,  .false.)
        call pc_put_sensitive_field_flag ('opt_norm' ,  .false.)
        call pc_put_sensitive_field_flag ('ampl_max' ,  .false.)
        call pc_put_sensitive_field_flag ('create_csv', .false.)
!     case ('QTROT')
!       obj%history = 'NONE'
!       obj%compress = 'NONE'
!       obj%create_csv='NO'
!       call pc_put_sensitive_field_flag ('segyheader', .false.)
!       call pc_put_sensitive_field_flag ('history'  ,  .false.)
!       call pc_put_sensitive_field_flag ('num_bits' ,  .false.)
!       call pc_put_sensitive_field_flag ('compress' ,  .false.)
!       call pc_put_sensitive_field_flag ('sn_ratio' ,  .false.)
!       call pc_put_sensitive_field_flag ('c_ratio'  ,  .false.)
!       call pc_put_sensitive_field_flag ('wave_dr'  ,  .false.)
!       call pc_put_sensitive_field_flag ('opt_norm' ,  .false.)
!       call pc_put_sensitive_field_flag ('ampl_max' ,  .false.)
!       call pc_put_sensitive_field_flag ('create_csv', .false.)
!     case ('CMPR')
!       obj%create_csv='NO'
!       call pc_put_sensitive_field_flag ('segyheader', .false.)
!       call pc_put_sensitive_field_flag ('history'  ,  .true.)
!       call pc_put_sensitive_field_flag ('compress' ,  .true.)
!       call pc_put_sensitive_field_flag ('sn_ratio' ,  .true.)
!       call pc_put_sensitive_field_flag ('c_ratio'  ,  .true.)
!       call pc_put_sensitive_field_flag ('wave_dr'  ,  .true.)
!       call pc_put_sensitive_field_flag ('create_csv' ,  .false.)
!       call pc_put_sensitive_field_flag ('num_bits' ,  .false.)
!       obj%num_bits = 32
!       call pc_get ('compress' , obj%compress , trot_compress_trap)
!       select case (obj%compress)
!         case ('SN_RATIO')
!           if(obj%sn_ratio == 0 ) obj%sn_ratio = 50
!           call pc_put_sensitive_field_flag ('sn_ratio' ,  .true.)
!           call pc_put_sensitive_field_flag ('c_ratio'  ,  .false.)
!           call pc_put_sensitive_field_flag ('wave_dr'  ,  .false.)
!           call pc_get ('sn_ratio' , obj%sn_ratio , trot_sn_ratio_trap)
!           obj%c_ratio = 0
!           obj%wave_dr = 0
!         case ('C_RATIO')
!           if(obj%c_ratio == 0 ) obj%c_ratio = 5
!           call pc_put_sensitive_field_flag ('sn_ratio' ,  .false.)
!           call pc_put_sensitive_field_flag ('c_ratio'  ,  .true.)
!           call pc_put_sensitive_field_flag ('wave_dr'  ,  .false.)
!           call pc_get ('c_ratio'  , obj%c_ratio  , trot_c_ratio_trap)
!           obj%sn_ratio = 0
!           obj%wave_dr = 0
!         case ('WAVE_DR')
!           if(obj%wave_dr == 0 ) obj%wave_dr = 50
!           call pc_put_sensitive_field_flag ('sn_ratio' ,  .false.)
!           call pc_put_sensitive_field_flag ('c_ratio'  ,  .false.)
!           call pc_put_sensitive_field_flag ('wave_dr'  ,  .true.)
!           call pc_get ('wave_dr'  , obj%wave_dr  , trot_dyn_range_trap)
!           obj%sn_ratio = 0
!           obj%c_ratio = 0
!         case default
!           call pc_put_sensitive_field_flag ('compress' ,  .false.)
!           call pc_put_sensitive_field_flag ('sn_ratio' ,  .false.)
!           call pc_put_sensitive_field_flag ('c_ratio'  ,  .false.)
!           call pc_put_sensitive_field_flag ('wave_dr'  ,  .false.)
!           call pc_put_sensitive_field_flag ('num_bits' ,   .true.)
!           obj%type = 'TRCIO'
!       end select
!       call pc_put_sensitive_field_flag ('opt_norm' ,  .false.)
!       call pc_put_sensitive_field_flag ('ampl_max' ,  .false.)
      case ('TRCIO')
        call pc_put_sensitive_field_flag ('segyheader', .false.)
        call pc_put_sensitive_field_flag ('history'  ,  .true.)
!        call pc_put_sensitive_field_flag ('compress' ,  .false.)
!        call pc_put_sensitive_field_flag ('sn_ratio' ,  .false.)
!        call pc_put_sensitive_field_flag ('c_ratio'  ,  .false.)
!        call pc_put_sensitive_field_flag ('wave_dr'  ,  .false.)
        call pc_put_sensitive_field_flag ('num_bits' ,  .true.)

        call pc_get ('num_bits' , obj%num_bits , trot_num_bits_trap)
        if (obj%num_bits < 32) then
          call pc_put_sensitive_field_flag ('opt_norm' ,  .true. )

          call pc_get ('opt_norm', obj%opt_norm)

          if (obj%opt_norm == 'AMPL_MAX') then
              call pc_put_sensitive_field_flag ('ampl_max' ,  .true. )

              old_ampl_max = obj%ampl_max
              call pc_get ('ampl_max', obj%ampl_max)
              if (obj%ampl_max <= 0.0) then
                  obj%ampl_max = old_ampl_max
              endif
          else
              call pc_put_sensitive_field_flag ('ampl_max' ,  .false.)
          endif

          if (  ((old_type /= 'TRCIO ') .or. (old_num_bits >= 32)) &
           .and. (obj%history /= 'NONE    ')) then
              obj%history = 'CURRENT '
          endif

        else
          call pc_put_sensitive_field_flag ('opt_norm' ,  .false.)
          call pc_put_sensitive_field_flag ('ampl_max' ,  .false.)
        endif
      case ('JSEIS')
print*,' Output of a javaseis volume.'
      case default

    end select
    call pc_get ('extent_size_mb',obj%extent_size_mb,trot_extent_size_mb_trap)
    call pc_get ('reserve_disk',obj%reserve_disk,trot_reserve_disk_trap)
    call pc_get ('type'     , obj%type     , trot_type_trap)
    call pc_get ('permission', obj%permission,trot_permission_trap)
    call pc_get ('history'  , obj%history  , trot_history_trap)
    call pc_get ('write'    , obj%write    , trot_write_trap)
    call pc_get ('num_bits' , obj%num_bits , trot_num_bits_trap)
    call pc_get ('create_csv',obj%create_csv,trot_create_csv_trap)

    ! friendly behavior if user changes type
    if(obj%update_count > 0 .and. obj%type /= old_type) then
      if    (obj%type == 'TRCIO') then
        obj%num_bits = 32
      elseif(obj%type == 'LBO' .or. obj%type == 'LBO2') then
        obj%num_bits = 16
      endif
    endif
    obj%update_count = obj%update_count + 1

    !if(obj%num_bits < 32 ) then
      !call pc_put_sensitive_field_flag ('dyn_range_cmpr' ,  .true.)
      !call pc_get ('dyn_range_cmpr' , &
      !                        obj%dyn_range_cmpr , &
      !                                       trot_dyn_range_cmpr_trap)
    !else
      !call pc_put_sensitive_field_flag ('dyn_range_cmpr' ,  .false.)
    !endif

    call pc_get ('hdr_flag' , obj%hdr_flag , trot_hdr_flag_trap)

    call pc_get ('extent_size_mb',obj%extent_size_mb,trot_extent_size_mb_trap)
    call pc_get ('reserve_disk',obj%reserve_disk,trot_reserve_disk_trap)

    obj%numtr = max(obj%numtr,1)

    if(obj%type == 'SEGY' .and. pc_verify_screen('screen2')) then
      if(obj%num_bits < 32 .and. obj%opt_norm == 'TR_LAV  ') then
        call pc_warning( &
          'TROT:  SEGY TR_LAV mode normalizes traces--relative amps are lost.')
      endif
    endif

    call pc_put_global  ('numtr'       , obj%numtr)        ! if changed.

    call pc_put_control ('twosets'     , .false.)       ! default false
    call pc_put_control ('iftd'        , .false.)       ! default false
    call pc_put_control ('ndisk'       , 0 )
    call pc_put_control ('setup_only'  , .false.)       ! default .false.

    call pc_put_options_field('type',opt_type,num_opt_type)
!    call pc_put_options_field('dyn_range_cmpr',opt_dyn_range_cmpr,&
!                                               num_opt_dyn_range_cmpr)
    call pc_put_options_field('history',opt_history,num_opt_history)
!    call pc_put_options_field('compress',opt_compress,num_compress)
    call pc_put_options_field('write',opt_write,num_opt_write)
    call pc_put_options_field('num_bits',opt_num_bits,num_num_bits)
    call pc_put_options_field('reserve_disk',opt_reserve_disk, &
                                             num_opt_reserve_disk)
    call pc_put_options_field('opt_norm', &
        opt_norm_options, opt_norm_noptions)
    call pc_put_options_field('create_csv',opt_csv,num_opt_csv)

    call mapsegy_update(obj%mapsegy)

    ! mfile_set_pathnames_ext must be called before mfile_update.
    !
    call mfile_set_pathnames_ext(obj%mfile, trot_get_pathname_ext(obj))
    call mfile_update           (obj%mfile)

    call trot_multifile_update(obj)

    call pc_call_end_trap(trot_end_trap)

    call pc_put ('type'     , obj%type     )
    call pc_put ('permission', obj%permission)
    call pc_put ('history'  , obj%history  )
    call pc_put ('write'    , obj%write    )

    if(obj%type == 'SEGY' .or. obj%type == 'SU' ) &
      call pc_put ('segyheader',obj%segyheader)
    call pc_put ('num_bits' , obj%num_bits )
    !if(obj%num_bits < 32 ) &
    !  call pc_put ('dyn_range_cmpr' , obj%dyn_range_cmpr )

    call pc_put ('hdr_flag' , obj%hdr_flag )

    if (((obj%type == 'TRCIO') .or. (obj%type == 'SEGY')) &
                              .and. (obj%num_bits < 32)) then
        call pc_put('opt_norm', obj%opt_norm)

        if (obj%opt_norm == 'AMPL_MAX') then
            call pc_put('ampl_max', obj%ampl_max)
        endif
    endif

!   if(obj%type == 'CMPR' ) then
!     call pc_put ('compress' , obj%compress)
!     select case (obj%compress)
!       case ('SN_RATIO')
!         call pc_put ('sn_ratio' , obj%sn_ratio)
!       case ('C_RATIO')
!         call pc_put ('c_ratio'  , obj%c_ratio )
!       case ('WAVE_DR')
!         call pc_put ('wave_dr'  , obj%wave_dr )
!       case default
!     end select
!   endif

    call pc_put ('extent_size_mb',obj%extent_size_mb)
    call pc_put ('reserve_disk',obj%reserve_disk)
    call pc_put ('create_csv',obj%create_csv)

    ! The following three calls were added because they were not always
    ! being called from this update routine.  SeisSpace requires all
    ! parameters to be put into the parameter cache by the update routine.
    ! This is because SeisSpace has no other way to find out what
    ! parameters are to be displayed in the gui.

    call pc_put          ('opt_norm ', obj%opt_norm)     ! added by stoeckley 2006-08-24
    call pc_put          ('ampl_max ', obj%ampl_max)     ! added by stoeckley 2006-08-24
    call pc_put          ('segyheader', obj%segyheader)  ! added by stoeckley 2008-04-22

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

!--- took out of below if block... .or. pc_get_update_state() /= PC_BACKEND) &

    if (pc_do_not_process_traces() ) return

    obj%skip_wrapup = .false.

    if (.not. mfile_get_filenames(obj%mfile, &
                            obj%filenames, obj%num_files)) then
      call pc_error('TROT:  bad filenames')
      return
    endif

    if ((obj%opt_switch == 'HDR') .and. (obj%num_files /= obj%bin_tot)) then
      call pc_error('TROT:  Must have same number of files as bins.')
      return
    endif

    allocate(obj%files(obj%num_files), stat=status)
    if (status /= 0) then
      call pc_error('TROT:  out of memory')
      return
    endif

    allocate(obj%traces_written(obj%num_files), stat=status)
    if (status /= 0) then
      call pc_error('TROT:  out of memory')
      return
    endif

    istat=hist_write(obj%ipn,'TROT_OUTPUT_FILES = ')
    do i = 1, obj%num_files
      card=' '
      if(i.eq.1)card(1:1)='('
      card(2:)=trim(obj%filenames(i))
      nc=len_trim(card)
      card(nc+1:nc+1)=','
      if(i.eq.obj%num_files)then
        card(nc+1:nc+2)='),'
      endif
      istat=hist_write(obj%ipn,card)
      nullify(obj%files(i)%p)
      obj%traces_written(i) = 0

      call manhist_check_name(obj%filenames(i),status)
      if(status.ne.0)then
        call pc_error('TROT:  ABORT - filename ' // trim(obj%filenames(i)) &
                           // ' already exists in history')
      endif

    enddo

    obj%tot_traces_written = 0
    obj%orphan_traces      = 0

    if(dooskip_get_num_skip(obj%DS) < 0 ) then
      call pc_error('trot: num_skip can not be less than zero.')
      return
    endif

    called_bhist = .false.

    if(.not. called_bhist) then
      called_bhist = .true.

      !--- this section sets up the number of globals and stores global cards
      !--- in an array.
        obj%num_global_cards = pc_num_global_cards()
        call pc_alloc_global_cards    (obj%global_cards, obj%num_global_cards)
        call pc_get_global_cards(obj%global_cards,obj%num_global_cards,errmsg)

    endif

  end subroutine trot_update



! Modified from old trot_pathname_update
!
function trot_get_pathname_ext(obj) result (ext)
  type(trot_struct),intent(in) :: obj
  character(len=24)            :: ext

  !if      (obj%type == 'CMPR') then
  !     ext = 'cmpr'
  if (obj%type == 'SEGY') then
       ext = 'segy'
  else if (obj%type == 'SU' ) then
       ext = 'su'
  else if (obj%type == 'LBO' .or. obj%type == 'LBO2') then
       ext = 'lbo'
  else if (obj%num_bits == 32) then
       ext = 'trc'
  else if (obj%num_bits == 8) then
       ext = 'trc8'
  else if (obj%num_bits == 16) then
       ext = 'trc16'
  else
       ext = 'trc'
  end if
  if (obj%type == 'JSEIS') then
print*,'obj%type == jseis so...'
ext = 'jsf'
  endif

end function trot_get_pathname_ext



!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



subroutine trot_retention_trap(retention)
  character(len=*) , intent(in) :: retention
end subroutine trot_retention_trap



!subroutine trot_compress_trap(compress)
! character(len=*) , intent(in) :: compress
!   select case (trp%compress)
!     case ('C_RATIO')
!       call pc_put_sensitive_field_flag ('sn_ratio' ,  .false.)
!       call pc_put_sensitive_field_flag ('c_ratio'  ,  .true.)
!       call pc_put_sensitive_field_flag ('wave_dr' ,  .false.)
!       call pc_put_sensitive_field_flag ('num_bits' ,  .false.)
!       trp%num_bits = 32
!       if(.not. trp%gathered) then
!         call pc_error&
!          ('TROT: Compression requires you call GATHER prior to TROT.')
!       endif
!     case ('SN_RATIO')
!       call pc_put_sensitive_field_flag ('sn_ratio' ,  .true.)
!       call pc_put_sensitive_field_flag ('c_ratio'  ,  .false.)
!       call pc_put_sensitive_field_flag ('wave_dr'  ,  .false.)
!       call pc_put_sensitive_field_flag ('num_bits' ,  .false.)
!       trp%num_bits = 32
!       if(.not. trp%gathered) then
!         call pc_error&
!          ('TROT: Compression requires you call GATHER prior to TROT.')
!       endif
!     case ('WAVE_DR')
!       call pc_put_sensitive_field_flag ('sn_ratio' ,  .false.)
!       call pc_put_sensitive_field_flag ('c_ratio'  ,  .false.)
!       call pc_put_sensitive_field_flag ('wave_dr'  ,  .true.)
!       call pc_put_sensitive_field_flag ('num_bits' ,  .false.)
!       trp%num_bits = 32
!       if(.not. trp%gathered) then
!         call pc_error&
!          ('TROT: Compression requires you call GATHER prior to TROT.')
!       endif
!     case ('NONE')
!       call pc_put_sensitive_field_flag ('sn_ratio' ,  .false.)
!       call pc_put_sensitive_field_flag ('c_ratio'  ,  .false.)
!       call pc_put_sensitive_field_flag ('wave_dr'  ,  .false.)
!       call pc_put_sensitive_field_flag ('num_bits' ,  .true.)
!       trp%type = 'TRCIO'
!       call pc_jump_field('type')
!   end select
!end subroutine trot_compress_trap



!subroutine trot_sn_ratio_trap(sn_ratio)
! character(len=*) , intent(in) :: sn_ratio
! if(trp%sn_ratio > 100 .or. trp%sn_ratio < 5 ) then
!   call pc_info('TROT: Signal to Noise Ratio between 5 and 100 dB only.')
!   trp%sn_ratio = 50
!   call pc_jump_field(sn_ratio)
! endif
!end subroutine trot_sn_ratio_trap



!subroutine trot_c_ratio_trap(c_ratio)
! character(len=*) , intent(in) :: c_ratio
! if(trp%c_ratio > 100 .or. trp%c_ratio < 1 ) then
!   call pc_info('TROT: Compression Ratio between 1:5 and 1:100 only.')
!   trp%c_ratio = 5
!   call pc_jump_field(c_ratio)
! endif
!end subroutine trot_c_ratio_trap



!subroutine trot_dyn_range_trap(wave_dr)
! character(len=*) , intent(in) :: wave_dr
! if(trp%wave_dr > 100 .or. trp%wave_dr < 5 ) then
!   call pc_info('TROT: Wavelet Dynamic Range between 5 and 100 dB only.')
!   trp%wave_dr = 50
!   call pc_jump_field(wave_dr)
! endif
!end subroutine trot_dyn_range_trap



subroutine trot_permission_trap(permission)
  character(len=*),intent(in)       :: permission
  integer                           :: j
  logical                           :: writable

  writable = .false.
  do j = 1,len(trp%permission)
    select case (trp%permission(j:j))
    case ('w','W') ! rw
      writable = .true.
    end select
  end do
  if(.not. writable) then
    call pc_info('TROT: File permission ('//trim(trp%permission)// &
    ') does not allow you to write.'// &
    ' Try using "rw-rw-r--".')
    trp%permission='rw-rw-r--';
    call pc_put(permission,trp%permission)
    call pc_jump_field(permission)
  endif
end subroutine trot_permission_trap



subroutine trot_type_trap(type)
  character(len=*),intent(in) :: type

  call string_to_upper(trp%type)
  select case (trp%type)
  case ('TRCIO')
    call pc_put_sensitive_field_flag ('segyheader', .false.)
    call pc_put_sensitive_field_flag ('history', .true.)
    call pc_put_sensitive_field_flag ('compress',.false.)
    call pc_put_sensitive_field_flag ('create_csv', .true.)
    trp%wtype = 'IEEE'
    trp%num_bits_hdr = 64
!  case ('QTROT')
!    call pc_put_sensitive_field_flag ('segyheader', .false.)
!    trp%history = 'NONE'
!    call pc_put('history',trp%history)
!    call pc_put_sensitive_field_flag ('history', .false.)
!    trp%wtype = 'IEEE'
!    call pc_put_sensitive_field_flag ('compress',.false.)
!    call pc_put_sensitive_field_flag ('create_csv', .false.)
!    trp%create_csv = 'NO'
!    trp%num_bits_hdr = 32
    !call pc_info( &
    !'TROT: QTROT is a deprecated format only for use in bridging to old CPS.')
  case ('SEGY')
    call pc_put_sensitive_field_flag ('create_csv', .true.)
    call pc_put_sensitive_field_flag ('history', .false.)
  case ('SU')
    call pc_put_sensitive_field_flag ('create_csv', .true.)
    call pc_put_sensitive_field_flag ('history', .false.)
    call pc_put_sensitive_field_flag ('compress',.false.)
  case ('LBO', 'LBO2')
    trp%wtype = 'LBO'
    trp%num_bits_hdr = 64
    call pc_put_sensitive_field_flag ('segyheader', .false.)
    call pc_put_sensitive_field_flag ('history', .true.)
    call pc_put_sensitive_field_flag ('compress',.false.)
    if(trp%num_bits > LBO_MAX) trp%num_bits = 16
    if(trp%num_bits < LBO_MIN) trp%num_bits = 16
! case ('CMPR')
!   call pc_error('TROT: type must be TRCIO, LBO, LBO2, or SEGY.')
!   call pc_jump_field(type)
!   return
!   trp%wtype = 'IEEE'
!   trp%num_bits = 32
!   call pc_put('num_bits',trp%num_bits)
!   trp%num_bits_hdr = 64
!   call pc_put_sensitive_field_flag ('segyheader', .false.)
!   call pc_put_sensitive_field_flag ('history', .true.)
!   call pc_put_sensitive_field_flag ('compress',.true.)
!   call pc_put_sensitive_field_flag ('create_csv', .false.)
!   trp%create_csv = 'NO'
  case ('JSEIS')
    call pc_put_sensitive_field_flag ('segyheader', .false.)
    call pc_put_sensitive_field_flag ('history', .false.)
    call pc_put_sensitive_field_flag ('compress',.false.)
    call pc_put_sensitive_field_flag ('create_csv', .false.)
  case default
    call pc_put_sensitive_field_flag ('segyheader', .false.)
    call pc_put_sensitive_field_flag ('history', .true.)
    call pc_put_sensitive_field_flag ('compress',.false.)
    call pc_error('TROT: type must be TRCIO, SU, LBO, LBO2, or SEGY.')
    call pc_jump_field(type)
  end select
end subroutine trot_type_trap



subroutine trot_segyheader_trap(segyheader)
  character(len=*),intent(in)     :: segyheader
  integer                         :: status
! character(len=260),save         :: oldsegyheader
! if(trp%type /= 'SEGY') return
  call pathcheck(segyheader,trp%segyheader, &
  required=.false.,status=status)
  !!!!required=.false.,ext='hdr',status=status)
  if(status == PATHCHECK_UNSPECIFIED) then
    call pc_put_gui_only ('found','~/cpseis/etc/default.ebcdic')
    !call pc_info(&
    !'TROT: If desired, '// &
    !'enter a file name where you have the data for the EBCDIC header.')
!   call pc_jump_field(segyheader)
  endif
! if(status /= PATHCHECK_VALID .and. status /= PATHCHECK_UNSPECIFIED) then
!   call pc_jump_field(segyheader)
! endif
  select case (finquire_file(trp%segyheader,quickly=.true.))
  case (FINQUIRE_FOUND)
    call pc_put_gui_only ('found','FOUND')
    return
  case (FINQUIRE_BLANK)
    call pc_put_gui_only ('found','-NONE-')
    return
  case default
    call pc_put_gui_only ('found','NOT FOUND')
!   if(trim(trp%segyheader) == trim(oldsegyheader)) return ! no more messages.
    call pc_info(&
    'TROT: segy ebcdic header file '//trim(trp%segyheader)// &
    ' not found.')
!   oldsegyheader = trp%segyheader
  end select 
  return
end subroutine trot_segyheader_trap



subroutine trot_num_bits_trap(num_bits)
  character(len=*),intent(in) :: num_bits
  integer                     :: j

  if(trp%type == 'LBO' .or. trp%type == 'LBO2') then
    if(trp%num_bits < LBO_MIN) then
      call pc_error('TRCIO: For LBO formats, NUM_BITS must be >=', LBO_MIN)
      return
    endif
    if(trp%num_bits > LBO_MAX) then
      call pc_error('TRCIO: For LBO formats, NUM_BITS must be <=', LBO_MAX)
      return
    endif
    return  ! no error
  endif

  if(trp%num_bits < 0 ) trp%num_bits = 1
  j = 2**nint(log(1.0*trp%num_bits)/log(2.0))
  if(j < 8 .or. j > 64 .or. j /= trp%num_bits) then
    call pc_info('TROT: num_bits must be a power of 2 between 8 and 64.')
    trp%num_bits = max(8,min(64,j))
    call pc_jump_field(num_bits)
  endif
!  if(trp%dyn_range_cmpr == 'YES' .and. trp%num_bits > 16 ) then
!    call pc_info('TROT: For dyn_range_cmpr, num_bits should be 8 or 16.')
!    call pc_jump_field('num_bits')
!  endif
end subroutine trot_num_bits_trap



!subroutine trot_dyn_range_cmpr_trap(dyn_range_cmpr)
!  character(len=*),intent(in)   :: dyn_range_cmpr
!  call string_to_upper(trp%dyn_range_cmpr)
!  select case (trp%dyn_range_cmpr)
!  !case ('YES')
!  !if(trp%num_bits > 16) then
!  !  call pc_info('TROT: dynamic range compression, set num_bits = 16 or 8')
!  !  call pc_jump_field('num_bits')
!  !endif
!  case ('NO')
!  case default
!    call pc_error('TROT: dyn_range_cmpr must be "NO"' )
!    call pc_jump_field(dyn_range_cmpr)
!  end select
!end subroutine trot_dyn_range_cmpr_trap



subroutine trot_history_trap(history)
  character(len=*),intent(in)   :: history
  call string_to_upper(trp%history)
  select case (trp%history)
  case ('NONE','ALL','MODEL','CURRENT')
  case default
    call pc_error('TROT: history must be "NONE, ALL, MODEL, or CURRENT"')
    call pc_jump_field(history)
  end select
end subroutine trot_history_trap



subroutine trot_write_trap(write)
  character(len=*),intent(in)   :: write
  call string_to_upper(trp%write)
  select case(trp%write)
  case('NEW')
  case('OVERWRITE')
  case('APPEND')
  case default
    call pc_error ('trot: write must be "OVERWRITE, NEW, or APPEND".')
    call pc_jump_field(write)
  end select
end subroutine trot_write_trap



subroutine trot_hdr_flag_trap(hdr_flag)
  character(len=*),intent(in)   ::  hdr_flag
  if(trp%hdr_flag < 0 .or. trp%hdr_flag > trp%nwih) then
    call pc_info&
    ('TROT: Header flag must be within range of valid header words OR 0.')
    trp%hdr_flag = min(trp%nwih,max(0,trp%hdr_flag))
  endif
end subroutine trot_hdr_flag_trap



subroutine trot_extent_size_mb_trap(extent_size_mb)
  character(len=*),intent(in) :: extent_size_mb
  !-- see bfio.h BF_FLSZ for maximum file size allowable, which is 2E6*1E6 bytes.
  if(trp%extent_size_mb > 2000000 .or. trp%extent_size_mb < 1) then
    trp%extent_size_mb = 250
    call pc_info('TROT: reset extent_size_mb to 250 (range = 1  - 2000000).')
  endif
  if(trp%extent_size_mb > 20480 ) then
    trp%reserve_disk = 'NO'
    call pc_jump_field('reserve_disk')
  endif
end subroutine trot_extent_size_mb_trap



subroutine trot_reserve_disk_trap(reserve_disk)
  character(len=*),intent(in) :: reserve_disk
  call string_to_upper(trp%reserve_disk)
  select case(trp%reserve_disk)
    case('YES')
      if(trp%extent_size_mb > 20480 ) then
        trp%reserve_disk = 'NO'
        call pc_info('TROT: reset reserve_disk to NO '// &
                     'because extent size > 20480 Megabytes.')
      endif
    case('NO')
    case default
      trp%reserve_disk = 'NO'
      call pc_info('TROT: reset reserve_disk to NO. Bad value read in.')
  end select
end subroutine trot_reserve_disk_trap



subroutine trot_create_csv_trap(create_csv)
  character(len=*),intent(in) :: create_csv
  call string_to_upper(trp%create_csv)
  select case(trp%create_csv)
    case('YES')
    case('NO')
    case default
      trp%create_csv = 'NO'
      call pc_info('TROT: reset create_csv to NO. Bad value read in.')
  end select
end subroutine trot_create_csv_trap


!<execute_only>

subroutine trot (obj,ntr,hd,tr)
  !----------- PASSED PARAMETERS ----------------------------------------
  type(trot_struct),pointer       :: obj                    ! parm block
  integer         ,intent(inout)  :: ntr                    ! num trc
  double precision,intent(inout)  :: hd(:,:)                ! headers
  real            ,intent(inout)  :: tr(:,:)                ! traces
  !----------- LOCAL VARIABLES ------------------------------------------
  double precision,dimension(:)   :: hd_local(obj%nwih)     ! temp stash
  real            ,dimension(:)   :: tr_local(obj%ndpt)     ! temp stash
  integer                         :: status,i       



  character(len=256)              :: errmsg
  integer                         :: ifile  
  !--- executable code starts -------------------------------------------

  !--- go away if error coming in.
  if (ntr == FATAL_ERROR) return

  !--- If I have exceeded my maximum, I need to quit.
  if(dooskip_hit_tr_max(obj%DS)) return

  ! removing cmpr type.  wmm 5/16/2006
  !select case (obj%type)
    !case ('CMPR')
    !  call trot_cmpr(obj,ntr,hd,tr)
    !  return
    !case default ! TRCIO, LBO, LBO2,  SEGY, SU
  !end select

  !--- for each incoming trace, ...
  do i = 1, ntr
    !------ test for flagged traces -------
    if(obj%hdr_flag > 0) then
        ! hdr_flag is valid and can be safely used as a subscript
        ! Fortran does not define order of evaluation within "if".
        if(hd(obj%hdr_flag,i) <= 0.0 ) cycle
    end if

    if(dooskip(obj%DS)) then ! skip if false in the do-skip pattern...

      ifile = trot_which_file(obj, hd(1:obj%nwih, i))

      if (ifile == -1) then
        call pc_error('TROT:  trot_which_file_error')
        ntr = FATAL_ERROR
        return
      else if (ifile == 0) then
        ! Do nothing.
      else

        ! Open file, if necessary.
        if (obj%traces_written(ifile) == 1) then
          if(trot_open_file(obj, ifile) /= 0 ) then
            call pc_error('TROT:  error opening file ' &
                           // trim(obj%filenames(ifile)))
            ntr = FATAL_ERROR
            return
          endif
        endif

        !--- Write the trace ----
        !--- save the header in a local array before writing.
        hd_local(2:) = hd(2:obj%nwih,i)
        !--- set header words --- always writes to next trace number, trot
        !--- can't handle overwriting traces in a file.
        hd_local(1)  = dble(obj%traces_written(ifile))
        !--- save the trace in a local array before writing.
        tr_local = tr(1:obj%ndpt,i)

        status = trcio_write_trace(obj%files(ifile)%p,hd_local,tr_local)

        if(status == trcio_ok ) then
          status = cio_ftell( &
            obj%files(ifile)%p%lun,obj%files(ifile)%p%data_end_pos)
        else
          obj%tot_traces_written = obj%tot_traces_written - 1
          write(errmsg,'(a,i11,a,a)')'TROT: Error writing trace # ',&
          nint(hd(1,i)),' to ',trim(obj%filenames(ifile))
          call string_compress_blanks(errmsg,status)
          NTR = FATAL_ERROR
          call pc_error(errmsg)
        endif
      endif
    endif

  end do !--- i = 1, ntr


end subroutine trot

!subroutine trot_cmpr (obj,ntr,hd,tr)
!  !----------- PASSED PARAMETERS ----------------------------------------
!  type(trot_struct),pointer       :: obj                    ! parm block
!  integer         ,intent(inout)  :: ntr                    ! num trc
!  double precision,intent(inout),target :: hd(:,:)          ! headers
!  real            ,intent(inout),target :: tr(:,:)          ! traces
!  !----------- LOCAL VARIABLES -----------------------------------------
!  double precision,dimension(:,:),pointer             :: hdi
!  real            ,dimension(:,:),pointer             :: tri
!  integer                         :: status,i,ntri  
!  character(len=256)              :: errmsg
!  !--- pointers
!
!  !--- executable code starts -------------------------------------------
!
!
!  !--- go away if error coming in.
!  if (ntr == FATAL_ERROR) return
!  if (ntr <=          0 ) return
!
!  !--- If I have exceeded my maximum, I need to quit.
!  if(dooskip_hit_tr_max(obj%DS)) return
!
!  if (obj%traces_written(1) > obj%file_max) then
!    call pc_error('TROT:  trot_cmpr program bug')
!    ntr = FATAL_ERROR
!    return
!  endif
!
!  call trot_alloc_global_hdi_tri(obj,ntr)
!  if (ntr == FATAL_ERROR) return
!  hdi => global_hdi
!  tri => global_tri
!  !--- set outgoing header word ---
!  ntri = 0
!  do i = 1, ntr
!    !------ test for flagged traces -------
!    if(obj%hdr_flag > 0 .and. hd(obj%hdr_flag,i) <= 0.0 )                cycle
!    if(.not.dooskip(obj%DS))                                             cycle
!    ntri = ntri + 1
!    hdi(:,ntri) = hd(:obj%nwih,i)
!    hdi(1,ntri) = 1d0*dooskip_nbr_done(obj%DS)
!
!    tri(1:obj%ndpt,ntri) = tr(1:obj%ndpt,i)
!  end do !--- i = 1, ntr
!
!  if (ntri > 0) then
!
!    if (obj%traces_written(1) == obj%file_max) then
!      obj%orphan_traces = obj%orphan_traces + ntri
!    else
!      ! Open file, if necessary.
!      if (obj%traces_written(1) == 0) then
!        if(trot_open_file(obj, 1) /= 0 ) then
!          call pc_error('TROT:  error opening file ' // trim(obj%filenames(1)))
!          ntr = FATAL_ERROR
!          return
!        endif
!      endif
!
!      if (obj%traces_written(1) + ntri > obj%file_max) then
!        obj%orphan_traces = obj%traces_written(1) + ntri - obj%file_max
!        ntri = ntri - obj%orphan_traces
!      endif
!
!      obj%traces_written(1)  = obj%traces_written(1)  + ntri
!      obj%tot_traces_written = obj%tot_traces_written + ntri
!
!      status = trcio_write_trace(obj%files(1)%p,hdi,tri,ntri)
!      if(status /= trcio_ok)  then
!        write(errmsg,'(a,F14.4,a,a)')'TROT: Error writing to ', &
!             trim(obj%filenames(1))
!        call string_compress_blanks(errmsg,status)
!        NTR = FATAL_ERROR
!        call pc_error(errmsg)
!      endif
!    endif
!
!  endif
!
!  nullify(hdi)
!  nullify(tri)
!
!
!end subroutine trot_cmpr

!</execute_only>

!<execute_only>

subroutine trot_wrapup (obj)
  type(trot_struct),pointer  :: obj
  !--- local vars.
  character (len=132)        :: string
  character (len=260)        :: file_name
  integer                    :: status
  integer,dimension(:)       :: file_size(2)
  integer                    :: total_traces
  integer                    :: buffer(128)
  integer                    :: crossline_header
  integer                    :: inline_header
  integer                    :: istat
  integer                    :: i

  if(.not. associated(obj) ) return

  if(obj%skip_wrapup) return
  obj%skip_wrapup = .true.

  ! total_traces is only used in csv file, which requires a single trot output
  ! file, so I guess total is the total from one file.
  !
  total_traces = 0

  if(associated(obj%global_cards) )deallocate(obj%global_cards)

  if (pc_get_update_state() /= PC_FRONTEND .and. &
      pc_get_update_state() /= PC_GUI ) then

    if (dooskip_nbr_done(obj%DS) &
        /= (obj%tot_traces_written + obj%orphan_traces)) then
      call pc_error('TROT:  do/skip error in trot_wrapup')
    endif

    write(string,*)'TROT: Wrote ', obj%tot_traces_written, ' traces'
    call string_compress_blanks(string,status)
    call pc_info(string(:status))

    write(string,*)'TROT: ', obj%orphan_traces, ' orphaned traces'
    call string_compress_blanks(string,status)
    call pc_info(string(:status))

    !!! add cards to current history !!!
    do i = 1, obj%num_files

      write(string,*)'TROT FILENAME='//trim(obj%filenames(i))
      call string_compress_blanks(string,status)
      if(hist_write(obj%ipn,string(:status)) /= hist_ok) continue
      call pc_info(string(:status))

      write(string,*)'---- NUMTRACES=', obj%traces_written(i)
      call string_compress_blanks(string,status)
      if(hist_write(obj%ipn,string(:status)) /= hist_ok) continue
      call pc_info(string(:status))

      if(obj%traces_written(i) > 0) then
        obj%files(i)%p%num_traces = obj%traces_written(i)
        total_traces = total_traces + obj%files(i)%p%num_traces
        file_name=obj%files(i)%p%filename
        call cio_flsz(trim(file_name),file_size)
        status = trcio_close(obj%files(i)%p)
        status = cio_truncate(trim(file_name),file_size)
        istat  = cio_chmod(trim(file_name), obj%permission)
        if(istat  /= cio_ok ) then
           call pc_info('TROT: Unable to set file permissions on file: ' &
           //trim(file_name)//'.')
           return
        endif
        nullify(obj%files(i)%p)
      endif

    enddo

  endif

  status = dooskip_wrapup(obj%DS)


  !Create a CSV file if requested
  if(obj%create_csv == 'YES') then

    if(total_traces == 0) then
      call pc_error('TROT: Unable to create CSV file... No traces found')
      return
    endif

    ! Set crossline and inline header words.
    ! May want these set by the user later?
    if(obj%type == 'SEGY') then
      crossline_header = 37
      inline_header    = 38
    else
      crossline_header = 7
      inline_header    = 8
    endif

    ! Use filenames(1) since can only have one trot file if using csv option.
    !
    call string_cc2hh(obj%filenames(1),buffer)

    ! C code used to create the CSV file
    call cube_trcio_create_file_batch(buffer, crossline_header, &
                                      inline_header, status)

    if(status == 0) then
      write(string,*)'TROT CSV: Wrote ', total_traces,' traces '
      call string_compress_blanks(string,status)
      call pc_info(string(:status))
    else
      call pc_error('TROT: Unable to create CSV file')
    endif

  endif ! End CSV option


end subroutine trot_wrapup

!</execute_only>

function trot_open_file(obj, ifile) result(status)
  type(trot_struct),intent(inout)       :: obj
  integer                               :: ifile
  integer                               :: status
  !--- local
  type(segy_ebcdic_hdr) :: ascii
  type(segy_bin_hdr)    :: bhed
  integer               :: ascunit,i
  character(len=2)      :: io_mode

  integer,dimension(:)  :: extsize(2)
  integer               :: cio_ext_size_mb
  character(len=8)      :: survey_units !--- for segy binary header.
  character(len=5)      :: ftype
  character(len=pc_datacard_length),pointer,dimension(:) :: cards
  integer               :: istat

  status = trcio_error ! assume an error will occur
  ! -- unit=0 passed to cio_get_file_ext_size because it is ignored.
  cio_ext_size_mb = cio_get_file_ext_size(0)/1000000

  !--- job globals are stored in "cards"
  cards => obj%global_cards

  if(obj%write     == 'NEW') then
    io_mode     = 'wn'
  elseif(obj%write == 'OVERWRITE' ) then
    io_mode     = 'w'
  elseif(obj%write == 'APPEND' ) then
    io_mode     = 'a'
    !--- Determine if file exists.  If it is unknown type, assume
    !--- that it is not a valid trace file and doesn't exist.
    ftype = trcio_determine_ftype(trim(obj%filenames(ifile)))
    if(ftype == 'UNK' ) then
      obj%write = 'NEW'
      io_mode = 'wn'
    endif
  endif

  extsize(1) = obj%extent_size_mb/cio_ext_size_mb
  extsize(2) = mod(obj%extent_size_mb,cio_ext_size_mb)*1000000


  select  case(obj%type)
  !case ('TRCIO','SEGY','LBO','LBO2','QTROT')
  case ('TRCIO','SEGY','LBO','LBO2','SU')
    if(.not.associated (obj%files(ifile)%p )) then
      status = cio_set_file_ext_size(extsize)
      if(status /= cio_ok) then
        call pc_error('TROT: Error setting file extent size.')
      endif
      if(obj%reserve_disk == 'YES') then
        call cio_set_file_space_commit(1)
      else
        call cio_set_file_space_commit(0)
      endif

      if(obj%type == 'LBO') then
        obj%files(ifile)%p => trcio_open( &
          obj%filenames(ifile),io_mode,history=obj%history,lbo_version=1)
      elseif(obj%type == 'LBO2') then
        obj%files(ifile)%p => trcio_open( &
          obj%filenames(ifile),io_mode,history=obj%history,lbo_version=2)
      else
        obj%files(ifile)%p => &
           trcio_open(obj%filenames(ifile),io_mode,history=obj%history)
      endif

    endif
    if(.not.associated (obj%files(ifile)%p )) then
       call pc_error('TROT: Unable to open file: '// &
                        trim(obj%filenames(ifile))//'.')
       return
    endif
    obj%files(ifile)%p%io_mode = io_mode
    if(io_mode /= 'a') then
      istat = trcio_set_ipn(obj%files(ifile)%p,obj%ipn)
      if(istat /= trcio_ok ) then
         call pc_error('TROT: Unable to set ipn in file: ' &
         //trim(obj%filenames(ifile))//'.')
         return
      endif
    endif

    if(io_mode /= 'a') then
      istat  = trcio_set_stdout(obj%files(ifile)%p,obj%stdout)
      if(istat  /= trcio_ok ) then
         call pc_error('TROT: Unable to set stdout in file: ' &
         //trim(obj%filenames(ifile))//'.')
         return
      endif
    endif

!  case ('CMPR')
!    if(io_mode == 'a') then
!      call pc_error('TROT: Unable to "append" to cmpr file.')
!      return
!    endif
!    if(.not. associated(obj%files(ifile)%p)) then
!      select case(obj%compress)
!        case('C_RATIO')
!          obj%sn_ratio = 0
!          obj%wave_dr = 0
!        case('SN_RATIO')
!          obj%c_ratio = 0
!          obj%wave_dr = 0
!        case('WAVE_DR')
!          obj%sn_ratio = 0
!          obj%c_ratio = 0
!        case default
!          call pc_error('Error detected: Bad compression type on trin.')
!          return
!      end select
!
!      status = cio_set_file_ext_size(extsize)
!      if(status /= cio_ok) then
!        call pc_error('TROT: Error setting file extent size.')
!      endif
!      if(obj%reserve_disk == 'YES') then
!        call cio_set_file_space_commit(1)
!      else
!        call cio_set_file_space_commit(0)
!      endif
!
!      obj%files(ifile)%p => &
!               trcio_open(filename=obj%filenames(ifile),io_mode=io_mode,&
!                 strt_val=obj%tstrt,srate=obj%dt, nwih=obj%nwih,ndpt=obj%ndpt,&
!                 compressed=.true.,snr=obj%sn_ratio,cmp=obj%c_ratio,&
!                 wdr=obj%wave_dr,&
!                 mtpc=obj%numtr,history=obj%history,&
!                 scratch=.false.)
!    endif
!
!    if(.not. associated(obj%files(ifile)%p)) then
!       call pc_error('TROT: Unable to open file: '&
!                        //trim(obj%filenames(ifile))//'.')
!       return
!    endif
!
!    ! Returns here will return trcio_error.
!    !
!    if(trcio_set_dx11   (obj%files(ifile)%p,1d0*grid_get_dx11   (obj%grid)) &
!        /= trcio_ok)  return
!    if(trcio_set_dx12   (obj%files(ifile)%p,1d0*grid_get_dx12   (obj%grid)) &
!        /= trcio_ok)  return
!    if(trcio_set_dx21   (obj%files(ifile)%p,1d0*grid_get_dx21   (obj%grid)) &
!        /= trcio_ok)  return
!    if(trcio_set_dx22   (obj%files(ifile)%p,1d0*grid_get_dx22   (obj%grid)) &
!        /= trcio_ok)  return
!    if(trcio_set_xorigin(obj%files(ifile)%p,1d0*grid_get_xorigin(obj%grid)) &
!        /= trcio_ok)  return
!    if(trcio_set_yorigin(obj%files(ifile)%p,1d0*grid_get_yorigin(obj%grid)) &
!        /= trcio_ok)  return
!
!    obj%files(ifile)%p%common%history = obj%history(:min(8,len(obj%history)))
!    call trcio_set_globals   (obj%files(ifile)%p,cards)
!
!    if (trcio_set_ipn    (obj%files(ifile)%p, obj%ipn)    /= 0) return
!    if (trcio_set_stdout (obj%files(ifile)%p, obj%stdout) /= 0) return
!
!    obj%num_bits     = 32
!    obj%num_bits_hdr = 64
!
!    status = trcio_ok
!    return
!
    case ('JSEIS')
      print*,' Opening jseis file...'
      print*,' IO mode = ',io_mode
      if (io_mode == 'w' .or. io_mode == 'wn' ) then
        print*, ' Cannot create javaseis file yet.'
        status = trcio_error
        return
      endif
      obj%files(ifile)%p => &
           trcio_open(obj%filenames(ifile),io_mode,history=obj%history)
    case default
      ! error message here...
  end select

  istat  = cio_chmod(obj%filenames(ifile),obj%permission)
  if(istat  /= cio_ok ) then
     call pc_error('TROT: Unable to set file permissions on file: ' &
     //trim(obj%filenames(ifile))//'.')
     return
  endif


!</execute_only>
  if(io_mode /= 'a') then

    obj%files(ifile)%p%retention   = obj%retention
    obj%files(ifile)%p%permission  = obj%permission
    obj%files(ifile)%p%ftype       = obj%type(1:5)  !ifc will give warning.
    obj%files(ifile)%p%nwih        = obj%nwih
    ! Cook:  don't be fiddling with my LBO word types!
    if(obj%files(ifile)%p%ftype(1:3) /= 'LBO') then
       if(obj%num_bits < 32 ) then
         obj%files(ifile)%p%wtype     = 'INT'
       else
         obj%files(ifile)%p%wtype     = 'IEEE'
       endif
    endif

    if (obj%opt_norm == 'AMPL_MAX') then
      obj%files(ifile)%p%use_ampl_max = .true.
      obj%files(ifile)%p%ampl_max     = obj%ampl_max
    else
      ! use_ampl_max is set to .false. in trcio_create, so I really
      ! do not need this.
      !
      obj%files(ifile)%p%use_ampl_max = .false.
    endif

    obj%files(ifile)%p%nbits       = obj%num_bits
    obj%files(ifile)%p%nbits_hd    = 64
    obj%files(ifile)%p%tmin        = obj%tstrt
    obj%files(ifile)%p%tmax        = obj%tstrt + real(obj%ndpt - 1) * obj%dt
    obj%files(ifile)%p%dt          = obj%dt
    obj%files(ifile)%p%nwih        = obj%nwih
    obj%files(ifile)%p%num_values  = obj%ndpt
    !--------- binning information -------
    obj%files(ifile)%p%nhd1        = 0
    obj%files(ifile)%p%vwidth1     = 0
    obj%files(ifile)%p%vbin1       = 0
    obj%files(ifile)%p%vmin1       = 0
    obj%files(ifile)%p%vmax1       = 0
    obj%files(ifile)%p%nhd2        = 0
    obj%files(ifile)%p%vwidth2     = 0
    obj%files(ifile)%p%vbin2       = 0
    obj%files(ifile)%p%vmin2       = 0
    obj%files(ifile)%p%vmax2       = 0
    obj%files(ifile)%p%common%xorigin = grid_get_xorigin(obj%grid)
    obj%files(ifile)%p%common%yorigin = grid_get_yorigin(obj%grid)
    obj%files(ifile)%p%common%dx11    = grid_get_dx11(obj%grid)
    obj%files(ifile)%p%common%dx12    = grid_get_dx12(obj%grid)
    obj%files(ifile)%p%common%dx21    = grid_get_dx21(obj%grid)
    obj%files(ifile)%p%common%dx22    = grid_get_dx22(obj%grid)
    obj%files(ifile)%p%endian      = swap_endian()

  endif

  select case(obj%type)
  !case('CMPR')
  case('TRCIO','LBO','LBO2')
    obj%num_bits_hdr = 64
    if(io_mode /= 'a') then
      istat = trcio_writeheader(obj%files(ifile)%p)
      if(istat /= trcio_ok)&
        call pc_error('trot: error writing TRCIO file header.')
      !!! we are going to capture the job globals in the file here.
      istat = trcio_write_globals(obj%files(ifile)%p,obj%num_global_cards,cards)
      if(istat /= trcio_ok)&
        call pc_error('trot: error writing TRCIO job globals.')
      call trcio_write_history_cards(obj%files(ifile)%p,obj%history,obj%ipn)
      called_bhist = .true.
    else
      if(obj%files(ifile)%p%endian /= swap_endian() ) then
        call pc_error('TROT: Cannot write on this architecture. ByteSwap!')
        return
      endif
    endif
! case('QTROT')
!   if(modulo(obj%files(ifile)%p%num_values,2) /= 0) &
!     obj%files(ifile)%p%num_values  = obj%files(ifile)%p%num_values+1
!   obj%files(ifile)%p%endian            = 1
!   obj%files(ifile)%p%nbits_hd          = 32
!   obj%files(ifile)%p%nbits             = 32
!   obj%num_bits                         = 32
!   obj%num_bits_hdr                     = 32
!   obj%files(ifile)%p%ftype             = 'QTROT'
!   obj%files(ifile)%p%wtype             = 'IEEE'
!   obj%files(ifile)%p%recl              = 4*(obj%files(ifile)%p%nwih &
!                                           + obj%files(ifile)%p%num_values)
!   obj%files(ifile)%p%data_start_pos    = (/0,4096/)
!   obj%files(ifile)%p%data_end_pos      = (/0,4096/)

!   istat  = trcio_writeheader(obj%files(ifile)%p)
!   if(istat /= trcio_ok) then
!     call pc_error('trot: error writing QTROT file header.')
!   endif

  case('SEGY')
    if(io_mode /= 'a') then
      call mapsegy(obj%mapsegy,obj%files(ifile)%p)
      obj%files(ifile)%p%endian      = 1
      obj%files(ifile)%p%nwih        = 60
      obj%files(ifile)%p%nbits_hd    = 32
      obj%num_bits_hdr               = 32
      select case(obj%num_bits)
      case (16)
    !    if(obj%dyn_range_cmpr    == 'YES' ) then
    !      obj%files(ifile)%p%wtype     = 'CMPI'
    !      bhed%format                  = 6
    !    else
          obj%files(ifile)%p%wtype     = 'INT'
          bhed%format                  = 3
    !    endif
      case (8)
    !    if(obj%dyn_range_cmpr    == 'YES' ) then
    !      obj%files(ifile)%p%wtype     = 'CMPI'
    !      bhed%format                  = 7
    !    else
          obj%files(ifile)%p%wtype     = 'INT'
          bhed%format                  = 6
    !    endif
      case default
        obj%files(ifile)%p%wtype       = 'IBM'
        obj%files(ifile)%p%nbits       = 32
        obj%num_bits                   = 32
        bhed%format                    = 1
      end select


      ascii%h(1) = 'C Default EBCDIC header for ConocoPhillips Seismic Imaging.'
      do i = 2,40
        ascii%h(i) = 'C'
      end do
      if(obj%segyheader /= PATHCHECK_EMPTY ) then
        ascunit = cio_fopen(trim(obj%segyheader),'r')
        if(ascunit <= 0 ) then
          call pc_info('TROT: Error opening SEGY header file: '// &
          trim(obj%segyheader)//'.')
        else
          istat = cio_fgetline(ascii%h(1),80,ascunit)
          call cio_frewind(ascunit)
          if(istat < 0 ) then ! no newlines
            do i = 1, 40
              istat = cio_fread(ascii%h(i),80,1,ascunit)
              if(istat < cio_ok) &
                call pc_error('TROT: Error reading SEGY header file.')
            end do
          else ! header file has newline characters...
            do i = 1, 40
              istat = cio_fgetline(ascii%h(i),80,ascunit)
              if(istat < cio_ok) &
                call pc_error('TROT: Error reading SEGY header file.')
            end do
          endif
          istat = cio_fclose(ascunit)
          if(istat /= cio_ok ) &
            call pc_error('TROT: Error closing SEGY header file.')
        endif
      endif

      !--- print out ebcdic header ---
      write(obj%stdout,'(a)')&
        '--- EBCDIC HEADER THAT WILL BE ATTACHED TO FILE ---'
      do i = 1,40
        write(obj%stdout,'(a)')ascii%h(i)
      end do

      istat = segy_write_ebchdr(obj%files(ifile)%p%lun,ascii)
      if(istat /= 0 ) call pc_error('TROT: Error writing SEGY ebcdic header.')

      bhed%jobid     = 1
      bhed%lino      = 1
      bhed%reno      = 1
      bhed%ntrpr     = 1
      bhed%nart      = 1
      bhed%hdt       = nint(obj%dt*1E6)
      bhed%dto       = nint(obj%dt*1E6)
      bhed%hns       = obj%ndpt
      bhed%nso       = obj%ndpt
      ! bhed%format already done above...
      bhed%fold      = 1
      bhed%tsort     = 1
      bhed%vscode    = 1
      bhed%hsfs      = 0
      bhed%hsfe      = 0
      bhed%hslen     = 0
      bhed%hstyp     = 0
      bhed%schn      = 0
      bhed%hstas     = 0
      bhed%hstae     = 0
      bhed%htatyp    = 0
      bhed%hcorr     = 1
      bhed%bgrcv     = 2
      bhed%rcvm      = 1

      call pc_get_pdata('SURVEY_UNITS',survey_units)
      select case(survey_units)
        case('FEET')
          bhed%mfeet = 2
        case('METERS')
          bhed%mfeet = 1
        case default
          bhed%mfeet = 1
      end select

      bhed%polyt     = 1

      istat = segy_write_binhdr(obj%files(ifile)%p%lun,bhed)
      if(istat /= 0 ) then
        call pc_error('TROT: Error writing SEGY binary header.')
      endif

      obj%files(ifile)%p%data_start_pos=(/0,3600/)
      obj%files(ifile)%p%data_end_pos=(/0,3600/)
      obj%files(ifile)%p%recl  = 240 + &
                (obj%files(ifile)%p%nbits*obj%files(ifile)%p%num_values)/8
    endif
  case ('SU')
    if(io_mode /= 'a') then
      call mapsegy(obj%mapsegy,obj%files(ifile)%p)
      obj%files(ifile)%p%endian      = 1
      obj%files(ifile)%p%nwih        = 60
      obj%files(ifile)%p%nbits_hd    = 32
      obj%num_bits_hdr               = 32
      obj%files(ifile)%p%wtype       = 'IBM'
      obj%files(ifile)%p%nbits       = 32
      obj%num_bits                   = 32
      bhed%format                    = 1
      obj%files(ifile)%p%data_start_pos=(/0,0/)
      obj%files(ifile)%p%data_end_pos=(/0,0/)
      obj%files(ifile)%p%recl  = 240 + &
                (obj%files(ifile)%p%nbits*obj%files(ifile)%p%num_values)/8
    endif
  case ('JSEIS')
    print*,' Jseis opened -- history and header update not supported.'
  case default
    call pc_error('TROT: file type not supported.')

  end select

  if(cio_fflush(obj%files(ifile)%p%lun) /= cio_ok ) then
    call pc_error('TROT: error flushing buffers.')
  endif

  if(cio_fseek(obj%files(ifile)%p%lun,0,2) /= cio_ok ) then
    call pc_error('TROT: error moving to EOF.')
  endif

  if(pc_previous_error() ) then
    status = trcio_error
  else
    status = trcio_ok
  endif

end function trot_open_file

  subroutine trot_alloc_global_hdi_tri(obj,ntr)
    type(trot_struct),pointer   :: obj
    integer,intent(inout)       :: ntr
    integer                     :: ierr

    if (ntr > global_ntr) then

      if(global_ntr > 0) then

        deallocate(global_hdi, STAT=ierr)
        if (ierr /= 0) goto 98

        deallocate(global_tri, STAT=ierr)
        if (ierr /= 0) goto 98

      endif

      global_ntr  = ntr
      global_nwih = obj%nwih
      global_ndpt = obj%ndpt

      allocate(global_hdi(global_nwih,global_ntr), STAT=ierr)
      if (ierr /= 0) goto 99

      allocate(global_tri(global_ndpt,global_ntr), STAT=ierr)
      if (ierr /= 0) goto 99

    else if (global_ntr > 0) then

      if (obj%nwih /= global_nwih) then

        deallocate(global_hdi, STAT=ierr)
        if (ierr /= 0) goto 98

        global_nwih = obj%nwih

        allocate(global_hdi(global_nwih,global_ntr), STAT=ierr)
        if (ierr /= 0) goto 99

      endif

      if (obj%ndpt /= global_ndpt) then

        deallocate(global_tri, STAT=ierr)
        if (ierr /= 0) goto 98

        global_ndpt = obj%ndpt

        allocate(global_tri(global_ndpt,global_ntr), STAT=ierr)
        if (ierr /= 0) goto 99

      endif

    endif

    return

98  write(obj%stdout, '(a)') 'deallocate error in trot_alloc_global_hdi_tri'
    ntr = FATAL_ERROR
    return

99  write(obj%stdout, '(a)') 'out of memory in trot_alloc_global_hdi_tri'
    ntr = FATAL_ERROR
    return

  end subroutine trot_alloc_global_hdi_tri

  subroutine trot_multifile_initialize(obj)
    type(trot_struct), intent(inout) :: obj

    obj%opt_switch = 'COUNT'
    obj%hdr_bin    = 3
    obj%bin_init   = 1.0
    obj%bin_inc    = 1.0
    obj%bin_last   = 1.0
    obj%bin_tot    = 1
    obj%file_max   = 99999999

  end subroutine trot_multifile_initialize

  subroutine trot_multifile_update(obj)
    type(trot_struct), intent(inout) :: obj

    logical                          :: binning
    integer                          :: old_hdr_bin
    double precision                 :: old_bin_inc
    double precision                 :: old_bin_last
    integer                          :: old_bin_tot
    integer                          :: old_file_max

    old_hdr_bin  = obj%hdr_bin
    old_bin_inc  = obj%bin_inc
    old_bin_last = obj%bin_last
    old_bin_tot  = obj%bin_tot
    old_file_max = obj%file_max

    call pc_get('opt_switch', obj%opt_switch)
    call pc_get('hdr_bin'   , obj%hdr_bin   )
    call pc_get('bin_init'  , obj%bin_init  )
    call pc_get('bin_inc'   , obj%bin_inc   )
    call pc_get('bin_last'  , obj%bin_last  )
    call pc_get('bin_tot'   , obj%bin_tot   )
    call pc_get('file_max'  , obj%file_max  )

    call pc_put_options_field('opt_switch', opt_opt_switch, num_opt_opt_switch)

    select case (obj%opt_switch)
      case ('COUNT')
        binning = .false.
      case ('HDR'  )
        binning = .true.
      case default
        call pc_error('TROT:  Illegal value for opt_switch')
        return
    end select

    call pc_put_sensitive_field_flag ('hdr_bin' ,       binning)
    call pc_put_sensitive_field_flag ('bin_init',       binning)
    call pc_put_sensitive_field_flag ('bin_inc' ,       binning)
    call pc_put_sensitive_field_flag ('bin_last',       binning)
    call pc_put_sensitive_field_flag ('bin_tot' ,       binning)
    call pc_put_sensitive_field_flag ('file_max', .not. binning)

    if ((obj%hdr_bin < 1) .or. (obj%hdr_bin > obj%nwih)) then
      call pc_error('TROT:  hdr_bin must be between 1 and nwih.')
      obj%hdr_bin = old_hdr_bin
    endif

    if (obj%bin_inc == 0.0) then
      call pc_error('TROT:  bin_inc cannot be zero.')
      obj%bin_inc = old_bin_inc
    endif

    if (obj%bin_tot < 1) then
      call pc_error('TROT:  bin_tot must be at least one.')
      obj%bin_tot = old_bin_tot
    endif

    if (obj%bin_last /= old_bin_last) then
      obj%bin_tot = mth_bin_number(obj%bin_init, obj%bin_inc, obj%bin_last)
      if (obj%bin_tot < 1) then
        obj%bin_tot = 1
      endif
    endif

    obj%bin_last = mth_bin_center(obj%bin_init, obj%bin_inc, obj%bin_tot)

    if (obj%file_max < 1) then
      call pc_error('TROT:  file_max must be at least one.')
      obj%file_max = old_file_max
    endif

    call pc_put('opt_switch', obj%opt_switch)
    call pc_put('hdr_bin'   , obj%hdr_bin   )
    call pc_put('bin_init'  , obj%bin_init  )
    call pc_put('bin_inc'   , obj%bin_inc   )
    call pc_put('bin_last'  , obj%bin_last  )
    call pc_put('bin_tot'   , obj%bin_tot   )
    call pc_put('file_max'  , obj%file_max  )

  end subroutine trot_multifile_update

  subroutine trot_end_trap

    character(len=FILENAME_LENGTH), dimension(:), pointer :: filenames
    integer                                               :: num_files
    logical                                               :: from_ascii_file
    logical                                               :: status
    logical                                               :: error
    character(len=200)                                    :: err_msg
    character(len=80)                                     :: thisext
    integer :: i,k

    nullify (filenames) ! jpa

    status = &
      mfile_get_filenames(trp%mfile, filenames, num_files, from_ascii_file)


!        If type is lbo, file extension should be lbo
    if(associated(filenames) .and.             &
      (trp%type=='LBO' .or. trp%type.eq.'LBO2'))then
      do i=1,num_files
        k=index(filenames(i),'.',.true.)
        thisext=filenames(i)(k+1:len_trim(filenames(i)))
        if(thisext.ne.'lbo')then
          call pc_warning('Type is LBO, but file extension is ',trim(thisext))
          exit
        endif
      enddo
    endif

    if (status) then
      call mem_free(filenames)
    endif

    !if ((trp%type == 'CMPR') .or. (trp%create_csv == 'YES')) then
    if (trp%create_csv == 'YES') then

      if ((.not. status) &
     .or. from_ascii_file &
     .or. (trp%opt_switch /= 'COUNT')) then

        error = .true.

      ! Check num_files separately because num_files is invalid if status
      ! is .false.
      !
      else if (num_files /= 1) then

        error = .true.

      else

        error = .false.

      endif

      if (error) then
        !err_msg = 'TROT:  With CMPR or csv file, you must enter a ' &
        err_msg = 'TROT:  With csv file, you must enter a ' &
               // 'single file into the array and set opt_switch to COUNT.'

         call pc_error(err_msg)
       endif

    else if (trp%opt_switch == 'HDR') then

      if (.not. status) then

        error = .true.

      ! Check num_files separately because num_files is invalid if status
      ! is .false.
      !
      else if (num_files /= trp%bin_tot) then

        error = .true.

      else

        error = .false.

      endif

      if (error) then
        err_msg = 'TROT:  Must have same number of files as bins.'

         if (from_ascii_file) then
           call pc_warning(err_msg)
         else
           call pc_error  (err_msg)
         endif
       endif

    endif

  end subroutine trot_end_trap

  ! trot_which_file is designed to be called from code which rights one
  ! trace at a time, not if writing compressed data.
  ! Returns  1 to num_files if ok.
  ! Returns  0 if no place for trace.
  ! Returns -1 if error.
  !
  function trot_which_file(obj, hd) result(ifile)
    type(trot_struct), pointer                 :: obj
    double precision, intent(in), dimension(:) :: hd
    integer                                    :: ifile

    select case (obj%opt_switch)
      case ('HDR')
        ifile = mth_bin_number(obj%bin_init, obj%bin_inc, hd(obj%hdr_bin))
      case ('COUNT')
        ifile = (obj%tot_traces_written / obj%file_max) + 1
      case default
        call pc_error('TROT:  illegal value for opt_switch in trot_which_file')
        ifile = -1
        return
    end select

    if ((ifile >= 1) .and. (ifile <= obj%num_files)) then
      obj%traces_written(ifile) = obj%traces_written(ifile) + 1
      obj%tot_traces_written    = obj%tot_traces_written    + 1
    else
      obj%orphan_traces         = obj%orphan_traces         + 1
      ifile = 0
    endif

  end function trot_which_file

end module trot_module
