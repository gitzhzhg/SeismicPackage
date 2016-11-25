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
! Name       : TTROT   
! Category   : io
! Written    : 2000-07-28   by: RSDay
! Revised    : 2007-07-10   by: Bill Menger
! Maturity   : beta
! Purpose    : Write trace data to magnetic tape.
! Portability: Limited to Unix OS
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! Type of Output
!
! Output options are:  SCR OR PS.
!
!       IF TYPE_OUT = SCR, traces are written to scratch tapes.
!
!       If TYPE_OUT = PS, traces (and optionally other processing information)
!       are written to new tapes.  These tapes are entered into the 
!       Conoco General Tape Library Database System and are stored indefinitely.
!
!
! PDN Descriptor
!
! PDNs are tape datasets consisting of one or more physical tapes identified by
! a single name, the PDN Descriptor.  PDNs are managed by the Computer 
! Operations Tape Database.  The PDN Descriptor consists of 7 elements, 
! described as follows,
!
!   PDN Descriptor = MED.USERID.PROJECT.SUB_PROJECT.TASK.FORMAT.COUNTER
!
!       MED  is the tape medium the PDN is written on (3590, 8mm, etc.).  It is
!       a display-only parameter that echoes the value of the MEDIA parameter.
!
!       USERID is any valid userid and defaults to the current userid.
!
!       PROJECT  defaults to the value of the PROJECT parameter in PROJECT_DATA.
!
!       SUB_PROJECT  can be used to designate a sub-category of the project, 
!       such as "decon" or "dmo" or a range of inlines.  It defaults to the
!       Sub-Project Name from JOB_DATA.
!
!       TASK  can be used to designate a part of the sub-project, such as 
!       "mute_test".  It defaults to blank.  If TYP_OUT=PS, TASK defaults to
!       permsave.
!
!       COUNTER  is a numeric counter, starting with 1 and incrementing by 1 for
!       subsequent PDNs with otherwise identical PDN Descriptors.
!
!       FORMAT defaults from the format parameter, CPS, SEGY, or BLK.
!
!
!
! Permanent Save
!
! The permanent save option is made available when TYPE_OUT=PS.
! When PATH_PS = filename, a file containing processing information can be 
! written on the permanent save tape in addition to trace data. (The permsave
! program is available that creates a permsave.list file containing a list of 
! filenames to be written on the permanent save tape.)  
! New tapes will be selected for the permsave job.
! A label file for the permanent save tape is created and sent to your
! current directory.  The name of the file will be jobname.label.  If the
! file already exists, ttrot will append new information to the file.  This
! file can be edited with a text editor if necessary. It should be sent to the
! tape librarians who will need to add the rack and slot numbers before 
! printing the label.  
! TYPE_OUT=PS_VMOD is a special permsave option for saving velocity models.
! The grid files must first be converted to trace files using the PGPS program.
! The PATH_PS parameter is required and the list must contain .hgrid and 
! .modgrid files only.  No label files is written for this option.
! 
!
!
! Monitoring Outputs
! 
! TTROT always prints a list of PDN descriptors or tape volumes for the outputs
! of that TTROT process.  TTROT also can list the minimum and maximum values of
! HDR_SUM1 and HDR_SUM2 for each PDN or tape volume output.
!
!
! Output Switching Control
!
! NUM_TR specifies the maximum number of traces to write to a PDN or user-
! specified tape.  However, output switching can also be controlled by the 
! Output Switching by Header Word parameters.  If header word switching is 
! enabled, TTROT ignores NUM_TR.
!
!
! Tape Format
!
! Traces are written out in 32 bit IBM floating point format.  Header words are
! placed at the head of the trace. The first file on each tape is a processing
! history file.
! 
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! PDN Defaults When Editing Old Workfiles
!
! When editing an old workfile, the DEFAULTS parameter allows users to select 
! either system default values (SYS) or default values from the old workfile 
! (OLD).  DEFAULTS is a front-end only parameter.
!
!
! Filling in Rows of PDN or VOLUME Arrays Automatically
!
! The FILL_ROWS parameter is a toggle for automatic filling of the PDN or 
! VOLUME arrays based on the entry in the first row.  Values of the first 5 PDN
! elements are repeated and COUNTER is incremented by 1 for each row.  The 
! alpha part of VOLUME is repeated and the numeric part is incremented by 1 for
! each row.  FILL_ROWS is a front-end only parameter.
!
!
! SEGY Header Word Mapping
!
! Parameters are provided that allow for customized mapping of CPS header words 
! onto SEGY tapes.  Normally these will not be required and the default will 
! suffice.  The default is applied first and only the headers specified in the 
! custom mapping will be changed.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a single-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process does not alter input traces.
! This process outputs the same traces as it receives.
!
! This process outputs traces with same gather status as the input traces.
!
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED          
!
! 
! Name     Description                           Action taken
! ----     -----------                           ------------
! IPN      process number                        used (must not be changed)
! MAXTR    max number of traces input/output     used but not changed
! NWIH     number of words in trace header       used but not changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
! GRID     grid transformation structure         used but not changed
!
! Current global values are transfered to the tape volumes.
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
! 1       Sequential Trace Count     Renumbered (for traces written to tape).
! Headers on SEGY tapes use the default standard unless custom mapping is used.
! 
! Headers for traces passing through TTROT are unchanged.
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author       Description
!     ----        ------       -----------
! 68. 2007-07-10  Bill Menger  Change name of %trin_filenames file to one that
!                              doesn't conflict with other jobs or nodes in job.
! 67. 2006-11-27  Stoeckley    Replace pc_register_tab_group w HelpSection line;
!                               fix a HelpSection error to fix a SeisSpace gui
!                               problem.
! 66. 2006-08-24  D. Glover    Added NULLIFY statements for Intel compiler.
! 65. 2006-06-20  Stoeckley    Add pc_register_tab_group for SeisSpace.
! 64. 2006-06-06  Stoeckley    Add pc_register_array_names for SeisSpace.
! 63. 2006-01-10  B. Menger    Removed Unused Variables.
! 62. 2005-05-17  Goodger      Add host odi91 and media 3592.
! 61. 2005-03-01  Goodger      Replace transfer function with cmem_cpy_c.
! 60. 2004-08-10  Goodger      Attempt a forced unload after a failure to load
!                              problem.  Get location from config file.
! 59. 2004-03-15  Goodger      Add tape_host hnomega1 for testing.  Set media
!                              to NR3590 if tape_host is jaws or hnomega1.
!                              Check that volume is 6 char or less if jaws or
!                              hnomega1.
! 58. 2004-01-19  Goodger      Change tape_host in Alaska from mink to jaws.
! 57. 2003-12-19  Goodger      Insure tape_host is hoeplgp01 for meda type LTO.
! 56. 2003-12-05  Goodger      Do not allow blanks in VOLUME, or any volume
!                              field.
! 55. 2003-11-20  Goodger      Change media default to 3590HD. Increase number
!                              of traces per tape calculation from 90 to 95
!                              percent tape capacity. Change tape_host default
!                              to odi90.
! 54. 2003-10-20  Goodger      Add media type 3590HD and insure that it is
!                              part of the pdn.  Set tape host to odi90 if
!                              3590HD and to host odi74 if 3590.  Increase
!                              the number of traces allowed per tape for 3590HD.
! 53. 2003-10-15  Goodger      Do not rewrite a scratch tape if it is already
!                              catalogued, even if the userid is the same as
!                              that in the catalog.
! 52. 2003-10-10  Goodger      Write out control card tapequeue which will be
!                              read by the job builder.  This change 
!                              corresponds to buildjob.f90 revision 129.
! 51. 2003-10-07  Goodger      Use NINT function when calculating integer
!                              sample rate which goes into the SEGY 400 byte
!                              header.  This resolves a floating point 
!                              arithmetic problem.
! 50. 2003-10-06  Goodger      Do not write a tape label on SEGY tapes.
! 49. 2003-09-29  RSDay        Changed close proceedure .
! 48. 2003-08-05  Goodger      Insure tape_host default is odi74.
! 47. 2003-07-30  RSDay        Added tape_host gui component for odi74 and
!                              odi90 access. Added ttrot_get_hosts.
! 46. 2003-04-01  RSDay        Check for catalogued scratch real on open
! 45. 2003-03-12  RSDay        Added asynchronous writes for TPR output format
! 44. 2003-02-14  RSDay        TPR format had a problem when headers were not
!                              packed. f90 transfer replaced by txfr function.
! 43. 2003-02-12  RSDay        Fixed logic of max_per_vol. The pc_get of the
!                              media parameter needed to be before the calc.
! 42. 2003-02-11  RSDay        Added LTO, 3480, and NR3590 media types.Added
!                              output format type=TPR which writes an integer
!                              multiple number of output traces to a tape
!                              record for CPS tapes. Max TPR buffer size
!                              used is 41600 bytes(A UDP socket limitation).
!                              Increased max_per_vol when media=LTO.
! 41. 2002-10-10  Ed Schmauch  Remove Ponca City specific configuration info.
!                              Rely on tapecnfg.h.
! 40. 2002-09-25  Ed Schmauch  Allow upper case if volume is typed in.
! 39. 2002-09-25  Ed Schmauch  Added ttrot_fill_rows_trap and ttrot_userid_trap.
! 38. 2002-08-26  K. Goodger   Clear PS fields when TYPE_OUT is changed from
!                              PS to SCR.
! 37. 2002-08-12  K. Goodger   Change default for NUM_TR parameter to 99999999.
! 36. 2002-07-30  K. Goodger   Add TYPE_OUT=PS_VMOD to Permsave velocity model
!                              files.
!                              Remove TIM_MAX parameter.  Data will be output
!                              according to globals.
! 35. 2002-04-18  K. Goodger   Insure header mapping array turned off if
!                              mod_segy is NO. Move tests on sbyte 
!                              arrayset to arrayset traps.
! 34. 2002-02-04  K. Goodger   Correct problem in routine wrprec. The number of
!                              cards calculated in a processing record was 1 too
!                              many, thereby adding a card of garbage to the
!                              record.  Fix warnings generated by intel
!                              compiler.
! 33. 2001-10-18  Ed Schmauch  Use tcatclient module for all tapecat
!                              communication.
! 32. 2001-09-14  Ed Schmauch  Fixed bug which caused trace to be lost if output
!                              tape had less capacity than expected.  Allowed
!                              writing to unlabeled tapes.
!                              The above bug was fixed by changing error checks
!                              on tpioclnt_write from < 0 to <= 0.  Write
!                              returns 0 if you try to write past the end of
!                              tape and < 0 if you try to write again.
!                              Note that continuing after a tpioclnt_write
!                              returns a 0 by going to the next tape is only
!                              handled for CPS and SEGY tapes; not for BLK.
! 31. 2001-08-31  Goodger      Move fill_rows parameter to top of pc_gets.
! 30. 2001-08-30  Goodger      Front end pdn changes.
! 29. 2001-08-03  Goodger      Replace history module with manhist module.
! 28. 2001-07-10  Goodger      Fix problem with overflowing history buffer.
! 27. 2001-07-10  Ed Schmauch  Argument list to tcclient_recv changed. PROD.
! 26. 2001-05-14  Goodger      Multiple reel permsave.
! 25. 2001-03-22  RSDay        uncatalogued PDN will default to NSCR reels if
!                              path_ps is set. Perm save records to reel 1 of N
! 24. 2001-03-08  RSDay        History=NONE was bypassing EOF write.
!                              Eliminated spurious permsave writes.
! 23. 2001-03-06  Goodger      Add label information for permsave tapes.
! 22. 2001-02-23  Goodger      Eliminate step of reading path_ps file names
!                              at setup time.
! 21. 2001-02-16  Goodger      Use the pathcheck primitive for path_ps.
! 20. 2001-02-15  Goodger      Force the segy header file written to tape to
!                              be 80 characters by 40 lines.
!                              Use the pathcheck primitive on path_segy.
! 19. 2001-02-15  RSDay        Increased allowed size of process record
!                              path names to 80. NSCR recognized as a new
!                              scratch tape. Increased process record
!                              limit to 5000 files from 500 files.
! 18. 2001-02-01  RSDay        More print on write errors. Stops SCR from
!                              being catalogued.
! 17. 2001-01-29  RSDay        Fixed problem with premature closing of tapecat
!                              conection in wrapup. Added DLT support in
!                              ttrot_open. 
! 16. 2001-01-10  RSDay        Moved call to tcclient_halt to fix problem of
!                              crashing tapecat server 
! 15. 2000-12-12  RSDay        Added ttrot_mvdata to overcome an ABSOFT bug
!                              that affected data move between two real arrays
!                              This is similar to a ttrin bug reported earlier.
!                              one of the arrays is an equivalence?
! 14. 2000-12-11  RSDay        SEGY output was not writing 3200 & 400 byte
!                              records for non-labeled tapes(in ttrot_open)
! 13. 2000-12-08  RSDay        Activated path_segy segy ebcidic header option
! 12. 2000-11-21  RSDay        Fixed permanent save problems with subroutine
!                              ttrot_get_prfile. Increased allowed length of
!                              perm save files to 80 characters.
! 11. 2000-10-19  RSDay        mod_segy not forced by gui to yes when nummap >0
!                              eliminated transfer function call for
!                              segy header mapping(absoft bug).
!                              Non labeled reels are no longer a fatal error.
! 10. 2000-10-11  RSDay        Eliminated backspace on write error.
!                              ttrot_pak_hdtr decrements vscnt,nseqo,volcnt
!                              when there is a write error. Added a variable,
!                              max_per_vol which limits how many traces can
!                              be output to 1 volser.
!  9. 2000-10-06  RSDay        Renamed FILL to FILL_ROWS, and moved location
!                              of some gui fields. Desensitized DEFAULT.
!  8. 2000-10-02  RSDay        Fixed multi-volume tape handling. Added a
!                              back space on write error at end of tape.
!                              Fixed a trace bulk shift error for new
!                              cps tapes that packed headers(dt*64).
!  7. 2000-09-27  RSDay        Corrected nsout to round to nearset int.
!                              Cures a sample round off problem with TIM_MAX.
!  6. 2000-09-20  RSDay        Corrected logic in ttrot_next_vol so that
!                              volcnt is reset when it exceeds num_tr. Was
!                              causing too many reel changes with writes to
!                              a PDN. Also put some logic in place to force
!                              a reel change when a write error occurs.
!                              Previously this was always fatal.
!  5. 2000-08-31  RSDay        Replaced tapeio_* calls by the tpioclnt layer.
!                              TTROT now auto detects the host it is
!                              running on and calls tpioclnt_open accordingly.
!  4. 2000-08-23  RSDay        Increased the size of volume field in the GUI.
!                              Fixed a bug in ttrot_getpdn. Eliminated the
!                              redundant internal array pdn. Added userid
!                              checking of catalogued volumes. Increased default
!                              value of num_tr to 9999999. jobname variable
!                              increased to 16 bytes.
!                              Added commented lines for future socket io calls.
!  3. 2000-08-21  RSDay        Improved the behavior of the auto FILL option
!                              for pdn names by rearranging some parameter cache
!                              calls in the update function. Increased job_name
!                              variable to 16 characters. Changed a pc_error
!                              call to pc_info for non catalogued pdns.
!  2. 2000-08-01  RSDay        Arguments of tapeio primitives changed to be
!                              consistent with pfio interface.
!  1. 2000-07-28  RSDay        Initial version.
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
! 
!
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
!    NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
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
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! TTROT communicates with a tape device through the tpioclnt layer. The
! tpioclnt functions may call tapeio routines(tapeio.c) directly, or they
! can call a socket communications layer to route io requests to a tape server.
!
! Actual io operations to tape, use low level Unix ioctl IO
! calls when writing the data to tape. Unix ioctl calls are used for
! some tape operations, such as rewind, dismounts, or writing tape marks.
! The actual data records are written to tape with the generic write
! function from c-language.
!
! tapeio fupnctions called by ttrot
! tpioclnt_open_server     tpioclnt_write    tpioclnt_weof
! tpioclnt_close    tpioclnt_gvol     tpioclnt_prnt
! 
!-------------------------------------------------------------------------------
!</programming_doc>

!<gui_def>
!<NS TTROT Process/NC=80>
!                 Write trace data to magnetic tape.
!            (Includes the former SEGY and TCAT processes)
!
! COMMENT=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! FORMAT~~=`CCCC       MEDIA~~~=`CCC    HISTORY=`CCCCCCC
!
! TYPE_OUT=`CCCCCC     MOD_SEGY=`CCC    PACK_HDR=`CCC    RETENTION=`III
!   
! USERID=`SSSSSSS PROJECT=`SSSSSSSSS SUB_PROJECT=`SSSSSSSSS TASK=`SSSSSSSSS
!
! DEFAULTS=`CCC    FILL_ROWS=`CCC     NUM_ROWS=`III  TAPE_HOST=`CCCCCCCC
!
!
! VOLUME                                          NUM_TR
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS`IIIIIII
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS`IIIIIII
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS`IIIIIII
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS`IIIIIII
!
! PATH_SEGY=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!
! HDR_FLAG~~=`IIII     HDR_SUM1=`IIII     HDR_SUM2=`IIII
!
! HDR_SWITCH=`IIII     BEG_INIT=`FFFFFFF  BEG_INC~=`FFFFFF
!
!   SBYTE BYTES CPS_HDR WTYPE
!   `IIIII`IIIII`IIIIIII`SSSS
!   `IIIII`IIIII`IIIIIII`SSSS
!   `IIIII`IIIII`IIIIIII`SSSS
!   `IIIII`IIIII`IIIIIII`SSSS
!
!<PARMS COMMENT[/ML=120/XST]>
!<PARMS VOLUME_ARRAYSET[/XST/YST]>
!<PARMS PATH_SEGY[/ML=80/XST]>
!<PARMS SBYTE_ARRAYSET[/XST/YST]>
!<NS Permsave/NC=80>
!
!                 Permsave Parameters
!
! PATH_PS=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! MAX_TR =`IIIIIII
!
!    LABEL_TYPE=`CCCCCCCCCCCCCC
!
!          QUAD=`SSSSSSSSSSSSSSSSSSSS     COUNTRY=`SSSSSSSSSSSSSSSSSSS
!
!           DIR=`SSS                 PROJECT_NAME=`SSSSSSSSSSSSSSSSSSS
!
!      HDR_LINE=`IIII                  CONTRACTOR=`SSSSSSSSSSSSSSSSSSS
!
!      HDR_SHOT=`IIII                   PROCESSES=`SSSSSSSSSSSSSSSSSSS
!
! COMMENT_LABEL=`SSSSSSSSSSSSSSS       LINE_NAMES=`SSSSSSSSSSSSSSSSSSS
!
!                                      SHOTPOINTS=`SSSSSSSSSSSSSSSSSSS
!
!<PARMS PATH_PS[/ML=80/XST]>
!</gui_def>
!
!
!<HelpSection>
!
!      ------------- selection of tapes to be written -------------
!
!<Help KEYWORD="CAT">
!<Tip> To catalogue or not catalogue a PDN. </Tip>
! Default = N
! Allowed = char(48) (array)
! VOLUME names over 6 characters will be treated as a permanent data set
! name. If VOLUME exists in the catalogue it will be overwritten. If
! it is not in the catalogue scratch tapes will be used and catalogued.
! A maximum of 50 tapes may be catalogued uner one volume name.
!</Help>
!
!<Help KEYWORD="COMMENT">
!<Tip> Comment to operator regarding tape selection or disposition. </Tip>
! Default = BLANK
! Allowed = char (120)
! Normally COMMENT will be instructions regarding selection of tapes to be 
! written on and/or instructions for handling of tapes once written.
!</Help>
!
!<Help KEYWORD="FORMAT">
!<Tip> Format of the tapes being written. </Tip>
! Default = CPS
! Allowed = CPS   (CPS tape with one trace per write)
! Allowed = BLK   (CPS tape with one block per write - faster)
! Allowed = SEGY
! BLK format writes multiple traces per record - faster and less reliable.
! CPS format writes one trace per record - slower and more reliable.
! FORMAT=BLK is less reliable since one read error will destroy several 
! traces rather than just one.
!</Help>
!
!<Help KEYWORD="MEDIA">
!<Tip> Media type of the output tapes. </Tip>
! Default = 3590HD
! Allowed = 3590HD   (Magstar cartridge high density)
! Allowed = 3590     (Magstar cartridges )
! Allowed = 3480
! Allowed = 8mm
! Allowed = DLT
! Allowed = 3420  (9-track round media)
! Allowed = NR3590 (non robot 3590 )
! Allowed = LTO
! Only 3590 is currently valid.
!</Help>
!
!<Help KEYWORD="HISTORY">
!<Tip> How to control transfer of processing history. </Tip>
! Default = MODEL   (Save a set of histories that model job processing)
! Allowed = ALL     (Write all old histories.)
! Allowed = CURRENT (Write current history only.)
! Allowed = NONE    (Do not write any history.)
!</Help>
!
!<Help KEYWORD="MOD_SEGY">
!<Tip> Whether to modify default mapping of CPS headers to SEGY headers. </Tip>
! Default = NO
! Allowed = YES/NO
! If MOD_SEGY = YES, parameters for specifying the CPS to SEGY header mapping 
! are activated.
!</Help>
!
!<Help KEYWORD="PACK_HDR">
!<Tip> Pack or dont pack headers saved to CPS or BLK tape formats.</Tip>
! Default = YES
! Allowed = YES/NO
! If PACK_HDR = YES, headers are saved as 32 bit IEEE words
! If PACK_HDR = NO,  headers are saved as 64 bit IEEE words
!</Help>
!
!
!      ----------------- attributes of written tapes --------------
!
!
!<Help KEYWORD="USERID">
!<Tip> Userid for PDN.  Second element of PDN descriptor array. </Tip>
! Default = current userid
! Allowed = any valid userid (char 8) 
!</Help>
!
!<Help KEYWORD="PROJECT">
!<Tip> Project name for PDN.  Third element of PDN descriptor array. </Tip>
! Default = value of PROJECT parameter in PROJ_DATA
! Allowed = char (10) 
!</Help>
!
!<Help KEYWORD="SUB_PROJECT">
!<Tip> Sub-project name for PDN.  Fourth element of PDN descriptor array. </Tip>
! Default = BLANK
! Allowed = char (10) 
!</Help>
!
!<Help KEYWORD="TASK">
!<Tip> Task name for PDN.  Fifth element of PDN descriptor array. </Tip>
! Default = BLANK or permsave
! Allowed = char (10) 
!</Help>
!
!<Help KEYWORD="TYPE_OUT">
!<Tip> Whether tape written will be SCR or PS. </Tip>
! Default = SCR
! Allowed = SCR
! Allowed = PS
! Allowed = PS_VMOD
!
!       If TYPE_OUT = SCR, traces 
!       are written to scratch tapes.  These tapes are entered into the 
!       Conoco General Tape Library Database System and are stored indefinitely.
!
!       If TYPE_OUT = PS, traces (and optionally other processing information)
!       are written to new tapes.  These tapes are entered into the 
!       Conoco General Tape Library Database System and are stored indefinitely.
!
!       If TYPE_OUT = PS_VMOD, new tapes are used.  The data saved are velocity
!       models prepared by the PGPS program. The PATH_PS parameter is 
!       required, and the list must contain .hgrid and .modgrid files only.  
!
!</Help>
!
!<Help KEYWORD="VOLUME">
!<Tip> A volser or PDN of a tape to be written. </Tip>
! Default = blank
! Allowed = char(48)  (array)
! VOLUME less than 7 characters is interpreted as a specific volser.
! VOLUME names over 6 characters will be treated as a permanent data set
! name, i.e. a PDN. If VOLUME exists in the catalogue it will be overwritten.
! If it is not in the catalogue scratch tapes will be used and catalogued.
! A maximum of 50 tapes will be catalogued under one volume name. The
! hdr_switch or hdr_flag can be used to control volser changes when writing
! to PDNs.
!
!</Help>
!
!<Help KEYWORD="NUM_TR">
!<Tip> Maximum number of traces to write to a volume. </Tip>
! Default = 99999999
! Allowed = int>1 (array)
! TTROT will write no more than NUM_TR traces to a PDN or user-specified tape.
! Once TTROT has written NUM_TR traces to a PDN or user-specified tape and 
! there are more traces to be written, it will call for the next PDN or user-
! specified tape (if no more PDNs or user-specified tapes are listed, TTROT 
! will wrapup and become a transparent process). To control reel changes when
! writing to a PDN, one must use the hdr_switch or hdr_flag variables.
!</Help>
!
!
!<Help KEYWORD="RETENTION">
!<Tip> Time, in days, to retain PDN.(future use) </Tip>
! Default = 180
! Allowed = int, 1 - 365
!</Help>
!
!      ----------------- data to be written --------------
!
!
!
!<Help KEYWORD="PATH_SEGY">
!<Tip> Pathame of text file used to supply the 3200 byte SEGY header. </Tip>
! Default = blank
! Allowed = char
! This file must consist of exactly 40 lines with no more than 80 characters 
! each.  If no file is specified, a default empty SEGY header will be supplied.
!</Help>
!
!      ----------------- control of tape writing --------------
!
!<Help KEYWORD="HDR_SWITCH">
!<Tip> Header word used to control automatic output switching. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! If HDR_SWITCH = 0, then output switching is controlled by NUM_TR only. 
!
! If HDR_SWITCH > 0, then output switching is controlled by the value of 
! HDR_SWITCH and NUM_TR is ignored. 
!</Help>
!
!<Help KEYWORD="BEG_INIT">
!<Tip> Value of header word HDR_SWITCH to start first PDN or volume. </Tip>
! Default = 1
! Allowed = real
! BEG_INIT should be set to the value of HDR_SWITCH of the first trace to be 
! output. 
!</Help>
!
!<Help KEYWORD="BEG_INC">
!<Tip> Increment between values of header word HDR_SWITCH. </Tip>
! Default = 1
! Allowed = real
! A new output PDN or volume is started whenever 
!
!    HDR_SWITCH = BEG_INIT + n * BEG_INC,  where n is 1, 2, 3, ...
! 
!</Help>
!
!      ----------------- front-end only parameters --------------
!
!
!<Help KEYWORD="DEFAULTS">
!<Tip> Whether to default PDN or VOLUME to system or old workfile. </Tip>
! Default = SYS
! Allowed = SYS  (Set PDN or VOLUME defaults to system values.)
! Allowed = OLD  (Set PDN or VOLUME defaults to old workfile values.)
! System defaults are:  
!       MED echoes the value of the MEDIA parameter.  
!       USERID defaults to the current userid.  
!       PROJECT defaults to the value of the PROJECT parameter in PROJECT_DATA.
!       SUB_PROJECT defaults to JOB_DATA Sub-Project Name.
!       TASK defaults to BLANK or permsave.
!       COUNTER defaults to 1.
!       FORMAT defaults to FORMAT, cps, segy, or blk.
! 
! DEFAULTS is a front-end only parameter and operates only when editing old 
! workfiles.
!</Help>
!
!<Help KEYWORD="FILL_ROWS">
!<Tip> Whether to fill in PDN or VOLUME array. </Tip>
! Default = YES
! Allowed = YES/NO
! FILL_ROWS is a toggle for automatic filling in of the PDN or VOLUME
! arrays based on the entry in the first row.  Values of the first 5 PDN 
! elements are repeated and COUNTER is incremented by 1 for each row.  The 
! alpha part of VOLUME repeats and the numeric part is incremented by 1 for each
! row.
!
! FILL_ROWS is a front-end only parameter.
!</Help>
!
!
!<Help KEYWORD="NUM_ROWS">
!<Tip> Number of PDN or VOLUME array rows to fill. </Tip>
! Default = 1
! Allowed = 1 - 100
! NUM_ROWS determines the number of array rows to be filled by the 
! FILL_ROWS parameter.  NUM_ROWS is a front-end only parameter.
!</Help>
!
!                     ----SEGY Header mapping---
!
!<Help KEYWORD="SBYTE">
!<Tip> Starting byte of the SEGY header to receive CPS header. </Tip>
! Default = -
! Allowed = int > 0 (linked array)
! Active if MOD_SEGY = YES.
!</Help>
!
!<Help KEYWORD="BYTES">
!<Tip> Number of bytes to use for the CPS header. </Tip>
! Default = -
! Allowed = int > 0 (linked array)
! Active if MOD_SEGY = YES.
!</Help>
!
!<Help KEYWORD="CPS_HDR">
!<Tip> CPS header word to be moved. </Tip>
! Default = -
! Allowed = 1 - NWIH (linked array)
! Active if MOD_SEGY = YES.
!</Help>
!
!<Help KEYWORD="WTYPE">
!<Tip> Treat Segy header word as I(integer), or F(floating poit). </Tip>
! Default = -
! Allowed = char(8) (linked array)
! Active if MOD_SEGY = YES.
!</Help>
!
!                           ----Output Monitoring---
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0 
! Allowed = 0 - NWIH
! If HDR_FLAG = 0, then all traces are written to tape.  Otherwise, only traces
! with a flag set in header word HDR_FLAG are written to tape.  
!</Help>
!
!<Help KEYWORD="HDR_SUM1">
!<Tip> Print headsum of HDR_SUM1 for each PDN or tape volume output. </Tip>
! Default = 0
! Allowed = 0 - NWIH 
! If HDR_SUM1 = 0, then do not print a headsum.
! If HDR_SUM1 > 0, then print in the .rpt file the minimum and maximum values 
! of HDR_SUM1 for each PDN or tape volume output.
!</Help>
!
!<Help KEYWORD="HDR_SUM2">
!<Tip> Print headsum of HDR_SUM2 for each PDN or tape volume output. </Tip>
! Default = 0
! Allowed = 0 - NWIH 
! If HDR_SUM1 = 0, then do not print a headsum of HDR_SUM2.
! If HDR_SUM2 > 0, then print in the .rpt file the minimum and maximum values 
! of HDR_SUM2 for each PDN or tape volume output.
!</Help>
!
!       tabgroup = Permsave
!
!<Help KEYWORD="PATH_PS">
!<Tip> Pathname of file listing processing information to be saved. </Tip>
! Default = permsave.list
! Allowed = char
! A file of file names that are to be saved as processing records.
! All files listed must be ascii files. Processing records are added
! to the history file on a tape. They are distinguished from true history
! records by a PR=CON string at the start of a processing record.
!</Help>
!
!<Help KEYWORD="MAX_TR">
!<Tip> Maximum number of traces per tape on the permsave pdn. </Tip>
! Default = 0
! Allowed = 0 to max traces a tape will hold
! If zero, ttrot will output the maximum number of traces a tape will hold on
! each tape in the pdn.  The TTROT front end calculates the maximum number of
! traces a tape will hold and informs the user during the front end session.
!</Help>
!
!<Help KEYWORD="LABEL_TYPE">
!<Tip> Whether label is domestic or international format. </Tip>
! Default = INTERNATIONAL
! Allowed = INTERNATIONAL, DOMESTIC
!</Help>
!
!<Help KEYWORD="QUAD">
!<Tip> Quadrangle field to identify output tape. </Tip>
! Default = -
! Allowed = char (20)
! Active if LABEL_TYPE = DOMESTIC
!</Help>
!
!<Help KEYWORD="COUNTRY">
!<Tip> Country field to identify output tape. </Tip>
! Default = -
! Allowed = char (20)
! Active if LABEL_TYPE = INTERNATIONAL
!</Help>
!
!<Help KEYWORD="PROJECT_NAME">
!<Tip> Project field to identify output tape. </Tip>
! Default = Project_Data information
! Allowed = char (20)
! The typical entry is the current project name, but any identification text
! is allowed.
! Active if LABEL_TYPE = INTERNATIONAL
!</Help>
!
!<Help KEYWORD="DIR">
!<Tip> Direction of line. </Tip>
! Default = -
! Allowed = char (4)
!</Help>
!
!<Help KEYWORD="HDR_LINE">
!<Tip> Header word to identify line number. </Tip>
! Default = 38
! Allowed = 0 - NWIH
! If zero, must fill out LINE_NAMES
!</Help>
!
!<Help KEYWORD="HDR_SHOT">
!<Tip> Header word to identify shot numbers. </Tip>
! Default = 37
! Allowed = 0 - NWIH
! If zero, must fill out SHOTPOINTS
!</Help>
!
!<Help KEYWORD="LINE_NAMES">
!<Tip> Name of lines on tape. </Tip>
! Default = -
! Allowed = char (20)
! Active if HDR_LINE = 0.
!</Help>
!
!<Help KEYWORD="SHOTPOINTS">
!<Tip> Shot point ranges on tape. </Tip>
! Default = -
! Allowed = char (20)
! Active if HDR_SHOT = 0.
!</Help>
!
!<Help KEYWORD="CONTRACTOR">
!<Tip> Contractor who shot the data. </Tip>
! Default = -
! Allowed = char (20)
!</Help>
!
!<Help KEYWORD="PROCESSES">
!<Tip> Processes field to identify output tape. </Tip>
! Default = -
! Allowed = char (20)
! The typical entry is one or more CPS process names, but any identification
! text is allowed.
!</Help>
!
!<Help KEYWORD="COMMENT_LABEL">
!<Tip> Comment for tape label. </Tip>
! Default = -
! Allowed = char (16)
! Any text information as a comment.
!</Help>
!
!      tabgroup = Main Tab
!
!<Help KEYWORD="TAPE_HOST">
!<Tip>  User can choose a specific tape host. </Tip>
! Default = odi90
! Allowed = odi90
! Allowed = odi74
! Allowed = odi91
! Allowed = hoeplgp01
! Allowed = hnomega1  (goodgkp only)
! Allowed = jaws
! User can choose a specific tape host
!</Help>

!
!</HelpSection>
!-------------------------------------------------------------------------------
!
! Begin Fortran here.
!
!
!-----------------------------------------------------------------------
!\END DOC
!\PROG DOC
!-----------------------------------------------------------------------
!                      REVISION HISTORY
!     Date     Author  Description
!     ----     ------  -----------
! 54. 98/06/25 Goodger Replace the ttrot_mounto calls with calls to 
!                      mounto2.  Remove the mounto subroutines from
!                      this program.  New mounto2 has an argument for
!                      data_typ.  
!                      Begin compiling with fortran90.
! 53. 98/06/16 Goodger Make work with TCAT.  Write out buffer before
!                      switching pdn's.  
! 52. 98/05/07 Goodger Add subroutine TTROT_MOUNTO for BLK format.
! 51. 98/03/10 Goodger Add blocked data type.
! 50. 97/11/11 Goodger Increase the allowable number of processing 
!                      records from 100 to 200.
! 49. 97/04/21 Goodger Add MAGstar tape drives.
! 48. 96/06/18 cooper  Increase LPARM to fix a re-entrant problem.
! 47. 96/06/18 Cooper  Make IRT blank filled
! 46. 96/04/30 Goodger Add parameter PRFILE. Moved revision history
!                      older than 1992 out of DOC.
! 45. 95/11/26 Goodger Convert keeptime to MSCI writetape format.
! 44. 95/11/15 Kopesky Commented out ISHELL(wrt.n).
! 43. 95/04/17 Troutt  Fix PUTP call (LPARM) for increased # processing
!                      files.
! 42. 95/04/12 Cooper  Increase number of processing files from 10 to 50
! 41. 95/01/10 Goodger Update documentation to reflect front end.
! 40. 94/12/12 Day     Altererd error message for tape error to reflect
!                      true trace number where error occured.
! 39. 93/09/14 Troutt   Change ISHELL command from whb.n to wrt.n. This
!                       is a script to log unrecovered erros in BUFFER
!                       OUT.  It was sending mail to Ball's account.  It
!                       now sends it to mine.
! 38. 92/05/26 Peterson Replace COS CALL ACTTABLE with CALL INITINFO
!                      to get JOBNAME and USERID from /BHISTC2/.
!                      Also sent JOBNAME to subroutine ttrotpr.
! 37. 92/03/03 Ball    Change default from TAPE  to CART.
!-----------------------------------------------------------------------
!                           CALLING SEQUENCE
!
!        CALL TTROTS(IPN,ISAVE,*ERR)       Set up.
!        CALL TTROT(IPN,N,HD,TR,LTR,*ERR)  Process traces.
!
!  IPN     - Process number.
!  ISAVE   - Not zero is flag to save parameters.
!  *ERR    - Label for error return.
!  N       - Number of traces to proccess this call. Zero for end of
!            traces. The value of n is not changed by this process.
!  HD      - Header values.
!  TR      - Trace values.
!  LTR     - Length per trace in memory.
!_______________________________________________________________________
!                                 NOTES
!
! 1. Some CPS routines like TCAT use the TTROT named common block
!    /TTROT1/ . Thus, TCAT is very sensitive to changes in TTROT1.
!-----------------------------------------------------------------------
! NAMED COMMON BLOCKS: GLOBALS,TTROT1 and TTROT2 .  CRAYSYS
! EXTRA ENTRY POINTS : TTROSW
!-----------------------------------------------------------------------
! EXTERNAL REFERENCES: GETP(CPS),   PUTP(CPS),  REPPI(CPS),  DMOUNT(CPS),
!                      MOUNTO2(CPS), WHIST(CPS), USSCTI(SCILIB), RDEOF,
!                      DCODES,  DCODES,  WHISTS,
!                      GETS(CPS),   INITINFO(CPS), 
!                      CPSPRT*(CPS),  BUFFER OUT(SCILIB), UNIT(SCILIB)
!-----------------------------------------------------------------------
!\END DOC
!     ***************  OLD REVISION HISTORY  ****************
!CC
! 36. 91/12/16 Troutt  Add input parameter DVICE in order to specify
!                      TAPE or CARTridge output. This is passed to
!                      MOUNTO by new 8th argument.
! 35. 91/10/24 Ball    Abort on BUFFER OUT error of 2 and send
!                      mail message to pwhbers.
! 34. 91/10/21 Ball    Error check BUFFER OUT and print message.
! 33. 91/09/17 Ball    Write Process Records on 1st tape of every PDN
!                      in a TTROT.  Not just the 1st PDN.
! 32. 91/09/10 Howard  Fix bug--TTROT with SELECT not reenterable.
! 31. 91/07/15 Ball    Change error message at label 92
! 30. 91/06/14 Howard  Change IONUM to a DIN for UNICOS rls.
! 29. 91/02/13 Peterson UNICOS upgrades. ISHELL('rm FILE') for RELEASE
! 28. 90/03/21 Ball    Correct Bug if Select Option skips 1st trace
! 27. 89/10/26 Day     Added processing record ability. TTROTPR,>storage
! 26. 89/01/23 Day     Altered dismount logic and output
! 25. 89/01/17 Day     Fixed mising trace problem for m-vol overflow
! 24. 88/12/08 Day     Fixed re-entrant problem
! 23. 88/12/05 Day     Cured problem with ISP output.(nwih-->nhdwdo)
!                      Added select option. Eliminate $ in dcodes.
!                      Support PDN oriented IO.
! 22. 88/09/26 Day     Variable header length, STATUS parameter
!                      added.
! 21. 88/07/14 Day     Allowed more than 100 output reels.
! 20. 88/06/15 Sinton  Modified to be consistant with
! 19. 88/06/15 Sinton  BHIST version,Added WHISTS call in TTROTS and
!                      modified call to WHIST.
! 18. 88/04/23 Baumel  Added CPSPRT calls.
! 17. 88/04/18 Day     TTROT1 altered for better TCAT control
! 16. 88/02/01 Sinton  Changed WHIST subroutine to correct history
! 15. 87/12/04 Day     Upped scratch for large historys
! 14. 87/11/05 Day     Alterered storage and packing slightly
! 13. 87/11/03 Day     TTROT made reentrant. More than 1 drive
!                      may be called in a loop.TCAT,BHIST,MOUNTO
!                      also altered. Variable unit numbers.
! 12. 87/10/20 Day     Tape access atered for COS116
! 11. 87/09/14 Day     Altered mounto call(irt added)
! 10. 87/09/04 Day     Major overhaul to mounto and ttrot
! 9.  87/08/30 Day     Does not DEFAULT to SCRATCH output reels.
! 8.  87/08/06 Day     Modified access of existing catalogued tapes.
! 7.  87/08/02 Day     Increased # of out drives to 26.
! 6.  87/05/07 Day     Rewind statment was eliminated
! 5.  87/04/01 Day     Corrected print out so proper PDN is displayed
! 4.  87/02/17 Day     Last word of packing-buffer zeroed.
! 3.  87/01/29 Day     Problem with pointers and history files corrected
! 2.  86/08/12 Day     Corrected PDN bug when NUM$REEL was < 0
! 1.  86/08/01 Day     First Version
! *********************************************************************
!
      module ttrot_module
      use array_module
      use cnfg_module
      use cio_module
      use getlun_module
      use getsys_module
      use grid_module         
      use hist_module
      use lav_module
      use manhist_module
      use mem_module
      use mutehw_module
      use named_constants_module
      use pathcheck_module
      use pc_module
      use putsys_module
      use segy_module
      use sizeof_module
      use string_module
      use swap_module
      use tcatclient_module
      use unix_module
      use wrdc_module
      implicit none

      private


character(len=100),public,save :: ttrot_ident = &
  '$Id: ttrot.f90,v 1.68 2007/07/11 14:07:24 Menger beta sps $'
      public :: ttrot_create     ! uses the parameter cache.
      public :: ttrot_initialize
      public :: ttrot_update     ! uses the parameter cache.
      public :: ttrot_delete
      public :: ttrot            ! main execution (trace processing) routine.
      public :: ttrot_wrapup
      public :: ttrot_change_reel
      private:: ttrot_open
      private:: ttrot_wr_predata
      private:: ttrot_wrheader
      private:: ttrot_wrhist
      private:: ttrot_wrprec
      private:: ttrot_dmnt       !writes eofs, rewinds, offlines, closes
      private:: ttrot_pak_hdtr
      private:: ttrot_wr_block
      private:: ttrot_flush
      private:: ttrot_catalogue_add
      private:: ttrot_iscat
      private:: ttrot_getpdn
      private:: ttrot_next_vol
      private:: ttrot_print
      private:: ttrot_stdo
      private:: ttrot_get_vol
      private:: ttrot_build_pdns

      type,public :: ttrot_struct

        private
        logical      :: skip_wrapup    !ttrot is no-op when true
        logical      :: fatal_error   !bad bad error detected
        integer      :: ipn
        integer      :: stdo
        integer      :: fd            !file descriptor for tape drive
        integer      :: rank          !for parallel environments
        character(len=8)  :: survey_units
        character(len=16) :: jobname  !name of the cps job(from init)
        character(len=8)  :: userid   !can be changed
        character(len=8)  :: trueuser !user running this instance
        character(len=10) :: project
        character(len=10) :: sub_project
        character(len=10) :: task
        character(len=120):: comment
        character(len=160):: frontend_path
        character(len=16) :: frontend_user
        character(len=8)  :: tape_server
        character(len=12) :: tape_host
        integer      :: nreel   !total volume count
        integer      :: oreel   !current volume number
        integer      :: nvols   !volser count for current volume
        integer      :: ivol    !current volser of current volume
        character(len=8)  :: volser  !current volser tape label
        character(len=6)  :: vols(50)!volser list for volume
        character(len=8)  :: format
        character(len=8)  :: media
        character(len=8)  :: history
        character(len=8)  :: mod_segy !custom segy header mapping
        character(len=8)  :: pack_hdr !YES/NO 32/64 bit
        character(len=8)  :: type_out
        character(len=16) :: lname
        integer      :: hdr_flag   !header to select traces
        integer      :: hdr_sum1   !report min-max of this header
        integer      :: hdr_sum2   !report min-max of this header
        integer      :: hdr_switch !header to force reel changes
        real         :: beg_init   !first bin center
        real         :: beg_inc    !bin width

        integer      :: retention  !retention time

            !name of file with proc records
        character(len=FILENAME_LENGTH) :: path_ps 
        integer           :: max_tr
        character(len=16) :: label_type,comment_label
        character(len=20) :: quad,country,project_name,line_names,shotpoints
        character(len=20) :: contractor,processes
        character(len=4)  :: dir
        integer           :: hdr_line,hdr_shot,knttr
        real              :: shotmin,shotmax,linemin,linemax,time

            !name of file with segy header
        character(len=FILENAME_LENGTH) :: path_segy 
        real         :: tim_max    !max time to output
        integer      :: nsout      !No. samples to output
        integer      :: ndead   !no of dead traces
        integer      :: nseqo   !Cummulative traces output,all volumes.
        integer      :: mntswo  !Mount switch controlling tape mounts.
        integer      :: volcnt  !trace number ready for output(volume)
        integer      :: vscnt   !volser trace count
        integer      :: ndx     !used with BLK option
        integer      :: nleft   !used with BLK option
        integer      :: bytes_per_trace ! bytes in a packed header trace
        integer      :: max_per_vol ! max traces per volser
        integer      :: max_per_vol_sav ! max traces per volser
        integer      :: nelhdtr ! No. of elements in a header-trace
        integer      :: paksiz
        integer      :: blksiz
        integer      :: tpr             !output traces per record
        integer      :: nib             !number traces in buffer
        integer      :: nbits           !trace precision
        integer      :: nbits_hd        !header precision
        integer      :: nbytesnword
        integer      :: endian          !1=sun, 0=intel
        character(len=4)   :: wdtype    !output word type
        real,     pointer  :: pak(:)    !n*(hd+tr) after packing
        real,     pointer  :: blkbuf(:) !n*(hd+tr) before packing
        integer,  pointer  :: num_tr(:) !max traces to output to a vol
        character,pointer  :: volume(:)*48
        character,pointer  :: stat(:)*8
        character(len=8)  :: defaults
        character(len=8)  :: fill_rows
        integer    :: num_rows  !number of rows to fill in volume array
        integer    :: nummap         !number of segy headers to map
        integer    :: sbyte(64)      !byte location of segy header,from 1
        integer    :: bytes(64)      !number of bytes in segy header
        integer    :: cps_hdr(64)      !map from this cps header word
        character(len=8)  :: wtype(64) !word type of the segy header
        logical    :: firsttrace
        integer    :: npr
        integer    :: ndpt
        integer    :: nwih
        real       :: tstrt
        real       :: dt
        type(grid_struct) :: grid
!
        character(len=12) :: location,rlocation
        integer           :: host_noptions
        character :: host_options(5)*12

      end type ttrot_struct

      integer :: plun ! global unit for print statements

      type(ttrot_struct),pointer,save :: object      ! needed for traps.
      logical,private,save           :: ttrot_first_call=.true.
      integer,private,save           :: stdo
      integer,private,save           :: ttrot_server_cnt
      character(len=24),private,save :: ttrot_servers(11)

      contains
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
      subroutine ttrot_create (obj)
      implicit none
      type(ttrot_struct),pointer :: obj       ! arguments
 
      allocate (obj)
 
      nullify  (obj%pak)
      nullify  (obj%blkbuf)
      nullify  (obj%num_tr)
      nullify  (obj%volume)
      nullify  (obj%stat)
!
      ttrot_server_cnt = ttrot_get_hosts() 
      call ttrot_initialize (obj)

!
      return
      end subroutine ttrot_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
      subroutine ttrot_delete (obj)
      implicit none
      type(ttrot_struct),pointer :: obj       ! arguments
      integer i_err
 
      if (.not. associated(obj)) return
      call ttrot_wrapup (obj)
 
      if (associated(obj%pak    )) deallocate (obj%pak)
      if (associated(obj%blkbuf )) deallocate (obj%blkbuf)
      if (associated(obj%num_tr )) deallocate (obj%num_tr)
      if (associated(obj%volume )) deallocate (obj%volume)
      if (associated(obj%stat   )) deallocate (obj%stat)
      if(associated(obj)) then
        deallocate(obj,stat=i_err)
        if(i_err /= 0) then
          call pc_info ('ttrot_delete: deallocation error')
        endif
      endif
      nullify(object)
 
      return
      end subroutine ttrot_delete
 
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
      subroutine ttrot_wrapup(obj)
      implicit none
      type(ttrot_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.
!
! finish last reel if there is one mounted
      write(obj%stdo,*) 'TTROT_WRAPUP:'
      call ttrot_dmnt(obj)
      return
      end subroutine ttrot_wrapup
!--
      subroutine ttrot_dmnt(obj)
      type(ttrot_struct),intent(inout) :: obj       ! arguments
      integer            :: neof
      integer            :: tpioclnt_weof, tpioclnt_close, tpioclnt_prnt
      integer            :: status,i_err
      integer    :: ilname(4)
      integer    :: ivolser(4)

      if(obj%fd > 0) then
        write(obj%stdo,*) 'TTROT_DMNT: volser=',obj%volser
        call ttrot_flush(obj)   !force buffered data to tape,noop unless blkd
        if(obj%num_tr(obj%oreel) < 1) then
          write(obj%stdo,*) 'TTROT_DMNT: warning, no traces on volume!'
        endif
        ! print summary for volser from tapeio primitive
        status =  tpioclnt_prnt(obj%fd)
        ! print ttrot running summary
        call ttrot_print(obj)
        ! write eofs to end data
        neof=2
        status = tpioclnt_weof(obj%fd,neof)  !write double eof
        if(status /= 2)  then
          write(obj%stdo,*) 'TTROT_DMNT: WARNING, DOUBLE EOF WRITE FAILED!'
          write(obj%stdo,*) 'TTROT_DMNT: WARNING, FD=',obj%fd
        endif
        ! add the volser to the catalogue
        call ttrot_catalogue_add(obj,obj%volser,i_err)
      endif
      ! clean up the server
      if(obj%fd <0 .and. obj%lname==' ')  then
        obj%volser = ' '
        return
      endif
      ilname = 0
      ivolser= 0
      call string_cc2hh(obj%lname,ilname)
      call string_cc2hh(obj%volser,ivolser)
      write(obj%stdo,*) 'TTROT_DMNT:',obj%lname,obj%volser
      i_err = tpioclnt_close(obj%fd,ilname,ivolser) !rewinds, offloads, closes
      if(obj%fd < 0) then
        obj%volser = ' '
        obj%lname = ' '
      endif
      return
      end subroutine ttrot_dmnt


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
      subroutine ttrot_initialize (obj)
      implicit none
      type(ttrot_struct),intent(inout) :: obj       ! arguments

      integer :: nbitsnword

 
!-- Initialize ALL NON-POINTER VARIABLES in your parameter structure
!-- as follows:
!--
      obj%survey_units = 'METERS'
      obj%jobname = ' '
      obj%userid  = ' '
      obj%frontend_user = ' '
      obj%tape_server   = 'PROD'
      call getsys_username(obj%trueuser)
      call cnfg_get_value('location',obj%location)
      call pc_get_jdata('RLOCATION',obj%rlocation)
      if(obj%location.eq.'alaska'.or.obj%rlocation.eq.'ALASKA')then
        obj%host_noptions=1
        obj%host_options(1)='jaws'
      else if (obj%trueuser.ne.'goodgkp'.and.obj%trueuser.ne.'sps')then
        obj%host_noptions=4
        obj%host_options(1)='odi90'
        obj%host_options(2)='odi74'
        obj%host_options(3)='hoeplgp01'
        obj%host_options(4)='odi91'
      else 
        obj%host_noptions=5
        obj%host_options(1)='odi90'
        obj%host_options(2)='odi74'
        obj%host_options(3)='hoeplgp01'
        obj%host_options(4)='hnomega1'
        obj%host_options(5)='odi91'
      endif
      obj%tape_host     = obj%host_options(1)

      obj%fatal_error=.false.
      obj%skip_wrapup =.true.
      obj%rank     = 0
      obj%ipn      = -1
      obj%stdo     = 6
      obj%format   = 'CPS'
      obj%type_out = 'SCR'
      obj%comment  = ' '
      obj%media    = '3590HD'
      obj%history  = 'MODEL'
      obj%mod_segy = 'NO'
      obj%pack_hdr = 'YES'
      obj%lname    = ' '
      obj%project  = ' '    !get from job
      obj%sub_project = ' '   !get from job
      obj%task     = ' '  
      obj%retention= 180
      obj%nsout    = 0

      obj%path_ps  = PATHCHECK_EMPTY 
      obj%max_tr   = 0
      obj%label_type='INTERNATIONAL'
      obj%quad=' '
      obj%country=' '
      call pc_get_pdata  ('project'     , obj%project_name)
      call string_squeeze_blanks(obj%project_name)
      obj%dir=' '
      obj%hdr_line=8
      obj%hdr_shot=7
      call pc_get_pdata  ('sub_project'     , obj%line_names)
      call string_squeeze_blanks(obj%line_names)
      obj%line_names=' '
      obj%shotpoints=' '
      obj%contractor=' '
      obj%processes=' '
      obj%comment_label=' '
      obj%shotmin=2**30
      obj%shotmax=-2**30
      obj%linemin=2**30
      obj%linemax=-2**30
      obj%knttr=0

      obj%path_segy= PATHCHECK_EMPTY 
      obj%npr      = 0

      obj%hdr_switch= 0
      obj%beg_init = 1.0
      obj%beg_inc  = 1.0

      obj%hdr_flag = 0
      obj%hdr_sum1 = 0
      obj%hdr_sum2 = 0

      obj%defaults= 'SYS'
      obj%fill_rows = 'YES'
      obj%num_rows= 1
      obj%volser  = ' '
      obj%vols    = ' '

      obj%nummap = 0 !number of segy headers to special map
      obj%sbyte  = 0 !starting byte location of segy header
      obj%bytes  = 0 !number of bytes in segy header
      obj%cps_hdr= 0 !map to this cps header word
      obj%wtype  = ' '!map to this cps header word

      obj%fd       =-1
      obj%nreel    = 0
      obj%oreel    = 0
      obj%nvols    = 0
      obj%ivol     = 0
      obj%ndead    = 0
      obj%nseqo    = 0
      obj%vscnt    = 0
      obj%max_per_vol= 1000000
      obj%bytes_per_trace= 10000 + 256
      obj%mntswo   = 0  !force immediate mount when 1st trace is read
      obj%volcnt   = 0
      obj%nbits    = 32
      obj%nbits_hd = 32
      obj%wdtype   = 'IEEE'
      obj%endian   = 1

      obj%tpr      = 1  !tpr format, traces per record
      obj%nib      = 0  !tpr format, number in buffer
      obj%blksiz   = 30000
      obj%paksiz   = 30000
      obj%ndx      = 1
      obj%nleft    = obj%blksiz

      obj%ndpt     = 0  ! will have to test later to make sure has been reset.
      obj%dt       = 0.0
      obj%tstrt    = 0.0
      obj%nwih     = 0

      obj%firsttrace=.true.

      plun=pc_get_lun()
      call pc_get_jdata('TRACE_LENGTH',obj%time)

!     call grid_initialize(obj%grid)

      nbitsnword=bit_size(nbitsnword)
      obj%nbytesnword=nbitsnword/8
!--
!-- If a process parameter has a default which is calculated from a global
!-- parameter, it can be set from the parameter cache
!--
      call ttrot_update (obj)
 
      return
      end subroutine ttrot_initialize
!--
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
 
      subroutine ttrot_update (obj)
      implicit none
      type(ttrot_struct),intent(inout),target :: obj             ! arguments
!
! COMBO BOX SETTINGS
      integer,parameter :: format_noptions=4
      character :: format_options(format_noptions)*8
      data format_options/'CPS','SEGY','BLK','TPR'/
      integer,parameter :: media_noptions=9
      character :: media_options(media_noptions)*8
      data media_options/'3590HD','3590','3480','8MM','DLT','3420','NR3590',&
                         &'LTO','3592'/
      integer,parameter :: history_noptions=4
      character :: history_options(history_noptions)*8
      data history_options/'MODEL','ALL','CURRENT','NONE'/
      integer,parameter :: mod_segy_noptions=2
      character :: mod_segy_options(mod_segy_noptions)*8
      data mod_segy_options/'NO','YES'/
      integer,parameter :: pack_hdr_noptions=2
      character :: pack_hdr_options(pack_hdr_noptions)*8
      data pack_hdr_options/'NO','YES'/
      integer,parameter :: fill_noptions=2
      character :: fill_options(fill_noptions)*8
      data fill_options/'NO','YES'/
      integer,parameter :: default_noptions=2
      character :: default_options(default_noptions)*8
      data default_options/'OLD','SYS'/
      integer,parameter :: typo_noptions=3
      character(len=8) :: typo_options(typo_noptions)
      data typo_options/'SCR','PS','PS_VMOD'/

      integer    ,parameter :: label_type_noptions = 2
      character(len=16)     :: label_type_options(label_type_noptions)   &
                               = (/ 'INTERNATIONAL   ','DOMESTIC        ' /)

 
!-- Declare all required local variables as follows:
!--

      integer     :: i,i_err,i1,i2,j1,j2,k,knt,lun1,lun2,nr1,nr2,nr3      
      integer     :: gigs_per_tape,traces_per_gig,max_per_vol
      integer     :: istat
      logical     :: foundhgrid,foundmodgrid
      character(len=8)  :: ctmp
      character(len=80) :: card
      character(len=132):: filenames_file_for_ttrot
      character(len=64) :: host
      character(len=160):: lcard,lcard2,msg
      character(len=160),pointer :: lcards(:)
!
      nullify (lcards) ! jpa
!
      object => obj               ! needed for traps.
      obj%skip_wrapup =.true.
      obj%fatal_error= .false.

      call pc_register_array_names ("volume_arrayset", (/  &
                                    "volume",              &
                                    "num_tr" /))

      call pc_register_array_names ("sbyte_arrayset", (/  &
                                    "sbyte  ",            &
                                    "bytes  ",            &
                                    "cps_hdr",            &
                                    "wtype  " /))

! Get globals, project and job data
      obj%ipn = pc_get_ipn()
      obj%stdo= pc_get_lun()

      call pc_get_global ('tstrt', obj%tstrt)
      call pc_get_global ('dt'   , obj%dt)
      call pc_get_global ('ndpt' , obj%ndpt)
      call pc_get_global ('nwih' , obj%nwih)
      call pc_get_global ('grid' , obj%grid)

! tape volume arrays
      nr1 = obj%nreel 
      nr2 = obj%nreel 
      call pc_alloc('volume'  , obj%volume, obj%nreel ,&
       ttrot_volume_element_trap)
      call pc_alloc('num_tr'   ,obj%num_tr, nr1)
      if (obj%nreel  /= nr1) then
         call pc_error ('volume arrays have different lengths')
         obj%nreel  = min(nr1,nr2,obj%nreel )
      end if
      if(nr1.eq.0)obj%num_tr=0
      do i = 1,obj%nreel 
        if(obj%num_tr(i)<1) obj%num_tr(i)=99999999
      enddo
      call pc_get('fill_rows',obj%fill_rows, ttrot_fill_rows_trap)
      call pc_get('num_rows' ,obj%num_rows, ttrot_num_rows_trap)
      call pc_get_pdata  ('survey_units', obj%survey_units)
      call pc_get('userid',obj%userid, ttrot_userid_trap)
      call pc_get_jdata  ('frontend_user' , obj%frontend_user)
      call pc_get_jdata  ('frontend_path' , obj%frontend_path)
      call pc_get_jdata  ('tape_server'   , obj%tape_server)
      call pc_get('project',obj%project,ttrot_project_trap)
      call pc_get_jdata  ('jobname'     , obj%jobname)
      if(mod(obj%nwih,2)/= 0) then
        call pc_error('TTROT: CAN NOT HANDLE AN ODD NO.&
        & OF HEADER WORDS!')
        obj%fatal_error=.true.
      endif
      if(obj%tape_server(1:1) == 'p') obj%tape_server='PROD'
      if(obj%tape_server(1:1) == 'P') obj%tape_server='PROD'
      if(obj%tape_server(1:1) == 'b') obj%tape_server='BETA'
      if(obj%tape_server(1:1) == 'B') obj%tape_server='BETA'
!
 
! variables for SEGY header mapping
      nr1 = obj%nummap
      nr2 = obj%nummap
      nr3 = obj%nummap
      call pc_get('sbyte'  ,obj%sbyte   ,obj%nummap)
      call pc_get('bytes'  ,obj%bytes   ,nr1)
      call pc_get('cps_hdr',obj%cps_hdr ,nr2)
      call pc_get('wtype'  ,obj%wtype   ,nr3)
      if(nr1 /= obj%nummap .or. nr2 /= obj%nummap .or. nr3 /=obj%nummap) then
        call pc_error ('TTROT_UPDATE: SBYTE,BYTES... arrays&
       & have different lengths')
        obj%nummap = min(obj%nummap,nr1,nr2,nr3)
      endif
      call pc_get('mod_segy' ,obj%mod_segy, ttrot_mod_segy_trap)
      call pc_put('sbyte'  ,obj%sbyte   ,obj%nummap)
      call pc_put('bytes'  ,obj%bytes   ,obj%nummap)
      call pc_put('cps_hdr',obj%cps_hdr ,obj%nummap)
      call pc_put('wtype'  ,obj%wtype   ,obj%nummap)
!
! Front end parameters
      call pc_get('sub_project',obj%sub_project,ttrot_sub_project_trap)
      call pc_get('comment'  ,obj%comment)
      call pc_get('format'   ,obj%format,ttrot_format_trap)
      obj%tim_max = obj%tstrt + (obj%ndpt-1)*obj%dt
      obj%nsout   = 1 + nint((obj%tim_max - obj%tstrt)/obj%dt)

      call pc_get('pack_hdr' ,obj%pack_hdr)
      if(obj%format=='CPS' .or. obj%format=='TPR') then
        obj%bytes_per_trace = 4*obj%nsout + 8*obj%nwih
        if(obj%pack_hdr=='YES') &
           obj%bytes_per_trace = 4*obj%nsout + 4*obj%nwih
      else if(obj%format=='SEGY') then
        obj%bytes_per_trace = 4*obj%nsout + 240
      else
        if(obj%pack_hdr=='YES') &
           obj%bytes_per_trace = 4*obj%nsout + 4*obj%nwih
      endif
      call pc_get('media'    ,obj%media, ttrot_media_trap)
      traces_per_gig = 1073741824/obj%bytes_per_trace
      if(obj%media.eq.'3590HD')then
        gigs_per_tape=20
      else if(obj%media.eq.'3592')then
        gigs_per_tape=300
      else if(obj%media.eq.'LTO')then
        gigs_per_tape=100
      else
        gigs_per_tape=10
      endif

      max_per_vol = 0.95*gigs_per_tape*traces_per_gig
      if(max_per_vol /= obj%max_per_vol) then
       write(card,'(''TTROT: max traces on one volser='',I8)') max_per_vol
       call pc_info(card)
      endif
      obj%max_per_vol = max_per_vol
      obj%max_per_vol_sav = max_per_vol


      call pc_get('history'  ,obj%history)
      call pc_get('defaults' ,obj%defaults)
      call pc_put_sensitive_field_flag ('defaults', .false.)
      call pc_get('type_out' ,obj%type_out,ttrot_type_out_trap)

      call pc_get('path_ps'  ,obj%path_ps,ttrot_path_ps_trap)
      call pc_get('max_tr'   ,obj%max_tr,ttrot_max_tr_trap)
      call pc_get('label_type'  ,obj%label_type,ttrot_label_type_trap)
      call pc_get('quad'  ,obj%quad,ttrot_quad_trap)
      call pc_get('country'  ,obj%country,ttrot_country_trap)
      call pc_get('project_name'  ,obj%project_name,ttrot_project_name_trap)
      call pc_get('dir'  ,obj%dir,ttrot_dir_trap)
      call pc_get('hdr_line'  ,obj%hdr_line,ttrot_hdr_line_trap)
      call pc_get('hdr_shot'  ,obj%hdr_shot,ttrot_hdr_shot_trap) 
      call pc_get('line_names'  ,obj%line_names,ttrot_line_names_trap)
      call pc_get('shotpoints'  ,obj%shotpoints,ttrot_shotpoints_trap)
      call pc_get('contractor'  ,obj%contractor,ttrot_contractor_trap)
      call pc_get('processes'  ,obj%processes,ttrot_processes_trap)
      call pc_get('comment_label'  ,obj%comment_label,ttrot_comment_label_trap)
      

      call pc_get('path_segy',obj%path_segy,ttrot_path_segy_trap)
      call pc_get('retention',obj%retention)
      call pc_get('task'     ,obj%task,ttrot_task_trap)
      call pc_get('hdr_switch',obj%hdr_switch)
      call pc_get('beg_init' ,obj%beg_init)
      call pc_get('beg_inc'  ,obj%beg_inc)
      call pc_get('hdr_flag' ,obj%hdr_flag)
      call pc_get('hdr_sum1' ,obj%hdr_sum1)
      call pc_get('hdr_sum2' ,obj%hdr_sum2)
      call pc_get('tape_host' , obj%tape_host,ttrot_tape_host_trap)
      if(obj%num_rows <0  ) obj%num_rows=0
      if(obj%num_rows >100) obj%num_rows=100
!!      if(obj%num_rows >obj%nreel  .and. obj%fill_rows(1:3)=='YES') then
!!          call ttrot_build_pdn(obj)
!!      endif
      if(obj%hdr_switch<0 .or. obj%hdr_switch>obj%nwih) obj%hdr_switch=0
      if(obj%hdr_sum1<0) obj%hdr_sum1=0
      if(obj%hdr_sum1>obj%nwih) obj%hdr_sum1=0
      if(obj%hdr_sum2>obj%nwih) obj%hdr_sum2=0
      if(obj%hdr_flag<0 .or. obj%hdr_flag>obj%nwih) obj%hdr_flag=0

      if(obj%retention<1) obj%retention=1
      if(obj%retention>365) obj%retention=365

!       Routine ttrot_get_prfile is no longer needed.  The file names are
!         read directly from the path_ps file in routine ttrot_wrprec
!         Revision 22
!!      if(obj%path_ps /= PATHCHECK_EMPTY)then
!!       write(card,'(''path_ps='',A)') trim(obj%path_ps)
!!       call pc_info(card)
!!        call ttrot_get_prfile(obj%stdo,obj%path_ps,obj%npr,obj%prname,i_err)
!!      endif
!     obj%paksiz = obj%nsout+obj%nwih
!     if(obj&format(1:3)=='BLK') obj%paksiz = obj%blksiz
      if(.not.associated(obj%blkbuf)) then
        allocate(obj%blkbuf(obj%blksiz),stat=i_err) 
      endif
      if(.not.associated(obj%pak)) then
        allocate(obj%pak(obj%paksiz),stat=i_err) 
        print *,'SIZE OF pak = ',size(obj%pak)
      endif
!
      if(obj%format(1:4)=='SEGY') then
        obj%wdtype='IBM'
        obj%nbits=32
        obj%nbits_hd=32
      endif
      if(obj%format(1:3)=='CPS' .or. obj%format(1:3)=='BLK') then
        obj%wdtype='IEEE'
        obj%nbits=32
        obj%nbits_hd=32
        if(obj%pack_hdr(1:2)=='NO') obj%nbits_hd=64
      endif
      if(obj%format(1:3)=='TPR') then !traces per record mode
        obj%wdtype='IEEE'
        obj%nbits=32
        obj%nbits_hd=32
        if(obj%pack_hdr(1:2)=='NO') obj%nbits_hd=64
        ! NOTE: max buffer size for socket may vary by system!!!!
        obj%tpr = 41600/obj%bytes_per_trace
        write(obj%stdo,*) 'TTROT: Traces Per Record, TPR=',obj%tpr
      endif
      if(obj%nbits /= 32) obj%nbits=32
      if(obj%nbits_hd /= 32) obj%nbits_hd=64
      if(obj%wdtype == 'IBM') then
        obj%nbits=32
        obj%nbits_hd=32
      endif
      call pc_call_arrayset_trap('SBYTE_ARRAYSET',ttrot_sbyte_arrayset_trap)

      call pc_call_end_trap(ttrot_end_trap)

      if (obj%fill_rows == 'YES') then
        call pc_put_sensitive_array_flag('volume', .false.)
      else
        call pc_put_sensitive_array_flag('volume', .true. )
      endif
!
! SET THE COMBO BOX OPTIONS
      call pc_put_options_field('format'  , format_options, format_noptions)
      call pc_put_options_field('media'   , media_options, media_noptions)
      call pc_put_options_field('history' , history_options, history_noptions)
      call pc_put_options_field('mod_segy', mod_segy_options,mod_segy_noptions)
      call pc_put_options_field('pack_hdr', pack_hdr_options,pack_hdr_noptions)
      call pc_put_options_field('defaults', default_options,default_noptions)
      call pc_put_options_field('fill_rows', fill_options, fill_noptions)
      call pc_put_options_field('type_out', typo_options, typo_noptions)
      call pc_put_options_field('label_type',label_type_options,&
                                 label_type_noptions)
      call pc_put_options_field ('tape_host',obj%host_options,obj%host_noptions)
!!      if(ttrot_server_cnt>0) then
!!        call pc_put_options_field ('tape_host' , ttrot_servers,&
!!        ttrot_server_cnt)
!!      else
!!        call pc_put_options_field ('tape_host',obj%host_options,&
!!                                    obj%host_noptions)
!!      endif
!
! SET THE VALUES IN THE PARAMETER CACHE
      call pc_put('volume',obj%volume ,obj%nreel )
      call pc_put('num_tr',obj%num_tr ,obj%nreel )
      call pc_put('comment'  ,obj%comment)
      call pc_put('format'   ,obj%format)
      call pc_put('media'    ,obj%media)
      call pc_put('history'  ,obj%history)
      call pc_put('mod_segy' ,obj%mod_segy)
      call pc_put('pack_hdr' ,obj%pack_hdr)
      call pc_put('userid'   ,obj%userid)
      call pc_put('project'  ,obj%project)
      call pc_put('sub_project',obj%sub_project)
      call pc_put('task'     ,obj%task)
      call pc_put('defaults' ,obj%defaults)
      call pc_put('fill_rows',obj%fill_rows)
      call pc_put('num_rows' ,obj%num_rows)

      call pc_put('path_ps'  ,obj%path_ps)
      call pc_put('max_tr'   ,obj%max_tr)
      call pc_put('label_type'  ,obj%label_type)
      call pc_put('quad'  ,obj%quad)
      call pc_put('country'  ,obj%country)
      call pc_put('project_name'  ,obj%project_name)
      call pc_put('dir'  ,obj%dir)
      call pc_put('hdr_line'  ,obj%hdr_line)
      call pc_put('hdr_shot'  ,obj%hdr_shot) 
      call pc_put('line_names'  ,obj%line_names)
      call pc_put('shotpoints'  ,obj%shotpoints)
      call pc_put('contractor'  ,obj%contractor)
      call pc_put('processes'  ,obj%processes)
      call pc_put('comment_label'  ,obj%comment_label)

      call pc_put('path_segy',obj%path_segy)
      call pc_put('type_out' ,obj%type_out)
      call pc_put('retention',obj%retention)
      call pc_put('hdr_switch',obj%hdr_switch)
      call pc_put('beg_init' ,obj%beg_init)
      call pc_put('beg_inc'  ,obj%beg_inc)
      call pc_put('hdr_flag' ,obj%hdr_flag)
      call pc_put('hdr_sum1' ,obj%hdr_sum1)
      call pc_put('hdr_sum2' ,obj%hdr_sum2)
      call pc_put('tape_host' , obj%tape_host)
      if(i_err < 0) then
        obj%fatal_error= .true.
      endif
      if(obj%type_out.eq.'PS'.or.obj%type_out.eq.'PS_VMOD')then
        call pc_put_sensitive_screen_flag('Permsave',.true.)
      else
        call pc_put_sensitive_screen_flag('Permsave',.false.)
        obj%path_ps=PATHCHECK_EMPTY
        obj%label_type='INTERNATIONAL'
        obj%quad=' '
        obj%country=' '
        call pc_get_pdata  ('project'     , obj%project_name)
        obj%dir=' '
        obj%hdr_line=8
        obj%hdr_shot=7
        call pc_get_pdata  ('sub_project'     , obj%line_names)
        obj%line_names=' '
        obj%shotpoints=' '
        obj%contractor=' '
        obj%processes=' '
        obj%comment_label=' '
        obj%shotmin=2**30
        obj%shotmax=-2**30
        obj%linemin=2**30
        obj%linemax=-2**30
        obj%knttr=0
      endif      
      call pc_put_control ('need_label'  , .false.)
      call pc_put_control ('ntapes'      , 1)
      call pc_put_control ('twosets'     , .false.)       ! default false
      call pc_put_control ('need_request', .false.)       ! default false
      call pc_put_control ('iftd'        , .false.)       ! default false
      call pc_put_control ('ndisk'       , 0)
      call pc_put_control ('setup_only'  , .false.)       ! default .false.
      ctmp='TODI74'
      if(obj%tape_host.eq.'odi90')then
        ctmp='TODI90'
      else if(obj%tape_host.eq.'odi91')then
        ctmp='TODI91'
      else if(obj%tape_host.eq.'jaws'.or.obj%tape_host.eq.'hnomega1')then
        ctmp='A'
      endif
      call pc_put_control ('tapequeue'   , ctmp)

      if(pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.
      call manhist_check_name(obj%volume(1),istat)
      if(istat.ne.0)then
        call pc_error('TTROT_update: ABORT - volume already exists in history')
      endif

!          If this is a velocity model permsave - compare names in 
!           %trin_filenames and path_ps

      if(obj%type_out.eq.'PS_VMOD')then
        call getlun(lun1,istat)
        call getlun(lun2,istat)
        call getsys_hostname(host)
        write(filenames_file_for_ttrot,'(3A,I8.8)')'%trin_filenames_',trim(host),'_',getsys_pid()
print*,'Filenames File=',filenames_file_for_ttrot

        open(lun1,file=filenames_file_for_ttrot,status='old',iostat=istat)
        if(istat.ne.0)then
          call pc_error('TTROT_update-->Unable to open file %trin_filenames')
        endif
        open(lun2,file=obj%path_ps,status='old',iostat=istat)
        if(istat.ne.0)then
          call pc_error('TTROT_update-->Unable to open file ',&
                        &obj%path_ps)
        endif
!          Count number of entries in path_ps
        knt=0
        do 
          read(lun2,'(A)',iostat=istat)lcard
          if(istat.lt.0)exit
          knt=knt+1
        enddo
        rewind lun2
!         Read path_ps into memory for easy search
        call mem_alloc(lcards,knt)
        do i=1,knt
          read(lun2,'(A)')lcards(i)
        enddo
!          Now read file names and look for corresponding processing records
        do
          read(lun1,'(A)',iostat=istat)lcard
          if(istat.lt.0)exit
          i2=index(lcard,'_vmod')
          if(i2.eq.0)then
            call pc_error('TRIN input names must contain _vmod')
            return
          endif
          i1=index(lcard,'/',.true.)
          if(i1.eq.0)i1=1
          lcard2=lcard(i1+1:i2-1)
!             There must be an hgrid and a modgrid file with this string
          foundhgrid=.false.
          foundmodgrid=.false.
          do i=1,knt
            k=index(lcards(i),trim(lcard2))

            if(k.eq.0)then
              cycle
            endif
            j1=index(lcards(i),'.modgrid')
            j2=index(lcards(i),'.hgrid')
            if(j1.ne.0)then
              foundmodgrid=.true.
            endif              
            if(j2.ne.0)then
              foundhgrid=.true.
            endif
            if(foundmodgrid.and.foundhgrid)go to 100
          enddo
          msg='There are not modgrid and hgrid files for file ' // trim(lcard2)
          call pc_error(msg)
 100      continue
        enddo
        call mem_free(lcards)

      endif
      end subroutine ttrot_update

      subroutine ttrot_end_trap

      integer :: nc

      if(object%type_out.eq.'PS_VMOD'.and.object%path_ps.eq.PATHCHECK_EMPTY)then
        call pc_error('You must answer PATH_PS when doing a velocity model &
                      &permsave')
      endif

      if(object%tape_host.ne.'jaws'.and.object%tape_host.ne.'hnomega1')then
       select case(object%media)
        
         case('3590HD')
 
           if(object%tape_host.ne.'odi90')then
             object%tape_host='odi90'
             call pc_info('Tape host set to odi90 based on media parameter')
           endif

         case('3592')
 
           if(object%tape_host.ne.'odi91')then
             object%tape_host='odi91'
             call pc_info('Tape host set to odi91 based on media parameter')
           endif

         case('LTO')

           if(object%tape_host.ne.'hoeplgp01')then
             object%tape_host='hoeplgp01'
             call pc_info('Tape host set to hoeplgp01 based on media parameter')
           endif

         case default
 
            if(object%tape_host.ne.'odi74'.and.object%tape_host.ne.'odi90'&
               .and.object%tape_host.ne.'odi91')then
              object%tape_host='odi74'
              call pc_info('TAPE_HOST set to odi74 based on media parameter')
            endif

       end select
      else
        if(object%media.ne.'NR3590')then
          object%media='NR3590'
          call pc_info('MEDIA changed to NR3590 based on TAPE_HOST')
        endif
        nc=len_trim(object%volume(1))
        if(nc.gt.6)then
          object%fill_rows='NO'
          call pc_warning('VOLUME should be 6 characters or less for ',&
                        &object%tape_host)
        endif
      endif

      end subroutine ttrot_end_trap
!
      subroutine ttrot_media_trap (keyword)

      character(len=*),intent(in) :: keyword
      if(object%media.eq.'3590HD')then
        if(object%tape_host.ne.'odi90')then
          object%tape_host='odi90'
          call pc_info('Tape host set to odi90 based on media parameter')
        endif
      endif
      if(object%media.eq.'3592')then
        if(object%tape_host.ne.'odi91')then
          object%tape_host='odi91'
          call pc_info('Tape host set to odi91 based on media parameter')
        endif
      endif
      call ttrot_build_pdns
      return
      end subroutine ttrot_media_trap

      subroutine ttrot_tape_host_trap (keyword)
      character(len=*),intent(in) :: keyword

      if((object%location.eq.'alaska'.or.object%rlocation.eq.'ALASKA')&
         .and.object%tape_host.ne.'jaws')then
        object%tape_host='jaws'
        object%host_noptions=1
        object%host_options(1)='jaws'
        call pc_info('tape_host set to jaws based on location')
      endif
      if(object%tape_host.eq.'hoepodi90')object%tape_host='odi90'
      if(object%tape_host.eq.'hoepodi91')object%tape_host='odi91'
      if(object%tape_host.eq.'hoepodi74')object%tape_host='odi74'

      if(object%tape_host.eq.'odi90')return
      if(object%tape_host.eq.'odi91')return
      if(object%tape_host.eq.'odi74')return
      if(object%tape_host.eq.'jaws')return
      if(object%tape_host.eq.'hoeplgp01')return
      if(object%tape_host.eq.'hnomega1')return

      call pc_error('Invalid TAPE_HOST = ',object%tape_host) 
      
      end subroutine ttrot_tape_host_trap

      subroutine ttrot_fill_rows_trap (keyword)

      character(len=*),intent(in) :: keyword
      call ttrot_build_pdns
      return
      end subroutine ttrot_fill_rows_trap

      subroutine ttrot_num_rows_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call ttrot_build_pdns
      return
      end subroutine ttrot_num_rows_trap

      subroutine ttrot_userid_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%userid==' ') then
        call pc_get_pdata  ('user_name'   , object%userid)
      endif
      call ttrot_build_pdns
      return
      end subroutine ttrot_userid_trap

      subroutine ttrot_project_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%project==' ') then
        call pc_get_pdata  ('project', object%project)
        call string_squeeze_blanks(object%project)
      endif
      call ttrot_build_pdns
      return
      end subroutine ttrot_project_trap

      subroutine ttrot_sub_project_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%sub_project==' ') then 
        call pc_get_jdata('sub_project',object%sub_project)
        call string_squeeze_blanks(object%sub_project)
      endif
      call ttrot_build_pdns
      return
      end subroutine ttrot_sub_project_trap

      subroutine ttrot_task_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%task.eq.' '.and.(object%type_out.eq.'PS'.or.object%type_out&
                                 .eq.'PS_VMOD'))object%task='permsave'
      if((object%type_out.ne.'PS'.and.object%type_out.ne.'PS_VMOD').and.&
         object%task.eq.'permsave')object%task=' '
      call string_squeeze_blanks(object%task)
      call ttrot_build_pdns
      return
      end subroutine ttrot_task_trap

      subroutine ttrot_type_out_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%task.eq.' '.and.object%type_out(1:2).eq.'PS')&
         object%task='permsave'
      if(object%type_out(1:2).ne.'PS'.and.object%task.eq.'permsave')&
         object%task=' '
      call ttrot_build_pdns
      return
      end subroutine ttrot_type_out_trap

      subroutine ttrot_format_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%format(1:4) /= 'SEGY') then
        call pc_put_sensitive_array_flag('sbyte_arrayset', .false.)
        object%nummap=0
        object%mod_segy='NO'
        call pc_put_sensitive_field_flag('mod_segy', .false.)
        call pc_put_sensitive_field_flag('pack_hdr', .true.)
        call pc_put_sensitive_field_flag('path_segy', .false.)
        call pc_put_sensitive_field_flag('history', .true.)
      else
        call pc_put_sensitive_field_flag('mod_segy', .true.)
        call pc_put_sensitive_field_flag('pack_hdr', .false.)
        call pc_put_sensitive_field_flag('path_segy', .true.)
        call pc_put_sensitive_field_flag('history', .false.)
      endif
      call ttrot_build_pdns
      return
      end subroutine ttrot_format_trap

      subroutine ttrot_mod_segy_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%mod_segy .eq. 'NO') then
        call pc_put_sensitive_array_flag('sbyte_arrayset', .false.)
        object%nummap=0
      else
        call pc_put_sensitive_array_flag('sbyte_arrayset', .true.)
      endif
      return
      end subroutine ttrot_mod_segy_trap

      subroutine ttrot_path_segy_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword

      if(object%path_segy.eq.PATHCHECK_EMPTY)return
      call pathcheck('path_segy',object%path_segy)
      return
      end subroutine ttrot_path_segy_trap

      subroutine ttrot_path_ps_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword

      if(object%path_ps.eq.PATHCHECK_EMPTY)return
      call pathcheck('path_ps',object%path_ps)
      if(object%volume(1).eq.'SCR'.or.object%volume(1).eq. ' ')then
        object%volume(1)='NSCR'
        if(object%media.eq.'3592')object%volume(1)='BNSCR'
      endif
      return
      end subroutine ttrot_path_ps_trap

      subroutine ttrot_max_tr_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      character(len=80) :: card
      if(object%max_tr.gt.object%max_per_vol_sav)then
        write(card,*)'MAX_TR cannot be greater than ',object%max_per_vol_sav
        call pc_error(card)
      endif
      if(object%max_tr.gt.0)object%max_per_vol=object%max_tr
      return
      end subroutine ttrot_max_tr_trap

      subroutine ttrot_label_type_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%label_type.eq.'INTERNATIONAL')then
        call pc_put_sensitive_field_flag('COUNTRY',.true.)
        call pc_put_sensitive_field_flag('PROJECT_NAME',.true.)
        call pc_put_sensitive_field_flag('QUAD',.false.)
      else
        call pc_put_sensitive_field_flag('COUNTRY',.false.)
        call pc_put_sensitive_field_flag('PROJECT_NAME',.false.)
        call pc_put_sensitive_field_flag('QUAD',.true.)
      endif
      return
      end subroutine ttrot_label_type_trap

      subroutine ttrot_quad_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%quad)
      return
      end subroutine ttrot_quad_trap

      subroutine ttrot_country_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%country)
      return
      end subroutine ttrot_country_trap

      subroutine ttrot_project_name_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%project_name)
      return
      end subroutine ttrot_project_name_trap

      subroutine ttrot_dir_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%dir)
      return
      end subroutine ttrot_dir_trap

      subroutine ttrot_hdr_line_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%hdr_line.eq.0)then
        call pc_put_sensitive_field_flag('LINE_NAMES',.true.)
      else
        call pc_put_sensitive_field_flag('LINE_NAMES',.false.)
      endif
      if(object%hdr_line.lt.0.or.object%hdr_line.gt.object%nwih)then
        call pc_error("HDR_LINE must be between 0 and NWIH")
      endif
      return
      end subroutine ttrot_hdr_line_trap

      subroutine ttrot_hdr_shot_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%hdr_shot.eq.0)then
        call pc_put_sensitive_field_flag('SHOTPOINTS',.true.)
      else
        call pc_put_sensitive_field_flag('SHOTPOINTS',.false.)
      endif
      if(object%hdr_shot.lt.0.or.object%hdr_shot.gt.object%nwih)then
        call pc_error("HDR_SHOT must be between 0 and NWIH")
      endif
      return
      end subroutine ttrot_hdr_shot_trap

      subroutine ttrot_line_names_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%line_names)
      return
      end subroutine ttrot_line_names_trap

      subroutine ttrot_shotpoints_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%shotpoints)
      return
      end subroutine ttrot_shotpoints_trap

      subroutine ttrot_contractor_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%contractor)
      return
      end subroutine ttrot_contractor_trap

      subroutine ttrot_processes_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%processes)
      return
      end subroutine ttrot_processes_trap

      subroutine ttrot_comment_label_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%comment_label)
      return
      end subroutine ttrot_comment_label_trap



      subroutine ttrot_volume_element_trap (keyword,indx,action)
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments
      integer         ,intent(in) :: indx              ! arguments
      integer         ,intent(in) :: action            ! arguments
      integer           :: i_err,stat,i,n,nvols,ip1,ip2
      character         :: userid*32,line*80,vols(50)*6
      character         :: lpdn*48
!
      if(action == PC_REMOVE) then
        if(object%num_rows > object%nreel) object%num_rows= object%nreel
        return
      endif
      if(action /= PC_MODIFY .and. action /= PC_INSERT) return
      i=indx
!     If you type it in, it can have upper case.
!     call string_to_lower(object%volume(i))
      if(len_trim(object%volume(i)).le.6.and.object%type_out(1:2).eq.'PS')then
        call pc_error('A pdn is required when TYPE_OUT=PS')
        return
      endif
      if(object%volume(i)==' ') then
        call pc_error ('TTROT: blank VOLUME entry is illegal')
        object%volume(i)='SCR'
        if(object%media.eq.'3592')object%volume(i)='BSCR'
        return
      endif
      if(len_trim(object%volume(i))> 6) then
        call ttrot_getpdn(object,object%volume(i),nvols,vols,userid)
         if(nvols==0) then
          if(object%type_out.eq.'SCR')then
            call pc_info ('TTROT: non catalogued volume ==>scratch tapes!')
          else if(object%type_out(1:2).eq.'PS')then
            call pc_info ('TTROT: non catalogued permsave ==> new tapes!')
          endif
         else if(nvols <= size(vols)) then
           line = 'TTROT: volser list for '//trim(object%volume(i))
           call pc_info(line)
           line = 'TTROT: ' 
           do n=1,nvols
             line = trim(line)//vols(n)//' '
             if(len_trim(line)> 72) then
               call pc_info(line)
               line = 'TTROT: ' 
             endif
           enddo
           if(len_trim(line)> 7) call pc_info(line)
           if(userid /= object%frontend_user) then
            line = 'TTROT: your id='//trim(object%frontend_user)//' /=&
           & the volume userid='//trim(userid)
            call pc_info(line)
            lpdn= ' '
            ip1 = index(object%volume(i),'.')
            if(ip1>0) then
              lpdn=object%volume(i)(1:ip1)
              ip2 = index(object%volume(i)(ip1+1:),'.')
              if(ip2 > 0) then
                lpdn = object%volume(i)(1:ip1)//trim(object%frontend_user)//&
                &trim(object%volume(i)(ip1+ip2:))
              endif
              object%volume(i)=lpdn
            endif
           endif
         else
           write(line,'(''TTROT: > 50 tapes for '',A)') &
           trim(object%volume(i))
           call pc_info(line)
         endif
      else
         stat = ttrot_iscat(object,object%volume(i),userid,i_err)
         if(stat == 1) then
          if(userid /= object%userid) then
           line='TTROT: warning,vol='//trim(object%volume(i))//&
           &' is owned by '//trim(userid)
           call pc_info(line)
           object%volume(i) = 'SCR'
           if(object%media.eq.'3592')object%volume(i)='BSCR'
          else
           line='TTROT: vol='//trim(object%volume(i))//&
           &',overwrite of existing volser!'
           call pc_error(line)
          endif
         endif
      endif
      return
      end subroutine ttrot_volume_element_trap

      subroutine ttrot_sbyte_arrayset_trap

      integer :: i

      do i = 1,object%nummap
        call string_to_upper(object%wtype(i))
        if(object%wtype(i) /= 'F') object%wtype(i)='I'
        if(object%bytes(i)   > 4) object%bytes(i)  =4
        if(object%sbyte(i)   < 1) object%sbyte(i)  =1
        if(object%sbyte(i)   < 1) object%sbyte(i)  =1
        if(object%cps_hdr(i) < 0 .or. object%cps_hdr(i) > object%nwih) then
          call pc_error ('TTROT_UPDATE: cps_hdr value is out of range')
          object%cps_hdr(i)=0         !no mapping
        endif
      enddo


      end subroutine ttrot_sbyte_arrayset_trap
!
! find the list of vols catalogued under pdn(indx)
      subroutine ttrot_getpdn(obj,volume,nvols,vols,userid)
      implicit none
      type(ttrot_struct),intent(in)    :: obj  !arguments
      character(len=*)                 :: volume     !arguments
      integer                          :: nvols      ! arguments
      character(len=6)                 :: vols(:)    !arguments
      character(len=*)                 :: userid     !arguments
      integer           :: i,ip
      character         :: req*80,msg*240

      if(volume==' ') return
      userid= ' '
      vols  = ' '
      nvols = 0
      if(len_trim(volume) <7) then
        nvols=1
        vols = volume
        return
      endif
      req = 'GETPDN='//volume

      if (tcatclient_query(req, msg) /= 1) then
        call pc_info('TTROT_GETPDN: catalogue server error!!')
        return
      endif

      if(index(msg,"SERVER FATAL ERROR") /= 0) then
        call pc_info('TTROT_GETPDN: catalogue server error!!')
        return
      endif

      ip = index(msg,"GETPDN=Y")
      if(ip /= 0) then
        ip = index(msg,"nvol=")
        if(ip /= 0) then
          read(msg(ip+5:),*) nvols
        endif
        ip = index(msg,"userid=")
        if(ip /= 0) then
          read(msg(ip+7:),*) userid
        endif
        ip = index(msg,"vols=")
        if(ip /= 0) then
          do i = 1,min(nvols,size(vols))
            vols(i)=msg(ip+5+(i-1)*6:ip+4+i*6)
          enddo
        endif
      else
        return
      endif

      return
      end subroutine ttrot_getpdn
!
! return the volser of the next tape to be written out
! set the variable oreel,ivol,nvols,vols(:),volcnt
! A non catalogued pdn will call for scratch tapes.
! A volume change is forced when:
!     it is the very first tape,
!     the nvols limit for the current volume is exceeded
!     the num_tr limit is exceeded
! A catalogued pdn will overwrite the volsers in the data set.
! The catalogue will be checked for conflicts with specific volsers.


      subroutine ttrot_next_vol(obj,vol)
      type(ttrot_struct),intent(inout)    :: obj  !arguments
      character(len=*),intent(out)        :: vol  !arguments
      integer           ::  nvols

      character(len=6)  ::  vols(50)
      character(len=32) ::  userid
      character(len=80) ::  line
      vol = ' '
!     write(obj%stdo,*) 'TTROT_NEXT_VOL: ivol=',obj%ivol,&
!      &' nvols=',obj%nvols
!     write(obj%stdo,*) 'TTROT_NEXT_VOL: oreel=',obj%oreel,&
!      &' nreel=',obj%nreel
      if(obj%oreel==0 .or. obj%ivol==obj%nvols .or. &
         obj%volcnt >= obj%num_tr(obj%oreel)) then
        obj%oreel = obj%oreel+1  !go to next volume, starts at zero
        obj%volcnt= 0  !reset the counter for a new volume
        obj%ivol  = 0
        obj%nvols = 0
      endif
      if(obj%oreel > obj%nreel) return  !input queue is exhausted
      if(obj%nvols==0) then
        if(len_trim(obj%volume(obj%oreel))> 6) then
         call ttrot_getpdn(obj,obj%volume(obj%oreel),nvols,vols,userid)
         if(nvols==0) then
           call pc_info ('TTROT: non catalogued volume, scratch tapes used!')
           nvols=50
           obj%vols(1:nvols) ='SCR'
           if(obj%media.eq.'3592')obj%vols(1:nvols) ='BSCR'
           if(obj%type_out(1:2).eq.'PS')then
              obj%vols(1:nvols) ='NSCR'
              if(obj%media.eq.'3592')obj%vols(1:nvols) ='BNSCR'
           endif
         else if(nvols < size(obj%vols)) then
           write(line,*) 'TTROT: overwrite catalogue, nvols=',nvols
           call pc_info (line)
           obj%vols(1:nvols) = vols(1:nvols)
           obj%vols(nvols+1:50) = 'SCR'
           if(obj%media.eq.'3592')obj%vols(nvols+1:50) = 'BSCR'
           if(obj%type_out(1:2).eq.'PS')then
              obj%vols(nvols+1:50) ='NSCR'
              if(obj%media.eq.'3592')obj%vols(nvols+1:50) ='BNSCR'
           endif
           nvols=50
         else
           obj%vols(1:size(vols)) = vols(1:size(vols))
           nvols=50
         endif
         obj%nvols   = nvols
        else
         obj%vols(1) = obj%volume(obj%oreel)
         obj%nvols   = 1
        endif
        obj%ivol = 1
!       obj%volser  will be set in ttrot_open
        vol =  obj%vols(1)
        return
      endif
      if(obj%ivol < obj%nvols) then
!     write(obj%stdo,*) 'TTROT_NEXT_VOL: vol=',obj%vols(obj%ivol+1)
        vol = obj%vols(obj%ivol+1)
        obj%ivol = obj%ivol+1
      endif
      return
      end subroutine ttrot_next_vol
!

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!
!   The following two routines are for use in old cps
!   Only 1 instance of ttrot can be supported in the old cps
!

      subroutine ttrot(obj,ntr,hd,tr)
      implicit none
      type(ttrot_struct), pointer :: obj       ! arguments
      integer :: istat,ntr
      integer :: tpioclnt_weof
      double precision :: hd(:,:)
      real :: tr(:,:)
!
!  Local variables
      integer ::           neof  
      character(len=6)   :: vol
      character(len=160) :: cmd,wrkdir

      integer ::         elun,errcnt,trno  

      real    ::                   hval,temp  
  
!
!  Check for object integrity
      if (.not.associated(obj)) then
        call pc_error('TTROT: object pointer is bad')
        return
      endif
!  The root cpu is the only one allowed to perform tape output
      if(obj%rank/=0)     return
      if(obj%skip_wrapup) return
      if(obj%fatal_error) return
      errcnt = 0
!
! Check to see if any traces for output
      if(ntr ==NO_MORE_TRACES) then
!                   Go make the permsave label
        if(obj%type_out .eq. 'PS')then
          call ttrot_permsave(obj,1)
        endif
        obj%mntswo=0
        call ttrot_wrapup(obj)    !finish with currently mounted tape
        return
      endif   !if(ntr == NO_MORE_TRACES) then
!
      do 20 trno=1,ntr
        if(obj%hdr_flag>0) then   !is this trace flagged for output?
          if( nint(hd(obj%hdr_flag,trno))==0) goto 20
        endif

        if(obj%oreel==0) then     !1ST OUTPUT REEL, MOUNT ONLY
          obj%mntswo=1
        else if(obj%oreel <= obj%nreel ) then !INTERMEDITE, DISMOUNT? & MOUNT
          if(obj%hdr_switch /= 0) then
            hval = hd(obj%hdr_switch,trno)
            if(hval <   obj%beg_init + (obj%oreel-1)*obj%beg_inc*0.5 .or. &
               hval >=  obj%beg_init + (obj%oreel+1)*obj%beg_inc*0.5) then
               obj%mntswo=1
            endif
          else   !hdr_switch has precedence over num_tr
            if(obj%volcnt>=obj%num_tr(obj%oreel)) obj%mntswo=1
          endif
          if(obj%vscnt >= obj%max_per_vol) then
            !force reel change if close to full
            obj%mntswo=1
          endif
        else                      !TTROT INACTIVE, PAST LAST REEL
          obj%mntswo=0
          return
        end if
!
! If calling for a new reel, then:
!    - flush buffers,
!    - catalogue the reel if requested,
!    - dismount the old reel,
!    - and mount the next reel.
 10     continue 
        if(obj%mntswo == 1) then
!                 Make a label if permsave
          if(obj%oreel.gt.0.and.obj%type_out=='PS')then
            call ttrot_permsave(obj,0)
            obj%linemin=2**30
            obj%linemax=-2**30
            obj%shotmin=2**30
            obj%shotmax=-2**30
            obj%knttr=0
          endif
!
! dismount any mounted tape.
! prints summary, writes EOD, catalogues, rewinds, unloads, closes
          call ttrot_dmnt(obj)
          obj%mntswo=0
!
! Determine the next output volser. set oreel,ivol,vols
          call ttrot_next_vol(obj,vol)
          if(vol /= ' ') then
            obj%fd =  ttrot_open(obj) !also writes predata such as historys
            if(obj%fatal_error) then
              ntr = FATAL_ERROR
              if(obj%location.eq.'houston')then
!               force unload the tape
                call getlun(elun,istat)
                open(elun,file='goodger_email',status='replace',iostat=istat)
                write(elun,'(A)')trim(obj%jobname)
                write(elun,'(A)')trim(obj%frontend_path)
                write(elun,'(A)')trim(obj%frontend_user)
                write(elun,'(A)')trim(obj%tape_host)
                write(elun,'(A)')trim(vol)
                close(elun,status='keep')
                call getsys_current_dir(wrkdir)
                cmd='rsh ' // trim(obj%tape_host) // ' "tapelib -op fu -vol ' &
                // trim(vol) // '" >> ' // trim(wrkdir) // 'goodger_email'
                call putsys_cmd(trim(cmd))
                print*,' TTRIN_OPEN-->back from tapelib'
                print*,trim(cmd)
                cmd='mail -s "Failure to Load" goodgkp@conocophillips.com < '&
                    //  'goodger_email'
                call putsys_cmd(cmd)
              endif         
              return
            endif
            obj%vscnt=0
          else
            call ttrot_wrapup(obj)    !ttrot is done
            return
          end if
        end if      !if(obj%mntswo == 1) then
!
! Place data into buffer and write to tape
!here
        if(obj%type_out.eq.'PS_VMOD'.and.nint(hd(31,trno)).eq.1)then
          neof=1
          if(.not.obj%firsttrace)istat = tpioclnt_weof(obj%fd,neof)  
          obj%firsttrace=.false.
        endif
        call ttrot_pak_hdtr(obj,hd(:,trno),tr(:,trno))
        if(obj%fatal_error) then      ! had a write problem
          if(obj%vscnt < 400000) then !true write problem or is reel full?
            call ttrot_wrapup(obj)    !give up, stop with the current volser
            ntr = FATAL_ERROR
            return
          endif
!         ii = -1;
!         i_err = tpioclnt_space(obj%fd, ii)
          errcnt = errcnt + 1
          obj%fatal_error= .false.
          obj%mntswo = 1              !save current trace to next volser
          goto 10
        endif
!
!          Saveheader ranges for permsave label
        if(obj%type_out.eq.'PS')then
          if(obj%hdr_line.ne.0)then
            temp=hd(obj%hdr_line,trno)
            obj%linemin=min(obj%linemin,temp)
            obj%linemax=max(obj%linemax,temp)
          endif
          if(obj%hdr_shot.ne.0)then
            temp=hd(obj%hdr_shot,trno)
            obj%shotmin=min(obj%shotmin,temp)
            obj%shotmax=max(obj%shotmax,temp)
          endif
        endif
        obj%knttr=obj%knttr+1
!
 20   continue
      return
!
      obj%fatal_error = .true.
      ntr = FATAL_ERROR
      return
      end subroutine ttrot
!
      subroutine ttrot_pak_hdtr(obj,hd,tr)
      type(ttrot_struct) :: obj       ! arguments
      double precision,intent(in):: hd(:)
      real,intent(in)            :: tr(:)
      type(segy_trc_hdr):: segyhd
      double precision  :: tstrt,ddt,old_lav
      double precision  :: hdo(obj%nwih)
      integer           :: iwdtype(64)
      real      :: hdr(obj%nwih)
      integer   :: i,muteo  
      integer   :: nwrds,swrd,nby,nw
      real      :: rlav,rlavo
      integer   :: ihd(60)
      real      :: thd(60)
      integer   :: tpioclnt_write
      integer   :: tpioclnt_aswrite
      integer   :: nowait
      integer   :: didwr
      integer   :: ntrwr
      logical   :: lswap=.false.
      equivalence (ihd,thd)
      if(obj%skip_wrapup .or. obj%fd < 0) return
!
! Place data into buffer and write to tape
! Always write data out as swap_endian()=1
      if(swap_endian() == 1) then
       lswap = .false.
      else
       lswap = .true.
      endif
      hdo(1:obj%nwih) = hd(1:obj%nwih)
      rlav =0.0
      rlavo=hd(25)
      muteo=nint(hdo(2))
      if(obj%nsout < muteo) muteo = obj%nsout
      do i=1,obj%nsout
        rlav = max(rlav,abs(tr(i)))
      enddo
      hdo(25)=rlav
      hdr(1:obj%nwih) = hdo(1:obj%nwih)
      if(rlav == 0.0) obj%ndead=obj%ndead+1
      ntrwr = 0

      if(obj%format == 'CPS') then
        obj%pak=0.0     !clear the buffer
        swrd = 1 +  obj%nwih
        nwrds= obj%nwih + obj%nsout
        if(obj%wdtype == 'IBM') then
          obj%pak(1:obj%nwih) = hd(1:obj%nwih)
          obj%pak(25) = rlavo
          swrd= obj%nwih + 1
          obj%pak(swrd:swrd+obj%nsout-1)= tr(1:obj%nsout)
          nwrds= obj%nwih + obj%nsout
          call wrdc_float_to_ibm(obj%pak, nwrds, lswap)
          nby= 4*nwrds
        else
          nw = sizeof(hd(1))/sizeof(obj%pak(1))
          if(obj%nbits_hd==64) then
            old_lav = hd(25)
            if(lswap) call swap_bytes(hdo(1),obj%nwih)
            call cmem_cpy_c(obj%pak,hdo,nw*obj%nwih*obj%nbytesnword*2)
!!!            obj%pak(1:nw*obj%nwih) = transfer(hdo(1:obj%nwih),(/0.0/))
            swrd= nw*obj%nwih + 1
          else
            obj%pak(1:obj%nwih) = hd(1:obj%nwih)
            obj%pak(25) = rlavo
            if(lswap) call swap_bytes(obj%pak(1),obj%nwih)
            swrd= obj%nwih + 1
          endif
          obj%pak(swrd:swrd+obj%nsout-1)= tr(1:obj%nsout)
          if(lswap) call swap_bytes(obj%pak(swrd),obj%nsout)
          nby= (obj%nbits_hd/8)*obj%nwih + (obj%nbits/8)*obj%nsout
        endif
        nw = tpioclnt_write(obj%fd,obj%pak,nby)
        if(nw <= 0) then
          write(obj%stdo, *) ' TTROT: error writing trace ',obj%volcnt+1
          write(obj%stdo, *) ' TTROT: while on reel ',&
           trim(obj%volume(obj%oreel))
          obj%fatal_error = .true.
          return
        endif
        ntrwr = 1
      else if(obj%format == 'BLK')then
        call ttrot_wr_block(obj,obj%nwih,hdr)
        if(obj%fatal_error) then
            return
        endif
        call ttrot_wr_block(obj,obj%nsout,tr)
        if(obj%fatal_error) then
          return
        endif
        ntrwr = 1
      else if(obj%format == 'SEGY')then
        nwrds = 240/sizeof(rlav)
        ihd(1:nwrds)=0
        obj%pak(1:obj%nsout+nwrds)=0.0     !clear the buffer
        tstrt = obj%tstrt
        ddt   = obj%dt
        call segy_cpshd_to_segyhd(hd,segyhd,tstrt,ddt,obj%nsout)
        call segy_pack_segyhd(segyhd,ihd,lswap)
        if(obj%nummap > 0 .and. obj%mod_segy(1:1)=='Y') then
          do i =1,obj%nummap
            iwdtype(i)=0
            if(obj%wtype(i)(1:1)=='F') iwdtype(i)=1
          enddo
          call ttrot_map_cps_to_segy(ihd,obj%nummap,obj%bytes,&
          obj%sbyte, obj%cps_hdr, iwdtype, hd)
        endif
! ABSOFT BUG? Following corrupts the headers on about 1% of the traces
! Replaced by mvdata routine
!       obj%pak(1:nwrds) = thd(1:nwrds)
        call ttrot_mvdata(thd,obj%pak,nwrds)
        swrd = 1 +  (240)/sizeof(rlav)
        obj%pak(swrd:swrd+obj%nsout-1)= tr(1:obj%nsout)
        call wrdc_float_to_ibm(obj%pak(swrd:swrd+obj%nsout-1),&
         obj%nsout, lswap)
        nby = 240 +4*obj%nsout
        nw = tpioclnt_write(obj%fd,obj%pak,nby)
        if(nw <= 0) then
          write(obj%stdo, *) ' TTROT: error writing trace ',hd(1)
          obj%fatal_error = .true.
          return
        endif
        ntrwr = 1
      end if
      if(obj%format == 'TPR') then
        nw = sizeof(hd(1))/sizeof(obj%pak(1)) ! 1 or 2
        if(obj%nib==0) obj%pak=0.0            ! clear the buffer
        swrd = 1+ obj%nib*obj%bytes_per_trace/4 ! 4byte wrd start pos
        
        if(obj%nbits_hd==64) then
            old_lav = hd(25)
            if(lswap) call swap_bytes(hdo(1),obj%nwih)
            i = 2*obj%nwih !number of 4 byte words to copy
            call txfr_r4_to_i4_c(hdo(1),obj%pak(swrd),i)
            swrd= swrd + nw*obj%nwih
        else
            obj%pak(swrd:swrd+obj%nwih-1) = hd(1:obj%nwih)
            obj%pak(swrd+25-1) = rlavo
            if(lswap) call swap_bytes(obj%pak(swrd),obj%nwih)
            swrd= swrd + obj%nwih
        endif
        obj%pak(swrd:swrd+obj%nsout-1)= tr(1:obj%nsout)
        if(lswap) call swap_bytes(obj%pak(swrd),obj%nsout)
        obj%nib = obj%nib+1
        if(obj%nib==obj%tpr) then
          nby= obj%nib*obj%bytes_per_trace
          nowait = 1;
          !go to synch mode when we get close to the limit.
          !need the trace count to be up to date
          if(obj%volcnt>=obj%num_tr(obj%oreel)-2*obj%tpr) nowait=0
          nw = tpioclnt_aswrite(obj%fd,obj%pak,nby, nowait, didwr)
          ntrwr = didwr/obj%bytes_per_trace
          if(nw <= 0) then
            write(obj%stdo, *) ' TTROT: error writing traces ',obj%volcnt+1
            write(obj%stdo, *) ' TTROT: while on reel ',&
             trim(obj%volume(obj%oreel))
            obj%fatal_error = .true.
            return
          endif
          obj%nib = 0
        endif
      endif
      obj%vscnt =obj%vscnt+ntrwr  !for volser
      obj%nseqo =obj%nseqo+ntrwr  !for ireel
      obj%volcnt=obj%volcnt+ntrwr !total
      return
      end subroutine ttrot_pak_hdtr
! Routine to overcome ABSOFT bug.
! replaces the following statement
!       obj%pak(1:nwrds) = thd(1:nwrds)
      subroutine ttrot_mvdata(in,out,nel)
      real in(*)
      real out(*)
      integer nel
      out(1:nel) = in(1:nel)
      return
      end subroutine ttrot_mvdata
!
!
      integer function ttrot_stdo()
      ttrot_stdo = pc_get_lun()
      end function ttrot_stdo
!
      subroutine ttrot_print(obj)
      implicit none
      type(ttrot_struct) :: obj       ! arguments
      integer stdo
      stdo = ttrot_stdo()
      if(obj%oreel < 1 .or. obj%oreel > obj%nreel ) return
      if(obj%oreel == 1) then
       write(stdo,*) 'TTROT: JOBNAME=',trim(obj%jobname), &
       ' USERID=',obj%userid,' TAPE FORMAT=',obj%format
       write(stdo,*) 'TTROT: RETENTION=',obj%retention,&
       ' OUTPUT NDPT SAMPLES=',obj%nsout
      endif
      write(stdo,*) 'TTROT: VOLSER=',obj%volser,&
      &' FOR VOLUME=',trim(obj%volume(obj%oreel))
      write(stdo,*) 'TTROT: TRACE COUNT, VOLSER=',obj%vscnt,&
      &' VOLUME=',obj%volcnt,' CUMMULATIVE=',obj%nseqo,' DEAD=',obj%ndead
      return
      end subroutine ttrot_print
!
      subroutine ttrot_flush(obj)
      implicit none
      type(ttrot_struct),intent(inout) :: obj       ! arguments
      integer lastblk,nby,nw
      integer  :: tpioclnt_write
      integer  :: tpioclnt_aswrite
      logical  :: lswap=.false.
      integer  :: nowait
      integer  :: didwr
      integer  :: ntrwr
      
      if(obj%fd < 0) return
      if(obj%format=='TPR') then
         !clean up any asych writes and dump remaining
         !traces in buffer to output
          nby= obj%nib*obj%bytes_per_trace
         !nw = tpioclnt_write(obj%fd,obj%pak,nby)
          nowait=0 
          nw = tpioclnt_aswrite(obj%fd,obj%pak,nby,nowait,didwr)
          ntrwr = didwr/obj%bytes_per_trace
          if(nw < 0) then
            write(obj%stdo, *) ' TTROT: error writing trace ',obj%volcnt
            write(obj%stdo, *) ' TTROT: while on reel ',&
             trim(obj%volume(obj%oreel))
            obj%fatal_error = .true.
            return
          endif
          obj%vscnt =obj%vscnt+ntrwr  !for volser
          obj%nseqo =obj%nseqo+ntrwr  !for ireel
          obj%volcnt=obj%volcnt+ntrwr !total
          obj%nib = 0
          return
      endif

      lastblk=obj%blksiz-obj%nleft
      if(obj%format(1:3)/='BLK' .or. lastblk<=0) return
      call wrdc_float_to_ibm(obj%blkbuf,obj%pak,lastblk, lswap)
      nby = 4*lastblk
      nw = tpioclnt_write(obj%fd,obj%pak,nby)
      if(nw <= 0)then
        write(ttrot_stdo(),*) 'ttrot_flush: write error'
        obj%fatal_error = .true.
      endif
      obj%ndx=1
      obj%nleft=obj%blksiz
      return
      end subroutine ttrot_flush
!
      subroutine ttrot_wr_block(obj,nval,bufi)
      implicit none
      type(ttrot_struct),intent(inout) :: obj       ! arguments
      real,intent(in)        ::  bufi(:)
      integer,intent(in)     ::  nval
      integer  :: i,istart,nby,nw
      integer  :: tpioclnt_write
      integer  :: ndx,nleft
      logical  :: lswap=.false.
! pack input array to the buffer ( and write buffer if needed)
      ndx  = obj%ndx
      nleft= obj%nleft
      if(nleft >= nval)then
        do i=1,nval
          obj%blkbuf(ndx)=bufi(i)
          ndx=ndx+1
        enddo
        nleft=nleft-nval
      else ! nleft < nval
        do i=1,nleft
          obj%blkbuf(ndx)=bufi(i)
          ndx=ndx+1
        enddo
        istart=nleft+1    !start of leftover input data
        nleft=nleft-nval  !less than 0 implies leftover input
      endif

      if(nleft <= 0)then  !output the filled buffer
        call wrdc_float_to_ibm(obj%blkbuf,obj%pak,obj%blksiz, lswap)
        nby = 4*obj%blksiz
        nw = tpioclnt_write(obj%fd,obj%pak,nby)
        if(nw <= 0)then
          obj%fatal_error = .true.
          call pc_error('TTROT_WR_BLOCK: WRITE ERROR')
          obj%ndx  = ndx
          obj%nleft= nleft
          return
        endif
      endif
      if(nleft==0)then
        ndx=1
        nleft=obj%blksiz
      endif
      if(nleft < 0)then  !put remaining input data into buffer
        ndx=1
        nleft=abs(nleft)
        do i=istart,nleft+istart-1
          obj%blkbuf(ndx)=bufi(i)
          ndx=ndx+1
        enddo
        nleft=obj%blksiz-nleft
      endif
      obj%ndx  = ndx
      obj%nleft= nleft
      return
      end subroutine ttrot_wr_block
!
! ttrot_open ... Opens a tape file,
!    Checks for and reads tape labels.
!    Verifies that label matches the request.
!    return value is -1 on error.
!    return value is a file descriptor if the open is successful.
!    writes initial header files such as history to the tape


      integer function ttrot_open(obj)
      implicit none
      type(ttrot_struct),intent(inout) :: obj       ! arguments
      character(len=8)  ::     vol
      character(len=8)  ::     mode
      character(len=32) ::     mediavol
      integer       :: imedia(4)
      integer       :: itapeserver(4)
      integer       :: itapehost(4)
      integer       :: ivol(8),imode(2)
      integer       :: imediavol(4)
      integer       :: stat
      integer       :: i_err
      integer       :: is_scratch
      character(len=32) ::  userid

      integer       :: ilname(4)
      integer       :: tpioclnt_open_server  
      integer       :: label

!
      ttrot_open=-1
      if(obj%ivol<1 .or. obj%ivol>obj%nvols) then
        call pc_error('TTROT_OPEN: ERROR, ivol out of range')
        obj%fatal_error = .true.
        ttrot_open=-1
        return
      endif

      vol = obj%vols(obj%ivol)
      if(vol==' ') then
        call pc_error('TTROT_OPEN: ERROR, blank volser is illegal')
        obj%fatal_error = .true.
        ttrot_open=-1
        return
      endif
      call string_cc2hh(vol,ivol)
      call string_cc2hh(obj%media ,imedia)
      call string_cc2hh(obj%tape_server,itapeserver)
      mode='w'
      call string_cc2hh(mode,imode)
      mediavol = '3590_node:'//trim(vol)
      if(obj%media== 'DLT') then
        mediavol = 'dlt_node:'//trim(vol)
      endif
      if(obj%media== 'NR3590') then
        mediavol = 'nr3590_node:'//trim(vol)
      endif
      if(obj%media== '3592') then
        mediavol = '3592_node:'//trim(vol)
      endif
      if(obj%media== 'LTO') then
        mediavol = 'lto_node:'//trim(vol)
      endif
      if(obj%media== '3480') then
        mediavol = '3480_node:'//trim(vol)
      endif
      write(obj%stdo,*) 'ttrot_open: mediavol=',trim(mediavol)
      call string_cc2hh(mediavol,imediavol)
      call string_cc2hh(obj%tape_host,itapehost)

      write(obj%stdo,*) 'TTROT_OPEN: tape_host=',trim(obj%tape_host)
      write(obj%stdo,*) 'TTROT_OPEN: tape vol =',trim(vol)
      label  = 1    !force label writes
      if(obj%format.eq.'SEGY')then
        label=0
      endif
      ilname = 0
      obj%fd = tpioclnt_open_server(imediavol,imode,imedia, itapeserver,&
              itapehost,label,ilname)
      ttrot_open = obj%fd
      call string_hh2cc(ilname,obj%lname)
      is_scratch = 0
      if(vol(1:3)=='SCR' .or. vol(1:4)=='NSCR' .or. vol(1:4)=='BSCR') then
        call string_hh2cc(ivol,vol)
         is_scratch = 1
      endif
      if(obj%fd < 0) then
        write(obj%stdo,*) 'TTROT_OPEN: OPEN FAILURE, FD<0 FOR VOL=',trim(vol)
        obj%fatal_error= .true.
        obj%volser = vol
        return
      else
        call ttrot_get_vol(obj%fd,vol) ! now we have the tape number
        write(ttrot_stdo(),*) 'TTROT_OPEN: volser name from open =',vol
        if(vol == ' ') then
          write(obj%stdo,*) 'TTROT_OPEN: blank volser? fd=',obj%fd
          write(obj%stdo,*) 'TTROT_OPEN: tape has no labels'
          vol = obj%vols(obj%ivol)
        else
         stat = ttrot_iscat(obj,vol,userid,i_err)
!!!         if(stat==1 .and. userid/=obj%userid .and. is_scratch==1) then
         if(stat==1 .and. is_scratch==1) then
            write(obj%stdo,*) 'TTROT_OPEN: error - scratch volser=',trim(vol),&
            ' was already in the catalogue'
            obj%fatal_error = .true.
         else
            print*,'TTROT_OPEN: OK to write tape ',trim(vol)
            print*,'            status = ',stat,' is_scratch = ',is_scratch
         endif
        endif
      endif
      obj%vols(obj%ivol) = vol
      obj%volser = vol

      if(obj%fatal_error) return
      call ttrot_wr_predata (obj)
      return
      end function ttrot_open
!
! Purpose: write data that precedes the trace data.
!  - SEGY: 3200 byte and 400 byte records
!  - CPS : conoco history records
      subroutine ttrot_wr_predata (obj)
      implicit none
      type(ttrot_struct),intent(inout) :: obj       ! arguments
      type(segy_bin_hdr) :: binhdr
      character(len=3200):: h3200
      character(len=80)  :: card
      integer   :: buf(801),nw,nby,i,i_err,i1,i2,istat,fh
      integer   :: tpioclnt_write
      integer   :: itype(4),one
      integer   :: tpioclnt_weof
!     equivalence (h3200,buf)
      if(obj%format /= 'SEGY') then
        call string_cc2hh("CPS",itype)
!       call tapeio_set_type(obj%fd,itype)
        call ttrot_wrheader(obj)
        if(obj%fatal_error) return
        if(obj%history /= 'NONE') then
          if(obj%path_ps /= PATHCHECK_EMPTY .and. obj%ivol==1)then
            call ttrot_wrprec(obj)
          endif
          if(obj%fatal_error) return
          call ttrot_wrhist(obj)
          if(obj%fatal_error) return
        endif
        ! write a file mark before the data
        one=1
        i_err = tpioclnt_weof(obj%fd,one);
      else
        h3200=' '
        call string_cc2hh("SEGY",itype)
!       call tapeio_set_type(obj%fd,itype)
        if(obj%path_segy /= PATHCHECK_EMPTY) then
          call getlun(fh,istat)
          if(istat.ne.0)then
            write(obj%stdo,*)'TTROT-->Unable to get unit number for segy &
                             &header file'
            obj%fatal_error=.true.
            return
          endif
          open(fh,file=obj%path_segy,status='old',iostat=istat)
          if(istat.ne.0)then
             write(obj%stdo,*)'TTROT-->Unable to open segy header file'
             write(obj%stdo,*)'unit = ',fh,' status = ',istat
             write(obj%stdo,*)'file = ',obj%path_segy
             obj%fatal_error=.true.
             return
          endif
          i1=1
          i2=80
          DO i=1,40
            read(fh,'(A)',iostat=istat)card
            if(istat.lt.0)exit
            if(istat.ne.0)then
              write(obj%stdo,*)'TTROT-->Error reading segy header file'
              obj%fatal_error=.true.
              return
            endif
            write(obj%stdo,*)card
            h3200(i1:i2)=card
            i1=i1+80
            i2=i2+80
          ENDDO
        else
          do i = 0,39  !40 card images
            h3200(i*80+1:(i+1)*80)='C '
            write(h3200(i*80+1:i*80+3),'("C",i2)') i+1
            if(i==0) h3200(1:80)   ='C 1    CONOCO SEGY FILE'
            if(i==1) h3200(81:160) ='C 2    JOB:'//obj%jobname
            if(i==2) h3200(161:240)='C 3    IBM 32 BIT FLOAT'
            if(i==3) then
              write(card,'(''C 4    NDPT='',I6,'' DT='',F10.6)') &
               obj%nsout,obj%dt
              h3200(241:320)=card
            endif
            write(ttrot_stdo(),*) h3200(i*80+1:(i+1)*80)
          enddo
        endif
        nby=3200
        call cmem_cpy_c(buf,h3200,3200)
!!!        buf(1:nby/sizeof(i)) = transfer(h3200(1:3200),buf(1:3200/sizeof(i)))
        call wrdc_asc_ebc_c(buf, nby)
        nw = tpioclnt_write(obj%fd,buf,nby)
        if(nw<=0) then
          obj%fatal_error = .true.
        endif
        buf=0.
        binhdr%reno=0
        binhdr%format=1
        if(obj%dt < 0.032) then
          binhdr%hdt  = nint(obj%dt*1000000)
        else
          binhdr%hdt  = nint(obj%dt*1000)
        endif
        binhdr%hns  = obj%nsout
        binhdr%tsort= 0
        binhdr%mfeet= 1
        if(obj%survey_units=='FEET') binhdr%mfeet=2
        nby=400
        i_err = segy_binh2buf(binhdr,buf)
        if(i_err/=0) then
          write(obj%stdo,*)'TTROT-->Error from segy_binh2buf = ',i_err
          obj%fatal_error = .true.
          return
        endif
        nw = tpioclnt_write(obj%fd,buf,nby)
        if(nw<=0) then
          obj%fatal_error = .true.
        endif
      endif
      return
      end subroutine ttrot_wr_predata
!
      subroutine ttrot_wrhist(obj)
      implicit none
      type(ttrot_struct),intent(inout) :: obj       ! arguments
      character,pointer :: cards(:)*80



      integer,pointer   :: buf(:)
      integer  :: tpioclnt_write
      integer  :: nwrds,nby,nw,i_err,i
      integer  :: ncards,nhcards,nhist,maxcards
!
! allocate temporary buffers
      if(obj%fd < 0 .or. obj%skip_wrapup) return
      call manhist_get_info(nhist,maxcards,obj%history)
      allocate(cards(maxcards),stat=i_err)
      if(i_err/=0) then
        write(obj%stdo,*) 'TTROT_WRHIST: cards allocation failed'
        obj%fatal_error=.true.
        return
      endif
      nwrds = (maxcards*80)/sizeof(nwrds)
      allocate(buf(nwrds),stat=i_err)
      if(i_err/=0) then
        write(obj%stdo,*) 'TTROT_WRHIST: buf allocation failed'
        deallocate(cards)
        obj%fatal_error=.true.
        return
      endif
!
! add a ttrot card to current history
      write (cards(1),'(1X,'' REEL='',A6,'' PDN='',A)')  &
       obj%volser(1:6),trim(obj%volume(obj%oreel))
      i_err = hist_write(obj%ipn,cards(1))
!
      
!
! read in history records and dump to tape
      do i = 1,nhist
        call manhist_gethistory(i,cards,ncards,nhcards,obj%history)
        ncards=ncards+nhcards
        if(ncards.gt.maxcards)ncards=maxcards
        if(ncards>0) then
          nwrds = 80/sizeof(nwrds)
          call cmem_cpy_c(buf,cards,ncards*nwrds*obj%nbytesnword)
!!!          buf(1:ncards*nwrds) = transfer(cards(1:ncards),&
!!!           buf(1:ncards*nwrds))
          nby = ncards*80
          nw = tpioclnt_write(obj%fd,buf,nby)
          if(nw<= 0) obj%fatal_error = .true.
        endif
      enddo
!
      deallocate(cards)
      deallocate(buf)
      return
      end subroutine ttrot_wrhist
!
      subroutine ttrot_wr_label(obj)
      implicit none
      type(ttrot_struct) :: obj       ! arguments
      integer ivol(2),iuser(4),ipdn(13),neof
      neof=1
      call string_cc2hh(obj%userid,iuser)
      call string_cc2hh(obj%volser,ivol)
      call string_cc2hh(obj%volume(obj%oreel),ipdn)
!     call tapeio_wr_label(obj%fd,ivol,iuser,ipdn)
      return
      end subroutine ttrot_wr_label
!
      subroutine ttrot_change_reel(obj)
      implicit none
      type(ttrot_struct) :: obj
      obj%mntswo=1  !force a reel change
      return
      end subroutine ttrot_change_reel
      subroutine ttrot_wrheader(obj)
!
! write a tape header record that self describes the data.
! parameters encoded are:
! format= tape type (CPS)
! ndpt  = number of samples per trace
! tstrt = start time of trace in seconds
! dt    = sample rate of trace in seconds
! nwih  = number of words in a trace header
! nbits = number of bits per trace-header word
! nbits_hd = number of bits per eader word
! history(ALL/CURRENT/BRIEF/NONE)
! endian(1-sun/0-intel)
! wdtype= output wordtype of trace data(IEEE,IBM,INT,CMPI - see trcio.f90)
! origin and rotation matrix parameters
      implicit none
      type(ttrot_struct) :: obj       ! arguments
      integer   ::  lastc,nw,nby,tpioclnt_write
      character ::  line*80
      character ::  cbuf*1024
      cbuf=' '
      lastc = 1
      write(line,'(a)')  '#<CPS_v1 type=cps_tape />'
      cbuf(lastc:) = line
      write(line,'(a,i6)')    '# ndpt=',obj%nsout
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,f14.7)') '# tstrt=',obj%tstrt
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,f14.7)') '# dt=',obj%dt
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,i3)')    '# nwih=',obj%nwih
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,a)')     '# history=',obj%history
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,a)')     '# wdtype=',obj%wdtype
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,i4)')    '# nbits=',obj%nbits
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,i4)')    '# nbits_hd=',obj%nbits_hd
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,i2)')    '# endian=',obj%endian
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,g16.8)') '# xorigin=',grid_get_xorigin(obj%grid)
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,g16.8)') '# yorigin=',grid_get_yorigin(obj%grid)
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,g16.8)') '# dx11=',grid_get_dx11(obj%grid)
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,g16.8)') '# dx21=',grid_get_dx21(obj%grid)
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,g16.8)') '# dx12=',grid_get_dx12(obj%grid)
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,g16.8)') '# dx22=',grid_get_dx22(obj%grid)
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      write(line,'(a,i3)') '# tpr=',obj%tpr
      lastc = len_trim(cbuf) + 2
      cbuf(lastc:)= trim(line)//char(10)
      lastc = len_trim(cbuf) + 2
  
      nw = 1024/sizeof(obj%pak(1))
      call cmem_cpy_c(obj%pak,cbuf,nw*obj%nbytesnword*2)
!!!      obj%pak(1:nw) = transfer(cbuf(1:1024),(/0.0/))
      nby = 1024
      nw = tpioclnt_write(obj%fd,obj%pak,nby)
      if(nw <= 0) then
        write(ttrot_stdo(),*) &
       'TTROT_WRHEADER: --FAILURE TO WRITE CPS GLOBAL TAPE RECORD --'
       obj%fatal_error=.true.
       return
      endif
      write(ttrot_stdo(),*) &
       'TTROT_WRHEADER: --CPS GLOBAL TAPE RECORD WRITTEN--'
      return
      end subroutine ttrot_wrheader
!
! write processing records(if any) to tape
! New CPS records are ascii, and old CPS records are EBCDIC
      subroutine ttrot_wrprec(obj)
      implicit none
      type(ttrot_struct) :: obj       ! arguments
      integer   pak(3000)
      character(len=120) :: cpak(100)
      character(len=150) :: line,prname
      integer   i_err,nby,nw,nwrds,lun,cno
      integer   i2
      integer   :: lsiz=120
      integer   tpioclnt_write
      integer :: fn_lun,istat,ps_lun
      logical :: finished
!     equivalence (pak,cpak)
 
      if(obj%fd <0) return
!
! fetch ntp files from the vax
! stream processing records to the tape currently mounted

      call getlun(lun,i_err)
      if(i_err/=0) then
        goto 88
      endif
      call getlun(ps_lun,i_err)
      if(i_err.ne.0)go to 88
      open(ps_lun,file=obj%path_ps,status='old',iostat=istat)
      if(istat.ne.0)then
        write(plun,*)'TTROT_wrprec-->Unable to open path_ps file'
        obj%fatal_error=.true.
        return
      endif

      call getlun(fn_lun,istat)
      if(istat.ne.0)then
        write(plun,*)'TTROT-wrprec-->Unable to get unit number for &
                     &%trin_filenames'
        obj%fatal_error=.true.
        return
      endif
      open(fn_lun,file='%trin_filenames',status='old',iostat=istat)
      if(istat.ne.0)then
        write(plun,*)'TTROT-wrprec-->Unable to open &
                     &%trin_filenames'
        obj%fatal_error=.true.
        return
      endif
      finished=.false.

      DO 
        cno=1
        line =' '

 48     continue
        cpak=' '
        if(finished)exit
        if(cno == 1) then !start
          read(ps_lun,'(A)',iostat=istat)prname
          if(istat.lt.0)then
             prname='./%trin_filenames'
             finished=.true.
          endif
          open(unit=lun,file=prname,status='old',err=90)
          goto 66
 90       continue
          cno=0
          write(plun,*) &
           'TTROT_WRPREC: OPEN ERROR, CHECK THE FILE NAMES?'
          write(ttrot_stdo(),*) &
            'TTROT_WRPREC: FILE NAME?',prname
          cycle
 66       line='PR='//trim(prname)//',NCHPL=120'
          i2 = index(line,',NCHPL')
          if(i2<=0) i2=68
          write(ttrot_stdo(),*) 'TTROT_WRPREC:PROCESS RECORD=',line(4:i2-1)
          cno = 1
          cpak(cno) = line
          cno = cno+2            ! two header lines
        else                     !continuation
          line(1:67)='PR=CON'
          cno = 1
          cpak(cno) = line
          cno = cno+2            ! two header lines
        endif

 50     continue             !read loop
        read (lun,fmt='(a120)',end=52,err=92) cpak(cno)
        if(cno >= 100) then  !dump filled buffer to tape
          nby=cno * lsiz
          nwrds = nby/sizeof(nby)
          call cmem_cpy_c(pak,cpak,nwrds*obj%nbytesnword)
!!!          pak(1:nwrds) = transfer(cpak(1:cno),pak(1),nwrds)
          nw = tpioclnt_write(obj%fd,pak,nby)
          if(nw <= 0) then
             go to 93
          endif
          goto 48
        else
          cno = cno+1
          goto 50
        endif

 52     continue               !end of file hit, final buffer flush

        cno=cno-1
        nby=cno*lsiz
        nwrds = nby/sizeof(nby)
        call cmem_cpy_c(pak,cpak,nwrds*obj%nbytesnword)
!!!        pak(1:nwrds) = transfer(cpak(1:cno),pak(1),nwrds)
        nw = tpioclnt_write(obj%fd,pak,nby)
        if(nw <= 0 ) then
           go to 93
        endif

        close (lun)
      ENDDO
      return

 88   continue
      write(ttrot_stdo(),*) 'TTROT_WRPREC: GETLUN ERROR '
      return
 92   continue ! read error
      write(ttrot_stdo(),*) 'TTROT_WRPREC: READ ERROR '
      close (lun)
      return
 93   continue ! write error
      write(ttrot_stdo(),*) 'TTROT_WRPREC: WRITE ERROR '
      obj%fatal_error = .true.
      close (lun)
      end subroutine ttrot_wrprec

!  Since ttrot_build_pdns uses object, it must only be called from traps.
      subroutine ttrot_build_pdns
!        PDN Descriptor = MED.USERID.PROJECT.SUB_PROJECT.TASK.FORMAT.COUNTER

      character(len=4)          :: cnt
      character(len=48),pointer :: tempvol(:)
      integer                   :: i  
      integer,pointer           :: tempnum(:)

      nullify (tempvol) ! jpa
      nullify (tempnum) ! jpa

      if(object%fill_rows.eq.'NO')return
      call array_alloc(tempvol,object%num_rows)
      call array_alloc(tempnum,object%num_rows)
      do i=1,object%num_rows

        tempvol(i)=trim(object%media) // '.'
        tempvol(i)=trim(tempvol(i)) // trim(object%userid) // '.'
        if(object%project.ne.' ')tempvol(i)=trim(tempvol(i)) // &
                                 trim(object%project)&
                                 // '.'
        if(object%sub_project.ne.' ')tempvol(i)=trim(tempvol(i)) &
           // &
           trim(object%sub_project) // '.'
        if(object%task.ne.' ')tempvol(i)=trim(tempvol(i)) // &
                              trim(object%task) // '.'
        tempvol(i)=trim(tempvol(i)) // trim(object%format) // '.'
        call string_ii2cc(i,cnt)
        tempvol(i)=trim(tempvol(i)) // trim(cnt)
        call string_to_lower(tempvol(i))
        call string_squeeze_blanks(tempvol(i))
        if(i.gt.object%nreel)then
          tempnum(i)=99999999
        else if(object%num_tr(i).le.0)then
          tempnum(i)=99999999
        else
          tempnum(i)=object%num_tr(i)
        endif

      enddo
      call pc_put('volume',tempvol ,object%num_rows )
      call pc_put('num_tr',tempnum ,object%num_rows )
      call pc_alloc('volume',object%volume,object%nreel)
      call pc_alloc('num_tr',object%num_tr,object%nreel)


      end subroutine ttrot_build_pdns
!
!
      subroutine ttrot_catalogue_add(obj, vol, i_err)
      implicit none
      type(ttrot_struct),intent(inout) :: obj       ! arguments
      character(len=*),intent(in)      :: vol       ! arguments
      integer                          :: i_err
      character  :: req*120,msg*240
      character  :: pdn*48,exptime*3
      character  :: line*80

      i_err= 0
      write(ttrot_stdo(),*) 'TTROT: catalogue_add for vol=',vol
      if(vol==' ' .or. vol(1:3)=='SCR' .or. vol(1:4)=='NSCR' &
         .or. vol(1:4)=='BSCR') then
        write(ttrot_stdo(),*) 'TTROT: invalid volser for cataloguing'
        return
      endif
      pdn=obj%volume(obj%oreel)
      if(len_trim(obj%volume(obj%oreel)) < 7 ) then
        pdn='BLANK'
      endif

      if ((obj%retention < 0) .or. (obj%retention > 999)) then
        write(line, '(''TTROT_CATALOGUE_ADD:  illegal retention'', i6)') &
           obj%retention
        call pc_info(line)
        i_err = -1
        return
      else if (obj%retention < 10) then
        write(exptime, '(i1)') obj%retention
      else if (obj%retention < 100) then
        write(exptime, '(i2)') obj%retention
      else
        write(exptime, '(i3)') obj%retention
      endif

! does check 1st to see if volser is already catalogued
! will only enter once, and retain the old information
      req = 'ADDVOL='//vol(1:6)//' pdn='//trim(pdn)//' userid='&
     &//trim(obj%userid)//' retention='//trim(exptime)

      if (tcatclient_query(req, msg) /= 1) then
        call pc_info('TTROT_CATALOGUE_ADD: catalogue server error!!')
        i_err = -1
        return
      endif

      if(index(msg,"SERVER FATAL ERROR") /= 0) then
        call pc_info('TTROT_CATALOGUE_ADD: catalogue server error!!')
        i_err = -1
        return
      endif

      if(index(msg,"ADDVOL=N") /=0) then
!!        write(ttrot_stdo(),*) 'TTROT: catalogueing failure'
        write(ttrot_stdo(),*) 'TTROT: tape already catalogued'
        write(ttrot_stdo(),*) 'TTROT: volser=',vol
        i_err = -1
      endif

      return
      end subroutine ttrot_catalogue_add
!
      integer function ttrot_iscat(obj,vol,userid,i_err)
      implicit none
      type(ttrot_struct) :: obj       ! arguments
      character(len=*),intent(inout) :: vol
      character(len=*),intent(out)   :: userid
      integer                        :: i_err
      integer      :: ip
      character    :: req*80,msg*80
      character    :: line*80

      i_err=0
      ttrot_iscat = 0
      userid=' '
      if(vol=='SCR' .or. vol(1:4)=='NSCR' .or. vol(1:4) == 'BSCR' &
        .or. vol == ' ') return
      req = 'ISCAT='//vol

      if (tcatclient_query(req, msg) /= 1) then
        call pc_info('TTROT_ISCAT: catalogue server error!!')
        i_err = -1
        return
      endif

      if(index(msg,"SERVER FATAL ERROR") /= 0) then
        call pc_info('TTROT_ISCAT: catalogue server error!!')
        i_err = -1
        return
      endif
      ip = index(msg,'ISCAT=Y')
      if(ip /= 0) then
       ttrot_iscat=1
       ip = index(msg,'userid=')
       if(ip /=0) then
         userid = msg(ip+7:ip+14)
       endif
      else
       if(obj%tape_host.ne.'jaws'.and.obj%tape_host.ne.'hnomega1')then
         write(line,'(''TTROT: vol='',A6,'' is not in the catalogue'')') &
          vol(1:6)
         call pc_info(line)
         write(line,'(''TTROT: this is a foreign tape?'')')
         call pc_info(line)
       endif
      endif

      return
      end function ttrot_iscat
!
      subroutine ttrot_get_vol(fd,vol)
      implicit none
      integer,intent(in) :: fd
      character(len=*) :: vol
!!!      character*(*) vol
      integer :: ivol(8),tpioclnt_gvol,status
      vol=' '
      ivol(1)=0
      status =  tpioclnt_gvol(fd,ivol)
      call string_hh2cc(ivol,vol)
      return
      end subroutine ttrot_get_vol

      subroutine ttrot_permsave(obj,finished)
      type(ttrot_struct),intent(inout) :: obj 
      integer, intent(in) :: finished

      integer :: i,istat,nskip,rate
      integer,save :: lab,kntvol=0
      logical :: there
      character(len=10) :: cdate
      character(len=20) :: clinemin,clinemax,cshotmin,cshotmax,ctime,crate
      character(len=20) :: cknttr
      character(len=40) :: line
      character(len=80) :: cdum1,cdum2
      character(len=160) :: kmd
      character(len=500) :: cdir,fullpath

      inquire(file='permsave.label',exist=there)
      if(.not.there)then
        call getlun(lab,istat)
        if(istat.ne.0)then
          write(plun,*)'Unable to get unit number for label file'
          write(plun,*)'No labels made'
          return
        endif
        open(lab,file='permsave.label',status='new',iostat=istat)
        if(istat.ne.0)then
          write(plun,*)'Unable to open file permsave.label'
          write(plun,*)'No labels made'
        endif
      endif

      write(plun,9001)' permsave label'
      write(plun,9001)' *****************************************'

      line=' '
      do i=1,3
        write(lab,9001)line
      enddo

      kntvol=kntvol+1
      line='Tape:'
      line(7:)=trim(obj%vols(kntvol))
      write(lab,9001)line
      write(plun,9001)line

      if(obj%label_type.eq.'DOMESTIC')then
        line='Quad: '
        line(7:)=trim(obj%quad)
        write(lab,9001)line
        write(plun,9001)line
      else
        line='Country:'
        line(10:)=trim(obj%country)
        write(lab,9001)line
        write(plun,9001)line
        line='Project:'
        line(10:)=trim(obj%project_name)
        write(lab,9001)line
        write(plun,9001)line
      endif

      line='Line: '
      if(obj%hdr_line.eq.0)then
        line(7:)=trim(obj%line_names)
      else
        call string_ff2cc(obj%linemin,clinemin,ndec=1)
        call string_ff2cc(obj%linemax,clinemax,ndec=1)
        line(7:)=trim(clinemin) // '-' // trim(clinemax)
      endif
      write(lab,9001)line
      write(plun,9001)line

      line='SP''s:'
      if(obj%hdr_shot.eq.0)then
        line(7:)=trim(obj%shotpoints)
      else
        call string_ff2cc(obj%shotmin,cshotmin,ndec=1)
        call string_ff2cc(obj%shotmax,cshotmax,ndec=1)
        line(7:)=trim(cshotmin) // '-' // trim(cshotmax)
      endif
      write(lab,9001)line
      write(plun,9001)line

      line='Contr: '
      line(8:)=trim(obj%contractor)
      line(19:)='Dir:'
      line(24:)=trim(obj%dir)
      write(lab,9001)line
      write(plun,9001)line

      line='Process: '
      line(10:)=trim(obj%processes)
      write(lab,9001)line
      write(plun,9001)line

      line='Time: '
      call string_ff2cc(obj%time,ctime)
      line(7:)=trim(ctime)
      line(19:)='BPI: 3590'
      write(lab,9001)line
      write(plun,9001)line

      line='Rate: '
      rate=nint(obj%dt*1000.0)
      call string_ii2cc(rate,crate)
      line(7:)=trim(crate)
      line(12:)='Nt:'
      call string_ii2cc(obj%knttr,cknttr)
      line(16:)=trim(cknttr)
      write(lab,9001)line
      write(plun,9001)line

      line='Format: CPS'
      write(lab,9001)line
      write(plun,9001)line

      line='Date:'
      call string_date(cdate)
      line(7:)=cdate
      write(lab,9001)line
      write(plun,9001)line

      line='Comment:'
      line(10:)=trim(obj%comment_label)
      write(lab,9001)line
      write(plun,9001)line
      write(plun,9001)'**************************************'
      write(plun,9001)' '

      line=' '
      nskip=6
      if(obj%label_type=='DOMESTIC')nskip=5
      do i=1,nskip
        write(lab,9001)line
      enddo
      if(finished.eq.0)return

      close(lab)

!         copy the file to the current directory

!         See if label file already exists
      call getsys_netinfo(cdum1,cdum2,cdir)
      fullpath=trim(cdir) // trim(obj%jobname) // '.label'
      inquire(file=fullpath,exist=there)
      if(there)then
        kmd='cat permsave.label >> ' // trim(fullpath)
      else
        kmd='cp permsave.label ' // trim(fullpath)
      endif
      call putsys_cmd(kmd,istat)
      if(istat.ne.0)then
        write(plun,*)'TTROT-->Error writing permsave.label file'
      endif


      kmd='rm -f permsave.label'
      call putsys_cmd(kmd,istat)
      if(istat.ne.0)then
        write(plun,*)'TTROT-->Unable to delete permsave.label file'
      endif
      

 9001 format(A)
      end subroutine ttrot_permsave
!
      integer function ttrot_get_hosts() result(cnt)
      integer   :: lservers(200)
      integer   ::n
      integer   ::i1,i2
      integer   :: tpioclnt_get_servers
      
      cnt = ttrot_server_cnt
      if(.not. ttrot_first_call) return
      ttrot_first_call = .false.
      n = 0
      ttrot_servers = ' '
      ttrot_server_cnt = tpioclnt_get_servers(lservers)
      cnt = ttrot_server_cnt
      do n = 1,min(cnt,size(ttrot_servers))
        i1 = (n-1)*24/4 + 1
        i2 = i1 + 24/4
        call string_hh2cc(lservers(i1:i2),ttrot_servers(n))
      enddo
      end function ttrot_get_hosts


      end module ttrot_module
