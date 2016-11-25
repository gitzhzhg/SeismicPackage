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
! Name       : TTRIN
! Category   : io
! Written    : 2000-07-28   by: RSDay
! Revised    : 2006-11-27   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Tape Input of Seismic Traces from magnetic tape
! Portability: Unix Operating Systems
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! TTRIN always starts a new loop within a CPS job.  If more than one TTRIN is 
! present in a job each starts its own loop and ignores traces from a previous 
! loop if one is present.
!
! TTRIN reads all the traces sequentially from the first PDN or tape specified
! then reads the traces from the second PDN or tape, etc.  An optional do-skip 
! selection may be specified for each PDN or tape to be read.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Tape Formats
!
! In this new version of TTRIN the DEBE, ISP and USER-defined tape formats are
! not supported.  These formats, and many others, can be read by the Seismic 
! Support operators and copied to disk or other tape formats.
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
! VOLUME arrays based on the entry in the last row.  Values of the first 5 PDN
! elements are repeated and a counter is incremented by 1 for each row.  The 
! alpha part of VOLUME is repeated and the numeric part is incremented by 1 for
! each row.  FILL_ROWS is a front-end only parameter.
!
!
! SEGY Header Word Mapping
!
! Parameters are provided that allow for customized mapping of CPS header words 
! from SEGY tapes.  Normally these will not be required and the default will 
! suffice.  The default is applied first and only the headers specified in the 
! custom mapping will be changed.
! 
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a trace supplying process.
!
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process outputs one trace at a time.
!
! This is a trace-supplying process.
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
! NWIH     number of words in trace header       used but not changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
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
! 1       Sequential Trace Count     Renumbered.
! 
! All other header words are read from the tapes.  SEGY tape option allows 
! mapping of SEGY headers to CPS headers.  Some formats may carry a restricted 
! amount of header information. 
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author     Description
!     ----        ------     -----------
! 71. 2006-11-27  Stoeckley  Replace pc_register_tab_group w HelpSection line;
!                             fix error in HelpSection to fix a gui problem in
!                             SeisSpace.
! 70. 2006-08-24  D. Glover  Added NULLIFY statements for Intel compiler.
! 69. 2006-06-20  Stoeckley  Add call to pc_register_tab_group for SeisSpace.
! 68. 2006-06-06  Stoeckley  Add call to pc_register_array_names for SeisSpace.
! 67. 2006-03-01  Goodger    Remove unused variables.
! 66. 2005-12-01  Goodger    Check flag to determine if header record has
!                            been found.
! 65. 2005-05-17  Goodger    Add 3592.
! 64. 2005-03-15  Goodger    Fix bug which would not allow a processing
!                            record to be in the first 6 records.
! 63. 2005-02-24  Goodger    Replace transfer function with cmem_cpy_c.
! 59. 2004-09-20  Goodger    Attempt a forced tape unload after a failure to
!                            open problem.
! 58. 2004-01-19  Goodger    Change tape_host in Alaska from mink to jaws.
! 57. 2003-12-19  Goodger    Insure tape_host is hoeplgp01 for LTO tapes.
! 56. 2003-12-16  Goodger    Add segy tape dump option.
! 55. 2003-12-05  Goodger    Do not allow blanks in VOLUME or any volume
!                            fields.
! 54. 2003-11-20  Goodger    Change media default to 3590HD.  Change 
!                            tape_host default to odi90.
! 53. 2003-10-31  Goodger    Insure mink tape_host option if Alaska.
! 52. 2003-10-22  Goodger    Check for media type 3590hd in pdn.  Force 
!                            tape_host to be odi90 in that case.  Activated
!                            end session trap.  One existed, but it was not
!                            being called.
! 51. 2003-10-10  Goodger    Write out control card tapequeue which will be 
!                            read by the job builder.  This change corresponds
!                            to buildjob.f90 revision 129.
! 50. 2003-10-06  Goodger    Added a trap for the tape_host parameter.  Default
!                            was changed from hoepodi90 to odi74.
! 49. 2003-09-29  RSDay      Changed close proceedure .
! 48. 2003-08-05  Goodger    Insure tape_host default is odi74.
! 47. 2003-07-30  RSDay      Added tape_host gui component for odi74 and odi90
!                            access. Added ttrin_get_hosts.
! 46. 2003-05-29  RSDay      3590HD added to media list(high density 3590)
! 45. 2003-05-01  RSDay      Fix for extra EOF after tape label. This caused
!                            problems with the file count and confusion over
!                            where data started.
! 44. 2003-03-25  RSDay      Added more error checking to ttrin_save_prec.
! 43. 2003-03-12  RSDAY      Fixed bug when last tape record had < tpr traces.
!                            Fixed initial skip bug that occured for tpr > 1.
!                            Start using tpioclnt_asread to do asynch read 
!                            aheads.
! 42. 2003-02-14  RSDay      Eliminated fixed array in ttrin_new_cps_tcf.
! 41. 2003-02-10  RSDay      added media types 3480, LTO,and NR3590. Added
!                            support for CPS input tapes with multiple
!                            traces per record(TPR format). ttrin_new_cps_tcf
!                            added to unpack traces for TPR format.
! 40. 2002-10-10  Schmauch   Remove Ponca City specific configuration info.
!                            Rely on tapecnfg.h.
! 39. 2002-09-25  Schmauch   Allow upper case if volume is typed in.
! 38. 2002-09-25  Schmauch   Made ttrin pdn building identical to ttrot.
! 37. 2002-08-12  Goodger    Change default on TOT_DO to 99999999.
! 36. 2002-07-30  Goodger    Read and output velocity model permsaves.
! 35. 2002-04-18  Schmauch   Always byte swap before =.
! 34. 2002-02-04  Goodger    Make volume gui field as large as TTROTs.
! 33. 2002-02-04  Goodger    Detect if there is a garbage card on permsave
!                            records, and if so skip it, to compensate for a
!                            ttrot bug.  Use fortran to write out records
!                            rather than cio to avoid extent problems.
!                            Fix warnings generated by intel compiler.
! 32. 2001-10-18  Schmauch   Use tcatclient module for all tapecat
!                            communication.
! 31. 2001-09-10  Schmauch   Fix logical bug with tot_do and multiple tapes.
! 30. 2001-08-27  Goodger    Fix bug causing header loss on cray tapes.
! 29. 2001-08-03  Goodger    Replace history module with manhist module.
! 28. 2001-07-10  Schmauch   Argument list to tcclient_recv changed. PRODUCTION.
! 27. 2001-04-04  R.S.Day    More information to stdout about reel switches.
!                            Stopped mount of extra reel in PDN when trace
!                            count is exceeded.
! 26. 2001-03-08  R.S.Day    Fixed logic with header count when history was
!                            missing for CPS tapes.
! 25. 2001-02-06  R.S.Day    allowed process record path name increased to 80
! 24. 2001-02-01  R.S.Day    Added DLT support.
!                            Added option BLK-NSC to read in multi-volume
!                            BLKD tape format. More diagnostic print.
! 23. 2001-01-18  R.S.Day    Restore of process records no longer uses rcp
!                            to place file in front end directory.
! 22. 2001-01-09  Selzler    Added support for CPS-NSC (Multi-reel tapes
!                            created on Network Supercomputer Center).
!                            Moved tcclient_halt call so gui does not open
!                            too many sockets to tapecat server.
! 21. 2001-01-04  RSDay      No longer throwing away empty lines in perm save
!                            records. Fixed problem with a card being dropped
!                            from PR=CON perm save records.
! 20. 2000-12-14  RSDay      Converted to skip_wrapup flag
! 19. 2000-12-07  RSDay      Fixed bug in perm save record restore that
!                            was creating blank cards on output.
! 18. 2000-11-28  RSDay      trimmed line feed from perm save records to
!                            compensate for cio_fputline adding a line feed
!                            fixed problem with unix file names for perm
!                            save records.
! 17. 2000-11-15  RSDay      replaced wrdc_ibm_to_float with
!                            wrdc_ibm_to_float_c for CPS reads
! 16. 2000-11-10  RSDay      Fixed tail mute setting. Was not set by mutehw.
! 15. 2000-10-25  RSDay      Fixed problem reporting label on non labeled tapes.
! 14. 2000-10-20  RSDay      Worked around an absoft bug on intels. The
!                            segy header parsing had problems until I replaced
!                            an explicit loop with array syntax
!                            i.e. thd(1:nwrd)=obj%rbuf(1:nwrd)
!                            changed gui behavior for segy header mapping.
! 13. 2000-10-18  RSDay      Fixed perm save files. Eliminated PR= card
!                            from the perm save files. Eliminated 1 column
!                            shift of the output card images and blank cards
! 12. 2000-10-11  RSDay      Fixed ttrin_getpdn logic for multi_reels.
!                            Slight change to ttrin_next_vol.
! 11. 2000-10-06  RSDay      Rnamed FILL to FILL_ROWS, and moved location
!                            of some gui fields. Desensitized DEFAULT.
!                            Removed LABEL field. Added ttrin_end_trap
! 10. 2000-10-02  RSDay      Added optional use of the clean module to
!                            catch bad data values. Eliminated unused pointer
!                            in the ttrin structure.
!  9. 2000-09-27  RSDay      Found another byte swapping problem on linux.
!                            This one was for old CPS tapes with IBM data.
!                            Also needed to invoke the optional swap
!                            parameter on segy_unpack_segyhd to correct linux
!                            problems. Fixed linux byte swapping for segy
!                            custom header mapping.
!  8. 2000-09-05  RSDay      Changed a pc_warning in update to pc_info.
!                            Corrected linux byte swapping for SEGY input.
!                            Redeleted COUNTER which somehow made it into the
!                            last update.
!  7. 2000-08-31  RSDay      Fixed a logic error in BLK format handling
!                            of multiple volumes. Also fixed the problem of
!                            BLK format passing out 1 less trace than
!                            requested. BLK format fixed to set lav and mute.
!                            Fixed linux byte swapping problem for BLK tapes.
!                            Removed irrelevant counter field in help section.
!                            The tpioclnt layer has replaced the direct calls
!                            to tapeio functions. TTRIN auto detects the node
!                            it is on and uses sockets if not on odi74.
!  6. 2000-08-23  RSDay      Increased the GUI field length for volume.
!                            Fixed a bug in ttrin_getpdn. Added the function
!                            ttrin_build_pdn, and added gui fields for project,
!                            sub_project,task, and userid. Rearranged some
!                            code to look more like ttrot. Fixed default
!                            behavior of tot_do. Added userid checking of
!                            catalogued volumes. jobname variable increased
!                            to 16 bytes. Sequential count set for header 1.
!                            Eliminated a real to integer transfer call
!                            since there is a bug in the transfer function.
!                            Added commented lines for future socket io calls.
!  5. 2000-08-17  RSDay      Fixed check of sample rate from history when
!                            format=BLK.
!                            Fixed bugs in buffer allocation for BLK format and
!                            in the logic in ttrin_blkd.
!                            Altered logic so BLK format will read in
!                            only tot_do traces from a reel. 
!  4. 2000-08-14  RSDay      Fixed up call to wrdc_ibm_to_float to
!                            avoid problem with passed buffer sizes.
!                            Using obj%nhdtape in place of nhdwd
!                            (eliminated) and introduced obj%ndpttape.The
!                            value of ndpttape is set by a call to history_rop
!                            Also changed the default wdtype behavior to IBM
!                            which corrects a problem reading in old CPS tapes.
!                            Fixed a problem that showed up with linux
!                            internal fortran reads of a character string
!                            when there were line feeds in the string
!  3. 2000-08-01  RSDay      Arguments of tapeio primitives changed to be
!                            consistent with pfio interface.
!  2. 2000-07-28  RSDay      Original
!  1. 1999-11-19  CI Burch   Initial redesign of new process 
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
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! 
! tpioclnt functions called by ttrin
! tpioclnt_open_server     tpioclnt_read     tpioclnt_close
! tpioclnt_gvol     tpioclnt_peof
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<gui_def>
!<NS TTRIN Process/NC=80>
!          Read seismic traces from magnetic tape.
!
! COMMENT=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! FORMAT= `CCC      MEDIA=~~~~`CCCCC  HISTORY=`CC  VERSION=`CCCCCCCC
!
! REST_PS=`CC    1CLEAN=`CCCC  TAPE_HOST=`CCCCCCCC
!
! USERID=`SSSSSSS PROJECT=`SSSSSSSSS SUB_PROJECT=`SSSSSSSSS TASK=`SSSSSSSSS
!
! DEFAULTS=`CCC  FILL_ROWS=`CCC  NUM_ROWS=`III
!
! VOLUME                                          TOT_DO SKIP_INIT NUM_DO NUM_SKIP
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS`IIIIII`IIIIIIII`IIIIII`IIIIIIII
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS`IIIIII`IIIIIIII`IIIIII`IIIIIIII
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS`IIIIII`IIIIIIII`IIIIII`IIIIIIII
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS`IIIIII`IIIIIIII`IIIIII`IIIIIIII
!
!   `- Segy Related Parameters-----------------------------------------------------+
!
!    MESS_SEGY=`CC   IG_EOF=`CC   MOD_SEGY=`CC     SBYTE BYTES CPS_HDR WTYPE         
!                                                  `IIIII`IIIII`IIIIIII`SSSS
!    DUMP_SEGY=`CC                                 `IIIII`IIIII`IIIIIII`SSSS
!                                                  `IIIII`IIIII`IIIIIII`SSSS
!                                                  `IIIII`IIIII`IIIIIII`SSSS
!
!   `------------------------------------------------------------------------------+
!
!<PARMS COMMENT[/ML=120/XST]>
!<PARMS VOLUME_ARRAYSET[/XST/YST]>
!<PARMS SBYTE_ARRAYSET[/XST/YST]>
!<NS Vmod_Permsave/NC=80>
!
!
! PATH_VMOD=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!<PARMS PATH_VMOD[/ML=128/XST]>
!</gui_def>

!<HelpSection>
!
!<Help KEYWORD="LABEL">
!<Tip> Type of label on the tapes being read. </Tip>
! Default = varies
! Allowed = ANSI  (ANSI labeled)
! Allowed = IBM   (IBM standard labeled)
! Allowed = NONE  (non-labeled)
! Normally the user should accept the default. This parameter is provided only 
! for the rare circumstance when the default must be overridden.  (All tape 
! inputs for a single TTRIN must have the same label type.)
!</Help>
!
!<Help KEYWORD="COMMENT">
!<Tip> Not Active-A comment to pass along to the tape operators. </Tip>
! Default = 
! Allowed = char < 120
! The comment field will be stored with the other parameters in the work file.
! It is meant to be a comment for the tape operators. At present it is
! not passed to the operators, and only serves as a document card.
!</Help>
!
!<Help KEYWORD="FORMAT">
!<Tip> Format of the tapes being read. </Tip>
! Default = CPS
! Allowed = CPS   (one trace per read)
! Allowed = BLK   (one block per read - faster read)
! Allowed = SEGY
! Allowed = CPS-NSC  (one trace per read, headers on 1st volume only)
! Allowed = BLK-NSC
! NA-Allowed = DIGI  (Digicon internal format)
! NA-Allowed = CONI  (Conoco Internal format)
! NA-Allowed = CON3  (Conoco Internal 3D format)
! NA-Allowed = SEGD  (SEGD-8084, 2D, 32 bit data, 10 16 bit headers)
! NA-Allowed = SERC  (SERCEL SEGD-8058, 32 bit IEEE)
! NA-Allowed = 8048  (SEGD-8048, no headers read)
! NA-Allowed = 8058  (SEGD 20 byte header, no headers read)
! Options SEGD and SERC write files containing RP and PP data for the CFG 
! geometry process.  These files are written to the current directory.
!</Help>
!
!<Help KEYWORD="MEDIA">
!<Tip> Media type of the tapes being read. </Tip>
! Default = 3590HD
! Allowed = 3590  (Magstar cartridges)
! Allowed = 3480
! Allowed = 8mm
! Allowed = DLT
! Allowed = NR3590
! Allowed = LTO
! Allowed = 3592
! 3590 and 3592 are IBM that will be handled by the IBM3494 tape robot.
! NR3590 is a non-robotic manually loaded IBM 3590 tape.
!</Help>
!
!
!<Help KEYWORD="HISTORY">
!<Tip> Whether to transfer histories from the input tapes. </Tip>
! Default = YES
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="CLEAN">
!<Tip> Controls checking for bad data values. </Tip>
! Default = NONE
! Allowed = NONE/ZERO/KILL
! NONE ... No checking for NAN or INF.
! ZERO ... Zero values that are NAN or INF
! KILL ... Kill whole trace if a NAN or INF is found.
!</Help>
!
!<Help KEYWORD="VERSION">
!<Tip> Customized versions to deal with particular vendor tapes. </Tip>
! Default = NONE
! Allowed = NONE      (No custom tape read)
! Allowed = WESTERN1  (Western Geophysical multi-file segy format)
!</Help>
!
!<Help KEYWORD="REST_PS">
!<Tip> Whether to restore records from permanent save tapes. </Tip>
! Default = YES
! Allowed = YES
! Allowed = NO
! If REST_PS = YES, then restore files to current directory and print filenames
! in .rpt file. The files will be copied back to the node and directory where
! the job was submitted. Make sure your rhosts file is set properly!
!
! If REST_PS = NO, then do not restore files but print filenames in .rpt file.
!</Help>
!
!<Help KEYWORD="MESS_SEGY">
!<Tip> Whether to print a diagnostic message regarding SEGY file size. </Tip>
! Default = NO
! Allowed = YES/NO  
!</Help>
!
!<Help KEYWORD="IG_EOF">
!<Tip> Whether to ignore single EOF marks on SEGY inputs. </Tip>
! Default = NO
! Allowed = YES/NO 
! If IG_EOF = NO, then TOT_DO parameter controls when to stop reading traces.  
!</Help>
!
!<Help KEYWORD="DUMP_SEGY">
!<Tip> An integer dump of the segy headers. </Tip>
! Default = NO
! Allowed = YES/NO 
!</Help>
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
! Default = BLANK
! Allowed = char (10) 
!</Help>
!
!
!                     ----SEGY Header mapping---
!
!<Help KEYWORD="MOD_SEGY">
!<Tip> Whether to modify default mapping of SEGY headers to CPS headers. </Tip>
! Default = NO
! Allowed = YES/NO
! If MOD_SEGY = YES, parameters for specifying the SEGY to CPS header mapping 
! are activated.
!</Help>
!
!<Help KEYWORD="SBYTE">
!<Tip> Starting byte of the SEGY header to be moved. </Tip>
! Default = -
! Allowed = int > 0 (linked array)
! Active if MOD_SEGY = YES.
!</Help>
!
!<Help KEYWORD="BYTES">
!<Tip> Number of bytes to be moved to a CPS header. </Tip>
! Default = -
! Allowed = int > 0 (linked array)
! Active if MOD_SEGY = YES.
!</Help>
!
!<Help KEYWORD="CPS_HDR">
!<Tip> CPS header word to receive the SEGY header. </Tip>
! Default = -
! Allowed = int > 0 (linked array)
! Active if MOD_SEGY = YES.
!</Help>
!
!<Help KEYWORD="WTYPE">
!<Tip> Treat Segy header word as I(integer), or F(floating point). </Tip>
! Default = -
! Allowed = char(8) (linked array)
! Active if MOD_SEGY = YES.
!</Help>
!
!                   ---tape identification---
!
!
!<Help KEYWORD="VOLUME">
!<Tip> VOLSER or PDN identifier label for tapes. </Tip>
! Default = blank
! Allowed = char(48) (array)
! VOLUME  is a tape data set identifier
! If the length of volume is <= 6 alpha-nums then volume is a volser label.
! Note that a volser is an external label, and the internal label will
! be blank if the tape is non-labeled.
! If the length of volume is >  6  but < 45 alpha-nums then the volume is
! interpreted as a catalogued data set name, i.e. a PDN. The tape catalogue
! server will look up the volsers corresponding to volume.
!</Help>
!
!                  ---do-skip trace selection---
!
!<Help KEYWORD="SKIP_INIT">
!<Tip> Number of traces to skip initially in the DO-SKIP selection. </Tip>
! Default = -
! Allowed = int >= 0 (linked array)
! The DO-SKIP trace selection method consists of initially skipping SKIP_INIT
! traces, then sequentially reading NUM_DO consecutive traces and skipping 
! NUM_SKIP consecutive traces until TOT_DO total traces are read. 
! Set to zero for BLK format.
!
! A DO-SKIP selection may be specified independently for each input tape or PDN.
!</Help>
!
!<Help KEYWORD="NUM_DO">
!<Tip> Number of traces to read at a time in the DO-SKIP selection. </Tip>
! Default = -
! Allowed = int > 0 (linked array)
!</Help>
!
!<Help KEYWORD="NUM_SKIP">
!<Tip> Number of traces to skip at a time in the DO-SKIP selection. </Tip>
! Default = -
! Allowed = int >= 0 (linked array)
!</Help>
!
!<Help KEYWORD="TOT_DO">
!<Tip> Total number of traces to read in the DO-SKIP selection. </Tip>
! Default = -
! Allowed = int > 0 (linked array)
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
!   MED echoes the value of the MEDIA parameter.  
!   USERID defaults to the current userid.  
!   PROJECT defaults to the value of the PROJECT parameter in PROJECT_DATA.
!   SUB_PROJECT defaults to BLANK.
!   TASK defaults to BLANK.
! 
! DEFAULTS is a front-end only parameter and operates only when editing old 
! workfiles.
!</Help>
!
!<Help KEYWORD="FILL_ROWS">
!<Tip> Whether to fill in PDN or VOLUME array. </Tip>
! Default = NO
! Allowed = YES/NO
! FILL_ROWS is a toggle for automatic filling in of the PDN or VOLUME
! arrays based on the entry in the last row.  Values of the first 5 PDN 
! elements are repeated and a counter is incremented by 1 for each row.  The 
! alpha part of VOLUME repeats and the numeric part is incremented by 1.
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
! NUM_ROWS determines the number of array rows to be filled by the FILL
! parameter.  NUM_ROWS is a front-end only parameter.
!</Help>
!
!      tabgroup = Permsave
!
!<Help KEYWORD="PATH_VMOD">
!<Tip> Pathname for restoring velocity model files.  </Tip>
! Default = NONE
! Allowed = char
! If answered, it is assumed that this data is a velocity model permsave
! tape.  The velocity model data as well as the processing records (in this
! case hgrid files), will be restored to path_vmod.
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
! Allowed = hoeplgp01    (LTO)
! Allowed = jaws         (Alaska only)
! User can choose a specific tape host
!</Help>
!</HelpSection>
!-------------------------------------------------------------------------------
!

!  
      MODULE TTRIN_MODULE

      use cio_module
      use clean_module
      use getlun_module
      use getsys_module
      use grid_module        
      use hist_module
      use lav_module
      use manhist_module
      use mutehw_module
      use named_constants_module
      use path_module
      use pathcheck_module
      use pc_module
      use putsys_module
      use rcpfile_module
      use segy_module
      use sizeof_module
      use string_module
      use swap_module
      use tcatclient_module
      use trcio_module
      use unix_module
      use wrdc_module

      implicit none

      private

character(len=100),public,save :: ttrin_ident = &
  '$Id: ttrin.f90,v 1.71 2006/11/27 14:07:49 Stoeckley prod sps $'
      public :: ttrin_create     ! uses the parameter cache.
      public :: ttrin_initialize
      public :: ttrin_update     ! uses the parameter cache.
      private:: ttrin_iscat
      private:: ttrin_getpdn
      private:: ttrin_next_vol
      public :: ttrin_delete
      public :: ttrin_curtape
!<execute_only>
      public :: ttrin            ! main execution (trace processing) routine.
      public :: ttrin_wrapup
      public :: ttrin_change_reel
      private:: ttrin_get_vol
      private:: ttrin_western_xy
      private:: ttrin_load
      private:: ttrin_open
      private:: ttrin_dmnt
      private:: ttrin_stdo
      private:: ttrin_blkd
      private:: ttrin_build_pdn
!</execute_only>



! Parameters for ttrin.f
!
      CHARACTER(LEN=8) :: INDIN(2)  !=/"INTAPEA ","INTAPEB "/
      CHARACTER VOLI*8
      INTEGER  :: NTHDC
      INTEGER  :: DRIVE

      type,public :: ttrin_struct

        private
        logical           :: skip_wrapup      ! wrapup flag.
        logical           :: fatal_error
        logical           :: isnewcps
        character(len=8)  :: clean            !to use clean module
        integer           :: ipn
        integer           :: fd               !file descriptor
        integer           :: sleep_time
        character(len=16) :: jobname    !name of the cps job(from init)
        character(len=8)  :: userid     !id field for pdn construction
        character(len=10) :: project
        character(len=10) :: sub_project
        character(len=10) :: task
        character(len=FILENAME_LENGTH) :: path_vmod
        character(len=120):: comment
        integer           :: ieofflg
        integer           :: max_blksiz
        integer           :: blksiz           !No. of 4byte elements in rbuf
        integer           :: bsize            !No. of words in rbuf
        integer           :: bytes_per_trace
        integer           :: nib
        integer           :: tpr
        integer           :: ndx
        integer           :: nleft
        integer           :: hdrndx
        integer           :: trcndx
        integer           :: cdc
        integer           :: nbits
        integer           :: nbits_hd
        integer           :: nbytesnword
        integer           :: endian  !1=sun, 0=intel
        character(len=4)  :: wdtype
        integer           :: nreel            !no. volumes to process
        integer           :: ireel            !current volume number
        integer           :: nvols            !reel count for current volume
        integer           :: ivol             !current reel of current volume
        character(len=6)  :: vols(50)         !stores volsers of volume
        character(len=8)  :: volser           !currently mounted volser
        character(len=16) :: media            !type of tape drive
        character(len=16) :: lname            !tape logical device name
        character(len=8)  :: format
        character(len=8)  :: label
        character(len=8)  :: history
        character(len=8)  :: rest_ps
        character(len=8)  :: mess_segy
        character(len=8)  :: ig_eof,dump_segy
        character(len=8)  :: mod_segy
        character(len=16) :: frontend_user
        character(len=16) :: frontend_node
        character(len=132):: frontend_path
        character(len=8)  :: tape_server !PROD,BETA,CUST
        character(len=12) :: tape_host   !host to connect to
        character(len=8)  :: version
        character(len=8)  :: defaults
        character(len=8)  :: fill_rows
        integer           :: host_noptions
        character(len=12) :: host_options(4)
        character(len=12) :: location
        integer    :: num_rows  !number of rows to fill in volume array
        integer    :: nummap    !number of segy headers to special map
        integer    :: sbyte(64) !byte location of segy header,from 1
        integer    :: bytes(64) !number of bytes in segy header
        integer    :: cps_hdr(64) !map to this cps header word
        character(len=8)  :: wtype(64) !word type of the segy header
        integer    :: filenum  !file where data starts
        integer    :: recnum   !1st record in filenum that is trace data
        integer    :: idead    !dead trace count
        integer    :: badcnt   !bad trace count
        integer    :: nfile    !tape current file number
        integer    :: nseqi    !input seq. number
        integer    :: irec     !tape record count
        integer    :: tac      !trace in accepted count
        integer    :: gcr      !group count for reel
        real       :: rlav     !lav for the reel
        integer    :: mntswi   !flag to force reel changes
        integer    :: nseqfil  !segy file count
        integer    :: ngrpc    !segy trace count
        integer    :: sgy_ln   !segy line number from 400 byte header
        character(len=48),pointer:: volume(:)
        character(len=48),pointer:: pdn(:)
        character(len=8),pointer:: cat(:)
        character, pointer:: ttyp(:)*8
        real   ,   pointer:: rbuf(:)
        integer,   pointer:: skip_init(:)
        integer,   pointer:: num_do(:)
        integer,   pointer:: tot_do(:)
        integer,   pointer:: num_skip(:)
        integer    :: ubytes(65)
        integer    :: hdrtyp(65)
        integer    :: nhdtape     !number of header words on tape
        integer    :: ndpttape    !number of trace values on tape
        integer    :: nwihcray,ndptcray,lun_prec,lun_fn,lun_modgrid
        integer    :: ndpt
        integer    :: nwih
        real       :: tstrt
        real       :: dt
        type(grid_struct) :: grid
        type(trcio_struct),pointer :: vmodptr

      end type ttrin_struct

      type(ttrin_struct),pointer,save :: object      ! needed for traps.

      logical,private,save           :: ttrin_first_call=.true.
      integer,private,save           :: stdo
      integer,private,save           :: ttrin_server_cnt
      character(len=24),private,save :: ttrin_servers(10)

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
      subroutine ttrin_create (obj)
      implicit none
      type(ttrin_struct),pointer :: obj       ! arguments
 
      allocate (obj)
 
      nullify  (obj%volume)
      nullify  (obj%pdn) ! jpa
      nullify  (obj%cat)
      nullify  (obj%ttyp)
      nullify  (obj%rbuf)
      nullify  (obj%skip_init)
      nullify  (obj%num_do)
      nullify  (obj%tot_do)
      nullify  (obj%num_skip)
      nullify  (obj%vmodptr) ! jpa
!
!!!      ttrin_server_cnt = ttrin_get_hosts() 
      call ttrin_initialize (obj)
! 
      return
      end subroutine ttrin_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
      subroutine ttrin_delete (obj)
      implicit none
      type(ttrin_struct),pointer :: obj       ! arguments
      integer i_err
 
!<execute_only>
      if (.not. associated(obj)) return
      call ttrin_wrapup (obj)
!</execute_only>
 
      if (associated(obj%volume )) deallocate (obj%volume)
      if (associated(obj%cat ))    deallocate (obj%cat)
      if (associated(obj%ttyp ))   deallocate (obj%ttyp)
      if (associated(obj%rbuf ))   deallocate (obj%rbuf)
      if (associated(obj%skip_init)) deallocate (obj%skip_init)
      if (associated(obj%num_do )) deallocate (obj%num_do)
      if (associated(obj%tot_do )) deallocate (obj%tot_do)
      if (associated(obj%num_skip)) deallocate (obj%num_skip)
      if(associated(obj)) then
        deallocate(obj,stat=i_err)
        if(i_err /= 0) then
          write(ttrin_stdo(),*) 'error deallocating obj'
        endif
      else
        write(ttrin_stdo(),*) 'ttrin_delete: obj not assoc?'
      endif
      nullify(object)
 
      return
      end subroutine ttrin_delete
 
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!<execute_only>
      subroutine ttrin_wrapup(obj)
      implicit none
      type(ttrin_struct),intent(inout) :: obj       ! arguments
      integer ilname(4),ivolser(4)
      integer stat,tpioclnt_prnt,tpioclnt_close

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.


      if(obj%fd >0) stat = tpioclnt_prnt(obj%fd)
      if(obj%fd >0 .or. obj%lname/=' ') then
        ilname = 0
        ivolser= 0
        call string_cc2hh(obj%lname,ilname) !used if fd < 0
        call string_cc2hh(obj%volser,ivolser) !used if fd < 0
! This will NOT eject the tape if obj%fd has been set to -1
!       The routine immediately returns if fd < 0
        stat = tpioclnt_close(obj%fd, ilname, ivolser)
      endif
      return
      end subroutine ttrin_wrapup
!</execute_only>


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
      subroutine ttrin_initialize (obj)
      implicit none
      type(ttrin_struct),intent(inout) :: obj       ! arguments
      integer max_blksiz,i_err,nbitsnword
      character(len=16) :: rlocation
      parameter (max_blksiz=30000)
 
!   Initialize ALL NON-POINTER VARIABLES in your parameter structure
!   as follows:
!  

!          Set tape_host menu - depending on where we are
!           This information should be added to the config file but for now...
      call getsys_env('HOST',obj%location)
      call pc_get_jdata('RLOCATION',rlocation)
      if(rlocation.eq.'ALASKA')obj%location(1:2)='ak'
      if(obj%location(1:2).eq.'ak')then
        obj%host_noptions=1
        obj%host_options(1)='jaws'
      else
        obj%host_noptions=3
        obj%host_options(1)='odi90'
        obj%host_options(2)='odi74'
        obj%host_options(3)='odi91'
        obj%host_options(4)='hoeplgp01'
      endif
      obj%tape_host     = obj%host_options(1)
      obj%jobname = ' '
      obj%userid  = ' '
      obj%project  = ' '    !get from job
      obj%sub_project = ' ' !get from job
      obj%task     = ' '    !get from job
      obj%path_vmod = PATHCHECK_EMPTY
      obj%frontend_user = ' '
      obj%frontend_node = ' '
      obj%frontend_path = ' '
      obj%tape_server   = 'PROD'

      obj%fatal_error=.false.
      obj%skip_wrapup =.true.

      obj%clean  = 'NONE'
      obj%badcnt = 0
      obj%fd    =-1
      obj%sleep_time = 100
      obj%sleep_time = 1
      obj%ieofflg = 0   !flag for eof on a blkd tape
      obj%cdc  = 0
      obj%max_blksiz = max_blksiz
      obj%blksiz= obj%max_blksiz
      obj%ndx   = 1
      obj%nleft = obj%max_blksiz
      obj%hdrndx= 1
      obj%trcndx= 1
      obj%fatal_error = .false.
      obj%bsize = obj%max_blksiz   !(8*4080)/sizeof(i_err)
      obj%nseqfil=0
      obj%ngrpc= 0
      obj%irec = 0    !trace bypassed on the reel
      obj%nfile= 0    !tape current file number
      obj%tac  = 0    !trace count for ireel
      obj%gcr  = 0    !group count for ireel

      obj%comment = ' '
      obj%media  = '3590HD'
      obj%lname  = ' '
      obj%format = 'CPS'
      obj%isnewcps= .false.
      obj%history= 'YES'
      obj%rest_ps= 'YES'
      obj%mess_segy='YES'
      obj%mod_segy   = 'NO'
      obj%defaults= 'SYS'
      obj%fill_rows    = 'YES'
      obj%num_rows= 1
      obj%volser = ' '
      obj%nreel  = 0
      obj%nvols  = 0
      obj%ivol   = 0
      obj%vols   = ' '
      obj%version= 'NONE'
      obj%nummap = 0 !number of segy headers to special map
      obj%sbyte  = 0 !starting byte location of segy header
      obj%bytes  = 0 !number of bytes in segy header
      obj%cps_hdr= 0 !map to this cps header word
      obj%ig_eof = 'NO'
      obj%dump_segy = 'NO'

      obj%ubytes  = 0
      obj%hdrtyp  = 0

      obj%filenum = 2
      obj%recnum  = 0
      obj%idead   = 0    !dead trace count per tape
      obj%rlav    = 0.0  !Max Abs Value of all traces on reel (real)
      obj%ireel   = 0
      obj%nseqi   = 0
      obj%mntswi  = 1     !force immediate mount when 1st trace is read
      obj%nbits   = 32    !default size of old CPS tapes
      obj%nbits_hd= 32    !default size of old CPS tapes
      obj%wdtype  = 'IBM' !default type of old CPS tapes
      obj%nib    = 0
      obj%tpr    = 1
      obj%endian  = 1
      obj%nhdtape= 32
      obj%ndpttape= 0
      obj%nwih   = 0     ! will have to test later to make sure has been reset.
      obj%ndpt   = 0     ! will have to test later to make sure has been reset.
      obj%nwihcray = 0
      obj%ndptcray = 0
      obj%dt     = 0.0
      obj%tstrt  = 0
      stdo=pc_get_lun()
      call grid_initialize(obj%grid)
!  
!   If a process parameter has a default which is calculated from a global
!   parameter, it can be set from the parameter cache
!  
      allocate(obj%rbuf(obj%bsize), stat=i_err)
      if(i_err /= 0) then
        call pc_error('TTRINS: RBUF ALLOCATION ERROR')
        i_err=-1
        return
      endif

      nbitsnword=bit_size(nbitsnword)
      obj%nbytesnword=nbitsnword/8
      call ttrin_update (obj)
 
      return
      end subroutine ttrin_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
 
 
      subroutine ttrin_update (obj)
      implicit none
      type(ttrin_struct),intent(inout),target :: obj             ! arguments

! COMBO BOX SETTINGS
      integer,parameter :: format_noptions=6
      character :: format_options(format_noptions)*8
      data format_options/'CPS','SEGY','BLK','DEBE','CPS-NSC','BLK-NSC'/
      integer,parameter :: media_noptions=8
      character :: media_options(media_noptions)*8
      data media_options/'3590HD','3590','3480','8MM','DLT','NR3590','LTO',&
                         '3592'/
!     integer,parameter :: label_noptions=3
!     character :: label_options(label_noptions)*8
!     data label_options/'ANSI','IBM','NONE'/
      integer,parameter :: history_noptions=2
      character :: history_options(history_noptions)*8
      data history_options/'NO','YES'/
      integer,parameter :: clean_noptions=3
      character :: clean_options(clean_noptions)*8
      data clean_options/'NONE','ZERO','KILL'/
      integer,parameter :: version_noptions=2
      character :: version_options(version_noptions)*8
      data version_options/'NONE','WESTERN1'/
      integer,parameter :: rest_ps_noptions=2
      character :: rest_ps_options(rest_ps_noptions)*8
      data rest_ps_options/'NO','YES'/
      integer,parameter :: mess_segy_noptions=2
      character :: mess_segy_options(mess_segy_noptions)*8
      data mess_segy_options/'NO','YES'/
      integer,parameter :: mod_segy_noptions=2
      character :: mod_segy_options(mod_segy_noptions)*8
      data mod_segy_options/'NO','YES'/
      integer,parameter :: default_noptions=2
      character :: default_options(default_noptions)*8
      data default_options/'OLD','SYS'/
      integer,parameter :: fill_noptions=2
      character :: fill_options(fill_noptions)*8
      data fill_options/'NO','YES'/
      integer,parameter :: noyes_noptions=2
      character :: noyes_options(noyes_noptions)*8
      data noyes_options/'NO','YES'/
!  LOCAL VARIABLES
      integer i,nr1,nr2,nr3,nr4,nr5   !local
      integer istat,lenvol
      character(len=8) :: ctmp
!
      object => obj               ! needed for traps.
      obj%skip_wrapup =.true.

      call pc_register_array_names ("volume_arrayset", (/  &
                                    "volume   ",           &
                                    "tot_do   ",           &
                                    "skip_init",           &
                                    "num_do   ",           &
                                    "num_skip " /))

      call pc_register_array_names ("sbyte_arrayset", (/  &
                                    "sbyte  ",            &
                                    "bytes  ",            &
                                    "cps_hdr",            &
                                    "wtype  " /))

! SET THE COMBO BOX OPTIONS
      call pc_put_options_field ('format'  , format_options, format_noptions)
      call pc_put_options_field ('media'   , media_options, media_noptions)
!     call pc_put_options_field ('label'   , label_options, label_noptions)
      call pc_put_options_field ('clean'   , clean_options, clean_noptions)
      call pc_put_options_field ('history' , history_options, history_noptions)
      call pc_put_options_field ('rest_ps' , rest_ps_options, rest_ps_noptions)
      call pc_put_options_field ('mess_segy', mess_segy_options,&
       mess_segy_noptions)
      call pc_put_options_field ('ig_eof'  , noyes_options, noyes_noptions)
      call pc_put_options_field ('dump_segy', noyes_options, noyes_noptions)
      call pc_put_options_field ('mod_segy', &
       mod_segy_options, mod_segy_noptions)
      call pc_put_options_field ('defaults', default_options, default_noptions)
      call pc_put_sensitive_field_flag ('defaults'    , .false.)
      call pc_put_options_field ('fill_rows'    , fill_options, fill_noptions)
      call pc_put_options_field ('version' , version_options, version_noptions)
      call pc_put_options_field ('tape_host',obj%host_options,obj%host_noptions)
!
! Get globals that we need
      obj%ipn = pc_get_ipn()
      call pc_get('userid',obj%userid)
      if(obj%userid==' ') then
        call pc_get_pdata  ('user_name'   , obj%userid)
      endif
      call pc_get_pdata ('grid' , obj%grid)
      call pc_get('project',obj%project,ttrin_project_trap)
      if(obj%project==' ') then
        call pc_get_pdata  ('project'     , obj%project)
      endif
      call string_squeeze_blanks(obj%project)
      call pc_get_jdata ('jobname'   , obj%jobname)
      call pc_get_jdata ('frontend_user'     , obj%frontend_user)
      call pc_get_jdata ('frontend_node'     , obj%frontend_node)
      call pc_get_jdata ('frontend_path'     , obj%frontend_path)
      call pc_get_jdata ('tape_server'       , obj%tape_server)
      call pc_get_jdata ('tstrt', obj%tstrt)
      call pc_get_jdata ('dt'   , obj%dt)
      call pc_get_jdata ('ndpt' , obj%ndpt)
      call pc_get_jdata ('nwih' , obj%nwih)
      if(obj%tape_server(1:1) == 'p') obj%tape_server='PROD'
      if(obj%tape_server(1:1) == 'P') obj%tape_server='PROD'
      if(obj%tape_server(1:1) == 'b') obj%tape_server='BETA'
      if(obj%tape_server(1:1) == 'B') obj%tape_server='BETA'

!
! Front end parameters
      call pc_get('sub_project',obj%sub_project,ttrin_sub_project_trap)
      if(obj%sub_project==' ') then
        call pc_get_jdata('sub_project',obj%sub_project)
      endif
      call string_squeeze_blanks(obj%sub_project)
      call pc_get('comment' ,obj%comment)
      call pc_get('format'  ,obj%format)
      call pc_get('media'   ,obj%media)
!     call pc_get('label'   ,obj%label)
      call pc_get('clean'   ,obj%clean)
!     call pc_put_sensitive_field_flag ('label'    , .false.)
      call pc_get('history' ,obj%history)
      call pc_get('rest_ps' ,obj%rest_ps)
      call pc_get('mess_segy',obj%mess_segy)
      call pc_get('ig_eof'  ,obj%ig_eof)
      call pc_get('dump_segy',obj%dump_segy)
      call pc_get('mod_segy',obj%mod_segy)
!     call pc_put_sensitive_field_flag ('defaults', .false.)
!     call pc_put_sensitive_field_flag ('fill_rows'    , .false.)
!     call pc_put_sensitive_field_flag ('num_rows', .false.)
      call pc_get('defaults' ,obj%defaults)
      call pc_get('fill_rows'    ,obj%fill_rows)
      call pc_get('num_rows',obj%num_rows)
      call pc_get('task'     ,obj%task,ttrin_task_trap)
      call pc_get('path_vmod',obj%path_vmod, ttrin_path_vmod_trap)
      call pc_get('version' ,obj%version)
      call pc_get('tape_host' , obj%tape_host, ttrin_tape_host_trap)
!
      if(obj%num_rows <0  ) obj%num_rows=0
      if(obj%num_rows >100) obj%num_rows=100

      if (obj%fill_rows(1:3)=='YES') then
          call ttrin_build_pdn(obj)
          call pc_put_sensitive_array_flag('volume', .false.)
      else
          call pc_put_sensitive_array_flag('volume', .true. )
      endif

      nr1 = obj%nreel 
      nr2 = nr1
      nr3 = nr1
      nr4 = nr1
      nr5 = nr1
      call pc_alloc('volume'   ,obj%volume    ,obj%nreel ,&
        ttrin_volume_element_trap)
      call pc_alloc('skip_init',obj%skip_init ,nr1)
      call pc_alloc('num_do'   ,obj%num_do    ,nr2)
      call pc_alloc('num_skip' ,obj%num_skip  ,nr3)
      call pc_alloc('tot_do'   ,obj%tot_do    ,nr4)
      if(nr1 /= obj%nreel  .or. nr1 /= nr2 .or. &
         nr1 /= nr3        .or. nr1 /= nr4) then
        call pc_error ('VOLUME,SKIP_INIT,... arrays have different lengths')
        obj%nreel  = min(obj%nreel ,nr1,nr2,nr3,nr4)
      endif
!     if(obj%nreel  <=0 ) then
!       call pc_info ('VOLUME,SKIP_INIT,... have zero lengths')
!     endif
      nr1=0
      lenvol=0
      do i = 1,obj%nreel 
        if(obj%volume(i)==' ') nr1 = nr1+1
        lenvol = len_trim(obj%volume(i))
        if(obj%num_skip(i)<0) obj%num_skip(i)=0
        if(obj%skip_init(i)<0) obj%skip_init(i)=0
        if(obj%tot_do(i)<1) obj%tot_do(i)=99999999
        if(obj%num_do(i)<1) obj%num_do(i)=1
      enddo
      if(nr1 > 0) then
        call pc_error ('ttrin_update: One or more VOLUME entries is blank')
      endif

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
        call pc_error ('TTRIN_UPDATE: SBYTE,BYTES,CPS_HDR &
        &arrays have different lengths')
        obj%nummap = min(obj%nummap,nr1,nr2,nr3)
      endif
      do i = 1,obj%nummap
        call string_to_upper(obj%wtype(i))
        if(obj%wtype(i) /= 'F') obj%wtype(i)='I'
        if(obj%bytes(i)   > 4) obj%bytes(i)  =4
        if(obj%bytes(i)   < 2) obj%bytes(i)  =2
        if(obj%sbyte(i)   < 1) obj%sbyte(i)  =1
        if(obj%cps_hdr(i) < 0 .or. obj%cps_hdr(i) > obj%nwih) then
          obj%cps_hdr(i)=0         !no mapping
        endif
      enddo
!
      if(obj%format=='SEGY') obj%wdtype='IBM'
      if(obj%nbits /= 32) obj%nbits=32
      if(obj%nbits_hd /= 32) obj%nbits=64
      if(obj%wdtype == 'IBM') then
        obj%nbits=32
        obj%nbits_hd=32
      else
        obj%wdtype='IBM'  !default to old-cps format
        obj%nbits=32
      endif

     call pc_call_end_trap(ttrin_end_trap)
!
!  SET THE VALUES IN THE PARAMETER CACHE
      call pc_put('volume',obj%volume ,obj%nreel )
      call pc_put('skip_init',obj%skip_init ,obj%nreel )
      call pc_put('num_do'   ,obj%num_do    ,obj%nreel )
      call pc_put('tot_do'   ,obj%tot_do    ,obj%nreel )
      call pc_put('num_skip' ,obj%num_skip  ,obj%nreel )
      call pc_put('sbyte'  ,obj%sbyte   ,obj%nummap)
      call pc_put('bytes'  ,obj%bytes   ,obj%nummap)
      call pc_put('cps_hdr',obj%cps_hdr ,obj%nummap)
      call pc_put('wtype'  ,obj%wtype   ,obj%nummap)
      call pc_put('comment'  ,obj%comment)
      call pc_put('format'   ,obj%format)
      call pc_put('media'    ,obj%media)
!     call pc_put('label'    ,obj%label)
      call pc_put('clean'   ,obj%clean)
      call pc_put('history'  ,obj%history)
      call pc_put('rest_ps'  ,obj%rest_ps)
      call pc_put('mess_segy',obj%mess_segy)
      call pc_put('mod_segy' ,obj%mod_segy)
      call pc_put('ig_eof'   ,obj%ig_eof)
      call pc_put('dump_segy',obj%dump_segy)
      call pc_put('userid'   ,obj%userid)
      call pc_put('project'  ,obj%project)
      call pc_put('sub_project',obj%sub_project)
      call pc_put('task'     ,obj%task)
      call pc_put('path_vmod',obj%path_vmod)
      call pc_put('defaults' ,obj%defaults)
      call pc_put('fill_rows'     ,obj%fill_rows)
      call pc_put('num_rows' ,obj%num_rows)
      call pc_put('version'  ,obj%version)
      call pc_put('tape_host' , obj%tape_host)
      if(obj%format(1:4) /= 'SEGY') then
        call pc_put_sensitive_array_flag('rest_ps', .true.)
        call pc_put_sensitive_array_flag('sbyte_arrayset', .false.)
        call pc_put_sensitive_field_flag('mod_segy', .false.)
        obj%mod_segy='NO'
        obj%dump_segy='NO'
        call pc_put_sensitive_field_flag('mess_segy', .false.)
        call pc_put_sensitive_field_flag('ig_eof', .false.)
        call pc_put_sensitive_field_flag('dump_segy', .false.)
        call pc_put_sensitive_field_flag('history', .true.)
        call pc_put_sensitive_arrayset_flag('sbyte_arrayset',.false.)
      else
        call pc_put_sensitive_array_flag('rest_ps', .false.)
        call pc_put_sensitive_field_flag('mod_segy', .true.)
        call pc_put_sensitive_field_flag('mess_segy', .true.)
        call pc_put_sensitive_field_flag('ig_eof', .true.)
        call pc_put_sensitive_field_flag('dump_segy', .true.)
        call pc_put_sensitive_field_flag('history', .false.)
      endif

      if(obj%mod_segy.eq.'YES')then
        call pc_put_sensitive_arrayset_flag('sbyte_arrayset',.true.)
      else
        call pc_put_sensitive_arrayset_flag('sbyte_arrayset',.false.)
      endif
!
      call pc_put_global ('numtr'       , 1)
      call pc_put_global ('gathered'       , .false.)
!     call pc_put_control ('numtr'       , 1)
      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('gathered'    , .false.)
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
      else if(obj%tape_host.eq.'jaws')then
        ctmp='A'
      endif
      call pc_put_control ('tapequeue'   , ctmp)

      obj%bytes_per_trace =  (obj%nbits/8)*obj%ndpt + &
        (obj%nbits_hd/8)*obj%nwih
      if(pc_do_not_process_traces()) return

!          Prepare for Execution
!          Get a unit number for restoring perm save files
      call getlun(obj%lun_prec,istat)
      if(istat.ne.0)then
        call pc_error('Unable to get unit number for prec')
      endif

      call manhist_initialize(istat)
      if(istat.ne.0)call pc_error('TTRIN: Unable to initialize history')


      obj%skip_wrapup = .false.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
      return
      end subroutine ttrin_update

      subroutine ttrin_end_trap
      implicit none
      integer   :: i,nvols,iodi90,iodi91,k
      character :: userid*32,line*80,vols(50)*6
      
      iodi90=0
      iodi91=0
      do i = 1,object%nreel
        if(len_trim(object%volume(i))> 6) then
           call ttrin_getpdn(object,object%volume(i),nvols,vols,userid)
           if(nvols==0) then
             write(line,'(''TTRIN: '',A,'' is not catalogued'')') &
             trim(object%volume(i))
             call pc_warning (line)
           endif
        endif
        k=index(object%volume(i),'3590hd.')
        if(k.ne.0.and.object%tape_host.ne.'odi90')then
          object%tape_host='odi90'
          iodi90=1
        endif
        k=index(object%volume(i),'3592.')
        if(k.ne.0.and.object%tape_host.ne.'odi91')then
          object%tape_host='odi91'
          iodi91=1
        endif
      enddo
      if(iodi90.eq.1.and.iodi91.eq.1)then
        call pc_error('ERROR you cannot mix 3590 and 3592')
      endif
      if(iodi90.eq.1)call pc_info('TAPE_HOST changed to odi90 based on pdn &
                                  &media type 3590hd')
      if(iodi91.eq.1)call pc_info('TAPE_HOST changed to odi91 based on pdn &
                                  &media type 3592')

      if(object%tape_host.ne.'jaws')then
        select case(object%media)
          case('3590HD')
            if(object%tape_host.ne.'odi90')then
             object%tape_host='odi90'
             call pc_info('TAPE_HOST changed to odi90 based on MEDIA parameter')
            endif
          case('3592')
            if(object%tape_host.ne.'odi91')then
             object%tape_host='odi91'
             call pc_info('TAPE_HOST changed to odi91 based on MEDIA parameter')
            endif
          case('LTO')
            if(object%tape_host.ne.'hoeplgp01')then
              object%tape_host='hoeplgp01'
              call pc_info&
                ('TAPE_HOST changed to hoeplgp01 based on MEDIA parameter')
            endif
          case default
            if(object%tape_host.ne.'odi90'.and.object%tape_host.ne.'odi74')then
             object%tape_host='odi74'
             call pc_info('TAPE_HOST changed to odi74 based on media parameter')
            endif
        end select
      endif
            

   

      if(object%dump_segy.eq.'YES')then
        k=0
        do i=1,object%nreel
          k=k+object%tot_do(i)
        enddo
        if(k.gt.2000)then
          call pc_warning('WARNING - You are dumping over 2000 traces')
          call pc_warning('          You should set TOT_DO to less than 2000')
        endif
      endif
      return
      end subroutine ttrin_end_trap

       subroutine ttrin_path_vmod_trap (keyword)
       implicit none
       character(len=*),intent(in) :: keyword
       if(object%path_vmod.eq.PATHCHECK_EMPTY)return
       call pathcheck('path_vmod',object%path_vmod)
       return
       end subroutine ttrin_path_vmod_trap

       subroutine ttrin_project_trap (keyword)
       implicit none
       character(len=*),intent(in) :: keyword
       call string_squeeze_blanks(object%project)
       end subroutine ttrin_project_trap

       subroutine ttrin_sub_project_trap (keyword)
       implicit none
       character(len=*),intent(in) :: keyword
       call string_squeeze_blanks(object%sub_project)
       end subroutine ttrin_sub_project_trap

       subroutine ttrin_task_trap (keyword)
       implicit none
       character(len=*),intent(in) :: keyword
       call string_squeeze_blanks(object%task)
       end subroutine ttrin_task_trap

      subroutine ttrin_tape_host_trap(keyword)

      character(len=*),intent(in) :: keyword     
      if(object%location(1:2).eq.'ak'.and.object%tape_host.ne.'jaws')then
        object%tape_host='jaws'
        call pc_info('tape_host set to jaws based on location')
      endif
      if(object%tape_host.eq.'hoepodi90')object%tape_host='odi90'
      if(object%tape_host.eq.'hoepodi74')object%tape_host='odi74'
      if(object%tape_host.eq.'hoepodi91')object%tape_host='odi91'

      if(object%tape_host.eq.'odi90')return
      if(object%tape_host.eq.'odi91')return
      if(object%tape_host.eq.'odi74')return
      if(object%tape_host.eq.'jaws')return
      if(object%tape_host.eq.'hoeplgp01')return

      call pc_error('Invalid TAPE_HOST')      

      end subroutine ttrin_tape_host_trap

      subroutine ttrin_volume_element_trap (keyword,indx,action)
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments
      integer         ,intent(in) :: indx              ! arguments
      integer         ,intent(in) :: action            ! arguments
      integer           :: i_err,stat,i,n,nvols
      character         :: userid*32,line*80,vols(50)*6,cnvol*8
!      ! INDX is a Fortran-style index (=1 for the first array element).
!      ! ACTION is PC_INSERT or PC_REMOVE or PC_MODIFY.
!      ! INDX refers to the array element inserted or removed or modified.
!  
      if(action == PC_REMOVE) then
        if(object%num_rows > object%nreel) object%num_rows= object%nreel
        return
      endif
      if(action /= PC_MODIFY .and. action /= PC_INSERT) return
      i=indx
!     If you type it in, it can have upper case.
!     call string_to_lower(object%volume(i))
      if(object%volume(i)==' ') then
        call pc_error ('TTRIN: blank VOLUME entry is illegal')
        return
      endif
      if(len_trim(object%volume(i))> 6) then
         call ttrin_getpdn(object,object%volume(i),nvols,vols,userid)
         if(nvols==0) then
           write(line,'(''TTRIN: '',A,'' is not catalogued'')') &
           trim(object%volume(i))
           call pc_error (line)
! Leave user entry alone after warning.
!          object%volume(i)='XXXXXX'
         else if(nvols <= size(vols)) then
           write(cnvol,'(I4)') nvols
           call pc_info('TTRIN:'//trim(cnvol)//' vols')
           line = 'TTRIN:'//' volser list for '//trim(object%volume(i))
           call pc_info(line)
           line = 'TTRIN: ' 
           do n=1,nvols
             line = trim(line)//' '//vols(n)
             if(len_trim(line)> 72) then
               call pc_info(line)
               line = 'TTRIN: ' 
             endif
           enddo
           if(len_trim(line)> 7) call pc_info(line)
         else
           write(line,'(''TTRIN: > 50 tapes for '',A)') &
           trim(object%volume(i))
           call pc_info(line)
         endif
      else
        stat = ttrin_iscat(object,object%volume(i),userid,i_err)
        if(stat == 1 .and. userid /= ' ') then
          line= 'TTRIN: '//trim(object%volume(i))//' is owned by '//&
          &trim(userid)
          call pc_info(line)
        endif
      endif
      return
      end subroutine ttrin_volume_element_trap
!  
! find the list of vols catalogued under pdn(indx)
      subroutine ttrin_getpdn(obj,volume,nvols,vols,userid)
      implicit none
      type(ttrin_struct),intent(in)    :: obj        ! arguments
      character(len=*),intent(in)      :: volume     !arguments
      integer,intent(out)              :: nvols      !arguments
      character(len=6),intent(out)     :: vols(:)    !arguments
      character(len=*),intent(out)     :: userid     !arguments
      character         :: msg*240, req*80
      integer           :: ip, i

      if(volume==' ') return
      userid= ' '
      vols  = ' '
      nvols = 0
      if(len_trim(volume) <7) then !volume is a volser
        nvols=1
        vols = volume
        return
      endif
      req = 'GETPDN='//volume

      if (tcatclient_query(req, msg) /= 1) then
        call pc_info('TTRIN_GETPDN: catalogue server error!!')
        return
      endif

      if(index(msg,"SERVER FATAL ERROR") /= 0) then
        call pc_info('TTRIN_GETPDN: catalogue server error!!')
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
          if(size(vols) < nvols) then
           call pc_info('TTRIN_GETPDN: catalogue overflows vols!!')
          endif
        endif
      else
        return
      endif
      return
      end subroutine ttrin_getpdn
!  
      integer function ttrin_iscat(obj,vol,userid,i_err)
      implicit none
      type(ttrin_struct),intent(in)  :: obj       ! arguments
      character(len=*),intent(inout) :: vol
      character(len=*),intent(out)   :: userid
      integer,intent(out)            :: i_err
      integer           :: ip
      character         :: req*80,msg*80
      character         :: line*80

      i_err=0
      ttrin_iscat = 0
      userid=' '
      if(vol=='SCR' .or. vol == ' ') return
      req = 'ISCAT='//vol

      if (tcatclient_query(req, msg) /= 1) then
        call pc_info('TTRIN_ISCAT: catalogue server error!!')
        i_err=-1
        return
      endif

      if(index(msg,"SERVER FATAL ERROR") /= 0) then
        call pc_info('TTRIN_ISCAT: catalogue server error!!')
        i_err=-1
        return
      endif
      ip = index(msg,'ISCAT=Y')
      if(ip /= 0) then
        ttrin_iscat=1
        ip = index(msg,'userid=')
        if(ip /=0) then
          userid = msg(ip+7:ip+14)
        endif
        return
      else
       line='TTRIN: vol='//trim(vol)//' is not in the catalogue'
       call pc_info(line)
       line='TTRIN: this is a foreign tape?'
       call pc_info(line)
      endif
      return
      end function ttrin_iscat
!
!   PDN Descriptor = MED.USERID.PROJECT.SUB_PROJECT.TASK.COUNTER
      subroutine ttrin_build_pdn(obj)
      implicit none
      type(ttrin_struct),intent(inout) :: obj       ! arguments
      character(len=10) :: proj,subp,task
      character(len=2)  :: cnt
      character(len=8)  :: format
      character(len=48) :: pdn
      character(len=48),pointer :: lpdn(:)
      integer,pointer   :: tot_do(:),num_do(:),num_skip(:),skip_init(:)
      integer num,i_err,i
      proj    = obj%project
      subp    = obj%sub_project
      task    = obj%task
      format  = obj%format
!     Rebuild all lines, ehs --- 16sep02
!     numtoadd = obj%num_rows - obj%nreel
!     if(numtoadd <=0) return
      allocate(lpdn(obj%num_rows),stat=i_err)
      if(i_err /= 0) return
      allocate(tot_do(obj%num_rows),stat=i_err)
      if(i_err /= 0) return
      allocate(num_do(obj%num_rows),stat=i_err)
      if(i_err /= 0) return
      allocate(num_skip(obj%num_rows),stat=i_err)
      if(i_err /= 0) return
      allocate(skip_init(obj%num_rows),stat=i_err)
      if(i_err /= 0) return
      num=1
      do i = 1,obj%num_rows
        if(i<=obj%nreel) then
          tot_do(i)    = obj%tot_do(i)
          num_do(i)    = obj%num_do(i)
          skip_init(i) = obj%skip_init(i)
          num_skip(i)  = obj%num_skip(i)
        else
          tot_do(i)  = 99999999
          num_do(i)  = 1
          num_skip(i)= 0
          skip_init(i)= 0
        endif
        pdn= trim(obj%media)//'.'//trim(obj%userid)
        if(proj /= ' ') then
          pdn = pdn(1:index(pdn,' ')-1)//'.'//trim(proj)
        endif
        if(num <10) then
          write(cnt,'(i1)') max(1,num)
        else
          write(cnt,'(i2)') max(1,num)
        endif
        num = num+1
        if(subp /= ' ') then
          pdn = pdn(1:index(pdn,' ')-1)//'.'//trim(subp)
        endif
        if(task /= ' ') then
          pdn = pdn(1:index(pdn,' ')-1)//'.'//trim(task)
        endif
        if(format /= ' ') then
          pdn = pdn(1:index(pdn,' ')-1)//'.'//trim(format)
        endif
        if(cnt /= ' ') then
          pdn = pdn(1:index(pdn,' ')-1)//'.'//trim(cnt)
        endif
        call string_to_lower(pdn)
        call string_squeeze_blanks(pdn)
        lpdn(i)    = pdn
      enddo
      if(associated(obj%volume)) deallocate(obj%volume)
      if(associated(obj%tot_do)) deallocate(obj%tot_do)
      if(associated(obj%num_do)) deallocate(obj%num_do)
      if(associated(obj%skip_init)) deallocate(obj%skip_init)
      if(associated(obj%num_skip)) deallocate(obj%num_skip)
      obj%volume    => lpdn
      obj%tot_do    => tot_do
      obj%skip_init => skip_init
      obj%num_do    => num_do
      obj%num_skip  => num_skip
      obj%nreel  = obj%num_rows
      return
      end subroutine ttrin_build_pdn


      subroutine ttrin(obj,ntr,hd,tr)
  !----------- PASSED PARAMETERS ----------------------------------------
      type(ttrin_struct),pointer :: obj
      integer         ,intent(inout)  :: ntr                    ! num trc
      double precision,intent(inout)  :: hd(:,:)                ! headers
      real            ,intent(inout)  :: tr(:,:)                ! traces
  !----------- LOCAL VARIABLES ------------------------------------------
      integer i_err
      CHARACTER TIM*32
     !INTEGER  ichar
      INTEGER  jr
      INTEGER  nwd
      INTEGER  lnl
      INTEGER  nblock
      INTEGER  nden
      INTEGER  nhop,nnum,nphys,npiece
      INTEGER  ntes,ntosk
!
!
      INTEGER I
! External functions
      integer     tpioclnt_peof
      integer     tpioclnt_read
      integer     tpioclnt_asread


      INTEGER     NRD,NTORD,I0,ITYP
      CHARACTER   CARD*80, FMT*8,STRING*120,vol*6

      INTEGER     BUFLEN,NBY
      INTEGER     NRPERT !records per trace
      integer     nin
      integer     donxt
      logical :: logflg






!          DIAGNOSTIC FLAG
!-----info saved from SEGD file header (except for 1st 3 items)
!  
      if(.not. associated(obj) ) then
        string = 'TTRIN:  object was lost.'
        go to 9000
      endif
      if (ntr == FATAL_ERROR) then
        return
      endif
      if(obj%skip_wrapup) then
        ntr=NO_MORE_TRACES
        return
      endif
!
!--Start of the trace read operation 
      nrpert=1
 9    ntr=0
      if(obj%ireel==0) goto 120
      if(obj%tac==obj%tot_do(obj%ireel))then
        obj%mntswi=1
      endif
!--mntswi initialized to 1
!--Forces immediate reel change if = 1. Throws away trace in the buffer
      if(obj%mntswi > 0) go to 120


      if(obj%format(1:3) == 'BLK'.or.obj%path_vmod.ne.PATHCHECK_EMPTY)then
        if(obj%format(1:3).eq.'BLK')then
          call ttrin_blkd(obj,hd(1:,1),tr(1:,1))
        else
          call ttrin_vmod(obj,hd(1:,1),tr(1:,1))
          write(stdo,*)'TTRIN-->Velocity models have been restored' 
          obj%mntswi=1
          goto 120
!!!          obj%ieeofflg=1
        endif
        if(obj%fatal_error) then
         ntr = FATAL_ERROR
         return
        endif
        ntr=1
        if(obj%ieofflg == 1)then
          nrd=0
          obj%ieofflg=0   !reset
          go to 20
        endif
        obj%tac   = obj%tac + 1
        obj%nseqi = obj%nseqi+1
        hd(1,1)   = obj%nseqi
        obj%irec  = obj%irec + 1
        obj%ngrpc = obj%ngrpc+1
        if(obj%tac >= obj%tot_do(obj%ireel)) then
          obj%mntswi = 1 !set switch so nnrd ext tape will mount when we reenter
        endif
        return
      endif
 

      nnum = obj%irec - obj%skip_init(obj%ireel)
      nden = obj%num_do(obj%ireel) + obj%num_skip(obj%ireel)
      ntes = mod(nnum,nden)

      buflen=0
      npiece=0
      i0=buflen + 1
      go to 6

 6    continue
      ntord = obj%bsize*sizeof(i)
      if(.not. obj%isnewcps) then
        nrd = tpioclnt_read(obj%fd,obj%rbuf(i0),ntord)
      else
        ! nin = number of bytes read from tape record
        ! nib = number of traces in read buffer
        ! tpr = number of traces max per physical record
        ! nrd = number of bytes to process
        if(obj%nib==0) then !read if no traces in buffer
        ! nin = tpioclnt_read(obj%fd,obj%rbuf(i0),ntord)
          donxt=1
          if(obj%tac>=obj%tot_do(obj%ireel)-obj%tpr) donxt=0
          nin = tpioclnt_asread(obj%fd,obj%rbuf(i0),ntord, donxt)
          nrd = nin
          obj%nib = nin/obj%bytes_per_trace
          obj%nib = max(0,obj%nib) !in case of read error
          if(obj%nib*obj%bytes_per_trace < nin .and. nin>80) then
            write(stdo,*) 'TTRIN: warning - buffer contains non integer&
            &number of traces'
          endif
          if(obj%nib > obj%tpr) then
            write(stdo,*) 'TTRIN: ERROR - too many traces in record!',obj%nib
            write(stdo,*) 'TTRIN: ERROR - read=',obj%nib,'>expected=',obj%tpr
            obj%fatal_error=.true.
            ntr=FATAL_ERROR
            return
          endif
          if(obj%nib> 0 .and. obj%nib /= obj%tpr) then
            !the last record on the tape may not have tpr traces in it.
            obj%tpr = obj%nib
          endif
        else
          !process data left in buffer from old read
          !indicate a trace in the buffer as long as nib>0
          nin = 0
          nrd = 0
          if(obj%nib > 0) then
            nin = obj%bytes_per_trace
            nrd = obj%bytes_per_trace
          endif
        endif
        if(nin>80) then !will let nrd= -1|0|80 unchanged
          nrd = obj%bytes_per_trace
        endif
      endif
      nwd = nrd/sizeof(i)
!
! Watch for 80 byte records
      if(nrd==80) then
        call cmem_cpy_c(card,obj%rbuf,80)
!!!        card(1:80) = transfer(obj%rbuf(1:20),card(1:80))
        write(ttrin_stdo(),*) 'TTRIN: 80 byte record'
        write(ttrin_stdo(),*) 'TTRIN:',CARD(1:80),':'
        goto 9
      endif
!
! Handle read errors
      if(nrd < 0) then
        write(ttrin_stdo(),*) 'TTRIN: Fatal error - '
        write(ttrin_stdo(),101) obj%irec+1
 101    format('0','TTRIN:  READ ERROR ON RECORD No.=',I6)
        ntr=0
        nblock=1
        go to 120   !unrecovered error go to next reel
      endif
!
! Handle EOFs
! IGNORE? SINGLE EOF AND QUIT ON DOUBLE EOF
 20   if(nrd==0) then
        ntr=0
        i_err = tpioclnt_peof(obj%fd)
        if(i_err==-1) then
          write(ttrin_stdo(),*) ' TTRIN: DOUBLE EOF AFTER FILE ',obj%nfile
          obj%mntswi=1
          goto 120
        else
          write(ttrin_stdo(),*) ' TTRIN: EOF AFTER FILE ',obj%nfile
        endif
        obj%nfile=obj%nfile+1
        if(obj%format=='SEGY'.and.obj%ig_eof=='YES') then
          obj%nseqfil = obj%nseqfil+1
          obj%ngrpc= 0
          write(ttrin_stdo(),*) 'TTRIN: SEGY FILE ',obj%nfile
          write(ttrin_stdo(),*) 'TTRIN: SEGY BLOCKS ',obj%ngrpc
          call ttrin_postn(obj)
          if(obj%fatal_error) then
            write(ttrin_stdo(),*) ' TTRIN: postn failure'
            obj%mntswi=1
            goto 120
          endif
          goto 9
        else
          obj%nseqfil = 0
          obj%ngrpc= 0
          obj%mntswi=1
          goto 120
        endif
      endif

      buflen=buflen + nwd - obj%cdc
      npiece=npiece+1
!
!--Successful read of traces. Test for appropriate action.
!--Either accept the trace, reject the trace, or issue a mount
      nrpert=npiece           !Update the records per trace variable
      ntr=1
      obj%irec = obj%irec+1
      obj%ngrpc= obj%ngrpc+1
      if(obj%irec - obj%skip_init(obj%ireel) < 0) goto 9  !initial skip
      if(ntes < obj%num_do(obj%ireel)-1) then
        !TRACE ACCEPTED AS A GROUP MEMBER-CONTINUE READING
        obj%tac   = obj%tac + 1
        obj%nseqi = obj%nseqi+1
        if(obj%isnewcps) then
          call ttrin_new_cps_tcf(obj,nrd,hd(:,1),tr(:,1))
        else
          call ttrin_tcf(obj,nrd,hd(:,1),tr(:,1))
        endif
        if(obj%fatal_error) ntr=FATAL_ERROR
        return
      else if(ntes==obj%num_do(obj%ireel)-1) then
        !TRACE ACCEPTED AS A GROUP MEMBER
        obj%tac   = obj%tac + 1
        obj%nseqi = obj%nseqi+1
        obj%gcr   = obj%gcr + 1
        if(obj%isnewcps) then
          call ttrin_new_cps_tcf(obj,nrd,hd(:,1),tr(:,1))
        else
          call ttrin_tcf(obj,nrd,hd(:,1),tr(:,1))
        endif
        if(obj%tac >= obj%tot_do(obj%ireel)) then
          !SET MOUNT SWITCH SO WE MOUNT NEW TAPE UPON REENTRY(2/25/87)
          obj%mntswi=1
        endif
        if(obj%fatal_error) ntr=FATAL_ERROR
        return
      else
        !TRACE REJECTED AS A GROUP MEMBER-BETWEEN GROUPS.
        if(obj%tac >= obj%tot_do(obj%ireel)) then
          !FINISHED WITH THIS TAPE. MOUNT NEW TAPE IMMEDIATELY.
          obj%mntswi=1
          ntr=NO_MORE_TRACES
          go to 120
        endif
        go to 9     !keep reading
      endif
!
!
!--A read error has been detected during CALL READ.
!--Perform a skipbad and supply dead traces to compensate
!--for the records which were skipped. Honor do-skip patterns.
!--(A) compute number of traces skipped                     (NTRSKP)
!--(B) compute records to skip to align on a trace boundary (N2SKP)
!--(C) compute the number of dead traces to pass to output  (NBY  )
!

 120  CONTINUE
! First check if this is a dump from a read error(mntswi=2)
      if(obj%mntswi==2) then
        if(nby==0) then
          obj%mntswi=0
          go to 9
        endif
        tr(1:obj%ndpt,1)  = 0.0
        hd(1:obj%nwih,1) = 0.0
        ntr  =1
        obj%tac  =obj%tac+1
        obj%nseqi=obj%nseqi+1
        obj%idead=obj%idead+1
        nby  =nby-1
        obj%mntswi=2
        hd(1,1)=obj%nseqi
        hd(2,1)=obj%ndpt+1
        return
      endif
!
!---A CALL HAS BEEN ISSUED FOR A NEW TAPE.
!-A-HAVE WE EXHAUSTED THE INPUT QUEUE?
!-B-SET THE VARIABLES THAT DEPEND ON THE INPUT TAPE FORMAT.
!-B-SKIP THE REEL IF IT IS AN UNKNOWN INPUT FORMAT.
!-C-SUMMARIZE THE CURRENT REEL STATISTICS AND MOUNT THE NEW TAPE.
!
      fmt = obj%format
      ityp = ttrin_init_type(fmt,obj%history, &
        obj%filenum,obj%recnum,lnl,obj%nhdtape,obj%cdc, obj%ubytes,obj%nwih,&
        obj)
      if(ityp==-1) then
        write(ttrin_stdo(),205) fmt
 205    format('0','TTRIN: ',A4,' IS NOT A RECOGNIZED INPUT FORMAT')
        obj%ireel=obj%ireel+1    !SKIP TO NEXT SEQUENCE OF INPUT REELS
        go to 120
      endif
!
! dismount any mounted tape. (rewinds, MTOFFL, closes)
      call ttrin_dmnt(obj)
      obj%mntswi=0
      obj%isnewcps=.false.
      fmt = obj%format
!
! print out stats from the dismounted volser
      if(obj%ireel >= 1) then
        if(obj%dump_segy.eq.'YES')then
          logflg=.true.
          call segy_hdrdump(logflg)
        endif
        print*,' '
        call ttrin_segpr(obj)
        call ttrin_prnt(ttrin_stdo(),obj%volume(obj%ireel), &
        obj%tac,obj%idead,obj%gcr,obj%rlav,obj%badcnt)
      endif
!
! determine the next volser to load
      call ttrin_next_vol(obj,vol)
!
!     Moved tot_do check into ttrin_next_vol.
!     ehs 10sep01
!
!     if(vol==' ' .or. obj%tac >= obj%tot_do(obj%ireel)) then
      if(vol==' ' ) then
        NTR= NO_MORE_TRACES
        call string_time(tim)
        write(ttrin_stdo(),*) 'TTRIN: FINISH TIME=',TIM
        call ttrin_wrapup (obj)
        return
      endif
!
!--mount a new volser & position to beginning of 1st trace
      call unix_sleep(obj%sleep_time)!give drive time to load next reel?
      call ttrin_load(obj)
      if(obj%fatal_error) then
        call pc_error('TTRIN: failure to load tape')
        ntr = FATAL_ERROR
        return
      endif
!
      call ttrin_postn(obj)
      if(obj%fatal_error) then
        call pc_error('TTRIN: failure to position to data')
        ntr = FATAL_ERROR
        return
      endif
!
!--Reset the trace and record counters if this is a new volume.
!--(i.e. the first tape out of nvols for the volume)
      if(obj%ivol==1) then
        obj%irec = 0    !trace bypassed on the reel
        obj%tac  = 0    !trace count for ireel
        obj%gcr  = 0    !group count on ireel
        obj%idead= 0    !dead trace count per tape
        obj%badcnt= 0   !number of bad traces if clean is turned on
        obj%rlav = 0.0  !Max Abs Value of all traces on reel (real)
      endif
!
!--Perform the initial trace skip
      ntosk=obj%skip_init(obj%ireel)
      if(obj%ivol>1) ntosk = 0
      nhop=0
      nphys=ntosk*nrpert       !Estimated initial records to skip
      if(obj%tpr > 1) then     !fix tpr iskp bug?
        nphys=ntosk/obj%tpr    !Estimated initial records to skip
      endif
      if(nphys==0 .and. ntosk==0) go to 9     !Is initial skip requested?
! Perform the initial skip on the reel
      ntord = obj%bsize*sizeof(jr)
      do jr=1,nphys
        nrd = tpioclnt_read(obj%fd, obj%rbuf, ntord)
        nwd = nrd/sizeof(jr)
        if(nrd <= 0) then
          call pc_error('TTRIN: failure on initial skip')
          return
        endif
        nhop = nhop + 1
      enddo
      obj%nib = obj%tpr - (ntosk - nphys*obj%tpr)

      obj%irec =obj%irec + (nhop-1)/nrpert + 1
      nby  =0                   !Compute dead traces for output
      do jr=obj%skip_init(obj%ireel)+1, obj%irec
        nnum=jr-obj%skip_init(obj%ireel)-1
        nden=obj%num_do(obj%ireel)+obj%num_skip(obj%ireel)
        ntes=mod(nnum,nden)
        if(ntes.le.obj%num_do(obj%ireel)-1) nby=nby+1
      enddo
      if(nhop.ne.abs(nphys)) then
        write(ttrin_stdo(),*) 'TTRIN: RECORDS SKIPPED=',&
        nhop,' REQUESTED=', nphys
        write(ttrin_stdo(),*) 'TTRIN: DEAD TRACES TO OUTPUT FROM ISKP=',nby
        obj%mntswi=2
        go to 120
      endif
      go to 9
9000  continue
!--- Error handling code here
      hd(:obj%nwih,ntr) = 0d0
      tr(:obj%ndpt,ntr) = 0.0
      call string_compress_blanks(string,i)
      call pc_info(string(1:i))
      ntr = FATAL_ERROR
      return
      end subroutine ttrin
!
      subroutine ttrin_load(obj)
      type(ttrin_struct),intent(inout)    :: obj       ! arguments
      character :: vol*48

      integer   :: scr(12)
      integer   :: tpioclnt_gvol,status

!
!   BLANK OUT PDN IF VSN IS SET AND PDN IS "SCRATCH"
      vol = obj%volume(obj%ireel)
      obj%fd= ttrin_open(obj)
      if(obj%fd < 0) then
        write(ttrin_stdo(),*) 'TTRIN_LOAD: failure to open'
        obj%fatal_error=.true.
        return
      else
        scr=0
        status =  tpioclnt_gvol(obj%fd,scr)
        call string_hh2cc(scr,vol)
        write(ttrin_stdo(),*) 'TTRIN: volser from open =',vol
        write(ttrin_stdo(),*) 'TTRIN: volser from dcode=',&
         obj%volume(obj%ireel)
        if(vol==' ') then
          write(ttrin_stdo(),*) 'TTRIN: blank vol? fd=',obj%fd
        endif
        if(vol /= ' ') obj%volume(obj%ireel) = vol
      endif
      return
      end subroutine ttrin_load
!  
! return the volser of the next tape to be read in
! set the variable ireel,ivol,nvols,vols(:)
      subroutine ttrin_next_vol(obj,vol)
      type(ttrin_struct),intent(inout)    :: obj  !arguments
      character(len=*),intent(out)        :: vol  !arguments
      integer           ::  nvols

      character(len=6)  ::  vols(50)
      character(len=32) ::  userid

      vol = ' '

!     Added tot_do check.   ehs 10sep01
!
      if(obj%ireel == 0 &
    .or. obj%ivol  == obj%nvols &
    .or. obj%tac   >= obj%tot_do(obj%ireel)) then
        obj%ireel = obj%ireel+1  !go to next volume, starts at zero
        obj%ivol  = 0
        obj%nvols = 0
      endif
      if(obj%ireel > obj%nreel) return  !input queue is exhausted
      write(ttrin_stdo(),*) 'ttrin_next_vol volume=',obj%volume(obj%ireel)
      if(obj%nvols==0) then
        if(len_trim(obj%volume(obj%ireel))> 6) then
         call ttrin_getpdn(obj,obj%volume(obj%ireel),nvols,vols,userid)
         write(ttrin_stdo(),*) 'ttrin_next_vol nvols=',nvols
         if(nvols==0) then
           call pc_error ('ttrin: volume is not catalogued!')
           obj%fatal_error = .true.
         else if(nvols < size(obj%vols)) then
           obj%vols(1:nvols) = vols(1:nvols)
         else
           obj%vols(1:size(vols)) = vols(1:size(vols))
         endif
         obj%nvols   = min(nvols,size(vols))
        else
         obj%vols(1) = obj%volume(obj%ireel)
         obj%nvols   = 1
        endif
        obj%ivol = 1
!       obj%volser  will be set in ttrin_open
        vol =  obj%vols(1)
        write(ttrin_stdo(),*) 'ttrin_next_vol:(1) vol=',vol
        return
      endif
      if(obj%ivol < obj%nvols) then
        vol = obj%vols(obj%ivol+1)
        obj%ivol = obj%ivol+1
      endif
      write(ttrin_stdo(),*) 'ttrin_next_vol: vol=',vol
      return
      end subroutine ttrin_next_vol
!  
      subroutine ttrin_change_reel(obj)
      type(ttrin_struct),pointer :: obj
      obj%mntswi=1  !force a reel change
      return
      end subroutine ttrin_change_reel
!  
      subroutine ttrin_postn(obj)
      implicit none
      type(ttrin_struct),pointer :: obj

      type(segy_bin_hdr) :: binhdr
      INTEGER :: IER1
      INTEGER :: BUFLEN,LEN8,NBY,NWRDS
      INTEGER :: I_ERR,I,istat,status

      character(len=80) :: card,rootname
      character(len=8)  :: fmt,hfrmt
      character(len=FILENAME_LENGTH) :: ctemp

      character(len=1024) :: cbuf
!!      CHARACTER      CBUF*1024
!!      character      hfrmt*8
!
! local variables
      INTEGER :: LENDATA,NBLKS
      INTEGER :: I0,IRCODE
      INTEGER :: MAXCRDS
      REAL    :: RNHDWD
      INTEGER :: KL,IRATE,IGRATE,iwdtyp(4),mf
      REAL    :: SR
      INTEGER :: NDPTR,NRD,NTORD
      integer :: tpioclnt_read
      integer :: tpioclnt_peof
      integer :: maxcards,maxwords
      CHARACTER :: hprt*4,pname*8
      logical :: cray
      

!          ttrin_phist also found in bhists routine phshist
      CHARACTER      DTYPID*8,INDIND*8
      CHARACTER VOLO*8,PDNO*48
      INTEGER IBIN(512)
! return status codes
! IER1=0......All is OK. Tape conforms to expected format.
! IER1=1......Non-standard tape format,but found correct trace
!             length or think we found start of data.
! IER1=2......Found some short records before the data
! IER1=3......Too many records(>100) in file before data
! IER1=5......Sequential multiple EOF found on a tape before
!             data, or severe format error
! IER1=10.....DTYPI is not given as DEBE,CPS,SEGY,ISP,OR DIGI
! IER1=11.....Error in NCODE for BHISTS.
! IER1=12.....Fatal error in history  IRCODE=5

      cray=.true.

!        GET THE USERID
      MAXCRDS=600
      IER1 = 0
      OBJ%NFILE= 1
      IF(OBJ%NFILE==0) THEN
        OBJ%NFILE= OBJ%NFILE+1
      ENDIF
      obj%irec = 0
      obj%ngrpc= 0
      obj%ndpttape = obj%ndpt
      lendata=(obj%ndpt+obj%nhdtape+1)/2
      if(obj%cdc==1) lendata=30 + (obj%ndpt*15/32) + .999
!
      INDIND = INDIN(DRIVE)
      DTYPID = obj%format
      CALL TTRIN_CURTAPE(VOLO, obj)
      PDNO= ' '
      if(obj%format=='CPS-NSC' .or. obj%format=='BLK-NSC') then
        if(obj%ireel>1 .or. obj%ivol>1) then
          obj%filenum=1
        endif
      endif
      hfrmt = obj%format
      if(hfrmt=='CPS-NSC') hfrmt= 'CPS'
      if(hfrmt=='BLK-NSC') hfrmt= 'BLK'

!
! Save history records in first file of tape.
! Determine the reel label with a gettp call.
!
 8    CONTINUE
      if(obj%nfile>=obj%filenum .and. obj%irec==obj%recnum) then
        call ttrin_get_vol(obj%fd,voli)
        if(voli==' ') voli=obj%volume(obj%ireel)
        write(ttrin_stdo(),*) 'TTRIN_POSTN: volser=',voli
! *** For Cray input, get #header words from history if possible
        fmt = obj%format
        if((fmt(1:4)=='CPS'.or. &
          (fmt=='CPS-NSC'.and.obj%ireel==1.and.obj%ivol==1).or. &
          fmt(1:3)=='BLK') &
          .and. obj%history=='YES') then
          rnhdwd=0
!            Set the number of header words on tape - default 32
          if(obj%nwihcray.gt.0)then
            obj%nhdtape=obj%nwihcray
          endif
        endif
!
!********************************************************************
! *** Add data cards to history file.  This is the ireel reel card.**
        if(obj%cdc==1) then
            nblks = (NTHDC-1)/480 + 1
        else
            nblks = 1
        endif
        write (card,'(1x,a6,4i7,1x,a44)') voli(1:6),&
           obj%skip_init(obj%ireel),obj%num_do(obj%ireel),&
           obj%num_skip(obj%ireel), &
           obj%tot_do(obj%ireel),obj%volume(obj%ireel)(1:44)
!     write(ttrin_stdo(),*) 'TTRIN: ',card
        i = hist_write(obj%ipn,card)
        call manhist_finished_reading
        return !DATA START
      endif ! obj%nfile==obj%FILENUM .AND. obj%IREC==obj%RECNUM
      BUFLEN=0
      I0 = BUFLEN + 1
      NTORD = OBJ%BSIZE*SIZEOF(I0)
      nrd = tpioclnt_read(obj%fd,obj%rbuf(i0),ntord)
      IF(NRD < 0) GOTO 30     !UNRECOVERED ERROR
      IF(NRD==0) GOTO 20     !EOD OR EOF
      BUFLEN = BUFLEN + NRD/SIZEOF(I) - obj%cdc
!
!  Normal record read
      obj%irec=obj%irec+1
      obj%ngrpc=obj%ngrpc+1

      if(obj%format=='SEGY' .and. obj%ngrpc==1) then  !DUMP SEGY HEADER
        if(nrd /= 3200) THEN
         write(ttrin_stdo(),*) 'TTRIN_POSTN: WRONG SEGY RECORD LENGTH'
         obj%fatal_error=.true.
         return
        endif
        call wrdc_ebc_asc_c(obj%rbuf, nrd)
        if(obj%mess_segy=='YES') then
          write(ttrin_stdo(),'(A)') &
       ' TTRIN_POSTN:  DUMP OF 3200 BYTE SEGY HEADER RECORD'
          write(ttrin_stdo(),'(1X,20A4)') (OBJ%RBUF(KL),KL=1,800)
        endif
      endif


      if(obj%format=='SEGY' .and. obj%ngrpc==2) then  !DUMP SEGY binary HEADER
!  Check the sample rate
        call cmem_cpy_c(ibin,obj%rbuf,obj%nbytesnword*100)
!!!        ibin(1:100) = transfer(obj%rbuf(1:100),ibin(1:100))
        status = segy_buf2binh(ibin,binhdr)
        obj%sgy_ln = binhdr%reno
        ndptr = binhdr%hns
        if(binhdr%format==1) iwdtyp = 2;
        if(binhdr%format==3) iwdtyp = 5;
        if(binhdr%format==5) iwdtyp = 16;
        if(binhdr%format>5)  iwdtyp = 15;
        sr = binhdr%hdt
        sr = sr/1000000.0
        mf = binhdr%mfeet

        WRITE(ttrin_stdo(),'(A)') &
       '  TTRIN: SEGY trace length and sample rate'
        WRITE(ttrin_stdo(),*) ' TTRIN: NDPTR=',NDPTR,' SR=',SR
        IRATE = SR*1000000
        IGRATE= OBJ%DT*1000000
        IF(IRATE==0)THEN
          write(stdo,*)'*** WARNING in TTRIN---SEGY sample rate header set to&
     & zero'
        ELSE IF(irate.ne.igrate)THEN
          if(sr <= 0) then
           write(stdo,*) 'WARNING: sample rate mismatch-init&
     & global is honored'
          else
           write(stdo,*)'*** ERROR *** in TTRIN--SEGY sample rate does not matc&
     &h global sample rate'
           write(stdo,*)' SEGY rate = ',sr,' global rate = ',obj%dt
!          CALL TTRIN_DELETE(obj)
           obj%fatal_error=.true.
           return
          endif
        ENDIF
!       obj%fatal_error=.true.
!       if(obj%fatal_error) return
      ENDIF
!
! SHORT RECORD
      if(nrd < 80) then
        if(obj%cdc==1) then
          obj%nfile=obj%filenum
          obj%irec=obj%recnum
          write(ttrin_stdo(),*) &
          'TTRIN_POSTN: no eof but short record,coni tape?'
        endif
        go to 8
      endif
!
! TAPE LABEL?
      if(nrd == 80) then
        call cmem_cpy_c(card,obj%rbuf,80)
!!!        card = transfer(obj%rbuf(1:20), card(1:80))
        write(ttrin_stdo(),*) &
        'TTRIN_POSTN:(80) ',card
        goto 8
      endif
!
!
! SEGD and SERC do not have history info
      IF(FMT(1:4)=='SEGD'.OR.FMT(1:4)=='SERC')RETURN
!
! If type is CPS, check for a global record
! Check consistency of init globals and tape globals
      if(obj%format=='CPS'.or. &
        (obj%format=='CPS-NSC'.and.obj%ireel==1.and.obj%ivol==1).or. &
        obj%format(1:3)=='BLK') then
        if(obj%irec < 6 .and. .not. obj%isnewcps) then
          nby = min(1024,nrd)
          nwrds   = nby/sizeof(nby) !number of words in rbuf
          call cmem_cpy_c(cbuf,obj%rbuf,nby)
!!!          cbuf(1:nby)=transfer(obj%rbuf(1:nwrds),cbuf(1:nby))
!         write(ttrin_stdo(),*) cbuf(1:80)
          call ttrin_rdheader(obj,cbuf,obj%isnewcps,i_err,maxcards)
          if(i_err < 0) then
            obj%fatal_error=.true.
            return
          endif 
          if(obj%isnewcps) then
            cray=.false.
            maxwords=maxcards*80/sizeof(maxcards)
            if(maxwords.gt.obj%bsize)then
              deallocate(obj%rbuf)
              allocate(obj%rbuf(maxwords),stat=i_err)
              if(i_err .ne. 0)then
                obj%fatal_error=.true.
                write(ttrin_stdo(),*)'TTRIN-Unable to reallocate rbuf'
                return
              endif
              obj%bsize=maxwords  
            endif
            goto 8
          endif
        endif
      endif

      IF(obj%history=='NO') GOTO 8
!
!  Deal with tapes that may have history records
      if ( (obj%format=='ISP')  .OR. &
           (obj%cdc==1)         .OR. &
           (obj%format=='CPS') .OR. &
           (obj%format=='CPS-NSC'.and.obj%ireel==1.and.obj%ivol==1) .OR. &
           (obj%format=='BLK-NSC'.and.obj%ireel==1.and.obj%ivol==1) .OR. &
           (obj%format=='DEBE') ) THEN
         len8 = nrd/8
         ircode=0
         hprt ='NO'
         pname='TTRIN'
         call cmem_cpy_c(ctemp,obj%rbuf,obj%nbytesnword)
!!!         ctemp=transfer(obj%rbuf(1),ctemp)
         if(cray)then
           call wrdc_ebc_asc(ctemp)
         endif
         if(ctemp(1:3).ne.'PR=')then
           call manhist_putrecord(obj%rbuf,maxcards,istat,cray,obj%nwihcray,&
                                  obj%ndptcray)
           if(istat.ne.0)then
             obj%fatal_error=.true.
             return
           endif
         else
           call ttrin_save_prec(obj,nrd,cray)
         endif
         call ttrin_rdheader(obj,cbuf,obj%isnewcps,i_err,maxcards)
      endif
!
      if(obj%irec >= 0) goto 8  !Loop till EOF, NO LIMIT ON NO. OF PR= RECORDS

      IER1=MAX(IER1,3)
      IF(BUFLEN==LENDATA) THEN
        WRITE(ttrin_stdo(),*) 'TTRIN_POSTN: BAD TAPE FORMAT BUT CORRECT'
        WRITE(ttrin_stdo(),*) ' TRACE LENGTH FOUND-GO FOR IT'
        RETURN
      ELSE
        WRITE(ttrin_stdo(),*) 'TTRIN_POSTN:  *****??BAD TAPE FORMAT??******'
        IER1=5
        RETURN
      ENDIF
!
!   EOF mark encountered
 20   continue
      if(obj%format=='CPS'.or.obj%format=='CPS-NSC'.or. &
         obj%format(1:3)=='BLK') then
        call ttrin_save_prec(obj,0,cray)
        if(obj%fatal_error)return
!             If velocity model permsave - get the filenames
           if(obj%path_vmod.ne.PATHCHECK_EMPTY)then
             call getlun(obj%lun_fn,istat)
             if(istat.ne.0)then
               write(ttrin_stdo(),*)'TTRIN_postn --> Unable to get a unit &
                                    &number for filename file'
               obj%fatal_error=.true.
               return
             endif
             ctemp=trim(obj%path_vmod) // '/%trin_filenames'
             open(obj%lun_fn,file=trim(ctemp),status='old',iostat=istat)
             if(istat.ne.0)then
               write(ttrin_stdo(),*)'TTRIN_postn --> Unable to open &
                                    &%trin_filenames'
               obj%fatal_error=.true.
               return
             endif
             read(obj%lun_fn,'(A)',iostat=istat)card
!                Use path_vmod to save files
             rootname=path_get_file(card)
             card=' '
             card=trim(obj%path_vmod) // '/' // trim(rootname)
!
             if(istat.ne.0)then
               write(stdo,*)'TTRIN_postn --> Error encountered reading &
                                    &vmod filenames'
               obj%fatal_error=.true.
               return
             endif
             call getlun(obj%lun_modgrid,istat)
             if(istat.ne.0)then
               write(ttrin_stdo(),*)'TTRIN_postn --> Unable to get unit number &
                                    &for modgrid file'
               obj%fatal_error=.true.
               return
             endif
             call ttrin_open_vmod(obj,card)
             if(obj%fatal_error)return
           endif

        if(obj%nwihcray.gt.0)then
          obj%nhdtape=obj%nwihcray
        endif
        if(obj%ndptcray.gt.0)obj%ndpttape=obj%ndptcray               
        if(obj%format=='BLK'.and. obj%ndpttape /= obj%ndpt)then
          WRITE(ttrin_stdo(),*) 'TTRIN_POSTN-->BLK error in trace length'
          WRITE(ttrin_stdo(),*) 'TTRIN_POSTN: Number of samples on tape = ',&
           obj%ndpttape
          WRITE(ttrin_stdo(),*) 'TTRIN_POSTN: Number samples requested  = ',&
           OBJ%NDPT
          obj%fatal_error=.true.
          return
        endif
      endif
      i_err = tpioclnt_peof(obj%fd)
      if(i_err==-1) then
        obj%fatal_error=.true.
        write(ttrin_stdo(),*) 'TTRIN_POSTN- EOFS ERROR!'
        return
      endif
! don't increment file count for an eof after the label
      if(obj%irec > 1) then
        obj%nfile=obj%nfile+1
        write(ttrin_stdo(),*) 'TTRIN_POSTN- EOF AT IREC=',obj%irec
      else
        if(obj%isnewcps) then
          obj%nfile=obj%nfile+1
        else
          write(ttrin_stdo(),*) 'TTRIN_POSTN- EOF AFTER TAPE LABEL?'
        endif
      endif
      if(obj%irec==0) then
        obj%fatal_error=.true.
        write(ttrin_stdo(),*) 'TTRIN_POSTN- MULTIPLE EOF FOUND BEFORE DATA'
        return
      else
        obj%irec=0
        len8 = nrd/8
        if(buflen > 1 .and. buflen < 4080) then
!         perm save files handled in history before - handle this in ttrin
!fixit         call histroy_db(obj%ipn,'TTRIN',obj%rbuf,len8,obj%rest_ps,volo,&
!fixit         maxcrds,ncrds,cards, ircode)
!        do ncr=1,ncrds
!         write(ttrin_stdo(),*) cards(ncr)
!        enddo
         if(ircode==5) then
!         HAVE FATAL ERROR IN history_db  -  SET TO ABORT JOB
          obj%fatal_error=.true.
          return
         endif
        endif
        go to 8
      endif
!
!   Read problems
 30   write(ttrin_stdo(),*) 'TTRIN_POSTN: Read error'
      obj%fatal_error=.true.
      return
!
      write(ttrin_stdo(),*) 'TTRIN_POSTN- error in BHISTS setup!'
      obj%fatal_error=.true.
      return
      end subroutine ttrin_postn
!
      subroutine ttrin_save_prec(obj,nby,cray)
      type(ttrin_struct),intent(inout) :: obj
      integer,intent(in) :: nby
      logical,intent(in) :: cray
      character(len=32320)    :: ctemp
      character(len=80),save  :: prenam
      character(len=120),save :: ofile=' '
      character(len=8),save   :: htype
      integer,save            :: i1,i2
      integer                 :: i,l,ncards120,nwrds,status,stdo
      logical,save            :: do_rcp=.false.
      integer,save            :: nbycnt=0
      character(len=120)      :: rfile,lcard
      stdo = ttrin_stdo()
      nwrds = nby/sizeof(nby)
      if(nby>32320) then
        write(stdo,*) 'TTRIN_SAVE_PREC: PROCESSING RECORD TOO BIG!'
        return
      endif
      ctemp(1:80)='PR=CON '
      if(nby> 0) then
        call cmem_cpy_c(ctemp,obj%rbuf,nby)
!!!        ctemp(1:nby) = transfer(obj%rbuf(1:nwrds),ctemp(1:nby))
      else
        write(stdo,*) 'ttrin_save_prec: nby<=0'
        return
      endif
      if(cray)then
        call wrdc_ebc_asc(ctemp)
      endif
      if(ctemp(1:3) /= 'PR=') return
      htype='PRCON'
      if(ctemp(1:6) /= 'PR=CON') THEN
        htype='PR'
        prenam=ctemp(4:67)
        i = index(ctemp,',NCHPL')
        if(i>4) then
          prenam = ctemp(4:i-1)
        endif
        !           Ignore vax path name and put in current directory
        i1= index(prenam,']')
        if(i1 == 0 .or.i1 >= 80)THEN
          i1=1
        else
          i1=i1+1
        endif
        !           Also remove version number
        i2=index(prenam,';')
        if(i2 <= 0 .or. i2 > 80)then
          i2=80
        else
          i2=i2-1
        endif
        write(stdo,*) 'TTRIN: PROCESSING RECORD FILE=',&
         trim(prenam)
        write(stdo,*) 'TTRIN: PROCESSING RECORD #BYTES LAST=',nbycnt&
       &,' CURRENT=',nby
        nbycnt=nby
      else
        nbycnt = nbycnt+nby
      endif
      if(obj%rest_ps /= 'YES') return

      if(htype=='PR' .or. nby==0) then
        if(do_rcp) then
          if(obj%frontend_user==' ') then
            rfile = trim(obj%frontend_node)//':'&
          &//trim(obj%frontend_path)//trim(ofile)
          else
            rfile = trim(obj%frontend_user)//'@'//trim(obj%frontend_node)//':'&
          &//trim(obj%frontend_path)//trim(ofile)
          endif
          if(obj%frontend_path/=' ' .and. ofile /= ' ') then
            call rcpfile(trim(ofile),rfile,'PUT')
          endif
        endif
      endif
      if(nby==0) return
      if(htype == 'PR') then
        if(prenam(1:1)=='/'.or.prenam(1:1).eq.'.') then
          prenam = path_get_file(prenam)
          i1=1
          i2=len_trim(prenam)
        endif
        ofile = prenam(i1:i2)
        if(.not.do_rcp) then
          if(obj%path_vmod.ne.PATHCHECK_EMPTY)then
            ofile = trim(obj%path_vmod)// '/' // prenam(i1:i2)
          else
            ofile = trim(obj%frontend_path)//prenam(i1:i2)
          endif
        endif
        if(ofile==' ') then
          write(stdo,*) 'ttrin_save_prec: bad file name!'
          write(stdo,*) 'ctemp=',ctemp(1:80)
          return
        endif
        open(obj%lun_prec,file=ofile,status='replace', iostat=status)
        if(status /=0) then
          write(stdo,*) 'ttrin_save_prec: open failed!'
          write(stdo,*)trim(ofile)
          return
        endif
!!!        fh = cio_fopen(ofile,'w')
      else
        if(ofile==' ') then
          write(stdo,*) 'ttrin_save_prec: bad file name!'
          write(stdo,*) 'ctemp=',ctemp(1:80)
          return
        endif
        open(obj%lun_prec,file=ofile,status='old',position='append',&
        iostat=status)
        if(status /=0) then
          write(stdo,*) 'ttrin_save_prec: open failed!'
          return
        endif
!!!        fh = cio_fopen(ofile,'a')
      endif
!           TTROT at one time had a bug which added a card of garbage at
!              the end of the processing record.  Check the last card and
!              skip it if it is garbage.
      ncards120=nby/120
      lcard=ctemp((ncards120-1)*120+1:i*120)
      do i=1,120
        l=ichar(lcard(i:i))
        if(l.lt.0.or.l.gt.127)then
          ncards120=ncards120-1
          exit
        endif
      enddo
      do i = 3,ncards120   !2 starts just after PR= card
        lcard = ctemp((i-1)*120+1:i*120)  
        l = index(lcard,char(10))
        if(l>0) then
          lcard(l:120) = ' '
        else
          l = len_trim(lcard) + 1
        endif
        write(obj%lun_prec,'(a)')lcard(1:l)
!!!        status = cio_fputline(lcard(1:l), 120,fh)
      enddo
      close(obj%lun_prec)
!!!      status = cio_fclose(fh)

      return
      end subroutine ttrin_save_prec
!  
      subroutine ttrin_rdheader(obj,cbuf,isnewcps,i_err,maxcards)
      type(ttrin_struct),intent(inout) :: obj
      character(len=*),intent(inout)   :: cbuf
      integer,intent(out)              :: i_err
      logical,intent(out)              :: isnewcps
      integer,intent(out)              :: maxcards
      character        :: names(17)*12
      integer          :: nnames=16
      integer          :: nbits,nbits_hd,ndpt,nwih,endian
      real             :: dt, tstrt
      character(len=8) :: wdtype,history
      double precision :: xorigin,yorigin,dx11,dx22,dx12,dx21
      integer          :: i,n,lc,tape_ms,init_ms,stdo
      integer          :: tpr

      maxcards=0
      i_err=0
      stdo = ttrin_stdo()
      if(index(cbuf(1:128),'#<CPS_v1') > 0) then
       write(stdo,*) '+ TTRIN_RDHEADER: FOUND NEW-CPS GLOBAL RECORD'
       isnewcps=.true.
      else
       return
      endif
      do i=1,512
        if(ichar(cbuf(i:i)) < 32) cbuf(i:i)=' '
      enddo
      wdtype  = ' '
      history = ' '
      nbits   = 0
      nbits_hd= 0
      nwih    = 0
      ndpt    = 0
      dt      = 0.0
      endian  = -1
      tstrt   = -1.0
      tpr     = -1
      names(1) = 'ndpt='
      names(2) = 'tstrt='
      names(3) = 'dt='
      names(4) = 'nwih='
      names(5) = 'history='
      names(6) = 'wdtype='
      names(7) = 'nbits='
      names(8) = 'nbits_hd='
      names(9) = 'endian='
      names(10)= 'xorigin='
      names(11)= 'yorigin='
      names(12)= 'dx11='
      names(13)= 'dx21='
      names(14)= 'dx22='
      names(15)= 'dx12='
      names(16)= 'tpr='
      names(17)= 'maxcards='
      do i = 1,nnames
        n = index(cbuf(1:1000),trim(names(i)))
        if(n > 0) then
         lc = n+len_trim(names(i))
         if(i==1) read(cbuf(lc:),*) ndpt
         if(i==2) read(cbuf(lc:),*) tstrt
         if(i==3) read(cbuf(lc:),*) dt
         if(i==4) read(cbuf(lc:),*) nwih
         if(i==5) read(cbuf(lc:),*) history
         if(i==6) read(cbuf(lc:),*) wdtype
         if(i==7) read(cbuf(lc:),*) nbits
         if(i==8) read(cbuf(lc:),*) nbits_hd
         if(i==9) read(cbuf(lc:),*) endian
         if(i==10) read(cbuf(lc:),*) xorigin
         if(i==11) read(cbuf(lc:),*) yorigin
         if(i==12) read(cbuf(lc:),*) dx11
         if(i==13) read(cbuf(lc:),*) dx21
         if(i==14) read(cbuf(lc:),*) dx22
         if(i==15) read(cbuf(lc:),*) dx12
         if(i==16) read(cbuf(lc:),*) tpr
        else
         write(stdo,*) 'ttrin_rdheader: did not find ',trim(names(i))
        endif
      enddo
      if(tpr /= -1) obj%tpr = max(1,tpr)
      write(stdo,*) '+ TTRIN_RDHEADER: traces per record = ',obj%tpr
      if(nbits > 0) obj%nbits = nbits
      if(nwih > 0)then
        obj%nhdtape = nwih
      endif
      if(ndpt > 0) obj%ndpttape=ndpt
      if(nbits_hd > 0) obj%nbits_hd = nbits_hd
      if(wdtype /= ' ') obj%wdtype = wdtype
      if(history/= ' ') then
        if(history(1:4) /= 'NONE') then
         obj%history = 'YES'
        else
         obj%history = 'NO'
        endif
      endif
      obj%bytes_per_trace =  (obj%nbits/8)*obj%ndpttape + &
        (obj%nbits_hd/8)*obj%nhdtape
      if(endian /= -1) obj%endian = endian
      write(stdo,*) '+ TTRIN_RDHEADER: ENDIAN  =',OBJ%ENDIAN
      write(stdo,*) '+ TTRIN_RDHEADER: NBITS_HD=',OBJ%NBITS_HD
      if(obj%nwih /= nwih) then
        write(stdo,*) '+ TTRIN_RDHEADER: WARNING-GLOBAL MISMATCH'
        write(stdo,*) '+ TTRIN_RDHEADER: NWIH FORM INIT=',OBJ%NWIH
        write(stdo,*) '+ TTRIN_RDHEADER: NWIH FROM TAPE =',NWIH
      endif
      if(obj%ndpt /= ndpt) then
        write(stdo,*) '+ TTRIN_RDHEADER: WARNING-GLOBAL MISMATCH'
        write(stdo,*) '+ TTRIN_RDHEADER: NDPT FROM INIT=',OBJ%NDPT
        write(stdo,*) '+ TTRIN_RDHEADER: NDPT FROM TAPE =',NDPT
      endif
      if(obj%tstrt /= tstrt) then
        write(stdo,*) '+ TTRIN_RDHEADER: WARNING-GLOBAL MISMATCH'
        write(stdo,*) '+ TTRIN_RDHEADER: TSTRT FROM INIT=',OBJ%TSTRT
        write(stdo,*) '+ TTRIN_RDHEADER: TSTRT FROM TAPE =',TSTRT
      endif
      tape_ms=(dt +.00005)*1000
      if(tape_ms /=0) then
       init_ms =(obj%dt+.00005)*1000
       if(init_ms /= tape_ms)    THEN
        i_err=-1
        write(stdo,*) '+ TTRIN_RDHEADER: ERROR-DT GLOBAL MISMATCH'
        write(stdo,*) '+ TTRIN_RDHEADER: DT FROM INIT=',OBJ%DT
        write(stdo,*) '+ TTRIN_RDHEADER: DT FROM TAPE =',DT
        return
       endif
      endif

      return
      end subroutine ttrin_rdheader
!  
      subroutine ttrin_western_xy(obj,hd)
      type(ttrin_struct),intent(inout) :: obj
      double precision,intent(inout) :: hd(:)
      real hdloc(4),hsav
      integer i,cnt
      save hsav
      data cnt/0/
      data hsav/-99999.230/
      i = 1 + 209/sizeof(hsav)
      call wrdc_ibm_to_float(obj%rbuf(i:),hdloc, 2)
      hd(7) = hdloc(2)
      hd(8) = hdloc(1)
      if(abs(hsav- hd(58)) > 0.001) then
        cnt=1
        hsav= hd(58)
      else
        cnt = cnt+1
      endif
      hd(59) = cnt
      hd(57) = obj%nseqfil

      return
      end subroutine ttrin_western_xy
!  
      subroutine ttrin_tcf(obj,nrd,hd,tr)
      implicit none
      type(ttrin_struct),intent(inout) :: obj
      integer,intent(in) :: nrd
      real,intent(inout) :: tr(:)
      double precision,intent(inout):: hd(:)

      integer            :: stdo
      logical            :: lswap,killed
      real               :: big
      type(segy_trc_hdr) :: syh
!-----------------------------------------
! NRD= number of bytes in input buffer 
! VAX and CPS TAPES have 32, 32 bit IBM F.P., header words
! SEGY tapes have 60, 32 bit IBM integer, header words
! CONSEIS DEBE tapes have 15, 32 bit IBM, header words
! DIGICON tapes have 36 header words(32 bit VAX f.p. and integers)
!**************************************************************
      real     thd(96),hvals(96)
      integer  ihd(100),iwdtype(64),i,k,isb,ncon,swrd,nwrd
      character line(96)
      double precision tstart
      integer  line_hdr
      logical :: logflg
      equivalence (thd,ihd)
!
!  Initialize the output header & trace  words
      DO K=1,OBJ%NWIH
        HD(K)=0.0
      ENDDO
      HD(1)=OBJ%NSEQI
      HD(2)=OBJ%NDPT
      DO K=1,OBJ%NDPT
        TR(K)=0.0
      ENDDO
      stdo = ttrin_stdo()
!
! for cps tapes the data is normally stored as swap_endian()=1
! obj%endian = as stored on tape(defaults to 1 if unknown)
      lswap = .false.
!
!
! SEGY TAPE FORMAT-HEADER AND DATA IS IBM
! NHDWD=No. OF HEADERS ON THE TRACES OF THE CURRENT REEL(60)
      if(obj%format=='SEGY') then
        obj%nhdtape=60
        nwrd = 240/sizeof(I)
!       do i = 1,nwrd   !ABSOFT BUG ON INTEL, checks for ligit number????
!        thd(i) = obj%rbuf(i)
!       enddo
        thd(1:nwrd) = obj%rbuf(1:nwrd)
        if(obj%endian == swap_endian() ) then
          lswap = .false.
        else
          lswap = .true.
        endif
!       ival = ihd(21)
!       if (lswap ) call swap_bytes(ival,4)
!       write(ttrin_stdo(),*) 'irec=',obj%irec,' ival=',ival

        call segy_unpack_segyhd(syh,ihd(1:nwrd),lswap)
        logflg=.false.
        if(obj%dump_segy.eq.'YES')call segy_hdrdump(logflg,syh)

        tstart = obj%tstrt
        call segy_segyhd_to_cpshd(hd,syh,tstart)
        nwrd = (nrd-240)/4
        swrd = 1 + 240/sizeof(i)
        ncon = min(obj%ndpt,nwrd)
        lswap = .false.
        call wrdc_ibm_to_float(obj%rbuf(swrd:swrd+ncon-1),tr(1:ncon),&
         ncon, lswap)
!
! Do additional custom mapping of segy to cps headers
        if(obj%nummap > 0 .and. obj%mod_segy(1:1)=='Y') then
          do i =1,obj%nummap
            iwdtype(i)=0
            if(obj%wtype(i)(1:1)=='F') iwdtype(i)=1
          enddo
          call ttrin_map_segy_to_cps(obj%rbuf,obj%nummap,obj%bytes,&
          obj%sbyte, obj%cps_hdr, iwdtype, hd)
        endif
        if(obj%version(1:8)=='WESTERN1') then
          line_hdr=58
          hd(line_hdr)= obj%sgy_ln;   !preserved from 400 byte record
          call  ttrin_western_xy(obj,hd)
        endif

        go to 1000
      endif
!
! CPS FORMAT TAPE-HEADER AND DATA IS IBM FLOATING POINT.
! NHDWD=No. OF HEADERS ON THE TRACES OF THE CURRENT REEL(32)
!
!  The following message should only be issued when reading
!  very old Cray tapes (when #headers=32 and not in history)
      if(obj%format(1:3)=='CPS' .or.  obj%format(1:3)=='BLK') then
        if(obj%nseqi <= 10 .and. obj%nhdtape /= obj%nwih) then
          write(stdo,*) 'TTRIN: WARNING* Number of header words ',&
         &'read on tape= ',obj%nhdtape,' from init=',obj%nwih
        end if
        if(obj%endian == swap_endian() ) then
          lswap = .false.
        else
          lswap = .true.
        endif
        hvals=0
        if(obj%wdtype(1:3)=='IBM') then
          lswap = .false.
!         call wrdc_ibm_to_float(obj%rbuf(1:obj%nhdtape),&
!          hvals(1:obj%nwih), obj%nhdtape, lswap)
           hvals(1:64) = obj%rbuf(1:64)
           call wrdc_ibm_to_float_c(hvals,obj%nhdtape,swap_endian())
!         write(stdo,*) 'irec=',obj%irec,'nrd=',nrd,' hvals(7)=',hvals(7)
!         call wrdc_ibm_to_float(obj%rbuf,hvals, obj%nhdtape, lswap)
          hd(1:obj%nwih) = hvals(1:obj%nwih)
!         write(stdo,*) 'A irec=',obj%irec,'nrd=',nrd,' hvals(7)=',hvals(7),&
!         &'hd(7)=',hd(7)
          swrd = 1 + obj%nhdtape*(4/sizeof(obj%rbuf(1)))
        else
          if(obj%nbits_hd==8*sizeof(hd(1))) then
            nwrd = obj%nhdtape*(obj%nbits_hd/(8*sizeof(obj%rbuf(1)) ))
            if(lswap) then
              call swap_bytes(obj%rbuf(1),nwrd)
            endif
            call cmem_cpy_c(hd,obj%rbuf,obj%nhdtape*obj%nbytesnword*2)
!!!            hd(1:obj%nhdtape) = transfer(obj%rbuf(1:nwrd),(/0.0d0/))
!           if(lswap) call swap_bytes(hd(1),obj%nwih)
          else
            if(obj%nbits_hd == 8*sizeof(obj%rbuf(1))) then
              if(lswap) then
                call swap_bytes(obj%rbuf(1),obj%nhdtape)
              endif
              hvals(1:obj%nhdtape) = obj%rbuf(1:obj%nhdtape)
!             if(lswap) call swap_bytes(hvals(1),obj%nhdtape)
              hd(1:obj%nwih) = hvals(1:obj%nwih)
            else
              write(stdo,*) 'TTRIN: ERROR nbits_hd=',obj%nbits_hd
              write(stdo,*) 'TTRIN: ERROR sizeof(real)=',sizeof(hvals(1))
              write(stdo,*) 'TTRIN: ERROR sizeof(double)=',sizeof(hd(1))
              write(stdo,*) 'TTRIN: nbits_hd must match a real or double!'
              obj%fatal_error=.true.
              return
            endif
          endif
          swrd = 1 + obj%nhdtape*(obj%nbits_hd/(8*sizeof(obj%rbuf(1))))
        endif
!
!  swrd is the word where the trace data starts in the input buffer
!  ncon is the number of trace values to convert
        isb =mod(obj%nhdtape,2)*4 + 1
        if(isb /= 1) then
          write(stdo,*) 'ISB != 1, AB'
          obj%fatal_error=.true.
          return
        endif
        if(obj%wdtype=='IBM') then
          lswap = .false.
          ncon=(8*nrd - obj%nhdtape*obj%nbits_hd)/32
          ncon=min(obj%ndpt,ncon)
          ncon=min(obj%ndpttape,ncon)
!         call wrdc_ibm_to_float(obj%rbuf(swrd:swrd+ncon-1),tr(1:ncon),&
!          ncon, lswap)
          tr(1:ncon) = obj%rbuf(swrd:swrd+ncon-1)
          call wrdc_ibm_to_float_c(tr,ncon,swap_endian())
        else
          if(8*sizeof(tr(1))==obj%nbits) then
            ncon=(8*nrd - obj%nhdtape*obj%nbits_hd)/obj%nbits
            ncon=min(obj%ndpt,ncon)
            ncon=min(obj%ndpttape,ncon)
            if(lswap) then
              call swap_bytes(obj%rbuf(swrd), ncon)
            endif
            tr(1:ncon) = obj%rbuf(swrd:swrd+ncon-1)
!           if(lswap) call swap_bytes(tr(1),ncon)
          else
            write(stdo,*) 'TTRIN_TCF: ERROR nbits /= sizeof(real)'
            write(stdo,*) 'TTRIN_TCF: ERROR nbits = ',obj%nbits
            obj%fatal_error=.true.
! convert to double from rbuf and transfer????
            return
          endif
        endif
        go to 1000
      endif  !CPS or BLK
!
! ISP FORMAT TAPE-HEADER AND DATA ARE IBM 32 BIT FLOATING POINT.
! NHDWD=No. OF HEADERS ON THE TRACES OF THE CURRENT REEL(32)
      if(obj%format(1:3)=='ISP') then
        call wrdc_ibm_to_float(obj%rbuf,thd, obj%nhdtape, lswap)
        ncon=nrd/4-obj%nhdtape
        ncon=min(obj%ndpt,ncon)
        isb =mod(obj%nhdtape,2)*4 + 1
        if(isb.ne.1) then
          write(stdo,*) 'ISB != 1, Ac'
          stop
        endif
        swrd = obj%nhdtape+1
        if(sizeof(i)==8) swrd = obj%nhdtape/2+1
        call wrdc_ibm_to_float(obj%rbuf(swrd:), tr,ncon,lswap)
! Put the ISP headers in the appropriate Cray header locations
        hd(2)=thd(5)
        hd(3)=thd(3)
        hd(4)=thd(4)
        hd(5)=thd(17)
        hd(6)=thd(9)
        hd(7)=thd(22)
        hd(9)=thd(6)
        hd(11)=thd(18)
        hd(12)=thd(19)
        hd(14)=thd(20)
        hd(15)=thd(21)
        hd(17)=thd(7)
        hd(18)=thd(8)
        hd(19)=thd(14)
        hd(41)=thd(10)
        hd(46)=thd(15)
        hd(47)=thd(16)
        go to 1000
      endif
!
! DEBE FORMAT TAPE-IBM INTEGER HEADERS AND IBM F.P. DATA
! NHDWD=15, DATA STARTS AT CRAY WORD 8
      if(obj%format(1:4) == 'DEBE') then
        do i=1,obj%nhdtape
          call ttrin_convstr(obj%rbuf(i),ihd(i), 4)
        enddo
        ncon=nrd/4-obj%nhdtape
        ncon=min(obj%ndpt,ncon)
        isb =mod(obj%nhdtape,2)*4 + 1
        if(isb.ne.1) then
          WRITE(stdo,*) 'ISB != 1, AD'
          stop
        endif
        swrd = obj%nhdtape+1
        if(sizeof(i)==8) swrd = obj%nhdtape/2+1
        call wrdc_ibm_to_float(obj%rbuf(swrd:), tr,ncon,lswap)
! Put the DEBE headers in the appropriate Cray header locations.
        hd(2)=ihd(2)   !mute
        hd(3)=ihd(8)   !profile number
        hd(6)=ihd(6)   !offset
        hd(7)=ihd(4)   !CMP number
        hd(9)=ihd(3)   !original shot number
        hd(11)=ihd(13) !Source East
        hd(14)=ihd(14) !Receiver East
        hd(17)=ihd(5)  !CMP East
        go to 1000
      endif
!
! Other tape formats
      write(line,'(''TTRIN: NO SUPPORT FOR FORMAT='',A)') &
       trim(obj%format)
      call pc_error('TTRIN: failure on initial skip')
      obj%fatal_error=.true.
      return


 1000 CONTINUE
!
!--- Set mutes and LAV header words
      if(obj%clean /= 'NONE') then
       if(obj%clean=='ZERO') then
         i = clean_zero(tr(1:obj%ndpt))
         if( i > 0) obj%badcnt = obj%badcnt + 1
       endif
       if(obj%clean=='KILL') then
         killed = clean_kill(tr(1:obj%ndpt))
         if( killed) obj%badcnt = obj%badcnt + 1
       endif
      endif
      if(hd(64)<=1) hd(64)=obj%ndpt
      call mutehw(hd(:),tr(:),obj%ndpt,0.0,0)
      call lav_set_hdr (hd(:),tr(:),obj%ndpt)
      big = hd(25)
      obj%rlav = max(obj%rlav,big)
      if(hd(25) == 0.) obj%idead=obj%idead+1
      hd(1) = obj%nseqi

!
      return
      end subroutine ttrin_tcf

      subroutine ttrin_new_cps_tcf(obj,nrd,hd,tr)
      implicit none
      type(ttrin_struct),intent(inout) :: obj
      integer,intent(in)               :: nrd
      double precision,intent(inout)   :: hd(:)
      real,intent(inout)               :: tr(:)

      integer      :: swrd
      integer      :: nwrd
      integer      :: ncnv
      logical      :: lswap,killed
      real         :: big
      real         :: hvals(96)
      integer      :: isb
      integer      :: stat

      hd = 0.0
      tr = 0.0
      hd(25) = 0.0
      hd(2)  = 1
      if(.not. obj%isnewcps) return
      if(obj%nib==0) then
        write(stdo,*) 'TTRIN_NEW_CPS_TCF: no data? nib=0'
        obj%fatal_error=.true.  !this should never happen
        return
      endif
      ! 0 < nib <= tpr
      swrd = (obj%tpr-obj%nib) *(obj%bytes_per_trace/4) + 1
      nwrd = obj%bytes_per_trace/4
      if(obj%endian == swap_endian() ) then
        lswap = .false.
      else
        lswap = .true.
      endif

      if(obj%nbits_hd==8*sizeof(hd(1))) then
        ncnv = obj%nhdtape*(obj%nbits_hd/(8*sizeof(obj%rbuf(1)) ))
   !bug if(lswap) call swap_bytes(data(1),ncnv)
        call cmem_cpy_c(hd,obj%rbuf(swrd),obj%nhdtape*obj%nbytesnword*2)
!!!        hd(1:obj%nhdtape) = transfer(obj%rbuf(swrd:swrd+ncnv-1),(/0.0d0/))
        if(lswap) call swap_bytes(hd(1),obj%nhdtape)
      else
        hvals=0
        if(obj%nbits_hd == 8*sizeof(obj%rbuf(1))) then
          if(lswap) call swap_bytes(obj%rbuf(swrd),obj%nhdtape)
          hvals(1:obj%nhdtape) = obj%rbuf(swrd:swrd+obj%nhdtape-1)
          hd(1:obj%nwih) = hvals(1:obj%nwih)
        else
          write(stdo,*) 'TTRIN_NEW_CPS_TCF: ERROR nbits_hd=',obj%nbits_hd
          write(stdo,*) &
          'TTRIN_NEW_CPS_TCF: nbits_hd must match a real or double!'
          obj%fatal_error=.true.
          return
        endif
      endif
!
!  swrd is the word where the trace data starts in the input buffer
!  ncnv is the number of trace values to convert
      isb =mod(obj%nhdtape,2)*4 + 1
      if(isb /= 1) then
        write(stdo,*) 'TTRIN_NEW_CPS_TCF: ISB != 1, AB'
        obj%fatal_error=.true.
        return
      endif
      swrd = swrd + obj%nhdtape*(obj%nbits_hd/(8*sizeof(obj%rbuf(1))))
      if(8*sizeof(tr(1))==obj%nbits) then
        ncnv=(8*nrd - obj%nhdtape*obj%nbits_hd)/obj%nbits
        ncnv=min(obj%ndpt,ncnv)
        ncnv=min(obj%ndpttape,ncnv)
        if(lswap) call swap_bytes(obj%rbuf(swrd), ncnv)
        tr(1:ncnv) = obj%rbuf(swrd:swrd+ncnv-1)
      else
        write(stdo,*) 'TTRIN_NEW_CPS_TCF: ERROR nbits /= sizeof(real)'
        write(stdo,*) 'TTRIN_NEW_CPS_TCF: ERROR nbits = ',obj%nbits
        obj%fatal_error=.true.
        return
      endif
! decrement the buffer tace counter
      obj%nib = obj%nib - 1
!
!--- Set mutes and LAV header words
      if(obj%clean /= 'NONE') then
       if(obj%clean=='ZERO') then
         stat = clean_zero(tr(1:obj%ndpt))
         if( stat > 0) obj%badcnt = obj%badcnt + 1
       endif
       if(obj%clean=='KILL') then
         killed = clean_kill(tr(1:obj%ndpt))
         if( killed) obj%badcnt = obj%badcnt + 1
       endif
      endif
      if(hd(64)<=1) hd(64)=obj%ndpt
      call mutehw(hd(:),tr(:),obj%ndpt,0.0,0)
      call lav_set_hdr (hd(:),tr(:),obj%ndpt)
      big = hd(25)
      obj%rlav = max(obj%rlav,big)
      if(hd(25) == 0.) obj%idead=obj%idead+1
      hd(1) = obj%nseqi

      return
      end subroutine ttrin_new_cps_tcf

      
!  
      subroutine ttrin_segpr(obj)
      type(ttrin_struct) :: obj
      if(obj%format=='SEGY') then
        write(ttrin_stdo(),'(1x,a,i10,a)') &
      ' TTRIN: Size of last  SEG-Y file on this reel  =',&
        obj%ngrpc,' traces.'
        write(ttrin_stdo(),'(1X,A,I10)')&
      ' TTRIN: Total number of files processed so far =',&
        obj%nseqfil
      endif
      return
      end subroutine ttrin_segpr
!  
! TTRIN_OPEN ... Opens a file reads the tape labels and positions
! to the first record of the file after the tape labels.
      integer function ttrin_open(obj)
      type(ttrin_struct),intent(inout) :: obj
      character(len=8)  ::     vol
      character(len=8)  ::     mode
      character(len=32) ::     mediavol
      character (len=160) :: cmd,wrkdir
      integer    :: imedia(8)
      integer    :: itapeserver(4)
      integer    :: itapehost(4)
      integer    :: ivol(8),imode(2)
      integer    :: imediavol(8)
      integer    :: status
      integer    :: ilname(4)
      integer    :: tpioclnt_open_server  !external function
      integer    :: elun,istat,label
!
      obj%lname = ' '
      if(obj%ivol<1 .or. obj%ivol>obj%nvols) then
        call pc_error('TTRIN_OPEN: ERROR, ivol out of range')
        ttrin_open=-1
        return
      endif
      vol= obj%vols(obj%ivol)(1:6)
      if(vol==' ') then
        call pc_error('TTRIN_OPEN: ERROR, blank volser is illegal')
        ttrin_open=-1
        return
      endif
      call string_cc2hh(vol,ivol)
      call string_cc2hh(obj%media ,imedia)
      call string_cc2hh(obj%tape_server ,itapeserver)

      mode='r'
      call string_cc2hh(mode,imode)
      mediavol = '3590_node:'//trim(vol)
      if(obj%media== '3592') then
        mediavol = '3592_node:'//trim(vol)
      endif
      if(obj%media== 'DLT') then
        mediavol = 'dlt_node:'//trim(vol)
      endif
      if(obj%media== 'NR3590') then
        mediavol = 'nr3590_node:'//trim(vol)
      endif
      if(obj%media== 'LTO') then
        mediavol = 'lto_node:'//trim(vol)
      endif
      if(obj%media== '3480') then
        mediavol = '3480_node:'//trim(vol)
      endif
      call string_cc2hh(mediavol,imediavol)
      call string_cc2hh(obj%tape_host,itapehost)
      
      write(stdo,*) 'TTRIN_OPEN: tape_host=',trim(obj%tape_host)
      write(stdo,*) 'TTRIN_OPEN: tape vol =',trim(vol)
      label = 0  !dont care about label on read
      ilname = 0
      ttrin_open = tpioclnt_open_server(imediavol,imode,imedia,itapeserver,&
                   itapehost, label,ilname)
      
      if(ttrin_open.lt.0)then
!             Failure to load problem - force unload the tape'
         call getlun(elun,istat)
         open(elun,file='goodger_email',status='replace',iostat=istat)
         write(elun,'(A)')trim(obj%jobname)
         write(elun,'(A)')trim(obj%frontend_path)
         write(elun,'(A)')trim(obj%frontend_user)
         write(elun,'(A)')trim(obj%tape_host)
         write(elun,'(A)')trim(vol)
         close(elun,status='keep')
         call getsys_current_dir(wrkdir)
         cmd='rsh ' // trim(obj%tape_host) // ' "tapelib -op fu -vol ' // &
           trim(vol) // '" >> ' // trim(wrkdir) // &
           'goodger_email'
         call putsys_cmd(trim(cmd))
         print*,' TTRIN_OPEN-->back from tapelib'
         print*,trim(cmd)
         cmd='mail -s "Failure to Load" goodgkp@conocophillips.com < ' // &
             'goodger_email'
        call putsys_cmd(cmd)
      endif
 
      status=0
      call tapeio_setverbose(status);
      call string_hh2cc(ilname,obj%lname)
      obj%volser = vol
      return
      end function ttrin_open
!  
      subroutine ttrin_dmnt(obj)
      type(ttrin_struct),intent(inout) :: obj
      integer i_err
      integer tpioclnt_close
      integer    :: ilname(4)
      integer    :: ivolser(4)
      if(obj%fd < 0) return
      if(obj%volser == ' ') return
      ilname = 0
      ivolser= 0
      call string_cc2hh(obj%lname,ilname)
      i_err = tpioclnt_close(obj%fd, ilname, ivolser)
      return
      end subroutine ttrin_dmnt
!  
      subroutine ttrin_curtape(val,obj)
      character(len=*),intent(inout):: vaL
      type(ttrin_struct),pointer :: obj
      val = obj%volume(obj%ireel)
      if(val(1:3)== '   ') then
       call ttrin_get_vol(obj%fd,val)
      endif
      return
      end subroutine ttrin_curtape
!
!   Get the internal label on the tape
      subroutine ttrin_get_vol(fd,label)
      implicit none
      integer,intent(in):: fd
      character(len=*) :: label
      integer ilabel(16)
      integer  :: tpioclnt_gvol,status
      label=' '
      ilabel=0
      status = tpioclnt_gvol(fd,ilabel)
      call string_hh2cc(ilabel,label)
      return
      end subroutine ttrin_get_vol
!  
      integer function ttrin_stdo()
      implicit none
      ttrin_stdo = pc_get_lun()
      return
      end function ttrin_stdo
!  
      subroutine ttrin_blkd(obj,header,trace)
!
!         Reads blocked data written by TTROT
!
!        ibuf  = buffer to read data in from tape
!      header  = array containing headers for 1 trace
!       trace  = array containing data for 1 trace
!
!
!
      implicit none
      type(ttrin_struct),pointer :: obj       ! arguments
      real trace(:),sphd(obj%nwih)
      double precision header(:)

      integer tpioclnt_read

      integer i
      integer len
      integer nrd,ntord,nread
      real               :: big
      logical lswap,killed
!
      integer    bufsiz
      parameter (bufsiz=15000)
!
!     if(obj%endian == swap_endian() ) then
!       lswap = .false.
!     else
!       lswap = .true.
!     endif
      lswap = .false.
      len = sizeof(obj%rbuf(1))
 100  continue
      if(obj%ndx == 1)then
        ntord = obj%bsize*sizeof(i)  ! should be 120000 bytes
        nrd = tpioclnt_read(obj%fd, obj%rbuf, ntord)
        if(nrd == 0)go to 1000  ! EOF
        if(nrd < 0)then
          obj%fatal_error=.true.
          call pc_error('TTRIN_BLKD: ERROR - tape read error')
          return
        endif
        if(nrd/sizeof(i) > size(obj%rbuf)) then
          obj%fatal_error=.true.
          call pc_error('TTRIN_BLKD: ERROR - buffer overflow')
          return
        endif
        obj%nleft= nrd/4    !number of 4 byte ibm words
        if(obj%nleft.lt.obj%ndpt)then
!           nleft could be odd
          if((obj%nleft+obj%trcndx-1) > obj%ndpt)obj%nleft=obj%nleft-1
        endif
      endif
      if(obj%trcndx == -99)then
        obj%trcndx=1
        go to 300
      else if(obj%trcndx > 1)then
        go to 300
      endif
!
!
!          Convert from IBM 32 bit to Cray 64 bit and split into
!            header and trace arrays
!
!
!        Convert the Header
      if(obj%nleft >= obj%nwih)then
        if(obj%hdrndx == 1)then
          nread=obj%nwih
        else
          nread=obj%nwih-obj%hdrndx+1
        endif
        if(mod(obj%ndx,2) == 0 .and. len==8) then
          write(ttrin_stdo(),*) 'TTRIN_BLK: - word alignment bug '
          obj%fatal_error= .true.
          return
        else
          i = 1 + 4*obj%ndx/sizeof(i) -1
        endif
        call wrdc_ibm_to_float(obj%rbuf(i:i+nread-1), sphd(obj%hdrndx:),&
         nread,lswap)
        header(obj%hdrndx:) = sphd(obj%hdrndx:)
!       nbyte=obj%ndx*4-3
!       call ussctc(rbuf,nbyte,header(obj%hdrndx),nread,1)
        obj%ndx=obj%ndx+nread
        obj%nleft=obj%nleft-nread
        obj%hdrndx=1
      else if(obj%nleft >= 1)then
        if(mod(obj%ndx,2) == 0 .and. len==8) then
          write(ttrin_stdo(),*) 'TTRIN_BLK: - word alignment bug '
          obj%fatal_error=.true.
          return
        else
          i = 1 + 4*obj%ndx/sizeof(i) -1
        endif
        call wrdc_ibm_to_float(obj%rbuf(i:i+obj%nleft-1), &
         sphd(obj%hdrndx:),obj%nleft,lswap)
        header(obj%hdrndx:) = sphd(obj%hdrndx:)
        obj%hdrndx=obj%nleft+1
        obj%ndx=1
        obj%nleft=obj%blksiz
        go to 100
      else if(obj%nleft==0)then
        obj%hdrndx=1
        obj%ndx=1
        obj%nleft=obj%blksiz
        go to 100
      else  ! something wrong
        write(ttrin_stdo(),*) 'TTRIN_BLK:- ERROR Converting header ,&
        & nleft = ',obj%nleft
        obj%fatal_error=.true.
        return
      endif
!
!
!        Convert the trace
 300  continue
      if(obj%nleft >= obj%ndpt)then
        if(obj%trcndx==1)then
          nread=obj%ndpt
        else
          nread=obj%ndpt-obj%trcndx+1
        endif
        if(mod(obj%ndx,2) == 0 .and. len==8) then
          write(ttrin_stdo(),*) 'TTRIN_BLK: - word alignment bug '
          obj%fatal_error=.true.
          return
        else
          i = 1 + 4*obj%ndx/sizeof(i) -1
        endif
        call wrdc_ibm_to_float(obj%rbuf(i:i+nread-1), &
         trace(obj%trcndx:obj%trcndx+nread-1),nread,lswap)
        obj%ndx=obj%ndx+nread
        obj%nleft=obj%nleft-nread
        obj%trcndx=1
      else if(obj%nleft >= 1)then
        if(mod(obj%ndx,2) == 0 .and. len==8) then
          write(ttrin_stdo(),*) 'TTRIN_BLK: - word alignment bug '
          obj%fatal_error=.true.
          return
        else
          i = 1 + 4*obj%ndx/sizeof(i) -1
        endif
!       call wrdc_ibm_to_float(obj%nleft, obj%rbuf(i), trace(obj%trcndx))
!       write(ttrin_stdo(),*) 'TTRIN_BLKD: A obj%nleft=',obj%nleft,' i=',i
        call wrdc_ibm_to_float(obj%rbuf(i:i+obj%nleft-1), trace(obj%trcndx:),&
        obj%nleft,lswap)
        obj%ndx=1
!          If full trace - return
        if((obj%trcndx+obj%nleft-1).eq.obj%ndpt)return
        obj%trcndx=obj%nleft+1
        obj%nleft=obj%blksiz
        go to 100
      else if(obj%nleft.eq.0)then
        obj%trcndx=-99
        obj%ndx=1
        obj%nleft=obj%blksiz
        go to 100
      else  ! something wrong
        write(ttrin_stdo(),*) 'TTRIN_BLK:- ERROR Converting trace ,&
        & nleft = ',obj%nleft
        obj%fatal_error=.true.
        return
      endif
!
!--- Set mutes and LAV header words
      if(obj%clean /= 'NONE') then
       if(obj%clean=='ZERO') then
         i = clean_zero(trace(1:obj%ndpt))
         if( i > 0) obj%badcnt = obj%badcnt + 1
       endif
       if(obj%clean=='KILL') then
         killed = clean_kill(trace(1:obj%ndpt))
         if( killed) obj%badcnt = obj%badcnt + 1
       endif
      endif
      call mutehw(header(:),trace(:),obj%ndpt,0.0,0)
      call lav_set_hdr (header(:),trace(:),obj%ndpt)
      big = header(25)
      obj%rlav = max(obj%rlav,big)
      if(header(25) == 0.) obj%idead=obj%idead+1
      header(1) = obj%nseqi
      return
!
!
 1000 continue  ! hit EOF
      obj%ieofflg=1
      obj%nleft = obj%blksiz
      obj%ndx   = 1
! don't reset buffer pointers in case this is a multi vol.
!     obj%hdrndx= 1
!     obj%trcndx= 1
      return
      end subroutine ttrin_blkd
!  
      SUBROUTINE TTRIN_PRNT (LFN,VOL,N1,IDEAD,N2,RLAV,BADCNT)
      IMPLICIT NONE
      INTEGER,INTENT(IN):: LFN,N1,N2,IDEAD,BADCNT
      REAL,INTENT(IN)   :: RLAV
      CHARACTER(len=48) :: VOL ,TVOL
      INTEGER   IC1
      TVOL = VOL
      IC1 = LEN_TRIM(VOL)
      IF (IC1 <= 0) then
       TVOL= ' '
      ENDIF
      WRITE(LFN,122) TRIM(TVOL),N1,IDEAD,N2,RLAV
      IF(BADCNT>0) THEN
        WRITE(LFN,*) 'TTRIN: BAD TRACE COUNT=',BADCNT
      ENDIF
      RETURN
 122  FORMAT('  ','TTRIN: VOLUME=',A,',',I8,' TRACES,',&
      I6,' DEAD',I8,' GROUPS, REEL LAV=',E15.8)
      END SUBROUTINE TTRIN_PRNT
!  
! Given FMT, set other parameters that are dependent
      INTEGER FUNCTION TTRIN_INIT_TYPE(FMT,HISTORY,&
        FILENUM,RECNUM,LNL,NHDWD,CDC, BYTES,NWIH,obj)
      IMPLICIT NONE
      CHARACTER(len=*) :: FMT,history
      INTEGER       FILENUM,RECNUM,LNL,NHDWD,cdc,NWIH
      INTEGER       ITYP,BYTES(*)
      type(ttrin_struct),intent(inout) :: obj
! ITYP=1,2,3,4,5 for CPS,DEBE,SEGY,ISP,DIGI,USER tape formats
      ITYP= -1
      cdc= 0
      IF(FMT(1:4)=='SEGY')      THEN
        FILENUM=1
        RECNUM =2
        ITYP   =3
        LNL    =1
        NHDWD  =60
      ELSE IF(FMT(1:3)=='CPS'.or.fmt=='CPS-NSC'.OR.FMT(1:3)=='BLK') THEN
        FILENUM=2
        RECNUM =0
        ITYP   =1
        LNL    =2
        IF(history.NE.'YES') THEN
          NHDWD  =64
        ELSE
          NHDWD  =32  !will be replaced by history value
        ENDIF
        if(obj%nwihcray.gt.0)nhdwd=obj%nwihcray
      ELSE IF(FMT(1:3)=='ISP')  THEN
        FILENUM=2
        RECNUM =0
        ITYP   =4
        LNL    =2
        NHDWD  =32
      ELSE IF(FMT(1:4)=='DEBE') THEN
        FILENUM=2
        RECNUM =0
        ITYP   =2
        LNL    =2
        NHDWD  =15
      ELSE IF(FMT(1:4)=='DIGI') THEN
        FILENUM=4
        RECNUM =0
        ITYP   =5
        LNL    =1
        NHDWD  =37
      ELSE IF(FMT(1:4)=='USER') THEN
!       NHDTAPE, FILENUM & RECNUM CHOSEN AT SET UP BY THE USER
        ITYP   =-1
        LNL    =2
        NHDWD  =BYTES(NWIH+1)/8
      ELSE IF(FMT(1:4)=='CONI') THEN
        FILENUM=2
        RECNUM =0
        ITYP   = -1
        LNL    =2
        NHDWD  =32
        cdc   =1
      ELSE IF(FMT(1:4)=='CON3') THEN
        FILENUM=2
        RECNUM =0
        ITYP   = -1
        LNL    =2
        NHDWD  =32
        cdc   =1
      ELSE IF(FMT(1:4)=='SERC') THEN
        FILENUM = 1   !TAPE FILE NO. TO START ON ??
        RECNUM  = 0   !RECORD NO. WITHIN FILE TO START ??
        ITYP    = -1
        LNL     = 1   ! 1= NON LABELED, 2=ANSI LABEL,3=STANDARD LABLE
        NHDWD   = 10  !NO. OF INPUT HDR VALUES (16 BIT)  ??
      ELSE IF(FMT(1:4)=='8048'.OR.FMT(1:4)=='8058')  THEN
        FILENUM = 1   !TAPE FILE NO. TO START ON ??
        RECNUM  = 0   !RECORD NO. WITHIN FILE TO START ??
        ITYP    = -1
        LNL     = 1   ! 1= NON LABELED, 2=ANSI LABEL,3=STANDARD LABLE
        NHDWD   = 10  !NO. OF INPUT HDR VALUES (16 BIT)  ??
      ELSE IF(FMT(1:4)=='SEGD') THEN
        FILENUM = 1   !TAPE FILE NO. TO START ON
        RECNUM  = 0   !RECORD NO. WITHIN FILE TO START
        ITYP    = -1
        LNL     = 3   ! 1= NON LABELED, 2=ANSI LABEL,3=STANDARD LABLE
        NHDWD   = 10  !NO. OF INPUT HDR VALUES (16 BIT)
      ELSE
        ITYP= -1
      ENDIF
      TTRIN_INIT_TYPE= ITYP
      RETURN
      END FUNCTION TTRIN_INIT_TYPE
      subroutine ttrin_open_vmod(obj,filename)

      type(ttrin_struct),intent(inout)    :: obj
      character(len=*),intent(in)         :: filename


      character(len=80)                   :: card,cards(30)
      character(len=FILENAME_LENGTH)      :: ctemp
      integer :: i,istat,k,k1,k2,nbits,ncards

      obj%vmodptr => trcio_open(filename,'w')
      if(.not.associated(obj%vmodptr))then
        call pc_error('TTRIN Abort - unable to open trcio file vmodptr')
      else
        write(stdo,*)'TTRIN_OPEN-->Writing file ',trim(filename)
        nbits=bit_size(obj%nwih)
        obj%vmodptr%nwih = obj%nwih
        obj%vmodptr%num_values = obj%ndpt
        obj%vmodptr%nbits = nbits
        obj%vmodptr%nbits_hd = 64
        obj%vmodptr%dt=obj%dt
        obj%vmodptr%tmin=obj%tstrt
        if(nbits.eq.32)then
          obj%vmodptr%wtype='IEEE'
        else
          obj%vmodptr%wtype='INT'
        endif
        istat=trcio_writeheader(obj%vmodptr)
        if(istat.ne.TRCIO_OK)then
          call pc_error('TTRIN-->Error in trcio_writeheader file vmodptr')
        endif
      endif

!           Now get the corresponding modgrid file
      k1=index(filename,'/',.true.) + 1
      k2=index(filename,'.') - 1
      ctemp=trim(obj%path_vmod) // '/' // filename(k1:k2) // '.modgrid'
      open(obj%lun_modgrid,file=ctemp,status='old',iostat=istat)
      if(istat.ne.0)then
        write(stdo,*)'TTRIN_OPEN_VMOD --> Unable to open modgrid file'
        write(stdo,*)trim(ctemp)
        obj%fatal_error=.true.
      endif

!         Output the modgrid file to the trcio file
      ncards=0
      DO i=1,30
        read(obj%lun_modgrid,'(A)',iostat=istat)card
        if(istat.lt.0)exit
        k=index(card,'modgrid>')
        if(k.ne.0)cycle
        ncards=ncards+1
        cards(ncards)=card
      ENDDO
      istat= trcio_write_section(obj%vmodptr,'modgrid',ncards, cards)


      end subroutine ttrin_open_vmod
      subroutine ttrin_vmod(obj,header,trace)
!
!         Reads a velocity model permsave tape and outputs the trc files
!           to path_vmod
!
      type(ttrin_struct),pointer :: obj       ! arguments
      real :: trace(:)
      double precision :: header(:)

      integer :: tpioclnt_read
      integer :: i
      integer :: istat,knteof
      integer :: nrd,ntord

      character(len=80) :: card,rootname
!
!
      knteof=0
 100  continue
      ntord = obj%bsize*sizeof(i)  ! should be 120000 bytes
      nrd = tpioclnt_read(obj%fd, obj%rbuf, ntord)
      if(nrd == 0)then  ! EOF
        knteof=knteof+1
        if(knteof.gt.1)then
          return
        endif
        go to 1000
      endif
      knteof=0
      if(nrd < 0)then
        obj%fatal_error=.true.
        call pc_error('TTRIN_VMOD: ERROR - tape read error')
        return
      endif
      if(nrd/sizeof(i) > size(obj%rbuf)) then
        obj%fatal_error=.true.
        call pc_error('TTRIN_VMOD: ERROR - buffer overflow')
        return
      endif
!          Move data in rbuf to header and trace arrays
      call ttrin_tcf(obj,nrd,header,trace)

      istat=trcio_write_trace(obj%vmodptr,header,trace)
      if(istat.ne.0)then
        obj%fatal_error=.true.
        call pc_error('TTRIN_VMOD-error writing vmod file')
        return
      endif
      go to 100

 1000 continue
      if(associated(obj%vmodptr))istat=trcio_close(obj%vmodptr)

!          Get the next file name
      read(obj%lun_fn,'(A)',iostat=istat)card
      if(istat.lt.0)then
        write(stdo,*)'TTRIN_VMOD--> All vmod file names have been read'
        return
      endif

!                Use path_vmod to save files
      rootname=path_get_file(card)
      card=' '
      card=trim(obj%path_vmod) // '/' // trim(rootname)
      call ttrin_open_vmod(obj,card)
      go to 100

      end subroutine ttrin_vmod

      integer function ttrin_get_hosts() result(cnt)
      integer   :: lservers(200)
      integer   ::n
      integer   ::i1,i2
      integer   :: tpioclnt_get_servers
      
      cnt = ttrin_server_cnt
      if(.not. ttrin_first_call) return
      ttrin_first_call = .false.
      n = 0
      ttrin_servers = ' '
      ttrin_server_cnt = tpioclnt_get_servers(lservers)
      cnt = ttrin_server_cnt
      do n = 1,min(cnt,size(ttrin_servers))
        i1 = (n-1)*24/4 + 1
        i2 = i1 + 24/4
        call string_hh2cc(lservers(i1:i2),ttrin_servers(n))
      enddo
      end function ttrin_get_hosts

      END MODULE TTRIN_MODULE
!  
