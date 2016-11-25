!<CPS_v1 type="PROCESS"/>

!!------------------------------- cfds.f90 ---------------------------------!!
!!------------------------------- cfds.f90 ---------------------------------!!
!!------------------------------- cfds.f90 ---------------------------------!!


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
!                        C P S   P R O C E S S
!
! Name       : CFDS      (Calculate Floating Datum Shifts) [Formerly AER]
! Category   : statics
! Written    : 1990-05-15   by: Tom Stoeckley
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Convert total statics to pre-NMO and post-NMO shifts.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                           GENERAL DESCRIPTION
!
! General
!
! CFDS operation consists of the following steps:
!
!    1.  CFDS accepts information on the total trace static from the FGD 
!    process, from input traces or from refraction statics files.
!
!    2.  It calculates the average static for each CMP.
!
!    3.  It optionally smooths the average static.
!
!    4.  Then it calculates the pre-NMO shift and the post-NMO shift and sets
!    these values in appropriate header words.
!
! The pre-NMO shift is also known as "shift-pre" and is used to convert to a 
! floating datum.  The post-NMO shift is also known as "shift-post" and is used
! to convert from a floating datum to the final datum.  (Pre-NMO shifts and 
! post-NMO shifts calculated from refraction statics files are known as 
! "shift-rpre" and "shift-rpost.")
!
! Shift-post is simply the average of the total statics for all traces in a 
! given CMP and is a constant for all the traces within a CMP.  Shift-pre, for
! a given trace, is the total static minus shift-post for that trace.  For any 
! trace, shift-pre plus shift-post equals the total static.
!
!
! Modes of Operation
!
! If MODE = FGD and refraction statics files are NOT listed:
!
!     CFDS receives information on trace total statics and CMP trace occupation
!     from the FGD process and from header word 40.  In this case, CFDS 
!     calculates shift-pre and shift-post values and sets shift-pre values in
!     header word 39 and shift-post values in header word 40 all in one job.
!
!     The FGD process must be present in the same job as CFDS regardless of 
!     whether headers are being applied.  If headers are not being applied, use
!     the SETUP option in FGD.
!
! If MODE = BUILD or MODE = SET and refraction statics files are NOT listed:
!
!     CFDS operates in a two-step mode with all the traces running though CFDS
!     in each step.  In the first job (MODE = BUILD) average statics for each 
!     CMP are calculated from information in the trace headers and written to a
!     CMP file.  Trace headers are not changed.  
!
!     In the second job, or another loop in the same job (MODE = SET), the CMP
!     file is read and shift-pre and shift-post values are calculated.  
!     Shift-pre values are set in header word 39 and shift-post values are set
!     in header word 40. This option is used if FGD is not available for the 
!     dataset.
!
! If MODE = FGD and refraction statics files ARE listed:
!
!     CFDS receives information on CMP trace occupation from the FGD process 
!     and uses as the total trace static the sum of the statics found in all 
!     the refraction statics files entered.  CFDS then separates the total 
!     refraction (or other) static into shift-rpre and shift-rpost values and
!     sets the appropriate headers (shift-rpre value in header word 56 and the
!     shift-rpost value in header word 57).  Input trace header word 40 values
!     are ignored.
!
!     The FGD process must be present in the same job as CFDS regardless of 
!     whether headers are being applied.  If headers are not being applied, use
!     the SETUP option in FGD.
!
! If MODE = BUILD or MODE = SET and refraction statics files ARE listed:
!
!     CFDS operates in a two-step mode with all the traces running though CFDS
!     in each step.  CFDS receives information on CMP trace occupation from the
!     input trace headers and uses as the total trace static the sum of the 
!     statics found in all the refraction statics files entered.  In the first
!     job (MODE = BUILD) average statics for each CMP are calculated and 
!     written to a CMP file.  Trace headers are not changed.  Input trace 
!     header word 40 values are ignored.
!
!     In the second job, or another loop in the same job (MODE = SET), the CMP
!     file is read and shift-rpre and shift-rpost values are calculated.  
!     Shift-rpre values are set in header word 56 and shift-rpost values are
!     set in header word 57. This option is used if FGD is not available for 
!     the dataset.
!
!     The refraction statics files must be entered in both JOBS.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                         ADVICE FOR USERS
!
!
! CFDS does NOT apply any static shifts.  Pre-NMO and post-NMO statics are 
! normally applied using the SHFT process.
!
!
! WARNING
!
! If refraction statics files are NOT listed then the total trace static must
! be set in header word 40 of input traces.  In all cases HDR_CMP_X and 
! HDR_CMP_Y must carry appropriate CMP information.  Normally it is easiest to
! do this by using CFG to apply proper header values.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                       TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process outputs the same traces as it receives (with altered headers if
! MODE=FGD or MODE=SET).
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                   GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of words in trace header       used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 39      shift-pre                  See General Description
! 40      shift-post                 See General Description
! 56      shift-rpre                 See General Description
! 57      shift-rpost                See General Description
!         HDR_CMP_X                  CMP x-grid coordinate
!         HDR_CMP_Y                  CMP y-grid coordinate
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!
!     Date       Author      Description
!     ----       ------      -----------
!031. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
! 30. 2002-05-22  Stoeckley  Add file selection boxes and file status fields;
!                              convert refraction static file pathnames from
!                              array to scalars; improve the GUI; simplify
!                              the code; fix a bug with the SET option when
!                              two or more refraction statics files are
!                              specified.
! 29. 2002-02-04  Stoeckley   Change name of two argument keywords to HIST
!                              and NHIST in call to statio_write_file.
! 28. 2001-06-04  CI Burch    Documentation change only.
! 27. 2001-02-13  Brad Kruse  Change name for wrapup flag to SKIP_WRAPUP for
!                             clarity, and slightly change how it is used.
! 26. 2000-11-29  Brad Kruse  Bug report #195.  Removed screen label, changed  
!                             default for hdr_cmp_y to zero (0), making default 
!                             operation 2D instead of 3D.
! 25. 2000-11-13  Brad Kruse  Added screen label 'For 2d, set hdr_cmp_y=0'
!                             for Bug Report #176.
! 24. 2000-10-05  Brad Kruse  Added HDR_PRE, HDR_POST helps sections.
! 23. 2000-08-29  Brad Kruse  Correct memory leak, due to not deallocating 
!                             obj%pathname_refr.  Reported by Donna Vunderink
!                             while debugging CFE memory leaks.  Corrected GUI
!                             to require PATHNAME_CMP if mode = BUILD or SET.
! 22. 2000-08-28  Brad Kruse  Correct error report #35.  Crash in FGD mode if 
!                             PATHNAME_CMP was not set.  Corrected error that 
!                             rejected Y_LAST and Y_LAST if <= 0.
! 21. 2000-08-10  Brad Kruse  Correct indexing error that caused the first
!                             refraction static file to be read each time, when 
!                             multiple refraction static files are specified.
! 20. 2000-08-09  Brad Kruse  Bug report 6.  Implemented pathname check for 
!                             PATHNAME_CMP in FGD mode, as well as BUILD 
!                             and SET modes.
! 19. 2000-08-08  Brad Kruse  Bug report 5.  Correct x_tot/x_last trap, y_tot/
!                             y_last trap, enable obj%lun to print info, limit 
!                             CMP summary in CFDS_SMOOTH to 120 values, and 
!                             detect when all refraction files have been 
!                             removed.
! 18. 2000-08-07  Brad Kruse  Bug report 4.  Update cfds_initialize to
!                             change default mode to FGD, and default filename
!                             to NONE.  Change Help and Update to allow 
!                             setting HDR_CMP_Y to 0.  Corrected cfds_update
!                             to check X/Y_LAST and X/Y_TOT updates in all 
!                             modes.
! 17. 2000-07-28  Brad Kruse  Update GUI per change from INL/CRL to Y/X grid
!                             parameters.
! 16. 2000-05-23  Brad Kruse  Correct FGD mode error: HDR 39,40 are set wrong
! 15. 2000-05-10  Brad Kruse  Fix infinite loop in update for FGD mode.
! 14. 2000-05-08  Kruse       Implement FGD mode.
! 13. 2000-04-05  Kruse       Update calls to newly revised statutil module. 
! 12. 2000-04-04  Kruse       Change for update to statutil module, add 
!                             read-only GUI parameters HDR_PRE and HDR_POST.
!                             Add the RCS ID.
! 11. 2000-03-24  Kruse       Remove TOP.LAY and BOT.LAY includes from the 
!                             GUI layout.
! 10. 2000-03-10  Kruse       Change to use STATIO and STATUTIL instead of
!                             STATICS_UTIL (by Stoeckley). Add combo boxes.
!                             Checkout GUI support.
! 9.  1999-11-14 Dorman      Convert to new CPS standards and Fortran 90
! 8.  1999-09-29 Troutt      Renamed from AER to CFDS per CI Burch.
! 7.  1999-08-31 Troutt      Begin full f90 conversion.
! 6.  1998-12-14 Vunderink   Begin using the f90 compiler.
! 5.  1994-02-11 Troutt      Add error checking for HPALLOC calls.
! 4.  1991-06-21 Howard      Fix unitialized variabel bug for UNICOS.
! 3.  1990-10-23 Peterson    Include error and abort arguments on calls to
!                             HPALLOC and HPDEALLC.
! 2.  1990-10-22 Stoeckley   Add new options MODE=BUILD and MODE=SET.
! 1.  1990-05-15 Stoeckley   Initial version.
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
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                    SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                   SPECIFIC CALLING CHARACTERISTICS
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
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
!                  ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                 ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                          PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS CFDS Process/NC=80/NR=26>
!                Calculate Floating Datum Shifts (Formerly AER)
!            Convert total statics to pre-NMO and post-NMO shifts.
!
!   MODE~~~~~= `CCCCC  [INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!   HDR_CMP_X= `III          HDR_CMP_Y= `III          [/L] X and Y bin header words.
!   X_INIT~~~= `FFFFFFFFFF   Y_INIT~~~= `FFFFFFFFFF   [/L] First bin coordinates.
!   X_INC~~~~= `FFFFFFFFFF   Y_INC~~~~= `FFFFFFFFFF   [/L] Bin widths.
!   X_LAST~~~= `FFFFFFFFFF   Y_LAST~~~= `FFFFFFFFFF   [/L] Last bin coordinates.
!   X_TOT~~~~= `IIIIIIII     Y_TOT~~~~= `IIIIIIII     [/L] Number of bins.
!   X_SMOOTH = `IIIIIIII     Y_SMOOTH = `IIIIIIII     [/L] Number of bins to smooth.
!
!   Header words containing pre-NMO and post-NMO static shifts:[HDR_PRE]`XXX [HDR_POST]`XXX
!
! [/L]File of average static for each CMP (for BUILD and SET Mode):
! Select PATHNAME_CMP[PATHNAME_CMP]~~=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                        [PATHNAME_CMP_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! [/L]Input Refraction Statics files:
! Select PATHNAME_REFR1[PATHNAME_REFR1]=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                        [PATHNAME_REFR1_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! Select PATHNAME_REFR2[PATHNAME_REFR2]=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                        [PATHNAME_REFR2_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! Select PATHNAME_REFR3[PATHNAME_REFR3]=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                        [PATHNAME_REFR3_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! Select PATHNAME_REFR4[PATHNAME_REFR4]=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                        [PATHNAME_REFR4_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!<PARMS PATHNAME_CMP  [/ML=128/XST]>
!<PARMS PATHNAME_REFR1[/ML=128/XST]>
!<PARMS PATHNAME_REFR2[/ML=128/XST]>
!<PARMS PATHNAME_REFR3[/ML=128/XST]>
!<PARMS PATHNAME_REFR4[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!<Help KEYWORD="MODE">
!<Tip> Whether to use FGD, BUILD or SET mode. </Tip>
! Default = FGD
! Allowed = FGD    (Get input information from FGD process in same job.)
! Allowed = BUILD  (Get input information from input trace stream.)
! Allowed = SET    (Set headers in input trace stream.)
!
! MODE = FGD is a one-step option requiring only one job.
!
! MODE = BUILD for the first job and MODE = SET for the second job (or another
! loop in the same job) is the two-step option you must use if FGD is not
! available for this dataset.
!</Help>
!
!
!
!<Help KEYWORD="HDR_CMP_X">
!<Tip> Header word for CMP X grid coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!
! Not active with MODE = SET.
!</Help>
!
!<Help KEYWORD="HDR_CMP_Y">
!<Tip> Header word for CMP Y grid coordinate. </Tip>
! Default = 0
! Allowed = 0 - NWIH
!
! Not active with MODE = SET.
!
! HDR_CMP_Y is set to zero (0) for 2D work.
!</Help>
!
!
!
!<Help KEYWORD="X_INIT">
!<Tip> Initial value for CMP X grid coordinate. </Tip>
! Default = 1.0
! Allowed = real
!
! Not active with MODE = SET.
!</Help>
!
!<Help KEYWORD="Y_INIT">
!<Tip> Initial value for CMP Y grid coordinate. </Tip>
! Default = 1.0
! Allowed = real
!
! Not active with MODE = SET.
!</Help>
!
!
!
!<Help KEYWORD="X_INC">
!<Tip> Increment for CMP X grid coordinate. </Tip>
! Default = 1.0
! Allowed = real>0.0
!
! Not active with MODE = SET.
!</Help>
!
!<Help KEYWORD="Y_INC">
!<Tip> Increment for CMP Y grid coordinate. </Tip>
! Default = 1.0
! Allowed = real>0.0
!
! Not active with MODE = SET.
!</Help>
!
!
!
!<Help KEYWORD="X_LAST">
!<Tip> Last value of CMP X grid coordinate. </Tip>
! Default = 1.0
! Allowed = real>=X_INIT
!
! Not active with MODE = SET.
!</Help>
!
!<Help KEYWORD="Y_LAST">
!<Tip> Last value of CMP Y grid coordinate. </Tip>
! Default = 1.0
! Allowed = real>=Y_INIT
!
! Not active with MODE = SET.
!</Help>
!
!
!
!<Help KEYWORD="X_TOT">
!<Tip> Total number of CMP X grid coordinate values. </Tip>
! Default = 1
! Allowed = int>0
!
! Not active with MODE = SET.
!</Help>
!
!<Help KEYWORD="Y_TOT">
!<Tip> Total number of CMP Y grid coordinate values. </Tip>
! Default = 1
! Allowed = int>0
!
! Not active with MODE = SET.
!</Help>
!
!
!
!<Help KEYWORD="X_SMOOTH">
!<Tip> Number of CMPs to smooth over in the X grid direction. </Tip>
! Default = 1
! Allowed = int>0 and odd
!
! Not active with MODE = BUILD.
!</Help>
!
!<Help KEYWORD="Y_SMOOTH">
!<Tip> Number of CMPs to smooth over in the Y grid direction. </Tip>
! Default = 1
! Allowed = int>0 and odd
!
! Not active with MODE = BUILD.
!</Help>
!
!
!<Help KEYWORD="INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Description of MODE parameter. </Tip>
!</Help>
!
!
!
!<Help KEYWORD="PATHNAME_CMP_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_CMP. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME_REFR1_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_REFR1. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME_REFR2_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_REFR2. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME_REFR3_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_REFR3. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME_REFR4_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_REFR4. </Tip>
!</Help>
!
!
!
!<Help KEYWORD="SELECT_PATHNAME_CMP">
!<Tip> Choose PATHNAME_CMP using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME_REFR1">
!<Tip> Choose PATHNAME_REFR1 using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME_REFR2">
!<Tip> Choose PATHNAME_REFR2 using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME_REFR3">
!<Tip> Choose PATHNAME_REFR3 using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME_REFR4">
!<Tip> Choose PATHNAME_REFR4 using a file selection dialog box. </Tip>
!</Help>
!
!
!
!<Help KEYWORD="PATHNAME_CMP">
!<Tip> Pathname for static file holding the average static for each CMP. </Tip>
! Default = NONE
! Allowed = char
!
! For MODE = FGD, the file may optionally be requested for information or 
!                 diagnostic purposes.  
! For MODE = BUILD, the file is created for use in a later job when MODE = SET.
! For MODE = SET, the file from a previous job is read in and used.
!</Help>
!
!
!
!<Help KEYWORD="PATHNAME_REFR1">
!<Tip> Pathname for statics file holding refraction statics values. </Tip>
! Default = NONE
! Allowed = char (array)
!
! CFDS operates on the sum of the statics found in all the files and assumes the
! statics are refraction statics.
!</Help>
!
!
!<Help KEYWORD="PATHNAME_REFR2">
!<Tip> Pathname for statics file holding refraction statics values. </Tip>
! Default = NONE
! Allowed = char (array)
!
! CFDS operates on the sum of the statics found in all the files and assumes the
! statics are refraction statics.
!</Help>
!
!
!<Help KEYWORD="PATHNAME_REFR3">
!<Tip> Pathname for statics file holding refraction statics values. </Tip>
! Default = NONE
! Allowed = char (array)
!
! CFDS operates on the sum of the statics found in all the files and assumes the
! statics are refraction statics.
!</Help>
!
!
!<Help KEYWORD="PATHNAME_REFR4">
!<Tip> Pathname for statics file holding refraction statics values. </Tip>
! Default = NONE
! Allowed = char (array)
!
! CFDS operates on the sum of the statics found in all the files and assumes the
! statics are refraction statics.
!</Help>
!
!
!
!<Help KEYWORD="HDR_PRE" TYPE= "DISPLAY_ONLY">
!<Tip> Number of the header word pre-shift will be written to.</Tip>
!
! The output pre-shift will be written to header word 56 if any refraction 
! files are entered.  Otherwise, the calculated pre-shift will be written to
! header word 39.
!
! This field reminds the user which header word is being written.  The value of
! this field cannot be set directly.
!</Help>
!
!
!<Help KEYWORD="HDR_POST" TYPE= "DISPLAY_ONLY">
!<Tip> Number of the header word post-shift will be written to.</Tip>
!
! The output post-shift will be written to header word 57 if any refraction 
! files are entered.  Otherwise, the calculated post-shift will be written to
! header word 40.
!
! This field reminds the user which header word is being written.  The value of
! this field cannot be set directly.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module cfds_module
  !
  ! - Module references
  !
  use getsys_module, only:              &
        getsys_username,                &
        getsys_hostname

  use named_constants_module

  use pathcheck_module, only:           &
        pathcheck,                      &
        PATHCHECK_EMPTY,                &
        PATHCHECK_INFO_INPUT,           &
        PATHCHECK_INFO_OUTPUT

  use pathchoose_module, only:          &
        pathchoose_struct,              &
        pathchoose_create,              &
        pathchoose_delete,              &
        pathchoose_update

  use pc_module, only:                  &
        pc_alloc_process_cards,         &
        pc_do_not_process_traces,       &
        pc_error,                       &
        pc_get,                         &
        pc_get_global,                  &
        pc_get_lun,                     &
        pc_put,                         &
        pc_put_gui_only,                &
        pc_put_control,                 &
        pc_put_options_field,           &
        pc_put_sensitive_field_flag

  use statutil_module, only:            &
        statutil_bld1,                  &
        statutil_bld2,                  &
        statutil_bld3,                  &
        statutil_get1,                  &
        statutil_get2,                  &
        statutil_rep_nilx,              &
        statutil_smooth

  use statio_module, only:              &
        statio_read_file,               &
        statio_write_file

  use string_module, only:              &
        string_to_upper

  use geomdata_module, only:            &
        GEOMDATA_FINISHED,              &
        GEOMDATA_ERROR

  use fgd_module, only:                 &
        fgd_initialize_headers,         &
        fgd_next_header

  use mth_module, only:                 &
        mth_fix_pattern,                &
        mth_constrain_odd

  use mem_module, only:                 &
        mem_alloc,                      &
        mem_free

  implicit none

  private
  public :: cfds_create  
  public :: cfds_initialize
  public :: cfds_update   
  public :: cfds_delete
!<execute_only>
  public :: cfds 
  public :: cfds_wrapup
!</execute_only>

  character(len=100),public,save :: cfds_ident =    &
       '$Id: cfds.f90,v 1.31 2006/09/18 13:32:40 Glover prod sps $'


!!------------------------- file structure --------------------------!!
!!------------------------- file structure --------------------------!!
!!------------------------- file structure --------------------------!!


  type,private :: file_struct

    private
    character(len=filename_length)  :: pathname
    integer                         :: hdr_x
    integer                         :: hdr_y
    integer                         :: hdr_x2
    integer                         :: hdr_y2
    integer                         :: x_tot
    integer                         :: y_tot
    real                            :: x_init
    real                            :: y_init
    real                            :: x_inc
    real                            :: y_inc
    real                            :: x_last
    real                            :: y_last
    real                   ,pointer :: svalue(:,:)
    type(pathchoose_struct),pointer :: pathchoose

  end type file_struct


!!------------------------- parameter structure --------------------------!!
!!------------------------- parameter structure --------------------------!!
!!------------------------- parameter structure --------------------------!!


  type,public :: cfds_struct

    private

    logical                     :: skip_wrapup  ! wrapup flag.
    character(len=5)            :: mode         ! process parameters.
    integer                     :: x_smooth     ! process parameters.
    integer                     :: y_smooth     ! process parameters.
    integer                     :: lun
    logical                     :: start        ! 1st main call flag
    integer                     :: ipre,ipost   ! hdr words for pre/post static
    integer                     :: ncards       ! for cmpfile
    character(len=80), pointer  :: cards(:)     ! for cmpfile
    integer,           pointer  :: scount(:,:)  ! cmp static counts
    logical                     :: use          ! true if any REFR files
    type (file_struct)          :: cmp
    type (file_struct)          :: refr1
    type (file_struct)          :: refr2
    type (file_struct)          :: refr3
    type (file_struct)          :: refr4

  end type cfds_struct


!!--------------------------------- data ---------------------------------!!
!!--------------------------------- data ---------------------------------!!
!!--------------------------------- data ---------------------------------!!


  integer,           parameter :: num_mode_vals = 3
  integer,           parameter :: smooth_nx_print_limit = 100
  character (len=5), parameter :: mode_vals (num_mode_vals)    &
                                         = (/ "FGD  ", "BUILD", "SET  " /)

contains


!!----------------------- file structure subroutines ---------------------!!
!!----------------------- file structure subroutines ---------------------!!
!!----------------------- file structure subroutines ---------------------!!


  subroutine cfds_file_initialize (file)
    implicit none
    type(file_struct),intent(out) :: file       ! arguments

    file%pathname = PATHCHECK_EMPTY
    file%hdr_x    = 7
    file%hdr_y    = 0
    file%hdr_x2   = 0
    file%hdr_y2   = 0
    file%x_tot    = 1
    file%y_tot    = 1
    file%x_init   = 1.0
    file%y_init   = 1.0
    file%x_inc    = 1.0
    file%y_inc    = 1.0
    file%x_last   = 1.0
    file%y_last   = 1.0
    return
  end subroutine cfds_file_initialize



  subroutine cfds_file_read (file, lunprint, ier)
    implicit none
    type(file_struct),intent(inout) :: file                 ! arguments
    integer          ,intent(in)    :: lunprint             ! arguments
    integer          ,intent(out)   :: ier                  ! arguments
    character(len=80)               :: msg                  ! local
    character(len=8)                :: stattype             ! local

    if (file%pathname == PATHCHECK_EMPTY) then
         ier = 0
         return
    end if

    call statio_read_file  (filename = file%pathname,   &
                            stattype = stattype,        &
                            nhx      = file%hdr_x,      &
                            nhy      = file%hdr_y,      &
                            nhx2     = file%hdr_x2,     &
                            nhy2     = file%hdr_y2,     &
                            x1       = file%x_init,     &
                            y1       = file%y_init,     &
                            xinc     = file%x_inc,      &
                            yinc     = file%y_inc,      &
                            nx       = file%x_tot,      &
                            ny       = file%y_tot,      &
                            pstatics = file%svalue,     &
                            err      = ier,             &
                            msg      = msg,             &
                            lunprint = lunprint)
    if (ier /= 0) then
      call pc_error ('CFDS error: cannot open input file', file%pathname)
      call pc_error (msg)
    end if
    return
  end subroutine cfds_file_read



  subroutine cfds_file_write (file, cards, ncards, lunprint, ier)
    implicit none
    type(file_struct),intent(in)    :: file                 ! arguments
    character(len=*) ,intent(in)    :: cards(:)             ! arguments
    integer          ,intent(in)    :: ncards               ! arguments
    integer          ,intent(in)    :: lunprint             ! arguments
    integer          ,intent(out)   :: ier                  ! arguments
    character(len=80)               :: msg                  ! local

    if (file%pathname == PATHCHECK_EMPTY) then
         ier = 0
         return
    end if

    call statio_write_file (filename = file%pathname,   &
                            stattype = 'CFDS',          &
                            nhx      = file%hdr_x,      &
                            nhy      = file%hdr_y,      &
                            nhx2     = 0,               &
                            nhy2     = 0,               &
                            x1       = file%x_init,     &
                            y1       = file%y_init,     &
                            xinc     = file%x_inc,      &
                            yinc     = file%y_inc,      &
                            nx       = file%x_tot,      &
                            ny       = file%y_tot,      &
                            statics  = file%svalue,     &
                            err      = ier,             &
                            msg      = msg,             &
                            hist     = cards,           &
                            nhist    = ncards,          &
                            progname = 'CFDS',          &
                            lunprint = lunprint)

    if (ier /= 0) then
      call pc_error ('CFDS error: cannot open output file', file%pathname)
      call pc_error (msg)
    end if
    return
  end subroutine cfds_file_write


!!-------------------------------- create --------------------------------!!
!!-------------------------------- create --------------------------------!!
!!-------------------------------- create --------------------------------!!

  subroutine cfds_create (obj)
    !
    ! - Arguments
    !
    type (cfds_struct), pointer :: obj       ! arguments
    !
    ! - Begin cfds_create
    !
    allocate (obj)
    !
    nullify (obj%cards)
    nullify (obj%scount)
    !
    nullify (obj%cmp%svalue)
    nullify (obj%refr1%svalue)
    nullify (obj%refr2%svalue)
    nullify (obj%refr3%svalue)
    nullify (obj%refr4%svalue)
    nullify (obj%cmp%pathchoose) ! jpa
    nullify (obj%refr1%pathchoose) ! jpa
    nullify (obj%refr2%pathchoose) ! jpa
    nullify (obj%refr3%pathchoose) ! jpa
    nullify (obj%refr4%pathchoose) ! jpa
    !
    call pathchoose_create (obj%cmp  %pathchoose, 'pathname_cmp'  ,'*')
    call pathchoose_create (obj%refr1%pathchoose, 'pathname_refr1','*')
    call pathchoose_create (obj%refr2%pathchoose, 'pathname_refr2','*')
    call pathchoose_create (obj%refr3%pathchoose, 'pathname_refr3','*')
    call pathchoose_create (obj%refr4%pathchoose, 'pathname_refr4','*')
    !
    call cfds_initialize (obj)
    !
  end subroutine cfds_create


!!-------------------------------- delete --------------------------------!!
!!-------------------------------- delete --------------------------------!!
!!-------------------------------- delete --------------------------------!!


  subroutine cfds_delete (obj)
    !
    ! - Arguments
    !
    type (cfds_struct), pointer :: obj       ! arguments
    !
    ! - Begin cfds_delete
    !
!<execute_only>
    call cfds_wrapup (obj)
!</execute_only>

    call mem_free (obj%cards)
    call mem_free (obj%scount)
    !
    call mem_free (obj%cmp  %svalue)
    call mem_free (obj%refr1%svalue)
    call mem_free (obj%refr2%svalue)
    call mem_free (obj%refr3%svalue)
    call mem_free (obj%refr4%svalue)
    !
    call pathchoose_delete (obj%cmp  %pathchoose)
    call pathchoose_delete (obj%refr1%pathchoose)
    call pathchoose_delete (obj%refr2%pathchoose)
    call pathchoose_delete (obj%refr3%pathchoose)
    call pathchoose_delete (obj%refr4%pathchoose)

    deallocate (obj)

  end subroutine cfds_delete


!!------------------------------ initialize ------------------------------!!
!!------------------------------ initialize ------------------------------!!
!!------------------------------ initialize ------------------------------!!


  subroutine cfds_initialize (obj)
    !
    ! - Arguments
    !
    type (cfds_struct), pointer :: obj       ! arguments
    !
    ! - Begin cfds_initialize
    !
    obj%mode           = 'FGD'
    obj%x_smooth       = 1
    obj%y_smooth       = 1
    obj%ncards         = 0
    obj%skip_wrapup    = .false.
    obj%start          = .true.
    obj%use            = .false.
    !
    call cfds_file_initialize (obj%cmp)
    call cfds_file_initialize (obj%refr1)
    call cfds_file_initialize (obj%refr2)
    call cfds_file_initialize (obj%refr3)
    call cfds_file_initialize (obj%refr4)
    !
    call cfds_update (obj)
    !
  end subroutine cfds_initialize


!!--------------------------- start of update ----------------------------!!
!!--------------------------- start of update ----------------------------!!
!!--------------------------- start of update ----------------------------!!


  subroutine cfds_update (obj)
    implicit none
    type (cfds_struct),intent(inout) :: obj
    integer                          :: ier, nscratch, nstore, nwih
    character(len=80)                :: info
    real                             :: x_last_keep, y_last_keep
    integer                          :: x_tot_keep,  y_tot_keep

    obj%skip_wrapup = .true.
    obj%start       = .true.


!!-------------------------- read parameters ---------------------------!!
!!-------------------------- read parameters ---------------------------!!
!!-------------------------- read parameters ---------------------------!!


    if (pathchoose_update (obj%cmp  %pathchoose, obj%cmp  %pathname)) return
    if (pathchoose_update (obj%refr1%pathchoose, obj%refr1%pathname)) return
    if (pathchoose_update (obj%refr2%pathchoose, obj%refr2%pathname)) return
    if (pathchoose_update (obj%refr3%pathchoose, obj%refr3%pathname)) return
    if (pathchoose_update (obj%refr4%pathchoose, obj%refr4%pathname)) return

    obj%lun = pc_get_lun ()

    call pc_get_global ('nwih',  nwih)

    x_last_keep = obj%cmp%x_last
    y_last_keep = obj%cmp%y_last
    x_tot_keep  = obj%cmp%x_tot
    y_tot_keep  = obj%cmp%y_tot

    call pc_get   ('MODE'          ,  obj%mode)
    call pc_get   ('X_SMOOTH'      ,  obj%x_smooth)
    call pc_get   ('Y_SMOOTH'      ,  obj%y_smooth)
    call pc_get   ('HDR_CMP_X'     ,  obj%cmp%hdr_x)
    call pc_get   ('HDR_CMP_Y'     ,  obj%cmp%hdr_y)
    call pc_get   ('X_INIT'        ,  obj%cmp%x_init)
    call pc_get   ('Y_INIT'        ,  obj%cmp%y_init)
    call pc_get   ('X_INC'         ,  obj%cmp%x_inc)
    call pc_get   ('Y_INC'         ,  obj%cmp%y_inc)
    call pc_get   ('X_LAST'        ,  obj%cmp%x_last)
    call pc_get   ('Y_LAST'        ,  obj%cmp%y_last)
    call pc_get   ('X_TOT'         ,  obj%cmp%x_tot)
    call pc_get   ('Y_TOT'         ,  obj%cmp%y_tot)
    call pc_get   ('PATHNAME_CMP'  ,  obj%cmp  %pathname)
    call pc_get   ('PATHNAME_REFR1',  obj%refr1%pathname)
    call pc_get   ('PATHNAME_REFR2',  obj%refr2%pathname)
    call pc_get   ('PATHNAME_REFR3',  obj%refr3%pathname)
    call pc_get   ('PATHNAME_REFR4',  obj%refr4%pathname)


!!------------------------- verify parameters --------------------------!!
!!------------------------- verify parameters --------------------------!!
!!------------------------- verify parameters --------------------------!!


call pathcheck ('PATHNAME_REFR1',obj%refr1%pathname, show=PATHCHECK_INFO_INPUT)
call pathcheck ('PATHNAME_REFR2',obj%refr2%pathname, show=PATHCHECK_INFO_INPUT)
call pathcheck ('PATHNAME_REFR3',obj%refr3%pathname, show=PATHCHECK_INFO_INPUT)
call pathcheck ('PATHNAME_REFR4',obj%refr4%pathname, show=PATHCHECK_INFO_INPUT)

    obj%use = (obj%refr1%pathname /= PATHCHECK_EMPTY) .or.  &
              (obj%refr2%pathname /= PATHCHECK_EMPTY) .or.  &
              (obj%refr3%pathname /= PATHCHECK_EMPTY) .or.  &
              (obj%refr4%pathname /= PATHCHECK_EMPTY)

    if (obj%use) then
         obj%ipre  = HDR_RPRE     ! 56
         obj%ipost = HDR_RPOST    ! 57
    else
         obj%ipre  = HDR_PRE      ! 39
         obj%ipost = HDR_POST     ! 40
    end if

    call string_to_upper (obj%mode)

    select case (obj%mode)
      case ('BUILD')
         call pathcheck ('PATHNAME_CMP', obj%cmp%pathname, required=.true., &
                                         show=PATHCHECK_INFO_OUTPUT)
         info = 'get input information from input trace stream'
      case ('SET')
         call pathcheck ('PATHNAME_CMP', obj%cmp%pathname, required=.true., &
                                         show=PATHCHECK_INFO_INPUT)
         info = 'set headers in input trace stream'
      case ('FGD')
         call pathcheck ('PATHNAME_CMP', obj%cmp%pathname, required=.false., &
                                         show=PATHCHECK_INFO_OUTPUT)
         info = 'get input information from FGD process in same job'
      case default
         call pc_error ('MODE must be FGD or BUILD or SET, not', obj%mode)
    end select

    if (obj%cmp%hdr_x < 1 .or. obj%cmp%hdr_x > nwih) then
      call pc_error ('CFDS: HDR_CMP_X out of range 1 to NWIH (',    &
                      nwih, ') - reset to 7')
      obj%cmp%hdr_x = 7
    end if

    if (obj%cmp%hdr_y < 0 .or. obj%cmp%hdr_y > nwih) then
      call pc_error ('CFDS: HDR_CMP_Y out of range 0 to NWIH (',    &
                      nwih, ') - reset to 0')
      obj%cmp%hdr_y = 0
    end if

    call mth_fix_pattern (obj%cmp%x_init, obj%cmp%x_inc, obj%cmp%x_last, &
                          obj%cmp%x_tot, x_last_keep, x_tot_keep)

    call mth_fix_pattern (obj%cmp%y_init, obj%cmp%y_inc, obj%cmp%y_last, &
                          obj%cmp%y_tot, y_last_keep, y_tot_keep)

    call mth_constrain_odd (obj%x_smooth, 1, obj%cmp%x_tot)
    call mth_constrain_odd (obj%y_smooth, 1, obj%cmp%y_tot)


!!-------------------------- write parameters --------------------------!!
!!-------------------------- write parameters --------------------------!!
!!-------------------------- write parameters --------------------------!!


    call pc_put_options_field (keyword  = 'MODE',        &
                               options  = mode_vals,     &
                               noptions = num_mode_vals)

    call pc_put   ('MODE'          ,  obj%mode)
    call pc_put   ('X_SMOOTH'      ,  obj%x_smooth)
    call pc_put   ('Y_SMOOTH'      ,  obj%y_smooth)
    call pc_put   ('HDR_CMP_X'     ,  obj%cmp%hdr_x)
    call pc_put   ('HDR_CMP_Y'     ,  obj%cmp%hdr_y)
    call pc_put   ('X_INIT'        ,  obj%cmp%x_init)
    call pc_put   ('Y_INIT'        ,  obj%cmp%y_init)
    call pc_put   ('X_INC'         ,  obj%cmp%x_inc)
    call pc_put   ('Y_INC'         ,  obj%cmp%y_inc)
    call pc_put   ('X_LAST'        ,  obj%cmp%x_last)
    call pc_put   ('Y_LAST'        ,  obj%cmp%y_last)
    call pc_put   ('X_TOT'         ,  obj%cmp%x_tot)
    call pc_put   ('Y_TOT'         ,  obj%cmp%y_tot)
    call pc_put   ('PATHNAME_CMP'  ,  obj%cmp  %pathname)
    call pc_put   ('PATHNAME_REFR1',  obj%refr1%pathname)
    call pc_put   ('PATHNAME_REFR2',  obj%refr2%pathname)
    call pc_put   ('PATHNAME_REFR3',  obj%refr3%pathname)
    call pc_put   ('PATHNAME_REFR4',  obj%refr4%pathname)

    nscratch     = 20 + nwih                                  ! hhh
    nstore       = 4 * 13                                  &  ! refr
                    + 4 * (obj%cmp%x_tot * obj%cmp%y_tot)  &
                    + obj%cmp%x_tot * obj%cmp%y_tot        &  ! obj%svalues
                    + obj%cmp%x_tot * obj%cmp%y_tot           ! obj%scounts

    call pc_put_control ('nscratch',     nscratch)
    call pc_put_control ('nstore',       nstore)  

    call pc_put_gui_only ('HDR_PRE' , obj%ipre)
    call pc_put_gui_only ('HDR_POST', obj%ipost)
    call pc_put_gui_only ('info'    , info)

    call pc_put_sensitive_field_flag ('HDR_CMP_Y', (obj%mode /= 'SET'))
    call pc_put_sensitive_field_flag ('HDR_CMP_X', (obj%mode /= 'SET'))
    call pc_put_sensitive_field_flag ('X_INIT',    (obj%mode /= 'SET'))
    call pc_put_sensitive_field_flag ('Y_INIT',    (obj%mode /= 'SET'))
    call pc_put_sensitive_field_flag ('X_INC',     (obj%mode /= 'SET'))
    call pc_put_sensitive_field_flag ('Y_INC',     (obj%mode /= 'SET'))
    call pc_put_sensitive_field_flag ('X_TOT',     (obj%mode /= 'SET'))
    call pc_put_sensitive_field_flag ('Y_TOT',     (obj%mode /= 'SET'))
    call pc_put_sensitive_field_flag ('X_LAST',    (obj%mode /= 'SET'))
    call pc_put_sensitive_field_flag ('Y_LAST',    (obj%mode /= 'SET'))
    call pc_put_sensitive_field_flag ('X_SMOOTH',  (obj%mode /= 'BUILD'))
    call pc_put_sensitive_field_flag ('Y_SMOOTH',  (obj%mode /= 'BUILD'))


!!----------------------- prepare for execution ------------------------!!
!!----------------------- prepare for execution ------------------------!!
!!----------------------- prepare for execution ------------------------!!

!<execute_only>

    if (pc_do_not_process_traces()) return
    obj%skip_wrapup = .false.

    call pc_alloc_process_cards (obj%cards, obj%ncards)

    call cfds_file_read (obj%refr1, obj%lun, ier)
    call cfds_file_read (obj%refr2, obj%lun, ier)
    call cfds_file_read (obj%refr3, obj%lun, ier)
    call cfds_file_read (obj%refr4, obj%lun, ier)

    if (obj%mode == 'BUILD' .or. obj%mode == 'FGD') then
         call mem_alloc (obj%cmp%svalue, obj%cmp%x_tot, obj%cmp%y_tot)
         call mem_alloc (obj    %scount, obj%cmp%x_tot, obj%cmp%y_tot)

         call statutil_bld1 (nx      = obj%cmp%x_tot,    &
                             ny      = obj%cmp%y_tot,    &
                             statics = obj%cmp%svalue,   &
                             kounts  = obj%scount)
    end if

    if (obj%mode == 'FGD') then
         call cfds_fgd_setup (obj,nwih)
    end if

    obj%start = .true.

!</execute_only>

!!--------------------------- finish update ----------------------------!!
!!--------------------------- finish update ----------------------------!!
!!--------------------------- finish update ----------------------------!!


  end subroutine cfds_update


!!---------------------------- cfds_fgd_setup ----------------------------!!
!!---------------------------- cfds_fgd_setup ----------------------------!!
!!---------------------------- cfds_fgd_setup ----------------------------!!


  subroutine cfds_fgd_setup (obj,nwih)
    !
    ! - Arguments
    !
    type (cfds_struct), intent (inout) :: obj
    integer           , intent (in)    :: nwih
    !
    ! - Local Variables
    !
    double precision  :: hhh(nwih)   ! header scratch
    integer           :: ier          
    integer           :: nnn          
    real              :: total        
    character(len=80) :: msg
    !
    ! - Begin cfds_fgd_setup
    !
    !
    ! - get the cdp static file from fgd trace headers.
    !
    nnn = 0
    !
    call fgd_initialize_headers ()
    !
  loop_get_fgd_statics:    &
    do
      !
      call fgd_next_header (hd  = hhh,    &
                            err = ier,    &
                            msg = msg)
      !
      call cfds_get (obj, hhh, total)
      !
      if (ier == GEOMDATA_FINISHED) then
        exit loop_get_fgd_statics
      else if  (ier == GEOMDATA_ERROR) then
        call pc_error ('CFDS: Error returned from fgd_next_header')
        call pc_error (msg)
        exit loop_get_fgd_statics
      end if
      !
      call statutil_bld2 (hd      = hhh,             &
                          statval = total,           &
                          statics = obj%cmp%svalue,  &
                          kounts  = obj%scount,      &
                          nhx     = obj%cmp%hdr_x,   &
                          nhy     = obj%cmp%hdr_y,   &
                          nhx2    = 0,               &
                          nhy2    = 0,               &
                          x1      = obj%cmp%x_init,  &
                          y1      = obj%cmp%y_init,  &
                          xinc    = obj%cmp%x_inc,   &
                          yinc    = obj%cmp%y_inc,   &
                          nx      = obj%cmp%x_tot,   &
                          ny      = obj%cmp%y_tot)
      !
    end do loop_get_fgd_statics
    !
    ! - finalize the cdp static file.
    !
    call statutil_bld3     (nx      = obj%cmp%x_tot,    &
                            ny      = obj%cmp%y_tot,    &
                            statics = obj%cmp%svalue,   &
                            kounts  = obj%scount)
    !
    call statutil_rep_nilx (nx      = obj%cmp%x_tot,    &
                            ny      = obj%cmp%y_tot,    &
                            statics = obj%cmp%svalue)
    !
    if (obj%cmp%pathname /= PATHCHECK_EMPTY) then
      !
      call cfds_save_statics (obj, ier)
      !
    end if
    !
    call cfds_smooth (x_tot    = obj%cmp%x_tot,   &
                      y_tot    = obj%cmp%y_tot,   &
                      svalue   = obj%cmp%svalue,  &
                      x_smooth = obj%x_smooth,    &
                      y_smooth = obj%y_smooth,    &
                      lunprint = obj%lun)
    !
  end subroutine cfds_fgd_setup


!!-------------------------- cfds_save_statics ---------------------------!!
!!-------------------------- cfds_save_statics ---------------------------!!
!!-------------------------- cfds_save_statics ---------------------------!!


  subroutine cfds_save_statics (obj, ier)
    !
    ! - Arguments
    !
    type (cfds_struct), intent (inout) :: obj
    integer,            intent (out)   :: ier
    !
    ! - Interfaces
    !
    intrinsic date_and_time
    !
    ! - Local Variables
    !
    character(len=80) :: cards(obj%ncards+3)   ! for cmpfile
    !
    ! - Begin cfds_save_statics
    !
    ier = 0
    !
    ! - This is a temporary file, build a placeholder comment card
    !
    cards = ''
    call date_and_time (date = cards (2), time = cards (3))
    cards (1) = 'Created By CPS/CFDS process, mode ' // trim (obj%mode)    &
                // '    Date: ' // trim (cards (2))                        &
                // ', '         // trim (cards (3))
    !
    ! - Build the second comment card
    !
    call getsys_username (cards (2))
    call getsys_hostname (cards (3))
    cards (2) = 'Username: '     // trim (cards (2))    &
                // '    Host:  ' // trim (cards (3))
    cards (3) = 'CFDS: Temporary inter-job CMP statics file'
    cards (4:) = obj%cards
    !
    call cfds_file_write (obj%cmp, cards, obj%ncards+3, obj%lun, ier)
    !
  end subroutine cfds_save_statics


!!-------------------------------- traps ---------------------------------!!
!!-------------------------------- traps ---------------------------------!!
!!-------------------------------- traps ---------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

  subroutine cfds (obj,ntr,hd,tr)
    !
    ! - Arguments
    !
    type (cfds_struct), intent (inout) :: obj                    ! arguments
    integer,            intent(inout)  :: ntr                    ! arguments
    double precision,   intent(inout)  :: hd(:,:)                ! arguments
    real,               intent(inout)  :: tr(:,:)                ! arguments
    !
    ! - Local variables
    !
    integer           :: i
    integer           :: ier
    real              :: post,total            ! local
    !
    ! - Begin 
    !
    ! - get or initialize the cmp file.
    !
    if (obj%start) then
      if (obj%mode .eq. 'SET') then

        call cfds_file_read (obj%cmp, obj%lun, ier)

        if(ier /= 0) then
          call cfds_wrapup (obj)
          ntr = FATAL_ERROR
          return
        end if

        call mem_alloc (obj%scount, obj%cmp%x_tot, obj%cmp%y_tot, ier)

        if(ier /= 0) then
          call cfds_wrapup (obj)
          ntr = FATAL_ERROR
          return
        end if

        call cfds_smooth (x_tot    = obj%cmp%x_tot,   &
                          y_tot    = obj%cmp%y_tot,   &
                          svalue   = obj%cmp%svalue,  &
                          x_smooth = obj%x_smooth,    &
                          y_smooth = obj%y_smooth,    &
                          lunprint = obj%lun)

      end if
      obj%start = .false.
    end if
    !
    ! - process a group of traces.
    !
    if (ntr > 0) then
      do i=1,ntr
        call cfds_get (obj, hd(:, i), total)

        if (obj%mode.eq.'BUILD') then
          call statutil_bld2 (hd      = hd(:,i),           &
                              statval = total,             &
                              statics = obj%cmp%svalue,    &
                              kounts  = obj%scount,        &
                              nhx     = obj%cmp%hdr_x,     &
                              nhy     = obj%cmp%hdr_y,     &
                              nhx2    = 0,                 &
                              nhy2    = 0,                 &
                              x1      = obj%cmp%x_init,    &
                              y1      = obj%cmp%y_init,    &
                              xinc    = obj%cmp%x_inc,     &
                              yinc    = obj%cmp%y_inc,     &
                              nx      = obj%cmp%x_tot,     &
                              ny      = obj%cmp%y_tot)

        else
          post = statutil_get1 (hd      = hd (:, i),         &
                                statics = obj%cmp%svalue,    &
                                nhx     = obj%cmp%hdr_x,     &
                                nhy     = obj%cmp%hdr_y,     &
                                nhx2    = 0,                 &
                                nhy2    = 0,                 &
                                x1      = obj%cmp%x_init,    &
                                y1      = obj%cmp%y_init,    &
                                xinc    = obj%cmp%x_inc,     &
                                yinc    = obj%cmp%y_inc,     &
                                nx      = obj%cmp%x_tot,     &
                                ny      = obj%cmp%y_tot)
          hd(obj%ipre,  i) = total - post
          hd(obj%ipost, i) = post
        end if
      end do
    !
    ! - wrapup processing.
    !
    else
      call cfds_wrapup (obj,ntr)
    end if
    !
  end subroutine cfds
!</execute_only>


!!----------------------------- cfds_smooth ------------------------------!!
!!----------------------------- cfds_smooth ------------------------------!!
!!----------------------------- cfds_smooth ------------------------------!!

  !
  ! - optionally smooth the cmp file.
  !
  subroutine cfds_smooth (x_tot,y_tot,svalue,x_smooth,y_smooth,lunprint)
    !
    ! - Arguments
    !
    integer, intent (in)    :: x_tot             ! dimensions for svalues
    integer, intent (in)    :: y_tot             ! dimensions for svalues
    real,    intent (inout) :: svalue (:, :)
    integer, intent (in)    :: x_smooth
    integer, intent (in)    :: y_smooth
    integer, intent (in)    :: lunprint
    !
    ! - Local variables
    !
    integer :: i
    integer :: limited_nx
    !
    ! - Begin cfds_smooth
    !
    limited_nx = min (a1 = x_tot,    &
                      a2 = smooth_nx_print_limit)
    !
    if ((x_smooth > 1) .or. (y_smooth > 1)) then
      !
      write(lunprint,'(1x,a)') &
          'CFDS:  POST STATIC FOR FIRST LINE -- BEFORE SMOOTHING:'
           write(lunprint,'(1x,30i4)')  (nint(svalue(i,1)),i=1,limited_nx)
      !
      call statutil_smooth (nx       = x_tot,        &
                            ny       = y_tot,        &
                            statics  = svalue,    &
                            nxsmooth = x_smooth,    &
                            nysmooth = y_smooth)
      !
      write(lunprint,'(1x,a)') &
          'CFDS:  POST STATIC FOR FIRST LINE -- AFTER SMOOTHING:'
      write(lunprint,'(1x,30i4)')  (nint(svalue(i,1)),i=1,limited_nx)
      !
    else
      !
      write(lunprint,'(1x,a)') &
          'CFDS:  POST STATIC FOR FIRST LINE -- NO SMOOTHING DONE:'
      write(lunprint,'(1x,30i4)')  (nint(svalue(i,1)),i=1,limited_nx)
      !
    end if
    !
  end subroutine cfds_smooth


!!------------------------------- cfds_get -------------------------------!!
!!------------------------------- cfds_get -------------------------------!!
!!------------------------------- cfds_get -------------------------------!!

  !
  ! - get the total static shift from refraction files or post header.
  !
  subroutine cfds_get (obj, hhh, total)
    !
    ! - Arguments
    !
    type (cfds_struct), intent (inout) :: obj
    double precision,   intent (inout) :: hhh(:)   ! args trace header
    real,               intent (out)   :: total    ! args total static
    !
    ! - Begin cfds_get
    !
    if (obj%use) then ! get the total static shift from refraction files.
      !
      total=0.0
      !
      if (obj%refr1%pathname /= PATHCHECK_EMPTY) then
          total = total + statutil_get2 (hhh, obj%refr1%svalue,              &
                                         obj%refr1%hdr_x , obj%refr1%hdr_y , &
                                         obj%refr1%hdr_x2, obj%refr1%hdr_y2, &
                                         obj%refr1%x_init, obj%refr1%y_init, &
                                         obj%refr1%x_inc , obj%refr1%y_inc , &
                                         obj%refr1%x_tot , obj%refr1%y_tot)
      end if
      if (obj%refr2%pathname /= PATHCHECK_EMPTY) then
          total = total + statutil_get2 (hhh, obj%refr2%svalue,              &
                                         obj%refr2%hdr_x , obj%refr2%hdr_y , &
                                         obj%refr2%hdr_x2, obj%refr2%hdr_y2, &
                                         obj%refr2%x_init, obj%refr2%y_init, &
                                         obj%refr2%x_inc , obj%refr2%y_inc , &
                                         obj%refr2%x_tot , obj%refr2%y_tot)
      end if
      if (obj%refr3%pathname /= PATHCHECK_EMPTY) then
          total = total + statutil_get2 (hhh, obj%refr3%svalue,              &
                                         obj%refr3%hdr_x , obj%refr3%hdr_y , &
                                         obj%refr3%hdr_x2, obj%refr3%hdr_y2, &
                                         obj%refr3%x_init, obj%refr3%y_init, &
                                         obj%refr3%x_inc , obj%refr3%y_inc , &
                                         obj%refr3%x_tot , obj%refr3%y_tot)
      end if
      if (obj%refr4%pathname /= PATHCHECK_EMPTY) then
          total = total + statutil_get2 (hhh, obj%refr4%svalue,              &
                                         obj%refr4%hdr_x , obj%refr4%hdr_y , &
                                         obj%refr4%hdr_x2, obj%refr4%hdr_y2, &
                                         obj%refr4%x_init, obj%refr4%y_init, &
                                         obj%refr4%x_inc , obj%refr4%y_inc , &
                                         obj%refr4%x_tot , obj%refr4%y_tot)
      end if
      !
    else          ! get the total static shift from the post header word.
      !
      total = hhh(40)
      !
    end if
    !
  end subroutine cfds_get


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

  subroutine cfds_wrapup (obj,ntr)
    !
    ! - Arguments
    !
    type (cfds_struct), intent(inout) :: obj       ! arguments
    integer,optional  , intent(inout) :: ntr       ! arguments
    !
    ! - Local variables
    !
    integer                            :: ier      ! local
    !
    ! - Begin cfds_wrapup
    !
    if (obj%skip_wrapup) return
    !
    obj%skip_wrapup = .true.
    !
    if (obj%mode.eq.'BUILD') then
      !
      call statutil_bld3 (nx      = obj%cmp%x_tot,    &
                          ny      = obj%cmp%y_tot,    &
                          statics = obj%cmp%svalue,   &
                          kounts  = obj%scount)
      !
      call statutil_rep_nilx (nx      = obj%cmp%x_tot,    &
                              ny      = obj%cmp%y_tot,    &
                              statics = obj%cmp%svalue)
      !
      call cfds_save_statics (obj, ier)
      !
      if (ier /= 0 .and. present(ntr)) ntr = FATAL_ERROR
      !
    end if
    !
  end subroutine cfds_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module cfds_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

