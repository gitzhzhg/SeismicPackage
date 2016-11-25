  !<CPS_v1 type="PROCESS"/>
!!------------------------------- abra.f90 ---------------------------------!!
!!------------------------------- abra.f90 ---------------------------------!!
!!------------------------------- abra.f90 ---------------------------------!!

! --> Edit the line below as appropriate:

        ! other files are:  abra_crou.c  abra_frou.f90  abra.h

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
! Name       : ABRA
! Category   : velocity_analysis
! Written    : 2003-05-06   by: Michael Ried
! Revised    : 2006-11-14   by: D. Glover
! Maturity   : production
! Purpose    : To Perform RMO Analysis
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Residual moveout (RMO) on reflected events after NMO or pre-stack migration 
! is present when the velocity is not correct.  ABRA (Automated Blackbox
! RMO Analysis) estimates this residual error based on semblance measurements.
! If the input data is NMO or pre-stack time migrated gathers, ABRA outputs the
! residual velocity error which can be added back to the input velocity.
! If the input data is pre-stack depth migrated gathers, ABRA outputs a Beta 
! coefficent that can be used in inversion routines such as TOMOCOP to update 
! the velocity.
! 
! Depth Domain 
! ------------
! For horizontally layered media, the residual moveout for an event can be 
! described by a hyperbolic formula that expresses migration depth (Z) as a 
! function of the half offset (H):
!
! Z**2  = Zo**2 + Beta*(H**2)
!
! Zo is the depth of the migrated event at zero offset.
!
! Beta is the coefficient automatically calculated by ABRA for each analysis
! window in a gather:
!
!               Migration_velocity**2
! Beta =     ---------------------------  -1    where the velocity type is
!                Correct velocity**2            average (not interval).
!
! If Beta equals zero, then the migration velocity is correct for the event.
! If Beta is less than zero, then the migration velocity is too slow, which
! causes the event on a gather to appear over-corrected.  If Beta is
! greater than zero, then the migration velocity is too fast, which causes the
! event to be under-corrected.
!
! The analysis window lengths for computing semblance can either be constant
! with a constant increment or variable with a constant increment.  The range
! of Beta values can be time or depth dependent and are defined by a search
! type (SRCH1TP=RMO, Delta V or Frac V).  ABRA automatically picks the Beta
! volume by computing the semblance over a range of Beta values and then
! picking the Beta value with the largest semblance.  ABRA also outputs
! constraint attributes such as the maximum amplitude that are associated with
! the picked Beta (See Section on TRACE OUTPUT).  These attributes are
! resampled to the input trace sampling prior to output.
! 
! Time Domain 
! -----------
! For time domain residual velocity analysis, the Beta coefficient described
! above can be related to the Velocity using:
!
! Beta = 4/Velocity**2
!
! Computing the residual velocity is accomplished using the following steps:
!
! 1. Pick the residual Beta (Br) using the maximum semblance as described above
! 2. Convert the input Velocity (Vm) to Beta (Bm):  Bm = 4/Vm**2
! 3. Calculate the true Beta (Bt):                  Bt = Bm + Br
! 4. Calculate the true Velocity (Vt):              Vt = 2/sqrt(Bt)
! 5. Calculate the residual Velocity (Vr):          Vr = Vt - Vm
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! DO NOT COMPRESS THE OUTPUT FROM THIS PROGRAM BECAUSE NULL VALUES WILL BE
! COMPRESSED TO ZERO. ZERO IS A VALID BETA VALUE.
!
! The Beta values or residual velocities computed in ABRA can be edited
! (culled) and smoothed using ABRA's constraint attributes with KA (Kombo
! Analysis).
!
! The Beta values, which describe trajectory, can be used as input into
! tomographic inversion (TOMOCOP).  For time domain analysis, the residual 
! velocity can be added back to the original velocity using COMBINE.
!
! Additionally, the Beta values can be input into TRMO (Two-term Residual
! Moveout), which is used to "flatten" the events on gathers before
! stacking or AVO analysis.  Applying a very mild smoothing filter with KA to
! the Beta field may improve moveout results (TRMO) and will reduce the
! stretching and compression of the seismic wavelet on the far offsets.
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Seismic Traces 
!    NMO corrected or time/depth migrated gathers with residual moveout
! Average Velocity Trace  (if not using an Average Velocity File)
!    Required for Time domain analysis 
!    Required for SRCH1TP=Delta V
!    TTYP_HNO=51, required Trace Identifier (HW48 is the default header word #)
! Interval Velocity Trace (if not using an Interval Velocity File)
!    Required for variable analysis windows
!    TTYP_HNO=86, required Trace Identifier (HW48 is the default header word #)
!    Used for variable window analysis
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! Beta Trace                                          TTYP_HNO=81**
! Maximum Semblance Trace                             TTYP_HNO=71**
! Gamma Trace (Depth domain only)                     TTYP_HNO=84**
! Delta Z Trace (Depth domain only)                   TTYP_HNO=87**
! RMS Amplitude Trace                                 TTYP_HNO=82**
! Maximum Absolute Amplitude Trace                    TTYP_HNO=83**
! Residual Average Velocity Trace (Time domain only)  TTYP_HNO=56**
!
! Input Seismic Traces (if LSTRC=YES)
! Input Velocity Traces (if LSTRC=YES)
!    Average  Velocity Trace                          TTYP_HNO=51**
!    Interval Velocity Trace                          TTYP_HNO=86**
! Computed Semblace functions (if LSEMF=YES)          TTYP_HNO=72**
!    Center time for each semblance function          HW62**
!
! ** (HW48 is the default header word #)
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
! Trace input documentation appears above as part of the advice for the
! user so that it will appear on the CFE input screens.
!</trace_in_doc>

!<trace_out_doc>
! Trace output documentation appears above as part of the advice for the
! user so that it will appear on the CFE input screens.
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! --> Insert globals that this process uses or changes:
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       --> specify action taken.
! GATHERED  whether traces are a legitimate gather  --> specify action taken.
! NWIH      number of words in trace header         --> specify action taken.
! NDPT      number of sample values in trace        --> specify action taken.
! TSTRT     starting time on trace                  --> specify action taken.
! DT        trace sample interval                   --> specify action taken.
! GRID      grid transformation structure           --> specify action taken.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! --> Insert header words used or changed by this process:
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!    3    HDR_CURRENT_GROUP          Input
!    4    HDR_CURRENT_CHANNEL        Made sure each trace is numbered
!    6    HDR_OFFSET                 Input into Calculations
!   48    TTYP_HNO                   Input/Output trace type **
!   55    DEBUG_HNO                  Trace containing a debug flag
!
! **(Trace Types: 0=Dead trace,1=Seismic trace,51=Input Average Velocity trace,
!    56=Delta V trace, 71=Semblance trace, 72=Semblance functions,
!    81=Beta trace, 82=RMS Amplitude,83=Max Absolute Amplitude,84=Gamma trace,
!    86=Input Interval Velocity trace,87=Delta Z trace
!
!    -HW48 is the default header word #)
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
! 15. 2006-11-14  D. Glover     Added NULLIFY statements for Intel compiler.
! 14. 2006-07-11  Michael Ried  The program outputs another attribute called
!                               deltaZ
! 13. 2006-01-17  B. Menger     Removed Unused Variables (line 3321 has unused
!                               var "mw" which I set to 0 because intent(out).
! 12. 2005-10-10  Michael Ried  New Changes to ABRA:
!                               -The program uses beginning window depth and
!                                and an ending window depth
! 12. 2005-04-26  Michael Ried  New Changes to ABRA: 
!                               -Depth or time header words now assumed to be
!                                in seconds, kft, or km.
!                               -The user must set the debug header word in
!                                order to display extra debug information
! 11. 2004-12-07  Michael Ried  New Changes to ABRA: 
!                               -Added the ability to read a modspec file
!                                and a CPS velocity file
! 10. 2004-06-30  Michael Ried  New Changes to ABRA: 
!                               -Input data can now by time data 
!                               -Trace type header word is inputted by user
!                               -Changed parameter units from ms->seconds
!                               -Changed the semblance smoothing algorithm
!                               -Made changes to variable length windows
!  9. 2004-03-26  Michael Ried  Fixed problem with it running parallel
!  8. 2004-02-18  Michael Ried  New Changes to ABRA: 
!                               -Change screens to make it easier for the user
!                               -Fixed problems when gathers are all zeroes
!  7. 2003-11-05  Michael Ried  New Changes to ABRA: 
!                               -Allowed it to run using parallel processors
!                               -TDATA window less than middle of next window
!  6. 2003-07-30  Michael Ried  New Changes to ABRA: 
!                               -Eliminate the Inner Mask table
!                               -Change search type from per cent to fraction
!                               -Changed method variable windows are determined
!  5. 2003-07-17  Michael Ried  New Changes to ABRA: 
!                               -The depth adjustment is added at the beginning
!                               -Process part of the data with a Constant depth
!  4. 2003-06-30  Michael Ried  New Changes to ABRA: 
!                               -Changed default for reference window length
!                               -Changed default for reference interval length
!                               -Make sure # of output traces is set correctly
!                               -Changed calculation for max # of windows
!                               -velocity traces are now read first
!                               -velocity traces outputted with seismic traces
!                               -default output trace headers to seismic headers
!                               -default beta output values to NILL
!  3. 2003-06-06  Michael Ried  New Changes to ABRA: 
!                               -Added Percent V & Delta V beta options
!                               -Allowed a minimum fold option
!                               -Added variable length windows
!                               -Added a Gamma output trace
!  2. 2003-06-03  Michael Ried  New Changes to ABRA: 
!                               -Set end window depth to last sample, 
!                               -Reference offset are divided by 2
!                               -Finds beta values not equal when interpolating
!                               -source detect distance now divided by 2
!  1. 2003-05-06  Michael Ried  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! --> Default values are shown below - edit as needed:
!
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  true      whether this process can be in a parallelized loop.
!
! --> Edit the following lines as needed:
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! --> Insert description of algorithms used.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! --> Insert any useful programming notes here.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!
!<NS Main_Menu1 Process/NC=80>
!                           ABRA - RMO Analysis
!`---------------------------------------------------------------------------
!  Minimum fold required for RMO analysis[/R]   FOLDMIN=`FFFFFFFFFFFFFFFFFFFF
!                           Type of input[/R]    DATATP=`CCCCCCCCCCCCCCCCCCCC
!       Header word number for trace type[/R]  TTYP_HNO=`FFFFFFFFFFFFFFFFFFFF
!`---------------------------------------------------------------------------
!                           Search Function Menu
!`---------------------------------------------------------------------------
!     Perform beta guide function search?[/R]     GSRCH=`CCCCCCCCCCCCCCCCCCCC
!               Select the Type of Search[/R]   SRCH1TP=`CCCCCCCCCCCCCCCCCCCC
!              Number of Search Functions[/R]   NSRCH1F=`II
!      Select the Average Velocity Option[/R]   AVELOPT=`CCCCCCCCCCCCCCCCCCCC
!                   Average Velocity file[/R]
!Select AVELNAM[AVELNAM]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!         [AVELNAM_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!`---------------------------------------------------------------------------
!                           Analysis Window Menu
!`---------------------------------------------------------------------------
!      Select the Type of Analysis Window[/R]     WINTP=`CCCCCCCCCCCCCCCCCCCC
!     Select the Interval Velocity Option[/R]    VELOPT=`CCCCCCCCCCCCCCCCCCCC
!                  Interval Velocity file[/R]
!  Select VELNAM[VELNAM]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!          [VELNAM_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                 Reference Window Length[/R]    WINLEN=`FFFFFFFFFFFFFFFFFFFF
!                        Window Increment[/R]      WINC=`FFFFFFFFFFFFFFFFFFFF
!  Ref Int Velocity for Ref Window Length[/R]    WINVEL=`FFFFFFFFFFFFFFFFFFFF
!                   Minimum Window Length[/R]     WLMIN=`FFFFFFFFFFFFFFFFFFFF
!                   Maximum Window Length[/R]     WLMAX=`FFFFFFFFFFFFFFFFFFFF
!       Select 1st Analysis Window Option[/R]    BWDEPO=`CCCCCCCCCCCCCCCCCCCC
!  1st Analysis Window Time/Depth/Hdr Loc[/R]     BWDEP=`FFFFFFFFFFFFFFFFFFFF
!       Depth Adjustment for First Window[/R]      BWDA=`FFFFFFFFFFFFFFFFFFFF
!      Select Last Analysis Window Option[/R]    EWDEPO=`CCCCCCCCCCCCCCCCCCCC
! Last Analysis Window Time/Depth/Hdr Loc[/R]     EWDEP=`FFFFFFFFFFFFFFFFFFFF
!        Depth Adjustment for Last Window[/R]      EWDA=`FFFFFFFFFFFFFFFFFFFF
!`---------------------------------------------------------------------------
!<NS Search1_Function_Table Process/NC=80>
!`---------------------------------------------------------------------------
!  Select the Reference Time/Depth Option[/R]   REFDEPO=`CCCCCCCCCCCCCCCCCCCC
!      Reference Depth or Header Location[/R]    REFDEP=`FFFFFFFFFFFFFFFFFFFF
!`---------------------------------------------------------------------------
!  Search1 Minimum Type  SRCH1_MIN_TP=`SSSSSSSSSSSSSSSSSSSS
!  Search1 Maximum Type  SRCH1_MAX_TP=`SSSSSSSSSSSSSSSSSSSS
!           SRCH1_DEPTH SRCH1_MIN   SRCH1_MAX   REF1_OFFSET
!           `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!           `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!           `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!           `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!           `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!<NS Main_Menu2 Process/NC=80>
!                             Semblance Menu
!`--------------------------------------------------------------------------
!            Length of Semblance Smoother[/R]       LSS=`IIII
!              Apply Semblance Peak Test?[/R]    SPEAKO=`KKK
!`--------------------------------------------------------------------------
!                             Amplitude Menu
!`--------------------------------------------------------------------------
!      Power Scalar for Normalizing Stack[/R]    SNORM=`FFFFFFFFFFFFFFFFFFFF
!   Attributes at time/depth of Max Stack[/R]
! Amplitudes instead of the window center[/R]    AMPOPT=`KKK
! Ratio of minimum distance between picks[/R]
!                     to Window Increment[/R]    ARATIO=`FFFFFFFFFFFFFFFFFFF
!`--------------------------------------------------------------------------
!                           Output Menu
!`--------------------------------------------------------------------------
!              Output the seismic traces?[/R]      LSTRC=`KKK
!         Output the semblance functions?[/R]      LSEMF=`KKK
! Header word number to output debug info[/R]  DEBUG_HNO=`FFFFFFFFFFFFFFFFFFFF
!`--------------------------------------------------------------------------
!<PARMS AVELNAM[/ML=128/XST]>
!<PARMS VELNAM[/ML=128/XST]>
!<PARMS AVELNAM_INFO[/ML=128/XST]>
!<PARMS VELNAM_INFO[/ML=128/XST]>
!
!</gui_def>
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="AMPOPT">
!<Tip> -->YES will output attributes at time/depth of max stack amplitude</Tip>
! Default = --> YES
! Allowed = --> YES
! Allowed = --> NO
! --> A YES will output attributes at the time/depth location of the maximum
!     stack amplitudes.
! --> A NO will output attributes at the center of the analysis windows.
!</Help>
!
!<Help KEYWORD="ARATIO">
!<Tip> -->Enter ratio of min distance between picks to window increment.</Tip>
! Default = --> 0.5
! Allowed = --> A Real value
! --> The ratio of the minimum distance between picks to the window increment.
!</Help>
!
!<Help KEYWORD="AVELNAM">
!<Tip> -->Use the file selection dialog to choose a Average Velocity file</Tip>
! Default = --> NONE
! Allowed = --> A file name
! --> The average velocity file
!</Help>
!
!<Help KEYWORD="AVELNAM_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> --> Status of the Average Velocity file.</Tip>
! DEFAULT = --> NONE
! Allowed = --> Status information
! --> Average velocity file status
!</Help>
!
!<Help KEYWORD="AVELOPT">
!<Tip> -->Select the Average velocity option</Tip>
! Default = --> Velocity trace input
! Allowed = --> Velocity file
! Allowed = --> Velocity trace input
! Allowed = --> Velocity trace file
! --> Velocity File -- Average CPS or Modspec velocity file used if
!     SRCH1TP=Delta V and/or DATATP=TIME
! --> Velocity trace input -- Average velocity trace data (TTYP_HNO=51) used if
!     SRCH1TP=Delta V and/or DATATP=TIME
! --> Velocity trace file -- Average velocity trace data (TTYP_HNO=51) used if
!     SRCH1TP=Delta V and/or DATATP=TIME
!</Help>
!
!<Help KEYWORD="BWDA">
!<Tip> -->Enter the time/depth adjustment for the first window</Tip>
! Default = --> 0.0
! Allowed = --> A Real value
! --> The time/depth adjustment for the first window.
! --> Units need to be in seconds, kft, or km.
!</Help>
!
!<Help KEYWORD="BWDEP">
!<Tip> -->Enter the time/depth or header word # of the 1st analysis window</Tip>
! Default = --> 0.0
! Allowed = --> A Real value
! --> The time/depth of the center of the first analysis window
! --> The header word number of the center of the first analysis window
! --> Units need to be in seconds, kft, or km.
!</Help>
!
!<Help KEYWORD="BWDEPO">
!<Tip> -->Select an option for the location of the first analysis window</Tip>
! Default = --> Constant
! Allowed = --> Constant
! Allowed = --> Header
! --> Constant Time/Depth -- Constant time/depth will be entered for the first
!     analysis window
! --> Header -- A Header word will contain for the first analysis window
!</Help>
!
!<Help KEYWORD="DATATP">
!<Tip> -->Enter the type of input (Depth or Time)</Tip>
! Default = --> Depth
! Allowed = --> Depth
! Allowed = --> Time
! --> Depth - Input data is depth migrated gathers
! --> Time - Input data is NMO corrected or time migrated gathers
!</Help>
!
!<Help KEYWORD="DEBUG_HNO">
!<Tip> -->Enter the header word number of the debug flag</Tip>
! Default = --> 0
! Allowed = --> A Header word number
! --> Header word number of the debug flag
!</Help>
!
!<Help KEYWORD="EWDA">
!<Tip> -->Enter the time/depth adjustment for the last window</Tip>
! Default = --> 0.0
! Allowed = --> A Real value
! --> The time/depth adjustment for the last window
! --> Units need to be in seconds, kft, or km.
!</Help>
!
!<Help KEYWORD="EWDEP">
!<Tip> -->The time/depth or header word # of the last analysis window</Tip>
! Default = --> 0.0
! Allowed = --> A Real value
! --> The time/depth of the last analysis window
! --> The header word number of the last analysis window
! --> Units need to be in seconds, kft, or km.
!</Help>
!
!<Help KEYWORD="EWDEPO">
!<Tip> -->Select an option for the location of the last analysis window</Tip>
! Default = --> Constant
! Allowed = --> Constant
! Allowed = --> Header
! --> Constant -- A Constant will be entered for the last analysis
!     window
! --> Header -- A Header word will contain for the last analysis window
!</Help>
!
!<Help KEYWORD="FOLDMIN">
!<Tip> -->Enter the Minimum Fold Required for RMO Analysis</Tip>
! Default = --> 20.0
! Allowed = --> A Real value
! --> Minimum Fold Required for RMO Analysis.  If the minimum fold is not met,
! Beta and the constraint attributes will be set to NULL.
!</Help>
!
!<Help KEYWORD="GSRCH">
!<Tip> -->Enter the search option in order to perform a second pass search</Tip>
! Default = --> NO
! Allowed = --> NO
! Allowed = --> EXTERNAL
! Allowed = --> INTERNAL
! --> EXTERNAL - An external second pass search will be performed. Use external
! if you want to QC the Beta field before running subsequent iterations of
! ABRA with a finer search spacing
! --> INTERNAL - An internal second pass search will be performed. Use internal
! if a QC of the first pass is not needed.
! --> NO - A second pass search will not be performed
!</Help>
!
!<Help KEYWORD="LSS">
!<Tip> -->Enter the length to smooth using semblances</Tip>
! Default = --> 3
! Allowed = --> An integer value
! --> The half length of the semblance smoothing operator.  This length can
! vary from 0 to half the number of search functions (NSRCH1F) used to generate
! the semblances.
!</Help>
!
!<Help KEYWORD="LSEMF">
!<Tip> -->Enter a YES to output seismic functions for each window</Tip>
! Default = --> NO
! Allowed = --> YES
! Allowed = --> NO
! --> A YES will output semblance functions for each windows
! --> A NO will cause the semblance functions not to be outputted
!
! Warning:  If selected, this outputs a lot of traces (Nwindows*Ngathers) with 
!           the first NSRCH1F samples defining the semblance function for each
!           window.  HW62 contains the center time for the semblance function.
!</Help>
!
!<Help KEYWORD="LSTRC">
!<Tip> -->Enter a YES to output the seismic traces</Tip>
! Default = --> NO
! Allowed = --> YES
! Allowed = --> NO
! --> A YES will output any input seismic traces
! --> A NO will cause the seismic traces not to be outputted
!</Help>
!
!<Help KEYWORD="WLMAX">
!<Tip> -->Enter the Maximum window length</Tip>
! Default = --> NONE
! Allowed = --> A Real value
! --> Enter the Maximum window length when the type of analysis windows are
!     variable
! --> Units need to be in seconds, kft, or km.
!</Help>
!
!<Help KEYWORD="WLMIN">
!<Tip> -->Enter the Minimum window length</Tip>
! Default = --> NONE
! Allowed = --> A Real value
! --> Enter the Minimum window length when the type of analysis windows are
!     variable
! --> Units need to be in seconds, kft, or km.
!</Help>
!
!<Help KEYWORD="NSRCH1F">
!<Tip> -->Enter the number of search functions</Tip>
! Default = --> 15
! Allowed = --> Integer
! --> The number of search functions when performing the first pass search
!</Help>
!
!<Help KEYWORD="REF1_OFFSET">
!<Tip> -->Enter a reference offset for each line in table</Tip>
! Default = --> NONE
! Allowed = --> A Real value
! --> Reference offset for each line in table.  The reference offset is only
!     needed when type of Search is RMO.
! --> Units need to be in seconds, F, or M.
!</Help>
!
!<Help KEYWORD="REFDEP">
!<Tip> -->Enter a Reference Time/Depth or Header word number</Tip>
! Default = --> 0.0
! Allowed = --> A Real value
! --> A reference time/depth or a header word number.  The reference time/depth 
!     is used to modify the time/depth values specified in the Search function
!     menu table.
! --> Units need to be in seconds, kft, or km.
!</Help>
!
!<Help KEYWORD="REFDEPO">
!<Tip> -->Choose an reference time/depth option</Tip>
! Default = --> Header
! Allowed = --> Header
! Allowed = --> Constant Time/Depth
! --> Header -- A Header word will contain the reference depth
! --> Constant Time/Depth -- A constant time/depth will be entered for the 
! reference time/depth
!</Help>
!
!<Help KEYWORD="SELECT_AVELNAM" TYPE= "DISPLAY_ONLY">
!<Tip> -->Select the Average Velocity file.</Tip>
!</Help>
!
!<Help KEYWORD="SELECT_VELNAM" TYPE= "DISPLAY_ONLY">
!<Tip> -->Select the interval  Velocity file.</Tip>
!</Help>
!
!<Help KEYWORD="SNORM">
!<Tip> -->Enter a power scalar for normalizing the stack</Tip>
! Default = --> 0.5
! Allowed = --> A Real value
! --> Power scalar for normalizing the stack
!</Help>
!
!<Help KEYWORD="SPEAKO">
!<Tip> -->Enter YES to apply semblance peak test</Tip>
! Default = --> YES
! Allowed = --> YES
! Allowed = --> NO
! --> YES will apply a semblance peak test to the data.  Beta will be set to a
!     NULL value it the maximum semblance occurs at the beginning or at the end
!     of the Beta search range.
!</Help>
!
!<Help KEYWORD="SRCH1TP">
!<Tip> -->Enter the type of search when performing a first pass search</Tip>
! Default = --> RMO
! Allowed = --> RMO
! Allowed = --> Delta V
! Allowed = --> Frac V
! --> RMO - Beta spacing increases with depth (specify delta time/depth at
!     REF1_OFFSET)
! --> DELTA V - Beta spacing decreases with depth (specify delta velocity at
!     REF1_OFFSET)
! --> Frac V - Beta spacing is constant at all depths (specify % velocity at
!     REF1_OFFSET, ie. 90 or 110)
!</Help>
!
!<Help KEYWORD="SRCH1_DEPTH">
!<Tip> -->Enter a search depth</Tip>
! Default = --> NONE
! Allowed = --> A Real value
! --> A search time/depth when performing a first pass search
! --> Units need to be in seconds, kft, or km.
!</Help>
!
!<Help KEYWORD="SRCH1_MAX">
!<Tip> -->Entering a maximum value when searching</Tip>
! Default = --> NONE
! Allowed = --> A Real value
! --> Maximum value when performing a first pass search
! --> Units need to be in seconds, kft, or km.
!</Help>
!
!<Help KEYWORD="SRCH1_MAX_TP">
!<Tip> -->Displays type of search function (Maximum value)</Tip>
! Default = --> RMO MAX
! Allowed = --> RMO MAX
! Allowed = --> Delta V MAX
! Allowed = --> Frac V MAX
! --> Type of search type (Maximum value)
!</Help>
!
!<Help KEYWORD="SRCH1_MIN">
!<Tip> -->Entering a minimum value when searching</Tip>
! Default = --> NONE
! Allowed = --> A Real value
! --> Minimum value when performing a first pass search
! --> Units need to be in seconds, kft, or km.
!</Help>
!
!<Help KEYWORD="SRCH1_MIN_TP">
!<Tip> -->Displays type of search function (Minimum value)</Tip>
! Default = --> RMO MIN
! Allowed = --> RMO MIN
! Allowed = --> Delta V MIN
! Allowed = --> Frac V MIN
! --> Type of search type (Minimum value)
!</Help>
!
!<Help KEYWORD="TTYP_HNO">
!<Tip> -->Enter the header word number for the trace type</Tip>
! Default = --> 48
! Allowed = --> A Header word number
! --> Header word number for the trace type
!</Help>
!
!<Help KEYWORD="VELNAM">
!<Tip> -->Use a file selection dialog to choose an Interval Velocity file</Tip>
! Default = --> NONE
! Allowed = --> A file name
! --> The interval velocity file
!</Help>
!
!<Help KEYWORD="VELNAM_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> -->Status of the Interval Velocity file.</Tip>
! Default = --> NONE
! Allowed = --> Status information
! --> Interval Velocity file status information
!</Help>
!
!<Help KEYWORD="VELOPT">
!<Tip> -->Select the Interval velocity option</Tip>
! Default = --> Velocity trace input
! Allowed = --> Velocity file
! Allowed = --> Velocity trace input
! Allowed = --> Velocity trace file
! --> Velocity File -- Interval CPS or Modspec velocity file used for variable
!     window analysis
! --> Velocity trace input -- Interval velocity trace data (TTYP_HNO=86) used
!     for variable window analysis
! --> Velocity trace file -- Interval velocity trace file (TTYP_HNO=86) used
!     for variable window analysis
!</Help>
!
!<Help KEYWORD="WINC">
!<Tip> -->Enter the the Window Increment</Tip>
! Default = --> 0.05
! Allowed = --> A real value
! --> The Window Increment specified in time/depth units.
! --> Units need to be in seconds, kft, or km.
!</Help>
!
!<Help KEYWORD="WINLEN">
!<Tip> -->Enter a reference window length</Tip>
! Default = --> 0.05
! Allowed = --> A Real value
! --> Reference window length in time/depth units.
! --> Units need to be in seconds, kft, or km.
!</Help>
!
!<Help KEYWORD="WINTP">
!<Tip> -->Select the type of Analysis Window</Tip>
! Default = --> Variable
! Allowed = --> Variable
! Allowed = --> Constant
! --> Constant - The length of all Analysis windows is the reference window
!     length
! --> Variable - The length of the Analysis windows will be calculated
!     based on the interval velocity, reference window length, and the
!     reference interval velocity:  LENGTH=WINLEN*Vint/WINVEL
!</Help>
!
!<Help KEYWORD="WINVEL">
!<Tip> -->Enter the Reference Int Velocity for the Reference Window Length</Tip>
! Default = --> 2000
! Allowed = --> A Real value
! --> The Reference Interval Velocity for the Reference Window Length
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module abra_module
      use ap1_module
      use grid_module            ! if you need the grid transformation.
      use mem_module
      use memman_module
      use named_constants_module
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.
      use pc_module
      use rantfile_module        ! velocity trace file
      use velterp_module         ! CPS or Modspec velocity file

! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: abra_create
      public :: abra_initialize
      public :: abra_update
      public :: abra_delete
      public :: abra            ! main trace processing routine.
      public :: abra_wrapup

      character(len=100),public,save :: ABRA_IDENT = &
'$Id: abra.f90,v 1.15 2006/11/14 14:32:49 Glover prod sps $'

      character(len=FILENAME_LENGTH)  :: avelnam
      character(len=FILENAME_LENGTH)  :: velnam

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: abra_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

! --> Below are commonly used globals - edit or remove as appropriate:

        integer                        :: ipn      ! process number.
        integer                        :: numtr    ! max number of input traces.
        logical                        :: gathered ! whether properly gathered.
        integer                        :: nwih     ! number of header words.
        integer                        :: ndpt     ! number of trace samples.
        real                           :: tstrt  !time of 1st trace sample (sec)
        real                           :: dt       ! trace sample interval (sec)
        type(grid_struct)              :: grid     ! grid transform.

        logical                        :: ampopt
        real                           :: aratio
        character(len=FILENAME_LENGTH) :: avelnam
        character(len=21)              :: avelopt
        real                           :: bwda
        real                           :: bwdep
        character(len=21)              :: bwdepo
        character(len=21)              :: datatp
        real                           :: debug_hno
        real                           :: ewda
        real                           :: ewdep
        character(len=21)              :: ewdepo
        real                           :: foldmin
        logical                        :: lsemf
        integer                        :: lss
        logical                        :: lstrc
        real                           :: wlmax
        real                           :: wlmin
        integer                        :: nsrch1f
        real                           :: refdep
        character(len=21)              :: refdepo
        real                           :: smb_min
        real                           :: snorm
        logical                        :: speako
        character(len=21)              :: srch1tp
        character(len=21)              :: srch1_max_tp
        character(len=21)              :: srch1_min_tp
        real                           :: ttyp_hno
        character(len=21)              :: gsrch
        character(len=FILENAME_LENGTH) :: velnam
        character(len=21)              :: velopt
        real                           :: winc
        real                           :: winlen
        character(len=21)              :: wintp
        real                           :: winvel

        integer                        :: n_srch1_depth          !#elements

        real,pointer                   :: ref1_offset(:)
        real,pointer                   :: srch1_depth(:)
        real,pointer                   :: srch1_max(:)
        real,pointer                   :: srch1_min(:)

        integer                        :: iavelo
        integer                        :: ibwdepo
        integer                        :: ict   
        integer                        :: idatatp   
        integer                        :: iet   
        integer                        :: iewdepo
        integer                        :: iewdo
        integer                        :: iivelo
        integer                        :: inc
        integer                        :: inf
        integer                        :: irefdepo
        integer                        :: is1tp 
        integer                        :: igsrch 
        integer                        :: ist   
        integer                        :: iunitst   
        integer                        :: iwintp
        integer                        :: lop
        integer                        :: lwin
        integer                        :: nop
        integer                        :: nwin
        character(len=21)              :: survey_units
        real                           :: velbias
        real                           :: velscale
        integer                        :: velsmode
        integer                        :: x_hno
        integer                        :: y_hno

        integer                        :: ie_s1
        integer                        :: ie_0n

        integer                        :: ix_tc
        integer                        :: ix_ms
        integer                        :: ix_beta
        integer                        :: ix_dz
        integer                        :: ix_fold
        integer                        :: ix_sx
        integer                        :: ix_rms
        integer                        :: ix_ma
        integer                        :: ix_s0
        integer                        :: ix_nn
        integer                        :: ix_sm
        integer                        :: ix_0n
        integer                        :: ix_op
        integer                        :: ix_s1
        integer                        :: ix_tmt
        integer                        :: ix_smt
        integer                        :: ix_ss
        integer                        :: ix_btr
        integer                        :: ix_dztr
        integer                        :: ix_gtr
        integer                        :: ix_rtr
        integer                        :: ix_mat
        integer                        :: ix_temp
        integer                        :: ix_wb
        integer                        :: ix_wc
        integer                        :: ix_we

! --> Insert any other needed variables or pointers here.
        type(pathchoose_struct),pointer :: pathchoose1
        type(pathchoose_struct),pointer :: pathchoose2
        type(rantfile_struct),pointer   :: id_avtfile
        type(velterp_struct),pointer    :: id_avfile
        type(rantfile_struct),pointer   :: id_ivtfile
        type(velterp_struct),pointer    :: id_ivfile

        real              ,pointer :: wa1(:)     ! work array #1
        real              ,pointer :: wa2(:)     ! work array #2
        integer           ,pointer :: iwa1(:)    ! Integer work array
        real              ,pointer :: bwa1(:,:)  ! Beta work array
        real              ,pointer :: dzwa1(:,:) ! Delta Z work array

      end type abra_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(abra_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine abra_create (obj)
      type(abra_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in abra_create")

      nullify (obj%pathchoose1)
      nullify (obj%pathchoose2)
      nullify (obj%id_avtfile) ! jpa
      nullify (obj%id_avfile) ! jpa
      nullify (obj%id_ivtfile) ! jpa
      nullify (obj%id_ivfile) ! jpa
      nullify (obj%ref1_offset) ! jpa
      nullify (obj%srch1_depth) ! jpa
      nullify (obj%srch1_max) ! jpa
      nullify (obj%srch1_min) ! jpa
      nullify (obj%wa1) ! jpa
      nullify (obj%wa2) ! jpa
      nullify (obj%iwa1) ! jpa
      nullify (obj%bwa1) ! jpa
      nullify (obj%dzwa1) ! jpa

      call pathchoose_create (obj%pathchoose1, 'avelnam' , 'vel')
      call pathchoose_create (obj%pathchoose2, 'velnam'  , 'vel')

      call memman_nullify (obj%ref1_offset , "abra_ref1_offset")
      call memman_nullify (obj%srch1_depth , "abra_srch1_depth")
      call memman_nullify (obj%srch1_max   , "abra_srch1_max")
      call memman_nullify (obj%srch1_min   , "abra_srch1_min")

! --> Nullify any additional pointers in the OBJ data structure here.
      call memman_nullify(obj%wa1)   ! must be done for all pointers.
      call memman_nullify(obj%wa2)
      call memman_nullify(obj%iwa1)
      call memman_nullify(obj%bwa1)
      call memman_nullify(obj%dzwa1)

      call abra_initialize (obj)
      end subroutine abra_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine abra_delete (obj)
      type(abra_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking
!
      integer :: iavelo, idatatp, iivelo, is1tp, iwintp
!
      call abra_wrapup (obj)
!
!     Get needed parameters
!
      iavelo=obj%iavelo
      iivelo=obj%iivelo
      is1tp=obj%is1tp
      idatatp=obj%idatatp
      iwintp=obj%iwintp
!
      call memman_free (obj%ref1_offset)
      call memman_free (obj%srch1_depth)
      call memman_free (obj%srch1_max)
      call memman_free (obj%srch1_min)

! --> Deallocate any additional pointers in the OBJ data structure here.
      if (associated(obj%wa1)) deallocate(obj%wa1)
      if (associated(obj%wa2)) deallocate(obj%wa2)
      if (associated(obj%iwa1)) deallocate(obj%iwa1)
      if (associated(obj%bwa1)) deallocate(obj%bwa1)
      if (associated(obj%dzwa1)) deallocate(obj%dzwa1)
      if (associated(obj%pathchoose1)) call pathchoose_delete(obj%pathchoose1)
      if (associated(obj%pathchoose2)) call pathchoose_delete(obj%pathchoose2)

! --> close the average velocity file
!     (if search1 type = Delta V or input data type = time)
      if (is1tp.eq.2 .or. idatatp.eq.2) then
        if (iavelo.eq.1) then
          call velterp_delete(obj%id_avfile)
        else if (iavelo.eq.3) then
          call rantfile_delete(obj%id_avtfile)
        end if
      end if

! --> close the interval velocity file
!     (if window = variable)
!
      if (iwintp.eq.2) then
        if (obj%iivelo.eq.1) then
          call velterp_delete(obj%id_ivfile)
        else if (obj%iivelo.eq.3) then
          call rantfile_delete(obj%id_ivtfile)
        end if
      end if

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in abra_delete")
      end subroutine abra_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine abra_initialize (obj)
      type(abra_struct),intent(inout) :: obj       ! arguments

! --> Change the default values below as needed
      obj%ampopt       = .TRUE.
      obj%aratio       = 0.5
      obj%avelnam      = 'NONE'
      obj%avelopt      = 'Velocity trace input'
      obj%bwda         = 0.0
      obj%bwdep        = 0.0
      obj%bwdepo       = 'Constant'
      obj%datatp       = 'Depth'
      obj%debug_hno    = 0.0
      obj%ewda         = 0.0
      obj%ewdep        = 0.0
      obj%ewdepo       = 'Constant'
      obj%foldmin      = 20.0
      obj%lsemf        = .FALSE.
      obj%lstrc        = .FALSE.
      obj%ttyp_hno     = 48.0
      obj%wlmax        = 0.0
      obj%wlmin        = 0.0
      obj%nsrch1f      = 15
      obj%refdep       = 0.0
      obj%refdepo      = 'Header'
      obj%snorm        = 0.5
      obj%speako       = .TRUE.
      obj%gsrch        = 'NO'
      obj%srch1tp      = 'RMO'
      obj%srch1_max_tp = 'RMO'
      obj%srch1_min_tp = 'RMO'
      obj%lss          = 3
      obj%smb_min      = 0.0
      obj%velnam       = 'NONE'
      obj%velopt       = 'Velocity trace input'
      obj%winc         = 0.05
      obj%winlen       = 0.05
      obj%wintp        = 'Variable'
      obj%winvel       = 2000.0
 
      obj%avelnam      = PATHCHECK_EMPTY
      obj%velnam       = PATHCHECK_EMPTY

      obj%iavelo       = 2
      obj%ibwdepo      = 1
      obj%ict          = 1
      obj%idatatp      = 1
      obj%iet          = 1
      obj%iewdepo      = 1
      obj%iewdo        = 2
      obj%iivelo       = 2
      obj%inc          = 1
      obj%inf          = 5
      obj%irefdepo     = 1
      obj%is1tp        = 1
      obj%igsrch       = 0
      obj%ist          = 1
      obj%iwintp       = 2
      obj%lop          = 10
      obj%lwin         = 1
      obj%nop          = 101
      obj%nwin         = 1
      obj%survey_units = 'Unknown'
      obj%velbias      = 0.0
      obj%velscale     = 1.0
      obj%velsmode     = 1
      obj%x_hno        = 7
      obj%y_hno        = 8

      obj%ie_s1        = 1
      obj%ie_0n        = 1

      obj%ix_tc        = 1
      obj%ix_ms        = 1
      obj%ix_beta      = 1
      obj%ix_dz        = 1
      obj%ix_fold      = 1
      obj%ix_sx        = 1
      obj%ix_s0        = 1
      obj%ix_nn        = 1
      obj%ix_sm        = 1
      obj%ix_0n        = 1
      obj%ix_op        = 1
      obj%ix_s1        = 1
      obj%ix_ss        = 1
      obj%ix_tmt       = 1
      obj%ix_smt       = 1
      obj%ix_btr       = 1
      obj%ix_dztr      = 1
      obj%ix_gtr       = 1
      obj%ix_rtr       = 1
      obj%ix_mat       = 1
      obj%ix_temp      = 1
      obj%ix_wb        = 1
      obj%ix_wc        = 1
      obj%ix_we        = 1

!
      if (obj%survey_units.eq.'METERS') then
        obj%iunitst=2
      else
        obj%iunitst=1
      end if
!
!      set default for reference window length based on survey_units
!
      if (obj%iunitst.eq.1) then
        if (obj%winlen.eq.0.15) then
          obj%winlen=0.05
        end if
      else
        if (obj%winlen.eq.0.05) then
          obj%winlen=0.15
        end if
      end if
!
!      set default for reference interval length based on survey_units
!
      if (obj%iwintp.eq.2) then
        if (obj%iunitst.eq.1) then
          if (obj%winvel.eq.6000.0) then
            obj%winvel=2000.0
          end if
        else
          if (obj%winvel.eq.2000.0) then
            obj%winvel=6000.0
          end if
        end if
      end if

! --> Change the default values below as needed

      obj%n_srch1_depth          = 0

      if(associated(obj%ref1_offset )) obj%ref1_offset  = 0.0
      if(associated(obj%srch1_depth )) obj%srch1_depth  = 0.0
      if(associated(obj%srch1_max   )) obj%srch1_max    = 0.0
      if(associated(obj%srch1_min   )) obj%srch1_min    = 0.0

      call abra_update (obj)
      end subroutine abra_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine abra_update (obj)
      type(abra_struct),intent(inout),target :: obj             ! arguments
!
      character cmsg*128, veltype*4
!
      integer :: iavelo, ibwdepo, ict, idatatp, iet, iewdepo, iivelo  
      integer :: imax, inc, inf, is1tp, igsrch, ist, iwintp
      integer :: lbwa1, ldzwa1, liwa1, lop, lss, lun, lwa1, lwa2, lwin
      integer :: max_samps
      integer :: nop, nsrch1f, ntvals, numtr_in, numtr_out, nvals, nwin, norder
!
      logical :: lavreq, lerror, livreq, lsemf, lstrc
!
      real :: bwda, bwdep, ewda, ewdep, samp_int_in, vsr, winc, winlen, winvel
!
! --> Insert code to declare all required local variables.

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      if (pathchoose_update (obj%pathchoose1, obj%avelnam)) return
      if (pathchoose_update (obj%pathchoose2, obj%velnam)) return

      call pc_register_array_names('SRCH1_DEPTH_ARRAYSET', (/'SRCH1_DEPTH',    &
        'SRCH1_MIN  ', 'SRCH1_MAX  ', 'REF1_OFFSET'/))

! --> Delete any of the globals below that are not needed:

      obj%ipn = pc_get_ipn()

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

      call pc_get('AMPOPT      ', obj%ampopt)
      call pc_get('ARATIO      ', obj%aratio)
      call pc_get('AVELNAM     ', obj%avelnam)
      call pc_get('AVELOPT     ', obj%avelopt)
      call pc_get('BWDA        ', obj%bwda)
      call pc_get('BWDEP       ', obj%bwdep)
      call pc_get('BWDEPO      ', obj%bwdepo)
      call pc_get('DATATP      ', obj%datatp)
      call pc_get('DEBUG_HNO   ', obj%debug_hno)
      call pc_get('EWDA        ', obj%ewda)
      call pc_get('EWDEP       ', obj%ewdep)
      call pc_get('EWDEPO      ', obj%ewdepo)
      call pc_get('GSRCH       ', obj%gsrch)
      call pc_get('FOLDMIN     ', obj%foldmin)
      call pc_get('LSEMF       ', obj%lsemf)
      call pc_get('LSS         ', obj%lss)
      call pc_get('LSTRC       ', obj%lstrc)
      call pc_get('WLMAX       ', obj%wlmax)
      call pc_get('WLMIN       ', obj%wlmin)
      call pc_get('NSRCH1F     ', obj%nsrch1f)
      call pc_get('REFDEP      ', obj%refdep)
      call pc_get('REFDEPO     ', obj%refdepo)
      call pc_get('SNORM       ', obj%snorm)
      call pc_get('SPEAKO      ', obj%speako)
      call pc_get('SRCH1TP     ', obj%srch1tp)
      call pc_get('SRCH1_MAX_TP', obj%srch1_max_tp)
      call pc_get('SRCH1_MIN_TP', obj%srch1_min_tp)
      call pc_get('TTYP_HNO    ', obj%ttyp_hno)
      call pc_get('VELNAM      ', obj%velnam)
      call pc_get('VELOPT      ', obj%velopt)
      call pc_get('WINC        ', obj%winc)
      call pc_get('WINLEN      ', obj%winlen)
      call pc_get('WINTP       ', obj%wintp)
      call pc_get('WINVEL      ', obj%winvel)

      call pc_get_pdata('SURVEY_UNITS', obj%survey_units)

!     ****** SRCH1_DEPTH_ARRAYSET ******

      call pc_alloc('SRCH1_DEPTH', obj%srch1_depth, obj%n_srch1_depth)
      call pc_alloc('SRCH1_MIN  ', obj%srch1_min  , obj%n_srch1_depth)
      call pc_alloc('SRCH1_MAX  ', obj%srch1_max  , obj%n_srch1_depth)
      call pc_alloc('REF1_OFFSET', obj%ref1_offset, obj%n_srch1_depth)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


! --> Insert code to verify process parameters here (and/or in traps).
!
! ..... Change options into integer values
!
      if (obj%datatp.eq.'Time                ') then
        obj%idatatp=2
      else
        obj%idatatp=1
      end if
!
      if (obj%avelopt.eq.'Velocity trace input') then
        obj%iavelo=2
      elseif (obj%avelopt.eq.'Velocity trace file ') then
        obj%iavelo=3
      else
        obj%iavelo=1
      end if
!
      if (obj%velopt.eq.'Velocity trace input') then
        obj%iivelo=2
      elseif (obj%velopt.eq.'Velocity trace file ') then
        obj%iivelo=3
      else
        obj%iivelo=1
      end if
!
      if (obj%gsrch.eq.'External            ') then
        obj%igsrch=1
      elseif (obj%gsrch.eq.'Internal            ') then
        obj%igsrch=2
      else
        obj%igsrch=0
      end if
!
      if (obj%refdepo.eq.'Header              ') then
        obj%irefdepo=2
      else
        obj%irefdepo=1
      end if
!
      if (obj%wintp.eq.'Variable            ') then
        obj%iwintp=2
      else
        obj%iwintp=1
      end if
!
      bwdep = obj%bwdep*1000.0
      if (obj%bwdepo.eq.'Header              ') then
        obj%ibwdepo=2
      else if (bwdep.gt.0.0001) then
        obj%ibwdepo=1
      else
        obj%ibwdepo=0
      end if
!
      ewdep = obj%ewdep*1000.0
      if (obj%ewdepo.eq.'Header              ') then
        obj%iewdepo=2
      else if (ewdep.gt.0.0001) then
        obj%iewdepo=1
      else
        obj%iewdepo=0
      end if
!
      if (obj%srch1tp.eq.'Delta V             ') then
        obj%is1tp=2
        obj%srch1_min_tp = 'Delta V'
        obj%srch1_max_tp = 'Delta V'
      else if (obj%srch1tp.eq.'Frac V              ') then
        obj%is1tp=3
        obj%srch1_min_tp = 'Frac V'
        obj%srch1_max_tp = 'Frac V'
      else
        obj%is1tp=1
        obj%srch1_min_tp = 'RMO'
        obj%srch1_max_tp = 'RMO'
      end if
!
!      Initialize needed variables
!
      iavelo=obj%iavelo
      idatatp=obj%idatatp
      iivelo=obj%iivelo
      iwintp=obj%iwintp
      is1tp=obj%is1tp
      igsrch=obj%igsrch
      lsemf=obj%lsemf
      lstrc=obj%lstrc
!
!      determine if the average velocity file is needed
!      (if search1 type = Delta V or input data type = time)
!
      if (is1tp.eq.2 .or. idatatp.eq.2) then
        if (iavelo.eq.1) then
          lavreq=.true.
        elseif (iavelo.eq.3) then
          lavreq=.true.
        else
          lavreq=.false.
        end if
      else
        lavreq=.false.
      end if
!
!      determine if the interval velocity file is needed
!      (if window = variable)
!
      if (iwintp.eq.2) then
        if (iivelo.eq.1) then
          livreq=.true.
        elseif (iivelo.eq.3) then
          livreq=.true.
        else
          livreq=.false.
        end if
      else
        livreq=.false.
      end if
!
!      get average velocity file if selected
!      (if search1 type = Delta V or input data type = time)
!
      if (iavelo.eq.1) then
        call pathcheck('avelnam', obj%avelnam, &
                       'vel', lavreq, show=PATHCHECK_INFO_INPUT)
      elseif (iavelo.eq.3) then
        call pathcheck('avelnam', obj%avelnam, &
                       '.trc', lavreq, show=PATHCHECK_INFO_INPUT)
      end if
!
!      get interval velocity file if file and window = variable
!
      if (iivelo.eq.1) then
        call pathcheck('velnam', obj%velnam, &
                       'vel', livreq, show=PATHCHECK_INFO_INPUT)
      elseif (iivelo.eq.3) then
        call pathcheck('velnam', obj%velnam, &
                       '.trc', livreq, show=PATHCHECK_INFO_INPUT)
      end if
!
!      determine the number of output traces
!
      numtr_in = obj%numtr
!      determine # of possible windows
      max_samps = obj%ndpt

!      Input units assumed to be in seconds, Kft, or Km
      samp_int_in = obj%dt*1000.0
      winc = obj%winc*1000.0
      winlen = obj%winlen*1000.0
!
      lwin =  winlen/samp_int_in
      lwin =  2*(lwin/2) + 1
      inc   =  winc/samp_int_in
      nwin  =  1 + (max_samps-lwin)/inc

!      default the # of output traces to 6
      numtr_out = 6

!      set the number of output traces based on # of possible windows
      if (lsemf) then
        numtr_out = numtr_out + nwin
      end if
!
      if (lstrc) then
        numtr_out = numtr_out+numtr_in
      end if
!
!       change max number of traces if necessary
!
      if (numtr_out.gt.numtr_in) then
        obj%numtr=numtr_out
      end if
!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put("avelnam" , obj%avelnam)
      call pc_put("velnam" , obj%velnam)
!
!      take out items that have not implemented
!
      call pc_put_sensitive_field_flag ('gsrch',       .false.)
      call pc_put_sensitive_field_flag ('ampopt',      .false.)
      call pc_put_sensitive_field_flag ('aratio',      .false.)
!
      if (is1tp.eq.2 .or. idatatp.eq.2) then
        call pc_put_sensitive_field_flag ('avelopt',        .true.)
        if (iavelo.eq.2) then
          call pc_put_sensitive_field_flag ('avelnam',        .false.)
          call pc_put_sensitive_field_flag ('select_avelnam', .false.)
        else
          call pc_put_sensitive_field_flag ('avelnam',        .true.)
          call pc_put_sensitive_field_flag ('select_avelnam', .true.)
        end if
      else
        call pc_put_sensitive_field_flag ('avelnam',        .false.)
        call pc_put_sensitive_field_flag ('select_avelnam', .false.)
        call pc_put_sensitive_field_flag ('avelopt',        .false.)
      end if
!
      if (iwintp.ne.2) then
        call pc_put_sensitive_field_flag ('velnam',         .false.)
        call pc_put_sensitive_field_flag ('select_velnam',  .false.)
        call pc_put_sensitive_field_flag ('velopt',         .false.)
      else if (iivelo.eq.2) then
        call pc_put_sensitive_field_flag ('velnam',         .false.)
        call pc_put_sensitive_field_flag ('select_velnam',  .false.)
        call pc_put_sensitive_field_flag ('velopt',         .true.)
      else
        call pc_put_sensitive_field_flag ('velnam',         .true.)
        call pc_put_sensitive_field_flag ('select_velnam',  .true.)
        call pc_put_sensitive_field_flag ('velopt',         .true.)
      end if
!
      if (iwintp.eq.2) then
        call pc_put_sensitive_field_flag ('winvel', .true.)
        call pc_put_sensitive_field_flag ('wlmin', .true.)
        call pc_put_sensitive_field_flag ('wlmax', .true.)
      else
        call pc_put_sensitive_field_flag ('winvel', .false.)
        call pc_put_sensitive_field_flag ('wlmin', .false.)
        call pc_put_sensitive_field_flag ('wlmax', .false.)
      end if
!
      call pc_put_options_field('AVELOPT', (/'Velocity file       ',           &
        'Velocity trace input','Velocity trace file '/) )
      call pc_put_options_field('BWDEPO ', (/'Header              ',           &
        'Constant            '/) )
      call pc_put_options_field('DATATP ', (/'Depth               ',           &
        'Time                '/) )
      call pc_put_options_field('EWDEPO ', (/'Header              ',           &
        'Constant            '/) )
      call pc_put_options_field('GSRCH  ', (/'External            ',           &
        'Internal            ','NO                  '/) )
      call pc_put_options_field('VELOPT ', (/'Velocity file       ',           &
        'Velocity trace input','Velocity trace file '/) )
      call pc_put_options_field('REFDEPO', (/'Header              ',           &
        'Constant Depth      '/) )
      call pc_put_options_field('SRCH1TP', (/'RMO                 ',           &
        'Delta V             ','Frac V              '/) )
      call pc_put_options_field('WINTP  ', (/'Constant            ',           &
        'Variable            '/) )

! --> Delete any of the globals below that have not changed:

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('grid'    , obj%grid)

      call pc_put('AMPOPT      ', obj%ampopt)
      call pc_put('ARATIO      ', obj%aratio)
      call pc_put('AVELNAM     ', obj%avelnam)
      call pc_put('AVELOPT     ', obj%avelopt)
      call pc_put('BWDA        ', obj%bwda)
      call pc_put('BWDEP       ', obj%bwdep)
      call pc_put('BWDEPO      ', obj%bwdepo)
      call pc_put('DATATP      ', obj%datatp)
      call pc_put('DEBUG_HNO   ', obj%debug_hno)
      call pc_put('EWDA        ', obj%ewda)
      call pc_put('EWDEP       ', obj%ewdep)
      call pc_put('EWDEPO      ', obj%ewdepo)
      call pc_put('FOLDMIN     ', obj%foldmin)
      call pc_put('LSEMF       ', obj%lsemf)
      call pc_put('LSS         ', obj%lss)
      call pc_put('LSTRC       ', obj%lstrc)
      call pc_put('WLMAX       ', obj%wlmax)
      call pc_put('WLMIN       ', obj%wlmin)
      call pc_put('NSRCH1F     ', obj%nsrch1f)
      call pc_put('REFDEP      ', obj%refdep)
      call pc_put('REFDEPO     ', obj%refdepo)
      call pc_put('SNORM       ', obj%snorm)
      call pc_put('SPEAKO      ', obj%speako)
      call pc_put('SRCH1_MAX_TP', obj%srch1_max_tp)
      call pc_put('SRCH1_MIN_TP', obj%srch1_min_tp)
      call pc_put('SRCH1TP     ', obj%srch1tp)
      call pc_put('GSRCH       ', obj%gsrch)
      call pc_put('VELNAM      ', obj%velnam)
      call pc_put('VELOPT      ', obj%velopt)
      call pc_put('TTYP_HNO    ', obj%ttyp_hno)
      call pc_put('WINC        ', obj%winc)
      call pc_put('WINLEN      ', obj%winlen)
      call pc_put('WINTP       ', obj%wintp)
      call pc_put('WINVEL      ', obj%winvel)

      call pc_put('SRCH1_DEPTH', obj%srch1_depth, obj%n_srch1_depth)
      call pc_put('SRCH1_MIN  ', obj%srch1_min  , obj%n_srch1_depth)
      call pc_put('SRCH1_MAX  ', obj%srch1_max  , obj%n_srch1_depth)
      call pc_put('REF1_OFFSET', obj%ref1_offset, obj%n_srch1_depth)

! --> Change the control defaults below as appropriate:

      call pc_put_control ('ntapes'       , 0)
      call pc_put_control ('need_request' , .false.)
      call pc_put_control ('need_label'   , .false.)
      call pc_put_control ('twosets'      , .false.)
      call pc_put_control ('nscratch'     , 0)
      call pc_put_control ('nstore'       , 0)
      call pc_put_control ('iftd'         , .false.)
      call pc_put_control ('ndisk'        , 0)
      call pc_put_control ('setup_only'   , .false.)

!      Make sure parrallel_safe is set to true
!     call pc_put_control ('parallel_safe'        , .false.)

      call pc_put_control ('parallel_safe'        , .true.)
      call pc_put_control ('pcps_send_mode'       ,'pcps_send_first_avail')
      call pc_put_control ('pcps_receive_mode'    ,'pcps_receive_passthru')
      call pc_put_control ('pcps_bunch_mode'      ,'pcps_bunch_trace_groups')
      call pc_put_control ('pcps_send_eof_mode'   ,'pcps_send_all_eof')
      call pc_put_control ('pcps_alt_send_mode'   ,'pcps_send_all')
      call pc_put_control ('pcps_alt_receive_mode','pcps_receive_all_eof')
      call pc_put_control ('pcps_resequence_mode' ,'pcps_resequence_traces')
      call pc_put_control ('pcps_generator_mode'  ,'pcps_no_trace_gen')
!
      call pc_put_visible_flag ('avelnam_info', lavreq  )
      call pc_put_visible_flag ('velnam_info', livreq )

! --> Add here any other parameter cache calls such as to set sensitivities.


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


! --> Insert code to initialize variables for execution or deallocate arrays
! --> which will be reallocated below.

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

! --> Insert code to allocate needed permanent memory.

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

! --> Insert code to initialize anything needed for actual execution of process.
      max_samps = obj%ndpt
      nsrch1f = obj%nsrch1f
!
!     SET NEEDED VARIABLES
!     (Input units assumed to be in seconds, Kft, or Km)
!
      bwdep = obj%bwdep*1000.0
      bwda = obj%bwda*1000.0
      ewdep = obj%ewdep*1000.0
      ewda = obj%ewda*1000.0
      ibwdepo = obj%ibwdepo
      idatatp = obj%idatatp
      iewdepo = obj%iewdepo
      lss = obj%lss
      samp_int_in = obj%dt*1000.0
      winc = obj%winc*1000.0
      winlen = obj%winlen*1000.0
      winvel = obj%winvel
      iavelo=obj%iavelo
      iivelo=obj%iivelo
      lun=6

! --> call velterp to open a CPS average velocity file
      if (lavreq) then
        if (iavelo.eq.1) then
          norder = 2
          veltype = 'VTNM'
          call velterp_create(obj%id_avfile, obj%avelnam, obj%ndpt, obj%dt, &
            obj%velbias, obj%velscale, obj%velsmode, obj%x_hno, obj%y_hno,  &
            lerror, cmsg, norder, veltype)
          if (lerror) call pc_error(cmsg)
! --> call rantfile to open an average velocity trace file
        else if (iavelo.eq.3) then
          call rantfile_create(obj%id_avtfile, obj%avelnam, obj%nwih, &
            obj%ndpt, 0.0, obj%dt, obj%x_hno, obj%y_hno, lun, lerror, cmsg)
          if (lerror) call pc_error(cmsg)
        end if
      end if

! --> call velterp to open a CPS interval velocity file
      if (livreq) then
        if (iivelo.eq.1) then
          norder = 2
          if (idatatp.eq.1) then
             veltype = 'VZIN'
             vsr = obj%dt
!              Put sample rate in Meters if the sample rate is kilometers
!              Put sample rate in Feet if the sample rate is kilofeet
!              (A sample rate less than one ussumes kilometers)
             if (vsr.lt.1.0) then
                vsr = obj%dt*1000.0
             end if
          else
             veltype = 'VTIN'
             vsr = obj%dt
          end if
          call velterp_create(obj%id_ivfile, obj%velnam, obj%ndpt, vsr,     &
            obj%velbias, obj%velscale, obj%velsmode, obj%x_hno, obj%y_hno,  &
            lerror, cmsg, norder, veltype)
          if (lerror) call pc_error(cmsg)
! --> call rantfile to open an interval velocity trace file
        else if (iivelo.eq.3) then
          call rantfile_create(obj%id_ivtfile, obj%velnam, obj%nwih, &
            obj%ndpt, 0.0, obj%dt, obj%x_hno, obj%y_hno, lun, lerror, cmsg)
          if (lerror) call pc_error(cmsg)
        end if
      end if
!
!     PERFORM INITIALIZATION
!
      lwin      =  winlen/samp_int_in
      lwin      =  2*(lwin/2) + 1
      inc       =  winc/samp_int_in
!
      if (ibwdepo.eq.1) then
        bwdep = bwdep+bwda
        ict   = bwdep/samp_int_in + 1.0
        ist   = bwdep/samp_int_in + 1.0 - (lwin/2)
        ict   = max(1,ict)
        ist   = max(1,ist)
      else
        ict   = 1
        ist   = 1
      end if
!
      if (iewdepo.eq.1) then
        ewdep = ewdep+ewda
        iet   = ewdep/samp_int_in + 1.0
        iet   = min0( iet,max_samps )
      else
        iet   = max_samps
      end if
!      determine # of possible windows
      nwin =  1 + (max_samps-lwin)/inc
!
      obj%inc = inc
      obj%ist = ist
      obj%ict = ict
      obj%iet = iet
      obj%lwin = lwin
      obj%nwin = nwin
!
! ..... Set interpolation parameters
      lop   = 10
      nop   = 101
      inf   = 5
      obj%lop   = lop
      obj%nop   = nop
      obj%inf   = inf
!
! --> Insert code to allocate needed permanent memory.
!
!     Set up the work arrays
!
      lwa1 = 0
      lwa2 = 0
      lbwa1 = 0
      liwa1 = 0
      ldzwa1 = 0
!      ix_tc  :  Center times for windows
      obj%ix_tc = lwa1 + 1
      lwa1 = lwa1 + nwin
!      ix_ms  :  Maximum stretch times for windows
      obj%ix_ms = lwa1 + 1
      lwa1 = lwa1 + nwin

!      IX_BETA  :  Beta values
      obj%ix_beta = lwa1 + 1
      lwa1 = lwa1 + nwin
!      IX_DZ    :  Delta Z values
      obj%ix_dz = lwa1 + 1
      lwa1 = lwa1 + nwin
!      IX_FOLD  :  Fold value in each window
      obj%ix_fold = lwa1 + 1
      lwa1 = lwa1 + nwin
!      IX_SX  :  Semblance maxima for each window
      obj%ix_sx = lwa1 + 1
      lwa1 = lwa1 + nwin
!      IX_RMS  :  RMS amplitudes
      obj%ix_rms = lwa1 + 1
      lwa1 = lwa1 + nwin
!      IX_MA  :  Maximum amplitudes
      obj%ix_ma = lwa1 + 1
      lwa1 = lwa1 + nwin
!      IX_S0  :  Number of traces for each (tc,vel,dta)
      obj%ix_s0 = lwa1 + 1
      lwa1 = lwa1 + nwin*nsrch1f
! ..... IX_NN    -   Summed energies for each (tc,vel,dta)
      obj%ix_nn = lwa1 + 1
      lwa1 = lwa1 + nwin*nsrch1f
! ..... IX_SM    -   Semblance matrix for each window
      obj%ix_sm = lwa1 + 1
      lwa1 = lwa1 + nwin*nsrch1f
! ..... IX_SS - Smoothed semblance matrix for each window
      obj%ix_ss = lwa1 + 1
      lwa1 = lwa1 + nwin*nsrch1f
! ..... IX_0N    -   Summed traces for each (tc,vel,dta)
      obj%ix_0n = lwa1 + 1
      lwa1 = lwa1 + lwin*nwin*nsrch1f
      obj%ie_0n = lwa1

! Use a 2nd work array
! ..... IX_OP  :  Interpolation operators
      obj%ix_op = lwa2 + 1
      lwa2 = lwa2 + nop*lop
! ..... IX_S1    -   Interpolated trace buffer
      obj%ix_s1 = lwa2 + 1
      lwa2 = lwa2 + inf*max_samps
      obj%ie_s1 = lwa2
! ..... IX_TMT   -   Time trace buffer
      obj%ix_tmt = lwa2 + 1
      lwa2 = lwa2 + max_samps
! ..... IX_SMT   -   Semblance trace buffer
      obj%ix_smt = lwa2 + 1
      lwa2 = lwa2 + max_samps
! ..... IX_BTR   -   beta trace buffer
      obj%ix_btr = lwa2 + 1
      lwa2 = lwa2 + max_samps
! ..... IX_GTR   -   gamma trace buffer
      obj%ix_gtr = lwa2 + 1
      lwa2 = lwa2 + max_samps
! ..... IX_DZTR  -   delta Z trace buffer
      obj%ix_dztr = lwa2 + 1
      lwa2 = lwa2 + max_samps
! ..... IX_RTR   -   RMS amplitude trace buffer
      obj%ix_rtr = lwa2 + 1
      lwa2 = lwa2 + max_samps
! ..... IX_MAT   -   Max amplitude trace buffer
      obj%ix_mat = lwa2 + 1
      lwa2 = lwa2 + max_samps
! ..... IX_TEMP  -   memory for a temporary work area
      obj%ix_temp = lwa2 + 1
      ntvals=(lop+2)*4
!
      nvals=max_samps+lop
      if (ntvals.le.nvals) ntvals=nvals
!
      nvals=lwin*2
      if (ntvals.le.nvals) ntvals=nvals
!
      nvals=(nwin+2)*3.0
      if (ntvals.le.nvals) ntvals=nvals
      lwa2 = lwa2+ntvals

!      IX_WB  :  Window beginning for each window
      obj%ix_wb = liwa1 + 1
      liwa1 = liwa1 + nwin
!      IX_WC  :  Window center for each window
      obj%ix_wc = liwa1 + 1
      liwa1 = liwa1 + nwin
!      IX_WE  :  Window end for each window
      obj%ix_we = liwa1 + 1
      liwa1 = liwa1 + nwin
!
!     Set up the beta work array     
!
      lbwa1 = lbwa1 + nsrch1f
!
!     Set up the delta z work array     
!
      ldzwa1 = ldzwa1 + nsrch1f
!
! ..... make sure smoother length is less than half the number of functions
!
      imax=(nsrch1f-1.0)/2.0
      if (lss.gt.imax) then
        lss=imax
        call pc_error ("The smoother length is too large!!")
        obj%lss = lss
      end if
!
!   Allocate your permanent memory like this:
!   Allocation errors will be reported to the parameter cache.
!  
      if (lwa1.gt.0) then
        call mem_alloc (obj%wa1, lwa1)
      end if
!
      if (lwa2.gt.0) then
        call mem_alloc (obj%wa2, lwa2)
      end if
!
      if (liwa1.gt.0) then
        call mem_alloc (obj%iwa1, liwa1)
      end if
!
      if (lbwa1.gt.0) then
        call mem_alloc (obj%bwa1, max_samps, lbwa1)
      end if
!
      if (ldzwa1.gt.0) then
        call mem_alloc (obj%dzwa1, max_samps, ldzwa1)
      end if

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine abra_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! --> Insert code for parameter traps here.
! --> (EZCPS with the -t option will insert skeleton code for you)


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine abra (obj,ntr,hd,tr)
      type(abra_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.
!                                                   _________________
!                                                  |                 |
!                                                  | Local Variables |
!                                                  |_________________|
!
      real, dimension(:), allocatable :: rseis
      double precision, dimension(size(hd,1),size(hd,2)) :: hdo ! Output headers
      double precision, dimension(size(hd,1)) :: velahd ! The vel headers
      real, dimension(size(tr,1)) :: velatr ! The average velocity trace
      double precision, dimension(size(hd,1)) :: velihd ! The int vel headers
      real, dimension(size(tr,1)) :: velitr
!
      character cmsg*128
!
      double precision ::  source_detect_dist_in, tc
!
      integer :: cmp_in, data_trace_type_in
      integer :: i, i1, i2, i3, i4, iavelo, iavtrc
      integer :: ib, ibetatr, ibwdepo, ibytes, ic, ict, icw
      integer :: idatatp, idead, idebug_hno, idvtrc, idztr
      integer :: ie, ie1, ie2, ieflg, ierr, iet, iewin, iewdepo, iewdo 
      integer :: ie_s1, ie_0n
      integer :: igamatr, ihdrno, iivelo, iivtrc, imatr, imax 
      integer :: inc, inf, ino, irefdepo, irmstr  
      integer :: is_a, is_r, is_g, is_f, is1tp, isftr, isigtr, ist, ist1 
      integer :: istrc, it, it1, it2, itc, itrcfg, ittyp_hno, itx, iwin 
      integer :: ix_0n, ix_btr, ix_beta, ix_dz, ix_dztr, ix_gtr, ix_fold
      integer :: ix_ma, ix_mat, ix_ms, ix_nn, ix_op
      integer :: ix_rms, ix_rtr, ix_s1, ix_sm, ix_smt, ix_s0, ix_ss
      integer :: ix_st, ix_sx
      integer :: ix_wb, ix_wc, ix_we
      integer :: ix_temp, ix_tc, ix_tmt, ivaflg, iwintp   
      integer :: jerror, jst, jx_0n, jx_nn, jx_s0
      integer :: kbytes, kx_0n, kx_beta, kx_dz, kx_fold, kx_ma, kx_nn, kx_s0
      integer :: kx_rms, kx_sm, kx_ss, kx_sx
      integer :: lcwin, lop, lss, ltot, lvwin, lwin, lwrk_st, max_samps
      integer :: nbytes, nhdrs, nintvls, nseis, n_srch1_depth, nsrch1f 
      integer :: ntr_vel, ntrc_gath_in, ntrc_gath_out, nvals, nwin
      integer :: trace_flag, trace_num_in, x_hno
      integer :: y_hno 
!
      logical :: lchange, lerror, lsemf, lstrc, speako
!
      real :: a1 
      real :: beta, betax, bmax, bmax1, bmax2, bmin, bmin1, bmin2
      real :: bwda, bwdep, bwdhno, cdep
      real :: dd, den, dtx, dtmx, dmax1, dmax2, dmin1, dmin2
      real :: dzmax, dzmax1, dzmax2, dzmin, dzmin1, dzmin2
      real :: bdepth, edepth, ewda, ewdep, ewdhno
      real :: foldmin, frnyq, href1, href2  
      real :: rate, ratio, rdhno, refdep, rmax, rmin
      real :: samp_int_in, smb_min, snorm, srs, sri, sts
      real :: tc2, te, tx, tx2, txx, x, xx, xcoord, ycoord 
      real :: winlen, winvel, wlmax, wlmin
      real :: vmax1, vmax2, vmin1, vmin2, vwinln, zod1, zod2, zs1, zs2
!
      real :: tb, time, vc, torig(10) 
      integer :: inilfg, iwflg, norig
!
      data  idead   / 0 /
      data  istrc   / 1 /
      data  iavtrc  /51 /
      data  idvtrc  /56 /
      data  iivtrc  /86 /
      data  ibetatr /81 /
      data  igamatr /84 /
      data  idztr   /87 /
      data  imatr   /83 /
      data  irmstr  /82 /
      data  isigtr  /71 /
      data  isftr   /72 /

! --> Insert code for processing logic.
!
!     The following conditional call to the WRAPUP routine should be made
!     before returning from this subroutine:
!
      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
        call abra_wrapup(obj)
        return
      end if
!
      ist1=0
!
!      set pointers into the work array
!
      ix_tc   = obj%ix_tc
      ix_ms   = obj%ix_ms
      ix_beta = obj%ix_beta
      ix_dz   = obj%ix_dz
      ix_fold = obj%ix_fold
      ix_sx   = obj%ix_sx
      ix_s0   = obj%ix_s0
      ix_nn   = obj%ix_nn
      ix_sm   = obj%ix_sm
      ix_0n   = obj%ix_0n
      ix_op   = obj%ix_op
      ix_s1   = obj%ix_s1
      ix_ss   = obj%ix_ss
      ix_rms  = obj%ix_rms
      ix_ma   = obj%ix_ma
      ix_smt  = obj%ix_smt
      ix_tmt  = obj%ix_tmt
      ix_btr  = obj%ix_btr
      ix_dztr = obj%ix_dztr
      ix_gtr  = obj%ix_gtr
      ix_rtr  = obj%ix_rtr
      ix_mat  = obj%ix_mat
      ix_temp = obj%ix_temp
      ix_wb   = obj%ix_wb
      ix_wc   = obj%ix_wc
      ix_we   = obj%ix_we
!
      ie_s1   = obj%ie_s1
      ie_0n   = obj%ie_0n
!
!      INITIALIZE
!      (obj%ndpt is number of sample values in a trace)
!      (obj%nwih is number of header words)
!      (obj%numtr is max number of traces input/output)
!      (obj%dt is trace sample interval)
!      (Input units in seconds, Kft, or Km)
!
      max_samps=obj%ndpt
      nhdrs=obj%nwih
      ntrc_gath_in=ntr
      samp_int_in=obj%dt*1000.0
!
      bwda=obj%bwda*1000.0
      bwdep=obj%bwdep*1000.0
      bwdhno=obj%bwdep
      ewda=obj%ewda*1000.0
      ewdep=obj%ewdep*1000.0
      ewdhno=obj%ewdep
      foldmin=obj%foldmin
      iavelo=obj%iavelo
      ibwdepo=obj%ibwdepo
      ict=obj%ict
      idatatp=obj%idatatp
      idebug_hno=obj%debug_hno
      iet=obj%iet
      iewdepo=obj%iewdepo
      iewdo=obj%iewdo
      iivelo=obj%iivelo
      inc=obj%inc
      inf=obj%inf
      irefdepo=obj%irefdepo
      is1tp=obj%is1tp
      ist=obj%ist
      ittyp_hno=obj%ttyp_hno
      iwintp=obj%iwintp
      lop=obj%lop
      lsemf=obj%lsemf
      lss=obj%lss
      lstrc=obj%lstrc
      lwin=obj%lwin
      wlmin=obj%wlmin*1000.0
      wlmax=obj%wlmax*1000.0
      nsrch1f=obj%nsrch1f
      nwin=obj%nwin
      n_srch1_depth=obj%n_srch1_depth
      rdhno=obj%refdep
      refdep=obj%refdep*1000.0
      speako=obj%speako
      snorm=obj%snorm
      smb_min=obj%smb_min
      x_hno=obj%x_hno
      winlen=obj%winlen*1000.0
      winvel=obj%winvel
      y_hno=obj%y_hno
!
      nbytes=4
      ivaflg=0
!
      cmp_in = hd( HDR_CURRENT_GROUP, 1 )
!
!      determine the trace type header number
!
      IF (ittyp_hno.LT.1 .OR. ittyp_hno.GT.NHDRS) THEN
        CALL PC_ERROR("Invalid header number for the trace type")
        RETURN
      END IF
!
!      the beginning & ending depths may be based on headers word 
!      (Input units assumed to be in seconds, Kft, or Km)
!
      it=1
      if (ibwdepo.eq.2) then
        ihdrno = bwdhno
        if (ihdrno.lt.1 .or. ihdrno.gt.nhdrs) then
          call pc_error("Invalid header word number for the First Analysis &
            &Window: %i",ihdrno)
          bwdep=bwda
          ict = bwdep/samp_int_in + 1.0
          ict = max(1,ict)
          ist = bwdep/samp_int_in + 1.0 - (lwin/2)
          ist = max(1,ist)
        else
          bwdep=(hd(ihdrno,it)*1000.0)+bwda
          ict = bwdep/samp_int_in + 1.0
          ict = max(1,ict)
          ist = bwdep/samp_int_in + 1.0 - (lwin/2)
          ist = max(1,ist)
        end if
      elseif (ibwdepo.eq.0) then
        bwdep=bwda
        ict = bwdep/samp_int_in + 1.0
        ict = max(1,ict)
        ist = bwdep/samp_int_in + 1.0 - (lwin/2)
        ist = max(1,ist)
      end if

!
      if (iewdepo.eq.2) then
        ihdrno = ewdhno
        if (ihdrno.lt.1 .or. ihdrno.gt.nhdrs) then
          call pc_error("Invalid header word number for the Last Analysis &
            &Window: %i",ihdrno)
          ewdep=(max_samps-1)*samp_int_in+ewda
          iet = ewdep/samp_int_in + 1.0
          iet =  min0( iet,max_samps )
        else
          iet=(hd(ihdrno,it)*1000.0)/samp_int_in + 1.0
          if (iet.le.1) then
            ewdep=(max_samps-1)*samp_int_in+ewda
            iet = ewdep/samp_int_in + 1.0
            iet =  min0( iet,max_samps )
          else
            iet=(hd(ihdrno,it)*1000.0)/samp_int_in + 1.0 + (lwin/2)
            ewdep=(iet-1)*samp_int_in+ewda
            iet = ewdep/samp_int_in + 1.0
            iet =  min0( iet,max_samps )
          end if
        end if
      elseif (iewdepo.eq.0) then
        ewdep=(max_samps-1)*samp_int_in+ewda
        iet = ewdep/samp_int_in + 1.0
        iet =  min0( iet,max_samps )
      end if
!
! ..... Interpolation controls
!
      srs   = samp_int_in/1000.
      sts   = srs*(ist-1)
      inf   = 5
      sri   = srs/float(inf)
      ltot  = max_samps - ist + 1
      it1   = ix_temp
!      determine # of possible windows
      nwin      =  1 + (iet-ist-lwin+1)/inc
!
! ..... Read the average CPS or Modspec velocity file
!       (If search type = Delta V)
!       (If Input data type = Time)
!
      if (is1tp.eq.2 .or. idatatp.eq.2) then
        if (iavelo.eq.1) then
          ntr_vel=1
          xcoord = hd(x_hno, 1)
          ycoord = hd(y_hno, 1)
          call velterp_find(obj%id_avfile, xcoord, ycoord, velatr, lchange, &
            norig, torig)
!
! ...... Get needed velocity data
!
        else if (iavelo.eq.2) then
          velatr(1:max_samps)=0.0
          it=0
          do i2 = 1, ntrc_gath_in
            data_trace_type_in = hd(ittyp_hno, i2 )
!
! ........   Copy trace data into the velocity array
            if (data_trace_type_in.eq.iavtrc) then
              it=i2
              ivaflg=1
              velatr(1:max_samps)=tr(1:max_samps,i2)
!              exit loop
              exit
            end if
          enddo

! ......   Display error if average velocity trace not found
          if (it.eq.0) then
            if (is1tp.eq.2) then
              call pc_error ("The average velocity trace not found!!  Type of &
                &Search = Delta V  TTYP_HNO=51")
            else if (idatatp.eq.2) then
              call pc_error ("The average velocity trace not found!!  Input &
                &data type = Time.  TTYP_HNO=51")
            end if
          end if
!
! ...... Read the velocity trace file
!
        else if (iavelo.eq.3) then
!
          ntr_vel=1
          xcoord = hd(x_hno, 1)
          ycoord = hd(y_hno, 1)
          call rantfile_find(obj%id_avtfile, xcoord, ycoord, velahd, velatr, &
            lerror, cmsg);
          if (lerror) then
            call pc_error(cmsg)
            return
          end if
        end if
!
! ..... zero out velocity values
!       (If search type not equal to Delta V)
!       (If Input data type not equal to Time)
!
      else
        velatr(1:max_samps)=0.0
      end if
!
! ..... Read the interval CPS or Modspec velocity file
!       (If window type is variable)
!
      if (iwintp.eq.2) then
        if (iivelo.eq.1) then
          ntr_vel=1
          xcoord = hd(x_hno, 1)
          ycoord = hd(y_hno, 1)
          call velterp_find(obj%id_ivfile, xcoord, ycoord, velitr, lchange, &
            norig, torig)
!
! ...... Get needed velocity data
!
        else if (iivelo.eq.2) then
          velitr(1:max_samps)=0.0
          it=0
          do i2 = 1, ntrc_gath_in
            data_trace_type_in = hd(ittyp_hno, i2 )
!
! ........   Copy trace data into the velocity array
            if (data_trace_type_in.eq.iivtrc) then
              it=i2
              ivaflg=1
              velitr(1:max_samps)=tr(1:max_samps,i2)
!              exit loop
              exit
            end if
          enddo

! ......   Display error if interval velocity trace not found
          if (it.eq.0) then
            call pc_error ("Interval velocity trace not found!!  Analysis &
              &window is variable.  TTYP_HNO=86")
          end if
!
! ...... Read the velocity trace file
!
        else if (iivelo.eq.3) then
!
          ntr_vel=1
          xcoord = hd(x_hno, 1)
          ycoord = hd(y_hno, 1)
          call rantfile_find(obj%id_ivtfile, xcoord, ycoord, velihd, velitr, &
            lerror, cmsg);
          if (lerror) then
            call pc_error(cmsg)
            return
          end if
        end if
!
! ..... zero out velocity values
!       (If window type is not variabe)
!
      else
        velitr(1:max_samps)=0.0
      end if
!
!      the reference depth may be based on a headers word 
!      (Input units for refence depth assumed to be in feet or meters
!
      if (irefdepo.eq.2) then
        ihdrno = rdhno
        it=1
        if (ihdrno.lt.1 .or. ihdrno.gt.nhdrs) then
          call pc_error("Invalid header word number for the Reference Depth: &
            &%i",ihdrno)
          refdep = 0.0
        else
          refdep = hd(ihdrno,it)
        end if
      end if
!
! ..... determine the length of variable analysis windows
!
      if (iwintp.eq.2) then
        ic = ict
        if (ic.eq.1) ic = ic + inc
        edepth = ewdep/1000.0
        iwin=0
        ieflg=0
        do while (ieflg.ne.1)
          iwin=iwin+1

!          determine window length in time
          vwinln=winlen*velitr(ic)/winvel
          if (vwinln.lt.wlmin) vwinln=wlmin
          if (vwinln.gt.wlmax) vwinln=wlmax
!
!          convert window length to # of samples
          lvwin=vwinln/samp_int_in+1
!          make sure the window length has an odd number of samples
          if (mod(lvwin,2).ne.1) then
            lvwin=lvwin+1
          end if
!
          ib = ic - lvwin/2
          ie = ib + lvwin - 1
          if (ib.lt.ist) then
            ib=ist
            ie = (ic - ib) + ic
          end if
!
          if (ie.gt.iet) then
            ie=iet
            ib = ic - (ie - ic)
          end if
!
          tc = srs*(ic-1)
          te = srs*(ie-1)
          if (iwin.gt.nwin) then
            iewin=iwin-1
            ieflg=1
          else if (iewdo.eq.2) then
            if (tc.gt.edepth) then
              iewin=iwin-1
              nwin=iewin
              ieflg=1
            end if
          else
            if (te.gt.edepth) then
              iewin=iwin-1
              nwin=iewin
              ieflg=1
            end if
          end if
!
          if (ieflg.ne.1) then
            dtmx = tc
            obj%iwa1(ix_wb+iwin-1) = ib
            obj%iwa1(ix_wc+iwin-1) = ic
            obj%iwa1(ix_we+iwin-1) = ie
            obj%wa1(ix_tc+iwin-1) = tc
            obj%wa1(ix_ms+iwin-1) = dtmx
            ic = ic + inc
          end if
        end do
!
! ..... Put center times and max. stretch factors in work array
!       (For constant length windows)
!
      else
        ib = ist
        ic = ist + lwin/2
        ie = ist + lwin - 1
        edepth = ewdep/1000.0
        iwin=0
        iewin = nwin
        ieflg=0
        do while (ieflg.ne.1)
          iwin=iwin+1
          tc = srs*(ic-1)
          te = srs*(ie-1)
!
          if (iwin.gt.nwin) then
            ieflg=1
          else if (iewdo.eq.2) then
            if (tc.gt.edepth) then
              iewin=iwin-1
              nwin=iewin
              ieflg=1
            end if
          else
            if (te.gt.edepth) then
              iewin=iwin-1
              nwin=iewin
              ieflg=1
            end if
          end if
!
          if (ieflg.ne.1) then
            dtmx = tc
            obj%iwa1(ix_wb+iwin-1) = ib
            obj%iwa1(ix_wc+iwin-1) = ic
            obj%iwa1(ix_we+iwin-1) = ie
            obj%wa1(ix_tc+iwin-1) = tc
            obj%wa1(ix_ms+iwin-1) = dtmx
            ib = ib + inc
            ic = ic + inc
            ie = ie + inc
          end if
        enddo
      end if
!
! ..... Determine the Beta values & delta Z values
!
      obj%bwa1(1:max_samps,1:nsrch1f)=0.0
      obj%dzwa1(1:max_samps,1:nsrch1f)=0.0
!
!      make sure the reference depth is a multiple of the sample rate
      ino=refdep/samp_int_in+1.0
      refdep=(ino-1)*samp_int_in
!
      nintvls=n_srch1_depth+1
      do i2=1,nintvls
        if (i2.eq.1) then
          ib=ist
          edepth=(obj%srch1_depth(i2)*1000.0)+refdep
          ie=edepth/samp_int_in+1.0
          if (ie.gt.iet) ie=iet
          if (ie.gt.ib) then
            dmin1=obj%srch1_min(i2)*1000.0
            dmax1=obj%srch1_max(i2)*1000.0
            href1=obj%ref1_offset(i2)/2.0
!
            dmin2=obj%srch1_min(i2)*1000.0
            dmax2=obj%srch1_max(i2)*1000.0
            href2=obj%ref1_offset(i2)/2.0
          end if
        else if (i2.eq.nintvls) then
          bdepth=(obj%srch1_depth(n_srch1_depth)*1000.0)+refdep
          ib=bdepth/samp_int_in+1.0
          ie=iet
          if (ie.gt.ib) then
            dmin1=obj%srch1_min(n_srch1_depth)*1000.0
            dmax1=obj%srch1_max(n_srch1_depth)*1000.0
            href1=obj%ref1_offset(n_srch1_depth)/2.0
!
            dmin2=obj%srch1_min(n_srch1_depth)*1000.0
            dmax2=obj%srch1_max(n_srch1_depth)*1000.0
            href2=obj%ref1_offset(n_srch1_depth)/2.0
          end if
        else
          bdepth=(obj%srch1_depth(i2-1)*1000.0)+refdep
          ib=bdepth/samp_int_in+1.0
          dmin1=obj%srch1_min(i2-1)*1000.0
          dmax1=obj%srch1_max(i2-1)*1000.0
          href1=obj%ref1_offset(i2-1)/2.0
!
          edepth=(obj%srch1_depth(i2)*1000.0)+refdep
          ie=edepth/samp_int_in+1.0
          if (ie.gt.iet) ie=iet
          dmin2=obj%srch1_min(i2)*1000.0
          dmax2=obj%srch1_max(i2)*1000.0
          href2=obj%ref1_offset(i2)/2.0
        end if
!
        if (ie.gt.ib) then
!          RMO values
          if (is1tp.eq.1) then
            zod1=(ib-1)*samp_int_in
            zod2=(ie-1)*samp_int_in
            zs1=zod1+dmin1
            if (zs1.gt.0) then
              bmin1=((zs1**2)-(zod1**2))/(href1**2)
            else
              bmin1=-1*(dmin1**2)/(href1**2)
            endif
!
            zs1=zod1+dmax1
            if (zs1.gt.0) then
              bmax1=((zs1**2)-(zod1**2))/(href1**2)
            else
              bmax1=-1*(dmax1**2)/(href1**2)
            endif
!
            zs2=zod2+dmin2
            if (zs2.gt.0) then
              bmin2=((zs2**2)-(zod2**2))/(href2**2)
            else
              bmin2=-1*(dmin2**2)/(href2**2)
            endif
!
            zs2=zod2+dmax2
            if (zs1.gt.0) then
              bmax2=((zs2**2)-(zod2**2))/(href2**2)
            else
              bmax2=-1*(dmax2**2)/(href2**2)
            endif
!
!          delta v values
          else if (is1tp.eq.2) then
            if (idatatp.eq.1) then
              bmin1=(((velatr(ib)+dmin1)/velatr(ib))**2)-1.0
              bmin2=(((velatr(ie)+dmin2)/velatr(ie))**2)-1.0
              bmax1=(((velatr(ib)+dmax1)/velatr(ib))**2)-1.0
              bmax2=(((velatr(ie)+dmax2)/velatr(ie))**2)-1.0
            else
              bmin1=4.0/((velatr(ib)+dmin1)**2)
              bmin2=4.0/((velatr(ie)+dmin1)**2)
              bmax1=4.0/((velatr(ib)+dmax1)**2)
              bmax2=4.0/((velatr(ie)+dmax2)**2)
            end if
!
!             v fraction values
          else if (is1tp.eq.3) then
            if (idatatp.eq.1) then
              bmin1=((dmin1/100.0)**2)-1.0
              bmin2=((dmin2/100.0)**2)-1.0
              bmax1=((dmax1/100.0)**2)-1.0
              bmax2=((dmax2/100.0)**2)-1.0
!
            else
              bmin1=4.0/((dmin1/100.0)**2)
              bmin2=4.0/((dmin2/100.0)**2)
              bmax1=4.0/((dmax1/100.0)**2)
              bmax2=4.0/((dmax2/100.0)**2)
            end if
          end if
!
          zod1=(ib-1)*samp_int_in
          zod2=(ie-1)*samp_int_in
          vmin1=(zod1**2)+((bmin1*(href1**2))/4.0)
          if (vmin1.gt.0.0) then
            dzmin1=sqrt(vmin1)-zod1
          else
            dzmin1=FNIL
          end if
!
          vmax1=(zod1**2)+((bmax1*(href1**2))/4.0)
          if (vmax1.gt.0.0) then
            dzmax1=sqrt(vmax1)-zod1
          else
            dzmax1=FNIL
          end if
!
          vmin2=(zod2**2)+((bmin2*(href2**2))/4.0)
          if (vmin2.gt.0.0) then
            dzmin2=sqrt(vmin2)-zod2
          else
            dzmin2=FNIL
          end if
!
          vmax2=(zod2**2)+((bmax2*(href2**2))/4.0)
          if (vmax2.gt.0.0) then
            dzmax2=sqrt(vmax2)-zod2
          else
            dzmax2=FNIL
          end if
!
          do i3=1,nsrch1f
            do i4=ib,ie
              if (i4.ge.ist .and. i4.le.iet) then
                if (bmin1.eq.bmin2) then
                  bmin=bmin1
                else
                  rate=(bmin2-bmin1)/(ie-ib)
                  ratio=((i4-ib)*rate)/(bmin2-bmin1)
                  bmin=(bmin2-bmin1)*ratio+bmin1
                end if
!
                if (dzmin1.eq.FNIL) then
                  dzmin=FNIL
                else if (dzmin2.eq.FNIL) then
                  dzmin=FNIL
                else if (dzmin1.eq.dzmin2) then
                  dzmin=dzmin1
                else
                  rate=(dzmin2-dzmin1)/(ie-ib)
                  ratio=((i4-ib)*rate)/(dzmin2-dzmin1)
                  dzmin=(dzmin2-dzmin1)*ratio+dzmin1
                end if
!
                if (bmax1.eq.bmax2) then
                  bmax=bmax1
                else
                  rate=(bmax2-bmax1)/(ie-ib)
                  ratio=((i4-ib)*rate)/(bmax2-bmax1)
                  bmax=(bmax2-bmax1)*ratio+bmax1
                end if
!
                if (dzmax1.eq.FNIL) then
                  dzmax=FNIL
                else if (dzmax2.eq.FNIL) then
                  dzmax=FNIL
                else if (dzmax1.eq.dzmax2) then
                  dzmax=dzmax1
                else
                  rate=(dzmax2-dzmax1)/(ie-ib)
                  ratio=((i4-ib)*rate)/(dzmax2-dzmax1)
                  dzmax=(dzmax2-dzmax1)*ratio+dzmax1
                end if
!
                if (bmin.eq.bmax) then
                  obj%bwa1(i4,i3)=bmin
                else
                  rate=(bmax-bmin)/(nsrch1f-1)
                  ratio=((i3-1)*rate)/(bmax-bmin)
                  obj%bwa1(i4,i3)=(bmax-bmin)*ratio+bmin
                end if
!
                if (dzmin.eq.FNIL) then
                  obj%dzwa1(i4,i3)=FNIL
                else if (dzmax.eq.FNIL) then
                  obj%dzwa1(i4,i3)=FNIL
                else if (dzmin.eq.dzmax) then
                  obj%dzwa1(i4,i3)=dzmin
                else
                  rate=(dzmax-dzmin)/(nsrch1f-1)
                  ratio=((i3-1)*rate)/(dzmax-dzmin)
                  obj%dzwa1(i4,i3)=(dzmax-dzmin)*ratio+dzmin
                end if
              end if
            end do
          end do
        end if
      end do
!
!------------------------------------------------------------------
!   Save for debugging purposes
!
!       write out beta values
!       (based on debug header number value)
!
      IF (idebug_hno.LT.1 .OR. idebug_hno.GT.NHDRS) then
        iwflg = 0
      else
        iwflg = hd(idebug_hno, 1 )
      end if
      if (iwflg.eq.1) then
        do i4=1,iet,50
          time=(i4-1)*samp_int_in
          cdep=time
          do i3=1,nsrch1f
            write(6,9150) i4,i3,time,obj%bwa1(i4,i3)
 9150       format(' int_beta',2(i5,1x),2(f10.3,1x))
          end do
        end do
      end if
!------------------------------------------------------------------
!             
!
! ..... Store interpolation operators in the work buffer
      frnyq  = .7
      is_r = ix_temp
      is_g = is_r + lop + 2
      is_f = is_g + lop + 2
      is_a = is_f + lop + 2
      call ap1_cofgen1(lop, frnyq, obj%wa2(is_r:), obj%wa2(is_g:), &
        obj%wa2(is_f:), obj%wa2(is_a:), obj%wa2(ix_op:))
!
! ..... allocate memory for seismic trace data
      it=0
      nseis=0
      ix_st=1
      if (lstrc) then
        lwrk_st=max_samps*ntrc_gath_in
        ibytes = nbytes * lwrk_st
!
        allocate (rseis(lwrk_st), stat=ierr)
!
        if (ierr .ne. 0)  then
          kbytes = ( ibytes + 1023) / 1024
          call pc_error("Error encountered while trying to allocate %i &
            &kbytes of memory.  You must have more memory available to &
            &run this process.  The index for the allocation request was &
            &ix_st",kbytes)
        end if
!
! ..... save seismic data
        do i = 1,ntrc_gath_in
          data_trace_type_in = hd(ittyp_hno, i)
          itrcfg=0
          if (data_trace_type_in.eq.istrc .or. &
              data_trace_type_in.eq.idead) then
            itrcfg=1
          else if (data_trace_type_in.eq.iavtrc) then
            if (iavelo.eq.2) itrcfg=1
          else if (data_trace_type_in.eq.iivtrc) then
            if (iivelo.eq.2) itrcfg=1
          end if
!
          if (itrcfg.eq.1) then
            it=it+1
            ib=(it-1)*max_samps+ix_st
            ie=ib+max_samps-1
            rseis(ib:ie)=tr(1:max_samps,i)
          end if
        enddo
 
        nseis=it
      end if
!
! ..... Zero arrays
!
      ie2=ix_s0+(nwin*nsrch1f)-1
      obj%wa1(ix_s0:ie2)=0.0
      ie2=ix_nn+(nwin*nsrch1f)-1
      obj%wa1(ix_nn:ie2)=0.0
      ie2=ix_0n+(nwin*nsrch1f*lwin)-1
      obj%wa1(ix_0n:ie2)=0.0
      ie2=ix_sx+nwin-1
      obj%wa1(ix_sx:ie2)=0.0
      ie2=ix_beta+nwin-1
      obj%wa1(ix_beta:ie2)=0.0
      ie2=ix_dz+nwin-1
      obj%wa1(ix_dz:ie2)=0.0
      ie2=ix_fold+nwin-1
      obj%wa1(ix_fold:ie2)=0.0
      ie2=ix_sm+(nwin*nsrch1f)-1
      obj%wa1(ix_sm:ie2)=0.0
      ie2=ix_rms+nwin-1
      obj%wa1(ix_rms:ie2)=0.0
      ie2=ix_ma+nwin-1
      obj%wa1(ix_ma:ie2)=0.0
      ie2=ix_gtr+max_samps-1
      obj%wa2(ix_gtr:ie2)=0.0
      ie2=ix_dztr+max_samps-1
      obj%wa2(ix_dztr:ie2)=0.0
!
      txx  = srs*(max_samps-2-lwin/2)
!
!
      jerror   = 0
!
      ntrc_gath_out = 0
      cmp_in = hd( HDR_CURRENT_GROUP, 1 )
!
      do i = 1, ntrc_gath_in
!
!                                         ___________________________
!                                        |                           |
!                                        |  GET NUMERICAL LITERALS   |
!                                        | FROM THE "HD" ARRAY       |
!                                        |___________________________|
!
        cmp_in = hd( HDR_CURRENT_GROUP, i )
! 
        trace_num_in = hd( HDR_CURRENT_CHANNEL, i )
!
        data_trace_type_in = hd(ittyp_hno, i )
!
        source_detect_dist_in = hd( HDR_OFFSET, i )
!
        trace_flag = hd( HDR_BOTTOM_MUTE, i )
!
!*********************************************************************
!*******************  PROCESS TRACE NUMBER "I"  *******************
        if (data_trace_type_in.eq.istrc .or. data_trace_type_in.eq.idead) then
!          Set the first seismic trace number
          if (ist1.eq.0) then
            if (trace_flag.ne.0) ist1 = i
          end if
!
          x = source_detect_dist_in / 2000.0 
          xx = x*x
!
! ..... Interpolate trace
!
          call ap1_trint(tr(ist:,i),ltot,obj%wa2(ix_op:),lop,inf, &
            obj%wa2(ix_s1:),obj%wa2(it1:))
!
! ----------------------------------------------------------------
!       
!
! ..... Loop on analysis parameters
!
          do i2 = 1,nsrch1f
!
! ----------------------------------------------------------------
!
! ..... Loop on analysis windows
!
            jx_s0  = ix_s0 + nwin*(i2-1)
            jx_nn  = ix_nn + nwin*(i2-1)
            jx_0n  = ix_0n + nwin*lwin*(i2-1)
            i1 = ix_s1
            do iwin = 1,nwin
!
              kx_s0  = jx_s0 + iwin - 1
              kx_nn  = jx_nn + iwin - 1
              kx_0n  = jx_0n + lwin*(iwin-1)
!
              icw = obj%iwa1(ix_wc+iwin-1)
!
!              determine window
              ib = obj%iwa1(ix_wb+iwin-1)
              ie = obj%iwa1(ix_we+iwin-1)
              lcwin=(ie-ib)+1

!              make sure current variable window is not too large
              if (iwintp.eq.2) then
                ie1=i1+(lcwin-1)*inf
                if (ie1.gt.ie_s1) then
                  lcwin=(ie_s1-i1)/inf+1
!                  make sure window length is an odd number
                  if (mod(lcwin,2).ne.1) lcwin=lcwin-1
                  if (lcwin.lt.1) lcwin=1
!                  set the window
                  ib = icw - lcwin/2
                  obj%iwa1(ix_wb+iwin-1)=ib
                  obj%iwa1(ix_we+iwin-1)=ib+lcwin-1
                endif
!
                ie1=kx_0n+lcwin-1
                if (ie1.gt.ie_0n) then
                  lcwin=(ie_0n-kx_0n)+1
!                  make sure window length is an odd number
                  if (mod(lcwin,2).ne.1) lcwin=lcwin-1
                  if (lcwin.lt.1) lcwin=1
!                  set the window
                  ib = icw - lcwin/2
                  obj%iwa1(ix_wb+iwin-1)=ib
                  obj%iwa1(ix_we+iwin-1)=ib+lcwin-1
                endif
              end if
!
              icw = obj%iwa1(ix_wc+iwin-1)
!
              beta=obj%bwa1(icw,i2)
              tc  = obj%wa1(ix_tc+iwin-1)
              tc2 = tc*tc
              itc = tc/srs + 1.01
              dtx = obj%wa1(ix_ms+iwin-1)
              betax = beta*xx
!
              tx2  = tc2 + betax
!
!               exit loop if the square root of tx2 would be invalid
              if (tx2.le.0.0) go to 300
!
! ..... Check stretch for this window
!
              tx   = sqrt( tx2 )
              if (tx.gt.txx ) go to 300
!
! ..... New I1 computation
!
              itx = (tx-sts)/sri
              i1  = ix_s1 + itx - inf*(lcwin/2)
              if (i1.lt.ix_s1) i1=ix_s1
!
! ..... Extract window
!
              ie1=i1+(lcwin-1)*inf 
              ie2=it1+lcwin-1 
              obj%wa2(it1:ie2)=obj%wa2(i1:ie1:inf)
!
!               skip if window center is zero
              icw=it1+(lcwin+1)/2
              if (obj%wa2(icw).eq.0.0) goto 300
!
! ..... Extract window and update summed trace, summed energy
!
              obj%wa1(kx_s0) = obj%wa1(kx_s0) + 1.
              ie1=it1+lcwin-1
              dd=dot_product(obj%wa2(it1:ie1),obj%wa2(it1:ie1))
              obj%wa1(kx_nn) = obj%wa1(kx_nn) + dd
!
              ie1=it1+lcwin-1
              ie2=kx_0n+lcwin-1
              obj%wa1(kx_0n:ie2)=obj%wa2(it1:ie1)+obj%wa1(kx_0n:ie2)
 
  300         continue
!
            enddo
          enddo
        end if
      end do
!
!  ---------------------------------------------------
!
! ..... Semblance computation
!
      it1 = ix_temp
      do i2 = 1,nsrch1f
! ..... Loop on analysis windows
!
        jx_s0  = ix_s0 + nwin*(i2-1)
        jx_nn  = ix_nn + nwin*(i2-1)
        jx_0n  = ix_0n + nwin*lwin*(i2-1)
        do iwin = 1,nwin
!
          ib = obj%iwa1(ix_wb+iwin-1)
          ie = obj%iwa1(ix_we+iwin-1)
          lcwin=(ie-ib)+1
!
          kx_s0   = jx_s0 + iwin - 1
          kx_nn   = jx_nn + iwin - 1
          kx_0n   = jx_0n + lwin*(iwin-1)
          kx_sm   = ix_sm + nsrch1f*(iwin-1) + i2 - 1
!
          ie1=kx_0n+lcwin-1
          ie2=it1+lcwin-1
          obj%wa2(it1:ie2)=obj%wa1(kx_0n:ie1)
!
          ie1=it1+lcwin-1
          dd=dot_product(obj%wa2(it1:ie1),obj%wa2(it1:ie1))
          den  = obj%wa1(kx_s0)*obj%wa1(kx_nn)
!
          if (den.ne.0.) then
            dd = dd/den
          else
            dd = 0.0
          end if
!
          obj%wa1(kx_sm) = dd
        enddo
      enddo
!
!------------------------------------------------------------------
!   Save for debugging purposes
!
!       display window begin, center, and end
!       (based on debug header number value)
!
      IF (idebug_hno.LT.1 .OR. idebug_hno.GT.NHDRS) then
        iwflg = 0
      else
        iwflg = hd(idebug_hno, 1 )
      end if
      if (iwflg.eq.1) then
        do i1 = 1,nwin
          iwin=i1
          ib = obj%iwa1(ix_wb+iwin-1)
          ic = obj%iwa1(ix_wc+iwin-1)
          ie = obj%iwa1(ix_we+iwin-1)
          tb=(ib-1)*samp_int_in
          tc=(ic-1)*samp_int_in
          te=(ie-1)*samp_int_in
          write(6,9170) iwin,tb,tc,te
 9170     format(' window: ',i5,1x,3(f10.3,1x))
        enddo
      end if
!
!------------------------------------------------------------------
! ..... Smooth the semblance data
!
! ..... Loop on analysis windows
!
      ie1=ix_sm+(nwin*nsrch1f)-1
      ie2=ix_ss+(nwin*nsrch1f)-1
      obj%wa1(ix_ss:ie2)=obj%wa1(ix_sm:ie1)
      do iwin = 1,nwin
!
        kx_sm  = ix_sm + nsrch1f*(iwin-1)
!
        imax=lss*2+1
        do i3 = 1,nsrch1f
          nvals = i3*2-1
          if (nvals.gt.nsrch1f) nvals=(nsrch1f-i3+1)*2-1
          if (nvals.gt.imax) nvals=imax
          if (nvals.gt.1) then
            ib = kx_sm+i3-(nvals/2)-1
            ie = kx_sm+i3+(nvals/2)-1
            kx_ss = ix_ss + nsrch1f*(iwin-1) + i3-1
            obj%wa1(kx_ss)=sum(obj%wa1(ib:ie))/nvals
          end if
        end do
      enddo
!
!  ---------------------------------------------------
!
! ..... determine other attributes based on smoothing
!
      it2 = ix_temp+lwin
      do i2 = 1,nsrch1f
! ..... Loop on analysis windows
!
        jx_s0  = ix_s0 + nwin*(i2-1)
        jx_nn  = ix_nn + nwin*(i2-1)
        jx_0n  = ix_0n + nwin*lwin*(i2-1)
        do iwin = 1,nwin
!
          ib = obj%iwa1(ix_wb+iwin-1)
          ie = obj%iwa1(ix_we+iwin-1)
          lcwin=(ie-ib)+1
!
          kx_s0   = jx_s0 + iwin - 1
          kx_0n   = jx_0n + lwin*(iwin-1)
          kx_sx   = ix_sx + iwin - 1
          kx_beta = ix_beta + iwin - 1
          kx_dz   = ix_dz + iwin - 1
          kx_fold = ix_fold + iwin - 1
          kx_ss   = ix_ss + nsrch1f*(iwin-1) + i2 - 1
          kx_rms  = ix_rms + iwin - 1
          kx_ma   = ix_ma + iwin - 1
!
          if (obj%wa1(kx_ss).gt.obj%wa1(kx_sx)) then
            obj%wa1(kx_sx) = obj%wa1(kx_ss)
!
!            save beta value at maximum semblance value
            icw = obj%iwa1(ix_wc+iwin-1)
            obj%wa1(kx_beta)=obj%bwa1(icw,i2)
!            save delta z value at maximum semblance value
            obj%wa1(kx_dz)=obj%dzwa1(icw,i2)
!
!            save the fold at the current window
            obj%wa1(kx_fold)=obj%wa1(kx_s0)
!
!            set beta & delta z to nill if not a true peak
            if (speako) then
              if (i2.eq.1 .or. i2.eq.nsrch1f) then
                obj%wa1(kx_beta)=FNIL
                obj%wa1(kx_dz)=FNIL
              end if
            end if
!
!            Normalize the stack
            a1=obj%wa1(kx_s0)**snorm
            ie1=kx_0n+lcwin-1
            ie2=it2+lcwin-1
            obj%wa2(it2:ie2)=obj%wa1(kx_0n:ie1)/a1
!            determine the rms_amplitude
            ie1=it2+lcwin-1
            obj%wa1(kx_rms)=sqrt(dot_product(obj%wa2(it2:ie1), &
            obj%wa2(it2:ie1))/lcwin)
!
!            determine the max absolute amplitude
            ie1=it2+lcwin-1
            rmax=abs(maxval(obj%wa2(it2:ie1)))
            rmin=abs(minval(obj%wa2(it2:ie1)))
!
            if (rmin.gt.rmax) then
              obj%wa1(kx_ma)=rmin
            else
              obj%wa1(kx_ma)=rmax
            end if
          end if
        enddo
      enddo
!
!      Make sure the minimum fold requirement is satisfied
!
      do iwin = 1,nwin
        kx_beta = ix_beta + iwin - 1
        kx_fold = ix_fold + iwin - 1
        if (obj%wa1(kx_fold).lt.foldmin) then
           obj%wa1(kx_beta)=FNIL
           obj%wa1(kx_dz)=FNIL
        end if
      enddo
!
!  ---------------------------------------------------
!
! ..... determine semblance, beta, rms, and max amplitude traces
!
      jst  = ist + (lwin-inc)/2
!
      srs  = samp_int_in/1000.
!
      call abra_tdata(nwin,obj%iwa1(ix_wb:),obj%iwa1(ix_wc:),obj%iwa1(ix_we:), &
        obj%wa1(ix_beta:),obj%wa1(ix_dz:),obj%wa1(ix_sx:),obj%wa1(ix_rms:),    &
        obj%wa1(ix_ma:),                                                       &
        smb_min,max_samps,samp_int_in,jst,inc,obj%wa2(ix_tmt:),                &
        obj%wa2(ix_smt:),obj%wa2(ix_btr:),obj%wa2(ix_dztr:),obj%wa2(ix_rtr:),  &
        obj%wa2(ix_mat:))
!
!      determine Gamma values
!
      ist = jst
      iet = obj%iwa1(ix_we+nwin-1)
      do i1 = ist,iet
        beta=obj%wa2(ix_btr+i1-1)
        if (beta.eq.FNIL) then
          obj%wa2(ix_gtr+i1-1)=0.0
        else if (beta .lt. -1.0) then
          obj%wa2(ix_gtr+i1-1)=0.0
        else
          obj%wa2(ix_gtr+i1-1)=sqrt(obj%wa2(ix_btr+i1-1)+1.0)
        end if
      end do
!
!      determine delta v values
!
      if (idatatp.eq.2 .and. ivaflg.eq.1) then
        do i1 = 1,max_samps
          inilfg=0
!
          beta=obj%wa2(ix_btr+i1-1)
          if (beta.eq.FNIL) then
            velatr(i1)=FNIL
            inilfg=1
            vc=FNIL
          else

            !Btrue = Bmig + Bresid
            beta=4.0/((velatr(i1)/1000)**2)+beta

            if (beta.le.0.0) then
              velatr(i1)=FNIL
              inilfg=2
              vc=FNIL
            else
              !Vresid = Vtrue - Vmig
              velatr(i1)=((2.0/sqrt(beta))*1000.0)-velatr(i1)

              vc=(2.0/sqrt(beta))*1000.0
            end if
          end if
        end do
      end if
!
!*********************************************************************
      if (ist1.eq.0) ist1=1
      it=1
!      Set beta trace header information
      ino=ist1+it-1
      hdo(1:nhdrs, it)=hd(1:nhdrs, ist1)
      hdo(HDR_CURRENT_GROUP, it) = hd(HDR_CURRENT_GROUP, ino)
      hdo(HDR_CURRENT_CHANNEL, it) = it
      hdo(ittyp_hno, it) = ibetatr
      hdo(HDR_OFFSET, it) = 0
!      Set the beta trace
      ie1=ix_btr+max_samps-1
      tr(1:max_samps,it)=obj%wa2(ix_btr:ie1)
      it=it+1
!
!------------------------------------------------------------------
!   Save for debugging purposes
!
!       write out beta trace values
!     do i4=1,iet,50
!       time=(i4-1)*samp_int_in
!       write(6,9180) i4,time,tr(i4,it-1)
!9180   format(' beta_trc: ',i5,1x,2(f10.3,1x))
!     end do
!------------------------------------------------------------------
!      Set semblance trace header information
      ino=ist1+it-1
      hdo(1:nhdrs, it)=hd(1:nhdrs, ist1)
      hdo(HDR_CURRENT_GROUP, it) = hd(HDR_CURRENT_GROUP, ino)
      hdo(HDR_CURRENT_CHANNEL, it) = it
      hdo(ittyp_hno, it) = isigtr
      hdo(HDR_OFFSET, it) = 0
!      Set the semblance trace
      ie1=ix_smt+max_samps-1
      tr(1:max_samps,it)=obj%wa2(ix_smt:ie1)
      it=it+1
!
!      Set gamma trace header information
!      (if depth data used)
!
      if (idatatp.eq.1) then
        ino=ist1+it-1
        hdo(1:nhdrs, it)=hd(1:nhdrs, ist1)
        hdo(HDR_CURRENT_GROUP, it) = hd(HDR_CURRENT_GROUP, ino)
        hdo(HDR_CURRENT_CHANNEL, it) = it
        hdo(ittyp_hno, it) = igamatr
        hdo(HDR_OFFSET, it) = 0
!        Set the gamma trace
        ie1=ix_gtr+max_samps-1
        tr(1:max_samps,it)=obj%wa2(ix_gtr:ie1)
        it=it+1
      end if
!
!      Set rms amplitude trace header information
      ino=ist1+it-1
      hdo(1:nhdrs, it)=hd(1:nhdrs, ist1)
      hdo(HDR_CURRENT_GROUP, it) = hd(HDR_CURRENT_GROUP, ino)
      hdo(HDR_CURRENT_CHANNEL, it) = it
      hdo(ittyp_hno, it) = irmstr
      hdo(HDR_OFFSET, it) = 0
!      Set the rms amplitude trace
      ie1=ix_rtr+max_samps-1
      tr(1:max_samps,it)=obj%wa2(ix_rtr:ie1)
      it=it+1
!
!      Set max amplitude trace header information
      ino=ist1+it-1
      hdo(1:nhdrs, it)=hd(1:nhdrs, ist1)
      hdo(HDR_CURRENT_GROUP, it) = hd(HDR_CURRENT_GROUP, ino)
      hdo(HDR_CURRENT_CHANNEL, it) = it
      hdo(ittyp_hno, it) = imatr
      hdo(HDR_OFFSET, it) = 0
!      Set the max amplitude trace
      ie1=ix_mat+max_samps-1
      tr(1:max_samps,it)=obj%wa2(ix_mat:ie1)
      it=it+1
!
!      Set delta v trace header information
!      (if time data used)
!      
      if (idatatp.eq.2) then
        ino=ist1+it-1
        hdo(1:nhdrs, it)=hd(1:nhdrs, ist1)
        hdo(HDR_CURRENT_GROUP, it) = hd(HDR_CURRENT_GROUP, ino)
        hdo(HDR_CURRENT_CHANNEL, it) = it
        hdo(ittyp_hno, it) = idvtrc
        hdo(HDR_OFFSET, it) = 0
!        Set delta v trace
        tr(1:max_samps,it)=velatr(1:max_samps)
        it=it+1
      end if
!
!      Set delta z trace header information
!      (if depth data used)
!      
      if (idatatp.eq.1) then
        ino=ist1+it-1
        hdo(1:nhdrs, it)=hd(1:nhdrs, ist1)
        hdo(HDR_CURRENT_GROUP, it) = hd(HDR_CURRENT_GROUP, ino)
        hdo(HDR_CURRENT_CHANNEL, it) = it
        hdo(ittyp_hno, it) = idztr
        hdo(HDR_OFFSET, it) = 0
!        Set the delta z trace
        ie1=ix_dztr+max_samps-1
        tr(1:max_samps,it)=obj%wa2(ix_dztr:ie1)
        it=it+1
      end if
!
!      Set the seismic traces
      if (lstrc) then
        it2=1
        do i1 = 1,ntrc_gath_in
          data_trace_type_in = hd(ittyp_hno, i)
          itrcfg=0
          if (data_trace_type_in.eq.istrc .or. &
              data_trace_type_in.eq.idead) then
            itrcfg=1
          else if (data_trace_type_in.eq.iavtrc) then
            if (iavelo.eq.2) itrcfg=1
          else if (data_trace_type_in.eq.iivtrc) then
            if (iivelo.eq.2) itrcfg=1
          end if
!
          if (itrcfg.eq.1) then
             hdo(1:nhdrs, it)=hd(1:nhdrs, i1)
!
             ib=(it2-1)*max_samps+ix_st
             ie=ib+max_samps-1
             tr(1:max_samps,it)=rseis(ib:ie)
             it=it+1
             it2=it2+1
          end if
        enddo
      end if
!
!      Set the semblance functions
      if (lsemf) then
        do iwin = 1,nwin
          hdo(1:nhdrs, it)=hd(1:nhdrs, ist1)
          hdo(HDR_CURRENT_GROUP, it) = hd(HDR_CURRENT_GROUP, ist1)
          hdo(HDR_CURRENT_CHANNEL, it) = it
          hdo(ittyp_hno, it) = isftr
          hdo(HDR_OFFSET, it) = 0
!
!          set the center of the current window
          ic = obj%iwa1(ix_wc+iwin-1)
          tc=(ic-1)*samp_int_in
          hdo(HDR_SCRATCH_62, it) = tc
!
!          Set the semblance smooothing trace
          tr(1:max_samps,it)=0.0
          do i2 = 1,nsrch1f
            kx_ss = ix_ss + nsrch1f*(iwin-1) + i2 - 1
            tr(i2,it)=obj%wa1(kx_ss)
          enddo
          it=it+1
        enddo
      end if
!
      ntrc_gath_out = it-1
      ntr = ntrc_gath_out + nwin
!      Save headers
      ntr = ntrc_gath_out
      hd = hdo
!
      if ( jerror .ne. 0 )   then
         ierr = jerror
      end if

      end subroutine abra
!                                                  
!               subroutine abra_tdata
!                                                 
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!                                                  
      subroutine abra_tdata(nw,iwbs,iwcs,iwes,        &
                            beta,dz,sx,rms,rma,       &
                            smin,ns,sr,ist,inc,tmtrc, &
                            smtrc,bttrc,dztrc,rmstr,rmatr)
!
      implicit none
      integer          ,intent(in)    :: nw       ! arguments
      integer          ,intent(in)    :: iwbs(:)  ! arguments (len=nw)
      integer          ,intent(in)    :: iwcs(:)  ! arguments (len=nw)
      integer          ,intent(in)    :: iwes(:)  ! arguments (len=nw)
      real             ,intent(in)    :: beta(:)  ! arguments (len=nw)
      real             ,intent(in)    :: dz(:)    ! arguments (len=nw)
      real             ,intent(in)    :: sx(:)    ! arguments (len=nw)
      real             ,intent(in)    :: rms(:)   ! arguments (len=nw)
      real             ,intent(in)    :: rma(:)   ! arguments (len=nw)
      real             ,intent(in)    :: smin     ! arguments
      integer          ,intent(in)    :: ns       ! arguments
      real             ,intent(in)    :: sr       ! arguments
      integer          ,intent(in)    :: ist      ! arguments
      integer          ,intent(in)    :: inc      ! arguments
      real             ,intent(out)   :: tmtrc(:) ! arguments (len=ns)
      real             ,intent(out)   :: smtrc(:) ! arguments (len=ns)
      real             ,intent(out)   :: bttrc(:) ! arguments (len=ns)
      real             ,intent(out)   :: dztrc(:) ! arguments (len=ns)
      real             ,intent(out)   :: rmstr(:) ! arguments (len=ns)
      real             ,intent(out)   :: rmatr(:) ! arguments (len=ns)
!
!                                                 
!            Determine semblance and beta trace data
!
!                Arguments (input)
!
!             -   nw     -  Number of windows 
!             -   iwbs   -  Window Beginning
!             -   iwcs   -  Window Centers
!             -   iwes   -  Window Ends
!             -   beta   -  Array of betas at maximum semblance
!             -   dz     -  Array of delta z values at maximum semblance
!             -   sx     -  Semblance maximum for each window
!             -   rms    -  Array of rms values at maximum semblance
!             -   rma    -  Array of maximum amplitudes at maximum semblance
!             -   smin   -  Minimum semblance
!             -   ns     -  Number of samples in traces
!             -   sr     -  Sample rate
!             -   ist    -  Start sample for analysis
!             -   inc    -  Sample increment for analysis
!
!                Arguments (output)
!
!             -   tmtrc  -  Time trace
!             -   smtrc  -  Semblance trace
!             -   bttrc  -  Beta trace
!             -   dztrc  -  Delta Z trace
!             -   rmstr  -  Rms trace
!             -   rmatr  -  Maximum amplitude trace
!
!            tmtrc(ns)      : Time trace (ns)
!            smtrc(ns)      : Semblance trace (ns)
!            bttrc(ns)      : Beta trace (ns)
!            dztrc(ns)      : Delta Z trace (ns)
!            rmstr(ns)      : Rms trace (ns)
!            rmatr(ns)      : Maximum amplitude trace (ns)
!
! --------------------------------------------------
!
      integer ::   i,i1,i2,ia,iaatt,ibatt,idzatt,imid,imidl,imidn
      integer ::   inilfg,intfg,iratt,isatt,iw,iwl,iwn,natts
!                                                             
      real ::      aval,time,tmid,tmidl,tmidn
!
      real :: ap(5), al(5), an(5)
!
!      initialize output traces to zero
!      (Beta trace & delta Z trace to NILL)
!
      tmtrc(1:ns)=0.0
      smtrc(1:ns)=0.0
      bttrc(1:ns)=FNIL
      dztrc(1:ns)=FNIL
      rmstr(1:ns)=0.0
      rmatr(1:ns)=0.0
!
!      initialize
!
      natts=5
      isatt=1
      ibatt=2
      iratt=3
      iaatt=4
      idzatt=5
!
!      For each window, put in D:
!          semblance, max semblance, beta at max.
!
      intfg=0
      do iw = 1,nw
        i1 = iwbs(iw)
        i2 = iwes(iw)
        imid=iwcs(iw)
        tmid=(imid-1)*sr
!
!         make sure values are between window centers
!
        if (iw.gt.1) then
          if (i1.le.iwcs(iw-1)) i1=iwcs(iw-1)+1
        end if
        if (iw.lt.nw) then
          if (i2.ge.iwcs(iw+1)) i2=iwcs(iw+1)-1
        end if
!
        ap(1) = sx(iw)
        ap(2) = beta(iw)
        ap(3) = rms(iw)
        ap(4) = rma(iw)
        ap(5) = dz(iw)
!
        if (iw.gt.1) then
          iwl=iw-1
          imidl=iwcs(iwl)
          tmidl=(imidl-1)*sr
          al(1)=sx(iwl)
          al(2)=beta(iwl)
          al(3)=rms(iwl)
          al(4)=rma(iwl)
          al(5)=dz(iwl)
        end if
!
        if (iw.lt.nw) then
          iwn=iw+1
          imidn=iwcs(iwn)
          tmidn=(imidn-1)*sr
          an(1)=sx(iwn)
          an(2)=beta(iwn)
          an(3)=rms(iwn)
          an(4)=rma(iwn)
          an(5)=dz(iwn)
        end if
!
        if (iw.eq.1 .or. iw.eq.nw) then
          do ia = 1,natts
            al(ia)=ap(ia)
            an(ia)=ap(ia)
          end do
          tmidl=(i1-1)*sr
          tmidn=(i2-1)*sr
        end if
!
        do i = i1,i2
          time=(i-1)*sr
          tmtrc(i)=time
!           Interpolate each of attributes
          do ia = 1,natts
            if (i.eq.imid) then
!
!              Determine if beta attribute value is a nill
!              (Set all beta & delta z values to nill &
!               all other values to zero if a Beta value is nill)
              inilfg=0
              if (ap(ibatt).eq.FNIL) then
                inilfg=1
              else if (ap(idzatt).eq.FNIL) then
                inilfg=1
              end if
!
              if (inilfg.eq.1) then
                if (ia.eq.ibatt) then
                  aval=FNIL
                else if (ia.eq.idzatt) then
                  aval=FNIL
                else
                  aval=0.0
                end if
              else
                aval=ap(ia)
              end if
!
            else if (i.lt.imid) then
!
!              Determine if beta attribute value is a nill
!              (Set all beta values & delta z vals to nill &
!               all other values to zero if a Beta value is nill)
              inilfg=0
              if (al(ibatt).eq.FNIL) then
                inilfg=1
              else if (al(idzatt).eq.FNIL) then
                inilfg=1
              end if
!
              if (inilfg.eq.1) then
                if (ia.eq.ibatt) then
                  aval=FNIL
                else if (ia.eq.idzatt) then
                  aval=FNIL
                else
                  aval=0.0
                end if
              else
                if (ap(ibatt).eq.FNIL) then
                  inilfg=1
                else if (ap(idzatt).eq.FNIL) then
                  inilfg=1
                end if
              end if
!
              if (inilfg.eq.1) then
                if (ia.eq.ibatt) then
                  aval=FNIL
                else if (ia.eq.idzatt) then
                  aval=FNIL
                else
                  aval=0.0
                end if
!
!              Interpolate an attribute value that is before the mid point
              else
                if (al(ia).le.ap(ia)) then
                  aval=al(ia)+(time-tmidl)/(tmid-tmidl)*(ap(ia)-al(ia))
                else
                  aval=al(ia)-(time-tmidl)/(tmid-tmidl)*(al(ia)-ap(ia))
                end if
              end if

            else if (i.gt.imid) then
!
!              Determine if beta attribute value is a nill
!              (Set all beta values to nill & all other 
!               values to zero if a beta value is nill)
              inilfg=0
              if (an(ibatt).eq.FNIL) then
                inilfg=1
              else if (an(idzatt).eq.FNIL) then
                inilfg=1
              end if
!
              if (inilfg.ne.1) then
                if (ia.eq.ibatt) then
                  aval=FNIL
                else if (ia.eq.idzatt) then
                  aval=FNIL
                else
                  aval=0.0
                end if
              else
                if (ap(ibatt).eq.FNIL) then
                  inilfg=1
                else if (ap(idzatt).eq.FNIL) then
                  inilfg=1
                end if
              end if
!
              if (inilfg.ne.1) then
                if (ia.eq.ibatt) then
                  aval=FNIL
                else if (ia.eq.idzatt) then
                  aval=FNIL
                else
                  aval=0.0
                end if
!
!              interpolate an attribute value that is after the mid point
              else
                if (ap(ia).le.an(ia)) then
                  aval=ap(ia)+(time-tmid)/(tmidn-tmid)*(an(ia)-ap(ia))
                else
                  aval=ap(ia)-(time-tmid)/(tmidn-tmid)*(ap(ia)-an(ia))
                end if
              end if
            end if
!
!            Set the output trace value
!
            if (ia.eq.isatt) then
              smtrc(i)=aval
            else if (ia.eq.ibatt) then
              bttrc(i)=aval
            else if (ia.eq.iratt) then
              rmstr(i)=aval
            else if (ia.eq.iaatt) then
              rmatr(i)=aval
            else if (ia.eq.idzatt) then
              dztrc(i)=aval
            end if
          end do
            
!           Make sure semblance value is greater than the minimum
          if (smtrc(i).lt.smin) then
            smtrc(i)=0.0
            bttrc(i)=FNIL
            rmstr(i)=0.0
            rmatr(i)=0.0
            dztrc(i)=FNIL
          end if
        end do
      enddo
!                                                             
      return                                                  
      end subroutine abra_tdata

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine abra_wrapup (obj)
      type(abra_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.

      end subroutine abra_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module abra_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

