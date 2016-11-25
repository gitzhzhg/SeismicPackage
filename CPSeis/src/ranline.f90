!<CPS_v1 type="PROCESS"/>
!!--------------------------- ranline.f90 ---------------------------------!!
!!--------------------------- ranline.f90 ---------------------------------!!
!!--------------------------- ranline.f90 ---------------------------------!!


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
! Name       : RANLINE                    (random line)
! Category   : miscellaneous
! Written    : 1989-08-17   by: Tom Stoeckley
! Revised    : 2006-12-04   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Trace interpolation from random 3D locations to a random line.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This process can be used to select interpolated random lines out of 3-D
! data sets, with optional X-Y dip search.  This process generates diagonal
! lines (or connected or disconnected straight or bent line segments), with
! evenly spaced traces along the lines.  Each output trace is interpolated
! from neighboring traces within a bin rectangle centered at the output trace
! location.  Dip searches are performed in the (X,Y) directions.
!
! This process is intended to operate on stacked or migrated data.  Although
! such data is usually arranged on a regular grid, this process has no such
! requirement.
!
! This process uses the SDIPUTIL primitive to do the dip search and
! calculate the output traces.
!
! This process contains a subset of the capabilities of the old TERP process.
!
!-------------------------------------------------------------------------------
!                         DETAILS OF OPERATION
!
! 1. Within each window, the MAX_SEARCH nearest traces within distances
!    SEARCH_X and SEARCH_Y of the output trace location are used to
!    perform a semblance dip search to determine which dip has the maximum
!    semblance.  Up to a maximum of (2*DIP_MAX_X + 1) times (2*DIP_MAX_Y + 1)
!    dips are tested.  The dip with the maximum semblance is referred to as
!    the dominant dip.  Each window length is WIN_LEN seconds long, and the
!    windows move down the trace one sample at a time.
!
! 2. For each window, the MAX_INTERP nearest traces within distances
!    SEARCH_X and SEARCH_Y of the output trace location are weighted
!    as described in ADVICE FOR USERS below and summed along the dominant
!    dip to form an interpolated trace.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
!  1. Parameter FRACMIX specifies the fractional mix performed during
!     interpolation.
!
!     FRACMIX=0 means a minimum amount of mixing is done, with all weights
!     inversely proportional to "distance" from the trace output location,
!     defined as SQRT([DX/SEARCH_X]**2 + [DY/SEARCH_Y]**2).  This is
!     appropriate for straight interpolation.
!
!     In the above definition for "distance", (DX,DY) is the (X,Y) distance
!     of an individual trace from the trace output location, and
!     (SEARCH_X,SEARCH_Y) is the maximum (X,Y) distance a trace can
!     be from the trace output location to be used for dip search,
!     interpolation, or mixing.
!
!     FRACMIX=1.414 or larger means all traces used for interpolation
!     will be weighted equally, regardless of their distances from the
!     interpolated trace output location.  This would be a strong mix
!     (equal-weight stack along dips) rather than an interpolation.
!
!     For example, FRACMIX=0.3 means that all traces within "distance"
!     0.3 (as defined above) will have a weight corresponding to
!     distance 0.3 (rather than a larger weight for shorter distance).
!     This would be appropriate if you want to do some mixing while
!     interpolating, so that an individual trace that may happen to
!     lie at the trace output location will not get all the weight.
!
!  2. Traces will contribute to a bin (i.e. used for dip search,
!     interpolation, or mixing) only if they fall within distance
!     (SEARCH_X,SEARCH_Y) from the trace output location.  A trace can
!     contribute to more than one bin (i.e. the bins can overlap).
!     However, MAX_SEARCH and MAX_INTERP limit the number of traces that
!     can contribute to a bin by deleting excess traces that are
!     the farthest from the trace output location.
!
!  3. Arrays X_COORD and Y_COORD specify the (X,Y) coordinates of
!     interpolated random lines through 3-D data.  The random lines are
!     connected or separate line segments.  All units are the
!     same as the units in HDR_X and HDR_Y.  if (Xi,Yi)=(0,0), that
!     point is not used, and the previous and following line segments
!     are not connected.
!
!     For example, given  X_COORD = X1,X2,X3,0,X5,X6
!                    and  Y_COORD = Y1,Y2,Y3,0,Y5,Y6 :
!        (X1,Y1) = beginning of first straight line segment.
!        (X2,Y2) = end of first line segment and beginning of second.
!        (X3,Y3) = end of second line segment.
!        (X5,Y5) = beginning of third line segment.
!        (X6,Y6) = end of third line segment.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! This process is a multiple-trace process.
! All traces are input before any traces are output.
! Traces can be input in any order.
! Traces can be input one or more at a time. 
! Input traces do not have to conform to any regular pattern in X and Y.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process outputs one trace at a time.
!
! One interpolated or mixed trace is output at each requested location
! along the specified random lines.  If no traces are available to contribute
! to the output trace at specifiic location, a dead trace is output.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       set to 1
! GATHERED  whether traces are a legitimate gather  set to false
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#          Description                Action taken
! ----          -----------                ------------
! 2             Head mute index            used but not changed
! 3             Current gather             reset
! 4             Trace in current gather    set to 1
! 25            LAV                        reset
! 64            Tail mute index            used but not changed
! HDR_X         X coordinate               used and reset
! HDR_Y         Y coordinate               used and reset
! 58,59,60      Scratch                    used
!
! Header words for the output trace will be taken from the nearest input
! trace which contributes to the output trace, with the exceptions specified
! above.  If a dead trace is output, most header words will be zero.
!
! Warning: If HDR_X and HDR_Y are grid header words, the survey header words
! will no longer correspond.  If HDR_X and HDR_Y are survey header words,
! the grid header words will no longer correspond.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
! 14. 2006-12-04  Stoeckley  Replace pc_register_tab_group w HelpSection line.
! 13. 2006-08-24  D. Glover  Added NULLIFY statements for Intel compiler.
! 12. 2006-06-20  Stoeckley  Add pc_register_tab_group for SeisSpace.
! 11. 2002-09-11  Stoeckley  Add help for PLOT pushbutton; also allow optional
!                             reuse of the same gnuplot for subsequent plots
!                             rather than closing the old and and opening a
!                             new gnuplot; move most of the gnuplot logic
!                             to the new primitive GPLOT.
! 10. 2002-09-06  Stoeckley  Add gnuplot capability to front end.
!  9. 2002-08-26  Stoeckley  Improve error printouts; add warnings for
!                             parameters which appear to be incorrect.
!  8. 2002-08-05  Stoeckley  Converted from the TERP process in the old system;
!                             previous history refers to the TERP process.
!  7. 1999-02-22  Goodger    Begin using the fortran90 compiler.
!  6. 1993-09-20  Stoeckley  Change FILE to type integer to match IBLK.
!  5. 1992-02-26  Peterson   New option to input SEGX and SEGY values from
!                             a file.
!  4. 1992-01-08  Peterson   Transfer FEET(3) array through TERPBEG and
!                             ENTRY TERPCC2 calling sequence for METHOD=
!                             CLOD.  New cft couldn't find FEET(3).
!  3. 1989-06-02  Stoeckley  Change TERPFAST to version which will not
!                             abort due to faulty compiler optimization,
!                             and add alternate error return to a few
!                             routines.
!  2. 1989-06-21  Stoeckley  Change parameter GATHER to parameter OUTPUT,
!                             with new option OUTPUT=SELECT.
!  1. 1989-08-17  Stoeckley  Fix bug in parameter FRACMIX, and fix an
!                             incorrect print statement.
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
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.       
! NSTORE          >0       amount of permanent memory needed.      
! IFTD           true      whether this process frees tape drives. 
! NDISK           >0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
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
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES              
!
! ranline_update calls:
!
!   sdiputil_update
!   ranline_help1
!   sdiputil_prepare
!
! ranline calls:
!
!   ranline_input
!     ranline_beg
!       ranline_help2
!         ranline_test
!
!   ranline_complete
!
!   ranline_output
!     ranline_out
!       ranline_help3
!       sdiputil_solve
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!
!<NS RANLINE Process/NC=80>
!
!                   3D trace interpolation to random lines
!
! NUM_TRACES=`IIIIIII [/L]Max # input traces to use altogether.
! WIN_LEN~~~=`FFFFFFF [/L]Semblance window length for dip search (seconds).
! MAX_SEARCH=`III     [/L]Maximum # traces to use for dip search.
! MAX_INTERP=`III     [/L]Maximum # traces to interpolate or mix.
! FRACMIX~~~=`FFFFFFF [/L]Fractional mix performed (0 to 1.414).
! FEET_OUT~~=`FFFFFFF [/L]Output trace spacing along random lines (feet or meters).
!
! `------------------- `-------------------
!  HDR_X~~~~=`II        HDR_Y~~~~=`II       [/L]Header word.
!  DIP_MAX_X=`FFFFFFF   DIP_MAX_Y=`FFFFFFF  [/L]Maximum dip (ms / unit hwd value).
!  dip_inc_x=`XXXXXXX   dip_inc_y=`XXXXXXX  [/L]Dip increment (ms / unit hwd value).
!  nxdips~~~=`XXXXXXX   nydips~~~=`XXXXXXX  [/L]Total number of dips.
!  SEARCH_X =`FFFFFFF   SEARCH_Y =`FFFFFFF  [/L]Maximum search distance (hwd units).
!  FEET_X~~~=`FFFFFFF   FEET_Y~~~=`FFFFFFF  [/L]Unit size of hwd value (feet or meters).
! `------------------- `-------------------
!
! [coords_info]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!<include sdiputil.f90>
!
!<NS Random Line Coordinates/NC=80>
!
! Select PATHNAME[PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                 [pathname_info]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!                  X_COORD    Y_COORD     (in units of header word values)
!                  `FFFFFFFFFF`FFFFFFFFFF
!                  `FFFFFFFFFF`FFFFFFFFFF
!                  `FFFFFFFFFF`FFFFFFFFFF
!                  `FFFFFFFFFF`FFFFFFFFFF
!                  `FFFFFFFFFF`FFFFFFFFFF
!                  `FFFFFFFFFF`FFFFFFFFFF             Plot`P
!                  `FFFFFFFFFF`FFFFFFFFFF
!                  `FFFFFFFFFF`FFFFFFFFFF            Replot`P
!                  `FFFFFFFFFF`FFFFFFFFFF
!                  `FFFFFFFFFF`FFFFFFFFFF
!                  `FFFFFFFFFF`FFFFFFFFFF
!                  `FFFFFFFFFF`FFFFFFFFFF
!
!<PARMS COORDS_INFO[/ML=128/XST]>
!<PARMS PATHNAME[/ML=140/XST]>
!<PARMS pathname_info[/EN]>
!<PARMS X_COORD_ARRAYSET[/XST/YST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!        tabgroup = Random Line Coordinates
!
!<Help KEYWORD="plot">
!<Tip> Press this button to plot the coordinates. </Tip>
!
! This button will pop up a gnuplot plot of the (X_COORD,Y_COORD) coordinates
! of the random lines.  To update this plot after changing these coordinates,
! you can simply press the REPLOT button.  To delete this plot and open up
! a new plot, simply press this PLOT button again.  To pop down the plot,
! simply choose Close on the window manager pulldown menu (upper left corner
! of the Gnuplot popup).
!</Help>
!
!
!<Help KEYWORD="replot">
!<Tip> Press this button to plot the coordinates. </Tip>
!
! This button will replot the (X_COORD,Y_COORD) coordinates using the same
! gnuplot plot previously used (or a new one if none is currently open).
! To update this plot after changing these coordinates, you can simply press
! this REPLOT button again.  To delete this plot and open up a new plot,
! simply press the PLOT button.  To pop down the plot, simply choose Close
! on the window manager pulldown (upper left corner of the Gnuplot popup).
!</Help>
!
!        tabgroup = Main Tab
!
!<Help KEYWORD="nxdips" TYPE="DISPLAY_ONLY">
!<Tip> Total number of dips to be tested in X direction. </Tip>
!</Help>
!
!
!<Help KEYWORD="nydips" TYPE="DISPLAY_ONLY">
!<Tip> Total number of dips to be tested in Y direction. </Tip>
!</Help>
!
!
!<Help KEYWORD="NUM_TRACES">
!<Tip> Total number of input traces to be used. </Tip>
! Default = 99999
! Allowed = int > 0
!
! This is the total number of input traces that will be used for generating
! the output traces.  It usually will be much smaller than the number of
! input traces.  This number can be approximate since it is used only for
! reserving disk space.
!</Help>
!
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length of the time window, in sec, for the semblance calculation. </Tip>
! Default = 0.1
! Allowed = real > 0.0
!
! WIN_LEN is used for dip search calculations prior to generating mixed traces.
!</Help>
!
!
!<Help KEYWORD="DIP_MAX_X">
!<Tip> Maximum dip in X direction to use in the dip search calculation. </Tip>
! Default = 10.0
! Allowed = real >= 0.0
!
! Dips are in ms/trace in the X direction.
!
! A total of (2*DIP_X/DIP_INC_X + 1) dips are tested in the X direction,
! from -DIP_MAX_X to +DIP_MAX_X.
!
! Computer time is roughly proportional to the number of dips tested in the
! X direction times the number of dips tested in the Y direction.
!</Help>
!
!
!<Help KEYWORD="DIP_MAX_Y">
!<Tip> Maximum dip in Y direction to use in the dip search calculation. </Tip>
! Default = 10.0
! Allowed = real >= 0.0
!
! Dips are in ms/trace in the Y direction.
!
! A total of (2*DIP_X/DIP_INC_Y + 1) dips are tested in the Y direction,
! from -DIP_MAX_Y to +DIP_MAX_Y.
!
! Computer time is roughly proportional to the number of dips tested in the
! X direction times the number of dips tested in the Y direction.
!</Help>
!
!
!<Help KEYWORD="dip_inc_x" TYPE="DISPLAY_ONLY">
!<Tip> Dip increment in X direction to use in the dip search calculation. </Tip>
!
! Dip increments are in ms/trace in the X direction.
!
! A total of (2*DIP_X/DIP_INC_X + 1) dips are tested in the X direction,
! from -DIP_MAX_X to +DIP_MAX_X.
!
! Computer time is roughly proportional to the number of dips tested in the
! X direction times the number of dips tested in the Y direction.
!
! This value is calculated from the Nyquist frequency and the SEARCH_X
! parameter to keep from aliasing the dip search on the farthest traces.
!</Help>
!
!
!<Help KEYWORD="dip_inc_y" TYPE="DISPLAY_ONLY">
!<Tip> Dip increment in Y direction to use in the dip search calculation. </Tip>
!
! Dip increments are in ms/trace in the Y direction.
!
! A total of (2*DIP_X/DIP_INC_Y + 1) dips are tested in the Y direction,
! from -DIP_MAX_Y to +DIP_MAX_Y.
!
! Computer time is roughly proportional to the number of dips tested in the
! X direction times the number of dips tested in the Y direction.
!
! This value is calculated from the Nyquist frequency and the SEARCH_Y
! parameter to keep from aliasing the dip search on the farthest traces.
!</Help>
!
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word containing the X coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word containing the Y coordinate. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!
!<Help KEYWORD="MAX_SEARCH">
!<Tip> Maximum number of traces to use for dip search. </Tip>
! Default = 8
! Allowed = int >= MAX_INTERP
!
! Maximum number of traces to use for dip searching for a given output
! trace.  If DIP_MAX_X and DIP_MAX_Y are both zero, no dip search and
! adjustment will be done.
!</Help>
!
!
!<Help KEYWORD="MAX_INTERP">
!<Tip> Maximum number of traces to use for interpolating. </Tip>
! Default = 4
! Allowed = int >= 1
!
! Maximum number of traces to use to interpolate a given output trace.
! If MAX_INTERP = 1, the nearest trace to the trace output location will
! be used.
!</Help>
!
!
!<Help KEYWORD="FRACMIX">
!<Tip> Fractional mix performed during interpolation. </Tip>
! Default = 0.0
! Allowed = real 0.0 to 1.414
!
! The value 0 means a minimum amount of mixing is done, with all weights
! inversely proportional to distance from the trace output location,
! defined as SQRT([DX/SEARCH_X]**2+[DY/SEARCH_Y]**2).  This is
! appropriate for straight interpolation.
!
! The value 1.414 or larger means all traces used for interpolation will
! be weighted equally, regardless of their distances from the interpolated
! trace output location.  This would be a strong mix (equal-weight stack
! along dips) rather than an interpolation.
!
! For example, FRACMIX=0.3 means that all traces within distance 0.3 will
! have a weight corresponding to distance 0.3 (rather than a larger weight).
! This would be appropriate if you want to do some mixing while
! interpolating, so that an individual trace that may happen to lie at the
! trace output location will not get all the weight.
!</Help>
!
!
!<Help KEYWORD="SEARCH_X">
!<Tip> Maximum X search distance of useable trace from output location. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
!
! This is in units of the X header word values.
!</Help>
!
!
!<Help KEYWORD="SEARCH_Y">
!<Tip> Maximum Y search distance of useable trace from output location. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
!
! This is in units of the Y header word values.
!</Help>
!
!
!<Help KEYWORD="FEET_X">
!<Tip> Unit size of the X header word values in feet or meters. </Tip>
! Default = nil
! Allowed = real > 0.0
!</Help>
!
!
!<Help KEYWORD="FEET_Y">
!<Tip> Unit size of the Y header word values in feet or meters. </Tip>
! Default = nil
! Allowed = real > 0.0
!</Help>
!
!
!<Help KEYWORD="FEET_OUT">
!<Tip> Output trace spacing along random lines in feet or meters. </Tip>
! Default = nil
! Allowed = real > 0.0
!</Help>
!
!
!<Help KEYWORD="COORDS_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of the specified random line coordinates. </Tip>
!
! This information summarizes the status of the random line coordinates
! whech are specified on the "Random Line Coordinates" screen.  That screen
! should be visited to insure that the specified coordinates are correct,
! or that the status of the file which specifies the coordinates is acceptable.
!</Help>
!
!        tabgroup = Random Line Coordinates
!
!<Help KEYWORD="PATHNAME_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of PATHNAME. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATHNAME">
!<Tip> Choose PATHNAME using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME">
!<Tip> Name of file containing X and Y coordinates along random lines. </Tip>
! Default = NONE
! Allowed = valid input file
!
! If this file is specified, X and Y coordinates will be read from the
! file instead of using the values in arrays X_COORD and Y_COORD.
!
! This file must be a CPS self defining file (or a foreign file) containing
! at least two columns of numbers, with X coordinates in the first column
! and Y coordinates in the second column, residing in the first section
! on the file.
!</Help>
!
!
!<Help KEYWORD="X_COORD">
!<Tip> List of X coordinates along random lines. </Tip>
! Default = none
! Allowed = real
!
! X_COORD and Y_COORD are the X and Y coordinates of interpolated random
! lines through 3-D data.  The random lines are connected or separate line
! segments.  All units are the same as the units in HDR_X and HDR_Y.
! If (Xi,Yi) = (0,0), that point is not used, and the previous and following
! line segments are not connected.
!
! For example, given  X_COORD = X1,X2,X3,0,X5,X6
!                and  Y_COORD = Y1,Y2,Y3,0,Y5,Y6 :
!         (X1,Y1) = beginning of first straight line segment.
!         (X2,Y2) = end of first line segment and beginning of second.
!         (X3,Y3) = end of second line segment.
!         (X5,Y5) = beginning of third line segment.
!         (X6,Y6) = end of third line segment.
!</Help>
!
!
!<Help KEYWORD="Y_COORD">
!<Tip> List of Y coordinates along random lines. </Tip>
! Default = none
! Allowed = real
!
! X_COORD and Y_COORD are the X and Y coordinates of interpolated random
! lines through 3-D data.  The random lines are connected or separate line
! segments.  All units are the same as the units in HDR_X and HDR_Y.
! If (Xi,Yi) = (0,0), that point is not used, and the previous and following
! line segments are not connected.
!
! For example, given  X_COORD = X1,X2,X3,0,X5,X6
!                and  Y_COORD = Y1,Y2,Y3,0,Y5,Y6 :
!         (X1,Y1) = beginning of first straight line segment.
!         (X2,Y2) = end of first line segment and beginning of second.
!         (X3,Y3) = end of second line segment.
!         (X5,Y5) = beginning of third line segment.
!         (X6,Y6) = end of third line segment.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module ranline_module
      use pc_module
      use named_constants_module
      use mem_module
      use mth_module
      use pathchoose_module
      use pathcheck_module
      use sdiputil_module
      use temptfile_module
      use triplesort_module
      use triplemerge_module
      use floatio_module
      use string_module
      use gplot_module
      implicit none
      private
      public :: ranline_create
      public :: ranline_initialize
      public :: ranline_update
      public :: ranline_delete
      public :: ranline    
      public :: ranline_wrapup

      character(len=100),public,save :: RANLINE_IDENT = &
'$Id: ranline.f90,v 1.14 2006/12/04 13:29:55 Stoeckley prod sps $'


!!------------------------ private structures ------------------------------!!
!!------------------------ private structures ------------------------------!!
!!------------------------ private structures ------------------------------!!


      type,private :: info_struct              
        private
        integer :: dead
        integer :: live
        integer :: xlo
        integer :: xup
        integer :: ylo
        integer :: yup
        integer :: num
        integer :: numlook
        integer :: numterp
        real    :: lastx                  ! step tests.
        real    :: lasty                  ! step tests.
        real    :: minstep                ! step tests.
        real    :: maxstep                ! step tests.
        real    :: sumstep                ! step tests.
        integer :: numstep                ! step tests.
        integer :: numbad                 ! step tests.
        integer :: iseg                   ! step tests.
        integer :: indx                   ! step tests.
      end type info_struct



      type,private :: segment_struct              
        private
        real    :: xlo
        real    :: ylo
        real    :: xstep
        real    :: ystep
        integer :: number
        logical :: newstart
      end type segment_struct


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: ranline_struct              
 
        private
        logical                          :: skip_wrapup        ! wrapup flag
        integer                          :: nwih,ndpt          ! globals  
        real                             :: tstrt,dt           ! globals

        character(len=28)                :: opt_output         ! not in gui
        real                             :: pwr_edge           ! not in gui
        real                             :: pwr_semb           ! not in gui
        logical                          :: opt_semb           ! not in gui
        real                             :: fctr_mix           ! not in gui
        character(len=8)                 :: opt_taper          ! not in gui
        integer                          :: win_nsamp          ! not in gui
        logical                          :: quick_dip_weights  ! not in gui
        logical                          :: quick_dip_search   ! not in gui

        integer                          :: hdr_x              ! was ihdr(1)
        integer                          :: hdr_y              ! was ihdr(2)
        integer                          :: max_search         ! was maxlook
        integer                          :: max_interp         ! was maxterp
        real                             :: win_len            ! was tslc
        real                             :: fracmix            ! unchanged
        real                             :: dip_max_x          ! was dip(1)
        real                             :: dip_max_y          ! was dip(2)
        real                             :: search_x           ! was dmax(1)
        real                             :: search_y           ! was dmax(2)
        real                             :: feet_x             ! was feet(1)
        real                             :: feet_y             ! was feet(2)
        real                             :: feet_out           ! was feet(3)
        integer                          :: ncoord             ! was nseg
        real            ,pointer         :: x_coord(:)         ! was segx
        real            ,pointer         :: y_coord(:)         ! was segy
        integer                          :: num_traces         ! was ntot
        character(len=FILENAME_LENGTH)   :: pathname           ! was file

        type(segment_struct),pointer     :: segment(:)         ! dependent
        integer                          :: kount              ! dependent
        integer                          :: key                ! dependent
        integer                          :: nseg               ! dependent
        integer                          :: nbin               ! dependent
        integer                          :: kkk                ! dependent
        integer                          :: izero              ! dependent
        integer                          :: index              ! dependent
        type(info_struct)                :: info               ! dependent
        integer                          :: ibin               ! dependent
        integer                          :: sequence           ! dependent

        type(pathchoose_struct) ,pointer :: pathchoose         ! dependent
        type(sdiputil_struct)   ,pointer :: sdiputil           ! dependent
        type(temptfile_struct)  ,pointer :: temptfile          ! dependent
        type(triplemerge_struct),pointer :: triplemerge        ! dependent
        type(gplot_struct)      ,pointer :: gplot              ! dependent

      end type ranline_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                     ,save :: lun      ! unit number for printing.
      type(ranline_struct),pointer,save :: object   ! needed for traps.

      integer,parameter :: SCRATCH_NSW = HDR_SCRATCH_60
      real   ,parameter :: DISTFACTOR  = 100000.0
      integer,parameter :: NPR         = 6           ! normal optional print.
  !!  integer,parameter :: NPR         = 100         ! debug optional print.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine ranline_create (obj)
      type(ranline_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%x_coord)  
      nullify (obj%y_coord) 
      nullify (obj%segment) 
      nullify (obj%pathchoose) ! jpa
      nullify (obj%sdiputil) ! jpa
      nullify (obj%temptfile)
      nullify (obj%triplemerge)
      nullify (obj%gplot)


      call pathchoose_create  (obj%pathchoose, 'pathname', 'ranline')
      call sdiputil_create    (obj%sdiputil)
      call ranline_initialize (obj)
      end subroutine ranline_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine ranline_delete (obj)
      type(ranline_struct),pointer :: obj       ! arguments

      call ranline_wrapup    (obj)
      call pathchoose_delete (obj%pathchoose)
      call sdiputil_delete   (obj%sdiputil)
      call temptfile_close   (obj%temptfile)
      call triplemerge_close (obj%triplemerge)
      call gplot_delete      (obj%gplot)

      call mem_free (obj%x_coord)  
      call mem_free (obj%y_coord) 

      if (associated(obj%segment)) deallocate (obj%segment)

      deallocate(obj)
      end subroutine ranline_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine ranline_initialize (obj)
      type(ranline_struct),intent(inout) :: obj       ! arguments

      lun = pc_get_lun()

      obj%opt_output         = 'MIXED TRACES'    ! not in gui
      obj%pwr_edge           = 2.0               ! not in gui
      obj%pwr_semb           = 0.5               ! not in gui
      obj%opt_semb           = .false.           ! not in gui
      obj%fctr_mix           = 0.5               ! not in gui
      obj%opt_taper          = 'NONE'            ! not in gui
      obj%win_nsamp          = 5                 ! not in gui
      obj%quick_dip_weights  = .true.            ! not in gui
      obj%quick_dip_search   = .true.            ! not in gui
 
      obj%hdr_x              = 7
      obj%hdr_y              = 8
      obj%max_search         = 8
      obj%max_interp         = 4
      obj%win_len            = 0.1
      obj%fracmix            = 0.0
      obj%dip_max_x          = 0.0
      obj%dip_max_y          = 0.0
      obj%search_x           = 0.0
      obj%search_y           = 0.0
      obj%feet_x             = FNIL
      obj%feet_y             = FNIL
      obj%feet_out           = FNIL
      obj%ncoord             = 0
      obj%num_traces         = 99999
      obj%pathname           = PATHCHECK_EMPTY

      call sdiputil_initialize (obj%sdiputil)
      call ranline_update      (obj)
      end subroutine ranline_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine ranline_update (obj)
      type(ranline_struct),intent(inout),target :: obj          ! arguments

      integer                       :: nstore,nscratch             ! local
      integer                       :: nxgather,nxmix,nxdips       ! local
      integer                       :: nygather,nymix,nydips       ! local
      integer                       :: num_tr_dip_x,num_tr_dip_y   ! local
      integer                       :: num_tr_mix_x,num_tr_mix_y   ! local
      integer                       :: max_x_bins,max_y_bins       ! local
      real                          :: dip_inc_x,dip_inc_y         ! local
      real                          :: x_inc,y_inc                 ! local
      logical                       :: error                       ! local
      integer                       :: ncolumns,indx,err           ! local
      real                          :: vline(2)                    ! local
      type(floatio_struct),pointer  :: floatio                     ! local
      character(len=80)             :: msg,coords_info             ! local

      nullify (floatio) ! jpa

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      if (pathchoose_update(obj%pathchoose, obj%pathname)) return

      call pc_register_array_names &
                      ('x_coord_arrayset', (/'X_COORD', 'Y_COORD'/))

      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt) 
   
      call pc_get ('hdr_x'             , obj%hdr_x      )
      call pc_get ('hdr_y'             , obj%hdr_y      )
      call pc_get ('max_search'        , obj%max_search )
      call pc_get ('max_interp'        , obj%max_interp )
      call pc_get ('win_len'           , obj%win_len    )
      call pc_get ('fracmix'           , obj%fracmix    )
      call pc_get ('dip_max_x'         , obj%dip_max_x  )
      call pc_get ('dip_max_y'         , obj%dip_max_y  )
      call pc_get ('search_x'          , obj%search_x   )
      call pc_get ('search_y'          , obj%search_y   )
      call pc_get ('feet_x'            , obj%feet_x     )
      call pc_get ('feet_y'            , obj%feet_y     )
      call pc_get ('feet_out'          , obj%feet_out   )
      call pc_get ('num_traces'        , obj%num_traces )
      call pc_get ('pathname'          , obj%pathname   )

      call pc_alloc ('x_coord'     , obj%x_coord   , obj%ncoord)  
      call pc_alloc ('y_coord'     , obj%y_coord   , obj%ncoord) 


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      call pathcheck &
              ('pathname', obj%pathname, 'ranline', show=PATHCHECK_INFO_INPUT)

      call mth_constrain (obj%fracmix, 0.0, 1.414)

      obj%search_x = max(obj%search_x, 0.0)
      obj%search_y = max(obj%search_y, 0.0)

      if (pc_verify_end()) then
           if (obj%feet_x == FNIL .or. obj%feet_x <= 0.0) then
                call pc_error ('FEET_X must be greater than zero')
           end if

           if (obj%feet_y == FNIL .or. obj%feet_y <= 0.0) then
                call pc_error ('FEET_Y must be greater than zero')
           end if

           if (obj%feet_out == FNIL .or. obj%feet_out <= 0.0) then
                call pc_error ('FEET_OUT must be greater than zero')
           end if

           if (obj%pathname == PATHCHECK_EMPTY .and. obj%ncoord <= 0) then
                call pc_error ('you must specify arrays X_COORD and Y_COORD')
           end if

           if (obj%pathname /= PATHCHECK_EMPTY .and. obj%ncoord >= 1) then
                call pc_warning ('arrays X_COORD and Y_COORD will not be used')
           end if
      end if

      num_tr_dip_x = nint(obj%search_x)     ! for getting dip_inc_x only.
      num_tr_dip_y = nint(obj%search_y)     ! for getting dip_inc_y only.
      num_tr_mix_x = num_tr_dip_x           ! to make sdiputil happy.
      num_tr_mix_y = num_tr_dip_y           ! to make sdiputil happy.
      max_x_bins   = 2                      ! to make sdiputil happy.
      max_y_bins   = 1                      ! to make sdiputil happy.
      x_inc        = 1.0                    ! to make sdiputil happy.
      y_inc        = 1.0                    ! to make sdiputil happy.

      call sdiputil_update (obj%sdiputil, obj%opt_output,                 &
                 obj%pwr_edge, obj%pwr_semb, obj%opt_semb, obj%fctr_mix,  &
                 obj%win_len          , obj%win_nsamp       ,             &
                 obj%quick_dip_weights, obj%quick_dip_search,             &
                 obj%dip_max_x        , obj%dip_max_y       ,             &
                 num_tr_dip_x         , num_tr_dip_y        ,             &
                 num_tr_mix_x         , num_tr_mix_y        ,             &
                 obj%hdr_x            , obj%hdr_y           ,             &
                 x_inc                , y_inc               ,             &
                 max_x_bins           , max_y_bins          ,             &
                 nxgather             , nygather            ,             &
                 nxmix                , nymix               ,             &
                 nxdips               , nydips              ,             &
                 dip_inc_x            , dip_inc_y)

      nscratch = obj%max_search * (2 * obj%nwih + obj%ndpt)
      nstore   = obj%ncoord * 5

      if (obj%pathname /= PATHCHECK_EMPTY) then
           coords_info = 'random line coordinates are specified on a file'
      else if (obj%ncoord > 0) then
           coords_info = trim(string_ii2ss(obj%ncoord))// &
                        ' random line coordinate pairs are specified in a table'
      else
           coords_info = 'random line coordinates have not been specified'
      end if

      if (pc_verify_end()) then
           if (nxdips > 30 .or. nydips > 30) then
               call pc_warning &
                      ('NXDIPS or NYDIPS appears excessive - check parameters')
               call pc_warning('(DIP_MAX_X and DIP_MAX_Y must be specified as')
               call pc_warning(' maximum dip per header word unit --')
               call pc_warning(' not necessarily the same as dip per trace)')
               call pc_warning('(SEARCH_X and SEARCH_Y must be specified in')
               call pc_warning(' header word units --')
               call pc_warning(' not necessarily the same as number of traces)')
           end if

           if (obj%feet_out < 5.0) then
             call pc_warning('FEET_OUT appears too small - check parameters')
             call pc_warning('FEET_OUT must be specified in feet or meters')
           end if

           if (obj%hdr_x == 7 .or. obj%hdr_x == 8) then
             if (obj%feet_x < 5.0) then
               call pc_warning('FEET_X appears too small - check parameters')
               call pc_warning('FEET_X should normally be larger for hdrs 7,8')
               call pc_warning('FEET_X must be specified in feet or meters')
             end if
             if (obj%search_x > 5.0) then
               call pc_warning('SEARCH_X appears too large - check parameters')
               call pc_warning('SEARCH_X is normally smaller for hdrs 7,8')
               call pc_warning('SEARCH_X must be specified in hwd units')
             end if
           end if

           if (obj%hdr_y == 7 .or. obj%hdr_y == 8) then
             if (obj%feet_y < 5.0) then
               call pc_warning('FEET_Y appears too small - check parameters')
               call pc_warning('FEET_Y should normally be larger for hdrs 7,8')
               call pc_warning('FEET_Y must be specified in feet or meters')
             end if
             if (obj%search_y > 5.0) then
               call pc_warning('SEARCH_Y appears too large - check parameters')
               call pc_warning('SEARCH_Y is normally smaller for hdrs 7,8')
               call pc_warning('SEARCH_Y must be specified in hwd units')
             end if
           end if

           if (obj%hdr_x == 17 .or. obj%hdr_x == 18) then
             if (obj%feet_x > 5.0) then
               call pc_warning('FEET_X appears too large - check parameters')
               call pc_warning('FEET_X should normally be 1 for hdrs 17,18')
               call pc_warning('FEET_X must be specified in feet or meters')
             end if
             if (obj%search_x < 5.0) then
               call pc_warning('SEARCH_X appears too small - check parameters')
               call pc_warning('SEARCH_X should be larger for hdrs 17,18')
               call pc_warning('SEARCH_X must be specified in hwd units')
             end if
             if (obj%dip_max_x > 1.0) then
               call pc_warning('DIP_MAX_X appears too large - check parameters')
               call pc_warning('DIP_MAX_X should be very small for hdrs 17,18')
               call pc_warning('DIP_MAX_X must be specified in dip / hwd unit')
             end if
           end if

           if (obj%hdr_y == 17 .or. obj%hdr_y == 18) then
             if (obj%feet_y > 5.0) then
               call pc_warning('FEET_Y appears too large - check parameters')
               call pc_warning('FEET_Y should normally be 1 for hdrs 17,18')
               call pc_warning('FEET_Y must be specified in feet or meters')
             end if
             if (obj%search_y < 5.0) then
               call pc_warning('SEARCH_Y appears too small - check parameters')
               call pc_warning('SEARCH_Y should be larger for hdrs 17,18')
               call pc_warning('SEARCH_Y must be specified in hwd units')
             end if
             if (obj%dip_max_y > 1.0) then
               call pc_warning('DIP_MAX_Y appears too large - check parameters')
               call pc_warning('DIP_MAX_Y should be very small for hdrs 17,18')
               call pc_warning('DIP_MAX_Y must be specified in dip / hwd unit')
             end if
           end if
      end if

      if (pc_pressed("plot")) then
           call ranline_add_points (obj)
           call gplot_plot         (obj%gplot)
      end if

      if (pc_pressed("replot")) then
           call ranline_add_points (obj)
           call gplot_replot       (obj%gplot)
      end if

      if (pc_verify_end()) then
           call gplot_delete (obj%gplot)
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!




!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_global ('numtr'   , 1) 
      call pc_put_global ('gathered', .false.)
   
      call pc_put ('hdr_x'             , obj%hdr_x      )
      call pc_put ('hdr_y'             , obj%hdr_y      )
      call pc_put ('max_search'        , obj%max_search )
      call pc_put ('max_interp'        , obj%max_interp )
      call pc_put ('win_len'           , obj%win_len    )
      call pc_put ('fracmix'           , obj%fracmix    )
      call pc_put ('dip_max_x'         , obj%dip_max_x  )
      call pc_put ('dip_max_y'         , obj%dip_max_y  )
      call pc_put ('search_x'          , obj%search_x   )
      call pc_put ('search_y'          , obj%search_y   )
      call pc_put ('feet_x'            , obj%feet_x     )
      call pc_put ('feet_y'            , obj%feet_y     )
      call pc_put ('feet_out'          , obj%feet_out   )
      call pc_put ('num_traces'        , obj%num_traces )
      call pc_put ('pathname'          , obj%pathname   )

      call pc_put ('x_coord'     , obj%x_coord   , obj%ncoord)  
      call pc_put ('y_coord'     , obj%y_coord   , obj%ncoord) 

 !!!  call pc_put_sensitive_arrayset_flag &
 !!!                  ('x_coord_arrayset', obj%pathname == PATHCHECK_EMPTY) 

 !!!!!!!!!!! the above call only desensitizes the labels.

      call pc_put_sensitive_array_flag &
                      ('x_coord', obj%pathname == PATHCHECK_EMPTY) 

      call pc_put_sensitive_array_flag &
                      ('y_coord', obj%pathname == PATHCHECK_EMPTY) 

      call pc_put_gui_only ('dip_inc_x'   , dip_inc_x  ,6,2)
      call pc_put_gui_only ('dip_inc_y'   , dip_inc_y  ,6,2)
      call pc_put_gui_only ('nxdips'      , nxdips         )
      call pc_put_gui_only ('nydips'      , nydips         )
      call pc_put_gui_only ('coords_info' , coords_info    )

      call pc_put_control ('need_request' , .true.)
      call pc_put_control ('need_label'   , .true.) 
      call pc_put_control ('iftd'         , .true.) 
      call pc_put_control ('nscratch'     , nscratch)
      call pc_put_control ('nstore'       , nstore)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call temptfile_close   (obj%temptfile)
      call triplemerge_close (obj%triplemerge)

      if (associated(obj%segment)) deallocate (obj%segment)

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

!----------get coordinates from file.

      if (obj%pathname /= PATHCHECK_EMPTY) then
           call floatio_easy_read &
                          (floatio,obj%pathname,obj%ncoord,ncolumns,err,msg)
           if (err /= FLOATIO_OK) then
                call pc_error('RANLINE: FATAL ERROR IN FLOATIO_EASY_READ')
                call pc_error (msg)
           else if (obj%ncoord <= 0) then
                call pc_error('RANLINE: BAD INFO FROM FLOATIO_EASY_READ')
                call pc_error ('no coordinates on file')
           else if (ncolumns < 2) then
                call pc_error('RANLINE: BAD INFO FROM FLOATIO_EASY_READ')
                call pc_error ('less than two columns of coordinates on file')
           end if

           call mem_alloc (obj%x_coord, obj%ncoord)
           call mem_alloc (obj%y_coord, obj%ncoord)

           if (pc_do_not_process_traces()) return
                                             ! in case of allocation errors.

           do indx = 1,obj%ncoord
                call floatio_read_line (floatio,err,msg,vline)
                if (err /= FLOATIO_OK) then
                     call pc_error('RANLINE: FATAL ERROR IN FLOATIO_READ_LINE')
                     call pc_error (msg)
                     exit
                end if
                obj%x_coord(indx) = vline(1)
                obj%y_coord(indx) = vline(2)
           end do
           call floatio_close (floatio)
      end if

!----------GET NUMBER OF BINS AND CALCULATE NEEDED PARAMETERS.

      allocate(obj%segment(obj%ncoord), stat=err)
      if (err /= 0) then
           call pc_error &
                    ('RANLINE: error allocating',obj%ncoord,'triplesort words')
           return
      end if

      call ranline_help1 (obj,error)
      if (error) then
           call pc_error('RANLINE: FATAL ERROR IN RANLINE_HELP1')
      end if

!----------set up a temporary file for trace and header storage.

      call temptfile_open (obj%temptfile, 'RANLINE', obj%nwih, obj%ndpt,  &
                           lun, err, maxrecords=obj%num_traces, vartypes='DR')
      if (err /= TEMPTFILE_OK) then
           call pc_error ('RANLINE: FATAL ERROR IN TEMPTFILE_OPEN')
      end if

!----------set up space for trace headers and sorting.

      call triplemerge_open (obj%triplemerge,error)
      if (error) then
           call pc_error ('RANLINE: FATAL ERROR IN TRIPLEMERGE_OPEN')
      end if

!----------FINISH UP AND RETURN.

      obj%kount        = 0
      obj%key          = 0
      obj%info%dead    = 0               ! input traces.
      obj%info%live    = 0               ! input traces.
      obj%info%xlo     = 999999          ! input traces.
      obj%info%xup     = 0               ! input traces.
      obj%info%ylo     = 999999          ! input traces.
      obj%info%yup     = 0               ! input traces.
      obj%info%numlook = 0               ! output traces.
      obj%info%numterp = 0               ! output traces.
      obj%info%lastx   = FNIL            ! output traces.
      obj%info%lasty   = FNIL            ! output traces.
      obj%info%minstep = FNIL            ! output traces.
      obj%info%maxstep = FNIL            ! output traces.
      obj%info%sumstep = FNIL            ! output traces.
      obj%info%numstep = 0               ! output traces.
      obj%info%numbad  = 0               ! output traces.
      obj%info%iseg    = 1               ! output traces.
      obj%info%indx    = 0               ! output traces.

      call sdiputil_prepare (obj%sdiputil,obj%hdr_x,obj%hdr_y,SCRATCH_NSW)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine ranline_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!------------------------- add points -------------------------------------!!
!!------------------------- add points -------------------------------------!!
!!------------------------- add points -------------------------------------!!


      subroutine ranline_add_points (obj)
      type(ranline_struct),intent(inout) :: obj                ! arguments
      integer                            :: indx               ! local

      do indx = 1,obj%ncoord
           if (obj%x_coord(indx) == 0.0 .and. obj%y_coord(indx) == 0.0) then
                call gplot_add_break (obj%gplot)
           else
                call gplot_add_point &
                        (obj%gplot,obj%x_coord(indx),obj%y_coord(indx))
           end if
          if (pc_update_error()) return
      end do
      end subroutine ranline_add_points


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine ranline (obj,ntr,hd,tr)
      type(ranline_struct),intent(inout) :: obj                ! arguments
      integer             ,intent(inout) :: ntr                ! arguments
      double precision    ,intent(inout) :: hd(:,:)            ! arguments
      real                ,intent(inout) :: tr(:,:)            ! arguments

      if (ntr >= 1) then

           call ranline_input (obj,ntr,hd,tr)

           if (ntr == FATAL_ERROR) then
                call pc_error ('RANLINE: FATAL ERROR IN RANLINE_INPUT')
                call pc_error ('RANLINE: INPUT NTR >= 1')
                call pc_error ('RANLINE: OUTPUT NTR = FATAL_ERROR')
                call ranline_wrapup (obj)
           else if (ntr /= NEED_TRACES) then
                call pc_error ('RANLINE: ILLEGAL NTR FROM RANLINE_INPUT')
                call pc_error ('RANLINE: INPUT NTR >= 1')
                call pc_error ('RANLINE: OUTPUT NTR =',ntr)
                call pc_error ('RANLINE: THIS IS A PROGRAMMING ERROR')
                call ranline_wrapup (obj)
           end if

      else if (ntr == NO_MORE_TRACES) then

           call ranline_complete (obj,ntr)

           if (ntr == FATAL_ERROR) then
                call pc_error ('RANLINE: FATAL ERROR IN RANLINE_COMPLETE')
                call pc_error ('RANLINE: INPUT NTR = NO_MORE_TRACES')
                call pc_error ('RANLINE: OUTPUT NTR = FATAL_ERROR')
                call ranline_wrapup (obj)
                return
           else if (ntr /= NEED_TRACES) then
                call pc_error ('RANLINE: ILLEGAL NTR FROM RANLINE_COMPLETE')
                call pc_error ('RANLINE: INPUT NTR = NO_MORE_TRACES')
                call pc_error ('RANLINE: OUTPUT NTR =',ntr)
                call pc_error ('RANLINE: THIS IS A PROGRAMMING ERROR')
                call ranline_wrapup (obj)
                return
           end if

           call ranline_output   (obj,ntr,hd,tr)

           if (ntr == FATAL_ERROR) then
                call pc_error ('RANLINE: FATAL ERROR IN RANLINE_OUTPUT')
                call pc_error ('RANLINE: INPUT NTR = NO_MORE_TRACES')
                call pc_error ('RANLINE: OUTPUT NTR = FATAL_ERROR')
                call ranline_wrapup (obj)
           else if (ntr == NO_MORE_TRACES) then
                call ranline_wrapup (obj)
           else if (ntr /= 1) then
                call pc_error ('RANLINE: ILLEGAL NTR FROM RANLINE_OUTPUT')
                call pc_error ('RANLINE: INPUT NTR = NO_MORE_TRACES')
                call pc_error ('RANLINE: OUTPUT NTR =',ntr)
                call pc_error ('RANLINE: THIS IS A PROGRAMMING ERROR')
                call ranline_wrapup (obj)
           end if

      else if (ntr == NEED_TRACES) then

           call ranline_output (obj,ntr,hd,tr)

           if (ntr == FATAL_ERROR) then
                call pc_error ('RANLINE: FATAL ERROR IN RANLINE_OUTPUT')
                call pc_error ('RANLINE: INPUT NTR = NEED_TRACES')
                call pc_error ('RANLINE: OUTPUT NTR = FATAL_ERROR')
                call ranline_wrapup (obj)
           else if (ntr == NO_MORE_TRACES) then
                call ranline_wrapup (obj)
           else if (ntr /= 1) then
                call pc_error ('RANLINE: ILLEGAL NTR FROM RANLINE_OUTPUT')
                call pc_error ('RANLINE: INPUT NTR = NEED_TRACES')
                call pc_error ('RANLINE: OUTPUT NTR =',ntr)
                call pc_error ('RANLINE: THIS IS A PROGRAMMING ERROR')
                call ranline_wrapup (obj)
           end if

      end if
      end subroutine ranline


!!--------------------------- ranline input ---------------------------------!!
!!--------------------------- ranline input ---------------------------------!!
!!--------------------------- ranline input ---------------------------------!!

! NTR must be >= 1 upon input to this routine.
! this routine sets NTR to NEED_TRACES or FATAL_ERROR.


      subroutine ranline_input (obj,ntr,hd,tr)
      type(ranline_struct),intent(inout) :: obj                ! arguments
      integer             ,intent(inout) :: ntr                ! arguments
      double precision    ,intent(inout) :: hd(:,:)            ! arguments
      real                ,intent(inout) :: tr(:,:)            ! arguments
      integer                            :: indx               ! local
      logical                            :: error              ! local

      if (ntr <= 0) then
           call pc_error ('illegal input NTR upon entry to RANLINE_INPUT')
           call pc_error ('INPUT NTR =',ntr)
           call pc_error ('THIS IS A PROGRAMMING ERROR')
           ntr = FATAL_ERROR
           return
      end if

      do indx = 1,ntr
           call ranline_beg (obj,HD(:,indx),TR(:,indx), error)
           if (error) then
                call pc_error ('RANLINE: FATAL ERROR IN RANLINE_BEG')
                ntr = FATAL_ERROR
                return
           end if
      end do

      ntr = NEED_TRACES
      end subroutine ranline_input


!!--------------------------- ranline complete ------------------------------!!
!!--------------------------- ranline complete ------------------------------!!
!!--------------------------- ranline complete ------------------------------!!

! NTR must be NO_MORE_TRACES upon input to this routine.
! this routine sets NTR to NEED_TRACES or FATAL_ERROR.


      subroutine ranline_complete (obj,ntr)
      type(ranline_struct),intent(inout) :: obj                ! arguments
      integer             ,intent(inout) :: ntr                ! arguments
      logical                            :: error              ! local

      if (ntr /= NO_MORE_TRACES) then
           call pc_error ('illegal input NTR upon entry to RANLINE_COMPLETE')
           call pc_error ('INPUT NTR =',ntr)
           call pc_error ('THIS IS A PROGRAMMING ERROR')
           ntr = FATAL_ERROR
           return
      end if

!----------PRINT TRACE INPUT SUMMARY.

      write(lun,*) ' '
      write(lun,*) '========== RANLINE TRACE INPUT SUMMARY =========='
      write(lun,*) ' '

      write(lun,*) 'DISK SPACE WAS SAVED FOR UP TO ',obj%num_traces,' TRACES'
      write(lun,*) obj%info%live+obj%info%dead,' TOTAL TRACES INPUT'
      write(lun,*) obj%info%dead,' DEAD TRACES INPUT AND DELETED'
      write(lun,*) obj%info%live - obj%key,' TRACES DELETED BY',  &
                      ' DIP SEARCH AND INTERPOLATION REQUIREMENTS'
      write(lun,*) obj%key, &
                      ' INPUT TRACES NEEDED FOR DIP SEARCH AND INTERPOLATION'
      write(lun,*) obj%kount,' TRACE HEADERS WILL BE SORTED', &
                      ' (each one duplicated as many times as needed)'
      write(lun,*) obj%nbin,' OUTPUT TRACES WILL BE CREATED'
      write(lun,*) 'EACH INPUT TRACE USED WILL CONTRIBUTE TO ',  &
                    FLOAT(obj%kount)/max(obj%key,1),' OUTPUT TRACES ON AVERAGE'
      write(lun,*) 'EACH OUTPUT TRACE WILL BE GENERATED FROM ',  &
                    FLOAT(obj%kount)/max(obj%nbin,1),' INPUT TRACES ON AVERAGE'
      write(lun,*) 'MIN AND MAX X-COORDS INPUT = ',obj%info%xlo,obj%info%xup
      write(lun,*) 'MIN AND MAX Y-COORDS INPUT = ',obj%info%ylo,obj%info%yup

      write(lun,*) ' '
      write(lun,*) '========== END RANLINE TRACE INPUT SUMMARY =========='
      write(lun,*) ' '

      if (obj%kount == 0 .or. obj%key == 0) then
           call pc_error ('RANLINE: FATAL ERROR IN RANLINE_COMPLETE')
           call pc_error ('RANLINE: KOUNT =',obj%kount)
           call pc_error ('RANLINE: KEY =',obj%key)
           ntr = FATAL_ERROR
           return
      end if

!----------SORT THE TRACE HEADERS.

      call triplemerge_sort (obj%triplemerge,lun,obj%kount,error)
      if (error) then
           call pc_error ('RANLINE: FATAL ERROR IN TRIPLEMERGE_SORT')
           ntr = FATAL_ERROR
           return
      end if

!----------GET READY TO OUTPUT TRACES.

      obj%ibin     = 0
      obj%kkk      = 0
      obj%sequence = 0            ! sequential trace number.
      obj%izero    = 0
      obj%index    = 1

      obj%info%dead    = 0               ! output traces.
      obj%info%live    = 0               ! output traces.
      obj%info%xlo     = 999999          ! output traces.
      obj%info%xup     = 0               ! output traces.
      obj%info%ylo     = 999999          ! output traces.
      obj%info%yup     = 0               ! output traces.
      obj%info%numlook = 0               ! output traces.
      obj%info%numterp = 0               ! output traces.
      obj%info%lastx   = FNIL            ! output traces.
      obj%info%lasty   = FNIL            ! output traces.
      obj%info%minstep = FNIL            ! output traces.
      obj%info%maxstep = FNIL            ! output traces.
      obj%info%sumstep = FNIL            ! output traces.
      obj%info%numstep = 0               ! output traces.
      obj%info%numbad  = 0               ! output traces.
      obj%info%iseg    = 1               ! output traces.
      obj%info%indx    = 0               ! output traces.

      ntr = NEED_TRACES
      end subroutine ranline_complete


!!--------------------------- ranline output --------------------------------!!
!!--------------------------- ranline output --------------------------------!!
!!--------------------------- ranline output --------------------------------!!

! NTR must be NEED_TRACES upon input to this routine.
! this routine sets NTR to 1 or NO_MORE_TRACES or FATAL_ERROR.


      subroutine ranline_output (obj,ntr,hd,tr)
      type(ranline_struct),intent(inout) :: obj                ! arguments
      integer             ,intent(inout) :: ntr                ! arguments
      double precision    ,intent(inout) :: hd(:,:)            ! arguments
      real                ,intent(inout) :: tr(:,:)            ! arguments
      logical                            :: error              ! local

      if (ntr /= NEED_TRACES) then
           call pc_error ('illegal input NTR upon entry to RANLINE_OUTPUT')
           call pc_error ('INPUT NTR =',ntr)
           call pc_error ('THIS IS A PROGRAMMING ERROR')
           ntr = FATAL_ERROR
           return
      end if

!----------GET A TRACE TO PASS OUT.

      obj%ibin = obj%ibin + 1
      if (obj%ibin > obj%nbin) then
           ntr = NO_MORE_TRACES
           return
      end if

      call ranline_out (obj,  HD,TR,error)
      if (error) then
           call pc_error ('RANLINE: FATAL ERROR IN RANLINE_OUT')
           ntr = FATAL_ERROR
           return
      end if

!----------SET HEADER WORD ONE AND RETURN.

      obj%sequence = obj%sequence + 1
      HD(1,1)      = obj%sequence
      ntr          = 1
      end subroutine ranline_output


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine ranline_wrapup (obj)
      type(ranline_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      write(lun,*) ' '
      write(lun,*) '========== RANLINE TRACE OUTPUT SUMMARY =========='
      write(lun,*) ' '

      call sdiputil_wrapup  (obj%sdiputil)

      write(lun,*) obj%info%live+obj%info%dead,' TOTAL TRACES OUTPUT'
      write(lun,*) obj%info%dead,' DEAD TRACES OUTPUT'
      write(lun,*) obj%info%live,' LIVE TRACES OUTPUT'
      write(lun,*) 'MIN AND MAX X-COORDS OUTPUT = ', &
                   obj%info%xlo,obj%info%xup
      write(lun,*) 'MIN AND MAX Y-COORDS OUTPUT = ', &
                   obj%info%ylo,obj%info%yup
      write(lun,*) 'THE FOLLOWING STATISTICS ARE FOR LIVE TRACES ONLY:'
      write(lun,*) 'AVERAGE NUMBER OF TRACES USED FOR DIP SEARCH = ', &
                   FLOAT(obj%info%numlook)/max(obj%info%live,1)
      write(lun,*) 'AVERAGE NUMBER OF TRACES USED FOR INTERPOLATION = ', &
                   FLOAT(obj%info%numterp)/max(obj%info%live,1)

      if (obj%dip_max_x == 0.0 .and. obj%dip_max_y == 0.0)  &
                  write(lun,*) 'NO DIP SEARCHES WERE ACTUALLY DONE'

      write(lun,*) ' '
      write(lun,*) '    requested step distance = ',obj%feet_out
      write(lun,*) '      minimum step distance = ',obj%info%minstep
      write(lun,*) '      maximum step distance = ',obj%info%maxstep
      write(lun,*) '      average step distance = ',obj%info%sumstep &
                                                    / max(obj%info%numstep,1)
      write(lun,*) '            number of steps = ',obj%info%numstep
      write(lun,*) '        number of bad steps = ',obj%info%numbad
      write(lun,*) ' '

      call temptfile_close   (obj%temptfile)
      call triplemerge_close (obj%triplemerge)

      write(lun,*) ' '
      write(lun,*) '========== END RANLINE TRACE OUTPUT SUMMARY =========='
      write(lun,*) ' '
      end subroutine ranline_wrapup



!!--------------------------- ranline beg -------------------------------!!
!!--------------------------- ranline beg -------------------------------!!
!!--------------------------- ranline beg -------------------------------!!

!     DECIDE WHAT TO DO WITH INPUT TRACE.
!     UPDATES STATISTICS obj%info = DEAD,LIVE,XLO,XUP,YLO,YUP.


      subroutine ranline_beg (obj,HD,TR, error)
      type(ranline_struct),intent(inout) :: obj              ! arguments
      double precision    ,intent(inout) :: hd(:)            ! arguments
      real                ,intent(inout) :: tr(:)            ! arguments
      logical             ,intent(out)   :: error            ! arguments
      integer                            :: err,keep         ! local
      double precision                   :: hx,hy            ! local

!----------GET STARTED.

      if (HD(25) == 0.0) then
           obj%info%dead = obj%info%dead + 1
           error = .false.
           return
      end if
      obj%info%live = obj%info%live + 1
      KEEP = obj%kount
      obj%key = obj%key + 1

!----------GET TRACE COORDINATES (AND SET TO ZERO FOR SCRATCH HEADER 26).

      HX = HD(obj%hdr_x)
      HY = HD(obj%hdr_y)
      obj%info%xlo = min(NINT(HX),obj%info%xlo)
      obj%info%xup = max(NINT(HX),obj%info%xup)
      obj%info%ylo = min(NINT(HY),obj%info%ylo)
      obj%info%yup = max(NINT(HY),obj%info%yup)

!----------SAVE THE BINS THE TRACE CONTRIBUTES TO.

      call ranline_help2 (obj,HX,HY,  error)
      if (error) then
           call pc_error ('RANLINE: FATAL ERROR IN RANLINE_HELP2')
           error = .true.
           return
      end if

!----------SAVE TRACE IN WORKSPACE IF IT IS NEEDED.

      if (obj%kount == KEEP) then
           obj%key = obj%key - 1
      else
           HD(5) = 1.0

           call temptfile_write (obj%temptfile,obj%key,hd,tr,err)
           if (err /= TEMPTFILE_OK) then
             call pc_error &
                   ('RANLINE: FATAL ERROR IN TEMPTFILE_WRITE at trace',obj%key)
             error = .true.
             return
           end if

      end if
      error = .false.
      end subroutine ranline_beg
           

!!--------------------------- ranline out -------------------------------!!
!!--------------------------- ranline out -------------------------------!!
!!--------------------------- ranline out -------------------------------!!

!     GET TRACES FROM WORKSPACE, GET BIN COORDINATES, AND OUTPUT
!        INTERPOLATED TRACES.
!     UPDATES STATISTICS obj%info = NUM,NUMLOOK,NUMTERP.
!     UPON OUTPUT, ALL TRACES THAT ARE MIXED ARE OUTPUT IN HDMIX,TRMIX,
!        AFTER DIP-ADJUSTMENT.  THE NUMBER OF THESE TRACES IS NTERP.
!        IF NTERP=0, THE FIRST HDMIX,TRMIX IS THE SAME AS HD,TR.
!     UPON OUTPUT, THE MIXED OR INTERPOLATED TRACE IS HD,TR,
!        THIS BEING DEAD IF NTERP=0.


      subroutine ranline_out (obj,  HD,TR,error)
      type(ranline_struct),intent(inout) :: obj                  ! arguments
      double precision    ,intent(inout) :: hd(:,:)              ! arguments
      real                ,intent(inout) :: tr(:,:)              ! arguments
      logical             ,intent(out)   :: error                ! arguments
      real                               :: xmid,ymid            ! local
      real                               :: wsum,dist            ! local
      integer                            :: nlook,nterp,indx     ! local
      integer                            :: ibbb,idist,err,ntr   ! local
      real                               :: xcoord,ycoord,step   ! local
      logical                            :: bad                  ! local
      double precision                   :: HDMIX(obj%nwih,obj%max_search)
      real                               :: TRMIX(obj%ndpt,obj%max_search)

!----------GET COORDINATES OF THIS BIN.

      call ranline_help3 (obj,   xmid,ymid,error)
      if (error) then
           call pc_error ('RANLINE: FATAL ERROR IN RANLINE_HELP3')
           error = .true.
           return
      end if

!----------PREPARE TO COLLECT TRACES.

      indx = 0
      do

!----------GET HEADER INFORMATION FOR NEXT TRACE.

           obj%kkk = obj%kkk + 1
           if (obj%kkk > obj%kount) exit

           call triplemerge_read (obj%triplemerge,IBBB,IDIST,obj%key,error)
           if (error) then
                call pc_error ('RANLINE: FATAL ERROR IN TRIPLEMERGE_READ')
                error = .true.
                return
           end if

           if (IBBB < obj%ibin) cycle
           if (IBBB > obj%ibin) then
                obj%kkk = obj%kkk - 1
                exit
           end if

!----------GET NEXT TRACE TO USE FOR THIS BIN.

           indx = indx + 1

           call temptfile_read &
                   (obj%temptfile,obj%key,hdmix(:,indx),trmix(:,indx),err)
           if (err /= TEMPTFILE_OK) then
             call pc_error &
                   ('RANLINE: FATAL ERROR IN TEMPTFILE_READ at trace',obj%key)
             error = .true.
             return
           end if

!----------GET WEIGHT FOR THIS TRACE.

           HDMIX(SCRATCH_NSW,indx) = 0.0
           if (indx <= obj%max_interp) then
                DIST = max(IDIST/DISTFACTOR,obj%fracmix,0.01)
                HDMIX(SCRATCH_NSW,indx) = 1.0/DIST
           end if

           if (indx >= obj%max_search) exit

      end do

!----------WE NOW HAVE ALL THE TRACES WE NEED FOR THIS BIN.

      obj%info%xlo  = min(NINT(xmid),obj%info%xlo)
      obj%info%xup  = max(NINT(xmid),obj%info%xup)
      obj%info%ylo  = min(NINT(ymid),obj%info%ylo)
      obj%info%yup  = max(NINT(ymid),obj%info%yup)

      nlook = indx
      nterp = min(indx,obj%max_interp)

!----------SPECIAL ACTION WHEN THERE ARE NO TRACES TO MIX.

      if (nterp == 0) then
           obj%info%dead        = obj%info%dead + 1
           HD   (1:obj%nwih ,1) = 0.0
           HD   (2          ,1) = 1
           HD   (64         ,1) = obj%ndpt
           HD   (3          ,1) = obj%ibin
           HD   (4          ,1) = 1.0
           HD   (25         ,1) = 0.0
           HD   (obj%hdr_x  ,1) = xmid
           HD   (obj%hdr_y  ,1) = ymid
           TR   (1:obj%ndpt ,1) = 0.0

!----------GET OUTPUT TRACE.

      else
           obj%info%live    = obj%info%live    + 1
           obj%info%numlook = obj%info%numlook + nlook
           obj%info%numterp = obj%info%numterp + nterp

           ntr = nlook
           call sdiputil_solve (obj%sdiputil,ntr,hdmix,trmix,xmid,ymid,hd,tr)
      end if

!----------COLLECT STEP SIZE STATISTICS.

      xcoord = hd(obj%hdr_x,1) * obj%feet_x
      ycoord = hd(obj%hdr_y,1) * obj%feet_y
      bad    = .false.

      if (obj%info%lastx == FNIL) then
           step = 0.0
      else
           step = sqrt((xcoord-obj%info%lastx)**2 + (ycoord-obj%info%lasty)**2)
      end if

      obj%info%indx = obj%info%indx + 1
      if (obj%info%indx > obj%segment(obj%info%iseg)%number) then
           obj%info%iseg = obj%info%iseg + 1
           obj%info%indx = 1
           if (obj%info%iseg > obj%nseg) then
                call pc_error ('RANLINE: SEGMENT COUNT TOO LARGE')
                call pc_error ('RANLINE: THIS IS A PROGRAMMING ERROR')
                error = .true.
                return
           end if
      end if
           
      if (obj%info%indx > 1) then
  !!  if (obj%info%indx > 1 .or. .not.obj%segment(obj%info%iseg)%newstart) then
           if (obj%info%numstep == 0) then
                obj%info%minstep = step
                obj%info%maxstep = step
                obj%info%sumstep = step
                obj%info%numstep = 1
           else
                obj%info%minstep = min(step, obj%info%minstep)
                obj%info%maxstep = max(step, obj%info%maxstep)
                obj%info%sumstep = obj%info%sumstep + step
                obj%info%numstep = obj%info%numstep + 1
           end if
           if (step < obj%feet_out - 0.1 .or. step > obj%feet_out + 0.1) then
                obj%info%numbad = obj%info%numbad + 1
                bad = .true.
           end if
      end if

      obj%info%lastx = xcoord
      obj%info%lasty = ycoord

!----------OPTIONAL PRINTOUT.

      if (obj%info%indx == 1) then
           write(lun,*) ' '
           write(lun,*) &
              '========== RANLINE INFO FOR SEGMENT',obj%info%iseg,'=========='
           write(lun,*) ' '
           continue
      else if (obj%info%indx == obj%segment(obj%info%iseg)%number) then
           continue
      else if (bad .and. obj%info%numbad <= 99) then
           continue
      else if (obj%ibin <= NPR) then
           continue
      else if (obj%ibin >= obj%nbin/2 - NPR/2 .and. &
               obj%ibin <= obj%nbin/2 + NPR/2) then
           continue
      else if (obj%ibin >= obj%nbin-NPR) then
           continue
      else
           error = .false.
           return
      end if

      WSUM = 0.0
      do indx = 1,nterp
           WSUM = WSUM + HDMIX(SCRATCH_NSW,indx)
      end do
      write(lun,1000) obj%ibin,nlook,nterp,HD(obj%hdr_x,1),HD(obj%hdr_y,1), &
                      WSUM,step

      do indx = 1,nlook
           write(lun,2000) indx,HDMIX(obj%hdr_x,indx),HDMIX(obj%hdr_y,indx), &
                                HDMIX(SCRATCH_NSW,indx),                     &
                                HDMIX(obj%hdr_x,indx) - HD(obj%hdr_x,1),     &
                                HDMIX(obj%hdr_y,indx) - HD(obj%hdr_y,1)
      end do

1000  format (' BIN=',I5,5X,'NLOOK=',I3,5X,'NTERP=',I3,5X,   &
              'XYBIN=',2F10.3,5X,'SUM OF WEIGHTS=',F7.3,4x,'STEP=',F9.1)
2000  format (5X,'TRACE=',I3,5X,'XY=',2F10.3,5X,'WEIGHT=',F7.3,   &
              5X,'XY-XYBIN=',2F10.3)

      error = .false.
      end subroutine ranline_out


!!--------------------------- ranline help1 -------------------------------!!
!!--------------------------- ranline help1 -------------------------------!!
!!--------------------------- ranline help1 -------------------------------!!

! WORKS WITH BINS AND COORDINATES FOR CONNECTED AND SEPARATE LINE SEGMENTS.
! USER MUST SET obj%izero=0 AND obj%index=1 BEFORE STARTING TO CALL
!  RANLINE_HELP1 AND RANLINE_HELP2 AND RANLINE_HELP3.
! output: obj%nseg obj%nbin obj%segment.


      subroutine ranline_help1 (obj,  error)
      type(ranline_struct),intent(inout) :: obj                    ! arguments
      logical             ,intent(out)   :: error                  ! arguments
      real                               :: frac                   ! local
      integer                            :: indx,number            ! local
      real                               :: xlo,xup,ylo,yup,dx,dy  ! local
      real                               :: theta,cost,sint        ! local
      real                               :: xstep,ystep,dist       ! local
      logical                            :: newstart               ! local
      character(len=12)                  :: newstart2              ! local

!----------PRINT HEADER.

      write(lun,*) ' '
      write(lun,1000)
1000  format (' SEGMENT                    XLO       YLO    ',  &
              '     XUP       YUP      XSTEP   YSTEP   #BINS    ANGLE')

!----------RETURN NUMBER OF BINS IN ALL LINE SEGMENTS.

      frac     = 0.0
      obj%nseg = 0
      obj%nbin = 0
      newstart = .true.

      do indx = 2,obj%ncoord
           if (obj%x_coord(indx)   == 0.0 .and. &
               obj%y_coord(indx)   == 0.0) then
                newstart = .true.
                cycle
           end if
           if (obj%x_coord(indx-1) == 0.0 .and. &
               obj%y_coord(indx-1) == 0.0) then
                newstart = .true.
                cycle
           end if
           if (obj%x_coord(indx) == obj%x_coord(indx-1) .and. &
               obj%y_coord(indx) == obj%y_coord(indx-1)) cycle   
           XLO   = obj%x_coord(indx-1)
           YLO   = obj%y_coord(indx-1)
           XUP   = obj%x_coord(indx)
           YUP   = obj%y_coord(indx)
           DX    = (XUP-XLO) * obj%feet_x
           DY    = (YUP-YLO) * obj%feet_y
           THETA = ATAN2(DY,DX)
           COST  = COS(THETA)
           SINT  = SIN(THETA)
           XSTEP = obj%feet_out * COST / obj%feet_x
           YSTEP = obj%feet_out * SINT / obj%feet_y
           XLO   = XLO + frac * XSTEP
           YLO   = YLO + frac * YSTEP
           DX    = (XUP - XLO) * obj%feet_x
           DY    = (YUP - YLO) * obj%feet_y
           DIST  = SQRT(DX**2 + DY**2) / obj%feet_out
           if (indx == obj%ncoord) then
                number = DIST + 1.5
                frac   = 0.0
           else if (obj%x_coord(indx+1) == 0.0 .and. &
                    obj%y_coord(indx+1) == 0.0) then
                number = DIST + 1.5
                frac   = 0.0
           else
                number = DIST + 1.0
                frac   = number - DIST
           end if
           obj%nbin = obj%nbin + number
           XUP = XLO + (number-1)*XSTEP
           YUP = YLO + (number-1)*YSTEP

!----------SAVE INFORMATION IN STRUCTURE ARRAY.

           obj%nseg = obj%nseg + 1
           obj%segment(obj%nseg)%xlo      = XLO
           obj%segment(obj%nseg)%ylo      = YLO
           obj%segment(obj%nseg)%xstep    = XSTEP
           obj%segment(obj%nseg)%ystep    = YSTEP
           obj%segment(obj%nseg)%number   = number
           obj%segment(obj%nseg)%newstart = newstart

!----------PRINT INFO FOR THIS LINE SEGMENT.

           if (newstart) then
                newstart2 = 'new start:'
           else
                newstart2 = 'continuing:'
           end if

           write(lun,2000) obj%nseg,newstart2,XLO,YLO,XUP,YUP,XSTEP,YSTEP,  &
                           number,THETA*DEGREES_PER_RADIAN
2000       format (I7,2x,a11,2(F12.2,F10.2),F10.2,F8.2,I8,F9.2)

           newstart = .false.
      end do

      if (obj%nseg == 0) then
           call pc_error ('NO LINE SEGMENTS SPECIFIED')
           error = .true.
           return
      end if
      call pc_print ('TOTAL NUMBER OF TRACES TO OUTPUT =',obj%nbin)
      call pc_print (' ')
      error = .false.
      end subroutine ranline_help1


!!--------------------------- ranline help2 -------------------------------!!
!!--------------------------- ranline help2 -------------------------------!!
!!--------------------------- ranline help2 -------------------------------!!

! WORKS WITH BINS AND COORDINATES FOR CONNECTED AND SEPARATE LINE SEGMENTS.
! USER MUST SET obj%izero=0 AND obj%index=1 BEFORE STARTING TO CALL
!  RANLINE_HELP1 AND RANLINE_HELP2 AND RANLINE_HELP3.


      subroutine ranline_help2 (obj,HX,HY,  error)
      type(ranline_struct),intent(inout) :: obj                     ! arguments
      double precision    ,intent(in)    :: hx,hy                   ! arguments
      logical             ,intent(out)   :: error                   ! arguments
      integer                            :: indx,izero2,ilo,iup,ii  ! local
      integer                            :: ixlo,ixup,ibbb,idist    ! local
      integer                            :: iylo,iyup               ! local
      integer                            :: number                  ! local
      real                               :: xlook,ylook,dist2,dx,dy ! local
      real                               :: xstep,ystep             ! local
      real                               :: xlo,ylo,xp,yp           ! local

!----------FIND AND SAVE THE BINS THIS TRACE CONTRIBUTES TO.

      XLOOK  = obj%search_x + 0.00001
      YLOOK  = obj%search_y + 0.00001
      IZERO2 = 0

      do indx = 1,obj%nseg

!----------GET INFORMATION FROM STRUCTURE ARRAY.

           XLO    = obj%segment(indx)%xlo
           YLO    = obj%segment(indx)%ylo
           XSTEP  = obj%segment(indx)%xstep
           YSTEP  = obj%segment(indx)%ystep
           number = obj%segment(indx)%number

!----------DO THE WORK.

           XP = HX - XLO
           YP = HY - YLO
           call ranline_test (XP,XLOOK,XSTEP,number,   IXLO,IXUP)
           call ranline_test (YP,YLOOK,YSTEP,number,   IYLO,IYUP)
           ilo = max(IXLO,IYLO)
           iup = min(IXUP,IYUP)
           do II = ilo,iup
                DX        = XP - (II-1)*XSTEP
                DY        = YP - (II-1)*YSTEP
                IBBB      = IZERO2 + II
                DIST2     = (DX/XLOOK)**2 + (DY/YLOOK)**2
                IDIST     = nint(DISTFACTOR*SQRT(DIST2))
                obj%kount = obj%kount + 1
                call triplemerge_write &
                                  (obj%triplemerge,IBBB,IDIST,obj%key,error)
                if (error) then
                    call pc_error ('RANLINE: FATAL ERROR IN TRIPLEMERGE_WRITE')
                    error = .true.
                    return
                end if
           end do
           IZERO2 = IZERO2 + number
      end do

      error = .false.
      end subroutine ranline_help2


!!--------------------------- ranline help3 -------------------------------!!
!!--------------------------- ranline help3 -------------------------------!!
!!--------------------------- ranline help3 -------------------------------!!

! WORKS WITH BINS AND COORDINATES FOR CONNECTED AND SEPARATE LINE SEGMENTS.
! USER MUST SET obj%izero=0 AND obj%index=1 BEFORE STARTING TO CALL
!  RANLINE_HELP1 AND RANLINE_HELP2 AND RANLINE_HELP3.


      subroutine ranline_help3 (obj,  xmid,ymid,error)
      type(ranline_struct),intent(inout) :: obj                  ! arguments
      real                ,intent(out)   :: xmid,ymid            ! arguments
      logical             ,intent(out)   :: error                ! arguments
      integer                            :: number,ii            ! local
      real                               :: xlo,ylo,xstep,ystep  ! local

!----------GET COORDINATES OF THIS BIN.

      do
           II = obj%ibin - obj%izero
           number = obj%segment(obj%index)%number
           if (II > number) then
                obj%izero = obj%izero + number
                obj%index = obj%index + 1
                if (obj%index > obj%nseg) then
                     call pc_error ('RANLINE: FATAL ERROR IN RANLINE_HELP3')
                     call pc_error &
                            ('INDEX',obj%index,'SHOULD BE <= NSEG',obj%nseg)
                     error = .true.
                     return
                end if
           else if (II < 1) then
                obj%index = obj%index - 1
                if (obj%index < 1) then
                     call pc_error ('RANLINE: FATAL ERROR IN RANLINE_HELP3')
                     call pc_error ('INDEX',obj%index,'SHOULD BE >= 1')
                     error = .true.
                     return
                end if
                number    = obj%segment(obj%index)%number
                obj%izero = obj%izero - number
           else
                XLO   = obj%segment(obj%index)%xlo
                YLO   = obj%segment(obj%index)%ylo
                XSTEP = obj%segment(obj%index)%xstep
                YSTEP = obj%segment(obj%index)%ystep
                xmid = XLO + (II-1)*XSTEP
                ymid = YLO + (II-1)*YSTEP
                exit
           end if
      end do

      error = .false.
      end subroutine ranline_help3


!!--------------------------- ranline test -------------------------------!!
!!--------------------------- ranline test -------------------------------!!
!!--------------------------- ranline test -------------------------------!!

!     RETURNS INDICES FOR WHICH POINT XP IS WITHIN DISTANCE XLOOK
!        FROM POINTS GIVEN BY XSTEP*(obj%index-1), WHERE INDEX RANGES
!        FROM 1 TO NUMBER.
!     RETURNS ilo > iup IF NO SUCH POINTS EXIST.


      subroutine ranline_test (XP,XLOOK,XSTEP,number,   ilo,iup)
      real   ,intent(in)  :: xp,xlook,xstep              ! arguments
      integer,intent(in)  :: number                      ! arguments
      integer,intent(out) :: ilo,iup                     ! arguments
      real                :: xlook2                      ! local

!----------CHECK FOR ZERO STEP.

      if (XSTEP == 0.0) then
           if (abs(XP) <= XLOOK) then
                ilo = 1
                iup = number
           else
                ilo = 2                ! there are no valid indices.
                iup = 1                ! there are no valid indices.
           end if

!----------DO THE WORK.

      else
           XLOOK2 = XLOOK
           if (XSTEP < 0.0) XLOOK2 = -XLOOK
           ilo = (XP - XLOOK2)/XSTEP + 2.0
           iup = (XP + XLOOK2)/XSTEP + 1.0
           if (ilo <= number .and. iup >= 1) then
                ilo = min(number,max(1,ilo))
                iup = min(number,max(1,iup))
           else
                ilo = 2                ! there are no valid indices.
                iup = 1                ! there are no valid indices.
           end if
      end if
      end subroutine ranline_test


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module ranline_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

