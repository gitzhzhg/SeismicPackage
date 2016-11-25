!<CPS_v1 type="PROCESS"/>
!
!!------------------------------ tdc.f90 ---------------------------------!!
!!------------------------------ tdc.f90 ---------------------------------!!
!!------------------------------ tdc.f90 ---------------------------------!!

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
!
!<brief_doc>
!------------------------------------------------------------------------------
!                         C P S   P R O C E S S
!
! Name       : tdc
! Category   : Transforms
! Written    : 2000-09-19   by: Douglas Hanson
! Revised    : 2007-04-12   by: Douglas Hanson Datumgrid changes.
! Maturity   : beta
! Purpose    : Time to Depth Conversion.
! Portability: No known limitations.
! Parallel   : Yes
!
!------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! TDC converts an input trace from time to depth or from depth to time
! or generates velocity traces using an interval velocity model.
!
! TDC has four different combinations of input and output controlled by
! parameters OUTPUT_TYPE and INPUT_UNITS.  
! 
! OUTPUT_TYPE controls whether TDC produces velocity traces at the input trace
! locations or does a time to depth or depth to time transformation of the input
! traces. 
! 
! INPUT_UNITS indicates what vertical units the input traces are in, time or 
! depth.  Combined with OUTPUT_TYPE this defines the vertical units of the 
! output traces as well.
! 
! The four combinations of input and output are:
! 
! 1) OUTPUT_TYPE=VELOCITY and INPUT_UNITS=TIME  data(time ) -> velocity(time )
! 2) OUTPUT_TYPE=VELOCITY and INPUT_UNITS=DEPTH data(depth) -> velocity(depth)
! 3) OUTPUT_TYPE=DATA     and INPUT_UNITS=TIME  data(time ) -> data(depth)
! 4) OUTPUT_TYPE=DATA     and INPUT_UNITS=DEPTH data(depth) -> data(time )
! 
! The combined values of OUTPUT_TYPE and INPUT_UNITS affect some of the 
! other input parameters that are sensitive and / or visible, that is parameters
! the user can change and / or see.
! 
! Parameters, OUTPUT_UNITS, TIM_USE and DEP_USE are for information purposes
! only and will always be insensitive or invisible. 
! 
! Parameter OUTPUT_UNITS defines the type of vertical unit the output data is 
! in.  It can have values of TIME and DEPTH.  
! OUTPUT_UNITS is provided for information only, the user can not directly
! change the value of OUTPUT_UNITS instead it will be determined by the combined
! values of OUTPUT_TYPE and INPUT_UNITS. 
! 
! Parameter TIM_USE indicates the type of use TDC will make of the TIME 
! input parameters: TIM_TOT, TIM_INIT, TIM_LAST and TIM_INC.  It can have 
! values of BOTH, INPUT, OUTPUT and NOT_USED.
! TIM_USE is provided for information only, the user can not directly change 
! the value of TIM_USE instead it will be determined by the combined values of
! OUTPUT_TYPE and INPUT_UNITS. 
! TIM_USE will always be insensitive, and will be invisible if the user chooses
! to produce velocity traces in depth. 
! 
! Parameter DEP_USE indicates the type of use TDC will make of the DEPTH 
! input parameters: DEP_TOT, DEP_INIT, DEP_LAST and DEP_INC.  It can have 
! values of BOTH, INPUT, OUTPUT and NOT_USED.
! DEP_USE is provided for information only, the user can not directly change
! the value of DEP_USE, instead it will be determined by the combined values 
! of OUTPUT_TYPE and INPUT_UNITS. 
! DEP_USE will always be insensitive, and will be invisible if the user
! chooses to produce velocity traces in time. 
! 
! The following notes detail the four different combinations of OUTPUTE_TYPE 
! and INPUT_UNITS.  
! 
! The number of input  global samples will be refered to as  NDPT_INPUT
! The input  global sample increment  will be refered to as    DT_INPUT
! The input  global sample origin     will be refered to as TSTRT_INPUT
! The number of output global samples will be refered to as  NDPT_OUTPUT
! The output global sample increment  will be refered to as    DT_OUTPUT
! The output global sample origin     will be refered to as TSTRT_OUTPUT
!
! Also in all cases:
!
! TIM_LAST   = (TIM_TOT  - 1) * TIM_INC  + TIM_INIT
! DEP_LAST  = (DEP_TOT - 1) * DEP_INC + DEP_INIT
! 
! 1) OUTPUT_TYPE=VELOCITY and INPUT_UNITS=TIME  
! 
! TDC prodcues a velocity trace in time at the input trace location.
! Both the input and output traces are in time,
!
! The input and output time parameters are defined by the input global time 
! values.  The depth parameters are not used and are invisible.
!
!  TIM_TOT =  NDPT_INPUT                 NDPT_OUTPUT =  TIM_TOT
!  TIM_INIT = TSTRT_INPUT                TSTRT_OUTPUT =  TIM_INIT 
!  TIM_INC =    DT_INPUT                   DT_OUTPUT =  TIM_INC 
!
! OUTPUT_UNITS = TIME, TIM_USE = BOTH, DEP_USE = NOT_USED 
! 
! 2) OUTPUT_TYPE=VELOCITY and INPUT_UNITS=DEPTH
! 
! TDC prodcues a velocity trace in depth at the input trace location.
! Both the input and output traces are in depth.
!
! The input and output depth parmeters are defined by the input global time 
! values and DEPTH_SCALE.  The time parameters are not used and are invisible.
!
! DEP_TOT =  NDPT_INPUT                 NDPT_OUTPUT = DEP_TOT
! DEP_INIT = TSTRT_INPUT * DEPTH_SCALE  TSTRT_OUTPUT = DEP_INIT / DEPTH_SCALE
! DEP_INC =    DT_INPUT * DEPTH_SCALE     DT_OUTPUT = DEP_INC / DEPTH_SCALE
! 
! OUTPUT_UNITS = DEPTH, TIM_USE = NOT_USED, DEP_USE = BOTH
! 
! 3) OUTPUT_TYPE=DATA and INPUT_UNITS=TIME  
! 
! TDC does a time to depth conversion of the input trace.
! The input trace is in time and the output trace is in depth.
!
! The input time parameters are defined by the input global time values and are 
! insensitive.
! The output global parameters are defined by the output depth parameter values
! scaled by 1. / DEPTH_SCALE.
!
!  TIM_TOT =  NDPT_INPUT                 NDPT_OUTPUT = DEP_TOT
!  TIM_INIT = TSTRT_INPUT                TSTRT_OUTPUT = DEP_INIT / DEPTH_SCALE 
!  TIM_INC =    DT_INPUT                   DT_OUTPUT = DEP_INC / DEPTH_SCALE 
!
! OUTPUT_UNITS=DEPTH, TIM_USE=INPUT, DEP_USE=OUTPUT
!
! 4) OUTPUT_TYPE=DATA and INPUT_UNITS=DEPTH
! 
! TDC does a depth to time conversion of the input trace.
! The input trace is in depth and the output trace is in time.
!
! The input depth parameters are defined by the input global time values !
! scaled by DEPTH_SCALE and are insensitive.  
! The output global values are defined by the output time parameters, 
! 
! DEP_TOT =  NDPT_INPUT                 NDPT_OUTPUT =  TIM_TOT
! DEP_INIT = TSTRT_INPUT * DEPTH_SCALE  TSTRT_OUTPUT =  TIM_INIT
! DEP_INC =    DT_INPUT * DEPTH_SCALE     DT_OUTPUT =  TIM_INC
!
! OUTPUT_UNITS=TIME, TIM_USE=OUTPUT, DEP_USE=INPUT
!
! If the user wants velocity traces on a uniform grid, synthetic traces can be
! generated on that grid using process SPIKE and then TDC can be used to create
! velocity traces for those synthetic input traces. 
!
! If the user selects OUTPUT_TYPE=DATA, TDC will interpolate the input trace 
! to a vertical grid INTERPOLATE_INPUT times finer than the input grid.  
! Increasing the value of INTERPOLATE_INPUT results in a more accurate mapping 
! of the input trace to the output trace.  If OUTPUT_TYPE=VELOCITY 
! INTERPOLATE_INPUT will be set to 1.
!
! TDC conducts its computations on a vertical grid INTERPOLATE_OUTPUT times 
! coarser than the output grid.  Increasing INTERPOLATE_OUTPUT reduces memory 
! usage the accuracy of the input to output map.
!
! When OUTPUT_TYPE=VELOCITY TDC interpolates the orignal input interval
! velocity field to the same horizontal grid as the original velocity file and a
! vertical grid INTERPOLATE_OUTPUT times coarser than the output vertical grid. 
!
! When OUTPUT_TYPE=DATA TDC saves the velocity filed on the orignal input
! velocity grid.  TDC computes the time to depth or depth to time mapping
! function on the same horizontal grid as the original velocity file and a
! vertical grid INTERPOLATE_OUTPUT times coarser than the output vertical grid. 
!
! TDC reads the input velocity field and converts into either VZIN, if 
! OUTPUT_UNITS=DEPTH, or VTIN, if OUTPUT_UNITS=TIME, form using standard CPS 
! velocity routines.  The current version of these routines assume constant 
! velocity layers between velocity pick.  Hence these are not yet consistent 
! with the old CPS depth migration velocity filed manipulation assumption.
!
! TDC computes the time to depth or depth to time transformation using a
! spatialy varying zero time depth datum. Eventualy this datum may be entered
! from a file if PATH_DATUM is defined or from a constant depth defined by
! CONST_DATUM. 
!
! Currently this zero time depth datum surface datum is interpolated to the same
! spatial grid as the velocity field.  Eventualy it will preserve the spatial
! grid of the orignal dtaums urface. 
! 
! Currently the CPS velocity reading and conversion routines assume that
! vertical time is measured from zero depth. Hence TDC assumes that the velocity
! field you input follows this same convention and corrects for non zero depth
! datum useing the depth datum information. 
! 
! TDC computes any lateral interpolations of the velocity or mapping fields
! relative to the zero time surface.  This could be slightly different from an
! interpolation from a zero depth surface. 
!
! To ensure you get the correct velocity field I suggest you put the input 
! velocity filed in the correct format, VZIN, if OUTPUT_UNITS=DEPTH, or VTIN, 
! if OUTPUT_UNITS=TIME, using RMOD.
! 
! All internal computations within TDC involving velocity assume a linear
! slowness variation between velocity picks.  This will preserve vertical travel
! time. 
! 
!------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!
!------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
! This process accepts more than one trace at a time.
!
!------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
!------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                          Action taken
! ----      -----------                          ------------
! NUMTR     max number of traces input/output    Not changed
! GATHERED  whether traces are gathered          Not changed
! NWIH      number of words in trace header      used but not changed
! NDPT      number of sample values in trace     used, possibly changed
!
!------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! HDR     Description                Action taken
! ----    -----------                ------------
! 2       Head mute                  Used and modified.
! 64      Tail mute                  Used and modified.
!
!------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author         Description
!     ----        ------         -----------
! 27  2007-04-12  Douglas Hanson Datumgrid changes.
! 26  2006-12-11  Douglas Hanson Improve efficiency, make parallel.
!                                Add OPT_VEL_INTERPOLATE.
! 25  2006-06-27  D. Glover      Added NULLIFY statements for Intel compiler.
! 24  2006-06-12  B. Menger      Removed Unused Variables.
! 23  2005-02-03  R.S.Day        Initialize xyz_inp when opt_vel=CONST.
! 22  2005-01-31  Douglas Hanson Add INTERPOLATE_ORIGINAL.
! 21  2004-09-14  R.S.Day        Promoted to beta.
! 20  2004-06-22  R.S.Day        Update for more interpolate module changes
! 19  2004-06-10  R.S.Day        Update for interpolate module changes
! 18  2004-03-15  R.S.Day        Issue run time warning if undersampled
! 17  2004-03-04  R.S.Day        Zero data that is stretched beyond end of input
! 16  2004-02-19  Douglas Hanson Reorganize code for efficiency.
! 15  2004-02-16  Douglas Hanson Buffer map function with trbuf. Use buffered
!                                modgrid reads for the input model.
! 14  2002-09-09  Douglas Hanson Set hd_lav header word. BR 843.
! 13  2002-04-22  Douglas Hanson Adjust parameter sensitivity. BR 738.
!     2002-04-04  Douglas Hanson Add SELECT_PATH_VEL tip.
! 12  2002-04-03  Douglas Hanson Add OPT_VEL and OPT_DATUM parameters.
! 11  2001-12-14  Douglas Hanson Fix velocity output bug.
! 10  2001-11-05  Douglas Hanson lengthen gui file name fields.
!  9  2001-10-30  Douglas Hanson Set surfacegrid_read file_grid=.false. 
!  8  2001-10-26  Karen Goodger  Change labels beginning with if to get around
!                                intel compiler bug.
!  7  2001-08-23  Douglas Hanson Add pathchoose.
!  6  2001-06-26  Douglas Hanson Use original velocity sampling.
!  5  2001-06-21  Douglas Hanson Use datumgrid.
!  4  2001-01-08  Douglas Hanson Add TIM_USE, DEP_USE defaults.
!  3  2001-01-04  Douglas Hanson Add skip_wrapup 
!  2  2000-09-22  Douglas Hanson Add time and depth use flags.
!  1  2000-09-19  Douglas Hanson Original Version
!
!------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     
! NEED_LABEL     false     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE        2*NDPT     small   amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!    NTR ignored on input because this is a trace supplying process.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS tdc_1/NC=80>
!
! Time to Depth Conversion
!
! OPT_VEL~~=`CCCCCCCCCC  MEMORY_SIZE=`IIIIIIIIII 
! CONST_VEL=`FFFFFFFF    DEPTH_SCALE=`FFFFFFFF 
! Select PATH_VEL [PATH_VEL]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! INTERPOLATE_ORIGINAL=`CC       INTERPOLATE_INVERSE=`CC
! INTERPOLATION_TYPE~~=`CCCCCCC  OPT_VEL_INTERPOLATE=`CCCCCCCCCCC
! INTERPOLATION_INPUT =`IIIIIII  NUM_VEL_INTERPOLATE=`IIIIIIIIIII
! INTERPOLATION_OUTPUT=`IIIIIII  OPT_MAP_INTERPOLATE=`CC
!
! OUTPUT_TYPE =`CCCCCCC  
! INPUT_UNITS =`CCCCCCC  
! OUTPUT_UNITS=`CCCCCCC   
!
! TIM_USE =`SSSSSSS   DEP_USE =`SSSSSSS
! TIM_TOT =`IIIIIIII  DEP_TOT =`IIIIIIII
! TIM_INIT=`FFFFFFFF  DEP_INIT=`FFFFFFFF
! TIM_LAST=`FFFFFFFF  DEP_LAST=`FFFFFFFF
! TIM_INC =`FFFFFFFF  DEP_INC =`FFFFFFFF
!
!<include surfacegrid.f90>
!
!<PARMS tdc_1[screen1]>
!
!</gui_def>
!<HelpSection>
!
!<Help KEYWORD="TIM_USE">
!<Tip> Time domain usage flag. </Tip>
! Default = INPUT
! Allowed = INPUT, OUTPUT, BOTH, NOT_USED
! Parameter TIM_USE indicates the type of use TDC will make of the TIME 
!</Help>
!
!<Help KEYWORD="TIM_TOT">
!<Tip> Number of time samples. </Tip>
! Default = 1001
! Allowed = int>=0
! Not used when OUTPUT_UNITS=DEPTH
!</Help>
!
!<Help KEYWORD="TIM_INIT">
!<Tip> Minimum time value. </Tip>
! Default = 0
! Allowed = real scalar
! Not used when OUTPUT_UNITS=DEPTH
!</Help>
!
!<Help KEYWORD="TIM_LAST">
!<Tip> Maximum time value. </Tip>
! Default = 4
! Allowed = real scalar
! Not used when OUTPUT_UNITS=DEPTH
!</Help>
!
!<Help KEYWORD="TIM_INC">
!<Tip> Time sample increment. </Tip>
! Default = .004
! Allowed = real scalar
! Not used when OUTPUT_UNITS=DEPTH
!</Help>
!
!<Help KEYWORD="DEP_USE">
!<Tip> Depth domain usage flag. </Tip>
! Default = OUTPUT
! Allowed = INPUT, OUTPUT, BOTH, NOT_USED
! Parameter DEP_USE indicates the type of use TDC will make of the DEPTH 
!</Help>
!
!<Help KEYWORD="DEP_TOT">
!<Tip> Number of depth samples. </Tip>
! Default = 1001
! Allowed = int>=0
! Not used when OUTPUT_UNITS=TIME
!</Help>
!
!<Help KEYWORD="DEP_INIT">
!<Tip> Minimum depth value. </Tip>
! Default = 0
! Allowed = real scalar
! Not used when OUTPUT_UNITS=TIME
!</Help>
!
!<Help KEYWORD="DEP_LAST">
!<Tip> Maximum depth value. </Tip>
! Default = 4000
! Allowed = real scalar
! Not used when OUTPUT_UNITS=TIME
!</Help>
!
!<Help KEYWORD="DEP_INC">
!<Tip> Depth value increment. </Tip>
! Default = 4
! Allowed = real scalar
! Not used when OUTPUT_UNITS=TIME
!</Help>
!
!<Help KEYWORD="MEMORY_SIZE">
!<Tip> How much memory to use for the map function, in words. </Tip>
! Default = -1
! Allowed = integer -1, >0   -1 means hold the entire map function in memory.
! tdc will hold at least two rows of x columns in memory. If the map is cached
! on disk, efficiency will be better if the velocity model is in the same order
! as the trace data being input to TDC. 
!</Help>
!
!<Help KEYWORD="OPT_VEL">
!<Tip> Whether to use constant or spatialy varying velocity. </Tip>
! Default = PATH
! Allowed = CONSTANT  Use a constant velocity value of CONST_VEL.
! Allowed = PATH      Use a spatialy varying velocity defined by PATH_VEL.
!</Help>
!
!<Help KEYWORD="OPT_DATUM">
!<Tip> Whether to use constant or spatialy varying datum. </Tip>
! Default = PATH
! Allowed = CONSTANT  Use a constant velocity value of CONST_VEL.
! Allowed = PATH      Use a spatialy varying velocity defined by PATH_VEL.
!</Help>
!
!<Help KEYWORD="CONST_VEL">
!<Tip> Constant velocity to use if no velocity file is input. </Tip>
! Default = 2000
! Allowed = real scalar
! If PATH_VEL is not defined the depth at which time=0 is constant and 
! is CONST_VEL
!</Help>
!
!<Help KEYWORD="PATH_VEL">
!<Tip> velocity file path name.</Tip>
! Default = None
! Allowed = Character
! Allowed = Character
!</Help>
!
!<Help KEYWORD="SELECT_PATH_VEL">
!<Tip> Select velocity file path name.</Tip>
! Default = None
! Allowed = Character
! Allowed = Character
!</Help>
!
!<Help KEYWORD="CONST_DATUM">
!<Tip> Constant datum depth to use if no datum file is input. </Tip>
! Default = 0
! Allowed = real scalar
! If PATH_DATUM is not defined the depth at which time=0 is constant and 
! is CONST_DATUM.
! If PATH_DATUM is defined the depth at which time=0 is spatialy varying
! and is interpolated from the values in the PATH_DATUM file.
!</Help>
!
!<Help KEYWORD="SELECT_PATH_DATUM">
!<Tip> Select datum file path name.</Tip>
! Default = NONE
! Allowed = Character
! If PATH_DATUM is not defined the depth at which time=0 is constant and 
! is CONST_DATUM.
! If PATH_DATUM is defined the depth at which time=0 is spatialy varying
! and is interpolated from the values in the PATH_DATUM file.
!</Help>
!
!<Help KEYWORD="PATH_DATUM">
!<Tip> Datum file path name.</Tip>
! Default = NONE
! Allowed = Character
! If PATH_DATUM is not defined the depth at which time=0 is constant and 
! is CONST_DATUM.
! If PATH_DATUM is defined the depth at which time=0 is spatialy varying
! and is interpolated from the values in the PATH_DATUM file.
!</Help>
!
!<Help KEYWORD="DEPTH_SCALE">
!<Tip> Depth scaling factor. </Tip>
! Default = 1000
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="OUTPUT_TYPE">
!<Tip> Type of output. </Tip>
! Default = DATA     - output converted input data trace
! Allowed = VELOCITY - output velocity traces.
!</Help>
!
!<Help KEYWORD="OUTPUT_UNITS">
!<Tip> Output units, TIME or DEPTH. </Tip>
! Default = DEPTH
! Allowed = DEPTH - input is in TIME  units, output is in DEPTH units.
! Allowed = TIME  - input is in DEPTH units, output is in TIME  units.
!</Help>
!
!<Help KEYWORD="INPUT_UNITS">
!<Tip> Input units, TIME or DEPTH. </Tip>
! Default = TIME
! Allowed = TIME   - input is in TIME  units, output is in DEPTH units.
! Allowed = DEPTH  - input is in DEPTH units, output is in TIME  units.
!</Help>
!
!<Help KEYWORD="INTERPOLATE_ORIGINAL">
!<Tip> How to interpolate the model coefficient. </Tip>
! Default = NO
! Allowed = NO     - Interpolate the inverse of the model coefficient.
! Allowed = YES    - Interpolate the model coefficient.
! Use INTERPOLATE_ORIGINAL = NO  and INTERPOLATE_INVERSE = YES 
! when trying to output interval velocity.
! Use INTERPOLATE_ORIGINAL = YES and INTERPOLATE_INVERSE = NO 
! when trying to output coefficients other than interval velocity.
! This should only be done with INPUT_UNITS=DEPTH and OUTPUT_TYPE=VELOCITY
!</Help>
!
!<Help KEYWORD="INTERPOLATE_INVERSE">
!<Tip> How to interpolate the model coefficient. </Tip>
! Default = NO
! Allowed = NO     - Interpolate the inverse of the model coefficient.
! Allowed = YES    - Interpolate the model coefficient.
! Use INTERPOLATE_ORIGINAL = NO  and INTERPOLATE_INVERSE = YES 
! when trying to output interval velocity.
! Use INTERPOLATE_ORIGINAL = YES and INTERPOLATE_INVERSE = NO 
! when trying to output coefficients other than interval velocity.
! This should only be done with INPUT_UNITS=DEPTH and OUTPUT_TYPE=VELOCITY
!</Help>
!
!<Help KEYWORD="INTERPOLATION_TYPE">
!<Tip> Type of input interpolation. </Tip>
! Default = FFT
! Allowed = FFT    - The input trace is interpolated by an fft.
! Allowed = LINEAR - The input trace is interpolated by linear interplation.
! When OUTPUT_TYPE=VELOCITY, INTERPOLATION_TYPE=LINEAR.
!</Help>
!
!<Help KEYWORD="OPT_VEL_INTERPOLATE">
!<Tip> Type of velocity interpolation. </Tip>
! Default = LIN_SLOWNESS 
! Allowed = LIN_SLOWNESS - interpolate linear slowness
! Allowed = LIN_VELOCITY - interpolate linear velocity
! Allowed = NODE_DOWN    - set the velocity below a node equal to that node.
! Allowed = NODE_UP      - set the velocity above a node equal to that node.
!
! The combination of OPT_VEL_INTERPOLATE and NUM_VEL_INTERPOLATE allows TDC 
! to assume different velocity variations between defined velocity 
! node locations.  This allows the user to correct for difference between
! different imaging algorithms which treat velocity fields differently.
! 
! If NUM_VEL_INTERPOLATE > 1 TDC will interpolate the input velocity field
! to a grid that is NUM_VEL_INTERPOLATE times finer than the original 
! vertical grid.
! 
! The manner in which TDC does the interpolation is controled by parameter
! OPT_VEL_INTERPOLATE.
! 
! if OPT_VEL_INTERPOLATE = LIN_SLOWNESS 
! TDC interpolates linear slowness between nodes this the historical behaviour
! and should produce the same results regardless of the value of 
! NUM_VEL_INTERPOLATE
! 
! if OPT_VEL_INTERPOLATE = LIN_VELOCITY 
! TDC interpolates linear velocity between nodes this would emulate the 
! behavior of algorithms that assume velocity changes linearly 
! between velocity nodes.
! 
! if OPT_VEL_INTERPOLATE = NODE_DOWN
! TDC sets the velocity at fine nodes interpolated between two coarse nodes 
! equal to the velocity of the upper node.
! this should make the velocity look slower and move points at a given time 
! to smaller depths or move points at a given depth to a larger travel time.
! 
! if OPT_VEL_INTERPOLATE = NODE_UP
! TDC sets the velocity at fine nodes interpolated between two coarse nodes 
! equal to the velocity of the lower node.
! this should make the velocity look faster and move points at a given time 
! to larger depths or move points at a given depth to a smaller travel time.
! 
!</Help>
!
!<Help KEYWORD="NUM_VEL_INTERPOLATE">
!<Tip> Order of velocity interpolation. </Tip>
! Default = 1
! Allowed = int>0
! 
! The combination of OPT_VEL_INTERPOLATE and NUM_VEL_INTERPOLATE allows TDC 
! to assume different velocity variations between defined velocity 
! node locations.  This allows the user to correct for difference between
! different imaging algorithms which treat velocity fields differently.
! 
! If NUM_VEL_INTERPOLATE > 1 TDC will interpolate the input velocity field
! to a grid that is NUM_VEL_INTERPOLATE times finer than the original 
! vertical grid.
! 
! The manner in which TDC does the interpolation is controled by parameter
! OPT_VEL_INTERPOLATE.
! 
! if OPT_VEL_INTERPOLATE = LIN_SLOWNESS 
! TDC interpolates linear slowness between nodes this the historical behaviour
! and should produce the same results regardless of the value of 
! NUM_VEL_INTERPOLATE
! 
! if OPT_VEL_INTERPOLATE = LIN_VELOCITY 
! TDC interpolates linear velocity between nodes this would emulate the 
! behavior of algorithms that assume velocity changes linearly 
! between velocity nodes.
! 
! if OPT_VEL_INTERPOLATE = NODE_DOWN
! TDC sets the velocity at fine nodes interpolated between two coarse nodes 
! equal to the velocity of the upper node.
! this should make the velocity look slower and move points at a given time 
! to smaller depths or move points at a given depth to a larger travel time.
! 
! if OPT_VEL_INTERPOLATE = NODE_UP
! TDC sets the velocity at fine nodes interpolated between two coarse nodes 
! equal to the velocity of the lower node.
! this should make the velocity look faster and move points at a given time 
! to larger depths or move points at a given depth to a smaller travel time.
! 
!</Help>
!
!<Help KEYWORD="OPT_MAP_INTERPOLATE">
!<Tip> Type of interpolation during map computation. </Tip>
! Default = NEW
! Allowed = NEW - Use new more efficient interpolation.
! Allowed = OLD - Use original efficient interpolation.
!
!</Help>
!
!<Help KEYWORD="INTERPOLATION_INPUT">
!<Tip> The input trace is interpolated by INTERPOLATION_INPUT </Tip>
! Default = 1
! Allowed = int>=1
! When OUTPUT_TYPE=VELOCITY, INTERPOLATION_INPUT=1.
!</Help>
!
!<Help KEYWORD="INTERPOLATION_OUTPUT">
!<Tip> Veritcal computational grid ration. </Tip>
! Default = 10
! Allowed = real integer
! TDC computes and saves a precomputed function on a
! computational grid sampled on the same x,y grid as the input velocity field 
! and a vertical grid whose sample interval is:
! TIM_INC  * INTERPOLATION_OUTPUT when OUTPUT_UNITS=TIME,
! and
! DEP_INC * INTERPOLATION_OUTPUT when OUTPUT_UNITS=DEPTH.,
! Making INTERPOLATION_OUTPUT smaller will increase the accuracy of the time to
! depth or depth to time transformation at the cost of greater cpu and memory
! usage. 
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!


module tdc_module
  !
  ! Module references
  !
  use cpucount_module
  use datumgrid_module
  use headsave_module
  use interpolate_module
  use lav_module
  use matfun_module
  use memfun_module
  use modgrid_module
  use named_constants_module
  use pathcheck_module
  use pathchoose_module
  use pattern_module
  use getsys_module
  use putsys_module
  use getlun_module
  use pc_module
  use pcpsx_module
  use prnfun_module
  use string_module
  use surfacegrid_module
  use timeglob_module
  use trbuf_module
  use velgrid_module
  !
  implicit none
  !
  private
  !
  public :: tdc_create                 ! create object
  public :: tdc_delete                 ! delete object
  public :: tdc_initialize             ! initalize parameters
  public :: tdc_update                 ! update parameters
  public :: tdc_wrapup                 ! wrapup
  public :: tdc                        ! main oepration shell
  public :: tdc_set_input_output       ! set output_type, input_units, ...
  public :: tdc_map_define             ! define the map grid
  public :: tdc_map_compute            ! compute the map function
  public :: tdc_map_compute_v0         ! read the velocity file
  public :: tdc_map_compute_v1         ! compute the vel function at 1 x,y
  public :: tdc_map_compute_m1         ! compute the map function at 1 x,y
  public :: tdc_map_apply              ! apply the map function
  public :: tdc_map_mute               ! map a mute index
  public :: tdc_map_print              ! print the map function
  !
  ! rcs identifier string
  character(len=100),public,save :: tdc_ident = &
  '$Id: tdc.f90,v 1.27 2007/04/13 14:10:14 Hanson beta sps $'
  !
  type, public :: tdc_struct
    !
    private
    !
    logical                             :: skip_wrapup    ! wrapup flag
    !
    type (interpolate_fft_struct), pointer :: interpolate_fft_obj ! fft interp 
    !
    integer                             :: n0_vel           ! num vel nodes
    !
    integer                             :: hx_vel   ! hdr x velocity value
    integer                             :: nx_vel   ! inc x velocity value
    real                                :: dx_vel   ! inc x velocity value
    real                                :: x0_vel   ! min x velocity value
    !
    integer                             :: hy_vel   ! hdr y velocity value
    integer                             :: ny_vel   ! inc y velocity value
    real                                :: dy_vel   ! inc y velocity value
    real                                :: y0_vel   ! min y velocity value
    !
    integer                             :: hx_map           ! x vel hdr
    integer                             :: nx_map           ! x vel num
    real                                :: x0_map           ! x vel min
    real                                :: dx_map           ! x vel inc
    !
    integer                             :: hy_map           ! y vel hdr
    integer                             :: ny_map           ! y vel num
    real                                :: y0_map           ! y vel min
    real                                :: dy_map           ! y vel inc
    !
    integer                             :: nt_vel           ! t,z vel num
    real                                :: t0_vel           ! t,z vel min
    real                                :: dt_vel           ! t,z vel inc
    real                                :: t1_vel           ! t,z vel max
    !
    integer                             :: nt_crs           ! t,z qqq num
    real                                :: t0_crs           ! t,z qqq min
    real                                :: dt_crs           ! t,z qqq inc
    real                                :: t1_crs           ! t,z qqq max
    !
    integer,                  pointer   :: i1_inp(:)  ! inp to fft index 1
    integer,                  pointer   :: i2_inp(:)  ! inp to fft index 2
    real,                     pointer   :: f1_inp(:)  ! inp to fft coeff 1
    real,                     pointer   :: f2_inp(:)  ! inp to fft coeff 2
    !
    integer,                  pointer   :: i1_out(:)  ! map to out index 1
    integer,                  pointer   :: i2_out(:)  ! map to out index 2
    real,                     pointer   :: f1_out(:)  ! map to out coeff 1
    real,                     pointer   :: f2_out(:)  ! map to out coeff 2
    !
    integer,                  pointer   :: i1_vel(:)  ! qqq to vel index 1
    integer,                  pointer   :: i2_vel(:)  ! qqq to vel index 2
    real,                     pointer   :: f1_vel(:)  ! qqq to vel coeff 1
    real,                     pointer   :: f2_vel(:)  ! qqq to vel coeff 2
    !
    character(len=filename_length)      :: path_vel   ! velocity file name
    type(pathchoose_struct),pointer     :: path_vel_button
    type(pathchoose_struct),pointer     :: path_datum_button
    !
    integer                             :: memory_size   ! map mem size
    character(len=8)                    :: opt_vel       ! vel   option
    real                                :: const_vel     ! constant velocity
    real                                :: depth_scale   ! depth scale
    integer                             :: interpolation_input
    integer                             :: interpolation_output
    !
    character(len=8)                    :: tim_use
    integer                             :: tim_tot
    real                                :: tim_init
    real                                :: tim_last
    real                                :: tim_inc
    !
    character(len=8)                    :: dep_use
    integer                             :: dep_tot
    real                                :: dep_init
    real                                :: dep_last
    real                                :: dep_inc
    !
    integer                             :: nt_map
    real                                :: t0_map
    real                                :: t1_map
    real                                :: dt_map
    !
    character (len=8)                   :: output_type
    character (len=8)                   :: input_units
    character (len=8)                   :: output_units
    character (len=8)                   :: interpolation_type
    logical                             :: interpolate_original
    logical                             :: interpolate_inverse
    character (len=12)                  :: opt_vel_interpolate
    integer                             :: num_vel_interpolate
    character (len=3)                   :: opt_map_interpolate
    logical                             :: old_map_interpolate
    logical                             :: new_map_interpolate
    !
    character (len=8)                   :: vel_type
    character (len=8)                   :: map_type
    character (len=8)                   :: vel_parm
    !
    integer                             :: i0_inp    ! input  trace counter
    integer                             :: i0_out    ! output trace counter
    !
    integer                             :: nh_inp
    !
    integer                             :: ft_inp    ! inp fft length
    integer                             :: nt_inp
    real                                :: t0_inp
    real                                :: dt_inp
    !
    integer                             :: ft_out    ! out fft length
    integer                             :: nt_out
    real                                :: t0_out
    real                                :: dt_out
    !
    integer                             :: nt_glb
    real                                :: t0_glb
    real                                :: dt_glb
    !
    integer                             :: nt_fft          ! tim fft num
    real                                :: t0_fft          ! tim fft min
    real                                :: dt_fft          ! tim fft inc
    !
    real                                :: r0_min
    real                                :: r0_max
    real                                :: rs_min
    real                                :: rs_max
    integer                             :: mem_lim
    !
    type ( trbuf_struct ),    pointer   :: trbuf           ! trbuf structure
    integer                             :: n2_dim          ! trbuf dim 2 
    integer                             :: nr_dsk          ! trbuf dsk size
    integer                             :: nr_mem          ! trbuf mem size
    integer                             :: jr_dsk          ! trbuf dsk index
    integer                             :: jr_mem          ! trbuf mem index
    character(len=filename_length)      :: trbuf_file_name ! trbuf name
    integer                             :: trbuf_memory    ! trbuf memory 
    logical                             :: l_change        ! trbuf change 
    character(len=64)                   :: user_id
    character(len=filename_length)      :: job_name        ! job name
    !
    integer                             :: mh_inp   ! first dimension of hd_inp
    !
    integer                             :: mt_inp   ! first dimension of tr_inp
    !
    type ( modgrid_struct ),    pointer :: mg_obj
    !
    character(len=4 )                   :: xyz_inp
    character(len=4 )                   :: xyz_out
    !
    type ( headsave_struct ),pointer :: h  ! headsave structure
    !
    type ( cpucount_struct ),   pointer :: c ! cpucount   cpu stat structure
    integer                             :: i_tdc_trbuf_create
    integer                             :: i_tdc_map_compute
    integer                             :: i_tdc_map_compute_vf
    integer                             :: i_tdc_map_compute_tr
    integer                             :: i_tdc_map_compute_v0
    integer                             :: i_tdc_map_compute_vn
    integer                             :: i_tdc_map_compute_v1
    integer                             :: i_tdc_map_compute_m1
    integer                             :: i_tdc_map_compute_ir
    integer                             :: i_tdc_map_apply
    integer                             :: i_tdc_inp_interpolate
    integer                             :: i_tdc_map_interpolate
    integer                             :: i_tdc_out_interpolate
    integer                             :: i_tdc_map_head
    logical                             :: l_lin_slowness
    logical                             :: l_lin_velocity
    logical                             :: l_node_down
    logical                             :: l_node_up
    integer                          :: ipn           ! process number
    type ( surfacegrid_struct ), pointer :: sur  ! surfacegrid structure
    !
  end type tdc_struct
  !
  logical,private,parameter :: homegrown = .true.
  !
  integer,          parameter :: n_yes_no = 2
  character(len=3), save      :: c_yes_no(n_yes_no) &
  = (/ 'YES', 'NO ' /)
  !
  integer,    parameter :: n_opt_const = 2
  character(len=8),save :: c_opt_const(n_opt_const) &
  = (/ 'CONSTANT', 'PATH    ' /)
  !
  integer,          parameter :: n_output_type = 2
  character(len=8), save      :: c_output_type(n_output_type) &
  = (/ 'DATA    ' , 'VELOCITY' /)
  !
  integer,          parameter :: n_input_units = 2
  character(len=8), save      :: c_input_units(n_input_units) &
  = (/ 'TIME    ' , 'DEPTH   ' /)
  !
  integer,          parameter :: n_output_units = 2
  character(len=8), save      :: c_output_units(n_output_units) &
  = (/ 'DEPTH   ' , 'TIME    ' /)
  !
  integer,          parameter :: n_interpolation_type = 2
  character(len=8), save      :: &
  c_interpolation_type(n_interpolation_type) &
  = (/ 'LINEAR  ' , 'FFT     ' /)
  !
  integer,          parameter :: n_opt_vel_interpolate = 4
  character(len=12), save      :: &
  c_opt_vel_interpolate(n_opt_vel_interpolate) &
  = (/ 'LIN_SLOWNESS', 'LIN_VELOCITY', 'NODE_DOWN   ', 'NODE_UP     ' /)
  !
  integer,          parameter :: n_opt_map_interpolate = 2
  character(len=3), save      :: &
  c_opt_map_interpolate(n_opt_map_interpolate) &
  = (/ 'NEW', 'OLD' /)
  !
  type ( tdc_struct ), pointer, save :: object      ! needed for traps.
  logical, save                      :: did_mail = .false.
  !
  contains
  !
  subroutine tdc_create ( o )
    !
    ! create the tdc structure
    !
    type ( tdc_struct ),        pointer :: o                ! tdc structure
    integer                             :: i_err
    !
    i_err = 0
    !
    !print'(" top tdc_create p=",i4)', pcpsx_i_pel()
    !
    allocate ( o )
    !
    nullify ( o%mg_obj )
    nullify ( o%interpolate_fft_obj )
    nullify (o%trbuf) ! jpa
    nullify (o%h) ! jpa
    nullify (o%c) ! jpa
    nullify (o%sur) ! jpa
    nullify (o%i1_inp) ! jpa
    nullify (o%i2_inp) ! jpa
    nullify (o%f1_inp) ! jpa
    nullify (o%f2_inp) ! jpa
    nullify (o%i1_out) ! jpa
    nullify (o%i2_out) ! jpa
    nullify (o%f1_out) ! jpa
    nullify (o%f2_out) ! jpa
    nullify (o%path_vel_button) ! jpa
    nullify (o%path_datum_button) ! jpa
    !
    call memfun_nul ( o%i1_inp )
    call memfun_nul ( o%i2_inp )
    call memfun_nul ( o%f1_inp )
    call memfun_nul ( o%f2_inp )
    call memfun_nul ( o%i1_out )
    call memfun_nul ( o%i2_out )
    call memfun_nul ( o%f1_out )
    call memfun_nul ( o%f2_out )
    call memfun_nul ( o%i1_vel )
    call memfun_nul ( o%i2_vel )
    call memfun_nul ( o%f1_vel )
    call memfun_nul ( o%f2_vel )
    if ( associated ( o%interpolate_fft_obj ) ) &
    call interpolate_fft_delete ( o%interpolate_fft_obj )
    !
    call pathchoose_create (o%path_vel_button,       'path_vel',       '*'   )
    call pathchoose_create (o%path_datum_button, 'path_datum',     '*'   )
    !print'(" bb0 tdc_create p=",i4)', pcpsx_i_pel()
    !
    ! create the surfacegrid structure
    !
    !print'(" bb1 tdc_create p=",i4)', pcpsx_i_pel()
    !
    call surfacegrid_create ( o%sur, 'tdc', i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    call tdc_initialize ( o )
    !
    !print'(" end tdc_create p=",i4)', pcpsx_i_pel()
    !
    return
    !
997 continue
    !
    print'( &
    & /, " error in tdc_create p=", i4, &
    & /, " during surfacegrid_create " &
    & )' 
    !
    return
    !
  end subroutine tdc_create
  !
  subroutine tdc_delete ( o )
    !
    ! delete the tdc structure
    !
    type ( tdc_struct ),        pointer :: o                ! tdc structure
    !
    integer                             :: i_err
    type ( surfacegrid_struct ),    pointer :: sur  ! surfacegrid structure
    sur  => o%sur  
    !
    !print'(" top tdc_delete p=",i4)', pcpsx_i_pel()
    !
    ! delete the surfacegrid structure
    !
    call surfacegrid_delete ( sur )
    !
    call memfun_del ( o%i1_inp )
    call memfun_del ( o%i2_inp )
    call memfun_del ( o%f1_inp )
    call memfun_del ( o%f2_inp )
    call memfun_del ( o%i1_out )
    call memfun_del ( o%i2_out )
    call memfun_del ( o%f1_out )
    call memfun_del ( o%f2_out )
    call memfun_del ( o%i1_vel )
    call memfun_del ( o%i2_vel )
    call memfun_del ( o%f1_vel )
    call memfun_del ( o%f2_vel )
    if ( associated ( o%interpolate_fft_obj ) ) &
    call interpolate_fft_delete ( o%interpolate_fft_obj )
    !
           if ( associated ( o%path_vel_button ) )  &
    call pathchoose_delete ( o%path_vel_button )
    !
           if ( associated ( o%path_datum_button ) )  &
    call pathchoose_delete ( o%path_datum_button )
    !
    if ( associated      ( o%h ) )  &
    call headsave_delete ( o%h )
    !
    ! delete the cpucount structure
    !
    if (      associated ( o%c ) ) &
    call cpucount_delete ( o%c )
    !
    ! delete the trbuf structure
    !
    if (   associated ( o%trbuf ) ) &
    call trbuf_delete ( o = o%trbuf, file_remove = .true., i_err = i_err )
    !
    deallocate( o )
    !print'(" end tdc_delete p=",i4)', pcpsx_i_pel()
    !
    return
    !
  end subroutine tdc_delete
  !
  subroutine tdc_wrapup ( o )
    !
    ! wrapup tdc stuff
    !
    type ( tdc_struct ),        pointer :: o                ! tdc structure
    !
    if ( o%skip_wrapup ) return
         o%skip_wrapup = .true.
    !
    return
    !
  end subroutine tdc_wrapup
  !
  subroutine tdc_initialize ( o )
    !
    ! initialize tdc parameters
    !
    type ( tdc_struct ),        pointer :: o                ! tdc structure
    !
    ! get the current globals
    !print'(" top tdc_initialize p=",i4)', pcpsx_i_pel()
    !
    call getsys_username ( o%user_id )
    !
    call pc_get_jdata ( 'jobname', o%job_name )
    !
    call timeglob_get ( nt_inp = o%nt_inp,    &
                        t0_inp = o%t0_inp,    &
                        dt_inp = o%dt_inp )
    o%ipn = 0 ! process number
    !
    o%opt_vel        = 'PATH'
    o%path_vel       = pathcheck_empty ! velocity file
    o%const_vel      = 2000.
    !
    o%output_type    = 'DATA'
    o%input_units    = 'TIME'
    o%output_units   = 'DEPTH'
    !
    o%interpolate_original = .false.
    o%interpolate_inverse  = .false.
    !
    o%memory_size    = -1
    o%depth_scale    = 1000.
    o%interpolation_type   = 'FFT'
    o%interpolation_input  = 4
    o%interpolation_output = 10
    o%opt_vel_interpolate  = 'LIN_SLOWNESS'
    o%num_vel_interpolate  = 1
    o%opt_map_interpolate  = 'NEW'
    !
    o%tim_use        = 'INPUT'
    o%tim_tot        = o%nt_inp
    o%tim_init       = o%t0_inp
    o%tim_inc        = o%dt_inp
    o%tim_last       = (o%tim_tot-1)*o%tim_inc+o%tim_init
    !
    o%dep_use        = 'OUTPUT'
    o%dep_tot        = o%nt_inp
    o%dep_init       = o%t0_inp * o%depth_scale
    o%dep_inc        = o%dt_inp * o%depth_scale
    o%dep_last       = (o%dep_tot-1)*o%dep_inc+o%dep_init
    o%r0_min         = 0.0
    o%r0_max         = 0.0
    o%rs_min         = 100000.0
    o%rs_max         = 0.0
    o%mem_lim        = 0.0
    o%xyz_inp        = 'ZXY'
    !
    !print'(" aa1 tdc_initialize p=",i4)', pcpsx_i_pel()
    !
    call tdc_update ( o )
    !
    !print'(" end tdc_initialize p=",i4)', pcpsx_i_pel()
    !
    return
    !
  end subroutine tdc_initialize
  !
  subroutine tdc_update ( o )
    !
    ! update tdc parameters
    !
    type ( tdc_struct ), target         :: o                ! tdc structure
    !
    ! Local variables
    !
    integer                             :: i_err
    !
    integer                             :: update_state 
    type ( surfacegrid_struct ),    pointer :: sur  ! surfacegrid structure
    !
    sur  => o%sur  
    !
    !print'(" top tdc_update p=",i4)', pcpsx_i_pel()
    !
    i_err = 0
    !
    object => o         ! needed for traps.
    !
    o%skip_wrapup = .true.         
    !
    ! get the input vertical grid values
    ! depending upon parameter values these may be scaled to depth units below
    !
    call timeglob_get ( nt_inp = o%nt_inp, &
                        t0_inp = o%t0_inp, &
                        dt_inp = o%dt_inp )
    !
    ! get the number of words in the header
    !
    call pc_get_global ('nwih',  o%nh_inp)
    !
    if ( pathchoose_update ( o%path_vel_button,   o%path_vel   ) ) return
    if ( pathchoose_update ( o%path_datum_button, sur%path_surface ) ) return
    !
    ! get parameters
    !
    !print'(" aa1 tdc_update p=",i4)', pcpsx_i_pel()
    !
    call tdc_get ( o )
    !
    ! verify parameters
    !
    !print'(" aa2 tdc_update p=",i4)', pcpsx_i_pel()
    !
    call tdc_verify ( o )
    !
    ! put parameters
    !
    !print'(" aa3 tdc_update p=",i4)', pcpsx_i_pel()
    !
    call tdc_put ( o )
    !
    ! set parameter sensitivity
    !
    !print'(" aa4 tdc_update p=",i4)', pcpsx_i_pel()
    !
    call tdc_sensitive ( o )
    !
    update_state = pc_get_update_state()
    !
    ! prepare for execution
    !
    if ( .not. pc_do_not_process_traces() ) &
    write ( pc_get_lun(), '(  &
    & /, " tdc_update " , /, " REVISION: ", &
    & " 27  2007-04-12  Douglas Hanson Datumgrid changes. " &
    & )')
    !
    !print'(" aa5 tdc_update p=",i4)', pcpsx_i_pel()
    !
    if ( .not. pc_do_not_process_traces() ) &
    call tdc_prep ( o )
    !
    !print'(" end tdc_update p=",i4)', pcpsx_i_pel()
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
998 continue
    !
    call pc_error ( ' error in tdc_update during tdc_map_define ' )
    !
    i_err = -1
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_update ' )
    !
    return
    !
  end subroutine tdc_update
  !
  subroutine tdc_mail ( o, i_err )
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    !
    ! Local variables
    !
    integer,              intent(  out) :: i_err
    !
    integer                             :: lu_mail
    character(len=120)                  :: c0_mail
    character(len=120)                  :: cmd
    !
    i_err = 0
    !
    xxif_rs_max : if ( o%rs_max .gt. 0 .and. o%interpolate_inverse ) then
      !
      xxif_dep_inc : if ( o%dep_inc .gt. o%tim_inc / o%rs_max ) then
        !
        write(pc_get_lun(),*) &
        ' tdc_mail: WARNING , SAMPLING IS TOO COARSE IN JOB '
        write(pc_get_lun(),*) ' tdc_mail: WARNING , CHECK DATA FOR NOISE'
        write(pc_get_lun(),*) ' tdc_mail: WARNING , Vmin   =',1.0/o%rs_max
        write(pc_get_lun(),*) ' tdc_mail: WARNING , DEP_INC=',o%dep_inc
        write(pc_get_lun(),*) ' tdc_mail: WARNING , Vmin*TIM_INC=',&
        o%tim_inc*1.0/o%rs_max
        !
        xxif_not_mail : if ( .not. did_mail ) then
          !
          did_mail = .true.
          !
          call getlun ( lu_mail, i_err )
          !
          open ( unit=lu_mail,access='sequential',file='tdc_mtext',&
          form='formatted',iostat=i_err)
          !
          xxif_no_err : if ( i_err .eq. 0 ) then
            !
            c0_mail = 'tdc_mail: WARNING , SAMPLING IS TOO COARSE, JOB='&
            & // trim ( o%job_name )
            !
            write(lu_mail,*)  trim(c0_mail)
            !
            write(lu_mail,*) 'tdc_mail: WARNING , CHECK DATA FOR NOISE'
            write(c0_mail,*) 'tdc_mail: WARNING , Vmin   =',1.0/o%rs_max
            !
            write(lu_mail,*)  trim(c0_mail)
            write(c0_mail,*) 'tdc_mail: WARNING , DEP_INC=',o%dep_inc
            !
            write(lu_mail,*)  trim(c0_mail)
            write(c0_mail,*) 'tdc_mail: WARNING , Vmin*TIM_INC=',&
            o%tim_inc*1.0/o%rs_max
            !
            write(lu_mail,*)  trim ( c0_mail )
            !
            close ( unit = lu_mail )
            !
            cmd = 'mail -s \"TDC_WARNING\" ' // trim ( o%user_id ) &
            & // '@conocophillips.net < tdc_mtext'
            !
            call putsys_cmd ( cmd, i_err )
            !
            !print *,'putsys status=', i_err
            !
          end if xxif_no_err 
          !
        end if xxif_not_mail 
        !
      end if xxif_dep_inc 
      !
    end if xxif_rs_max 
    !
    i_err = 0
    !
    return
    !
  end subroutine tdc_mail 
  !
  subroutine tdc_get ( o )
    !
    ! get tdc parameters
    !
    type ( tdc_struct ), target         :: o                ! tdc structure
    type ( surfacegrid_struct ),    pointer :: sur  ! surfacegrid structure
    !
    !print'(" top tdc_get p=",i4)', pcpsx_i_pel()
    !
    sur  => o%sur  
    !
    ! Local variables
    !
    ! get the input vertical grid values
    ! depending upon parameter values these may be scaled to depth units below
    !
    call timeglob_get ( nt_inp = o%nt_inp, &
                        t0_inp = o%t0_inp, &
                        dt_inp = o%dt_inp )
    !
    ! get the number of words in the header
    !
    call pc_get_global ('nwih',  o%nh_inp)
    !
    call pc_get ( 'opt_vel',      o%opt_vel          )
    call pc_get ( 'path_vel',     o%path_vel         )
    call pc_get ( 'const_vel',    o%const_vel        )
    !
    call pc_put_options_field ('opt_vel',       c_opt_const,    n_opt_const )
    !
    call tdc_set_constant_path ( o%opt_vel            )
    !
    ! get surfacegrid parameters
    !
    call surfacegrid_get ( sur )
    !
    call pc_put_options_field ('opt_datum',     c_opt_const,    n_opt_const )
    !
    call tdc_set_constant_path ( sur%opt_surface      )
    !
    call pc_get ( 'output_type',  o%output_type  )
    call pc_get ( 'input_units',  o%input_units  )
    call pc_get ( 'output_units', o%output_units )
    !
    call pc_get ( 'memory_size',       o%memory_size       )
    !
    call pc_get ( 'depth_scale',   o%depth_scale   )
    !
    call pc_get ( 'interpolate_original', o%interpolate_original )
    call pc_get ( 'interpolate_inverse',  o%interpolate_inverse  )
    call pc_get ( 'interpolation_type',   o%interpolation_type   )
    call pc_get ( 'interpolation_input',  o%interpolation_input  )
    call pc_get ( 'interpolation_output', o%interpolation_output )
    call pc_get ( 'opt_vel_interpolate',  o%opt_vel_interpolate  )
    call pc_get ( 'num_vel_interpolate',  o%num_vel_interpolate  )
    call pc_get ( 'opt_map_interpolate',  o%opt_map_interpolate  )
    !
    call pc_get ( 'tim_use',  o%tim_use  )
    call pc_get ( 'tim_tot',  o%tim_tot  )
    call pc_get ( 'tim_init', o%tim_init )
    call pc_get ( 'tim_last', o%tim_last )
    call pc_get ( 'tim_inc',  o%tim_inc  )
    !
    call pc_get ( 'dep_use',  o%dep_use  )
    call pc_get ( 'dep_tot',  o%dep_tot  )
    call pc_get ( 'dep_init', o%dep_init )
    call pc_get ( 'dep_last', o%dep_last )
    call pc_get ( 'dep_inc',  o%dep_inc  )
    !
    !print'(" end tdc_get p=",i4)', pcpsx_i_pel()
    !
    return
    !
  end subroutine tdc_get
  !
  subroutine tdc_put ( o )
    !
    ! put tdc parameters
    !
    type ( tdc_struct ), target         :: o                ! tdc structure
    !
    ! local variables
    !
    integer                             :: n0_inp        
    integer                             :: n0_scr        
    integer                             :: n0_sto        
    type ( surfacegrid_struct ),    pointer :: sur  ! surfacegrid structure
    !
    sur  => o%sur  
    !
    call tdc_line_feed ( 'tdc_velocity' )
    call pc_put ( 'path_vel',     o%path_vel     )
    call pc_put ( 'opt_vel',      o%opt_vel      )
    call pc_put ( 'const_vel',    o%const_vel    )
    call pc_put ( 'depth_scale',  o%depth_scale  )
    !
    ! put surfacegrid parameters
    !
    call tdc_line_feed ( 'tdc_datum' )
    !
    call surfacegrid_put ( sur )
    !
    call tdc_line_feed ( 'tdc_units' )
    call pc_put ( 'output_type',  o%output_type  )
    call pc_put ( 'input_units',  o%input_units  )
    call pc_put ( 'output_units', o%output_units )
    !
    call tdc_line_feed ( 'tdc_interpolation' )
    call pc_put ( 'interpolate_original', o%interpolate_original )
    call pc_put ( 'interpolate_inverse',  o%interpolate_inverse  )
    call pc_put ( 'interpolation_type',   o%interpolation_type   )
    call pc_put ( 'interpolation_input',  o%interpolation_input  )
    call pc_put ( 'interpolation_output', o%interpolation_output )
    call pc_put ( 'opt_vel_interpolate',  o%opt_vel_interpolate  )
    call pc_put ( 'num_vel_interpolate',  o%num_vel_interpolate  )
    call pc_put ( 'opt_map_interpolate',  o%opt_map_interpolate  )
    !
    call tdc_line_feed ( 'tdc_memory' )
    call pc_put ( 'memory_size',  o%memory_size  )
    !
    call tdc_line_feed ( 'tdc_tim' )
    call pc_put ( 'tim_use',  o%tim_use  )
    call pc_put ( 'tim_tot',  o%tim_tot  )
    call pc_put ( 'tim_init', o%tim_init )
    call pc_put ( 'tim_last', o%tim_last )
    call pc_put ( 'tim_inc',  o%tim_inc  )
    !
    call tdc_line_feed ( 'tdc_dep' )
    call pc_put ( 'dep_use',  o%dep_use  )
    call pc_put ( 'dep_tot',  o%dep_tot  )
    call pc_put ( 'dep_init', o%dep_init )
    call pc_put ( 'dep_last', o%dep_last )
    call pc_put ( 'dep_inc',  o%dep_inc  )
    !
    call pc_put_options_field ( &
    'output_type', c_output_type, n_output_type )
    !
    call pc_put_options_field ( &
    'input_units', c_input_units, n_input_units )
    !
    call pc_put_options_field ( &
    'output_units', c_output_units, n_output_units )
    !
    call pc_put_options_field ( 'interpolate_original', c_yes_no, n_yes_no )
    !
    call pc_put_options_field ( &
    'interpolation_type', c_interpolation_type, n_interpolation_type )
    !
    call pc_put_options_field ( &
    'opt_vel_interpolate', c_opt_vel_interpolate, n_opt_vel_interpolate )
    !
    call pc_put_options_field ( &
    'opt_map_interpolate', c_opt_map_interpolate, n_opt_map_interpolate )
    !
    !  put control characteristics
    !
    n0_inp = 1
    n0_scr = 0
    n0_sto = 100000
    !
    ! put the current globals
    !
    call timeglob_put (nt_inp = o%nt_glb, &
                       t0_inp = o%t0_glb, &
                       dt_inp = o%dt_glb )
    !
    !print'(" end tdc_put p=",i4," nt=",i8," t0=",f8.4," dt=",f8.4)', &
    !pcpsx_i_pel(), o%nt_glb, o%t0_glb, o%dt_glb
    !
    call pc_put_global  ('numtr',      n0_inp  )
    call pc_put_global  ('gathered',   .false. )
    call pc_put_control ('need_label', .false. )
    call pc_put_control ('nscratch',   n0_scr  )
    call pc_put_control ('nstore',     n0_sto  )
    !
    ! kmig type
    !
    call pc_put_control ( 'parallel_safe' ,         .true.                )
    call pc_put_control ( 'pcps_send_mode' ,        'PCPS_BOSS_EXECS'     )
    !call pc_put_control ( 'pcps_generator_mode' ,   'PCPS_TRACE_GEN'      )
    !
    ! mute type
    !
    !call pc_put_control ('PARALLEL_SAFE'        ,.true.)
    !call pc_put_control ('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
    !call pc_put_control ('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
    !call pc_put_control ('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
    !call pc_put_control ('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
    !call pc_put_control ('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
    !call pc_put_control ('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')
    !
    return
    !
  end subroutine tdc_put
  !
  subroutine tdc_verify ( o )
    !
    ! verify tdc parameters
    !
    type ( tdc_struct ), target         :: o                ! tdc structure
    !
    ! local variables
    !
    integer                             :: i_stat
    type ( surfacegrid_struct ),    pointer :: sur  ! surfacegrid structure
    !
    sur  => o%sur  
    !
    ! set output_type, input_units, output_units to allowed values
    !
    call tdc_set_input_output ( o )
    !
    o%new_map_interpolate = &
    string_upper_compare ( o%opt_map_interpolate, 'new' )
    !
    o%old_map_interpolate = &
    .not. o%new_map_interpolate 
    !
    ! if output_type=velocity or if output_type=data and input_units=time  
    ! set the time  units from the input units
    !
    xxif_time : if ( &
      string_upper_compare ( o%output_type, 'VELOCITY' )   &
    .or.                                                     &
    ( string_upper_compare ( o%output_type, 'DATA'     )   &
    .and.                                                    &
      string_upper_compare ( o%input_units, 'TIME'     ) ) &
     ) then
      !
      o%tim_tot  = o%nt_inp
      o%tim_init = o%t0_inp
      o%tim_inc  = o%dt_inp
      o%tim_last = (o%tim_tot-1)*o%tim_inc+o%tim_init
      !
    end if xxif_time 
    !
    ! if output_type=velocity or if output_type=data and input_units=time 
    ! set the depth units from the input units
    !
    xxif_depth : if ( &
      string_upper_compare ( o%output_type, 'VELOCITY' )    &
    .or.                                                      &
    ( string_upper_compare ( o%output_type, 'DATA'     )    &
    .and.                                                     &
      string_upper_compare ( o%input_units, 'DEPTH'     ) ) &
     ) then
      !
      o%dep_tot = o%nt_inp
      o%dep_init = o%t0_inp * o%depth_scale
      o%dep_inc = o%dt_inp * o%depth_scale
      o%dep_last = (o%dep_tot-1)*o%dep_inc+o%dep_init
      !
    end if xxif_depth 
    !
    ! set the time and depth use labels
    !
    xxif_data_time : &
    if (                                                   &
      string_upper_compare ( o%output_type, 'DATA'     ) &
    .and.                                                  &
      string_upper_compare ( o%input_units, 'TIME'     ) &
     ) then
      !
      o%tim_use  = 'INPUT'
      o%dep_use = 'OUTPUT'
      !
    else if (                                              &
      string_upper_compare ( o%output_type, 'DATA'     ) &
    .and.                                                  &
      string_upper_compare ( o%input_units, 'DEPTH'    ) &
     ) then
      !
      o%tim_use  = 'OUTPUT'
      o%dep_use = 'INPUT'
      !
    else if (                                              &
      string_upper_compare ( o%output_type, 'VELOCITY' ) &
    .and.                                                  &
      string_upper_compare ( o%input_units, 'TIME'     ) &
     ) then
      !
      o%tim_use  = 'BOTH'
      o%dep_use = 'NOT_USED'
      !
    else if (                                              &
      string_upper_compare ( o%output_type, 'VELOCITY' ) &
    .and.                                                  &
      string_upper_compare ( o%input_units, 'DEPTH'    ) &
     ) then
      !
      o%tim_use  = 'NOT_USED'
      o%dep_use = 'BOTH'
      !
    end if xxif_data_time 
    !
    ! if the input units are in depth scale them here
    !
    xxif_depth_2 : &
    if (string_upper_compare ( o%input_units, 'DEPTH' ) ) then
      !
      o%t0_inp = o%t0_inp * o%depth_scale
      o%dt_inp = o%dt_inp * o%depth_scale
      !
    end if xxif_depth_2
    !
    if (o%tim_tot < 0) &
    call pc_error ('tdc : tim_tot must be >= 0')
    !
    if (o%tim_inc == 0.0) &
    call pc_error ('tdc : tim_inc must not be 0 ')
    !
    if ( o%tim_tot > 0 ) &
    i_stat = pattern_stop2('tdc:', .true., &
       o%tim_init, o%tim_inc, o%tim_last, o%tim_tot, &
       'tim_init', 'tim_inc', 'tim_last', 'tim_tot', &
       pc_verify_scalar('tim_init'), pc_verify_scalar('tim_inc'), &
       pc_verify_scalar('tim_last'), pc_verify_scalar('tim_tot'))   
    !
    if (o%dep_tot < 0) &
    call pc_error ('tdc : dep_tot must be >= 0')
    !
    if (o%dep_inc == 0.0) &
    call pc_error ('tdc : dep_inc must not be 0 ')
    !
    if ( o%dep_tot > 0 ) &
    i_stat = pattern_stop2('tdc:', .true., &
       o%dep_init, o%dep_inc, o%dep_last, o%dep_tot, &
       'dep_init', 'dep_inc', 'dep_last', 'dep_tot', &
       pc_verify_scalar('dep_init'), pc_verify_scalar('dep_inc'), &
       pc_verify_scalar('dep_last'), pc_verify_scalar('dep_tot'))   
    !
    !!------------------- call processes internally ------------------------!!
    !!------------------- call processes internally ------------------------!!
    !!------------------- call processes internally ------------------------!!
    !
    call pathcheck ('path_vel',o%path_vel,required=.false.)
    !
    !call string_replace_zeroes ( o%path_vel )
    !
    if ( len_trim(o%path_vel) .lt. 1) o%path_vel = pathcheck_empty
    !
    call pathcheck ('path_datum',sur%path_surface,required=.false.)
    !
    !call string_replace_zeroes ( sur%path_surface )
    !
    if ( len_trim(sur%path_surface) .lt. 1) sur%path_surface = pathcheck_empty
    o%l_lin_slowness = &
    string_upper_compare ( o%opt_vel_interpolate, 'lin_slowness' )
    !
    o%l_lin_velocity = &
    string_upper_compare ( o%opt_vel_interpolate, 'lin_velocity' )
    !
    o%l_node_down    = &
    string_upper_compare ( o%opt_vel_interpolate, 'node_down'    )
    !
    o%l_node_up      = &
    string_upper_compare ( o%opt_vel_interpolate, 'node_up'      )
    !
    xxif_opt_vel_interpolate_error : &
    if ( .not. o%l_lin_slowness &
   .and. .not. o%l_lin_velocity &
   .and. .not. o%l_node_down    &
   .and. .not. o%l_node_up      ) then
    !
    call pc_error ( ' error in tdc_verify opt_vel_interpolate', &
    o%opt_vel_interpolate )
    !
    return
    !
    end if xxif_opt_vel_interpolate_error 
    !
    xxif_depth_3 : &
    if ( string_upper_compare ( o%output_units, 'DEPTH' ) ) then
      !
      o%nt_out = o%dep_tot
      o%t0_out = o%dep_init
      o%dt_out = o%dep_inc
      !
      o%nt_glb = o%dep_tot
      o%t0_glb = o%dep_init / o%depth_scale
      o%dt_glb = o%dep_inc / o%depth_scale
      !
    else xxif_depth_3 
      !
      o%nt_out = o%tim_tot
      o%t0_out = o%tim_init
      o%dt_out = o%tim_inc
      !
      o%nt_glb = o%tim_tot
      o%t0_glb = o%tim_init
      o%dt_glb = o%tim_inc
      !
    end if xxif_depth_3 
    !
    ! verify surfacegrid parameters
    !
    call surfacegrid_verify ( o%sur )
    !
    return
    !
  end subroutine tdc_verify
  !
  subroutine tdc_sensitive ( o )
    !
    ! set tdc parameter sensitivity
    !
    type ( tdc_struct ), target         :: o                ! tdc structure
    !
    ! local variables
    !
    logical                             :: time_sensitive
    logical                             :: time_visible
    logical                             :: depth_sensitive
    logical                             :: depth_visible
    type ( surfacegrid_struct ),    pointer :: sur  ! surfacegrid structure
    !
    sur  => o%sur  
    !
    ! if output_type=velocity or if output_type=data and input_units=time  
    ! set the time  units from the input units
    !
    xxif_time_sen : &
    if ( &
      string_upper_compare ( o%output_type, 'VELOCITY' )   &
    .or.                                                     &
    ( string_upper_compare ( o%output_type, 'DATA'     )   &
    .and.                                                    &
      string_upper_compare ( o%input_units, 'TIME'     ) ) &
     ) then
      !
      time_sensitive = .false.
      !
    else xxif_time_sen  
      !
      time_sensitive = .true.
      !
    end if xxif_time_sen 
    !
    ! for output_type=velocity and input_units=depth 
    ! make the time values invisble
    !
    xxif_time_vis : &
    if ( &
      string_upper_compare ( o%output_type, 'VELOCITY' ) &
    .and.                                                  &
      string_upper_compare ( o%input_units, 'DEPTH'    ) &
     ) then
      !
      time_visible = .false.
      !
    else xxif_time_vis 
      !
      time_visible = .true.
      !
    end if xxif_time_vis 
    !
    ! if output_type=velocity or if output_type=data and input_units=time 
    ! set the depth units from the input units
    !
    xxif_depth_sen : &
    if ( &
      string_upper_compare ( o%output_type, 'VELOCITY' )    &
    .or.                                                      &
    ( string_upper_compare ( o%output_type, 'DATA'     )    &
    .and.                                                     &
      string_upper_compare ( o%input_units, 'DEPTH'     ) ) &
     ) then
      !
      depth_sensitive = .false.
      !
    else xxif_depth_sen 
      !
      depth_sensitive = .true.
      !
    end if xxif_depth_sen 
    !
    ! for output_type=velocity and input_units=depth 
    ! make the depth values invisble
    !
    xxif_depth_vis : &
    if ( &
      string_upper_compare ( o%output_type, 'VELOCITY' ) &
    .and.                                                  &
      string_upper_compare ( o%input_units, 'TIME'     ) &
     ) then
      !
      depth_visible = .false.
      !
    else xxif_depth_vis
      !
      depth_visible = .true.
      !
    end if xxif_depth_vis
    !
    ! turn the output units field insensitive
    ! turn the interpolation type and interpolation factor fields insensitive 
    ! if the output is velocity
    !
    call pc_put_sensitive_field_flag  ( 'output_units', .false. )
    call pc_put_sensitive_field_flag  ( 'interpolate_original', &
                        .not. string_upper_compare ( o%output_type, 'DATA' ) )
    call pc_put_sensitive_field_flag  ( 'interpolate_inverse', .false. )
    call pc_put_sensitive_field_flag  ( 'interpolation_type', &
                              string_upper_compare ( o%output_type, 'DATA' ) )
    call pc_put_sensitive_field_flag  ( 'interpolation_input', &
                              string_upper_compare ( o%output_type, 'DATA' ) )
    !
    ! apply the time and depth sensitivity controls
    !
    call pc_put_sensitive_field_flag  ( 'tim_use',  .false.         )
    call pc_put_sensitive_field_flag  ( 'tim_tot',  time_sensitive  )
    call pc_put_sensitive_field_flag  ( 'tim_init', time_sensitive  )
    call pc_put_sensitive_field_flag  ( 'tim_last', time_sensitive  )
    call pc_put_sensitive_field_flag  ( 'tim_inc',  time_sensitive  )
    !
    call pc_put_sensitive_field_flag  ( 'dep_use',  .false.         )
    call pc_put_sensitive_field_flag  ( 'dep_tot',  depth_sensitive )
    call pc_put_sensitive_field_flag  ( 'dep_init', depth_sensitive )
    call pc_put_sensitive_field_flag  ( 'dep_last', depth_sensitive )
    call pc_put_sensitive_field_flag  ( 'dep_inc',  depth_sensitive )
    !
    ! apply the time and depth visibility controls
    !
    call pc_put_visible_flag  ( 'tim_use',     time_visible  )
    call pc_put_visible_flag  ( 'tim_tot',     time_visible  )
    call pc_put_visible_flag  ( 'tim_init',    time_visible  )
    call pc_put_visible_flag  ( 'tim_last',    time_visible  )
    call pc_put_visible_flag  ( 'tim_inc',     time_visible  )
    !
    call pc_put_visible_flag  ( 'dep_use',     depth_visible )
    call pc_put_visible_flag  ( 'dep_tot',     depth_visible )
    call pc_put_visible_flag  ( 'dep_init',    depth_visible )
    call pc_put_visible_flag  ( 'dep_last',    depth_visible )
    call pc_put_visible_flag  ( 'dep_inc',     depth_visible )
    call pc_put_visible_flag  ( 'depth_scale', depth_visible )
    !
    ! set const_vel and path_vel sensitivity
    !
    xxif_const_vel : &
    if ( string_upper_compare ( o%opt_vel(1:1), 'C' ) ) then
      !
      call pc_put_sensitive_field_flag  ( 'const_vel', .true.  )
      call pc_put_sensitive_field_flag  ( 'path_vel',  .false. )
      !
    else xxif_const_vel 
      !
      call pc_put_sensitive_field_flag  ( 'const_vel', .false. )
      call pc_put_sensitive_field_flag  ( 'path_vel',  .true.  )
      !
    end if xxif_const_vel 
    !
    ! set const_datum and path_datum sensitivity
    !
    xxif_const_datum : &
    if ( string_upper_compare ( sur%opt_surface(1:1), 'C' ) ) then
      !
      call pc_put_sensitive_field_flag  ( 'const_datum', .true.  )
      call pc_put_sensitive_field_flag  ( 'path_datum',  .false. )
      !
    else xxif_const_datum 
      !
      call pc_put_sensitive_field_flag  ( 'const_datum', .false. )
      call pc_put_sensitive_field_flag  ( 'path_datum',  .true.  )
      !
    end if xxif_const_datum
    !
    return
    !
  end subroutine tdc_sensitive
  !
  subroutine tdc_prep ( o )
    !
    ! update tdc parameters
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    !
    ! Local variables
    !
    integer                             :: i_err
    type ( surfacegrid_struct ),    pointer :: sur  ! surfacegrid structure
    !
    sur  => o%sur  
    !
    !print'(" top tdc_prep p=",i4)', pcpsx_i_pel()
    !
    o%ipn = pc_get_ipn()
    !
    call headsave_create ( o%h, 'tdc', o%nh_inp, i_err )
    !
    call cpucount_create ( o%c, 20 )
    !
call cpucount_add ( o%c, 'tdc_trbuf_create',    o%i_tdc_trbuf_create,    i_err )
call cpucount_add ( o%c, 'tdc_map_compute',     o%i_tdc_map_compute,     i_err )
call cpucount_add ( o%c, 'tdc_map_compute_vf',  o%i_tdc_map_compute_vf,  i_err )
call cpucount_add ( o%c, 'tdc_map_compute_tr',  o%i_tdc_map_compute_tr,  i_err )
call cpucount_add ( o%c, 'tdc_map_compute_v0',  o%i_tdc_map_compute_v0,  i_err )
call cpucount_add ( o%c, 'tdc_map_compute_vn',  o%i_tdc_map_compute_vn,  i_err )
call cpucount_add ( o%c, 'tdc_map_compute_v1',  o%i_tdc_map_compute_v1,  i_err )
call cpucount_add ( o%c, 'tdc_map_compute_m1',  o%i_tdc_map_compute_m1,  i_err )
call cpucount_add ( o%c, 'tdc_map_compute_ir',  o%i_tdc_map_compute_ir,  i_err )
call cpucount_add ( o%c, 'tdc_map_apply',       o%i_tdc_map_apply,       i_err )
call cpucount_add ( o%c, 'tdc_inp_interpolate', o%i_tdc_inp_interpolate, i_err )
call cpucount_add ( o%c, 'tdc_map_interpolate', o%i_tdc_map_interpolate, i_err )
call cpucount_add ( o%c, 'tdc_out_interpolate', o%i_tdc_out_interpolate, i_err )
call cpucount_add ( o%c, 'tdc_map_head',        o%i_tdc_map_head,        i_err )
    !
    if ( string_upper_compare ( o%output_type, 'DATA' ) ) &
    o%interpolate_original = .false.
    !
    o%interpolate_inverse = .not. o%interpolate_original  
    !
    o%skip_wrapup = .false.
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'( &
    & /, " tdc_prep opt_map_interpolate=",a, &
    & /, " tdc_prep opt_vel_interpolate=",a, &
    & /, " tdc_prep num_vel_interpolate=",i8, &
    & /, " tdc_prep l_lin_slowness     =",l2, &
    & /, " tdc_prep l_lin_velocity     =",l2, &
    & /, " tdc_prep l_node_down        =",l2, &
    & /, " tdc_prep l_node_up          =",l2 &
    & )', &
    trim(o%opt_map_interpolate), &
    trim(o%opt_vel_interpolate), o%num_vel_interpolate, &
    o%l_lin_slowness, o%l_lin_velocity, o%l_node_down, o%l_node_up
    !
    !print'(" aa1 tdc_prep p=",i4)', pcpsx_i_pel()
    !
    ! define the mapping grid
    !
    call tdc_map_define ( o, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    if ( string_upper_compare ( sur%opt_surface,       'CONSTANT' ) ) &
                                sur%path_surface     = pathcheck_empty
    !
    ! get the x,y datum function on the map grid
    ! this includes memory allocation
    !
    !print'(" aa2 tdc_prep p=",i4)', pcpsx_i_pel()
    !
    call surfacegrid_read ( &
                       sur%opt_surface, sur%path_surface, sur%const_surface, &
                       o%hx_map, o%nx_map, o%x0_map, o%dx_map, 1., &
                       o%hy_map, o%ny_map, o%y0_map, o%dy_map, 1., &
                       sur%rz_sur, &
                       .true., .false., .false., &
                       i_err &
                        )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! print the source and receiver datum levels
    !
    if ( o%hx_map .eq. -999 ) &
    call surfacegrid_print ( &
                        'tdc travel time datums', pc_get_lun(), &
                        sur%opt_surface, sur%path_surface, sur%const_surface, &
                        o%hx_map, o%nx_map, o%x0_map, o%dx_map, 1., &
                        o%hy_map, o%ny_map, o%y0_map, o%dy_map, 1., &
                        sur%rz_sur &
                         )
    !
    if ( string_upper_compare ( o%opt_vel,       'CONSTANT' ) ) &
                                o%path_vel       = pathcheck_empty
    !
    ! compute the fft interpolation grid characteristics
    ! nt_fft = len of input trace after  fft interpolation 
    ! ft_fft = len of input trace during fft interpolation >= nt_fft
    !
    o%nt_fft = o%interpolation_input * ( o%nt_inp - 1 ) + 1 ! terp num
    o%t0_fft = o%t0_inp                                     ! terp org
    o%dt_fft = o%dt_inp / o%interpolation_input             ! terp inc
    !
    o%ft_inp = interpolate_pow2 ( o%nt_inp )
    o%ft_out = o%ft_inp * o%interpolation_input  ! fft len of terp trace
    !
    ! allocate memory for the input to fft grid linear interpolation coeff
    !
    call memfun_all ( o%i1_inp, o%nt_fft, 'i1_inp', i_err )
    call memfun_all ( o%i2_inp, o%nt_fft, 'i2_inp', i_err )
    call memfun_all ( o%f1_inp, o%nt_fft, 'f1_fft', i_err )
    call memfun_all ( o%f2_inp, o%nt_fft, 'f2_fft', i_err )
    !
    ! allocate memory the map to output grid linear interpolation coef
    !
    call memfun_all ( o%i1_out, o%nt_out, 'i1_out', i_err )
    call memfun_all ( o%i2_out, o%nt_out, 'i2_out', i_err )
    call memfun_all ( o%f1_out, o%nt_out, 'f1_out', i_err )
    call memfun_all ( o%f2_out, o%nt_out, 'f2_out', i_err )
    !
    ! allocate memory the map to output grid linear interpolation coef
    !
    call memfun_all ( o%i1_vel, o%nt_vel, 'i1_vel', i_err )
    call memfun_all ( o%i2_vel, o%nt_vel, 'i2_vel', i_err )
    call memfun_all ( o%f1_vel, o%nt_vel, 'f1_vel', i_err )
    call memfun_all ( o%f2_vel, o%nt_vel, 'f2_vel', i_err )
    !
    if ( i_err .ne. 0 ) go to 996
    !
    !print'(" aa3 tdc_prep p=",i4)', pcpsx_i_pel()
    !
    !  create the interpolate_fft object
    !
    call interpolate_fft_create ( &
                                  o                = o%interpolate_fft_obj, &
                                  c_title          = 'tdc', &
                                  interpolate_real = .true., &
                                  interpolate_cplx = .false., &
                                  ft_inp           = o%ft_inp, &
                                  nt_inp           = o%nt_inp, &
                                  t0_inp           = o%t0_inp, &
                                  dt_inp           = o%dt_inp, &
                                  ft_out           = o%ft_out, &
                                  nt_out           = o%nt_fft, &
                                  t0_out           = o%t0_fft, &
                                  dt_out           = o%dt_fft, &
                                  lu_out           = pc_get_lun(), &
                                  i_err            = i_err &
                                )
    !
    if ( i_err .ne. 0 ) go to 995
    !
    ! compute the input to fft grid linear interpolation coefficients
    !
    call interpolate_find_index_h ( &
                                    o%nt_inp, o%t0_inp, o%dt_inp, &
                                    o%nt_fft, o%t0_fft, o%dt_fft, &
                                    o%i1_inp, o%i2_inp,           &
                                    o%f1_inp, o%f2_inp            &
                                  )
    !
    ! compute the map to output grid linear interpolation coefficients
    !
    call interpolate_find_index_h ( &
                                    o%nt_map, o%t0_map, o%dt_map, &
                                    o%nt_out, o%t0_out, o%dt_out, &
                                    o%i1_out, o%i2_out,           &
                                    o%f1_out, o%f2_out            &
                                  )
    !
    ! compute the crs to vel grid linear interpolation coefficients
    !
    call interpolate_find_index_h ( &
                                    o%nt_crs, o%t0_crs, o%dt_crs, &
                                    o%nt_vel, o%t0_vel, o%dt_vel, &
                                    o%i1_vel, o%i2_vel,           &
                                    o%f1_vel, o%f2_vel            &
                                  )
    !
    if ( o%l_node_down ) o%i2_vel(1:o%nt_vel) = o%i1_vel(1:o%nt_vel)
    !
    if ( o%l_node_up   ) o%i1_vel(1:o%nt_vel) = o%i2_vel(1:o%nt_vel)
    !
    !print'(1x,i5,1x,g12.6,1x,g12.6,1x,g12.6,&
    !& 1x,i5,1x,i5,1x,g12.6,1x,g12.6,1x,i3," tdc1 ")', &
    !( jt_vel, (jt_vel-1)*o%dt_vel+o%t0_vel, &
    !(o%i1_vel(jt_vel)-1)*o%dt_crs+o%t0_crs, &
    !(o%i2_vel(jt_vel)-1)*o%dt_crs+o%t0_crs, &
    !o%i1_vel(jt_vel), o%i2_vel(jt_vel), o%f1_vel(jt_vel), o%f2_vel(jt_vel), &
    !o%ipn, jt_vel = 1, o%nt_vel )
    !
    ! create the trbuf structure
    !
    !print'(" bef tdc_trbuf_create p=",i4)', pcpsx_i_pel()
    !
    call tdc_trbuf_create ( o, i_err )
    !
    if ( i_err .ne. 0 ) go to 994
    !
    ! compute the mapping function
    !
    !print'(" bef tdc_map_compute p=",i4)', pcpsx_i_pel()
    !
    call tdc_map_compute ( o, i_err )
    !
    if ( i_err .ne. 0 ) go to 993
    !
    ! mail an error message
    !
    !call tdc_mail ( o, i_err )
    !
    ! print the mapping function
    !
    !print'(" bef tdc_map_print p=",i4)', pcpsx_i_pel()
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    call tdc_map_print ( &
                         o, &
                         pc_get_lun(), 'after tdc_map_compute', &
                         .true., &
                         !.false., &
                         1, 1, 20, &
                         1, 1, 20, &
                         !1, o%nx_map, 20, &
                         !1, o%ny_map, 20, &
                         1, o%nt_map, 20 &
                         !1, o%nt_map, 20 &
                       )
    !
    ! initialize the number of output traces, 
    !
    o%i0_out = 0
    !
    !print'(" bef tdc_prep p=",i4)', pcpsx_i_pel()
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    call cpucount_print ( o%c, ' tdc_prep ' )
    !
    return
    !
993 continue
    !
    call pc_error ( ' error in tdc_prep during tdc_map_compute ' )
    !
    i_err = -1
    !
    go to 999
    !
994 continue
    !
    call pc_error ( ' error in tdc_prep during tdc_trbuf_create ' )
    !
    go to 999
    !
995 continue
    !
    call pc_error ( &
    ' error in tdc_prep during interpolate_fft_create ' )
    !
    i_err = -1
    !
    go to 999
    !
996 continue
    !
    call pc_error ( &
    ' error in tdc_prep during memory allocation ' )
    !
    i_err = -1
    !
    go to 999
    !
997 continue
    !
    call pc_error ( ' error in tdc_prep during surfacegrid_read ' )
    !
    i_err = -1
    !
    go to 999
    !
998 continue
    !
    call pc_error ( ' error in tdc_prep during tdc_map_define ' )
    !
    i_err = -1
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_prep ' )
    !
    return
    !
  end subroutine tdc_prep
  !
  subroutine tdc ( o, n0_inp, hd_inp, tr_inp )
    !
    ! process traces
    !
    type ( tdc_struct ),  intent(inout) :: o                ! tdc structure
    integer,              intent(inout) :: n0_inp            ! num traces
    double precision,     intent(inout) :: hd_inp (:, :)    ! headers
    real,                 intent(inout) :: tr_inp (:, :)    ! traces
    !
    ! local variables
    !
    integer                           :: i_err     ! error flag
    integer                           :: i0_inp    ! input trace index
    integer, save                     :: i_call = 0
    !
    i_call = i_call + 1
    !
    !print'(" top tdc p=",i4," c=",i8)', pcpsx_i_pel(), i_call
    !
    ! Begin tdc
    ! get the header and trace first dimensions
    !
    o%mh_inp = size ( hd_inp, 1 )
    !
    o%mt_inp = size ( tr_inp, 1 )
    !
    i_err = 0
    !
    ! cycle over input traces
    !
    loop_i0_inp: do i0_inp = 1 , n0_inp
      !
      ! increment the total trace counter
      !
      o%i0_inp = i0_inp
      !
      o%i0_out = o%i0_out + 1
      !
      !print'(" qq1 p=",i4," q=",i4," c=",i8," i0=",i8, &
      !& " h7=",g12.6," h8=",g12.6," h25=",g12.6)', &
      !pcpsx_i_pel(), mod ( i0_inp-1, pcpsx_n_pel() ), i_call, i0_inp, &
      !hd_inp(7,i0_inp), hd_inp(8,i0_inp), hd_inp(25,i0_inp)
      !
      xxif_this_pe : &
      !if ( pcpsx_i_pel() .eq. 0 ) then
      if ( mod ( i0_inp-1, pcpsx_n_pel() ) .eq. pcpsx_i_pel() ) then
        !
        ! save input header word stats
        !
        call headsave_store ( o%h, o%i0_out, 1, hd_inp ( :, i0_inp ) )
        !
        ! map the input trace from inp to out useing map
        !
call tdc_map_apply ( o, hd_inp ( :, i0_inp ), tr_inp ( :, i0_inp ), i_err )
        !
        if ( i_err .ne. 0 ) exit
        !
        ! save saved header word stats
        !
        call headsave_store ( o%h, o%i0_out, 5, hd_inp ( :, i0_inp ) )
        !
        !if ( i_call .eq. 1 .and. i0_inp .eq. 1 ) &
        !print'(1x,i4,1x,i8,1x,i8,1x,f8.4,1x,g12.6," end_tdc" )', &
        !( pcpsx_i_pel(), i_call, &
        !jt_glb, (jt_glb-1)*o%dt_glb+o%t0_glb, tr_inp(jt_glb,1), &
        !jt_glb = 1 , o%nt_glb)
        !
      else xxif_this_pe 
        !
        ! zero this header and trace on this pe
        !
        tr_inp ( :, i0_inp ) = 0.
        !
        hd_inp ( :, i0_inp ) = 0.
        !
      end if xxif_this_pe 
      !
    end do loop_i0_inp
    !
    ! reduce all data over pes
    !
    call pcpsx_sum_all_reduce ( n0_inp, hd_inp, hd_inp )
    !
    call pcpsx_sum_all_reduce ( n0_inp, tr_inp, tr_inp )
    !
    !print'(" qq2 p=",i4," q=",i4," c=",i8," i0=",i8, &
    !& " h7=",g12.6," h8=",g12.6," h25=",g12.6)', &
    !( pcpsx_i_pel(), mod ( i0_inp-1, pcpsx_n_pel() ), i_call, i0_inp, &
    !hd_inp(7,i0_inp), hd_inp(8,i0_inp), hd_inp(25,i0_inp), &
    !i0_inp = 1 , n0_inp )
    !
    ! fatal error
    !
    xxif_error : if ( i_err .ne. 0 ) then
      !
      n0_inp = fatal_error
      !
      ! all done
      !
    else if ( n0_inp .eq. 0 ) then
      !
      n0_inp = no_more_traces
      !
      ! print input and saved header word stats
      !
      call headsave_print ( o%h, ' tdc ', 1 )
      !
      ! print cpu usage stats
      !
      if ( pcpsx_i_pel() .eq. 0 ) &
      call cpucount_print ( o%c, ' tdc end ' )
      !
    end if xxif_error 
    !
    !print'(" end tdc p=",i4," c=",i8)', pcpsx_i_pel(), i_call
    !
    !print'(" end tdc p=",i4," c=",i8," nt=",i8," t0=",f8.4," dt=",f8.4)', &
    !pcpsx_i_pel(), i_call, o%nt_glb, o%t0_glb, o%dt_glb
    !
    return
    !
  end subroutine tdc
  !
  subroutine tdc_set_input_output ( o )
    !
    ! set output_type, input_units, output_units to allowed values
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    !
    call string_to_upper ( o%output_type  )
    call string_to_upper ( o%input_units  )
    call string_to_upper ( o%output_units )
    call string_to_upper ( o%interpolation_type  )
    call string_to_upper ( o%opt_vel_interpolate )
    call string_to_upper ( o%opt_map_interpolate )
    !
    ! for data output, input units is the opposite of output_units
    !
    xxif_output_data : &
    if ( string_upper_compare ( o%output_type(1:1), 'D' ) ) then
      !
      o%output_type = 'DATA'
      !
      xxif_input_depth_1 : &
      if ( string_upper_compare ( o%input_units(1:1), 'D' ) ) then
        !
        o%input_units  = 'DEPTH'
        o%output_units = 'TIME'
        !
      else xxif_input_depth_1 
        !
        o%input_units  = 'TIME'
        o%output_units = 'DEPTH'
        !
      end if xxif_input_depth_1 
      !
      ! for velocity output, input units is the same as output_units
      !
    else xxif_output_data 
      !
      o%output_type = 'VELOCITY'
      !
      ! the time interpolation type should be LINEAR and
      ! the time interpolation factor should be 1 if the output is velocity
      ! and the input and output units should be the same
      !
      o%interpolation_type   = 'LINEAR'
      o%interpolation_input = 1
      !
      xxif_input_depth_2 : &
      if ( string_upper_compare ( o%input_units(1:1), 'D' ) ) then
        !
        o%input_units  = 'DEPTH'
        o%output_units = 'DEPTH'
        !
      else  xxif_input_depth_2 
        !
        o%input_units  = 'TIME'
        o%output_units = 'TIME'
        !
      end if xxif_input_depth_2 
      !
    end if xxif_output_data 
    !
    o%interpolate_inverse = .not. o%interpolate_original  
    !
    o%interpolation_input = max ( 1, o%interpolation_input )
    !
    xxif_interpolation_fft : &
    if ( string_upper_compare ( o%interpolation_type(1:1), 'F' ) ) then
      !
      o%interpolation_type = 'FFT'
      !
    else xxif_interpolation_fft 
      !
      o%interpolation_type = 'LINEAR'
      !
    end if xxif_interpolation_fft 
    !
    return
    !
  end subroutine tdc_set_input_output
  !
  subroutine tdc_trbuf_create ( o, i_err )
    !
    ! create the trbuf structure
    ! trbuf will hold nr_dsk traces on disk and nr_mem traces in memory
    !
    type ( tdc_struct ), target         :: o                ! tdc structure
    integer,              intent(inout) :: i_err        ! 0 o.k. -1 err
    !
    ! local variables
    !
    call cpucount ( o%c, o%i_tdc_trbuf_create, 1 )
    !
    o%trbuf_file_name = 'tdc'
    !
    o%n2_dim = o%nx_map ! length of second dim of a single record
    !
    o%nr_dsk = o%ny_map ! number of records (nt_map, nx_map ) on disk
    !
    ! set the number of y s, nr_mem, held in the trbuf memory
    !
    xxif_memory_size : if ( o%memory_size .le. 0 ) then
      !
      o%nr_mem = o%ny_map
      !
    else xxif_memory_size 
      !
      o%nr_mem = o%memory_size / ( o%nt_map * o%nx_map ) 
      !
    end if xxif_memory_size 
    !
    ! we must keep at least two rows for interpolation efficiency
    !
    o%nr_mem = min ( o%ny_map, max ( 2, o%nr_mem ) )
    !
    call trbuf_create ( &
                        o             = o%trbuf, &
                        file_name     = o%trbuf_file_name, &
                        add_job_name  = .true., &
                        add_cps_name  = .true., &
                        add_ipn_name  = .true., &
                        !must_use_disk = .true., &
                        must_use_disk = .false., &
                        nr_dsk        = o%nr_dsk, &
                        nr_mem        = o%nr_mem, &
                        n2_dim        = o%n2_dim, &
                        nh_inp        = 0, &
                        nt_inp        = o%nt_map, &
                        t0_inp        = o%t0_map, &
                        dt_inp        = o%dt_map, &
                        memory_size   = o%trbuf_memory, &
                        i_err         = i_err, &
                        check_errors  = .true. &
                      )
    !
    call pcpsx_check_worker_errors ( i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
1999 continue
    !
    call cpucount ( o%c, o%i_tdc_trbuf_create, 2 )
    !
    return
    !
998 continue
    !
    call pc_error ( ' error in tdc_trbuf_create during trbuf_create ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_trbuf_create ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine tdc_trbuf_create 
  !
  subroutine tdc_map_define ( o, i_err )
    !
    ! define the mapping grid
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    integer,              intent(inout) :: i_err        ! 0 o.k. -1 err
    type ( surfacegrid_struct ),    pointer :: sur  ! surfacegrid structure
    !
    sur  => o%sur  
    !
    ! local variables
    !
    i_err = 0
    !
    !  set the output interpolation coefficient
    !
    o%interpolation_output = max ( 1, o%interpolation_output )
    !
    ! get the velocity grid sizes
    !
    xxif_path_vel_empty  : &
    if ( string_upper_compare ( o%opt_vel, 'CONSTANT' ) &
    .or. string_upper_compare ( o%path_vel, pathcheck_empty ) ) then
      !
      o%hx_vel = 7
      o%nx_vel = 1
      o%x0_vel = 0.
      o%dx_vel = 1.
      !
      o%hy_vel = 8
      o%ny_vel = 1
      o%y0_vel = 0.
      o%dy_vel = 1.
      !
      o%nt_vel = 1
      o%t0_vel = 0.
      o%dt_vel = o%dep_inc
      !
      o%vel_type = 'VZIN'
      o%xyz_inp  = 'ZXY'
      !
    else xxif_path_vel_empty
      !
      call velgrid_size ( &
                          pc_get_lun(), 'tdc velocity file', &
                          o%path_vel, o%vel_type, &
                          o%n0_vel, o%hx_vel, o%hy_vel, &
                          o%nx_vel, o%x0_vel, o%dx_vel, &
                          o%ny_vel, o%y0_vel, o%dy_vel, &
                          o%nt_vel, o%t0_vel, o%dt_vel, &
                          i_err &
                        )
      !
      if ( i_err .ne. 0 ) go to 998
      !
    end if xxif_path_vel_empty
    !
    ! get the datum grid size
    !
    xxif_path_datum_empty  : &
    if ( string_upper_compare ( sur%opt_surface, 'CONSTANT' ) &
    .or. string_upper_compare ( sur%path_surface, pathcheck_empty ) ) then
      !
      sur%hx_sur = o%hx_vel
      sur%nx_sur = 1
      sur%x0_sur = 0.
      sur%dx_sur = 1.
      !
      sur%hy_sur = o%hy_vel
      sur%ny_sur = 1
      sur%y0_sur = 0.
      sur%dy_sur = 1.
      !
    else xxif_path_datum_empty
      !
      call surfacegrid_size ( &
                              sur%opt_surface, sur%path_surface, &
                              sur%hx_sur, sur%nx_sur, sur%x0_sur, sur%dx_sur, &
                              sur%hy_sur, sur%ny_sur, sur%y0_sur, sur%dy_sur, &
                              i_err &
                            )
      !
      if ( i_err .ne. 0) go to 997
      !
    end if xxif_path_datum_empty
    !
    ! get the x map grid coefficients
    !
    call tdc_map_define_x ( &
                            o%path_vel, &
                            o%hx_vel, o%nx_vel, o%x0_vel, o%dx_vel, &
                            sur%hx_sur, sur%nx_sur, sur%x0_sur, sur%dx_sur, &
                            o%hx_map, o%nx_map, o%x0_map, o%dx_map, &
                            i_err &
                          ) 
    !
    if ( i_err .ne. 0 ) go to 996
    !
    ! get the y map grid coefficients
    !
    call tdc_map_define_x ( &
                            o%path_vel, &
                            o%hy_vel, o%ny_vel, o%y0_vel, o%dy_vel, &
                            sur%hy_sur, sur%ny_sur, sur%y0_sur, sur%dy_sur, &
                            o%hy_map, o%ny_map, o%y0_map, o%dy_map, &
                            i_err &
                          ) 
    !
    if ( i_err .ne. 0 ) go to 996
    !
    ! define the map grid in depth
    !
    xxif_output_depth : &
    if ( string_upper_compare ( o%output_units, 'DEPTH' ) )then
      !
      o%t0_map = o%dep_init
      o%t1_map = o%dep_last
      o%dt_map = o%interpolation_output * o%dep_inc
      o%nt_map = int ( ( o%t1_map - o%t0_map ) / o%dt_map ) + 1
      if ( ( o%nt_map - 1 ) * o%dt_map +  o%t0_map .lt. o%t1_map ) &
      o%nt_map = o%nt_map + 1
      o%map_type = 'VZIN'
      !
      ! define the map grid in time
      !
    else xxif_output_depth
      !
      o%t0_map = o%tim_init
      o%t1_map = o%tim_last
      o%dt_map = o%interpolation_output * o%tim_inc 
      o%nt_map = int ( ( o%t1_map - o%t0_map ) / o%dt_map ) + 1
      if ( ( o%nt_map - 1 ) * o%dt_map +  o%t0_map .lt. o%t1_map ) &
      o%nt_map = o%nt_map + 1
      o%map_type = 'VTIN'
      !
    end if xxif_output_depth
    !
    o%nt_crs = o%nt_vel
    o%dt_crs = o%dt_vel
    o%t0_crs = o%t0_vel
    o%t1_crs = o%t1_vel
    !
    o%nt_vel = ( o%nt_vel - 1 ) * o%num_vel_interpolate + 1
    !
    o%dt_vel = o%dt_vel / o%num_vel_interpolate
    !
    ! define the velocity vertical grid, this assumes t=0 is at z=0
    !
    !o%t0_vel = 0.
    !o%t1_vel = max ( o%t1_map , o%t1_map - o%t0_map )
    !o%dt_vel = o%dt_map
    !o%nt_vel = int ( ( o%t1_map - o%t0_map ) / o%dt_map ) + 2
    !
    if (  .not. string_upper_compare ( o%vel_type, 'VTIN' ) &
    .and. .not. string_upper_compare ( o%vel_type, 'VZIN' ) ) go to 995
    !
1999 continue
    !
    call pcpsx_check_worker_errors ( i_err )
    !
    return
    !
995 continue
    !
    call pc_error ( ' error in tdc_map_define in input velocity type ' )
    call pc_error ( ' the velocity type must be VTIN or VZIN vel_type=', &
                    o%vel_type )
    !
    go to 999
    !
996 continue
    !
    call pc_error ( ' error in tdc_map_define during tdc_map_define_x ' )
    call pc_error ( ' the velocity and datum header word must be the same ' )
call pc_error ( ' map header words hx_map= ', o%hx_map, ' hy_map= ', o%hy_map )
call pc_error ( ' vel header words hx_vel= ', o%hx_vel, ' hy_vel= ', o%hy_vel )
call pc_error ( ' dtm header words hx_dtm= ', sur%hx_sur, ' hy_dtm= ', sur%hy_sur )
    !
    go to 999
    !
997 continue
    !
    call pc_error ( ' error in tdc_map_define during surfacegrid_size ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( ' error in tdc_map_define during velgrid_size ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_map_define ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine tdc_map_define  
  !
  subroutine tdc_map_define_x ( &
                                path_vel, &
                                hx_vel, nx_vel, x0_vel, dx_vel, &
                                hx_dtm, nx_dtm, x0_dtm, dx_dtm, &
                                hx_map, nx_map, x0_map, dx_map, &
                                i_err &
                              ) 
    !
    ! define the mapping grid
    !
    character(len=*),   intent(in   ) :: path_vel
    !
    integer,            intent(inout) :: hx_vel        ! x header word
    integer,            intent(inout) :: nx_vel        ! num vel x sample 
    real,               intent(inout) :: x0_vel        ! min vel x sample 
    real,               intent(inout) :: dx_vel        ! vel x sample inc
    !
    integer,            intent(inout) :: hx_dtm        ! x header word
    integer,            intent(inout) :: nx_dtm        ! num dtm x sample 
    real,               intent(inout) :: x0_dtm        ! min dtm x sample 
    real,               intent(inout) :: dx_dtm        ! dtm x sample inc
    !
    integer,            intent(inout) :: hx_map        ! x header word
    integer,            intent(inout) :: nx_map        ! num map x sample 
    real,               intent(inout) :: x0_map        ! min map x sample 
    real,               intent(inout) :: dx_map        ! map x sample inc
    !
    integer,            intent(inout) :: i_err         ! err flag 0 o.k. -1 err
    !
    ! local variables
    !
    real                              :: x1_vel   ! max x velocity value
    real                              :: x1_dtm   ! max x datum value
    real                              :: x1_map   ! max x map value
    !
    i_err = 0
    !
    ! if there is no velocity file set the velocity header words to the datum
    !
    if ( string_upper_compare ( path_vel, pathcheck_empty ) ) hx_vel = hx_dtm
    !
    hx_map = hx_vel
    !
    ! make sure the datum and velocity grid headers are the same
    !
    if ( hx_dtm .ne. hx_vel ) go to 996
    !
    ! set the map horizontal grid to be the finer of the velocity, datum grids
    !
    x1_vel = ( nx_vel - 1 ) * dx_vel + x0_vel
    x1_dtm = ( nx_dtm - 1 ) * dx_dtm + x0_dtm
    !
    x0_map = min ( x0_vel, x0_dtm )
    if ( nx_vel .eq. 1 ) x0_map = x0_dtm
    if ( nx_dtm .eq. 1 ) x0_map = x0_vel
    !
    x1_map = max ( x1_vel, x1_dtm )
    if ( nx_vel .eq. 1 ) x1_map = x1_dtm
    if ( nx_dtm .eq. 1 ) x1_map = x1_vel
    !
    dx_map = min ( dx_vel, dx_dtm )
    if ( nx_vel .eq. 1 ) dx_map = dx_dtm
    if ( nx_dtm .eq. 1 ) dx_map = dx_vel
    !
    nx_map = nint ( ( x1_map - x0_map ) / dx_map ) + 1
    !
    dx_map = ( x1_map - x0_map ) / max ( 1, nx_map - 1 )
    !
    if ( nx_map .eq. 1 ) dx_map = 1.
    !
    return
    !
996 continue
    !
    call pc_error ( ' error in tdc_map_define_x in header words ' )
    call pc_error ( ' the velocity and datum header word must be the same ' )
    call pc_error ( ' map header words hx_map= ', hx_map )
    call pc_error ( ' vel header words hx_vel= ', hx_vel )
    call pc_error ( ' dtm header words hx_dtm= ', hx_dtm )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_map_define_x ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine tdc_map_define_x
  !
  subroutine tdc_map_compute ( o, i_err )
    !
    ! compute the map tz_map
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    integer,              intent(inout) :: i_err        ! 0 o.k. -1 err
    !
    ! local variables
    !
    integer                             :: jx_map         ! vel x index
    integer                             :: jy_map         ! vel y index
    real                                :: rx_map         ! map x value
    real                                :: ry_map         ! map y value
    real                                :: rz_dtm         ! map z datum
    real                                :: rt_dtm         ! map t datum
    real                                :: rs_vel ( o%nt_vel+1 )! slowness val
    real                                :: rz_vel ( o%nt_vel )  ! vel z val
    real                                :: rt_vel ( o%nt_vel )  ! vel t val
    real                                :: rs_loc ( o%nt_crs, o%nx_map )
    real                                :: rs_crs ( o%nt_crs )
    real                                :: rs_fin ( o%nt_vel )
    real                                :: tz_loc ( o%nt_map, o%nx_map )
    character(len=12)                   :: c_time
    type ( surfacegrid_struct ),    pointer :: sur  ! surfacegrid structure
    !
    integer, save                       :: i_call = 0
    !
    sur  => o%sur  
    !
    i_call = i_call + 1
    !
    !print'(" top tdc_map_compute p=",i4)', pcpsx_i_pel()
    !
    call cpucount ( o%c, o%i_tdc_map_compute, 1 )
    !
    i_err = 0
    !
    ! read the velocity file
    ! this will open the file and prep it for reading
    ! one y column at a time so the map can be computed and stored by trbuf
    !
    call tdc_map_compute_v0 ( o, i_err )
    !
    call pcpsx_check_worker_errors ( i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    write ( pc_get_lun(), '( &
    & /, " tdc_map_compute ", &
    & /, " interpolate_original=", l2, &
    & /, " interpolate_inverse =", l2, &
    & /, " output_type=", a, &
    & /, " input_units=", a, &
    & /, " output_units=", a, &
    & /, " vel_type=", a, &
    & /, " map_type=", a, &
    & /, " vel_parm=", a, &
    & /, " nx_map=", i8, " x0_map=", g12.6, " dx_map=", g12.6, &
    & /, " ny_map=", i8, " y0_map=", g12.6, " dy_map=", g12.6, &
    & /, " nt_vel=", i8, " t0_vel=", g12.6, " dt_vel=", g12.6, &
    & /, " nt_fft=", i8, " t0_fft=", g12.6, " dt_fft=", g12.6, &
    & /, " nt_map=", i8, " t0_map=", g12.6, " dt_map=", g12.6, &
    & /, " nt_out=", i8, " t0_out=", g12.6, " dt_out=", g12.6 &
    & )' ) &
    o%interpolate_original, &
    o%interpolate_inverse, &
    trim(o%output_type), &
    trim(o%input_units), &
    trim(o%output_units), &
    trim(o%vel_type), &
    trim(o%map_type), &
    trim(o%vel_parm), &
    o%nx_map, o%x0_map, o%dx_map, &
    o%ny_map, o%y0_map, o%dy_map, &
    o%nt_vel, o%t0_vel, o%dt_vel, &
    o%nt_fft, o%t0_fft, o%dt_fft, &
    o%nt_map, o%t0_map, o%dt_map, &
    o%nt_out, o%t0_out, o%dt_out
    !
    ! loop over the map y locations.
    !
    loop_jy_map: do jy_map = 1 , o%ny_map
      !
      ! compute y map location, ry_map
      !
      ry_map = ( jy_map - 1 ) * o%dy_map + o%y0_map
      !
      call string_time ( c_time )
      !
      !if ( pcpsx_i_pel() .eq. 0 ) &
      !print'(" aa1 tdc_map_compute p=",i4," c=",i8," y=",i8,1x,g12.6,1x,a)', &
      !pcpsx_i_pel(), i_call, jy_map, ry_map, trim(c_time)
      !
      ! read in slowness. rs_loc ( t, x ) for y = ry_map
      !
      !if ( i_err .ne. -999 ) &
      !rs_loc = 1. / 2000.
      !
      !if ( i_err .eq. -999 ) &
      !if ( jy_map .eq. 1 ) &
      call tdc_map_compute_vn ( o, ry_map, rs_loc, i_err )
      !
      call pcpsx_check_worker_errors ( i_err )
      !
      if ( i_err .ne. 0 ) go to 997
      !
      o%rs_min = min ( o%rs_min, minval ( rs_loc ) )
      !
      o%rs_max = max ( o%rs_max, maxval ( rs_loc ) )
      !
      ! initilaze the map function,t z_map
      !
      !if ( jy_map .eq. 1 ) &
      tz_loc = 0.
      !
      ! loop over the map x locations.
      !
      loop_jx_map: do jx_map = pcpsx_i_pel()+1, o%nx_map, pcpsx_n_pel()
        !
        ! compute x map location, rx_map
        !
        rx_map = ( jx_map - 1 ) * o%dx_map + o%x0_map
        !
        !if ( mod(jx_map,50) .eq. 1 ) &
        !print'(" tdc_map_compute p=",i4," c=",i8," x=",i8,1x,g12.6)', &
        !pcpsx_i_pel(), i_call, jx_map, rx_map
        !
        ! compute the datum depth at the map x,y location
        !
        rz_dtm = sur%rz_sur ( jx_map, jy_map )  ! z(t=rt_dtm=0)
        !
        rt_dtm = 0.                             ! t(z=rz_dtm)
        !
        ! interpolate from the original velcoity grid to the fin velocity grid
        !
        call cpucount ( o%c, o%i_tdc_map_compute_vf, 1 )
        !
        rs_crs(1:o%nt_crs) = rs_loc(1:o%nt_crs,jx_map) 
        !
        if ( o%l_lin_velocity ) &
        rs_crs = 1. / rs_crs
        !
        ! fill in rs_fin via linear interpolation
        !
        !if ( i_err .eq. -999 ) &
        !if ( jx_map .eq. pcpsx_i_pel()+1 .and. jy_map .eq. 1 ) &
        call interpolate_i_to_r_p ( &
                                    ix_inp_1 = o%i1_vel, &
                                    ix_inp_2 = o%i2_vel, &
                                    fx_inp_1 = o%f1_vel, &
                                    fx_inp_2 = o%f2_vel, &
                                    ry_inp   = rs_crs, &
                                    nx_out   = o%nt_vel, &
                                    ry_out   = rs_fin &
                                  )
        !
        if ( o%l_lin_velocity ) &
        rs_fin = 1. / rs_fin
        !
        call cpucount ( o%c, o%i_tdc_map_compute_vf, 2 )
        !
        !if ( jx_map .eq. 1 .and. jy_map .eq. 1 ) &
        !print'(1x,i5,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,i3," tdc2 " )', &
        !( jt_vel, (jt_vel-1)*o%dt_vel+o%t0_vel, &
        !1./rs_crs(o%i1_vel(jt_vel)), 1./rs_crs(o%i2_vel(jt_vel)), &
        !1./rs_fin(jt_vel), &
        !o%ipn, jt_vel = 1, o%nt_vel )
        !
        ! for each slowness node compute the slowness, rs_vel, 
        ! depth, rz_vel, and time, rt_vel, to that point
        !
        !if ( i_err .eq. -999 ) &
        !if ( jx_map .eq. pcpsx_i_pel()+1 .and. jy_map .eq. 1 ) &
        call tdc_map_compute_v1 ( &
                                 o, &
                                 rs_fin, rz_dtm, rt_dtm, &
                                 rs_vel, rz_vel, rt_vel, &
                                 i_err &
                               )
        !
        if ( i_err .ne. 0 ) go to 1996
        !
        !if ( jx_map .eq. 1 .and. jy_map .eq. -1 ) &
        !print'(1x,i5,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,i3," tdc3 " )', &
        !( jt_vel, (jt_vel-1)*o%dt_vel+o%t0_vel, &
        !1./rs_vel(jt_vel), rz_vel(jt_vel), rt_vel(jt_vel), &
        !o%ipn, jt_vel = 1, o%nt_vel )
        !
        ! compute the map function, tz_loc ( :, jx_map )
        !
        !if ( jx_map .eq. pcpsx_i_pel()+1 .and. jy_map .eq. 1 ) &
        call tdc_map_compute_m1 ( o, &
                                  rs_fin, rz_dtm, rt_dtm, &
                                  rs_vel, rz_vel, rt_vel, &
                                  tz_loc ( :, jx_map ), &
                                  i_err &
                                )
        !
        !tz_loc ( :, jx_map ) = tz_loc ( :, pcpsx_i_pel()+1 )
        !
        if ( i_err .ne. 0 ) go to 1995
        !
      end do loop_jx_map
      !
      go to 2001
      !
1996 continue
      !
      call pcpsx_check_worker_errors ( i_err )
      !
      if ( i_err .ne. 0 ) go to 996
      !
1995 continue
      !
      call pcpsx_check_worker_errors ( i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
2001  continue
      !
      ! reduce the map over all pes
      !
      !call pcpsx_broadcast ( 0, o%nx_map, tz_loc )
      !
      !if ( pcpsx_i_pel() .eq. 0 ) &
      !print'(1x,i4,1x,i8,1x,g12.6,1x,g12.6," tdc_map_max_1")', &
      !( pcpsx_i_pel(), jx_map, (jx_map-1)*o%dx_map+o%x0_map, &
      !maxval(tz_loc ( :, jx_map )), jx_map = 1 , o%nx_map )
      !
      !if ( jy_map .eq. 1 ) &
      call pcpsx_sum_all_reduce ( o%nx_map, tz_loc, tz_loc )
      !
      !if ( pcpsx_i_pel() .eq. 0 ) &
      !print'(1x,i4,1x,i8,1x,g12.6,1x,g12.6," tdc_map_max_2")', &
      !( pcpsx_i_pel(), jx_map, (jx_map-1)*o%dx_map+o%x0_map, &
      !maxval(tz_loc ( :, jx_map )), jx_map = 1 , o%nx_map )
      !
      o%r0_min = min ( o%r0_min, minval(tz_loc) )
      !
      o%r0_max = max ( o%r0_max, maxval(tz_loc) )
      !
      ! set the map buffer disk index, jr_dsk
      !
      o%jr_dsk = jy_map 
      !
      ! get the memory trace idnex, jr_mem, for this disk trace
      ! this will load from disk into memory if needed.
      ! the headers for this trace will be used in beamsyn_map_head
      !
      call cpucount ( o%c, o%i_tdc_map_compute_tr, 1 )
      !
      call string_time ( c_time )
      !
      !print'(" aa2 tdc_map_compute p=",i4," c=",i8," y=",i8,1x,g12.6,1x,a)', &
      !pcpsx_i_pel(), i_call, jy_map, ry_map, trim(c_time)
      !
      call trbuf_get_index ( &
                             o        = o%trbuf, &
                             jr_mem   = o%jr_mem, &
                             jr_dsk   = o%jr_dsk, &
                             l_change = o%l_change, &
                             i_err    = i_err &
                           )
      !
      call pcpsx_check_worker_errors ( i_err )
      !
      if ( i_err .ne. 0 ) go to 994
      !
      ! add this map function, tz_map, into the buffer
      !
      o%mem_lim = max ( o%mem_lim, o%jr_mem )
      !
      o%trbuf%tr_mem ( 1:o%nt_map, 1:o%nx_map, o%jr_mem ) = &
              tz_loc ( 1:o%nt_map, 1:o%nx_map )
      !
      call cpucount ( o%c, o%i_tdc_map_compute_tr, 2 )
      !
      call string_time ( c_time )
      !
      !print'(" aa3 tdc_map_compute p=",i4," c=",i8," y=",i8,1x,g12.6,1x,a)', &
      !pcpsx_i_pel(), i_call, jy_map, ry_map, trim(c_time)
      !
    end do loop_jy_map
    !
    !print*,'tdc_map_compute: global map minimum =',o%r0_min
    !print*,'tdc_map_compute: global map maximum =',o%r0_max
    !print*,'tdc_map_compute: global max of jr_mem =',o%mem_lim
    !
1999 continue
    !
    call pcpsx_check_worker_errors ( i_err )
    !
    ! delete the mg_obj
    !
    if ( .not. string_upper_compare ( o%opt_vel,       'CONSTANT' ) &
   .and. .not. string_upper_compare ( o%path_vel, pathcheck_empty ) ) &
    call modgrid_delete ( o%mg_obj )
    !
    call cpucount ( o%c, o%i_tdc_map_compute, 2 )
    !
    !print'(" end tdc_map_compute p=",i4)', pcpsx_i_pel()
    !
    return
    !
994 continue
    !
    call pc_error ( ' error in tdc_map_compute in trbuf_get_index ' )
    !
    go to 999
    !
995 continue
    !
    call pc_error ( ' error in tdc_map_compute during tdc_map_compute_m1 ' )
    !
    go to 999
    !
996 continue
    !
    call pc_error ( ' error in tdc_map_compute during tdc_map_compute_v1 ' )
    !
    go to 999
    !
997 continue
    !
    call pc_error ( ' error in tdc_map_compute during tdc_map_compute_vn ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( ' error in tdc_map_compute during tdc_map_compute_v0 ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_map_compute ' )
    call pc_error ( ' vel_type    =', o%vel_type     )
    call pc_error ( ' output_type =', o%output_type  )
    call pc_error ( ' output_units=', o%output_units )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine tdc_map_compute  
  !
  subroutine tdc_map_compute_v0 ( o, i_err )
    !
    ! read the input velocity file
    ! it must be in either VTIN or VZIN form
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    integer,              intent(inout) :: i_err        ! 0 o.k. -1 err
    !
    ! local variables
    !
    character(len=12)                   :: ftype
    character(len=96)                   :: dfile
    integer                             :: rank
    character(len=32)                   :: lab1
    character(len=32)                   :: lab2
    character(len=32)                   :: lab3
    integer                             :: h1
    integer                             :: h2
    integer                             :: h3
    integer                             :: n1
    integer                             :: n2
    integer                             :: n3
    real                                :: o1
    real                                :: o2
    real                                :: o3
    real                                :: d1
    real                                :: d2
    real                                :: d3
    integer                             :: hx_scn !scanning header
    integer                             :: hy_scn !scanning header
    !
    call cpucount ( o%c, o%i_tdc_map_compute_v0, 1 )
    !
    i_err = 0
    !
    !  read the vertical coefficient from path_vel
    !  get on pe 0 and broadcast to all pes
    !
    ! set the velocity interpolation type
    !
    if ( o%interpolate_inverse ) &
    o%vel_parm = 'SLOWNESS' ! interpolate slowness not velocity 
    !
    if ( o%interpolate_original ) &
    o%vel_parm = 'VELOCITY' ! interpolate velocity not slowness 
    !
    o%xyz_out = 'ZXY'
    !
    hx_scn= 17
    !
    hy_scn= 18
    !
    ! open the velocity model and get size characteristics
    !
    if ( .not. string_upper_compare ( o%opt_vel,       'CONSTANT' ) &
   .and. .not. string_upper_compare ( o%path_vel, pathcheck_empty ) ) &
    i_err = modgrid_rddesc_verbose ( &
                                     o%mg_obj, o%path_vel, &
                                     pc_get_lun(), &
                                     dfile, ftype, rank, &
                                     lab1, h1, n1, o1, d1, & 
                                     lab2, h2, n2, o2, d2, & 
                                     lab3, h3, n3, o3, d3, & 
                                     o%xyz_inp, hx_scn, hy_scn, o%vel_type &
                                   )
    !
    !print'("#tdc_map_compute: aft modgrid_rddesc_verbose i_err=", i8 )', i_err
    !
    if ( i_err .lt. 0 ) goto 998
    !
    o%xyz_out = 'ZXY'
    !
    ! determine the output ordering of X, Y and Z axis
    !
    xxif_index : if ( index ( o%xyz_inp, 'Z' ) .eq. 1 ) then
      !
      if ( o%hx_map .eq. 7 .or. o%hx_map .eq. 17 ) o%xyz_out = 'ZXY'
      !
      if ( o%hx_map .eq. 8 .or. o%hx_map .eq. 18 ) o%xyz_out = 'ZYX'
      !
    else xxif_index 
      !
      ! possible but will treat as an error here
      !
      goto 997
      !
    end if xxif_index 
    !
1999 continue
    !
    call cpucount ( o%c, o%i_tdc_map_compute_v0, 2 )
    !
    return
    !
997 continue
    !
    call pc_error ( ' error in tdc_map_compute_v0 during xxx ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( &
    ' error in tdc_map_compute_v0 during modgrid_rddesc_verbose ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_map_compute_v0 ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine tdc_map_compute_v0
  !
  subroutine tdc_map_compute_vn ( o, ry_map, rs_loc, i_err )
    !
    ! compute the map tz_map
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    real,                 intent(in   ) :: ry_map       ! y loc
    real,                 intent(inout) :: rs_loc ( :, : ) ! vel (torz,x)
    integer,              intent(inout) :: i_err        ! 0 o.k. -1 err
    !
    ! local variables
    !
    integer                             :: nt_tmp ! vel t grid num
    real                                :: t0_tmp ! vel t grid min
    real                                :: dt_tmp ! vel t grid inc
    !
    integer                             :: nx_tmp ! vel x grid num
    real                                :: x0_tmp ! vel x grid min
    real                                :: dx_tmp ! vel x grid inc
    !
    integer                             :: ny_tmp ! vel y grid num
    real                                :: y0_tmp ! vel y grid min
    real                                :: dy_tmp ! vel y grid inc
    !
    integer                             :: vel_mem ! vel mem size
    !
    call cpucount ( o%c, o%i_tdc_map_compute_vn, 1 )
    !
    ! set the slowness for this y column to a constant value
    !
    rs_loc = o%const_vel
    !
    if ( o%interpolate_inverse ) &
    rs_loc = 1. / rs_loc 
    !
    ! read in all t, x values for this y
    !
    nt_tmp = o%nt_crs
    t0_tmp = o%t0_crs
    dt_tmp = o%dt_crs
    !
    ny_tmp = 1
    y0_tmp =   ry_map
    dy_tmp = o%dy_map
    !
    nx_tmp = o%nx_map
    x0_tmp = o%x0_map
    dx_tmp = o%dx_map
    !
    xxif_opt_vel_constant : &
    if ( .not. string_upper_compare ( o%opt_vel,       'CONSTANT' ) &
   .and. .not. string_upper_compare ( o%path_vel, pathcheck_empty ) ) then
      !
      vel_mem = nt_tmp * o%nx_vel * min(2,o%ny_vel)
      !
      i_err = velgrid_paint_by_obj_par ( &
                                         o%mg_obj, pc_get_lun(), &
                                         'TDC', o%vel_parm, o%vel_parm, &
                                         0, .true., vel_mem, &
                                         o%hx_map, o%hy_map, &
                                         nt_tmp, t0_tmp, dt_tmp, &
                                         nx_tmp, x0_tmp, dx_tmp, &
                                         ny_tmp, y0_tmp, dy_tmp, &
                                         rs_loc, o%xyz_out, o%vel_type &
                                       )
      !
      if ( i_err .ne. 0 ) go to 998
      !
    end if xxif_opt_vel_constant 
    !
1999 continue
    !
    call cpucount ( o%c, o%i_tdc_map_compute_vn, 2 )
    !
    return
    !
998 continue
    !
    call pc_error ( &
    ' error in tdc_map_compute_vn during velgrid_paint_by_obj_par ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_map_compute_vn ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine tdc_map_compute_vn   
  !
  subroutine tdc_map_compute_v1 ( &
                                 o, &
                                 rs_loc, rz_dtm, rt_dtm, &
                                 rs_vel, rz_vel, rt_vel, &
                                 i_err &
                               )
    !
    ! return the velocity for a single map x,y column
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    real,                 intent(in   ) :: rs_loc(:)    ! inp velocity
    real,                 intent(in   ) :: rz_dtm       ! datum depth
    real,                 intent(in   ) :: rt_dtm       ! datum time 
    real,                 intent(inout) :: rs_vel(:)    ! out velocity
    real,                 intent(inout) :: rz_vel(:)    ! out depth
    real,                 intent(inout) :: rt_vel(:)    ! vertical two way time
    integer,              intent(inout) :: i_err        ! 0 o.k. -1 err
    !
    ! local variables
    !
    integer                             :: jt_vel         ! vel t index
    !
    call cpucount ( o%c, o%i_tdc_map_compute_v1, 1 )
    !
    ! for each velocity point compute the time and depth to that point
    !
    xxif_veltype_vtin : &
    if ( string_upper_compare ( o%vel_type, 'vtin' ) ) then
      !
      ! for vtin the velocity grid is in time 
      ! and we compute the depth to each velocity time point
      !
      rs_vel ( o%nt_vel + 1 ) = rs_loc ( o%nt_vel )
      rs_vel (            1 ) = rs_loc ( 1        )
      !
      rt_vel (            1 ) = o%t0_vel
      !
      xxif_linear_slowness_vtin : &
      if ( o%l_lin_slowness ) then
        !
        rz_vel (            1 ) = rz_dtm + &
        .5 * max ( 0., o%t0_vel - rt_dtm ) / rs_vel ( 1 )
        !
        do_jt_vel_vtin_1 : do jt_vel = 2 , o%nt_vel
          !
          rs_vel ( jt_vel     ) = &
   .5 * ( rs_loc ( jt_vel     ) &
        + rs_loc ( jt_vel - 1 ) )
          !
          rt_vel ( jt_vel ) = ( jt_vel - 1 ) * o%dt_vel + o%t0_vel
          !
          rz_vel ( jt_vel ) = rz_vel ( jt_vel - 1 ) + &
          .5 * min ( o%dt_vel , max ( 0., rt_vel ( jt_vel ) - rt_dtm ) ) &
          / rs_vel ( jt_vel )
          !
        end do do_jt_vel_vtin_1 
        !
      else if ( o%l_lin_velocity ) then
        !
        rz_vel (            1 ) = rz_dtm + &
        .5 * max ( 0., o%t0_vel - rt_dtm ) / rs_vel ( 1 )
        !
        do_jt_vel_vtin_2 : do jt_vel = 2 , o%nt_vel
          !
          rs_vel ( jt_vel     ) = &
   .5 / ( 1. / rs_loc ( jt_vel     ) &
        + 1. / rs_loc ( jt_vel - 1 ) )
          !
          rt_vel ( jt_vel ) = ( jt_vel - 1 ) * o%dt_vel + o%t0_vel
          !
          rz_vel ( jt_vel ) = rz_vel ( jt_vel - 1 ) + &
          .5 * min ( o%dt_vel , max ( 0., rt_vel ( jt_vel ) - rt_dtm ) ) &
          / rs_vel ( jt_vel )
          !
        end do do_jt_vel_vtin_2 
        !
      else if ( o%l_node_down ) then
        !
        rz_vel (            1 ) = rz_dtm + &
        .5 * max ( 0., o%t0_vel - rt_dtm ) / rs_vel ( 1 )
        !
        do_jt_vel_vtin_3 : do jt_vel = 2 , o%nt_vel
          !
          rs_vel ( jt_vel     ) = &
   .5 * ( rs_loc ( jt_vel - 1 ) &
        + rs_loc ( jt_vel - 1 ) )
          !
          rt_vel ( jt_vel ) = ( jt_vel - 1 ) * o%dt_vel + o%t0_vel
          !
          rz_vel ( jt_vel ) = rz_vel ( jt_vel - 1 ) + &
          .5 * min ( o%dt_vel , max ( 0., rt_vel ( jt_vel ) - rt_dtm ) ) &
          / rs_vel ( jt_vel )
          !
        end do do_jt_vel_vtin_3 
        !
      else if ( o%l_node_up ) then
        !
        rz_vel (            1 ) = rz_dtm + &
        .5 * max ( 0., o%t0_vel - rt_dtm ) / rs_vel ( min(o%nt_vel,2) )
        !
        do_jt_vel_vtin_4 : do jt_vel = 2 , o%nt_vel
          !
          rs_vel ( jt_vel     ) = &
   .5 * ( rs_loc ( jt_vel     ) &
        + rs_loc ( jt_vel     ) )
          !
          rt_vel ( jt_vel ) = ( jt_vel - 1 ) * o%dt_vel + o%t0_vel
          !
          rz_vel ( jt_vel ) = rz_vel ( jt_vel - 1 ) + &
          .5 * min ( o%dt_vel , max ( 0., rt_vel ( jt_vel ) - rt_dtm ) ) &
          / rs_vel ( jt_vel )
          !
        end do do_jt_vel_vtin_4 
        !
      else xxif_linear_slowness_vtin 
        !
        go to 997
        !
      end if xxif_linear_slowness_vtin 
      !
    else if ( string_upper_compare ( o%vel_type, 'vzin' ) ) then
      !
      ! for vzin the velocity grid is in depth 
      ! and we compute the time to each velocity depth point
      !
      rs_vel ( o%nt_vel + 1 ) = rs_loc ( o%nt_vel )
      !
      rs_vel (            1 ) = rs_loc ( 1        )
      !
      rz_vel (            1 ) = o%t0_vel
      !
      ! there are four modes for interpolating the velocity
      ! 1) linerar interpolation of slowness - the default
      ! 2) linerar interpolation of velocity
      ! 3) linerar interpolation of velocity
      ! 4) linerar interpolation of velocity
      !
      xxif_linear_slowness_vzin : &
      if ( o%l_lin_slowness ) then
        !
        rt_vel (            1 ) = rt_dtm + &
        2. * max ( 0., o%t0_vel - rz_dtm ) * rs_loc ( 1 )
        !
        do_jt_vel_vzin_1 : do jt_vel = 2 , o%nt_vel
          !
          rz_vel ( jt_vel ) = ( jt_vel - 1 ) * o%dt_vel + o%t0_vel
          !
          rs_vel ( jt_vel ) = .5 * ( rs_loc ( jt_vel     ) &
                                   + rs_loc ( jt_vel - 1 ) )
          !
          rt_vel ( jt_vel ) = &
          rt_vel ( jt_vel - 1 ) + &
          2. * min ( o%dt_vel , max ( 0., rz_vel ( jt_vel ) - rz_dtm ) ) &
        * rs_vel ( jt_vel )
          !
        end do do_jt_vel_vzin_1 
        !
      else if ( o%l_lin_velocity ) then
        !
        rt_vel (            1 ) = rt_dtm + &
        2. * max ( 0., o%t0_vel - rz_dtm ) * rs_loc ( 1 )
        !
        do_jt_vel_vzin_2 : do jt_vel = 2 , o%nt_vel
          !
          rs_vel ( jt_vel ) = .5 / ( 1. / rs_loc ( jt_vel     ) &
                                   + 1. / rs_loc ( jt_vel - 1 ) )
          !
          rz_vel ( jt_vel ) = ( jt_vel - 1 ) * o%dt_vel + o%t0_vel
          !
          rt_vel ( jt_vel ) = &
          rt_vel ( jt_vel - 1 ) + &
          2. * min ( o%dt_vel , max ( 0., rz_vel ( jt_vel ) - rz_dtm ) ) &
        * rs_vel ( jt_vel )
          !
        end do do_jt_vel_vzin_2 
        !
      else if ( o%l_node_down ) then
        !
        do_jt_vel_vzin_3 : do jt_vel = 2 , o%nt_vel
          !
          rs_vel ( jt_vel ) = .5 * ( rs_loc ( jt_vel - 1 ) &
                                   + rs_loc ( jt_vel - 1 ) )
          !
          rz_vel ( jt_vel ) = ( jt_vel - 1 ) * o%dt_vel + o%t0_vel
          !
          rt_vel ( jt_vel ) = &
          rt_vel ( jt_vel - 1 ) + &
          2. * min ( o%dt_vel , max ( 0., rz_vel ( jt_vel ) - rz_dtm ) ) &
        * rs_vel ( jt_vel )
          !
        end do do_jt_vel_vzin_3 
        !
      else if ( o%l_node_up ) then
        !
        rt_vel (            1 ) = rt_dtm + &
        2. * max ( 0., o%t0_vel - rz_dtm ) * rs_loc ( min(o%nt_vel,2) )
        !
        do_jt_vel_vzin_4 : do jt_vel = 2 , o%nt_vel
          !
          rs_vel ( jt_vel ) = .5 * ( rs_loc ( jt_vel     ) &
                                   + rs_loc ( jt_vel     ) )
          !
          rz_vel ( jt_vel ) = ( jt_vel - 1 ) * o%dt_vel + o%t0_vel
          !
          rt_vel ( jt_vel ) = &
          rt_vel ( jt_vel - 1 ) + &
          2. * min ( o%dt_vel , max ( 0., rz_vel ( jt_vel ) - rz_dtm ) ) &
        * rs_vel ( jt_vel )
          !
        end do do_jt_vel_vzin_4 
        !
      else xxif_linear_slowness_vzin 
        !
        go to 997
        !
      end if xxif_linear_slowness_vzin 
      !
    else xxif_veltype_vtin
      !
      go to 998
      !
    end if xxif_veltype_vtin
    !
    !print'(" qq3 ",1x,i8,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6 )', &
    !( jt_vel, rt_vel(jt_vel), rz_vel(jt_vel), &
    !rs_vel(jt_vel), 1./rs_vel(jt_vel), &
    !jt_vel = 1,o%nt_vel)
    !
    !if ( o%nt_vel .ne. -999 ) stop
    !
1999 continue
    !
    call cpucount ( o%c, o%i_tdc_map_compute_v1, 2 )
    !
    return
    !
997 continue
    !
    call pc_error ( &
' error in tdc_map_compute_v1 in opt_vel_interpolate', o%opt_vel_interpolate )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( ' error in tdc_map_compute_v1 in vel_type ', o%vel_type )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_map_compute_v1 ' )
    call pc_error ( ' vel_type    =', o%vel_type     )
    call pc_error ( ' output_type =', o%output_type  )
    call pc_error ( ' output_units=', o%output_units )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine tdc_map_compute_v1 
  !
  subroutine tdc_map_compute_v1a ( &
                                 o, &
                                 rs_loc, rz_dtm, rt_dtm, &
                                 rs_vel, rz_vel, rt_vel, &
                                 i_err &
                               )
    !
    ! return the velocity for a single map x,y column
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    real,                 intent(in   ) :: rs_loc(:)    ! inp velocity
    real,                 intent(in   ) :: rz_dtm       ! datum depth
    real,                 intent(in   ) :: rt_dtm       ! datum time 
    real,                 intent(inout) :: rs_vel(:)    ! out velocity
    real,                 intent(inout) :: rz_vel(:)    ! out depth
    real,                 intent(inout) :: rt_vel(:)    ! vertical two way time
    integer,              intent(inout) :: i_err        ! 0 o.k. -1 err
    !
    ! local variables
    !
    integer                             :: jt_vel         ! vel t index
    !
    call cpucount ( o%c, o%i_tdc_map_compute_v1, 1 )
    !
    ! for each velocity point compute the time and depth to that point
    !
    xxif_veltype_vtin : &
    if ( string_upper_compare ( o%vel_type, 'vtin' ) ) then
      !
      ! for vtin the velocity grid is in time 
      ! and we compute the depth to each velocity time point
      !
      rs_vel ( o%nt_vel + 1 ) = rs_loc ( o%nt_vel )
      rs_vel (            1 ) = rs_loc ( 1        )
      !
      rt_vel (            1 ) = o%t0_vel
      !
      rz_vel (            1 ) = rz_dtm + &
      .5 * max ( 0., o%t0_vel - rt_dtm ) / rs_vel ( 1 )
      !
      do_jt_vel_vtin : do jt_vel = 2 , o%nt_vel
        !
        rs_vel ( jt_vel     ) = &
 .5 * ( rs_loc ( jt_vel     ) &
      + rs_loc ( jt_vel - 1 ) )
        !
        rt_vel ( jt_vel ) = ( jt_vel - 1 ) * o%dt_vel + o%t0_vel
        !
        rz_vel ( jt_vel ) = rz_vel ( jt_vel - 1 ) + &
        .5 * min ( o%dt_vel , max ( 0., rt_vel ( jt_vel ) - rt_dtm ) ) &
        / rs_vel ( jt_vel )
        !
      end do do_jt_vel_vtin
      !
    else if ( string_upper_compare ( o%vel_type, 'vzin' ) ) then
      !
      ! for vzin the velocity grid is in depth 
      ! and we compute the time to each velocity depth point
      !
      rs_vel ( o%nt_vel + 1 ) = rs_loc ( o%nt_vel )
      rs_vel (            1 ) = rs_loc ( 1        )
      !
      rz_vel (            1 ) = o%t0_vel
      !
      rt_vel (            1 ) = rt_dtm + &
      2. * max ( 0., o%t0_vel - rz_dtm ) * rs_loc ( 1 )
      !
      do_jt_vel_vzin : do jt_vel = 2 , o%nt_vel
        !
        rs_vel ( jt_vel ) = .5 * ( rs_loc ( jt_vel     ) &
                                 + rs_loc ( jt_vel - 1 ) )
        !
        rz_vel ( jt_vel ) = ( jt_vel - 1 ) * o%dt_vel + o%t0_vel
        !
        rt_vel ( jt_vel ) = &
        rt_vel ( jt_vel - 1 ) + &
        2. * min ( o%dt_vel , max ( 0., rz_vel ( jt_vel ) - rz_dtm ) ) &
      * rs_vel ( jt_vel )
        !
      end do do_jt_vel_vzin
      !
    else xxif_veltype_vtin
      !
      go to 998
      !
    end if xxif_veltype_vtin
    !
1999 continue
    !
    call cpucount ( o%c, o%i_tdc_map_compute_v1, 2 )
    !
    return
    !
998 continue
    !
    call pc_error ( ' error in tdc_map_compute_v1 in vel_type ', o%vel_type )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_map_compute_v1 ' )
    call pc_error ( ' vel_type    =', o%vel_type     )
    call pc_error ( ' output_type =', o%output_type  )
    call pc_error ( ' output_units=', o%output_units )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine tdc_map_compute_v1a 
  !
  subroutine tdc_map_compute_m1 ( o, &
                                 rs_loc, rz_dtm, rt_dtm, &
                                 rs_vel, rz_vel, rt_vel, &
                                 tz_loc, &
                                 i_err &
                               )
    !
    ! compute the map tz_map
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    real,                 intent(in   ) :: rs_loc(:)    ! inp velocity
    real,                 intent(in   ) :: rz_dtm       ! datum depth
    real,                 intent(in   ) :: rt_dtm       ! datum time 
    real,                 intent(in   ) :: rs_vel(:)    ! out velocity
    real,                 intent(in   ) :: rz_vel(:)    ! out depth
    real,                 intent(in   ) :: rt_vel(:)    ! vertical two way time
    real,                 intent(inout) :: tz_loc(:)    ! map function
    integer,              intent(inout) :: i_err        ! 0 o.k. -1 err
    !
    ! local variables
    !
    integer                             :: jt_map         ! map t index
    real                                :: rz_map         ! map z value
    real                                :: rt_map         ! map t value
    !
    call cpucount ( o%c, o%i_tdc_map_compute_m1, 1 )
    !
    ! construct the required velocity at this location.
    !
    xxif_output_type_units : &
    if ( string_upper_compare ( o%output_type, 'VELOCITY' ) &
   .and. string_upper_compare ( o%output_units, 'DEPTH' ) ) then
      !
      ! output velocity in depth
      ! interpolate the velocity from the velocity grid to the map grid
      !
      call cpucount ( o%c, o%i_tdc_map_compute_ir, 1 )
      !
      if ( o%new_map_interpolate ) &
      call interpolate_i_to_r_0 ( &
                                nx_inp = o%nt_vel, &
                                rx_inp =   rz_vel, &
                                ry_inp =   rs_vel, &
                                nx_out = o%nt_map, &
                                x0_out = o%t0_map, &
                                dx_out = o%dt_map, &
                                ry_out =   tz_loc &
                               )
      !
      if ( o%old_map_interpolate ) &
      call interpolate_i_to_r ( &
                                nx_inp = o%nt_vel, &
                                rx_inp =   rz_vel, &
                                ry_inp =   rs_vel, &
                                nx_out = o%nt_map, &
                                x0_out = o%t0_map, &
                                dx_out = o%dt_map, &
                                ry_out =   tz_loc &
                               )
      !
      call cpucount ( o%c, o%i_tdc_map_compute_ir, 2 )
      !
    else if ( string_upper_compare ( o%output_type, 'VELOCITY' ) &
        .and. string_upper_compare ( o%output_units, 'TIME' ) ) then
      !
      ! output velocity in time
      ! interpolate the velocity from the velocity grid to the map grid
      !
      call cpucount ( o%c, o%i_tdc_map_compute_ir, 1 )
      !
      if ( o%new_map_interpolate ) &
      call interpolate_i_to_r_0 ( &
                                nx_inp = o%nt_vel, &
                                rx_inp =   rt_vel, &
                                ry_inp =   rs_vel, &
                                nx_out = o%nt_map, &
                                x0_out = o%t0_map, &
                                dx_out = o%dt_map, &
                                ry_out =   tz_loc &
                              )
      !
      if ( o%old_map_interpolate ) &
      call interpolate_i_to_r ( &
                                nx_inp = o%nt_vel, &
                                rx_inp =   rt_vel, &
                                ry_inp =   rs_vel, &
                                nx_out = o%nt_map, &
                                x0_out = o%t0_map, &
                                dx_out = o%dt_map, &
                                ry_out =   tz_loc &
                              )
      !
      call cpucount ( o%c, o%i_tdc_map_compute_ir, 2 )
      !
    else if ( string_upper_compare ( o%output_type, 'DATA' ) &
        .and. string_upper_compare ( o%output_units, 'DEPTH' ) ) then
      !
      ! output is in depth, tz_vel is the input time for each map depth
      !
      ! compute the time to each map depth node
      !              z
      ! t(z) = 2. * int dz / v(z)
      !              0
      !
      ! tz_vel will be the input time for each velocity depth
      ! interpolate the input time from the velocity grid to the map grid
      !
      call cpucount ( o%c, o%i_tdc_map_compute_ir, 1 )
      !
      if ( o%new_map_interpolate ) &
      call interpolate_i_to_r_0 ( &
                                nx_inp = o%nt_vel, &
                                rx_inp =   rz_vel, &
                                ry_inp =   rt_vel, &
                                nx_out = o%nt_map, &
                                x0_out = o%t0_map, &
                                dx_out = o%dt_map, &
                                ry_out =   tz_loc &
                              )
      !
      if ( o%old_map_interpolate ) &
      call interpolate_i_to_r ( &
                                nx_inp = o%nt_vel, &
                                rx_inp =   rz_vel, &
                                ry_inp =   rt_vel, &
                                nx_out = o%nt_map, &
                                x0_out = o%t0_map, &
                                dx_out = o%dt_map, &
                                ry_out =   tz_loc &
                              )
      !
      call cpucount ( o%c, o%i_tdc_map_compute_ir, 2 )
      !
      ! now check each map grid to see if it is outside the velocity grid
      !
      do_jt_map_depth : do jt_map = 1 , o%nt_map
        !
        rz_map = ( jt_map - 1 ) * o%dt_map + o%t0_map
        !
        if_jt_map_depth : if ( rz_map .lt. rz_vel ( 1 ) ) then
          !
          tz_loc ( jt_map ) = &
          rt_dtm + 2. * max ( 0., rz_map - rz_dtm ) * rs_vel ( 1 )
          !
        else if ( rz_map .gt. rz_vel ( o%nt_vel ) ) then
          !
          tz_loc ( jt_map ) = &
          rt_vel ( o%nt_vel ) + &
          2. * max ( 0., rz_map - rz_vel ( o%nt_vel ) ) * &
                                  rs_vel ( o%nt_vel + 1 )
          !
        end if if_jt_map_depth 
        !
        !if ( jx_map .le. -11 .and. jy_map .eq. 1 ) &
        !write ( pc_get_lun(), '( &
        !& " q1 ", i5, 1x, i5, &
        !& 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, g12.6 &
        !& )' ) &
        !jx_map, jt_map, (jt_map-1)*dt_map+t0_map, &
        !z_map, z_vel(nt_vel), 1./ v_vel(nt_vel+1), &
        !tz_loc ( jt_map )
        !
      end do do_jt_map_depth
      !
    else if ( string_upper_compare ( o%output_type, 'DATA' ) &
        .and. string_upper_compare ( o%output_units, 'TIME' ) ) then
      !
      ! output is in time, tz_vel is the input depth for each map depth
      !
      ! compute the depth to each velocity time node
      !              t
      ! z(t) = .5 * int dt * v(t)
      !              0
      !
      ! tz_vel will be the input depth for each velocity time
      ! interpolate the input depth from the velocity grid to the map grid
      !
      call cpucount ( o%c, o%i_tdc_map_compute_ir, 1 )
      !
      if ( o%new_map_interpolate ) &
      call interpolate_i_to_r_0 ( &
                                nx_inp = o%nt_vel, &
                                rx_inp =   rt_vel, &
                                ry_inp =   rz_vel, &
                                nx_out = o%nt_map, &
                                x0_out = o%t0_map, &
                                dx_out = o%dt_map, &
                                ry_out =   tz_loc    &
                              )
      !
      if ( o%old_map_interpolate ) &
      call interpolate_i_to_r ( &
                                nx_inp = o%nt_vel, &
                                rx_inp =   rt_vel, &
                                ry_inp =   rz_vel, &
                                nx_out = o%nt_map, &
                                x0_out = o%t0_map, &
                                dx_out = o%dt_map, &
                                ry_out =   tz_loc    &
                              )
      !
      call cpucount ( o%c, o%i_tdc_map_compute_ir, 2 )
      !
      ! now check each map grid to see if it is outside the velocity grid
      !
      do_jt_map_time : do jt_map = 1 , o%nt_map
        !
        rt_map = ( jt_map - 1 ) * o%dt_map + o%t0_map
        !
        if_jt_map_time : if ( rt_map .lt. rt_vel ( 1 ) ) then
          !
          tz_loc ( jt_map ) = &
          rz_dtm + &
          .5 * max ( 0., rt_map - rt_dtm ) / rs_vel ( 1 )
          !
        else if ( rt_map .gt. rt_vel ( o%nt_vel ) ) then
          !
          tz_loc ( jt_map ) = &
          rz_vel ( o%nt_vel ) + &
          .5 * max ( 0., rt_map - rt_vel ( o%nt_vel ) ) &
                                / rs_vel ( o%nt_vel + 1 )
          !
        end if if_jt_map_time 
        !
      end do do_jt_map_time
      !
    else xxif_output_type_units
      !
      go to 998
      !
    end if xxif_output_type_units
    !
1999 continue
    !
    call cpucount ( o%c, o%i_tdc_map_compute_m1, 2 )
    !
    return
    !
998 continue
    !
    call pc_error ( &
    ' error in tdc_map_compute_m1 in output_type, output_units ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_map_compute_m1 ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine tdc_map_compute_m1   
  !
  subroutine tdc_map_apply ( o, hd_inp, tr_inp, i_err ) 
    !
    ! apply the map v0 map to tr_inp 
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    !
    double precision,     intent(inout) :: hd_inp(:) ! input headers (nh_inp)
    real,                 intent(inout) :: tr_inp(:) ! input trace (nt_inp)
    integer,              intent(inout) :: i_err     ! 0 o.k. -1 err
    !
    ! local variables
    !
    real                                :: rx_map         ! map x value
    real                                :: ry_map         ! map y value
    real                                :: tr_fft(o%nt_fft)! interp inp trace 
    real                                :: tz_x_y(o%nt_out)! map func at x, y
    integer                             :: it_fft(o%nt_out)! input map index
    !
    call cpucount ( o%c, o%i_tdc_map_apply, 1 )
    !
    i_err = 0
    !
    ! compute the input trace x,y location
    !
    rx_map = hd_inp ( o%hx_map )
    !
    ry_map = hd_inp ( o%hy_map )
    !
    ! interpolate the input trace, tr_inp on the original input grid
    ! to a temporary trace, tr_fft, on a fine computational grid
    !
    call tdc_inp_interpolate ( o, tr_inp, tr_fft )
    !
    ! compute the map function at this x,y lcoation
    !
    call tdc_map_interpolate ( o, rx_map, ry_map, tz_x_y, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! construct the output trace, tr_inp, from the interpolated input trace, 
    ! tr_fft, and the interpolated map grid, tz_x_y.
    !
    call tdc_out_interpolate ( o, tr_inp, tr_fft, tz_x_y , it_fft )
    !
    ! map the mute and lav header words
    !
    call tdc_map_head ( o, hd_inp, tr_inp, it_fft )
    !
1999 continue
    !
    call cpucount ( o%c, o%i_tdc_map_apply, 2 )
    !
    return
    !
998 continue
    !
    call pc_error ( ' error in tdc_map_apply during tdc_map_interpolate ' )
    !
    go to 999
    !
999 continue
    !
    call tdc_map_print ( &
                         o, &
                         pc_get_lun(), 'error in tdc_map_apply', &
                         .false., &
                         1, 1, 20, &
                         1, 1, 20, &
                         !1, o%nx_map, 20, &
                         !1, o%ny_map, 20, &
                         1, o%nt_map, 20 &
                         !1, o%nt_map, 20 &
                       )
    !
    call pc_error ( ' error in tdc_map_apply ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine tdc_map_apply  
  !
  subroutine tdc_inp_interpolate ( o, tr_inp, tr_fft )
    !
    ! interpolate the input trace, tr_inp on the original input grid
    ! to a temporary trace, tr_fft, on a fine computational grid
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    real,                 intent(inout) :: tr_inp(:) ! input  trace (nt_inp)
    real,                 intent(inout) :: tr_fft(:) ! output trace (nt_fft)
    !
    ! local variables
    !
    call cpucount ( o%c, o%i_tdc_inp_interpolate, 1 )
    !
    tr_fft(1:o%nt_fft) = 0.
    !
    ! interpolate the input trace into tr_fft
    !
    xxif_interpolation_type_fft : &
    if ( string_upper_compare ( o%interpolation_type, 'FFT' ) ) then
      !
      ! use fft interpolation
      ! 
      call interpolate_fft_apply ( o=o%interpolate_fft_obj, &
                                   tr_inp=tr_inp, &
                                   tr_out=tr_fft )
      !
    else xxif_interpolation_type_fft 
      !
      ! use linear interpolation
      !
      call interpolate_i_to_r_p ( &
                                  ix_inp_1=o%i1_inp, ix_inp_2=o%i2_inp, &
                                  fx_inp_1=o%f1_inp, fx_inp_2=o%f2_inp, &
                                                     ry_inp=tr_inp, &
                                  nx_out=o%nt_fft,   ry_out=tr_fft &
                                )
                       
      !
    end if xxif_interpolation_type_fft 
    !
    call cpucount ( o%c, o%i_tdc_inp_interpolate, 2 )
    !
    return
    !
  end subroutine tdc_inp_interpolate 
  !
  subroutine tdc_map_interpolate ( o, rx_map, ry_map, tz_x_y, i_err )
    !
    ! apply the map v0 map to tr_inp 
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    real,                 intent(in   ) :: rx_map       ! map x loc
    real,                 intent(in   ) :: ry_map       ! map y loc
    real,                 intent(inout) :: tz_x_y ( : ) ! map function
    integer,              intent(inout) :: i_err        ! 0 o.k. -1 err
    !
    ! local variables
    !
    integer                           :: jt_map        ! t map index
    integer                           :: jt_out        ! t out index
    !
    integer                           :: ix_map_1      ! x1 map index
    integer                           :: ix_map_2      ! x2 map index
    !
    integer                           :: iy_map_1      ! y1 map index
    integer                           :: iy_map_2      ! y2 map index
    !
    integer                           :: iy_mem_1      ! y1 mem index
    integer                           :: iy_mem_2      ! y2 mem index
    !
    real                              :: fx_map_1      ! x1 map coef
    real                              :: fx_map_2      ! x2 map coef
    !
    real                              :: fy_map_1      ! y1 map coef
    real                              :: fy_map_2      ! y2 map coef
    !
    real                              :: tz_map  (o%nt_map) ! x,  y  map
    real                              :: tz_x1_y1(o%nt_map) ! x1, y1 map
    real                              :: tz_x2_y1(o%nt_map) ! x2, y1 map
    real                              :: tz_x1_y2(o%nt_map) ! x1, y2 map
    real                              :: tz_x2_y2(o%nt_map) ! x2, y2 map
    !
    call cpucount ( o%c, o%i_tdc_map_interpolate, 1 )
    !
    i_err = 0
    !
    ! compute the x map interpolation indicies and coefficients
    !
    call interpolate_find_index ( o%nx_map, o%x0_map, o%dx_map, rx_map, &
                                  ix_map_1, ix_map_2, fx_map_1, fx_map_2 )
    !
    ! compute the y map interpolation indicies and coefficients
    !
    call interpolate_find_index ( o%ny_map, o%y0_map, o%dy_map, ry_map, &
                                  iy_map_1, iy_map_2, fy_map_1, fy_map_2 )
    !
    ! get the memory y trace index, iy_mem_1 for this disk y trace
    ! this will load nx_map x columns from disk into memory if needed.
    !
    call trbuf_get_index ( &
                           o        = o%trbuf, &
                           jr_mem   = iy_mem_1, &
                           jr_dsk   = iy_map_1, &
                           l_change = o%l_change, &
                           i_err    = i_err &
                         )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! copy the map function for x=ix_map_1, y=iy_map_1 to tz_x1_y1 
    !
    tz_x1_y1 ( 1:o%nt_map ) = o%trbuf%tr_mem ( 1:o%nt_map, ix_map_1, iy_mem_1 )
    !
    ! copy the map function for x=ix_map_2, y=iy_map_1 to tz_x2_y1 
    !
    tz_x2_y1 ( 1:o%nt_map ) = o%trbuf%tr_mem ( 1:o%nt_map, ix_map_2, iy_mem_1 )
    !
    ! get the memory y trace index, iy_mem_2 for this disk y trace
    ! this will load nx_map x columns from disk into memory if needed.
    !
    call trbuf_get_index ( &
                           o        = o%trbuf, &
                           jr_mem   = iy_mem_2, &
                           jr_dsk   = iy_map_2, &
                           l_change = o%l_change, &
                           i_err    = i_err &
                         )
    !
    if ( i_err .ne. 0 ) go to 997 
    !
    ! copy the map function for x=ix_map_1, y=iy_map_2 to tz_x1_y2 
    !
    tz_x1_y2 ( 1:o%nt_map ) = o%trbuf%tr_mem ( 1:o%nt_map, ix_map_1, iy_mem_2 )
    !
    ! copy the map function for x=ix_map_2, y=iy_map_2 to tz_x2_y2 
    !
    tz_x2_y2 ( 1:o%nt_map ) = o%trbuf%tr_mem ( 1:o%nt_map, ix_map_2, iy_mem_2 )
    !
    ! interpolate the map function for each of the map nodes
    ! between these four x,y corners
    !
    do_jt_map : do jt_map = 1 , o%nt_map
      !
                          tz_map   ( jt_map ) = &
    fx_map_1 * fy_map_1 * tz_x1_y1 ( jt_map ) &
  + fx_map_2 * fy_map_1 * tz_x2_y1 ( jt_map ) &
  + fx_map_1 * fy_map_2 * tz_x1_y2 ( jt_map ) &
  + fx_map_2 * fy_map_2 * tz_x2_y2 ( jt_map ) 
      !
    end do do_jt_map 
    !
    ! interpolate the map function from the map grid to the output grid
    !
    do_jt_out : do jt_out = 1 , o%nt_out
      !
                       tz_x_y (            jt_out   ) = &
    o%f1_out(jt_out) * tz_map ( o%i1_out ( jt_out ) ) &
  + o%f2_out(jt_out) * tz_map ( o%i2_out ( jt_out ) ) 
      !
    end do do_jt_out 
    !
1999 continue
    !
    call cpucount ( o%c, o%i_tdc_map_interpolate, 2 )
    !
    return
    !
997 continue
    !
    call pc_error ( ' error in tdc_map_interpolate in trbuf_get_index 2 ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( ' error in tdc_map_interpolate in trbuf_get_index 1 ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_map_interpolate ' )
    !
    i_err = 0
    !
    go to 1999
    !
  end subroutine tdc_map_interpolate 
  !
  subroutine tdc_out_interpolate ( o, tr_inp, tr_fft, tz_x_y, it_fft )
    !
    ! construct the output trace, tr_inp, from the interpolated input trace, 
    ! tr_fft, and the interpolated map grid, tz_x_y.
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    real,                 intent(inout) :: tr_inp(:) ! input  trace (nt_inp)
    real,                 intent(in   ) :: tr_fft(:) ! output trace (nt_fft)
    real,                 intent(in   ) :: tz_x_y(:) ! map function
    integer,              intent(inout) :: it_fft(:) ! map index
    !
    ! local variables
    !

    integer                             :: jt_out    ! out tim index
    real                                :: rt_fft_1  ! lower inter fft tim val
    integer                             :: it_fft_1  ! interp fft tim indices
    integer                             :: it_fft_2  ! interp fft tim indices
    real                                :: ft_fft_1  ! interp fft tim coef
    real                                :: ft_fft_2  ! interp fft tim coef
    real                                :: dt_inv    ! 1. / dt_fft
    !
    call cpucount ( o%c, o%i_tdc_out_interpolate, 1 )
    !
    ! construct the output trace, tr_inp, from the interpolated input trace, 
    ! tr_fft, and the interpolated map grid, tz_x_y.
    !
    ! for output=velocity this is a straight copy
    !
    tr_inp ( 1:o%nt_out ) = 0.
    !
    xxif_output_velocity : &
    if ( string_upper_compare ( o%output_type, 'VELOCITY' ) ) then
      !
      ! convert tz_x_y from slowness to velocity
      !
      tr_inp ( 1:o%nt_out ) = tz_x_y ( 1:o%nt_out )
      !
      if ( o%interpolate_inverse ) &
      tr_inp ( 1:o%nt_out ) = 1. / tr_inp ( 1:o%nt_out ) 
      !
      !if ( i0_out .eq. +1 ) write ( pc_get_lun(), '( &
      !& /, " tdc_map_apply ", &
      !& /, " jt_out rt_out tz_x_y tr_inp" &
      !& )' ) 
      !
      !if ( i0_out .eq. +1 ) write ( pc_get_lun(), '( &
      !& " q0", 1x, i5, 1x, g10.4, 1x, g10.4, 1x, g10.4 &
      !& )' ) &
      !( jt_out, &
      !(jt_out-1)*o%dt_out+o%t0_out, &
      !tz_x_y(jt_out), tr_inp(jt_out), jt_out = 1 , o%nt_out )
      !
    else xxif_output_velocity 
      !
      ! for output=dat we must map from input to output
      !
      ! map from time to depth or depth to time
      ! initialize the output trace to zero
      !
      tr_inp = 0.
      !
      dt_inv = 1. / o%dt_fft
      !
      ! loop over output samples
      !
      loop_jt_out: do jt_out = 1 , o%nt_out
        !
        ! compute the lower fft indice
        !
        it_fft_1 = nint ( ( tz_x_y(jt_out) - o%t0_fft ) * dt_inv )
        !
        ! compute the lower fft node time
        !
        rt_fft_1 = ( it_fft_1 - 1 ) * o%dt_fft + o%t0_fft
        !
        ! compute the lower fft node time
        !
        if ( rt_fft_1 .gt. tz_x_y(jt_out) ) it_fft_1 = it_fft_1 - 1
        !
        ! limit the two bracketting fft indices to the allowed range
        !
        it_fft_1 = max ( 1, min ( o%nt_fft, it_fft_1   ) )
        !
        it_fft_2 =          min ( o%nt_fft, it_fft_1+1 ) 
        !
        ! save the top index for mute computations
        !
        it_fft(jt_out) = it_fft_1
        !
        !if ( i0_out .eq. +1 ) &
        !write ( pc_get_lun(), '( &
        !& " q3 ",1x,g12.6,1x,g12.6,1x,i8,1x,i8 &
        !& )' ) &
        !(jt_out-1)*dt_out+t0_out,tz_x_y(jt_out),it_fft_1,jt_out
        !
        ! compute the lower fft node time
        !
        rt_fft_1 = ( it_fft_1 - 1 ) * o%dt_fft + o%t0_fft
        !
        ! compute the two bracketting fft interpolation coefficients
        !
        ft_fft_2 = max ( 0., min (1. , (tz_x_y(jt_out) - rt_fft_1 ) * dt_inv ) )
        ft_fft_1 = 1. - ft_fft_2
        !
        ! apply the map by interpolating between the two fft samples
        !
        xxif_past_end : if ( it_fft_1 .ge. o%nt_fft ) then
          tr_inp ( jt_out ) = 0.0
        else xxif_past_end 
          tr_inp ( jt_out ) = ft_fft_1 * tr_fft ( it_fft_1 ) &
                            + ft_fft_2 * tr_fft ( it_fft_2 )
        end if xxif_past_end 
        !
        ! write statistics for the first output trace
        !
        !if ( i0_out .eq. +1 .and. jt_out .eq. 1 ) write ( pc_get_lun(), '( &
        !& /, " tdc_map_apply ", &
        !& /, " jt_out it_fft_1 it_fft_2 ft_fft_1 ft_fft_2 ", &
        !& "t_out tz_x_y tr_fft tr_inp" &
        !& )' ) 
        !
        !if ( i0_out .eq. +1 ) write ( pc_get_lun(), '( &
        !& " q1", 1x, i5, 1x, i5, 1x, i5, 1x, &
        !& f7.5, 1x, f7.5, 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4 &
        !& )' ) &
        !jt_out, it_fft_1, it_fft_2, ft_fft_1, ft_fft_2, &
        !(jt_out-1)*o%dt_out+o%t0_out, &
        !tz_x_y(jt_out), tr_fft(it_fft_1), tr_inp(jt_out)
        !
      end do loop_jt_out
      !
    end if xxif_output_velocity 
    !
    call cpucount ( o%c, o%i_tdc_out_interpolate, 1 )
    !
    return
    !
  end subroutine tdc_out_interpolate 
  !
  subroutine tdc_map_head ( o, hd_inp, tr_inp,it_fft )
    !
    ! map tre trace header words
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    double precision,     intent(inout) :: hd_inp(:) ! input headers (nh_inp)
    real,                 intent(in   ) :: tr_inp(:) ! input  trace (nt_inp)
    integer,              intent(in   ) :: it_fft(:) ! map index
    !
    ! local variables
    !
    integer                             :: top_mute_inp  ! top    inp mute index
    integer                             :: bot_mute_inp  ! bottom inp mute index
    integer                             :: top_mute_out  ! top    out mute index
    integer                             :: bot_mute_out  ! bottom out mute index
    !
    call cpucount ( o%c, o%i_tdc_map_head, 1 )
    !
    ! get the input top and bottom mute in interpolated input coordinates
    !
    top_mute_inp = &
    nint ( hd_inp(hdr_top_mute   ) - 1. ) * o%interpolation_input + 1
    !
    bot_mute_inp = &
    nint ( hd_inp(hdr_bottom_mute) - 1. ) * o%interpolation_input + 1
    !
    xxif_output_velocity : &
    if ( string_upper_compare ( o%output_type, 'VELOCITY' ) ) then
      !
      ! set the top and bottom mutes
      !
      top_mute_out = 1
      !
      bot_mute_out = o%nt_out
      !
    else xxif_output_velocity 
      !
      ! compute the output top and bottom mute indices
      !
      call tdc_map_mute ( top_mute_inp, top_mute_out, o%nt_out, it_fft )
      !
      call tdc_map_mute ( bot_mute_inp, bot_mute_out, o%nt_out, it_fft )
      !
    end if xxif_output_velocity 
    !
    ! set the output mute indices
    !
    hd_inp ( hdr_top_mute    ) = top_mute_out
    !
    hd_inp ( hdr_bottom_mute ) = bot_mute_out
    !
    hd_inp ( hdr_lav )  = lav ( tr_inp ( 1:o%nt_out ), o%nt_out )  ! max amp
    !
    call cpucount ( o%c, o%i_tdc_map_head, 1 )
    !
    return
    !
  end subroutine tdc_map_head 
  !
  subroutine tdc_map_mute ( mute_inp, mute_out, nt_out, jt_inp )
    !
    ! compute a mute index
    !
    ! Arguments
    !
    integer,            intent(in   ) :: nt_out        ! number output samples
    integer,            intent(in   ) :: jt_inp(:)     ! input indices
    integer,            intent(in   ) :: mute_inp      ! input  mute index
    integer,            intent(inout) :: mute_out      ! output mute index
    !
    ! local variables
    !
    integer                           :: jt_out             ! output time index
    !
    ! loop over output samples
    ! search through the input indices until we find one >= the mute index
    !
    loop_jt_out: do jt_out = 1 , nt_out
      !
      mute_out = jt_out
      !
      if ( jt_inp(jt_out) .ge. mute_inp ) go to 1
      !
    end do loop_jt_out
    !
    mute_out = nt_out
    !
  1 continue
    !
    return
    !
  end subroutine tdc_map_mute  
  !
  subroutine tdc_map_print ( &
                             o, &
                             lu_out, c_title, &
                             l_map_print, &
                             jx_map_1, jx_map_2, jx_map_0, &
                             jy_map_1, jy_map_2, jy_map_0, &
                             jt_map_1, jt_map_2, jt_map_0 &
                           ) 
    !
    ! print the map tz_map
    !
    type ( tdc_struct ),  intent(inout) :: o            ! tdc structure
    !
    integer,            intent(in   ) :: lu_out        ! print unit
    character(len=*),   intent(in   ) :: c_title       ! print title
    !
    logical,            intent(in   ) :: l_map_print   ! print map flag
    !
    integer,            intent(in   ) :: jx_map_1      ! first x sample to print
    integer,            intent(in   ) :: jx_map_2      ! last  x sample to print
    integer,            intent(in   ) :: jx_map_0      ! inc   x sample to print
    !
    integer,            intent(in   ) :: jy_map_1      ! first y sample to print
    integer,            intent(in   ) :: jy_map_2      ! last  y sample to print
    integer,            intent(in   ) :: jy_map_0      ! inc   y sample to print
    !
    integer,            intent(in   ) :: jt_map_1      ! first t sample to print
    integer,            intent(in   ) :: jt_map_2      ! last  t sample to print
    integer,            intent(in   ) :: jt_map_0      ! inc   t sample to print
    !
    ! local variables
    !
    integer                           :: i_err
    integer                           :: jy_mem
    integer                           :: jx_map
    integer                           :: jy_map
    !
    integer                           :: jt_map
    real                              :: rt_map
    !
    ! print the map grid
    !
    i_err = 0
    !
    if ( lu_out .lt. 0 ) return
    !
    write ( lu_out, '( &
    & /, " tdc_map_print ", a, &
    & /, " tdc_map_print interpolate_original=", l2, &
    & /, " tdc_map_print interpolate_inverse =", l2, &
    & /, " tdc_map_print output_type =", a, &
    & /, " tdc_map_print input_units =", a, &
    & /, " tdc_map_print output_units=", a, &
    & /, " tdc_map_print vel_type    =", a, &
    & /, " tdc_map_print map_type    =", a, &
    & /, " tdc_map_print vel_parm    =", a, &
    & /, " tdc_map_print memory_size =", i12, &
    & /, " tdc_map_print trbuf_memory=", i12, &
    & /, " tdc_map_print map_size    =", i12, &
    & /, " tdc_map_print vel_size    =", i12, &
    & /, " tdc_map_print nx_map=", i8, " x0_map=", g12.6, " dx_map=", g12.6, &
    & /, " tdc_map_print ny_map=", i8, " y0_map=", g12.6, " dy_map=", g12.6, &
    & /, " tdc_map_print nt_crs=", i8, " t0_crs=", g12.6, " dt_crs=", g12.6, &
    & /, " tdc_map_print nt_vel=", i8, " t0_vel=", g12.6, " dt_vel=", g12.6, &
    & /, " tdc_map_print nt_fft=", i8, " t0_fft=", g12.6, " dt_fft=", g12.6, &
    & /, " tdc_map_print nt_inp=", i8, " t0_inp=", g12.6, " dt_inp=", g12.6, &
    & /, " tdc_map_print nt_map=", i8, " t0_map=", g12.6, " dt_map=", g12.6 &
    & )' ) &
    trim(c_title), &
    o%interpolate_original, o%interpolate_inverse, &
    trim(o%output_type), trim(o%input_units), trim(o%output_units), &
    trim(o%vel_type), trim(o%map_type), trim(o%vel_parm), &
    o%memory_size, o%trbuf_memory, &
    o%nt_map * o%nx_map * o%ny_map, &
    o%nt_vel * o%nx_vel * o%ny_vel, &
    o%nx_map, o%x0_map, o%dx_map, &
    o%ny_map, o%y0_map, o%dy_map, &
    o%nt_crs, o%t0_crs, o%dt_crs, &
    o%nt_vel, o%t0_vel, o%dt_vel, &
    o%nt_fft, o%t0_fft, o%dt_fft, &
    o%nt_inp, o%t0_inp, o%dt_inp, &
    o%nt_map, o%t0_map, o%dt_map
    !
    xxif_map_print : if ( l_map_print ) then
      !
write ( lu_out, '( &
& /, " tdc_map_print ", &
& /, " tdc_map_print jx_map_1=", i8, " jx_map_2=", i8, " jx_map_0=", i8, & 
& /, " tdc_map_print jy_map_1=", i8, " jy_map_2=", i8, " jy_map_0=", i8, & 
& /, " tdc_map_print jt_map_1=", i8, " jt_map_2=", i8, " jt_map_0=", i8 & 
      & )' ) &
      jx_map_1, jx_map_2, jx_map_0, &
      jy_map_1, jy_map_2, jy_map_0, &
      jt_map_1, jt_map_2, jt_map_0
      !
      write ( lu_out, '( &
      & /, "   it_map   ix_map   iy_map  t_inp       rt_out         ipn", &
      & " tdc_map_print " &
      & )' ) 
      !
      loop_jy_map: do jy_map = jy_map_1 , jy_map_2 , jy_map_0
        !
        ! get the memory y trace index, jy_mem for this disk y trace
        ! this will load nx_map x columns from disk into memory if needed.
        !
        call trbuf_get_index ( &
                               o        = o%trbuf, &
                               jr_mem   = jy_mem, &
                               jr_dsk   = jy_map, &
                               l_change = o%l_change, &
                               i_err    = i_err &
                             )
        !
        if ( i_err .ne. 0 ) go to 998
        !
        loop_jx_map: do jx_map = jx_map_1 , jx_map_2 , jx_map_0
          !
          loop_jt_map: do jt_map = jt_map_1 , jt_map_2 , jt_map_0
            !
            rt_map = ( jt_map - 1 ) * o%dt_map + o%t0_map  ! map time
            !
            write ( lu_out, '( &
            & 1x, i8, 1x, i8, 1x, i8, 1x, g12.6, 1x, g12.6, 1x, i3, &
            & " tdc_map_print_a " &
            & )' ) &
            jt_map, jx_map, jy_map, &
            o%trbuf%tr_mem ( jt_map, jx_map, jy_mem ), rt_map, o%ipn
            !
          end do loop_jt_map
          !
        end do loop_jx_map
        !
      end do loop_jy_map
      !
    end if xxif_map_print 
    !
    return
    !
998 continue
    !
    call pc_error ( ' error in tdc_map_print during trbuf_get_index ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in tdc_map_print ' )
    !
    i_err = 0
    !
    return
    !
  end subroutine tdc_map_print  
  !
  subroutine tdc_set_constant_path ( opt_file )
    !
    !  set the constant_path to CONSTANT or PATH
    !
    character(len=*),   intent(inout) :: opt_file
    !
    call string_to_upper ( opt_file )
    !
    xxif_constant : if ( string_upper_compare ( opt_file(1:1), 'C' ) ) then
      !
      opt_file = 'CONSTANT'
      !
    else xxif_constant
      !
      opt_file = 'PATH'
      !
    end if xxif_constant
    !
    return
    !
  end subroutine tdc_set_constant_path
  !
  subroutine tdc_line_feed ( c_title )
    !
    ! add a line feed to the pc deck
    !
    character(len=*), intent(in   ) :: c_title 
    !
    ! local variables
    !
    character(len=60)               :: c_title_0
    !
    ! add a line feed
    !
    c_title_0 = &
'------------------------------------------------------------------------'
!123456789012345678901234567890123456789012345678901234567890123456789012
    !
    !c_title_0 (1:len_trim(c_title)) = trim ( c_title ) 
    !
    c_title_0 (60-len_trim(c_title):) = ' '
    !
    call pc_put ( c_title, c_title_0 )
    !
    return
    !
  end subroutine tdc_line_feed 
  !
end module tdc_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
