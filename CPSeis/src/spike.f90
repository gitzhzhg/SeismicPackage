!<CPS_v1 type="PROCESS"/>
!
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
! Name       : SPIKE
! Category   : Synthetics
! Written    : 2000-01-13   by: Douglas Hanson
! Revised    : 2007-09-11   by: Douglas Hanson Make parallel.
! Maturity   : beta
! Purpose    : Generate synthetic traces.
! Portability: No known limitations.
! Parallel   : No.
!
!------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!  SPIKE can generate traces using a variety of methods to describe event times.
!
!------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!  SPIKE generates synthetic traces.
!
!  Control of the trace output locations is described in
!  ( NOTE 1 ) TRACE LOCATIONS.
!
!  Control of the output trace ordering is described in
!  ( NOTE 2 ) TRACE ORDERING.
!
!  Control of the trace event locations and noise is described in
!  ( NOTE 3 ) TRACE EVENT TRAVEL TIMES AND AMPLITUDES.
!
!  Control of the noise added to each output trace is described in
!  ( NOTE 4 ) TRACE RANDOM NOISE
!
!  Control of the trace wavelet and filter characteristics is described in
!  ( NOTE 5 ) TRACE WAVELETS AND FILTERING
!
!
!  ( NOTE 1 ) TRACE LOCATIONS
!
!  SPIKE generates a set of ouptut traces on a spatial grid defined
!  by parameters:
!
!   AZIMUTH                                - Trace azimuth in degrees
!  OFF_X_TOT,  OFF_X_INIT,  OFF_X_LAST,  OFF_X_INC - Offset.
!  GAT_X_TOT, GAT_X_INIT, GAT_X_LAST, GAT_X_INC 
!  Fast changeing lateral dimension.
!  GAT_Y_TOT, GAT_Y_INIT, GAT_Y_LAST, GAT_Y_INC 
!  Slow changeing lateral dimension.
!
!  in the order specified by parameter GATHER_TYPE.
!
!  The output trace time grid characterisitics are defined by the job global
!  time parameters.
!
!  HDR_X and HDR_Y and define the fast and slow lateral
!  position variation of ouput traces.  The allowed combinations are:
!  HDR_X =  7, HDR_Y =  8
!  HDR_X =  8, HDR_Y =  7
!  HDR_X = 17, HDR_Y = 18
!  HDR_X = 18, HDR_Y = 17
!
!  For the FAST, SLOW and OFF header words:
!  If the value of INIT < LAST output headers will vary from small to large.
!  If the value of INIT > LAST output headers will vary from large to small.
!
!  The trace azimuth is based upon the CPS definition and is independant of
!  which header words are selected for HDR_X and HDR_Y and is defined
!  in degrees.
!  Zero   degree azimuth is parrellel to the X axis.
!  Ninety degree azimuth is parrellel to the Y axis.
!
!
!  ( NOTE 2 ) TRACE ORDERING
!
!  Parameter GATHER_TYPE controls the order of the header word variation
!  within the output traces, that is the type of goup output by SPIKE.
!
!  If GATHER_TYPE=OFFSET, the data is output in common offset order.
!  Then the grids defined by FAST and SLOW parameters define the midpoint
!  positions of the output traces.  The trace source and receiver positions
!  will be determined from the offset, azimuth and midpoint values.
!
!  If GATHER_TYPE=CMP, the data is output in common CMP order.
!  Then the grids defined by FAST and SLOW parameters define the midpoint
!  positions of the output traces.  The trace source and receiver positions
!  will be determined from the offset, azimuth and midpoint values.
!
!  If GATHER_TYPE=SHOT, the data is output in common shot order.
!  Then the grids defined by FAST and SLOW parameters define the shot
!  positions of the output traces.  The trace receiver and midpoint positions
!  will be determined from the offset, azimuth and source values.
!
!  If GATHER_TYPE=RECEIVER, the data is output in common receiver order.
!  Then the grids defined by FAST and SLOW parameters define the receiver
!  positions of the output traces.  The trace source and midpoint positions
!  will be determined from the offset, azimuth and receiver values.
!
!  If GATHER_TYPE=HYBRID, the data is output in hybrid gather order.
!  Then the grids defined by FAST and SLOW parameters define the source and 
!  receiver positions of the output traces.  
!  The sources will follow the x_offset = 0 values
!  The receivers will follow the y_offset = 0 values
!
!  SPIKE passes out its traces up to GROUP_SIZE traces at a time.
!  However SPIKE will not pass out traces from two different groups together.
!  Hence if you define GATHER_TYPE=CMP, OFF_X_TOT=24, GROUP_SIZE=10
!  SPIKE will pass out traces of each CMP groups of 10, 10, 4, 10, 10, 4 ...
!  So the traces from one CMP will not be combined with those from another CMP.
!
!
!  ( NOTE 3 ) TRACE EVENT TRAVEL TIMES AND AMPLITUDES.
!
!  The trace spike locations and amplitudes are a combination of 5
!  different types of events.  Any of these events can be turned on or off
!  through the apporopriate parameter slections.
!
!  1 - evenly sampled events with amplitude SPIKE_AMP, on the time grid
!  defined by SPIKE_AMP, SPIKE_TOT, SPIKE_MIN, SPIKE_MAX, SPIKE_INC,
!  The amplitude of these events are scaled by 1. / MAX(.1,TIME)**PWR_DECAY
!  These events can be turned off by setting the spike amplitude, SPIKE_AMP,
!  to zero or by setting the number of spikes, SPIKE_TOT, to zero.
!  The spikes defined by SPIKE_AMP, SPIKE_TOT, SPIKE_MIN, SPIKE_MAX, and
!  SPIKE_INC will be the same on every output trace.
!
!  2 - events with travel times and amplitudes associated with the reflectors
!  defined by AMP_REF, DEP_REF, X_DIP_REF, Y_DIP_REF
!  The amplitude of these events are scaled by
!  COSINE(DIP)**COS_DECAY / MAX(.1,TIME)**PWR_DECAY
!  These events can be turned off by setting the reflector amplitude, AMP_REF,
!  to zero or by not entering any values into the reflector arrays.
!
!  3 - events with travel times and amplitudes associated with the diffractors
!  defined by AMP_DIF, DEP_DIF, X_LOC_DIF, Y_LOC_DIF
!  The amplitude of these events are scaled by
!  COSINE(DIP)**COS_DECAY / MAX(.1,TIME)**PWR_DECAY
!  These events can be turned off by setting the diffractor amplitude, AMP_DIF,
!  to zero or by not entering any values into the diffractor arrays.
!
!  4 - events with travel times and amplitudes associated with the diffractors
!  contained in file PATH_DIF.
!  PATHENAME _DIF is an ascii file.  The first line of this file are two
!  integers defining the fast and slow coordinate system of the diffractor
!  locations contained in the file.  Note these can be in the opposite order
!  from the fast and slow trace definitions used above but should be in the
!  same units.  That is if you define HDR_X=7, HDR_Y=8 the fast and slow
!  header words in the diffractor file should be 7,8 or 8,7. The remaining lines
!  of the file each contain 4 values describing the amplitude, fast and slow
!  lateral positions and depth of a point diffractor.
!  The amplitude of these events are scaled by
!  COSINE(DIP)**COS_DECAY / MAX(.1,TIME)**PWR_DECAY
!  These events can be turned off by setting the diffractor amplitude in the
!  file to zero or settig PATH_DIF=NONE.
!
!  5 - Gaussian distributed pseudo-random values at each output time sample
!  with mean zero and standard deviation AMP_NOISE.
!  The amplitude of these events is NOT affected by the PWR_DECAY parameter.
!  These events can be turned off by setting the standard deviation,
!  AMP_NOISE, to zero, or by setting the noise type, NOISE_TYPE, to DEAD.
!
!  You can duplicate the original RNSYN events by setting the number of
!  spike events, SPIKE_TOT, or their amplitude SPIKE_AMP, to zero and by
!  ommitting any reflectors or diffractors.
!  You can get zero noise by setting the random number standard deviation,
!  AMP_NOISE, to zero.
!
!  All computations are actually done on a time grid TIM_INTERP times finer
!  than the final output time grid defined by the job setup globals.  The
!  output trace samples are extracted from this finer computational grid.  This
!  allows the user to get more accurate positioning of reflection and
!  diffraction events.  If you set the wavlet type to a spike,
!  WAVELET_TYPE=SPIKE, and choose no filtering of either the wavelet or trace,
!  FILTER_LEVEL=NONE,  then TIM_INTERP will be set to 1 so that no spike will
!  be missing from the output traces.
!
!  Parameter PATH_VEL defines the three dimensionaly varying straight
!  ray velocity field used to determine the travel time to the dipping
!  reflectors and the diffractors.
!
!  The two way vertical time to any dipping plane reflector or point
!  diffractor is 2. * depth / rms velocity.  The velocity used for the
!  travel time is that at the reflecting or diffracting position.
!
!  The plane dipping reflectors are defined by their amplitude AMP_REF, their
!  depth, DEP_REF, at FAST=0., SLOW=0., and their dip in the fast and slow
!  directions, X_DIP_REF and Y_DIP_REF respectively.  X_DIP_REF and Y_DIP_REF
!  are the dip in degrees in the X and Y directions respectively.
!  distance respectively.
!
!  For the travel time computation method for plane dipping reflectors see:
!
!  Slotnick - Lessons in Seismic Computing pp 145.
!
!  and
!
!  Data Reconstruction and Reflecitivy Mapping in 3 Dimensions.
!  Appendix A
!  Robert H. Stolt
!  Conoco research report 2558-1-98 October 98
!
!  The point diffractors are defined by their amplitude AMP_DIF, and their
!  spatial locations in depth, and the fast and slow directions,
!  DEP_DIF, X_LOC_DIF, Y_LOC_DIF, respectively.
!
!
!  ( NOTE 4 ) TRACE RANDOM NOISE
!
!  Parameter NOISE_TYPE controls the type of random number noise pattern for
!  each output trace.
!
!  If NOISE_TYPE=SAME, the same set of random spikes is added to each trace.
!  If NOISE_TYPE=DIFF, a different set of random spikes is added to each trace.
!  If NOISE_TYPE=DEAD, the traces contain only zero values.
!
!  In any case the mute header words (2 and 64) are set to 1 and NDPT.
!  Thus, it is assumed that you may supply live values to any dead traces.
!
!  NOISE_TYPE = SAME and NOISE_TYPE = DEAD, are very fast because only ONE
!  trace is generated at setup time; this trace is then passed out over and
!  over, with only the headers varying.  NOISE_TYPE = DIFF is slower, but
!  should be used if you need different random noise values in each trace.
!
!
!  ( NOTE 5 ) TRACE WAVELETS AND FILTERING
!
!  Output traces are constructed by convolving a series of spikes
!  with a wavelet defined by parameters WAVELET_TYPE and WAVELET_LENGTH.
!
!  Both the wavelet and convolved output trace can be filtered by a bandpass
!  filter defined by the parameters:
!  FREQ_LOW_NONE, FREQ_LOW_FULL, FREQ_HIGH_FULL, FREQ_HIGH_NONE and PHASE.
!
!  Parameter FILTER_LEVEL controls how the bandpass filter is applied to the
!  wavelet and or the trace.  It can be applied to either on, both or neiher.
!
!  If FILTER_LEVEL = WAVELET the filtered wavelet is convolved with spikes
!  to form the output trace.  The trace is not filtered after convolution.
!
!  If FILTER_LEVEL = TRACE the unfiltered wavelet is convolved with spikes
!  to form the output trace.  The trace is filtered after convolution.
!
!  If FILTER_LEVEL = BOTH the filtered wavelet is convolved with spikes
!  to form the output trace.  The trace is filtered after convolution.
!
!  If FILTER_LEVEL = NONE the unfiltered wavelet is convolved with spikes
!  to form the output trace.  The trace is not filtered after convolution.
!
!  Filtering the traces tend to give larger side lobes than filtering
!  the wavelet only.
!
!  Parameter WAVELET_TYPE controls the type of wavelet convolved with the
!  output traces.  Currently a spike, a ricker, a gaussian or its first two
!  derivitives is supported.
!
!  For a ricker wavelet the expression is:
!
!  WAVELET(TIME) =
!  ( 1 - 2 * PI**2 * FREQ_RICKER * TIME**2 ) * EXP (-(PI*FREQ_RICKER*TIME)**2)
!
!  Where FREQ_RICKER is the central frequeuncy of the filter and is
!  (FREQ_LOW_NONE+FREQ_HIGH_NONE) * .5
!  Make sure WAVELET_LENGTH is large enough to capture the ricker wavelet.
!
! SOURCE AND RECEIVER DATUMS
!
! For depth migration spike computes travel times from the source and receiver
! surface positions to image points.
! spike gets the source and receiver surface
! depths from either constant values defined by CONST_GAT_DATUM and
! CONST_TIG_DATUM or from the spatialy varying datum files PATH_GAT_DATUM
! and PATH_TIG_DATUM.
!
! If the source andreceivers have the same datum depths the user should use the
! PATH_TIG_DATUM = PATH_GAT_DATUM and RECEVIER_DATUM=CONST_GAT_DATUM
!
! If PATH_GAT_DATUM is other than NONE, spike gets the source datum depths from
! that file.  Other wise spike set the source datum depths = to the constant
! CONST_GAT_DATUM.  SPIKE acts similarly for the receiver datum.
!
! The first two cards of the datum file describe a uniform cartesion grid on
! which the datum depths are posted while the later cards define the
! datum X, Y, depth coordinates.
!
! Card 1 contains the X header word, number of points, origin and increment.
! Card 2 contains the Y header word, number of points, origin and increment.
! Card 3-nx_fil*ny_fil+2 contain the X and Y locations and datum depths with 1
! value per card in file X, Y order.
! Note the file X and Y header words must be either the image X and Y header
! words or the image Y and X header words.
!
! The following is an example of a simple datum file which has 3 X nodes
! defined by header word 7 starting at 0 and incrementing by 100. and 2
! Y nodes defined by header word 8 starting at 0 and incrementing
! by 200.
!
! 7, 3, 0., 100.      ! X header, number, origin, increment
! 8, 2, 0., 200.      ! Y header, number, origin, increment
!   0.,   0., -100.   ! depth at X =   0., Y =   0.
! 100.,   0., -200.   ! depth at X = 100., Y =   0.
! 200.,   0., -300.   ! depth at X = 200., Y =   0.
!   0., 200.,  200.   ! depth at X =   0., Y = 200.
! 100., 200., -300.   ! depth at X = 100., Y = 200.
! 200., 200., -400.   ! depth at X = 200., Y = 200.
!
!
!------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! NO input traces -- spike is a trace supplying process.
!
!------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs GROUP_SIZE traces at a time.
! This is a trace-supplying process.
!
!------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                          Action taken
! ----      -----------                          ------------
! NUMTR     max number of traces input/output    Set to 1
! GATHERED  whether traces are gathered          Set to .false.
! NWIH      number of words in trace header      used but not changed
! NDPT      number of sample values in trace     used but not changed
!
!------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! HDR     Description                Action taken
! ----    -----------                ------------
! 1       Sequential Trace Count     Set
! 2       Head mute                  Set
! 3       Current gather number      Set
! 4       No. within current gather  Set
! 64      Tail mute                  Set
!         All X,Y headers            Set
!
!------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author         Description
!     ----        ------         -----------
! 61  2007-09-11  Douglas Hanson Make parallel.
! 60  2007-05-03  Douglas Hanson Add OPT_GHOST.
! 59  2007-02-22  Douglas Hanson Fix datumgrid pe index 2.
! 58  2007-02-06  Douglas Hanson Fix datumgrid pe index.
! 57  2007-01-03  Douglas Hanson New matfun_filter list.
! 56  2006-10-10  D. Glover      Added NULLIFY statements for Intel compiler
! 55  2006-09-21  Douglas Hanson use r index.
! 54  2006-06-22  Douglas Hanson Change tab order.
! 53  2006-06-08  Douglas Hanson Add OPT_SPIKE.
! 52  2006-04-20  Douglas Hanson New layout.
! 51  2006-04-18  Douglas Hanson Remove print.
! 50  2006-04-04  Douglas Hanson Use syn modules.
! 49  2006-03-28  Douglas Hanson Use datumgrid frontend.
! 48  2005-08-16  Ioan Vlad      Accomodate new zoeppritz API
! 47  2005-08-09  Karen Goodger  Change argument n0_inp in routine compute
!                                from intent(out) to intent(inout) to make
!                                absoft 9.0 happy.
! 46  2005-05-24  Douglas Hanson Restrict valid z r depth.
! 45  2005-05-12  Douglas Hanson Add HYBRID gathers.
! 44  2005-03-24  Douglas Hanson Add OPT_OFFSET_SIGN
! 43  2005-01-31  Douglas Hanson Add OPT_INPUT=DUPLICATE.
! 42  2004-09-21  Douglas Hanson Add OPT_INPUT.
! 41  2004-08-31  Douglas Hanson Fix ang_inc bug in spike_compute_dif_0.
! 40  2004-08-17  Michael Ried   User can now change HDR_X with no error
! 39  2004-04-16  Douglas Hanson Fix different src, rec datums.
! 38  2004-03-02  Douglas Hanson Fix azimuth=0 bug.
! 37  2004-01-13  Douglas Hanson Use const_gat_datum and cosnt_tig_datum.
!                                to be consistent with kmig.
! 36  2003-10-09  Douglas Hanson Add i0_ref index.
! 35  2003-08-18  Douglas Hanson Add line feeds for pc_put.
! 34  2003-07-31  R.S.Day        Fixed yo_out, xo_out binning logic for
!                                cmp,src,rec gathers in spike_compute_trace.
!                                Eliminated lines that forced rx_off ry_off>0
! 33  2003-06-19  Douglas Hanson Fix jt_out index error.
! 32  2003-06-11  Douglas Hanson Add V(z) opton.
! 31  2003-02-26  Douglas Hanson Fix offset header value.
! 30  2003-02-05  Douglas Hanson Fix output group indicies.
! 29  2003-02-04  Douglas Hanson Add print of dip info.
! 28  2003-01-31  Douglas Hanson Fix negative offset.
! 27  2003-01-30  Douglas Hanson Input dip in degrees.
! 26  2002-12-16  Douglas Hanson Make subroutines public.
! 25  2002-04-15  Douglas Hanson Change pc_error to pc_info.
! 24  2002-02-27  Douglas Hanson Fix TIM_TYPE options.
! 23  2002-02-21  Douglas Hanson Change V_GAT_RCV to V_GAT_TIG.
! 22  2002-02-04  Douglas Hanson Add y_off coeff.
! 21  2001-10-25  Karen Goodger  Rename labels beginning with if to get around
!                                intel compiler bug.
! 20  2001-08-23  Douglas Hanson Add pathchoose.
! 19  2001-08-10  Faqi Liu       Fix azimuth conversion (degree to radian) bug.
! 18  2001-08-02  Douglas Hanson Fix old datumgrid bug.
! 17  2001-06-28  R.S.Day        Bypass interpolate for datumgrid when nxpos==0
! 16  2001-06-21  Douglas Hanson Use datumgrid.
! 15  2001-06-20  Douglas Hanson Add v_gat_tig.
! 14  2001-06-14  Douglas Hanson Add source and receiver datums.
! 13  2001-03-21  Douglas Hanson Correct azimuth usage.
! 12  2001-03-05  Douglas Hanson Correct gathered flag.
! 11  2001-01-15  Douglas Hanson Correct use of no_more_traces
! 10  2000-12-12  Douglas Hanson Add skip_wrapup
!  9  2000-10-30  Douglas Hanson Set spike location with nint.
!  8  2000-10-18  Douglas Hanson Fix header bug.
!  7  2000-09-19  Douglas Hanson No if test for control cards.s
!  6  2000-09-06  Douglas Hanson Modify velgrid_size usage
!  5  2000-09-06  Douglas Hanson fix COS_DECAY error.
!  4  2000-09-05  Douglas Hanson adopt cio_
!  3  2000-08-25  Douglas Hanson cpsfcr
!  2  2000-06-08  Brad Kruse     Review coding standards
!  1  2000-01-13  Douglas Hanson Original Version
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
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
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
!    NTR = 1- GROUP_SIZE   if this process is outputting traces.
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
!<NS SPIKE_DATA/NC=80>
!
! Create synthetic traces on a grid
!
! NUM_CPUS=`IIIIIII  BROADCAST_TRACES=`CC
!
!<include syngrid.f90>
!
!<NS SPIKE_EVENTS/NC=80>
!
!<include synref.f90>
!
!<NS SPIKE_VELOCITY/NC=80>
!
!<include synvofz.f90>
!
!<NS SPIKE_DATUM/NC=80>
!
!<include datumgrid.f90>
!
!<NS SPIKE_WAVELET/NC=80>
!
!<include wavelet.f90>
!
!<PARMS AMP_REF_ARRAYSET[/ML=128/XST/YST]>
!<PARMS DN1_REF_ARRAYSET[/ML=128/XST/YST]>
!<PARMS AMP_DIF_ARRAYSET[/ML=128/XST/YST]>
!<PARMS DN1_DIF_ARRAYSET[/ML=128/XST/YST]>
!
!<PARMS SPIKE_DATA[screen1]>
!<PARMS SPIKE_EVENTS[screen2]>
!<PARMS SYNREF_REFLECTORS[screen3]>
!<PARMS SYNREF_DIFFRACTORS[screen4]>
!<PARMS SYNREF_PICKS[screen5]>
!<PARMS SPIKE_VELOCITY[screen6]>
!<PARMS SPIKE_DATUM[screen7]>
!<PARMS SPIKE_WAVELET[screen8]>
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="NUM_CPUS">
!<Tip> Number of cpus in job. </Tip>
! Default = 1
! Allowed = int>0
!
!<Help KEYWORD="BROADCAST_TRACES">
!<Tip> Broadcast traces to all processors. </Tip>
! Default = NO
! Allowed = NO
! Allowed = YES
!
!</HelpSection>
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!
!
module spike_module
  !
  ! Module references
  !
  use amod_module
  use cio_module
  use getlun_module
  use grid_module
  use cpucount_module
  use cpsio_module
  use datumgrid_module
  use headsave_module
  use interpolate_module
  use matfun_module
  use memfun_module
  use migfun_module
  use named_constants_module
  use pathchoose_module
  use pathcheck_module
  use pattern_module
  use pc_module
  use pcpsx_module
  use string_module
  use syngrid_module
  use synref_module
  use synvofz_module
  use timeglob_module
  use velgrid_module
  use wavelet_module
  use zoeppritz_module
  !
  implicit none
  !
  private
  !
  public :: spike_create
  public :: spike_delete
  public :: spike_initialize
  public :: spike_update
  public :: spike
  public :: spike_wrapup
  public :: spike_compute
  public :: spike_compute_velocity 
  public :: spike_compute_trace
  public :: spike_compute_event
  public :: spike_compute_ref
  public :: spike_compute_dif
  public :: spike_compute_dif_vofz
  public :: spike_compute_dif_cons
  !
  ! rcs identifier string
  character(len=100),public,save :: spike_ident = &
  "$Id: spike.f90,v 1.61 2007/09/12 14:29:47 Hanson beta sps $"
  !
  !integer,    parameter :: n_yes_no = 2
  !character(len=3),save :: c_yes_no(n_yes_no) &
  != (/ 'YES', 'NO ' /)
  !
  type, public :: spike_struct
    !
    private
    !
    logical                                 :: skip_wrapup    ! wrapup flag
    !
    integer                                 :: num_cpus
    logical                                 :: broadcast_traces
    !
    integer                                 :: nh_inp
    integer                                 :: nt_glb
    real                                    :: t0_glb
    real                                    :: t1_glb
    real                                    :: dt_glb
    !
    integer                                 :: i0_ref
    integer                                 :: i0_dif
    integer                                 :: i0_pik
    !
    real                                    :: rt_out
    !
    integer                                 :: mh_inp
    integer                                 :: mt_inp
    integer                                 :: n0_scr
    integer                                 :: n0_sto
    !
    integer                                 :: ipn
    !
    type ( headsave_struct ),       pointer :: h    ! headsave structure
    type ( datumgrid_struct ),      pointer :: dtm  ! datumgrid structure
    type ( wavelet_struct ),        pointer :: wlt  ! wavelet
    type ( syngrid_struct ),        pointer :: g    ! syngrid
    type ( synref_struct ),         pointer :: r    ! synref
    type ( synvofz_struct ),        pointer :: v    ! synvofz
    !
  end type spike_struct
  !
  type ( spike_struct ), save,      pointer :: object      ! needed for traps.
  !
  contains
  !
  subroutine spike_create ( o )
    !
    ! create spike structure
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    !
    integer                                 :: i_err
    !
    ! allocate the spike structure
    !
    allocate ( o )
    !
    ! nullify pointers
    !
    nullify (o%h)   ! jpa
    nullify (o%dtm) ! jpa
    nullify (o%wlt) ! jpa
    nullify (o%g)   ! jpa
    nullify (o%r)   ! jpa
    nullify (o%v)   ! jpa
    !
    call spike_nullify ( o )
    !
    ! create the syngrid structure
    !
    call syngrid_create ( o%g, 'spike', i_err )
    !
    ! create the synref structure
    !
    call synref_create ( o%r, 'spike', i_err )
    !
    ! create the synvofz structure
    !
    call synvofz_create ( o%v, 'spike', i_err )
    !
    ! create the wavelet structure
    !
    call wavelet_create ( o%wlt, 'spike', i_err )
    !
    ! create the datumgrid structure
    !
    call datumgrid_create ( o%dtm, 'spike', i_err )
    !
    ! initialize spike parameters
    !
    call spike_initialize ( o )
    !
    return
    !
  end subroutine spike_create
  !
  subroutine spike_nullify ( o )
    !
    ! nyullify pointers
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    !
    nullify ( o%h    ) ! headsave structure
    nullify ( o%dtm  ) ! datumgrid structure
    nullify ( o%wlt  ) ! wavelet
    nullify ( o%g  ) ! syngrid
    nullify ( o%r  ) ! synref
    nullify ( o%v ) ! synvofz
    !
    return
    !
  end subroutine spike_nullify 
  !
  subroutine spike_delete ( o )
    !
    ! Arguments
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    !
    ! Begin spike_delete
    !
    if ( associated      ( o%h ) )  &
    call headsave_delete ( o%h )
    !
    ! delete the syngrid structure
    !
    if ( associated    ( o%g ) ) &
    call syngrid_delete ( o%g )
    !
    ! delete the synref structure
    !
    if ( associated    ( o%r ) ) &
    call synref_delete ( o%r )
    !
    ! delete the synvofz structure
    !
    if ( associated     ( o%v ) ) &
    call synvofz_delete ( o%v )
    !
    ! delete the wavelet structure
    !
    if ( associated     ( o%wlt ) ) &
    call wavelet_delete ( o%wlt )
    !
    ! delete the datumgrid structure
    !
    if ( associated       ( o%dtm ) ) &
    call datumgrid_delete ( o%dtm )
    !
    deallocate ( o )
    !
    return
    !
  end subroutine spike_delete
  !
  subroutine spike_initialize ( o )
    !
    ! Arguments
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    !
    o%num_cpus         = 1
    o%broadcast_traces = .false.
    !
    ! Begin spike_initialize
    !
    call spike_initialize_0 ( o )
    !
    ! initialize parameters
    !
    ! get the current globals
    !
    call timeglob_get ( o%nt_glb, o%t0_glb, o%t1_glb, o%dt_glb )
    !
    call datumgrid_initialize ( o%dtm )
    !
    call spike_update ( o )
    !
    return
    !
  end subroutine spike_initialize
  !
  subroutine spike_initialize_0 ( o )
    !
    ! Arguments
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    !
    ! Begin spike_initialize
    !
    call memfun_init ( o%skip_wrapup )
    call memfun_init ( o%num_cpus )
    call memfun_init ( o%broadcast_traces )
    call memfun_init ( o%nh_inp )
    call memfun_init ( o%nt_glb )
    call memfun_init ( o%t0_glb )
    call memfun_init ( o%t1_glb )
    call memfun_init ( o%dt_glb )
    call memfun_init ( o%rt_out )
    call memfun_init ( o%v%rv_gat )
    call memfun_init ( o%v%rv_tig )
    call memfun_init ( o%mh_inp )
    call memfun_init ( o%mt_inp )
    call memfun_init ( o%n0_scr )
    call memfun_init ( o%n0_sto )
    !
    return
    !
  end subroutine spike_initialize_0
  !
  subroutine spike_update ( o )
    !
    ! Arguments
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    !
    ! Local variables
    !
    integer                                 :: i_err
    !
    ! Begin spike_update
    !
    object => o         ! needed for traps.
    !
    o%skip_wrapup = .true.
    !
    i_err = 0
    !
    call pc_put_options_field ( 'broadcast_traces', c_yes_no, n_yes_no )
    !
    ! get spike parameters
    !
    call spike_get ( o )
    !
    ! verify spike parameters
    !
    call spike_verify ( o )
    !
    ! put spike parameters
    !
    call spike_put ( o )
    !
    ! prep spike parameters
    !
    call spike_prep ( o, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    &  /, " error in spike_update" &
    &  /, " during spike_prep " &
    & )')
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    &  /, " error in spike_update" &
    & )')
    !
    call pc_error ( ' spike_update error ' )
    !
    return
    !
  end subroutine spike_update
  !
  subroutine spike_get ( o )
    !
    ! get spike parameters
    !
    ! Arguments
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    !
    ! get the rotation grid object
    !
    o%ipn = pc_get_ipn()
    !
    call timeglob_get ( o%nt_glb, o%t0_glb, o%t1_glb, o%dt_glb )
    !
    call pc_get_global ( 'nwih',  o%nh_inp )
    !
    if ( pathchoose_update ( o%v%select_path_vel, o%v%path_vel ) ) return
    if ( pathchoose_update ( o%r%select_path_dif,  o%r%path_dif  ) ) return
    if ( pathchoose_update ( o%r%select_path_pik,  o%r%path_pik  ) ) return
    !
    call pc_get ( 'num_cpus',        o%num_cpus          )
    call pc_get ( 'broadcast_traces', o%broadcast_traces )
    !  
    !
    ! get syngrid parameters
    !
    call syngrid_get ( o%g )
    !
    ! get synref parameters
    !
    call synref_get ( o%r )
    !
    ! get synvofz parameters
    !
    call synvofz_get ( o%v )
    !
    ! get wavelet parameters
    !
    call wavelet_get ( o%wlt )
    !
    ! get datumgrid parameters
    !
    call datumgrid_get ( o%dtm )
    !
    return
    !
  end subroutine spike_get
  !
  subroutine spike_put ( o )
    !
    ! put spike parameters
    !
    ! Arguments
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    !
    call pc_put ( 'num_cpus',         o%num_cpus         )
    call pc_put ( 'broadcast_traces', o%broadcast_traces )
    !
    ! put syngrid parameters
    !
    call syngrid_put ( o%g )
    !
    ! put synref parameters
    !
    call synref_put ( o%r )
    !
    ! put synvofz parameters
    !
    call synvofz_put ( o%v )
    !
    ! put wavelet parameters
    !
    call wavelet_put ( o%wlt )
    !
    ! put datumgrid parameters
    !
    call datumgrid_put ( o%dtm )
    !
    ! put the current globals
    !
    call timeglob_put ( o%nt_glb, o%t0_glb, o%dt_glb )
    !
    call pc_put_control ( 'nscratch',   o%n0_scr     )
    call pc_put_control ( 'nstore',     o%n0_sto     )
    call pc_put_control ( 'need_label',   .true.  )
    if ( spike_n_pel() .gt. 1 ) &
    call pc_put_control ( 'need_request', .true.  )
    call pc_put_control ( 'parallel_safe' ,         .true.                )
    call pc_put_control ( 'pcps_send_mode' ,        'PCPS_BOSS_EXECS'     )
    call pc_put_control ( 'pcps_generator_mode' ,   'PCPS_TRACE_GEN'      )
    !
    !print'(" spike_put i=",i4," n=",i8," num_cpus=",i8)', &
    !spike_i_pel(), spike_n_pel(), o%num_cpus
    !
    return
    !
  end subroutine spike_put
  !
  subroutine spike_verify ( o )
    !
    ! verify spike parameters
    !
    ! Arguments
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    !
    integer                                 :: i_err
    !
    i_err = 0
    !
    ! set the memory size characteristics
    !
    o%n0_scr     = 0
    !
    o%n0_sto     = o%r%nt_fin * 4
    !
    o%num_cpus = spike_n_pel()
    !
    call pc_put_sensitive_field_flag ( 'num_cpus',   .false.         )
    !
    ! verify syngrid parameters
    !
    call syngrid_verify ( o%g )
    !
    ! verify synref parameters
    !
    if ( string_upper_compare ( o%wlt%wavelet_type, 'SPIKE' ) &
   .and. string_upper_compare ( o%wlt%wavelet_filter_level, 'NONE' )  ) &
    o%r%tim_interp = 1
    !
    call synref_verify ( o%r )
    !
    ! verify synvofz parameters
    !
    call synvofz_verify ( o%v )
    !
    ! verify wavelet parameters
    !
    o%wlt%wavelet_inc = o%r%dt_fin 
    !
    call wavelet_verify ( o%wlt )
    !
    ! verify datumgrid parameters
    !
    call datumgrid_verify ( o%dtm )
    !
    return
    !
996 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in spike_verify ", &
    & /," during y migfun_get_scale " &
    & )')
    !
    go to 999
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in spike_verify ", &
    & /," during x migfun_get_scale " &
    & )')
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in spike_verify ", &
    & /," during migfun_get_scale " &
    & /," hdr_x=", i8, " hdr_y=", i8 &
    & )') &
    o%g%hdr_x, o%g%hdr_y
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in spike_verify " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine spike_verify
  !
  subroutine spike_prep ( o, i_err )
    !
    ! prep  spike parameters
    !
    ! Arguments
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    integer,                  intent(  out) :: i_err
    !
    i_err = 0
    !
    if ( pc_do_not_process_traces()  ) return
    !
    !print'(" top spike_prep p=",i4," n=",i8)', spike_i_pel(), spike_n_pel()
    !
    o%skip_wrapup = .false.
    !
    write ( pc_get_lun(), '(  &
    & /, " spike_update " ,/ ," REVISION: ", &
    & " 61  2007-09-11  Douglas Hanson Make parallel. " &
    & )')
    !
    ! prep the syngrid structure for execution
    !
    call syngrid_prep ( o%g, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! prep the synref structure for execution
    !
    call synref_prep ( &
    o%r, o%g%hdr_x, o%g%hdr_y, o%g%rx_scl, o%g%ry_scl, i_err )
    !
    if ( i_err .ne. 0 ) go to 996
    !
    ! prep the synvofz structure for execution
    !
    call synvofz_prep ( &
    o%v, o%g%hdr_x, o%g%hdr_y, o%g%rx_scl, o%g%ry_scl, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! read the datums
    !
    call datumgrid_read ( &
    o%dtm, 0, .false., .true., o%g%hdr_x, o%g%hdr_y, i_err )
    !
    if ( i_err .ne. 0 ) go to 995
    !
    ! compute the wavelet
    !
    call wavelet_prep ( o%wlt, i_err )
    !
    if ( i_err .ne. 0 ) go to 994
    !
    ! create the headsave structure
    !
    call headsave_create ( o%h, 'spike', o%nh_inp, i_err )
    !
    if ( i_err .ne. 0 ) go to 993
    !
    return
    !
993 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in spike_prep  ", &
    & /," during headsave_create " &
    & )')
    !
    go to 999
    !
994 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in spike_prep  ", &
    & /," during wavelet_prep " &
    & )')
    !
    go to 999
    !
995 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in spike_prep  ", &
    & /," during datumngrid_read " &
    & )')
    !
    go to 999
    !
996 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in spike_prep  ", &
    & /," during synref_prep " &
    & )')
    !
    go to 999
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in spike_prep  ", &
    & /," during syngrid_prep " &
    & )')
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in spike_prep  ", &
    & /," during synvofz_prep " &
    & )')
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in spike_prep  " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine spike_prep
  !
  subroutine spike_wrapup ( o )
    !
    ! Arguments
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    !
    if ( o%skip_wrapup ) return
    !
         o%skip_wrapup = .true.
    !
    return
    !
  end subroutine spike_wrapup
  !
  subroutine spike ( o, n0_inp, hd_inp, tr_inp )
    !
    ! Arguments
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    integer,                  intent(inout) :: n0_inp          ! num traces
    double precision,         intent(inout) :: hd_inp ( :, : ) ! header
    real,                     intent(inout) :: tr_inp ( :, : ) ! traces
    !
    ! Local variables
    !
    integer                                 :: i_err
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    !print'(" spike c=",i8," n=",i8," sh=",i8,1x,i8," st=",i8,1x,i8)', &
    !i_call, n0_inp, &
    !size(hd_inp,1), size(hd_inp,2), &
    !size(tr_inp,1), size(tr_inp,2)
    !
    ! Begin spike
    !
    o%mh_inp = size ( hd_inp, 1 )
    !
    o%mt_inp = size ( tr_inp, 1 )
    !
    ! compute the trace
    !
    call spike_compute ( o, o%h, o%dtm, o%wlt, o%g, o%r, o%v, &
    n0_inp, hd_inp, tr_inp, i_err )
    !
    ! fatal error
    !
    xxif_error : if ( i_err .ne. 0 ) then
      !
      n0_inp = fatal_error
      !
    else if ( n0_inp .eq. 0 ) then
      !
      ! all done
      !
      n0_inp = no_more_traces
      !
      if ( o%r%n0_pik .gt. 0 ) &
      write ( pc_get_lun(), '( &
      & " end of spike number of picks input n0_pik=", i8, &
      & " number of picks assigned m0_pik=", i8 &
      & )') &
      o%r%n0_pik, o%r%m0_pik
      !
      ! print output header words
      !
      call headsave_print ( o%h, 'end of spike', 9 )
      !
    end if xxif_error
    !
    return
    !
  end subroutine spike
  !
  subroutine spike_compute ( o, h, dtm, wlt, g, r, v, &
  n0_inp, hd_inp, tr_inp, i_err )
    !
    ! compute the spike trace
    !
    type ( spike_struct ),          pointer :: o ! spike structure
    type ( headsave_struct ),       pointer :: h    ! headsave structure
    type ( datumgrid_struct ),      pointer :: dtm  ! datumgrid structure
    type ( wavelet_struct ),        pointer :: wlt  ! wavelet
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    integer,                  intent(inout) :: n0_inp
    double precision,         intent(  out) :: hd_inp ( :, : )
    real,                     intent(  out) :: tr_inp ( :, : )
    integer,                  intent(  out) :: i_err
    !
    ! Local variables
    !
    integer                                 :: pe_compute 
    integer                                 :: jt_crs
    integer                                 :: i0_inp
    integer                                 :: n0_inp_do
    integer                                 :: nh_inp
    real                                    :: tr_dup ( r%nt_crs )
    integer                                 :: i0_out 
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    !print'(" spike_compute c=",i8," n=",i8," sh=",i8,1x,i8," st=",i8,1x,i8)', &
    !i_call, n0_inp, &
    !size(hd_inp,1), size(hd_inp,2), &
    !size(tr_inp,1), size(tr_inp,2)
    !
    nh_inp = size ( hd_inp, 1 )
    !
    g%n0_inp = n0_inp
    !
    n0_inp = no_more_traces
    !
    i_err = 0
    !
    ! make sure there is only one input trace for extrenal traces
    !
    if ( g%use_duplicate .and. g%n0_inp .gt. 1 ) go to 994
    !
    ! make sure the time interpolation is well behaved
    !
    r%nt_crs = ( r%nt_fin - 1 ) / r%tim_interp + 1
    !
    if ( r%nt_fin .ne. ( r%nt_crs-1 ) *r%tim_interp+1 &
    .or. r%nt_crs .gt. size ( tr_inp, 1 ) ) go to 998
    !
    tr_dup ( 1:r%nt_crs ) = tr_inp ( 1:r%nt_crs, 1 )
    !
    xxif_duplicate : if ( g%use_duplicate ) then
      !
      n0_inp_do = g%num_duplicate * g%n0_inp
      !
    else if ( g%use_external ) then
      !
      n0_inp_do = g%n0_inp
      !
    else xxif_duplicate 
      !
      n0_inp_do = min ( g%n0_out-g%i0_out, g%group_size )
      !
    end if xxif_duplicate 
    !
    n0_inp_do = min ( n0_inp_do, size(hd_inp,2) )
    !
    !print'(" spike_compute c=",i5," n=",i8,1x,i8)', &
    !i_call, g%n0_inp, n0_inp_do
    !
    ! fill in up to g%group_size traces
    !
    hd_inp (:,:) = 0.
    !
    tr_inp (:,:) = 0.
    !
    i0_out = g%i0_out
    !
    loop_thru_traces_1 : do i0_inp = 1,   n0_inp_do
      !
      g%i0_inp = i0_inp
      !
      g%i0_out = g%i0_out + 1 ! increment the output trace counter
      !
      ! get the trace location
      !
      !print'(" aa1 spike_compute c=",i5,&
      !& " i0_inp=",i8," i0_out=",i8)', &
      !i_call, g%i0_inp, g%i0_out
      !
      call syngrid_compute_location ( g, dtm, hd_inp(:,i0_inp), i_err )
      !
      if ( i_err .ne. 0 ) go to 997
      !
      if ( g%group_end  ) exit loop_thru_traces_1
      !
      ! reset the last group index, g%ig_lst
      !
      g%ig_lst = g%ig_out
      !
      pe_compute = mod ( i0_inp-1, spike_n_pel() ) 
      !
      !print'(" aa1 spike_compute p=",i4," c=",i4," i0=",i8," l0=",l2)', &
      !spike_i_pel(), pe_compute, i0_inp, spike_i_pel() .eq. pe_compute 
      !
      xxif_pe_compute : if ( spike_i_pel() .eq. pe_compute ) then
        !
        ! compute the velocity
        !
        !print'(" aa2 spike_compute p=",i4," c=",i4," i0=",i8," l0=",l2)', &
        !spike_i_pel(), pe_compute, i0_inp, spike_i_pel() .eq. pe_compute 
        !
        call spike_compute_velocity ( g, v )
        !
        !print'(" aa3 spike_compute c=",i5," i0_inp=",i8," i0_out=",i8)', &
        !i_call, g%i0_inp, g%i0_out
        !
        call spike_compute_trace ( wlt, dtm, g, r, v, &
        tr_dup, tr_inp(:,i0_inp), i_err )
        !
        if ( i_err .ne. 0 ) go to 996
        !
        !print'(" aa4 spike_compute c=",i5," i0_inp=",i8," i0_out=",i8)', &
        !i_call, g%i0_inp, g%i0_out
        !
        call syngrid_compute_head ( &
        g, hd_inp(:,i0_inp), tr_inp(:,i0_inp), i_err )
        !
        if ( i_err .ne. 0 ) go to 995
        !
        !print'(" aa5 spike_compute c=",i5," i0_inp=",i8," i0_out=",i8)', &
        !i_call, g%i0_inp, g%i0_out
        !
        !print'( " sp ", &
        !& i5, 1x, i5, 1x, i5, 1x, f8.0, 1x, f8.0, 1x, f8.0, 1x, f8.0 )', &
        !g%i0_out, g%xo_out, g%jx_out, g%ro_out, &
        !g%rx_cmp, g%rx_gat, g%rx_tig
        !
      end if xxif_pe_compute 
      !
      n0_inp = n0_inp + 1
      !
    end do loop_thru_traces_1
    !
    ! cycle over the traces and send traces from the pe that computed them 
    ! to the root pe
    !
    loop_thru_traces_2 : do i0_inp = 1,   n0_inp_do
      !
      pe_compute = mod ( i0_inp-1, spike_n_pel() ) 
      !
      xxif_pe_compute_not_root : if ( pe_compute .ne. 0 ) then
        !
        xxif_root : if ( spike_i_pel() .eq. 0 ) then
          !
          ! receive data from pe pe_compute
          !
          call pcpsx_receive_data ( pe_compute,   nh_inp, hd_inp(:,i0_inp), 22 )
          !
          call pcpsx_receive_data ( pe_compute, r%nt_crs, tr_inp(:,i0_inp), 22 )
          !
        else if ( spike_i_pel() .eq. pe_compute ) then
          !
          ! send data to pe 0
          !
          call pcpsx_send_data    ( 0,            nh_inp, hd_inp(:,i0_inp), 22 )
          !
          call pcpsx_send_data    ( 0,          r%nt_crs, tr_inp(:,i0_inp), 22 )
          !
        end if xxif_root 
        !
      end if xxif_pe_compute_not_root 
      !
    end do loop_thru_traces_2 
    !
    ! broadcast the traces from the root pe to the other pes
    !
    if ( o%broadcast_traces ) &
    call pcpsx_broadcast ( 0, n0_inp_do, hd_inp )
    !
    if ( o%broadcast_traces ) &
    call pcpsx_broadcast ( 0, n0_inp_do, tr_inp )
    !
    ! save the output header words
    !
    loop_thru_traces_3 : do i0_inp = 1,   n0_inp_do
      !
      i0_out = i0_out + 1
      !
      ! save headers for output traces (locations 9 - 12)
      !
      call headsave_store ( h, i0_out, 9, hd_inp ( :, i0_inp ) )
      !
    end do loop_thru_traces_3 
    !
    return
    !
994 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in spike_compute using OPT_INP=", a, &
    & /, " number of input traces = n0_inp=", i8, &
    & /, " you should enter a single trace at a time for this option. " &
    & )') &
    trim ( g%opt_input ), g%n0_inp
    !
    go to 999
    !
995 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in spike_compute during syngrid_compute_head " &
    & )')
    !
    go to 999
    !
996 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in spike_compute during spike_compute_trace " &
    & )')
    !
    go to 999
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & " error in spike_compute during syngrid_compute_location " &
    & )') 
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & " error in spike_compute in nt_fin=",i8 &
    & )') &
    r%nt_fin 
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & " error in spike_compute " &
    & )') 
    !
    i_err = -1
    !
    return
    !
  end subroutine spike_compute
  !
  subroutine spike_compute_velocity ( g, v )
    !
    ! compute the velocity at the source location
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synvofz_struct ),        pointer :: v ! synvofz
    !
    ! Local variables
    !
    call interpolate_3d_to_0d ( &
                                v%nz_vel, v%z0_vel, v%dz_vel, &
                                v%nx_vel, v%x0_vel, v%dx_vel, &
                                v%ny_vel, v%y0_vel, v%dy_vel, &
                                v%rv_vel, &
                                g%rz_gat, &
                                g%rx_gat, &
                                g%ry_gat, &
                                v%rv_gat &
                              )
    !
    ! compute the velocity at the source location
    !
    call interpolate_3d_to_0d ( &
                                v%nz_vel, v%z0_vel, v%dz_vel, &
                                v%nx_vel, v%x0_vel, v%dx_vel, &
                                v%ny_vel, v%y0_vel, v%dy_vel, &
                                v%rv_vel, &
                                g%rz_tig, &
                                g%rx_tig, &
                                g%ry_tig, &
                                v%rv_tig &
                              )
    !
    return
    !
  end subroutine spike_compute_velocity 
  !
  subroutine spike_compute_trace ( wlt, dtm, g, r, v, tr_dup, tr_inp, i_err )
    !
    ! compute a trace
    !
    type ( wavelet_struct ),        pointer :: wlt  ! wavelet
    type ( datumgrid_struct ),      pointer :: dtm  ! datumgrid structure
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    real,                     intent(in   ) :: tr_dup ( : )
    real,                     intent(  out) :: tr_inp ( : )
    integer,                  intent(  out) :: i_err
    !
    ! Local variables
    !
    integer                                 :: jt_fin
    integer                                 :: jt_crs
    !
    i_err = 0
    !
    xxif_duplicate : if ( g%use_duplicate ) then
      !
      tr_inp ( 1:r%nt_crs ) = tr_dup ( 1:r%nt_crs ) 
      !
    else xxif_duplicate 
    !
    ! initialize header and trace to zero
    !
      tr_inp ( : ) = 0.
    !
    r%tr_fin ( : ) = 0.
    !
    ! print some info for debug
    !
    !print*,' rx_out=',g%rx_out,' rx_cmp=',g%rx_cmp,' ry_off=',g%ry_off
    !print*,' rx_gat=',g%rx_gat,' rx_tig=',g%rx_tig
    !print*,' ry_gat=',g%ry_gat,' ry_tig=',g%ry_tig
    !print*,' rz_gat=',g%rz_gat,' rz_tig=',g%rz_tig
    !print*,' nx_gat=',dtm%nx_gat,' ny_gat=',dtm%ny_gat
    !print*,' rz_gat=',dtm%rz_gat ( 1,1 )
    !
    ! compute the reflection, diffraction, direct arrival trace
    ! x=fast and y=slow in distance units
    !
    call spike_compute_event ( g, r, v, r%tr_rflct )
    !
    ! if need be compute a new noise trace
    !
    if ( string_upper_compare ( r%noise_type, 'DIFF' )  ) &
    call synref_compute_noise ( r%amp_noise, r%nt_fin, r%tr_noise )
    !
    ! add the noise trace to the spike trace to form the ouput trace
    !
    r%tr_fin   ( 1:r%nt_fin ) = &
    r%tr_spike ( 1:r%nt_fin ) * r%tr_scale ( 1:r%nt_fin ) &
  + r%tr_rflct ( 1:r%nt_fin ) * r%tr_scale ( 1:r%nt_fin ) &
  + r%tr_noise ( 1:r%nt_fin )
    !
    ! apply the wavelet, this includes a filter
    !
    call wavelet_apply ( wlt, r%nt_fin, r%tr_fin, r%tr_fin )
    !
    !print'(1x,i8,1x,g12.6,1x,g12.6," aa1_spike_compute " )', &
    !( jt_fin, (jt_fin-1)*r%dt_fin, r%tr_fin(jt_fin), jt_fin=1,r%nt_fin)
    !
    ! filter the trace
    !
    if ( wlt%wavelet_filter_level_t ) &
    call matfun_filter ( freq_low_none  = wlt%wavelet_freq_low_none,  &
                         freq_low_full  = wlt%wavelet_freq_low_full,  &
                         freq_high_full = wlt%wavelet_freq_high_full, &
                         freq_high_none = wlt%wavelet_freq_high_none, &
                         freq_phase     = wlt%wavelet_phase,     &
                         nt_inp         = r%nt_fin,         &
                         t0_inp         = r%t0_fin,         &
                         dt_inp         = r%dt_fin,         &
                         tr_inp         = r%tr_fin,         &
                         i_err          = i_err               &
                       )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! if we want dead traces zero the output trace here
    !
    if ( string_upper_compare ( r%noise_type, 'DEAD' )  ) r%tr_fin = 0.
    !
    !print'(1x,i8,1x,g12.6,1x,g12.6," aa2_spike_compute " )', &
    !( jt_fin, (jt_fin-1)*r%dt_fin, r%tr_fin(jt_fin), jt_fin=1,r%nt_fin)
    !
    ! copy the work trace r%tr_fin to tr_inp
    !
    jt_crs = 1
    !
    do_jt_fin : do jt_fin = 1 , r%nt_fin, r%tim_interp
      !
      tr_inp ( jt_crs ) = r%tr_fin ( jt_fin )
      !
      jt_crs = jt_crs + 1
      !
    end do do_jt_fin
    !
    end if xxif_duplicate 
    !
    ! get max amp for this trace, r%tr_spike_0, and all traces, r%tr_spike
    !
    r%tr_max_0 = maxval ( abs ( tr_inp ( 1:r%nt_crs ) ) )
    !
    r%tr_max = max ( r%tr_max, r%tr_max_0 )
    !
    !print'(1x,i8,1x,g12.6,1x,g12.6," aa3_spike_compute " )', &
    !( jt_crs, (jt_crs-1)*r%dt_fin*r%tim_interp, tr_inp(jt_crs), &
    !jt_crs=1,r%nt_crs)
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in spike_compute_trace " &
    & /, " during matfun_filter " &
    & )')
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & " error in spike_compute_trace " &
    & )') 
    !
    i_err = -1
    !
    return
    !
  end subroutine spike_compute_trace
  !
  subroutine spike_compute_event ( g, r, v, tr_out )
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    real,                     intent(inout) :: tr_out ( : ) ! trace
    !
    integer                                 :: j0_ghost 
    real                                    :: dz_tig
    real                                    :: dz_gat
    real                                    :: rz_tig
    real                                    :: rz_gat
    logical                                 :: l0_tig
    logical                                 :: l0_gat
    logical                                 :: l0_g_t 
    logical                                 :: add_event
    real                                    :: tr_tmp ( r%nt_fin ) 
    real                                    :: sign_event
    !
    !  initalize the trace to zero
    !
    tr_out ( 1:r%nt_fin ) = 0.
    !
    ! cycle over the ghost events
    !
    rz_gat = g%rz_gat
    !
    rz_tig = g%rz_tig
    !
    dz_gat = rz_gat - r%dep_ghost 
    !
    dz_tig = rz_tig - r%dep_ghost 
    !
    l0_gat = dz_gat .gt. 0. .and. r%opt_ghost_gat 
    !
    l0_tig = dz_tig .gt. 0. .and. r%opt_ghost_tig 
    !
    l0_g_t = l0_gat .and. l0_tig
    !
    do_j0_ghost : do j0_ghost = 1 , 4
      !
      if ( j0_ghost .eq. 1. .or. j0_ghost .eq. 4 ) &
      sign_event = +1
      !
      if ( j0_ghost .eq. 2. .or. j0_ghost .eq. 3 ) &
      sign_event = -1
      !
      add_event = r%opt_ghost_non .and. j0_ghost .eq. 1 &
             .or. r%opt_ghost_gat .and. j0_ghost .eq. 2 .and. l0_gat &
             .or. r%opt_ghost_tig .and. j0_ghost .eq. 3 .and. l0_tig &
             .or. r%opt_ghost     .and. j0_ghost .eq. 4 .and. l0_g_t
      !
      g%rz_tig = rz_tig 
      !
      g%rz_gat = rz_gat 
      !
      if ( j0_ghost .eq. 2 .or. j0_ghost .eq. 4 ) &
      g%rz_tig = rz_tig - 2. * dz_tig
      !
      if ( j0_ghost .eq. 3 .or. j0_ghost .eq. 4 ) &
      g%rz_gat = rz_gat - 2. * dz_gat
      !
      xxif_add_event : if ( add_event ) then
        !
        call spike_compute_event_1 ( g, r, v, tr_tmp )
        !
        tr_out ( 1:r%nt_fin ) = &
        tr_out ( 1:r%nt_fin ) &
      + tr_tmp ( 1:r%nt_fin ) * sign_event
        !
      end if xxif_add_event 
      !
    end do do_j0_ghost 
    !
    g%rz_tig = rz_tig 
    !
    g%rz_gat = rz_gat 
    !
    return
    !
  end subroutine spike_compute_event
  !
  subroutine spike_compute_event_1 ( g, r, v, tr_out )
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    real,                     intent(inout) :: tr_out ( : ) ! trace
    !
    tr_out ( 1:r%nt_fin ) = 0.
    !
    !  sum in the contribution of each reflector
    !
    call spike_compute_event_ref ( g, r, v, tr_out )
    !
    ! sum in the contribution of each diffractor
    !
    call spike_compute_event_dif ( g, r, v, tr_out )
    !
    ! sum in the contribution of each pik
    !
    call spike_compute_event_pik ( g, r, v, tr_out )
    !
    ! sum in the direct arrival event
    !
    call spike_compute_event_direct ( g, r, v, tr_out )
    !
    return
    !
  end subroutine spike_compute_event_1
  !
  subroutine spike_compute_event_ref ( g, r, v, tr_out )
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    real,                     intent(inout) :: tr_out ( : ) ! trace
    !
    integer                                 :: i_err
    integer                                 :: i0_ref
    complex                                 :: rp ! refl coeff for P waves
    complex                                 :: rs ! refl coeff for S waves
    complex                                 :: tp ! tran coeff for P waves
    complex                                 :: ts ! tran coeff for S waves
    !
    !  sum in the contribution of each reflector
    !
    do_i0_ref : do i0_ref = 1 , r%n0_ref
      !
      r%i0_ref = i0_ref
      !
      xxif_amp_ref : &
      if ( abs ( r%amp_ref (r%i0_ref ) ) .gt. 1.e-6 ) then
      !
      !  compute the amplitude and travel time
      !
      call spike_compute_ref ( g, r, v, i_err )
      !
      xxif_no_err : if ( i_err .eq. 0 ) then
        !
        ! put the event on a discrete time sample
        !
        r%rt_fin = nint ( ( r%rt_ref - r%t0_fin ) / r%dt_fin ) &
                          * r%dt_fin + r%t0_fin
        !
        r%jt_fin = nint ( ( r%rt_fin - r%t0_fin ) / r%dt_fin ) + 1
        !
        ! set the trace amplitude
        !
        r%ra_ref = r%amp_ref ( i0_ref )
        !
        if ( r%opt_zoeppritz ) then
           !
           r%att_flag = 'c'
           !
           call zoeppritz ( &
           r%ang_ref, r%inc_flag, r%att_flag, &
           r%dn1_ref(i0_ref), r%vp1_ref(i0_ref), r%vs1_ref(i0_ref), &
           r%dn2_ref(i0_ref), r%vp2_ref(i0_ref), r%vs2_ref(i0_ref), &
           rp, rs, tp, ts, i_err &
                        )
           !
           r%ra_ref = real ( rp * cmplx ( r%ra_ref, 0. ))
           !
        end if
        !
        !print'(" spike1 ",i5,1x,i5,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6)', &
        !g%i0_out, r%i0_ref, r%ang_ref, r%ra_ref, r%refl_p_ref, &
        !r%ra_ref * r%rc_ref ** r%cos_decay
        !
        ! scale the ampltidue by the cosine factor
        !
        if ( r%rc_ref .ne. 0. .and. r%cos_decay .ne. 0. ) &
        r%ra_ref = r%ra_ref * r%rc_ref ** r%cos_decay
        !
        ! add the amplitde into the trace
        !
        if ( r%jt_fin .ge. 1 .and. r%jt_fin .le. r%nt_fin ) &
        tr_out ( r%jt_fin ) = tr_out ( r%jt_fin ) + r%ra_ref
        !
      end if xxif_no_err
      !
      end if xxif_amp_ref
      !
    end do do_i0_ref
    !
    return
    !
  end subroutine spike_compute_event_ref
  !
  subroutine spike_compute_event_dif ( g, r, v, tr_out )
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    real,                     intent(inout) :: tr_out ( : ) ! trace
    !
    integer                                 :: i_err
    integer                                 :: i0_dif
    complex                                 :: rp ! refl coeff for P waves
    complex                                 :: rs ! refl coeff for S waves
    complex                                 :: tp ! tran coeff for P waves
    complex                                 :: ts ! tran coeff for S waves
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! sum in the contribution of each diffractor
    !
    do_i0_dif : do i0_dif = 1 , r%n0_dif
      !
      r%i0_dif = i0_dif
      !
      xxif_amp_dif : if ( abs ( r%amp_dif (r%i0_dif ) ) .gt. 1.e-6 ) then
      !
      !  compute the amplitude and travel time
      !
      call spike_compute_dif ( g, r, v )
      !
      ! put the event on a discrete time sample
      !
      r%rt_fin = nint ( ( r%rt_dif - r%t0_fin ) / r%dt_fin ) &
                        * r%dt_fin + r%t0_fin
      !
      r%jt_fin = nint ( ( r%rt_fin - r%t0_fin ) / r%dt_fin ) + 1
      !
      ! make sure it is within the oputput time window
      !
      !if ( i_call .le. 1 ) &
      !print'(" spike_compute_event_dif i0=", i8, &
      !& " it=", i8, " rt=", g10.4, " a=", g12.6, &
      !& " x=", g10.4, " y=", g10.4, " z=", g10.4 )', &
      !i0_dif, r%jt_fin, r%rt_fin, r%amp_dif ( i0_dif ), &
      !r%x_loc_dif ( i0_dif ), &
      !r%y_loc_dif ( i0_dif ), r%dep_dif ( i0_dif )
      !
      ! set the trace amplitude
      ! scale the ampltidue by the cosine factor
      !
      r%ra_dif = r%amp_dif ( i0_dif )
      !
      xxif_zoeppritz : if ( r%opt_zoeppritz ) then
         !
         r%att_flag = 'c'
         !
         call zoeppritz ( &
         r%ang_dif, r%inc_flag, r%att_flag, &
         r%dn1_dif(i0_dif), r%vp1_dif(i0_dif), r%vs1_dif(i0_dif), &
         r%dn2_dif(i0_dif), r%vp2_dif(i0_dif), r%vs2_dif(i0_dif), &
         rp, rs, tp, ts, i_err &
                       )
         !
         r%ra_dif = real ( rp * cmplx ( r%ra_dif, 0. ))
         !
      end if xxif_zoeppritz
      !
      ! scale the ampltidue by the cosine factor
      !
      if ( r%rc_dif .ne. 0. .and. r%cos_decay .ne. 0. ) &
      r%ra_dif = r%ra_dif * r%rc_dif ** r%cos_decay
      !
      ! add the amplitde into the trace
      !
      if ( r%jt_fin .ge. 1 .and. r%jt_fin .le. r%nt_fin ) &
      tr_out ( r%jt_fin ) = tr_out ( r%jt_fin ) + r%ra_dif
      !
      end if xxif_amp_dif
      !
    end do  do_i0_dif
    !
    return
    !
  end subroutine spike_compute_event_dif
  !
  subroutine spike_compute_event_pik ( g, r, v, tr_out )
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    real,                     intent(inout) :: tr_out ( : ) ! trace
    !
    integer                                 :: i0_pik
    !
    !
    ! sum in the contribution of each pick
    !
    do_i0_pik : do i0_pik = 1 , r%n0_pik
      !
      r%i0_pik = i0_pik
      !
      ! if this is the same x midpoint and offset
      !
      !if ( i0_pik .eq. 1 ) &
      !print'( &
      !& " i=", i6, " l=",1x,l2,1x,l2,1x,l2,1x,l2, &
      !& " x=",g10.4,1x,g10.4,1x,g10.4,1x,g10.4, &
      !& " y=",g10.4,1x,g10.4,1x,g10.4,1x,g10.4 &
      !& )', &
      !g%i0_out, &
      !matfun_same_value ( g%rx_cmp, r%x_loc_pik ( i0_pik ) ), &
      !matfun_same_value ( g%rx_off, r%x_off_pik ( i0_pik ) ), &
      !matfun_same_value ( g%ry_cmp, r%y_loc_pik ( i0_pik ) ), &
      !matfun_same_value ( g%ry_off, r%y_off_pik ( i0_pik ) ), &
      !g%rx_cmp, r%x_loc_pik ( i0_pik ), &
      !g%rx_off, r%x_off_pik ( i0_pik ), &
      !g%ry_cmp, r%y_loc_pik ( i0_pik ), &
      !g%ry_off, r%y_off_pik ( i0_pik )
      !
      xxif_same_location : &
      if ( matfun_same_value ( g%rx_cmp, r%x_loc_pik ( i0_pik ) ) &
     .and. matfun_same_value ( g%rx_off, r%x_off_pik ( i0_pik ) ) &
     .and. matfun_same_value ( g%ry_cmp, r%y_loc_pik ( i0_pik ) ) &
     .and. matfun_same_value ( g%ry_off, r%y_off_pik ( i0_pik ) ) ) then
      !
      r%m0_pik = r%m0_pik + 1
      !
      ! put the event on a discrete time sample
      !
      r%rt_pik = r%dep_pik ( i0_pik )
      !
      r%rt_fin = nint ( ( r%rt_pik - r%t0_fin ) / r%dt_fin ) &
                        * r%dt_fin + r%t0_fin
      !
      r%jt_fin = nint ( ( r%rt_fin - r%t0_fin ) / r%dt_fin ) + 1
      !
      ! make sure it is within the oputput time window
      !
      !print*,' i0=',i0_pik,' jt_fin=',r%jt_fin,' rt_fin=',r%rt_fin,&
      !r%amp_pik ( i0_pik ),
      !r%x_loc_pik ( i0_pik ),r%x_off_pik ( i0_pik ),r%dep_pik ( i0_pik )
      !
      ! set the trace amplitude
      ! scale the ampltidue by the cosine factor
      !
      r%ra_pik = r%amp_pik ( i0_pik )
      !
      ! add the amplitde into the trace
      !
      if ( r%jt_fin .ge. 1 .and. r%jt_fin .le. r%nt_fin ) &
      tr_out ( r%jt_fin ) = tr_out ( r%jt_fin ) + r%ra_pik
      !
      end if xxif_same_location
      !
    end do  do_i0_pik
    !
    return
    !
  end subroutine spike_compute_event_pik
  !
  subroutine spike_compute_event_direct ( g, r, v, tr_out )
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    real,                     intent(inout) :: tr_out ( : ) ! trace
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    r%rt_dir = sqrt ( &
                      ( g%rx_gat - g%rx_tig ) ** 2 &
                    + ( g%ry_gat - g%ry_tig ) ** 2 &
                    + ( g%rz_gat - g%rz_tig ) ** 2 ) &
                    / ( .5 * ( v%rv_gat + v%rv_tig ) )
    !
    ! put the event on a discrete time sample
    !
    r%rt_fin = nint ( ( r%rt_dir - r%t0_fin ) / r%dt_fin ) &
                      * r%dt_fin + r%t0_fin
    !
    r%jt_fin = nint ( ( r%rt_fin - r%t0_fin ) / r%dt_fin ) + 1
    !
    ! set the trace amplitude
    !
    r%ra_dir = r%amp_direct * r%rt_dir ** r%pwr_direct
    !
    ! add the amplitde into the trace
    !
    if ( r%jt_fin .ge. 1 .and. r%jt_fin .le. r%nt_fin ) &
    tr_out ( r%jt_fin ) = tr_out ( r%jt_fin ) + r%ra_dir 
    !
    !print'(" qq1 c=",i8," rt_dir=",g12.6," ra_dir=",g12.6)', &
    !i_call, r%rt_dir, r%ra_dir
    !
    return
    !
  end subroutine spike_compute_event_direct 
  !
  subroutine spike_compute_ref ( g, r, v, i_err )
    !
    !  compute reflection time
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    integer,                 intent (inout) :: i_err
    !
    real                                    :: rx_mid
    real                                    :: ry_mid
    real                                    :: rh_mid
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    i_err = 0
    !
    !if ( i_call .le. 1 ) &
    !print'(" spike_compute_ref i_call=", i8, &
    !& " src=", g10.4, 1x, g10.4, &
    !& " rec=", g10.4, 1x, g10.4 &
    !& )', &
    !i_call, &
    !g%rx_gat, g%ry_gat, &
    !g%rx_tig, g%ry_tig
    !
    rx_mid = ( g%rx_gat + g%rx_tig ) * .5
    !
    ry_mid = ( g%ry_gat + g%ry_tig ) * .5
    !
    rh_mid = sqrt ( ( g%rx_gat-g%rx_tig ) **2 &
                  + ( g%ry_gat-g%ry_tig ) **2 )
    !
    xxif_x : if ( string_upper_compare ( r%tim_type, 'X_SLICE' )  ) then
      !
      !  dip in the x direction
      !
      r%dep_ref_0 = r%dep_ref ( r%i0_ref ) &
                 + r%dz_dy   ( r%i0_ref ) * ry_mid
      !
      r%dz_dx_0  = r%dz_dx   ( r%i0_ref )
      !
      r%dz_dy_0  = 0.
      !
    else if ( string_upper_compare ( r%tim_type, 'Y_SLICE' )  ) then
      !
      !  dip in the y direction
      !
      r%dep_ref_0 = r%dep_ref ( r%i0_ref ) &
                 + r%dz_dx   ( r%i0_ref ) * rx_mid
      !
      r%dz_dx_0 = 0.
      !
      r%dz_dy_0 = r%dz_dy    ( r%i0_ref )
      !
    else if ( string_upper_compare ( r%tim_type, 'VERTICAL' )  ) then
      !
      !  vertical time
      !
      r%dep_ref_0 = r%dep_ref ( r%i0_ref ) &
                 + r%dz_dx   ( r%i0_ref ) * rx_mid &
                 + r%dz_dy   ( r%i0_ref ) * ry_mid
      !
      r%dz_dx_0 = 0.
      !
      r%dz_dy_0 = 0.
      !
    else xxif_x
      !
      !  normal time
      !
      r%dep_ref_0 = r%dep_ref ( r%i0_ref )
      r%dz_dx_0  = r%dz_dx   ( r%i0_ref )
      r%dz_dy_0  = r%dz_dy   ( r%i0_ref )
      !
    end if xxif_x
    !
    !  compute the reflection dip cosine
    !
    r%rc_ref = cos ( atan ( sqrt ( r%dz_dx_0**2 + r%dz_dy_0**2 )  )  )
    !
    ! compute the reflection position, x2, y2, z2, the travel time, t2,
    ! and the velocity, v2, for a dipping plane reflector
    ! defined by z ( x, y ) = r%dep_ref + x * r%dz_dx + y * r%dz_dy
    !
    call synref_plane_reflection_pos ( &
    r%dz_dx_0, r%dz_dy_0, r%dep_ref_0, &
    g%rx_gat, g%ry_gat, g%rz_gat, &
    g%rx_tig, g%ry_tig, g%rz_tig, &
    r%rx_ref, r%ry_ref, r%rz_ref &
                                      )
    !
    ! compute the velocity at this reflection point
    !
    call interpolate_3d_to_0d ( &
                                v%nz_vel, v%z0_vel, v%dz_vel, &
                                v%nx_vel, v%x0_vel, v%dx_vel, &
                                v%ny_vel, v%y0_vel, v%dy_vel, &
                                v%rv_vel, &
                                r%rz_ref, &
                                r%rx_ref, &
                                r%ry_ref, &
                                r%rv_ref &
                              )
    !
    call spike_compute_dif_0 ( &
                               g, r, v, &
                               r%rx_ref, r%ry_ref, r%rz_ref, &
                               r%rv_ref, r%rt_ref, r%ra_ref, &
                               r%ang_ref, r%rc_ref &
                             )
    !
    !print'(" spike2 ",i5,1x,i5,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6)', &
    !g%i0_out, r%i0_ref, r%ang_ref, r%rx_ref, r%ry_ref, r%rz_ref
    !
    !write ( pc_get_lun(), '( &
    !& " qq1 ", i8, 1x, i8, &
    !& 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4 &
    !& )') &
    !i_call, &
    !r%i0_ref, r%rt_ref, r%ra_ref, &
    !r%rx_ref, r%ry_ref, r%rz_ref, r%rv_ref
    !
    ! do a simplex search around this point
    !
    call spike_compute_ref_0 ( g, r, v, i_err )
    !
    !write ( pc_get_lun(), '( &
    if ( spike_i_pel() .eq. 0 .and. g%i0_out .eq. 1 ) &
    print'( &
    & " qq2 ", i8, 1x, i8, &
    & 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4 &
    & )', &
    i_call, &
    r%i0_ref, r%rt_ref, r%ra_ref, &
    r%rx_ref, r%ry_ref, r%rz_ref, r%rv_ref 
    !
    return
    !
  end subroutine spike_compute_ref
  !
  subroutine spike_compute_dif ( g, r, v )
    !
    !  compute two way time from source to diffractor to receiver
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    !
    real                                    :: rz_dif_0
    real                                    :: rx_dif_0
    real                                    :: ry_dif_0
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    !if ( i_call .le. 1 ) &
    !print'(" spike_compute_dif i_call=", i8, &
    !& " src=", g10.4, 1x, g10.4, &
    !& " rec=", g10.4, 1x, g10.4 &
    !& )', &
    !i_call, &
    !g%rx_gat, g%ry_gat, &
    !g%rx_tig, g%ry_tig
    !
    if ( string_upper_compare ( r%tim_type, 'X_SLICE' )  ) then
      !
      !  dip in the x direction
      !
      rz_dif_0 = r%dep_dif ( r%i0_dif )
      !
      rx_dif_0 = r%x_loc_dif ( r%i0_dif )
      !
      ry_dif_0 = 0.
      !
    else if ( string_upper_compare ( r%tim_type, 'Y_SLICE' )  ) then
      !
      !  dip in the y direction
      !
      rz_dif_0 = r%dep_dif ( r%i0_dif )
      !
      rx_dif_0 = 0.
      !
      ry_dif_0 = r%y_loc_dif ( r%i0_dif )
      !
    else if ( string_upper_compare ( r%tim_type, 'VERTICAL' )  ) then
      !
      !  vertical time
      !
      rz_dif_0 = r%dep_dif ( r%i0_dif )
      !
      rx_dif_0 = 0.
      !
      ry_dif_0 = 0.
      !
    else    ! if ( string_upper_compare ( r%tim_type, 'X_SLICE' )  ) then
      !
      !  normal time
      !
      rz_dif_0 = r%dep_dif   ( r%i0_dif )
      !
      rx_dif_0 = r%x_loc_dif ( r%i0_dif )
      !
      ry_dif_0 = r%y_loc_dif ( r%i0_dif )
      !
    end if    ! if ( string_upper_compare ( r%tim_type, 'X_SLICE' )  ) then
    !
    !  compute the reflection position, x2, y2, z2, the travel time, t2,
    !  and the velocity, v2, for a dipping plane reflector
    !  defined by z ( x, y ) = r%dep_dif + x * r%dz_dx + y * r%dz_dy
    !
    r%rz_dif = rz_dif_0
    !
    r%rx_dif = rx_dif_0
    !
    r%ry_dif = ry_dif_0
    !
    !  compute the velocity at this reflection point
    !
    call interpolate_3d_to_0d ( &
                                v%nz_vel, v%z0_vel, v%dz_vel, &
                                v%nx_vel, v%x0_vel, v%dx_vel, &
                                v%ny_vel, v%y0_vel, v%dy_vel, &
                                v%rv_vel, &
                                r%rz_dif, &
                                r%rx_dif, &
                                r%ry_dif, &
                                r%rv_dif &
                              )
    !
    call spike_compute_dif_0 ( &
                               g, r, v, &
                               r%rx_dif, r%ry_dif, r%rz_dif, &
                               r%rv_dif, r%rt_dif, r%ra_dif, &
                               r%ang_dif, r%rc_dif &
                             )
    !
    !if ( i_call .le. 1 ) &
    !print'( &
    !&    " p0 spike_compute_dif i0=", i8, &
    !& /, " p0 xyz_i0 =", g10.4, " y=", g10.4, " z=", g10.4, &
    !& /, " p0 xyz_dif=", g12.6, 1x, g12.6, 1x, g12.6, &
    !& /, " p0 vta_dif=", g12.6, 1x, g12.6, 1x, g12.6, &
    !& /, " p0  ac_dif=", g12.6, 1x, g12.6 &
    !& )', &
    !r%i0_dif, &
    !r%x_loc_dif ( r%i0_dif ), &
    !r%y_loc_dif ( r%i0_dif ), &
    !r%dep_dif ( r%i0_dif ), &
    !r%rx_dif, r%ry_dif, r%rz_dif, &
    !r%rv_dif, r%rt_dif, r%ra_dif, &
    !r%ang_dif, r%rc_dif
    !
    return
    !
  end subroutine spike_compute_dif
  !
  subroutine spike_compute_dif_0 ( &
                                   g, r, v, &
                                   rx_dif, ry_dif, rz_dif, &
                                   rv_dif, rt_dif, ra_dif, &
                                   ang_inc, rc_dif &
                                 )
    !
    ! compute two way time from source to diffractor to receiver
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    real,                    intent (in   ) :: rx_dif
    real,                    intent (in   ) :: ry_dif
    real,                    intent (in   ) :: rz_dif
    real,                    intent (in   ) :: rv_dif
    real,                    intent (inout) :: rt_dif
    real,                    intent (inout) :: ra_dif
    real,                    intent (inout) :: ang_inc
    real,                    intent (inout) :: rc_dif
    !
    real                                    :: pz_gat
    real                                    :: ax_gat
    real                                    :: ay_gat
    real                                    :: az_gat
    real                                    :: bx_gat
    real                                    :: by_gat
    real                                    :: bz_gat
    real                                    :: dx_gat
    real                                    :: dy_gat
    real                                    :: dz_gat
    real                                    :: xy_gat
    real                                    :: rr_gat
    real                                    :: rt_gat
    real                                    :: ra_gat
    real                                    :: rf_gat
    real                                    :: rp_gat
    real                                    :: rv_gat
    !
    real                                    :: pz_tig
    real                                    :: ax_tig
    real                                    :: ay_tig
    real                                    :: az_tig
    real                                    :: bx_tig
    real                                    :: by_tig
    real                                    :: bz_tig
    real                                    :: dx_tig
    real                                    :: dy_tig
    real                                    :: dz_tig
    real                                    :: xy_tig
    real                                    :: rr_tig
    real                                    :: rt_tig
    real                                    :: ra_tig
    real                                    :: rf_tig
    real                                    :: rp_tig
    real                                    :: rv_tig
    !
    real                                    :: ang_str
    real                                    :: ang_err
    !
    real                                    :: cos_theta_gat
    real                                    :: sin_theta_gat
    real                                    :: cos_theta_tig
    real                                    :: sin_theta_tig
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    rv_gat = rv_dif
    !
    rv_tig = rv_dif / v%v_gat_tig
    !
    !  compute the ray pathlength
    !
    dx_gat = ( rx_dif - g%rx_gat )
    !
    dy_gat = ( ry_dif - g%ry_gat )
    !
    dz_gat = ( rz_dif - g%rz_gat )
    !
    xy_gat = sqrt ( dx_gat**2 + dy_gat**2 )
    !
    rr_gat = sqrt ( xy_gat**2 + dz_gat**2 )
    !
    dx_tig = ( rx_dif - g%rx_tig )
    !
    dy_tig = ( ry_dif - g%ry_tig )
    !
    dz_tig = ( rz_dif - g%rz_tig )
    !
    xy_tig = sqrt ( dx_tig**2 + dy_tig**2 )
    !
    rr_tig = sqrt ( xy_tig**2 + dz_tig**2 )
    !
    !if ( i_call .le. 1 ) &
    !print'( &
    !& /, " q0 xsrd==",g12.6,1x,g12.6,1x,g12.6," dxsr=",g12.6, &
    !& /, " q0 ysrd==",g12.6,1x,g12.6,1x,g12.6," dysr=",g12.6, &
    !& /, " q0 zsrd==",g12.6,1x,g12.6,1x,g12.6," dzsr=",g12.6 &
    !& )', &
    !g%rx_gat, g%rx_tig, rx_dif, dx_gat, dx_tig, &
    !g%ry_gat, g%ry_tig, ry_dif, dy_gat, dy_tig, &
    !g%rz_gat, g%rz_tig, rz_dif, dz_gat, dz_tig
    !
    xif_opt_vofz : if ( v%opt_vofz ) then
      !
      call spike_compute_dif_vofz ( &
                                    r, v, &
                                    v%i3_norm, v%i4_gat, &
                                    rv_gat, xy_gat, dz_gat, &
                                    rt_gat, ra_gat, rf_gat, rp_gat &
                                  )
      !
      call spike_compute_dif_vofz ( &
                                    r, v, &
                                    v%i3_norm, v%i4_gat, &
                                    rv_tig, xy_tig, dz_tig, &
                                    rt_tig, ra_tig, rf_tig, rp_tig &
                                  )
      !
    else xif_opt_vofz
      !
      call spike_compute_dif_cons ( &
                                    rv_gat, xy_gat, dz_gat, &
                                    rt_gat, ra_gat, rf_gat, rp_gat &
                                  )
      !
      call spike_compute_dif_cons ( &
                                    rv_tig, xy_tig, dz_tig, &
                                    rt_tig, ra_tig, rf_tig, rp_tig &
                                  )
      !
    end if xif_opt_vofz
    !
    rt_dif = rt_gat + rt_tig
    !
    ra_dif = ra_gat * ra_tig
    !
    rc_dif = cos ( matfun_atan2 ( xy_gat, dz_gat ) )
    !
    ! compute the source ray vector at the diffractor, ax_gat, ay_gat, az_gat
    !
    pz_gat = rp_gat
    !
    sin_theta_gat = pz_gat * rv_gat
    !
    cos_theta_gat = sqrt ( max( 0., 1. - sin_theta_gat ** 2 ) )
    !
    xxif_xy_gat : if ( xy_gat .ne. 0. ) then
      !
      ax_gat = sin_theta_gat * dx_gat / xy_gat
      ay_gat = sin_theta_gat * dy_gat / xy_gat
      az_gat = cos_theta_gat
      !
      bx_gat = dx_gat / rr_gat
      by_gat = dy_gat / rr_gat
      bz_gat = dz_gat / rr_gat
      !
    else xxif_xy_gat
      !
      ax_gat = 0.
      ay_gat = 0.
      az_gat = cos_theta_gat
      !
      bx_gat = 0.
      by_gat = 0.
      bz_gat = 1.
      !
    end if xxif_xy_gat
    !
    ! compute the recevier ray vector at the diffractor, ax_tig, ay_tig, az_tig
    !
    pz_tig = rp_tig
    !
    sin_theta_tig = pz_tig * rv_tig
    !
    cos_theta_tig = sqrt ( max( 0., 1. - sin_theta_tig ** 2 ) )
    !
    xxif_xy_tig : if ( xy_tig .ne. 0. ) then
      !
      ax_tig = sin_theta_tig * dx_tig / xy_tig
      ay_tig = sin_theta_tig * dy_tig / xy_tig
      az_tig = cos_theta_tig
      !
      bx_tig = dx_tig / rr_tig
      by_tig = dy_tig / rr_tig
      bz_tig = dz_tig / rr_tig
      !
    else xxif_xy_tig
      !
      ax_tig = 0.
      ay_tig = 0.
      az_tig = cos_theta_tig
      !
      bx_tig = 0.
      by_tig = 0.
      bz_tig = 1.
      !
    end if xxif_xy_tig
    !
    ! compute the angle of incidence by taking the dot product
    ! of the two vectors at the diffractor
    ! dot product = ax_gat * ax_tig + ay_gat * ay_tig + az_gat * az_tig
    !               |a_gat| * |a_tig| cos ( ang_inc * 2. )
    !
    ang_inc = .5 * acos ( max ( -1., min ( + 1., &
                        ( ( ax_gat * ax_tig ) &
                        + ( ay_gat * ay_tig ) &
                        + ( az_gat * az_tig ) ) &
                                              ) ) )
    !
    ! compute the straight ray incidience angle by taking the dot product
    ! of the vectors from the source to diffractor and receiver to diffractor
    !
    ang_str = .5 * acos ( max ( -1., min ( + 1., &
                        ( ( dx_gat * dx_tig ) &
                        + ( dy_gat * dy_tig ) &
                        + ( dz_gat * dz_tig ) ) &
                        / ( rr_gat * rr_tig ) ) ) )
    !
    ang_err = ang_inc - ang_str
    !
    !print'(" ww3 t=",g10.4,1x,g10.4," p=",g10.4,1x,g10.4,&
    !& " a=",g10.4,1x,g10.4)',&
    !minval(v%rt_tab(:,:,1,1)), maxval(v%rt_tab(:,:,1,1)), &
    !minval(v%rp_tab(:,:,1,1)), maxval(v%rp_tab(:,:,1,1)), &
    !minval(v%ra_tab(:,:,1,1)), maxval(v%ra_tab(:,:,1,1))
    !
    !if ( i_call .le. 1 ) &
    !print'( &
    !& /, " q1 a ray      src =",g12.6,1x,g12.6,1x,g12.6,1x,g12.6, &
    !& /, " q1 a straight src =",g12.6,1x,g12.6,1x,g12.6,1x,g12.6, &
    !& /, " q1 a ray      rec =",g12.6,1x,g12.6,1x,g12.6,1x,g12.6, &
    !& /, " q1 a straight rec =",g12.6,1x,g12.6,1x,g12.6,1x,g12.6 &
    !& )', &
    !ax_gat,ay_gat,az_gat,sqrt(ax_gat**2+ay_gat**2+az_gat**2), &
    !bx_gat,by_gat,bz_gat,sqrt(bx_gat**2+by_gat**2+bz_gat**2), &
    !ax_tig,ay_tig,az_tig,sqrt(ax_tig**2+ay_tig**2+az_tig**2), &
    !bx_tig,by_tig,bz_tig,sqrt(bx_tig**2+by_tig**2+bz_tig**2)
    !
    !if ( i_call .le. 1 ) &
    !print'( &
    !& /, " q2 v_dif =",g12.6, " v_gat =",g12.6, " v_tig =",g12.6, &
    !& " v_gat_tig =",g12.6, &
    !& /, " q2 xy_gat =",g12.6, " xy_tig =",g12.6, &
    !& /, " q2 x =",g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6, &
    !& /, " q2 y =",g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6, &
    !& /, " q2 z =",g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6 &
    !& )', &
    !rv_dif, rv_gat, rv_tig, v%v_gat_tig, &
    !xy_gat, xy_tig, &
    !g%rx_gat, g%rx_tig, rx_dif, dx_gat, dx_tig, &
    !g%ry_gat, g%ry_tig, ry_dif, dy_gat, dy_tig, &
    !g%rz_gat, g%rz_tig, rz_dif, dz_gat, dz_tig
    !
    !if ( i_call .le. 1 ) &
    !print'( &
    !& /, " q3 p src =",g12.6,1x,g12.6,1x,g12.6,1x,g12.6, &
    !& /, " q3 p rec =",g12.6,1x,g12.6,1x,g12.6,1x,g12.6 &
    !& )', &
    !ax_gat, ay_gat, az_gat, sqrt(ax_gat**2+ay_gat**2+az_gat**2), rp_gat, &
    !ax_tig, ay_tig, az_tig, sqrt(ax_tig**2+ay_tig**2+az_tig**2), rp_tig
    !
    !if ( i_call .le. 1 ) &
    !print'( &
    !&    " q4 a=", f10.4, 1x, f10.4, 1x, g10.4, 1x, i8, &
    !& /, " q4 v=", g10.4, 1x, g10.4, 1x, g10.4, &
    !& /, " q4 t=", g12.6, 1x, g12.6, 1x, g12.6 &
    !& )', &
    !ang_inc*90./asin(1.), ang_str*90./asin(1.), ang_err*90./asin(1.), i_call, &
    !rv_gat, rv_tig, rv_dif, &
    !rt_gat, rt_tig, rt_dif
    !
    !if ( i_call .le. 1 ) &
    !print'(" q5 ", &
    !& " a=", f10.4, 1x, f10.4, &
    !& " x=", g10.4, 1x, g10.4, 1x, g10.4, &
    !& " z=", g10.4, &
    !& " t=", g12.6 ,1x, g12.6, 1x, g12.6, &
    !& " xy=", g10.4, 1x, g10.4 &
    !& )', &
    !ang_err*90./asin(1.), ang_str*90./asin(1.), &
    !g%rx_gat, g%rx_tig, rx_dif, &
    !rz_dif, rt_dif, &
    !rt_gat, rt_tig, &
    !xy_gat, xy_tig
    !
    if ( rz_dif .lt. min ( g%rz_tig, g%rz_gat ) ) &
    rt_dif = -1.
    !
    if ( min ( rt_gat, rt_tig ) .lt. 0. ) rt_dif = -1.
    !
    if ( rt_dif .lt. 0. ) ra_dif = 0.
    !
    !if ( i_call .ge. 1 ) stop
    !
    return
    !
  end subroutine spike_compute_dif_0
  !
  subroutine spike_compute_dif_vofz ( &
                                      r, v, &
                                      i3_tab, i4_tab, &
                                      rv_dif, xy_dif, dz_dif, &
                                      rt_dif, ra_dif, rf_dif, rp_dif &
                                    )
    !
    !  compute reflection time
    !
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    integer,                  intent(in   ) :: i3_tab
    integer,                  intent(in   ) :: i4_tab
    real,                     intent(inout) :: rv_dif
    real,                     intent(in   ) :: xy_dif
    real,                     intent(in   ) :: dz_dif
    real,                     intent(inout) :: rt_dif
    real,                     intent(inout) :: ra_dif
    real,                     intent(inout) :: rf_dif
    real,                     intent(inout) :: rp_dif
    !
    integer                                 :: jx_tab_1
    integer                                 :: jx_tab_2
    real                                    :: fx_tab_1
    real                                    :: fx_tab_2
    !
    integer                                 :: iz_tab_1
    integer                                 :: iz_tab_2
    real                                    :: fz_tab_1
    real                                    :: fz_tab_2
    !
    real                                    :: rf_x1_z1
    real                                    :: rf_x2_z1
    real                                    :: rf_x1_z2
    real                                    :: rf_x2_z2
    !
    !  determine the 2 x and z values to interpolate between
    !
    call interpolate_find_index_h1 ( &
    v%nx_tab, v%x0_tab, v%dx_tab, xy_dif, &
    jx_tab_1, jx_tab_2, fx_tab_1, fx_tab_2 &
                                    )
    !
    call interpolate_find_index_h1 ( &
    v%nz_tab, v%z0_tab, v%dz_tab, dz_dif, &
    iz_tab_1, iz_tab_2, fz_tab_1, fz_tab_2 &
                                    )
    !
    !print*,' spike_compute_dif_vofz xy_dif=',xy_dif,' dz_dif=',dz_dif
    !print*,' spike_compute_dif_vofz rt_tab=',minval(v%rt_tab), &
    !maxval(v%rt_tab)
    !print*,' nx_tab=', v%nx_tab, v%x0_tab, v%dx_tab, xy_dif
    !print*,' jx_tab=', jx_tab_1, jx_tab_2, fx_tab_1, fx_tab_2
    !print*,' nz_tab=', v%nz_tab, v%z0_tab, v%dz_tab, dz_dif
    !print*,' iz_tab=', iz_tab_1, iz_tab_2, fz_tab_1, fz_tab_2
    !
    rf_x1_z1 = fx_tab_1 * fz_tab_1
    !
    rf_x2_z1 = fx_tab_2 * fz_tab_1
    !
    rf_x1_z2 = fx_tab_1 * fz_tab_2
    !
    rf_x2_z2 = fx_tab_2 * fz_tab_2
    !
    rt_dif = &
    rf_x1_z1 * v%rt_tab ( jx_tab_1, iz_tab_1, i3_tab, i4_tab ) &
  + rf_x2_z1 * v%rt_tab ( jx_tab_2, iz_tab_1, i3_tab, i4_tab ) &
  + rf_x1_z2 * v%rt_tab ( jx_tab_1, iz_tab_2, i3_tab, i4_tab ) &
  + rf_x2_z2 * v%rt_tab ( jx_tab_2, iz_tab_2, i3_tab, i4_tab )
    !
    ra_dif = &
    rf_x1_z1 * v%ra_tab ( jx_tab_1, iz_tab_1, i3_tab, i4_tab ) &
  + rf_x2_z1 * v%ra_tab ( jx_tab_2, iz_tab_1, i3_tab, i4_tab ) &
  + rf_x1_z2 * v%ra_tab ( jx_tab_1, iz_tab_2, i3_tab, i4_tab ) &
  + rf_x2_z2 * v%ra_tab ( jx_tab_2, iz_tab_2, i3_tab, i4_tab )
    !
    rf_dif = &
    rf_x1_z1 * v%rf_tab ( jx_tab_1, iz_tab_1, i3_tab, i4_tab ) &
  + rf_x2_z1 * v%rf_tab ( jx_tab_2, iz_tab_1, i3_tab, i4_tab ) &
  + rf_x1_z2 * v%rf_tab ( jx_tab_1, iz_tab_2, i3_tab, i4_tab ) &
  + rf_x2_z2 * v%rf_tab ( jx_tab_2, iz_tab_2, i3_tab, i4_tab )
    !
    rp_dif = &
    rf_x1_z1 * v%rp_tab ( jx_tab_1, iz_tab_1, i3_tab, i4_tab ) &
  + rf_x2_z1 * v%rp_tab ( jx_tab_2, iz_tab_1, i3_tab, i4_tab ) &
  + rf_x1_z2 * v%rp_tab ( jx_tab_1, iz_tab_2, i3_tab, i4_tab ) &
  + rf_x2_z2 * v%rp_tab ( jx_tab_2, iz_tab_2, i3_tab, i4_tab )
    !
    rv_dif = 1. / ( &
    fz_tab_1 * v%rs_tab ( iz_tab_1 ) &
  + fz_tab_2 * v%rs_tab ( iz_tab_2 ) &
                  )
    !
    !print*,' spike_compute_dif_vofz rt_dif=',rt_dif, &
    !v%rt_tab ( jx_tab_1, iz_tab_1, i3_tab, i4_tab ), &
    !v%rt_tab ( jx_tab_2, iz_tab_1, i3_tab, i4_tab ), &
    !v%rt_tab ( jx_tab_1, iz_tab_2, i3_tab, i4_tab ), &
    !v%rt_tab ( jx_tab_2, iz_tab_2, i3_tab, i4_tab )
    !
    !    xxif_rt_dif : if ( min ( &
    !v%rt_tab ( jx_tab_1, iz_tab_1, i3_tab, i4_tab ), &
    !v%rt_tab ( jx_tab_2, iz_tab_1, i3_tab, i4_tab ), &
    !v%rt_tab ( jx_tab_1, iz_tab_2, i3_tab, i4_tab ), &
    !v%rt_tab ( jx_tab_2, iz_tab_2, i3_tab, i4_tab ) ) .lt. 0. ) then
    !
    !print*,' xy_dif=',xy_dif
    !print*,' dz_dif=',dz_dif
    !print*,' rt_dif=',rt_dif
    !print*,' nx=',v%nx_tab,v%x0_tab,v%dx_tab
    !print*,' nz=',v%nz_tab,v%z0_tab,v%dz_tab
    !print*,' rf=', rf_x1_z1, rf_x2_z1, rf_x1_z2, rf_x2_z2
    !print*,' fx=', fx_tab_1, fx_tab_2
    !print*,' fz=', fz_tab_1, fz_tab_1
    !print*,' ix=', jx_tab_1, jx_tab_2
    !print*,' iz=', iz_tab_1, iz_tab_1
    !print*,' i3=', i3_tab, i4_tab
    !print*,' rt=', &
    !v%rt_tab ( jx_tab_1, iz_tab_1, i3_tab, i4_tab ), &
    !v%rt_tab ( jx_tab_2, iz_tab_1, i3_tab, i4_tab ), &
    !v%rt_tab ( jx_tab_1, iz_tab_2, i3_tab, i4_tab ), &
    !v%rt_tab ( jx_tab_2, iz_tab_2, i3_tab, i4_tab )
    !print*,' min(rt_tab)=',minval(v%rt_tab)
    !print*,' max(rt_tab)=',maxval(v%rt_tab)
    !
    !stop
    !
    !    end if xxif_rt_dif
    !
    if ( min ( &
    v%rt_tab ( jx_tab_1, iz_tab_1, i3_tab, i4_tab ), &
    v%rt_tab ( jx_tab_2, iz_tab_1, i3_tab, i4_tab ), &
    v%rt_tab ( jx_tab_1, iz_tab_2, i3_tab, i4_tab ), &
    v%rt_tab ( jx_tab_2, iz_tab_2, i3_tab, i4_tab ) ) .lt. 0. ) rt_dif = 0.
    !
    return
    !
  end subroutine spike_compute_dif_vofz
  !
  subroutine spike_compute_dif_cons ( &
                                      rv_dif, xy_dif, dz_dif, &
                                      rt_dif, ra_dif, rf_dif, rp_dif &
                                    )
    !
    !  compute reflection time
    !
    real,                     intent(in   ) :: rv_dif
    real,                     intent(in   ) :: xy_dif
    real,                     intent(in   ) :: dz_dif
    real,                     intent(inout) :: rt_dif
    real,                     intent(inout) :: ra_dif
    real,                     intent(inout) :: rf_dif
    real,                     intent(inout) :: rp_dif
    !
    real                                    :: rr_dif
    !
    rr_dif = sqrt ( xy_dif ** 2 + dz_dif ** 2 )
    !
    rt_dif = rr_dif / rv_dif
    !
    ra_dif = 1. / rr_dif
    !
    rf_dif = 0.
    !
    rp_dif = xy_dif / rr_dif / rv_dif ! sin ( angle ) / velocity
    !
    return
    !
  end subroutine spike_compute_dif_cons
  !
  subroutine spike_compute_ref_0 ( g, r, v, i_err )
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    integer,                 intent (inout) :: i_err
    !
    ! Local variables
    !
    integer                                 :: i0_iter
    integer                                 :: l1
    integer                                 :: n2 = 2
    real                                    :: p0(3,2)
    real                                    :: y0(3)
    real                                    :: dx
    real                                    :: dy
    real                                    :: t0_tol
    real                                    :: r0_tol
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    !if ( i_call .le. 1 ) &
    !print'(" spike_compute_ref_0 i_call=", i8 &
    !& )', &
    !i_call
    !
    i_err = 0
    !
    dx = v%dx_tab * 5.
    !
    dy = v%dx_tab * 5.
    !
    !dx = v%dx_tab * 20.
    !
    !dy = v%dx_tab * 20.
    !
    p0(1,1) = r%rx_ref + dx
    !
    p0(1,2) = r%ry_ref
    !
    p0(2,1) = r%rx_ref - dx
    !
    p0(2,2) = r%ry_ref + dy
    !
    p0(3,1) = r%rx_ref - dx
    !
    p0(3,2) = r%ry_ref - dy
    !
    p0(1,1) = r%rx_ref
    !
    p0(1,2) = r%ry_ref
    !
    p0(2,1) = r%rx_ref + dx
    !
    p0(2,2) = r%ry_ref
    !
    p0(3,1) = r%rx_ref
    !
    p0(3,2) = r%ry_ref - dy
    !
    t0_tol = .0001
    !
    ! find reflection posiiton by minimizing the time using a simplex method
    !
    call spike_simplex ( g, r, v, &
    n2, p0, y0, t0_tol, r0_tol, i0_iter, i_err )
    !
    if ( i_err .ne. 0 ) return
    !
    ! select the minimum time
    !
    l1 = 1
    if ( y0(2) .lt. y0(l1) ) l1 = 2
    if ( y0(3) .lt. y0(l1) ) l1 = 3
    !
    r%rx_ref = p0(l1,1)
    r%ry_ref = p0(l1,2)
    r%rz_ref = &
    r%dep_ref_0 + r%dz_dx_0 * r%rx_ref + r%dz_dy_0 * r%ry_ref
    !
    call spike_compute_dif_0 ( &
                               g, r, v, &
                               r%rx_ref, r%ry_ref, r%rz_ref, &
                               r%rv_ref, r%rt_ref, r%ra_ref, &
                               r%ang_ref, r%rc_ref &
                             )
    !
    return
    !
  end subroutine spike_compute_ref_0
  !
  subroutine spike_simplex ( g, r, v, &
  n2, p0, y0, t0_tol, r0_tol, i0_iter, i_err )
    !
    ! find the p0 location with the minum y0 value via a simplex method
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    integer,                 intent (in   ) :: n2
    real,                    intent (inout) :: p0 ( :, : )
    real,                    intent (inout) :: y0 ( : )
    integer,                 intent (inout) :: i0_iter
    real,                    intent (in   ) :: t0_tol
    real,                    intent (inout) :: r0_tol
    integer,                 intent (inout) :: i_err
    !
    integer,                      parameter :: n0_iter = 500
    integer,                      parameter :: n0_max = 20
    real,                         parameter :: alpha = 1.0
    real,                         parameter :: beta = 0.5
    real,                         parameter :: gamma = 2.0
    !
    integer                                 :: n1
    integer                                 :: l1
    integer                                 :: h1
    integer                                 :: h2
    integer                                 :: i1
    real                                    :: y1
    real                                    :: y2
    real                                    :: y0_inv
    real                                    :: p1 ( n0_max )
    real                                    :: p2 ( n0_max )
    real                                    :: p3 ( n0_max )
    real                                    :: p4 ( n2 )
    !
    i_err = 0
    !
    n1 = n2 + 1
    !
    i0_iter = 0
    !
    r0_tol = max ( t0_tol * 10. , 1.e10 )
    !
    ! compute the initial values at the trial points
    !
    do_init_y : do i1= 1 , n1
      !
      p4(1:n2) = p0(i1,1:n2)
      !
      y0(i1) = spike_simplex_0 ( g, r, v, p4, i_err )
      !
      if ( i_err .ne. 0 ) go to 998
      !
    end do do_init_y
    !
1   continue
    !
    l1 = 1
    !
    xxif_1_gt_2 : if ( y0 ( 1 ) .gt. y0 ( 2 ) ) then
      !
      h1 = 1
      !
      h2 = 2
      !
    else xxif_1_gt_2
      !
      h1 = 2
      !
      h2 = 1
      !
    end if xxif_1_gt_2
    !
    do_loop_1 : do i1 = 1, n1
      !
      if ( y0 ( i1 ) .lt. y0 ( l1 ) ) l1 = i1
      !
      xxif_i_gt_h1 : if ( y0 ( i1 ) .gt. y0 ( h1 ) ) then
        !
        h2 = h1
        !
        h1 = i1
        !
      else if ( y0 ( i1 ) .gt. y0 ( h2 ) ) then
        !
        if ( i1 .ne. h1 ) h2 = i1
        !
      end if xxif_i_gt_h1
      !
    end do do_loop_1
    !
    y0_inv = abs ( y0 ( h1 ) ) + abs ( y0 ( l1 ) )
    !
    xxif_y0 : if ( y0_inv .eq. 0. ) then
      !
      r0_tol = 0.
      !
    else xxif_y0
      !
      r0_tol = 2. * abs ( y0 ( h1 ) - y0 ( l1 ) ) / y0_inv
      !
    end if xxif_y0
    !
    !print*,' a2 i0_iter=',i0_iter,' r0_tol=',r0_tol, &
    !h1,l1,y0 ( h1 ), y0 ( l1 )
    !
    if ( r0_tol .lt. t0_tol ) go to 2
    !
    xxif_i0_iter : if ( i0_iter .eq. n0_iter ) then
      !
      print'(" exceeding maximum iterations n0_iter=", i8) ', n0_iter
      !
      go to 2
      !
    end if xxif_i0_iter
    !
    i0_iter = i0_iter + 1
    !
    p3 ( 1:n2 ) = 0.
    !
    do_loop_2 : do i1 = 1, n1
      !
      if ( i1 .ne. h1 ) &
      p3 ( 1:n2 ) = p3 ( 1:n2 ) + p0 ( i1, 1:n2 )
      !
    end do do_loop_2
    !
    p3 ( 1:n2 ) = p3 ( 1:n2 ) / n2
    !
    p1 ( 1:n2 ) = ( 1. + alpha ) * p3 ( 1:n2 ) &
                          - alpha   * p0 ( h1, 1:n2 )
    !
    y1 = spike_simplex_0 ( g, r, v, p1, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    xxif_y1_le_l1 : if ( y1 .le. y0 ( l1 ) ) then
      !
      p2 ( 1:n2 ) =        gamma   * p1 ( 1:n2 ) &
                  + ( 1. - gamma ) * p3 ( 1:n2 )
      !
      y2 = spike_simplex_0 ( g, r, v, p2, i_err )
      !
      if ( i_err .ne. 0 ) go to 996
      !
      xxif_y2_lt_l1 : if ( y2 .lt. y0 ( l1 ) ) then
        !
        p0 ( h1, 1:n2 ) = p2 ( 1:n2 )
        !
        y0 ( h1 ) = y2
        !
      else xxif_y2_lt_l1
        !
        p0 ( h1, 1:n2 ) = p1 ( 1:n2 )
        !
        y0 ( h1 ) = y1
        !
      end if xxif_y2_lt_l1
      !
    else if ( y1 .ge. y0 ( h2  ) ) then
      !
      xxif_y1_lt_h1 : if ( y1 .lt. y0 ( h1 ) ) then
        !
        p0 ( h1, 1:n2 ) = p1 ( 1:n2 )
        !
        y0 ( h1 ) = y1
        !
      end if xxif_y1_lt_h1
      !
      p2 ( 1:n2 ) =        beta   * p0 ( h1, 1:n2 ) &
                  + ( 1. - beta ) * p3 ( 1:n2 )
      !
      y2 = spike_simplex_0 ( g, r, v, p2, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
      xxif_y2_lt_h1 : if ( y2 .lt. y0 ( h1 ) ) then
        !
        p0 ( h1, 1:n2 ) = p2 ( 1:n2 )
        !
        y0 ( h1 ) = y2
        !
      else xxif_y2_lt_h1
        !
        do_loop_3 : do i1 = 1, n1
          !
          xxif_i_ne_l1 : if ( i1 .ne. l1 ) then
            !
            p1 ( 1:n2 ) = 0.5 * ( p0 ( i1, 1:n2 ) &
                                + p0 ( l1, 1:n2 ) )
            !
            p0 ( i1, 1:n2 ) = p1 ( 1:n2 )
            !
            y0 ( i1 ) = spike_simplex_0 ( g, r, v, p1, i_err )
            !
            if ( i_err .ne. 0 ) go to 994
            !
          end if xxif_i_ne_l1
          !
        end do do_loop_3
        !
      end if xxif_y2_lt_h1
      !
    else xxif_y1_le_l1
      !
      p0 ( h1, 1:n2 ) = p1 ( 1:n2 )
      !
      y0 ( h1 )  = y1
      !
    end if xxif_y1_le_l1
    !
    go to 1
    !
  2 continue
    !
    !print*,' a3 i0_iter=',i0_iter,' r0_tol=',r0_tol
    !
    return
    !
994 continue
    !
    go to 999
    !
995 continue
    !
    go to 999
    !
996 continue
    !
    go to 999
    !
997 continue
    !
    go to 999
    !
998 continue
    !
    go to 999
    !
999 continue
    !
    go to 2
    !
  end subroutine spike_simplex
  !
  real function spike_simplex_0 ( g, r, v, p, i_err )
    !
    ! compute two way time from source to diffractor to receiver
    !
    type ( syngrid_struct ),        pointer :: g ! syngrid
    type ( synref_struct ),         pointer :: r ! synref
    type ( synvofz_struct ),        pointer :: v ! synvofz
    real,                     intent(in   ) :: p(:)
    integer,                  intent(inout) :: i_err
    !
    real                                    :: rx_ref
    real                                    :: ry_ref
    real                                    :: rz_ref
    real                                    :: rv_ref
    real                                    :: rt_ref
    real                                    :: ra_ref
    real                                    :: ang_ref
    real                                    :: rc_ref
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    !if ( i_call .le. 1 ) &
    !print'(" spike_simplex_0 i_call=", i8 &
    !& )', &
    !i_call
    !
    i_err = 0
    !
    rx_ref = p(1)
    ry_ref = p(2)
    rz_ref = r%dep_ref_0 + r%dz_dx_0 * rx_ref + r%dz_dy_0 * ry_ref
    !
    !  compute the velocity at this reflection point
    !
    call interpolate_3d_to_0d ( &
                                v%nz_vel, v%z0_vel, v%dz_vel, &
                                v%nx_vel, v%x0_vel, v%dx_vel, &
                                v%ny_vel, v%y0_vel, v%dy_vel, &
                                v%rv_vel, &
                                rz_ref, &
                                rx_ref, &
                                ry_ref, &
                                rv_ref &
                              )
    !
    call spike_compute_dif_0 ( &
                               g, r, v, &
                               rx_ref, ry_ref, rz_ref, &
                               rv_ref, rt_ref, ra_ref, &
                               ang_ref, rc_ref &
                             )
    !
    spike_simplex_0 = rt_ref
    !
    if ( rt_ref .lt. 0. ) i_err = -1
    !
    return
    !
  end function spike_simplex_0
  !
  integer function spike_i_pel ( )
    !
    ! current worker index
    !
    spike_i_pel = pcpsx_i_pel()
    !
    return
    !
  end function spike_i_pel
  !
  integer function spike_n_pel ( )
    !
    ! number of workers
    !
    integer                          :: num_cpus
    !
    xxif_process_traces : if ( .not. pc_do_not_process_traces() ) then
      !
      ! during execution use the mpi call for the number of pes
      !
      spike_n_pel = pcpsx_n_pel()
      !
    else xxif_process_traces
      !
      ! in the cps frontend use the jobdata call for the number of pes
      !
      call pc_get_jdata ( 'num_cpus', num_cpus )
      !
      spike_n_pel = num_cpus
      !
    end if xxif_process_traces
    !
    return
    !
  end function spike_n_pel
  !
end module spike_module
