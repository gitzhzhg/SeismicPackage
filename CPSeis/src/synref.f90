!<CPS_v1 type="PRIMITIVE"/>
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
!                         C P S   P R I M I T I V E
!
! Name       : SYNREF
! Category   : Synthetics
! Written    : 2006-04-04   by: Douglas Hanson
! Revised    : 2007-09-11   by: Douglas Hanson Make parallel.
! Maturity   : beta
! Purpose    : Generate synthetic trace locations.
! Portability: No known limitations.
! Parallel   : No.
!
!------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
!------------------------------------------------------------------------------
!</descript_doc>

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

!<advice_doc>
!------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!  SYNREF defines reflector and difractor attributes for synthetic generation.
!
!------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! NO input traces -- synref is a trace supplying process.
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
! 13  2007-09-11  Douglas Hanson Make parallel.
! 12  2007-05-29  Douglas Hanson Add events arrays.
! 11  2007-05-03  Douglas Hanson Add OPT_GHOST.
! 10  2007-04-12  Douglas Hanson Remove use synloc statement.
!  9  2007-01-16  Douglas Hanson Fix ref scale.
!  8  2007-01-03  Douglas Hanson Fix ref double write.
!  7  2006-09-21  Douglas Hanson Add synref_compute_ref.
!  6  2006-06-22  Douglas Hanson Add PIK register arrays.
!  5  2006-06-08  Douglas Hanson Add OPT_SPIKE.
!  4  2006-06-01  Stoeckley      Add pc_register_array_names for SeisSpace.
!  3  2006-05-02  Douglas Hanson Fix OPT_ZOEPPRITZ get.
!  2  2006-04-20  Douglas Hanson Modify layout.
!  1  2006-04-04  Douglas Hanson Original version.
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
! Synthetic trace events.
!
! TIM_INTERP=`IIIIIIIIIII  TIM_TYPE=`CCCCCCC       
!
! OPT_GHOST_NON=`CC  OPT_GHOST_GAT=`CC  OPT_GHOST_TIG=`CC  
! DEP_GHOST =`FFFFFFFFFFF  RC_GHOST =`FFFFFFFFFFF  
!
! OPT_DECAY =`CC  COS_DECAY =`FFFFFFFFFFF  PWR_DECAY =`FFFFFFFFFFF   
!
! OPT_NOISE =`CC  AMP_NOISE =`FFFFFFFFFFF  NOISE_TYPE=`CCCCCCC  
!
! OPT_DIRECT=`CC  AMP_DIRECT=`FFFFFFFFFFF  PWR_DIRECT=`FFFFFFFFFFF  
!
! OPT_SPIKE =`CC  AMP_SPIKE =`FFFFFFFF      
! SPIKE_TOT=`IIIIIII SPIKE_MIN =`FFFFFFF SPIKE_MAX =`FFFFFFF SPIKE_INC=`FFFFFFFF
!
! OPT_ZOEPPRITZ=`CC OPT_REF=`CC  OPT_DIF=`CC  OPT_PIK=`CC  
!
! OPT_COMPUTE_REF=`CC  
! Select PATH_REF_INP [PATH_REF_INP]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! Select PATH_REF_OUT [PATH_REF_OUT]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!<NS SYNREF_REFLECTORS/NC=80>
!
!                REFLECTOR DEFINIITON
!  AMP_REF       DEP_REF       X_DIP_REF     Y_DIP_REF
!  `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!  `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!  `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!
!    ABOVE REFLECTOR DEFINIITON          BELOW REFLECTOR DEFINIITON
!  DN1_REF     VP1_REF     VS1_REF     DN2_REF     VP2_REF     VS2_REF
!  `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!  `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!  `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!
!<NS SYNREF_DIFFRACTORS/NC=80>
!
! Select PATH_DIF [PATH_DIF]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!               DIFFRACTOR DEFINIITON
!  AMP_DIF       DEP_DIF       X_LOC_DIF     Y_LOC_DIF
!  `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!  `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!  `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!
!    ABOVE DIFFRACTOR DEFINIITON          BELOW DIFFRACTOR DEFINIITON
!  DN1_DIF     VP1_DIF     VS1_DIF     DN2_DIF     VP2_DIF     VS2_DIF
!  `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!  `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!  `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
!
!<NS SYNREF_PICKS/NC=80>
!
! Select PATH_PIK [PATH_PIK]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!               PICK DEFINIITON
!  AMP_PIK       DEP_PIK       X_LOC_PIK     Y_LOC_PIK
!  `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!  `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!  `FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!
!<PARMS AMP_REF_ARRAYSET[/ML=128/XST/YST]>
!<PARMS DN1_REF_ARRAYSET[/ML=128/XST/YST]>
!<PARMS AMP_DIF_ARRAYSET[/ML=128/XST/YST]>
!<PARMS DN1_DIF_ARRAYSET[/ML=128/XST/YST]>
!<PARMS AMP_PIK_ARRAYSET[/ML=128/XST/YST]>
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="TIM_TYPE">
!<Tip> Type of time computetaion for reflctor and diffractors. </Tip>
! Default = NORMAL
! Allowed = NORMAL   Use normal 3D model.
! Allowed = X_SLICE  Model data for a vertical slice along the X dierection.
! Allowed = Y_SLICE  Model data for a vertical slice along the Y dierection.
! Allowed = VERTICAL Model data for a vertical slice at each X,Y location.
!Spike can model data using different interpretations of the
!reflector and diffractor coefficients.
!
!If TIM_TYPE = NORMAL SPIKE honors the trace source and reciever positions
!and the reflector and diffractor positions in a 3D sense.
!
!If TIM_TYPE = X_SLICE SPIKE will model data using a slice in the X direction
!through the reflector model at each output traces X midpoint.
!All the diffractor Y locations will be reset to each output traces Y midpoint.
!
!If TIM_TYPE = Y_SLICE SPIKE will model data using a slice in the Y direction
!through the reflector model at each output traces Y midpoint.
!All the diffractor X locations will be reset to each output traces X midpoint.
!
!If TIM_TYPE = VERTICAL SPIKE will model data using a vertical slice
!through the reflector model at each output traces X,Y midpoint.
!All the diffractor X,Y locations will be reset
!to each output traces X,Y midpoint.
!
!<Help KEYWORD="TIM_INTERP">
!<Tip> Trace Time interpolation factor. </Tip>
! Default = 4
! Allowed = integer > 0
!  All computations are done on a time grid TIM_INTERP times finer than the
!  output time grid.  The final out trace is decimated from this computed trace.
!</Help>
!
!<Help KEYWORD="OPT_ZOEPPRITZ">
!<Tip> Whether to use ZOEPPRITZ solution for reflection coefficient.</Tip>
! Allowed = NO   Do not use zoeppritz reflection coefficients.
! Default = NO
! Allowed = NO   Do not use zoeppritz reflection coefficients.
! Allowed = YES  Do     use zoeppritz reflection coefficients.
!</Help>
!
!<Help KEYWORD="OPT_NOISE">
!<Tip> Whether to add NOISE events to traces. </Tip>
! Default = NO
! Allowed = NO   Do not add NOISE events to traces. 
! Allowed = YES  Do     add NOISE events to traces. 
!  Noise eventes are only added if OPT_NOISE = YES
!</Help>
!
!<Help KEYWORD="AMP_NOISE">
!<Tip> Standard deviation of the random trace values. </Tip>
! Default = 0.1
! Allowed = real>=0.0
!
!  Noise eventes are added if OPT_NOISE = YES
!
!  The trace noise characteristics are defined by Gaussian distributed
!  pseudo-random values at each output time sample with mean zero and
!  standard deviation AMP_NOISE.
!
!  You can get zero noise by setting the random number standard deviation,
!  AMP_NOISE, to zero.
!
!</Help>
!
!<Help KEYWORD="NOISE_TYPE">
!<Tip> Type of random numbers to add to the output traces. </Tip>
! Default = DIFF
! Allowed = SAME
! Allowed = DEAD
!
!  Noise eventes are added if OPT_NOISE = YES
!
!  Parameter NOISE_TYPE controls the type of random number noise pattern for
!  each output trace.
!
!  If NOISE_TYPE = SAME, the same set of random spikes are added to each trace.
!  If NOISE_TYPE = DIFF, a different set of random spikes is added to each
!  trace.
!  If NOISE_TYPE = DEAD, the traces contain only zero values.
!
!  In any case the mute header words (2 and 64) are set to 1 and NDPT.
!  Thus, it is assumed that you may supply live values to any dead traces.
!
!  NOISE_TYPE = SAME and NOISE_TYPE = DEAD, are very fast because only ONE is
!  trace generated at setup time; this trace is then passed out over and over,
!  with only the headers varying.  NOISE_TYPE = DIFF is slower, but should be
!  used if you need different random noise values in each trace.
!
!</Help>
!
!<Help KEYWORD="OPT_GHOST_NON">
!<Tip> Whether to add non ghost events. </Tip>
! Default = YES
! Allowed = NO   Do not add non ghost events. 
! Allowed = YES  Do     add non ghost events. 
!</Help>
!
!<Help KEYWORD="OPT_GHOST_GAT">
!<Tip> Whether to add GAT ghost events. </Tip>
! Default = NO
! Allowed = NO   Do not add GAT ghost events. 
! Allowed = YES  Do     add GAT ghost events. 
!</Help>
!
!<Help KEYWORD="OPT_GHOST_TIG">
!<Tip> Whether to add TIG ghost events. </Tip>
! Default = NO
! Allowed = NO   Do not add TIG ghost events. 
! Allowed = YES  Do     add TIG ghost events. 
!</Help>
!
!<Help KEYWORD="DEP_GHOST">
!<Tip> Free surface depth for ghosting. </Tip>
! Default = 0.
! Allowed = real
!</Help>
!
!<Help KEYWORD="RC_GHOST">
!<Tip> Free surface reflection coefficient gor ghosting. </Tip>
! Default = -1.
! Allowed = real
!</Help>
!
!<Help KEYWORD="OPT_DECAY">
!<Tip> Whether to include COS_DECAY and PWR_DECAY terms in amplitudes. </Tip>
! Default = NO
! Allowed = NO   Do not include COS_DECAY and PWR_DECAY terms in amplitudes. 
! Allowed = YES  Do     include COS_DECAY and PWR_DECAY terms in amplitudes. 
!  Event amplitudes are modified by COS_DECAY and PWR_DECAY if OPT_DECAY = YES
!  Setting PWR_DECAY = .5 yields 2D gemoetrical divergence.
!  Setting PWR_DECAY = 1. yields 3D gemoetrical divergence.
!  The trace spike, reflector and diffractor amplitudes are scaled by
!  1. / MAX(.1,TIME)**PWR_DECAY
!  The trace reflector and diffractor amplitudes are scaled by
!  COSINE(DIP)**COS_DECAY
!  The trace noise spikes are not scaled.
!</Help>
!
!<Help KEYWORD="PWR_DECAY">
!<Tip> Trace amplitude scaling exponential. </Tip>
! Default = 0.0
! Allowed = real scalar
!  Event amplitudes are modified by COS_DECAY and PWR_DECAY if OPT_DECAY = YES
!  Setting PWR_DECAY = .5 yields 2D gemoetrical divergence.
!  Setting PWR_DECAY = 1. yields 3D gemoetrical divergence.
!  The trace spike, reflector and diffractor amplitudes are scaled by
!  1. / MAX(.1,TIME)**PWR_DECAY
!  The trace reflector and diffractor amplitudes are scaled by
!  COSINE(DIP)**COS_DECAY
!  The trace noise spikes are not scaled.
!</Help>
!
!<Help KEYWORD="COS_DECAY">
!<Tip> Dip cosine amplitude scaling exponential. </Tip>
! Default = 1.0
! Allowed = real scalar
!  Event amplitudes are modified by COS_DECAY and PWR_DECAY if OPT_DECAY = YES
!  Setting PWR_DECAY = .5 yields 2D gemoetrical divergence.
!  Setting PWR_DECAY = 1. yields 3D gemoetrical divergence.
!  The trace spike, reflector and diffractor amplitudes are scaled by
!  1. / MAX(.1,TIME)**PWR_DECAY
!  The trace reflector and diffractor amplitudes are scaled by
!  COSINE(DIP)**COS_DECAY
!  The trace noise spikes are not scaled.
!</Help>
!
!<Help KEYWORD="OPT_DIRECT">
!<Tip> Whether to add DIRECT arrival events to traces. </Tip>
! Default = NO
! Allowed = NO   Do not add DIRECT arrival events to traces. 
! Allowed = YES  Do     add DIRECT arrival events to traces. 
!  A direct arrival event is added to traces if OPT_DIRECT = YES
!  Direct arrival event amplitudes are modified PWR_DIRECT.
!  Setting PWR_DIRECT = 0. yields 2D gemoetrical divergence for direct arrivals.
!  Setting PWR_DIRECT = .5 yields 3D gemoetrical divergence for direct arrivals.
!  The trace direct arrival amplitudes are 
!  AMP_DIRECT / MAX(.1,TIME)**PWR_DIRECT.
!</Help>
!
!<Help KEYWORD="AMP_DIRECT">
!<Tip> Amplitude for direct arrival event. </Tip>
! Default = 0.0
! Allowed = real scalar
!  A direct arrival event is added to traces if OPT_DIRECT = YES
!  Direct arrival event amplitudes are modified PWR_DIRECT.
!  Setting PWR_DIRECT = 0. yields 2D gemoetrical divergence for direct arrivals.
!  Setting PWR_DIRECT = .5 yields 3D gemoetrical divergence for direct arrivals.
!  The trace direct arrival amplitudes are 
!  AMP_DIRECT / MAX(.1,TIME)**PWR_DIRECT.
!</Help>
!
!<Help KEYWORD="PWR_DIRECT">
!<Tip> Amplitude decay power for direct arrival event. </Tip>
! Default = .5
! Allowed = real scalar
!  A direct arrival event is added to traces if OPT_DIRECT = YES
!  Direct arrival event amplitudes are modified PWR_DIRECT.
!  Setting PWR_DIRECT = 0. yields 2D gemoetrical divergence for direct arrivals.
!  Setting PWR_DIRECT = .5 yields 3D gemoetrical divergence for direct arrivals.
!  The trace direct arrival amplitudes are 
!  AMP_DIRECT / MAX(.1,TIME)**PWR_DIRECT.
!</Help>
!
!<Help KEYWORD="OPT_SPIKE">
!<Tip> Whether to add SPIKE events to traces. </Tip>
! Default = YES
! Allowed = NO   Do not add SPIKE events to traces. 
! Allowed = YES  Do     add SPIKE events to traces. 
!  Spike eventes are added if OPT_SPIKE = YES
!  Note the spikes defined by AMP_SPIKE, SPIKE_TOT, SPIKE_MIN, SPIKE_MAX, and
!  SPIKE_INC will be the same on every output trace.
!</Help>
!
!<Help KEYWORD="AMP_SPIKE">
!<Tip> Amplitude of spike events. </Tip>
! Default = 1
! Allowed = real
!  Spike eventes are added if OPT_SPIKE = YES
!  Note the spikes defined by AMP_SPIKE, SPIKE_TOT, SPIKE_MIN, SPIKE_MAX, and
!  SPIKE_INC will be the same on every output trace.
!</Help>
!
!<Help KEYWORD="SPIKE_TOT">
!<Tip> Number of spikes in the output trace. </Tip>
! Default = 1
! Allowed = int>=0
!  Spike eventes are added if OPT_SPIKE = YES
!  Note the spikes defined by AMP_SPIKE, SPIKE_TOT, SPIKE_MIN, SPIKE_MAX, and
!  SPIKE_INC will be the same on every output trace.
!</Help>
!
!<Help KEYWORD="SPIKE_MIN">
!<Tip> Minimum output spike time value. </Tip>
! Default = .5
! Allowed = real scalar
!  Spike eventes are added if OPT_SPIKE = YES
!  Note the spikes defined by AMP_SPIKE, SPIKE_TOT, SPIKE_MIN, SPIKE_MAX, and
!  SPIKE_INC will be the same on every output trace.
!</Help>
!
!<Help KEYWORD="SPIKE_MAX">
!<Tip> Maximum output spike time value. </Tip>
! Default = .5
! Allowed = real scalar
!  Spike eventes are added if OPT_SPIKE = YES
!  Note the spikes defined by AMP_SPIKE, SPIKE_TOT, SPIKE_MIN, SPIKE_MAX, and
!  SPIKE_INC will be the same on every output trace.
!</Help>
!
!<Help KEYWORD="SPIKE_INC">
!<Tip> Output spike time increment. </Tip>
! Default = 1.0
! Allowed = real scalar
!  Spike eventes are added if OPT_SPIKE = YES
!  Note the spikes defined by AMP_SPIKE, SPIKE_TOT, SPIKE_MIN, SPIKE_MAX, and
!  SPIKE_INC will be the same on every output trace.
!</Help>
!
!<Help KEYWORD="OPT_REF">
!<Tip> Whether to add REFLECTION events to traces. </Tip>
! Default = NO
! Allowed = NO   Do not add REFLECTION events to traces. 
! Allowed = YES  Do     add REFLECTION events to traces. 
!  Reflector eventes are added if OPT_REF = YES
!
! SPIKE assumes that each reflector depth, DEPTH ( X, Y )
! at any X, Y location is defined by
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF ) * X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in Hdr_x and Hdr_y units.
!</Help>
!
!<Help KEYWORD="AMP_REF">
!<Tip> Reflector amplitude array. </Tip>
! Default = 1
! Allowed = real (array)
!  Reflector eventes are added if OPT_REF = YES
!
! SPIKE assumes that each reflector depth, DEPTH ( X, Y )
! at any X, Y location is defined by
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF ) * X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in Hdr_x and Hdr_y units.
!</Help>
!
!<Help KEYWORD="DEP_REF">
!<Tip> Reflector depth array. </Tip>
! Default = 1
! Allowed = real (array)
!  Reflector eventes are added if OPT_REF = YES
!
! SPIKE assumes that each reflector depth, DEPTH ( X, Y )
! at any X, Y location is defined by
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF ) * X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in Hdr_x and Hdr_y units.
!</Help>
!
!<Help KEYWORD="X_DIP_REF">
!<Tip> Reflector X dip array. </Tip>
! Default = 1
! Allowed = real (array)
! Allowed = real (array)
!  Reflector eventes are added if OPT_REF = YES
!
! SPIKE assumes that each reflector depth, DEPTH ( X, Y )
! at any X, Y location is defined by
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF ) * X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in Hdr_x and Hdr_y units.
!</Help>
!
!<Help KEYWORD="Y_DIP_REF">
!<Tip> Reflector Y dip array. </Tip>
! Default = 1
! Allowed = real (array)
! Allowed = real (array)
!  Reflector eventes are added if OPT_REF = YES
!
! SPIKE assumes that each reflector depth, DEPTH ( X, Y )
! at any X, Y location is defined by
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF ) * X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in Hdr_x and Hdr_y units.
!</Help>
!
!<Help KEYWORD="DN1_REF">
!<Tip> Above reflector density. </Tip>
! Default = 1
! Allowed = real (aray)
!  Reflector eventes are added if OPT_REF = YES
!
! SPIKE assumes that each reflector depth, DEPTH ( X, Y )
! at any X, Y location is defined by
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF ) * X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in Hdr_x and Hdr_y units.
!</Help>
!
!<Help KEYWORD="VP1_REF">
!<Tip> Above reflector P velocity. </Tip>
! Default = 1
! Allowed = real (aray)
!  Reflector eventes are added if OPT_REF = YES
!
! SPIKE assumes that each reflector depth, DEPTH ( X, Y )
! at any X, Y location is defined by
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF ) * X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in Hdr_x and Hdr_y units.
!</Help>
!
!<Help KEYWORD="VS1_REF">
!<Tip> Above reflector S velocity. </Tip>
! Default = 1
! Allowed = real (aray)
!  Reflector eventes are added if OPT_REF = YES
!
! SPIKE assumes that each reflector depth, DEPTH ( X, Y )
! at any X, Y location is defined by
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF ) * X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in Hdr_x and Hdr_y units.
!</Help>
!
!<Help KEYWORD="DN2_REF">
!<Tip> Above reflector density. </Tip>
! Default = 1
! Allowed = real (aray)
!  Reflector eventes are added if OPT_REF = YES
!
! SPIKE assumes that each reflector depth, DEPTH ( X, Y )
! at any X, Y location is defined by
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF ) * X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in Hdr_x and Hdr_y units.
!</Help>
!
!<Help KEYWORD="VP2_REF">
!<Tip> Above reflector P velocity. </Tip>
! Default = 1
! Allowed = real (aray)
!  Reflector eventes are added if OPT_REF = YES
!
! SPIKE assumes that each reflector depth, DEPTH ( X, Y )
! at any X, Y location is defined by
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF ) * X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in Hdr_x and Hdr_y units.
!</Help>
!
!<Help KEYWORD="VS2_REF">
!<Tip> Above reflector S velocity. </Tip>
! Default = 1
! Allowed = real (aray)
!  Reflector eventes are added if OPT_REF = YES
!
! SPIKE assumes that each reflector depth, DEPTH ( X, Y )
! at any X, Y location is defined by
! DEPTH ( X, Y ) = DEP_REF + TAN ( X_DIP_REF ) * X + TAN ( Y_DIP_REF ) * Y
! where  X and Y are in Hdr_x and Hdr_y units.
!</Help>
!
!<Help KEYWORD="OPT_DIF">
!<Tip> Whether to add DIFFRACTION events to traces. </Tip>
! Default = NO
! Allowed = NO   Do not add DIFFRACTION events to traces. 
! Allowed = YES  Do     add DIFFRACTION events to traces. 
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="SELECT_PATH_DIF">
!<Tip> Diffractor file name. </Tip>
! Default = NONE
! Allowed = character string
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="PATH_DIF">
!<Tip> Diffractor file name. </Tip>
! Default = NONE
! Allowed = character string
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="AMP_DIF">
!<Tip> Diffractor amplitude array. </Tip>
! Default = 1
! Allowed = real (array)
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="DEP_DIF">
!<Tip> Diffractor depth array. </Tip>
! Default = 1
! Allowed = real (array)
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="X_LOC_DIF">
!<Tip> Diffractor X dip array. </Tip>
! Default = 1
! Allowed = real (array)
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="Y_LOC_DIF">
!<Tip> Diffractor Y dip array. </Tip>
! Default = 1
! Allowed = real (array)
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="DN1_DIF">
!<Tip> Above diffractor density. </Tip>
! Default = 1
! Allowed = real (aray)
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="VP1_DIF">
!<Tip> Above diffractor P velocity. </Tip>
! Default = 1
! Allowed = real (aray)
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="VS1_DIF">
!<Tip> Above diffractor S velocity. </Tip>
! Default = 1
! Allowed = real (aray)
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="DN2_DIF">
!<Tip> Above diffractor density. </Tip>
! Default = 1
! Allowed = real (aray)
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="VP2_DIF">
!<Tip> Above diffractor P velocity. </Tip>
! Default = 1
! Allowed = real (aray)
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="VS2_DIF">
!<Tip> Above diffractor S velocity. </Tip>
! Default = 1
! Allowed = real (aray)
!  Diffractor eventes are added if OPT_DIF = YES
!</Help>
!
!<Help KEYWORD="OPT_PIK">
!<Tip> Whether to add PICK events to traces. </Tip>
! Default = NO
! Allowed = NO   Do not add PICK events to traces. 
! Allowed = YES  Do     add PICK events to traces. 
!  Pick eventes are added if OPT_PIK = YES
!</Help>
!
!<Help KEYWORD="SELECT_PATH_PIK">
!<Tip> Spike event file name. </Tip>
! Default = NONE
! Allowed = character string
!  Pick eventes are added if OPT_PIK = YES
!</Help>
!
!<Help KEYWORD="PATH_PIK">
!<Tip> Spike event file name. </Tip>
! Default = NONE
! Allowed = character string
!  Pick eventes are added if OPT_PIK = YES
!</Help>
!
!<Help KEYWORD="AMP_PIK">
!<Tip> Pick amplitude array. </Tip>
! Default = 1
! Allowed = real (array)
!  Pick eventes are added if OPT_PIK = YES
!</Help>
!
!<Help KEYWORD="DEP_PIK">
!<Tip> Pick depth array. </Tip>
! Default = 1
! Allowed = real (array)
!  Pick eventes are added if OPT_PIK = YES
!</Help>
!
!<Help KEYWORD="X_LOC_PIK">
!<Tip> Pick X dip array. </Tip>
! Default = 1
! Allowed = real (array)
!  Pick eventes are added if OPT_PIK = YES
!</Help>
!
!<Help KEYWORD="Y_LOC_PIK">
!<Tip> Pick Y dip array. </Tip>
! Default = 1
! Allowed = real (array)
!  Pick eventes are added if OPT_PIK = YES
!</Help>
!
!<Help KEYWORD="OPT_COMPUTE_REF">
!<Tip> Create a reflectivity file. </Tip>
! Default = NO
! Allowed = NO   Do not create a reflectivity file. 
! Allowed = YES  Do     create a reflectivity file. 
!
! if OPT_COMPUTE_REF = YES  SPIKE will create a reflectivity file. 
! by reading a coefficient grid, v0_inp(z,x,y), in from PATH_REF_INP
! and writing the reflectivity grid, v0_out(z,x,y), out to PATH_REF_OUT
!
! Where the reflectivity grid for each x, jx_ref and y, jy_ref,  column
! is computed via the following:
!
! take the differnce between two consequtive z nodes, jz_ref, jz_ref-1
! within the input coefficient grid 
!
!         v0_out ( jz_ref,   jx_ref, jy_ref ) = &
!  .5 * ( v0_inp ( jz_ref,   jx_ref, jy_ref ) &
!       - v0_inp ( jz_ref-1, jx_ref, jy_ref ) ) &
!     / ( v0_inp ( jz_ref,   jx_ref, jy_ref ) &
!       + v0_inp ( jz_ref-1, jx_ref, jy_ref ) ) 
!
! then add the reflector, difrractor and pick events by setting
!
!         v0_out ( jz_ref,   jx_ref, jy_ref ) = &
!         v0_out ( jz_ref,   jx_ref, jy_ref ) &
!       + amp_ref or amp_dif or amp_pik
!
! where the event location is defined by the 
! reflector, diffractor and pick geometries.
!
!</Help>
!
!<Help KEYWORD="PATH_REF_INP">
!<Tip> Input velocity file path name.</Tip>
! Default = None
! Allowed = Character
! Allowed = Character
!</Help>
!
!<Help KEYWORD="SELECT_PATH_REF_INP">
!<Tip> Select output velocity file path name.</Tip>
! Default = None
! Allowed = Character
! Allowed = Character
!</Help>
!
!<Help KEYWORD="PATH_REF_OUT">
!<Tip> Output velocity file path name.</Tip>
! Default = None
! Allowed = Character
! Allowed = Character
!</Help>
!
!<Help KEYWORD="SELECT_PATH_REF_OUT">
!<Tip> Select output velocity file path name.</Tip>
! Default = None
! Allowed = Character
! Allowed = Character
!</Help>
!
!</HelpSection>
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!
!
module synref_module
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
  use modgrid_module
  use mth_module
  use named_constants_module
  use pathchoose_module
  use pathcheck_module
  use pattern_module
  use pc_module
  use pcpsx_module
  use string_module
  use timeglob_module
  use trcio_module
  use velgrid_module
  use wavelet_module
  use zoeppritz_module
  !
  implicit none
  !
  !private
  !
  public :: synref_create
  public :: synref_delete
  public :: synref_initialize
  public :: synref_get
  public :: synref_put
  public :: synref_verify
  public :: synref_set_noise_type
  public :: synref_compute_noise
  public :: synref_plane_reflection_pos
  public :: synref_event_add 
  !
  ! rcs identifier string
  !
  character(len=100),public,save :: synref_ident = &
  "$Id: synref.f90,v 1.13 2007/09/12 14:29:47 Hanson beta sps $"
  !
  integer, private, parameter :: n_yes_no = 2
  character(len=3), private,  save      :: c_yes_no ( n_yes_no ) &
  = (/'YES', 'NO '/)
  !
  integer, private, parameter :: n_tim_type = 4
  character(len=8), save      :: c_tim_type ( n_tim_type ) &
  = (/'NORMAL  ' , 'X_SLICE ' , 'Y_SLICE ' , 'VERTICAL'/)
  !
  integer,          parameter :: n_noise_type = 3
  character(len=8), save      :: c_noise_type ( n_noise_type ) &
  = (/'DIFF    ' , 'SAME    ' , 'DEAD    '/)
  !
  logical, private, parameter               :: set_random_seed = .true.
  !
  type, public :: synref_struct
    !
    !private
    !public
    !
    integer                                 :: hdr_x
    integer                                 :: hdr_y
    integer                                 :: hdr_z
    real                                    :: rx_scl
    real                                    :: ry_scl
    !
    character(len=8)                        :: tim_type
    integer                                 :: tim_interp
    !
    logical                                 :: opt_zoeppritz
    !
    logical                                 :: opt_ghost
    logical                                 :: opt_ghost_non
    logical                                 :: opt_ghost_gat
    logical                                 :: opt_ghost_tig
    real                                    :: dep_ghost
    real                                    :: rc_ghost
    integer                                 :: j0_ghost
    !
    logical                                 :: opt_decay
    real                                    :: pwr_decay
    real                                    :: cos_decay
    !
    logical                                 :: opt_noise
    real                                    :: amp_noise
    character(len=8)                        :: noise_type
    !
    logical                                 :: opt_direct
    real                                    :: amp_direct
    real                                    :: pwr_direct
    !
    logical                                 :: opt_spike
    integer                                 :: spike_tot
    real                                    :: spike_min
    real                                    :: spike_max
    real                                    :: spike_inc
    real                                    :: amp_spike
    !
    logical                                 :: opt_ref
    integer                                 :: n0_ref
    integer                                 :: namp_ref
    integer                                 :: ndep_ref
    integer                                 :: nx_dip_ref
    integer                                 :: ny_dip_ref
    real,                           pointer :: amp_ref ( : )
    real,                           pointer :: dep_ref ( : )
    real,                           pointer :: x_dip_ref ( : )
    real,                           pointer :: y_dip_ref ( : )
    integer                                 :: ndn1_ref
    integer                                 :: nvp1_ref
    integer                                 :: nvs1_ref
    integer                                 :: ndn2_ref
    integer                                 :: nvp2_ref
    integer                                 :: nvs2_ref
    real,                           pointer :: dn1_ref(:)
    real,                           pointer :: vp1_ref(:)
    real,                           pointer :: vs1_ref(:)
    real,                           pointer :: dn2_ref(:)
    real,                           pointer :: vp2_ref(:)
    real,                           pointer :: vs2_ref(:)
    !
    logical                                 :: opt_dif
    integer                                 :: n0_dif
    integer                                 :: namp_dif
    integer                                 :: ndep_dif
    integer                                 :: nx_loc_dif
    integer                                 :: ny_loc_dif
    real,                           pointer :: amp_dif ( : )
    real,                           pointer :: dep_dif ( : )
    real,                           pointer :: x_loc_dif ( : )
    real,                           pointer :: y_loc_dif ( : )
    integer                                 :: ndn1_dif
    integer                                 :: nvp1_dif
    integer                                 :: nvs1_dif
    integer                                 :: ndn2_dif
    integer                                 :: nvp2_dif
    integer                                 :: nvs2_dif
    real,                           pointer :: dn1_dif(:)
    real,                           pointer :: vp1_dif(:)
    real,                           pointer :: vs1_dif(:)
    real,                           pointer :: dn2_dif(:)
    real,                           pointer :: vp2_dif(:)
    real,                           pointer :: vs2_dif(:)
    !
    logical                                 :: opt_pik
    integer                                 :: m0_pik
    integer                                 :: n0_pik
    integer                                 :: namp_pik
    integer                                 :: ndep_pik
    integer                                 :: nx_loc_pik
    integer                                 :: ny_loc_pik
    integer                                 :: nx_off_pik
    integer                                 :: ny_off_pik
    real,                           pointer :: amp_pik ( : )
    real,                           pointer :: dep_pik ( : )
    real,                           pointer :: x_loc_pik ( : )
    real,                           pointer :: y_loc_pik ( : )
    real,                           pointer :: x_off_pik ( : )
    real,                           pointer :: y_off_pik ( : )
    !
    character(len=filename_length)          :: path_dif
    character(len=filename_length)          :: path_pik
    type ( pathchoose_struct ),     pointer :: select_path_dif
    type ( pathchoose_struct ),     pointer :: select_path_pik
    !
    real,                           pointer :: dz_dx ( : )
    real,                           pointer :: dz_dy ( : )
    real,                           pointer :: dx_dz ( : )
    real,                           pointer :: dy_dz ( : )
    integer                                 :: i0_ref
    integer                                 :: i0_dif
    integer                                 :: i0_pik
    !
    real                                    :: rx_ref
    real                                    :: ry_ref
    real                                    :: rz_ref
    !
    real                                    :: dep_ref_0
    real                                    :: dz_dx_0
    real                                    :: dz_dy_0
    !
    real                                    :: rt_dir
    real                                    :: ra_dir
    !
    real                                    :: rt_ref
    real                                    :: rv_ref
    real                                    :: rc_ref
    real                                    :: ra_ref
    real                                    :: za_ref
    !
    real                                    :: rx_dif
    real                                    :: ry_dif
    real                                    :: rz_dif
    real                                    :: rt_dif
    real                                    :: rv_dif
    real                                    :: rc_dif
    real                                    :: ra_dif
    real                                    :: za_dif
    !
    real                                    :: rt_pik
    real                                    :: ra_pik
    !
    character(len=16)                       :: inc_flag
    character(len=16)                       :: att_flag
    real                                    :: ang_ref
    real                                    :: ang_dif
    real                                    :: ang_inc
    real                                    :: refl_p_ref
    real                                    :: refl_s_ref
    real                                    :: tran_p_ref
    real                                    :: tran_s_ref
    real                                    :: refl_p_dif
    real                                    :: refl_s_dif
    real                                    :: tran_p_dif
    real                                    :: tran_s_dif
    !
    integer                                 :: jt_out_1
    integer                                 :: jt_out_2
    integer                                 :: jt_inc
    !
    integer                                 :: nh_inp
    integer                                 :: nt_glb
    real                                    :: t0_glb
    real                                    :: t1_glb
    real                                    :: dt_glb
    !
    integer                                 :: jt_fin
    real                                    :: rt_fin
    integer                                 :: nt_fin
    real                                    :: t0_fin
    real                                    :: t1_fin
    real                                    :: dt_fin
    !
    integer                                 :: nt_crs
    real                                    :: t0_crs
    real                                    :: t1_crs
    real                                    :: dt_crs
    !
    real                                    :: tr_max
    real                                    :: tr_max_0
    !
    real,                           pointer :: tr_spike ( : ) ! spike trace
    real,                           pointer :: tr_rflct ( : ) ! refl  trace
    real,                           pointer :: tr_noise ( : ) ! noise trace
    real,                           pointer :: tr_scale ( : ) ! scale trace
    real,                           pointer :: tr_fin ( : )
    !
    integer                                 :: ipn 
    !
    character(len=32)                       :: c_title 
    !
    type ( grid_struct )                    :: grid_obj       ! trans grid
    !
    logical                                 :: opt_compute_ref
    !
    character(len=filename_length)          :: path_ref_inp   ! inp vel file name
    type(pathchoose_struct),pointer         :: select_path_ref_inp
    !
    character(len=filename_length)          :: path_ref_out   ! out vel file name
    type(pathchoose_struct),pointer         :: select_path_ref_out
    !
    type ( modgrid_struct ),   pointer :: mg_inp    ! inp modgrid structure
    type ( modgrid_struct ),   pointer :: mg_out    ! out modgrid structure
    type ( trcio_struct ),     pointer :: tr_obj    ! out trcio   structure
    character(len=4 )                  :: mg_out_xyz
    character(len=4 )                  :: mg_inp_xyz
    integer                            :: mg_max_mem
    character(len=12)                  :: mg_type
    character(len=96)                  :: mg_file
    integer                            :: mg_rank
    character(len=32)                  :: mg_l1, mg_l2, mg_l3
    integer                            :: mg_h1, mg_h2, mg_h3
    integer                            :: mg_n1, mg_n2, mg_n3
    real                               :: mg_o1, mg_o2, mg_o3
    real                               :: mg_d1, mg_d2, mg_d3
    integer                            :: mg_hdr_x !scanning header
    integer                            :: mg_hdr_y !scanning header
    character(len=8)                   :: mg_ref_type
    !
    character(len=32)                  :: lx_ref
    character(len=32)                  :: ly_ref
    character(len=32)                  :: lz_ref
    integer                            :: nx_ref
    integer                            :: ny_ref
    integer                            :: nz_ref
    real                               :: x0_ref
    real                               :: y0_ref
    real                               :: z0_ref
    real                               :: x1_ref
    real                               :: y1_ref
    real                               :: z1_ref
    real                               :: dx_ref
    real                               :: dy_ref
    real                               :: dz_ref
    character(len=8)                   :: parm_coef 
    !
    integer                                 :: m0_sav    ! events max
    integer                                 :: n0_sav    ! events num
    real,                           pointer :: rt_sav(:) ! events tim
    real,                           pointer :: ra_sav(:) ! events amp
    logical                                 :: la_scl 
    real                                    :: ra_scl 
    !
  end type synref_struct
  !
  contains
  !
  subroutine synref_create ( r, c_title, i_err )
    !
    ! Create a synref structure
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    character(len=*),         intent(in   ) :: c_title 
    integer,                  intent(inout) :: i_err
    !
    i_err = 0
    !
    ! allocate the structure
    !
    allocate ( r )
    !
    ! initialize coefficients
    !
    call synref_initialize ( r )
    !
    r%c_title = c_title
    !
    return
    !
  end subroutine synref_create
  !
  subroutine synref_delete ( r )
    !
    ! delete memory
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    !
    call synref_del ( r )
    !
    if ( associated (        r%select_path_ref_inp ) )  &
    call pathchoose_delete ( r%select_path_ref_inp )
    !
    if ( associated (        r%select_path_ref_out ) )  &
    call pathchoose_delete ( r%select_path_ref_out )
    !
    if ( associated        ( r%select_path_dif ) ) &
    call pathchoose_delete ( r%select_path_dif )
    !
    if ( associated        ( r%select_path_pik ) ) &
    call pathchoose_delete ( r%select_path_pik )
    !
    deallocate ( r )
    !
    return
    !
  end subroutine synref_delete
  !
  subroutine synref_initialize ( r )
    !
    ! Arguments
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    !
    call synref_nul ( r )
    !
    ! initialize parameters
    !
    call synref_initialize_0 ( r )
    !
    ! get the current globals
    !
    call timeglob_get ( r%nt_glb, r%t0_glb, r%t1_glb, r%dt_glb )
    !
    r%opt_compute_ref = .false.
    r%path_ref_inp = pathcheck_empty ! inp velocity file
    r%path_ref_out = pathcheck_empty ! out velocity file
    !
    r%jt_out_1      = 1
    r%jt_out_2      = r%nt_glb
    r%jt_inc        = 100
    !
    r%path_dif  = pathcheck_empty ! velocity file
    !
    r%path_pik  = pathcheck_empty ! velocity file
    !
    r%j0_ghost = 0
    r%opt_ghost_non = .true.
    r%opt_ghost_gat = .false.
    r%opt_ghost_tig = .false.
    r%dep_ghost     = 0.
    r%rc_ghost      = -1.
    !
    r%opt_zoeppritz = .false.
    r%opt_decay     = .false.
    r%opt_spike     = .true.
    r%opt_noise     = .false.
    r%opt_ref       = .false.
    r%opt_dif       = .false.
    r%opt_pik       = .false.
    r%opt_direct    = .false.
    r%tim_type      = 'NORMAL'
    !
    r%amp_spike     = 1.
    r%spike_tot     = ( r%jt_out_2 - r%jt_out_1 ) / r%jt_inc + 1
    r%spike_min     = ( r%jt_out_1 - 1 ) * r%dt_glb + r%t0_glb
    r%spike_inc     = r%dt_glb * r%jt_inc
    !
    xxif_spike_min : if ( abs ( r%spike_min ) .le. .001 ) then
      !
      r%spike_tot = max ( 1, r%spike_tot - 1 )
      r%spike_min = r%spike_min + r%spike_inc
      !
    end if xxif_spike_min
    !
    r%spike_max       = ( r%spike_tot-1 ) *r%spike_inc+r%spike_min
    !
    r%n0_ref = 0
    r%n0_dif = 0
    r%n0_pik = 0
    !
    r%inc_flag = 'p'
    !
    r%att_flag = 'a'
    !
    r%noise_type    = 'DIFF'
    r%tim_interp    = 4
    !
    r%amp_noise     = .1
    !
    r%amp_direct    = 0.
    r%pwr_direct    = 0.
    !
    r%pwr_decay     = 0.
    r%cos_decay     = 0.
    !
    r%m0_sav = 0   ! events max
    r%n0_sav = 0   ! events num
    r%la_scl = .false.
    r%ra_scl = 1.   
    !
    call pathchoose_create ( r%select_path_dif, 'path_dif', '*' )
    call pathchoose_create ( r%select_path_pik, 'path_pik', '*' )
    call pathchoose_create ( r%select_path_ref_inp, 'path_ref_inp', '*' )
    call pathchoose_create ( r%select_path_ref_out, 'path_ref_out', '*' )
    !
    return
    !
  end subroutine synref_initialize
  !
  subroutine synref_nul ( r )
    !
    ! nullify structure pointer
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    !
    ! Begin synref_initialize
    !
    call memfun_nul ( r%amp_ref )
    call memfun_nul ( r%dep_ref )
    call memfun_nul ( r%x_dip_ref )
    call memfun_nul ( r%y_dip_ref )
    call memfun_nul ( r%dn1_ref )
    call memfun_nul ( r%vp1_ref )
    call memfun_nul ( r%vs1_ref )
    call memfun_nul ( r%dn2_ref )
    call memfun_nul ( r%vp2_ref )
    call memfun_nul ( r%vs2_ref )
    call memfun_nul ( r%amp_dif )
    call memfun_nul ( r%dep_dif )
    call memfun_nul ( r%x_loc_dif )
    call memfun_nul ( r%y_loc_dif )
    call memfun_nul ( r%dn1_dif )
    call memfun_nul ( r%vp1_dif )
    call memfun_nul ( r%vs1_dif )
    call memfun_nul ( r%dn2_dif )
    call memfun_nul ( r%vp2_dif )
    call memfun_nul ( r%vs2_dif )
    call memfun_nul ( r%amp_pik )
    call memfun_nul ( r%dep_pik )
    call memfun_nul ( r%x_loc_pik )
    call memfun_nul ( r%y_loc_pik )
    call memfun_nul ( r%x_off_pik )
    call memfun_nul ( r%y_off_pik )
    nullify ( r%select_path_dif )
    nullify ( r%select_path_pik )
    call memfun_nul ( r%dz_dx )
    call memfun_nul ( r%dz_dy )
    call memfun_nul ( r%dx_dz )
    call memfun_nul ( r%dy_dz )
    call memfun_nul ( r%tr_spike )
    call memfun_nul ( r%tr_rflct )
    call memfun_nul ( r%tr_noise )
    call memfun_nul ( r%tr_scale )
    call memfun_nul ( r%tr_fin )
    nullify (r%select_path_ref_inp) ! inp vel file name
    nullify (r%select_path_ref_out) ! out vel file name
    nullify ( r%mg_inp )
    nullify ( r%mg_out )
    nullify ( r%tr_obj )
    call memfun_nul ( r%rt_sav )
    call memfun_nul ( r%ra_sav )
    !
    return
    !
  end subroutine synref_nul 
  !
  subroutine synref_del ( r )
    !
    ! delete structure pointer
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    !
    call memfun_del ( r%amp_ref )
    call memfun_del ( r%dep_ref )
    call memfun_del ( r%x_dip_ref )
    call memfun_del ( r%y_dip_ref )
    call memfun_del ( r%dn1_ref )
    call memfun_del ( r%vp1_ref )
    call memfun_del ( r%vs1_ref )
    call memfun_del ( r%dn2_ref )
    call memfun_del ( r%vp2_ref )
    call memfun_del ( r%vs2_ref )
    call memfun_del ( r%amp_dif )
    call memfun_del ( r%dep_dif )
    call memfun_del ( r%x_loc_dif )
    call memfun_del ( r%y_loc_dif )
    call memfun_del ( r%dn1_dif )
    call memfun_del ( r%vp1_dif )
    call memfun_del ( r%vs1_dif )
    call memfun_del ( r%dn2_dif )
    call memfun_del ( r%vp2_dif )
    call memfun_del ( r%vs2_dif )
    call memfun_del ( r%amp_pik )
    call memfun_del ( r%dep_pik )
    call memfun_del ( r%x_loc_pik )
    call memfun_del ( r%y_loc_pik )
    call memfun_del ( r%x_off_pik )
    call memfun_del ( r%y_off_pik )
    call memfun_del ( r%dz_dx )
    call memfun_del ( r%dz_dy )
    call memfun_del ( r%dx_dz )
    call memfun_del ( r%dy_dz )
    call memfun_del ( r%tr_spike )
    call memfun_del ( r%tr_rflct )
    call memfun_del ( r%tr_noise )
    call memfun_del ( r%tr_scale )
    call memfun_del ( r%tr_fin )
    call memfun_del ( r%rt_sav )
    call memfun_del ( r%ra_sav )
    return
    !
  end subroutine synref_del 
  !
  subroutine synref_all ( r, i_err ) 
    !
    ! allocate structure pointer
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    integer,                  intent(inout) :: i_err
    !
    i_err = 0
    !
    r%m0_sav = ( r%n0_ref + r%n0_dif + r%n0_pik ) * 4
    r%n0_sav = 0   ! events num
    !
    call memfun_all ( r%dx_dz,    r%n0_ref, 'dx_dz',    i_err )
    call memfun_all ( r%dy_dz,    r%n0_ref, 'dy_dz',    i_err )
    call memfun_all ( r%dz_dx,    r%n0_ref, 'dz_dx',    i_err )
    call memfun_all ( r%dz_dy,    r%n0_ref, 'dz_dy',    i_err )
    call memfun_all ( r%tr_fin,   r%nt_fin, 'tr_fin',   i_err )
    call memfun_all ( r%tr_spike, r%nt_fin, 'tr_spike', i_err )
    call memfun_all ( r%tr_rflct, r%nt_fin, 'tr_rflct', i_err )
    call memfun_all ( r%tr_noise, r%nt_fin, 'tr_noise', i_err )
    call memfun_all ( r%tr_scale, r%nt_fin, 'tr_scale', i_err )
    call memfun_all ( r%rt_sav, r%m0_sav , 'rt_sav',  i_err )
    call memfun_all ( r%ra_sav, r%m0_sav , 'ra_sav',  i_err )
    !
    return
    !
  end subroutine synref_all 
  !
  subroutine synref_initialize_0 ( r )
    !
    ! initialze structure values
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    !
    call memfun_init ( r%path_ref_inp ) ! inp velocity file
    call memfun_init ( r%path_ref_out ) ! out velocity file
    call memfun_init ( r%tim_interp )
    call memfun_init ( r%amp_direct )
    call memfun_init ( r%pwr_direct )
    call memfun_init ( r%pwr_decay )
    call memfun_init ( r%cos_decay )
    call memfun_init ( r%amp_noise )
    call memfun_init ( r%noise_type )
    call memfun_init ( r%jt_fin )
    call memfun_init ( r%rt_fin )
    call memfun_init ( r%nt_fin )
    call memfun_init ( r%t0_fin )
    call memfun_init ( r%t1_fin )
    call memfun_init ( r%dt_fin )
    call memfun_init ( r%tr_fin )
    call memfun_init ( r%nt_crs )
    call memfun_init ( r%t0_crs )
    call memfun_init ( r%t1_crs )
    call memfun_init ( r%dt_crs )
    !
    call memfun_init ( r%n0_ref )
    call memfun_init ( r%namp_ref )
    call memfun_init ( r%ndep_ref )
    call memfun_init ( r%nx_dip_ref )
    call memfun_init ( r%ny_dip_ref )
    call memfun_init ( r%amp_ref )
    call memfun_init ( r%dep_ref )
    call memfun_init ( r%x_dip_ref )
    call memfun_init ( r%y_dip_ref )
    call memfun_init ( r%n0_dif )
    call memfun_init ( r%namp_dif )
    call memfun_init ( r%ndep_dif )
    call memfun_init ( r%nx_loc_dif )
    call memfun_init ( r%ny_loc_dif )
    call memfun_init ( r%amp_dif )
    call memfun_init ( r%dep_dif )
    call memfun_init ( r%x_loc_dif )
    call memfun_init ( r%y_loc_dif )
    call memfun_init ( r%m0_pik )
    call memfun_init ( r%n0_pik )
    call memfun_init ( r%namp_pik )
    call memfun_init ( r%ndep_pik )
    call memfun_init ( r%nx_loc_pik )
    call memfun_init ( r%ny_loc_pik )
    call memfun_init ( r%nx_off_pik )
    call memfun_init ( r%ny_off_pik )
    call memfun_init ( r%amp_pik )
    call memfun_init ( r%dep_pik )
    call memfun_init ( r%x_loc_pik )
    call memfun_init ( r%y_loc_pik )
    call memfun_init ( r%x_off_pik )
    call memfun_init ( r%y_off_pik )
    call memfun_init ( r%path_dif )
    call memfun_init ( r%path_pik )
    call memfun_init ( r%dz_dx )
    call memfun_init ( r%dz_dy )
    call memfun_init ( r%dx_dz )
    call memfun_init ( r%dy_dz )
    call memfun_init ( r%nh_inp )
    call memfun_init ( r%nt_glb )
    call memfun_init ( r%t0_glb )
    call memfun_init ( r%t1_glb )
    call memfun_init ( r%dt_glb )
    call memfun_init ( r%jt_out_1 )
    call memfun_init ( r%jt_out_2 )
    call memfun_init ( r%jt_inc )
    call memfun_init ( r%inc_flag )
    call memfun_init ( r%att_flag )
    call memfun_init ( r%ang_ref )
    call memfun_init ( r%ang_dif )
    call memfun_init ( r%ang_inc )
    call memfun_init ( r%refl_p_ref )
    call memfun_init ( r%refl_s_ref )
    call memfun_init ( r%tran_p_ref )
    call memfun_init ( r%tran_s_ref )
    call memfun_init ( r%refl_p_dif )
    call memfun_init ( r%refl_s_dif )
    call memfun_init ( r%tran_p_dif )
    call memfun_init ( r%tran_s_dif )
    !
    call memfun_init ( r%tr_max )
    call memfun_init ( r%tr_max_0 )
    call memfun_init ( r%tr_spike )
    call memfun_init ( r%tr_rflct )
    call memfun_init ( r%tr_noise )
    call memfun_init ( r%tr_scale )
    call memfun_init ( r%tr_fin )
    call memfun_init ( r%la_scl )
    call memfun_init ( r%ra_scl )
    !
    return
    !
  end subroutine synref_initialize_0
  !
  subroutine synref_get ( r )
    !
    ! get synref parameters
    !
    ! Arguments
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    !
    !integer, save :: i_call=0
    !i_call = i_call+1
    ! get the rotation grid object
    !
    r%ipn = pc_get_ipn()
    !
    call pc_get_global ( 'grid' , r%grid_obj )
    !
    call timeglob_get ( r%nt_glb, r%t0_glb, r%t1_glb, r%dt_glb )
    !
    call pc_get_global ( 'nwih',  r%nh_inp )
    !
    if ( pathchoose_update ( r%select_path_ref_inp,   r%path_ref_inp   ) ) return
    if ( pathchoose_update ( r%select_path_ref_out,   r%path_ref_out   ) ) return
    !
    if ( pathchoose_update ( r%select_path_dif,   r%path_dif   )  ) return
    if ( pathchoose_update ( r%select_path_pik,   r%path_pik   )  ) return
    !
    call synref_register_array_names 
    !
    call pc_get ( 'tim_type',       r%tim_type         )
    call pc_get ( 'tim_interp',     r%tim_interp       )
    call pc_get ( 'opt_zoeppritz',  r%opt_zoeppritz    )
    !
    call pc_get ( 'opt_ghost_non',  r%opt_ghost_non    )
    call pc_get ( 'opt_ghost_gat',  r%opt_ghost_gat    )
    call pc_get ( 'opt_ghost_tig',  r%opt_ghost_tig    )
    call pc_get ( 'dep_ghost',      r%dep_ghost        )
    call pc_get ( 'rc_ghost',       r%rc_ghost         )
    !
    call pc_get ( 'opt_decay',      r%opt_decay        )
    call pc_get ( 'pwr_decay',      r%pwr_decay        )
    call pc_get ( 'cos_decay',      r%cos_decay        )
    !
    call pc_get ( 'opt_noise',      r%opt_noise        )
    call pc_get ( 'noise_type',     r%noise_type       )
    call pc_get ( 'amp_noise',      r%amp_noise        )
    !
    call pc_get ( 'opt_direct',     r%opt_direct       )
    call pc_get ( 'amp_direct',     r%amp_direct       )
    call pc_get ( 'pwr_direct',     r%pwr_direct       )
    !
    call pc_get ( 'path_dif',       r%path_dif         )
    call pc_get ( 'path_pik',       r%path_pik         )
    !
    call pc_get ( 'opt_spike',      r%opt_spike        )
    call pc_get ( 'amp_spike',      r%amp_spike        )
    call pc_get ( 'spike_tot',      r%spike_tot        )
    call pc_get ( 'spike_min',      r%spike_min        )
    call pc_get ( 'spike_max',      r%spike_max        )
    call pc_get ( 'spike_inc',      r%spike_inc        )
    !
    r%namp_ref   = r%n0_ref
    r%ndep_ref   = r%n0_ref
    r%nx_dip_ref = r%n0_ref
    r%ny_dip_ref = r%n0_ref
    r%ndn1_ref   = r%n0_ref
    r%nvp1_ref   = r%n0_ref
    r%nvs1_ref   = r%n0_ref
    r%ndn2_ref   = r%n0_ref
    r%nvp2_ref   = r%n0_ref
    r%nvs2_ref   = r%n0_ref
    !
    call pc_get ( 'opt_ref',        r%opt_ref          )
    call pc_alloc ( 'amp_ref',   r%amp_ref,   r%namp_ref   )
    call pc_alloc ( 'dep_ref',   r%dep_ref,   r%ndep_ref   )
    call pc_alloc ( 'x_dip_ref', r%x_dip_ref, r%nx_dip_ref )
    call pc_alloc ( 'y_dip_ref', r%y_dip_ref, r%ny_dip_ref )

    call pc_alloc ( 'dn1_ref',   r%dn1_ref,   r%ndn1_ref   )
    call pc_alloc ( 'vp1_ref',   r%vp1_ref,   r%nvp1_ref   )
    call pc_alloc ( 'vs1_ref',   r%vs1_ref,   r%nvs1_ref   )
    call pc_alloc ( 'dn2_ref',   r%dn2_ref,   r%ndn2_ref   )
    call pc_alloc ( 'vp2_ref',   r%vp2_ref,   r%nvp2_ref   )
    call pc_alloc ( 'vs2_ref',   r%vs2_ref,   r%nvs2_ref   )
    r%n0_ref     = r%namp_ref
    !
    r%namp_dif   = r%n0_dif
    r%ndep_dif   = r%n0_dif
    r%nx_loc_dif = r%n0_dif
    r%ny_loc_dif = r%n0_dif
    r%ndn1_dif   = r%n0_dif
    r%nvp1_dif   = r%n0_dif
    r%nvs1_dif   = r%n0_dif
    r%ndn2_dif   = r%n0_dif
    r%nvp2_dif   = r%n0_dif
    r%nvs2_dif   = r%n0_dif
    !
    !
    call pc_get ( 'opt_dif',        r%opt_dif          )
    call pc_alloc ( 'amp_dif',   r%amp_dif,   r%namp_dif   )
    call pc_alloc ( 'dep_dif',   r%dep_dif,   r%ndep_dif   )
    call pc_alloc ( 'x_loc_dif', r%x_loc_dif, r%nx_loc_dif )
    call pc_alloc ( 'y_loc_dif', r%y_loc_dif, r%ny_loc_dif )
    call pc_alloc ( 'dn1_dif',   r%dn1_dif,   r%ndn1_dif   )
    call pc_alloc ( 'vp1_dif',   r%vp1_dif,   r%nvp1_dif   )
    call pc_alloc ( 'vs1_dif',   r%vs1_dif,   r%nvs1_dif   )
    call pc_alloc ( 'dn2_dif',   r%dn2_dif,   r%ndn2_dif   )
    call pc_alloc ( 'vp2_dif',   r%vp2_dif,   r%nvp2_dif   )
    call pc_alloc ( 'vs2_dif',   r%vs2_dif,   r%nvs2_dif   )
    r%n0_dif     = r%namp_dif
    !
    r%namp_pik   = r%n0_pik
    r%ndep_pik   = r%n0_pik
    r%nx_loc_pik = r%n0_pik
    r%ny_loc_pik = r%n0_pik
    r%nx_off_pik = r%n0_pik
    r%ny_off_pik = r%n0_pik
    !
    call pc_get ( 'opt_pik',        r%opt_pik          )
    call pc_alloc ( 'amp_pik',   r%amp_pik,   r%namp_pik   )
    call pc_alloc ( 'dep_pik',   r%dep_pik,   r%ndep_pik   )
    call pc_alloc ( 'x_loc_pik', r%x_loc_pik, r%nx_loc_pik )
    call pc_alloc ( 'y_loc_pik', r%y_loc_pik, r%ny_loc_pik )
    call pc_alloc ( 'x_off_pik', r%x_off_pik, r%nx_off_pik )
    call pc_alloc ( 'y_off_pik', r%y_off_pik, r%ny_off_pik )
    !
    r%n0_pik     = r%namp_pik
    !
    call pc_get ( 'opt_create_ref',       r%opt_compute_ref        )
    call pc_get ( 'opt_compute_ref',      r%opt_compute_ref        )
    call pc_get ( 'path_inp',     r%path_ref_inp     )
    call pc_get ( 'path_out',     r%path_ref_out     )
    call pc_get ( 'path_ref_inp',     r%path_ref_inp     )
    call pc_get ( 'path_ref_out',     r%path_ref_out     )
    !
    return
    !
  end subroutine synref_get
  !
  subroutine synref_put ( r )
    !
    ! put synref parameters
    !
    ! Arguments
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    !
    call amod_line_feed ( 'synref_zoeppritz' )
    call pc_put ( 'opt_zoeppritz',  r%opt_zoeppritz    )
    call pc_put ( 'tim_type',       r%tim_type         )
    call pc_put ( 'tim_interp',     r%tim_interp       )
    !
    call amod_line_feed ( 'synref_ghost' )
    call pc_put ( 'opt_ghost_non',  r%opt_ghost_non    )
    call pc_put ( 'opt_ghost_gat',  r%opt_ghost_gat    )
    call pc_put ( 'opt_ghost_tig',  r%opt_ghost_tig    )
    call pc_put ( 'dep_ghost',      r%dep_ghost        )
    call pc_put ( 'rc_ghost',       r%rc_ghost         )
    !
    call amod_line_feed ( 'synref_decay' )
    call pc_put ( 'opt_decay',      r%opt_decay        )
    call pc_put ( 'pwr_decay',      r%pwr_decay        )
    call pc_put ( 'cos_decay',      r%cos_decay        )
    !
    call amod_line_feed ( 'synref_noise' )
    !
    call pc_put ( 'opt_noise',      r%opt_noise        )
    call pc_put ( 'noise_type',     r%noise_type       )
    call pc_put ( 'amp_noise',      r%amp_noise        )
    !
    call amod_line_feed ( 'synref_direct' )
    !
    call pc_put ( 'opt_direct',     r%opt_direct       )
    call pc_put ( 'amp_direct',     r%amp_direct       )
    call pc_put ( 'pwr_direct',     r%pwr_direct       )
    !
    call amod_line_feed ( 'synref_spikes' )
    !
    call pc_put ( 'opt_spike',      r%opt_spike        )
    call pc_put ( 'amp_spike',      r%amp_spike        )
    call pc_put ( 'spike_tot',      r%spike_tot        )
    call pc_put ( 'spike_min',      r%spike_min        )
    call pc_put ( 'spike_max',      r%spike_max        )
    call pc_put ( 'spike_inc',      r%spike_inc        )
    !
    r%namp_ref   = min(r%n0_ref,size(r%amp_ref))
    r%ndep_ref   = min(r%n0_ref,size(r%dep_ref))
    r%nx_dip_ref = min(r%n0_ref,size(r%x_dip_ref))
    r%ny_dip_ref = min(r%n0_ref,size(r%y_dip_ref))
    r%ndn1_ref   = min(r%n0_ref,size(r%dn1_ref))
    r%nvp1_ref   = min(r%n0_ref,size(r%vp1_ref))
    r%nvs1_ref   = min(r%n0_ref,size(r%vs1_ref))
    r%ndn2_ref   = min(r%n0_ref,size(r%dn2_ref))
    r%nvp2_ref   = min(r%n0_ref,size(r%vp2_ref))
    r%nvs2_ref   = min(r%n0_ref,size(r%vs2_ref))

    call amod_line_feed ( 'synref_ref' )
    !
    call pc_put ( 'opt_ref',        r%opt_ref          )
    call pc_put ( 'amp_ref',   r%amp_ref,   r%namp_ref   )
    call pc_put ( 'dep_ref',   r%dep_ref,   r%ndep_ref   )
    call pc_put ( 'x_dip_ref', r%x_dip_ref, r%nx_dip_ref )
    call pc_put ( 'y_dip_ref', r%y_dip_ref, r%ny_dip_ref )
    call pc_put ( 'dn1_ref',   r%dn1_ref,   r%ndn1_ref   )
    call pc_put ( 'vp1_ref',   r%vp1_ref,   r%nvp1_ref   )
    call pc_put ( 'vs1_ref',   r%vs1_ref,   r%nvs1_ref   )
    call pc_put ( 'dn2_ref',   r%dn2_ref,   r%ndn2_ref   )
    call pc_put ( 'vp2_ref',   r%vp2_ref,   r%nvp2_ref   )
    call pc_put ( 'vs2_ref',   r%vs2_ref,   r%nvs2_ref   )
    !
    r%namp_dif   = min(r%n0_dif,size(r%amp_dif))
    r%ndep_dif   = min(r%n0_dif,size(r%dep_dif))
    r%nx_loc_dif = min(r%n0_dif,size(r%x_loc_dif))
    r%ny_loc_dif = min(r%n0_dif,size(r%y_loc_dif))
    r%ndn1_dif   = min(r%n0_dif,size(r%dn1_dif))
    r%nvp1_dif   = min(r%n0_dif,size(r%vp1_dif))
    r%nvs1_dif   = min(r%n0_dif,size(r%vs1_dif))
    r%ndn2_dif   = min(r%n0_dif,size(r%dn2_dif))
    r%nvp2_dif   = min(r%n0_dif,size(r%vp2_dif))
    r%nvs2_dif   = min(r%n0_dif,size(r%vs2_dif))
    !
    call amod_line_feed ( 'synref_dif' )
    call pc_put ( 'opt_dif',        r%opt_dif          )
    call pc_put ( 'path_dif',       r%path_dif         )
    call pc_put ( 'amp_dif',   r%amp_dif,   r%namp_dif   )
    call pc_put ( 'dep_dif',   r%dep_dif,   r%ndep_dif   )
    call pc_put ( 'x_loc_dif', r%x_loc_dif, r%nx_loc_dif )
    call pc_put ( 'y_loc_dif', r%y_loc_dif, r%ny_loc_dif )
    call pc_put ( 'dn1_dif',   r%dn1_dif,   r%ndn1_dif   )
    call pc_put ( 'vp1_dif',   r%vp1_dif,   r%nvp1_dif   )
    call pc_put ( 'vs1_dif',   r%vs1_dif,   r%nvs1_dif   )
    call pc_put ( 'dn2_dif',   r%dn2_dif,   r%ndn2_dif   )
    call pc_put ( 'vp2_dif',   r%vp2_dif,   r%nvp2_dif   )
    call pc_put ( 'vs2_dif',   r%vs2_dif,   r%nvs2_dif   )
    !
    r%namp_pik   = r%n0_pik
    r%ndep_pik   = r%n0_pik
    r%nx_loc_pik = r%n0_pik
    r%ny_loc_pik = r%n0_pik
    r%nx_off_pik = r%n0_pik
    r%ny_off_pik = r%n0_pik
    !
    call amod_line_feed ( 'synref_pik' )
    call pc_put ( 'opt_pik',        r%opt_pik          )
    call pc_put ( 'path_pik',       r%path_pik         )
    call pc_put ( 'amp_pik',   r%amp_pik,   r%namp_pik   )
    call pc_put ( 'dep_pik',   r%dep_pik,   r%ndep_pik   )
    call pc_put ( 'x_loc_pik', r%x_loc_pik, r%nx_loc_pik )
    call pc_put ( 'y_loc_pik', r%y_loc_pik, r%ny_loc_pik )
    call pc_put ( 'x_off_pik', r%x_off_pik, r%nx_off_pik )
    call pc_put ( 'y_off_pik', r%y_off_pik, r%ny_off_pik )
    !
    call amod_line_feed ( 'synref_reflectivity' )
    call pc_put ( 'opt_compute_ref',      r%opt_compute_ref        )
    call pc_put ( 'path_ref_inp',     r%path_ref_inp     )
    call pc_put ( 'path_ref_out',     r%path_ref_out     )
    !
    return
    !
  end subroutine synref_put
  !
  subroutine synref_verify ( r )
    !
    ! verify synref parameters
    !
    ! Arguments
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    !
    integer                                 :: i_err
    integer                                 :: i_stat
    !
    i_err = 0
    !
    call pc_put_options_field ( 'opt_compute_ref', c_yes_no, n_yes_no )
    !
    call pc_put_options_field ( 'tim_type', c_tim_type, n_tim_type )
    !
    call pc_put_options_field ( 'opt_zoeppritz', c_yes_no, n_yes_no )
    !
    call pc_put_options_field ( 'opt_ghost_non', c_yes_no, n_yes_no )
    call pc_put_options_field ( 'opt_ghost_gat', c_yes_no, n_yes_no )
    call pc_put_options_field ( 'opt_ghost_tig', c_yes_no, n_yes_no )
    !
    call pc_put_options_field ( 'opt_decay', c_yes_no, n_yes_no )
    !
    call pc_put_options_field ( 'opt_spike', c_yes_no, n_yes_no )
    !
    call pc_put_options_field ( 'opt_noise', c_yes_no, n_yes_no )
    !
    call pc_put_options_field ( 'noise_type', c_noise_type, n_noise_type )
    !
    call pc_put_options_field ( 'opt_ref', c_yes_no, n_yes_no )
    !
    call pc_put_options_field ( 'opt_dif', c_yes_no, n_yes_no )
    !
    call pc_put_options_field ( 'opt_pik', c_yes_no, n_yes_no )
    !
    call pc_put_options_field ( 'opt_direct', c_yes_no, n_yes_no )
    !
    ! set the noise_type to allowed values
    !
    call synref_set_noise_type ( r%noise_type )
    !
    if ( r%spike_inc .eq. 0.0 ) &
    call pc_warning ( 'spike : spike_inc must not be 0 ' )
    !
    if ( r%spike_tot > 0 ) &
    i_stat = pattern_stop2 ( 'spike:', .true., &
     r%spike_min, r%spike_inc, r%spike_max, r%spike_tot, &
     'spike_min', 'spike_inc', 'spike_max', 'spike_tot', &
     pc_verify_scalar ( 'spike_min' ), pc_verify_scalar ( 'spike_inc' ), &
     pc_verify_scalar ( 'spike_max' ), pc_verify_scalar ( 'spike_tot' )  )
    !
    ! set the tim_type to allowed values
    !
    call synref_set_timtype ( r%tim_type )
    !
    xxif_spike_tot : if ( r%spike_tot .lt. 0 ) then
      !
      r%spike_min   = ( r%jt_out_1 - 1 ) * r%dt_glb + r%t0_glb
      !
      r%jt_out_1 = nint (  ( r%spike_min-r%t0_glb ) /r%spike_inc ) + 1
      !
      r%jt_out_2 = r%nt_glb
      !
      r%jt_inc   = nint ( r%spike_inc / r%dt_glb )
      !
      r%spike_tot   = ( r%jt_out_2 - r%jt_out_1 ) / r%jt_inc + 1
      !
    end if xxif_spike_tot
    !
    ! the time interpoaltion factor should be 1 if there is no filtering or
    ! wavelet to spread the spikes out
    !
    r%tim_interp = max ( 1, r%tim_interp )
    !
    !if ( string_upper_compare ( r%wlt%wavelet_type, 'SPIKE' ) &
    !.and. string_upper_compare ( r%wlt%filter_level, 'NONE' )  ) &
    !r%tim_interp = 1
    !
    !  determine the computational time grid definition
    !
    r%nt_fin = ( r%nt_glb - 1 ) * r%tim_interp + 1
    !
    r%dt_fin = r%dt_glb / r%tim_interp
    !
    r%t0_fin = r%t0_glb
    !
    r%t1_fin = ( r%nt_fin - 1 ) * r%dt_fin + r%t0_fin
    !
    r%nt_crs = ( r%nt_fin - 1 ) / r%tim_interp + 1
    !
    r%dt_crs = r%dt_fin * r%tim_interp 
    !
    r%t0_crs = r%t0_fin 
    !
    r%t1_crs = ( r%nt_crs - 1 ) * r%dt_crs + r%t0_crs
    !
    call pathcheck ( 'path_dif',  r%path_dif,  required=.false. )
    !
    call string_replace_zeroes ( r%path_dif )
    !
    if ( len_trim ( r%path_dif ) .lt. 1 ) r%path_dif = pathcheck_empty
    !
    call pathcheck ( 'path_pik', r%path_pik, required=.false. )
    !
    call string_replace_zeroes ( r%path_pik )
    !
    if ( len_trim ( r%path_pik ) .lt. 1 ) r%path_pik = pathcheck_empty
    !
    ! make sure some event is on
    !
    if (       pc_do_not_process_traces() &
   .and. .not. r%opt_noise &
   .and. .not. r%opt_direct &
   .and. .not. r%opt_spike &
   .and. .not. r%opt_ref &
   .and. .not. r%opt_dif &
   .and. .not. r%opt_pik ) &
    call pc_warning ( ' SYNREF_EVENTS you have not turned any events on ' )
    !
    ! make sure some event is on
    !
    if ( .not. pc_do_not_process_traces() &
   .and. .not. r%opt_noise &
   .and. .not. r%opt_direct &
   .and. .not. r%opt_spike &
   .and. .not. r%opt_ref &
   .and. .not. r%opt_dif &
   .and. .not. r%opt_pik ) &
    call pc_error ( ' SYNREF_EVENTS you have not turned any events on ' )
    !
    ! set synref parameters sensitivity
    !
    call synref_sensitive ( r )
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synref_verify ", &
    & /," during xxx " &
    & )') 
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synref_verify " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine synref_verify
  !
  subroutine synref_sensitive ( r )
    !
    ! set synref parameters sensitivity
    !
    ! Arguments
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    logical                                 :: l_opt_ref  
    logical                                 :: l_opt_dif  
    !
    call pc_put_sensitive_field_flag ( 'path_ref_inp', r%opt_compute_ref )
    call pc_put_sensitive_field_flag ( 'path_ref_out', r%opt_compute_ref )
    !
    r%opt_ghost = r%opt_ghost_gat .or. r%opt_ghost_tig 
    !
    call pc_put_sensitive_field_flag ( 'dep_ghost', r%opt_ghost )
    call pc_put_sensitive_field_flag ( 'rc_ghost',  r%opt_ghost )
    !
    call pc_put_sensitive_field_flag ( 'cos_decay', r%opt_decay )
    call pc_put_sensitive_field_flag ( 'pwr_decay', r%opt_decay )
    !
    call pc_put_sensitive_field_flag ( 'amp_noise', r%opt_noise  )
    call pc_put_sensitive_field_flag ( 'noise_type', r%opt_noise  )
    !
    call pc_put_sensitive_field_flag ( 'amp_direct', r%opt_direct )
    call pc_put_sensitive_field_flag ( 'pwr_direct', r%opt_direct )
    !
    call pc_put_sensitive_field_flag ( 'amp_spike', r%opt_spike )
    call pc_put_sensitive_field_flag ( 'spike_tot', r%opt_spike )
    call pc_put_sensitive_field_flag ( 'spike_min', r%opt_spike )
    call pc_put_sensitive_field_flag ( 'spike_max', r%opt_spike )
    call pc_put_sensitive_field_flag ( 'spike_inc', r%opt_spike )
    !
    l_opt_ref  = r%opt_ref .and. r%opt_zoeppritz 
    !
    call pc_put_sensitive_field_flag ( 'amp_ref',   r%opt_ref )
    call pc_put_sensitive_field_flag ( 'dep_ref',   r%opt_ref )
    call pc_put_sensitive_field_flag ( 'x_dip_ref', r%opt_ref )
    call pc_put_sensitive_field_flag ( 'y_dip_ref', r%opt_ref )
    call pc_put_sensitive_field_flag ( 'dn1_ref',   l_opt_ref )
    call pc_put_sensitive_field_flag ( 'vp1_ref',   l_opt_ref )
    call pc_put_sensitive_field_flag ( 'vs1_ref',   l_opt_ref )
    call pc_put_sensitive_field_flag ( 'dn2_ref',   l_opt_ref )
    call pc_put_sensitive_field_flag ( 'vp2_ref',   l_opt_ref )
    call pc_put_sensitive_field_flag ( 'vs2_ref',   l_opt_ref )
    !
    l_opt_dif  = r%opt_dif .and. r%opt_zoeppritz 
    !
    call pc_put_sensitive_field_flag ( 'amp_dif',   r%opt_dif )
    call pc_put_sensitive_field_flag ( 'dep_dif',   r%opt_dif )
    call pc_put_sensitive_field_flag ( 'x_loc_dif', r%opt_dif )
    call pc_put_sensitive_field_flag ( 'y_loc_dif', r%opt_dif )
    call pc_put_sensitive_field_flag ( 'dn1_dif',   l_opt_dif )
    call pc_put_sensitive_field_flag ( 'vp1_dif',   l_opt_dif )
    call pc_put_sensitive_field_flag ( 'vs1_dif',   l_opt_dif )
    call pc_put_sensitive_field_flag ( 'dn2_dif',   l_opt_dif )
    call pc_put_sensitive_field_flag ( 'vp2_dif',   l_opt_dif )
    call pc_put_sensitive_field_flag ( 'vs2_dif',   l_opt_dif )
    !
    call pc_put_sensitive_field_flag ( 'path_pik',  r%opt_pik )
    call pc_put_sensitive_field_flag ( 'amp_pik',   r%opt_pik )
    call pc_put_sensitive_field_flag ( 'dep_pik',   r%opt_pik )
    call pc_put_sensitive_field_flag ( 'x_loc_pik', r%opt_pik )
    call pc_put_sensitive_field_flag ( 'y_loc_pik', r%opt_pik )
    call pc_put_sensitive_field_flag ( 'x_off_pik', r%opt_pik )
    call pc_put_sensitive_field_flag ( 'y_off_pik', r%opt_pik )
    !
    return
    !
  end subroutine synref_sensitive 
  !
  subroutine synref_prep ( r, hdr_x, hdr_y, rx_scl, ry_scl, i_err )
    !
    ! prep  synref parameters
    !
    ! Arguments
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    integer,                  intent(in   ) :: hdr_x  ! x header iindex
    integer,                  intent(in   ) :: hdr_y  ! y header iindex
    real,                     intent(in   ) :: rx_scl ! x scale factor
    real,                     intent(in   ) :: ry_scl ! y scale factor
    integer,                  intent(  out) :: i_err
    !
    integer                                 :: i0_ref
    !
    i_err = 0
    !
    if ( pc_do_not_process_traces()  ) return
    !
    write ( pc_get_lun(), '(  &
    & /, " synref_update " ,/ ," REVISION: ", &
    & " 13  2007-09-11  Douglas Hanson Make parallel. " &
    & )')
    !
    r%hdr_x  = hdr_x  ! x header iindex
    r%hdr_y  = hdr_y  ! y header iindex
    r%rx_scl = rx_scl ! x scale factor
    r%ry_scl = ry_scl ! y scale factor
    !
    ! get the diffractor file
    !
    call synref_read_diffractor_file ( r )
    !
    ! get the pick file
    !
    call synref_read_pick_file ( r )
    !
    ! set the amplitudes of events
    !
    if ( .not. r%opt_decay  ) r%cos_decay  = 0.
    if ( .not. r%opt_decay  ) r%pwr_decay  = 0.
    if ( .not. r%opt_spike  ) r%amp_spike  = 0.
    if ( .not. r%opt_noise  ) r%amp_noise  = 0.
    if ( .not. r%opt_direct ) r%amp_direct = 0.
    if ( .not. r%opt_direct ) r%pwr_direct = 0.
    if ( .not. r%opt_ref    ) r%amp_ref    = 0.
    if ( .not. r%opt_dif    ) r%amp_dif    = 0.
    if ( .not. r%opt_pik    ) r%amp_pik    = 0.
    !
    ! print reflector info
    !
    call synref_print_event ( &
                              ' spike reflectors ', 'path_ref', &
                              r%hdr_x, r%hdr_y, &
                              r%n0_ref, r%amp_ref, r%dep_ref, &
                              r%x_dip_ref, r%y_dip_ref &
                            )
    !
    ! print diffractor info after scaling
    !
    call synref_print_event ( &
                              ' scaled spike diffractors ', r%path_dif, &
                              r%hdr_x, r%hdr_y, &
                              r%n0_dif, r%amp_dif, r%dep_dif, &
                              r%x_loc_dif, r%y_loc_dif &
                            )
    !
    call synref_print_event ( &
                              ' scaled spike picks ', r%path_pik, &
                              r%hdr_x, r%hdr_y, &
                              r%n0_pik, r%amp_pik, r%dep_pik, &
                              r%x_loc_pik, r%x_off_pik &
                            )
    !
    call synref_all ( r, i_err ) 
    !
    if ( i_err .ne. 0 ) go to 998
    !
    do_i0_ref : do i0_ref = 1 , r%n0_ref
      !
      r%dz_dx ( i0_ref ) = tan ( r%x_dip_ref ( i0_ref ) * radians_per_degree )
      !
      r%dz_dy ( i0_ref ) = tan ( r%y_dip_ref ( i0_ref ) * radians_per_degree )
      !
      r%dx_dz ( i0_ref ) = 0.
      !
 if ( r%dz_dx ( i0_ref ) .ne. 0. ) &
      r%dx_dz ( i0_ref ) = &
 1. / r%dz_dx ( i0_ref )
      !
           r%dy_dz ( i0_ref ) = 0.
      !
 if ( r%dz_dy ( i0_ref ) .ne. 0. ) &
      r%dy_dz ( i0_ref ) = &
 1. / r%dz_dy ( i0_ref )
      !
    end do do_i0_ref
    !
    write ( pc_get_lun(), '( &
    & /, " synref_prep n0_ref=", i8, &
    & /, "   i0_ref amp_ref   dep_ref  x_dip_ref  y_dip_ref      ", &
    & "dz_dx     dz_dy      dx_dz      dy_dz" &
    & )') &
    r%n0_ref
    !
    write ( pc_get_lun(), '( &
    & 1x, i8, &
    & 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4, &
    & 1x, g10.4, 1x, g10.4, 1x, g10.4, 1x, g10.4 &
    & )') &
    (             i0_ref, &
    r%amp_ref  ( i0_ref ), r%dep_ref  ( i0_ref ), &
    r%x_dip_ref ( i0_ref ), r%y_dip_ref ( i0_ref ), &
    r%dz_dx    ( i0_ref ), r%dz_dy    ( i0_ref ), &
    r%dx_dz    ( i0_ref ), r%dy_dz    ( i0_ref ), &
                i0_ref = 1 , r%n0_ref )
    !
    ! intialize the random number generator
    !
    call mth_ranset ( 1, set_random_seed ) ! initialize random number seed.
    !
    ! compute the spike trace
    !
    call synref_compute_spike ( r )
    !
    ! compute the noise trace
    !
    call synref_compute_noise ( r%amp_noise, r%nt_fin, r%tr_noise )
    !
    ! compute the scale trace
    !
    call synref_compute_scale ( r )
    !
    if ( r%opt_compute_ref ) &
    call synref_compute_ref ( r, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    return
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synref_prep  ", &
    & /," during synref_compute_ref " &
    & )')
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synref_prep  ", &
    & /," during memory allocation " &
    & )')
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in synref_prep  " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine synref_prep
  !
  subroutine synref_read_diffractor_file ( r )
    !
    ! add the file diffractors to the setup diffractors
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    !
    integer                                 :: i_stat
    integer                                 :: i_err
    integer                                 :: lu_inp
    integer                                 :: i0_dif
    integer                                 :: n0_dif
    integer                                 :: n0_fil
    integer                                 :: hdr_x
    integer                                 :: hdr_y
    integer                                 :: l_crd_160
    real                                    :: x_loc_dif
    real                                    :: y_loc_dif
    character(len=160)                      :: crd_160
    integer                                 :: temp
    !
    call synref_print_event ( &
                              ' synref_read_diffractor_file before read ', &
                              r%path_dif, &
                              r%hdr_x, r%hdr_y, &
                              r%n0_dif, r%amp_dif, r%dep_dif, &
                              r%x_loc_dif, r%y_loc_dif &
                            )
    !
    ! open and read the dffractor file
    !
    xxif_path_dif : &
    if ( .not. string_upper_compare ( r%path_dif, pathcheck_empty )  ) then
      !
      ! open the dffractor file
      !
      lu_inp = cio_fopen ( r%path_dif, 'r+' )
      !
      if ( lu_inp .le. 0 ) go to 999
      !
      ! read the number of lines in the file, diffractors = lines - 1
      !
      n0_fil = 0
      !
    1 continue
      !
      temp      = len (crd_160)
      l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp )
      !
      if ( l_crd_160 .gt. 0 ) &
      n0_fil = n0_fil + 1
      !
      if ( l_crd_160 .gt. 0 ) go to 1
      !
      ! compute n0_dif, the total number of input and file diffractors
      !
      n0_fil = max ( 0, n0_fil - 1 ) ! subtract 1 for the header word line
      !
      n0_dif = r%n0_dif + n0_fil
      !
      ! re-allocate memory, preserving what is currently in the arrays
      !
      call mem_realloc ( r%amp_dif, n0_dif, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
      call mem_realloc ( r%dep_dif, n0_dif, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
      call mem_realloc ( r%x_loc_dif, n0_dif, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
      call mem_realloc ( r%y_loc_dif, n0_dif, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
      ! rewind the input file
      !
      call cio_frewind ( lu_inp )
      !
      ! read the fast and slow header words
      !
      l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp )
      !
      read ( crd_160, *, err=997 ) hdr_x, hdr_y
      !
      xxif_hdr_x_ok : &
      if ( r%hdr_x .eq. hdr_x .and. r%hdr_y .eq. hdr_y &
      .or. r%hdr_x .eq. hdr_y .and. r%hdr_y .eq. hdr_x ) then
        !
      else xxif_hdr_x_ok
        !
        call pc_warning ( &
' you must use consistent header words for the diffractors and output' )
        !
        call pc_error ( ' you must use hdr_x_fil=hdr_x, hdr_y_fil=,hdr_y' )
        !
        call pc_error ( '           or hdr_y_fil=hdr_x, hdr_x_fil=,hdr_y' )
        !
        call pc_error ( ' input hdr_x=',r%hdr_x,' hdr_y=',r%hdr_y )
        !
        call pc_error ( ' file  hdr_x=',hdr_x,' hdr_y=',hdr_y )
        !
        go to 996
        !
      end if xxif_hdr_x_ok
      !
      ! read in amp, fast, slow, depth order
      !
      do_i0_dif : do i0_dif = r%n0_dif+1 , n0_dif
        !
        temp      = len (crd_160)
        l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp )
        !
        read ( crd_160, *, err=997 ) &
        r%amp_dif ( i0_dif ), x_loc_dif, y_loc_dif, r%dep_dif ( i0_dif )
        !
        xxif_hdr_x : &
        if ( r%hdr_x .eq. hdr_x .and. r%hdr_y .eq. hdr_y ) then
          !
          r%x_loc_dif ( i0_dif ) = x_loc_dif
          !
          r%y_loc_dif ( i0_dif ) = y_loc_dif
          !
        else if ( r%hdr_x .eq. hdr_y .and. r%hdr_y .eq. hdr_x ) then
          !
          ! read in amp, slow, fast, depth order
          !
          r%x_loc_dif ( i0_dif ) = y_loc_dif
          !
          r%y_loc_dif ( i0_dif ) = x_loc_dif
          !
        end if xxif_hdr_x
        !
      end do do_i0_dif
      !
      r%n0_dif = n0_dif
      !
      ! close the file
      !
      i_stat = cio_fclose ( lu_inp )
      !
    end if xxif_path_dif
    !
    call synref_print_event ( &
                              ' synref_read_diffractor_file after read ', &
                              r%path_dif, r%hdr_x, r%hdr_y, &
                              r%n0_dif, r%amp_dif, r%dep_dif, &
                              r%x_loc_dif, r%y_loc_dif &
                            )
    !
    ! scale the point diffractor x,y location values to distance units
    !
    r%x_loc_dif = r%x_loc_dif * r%rx_scl
    !
    r%y_loc_dif = r%y_loc_dif * r%ry_scl
    !
    return
    !
995 continue
    !
    call pc_error ( ' error in synref_read_diffractor_file during mem alloc' )
    !
    go to 999
    !
996 continue
    !
    call pc_error ( ' error in synref_read_diffractor_file in header' )
    !
    go to 999
    !
997 continue
    !
    call pc_error ( ' error in synref_read_diffractor_file during read' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( ' error in synref_read_diffractor_file during mem alloc' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in synref_read_diffractor_file ' )
    !
    return
    !
  end subroutine synref_read_diffractor_file
  !
  subroutine synref_read_pick_file ( r )
    !
    ! add the file picks to the setup picks
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    !
    integer                                 :: i_stat
    integer                                 :: i_err
    integer                                 :: lu_inp
    integer                                 :: i0_pik
    integer                                 :: n0_pik
    integer                                 :: n0_fil
    integer                                 :: hdr_x, hdr_y
    integer                                 :: l_crd_160
    real                                    :: x_loc_pik
    real                                    :: x_off_pik
    character(len=160)                      :: crd_160
    integer                                 :: temp
    !
    call synref_print_event ( &
                              ' synref_read_pick_file before read ', &
                              r%path_pik, &
                              r%hdr_x, r%hdr_y, &
                              r%n0_pik, r%amp_pik, r%dep_pik, &
                              r%x_loc_pik, r%x_off_pik &
                            )
    !
    ! open and read the dffractor file
    !
    xxif_path_pik : &
    if ( .not. string_upper_compare ( r%path_pik, pathcheck_empty )  ) then
      !
      ! open the dffractor file
      !
      lu_inp = cio_fopen ( r%path_pik, 'r+' )
      !
      if ( lu_inp .le. 0 ) go to 999
      !
      ! read the number of lines in the file, picks = lines - 1
      !
      n0_fil = 0
      !
      n0_pik = r%n0_pik
      !
      !print'(" a n0_fil=", i8, " n0_pik=", i8)', n0_fil, n0_pik
      !
    1 continue
      !
      temp      = len (crd_160)
      l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp )
      !
      !if ( n0_fil .le. 10 .or. mod(n0_fil,100) .eq. 1 &
      !.or. ( n0_fil .ge. 5000 .and. n0_fil .le. 6000 ) ) &
      !print'(" a n0_fil=",i8," l=",a)',n0_fil, trim(crd_160)
      !
      if ( l_crd_160 .gt. 0 ) &
      n0_fil = n0_fil + 1
      !
      if ( l_crd_160 .gt. 0 ) go to 1
      !
      ! compute n0_pik, the total number of input and file picks
      !
      n0_fil = max ( 0, n0_fil - 1 ) ! subtract 1 for the header word line
      !
      n0_pik = n0_pik + n0_fil
      !
      !print'(" b n0_fil=", i8, " n0_pik=", i8)', n0_fil, n0_pik
      !
      ! re-allocate memory, preserving what is currently in the arrays
      !
      call mem_realloc ( r%amp_pik, n0_pik, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
      call mem_realloc ( r%dep_pik, n0_pik, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
      call mem_realloc ( r%x_loc_pik, n0_pik, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
      call mem_realloc ( r%y_loc_pik, n0_pik, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
      call mem_realloc ( r%x_off_pik, n0_pik, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
      call mem_realloc ( r%y_off_pik, n0_pik, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
      ! rewind the input file
      !
      call cio_frewind ( lu_inp )
      !
      ! read the fast and slow header words
      !
      l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp )
      !
      hdr_x = r%hdr_x
      !
      hdr_y = r%hdr_y
      !
      !read ( crd_160, *, err=997 ) hdr_x, hdr_y
      !
      xxif_hdr_x_ok : &
      if ( r%hdr_x .eq. hdr_x .and. r%hdr_y .eq. hdr_y &
      .or. r%hdr_x .eq. hdr_y .and. r%hdr_y .eq. hdr_x ) then
        !
      else xxif_hdr_x_ok
        !
        call pc_warning ( &
' you must use consistent header words for the picks and output' )
        !
        call pc_error ( ' you must use hdr_x_fil=hdr_x, hdr_y_fil=,hdr_y' )
        !
        call pc_error ( '           or hdr_y_fil=hdr_x, hdr_x_fil=,hdr_y' )
        !
        call pc_error ( ' input hdr_x=',r%hdr_x,' hdr_y=',r%hdr_y )
        !
        call pc_error ( ' file  hdr_x=',hdr_x,' hdr_y=',hdr_y )
        !
        go to 996
        !
      end if xxif_hdr_x_ok
      !
      ! read in amp, fast, slow, depth order
      !
      do_i0_pik : do i0_pik = r%n0_pik+1 , n0_pik
        !
        temp = len(crd_160)
        l_crd_160 = cio_fgetline ( crd_160, temp, lu_inp )
        !
        !if ( i0_pik .le. 10 .or. mod(i0_pik,100) .eq. 1 ) &
        !print'(" b n0_fil=",i8," l=",a)', i0_pik, trim(crd_160)
        !
        read ( crd_160, *, err=997 ) &
        x_loc_pik, x_off_pik, r%dep_pik ( i0_pik )
        !
        !if ( i0_pik .le. 10 .or. mod(i0_pik,100) .eq. 1 ) &
        !print'(" x=",g12.6," o=",g12.6," d=",g12.6)', &
        !x_loc_pik, x_off_pik, r%dep_pik ( i0_pik )
        !
        xxif_hdr_x : &
        if ( r%hdr_x .eq. hdr_x .and. r%hdr_y .eq. hdr_y ) then
          !
          r%x_loc_pik ( i0_pik ) = x_loc_pik
          !
          r%x_off_pik ( i0_pik ) = x_off_pik
          !
          r%y_loc_pik ( i0_pik ) = 0
          !
          r%y_off_pik ( i0_pik ) = 0
          !
          r%amp_pik ( i0_pik ) = 1.
          !
        else if ( r%hdr_x .eq. hdr_y .and. r%hdr_y .eq. hdr_x ) then
          !
          ! read in amp, slow, fast, depth order
          !
          !
          r%x_loc_pik ( i0_pik ) = x_off_pik
          !
          r%x_off_pik ( i0_pik ) = x_loc_pik
          !
          r%y_loc_pik ( i0_pik ) = 0
          !
          r%y_off_pik ( i0_pik ) = 0
          !
          r%amp_pik ( i0_pik ) = 1.
          !
        end if xxif_hdr_x
        !
      end do do_i0_pik
      !
      r%n0_pik = n0_pik
      !
      ! close the file
      !
      i_stat = cio_fclose ( lu_inp )
      !
    end if xxif_path_pik
    !
    call synref_print_event ( &
                              ' synref_read_pick_file after read ', &
                              r%path_pik, &
                              r%hdr_x, r%hdr_y, &
                              r%n0_pik, r%amp_pik, r%dep_pik, &
                              r%x_loc_pik, r%x_off_pik &
                            )
    !
    return
    !
995 continue
    !
    call pc_error ( ' error in synref_read_pick_file during mem alloc' )
    !
    go to 999
    !
996 continue
    !
    call pc_error ( ' error in synref_read_pick_file in header' )
    !
    go to 999
    !
997 continue
    !
    call pc_error ( ' error in synref_read_pick_file during read' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( ' error in synref_read_pick_file during mem alloc' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in synref_read_pick_file ' )
    !
    return
    !
  end subroutine synref_read_pick_file
  !
  subroutine synref_print_event ( &
                                  c_title_1, c_title_2, hdr_x, hdr_y, &
                                  n0_dif, amp_dif, dep_dif, &
                                  x_loc_dif, y_loc_dif &
                                )
    !
    ! print reflector or diffractor info
    !
    character(len=*),         intent(in   ) :: c_title_1! title 1 to print
    character(len=*),         intent(in   ) :: c_title_2! title 2 to print
    integer,                  intent(in   ) :: hdr_x    ! fast header word
    integer,                  intent(in   ) :: hdr_y    ! slow header word
    integer,                  intent(inout) :: n0_dif   ! number of events
    real,                           pointer :: amp_dif ( : )  ! dif amp
    real,                           pointer :: dep_dif ( : )  ! dif z location
    real,                           pointer :: x_loc_dif ( : )! dif x location
    real,                           pointer :: y_loc_dif ( : )! dif y location
    !
    integer                                 :: i0_dif
    integer                                 :: l0_dif
    !
    l0_dif = 1
    !
    if ( n0_dif .gt. 100 ) l0_dif = n0_dif / 100
    !
    write ( pc_get_lun(), '( &
    & /, " spike_print_event ", &
    & /, a, &
    & /, a, &
    & /, " number of events =", i8, &
    & /, " hdr_x    =", i8 ," hdr_y    =", i8 &
    & /, "      amp        fast         slow        depth         index " &
    & )') &
    trim ( c_title_1 ), &
    trim ( c_title_2 ), &
    n0_dif, &
    hdr_x, hdr_y
    !
    do_i0_dif : do i0_dif = 1 , n0_dif
      !
      if ( i0_dif .eq. 1 &
      .or. i0_dif .eq. n0_dif &
      .or. l0_dif .eq. 1 &
      .or. mod ( i0_dif, l0_dif ) .eq. 1 ) &
      write ( pc_get_lun(), '( &
      & 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, i8 &
      & )') &
      amp_dif ( i0_dif ), x_loc_dif ( i0_dif ), y_loc_dif ( i0_dif ), &
      dep_dif ( i0_dif ), i0_dif
      !
    end do do_i0_dif
    !
    return
    !
  end subroutine synref_print_event
  !
  subroutine synref_set_timtype ( tim_type )
    !
    character tim_type* ( * )
    !
    call string_to_upper ( tim_type )
    !
    xxif_tim_type : &
    if ( string_upper_compare ( tim_type ( 1:1 ), 'V' )  ) then
      !
      tim_type = 'VERTICAL'
      !
    else if ( string_upper_compare ( tim_type ( 1:1 ), 'X' )  ) then
      !
      tim_type = 'X_SLICE'
      !
    else if ( string_upper_compare ( tim_type ( 1:1 ), 'Y' )  ) then
      !
      tim_type = 'Y_SLICE'
      !
    else xxif_tim_type
      !
      tim_type = 'NORMAL'
      !
    end if xxif_tim_type
    !
    return
    !
  end subroutine synref_set_timtype
  !
  subroutine synref_set_noise_type ( noise_type )
    !
    ! set the noise_type to allowed values
    !
    character(len=*),         intent(inout) :: noise_type
    !
    call string_to_upper ( noise_type )
    !
    if ( string_upper_compare ( noise_type ( 1:2 ), 'DE' )  ) then
      !
      noise_type = 'DEAD'
      !
    else if ( string_upper_compare ( noise_type ( 1:1 ), 'S' )  ) then
      !
      noise_type = 'SAME'
      !
    else
      !
      noise_type = 'DIFF'
      !
    end if
    !
    return
    !
  end subroutine synref_set_noise_type
  !
  subroutine synref_compute_spike ( r )
    !
    ! compute a trace with constant spikes at even time intervals
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    !
    real                                    :: rt_out ! spike time
    integer                                 :: jt_out ! spike index
    integer                                 :: jt_fin ! time point index
    !
    ! initalize the trace to zero
    !
    r%tr_spike ( 1:r%nt_fin ) = 0.0
    !
    ! set trace values to a constant
    !
    loop_set_tr_vals:    &
    do jt_out = 1, r%spike_tot
      !
      rt_out = ( jt_out - 1 ) * r%spike_inc + r%spike_min
      !
      jt_fin = nint (  ( rt_out - r%t0_fin ) / r%dt_fin ) + 1
      !
      if ( jt_fin .ge. 1. .and. jt_fin .le. r%nt_fin ) &
      r%tr_spike ( jt_fin ) = r%tr_spike ( jt_fin ) + r%amp_spike
      !
      !print'(" qq0",1x,i8,1x,i8,1x,g12.6,1x,g12.6)', &
      !jt_out, jt_fin, rt_out, r%tr_spike ( jt_fin )
      !
    end do loop_set_tr_vals
    !
    return
    !
  end subroutine synref_compute_spike
  !
  subroutine synref_compute_noise ( amp_noise, nt_fin, tr_noise )
    !
    ! compute a trace with constant spikes at random time intervals
    !
    real,                     intent(in   ) :: amp_noise     ! standard dev
    integer,                  intent(in   ) :: nt_fin        ! num time points
    real,                     intent(inout) :: tr_noise ( : )! noise trace
    !
    integer                                 :: jt_fin        ! time point index
    !
    ! initalize the trace to zero
    !
    tr_noise ( 1:nt_fin ) = 0.0
    !
    ! set trace amplitudes to random values
    !
    xxif_noise : if ( amp_noise .ne. 0 ) then
      !
      do_jt_fin : do jt_fin = 1, nt_fin - 1, 2
        !
        call mth_gauss_ranf ( amp_noise, &
        tr_noise ( jt_fin ), tr_noise ( jt_fin+1 ), set_random_seed )
        !
      end do do_jt_fin
      !
    end if xxif_noise
    !
    return
    !
  end subroutine synref_compute_noise
  !
  subroutine synref_compute_scale ( r )
    !
    ! compute a trace sclaing vector for 1. / time **pwr_decay
    !
    type ( synref_struct ),         pointer :: r   ! synref structure
    !
    integer                                 :: jt_fin    ! time point indec
    real                                    :: rt_fin    ! time value
    !
    ! initalize the trace to zero
    !
    r%tr_scale ( 1:r%nt_fin ) = 1.
    !
    ! set trace scale value to 1. /time**pwr_decay
    !
    xxif_pwr_decay : if ( r%pwr_decay .ne. 0 ) then
      !
      do_jt_fin : do jt_fin = 1, r%nt_fin
        !
        rt_fin = max ( .1, abs (  ( jt_fin - 1 ) * r%dt_fin + r%t0_fin )  )
        !
        r%tr_scale ( jt_fin ) = 1. / rt_fin ** r%pwr_decay
        !
      end do do_jt_fin
      !
    end if xxif_pwr_decay
    !
    return
    !
  end subroutine synref_compute_scale
  !
  subroutine synref_plane_reflection_pos ( &
                                           ax, ay, az, &
                                           xs, ys, zs, &
                                           xr, yr, zr, &
                                           x2, y2, z2 &
                                         )
    !
    !  compute the specular reflection position, x2, y2, z2,
    !  for a dipping plane reflector
    !  defined by z ( x, y ) = az + ax * x + ay * y
    !  with source, xs, ys, zs, and receiver, xr, yr, zr
    !
    ! 1. - compute the normal, x0, y0, z0, to the dipping plane,
    !      through the source position, xs, ys, zs
    !
    ! 2. - compute the virtual source, x1, y1, z1, on the opposite side
    !      of the dipping plane from the source.
    !
    ! 3. - compute the intersection, x2, y2, z2, of the dipping plane and the
    !      line from the virtual source, x1, y1, z1,
    !      to the receiver, xr, yr, zr.
    !
    !  see
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
    real,                     intent(in   ) :: xs, ys, zs
    real,                     intent(in   ) :: xr, yr, zr
    real,                     intent(in   ) :: az, ax, ay
    !
    real,                     intent(inout) :: x2, y2, z2
    !
    real                                    :: bx, by
    real                                    :: xn, yn, zn
    real                                    :: x0, y0, z0
    real                                    :: x1, y1, z1
    real                                    :: c1, c2, c3
    real                                    :: r0_eps
    !
    r0_eps = 1.e-9
    !
    ! compute the point x0, y0, z0 within, and normal to, the plane reflector
    ! which passes through the source xs, ys, zs
    !
    c1 = ( ax * xs + ay * ys + az - zs ) / ( 1. + ax * ax + ay * ay )
    x0 = xs - ax * c1
    y0 = ys - ay * c1
    z0 = az + ax * x0 + ay * y0
    !
    zn = zs
    xn = x0 + ax * ( z0 - zn )
    yn = y0 + ay * ( z0 - zn )
    !
    ! compute the virtual source image point x1, y1, z1
    ! on the opposite side of the reflector from the source
    !
    x1 = xs + 2. * ( x0 - xs )
    y1 = ys + 2. * ( y0 - ys )
    z1 = zs + 2. * ( z0 - zs )
    !
    ! compute the intersection, x2, y2, z2,
    ! bewteen the the dipping reflector, z ( x, y ) = az + ax * x + ay * y
    ! and the line defined by the receiver xr, yr, zr
    ! and virtual source x1, y1, z1
    ! x ( z ) = xr + bx * ( z - zr ) ; bx = ( x1 - xr ) / ( z1 - zr )
    ! y ( z ) = yr + by * ( z - zr ) ; by = ( y1 - yr ) / ( z1 - zr )
    !
    xxif_z : if ( abs ( z1-zr ) .le. r0_eps ) then
      !
      bx = 0.
      by = 0.
      !
    else xxif_z
      !
      bx = ( x1 - xr ) / ( z1 - zr )
      by = ( y1 - yr ) / ( z1 - zr )
      !
    end if xxif_z
    !
    c3 = ( 1. - ax * bx - ay * by )
    !
    xxif_c : if ( abs ( c3 ) .le. r0_eps ) then
      !
      c2 = 0.
      !
    else
      !
      c2 = ( ax * xr + ay * yr + az - zr ) / c3
      !
    end if xxif_c
    !
    x2 = xr + bx * c2
    y2 = yr + by * c2
    z2 = az + ax * x2 + ay * y2
    !
    !print'( &
    !& /, " ss0 a x=",g12.6," y=",g12.6," z=",g12.6, &
    !& /, " ss0 d x=",g12.6," y=",g12.6," z=",g12.6, &
    !& /, " ss0 n x=",g12.6," y=",g12.6," z=",g12.6, &
    !& /, " ss0 s x=",g12.6," y=",g12.6," z=",g12.6, &
    !& /, " ss0 r x=",g12.6," y=",g12.6," z=",g12.6, &
    !& /, " ss0 r x=",g12.6," y=",g12.6," z=",g12.6, &
    !& /, " ss0 1 x=",g12.6," y=",g12.6," z=",g12.6, &
    !& /, " ss0 2 x=",g12.6," y=",g12.6," z=",g12.6 &
    !& )', &
    !ax, ay, az, &
    !xn, yn, zn, &
    !xs, ys, zs, &
    !xr, yr, zr, &
    !x0, y0, z0, &
    !x1, y1, z1, &
    !x2, y2, z2
    !
    !if ( x1 .ne. -999 ) stop
    !
    return
    !
  end subroutine synref_plane_reflection_pos
  !
  subroutine synref_register_array_names 
    !
    ! register array names
    !
    call pc_register_array_names ( 'AMP_REF_ARRAYSET', &
                                 (/'AMP_REF  ', &
                                   'DEP_REF  ', &
                                   'X_DIP_REF', &
                                   'Y_DIP_REF'/))
    !
    call pc_register_array_names ( 'DN1_REF_ARRAYSET', &
                                 (/'DN1_REF', &
                                   'VP1_REF', &
                                   'VS1_REF', &
                                   'DN2_REF', &
                                   'VP2_REF', &
                                   'VS2_REF'/))
    !
    call pc_register_array_names ( 'AMP_DIF_ARRAYSET', &
                                 (/'AMP_DIF  ', &
                                   'DEP_DIF  ', &
                                   'X_LOC_DIF', &
                                   'Y_LOC_DIF'/))
    !
    call pc_register_array_names ( 'DN1_DIF_ARRAYSET', &
                                 (/'DN1_DIF', &
                                   'VP1_DIF', &
                                   'VS1_DIF', &
                                   'DN2_DIF', &
                                   'VP2_DIF', &
                                   'VS2_DIF'/))
    !
    call pc_register_array_names ( 'AMP_PIK_ARRAYSET', &
                                 (/'AMP_PIK  ', &
                                   'DEP_PIK  ', &
                                   'X_LOC_PIK', &
                                   'Y_LOC_PIK'/))
    !
    return
    !
  end subroutine synref_register_array_names 
  !
  subroutine synref_compute_ref ( r, i_err )
    !
    ! compute the reflectivity 
    !
    type ( synref_struct ),     pointer :: r      ! synref structure
    integer,             intent(inout) :: i_err  ! error 0 O.K. -1 err
    !
    ! Local variables
    !
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    i_err = 0
    !
    !print'(" top synref_compute_ref p=",i4," e=",i8)', pcpsx_i_pel(), i_err
    !
    !xxif_root : if ( pcpsx_i_pel() .eq. 0 ) then
    !
    ! open the input file
    !
    call synref_open_inp ( r, i_err )
    !
    !print'(" aa1 synref_compute_ref p=",i4," e=",i8)', pcpsx_i_pel(), i_err
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! open the otuput file
    !
    call synref_open_out ( r, i_err )
    !
    !print'(" aa2 synref_compute_ref p=",i4," e=",i8)', pcpsx_i_pel(), i_err
    !
    if ( i_err .ne. 0 ) go to 997
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'( &
    & /," synref_compute_ref p=",i4," c=",i8, &
    & /," path_ref_inp=",a, &
    & /," path_ref_out=",a, &
    & /," mg_ref_type=",a, &
    & /," mg_inp_xyz =",a, &
    & /," mg_hdr_x=",i8, &
    & /," mg_hdr_y=",i8, &
    & /," nx=",i8," x0=",g12.6," x1=",g12.6," dx=",g12.6," hx=",i8," lx=", a, &
    & /," ny=",i8," y0=",g12.6," y1=",g12.6," dy=",g12.6," hy=",i8," ly=", a, &
    & /," nz=",i8," z0=",g12.6," z1=",g12.6," dz=",g12.6," hz=",i8," lz=", a &
    & )', &
    pcpsx_i_pel(), i_call, &
    trim ( r%path_ref_inp ), &
    trim ( r%path_ref_out ), &
    trim ( r%mg_ref_type ), &
    r%mg_inp_xyz, r%mg_hdr_x, r%mg_hdr_y, &
    r%nx_ref, r%x0_ref, r%x1_ref, r%dx_ref, r%hdr_x, trim(r%lx_ref), &
    r%ny_ref, r%y0_ref, r%y1_ref, r%dy_ref, r%hdr_y, trim(r%ly_ref), &
    r%nz_ref, r%z0_ref, r%z1_ref, r%dz_ref, r%hdr_x, trim(r%lz_ref)
    !
    ! compute all y columns
    !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    call synref_compute_ref_yn ( r, i_err )
    !
    !print'(" aa3 synref_compute_ref p=",i4," e=",i8)', pcpsx_i_pel(), i_err
    !
    if ( i_err .ne. 0 ) go to 996
    !
    ! delete the inp modgrid structure 
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    call modgrid_delete ( r%mg_inp )
    !
    !print'(" aa4 synref_compute_ref p=",i4," e=",i8)', pcpsx_i_pel(), i_err
    !
    ! delete the out modgrid structure 
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    call modgrid_delete ( r%mg_out )
    !
    !print'(" aa5 synref_compute_ref p=",i4," e=",i8)', pcpsx_i_pel(), i_err
    !
    ! delete the out trcio structure 
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    call modgrid_close_binfile ( r%tr_obj )
    !
    !print'(" aa6 synref_compute_ref p=",i4," e=",i8)', pcpsx_i_pel(), i_err
    !
    !end if xxif_root 
    !
1999 continue
    !
    !print'(" end synref_compute_ref p=",i4," e=",i8)', pcpsx_i_pel(), i_err
    !
    !if ( i_err .ne. -999 ) stop
    !
    return
    !
996 continue
    !
    print'( &
    & /, " error in synref_compute_ref p=",i4, &
    & /, " during synref_compute_ref_yn ", a &
    & )', &
    pcpsx_i_pel(), trim ( r%mg_inp_xyz )
    !
    go to 999
    !
997 continue
    !
    print'( &
    & /, " error in synref_compute_ref ", &
    & /, " during synref_open_out " &
    & )'
    !
    go to 999
    !
998 continue
    !
    print'( &
    & /, " error in synref_compute_ref ", &
    & /, " during synref_open_inp " &
    & )'
    !
    go to 999
    !
999 continue
    !
    print'( &
    & /, " error in synref_compute_ref p=",i4, &
    & /, " path_ref_inp=", a &
    & )', &
    pcpsx_i_pel(), trim(r%path_ref_inp)
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine synref_compute_ref 
  !
  subroutine synref_open_inp ( r, i_err )
    !
    ! open the input file 
    !
    type ( synref_struct ),     pointer :: r      ! synref structure
    integer,             intent(inout) :: i_err  ! error 0 O.K. -1 err
    !
    ! Local variables
    !
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    i_err = 0
    !
    r%mg_max_mem = 2000000
    !
    r%mg_out_xyz = 'ZXY'
    !
    r%mg_hdr_x= 17
    !
    r%mg_hdr_y= 18
    !
    r%mg_hdr_x= r%hdr_x
    !
    r%mg_hdr_y= r%hdr_y
    !
    r%mg_ref_type = ' '
    !
    ! open the file
    !
    i_err = modgrid_rddesc_verbose ( &
                            r%mg_inp, &
                            r%path_ref_inp, &
                            pc_get_lun(), &
                            r%mg_file, r%mg_type, r%mg_rank, &
                            r%lz_ref, r%hdr_z,  r%nz_ref, r%z0_ref, r%dz_ref, &
                            r%lx_ref, r%hdr_x,  r%nx_ref, r%x0_ref, r%dx_ref, &
                            r%ly_ref, r%hdr_y,  r%ny_ref, r%y0_ref, r%dy_ref, &
                                     !mg_l1, mg_h1, mg_n1, mg_o1, mg_d1, & 
                                     !mg_l2, mg_h2, mg_n2, mg_o2, mg_d2, & 
                                     !mg_l3, mg_h3, mg_n3, mg_o3, mg_d3, & 
                            r%mg_inp_xyz, r%mg_hdr_x, r%mg_hdr_y, &
                            r%mg_ref_type &
                                     )
    !
    if ( i_err .lt. 0 ) go to 998
    !
    r%x1_ref = ( r%nx_ref - 1 ) * r%dx_ref + r%x0_ref
    r%y1_ref = ( r%ny_ref - 1 ) * r%dy_ref + r%y0_ref
    r%z1_ref = ( r%nz_ref - 1 ) * r%dz_ref + r%z0_ref
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'( &
    & /," synref_open_inp p=",i4," c=",i8, &
    & /," path_ref_inp=",a, &
    & /," path_ref_out=",a, &
    & /," mg_ref_type=",a, &
    & /," mg_inp_xyz =",a, &
    & /," mg_hdr_x=",i8, &
    & /," mg_hdr_y=",i8, &
    & /," nx=",i8," x0=",g12.6," x1=",g12.6," dx=",g12.6," hx=",i8," lx=", a, &
    & /," ny=",i8," y0=",g12.6," y1=",g12.6," dy=",g12.6," hy=",i8," ly=", a, &
    & /," nz=",i8," z0=",g12.6," z1=",g12.6," dz=",g12.6," hz=",i8," lz=", a &
    & )', &
    pcpsx_i_pel(), i_call, &
    trim ( r%path_ref_inp ), &
    trim ( r%path_ref_out ), &
    trim ( r%mg_ref_type ), &
    r%mg_inp_xyz, r%mg_hdr_x, r%mg_hdr_y, &
    r%nx_ref, r%x0_ref, r%x1_ref, r%dx_ref, r%hdr_x, trim(r%lx_ref), &
    r%ny_ref, r%y0_ref, r%y1_ref, r%dy_ref, r%hdr_y, trim(r%ly_ref), &
    r%nz_ref, r%z0_ref, r%z1_ref, r%dz_ref, r%hdr_z, trim(r%lz_ref)
    !
    ! determine the output ordering of X, Y and Z axis
    !
    if ( index ( r%mg_inp_xyz, 'Z' ) .ne. 1 ) go to 997
    !
    r%mg_out_xyz = r%mg_inp_xyz
    !
    !print'(" end synref_open_inp ")'
    !
1999 continue
    !
    call pcpsx_check_worker_errors ( i_err )
    !
    return
    !
997 continue
    !
    print'( &
    & /, " error in synref_open_inp ", &
    & /, " in file x,y,z order =", a &
    & )', &
    trim ( r%mg_inp_xyz )
    !
    go to 999
    !
998 continue
    !
    print'( &
    & /, " error in synref_open_inp ", &
    & /, " during modgrid_rddesc_verbose " &
    & )'
    !
    go to 999
    !
999 continue
    !
    print'( &
    & /, " error in synref_open_inp ", &
    & /, " path_ref_inp=", a &
    & )', &
    trim(r%path_ref_inp)
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine synref_open_inp 
  !
  subroutine synref_open_out ( r, i_err )
    !
    ! Create the out modgrid structure
    !
    type ( synref_struct ),     pointer :: r      ! synref structure
    integer,             intent(inout) :: i_err
    !
    integer                            :: ng(3)
    real                               :: og(3)
    real                               :: dg(3)
    integer                            :: hd(3)
    real                               :: oxyz(3) 
    real                               :: axis1(3)
    real                               :: axis2(3)
    real                               :: axis3(3)
    character(len=12)                  :: label(3)
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    i_err = 0
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'( &
    & /," synref_open_out p=",i4," c=",i8, &
    & /," path_ref_inp=",a, &
    & /," path_ref_out=",a, &
    & /," mg_ref_type=",a, &
    & /," mg_inp_xyz =",a, &
    & /," mg_hdr_x=",i8, &
    & /," mg_hdr_y=",i8, &
    & /," nx=",i8," x0=",g12.6," x1=",g12.6," dx=",g12.6," hx=",i8," lx=", a, &
    & /," ny=",i8," y0=",g12.6," y1=",g12.6," dy=",g12.6," hy=",i8," ly=", a, &
    & /," nz=",i8," z0=",g12.6," z1=",g12.6," dz=",g12.6," hz=",i8," lz=", a &
    & )', &
    pcpsx_i_pel(), i_call, &
    trim ( r%path_ref_inp ), &
    trim ( r%path_ref_out ), &
    trim ( r%mg_ref_type ), &
    r%mg_inp_xyz, r%mg_hdr_x, r%mg_hdr_y, &
    r%nx_ref, r%x0_ref, r%x1_ref, r%dx_ref, r%hdr_x, trim(r%lx_ref), &
    r%ny_ref, r%y0_ref, r%y1_ref, r%dy_ref, r%hdr_y, trim(r%ly_ref), &
    r%nz_ref, r%z0_ref, r%z1_ref, r%dz_ref, r%hdr_z, trim(r%lz_ref)
    !
    ! Create dummy output obj
    !
    ng(1) = r%nz_ref
    !
    ng(2) = r%nx_ref
    !
    ng(3) = r%ny_ref
    !
    og(1) = r%z0_ref 
    !
    og(2) = r%x0_ref 
    !
    og(3) = r%y0_ref 
    !
    dg(1) = r%dz_ref 
    !
    dg(2) = r%dx_ref 
    !
    dg(3) = r%dy_ref 
    !
    hd(:) = -1
    !
    hd(2) = r%hdr_x
    !
    hd(3) = r%hdr_y
    !
    !print'(" aa1 synref_open_out p=",i4," ass=",l2," e=",i8)', &
    !pcpsx_i_pel(), associated ( r%mg_out ), i_err
    !
    call modgrid_create ( r%mg_out, 3, 'synref', 'ref', &
                          's/m', ng, og, dg, pc_get_lun() )
    !
    if ( pcpsx_i_pel() .ne. 0 .and. .not. associated ( r%mg_out ) ) i_err = -1
    !
    !print'(" aa2 synref_open_out p=",i4," ass=",l2," e=",i8)', &
    !pcpsx_i_pel(), associated ( r%mg_out ), i_err
    !
    if ( i_err .ne. 0 ) go to 998
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    call modgrid_set_grid ( r%mg_out, r%grid_obj )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    call modgrid_set_hdwd ( r%mg_out, hd )
    !
    ! set default orientation & size to align with XYZ axis
    !
    axis1=0.0
    !
    axis2=0.0
    !
    axis3=0.0
    !
    axis1(3)= (ng(1)-1)*dg(1)
    !
    axis2(1)= (ng(2)-1)*dg(2)
    !
    axis3(2)= (ng(3)-1)*dg(3)
    !
    oxyz(1) = og(2)  !x origin
    !
    oxyz(2) = og(3)  !y origin
    !
    oxyz(3) = og(1)
    !
    label(1) = 'DEPTH'
    !
    if ( r%hdr_x .eq. 7 ) &
    label(2) = 'XGRID'
    !
    if ( r%hdr_x .eq. 8 ) &
    label(2) = 'YGRID'
    !
    if ( r%hdr_x .eq. 17 ) &
    label(2) = 'XBASEMENT'
    !
    if ( r%hdr_x .eq. 18 ) &
    label(2) = 'YBASEMENT'
    !
    if ( r%hdr_y .eq. 7 ) &
    label(3) = 'XGRID'
    !
    if ( r%hdr_y .eq. 8 ) &
    label(3) = 'YGRID'
    !
    if ( r%hdr_y .eq. 17 ) &
    label(3) = 'XBASEMENT'
    !
    if ( r%hdr_y .eq. 18 ) &
    label(3) = 'YBASEMENT'
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    call modgrid_set_xyz ( r%mg_out, label, oxyz, axis1, axis2, axis3 )
    !
    ! remove the output file
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    i_err = cio_remove ( r%path_ref_out )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! create a trace file to hold the output reflectivity
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    r%tr_obj => modgrid_wr_trcio_header ( r%mg_out, r%path_ref_out, pc_get_lun() )
    !
    if ( i_err .lt. 0 ) go to 996
    !
1999 continue
    !
    call pcpsx_check_worker_errors ( i_err )
    !
    !print'(" end synref_open_out p=",i4," e=",i8)', &
    !pcpsx_i_pel(), i_err
    !
    return
    !
996 continue
    !
    print'( &
    & /, " error in synref_open_out ", &
    & /, " during modgrid_wr_trcio_header " &
    & )'
    !
    go to 999
    !
997 continue
    !
    print'( &
    & /, " error in synref_open_out ", &
    & /, " during cio_remove " &
    & )'
    !
    go to 999
    !
998 continue
    !
    print'( &
    & /, " error in synref_open_out ", &
    & /, " during modgrid_create " &
    & )'
    !
    go to 999
    !
999 continue
    !
    print'( &
    & /, " error in synref_open_out ", &
    & /, " path_ref_inp=", a &
    & )', &
    trim(r%path_ref_inp)
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine synref_open_out
  !
  subroutine synref_compute_ref_yn ( r, i_err )
    !
    ! read the input file 
    !
    type ( synref_struct ),     pointer :: r      ! synref structure
    integer,             intent(inout) :: i_err  ! error 0 O.K. -1 err
    !
    ! Local variables
    !
    integer                            :: jx_ref ! x vel index
    real                               :: rx_ref
    integer                            :: jy_ref ! y vel index
    real                               :: ry_ref
    !
    integer                            :: i0_out
    integer                            :: nx_ref
    real                               :: x0_ref
    real                               :: dx_ref
    integer                            :: ny_tmp
    real                               :: y0_tmp
    real                               :: dy_tmp
    integer                            :: nz_ref
    real                               :: z0_ref
    real                               :: dz_ref
    real                               :: v0_inp ( r%nz_ref, r%nx_ref )
    real                               :: v0_out ( r%nz_ref, r%nx_ref )
    double precision                   :: hd_out ( r%nh_inp, r%nx_ref )
    !
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    i_err = 0
    !
    !print'(" top synref_compute_ref_yn p=",i4," e=",i8)', pcpsx_i_pel(), i_err
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'( &
    & /," synref_compute_ref_yn p=",i4," c=",i8, &
    & /," path_ref_inp=",a, &
    & /," path_ref_out=",a, &
    & /," mg_ref_type=",a, &
    & /," mg_inp_xyz =",a, &
    & /," mg_hdr_x=",i8, &
    & /," mg_hdr_y=",i8, &
    & /," nx=",i8," x0=",g12.6," x1=",g12.6," dx=",g12.6, &
    & " xs=",g12.6, " hx=",i8," lx=", a, &
    & /," ny=",i8," y0=",g12.6," y1=",g12.6," dy=",g12.6, &
    & " ys=",g12.6, " hy=",i8," ly=", a, &
    & /," nz=",i8," z0=",g12.6," z1=",g12.6," dz=",g12.6, &
    & " zs=",g12.6, " hz=",i8," lz=", a &
    & )', &
    pcpsx_i_pel(), i_call, &
    trim ( r%path_ref_inp ), &
    trim ( r%path_ref_out ), &
    trim ( r%mg_ref_type ), &
    r%mg_inp_xyz, r%mg_hdr_x, r%mg_hdr_y, &
r%nx_ref, r%x0_ref, r%x1_ref, r%dx_ref, r%rx_scl, r%hdr_x, trim(r%lx_ref), &
r%ny_ref, r%y0_ref, r%y1_ref, r%dy_ref, r%ry_scl, r%hdr_y, trim(r%ly_ref), &
r%nz_ref, r%z0_ref, r%z1_ref, r%dz_ref, 1.,       r%hdr_z, trim(r%lz_ref)
    !
    ! cycle over each y node
    !
    !print'(" aa1 synref_compute_ref_yn p=",i4," e=",i8)', pcpsx_i_pel(), i_err
    !
    do_jy_ref : do jy_ref = 1 , r%ny_ref
      !
      ! define the y location of this y node
      !
      ry_ref = ( jy_ref - 1 ) * r%dy_ref + r%y0_ref
      !
      ny_tmp = 1
      !
      y0_tmp = ry_ref
      !
      dy_tmp = r%dy_ref 
      !
      nx_ref = r%nx_ref
      !
      x0_ref = r%x0_ref
      !
      dx_ref = r%dx_ref
      !
      nz_ref = r%nz_ref
      !
      z0_ref = r%z0_ref
      !
      dz_ref = r%dz_ref
      !
      r%parm_coef = 'VELOCITY'
      !
      i_err = velgrid_paint_by_obj_par ( r%mg_inp, pc_get_lun(), &
                                         'SYNREF', &
                                          r%parm_coef, &
                                          r%parm_coef, &
                                          0, .true., r%mg_max_mem, &
                                          r%hdr_x, r%hdr_y, &
                                          nz_ref, z0_ref, dz_ref, &
                                          nx_ref, x0_ref, dx_ref, &
                                          ny_tmp, y0_tmp, dy_tmp, &
                                          v0_inp ( :, : ), &
                                          r%mg_out_xyz, r%mg_ref_type &
                                        )
      !
      if ( i_err .ne. 0 ) go to 998
      !
      ! compute the reflectivity from the velocity field and add reflectors
      !
      call synref_compute_ref_y1 ( r, jy_ref, ry_ref, v0_inp, v0_out )
      !
      ! write this y slice of ray step incs to the output disk file 
      !
      do_jx_ref : do jx_ref = 1, r%nx_ref
        !
        rx_ref = ( jx_ref - 1 ) * r%dx_ref + r%x0_ref
        !
        ! set the output trace header
        !
        call migfun_output_headers ( &
                                     grid_obj = r%grid_obj, &
                                     trace_number = i0_out, &
                                     trace_group = jx_ref, &
                                     group_number = jy_ref, &
                                     hx_mig = r%hdr_x, &
                                     xm_mig = rx_ref, &
                                    !x0_scl = r%rx_scl, &
                                     x0_scl = 1., &
                                     hy_mig = r%hdr_y, &
                                     ym_mig = ry_ref, &
                                    !y0_scl = r%ry_scl, &
                                     y0_scl = 1., &
                                     om_mig = 0., &
                                     azimuth = 0., &
                                     top_mute = -1, &
                                     bot_mute = -1, &
                                     nt_fos = 0, &
                                     at_fos = 0., &
                                     tr_fos = v0_out(:,jx_ref), &
                                     i_velocity_scale = 1, &
                                     velocity_scale = 0., &
                                     mh_mig = r%nh_inp, &
                                     hd_mig = hd_out(:,jx_ref), &
                                     nt_mig = r%nz_ref, &
                                     tr_mig = v0_out(:,jx_ref), &
                                     i_err = i_err &
                                   )
        !
        if ( i_err .ne. 0 ) go to 997
        !
        ! write this trace to disk location i0_out
        !
        i0_out = ( jy_ref - 1 ) * r%nx_ref + jx_ref
        !
        !if ( i0_out .eq. -999 ) &
        !i_err = trcio_write_trace ( &
        !                            file = r%tr_obj,         &
        !                            hd   = hd_out(:,jx_ref), &
        !                            tr   = v0_out(:,jx_ref), &
        !                            tnum = i0_out            &
        !                          )
        !
        if ( i_err .ne. 0 ) go to 997
        !
      end do do_jx_ref 
      !
      if ( pcpsx_i_pel() .eq. 0 ) &
      i_err = modgrid_wr_binslice ( r%mg_out, r%tr_obj, jy_ref, &
      pc_get_lun(), 'TRCIO', r%nz_ref, r%nx_ref, v0_out )
      !
      if ( i_err .ne. 0 ) go to 997
      !
      if ( pcpsx_i_pel() .eq. 0 ) &
      print'(" jy=",i4, " jx=",i4, " h7=",g12.6," h17=",g12.6, &
      & " v0_inp=",g12.6,1x,g12.6, &
      & " v0_out=",g12.6,1x,g12.6 &
      & )', &
      ( jy_ref, jx_ref, &
      hd_out(7,jx_ref), hd_out(17,jx_ref), &
      minval(v0_inp(:,jx_ref)), maxval(v0_inp(:,jx_ref)), &
      minval(v0_out(:,jx_ref)), maxval(v0_out(:,jx_ref)), &
      jx_ref= 1 , r%nx_ref )
      !
    end do do_jy_ref 
    !
    !print'(" aa2 synref_compute_ref_yn p=",i4," e=",i8)', pcpsx_i_pel(), i_err
    !
1999 continue
    !
    !print'(" end synref_compute_ref_yn p=",i4," e=",i8)', pcpsx_i_pel(), i_err
    !
    call pcpsx_check_worker_errors ( i_err )
    !
    if ( i_err .ne. 0 ) stop
    !
    return
    !
997 continue
    !
    print'( &
    & /, " error in synref_compute_ref_yn ", &
    & /, " during modgrid_wr_binslice jy_ref=",i8 &
    & )', &
    jy_ref
    !
    go to 999
    !
998 continue
    !
    print'( &
    & /, " error in synref_compute_ref_yn ", &
    & /, " during velgrid_paint_by_obj_par jy_ref=",i8 &
    & )', &
    jy_ref
    !
    go to 999
    !
999 continue
    !
    print'( &
    & /, " error in synref_compute_ref_yn ", &
    & /, " path_ref_inp=", a &
    & )', &
    trim(r%path_ref_inp)
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine synref_compute_ref_yn 
  !
  subroutine synref_compute_ref_y1 ( r, jy_ref, ry_ref, v0_inp, v0_out )
    !
    ! add in the synref events 
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    integer,                  intent(in   ) :: jy_ref
    real,                     intent(in   ) :: ry_ref
    real,                     intent(in   ) :: v0_inp(:,:)
    real,                     intent(inout) :: v0_out(:,:)
    !
    integer                                 :: jx_ref ! ref x index
    real                                    :: rx_ref ! ref x value
    integer                                 :: jz_ref ! mig z index
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    !print*,' synref_compute_ref_y1 n0_dif=',r%n0_dif
    !print*,' synref_compute_ref_y1 amp_dif=',r%amp_dif(1:r%n0_dif)
    !print*,' synref_compute_ref_y1 x_loc_dif=',r%x_loc_dif(1:r%n0_dif)
    !print*,' synref_compute_ref_y1 y_loc_dif=',r%y_loc_dif(1:r%n0_dif)
    !print*,' synref_compute_ref_y1 dep_dif=',r%dep_dif(1:r%n0_dif)
    !print*,' synref_compute_ref_y1 n0_pik=',r%n0_pik
    !print*,' synref_compute_ref_y1 amp_pik=',r%amp_pik(1:r%n0_pik)
    !print*,' synref_compute_ref_y1 x_loc_pik=',r%x_loc_pik(1:r%n0_pik)
    !print*,' synref_compute_ref_y1 y_loc_pik=',r%y_loc_pik(1:r%n0_pik)
    !print*,' synref_compute_ref_y1 dep_pik=',r%dep_pik(1:r%n0_pik)
    !
    !call synref_print_event ( &
    !                          ' synref_compute_ref_y1 ', &
    !                          r%path_dif, r%hdr_x, r%hdr_y, &
    !                          r%n0_dif, r%amp_dif, r%dep_dif, &
    !                          r%x_loc_dif, r%y_loc_dif &
    !                        )
    !
    do_jx_ref : do jx_ref = 1, r%nx_ref
      !
      rx_ref = ( jx_ref - 1 ) * r%dx_ref + r%x0_ref
      !
      ! convert from velocity to reflectivity
      !
        v0_out ( 1,        jx_ref ) = 0.
      !
      do_jz_ref : do jz_ref = 2, r%nz_ref
        !
        v0_out ( jz_ref,   jx_ref ) = &
 2. * ( v0_inp ( jz_ref,   jx_ref ) &
      - v0_inp ( jz_ref-1, jx_ref ) ) &
    / ( v0_inp ( jz_ref,   jx_ref ) &
      + v0_inp ( jz_ref-1, jx_ref ) ) 
        !
!        v0_out ( jz_ref,   jx_ref ) = &
!  max ( v0_out ( jz_ref,   jx_ref ), 0. )
        !
      end do do_jz_ref 
      !
      if ( jx_ref .eq. 1 .and. jy_ref .eq. 1 ) &
      print'(" a ref ", &
      & 1x,i6,1x,i6,1x,i6,1x,f8.1,1x,f8.1,1x,f8.1,1x,g12.6,1x,g12.6)', &
      ( jy_ref, jx_ref, jz_ref, rx_ref, ry_ref, &
      ( jz_ref - 1 ) * r%dz_ref + r%z0_ref, &
      v0_inp ( jz_ref, jx_ref ), &
      v0_out ( jz_ref, jx_ref ), &
      jz_ref = 1 , r%nz_ref )
      !
      ! add reflectors
      !
      call synref_compute_ref_add_ref ( &
                                        r, &
                                        jx_ref, jy_ref, rx_ref, ry_ref, &
                                        v0_inp, v0_out &
                                      )
      !
      ! add diffractors
      !
      call synref_compute_ref_add_dif ( &
                    r, &
                    jx_ref, jy_ref, rx_ref, ry_ref, &
                    r%n0_dif, r%amp_dif, r%x_loc_dif, r%y_loc_dif, r%dep_dif, &
                    v0_inp, v0_out &
                                      )
      !
      ! add piks
      !
      call synref_compute_ref_add_dif ( &
                    r, &
                    jx_ref, jy_ref, rx_ref, ry_ref, &
                    r%n0_pik, r%amp_pik, r%x_loc_pik, r%y_loc_pik, r%dep_pik, &
                    v0_inp, v0_out &
                                      )
      !
      if ( jx_ref .eq. 1 .and. jy_ref .eq. 1 ) &
      print'(" b ref ", &
      & 1x,i6,1x,i6,1x,i6,1x,f8.1,1x,f8.1,1x,f8.1,1x,g12.6,1x,g12.6)', &
      ( jy_ref, jx_ref, jz_ref, rx_ref, ry_ref, &
      ( jz_ref - 1 ) * r%dz_ref + r%z0_ref, &
      v0_inp ( jz_ref, jx_ref ), &
      v0_out ( jz_ref, jx_ref ), &
      jz_ref = 1 , r%nz_ref )
      !
    end do do_jx_ref  
    !
    return
    !
  end subroutine synref_compute_ref_y1 
  !
  subroutine synref_compute_ref_add_ref ( r, &
                                          jx_ref, jy_ref, rx_ref, ry_ref, &
                                          v0_inp, v0_out )
    !
    ! add in the synref events 
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    integer,                  intent(in   ) :: jx_ref
    integer,                  intent(in   ) :: jy_ref
    real,                     intent(in   ) :: rx_ref
    real,                     intent(in   ) :: ry_ref
    real,                     intent(in   ) :: v0_inp(:,:)
    real,                     intent(inout) :: v0_out(:,:)
    !
    integer                                 :: i0_ref ! ref z index
    integer                                 :: jz_ref ! mig z index
    real                                    :: rz_ref ! mig z value
    real                                    :: v0_ref ! out ref value
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    !
    ! add reflectors
    !
    do_i0_ref : do i0_ref = 1 , r%n0_ref
      !
      r%i0_ref = i0_ref
      !
      rz_ref = r%dep_ref(i0_ref) &
    + rx_ref * r%dz_dx  (i0_ref) * r%rx_scl &
    + ry_ref * r%dz_dy  (i0_ref) * r%ry_scl 
      !
      jz_ref = nint ( ( rz_ref - r%z0_ref ) / r%dz_ref ) + 1
      !
      v0_ref = r%amp_ref(i0_ref)
      !
      if ( jz_ref .ge. 1 .and. jz_ref .le. r%nz_ref ) &
      v0_out ( jz_ref, jx_ref ) = &
      v0_out ( jz_ref, jx_ref ) &
    + r%amp_ref(i0_ref)
      !
      if ( jz_ref .ge. 1 .and. jz_ref .le. r%nz_ref ) &
      v0_ref = v0_out ( jz_ref, jx_ref ) 
      !
!print'(" ref ",i2,1x,i6,1x,i6,1x,i6,1x,f8.1,1x,f8.1,1x,f8.1,1x,g12.6)', &
!i0_ref, jy_ref, jx_ref, jz_ref, rx_ref, ry_ref, rz_ref, aw_ref 
      !
    end do do_i0_ref 
    !
    return
    !
  end subroutine synref_compute_ref_add_ref 
  !
  subroutine synref_compute_ref_add_dif ( &
                              r, &
                              jx_ref, jy_ref, rx_ref, ry_ref, &
                              n0_dif, amp_dif, x_loc_dif, y_loc_dif, dep_dif, &
                              v0_inp, v0_out &
                                         )
    !
    ! add in the synref events 
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    integer,                  intent(in   ) :: jx_ref
    integer,                  intent(in   ) :: jy_ref
    real,                     intent(in   ) :: rx_ref
    real,                     intent(in   ) :: ry_ref
    integer,                  intent(in   ) :: n0_dif
    real,                     intent(in   ) :: amp_dif(:)
    real,                     intent(in   ) :: x_loc_dif(:)
    real,                     intent(in   ) :: y_loc_dif(:)
    real,                     intent(in   ) :: dep_dif(:)
    real,                     intent(in   ) :: v0_inp(:,:)
    real,                     intent(inout) :: v0_out(:,:)
    !
    integer                                 :: i0_dif ! ref z index
    integer                                 :: jx_dif ! mig x index
    real                                    :: rx_dif ! mig x value
    integer                                 :: jy_dif ! mig y index
    real                                    :: ry_dif ! mig y value
    integer                                 :: jz_dif ! mig z index
    real                                    :: rz_dif ! mig z value
    real                                    :: v0_dif ! out dif value
    !
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! add diffractors
    !
    do_i0_dif : do i0_dif = 1 , n0_dif 
      !
      rx_dif  = x_loc_dif ( i0_dif ) / r%rx_scl
      !
      jx_dif = nint ( ( rx_dif - r%x0_ref ) / r%dx_ref ) + 1
      !
      ry_dif  = y_loc_dif ( i0_dif )  / r%ry_scl
      !
      jy_dif = nint ( ( ry_dif - r%y0_ref ) / r%dy_ref ) + 1
      !
      rz_dif  = dep_dif ( i0_dif ) 
      !
      jz_dif = nint ( ( rz_dif - r%z0_ref ) / r%dz_ref ) + 1
      !
      v0_dif = amp_dif ( i0_dif )
      !
      !if ( i_call .eq. 1 &
      !.or. i_call .eq. r%nx_ref/2+1 &
      !.or. i_call .eq. r%nx_ref ) &
      if ( jx_dif .eq. jx_ref .and. jy_dif .eq. jy_ref &
     .and. jz_dif .ge. 1      .and. jz_dif .le. r%nz_ref ) &
      print'(" add_dif i=",i6,1x,i6,1x,i6,1x,i6,1x, &
      & g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6 &
      & )', &
      i0_dif, jx_dif, jy_dif, jz_dif, x_loc_dif ( i0_dif ), &
      rx_dif, ry_dif, rz_dif, v0_dif
      !
      if ( jx_dif .eq. jx_ref .and. jy_dif .eq. jy_ref &
     .and. jz_dif .ge. 1      .and. jz_dif .le. r%nz_ref ) &
      v0_out ( jz_dif, jx_ref ) = &
      v0_out ( jz_dif, jx_ref ) &
    + v0_dif
      !
    end do do_i0_dif 
    !
    return
    !
  end subroutine synref_compute_ref_add_dif 
  !
  subroutine synref_event_add ( r, rt_ref, ra_ref, tr_out )
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    real,                     intent(in   ) :: rt_ref ! event time
    real,                     intent(in   ) :: ra_ref ! event amp
    real,                     intent(inout) :: tr_out(:) ! trace
    !
    integer                                 :: m0_sav
    integer, save                           :: j_call = 0
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    r%rt_fin = nint ( (   rt_ref - r%t0_fin ) / r%dt_fin ) &
                      * r%dt_fin + r%t0_fin
    !
    r%jt_fin = nint ( ( r%rt_fin - r%t0_fin ) / r%dt_fin ) + 1
    !
    ! add the amplitde into the trace
    !
    if ( r%jt_fin .ge. 1 .and. r%jt_fin .le. r%nt_fin ) &
    tr_out ( r%jt_fin ) = tr_out ( r%jt_fin ) + ra_ref
    !
    xxif_event_save : if ( r%n0_sav .lt. m0_sav .and. r%j0_ghost .eq. 1 ) then
      !
      if ( r%n0_sav .eq. 0 ) j_call = j_call + 1
      !
      r%n0_sav = r%n0_sav + 1
      !
      r%rt_sav( r%n0_sav ) = rt_ref
      !
      r%ra_sav( r%n0_sav ) = ra_ref
      !
      print'(1x,i8,1x,i8,1x,i8,1x,g12.6,1x,g12.6,1x,g12.6, &
      & " synref_event_save" )', &
      j_call, r%n0_sav, r%jt_fin, &
      r%rt_sav(r%n0_sav), r%ra_sav(r%n0_sav), &
      tr_out(r%jt_fin)
      !
    end if xxif_event_save 
    !
    return
    !
  end subroutine synref_event_add 
  !
  subroutine synref_event_print ( r, c_title, rx_tig, rx_gat, tr_inp, tr_tmp )
    !
    type ( synref_struct ),         pointer :: r ! synref structure
    character(len=*),         intent(in   ) :: c_title
    real,                     intent(in   ) :: rx_tig
    real,                     intent(in   ) :: rx_gat
    real,                     intent(in   ) :: tr_inp(:)
    real,                     intent(in   ) :: tr_tmp(:)
    !
    logical                                 :: set_crs 
    integer                                 :: nt_wid
    integer                                 :: jt_crs_1
    integer                                 :: jt_crs_2
    real                                    :: tr_inp_max 
    real                                    :: tr_tmp_max 
    integer                                 :: j0_sav
    integer                                 :: jt_crs
    integer, save                           :: i_call = 0
    !
    i_call = i_call + 1
    !
    doj0_sav : do j0_sav = 1 , r%n0_sav
      !
      jt_crs = nint ( ( r%rt_sav(j0_sav) - r%t0_crs ) / r%dt_crs ) + 1
      !
      set_crs = ( jt_crs .ge. 1 .and. jt_crs .le. r%nt_crs )
      !
      xxif_set_scl : if ( set_crs .and. .not. r%la_scl ) then
        !
        r%la_scl = .true.
        !
        r%ra_scl = tr_inp(jt_crs) / tr_tmp(jt_crs)
        !
        print'(" synref_event_print ra_scl=",g12.6,1x,g12.6,1x,g12.6)', &
        r%ra_scl, tr_inp(jt_crs), tr_tmp(jt_crs)
        !
        r%ra_scl = 1.
        !
      end if xxif_set_scl 
      !
      r%ra_scl = 0.460783 / 0.966983E-02 
      !
      nt_wid = 10
      xxif_set_crs : if ( set_crs ) then
        !
        jt_crs_1 = max ( jt_crs - nt_wid, 1 )
        !
        jt_crs_2 = max ( jt_crs + nt_wid, r%nt_crs )
        !
        tr_inp_max = maxval ( abs ( tr_inp ( jt_crs_1:jt_crs_2 ) ) )
        !
        tr_tmp_max = maxval ( abs ( tr_tmp ( jt_crs_1:jt_crs_2 ) ) )
        !
        print'(1x,i8,1x,i8,"a1",i8, &
        & 1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6, &
        & " synref_event_print",a )', &
        i_call, j0_sav, jt_crs, rx_gat, rx_tig, &
        r%rt_sav(j0_sav), r%ra_sav(j0_sav), &
        tr_inp_max, tr_tmp_max, tr_tmp_max * r%ra_scl, &
        trim ( c_title )
        !
      end if xxif_set_crs 
      !
    end do doj0_sav 
    !
    return
    !
  end subroutine synref_event_print 
  !
end module synref_module
!!---------------------------- end of module -----------------------------!!
!!---------------------------- end of module -----------------------------!!
!!---------------------------- end of module -----------------------------!!
