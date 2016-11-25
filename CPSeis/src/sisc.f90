!<CPS_v1 type="PROCESS"/>
!!------------------------------- sisc.f90 ---------------------------------!!
!!------------------------------- sisc.f90 ---------------------------------!!
!!------------------------------- sisc.f90 ---------------------------------!!


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
! Name       : SISC      (Static Increments by Stacked Correlations)
! Category   : statics
! Written    : 1989-03-28   by: Tom Stoeckley
! Revised    : 2007-04-24   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : 2D (and perhaps 3D) surface consistent residual statics process.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
!  Purpose:       This is a residual static program designed to
!                 derive surface consistent static files from
!                 2D (and possibly 3D) reflection data.
!
!  Method:        Unlike most residual statics processes, SISC is not
!                 a base-trace oriented statics process.  This process
!                 derives static increments by correlating from one
!                 ground position to the next, then integrates the
!                 resulting static file.  This is in contrast to the
!                 more usual approach of correlating to a base (or pilot)
!                 trace centered at the same CDP location.
!
!  Advantages:    Large statics or static discontinuities may be more
!                 successfully detected by this approach.  This
!                 approach may also help when the base traces are
!                 badly damaged by the static.
!
!  Limitations:   This process is inherently a 2D process and therefore
!                 has historically been applied only to 2D data.  However,
!                 this process can be used on 3D data because it operates
!                 on each 2D line in the 3D dataset separately.  Because
!                 of the nature of this type of static solution, it has
!                 not been determined whether generalization to 3D is
!                 advisable.
!
!  Editing:       This process outputs stacked correlation files in
!                 addition to the statics files.  The correlation files
!                 can be viewed in CBYT in order to help edit the statics
!                 files.  The statics files can also be edited in ISEP
!                 or MSEPITA.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
! SISC Operation Summary:
!
!   1. Correlate each trace in a shot profile with the trace, having the same
!      receiver, in the next shot profile.  Changes in timing here cannot be
!      due to receiver statics (since the receivers are the same) but must be
!      due to the difference in the source static between the two sources.
!
!   2. Composite the correlations between individual traces in the two shot
!      profiles and pick the time shift from the composited correlation.
!      This is the static increment (difference in the source statics) between
!      these two sources.
!
!   3. Repeat steps 1 and 2 for all shot profiles to find the source static
!      increment for all the source locations.
!
!   4. Starting at the beginning of the line, integrate (form the running sum
!      of) the source static increments to generate the actual static for each
!      source location.
!
!   5. Remove a user specified running average from the source static sequence.
!      This amounts to a low-cut spatial frequency filter to remove the
!      undesirable long wavelength components produced by integrating noise
!      fluctuations.  (The running average length is usually approximately half
!      the maximum offset.)
!
!   6. Repeat steps 1 - 5 exchanging the word "source" (or "shot") with
!      "receiver" to find the receiver static for all the receiver locations.
!
!
! SISC can partially composite traces prior to the correlation step:
!
! For a source static solution, several adjacent traces with the same source
! will be composited over a range of receiver ground positions, and then
! correlated with traces from the next source which were composited over an
! identical range of receiver ground positions.  The correlations are then
! composited and picked to form the static increment as previously described.
! (Compositing for the receiver static solution proceeds similarly.)
!
! This compositing step helps attenuate energy that is not flat with offset,
! such as linear interference and multiples, and may also help the solution
! when random noise is a problem.  Five CMPs is a typical number to composite
! and is helpful even on good data.
!
!
! Determining Profiles to be Correlated:
!
! SISC will always correlate to the next available ground position,
! automatically jumping over unoccupied ground positions, or ground positions
! to which correlations cannot be made.  If correlations are contaminated
! because of overlapping source or receiver arrays, the user can avoid
! correlating traces that were mixed during acquisition by simply deleting
! or flagging traces (with the SELECT process) for every other ground position
! before running SISC.
!
!
! Running Average Removal:
!
! SISC grades through skips and nils in the integrated static file prior to
! calculating the running average.  For group files, the running average is
! calculated from group numbers.  The graded ends method is used for
! calculating the running average at the ends of the line.
!
! When editing stacked correlations in CBYT, ISEP must be used to integrate the
! static increment file and remove a running average.
!
!
! Editing Stacked Correlations:
!
! A CBYT option allows the bytefile containing stacked correlations to be
! plotted and edited easily.  This option automatically updates the static
! increment file based on correlation edits.
!
! The static increment file can be integrated and a running average removed
! with the ISEP or MSEPITA utility to form the actual static file that can
! be applied with the SHFT process.  ISEP or MSEPITA can easily be used to
! test the effect of different running averge lengths.
!
!
! Reference:
!
! Disher, D. A. and Naquin, P. J., 1970, Statistical Automatic Statics Analysis:
! Geophysics, v. 35, p. 574 - 585.
!
!
! 3D Work:
!
! SISC can be used on 2D or 3D data, although on 3D data it must operate on
! each line separately.  The process CS3D is designed specifically for 3D work
! and has an algorithm similar to SISC.
!
!
! Binning Correlation Ground Positions:
!
! Setting BIN_CORR_SRC and/or BIN_CORR_REC greater than one bins the
! correlation header words.  This can facilitate a solution when the
! correlation header words for adjacent ground positions do not match.
! An example would be when adjacent sources have alternate receiver ground
! positions.
!
!
! Using SISC Before NMO is Applied:
!
! SISC can be run before velocity analysis or before NMO is applied to the data
! if the HDR_CORR parameters are set to one of the offset options - signed
! offset (SOFF) or absolute offset (AOFF).  In this case, NUM_COMP should be
! kept small, typically less than 5.
!
!
! Advantages of SISC
!
!   1. SISC is excellent at solving for large and abrupt statics.
!
!   2. Because SISC composites correlations before picking (and optionally
!      composites before correlating) it works better on noisy data than
!      industry standard automatic statics processes.
!
!   3. SISC allows stacked correlations to be displayed and edited.  This
!      is essential for virtually all data.
!
!   4. The SISC static solution contains usable components at longer
!      wavelengths than most other residual statics processes.
!
!
! Disadvantages of SISC
!
!   1. Choosing the running average and removing it is an extra step.
!      Structure can contaminate the solution if too large a running average
!      is used.
!
!   2. Editing the correlations is often not obvious.
!
!   3. SISC statics are not as precise as FISH or IMS.  SISC is always
!      followed by other statics processes.
!
!   4. SISC is of limited value on very poor data.
!
!-------------------------------------------------------------------------------
!                     OPTIONS NO LONGER AVAILABLE
!
! The FORREV option in the old CPS system allowed SISC to display separately,
! in the correlation file, correlations for receivers forward of the source
! and reverse of the source, for diagnostic purposes (for split-spread
! acquisition only).  This option has not been implemented in the upgraded
! CPS system, but can be mimicked by simply deleting or flagging traces
! (with the SELECT process) for one side of the spread before running SISC.
! This procedure also has the advantage that a static file will be derived
! from just one side of the spread, a capability that the old FORREV option
! did not have.
!
! The LEAP option in the old CPS system allowed SISC to display separately,
! in the correlation file, correlations leapfrogging over ground position,
! for diagnostic purposes.  This option has not been implemented in the
! upgraded CPS system, but can be mimicked by simply deleting or flagging
! traces (with the SELECT process) for every other ground position before
! running SISC.  This procedure also has the advantage that a static file
! will be derived from every other ground position, a capability that the
! old LEAP option did not have.
! 
! The NINC options in the old CPS system allowed SISC to generate a static
! file in which correlations leapfrog over ground positions, similar to the
! LEAP option described above.  This option was useful to avoid correlating
! traces which were mixed during acquisiton due to overlapping source or
! receiver arrays.  The resulting static file, however, was not very useful
! because it could not be correctly integrated because of the overlapping
! static increments.  Such a file also was often faulty because of decoupling
! between the even and odd ground positions.  This option has not been
! implemented in the upgraded CPS system, but can be safely mimicked in the
! same manner as the LEAP option.
!
! The NBIN options in the old CPS system provided two capabilities for SISC.
! One of these was to composite ground positions prior to solving the statics.
! This option could be dangerous because of the smearing of static increments
! which often are very different from one ground position to the next.  This
! option has not been implemented in the upgraded CPS system, but can be
! mimicked by simply compositing ground positions before running SISC.  This
! procedure also has the advantage that the user has greater control over how
! the compositing is done.
!
! The other capability provided by the NBIN options in the old CPS system
! was to allow binning the correlation header words.  This can facilitate a
! solution when the correlation header words for adjacent ground positions
! do not match.  An example would be when adjacent sources have alternate
! receiver ground positions.  This capability is retained in the upgraded
! CPS system with the keywords BIN_CORR_SRC and BIN_CORR_REC.
!
! The RA_SRC and RA_REC parameters for the running average length allowed
! a value of 0 in the old CPS system, which then defaulted to half of the
! maximum offset.  Because of the increased number of header word options
! for static file description, and the more accurate way the offsets are
! saved in the upgraded CPS system, it is no longer possible to correlate
! the offset lengths with the static header words, and therefore the optional
! value of 0 is no longer available.  The user must supply a reasonable
! value for the running average in units of the ground position header
! word in the direction of the source or receiver acquisition line.  The
! recommended value remains half of the maximum offset.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! This process is an all-trace (loop-splitting) process.
! This process allows traces to be input in gathers or one at a time.
! This process allows traces to be input in any sort order.
! Traces should have been expanded with XP or MVXP or equivalent.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! If DEAD_END = YES, this process does not output any traces.
! If DEAD_END = NO, this process outputs the same traces it received,
! one at a time, shifted by the statics solution, in the same order it
! received them.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       input unused; set to 1.
! GATHERED  whether traces are a legitimate gather  input unused; set to false.
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
! Hwd#       Description                       Action taken
! ----       -----------                       ------------
!  6         offset                            used but not changed
!  7         CMP X grid coordinate             used but not changed
!  8         CMP Y grid coordinate             used but not changed
! HDR_FLAG   trace flag                        used but not changed
! HDR_SX     source X ground position          used but not changed
! HDR_SY     source Y ground position          used but not changed
! HDR_RX     source X ground position          used but not changed
! HDR_RY     source Y ground position          used but not changed
! WIN_HDR_X  first coord of window location    used but not changed
! WIN_HDR_Y  second coord of window location   used but not changed
!  2         top mute index                    used (updated if DEAD_END is NO)
! 64         bottom mute index                 used (updated if DEAD_END is NO)
! 25         largest absolute value                 (updated if DEAD_END is NO)
! 43         cumulative residual static                 (set if DEAD_END is NO)
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
! 38. 2007-04-24  Stoeckley  Never set NUMTR to zero.
! 37. 2006-12-04  D. Glover  Added NULLIFY statements for Intel compiler.
! 36. 2001-10-18  Stoeckley  Add file selection boxes and file status messages.
! 35. 2001-05-14  Stoeckley  Change wrapup flag.
! 34. 2000-10-20  Stoeckley  Documentation change only to clarify the definition
!                             of the NUM_COMP parameter.
! 33. 2000-08-08  Stoeckley  Minor documentation change regarding header words
!                             7, 8, WIN_HDR_X, and WIN_HDR_Y.
! 32. 2000-07-20  Stoeckley  Removed one line of code which tested the ERROR
!                             variable before it was defined.
! 31. 2000-06-23  Stoeckley  Converted from old system.
! 30. 1998-12-15  Vunderink  Begin using the f90 compiler.
! 29. 1997-09-02  Vunderink  Fixed bug in SISC_READ whenever NHD=2.
! 28. 1997-08-13  Vunderink  Fixed bug in spatial window which was causing
!                             the traces to be windowed on output
! 27. 1997-08-11  Vunderink  Added spatial correlation window
! 26. 1997-03-12  Vunderink  Fixed bug caused by SISC1 common not being
!                             updated in SISC_WRITE whenever PRNT was added.
! 25. 1997-03-05  Vunderink  Added PRNT keyword.
! 24. 1996-09-26  Vunderink  Added RLSTAPE and removed SORTDIR keywords.
! 23. 1995-11-02  Vunderink  The program will now determine the number of
!                             bits required to store the sequential trace
!                             number.  If the number of bits is less than
!                             21, then packed word for each trace is still
!                             split into 3 equal part of 21 bits each.
!                             Otherwise, the required number of bits are
!                             allocated to store the trace number and the
!                             remaining bits are split into 2 equal parts.
! 22. 1995-09-26  Vunderink  Add SORTDIR, STOP, and BYTSAV keywords
! 21. 1994-05-25  Troutt     Save lun1 for now (common later) so that file
!                             can be successfully removed from $SORTDIR
! 20. 1994-05-20  Troutt     Add SORTDIR logic (partial).  Have not yet
!                             put SORTDIR in the DCODE list - it is hard-
!                             wired to YES for the YK Survey.
! 19. 1994-02-11  Troutt     Add error check for HPALLOC call.
! 18. 1993-07-21  Troutt     "Unfix" minor problem in SISCINT that didn't
!                             preserve last nil (see last update).  I had
!                             made the "fix" in order to resolve differences
!                             between SISC and ISEP.  It turns out that ISEP
!                             was in error, but ISEP has now been fixed.
! 17. 1993-06-18  Troutt     Fix minor problem in SISCINT that didn't pre-
!                             serve last nil.  Also saved NRUN in SISCF1 so
!                             SISCF3 could print it out.  Added 2 new DCODE
!                             parameters, #RUN1 and #RUN2 to be used for
!                             FILE1 and FILE2.  The old #RUN parameter was
!                             deactivated (if an old job comes in w- #RUN,
!                             it is used to set #RUN1 and #RUN2).  The old
!                             parameter ORDER was also deactivated.  It was
!                             never really supported - ORDER=NO was always
!                             used!  A new comment card was added to the
!                             static file showing the actual #points used
!                             in the running average calculation.
! 16. 1993-03-23  Troutt     Add PATH parameter to allow stacked correla-
!                             tions to be put into BYTE file(s).
! 15. 1992-10-30  Troutt     Change running average removal for group
!                             files (hw 9) by removing "fancy" logic that
!                             related groups to source ground positions.
!                             This change was made in SISCFAC.
! 14. 1992-02-25  Troutt     Add logic for tail mute HW64 by updating
!                             primitive FISIUTIL (routine FISISHFT).
! 13. 1991-03-22  Troutt     Save INC in SISCP1 so SISCP2 can use it.
! 12. 1991-01-07  Stoeckley  Change running average to correspond to
!                             documentation.  Previously was number of
!                             points rather than number of ground positions.
! 11. 1990-10-23  Peterson   Include error and abort arguments on calls to
!                             HPALLOC and HPDEALLC.
! 10. 1990-07-23  Stoeckley  Fix failed static bug introduced in revis #8.
!  9. 1990-07-18  Stoeckley  Fix shotpoint bug introduced in revision #8.
!  8. 1990-06-19  Stoeckley  Replace some routines by calls to FISIUTIL
!                             routines.
!  7. 1989-06-06  Stoeckley  Fix bug in static file application when FORREV
!                             is set to yes.
!  6. 1989-06-02  Stoeckley  Change SISCSORT to better version.
!  5. 1989-05-26  Stoeckley  Add if-check in SISCSORT to prohibit operand
!                             range error which sometimes occurs when not
!                             compiled with DEBUG option.
!  4. 1989-05-25  Stoeckley  Add check for dead traces.
!  3. 1989-05-15  Stoeckley  Change use of parameter HF#.
!  2. 1989-05-08  Stoeckley  Change MXSH and MXPK from seconds to millisecs.
!  1. 1989-03-28  Stoeckley  Original Version.
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
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed (unknown).
! NSTORE           0       amount of permanent memory needed (unknown).
! IFTD           true      whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large (unknown).
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces (DEAD_END = NO).
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces (DEAD_END = NO).
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
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS SISC Process/NC=80>
!
!<include stathelper.f90>
!
!                     Static Calculation Parameters
! `---------------------------------------------------------------------------
!                MAX_STATIC =`FFFFF ms      ICC_MIN=`FFFF
!                NUM_COMP~~~=`III           SCC_MIN=`FFFF
!
!   [/L]Source file:    RA_SRC=`IIII   HDR_CORR_SRC=`CCCC   BIN_CORR_SRC=`I
!   [/L]Receiver file:  RA_REC=`IIII   HDR_CORR_REC=`CCCC   BIN_CORR_REC=`I
! `---------------------------------------------------------------------------
!
!<NS Correlation Window Specifications/NC=80>
!<include latwin.f90>
!
!<PARMS Correlation Window Specifications [screen2]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!
!<Help KEYWORD="MAX_STATIC">
!<Tip> Maximum static shift (milliseconds). </Tip>
! Default = 40.0
! Allowed = real > 0.0
!
! This is the maximum static increment that is solved for.  The final
! integrated static, particularly when using long running averages, will
! probably exceed this value for some ground positions.
!</Help>
!
!
!<Help KEYWORD="NUM_COMP">
!<Tip> Max number of ground positions to composite prior to correlating. </Tip>
! Default = 5 
! Allowed = int >= 1
!
! Partial composite over ground positions prior to correlation reduces the
! effects of random and linear noise.  Some partial composite prior to
! correlation is beneficial even on good data.
!
! For example: For a source file solution, several traces with the same
! source ground position will be composited over several receiver ground
! positions (or over several CMPs or offsets depending on the choice for
! HDR_CORR_SRC), and then correlated with the next source ground position
! which is composited over the same receiver ground positions.  This helps
! attenuate energy that is not flat with offset, such as linear interference
! or multiples, and may also help the solution when random noise is a problem.
!
! NOTE: The maximum ground position range to be composited is NUM_COMP times
! the ground position increment which is occupied by traces.  Normally,
! NUM_COMP is the number of traces composited, but can be larger or smaller
! with irregular data acquisition (e.g. duplicate shots or skips).
!
! WARNING: This NUM_COMP parameter differs slightly from the corresponding
! parameter in the old Cray Processing System.  In the old system, this
! parameter specified a ground position range rather than the number of
! occupied ground positions.  Therefore, to get a certain level of compositing
! in the old system required adjusting this parameter based on the occupied
! ground position increment.  For example, if the ground position increment
! was 4, this parameter would have to be set to 20 to get a 5-fold composite.
! Now, NUM_COMP can be set to 5 to get a 5-fold composite regardless of the
! ground position increment.
!</Help>
!
!
!<Help KEYWORD="ICC_MIN">
!<Tip> Minimum individual correlation coefficient to use. </Tip>
! Default = 0.0
! Allowed = 0.0 <= real < 1.0
!
! If the correlation coefficient for an individual correlation is less than
! ICC_MIN, then that correlation will not be added to the stacked correlation.
!</Help>
!
!
!<Help KEYWORD="SCC_MIN">
!<Tip> Minimum stacked correlation coefficient to use. </Tip>
! Default = 0.0
! Allowed = 0.0 <= real < 1.0
! 
! If the correlation coefficient for a stacked correlation is less than
! SCC_MIN, then the static value will be set to nil.
!</Help>
!
!
!<Help KEYWORD="HDR_CORR_SRC">
!<Tip> Header word to define source static correlations. </Tip>
! Default = REC
! Allowed = REC   (correlate traces with common receiver)
! Allowed = CMP   (correlate traces with common midpoint)
! Allowed = SOFF  (correlate traces with common signed offset)
! Allowed = AOFF  (correlate traces with common absolute offset)
!</Help>
!
!
!<Help KEYWORD="HDR_CORR_REC">
!<Tip> Header word to define receiver static correlations. </Tip>
! Default = SRC
! Allowed = SRC   (correlate traces with common source)
! Allowed = CMP   (correlate traces with common midpoint)
! Allowed = SOFF  (correlate traces with common signed offset)
! Allowed = AOFF  (correlate traces with common absolute offset)
!</Help>
!
!
!<Help KEYWORD="RA_SRC">
!<Tip> Length of running average to remove from source static file. </Tip>
! Default = 21
! Allowed = int >= 5
!
! Length of running average, in ground position units in the source
! acquisition line direction, to remove from the source static file.
!
! It is recommended that RA_SRC be set to the source ground position range
! corresponding to half of the maximum offset.  This should be the number
! of total ground positions, not the number of occupied ground positions,
! corresponding to the desired running average distance.
!
! For example, if the separation between adjacent source ground positions
! along a source acquisition line is 100 meters, and the maximum offset is
! 4000 meters, RA_SRC should normally be set to 0.5 * 4000 / 100 = 20.
! This is true whether or not every ground position is occupied by a source.
!
! If RA_SRC == 20:
!  (1) If every ground position is occupied, the running average will
!       contain 21 points.
!  (2) If only every other ground position is occupied, the running average
!       will contain 11 points.
!  (3) The actual number of points in a running average will always be odd
!       for the sake of symmetry.
!</Help>
!
!
!<Help KEYWORD="RA_REC">
!<Tip> Length of running average to remove from receiver static file. </Tip>
! Default = 21
! Allowed = int >= 5
!
! Length of running average, in ground position units in the receiver
! acquisition line direction, to remove from the receiver static file.
!
! It is recommended that RA_REC be set to the receiver ground position range
! corresponding to half of the maximum offset.  This should be the number
! of total ground positions, not the number of occupied ground positions,
! corresponding to the desired running average distance.
!
! For example, if the separation between adjacent receiver ground positions
! along a receiver acquisition line is 100 meters, and the maximum offset is
! 4000 meters, RA_REC should normally be set to 0.5 * 4000 / 100 = 20.
! This is true whether or not every ground position is occupied by a receiver.
!
! If RA_REC == 20:
!  (1) If every ground position is occupied, the running average will
!       contain 21 points.
!  (2) If only every other ground position is occupied, the running average
!       will contain 11 points.
!  (3) The actual number of points in a running average will always be odd
!       for the sake of symmetry.
!</Help>
!
!
!<Help KEYWORD="BIN_CORR_SRC">
!<Tip> Range to bin source correlation headers prior to correlating. </Tip>
! Default = 1
! Allowed = int >= 1
!
! Setting BIN_CORR_SRC > 1 bins the correlation header words.  This can
! facilitate a solution when the correlation header words for adjacent
! source ground positions do not match.  An example would be when adjacent
! sources have alternate receiver ground positions.
!</Help>
!
!
!<Help KEYWORD="BIN_CORR_REC">
!<Tip> Range to bin receiver correlation headers prior to correlating. </Tip>
! Default = 1
! Allowed = int >= 1
!
! Setting BIN_CORR_REC > 1 bins the correlation header words.  This can
! facilitate a solution when the correlation header words for adjacent
! receiver ground positions do not match.  An example would be when adjacent
! receivers have alternate source ground positions.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module sisc_module
      use pc_module
      use named_constants_module
      use stathelper_module
      use mth_module
      implicit none
      private
      public :: sisc_create
      public :: sisc_initialize
      public :: sisc_update
      public :: sisc_delete
!<execute_only>
      public :: sisc
      public :: sisc_wrapup
!</execute_only>


      character(len=100),public,save :: SISC_IDENT = &
       '$Id: sisc.f90,v 1.38 2007/04/25 15:46:23 Stoeckley beta sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: sisc_struct              
 
        private
        logical                         :: skip_wrapup       ! wrapup flag.
        real                            :: max_static        ! process params
        integer                         :: num_comp          ! process params
        real                            :: icc_min           ! process params
        real                            :: scc_min           ! process params
        character(len=8)                :: hdr_corr_src      ! process params
        character(len=8)                :: hdr_corr_rec      ! process params
        integer                         :: ra_src,ra_rec     ! process params
        integer                         :: bin_corr_src      ! process params
        integer                         :: bin_corr_rec      ! process params
        type(stathelper_struct),pointer :: helper            ! dependent
        integer                         :: nbins(2)          ! dependent

      end type sisc_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(sisc_struct),pointer,save :: object      ! needed for traps.

      integer,parameter,private :: HDR_CORR_SRC_NOPT = 4
      integer,parameter,private :: HDR_CORR_REC_NOPT = 4

      character(len=8),save,private :: hdr_corr_src_options(HDR_CORR_SRC_NOPT)
      character(len=8),save,private :: hdr_corr_rec_options(HDR_CORR_REC_NOPT)

      data      hdr_corr_src_options /'REC','CMP','SOFF','AOFF'/
      data      hdr_corr_rec_options /'SRC','CMP','SOFF','AOFF'/

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine sisc_create (obj)
      implicit none
      type(sisc_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify (obj%helper) ! jpa

      call stathelper_create    (obj%helper, 'SISC')
      call sisc_initialize      (obj)
      return
      end subroutine sisc_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine sisc_delete (obj)
      implicit none
      type(sisc_struct),pointer :: obj       ! arguments

!<execute_only>
      call sisc_wrapup (obj)
!</execute_only>

      call stathelper_delete (obj%helper)

      deallocate(obj)
      return
      end subroutine sisc_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine sisc_initialize (obj)
      implicit none
      type(sisc_struct),intent(inout) :: obj       ! arguments

      obj%max_static   = 40.0          ! was mxpk
      obj%num_comp     = 5             ! was ncomp
      obj%icc_min      = 0.0           ! was mncc
      obj%scc_min      = 0.0           ! was mnfc
      obj%hdr_corr_src = 'REC'         ! was hcorr(2)
      obj%hdr_corr_rec = 'SRC'         ! was hcorr(2)
      obj%ra_src       = 21            ! was nrun(2)
      obj%ra_rec       = 21            ! was nrun(2)
      obj%bin_corr_src = 1             ! was nbin(2)
      obj%bin_corr_rec = 1             ! was nbin(2)

      call stathelper_initialize (obj%helper)
      call sisc_update           (obj)
      return
      end subroutine sisc_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine sisc_update (obj)
      implicit none
      type(sisc_struct),intent(inout),target :: obj               ! arguments
      logical                                :: error             ! local
      integer                                :: ifile             ! local
      integer         ,parameter             :: NUM_ITER = 1      ! local
      real            ,parameter             :: CONVERGE = 0.0    ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_get        ('max_static'   , obj%max_static   )
      call pc_get        ('num_comp'     , obj%num_comp     )
      call pc_get        ('icc_min'      , obj%icc_min      )
      call pc_get        ('scc_min'      , obj%scc_min      )
      call pc_get        ('hdr_corr_src' , obj%hdr_corr_src )
      call pc_get        ('hdr_corr_rec' , obj%hdr_corr_rec )
      call pc_get        ('ra_src'       , obj%ra_src       )
      call pc_get        ('ra_rec'       , obj%ra_rec       )
      call pc_get        ('bin_corr_src' , obj%bin_corr_src )
      call pc_get        ('bin_corr_rec' , obj%bin_corr_rec )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%hdr_corr_src /= 'CMP'  .and. &
          obj%hdr_corr_src /= 'SOFF' .and. &
          obj%hdr_corr_src /= 'AOFF') obj%hdr_corr_src = 'REC'

      if (obj%hdr_corr_rec /= 'CMP'  .and. &
          obj%hdr_corr_rec /= 'SOFF' .and. &
          obj%hdr_corr_rec /= 'AOFF') obj%hdr_corr_rec = 'SRC'

      call mth_constrain (obj%max_static   , 1.0, 999.0)
      call mth_constrain (obj%num_comp     ,   1,   999)
      call mth_constrain (obj%icc_min      , 0.0,   0.9)
      call mth_constrain (obj%scc_min      , 0.0,   0.9)
      call mth_constrain (obj%ra_src       ,   5,  9999)
      call mth_constrain (obj%ra_rec       ,   5,  9999)
      call mth_constrain (obj%bin_corr_src ,   1,  9999)
      call mth_constrain (obj%bin_corr_rec ,   1,  9999)

      call stathelper_update (obj%helper,                              &
                              NUM_ITER, obj%max_static, CONVERGE,      &
                              obj%ra_src, obj%ra_rec,                  &
                              obj%hdr_corr_src, obj%hdr_corr_rec,      &
                              error)

      do ifile = 1,2
           if (stathelper_file_is_source(obj%helper,ifile)) then
                obj%nbins(ifile) = obj%bin_corr_src
           else if (stathelper_file_is_receiver(obj%helper,ifile)) then
                obj%nbins(ifile) = obj%bin_corr_rec
           else
                obj%nbins(ifile) = 1
           end if
      end do


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field &
                ('hdr_corr_src', hdr_corr_src_options, hdr_corr_src_nopt)
      call pc_put_options_field &
                ('hdr_corr_rec', hdr_corr_rec_options, hdr_corr_rec_nopt)

      call pc_put        ('max_static'   , obj%max_static   )
      call pc_put        ('num_comp'     , obj%num_comp     )
      call pc_put        ('icc_min'      , obj%icc_min      )
      call pc_put        ('scc_min'      , obj%scc_min      )
      call pc_put        ('hdr_corr_src' , obj%hdr_corr_src )
      call pc_put        ('hdr_corr_rec' , obj%hdr_corr_rec )
      call pc_put        ('ra_src'       , obj%ra_src       )
      call pc_put        ('ra_rec'       , obj%ra_rec       )
      call pc_put        ('bin_corr_src' , obj%bin_corr_src )
      call pc_put        ('bin_corr_rec' , obj%bin_corr_rec )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine sisc_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!




!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine sisc (obj,ntr,hd,tr)
      implicit none
      type(sisc_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments
      logical                         :: error                  ! local

      if(ntr /= NEED_TRACES) then
           call stathelper_input_traces (obj%helper,ntr,hd,tr)
           if (ntr == NEED_TRACES) return
      end if

      if (ntr == NO_MORE_TRACES) then
           call sisc_solve (obj,error)
           if (error) then
                ntr = FATAL_ERROR
           else
                ntr = NEED_TRACES
           end if
      end if

      if (ntr == NEED_TRACES .or. ntr == FATAL_ERROR) then
           call sisc_wrapup (obj)
           call stathelper_output_traces (obj%helper,ntr,hd,tr)
      end if
      return
      end subroutine sisc


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine sisc_wrapup (obj)
      implicit none
      type(sisc_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine sisc_wrapup


!!-------------------------- sisc solve ------------------------------------!!
!!-------------------------- sisc solve ------------------------------------!!
!!-------------------------- sisc solve ------------------------------------!!


      subroutine sisc_solve (obj,error)
      implicit none
      type(sisc_struct),intent(inout) :: obj                    ! arguments
      logical          ,intent(out)   :: error                  ! arguments
      integer                         :: nfiles,nwin,ncorr      ! local
      integer                         :: ifile                  ! local
      logical                         :: converged              ! local

      nfiles = stathelper_get_nfiles (obj%helper)
      nwin   = stathelper_get_nwin   (obj%helper)
      ncorr  = stathelper_get_ncorr  (obj%helper)

      call stathelper_begin_iteration (obj%helper,error)
      if (error) return

      do ifile = 1,nfiles

           call stathelper_begin_file_iteration (obj%helper,ifile,error)
           if (error) return

           call sisc_algorithm (obj,ifile,nwin,ncorr,error)
           if (error) then
                call pc_error ('SISC: error executing solution algorithm')
                return
           end if

           call stathelper_end_file_iteration (obj%helper,ifile,error)
           if (error) return

      end do

      call stathelper_end_iteration (obj%helper,converged,error)
      if (error) return

      error = .false.
      return
      end subroutine sisc_solve


!!------------------------- sisc algorithm ---------------------------------!!
!!------------------------- sisc algorithm ---------------------------------!!
!!------------------------- sisc algorithm ---------------------------------!!


      subroutine sisc_algorithm (obj,ifile,nwin,ncorr,error)
      implicit none
      type(sisc_struct)   ,intent(inout) :: obj                     ! arguments
      integer             ,intent(in)    :: ifile,nwin,ncorr        ! arguments
      logical             ,intent(out)   :: error                   ! arguments
      integer                            :: nnc,nnt,icr,icr2,keep   ! local
      integer                            :: igp2,igp,ngp,istop,ib12 ! local
      integer                            :: itrace,ntraces,ikeep    ! local
      integer                            :: itrace2,itest,inc2,inc3 ! local
      integer                            :: inc,itest2,nbins        ! local
      real                               :: denom2,denom            ! local
      real                               :: stat,ccoef              ! local
      real                               :: ttt(nwin)               ! local
      real                               :: aaa(nwin)               ! local
      real                               :: bbb(nwin)               ! local
      real                               :: ccc(ncorr)              ! local
      real                               :: ddd(ncorr)              ! local

!----------get started.

      call stathelper_sort_to_file (obj%helper,ifile)

      ntraces = stathelper_get_ntraces (obj%helper)
      ngp     = stathelper_get_ngp     (obj%helper,ifile)
      itrace  = 1
 !!!  inc     = obj%src_inc or obj%rec_inc
      inc     = 1           !!!!!!!!!!!!!!  temporary       
 !!!! if (obj%leap) inc = inc + 1
      nbins   = obj%nbins(ifile)

!----------go through the loop.

      do igp = 1,ngp
           inc2   = inc
           ikeep  = itrace
11         nnt    = 0
           nnc    = 0
           ddd(:) = 0.0
           denom  = 0.0
           igp2   = igp

!----------find starting trace for current ground position.

           itrace2 = ntraces + 1
           do
                if (itrace > ntraces) go to 40
                itest = stathelper_get_igp   (obj%helper,itrace)
                icr   = stathelper_get_icorr (obj%helper,itrace)
                icr   = nbins * (icr / nbins)
                if (itest >  igp) go to 40
                if (itest == igp) exit
                itrace = itrace + 1
           end do

!----------find starting trace for incremented ground position.

           itrace2 = itrace + 1
           do inc3 = 1,inc2
                do
                      if (itrace2 > ntraces) go to 40
                      itest = stathelper_get_igp   (obj%helper,itrace2)
                      icr2  = stathelper_get_icorr (obj%helper,itrace2)
                      icr2  = nbins * (icr2 / nbins)
                      if (itest > igp2) then
                           igp2 = itest
                           exit
                      end if
                      itrace2 = itrace2 + 1
                end do
           end do

!----------find traces to composite.

18         ib12 = 0
           aaa(:) = 0.0
           bbb(:) = 0.0
22         if (icr2 < icr) go to 77
           if (icr2 > icr) go to 76
           if (ib12 == 0) istop = icr + obj%num_comp - 1
           if (icr > istop) go to 36
           ib12 = 1
           keep = icr

!----------add to first composited trace.

24         continue
           call stathelper_read_shifted_window &
                                        (obj%helper,itrace,ttt,error)
           if (error) return
           aaa(:) = aaa(:) + ttt(:)
           nnt = nnt + 1
           itrace   = itrace + 1
!!!        if (itrace > ntraces) go to 26
           itest = stathelper_get_igp   (obj%helper,itrace)
           icr   = stathelper_get_icorr (obj%helper,itrace)
           icr   = nbins * (icr / nbins)
           if (itest == igp .and. icr == keep) go to 24

!----------add to second composited trace.

26         continue
           call stathelper_read_shifted_window &
                                        (obj%helper,itrace2,ttt,error)
           if (error) return
           bbb(:) = bbb(:) + ttt(:)
           nnt = nnt + 1
           itrace2  = itrace2 + 1
           if (itrace2 > ntraces) go to 35
           itest2 = stathelper_get_igp   (obj%helper,itrace2)
           icr2   = stathelper_get_icorr (obj%helper,itrace2)
           icr2   = nbins * (icr2 / nbins)
           if (itest2 == igp2 .and. icr2 == keep) go to 26

!----------go back to get more traces to add.

!!!        if (itrace > ntraces) go to 35
           if (itest > igp .or. itest2 > igp2) go to 35
           go to 22

!----------move to next trace of current ground position.

76         itrace = itrace + 1
!!!        if (itrace > ntraces) go to 34
           itest = stathelper_get_igp   (obj%helper,itrace)
           icr   = stathelper_get_icorr (obj%helper,itrace)
           icr   = nbins * (icr / nbins)
           if (itest > igp) go to 34
           go to 22

!----------move to next trace of incremented ground position.

77         itrace2 = itrace2 + 1
           if (itrace2 > ntraces) go to 34
           itest = stathelper_get_igp   (obj%helper,itrace2)
           icr2  = stathelper_get_icorr (obj%helper,itrace2)
           icr2  = nbins * (icr2 / nbins)
           if (itest > igp2) go to 34
           go to 22

!----------correlate the composited traces.

34         if (ib12 == 0) go to 40
35         ib12 = -1
36         continue
           call stathelper_corr (obj%helper,bbb,aaa,ccc,denom2)

!----------decide whether to use this correlation.

           call stathelper_pick &
                        (obj%helper,ccc,obj%icc_min,denom2,  stat,ccoef)
           if (stat /= FNIL) then
                nnc    = nnc + 1
                denom  = denom + denom2
                ddd(:) = ddd(:) + ccc(:)
           end if
           if (ib12 > 0) go to 18

!----------pick the static.

40         continue
           call stathelper_pick &
                        (obj%helper,ddd,obj%scc_min,denom,  stat,ccoef)

!----------decide whether to increase the increment.

           if (stat == FNIL .and. ccoef == 0.0 .and. inc2 <= inc+1 .and. &
              itrace2 <= ntraces .and. inc <= 1) then
                itrace = ikeep
                inc2 = inc2 + 1
                go to 11
           end if

!----------finish this ground position.

           call stathelper_report_static &
                        (obj%helper,ifile,igp,ddd,stat,ccoef,nnt,nnc,error)
           if (error) return
      end do

      error = .false.
      return
      end subroutine sisc_algorithm


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


!</execute_only>

      end module sisc_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

