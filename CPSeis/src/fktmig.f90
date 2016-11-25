!<CPS_v1 type="PROCESS"/>
!
!<-- This documentation header was last revised by CI Burch on 2000-07-19. />
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
!-------------------------------------------------------------------------------
!                         C P S   P R O C E S S
!
! Name       : FKTMIG     (F-K Time MIGration)
! Category   : migrations
! Written    : 1988-02-01   by: Richard Day and Doug Hanson
! Revised    : 2006-12-04   by: D. Glover
! Maturity   : production
! Purpose    : 2D zero-offset F-K time migration (Stolt or cascaded).
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!  
! FKTMIG is a 2D zero-offset F-K time migration process.  Because F-K migration 
! is a constant velocity migration, FKTMIG calls the Stolt Stretch primitive 
! before the migration to stretch traces so that they mimic the constant 
! velocity used in the migration.  After the migration the Stolt Stretch 
! primitive is called again to shrink the traces back to the input stacking 
! velocity.  FKTMIG calculates the constant velocity for migration based on the 
! input stacking velocity field.
!
!
! Cascaded F-K Migration
!
! FKTMIG can also perform Cascaded F-K Migration, in which data is migrated with
! a sequence of velocities.  This results in somewhat more precise image 
! locations for steeply dipping events than would be the case with Stolt F-K
! migration.  
!
! The sequence of velocities to be used in the cascade is specified by the 
! velocity function named NAME_FUNCT that is contained in the velocity file 
! PATHNAME_CAS.  The number of migrations in the cascade normally need not
! exceed 3 or 4.
! 
!
! Migration Velocity Analysis (not implememted in this version)
!
! FKTMIG can perform migration velocity analysis by doing the migration multiple
! times, each with its own output.  Each individual migration uses a velocity 
! field that is the same trial velocity field multiplied by a different scale 
! factor.  By examining these migration velocity panels, the user can determine
! what velocity causes a given event to be neither undermigrated nor 
! overmigrated. (Use process FKTMIGVA to do Migration Velocity Analysis.)
!
!
! Experimental FKTMIG Operation
!
! For experimental purposes it is possible to run FKTMIG without calling the 
! stretch primitive before and after the actual F-K migration.  If 
! PATHNAME_MIG = NONE, then no calls to stretch are made and the F-K migration 
! runs by itself.  In this case CALC_CV must be set to USER so the constant 
! migration velocity can be input by the user.  The STRETCH parameter also 
! allows independent control over whether the stretch primitive is called.
!
!
! Mute Restoration
!
! FKTMIG automatically restores the head and tail mutes.
!
!
! References
!
! Stolt, R., H., 1978, Migration by Fourier Transform:  Geophysics, v. 43, p.
! 23 - 48.
!
! Beasley, C., et. al., 1987, Cascaded Frequency-Wavenumber (F-K) Migration, 
! Presented at 57th SEG Convention, New Orleans, LA.
!-------------------------------------------------------------------------------
!</descript_doc>
!
!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! TSTRT Value
!
! Input data for FKTMIG cannot have a negative TSTRT.
!
!
! 2-Pass 3D F-K Migration
!
! FKTMIG can be used to perform 2-pass 3D F-K migration by first migrating 
! inlines then sorting to crosslines and migrating crosslines (or crosslines 
! first and then inlines).  Some hints for 2-pass 3D F-K migration:
!
!     1. Insure that the Input Characterization parameters are consistent
!        with the data as it is currently sorted (HDR_SLOW must be the primary 
!        sort header and HDR_FAST the secondary sort header). These parameters 
!        must be changed to reflect the new state of the data after the sort 
!        between passes. 
!
!     2. Check that DIST_FAST is the physical distance associated with a unit
!        change in HDR_FAST.  It may need to be changed between passes.
!
!     3. The FSWP and DAP parameters may need special attention so that they 
!        are not inadvertently applied twice.  Usually they are set to neutral 
!        values for the second pass.
!-------------------------------------------------------------------------------
!</advice_doc>
! 
!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a multiple-trace process.
!
! This process requires traces to be input one at a time.
!
!
!-------------------------------------------------------------------------------
!</trace_in_doc>
!
!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process alters input traces.
! This process outputs the same traces as it receives.
!
! This process outputs one trace at a time.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>
!
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
! 
!-------------------------------------------------------------------------------
!</global_doc>
!
!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#    Description                  Action taken
! ----    -----------                  ------------
! 
! 2, 64   Head and tail mute headers   Mute is reset.
! 25      LAV                          Reset
!
!-------------------------------------------------------------------------------
!</header_word_doc>
!
!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date       Author     Description    
!     ----       ------     ---------
! 88. 2006-12-04 D. Glover  Added NULLIFY statements for Intel compiler.
! 87. 2006-01-10 B. Menger  Removed Unused Variables.
! 86  2005-04-28 Glover     Changed the array WORK in FKTMIG_DINIT to be 
!                           allocatable rather than automatic.
!                           This moves the array from the stack to the heap
!                           to avoid SIGSEGV aborts.
! 85  2004-09-07 Chiu       Add file dialog boxes on the front-end for 
!                           velocity files.                          
! 84  2002-05-06 Chiu       Don't execute STRETCH parameter traps in 
!                           the GUI stage.
! 83  2002-02-06 Chiu       Fix problem when STRETCH = NO.
! 82  2002-01-30 Chiu       Added OPT_MDT option.
! 81  2001-12-10 Chiu       Removed 4 lines of useless codes when NTR = 0.
! 80  2001-11-08 Chiu       Replaced array(*) by array(:). Changed the number
!                           of sample in frequency swath from 512 to 1024
!                           to reduce the number of temporary files.
! 79  2001-11-01 Schmauch   Changed cio_fread and cio_fwrite buffer argument
!                           to send full array.  Necessary due to increased
!                           error checking in cio.
! 78  2001-02-14 Chiu       Fix problem of 0.5 increment of FAST_INC.
! 77  2001-01-03 Chiu       Change migration spacing to DIST_FAST*FAST_INC.
! 76  2000-12-07 Chiu       Change wrapped_up to skip_wrapup.
! 75  2000-11-29 Chiu       Change default of CALC_CV from USER to MIN_STR. 
! 74  2000-11-17 Chiu       Change document only.
! 73  2000-09-05 Chiu       Initialize parameter FAST_TOT to 1.
! 72  2000-07-20 Chiu       Update C.I Burch new doc.
! 71  2000-07-14 Chiu       Replace Fortran i/o by Cio.
! 70  2000-07-11 Chiu       Fix problem with multiple input panels.
! 69  2000-06-26 Chiu       Fix problem when SLOW_TOT > 1.
! 68  2000-06-14 Chiu       Remove temporary work files.
! 67  2000-05-11 Chiu       Rename FKMIG to FKTMIG and remove Migration 
!                           Velocity Analysis documentation.
! 66  2000-04-25 Chiu       Fix Gui problems.
! 65  2000-04-06 Chiu       Add Gui.
! 64  2000-04-05 Chiu       Convert to new CPS.
! 63. 1998-12-08 Goodger    Begin using the f90 compiler.              
! 62. 1998-06-25 Goodger    Remove custom code message.                
! 61. 1997-11-04 Goodger    Fix problem with call to fktmig_migrate from
!                           the main logic.  The last argument, CTR, needs
!                           to be indexed by the offset result of the
!                           offset_index_words call.
!                           Force a disk job if num_panels is greater than one.
!                           The program is expecting the data to be on
!                           disk in this case, even though all should fit
!                           in memory.
! 60. 1997-08-25 Day        Overhauled IO to avoid buffer conflicts when
!                           multiple jobs are running.
! 59. 1997-06-25 Goodger    Changed some parameter names.  Use one dcode
!                           rather than two.
! 58. 1997-06-25 Day        A major overhaul. Eliminated cray specific
!                           syntax such as pointers and packing of arrays
!                           in memory. Made FKTMIG more modular for easier
!                           maintenence. Changed the internal calls to
!                           SSTRANS. Totally overhauled the way data is
!                           cached to disk. Only 3 C-language stream type
!                           files are employed now. The logic for storing
!                           and outputting velocity panels was changed to
!                           conserve disk space. Added the AVERAGE option
!                           for MACVEL.
! 57. 1997-03-25 Vunderink  Added MACFILE parameter and changed I-O
!                           from READWA-WRITEWA to PUTWA-GETWA
! 56. 1996-11-11 Vunderink  Added FFT parameters NBFFT and NTFFT
! 55. 1994-10-18 Troutt     Add check for zero divide in FKTMIG1 due to
!                           non-increasing times.
! 54. 1994-06-03 Troutt     Raised limit of NBASE from 16384 to 32768.
! 53. 1993-11-29 Troutt     Make sure that power of two for spatial
!                           transform is at least 200 more than NBASE.
!                           Raised limit of NBASE from 198192 to 16384.
!                           Note that 16384 limit on NBASE is arbitrary
!                           and can be increased if needed (memory is the
!                           ultimate limit).
! 52. 1993-06-16 Troutt     Add check for negative TSTART (NOT ALLOWED).
!                           Added note #5.
! 51. 1992-08-07 Troutt     Fix minor bug regarding header interpolation
!                           for "missing" traces. Problem occurred when
!                           bin before last input bin was empty.
!                           Also add warning for header file read error.
!                           Change misleading message regarding LIVE
!                           traces to say INPUT traces (includes dead
!                           traces and unused traces when a bin gets more
!                           than one input).
! 50. 1992-07-27 Troutt     Remove restriction of 300 velocity functions
!                           (let GETV dictate the limit). Also reduce
!                           printout of each function to only 1st and
!                           last if > 50 functions.
! 49. 1992-04-30 Troutt     Change default ORDER=0.
! 48. 1992-03-18 Troutt     Add tail mute restoration to output (call
!                           MUTEHW).
! 47. 1992-01-09 Troutt     a)Fix NDEAD count. It was wrong for multi-group
!                           case if 1st trace of "next" group was dead.
!                           Had to add ntsav to common for this.
!                           b)Changed argument passed to FKTMIGF from NTC to
!                           ntsav.
!                           c)Fixed problem with missing traces for multiple
!                           lines when doing out-of-core migration. This
!                           required clearing out all of the disk file
!                           for each new line (not just at the start).
!                           Increased size of WORK to 512 and used it for
!                           clearing instead of TRO.
!                           (Noted Lazear's note of 10-90)
! 46. 1991-12-31 Troutt     Add NTC argument to FKTMIGF so that inserted
!                           missing traces could have mutes indicating
!                           dead traces. (Requested by M. Howard)
! 45. 1991-10-30 Troutt     Increase limit for number of T,V pairs allowed
!                           on velocity functions from 50 to 100 (new
!                           parameter NWTV) for compatability with GETV.
! 44. 1991-08-19 Peterson   Fix to not allow NLIVE traces to overflow BIN.
! 43. 1991-04-19 Howard     Fix bug in reapplying original mute.
! 42  1991-01-30 Day        Corrected IO-memory problem in FKTMIG1.
! 41  1991-01-24 Day        Increased to 300 the velocity function limit.
! 40. 1990-10-10 Lazear     Fix bug in migration of multiple lines.
! 39. 1990-08-27 Lazear     Changed file naming scheme for max. of 52
!                           time swath files.
! 38. 1990-08-14 Lazear     Reset limit on number of time swaths from
!                           16 to 26 (limited by 26 letters for file name)
! 37. 1990-08-10 Lazear     Pass IPN to SSTRANSS for restoring globals
!                           on the inverse call. Also removed 198192 limit
!                           on time FFT size.
! 36. 1990-07-25 Lazear     Fix zeroing of Nyquist wavenumber and memory
!                           allocation for time swath in memory.
! 35. 1990-07-23 Lazear     Save parameters to make reenterant
! 34. 1990-07-17 Lazear     Combined origin shift and FAC into FSHIFT, and
!                           compute origin phase shift exactly rather than
!                           interpolate. Do Bill Harlans frequency domain
!                           interpolation and remove IRATIO padding of
!                           time FFT.
! 33. 1990-07-05 Lazear     Fix frequency shift at k=0, remove hardwire
!              Day          of PWRW=1, moved X-FFT init outside of block
!                           loop, removed filtering out of K-nyquist, and
!                           fixed overwritting of array BUFR.
! 32. 1990-04-12 Day        Cascade bug fixed.
! 31. 1990-04-02 Day        IOPFK option fixed.
! 30. 1990-03-30 Day        Fixed problem with bypassed sstrans calls.
! 29. 1990-03-27 Day        Total overhaul.
! 28. 1989-12-14 Troutt     Change SPWR logic so that low fold traces are
!                           still weighted down before migration, but are
!                           NOT WEIGHTED BACK UP AFTER MIGRATION.
! 27. 1989-12-04 Day        Dead traces passed to output. Added vcon0 call
!                           to fix cascade fk conventions. Fixed disk
!                           initialization problems with ngrp>0. Cascade
!                           problem with sqrt fixed.
! 26. 1989-11-08 Day        Demig amplitude clipped at min and max levels
! 25. 1989-11-03 Day        Blend DEMIG into migrated section
! 24. 1989-11-02 Day        TRO initializated prior to disk initialization
! 23. 1989-11-01 Day        IO error fixed when NVEL>1 .
! 22. 1989-10-31 Day        WBAR factor, BHDR changed, Slightly altered IO,
!                           Cascade conventions altered slightly.
! 21. 1989-08-31 Day        Corrected stretch bypass logic
! 20. 1989-08-24 Day        MACTYPE default set to VTRM.
! 19. 1989-08-23 Troutt     Fix logic for TSTRT not zero. ("T00" is TSTRT)
! 18. 1989-08-11 Day        Brought MACTYPE up to current CPS conventions.
! 17. 1989-07-19 Howard     Change to use all SCRATCH available.
! 16. 1989-06-27 Day        Corrected file naming problem with long traces.
! 15. 1989-06-26 Day        Limit on time swaths increased to 16.
! 14. 1989-06-22 Day        Call to stretch altered. Memory changed due
!                           to regridding for stretch. DEMIG option
! 13. 1989-05-25 Day        Added ORDER parameter. Remove SPWR weighting
!                           before dumping traces to output.
! 12. 1989-05-23 Day        Added SPWR and PWRW parameters.
! 11. 1989-04-07 Day        Velocity panel option added
! 10. 1989-01-13 Day        BYPASS parameter added. Converted to the new
!                           Velocity function protocol.
! 9.  1988-05-11 Day        MAJOR REVISION. SINGLE CHANNEL INPUT,
!                           MULTIPLE GROUPS ENABLED, C0 INPUT ADDED, WORD
!                           ADDRESSABLE DISK IO INCORPORATED.
! 8.  1988-03-26 Day        Mute is back,Header fill corrected
! 7.  1988-03-25 Day        REMOVED MUTE,GETV check correctd
! 6.  1988-03-24 Day        Switched call order of mute & unstretch
! 5.  1988-03-09 Day        Mute clear correction. BINHDR option added
! 4.  1988-03-03 Hanson     SAVE AND INTERPOLATE HEADER WORDS
! 3.  1988-02-16 Day        DISK IO FOR LARGE SECTIONS. SSTRANS
!                           CALLED INTERNALLY.( MINIMUM MUTE 3-1-88)
! 2.  1988-02-02 Day        GOT RID OF INADVERTANT GETVN CALL
! 1.  1988-02-02 Hanson     INCLUDE CASCADED MIGRATION OPTION 
!    
!
!-------------------------------------------------------------------------------
!</history_doc>
!
!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS               
!
! No known limitations.
! 
!
!-------------------------------------------------------------------------------
!</portability_doc>
!
!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS             
!
! No special requirements.
!
! 
!-------------------------------------------------------------------------------
!</compile_doc>
!
!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS            
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
!                    ALTERNATE INTERNAL CALLING METHODS          
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>
!
!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS         
!
!
! THIS ROUTINE PERFORMS AN F-K MIGRATION ON POST STACK(ZERO-OFFSET)DATA
! OR A CASCADED F-K MIGRATION.
!        THE MIGRATION FORMULA IN F-K SPACE IS GIVEN BELOW.
!
!   M(QX,QT) = (QT/W)*P(QX,W)     M=MIGRATED FIELD, P=INPUT STACK
!
!          W =0.5*V*SQRT(QX*QX + QT*QT)=FREQUENCY (FREQUENCY STRETCH)
!          QX=HORIZONTAL WAVENUMBER
!          QT=TOTAL VERTICAL WAVENUMBER
!          V =CONSTANT BACKGROUND VELOCITY,  HV=0.5*V
! THERE ARE CONSTRAINTS ON THE ALLOWABLE ABSOL. VALUES OF QT AND W.
!         .5*QX*V <= W <= PI/DT (I.E. NYQUIST FREQUENCY)
!
!               0 <= QT <= SQRT(A*A - QX*QX)  A=PI/(0.5*V*DT)
!-----------------  ALGORITHM  OUTLINE  --------------------------------
! CONSTRUCT THE MIGRATED FIELD M ON A REGULAR GRID IN QX AND QT
!  1. TRANSFORM P(X,T) OVER ROWS TO P(QX,T)
!  2. TRANSFORM P(QX,T) OVER COLUMNS TO P(QX,W)
!     NOTE: THE TIME TRACE IS ZERO PADDED TO AVOID PROBLEMS.
!  3. FOR EACH QX VALUE, FORM THE MIGRATION OPERATOR QT/W ON THE
!     REGULAR QT GRID. SHIFT AND INTERPOLATE  P FROM THE W GRID
!     TO THE QT GRID AND MULTIPLY BY THE MIGRATION OPERATOR.
!  4. INVERSE TRANSFORM FROM QT TO Z
!  5. INVERSE TRANSFORM FROM QX TO X(COMPLEX TO REAL)
!
!  6. FOR CASCADED MIGRATION STEPS 2 - 4 ARE REPEATED
!     A NUMBER OF TIMES USING A RESIDUAL VELOCITY FOR EACH STAGE.
!     THE RESIDUAL VELOCITY USED IS
!     C = SQRT(V(I)**2 - V(I-1)**2)
!     WHERE V(I) IS THE INTERVAL VELOCITY OF THE ITH LAYER.
!     FOR THE FIRST STAGE C = V(1).
!     SEE CASCADED FREQUENCY-WAVENUMBER (F-K) MIGRATION
!         BEASLEY, C.; LYNN, W.; LARNER, K. AND NGUYEN,H.
!         57TH SEG, OCT. 1987, NEW ORLEANS LA.
!---------------------------------------------------------------
!      DW=2.0*PI/(NT*DT)     DQT=DW/(.5*V)
! NOTE THAT THE Z GRID AFTER MIGRATION IS DEPENDENT UPON OUR
!      CHOICE OF BACKGROUND MIGRATION VELOCITY.
!      ALTERNATIVELY WE MAY CHOOSE A FIXED QT GRID
!
!-------------------------------------------------------------------------------
!</algorithm_doc>
!
!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>
!-------------------------------------------------------------------------------
!<gui_def>
!<NS FKTMIG Process/NC=80>
! 2D zero-offset F-K time migration (Stolt or cascaded).
!    `----------------------------------------------
!     HDR_SLOW= `IIIIII  
!     SLOW_INIT=`FFFFFFFFFFF  SLOW_INC=`FFFFFFFFFFF 
!     SLOW_LAST=`FFFFFFFFFFFF SLOW_TOT=`IIIIIIII
!    `----------------------------------------------
!
!    `----------------------------------------------
!     HDR_FAST= `IIIIII
!     FAST_INIT=`FFFFFFFFFFF  FAST_INC=`FFFFFFFFFFF 
!     FAST_LAST=`FFFFFFFFFFF  FAST_TOT=`IIIIIIII
!     DIST_FAST=`FFFFFFFFFFF  
!    `----------------------------------------------
!
! MODE=`CCCCCCCC  CALC_CV=`CCCCCCC  VEL_CONST=`FFFFFFFFFFFF
!
!Select PATHNAME_CAS[PATHNAME_CAS]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                [PATHNAME_CAS_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! NAME_FUNCT=~~`SSSSSSSSSSSSSSSSSSSS
!
!
!Select PATHNAME_MIG[PATHNAME_MIG]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                [PATHNAME_MIG_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! SMOOTH=`CCCC  DEGREE=`I  FSWP=`FFFFFFFFFFF  DAP=`IIIIIIII REE=`CC
!
! STRETCH=`CC   OPT_MDT=`CCCC
! MEM_MAX=`IIIIIIII
!
!<PARMS PATHNAME_CAS[/ML=128/XST]>
!<PARMS PATHNAME_MIG[/ML=128/XST]>
!</gui_def>
!
!<HelpSection>
!<        ---------------characterizing input------------------/>
!
!<Help KEYWORD="HDR_SLOW">
!<Tip> Header word designating input 2D lines. </Tip>
! Default = 8
! Allowed = 1 - NWIH 
! HDR_SLOW should be the primary sort header word (changing slowly).
!</Help>
!
!<Help KEYWORD="SLOW_INIT">
!<Tip> Value of HDR_SLOW for first input 2D line. </Tip>
! Default = 1.0
! Allowed = real 
!</Help>
!
!<Help KEYWORD="SLOW_INC">
!<Tip> Increment of HDR_SLOW between input 2D lines. </Tip>
! Default = 1.0
! Allowed = real 
!</Help>
!
!<Help KEYWORD="SLOW_LAST">
!<Tip> Value of HDR_SLOW for last input 2D line. </Tip>
! Default = 1.0
! Allowed = real 
!</Help>
!
!<Help KEYWORD="SLOW_TOT">
!<Tip> Number of input 2D lines to migrate. </Tip>
! Default = 1
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="HDR_FAST">
!<Tip> Header word designating traces within an input 2D line. </Tip>
! Default = 7
! Allowed = 1 - NWIH 
! HDR_FAST should be the secondary sort header word (changing rapidly).
!</Help>
!
!<Help KEYWORD="FAST_INIT">
!<Tip> Value of HDR_FAST for first trace in an input 2D line. </Tip>
! Default = 1.0
! Allowed = real 
!</Help>
!
!<Help KEYWORD="FAST_INC">
!<Tip> Increment of HDR_FAST between traces in an input 2D line. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="FAST_LAST">
!<Tip> Value of HDR_FAST for last trace in an input 2D line. </Tip>
! Default = 1.0
! Allowed = real 
!</Help>
!
!<Help KEYWORD="FAST_TOT">
!<Tip> Maximum number of traces in any input 2D line. </Tip>
! Default = 1
! Allowed = int < 32769 
! Size of the spatial transform will be at least 200 more than TR_TOT.
!</Help>
!
!<Help KEYWORD="DIST_FAST">
!<Tip> Physical distance associated with unit change in HDR_FAST. </Tip>
! Default = 1.0
! Allowed = real > 0.0
! DIST_FAST is the physical distance in feet or meters associated with a change 
! in HDR_FAST of 1.0.  It is not necessarily the distance between actual 
! traces (which may be associated with a change of HDR_FAST of 2.0 or more).
!</Help>
!
!<          -----------------memory use------------------  />
!
!<Help KEYWORD="MEM_MAX">
!<Tip> Maximum amount of memory to use, in traces. </Tip>
! Default = 10000
! Allowed = int > 0
! If the input dataset has no more than MEM_MAX traces, FKTMIG will operate in
! memory alone.  If the dataset has more than MEM_MAX traces, disk swapping 
! will be done.  Check with the system administrator before setting MEM_MAX 
! substantially larger than the default.
!</Help>
!
!<         ---------------migration type------------------  />
!<Help KEYWORD="MODE">
!<Tip> Whether to use Stolt or Cascaded F-K migration. </Tip>
! Default = STOLT
! Allowed = STOLT
! Allowed = CASCADED
!</Help>
!
!<Help KEYWORD="CALC_CV">
!<Tip> Method to use for calculating constant velocity. </Tip>
! Default = MIN_STR
! Allowed = MIN_STR  (Use minimum stretching criteria.)
! Allowed = USER     (Constant velocity is input by the user.)
! Method to use for calculating constant velocity for Stolt F-K migration.  
! Normally the default is used.
!</Help>
!
!<Help KEYWORD="VEL_CONST">
!<Tip> User supplied constant velocity for Stolt F-K migration. </Tip>
! Default = 1500.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME_CAS">
!<Tip> Choose PATHNAME_CAS using a file selection dialog box. </Tip>
! Pathname for velocity file containing a velocity function specifying a 
! velocity for each migration in the cascade.
!</Help>
!
!<Help KEYWORD="PATHNAME_CAS_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_CAS. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME_CAS">
!<Tip> Pathname for velocity file specifying cascade velocities. </Tip>
! Default = -
! Allowed = char
! Pathname for velocity file containing a velocity function specifying a 
! velocity for each migration in the cascade.
!</Help>
!
!<Help KEYWORD="NAME_FUNCT">
!<Tip> Name of the desired velocity function within PATHNAME_CAS. </Tip>
! Default = FIRST
! Allowed = char
! If NAME_FUNCT = FIRST, then the first velocity function in the velocity file
! PATHNAME_CAS will be used.
!</Help>
!
!<       ---------------migration velocity and smoothing-----------------/>
!
!<Help KEYWORD="SELECT_PATHNAME_MIG">
!<Tip> Choose PATHNAME_MIG using a file selection dialog box. </Tip>
! Pathname for velocity file specifying migration velocity field for FKTMIG.  
! This pathname is also used by the Stolt Stretch primitive, which is called 
! internally.
!
! If PATHNAME_MIG = NONE, then no stretching is done and CALC_CV must be set to
! USER.
!</Help>
!
!<Help KEYWORD="PATHNAME_MIG_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_MIG. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME_MIG">
!<Tip> Pathname for velocity file specifying migration velocity field. </Tip>
! Default = -
! Allowed = char
! Pathname for velocity file specifying migration velocity field for FKTMIG.  
! This pathname is also used by the Stolt Stretch primitive, which is called 
! internally.
!
! If PATHNAME_MIG = NONE, then no stretching is done and CALC_CV must be set to
! USER.
!</Help>
!
!<Help KEYWORD="SMOOTH">
!<Tip> Smooth the velocity field by interpolation or polynomial fitting. </Tip>
! Default = FIT
! Allowed = FIT   
! Allowed = INTERP 
! If SMOOTH = FIT, then smooth the migration velocity field by using a least-
! squares fit to a polynomial of degree specified by the DEGREE parameter.
!
! If SMOOTH = INTERP, then do linear interpolation between velocity function 
! locations (and use constant extrapolation at the ends).  WARNING:  this 
! option may cause chevron shaped artifacts unless the velocity field is 
! naturally very smooth or you have previously smoothed it.
!</Help>
!
!<Help KEYWORD="DEGREE">
!<Tip> Degree of polynomial to use when SMOOTH = FIT. </Tip>
! Default = 1
! Allowed = int >= 0  
! Normally DEGREE should not exceed 2 or 3.  DEGREE = 0 allows no lateral
! variation.
!</Help>
!
!<       ---------------migration controls------------------ />
!
!<Help KEYWORD="FSWP">
!<Tip> Fold of Stack Weighting Power. </Tip>
! Default = 0.5
! Allowed = 1.0 >= real >= 0.0 
! FSWP weighting causes traces to be weighted such that New Amplitude = Old
! Amplitude *fold** FSWP.  This is an effort to control lateral edge artifacts.
!</Help>
!
!<Help KEYWORD="DAP">
!<Tip> Dip Attenuation Parameter. </Tip>
! Default = 1
! Allowed = int >= 0
! FKTMIG can perform a sort of dip filtering in the F-K plane.  If DAP > 1 then 
! steep dips will be attenuated (with more attenuation for larger values of 
! DAP).  DAP = 1 produces no dip filtering.  DAP = 0 causes increased steeply 
! dipping energy.  
!
! DAP = 2 or 3 is often used to attenuate random noise if there is little 
! steeply dipping desirable signal.
!</Help>
!
!<Help KEYWORD="REE">
!<Tip> Restore Evanescent Energy. </Tip>
! Default = NO
! Allowed = YES/NO
! REE = NO removes energy with unphysically steep dips from the data. 
! (Migration naturally does this.)
!
! REE = Y saves the energy with unphysically steep dips and restores it to the 
! data after migration.  This is sometimes done to make datasets more 
! cosmetically acceptable that would otherwise have a mixed appearance.
!</Help>
!
!<Help KEYWORD="STRETCH">
!<Tip> Call the Stolt Stretch primitive internally? </Tip>
! Default = YES
! Allowed = YES  (Call the Stolt Stretch primitive before and after migration.)
! Allowed = NO   (Do not call the Stolt Stretch primitive.)
!</Help>
!
!<Help KEYWORD="OPT_MDT">
!<Tip> Option for handling Missing or Dead input Traces. </Tip>
! Default = DEL
! Allowed = DEL   
! Allowed = FILL
! Allowed = KILL
! Output traces corresponding to missing or dead input traces are deleted 
! (OPT_MDT = DEL), filled in (OPT_MDT = FILL) or killed (OPT_MDT = KILL).
! Traces affected by the FILL option will always have their mute headers set to
! the first and last non-zero samples.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
! Begin Fortran here.
!
!
! NOTES FOR CONVERSION PROGRAMMER
!
! 1.  Note addition of LINE_LAST and TR_LAST parameters.
!
! 2.  "CVELNAME = AVERAGE" functionality is removed.
!
! 3.  Note breaking of ORDER into FIT and INTERP options.
!
! 4.  SPWR is now FSWP, PWRW is now DAP, DEMIG is now REE.
!
! 5.  BHDR should be determined from the velocity file. 
!
! 6.  NBFFT, NTFFT and MUSE should not be needed.
!
! 7.  Currently lines of modest length can take a very long time to finish in 
! the migration velocity analysis mode (sometimes they never complete).  
! Possibly the disk-swapping needs attention.
!
!
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module fktmig_module
      use pc_module
      use named_constants_module
      use getlun_module
      use sizeof_module
      use velutil_module
      use velio_module
      use pathcheck_module
      use pathchoose_module
      use string_module
      use lav_module
      use getsys_module
      use fft_module
      use cio_module
      use tempname_module
      use pattern_module

      use mutehw_module
      use stretch_module

      implicit none
      private
      public :: fktmig_create     ! uses the parameter cache.
      public :: fktmig_initialize
      public :: fktmig_update     ! uses the parameter cache.
      public :: fktmig_delete
!<execute_only>
      public :: fktmig            ! main execution (trace processing) routine.
      public :: fktmig_wrapup
!</execute_only>


      character(len=100),public,save :: fktmig_IDENT = &
       '$Id: fktmig.f90,v 1.88 2006/12/04 13:29:53 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

    integer,parameter :: MAX_LINE = 120
                                     
    type,public :: fktmig_struct              
 
    private
    logical               :: skip_wrapup         ! wrapup flag.
    logical               :: gathered           ! gathered flag.

    character (len=8)     :: stretch            ! Process parameter.
    character (len=8)     :: calc_cv            ! Process parameter.
    character (len=8)     :: name_funct         ! Process parameter.
    character (len=FILENAME_LENGTH)   :: pathname_cas     ! Process parameter.
    character (len=FILENAME_LENGTH)   :: pathname_mig     ! Process parameter.
    type(pathchoose_struct),pointer   :: dialog_cas
    type(pathchoose_struct),pointer   :: dialog_mig
    character (len=8)     :: smooth             ! Process parameter.
    character (len=8)     :: mode               ! Process parameter.
    character (len=8)     :: ree                ! Process parameter.
    character (len=4)     :: opt_mdt            ! process parameter.
    integer               :: dap                ! Process parameter.
    integer               :: degree             ! Process parameter.  
    integer               :: fast_tot           ! Process parameter. 
    integer               :: hdr_fast           ! Process parameter. 
    integer               :: hdr_slow           ! Process parameter. 
    integer               :: slow_tot           ! Process parameter.
    integer               :: mem_max            ! Process parameter.

    real                  :: dist_fast          ! Process parameter.
    real                  :: fast_inc           ! Process parameter.
    real                  :: fast_init          ! Process parameter. 
    real                  :: fast_last          ! Process parameter. 
    real                  :: fswp               ! Process parameter. 
    real                  :: slow_inc           ! Process parameter. 
    real                  :: slow_init          ! Process parameter. 
    real                  :: slow_last          ! Process parameter. 
    real                  :: vel_const          ! Process parameter.
    integer               :: ndpt               ! globals.
    integer               :: nwih               ! globals.
    real                  :: dt                 ! globals.
    real                  :: tstrt              ! globals.

    integer               :: num_panels         ! dependent variables.
    real                  :: mult_inc           ! dependent variables.
    real                  :: mult_init          ! dependent variables.

    integer               :: bcnt               ! dependent variables.
    integer               :: fdhdr(1)           ! dependent variables.
    integer               :: icnt               ! dependent variables.
    integer               :: iflag              ! dependent variables.  
     integer              :: istretch           ! dependent variables.
    integer               :: linenew            ! dependent variables.
    integer               :: lineold            ! dependent variables.
    integer               :: nb2                ! dependent variables.
    integer               :: nb22               ! dependent variables.
    integer               :: nbfft              ! dependent variables.
    integer               :: nblks              ! dependent variables.
    integer               :: nbnyq              ! dependent variables.
    integer               :: ndump              ! dependent variables.
    integer               :: nline              ! dependent variables.
    integer               :: nlive              ! dependent variables.
    integer               :: nt2                ! dependent variables.
    integer               :: ntc                ! dependent variables.
    integer               :: ntsw               ! dependent variables.
    integer               :: nvmac              ! dependent variables.
    integer               :: velpan             ! dependent variables.
    integer               :: byt_size           ! dependent variables.
    integer               :: dbyt_size          ! dependent variables.
    integer               :: origin             ! dependent variables.
 

    real                  :: dtc                ! dependent variables.
    real                  :: mute               ! dependent variables.
    real                  :: t0c                ! dependent variables.
    real                  :: wfac               ! dependent variables.
    integer               :: ndpt_fin           ! dependent variables.
    real                  :: dt_fin             ! dependent variables.
    real                  :: tstrt_fin          ! dependent variables.
    integer               :: ndpt_fout          ! dependent variables.
    real                  :: dt_fout            ! dependent variables.
    real                  :: tstrt_fout         ! dependent variables.
     
    integer                    :: print_lun     ! dependent variables. 
    type(velio_struct),pointer :: velio         ! dependent variables.

    type(fft_struct),pointer :: fftcc_pos_obj   ! dependent variables.
    type(fft_struct),pointer :: fftcc_neg_obj   ! dependent variables.

    type(fft_struct),pointer :: fftrc_xobj      ! dependent variables.
    type(fft_struct),pointer :: fftcr_xobj      ! dependent variables.

    type(stretch_struct),pointer :: stretch_for   ! dependent variables.
    type(stretch_struct),pointer :: stretch_inv   ! dependent variables.

    real,      pointer    :: sect(:,:)          ! dependent variables.
    real,      pointer    :: tmac(:)            ! dependent variables.
    real,      pointer    :: vmac(:)            ! dependent variables.
    real,      pointer    :: tr_stor(:)         ! dependent variables.
    complex,   pointer    :: cdelta(:)          ! dependent variables.
    integer,   pointer    :: fdcur(:)           ! dependent variables.
    integer,   pointer    :: fdout(:)           ! dependent variables.
    integer,   pointer    :: binlim(:)          ! dependent variables.

  end type fktmig_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(fktmig_struct),pointer,save :: object      ! needed for traps.

      integer    ,parameter :: mode_noptions = 2
      character(len=8),save :: mode_options(mode_noptions)              &
                = (/ 'STOLT   ', 'CASCADED' /)

      integer    ,parameter :: calc_cv_noptions = 2
      character(len=8),save :: calc_cv_options(calc_cv_noptions)         &
                = (/ 'MIN_STR ', 'USER    ' /)

      integer    ,parameter :: smooth_noptions = 2
      character(len=8),save :: smooth_options(smooth_noptions)           &
                = (/ 'FIT     ', 'INTERP  ' /)

      integer    ,parameter :: ree_noptions = 2
      character(len=8),save :: ree_options(ree_noptions)                 &
                = (/ 'YES     ', 'NO      ' /)

      integer    ,parameter :: stretch_noptions = 2
      character(len=8),save :: stretch_options(stretch_noptions)         &
                = (/ 'YES     ', 'NO      ' /)

      integer,parameter     :: opt_mdt_noptions = 3
      character(len=4),save :: opt_mdt_options(opt_mdt_noptions)
      data opt_mdt_options/'DEL','FILL', 'KILL'/
      
      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine fktmig_create (obj)
      implicit none
      type(fktmig_struct),pointer :: obj            ! arguments

      allocate (obj)

      nullify (obj%fftcc_pos_obj)
      nullify (obj%fftcc_neg_obj)
      nullify (obj%fftrc_xobj)
      nullify (obj%fftcr_xobj)
      nullify (obj%stretch_for)
      nullify (obj%stretch_inv)
      nullify (obj%sect)
      nullify (obj%tmac)
      nullify (obj%vmac)
      nullify (obj%cdelta)
      nullify (obj%fdcur)
      nullify (obj%fdout)
      nullify (obj%binlim)
      nullify (obj%tr_stor)
      nullify (obj%velio) ! jpa
      nullify (obj%dialog_cas) ! jpa
      nullify (obj%dialog_mig) ! jpa

      call pathchoose_create (obj%dialog_cas, 'PATHNAME_CAS', 'vel')
      call pathchoose_create (obj%dialog_mig, 'PATHNAME_MIG', 'vel')

      call fktmig_initialize (obj)

      return
      end subroutine fktmig_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine fktmig_delete (obj)
      implicit none
      type(fktmig_struct),pointer :: obj       ! arguments

!<execute_only>
      call fktmig_wrapup (obj)
!</execute_only>

     ! Make sure ALL POINTERS in your parameter structure are deallocated

      if (associated(obj%sect ))   deallocate      (obj%sect)
      if (associated(obj%tmac ))   deallocate      (obj%tmac)
      if (associated(obj%vmac ))   deallocate      (obj%vmac)
      if (associated(obj%cdelta )) deallocate      (obj%cdelta)
      if (associated(obj%fdcur ))  deallocate      (obj%fdcur)
      if (associated(obj%fdout ))  deallocate      (obj%fdout)
      if (associated(obj%binlim )) deallocate      (obj%binlim)
      if (associated(obj%tr_stor)) deallocate      (obj%tr_stor)

      if (associated(obj%fftcc_pos_obj))  call fft_delete (obj%fftcc_pos_obj)
      if (associated(obj%fftcc_neg_obj))  call fft_delete (obj%fftcc_neg_obj)
      if (associated(obj%fftrc_xobj))     call fft_delete (obj%fftrc_xobj)
      if (associated(obj%fftcr_xobj))     call fft_delete (obj%fftcr_xobj)
      if (associated(obj%stretch_for))    call stretch_delete (obj%stretch_for)
      if (associated(obj%stretch_inv))    call stretch_delete (obj%stretch_inv)

      if (associated(obj%dialog_cas)) call pathchoose_delete (obj%dialog_cas)
      if (associated(obj%dialog_mig)) call pathchoose_delete (obj%dialog_mig)

      deallocate(obj)
      return
      end subroutine fktmig_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine fktmig_initialize (obj)
      implicit none
      type(fktmig_struct),intent(inout) :: obj       ! arguments

      real  :: size_byte                            ! local
      double precision  :: dsize_byte               ! local


      obj%origin = 0 
      obj%byt_size = sizeof(size_byte)
      obj%dbyt_size = sizeof(dsize_byte)


      obj%hdr_slow     = 8
      obj%slow_init    = 1.0
      obj%slow_inc     = 1.0
      obj%slow_last    = 1.0
      obj%slow_tot     = 1
      obj%hdr_fast     = 7
      obj%fast_init    = 1.0
      obj%fast_inc     = 1.0
      obj%fast_last    = 1.0
      obj%fast_tot     = 1
      obj%dist_fast    = 1.0
      obj%mode         = 'STOLT'
      obj%calc_cv      = 'MIN_STR'
      obj%vel_const    = 1500.0
      obj%pathname_cas = PATHCHECK_EMPTY
      obj%name_funct   = 'FIRST'
      obj%pathname_mig = PATHCHECK_EMPTY
      obj%smooth       = 'FIT'
      obj%degree       = 1
      obj%fswp         = 0.5
      obj%dap          = 1
      obj%ree          = 'NO'
      obj%stretch      = 'YES'
      obj%mem_max      = 10000
      obj%opt_mdt      = 'DEL'
      
      obj%gathered     = .false. ! hope it gets set right later
      obj%nwih         = inil    ! must test later to make sure has been set
      obj%ndpt         = inil    ! must test later to make sure has been set
      obj%tstrt        = fnil    ! must test later to make sure has been set
      obj%dt           = fnil    ! must test later to make sure has been set

      obj%print_lun = pc_get_lun()
      call fktmig_update (obj)
      return
      end subroutine fktmig_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine fktmig_update (obj)
      implicit none
      type(fktmig_struct),intent(inout),target :: obj            ! arguments

!! Declare all required local variables as follows:

      integer     :: ier                                        ! local
      logical     :: iftd                                       ! local
      integer     :: numtr,ntapes,nscratch,nstore,ndisk         ! local
      integer     :: len_string                                 ! local
      integer     :: state                                      ! local
      integer     :: istatus                                    ! local
      logical     :: verify                                     ! local

      character(len=3)       :: need_label,need_request         ! local
      character(len=3)       :: twosets                         ! local
      character(len=FILENAME_LENGTH)   :: tfile                 ! local
      character(len=8)       :: stretch_mode                    ! local
      character(len=8)       :: stretch_calc_cv                 ! local
      integer                :: n, L        , i ! local
      integer                :: ndpt_max, mem_use               ! local
      real                   :: tmax, sqfac  ! local
      real                   :: vmig                            ! local



      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.
      
      state = pc_get_update_state()

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        verify = .true.
      else
        verify = .false.
      end if
 
   

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      numtr = inil
      call pc_get_global ('numtr'   ,numtr)        ! maximum number of traces.
      call pc_get_global ('gathered',obj%gathered) ! whether properly gathered.
      call pc_get_global ('nwih'  , obj%nwih)  ! number of header words.
      call pc_get_global ('ndpt'  , obj%ndpt)  ! number of trace samples.
      call pc_get_global ('tstrt' , obj%tstrt) ! time of 1st trace sample (s).
      call pc_get_global ('dt'    , obj%dt)    ! trace sample interval (s).

!------------Check that globals are set:

      if (numtr == inil) call pc_error ("NUMTR global hasn't been set.")
      if (obj%nwih == inil) call pc_error ("NWIH global hasn't been set.")
      if (obj%ndpt == inil) call pc_error ("NDPT global hasn't been set.")
      if (obj%tstrt == fnil) call pc_error ("TSTRT global hasn't been set.")
      if (obj%dt   == fnil) call pc_error ("DT global hasn't been set.")


      if (pathchoose_update(obj%dialog_cas, obj%pathname_cas)) return
      if (pathchoose_update(obj%dialog_mig, obj%pathname_mig)) return

      call pc_get ('HDR_SLOW',     obj%hdr_slow)
      call pc_get ('SLOW_INIT',    obj%slow_init)
      call pc_get ('SLOW_INC',     obj%slow_inc)
      call pc_get ('SLOW_LAST',    obj%slow_last)
      call pc_get ('SLOW_TOT',     obj%slow_tot)
      call pc_get ('HDR_FAST',     obj%hdr_fast)
      call pc_get ('FAST_INIT',    obj%fast_init)
      call pc_get ('FAST_INC',     obj%fast_inc)
      call pc_get ('FAST_LAST',    obj%fast_last)
      call pc_get ('FAST_TOT',     obj%fast_tot)
      call pc_get ('DIST_FAST',    obj%dist_fast)
      call pc_get ('MODE',         obj%mode)
      call pc_get ('CALC_CV',      obj%calc_cv)
      call pc_get ('VEL_CONST',    obj%vel_const)
      call pc_get ('PATHNAME_CAS', obj%pathname_cas)
      call pc_get ('NAME_FUNCT',   obj%name_funct)
      call pc_get ('PATHNAME_MIG', obj%pathname_mig)
      call pc_get ('SMOOTH',       obj%smooth)
      call pc_get ('DEGREE',       obj%degree)
      call pc_get ('FSWP',         obj%fswp)
      call pc_get ('DAP',          obj%dap)
      call pc_get ('REE',          obj%ree)
      call pc_get ('STRETCH',      obj%stretch)
      call pc_get ('MEM_MAX',      obj%mem_max)
      call pc_get ('opt_mdt',      obj%opt_mdt)
      
      call string_to_upper (obj%mode)
      call string_to_upper (obj%calc_cv)
      call string_to_upper (obj%name_funct)
      call string_to_upper (obj%ree)
      call string_to_upper (obj%smooth)
      call string_to_upper (obj%stretch)
      call string_to_upper (obj%opt_mdt)

    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!


      len_string = len_trim(obj%pathname_cas)
      if ( len_string < 1) obj%pathname_cas = PATHCHECK_EMPTY

      len_string = len_trim(obj%pathname_mig)
      if ( len_string < 1) obj%pathname_mig = PATHCHECK_EMPTY

      len_string = len_trim(obj%name_funct)
      if ( len_string < 1) obj%name_funct = PATHCHECK_EMPTY

      if (obj%pathname_cas /= PATHCHECK_EMPTY) then
         call pathcheck (KEYWORD='PATHNAME_CAS', PATHNAME=obj%pathname_cas, &
                        EXT='', REQUIRED=.false., SCREEN='FKTMIG',   &
                        show=PATHCHECK_INFO_INPUT)

         if (obj%name_funct == PATHCHECK_EMPTY) then
            call pc_error('Invalid in STRETCH: NAME_FUNCT is required')
         end if
         call pc_put_sensitive_field_flag  ('VEL_CONST',  .false.)
      else
         call pc_put_sensitive_field_flag ('VEL_CONST', .true.)
      end if

      if (obj%pathname_mig /= PATHCHECK_EMPTY) then
        call pathcheck (KEYWORD='PATHNAME_MIG', PATHNAME=obj%pathname_mig, &
                        EXT='', REQUIRED=.false., SCREEN='FKTMIG',   &
                        show=PATHCHECK_INFO_INPUT)
      end if

      if (obj%hdr_slow <= 0) then    
        call pc_error ('FKTMIG : HDR_SLOW must be > 0 ')  
      end if

      if (obj%hdr_slow > obj%nwih) then    
        call pc_error ('FKTMIG : HDR_SLOW must be < NWIH ')  
      end if

      if (obj%slow_tot <= 0) then   
        call pc_error ('FKTMIG : SLOW_TOT must be > 0')
      end if 
 
      if (obj%slow_inc == 0.0) then   
        call pc_error ('FKTMIG : SLOW_INC must not be 0 ')
      end if 

     if (obj%hdr_fast <= 0) then    
        call pc_error ('FKTMIG : HDR_FAST must be > 0')
      end if 

     if (obj%hdr_fast > obj%nwih) then    
        call pc_error ('FKTMIG : HDR_FAST must be < NWIH')
      end if 

      if (obj%fast_tot <= 0) then    
        call pc_error ('FKTMIG : FAST_TOT must be > 0')
      end if 

      if (obj%fast_tot > 32769) then    
        call pc_error ('FKTMIG : FAST_TOT must be < 32769 ')
      end if 

      if (obj%fast_inc == 0.0) then   
        call pc_error ('FKTMIG : FAST_INC must not be 0')
      end if 


      if (obj%dist_fast <= 0.0) then   
        call pc_error ('FKTMIG : DIST_FAST must be > 0')
       end if 

      if(obj%mode /= 'STOLT'.and. obj%mode /= 'CASCADED') then
        call pc_error('Invalid in FKTMIG: MODE MUST BE STOLT OR CASCADED')
      end if

      if (obj%vel_const <= 0.0) then   
        call pc_error ('FKTMIG : VEL_CONST must be > 0')
      end if 

      if(obj%calc_cv /= 'MIN_STR'.and. obj%calc_cv /= 'USER') then
        call pc_error('Invalid in FKTMIG: CALC_CV MUST BE MIN_STR OR USER')
      end if

      if(obj%smooth /= 'FIT'.and. obj%smooth /= 'INTERP') then
        call pc_error('Invalid in FKTMIG: SMOOTH MUST BE FIT OR INTERP')
      end if

      if (obj%degree < 0) then     
         call pc_error ('FKTMIG: DEGREE must be >= 0 ')  
      end if 

      if (obj%fswp < 0.0) then    
        call pc_error ('FKTMIG: FSWP must be >= 0')
      end if 

      if (obj%fswp > 1.0) then    
        call pc_error ('FKTMIG: FSWP must be <= 1 ')
      end if 

     if (obj%dap < 0) then   
       call pc_error ('FKTMIG : DAP must be >= 0')
     end if 

     if(obj%ree /= 'YES'.and. obj%ree /= 'NO') then
        call pc_error('Invalid in FKTMIG: REE MUST BE YES OR NO')
     end if

     if(obj%stretch /= 'YES'.and. obj%stretch /= 'NO') then
        call pc_error('Invalid in FKTMIG: STRETCH MUST BE YES OR NO')
     end if

     if (obj%mem_max <= 0) then   
       call pc_error ('FKTMIG : MEM_MAX must be > 0')
     end if

      if(obj%opt_mdt /='DEL' .and. obj%opt_mdt /='FILL'                   &
        .and. obj%opt_mdt /='KILL') then 
        call pc_error('Invalid in FKTMIG: OPT_MDT must be DEL, FILL or KILL')  
      end if
      
      if (pc_get_update_state() /= PC_GUI ) then
        if(obj%tstrt > 0.0) then
          call pc_warning('Invalid in FKTMIG: Global start time must be zero')
        end if
      end if


     istatus = pattern_stop2('FKTMIG:', .true., &
       obj%slow_init, obj%slow_inc, obj%slow_last, obj%slow_tot, &
       'SLOW_INIT', 'SLOW_INC', 'SLOW_LAST', 'SLOW_TOT', &
       pc_verify_scalar('SLOW_INIT'), pc_verify_scalar('SLOW_INC'), &
       pc_verify_scalar('SLOW_LAST'), pc_verify_scalar('SLOW_TOT'))   

     istatus = pattern_stop2('FKTMIG:', .true., &
       obj%fast_init, obj%fast_inc, obj%fast_last, obj%fast_tot, &
       'FAST_INIT', 'FAST_INC', 'FAST_LAST', 'FAST_TOT', &
       pc_verify_scalar('FAST_INIT'), pc_verify_scalar('FAST_INC'), &
       pc_verify_scalar('FAST_LAST'), pc_verify_scalar('FAST_TOT')) 

                          ! end trap

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


     obj%ndpt_fin  = obj%ndpt
     obj%tstrt_fin = obj%tstrt
     obj%dt_fin    = obj%dt

     obj%ndpt_fout =  obj%ndpt
     obj%tstrt_fout = obj%tstrt
     obj%dt_fout    = obj%dt 
     
     vmig = obj%vel_const 
     obj%wfac   = 1.0 
     sqfac  = 1.0

  if (pc_get_update_state() /= PC_GUI) then

     if (obj%stretch == 'YES') then
       call pc_clear

       stretch_fktmig = .true.
       stretch_mode  = 'FORWARD'
       stretch_calc_cv = obj%calc_cv

       call pc_put_process ('PATHNAME_MIG',  obj%pathname_mig)
       call pc_put_process ('PATHNAME_REF',  obj%pathname_cas)
       call pc_put_process ('NAME_FUNCT',    obj%name_funct)
       call pc_put_process ('VEL_CONST',     obj%vel_const)
       call pc_put_process ('MODE',          stretch_mode)      
       call pc_put_process ('SMOOTH',        obj%smooth)
       call pc_put_process ('DEGREE',        obj%degree)
       call pc_put_process ('CALC_CV',       stretch_calc_cv) 
       call pc_put_process ('NDPT_OUT',      obj%ndpt_fin) 
       call pc_put_process ('TSTRT_OUT',     obj%tstrt_fin)
       call pc_put_process ('DT_OUT',        obj%dt_fin)


       if (associated(obj%stretch_for)) then                
           call stretch_update (obj%stretch_for)
       else
           call stretch_create (obj%stretch_for)
       end if

       !  get vmig, wfac and sqfac from stretch modules.

       call  stretch_get_fkpar(obj%stretch_for, vmig, obj%wfac, sqfac, tmax)

       if (obj%calc_cv == 'MIN_STR')  then
         obj%vel_const = vmig
       end if

       obj%tstrt_fout = obj%tstrt
       obj%dt_fout    = obj%dt * sqfac
       obj%ndpt_fout = (tmax - obj%tstrt_fout)/obj%dt_fout + 1.0005

       write (obj%print_lun, *)      &
             'FKTMIG: Anti Aliasing sample rate=', obj%dt_fout
       write (obj%print_lun, *)     &
             'FKTMIG: Number of samples in output=', obj%ndpt_fout
       write(obj%print_lun, *) 'FKTMIG:  WFAC = ', obj%wfac

       call pc_put_process ('NDPT_OUT',      obj%ndpt_fout) 
       call pc_put_process ('DT_OUT',        obj%dt_fout)
       call pc_put_process ('TSTRT_OUT',     obj%tstrt_fout)

       !  update the stretch with modified output sample rate        
       call stretch_update (obj%stretch_for)

       if (obj%calc_cv == 'MIN_STR') then
          if ( obj%vel_const <= 0.0) then
             call pc_error(' FKTMIG: Minimum stretch velocity is invalid ')
             return
           end if
        end if

        call pc_restore

        call pc_clear
 
        stretch_mode  = 'INVERSE'
        stretch_calc_cv   = 'USER'

        call pc_put_global  ('NDPT',          obj%ndpt_fout) 
        call pc_put_global  ('TSTRT',         obj%tstrt_fout)
        call pc_put_global  ('DT',            obj%dt_fout)
        call pc_put_process ('PATHNAME_MIG',  obj%pathname_mig)
        call pc_put_process ('PATHNAME_REF',  obj%pathname_cas)
        call pc_put_process ('NAME_FUNCT',    obj%name_funct)
        call pc_put_process ('VEL_CONST',     obj%vel_const)
        call pc_put_process ('MODE',          stretch_mode)      
        call pc_put_process ('SMOOTH',        obj%smooth)
        call pc_put_process ('DEGREE',        obj%degree)
        call pc_put_process ('CALC_CV',       stretch_calc_cv)

        call pc_put_process ('NDPT_OUT',      obj%ndpt_fin) 
        call pc_put_process ('TSTRT_OUT',     obj%tstrt_fin)
        call pc_put_process ('DT_OUT',        obj%dt_fin)

        if (associated(obj%stretch_inv)) then                       
            call stretch_update (obj%stretch_inv)
        else
           call stretch_create (obj%stretch_inv)
        end if

        call pc_restore

    end if

  end if
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field ('MODE', mode_options, mode_noptions) 
      call pc_put_options_field ('CALC_CV',calc_cv_options, calc_cv_noptions)
      call pc_put_options_field ('SMOOTH',smooth_options, smooth_noptions)
      call pc_put_options_field ('REE',ree_options, ree_noptions)
      call pc_put_options_field ('STRETCH',stretch_options, stretch_noptions)
      call pc_put_options_field ('opt_mdt', opt_mdt_options, opt_mdt_noptions)
                                                              
      call pc_put ('HDR_SLOW',     obj%hdr_slow)
      call pc_put ('SLOW_INIT',    obj%slow_init)
      call pc_put ('SLOW_INC',     obj%slow_inc)
      call pc_put ('SLOW_LAST',    obj%slow_last)
      call pc_put ('SLOW_TOT',     obj%slow_tot)

      call pc_put ('HDR_FAST',     obj%hdr_fast)
      call pc_put ('FAST_INIT',    obj%fast_init)
      call pc_put ('FAST_INC',     obj%fast_inc)
      call pc_put ('FAST_LAST',    obj%fast_last)
      call pc_put ('FAST_TOT',     obj%fast_tot)

      call pc_put ('DIST_FAST',    obj%dist_fast)
      call pc_put ('MODE',         obj%mode)
      call pc_put ('CALC_CV',      obj%calc_cv)
      call pc_put ('VEL_CONST',    obj%vel_const)
      call pc_put ('PATHNAME_CAS', obj%pathname_cas)
      call pc_put ('NAME_FUNCT',   obj%name_funct)
      call pc_put ('PATHNAME_MIG', obj%pathname_mig)
      call pc_put ('SMOOTH',       obj%smooth)
      call pc_put ('DEGREE',       obj%degree)
      call pc_put ('FSWP',         obj%fswp)
      call pc_put ('DAP',          obj%dap)
      call pc_put ('REE',          obj%ree)
      call pc_put ('STRETCH',      obj%stretch)
      call pc_put ('MEM_MAX',      obj%mem_max)
      call pc_put ('opt_mdt',      obj%opt_mdt)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

! Deallocate the permanent buffers

      if (associated(obj%sect ))   deallocate      (obj%sect)
      if (associated(obj%tmac ))   deallocate      (obj%tmac)
      if (associated(obj%vmac ))   deallocate      (obj%vmac)
      if (associated(obj%cdelta )) deallocate      (obj%cdelta)
      if (associated(obj%fdcur ))  deallocate      (obj%fdcur)
      if (associated(obj%fdout ))  deallocate      (obj%fdout)
      if (associated(obj%binlim )) deallocate      (obj%binlim)
      if (associated(obj%tr_stor)) deallocate      (obj%tr_stor)

      if (associated(obj%fftcc_pos_obj))  call fft_delete (obj%fftcc_pos_obj)
      if (associated(obj%fftcc_neg_obj))  call fft_delete (obj%fftcc_neg_obj)
      if (associated(obj%fftrc_xobj))     call fft_delete (obj%fftrc_xobj)
      if (associated(obj%fftcr_xobj))     call fft_delete (obj%fftcr_xobj)

!   initialize variables
                                      
      obj%num_panels   = 1
      obj%mult_init    = 1.0
      obj%mult_inc     = 0.05
        
      mem_use = obj%mem_max * obj%ndpt
      ndpt_max = max( obj%ndpt_fin, obj%ndpt_fout) 

      ier = fktmig_grid (obj, ndpt_max)
      if (ier/=0) call pc_error ('Error creating FKTMIG: GRID')

      ier = fktmig_mem (obj, ndpt_max, mem_use, nstore, nscratch)

      if (ier/=0) call pc_error ('Error in FKTMIG: creating MEMORY')
 
      need_label   = 'YES'
      need_request = 'YES'
      twosets      = 'NO'
      iftd         = .false.
      ndisk        = 0
      ntapes       = 0 

      nstore = nstore + 1
      nscratch = 1                       

      call pc_put_control ('nstore',             nstore)
      call pc_put_control ('nscratch',         nscratch)
      call pc_put_control ('need_label',     need_label)
      call pc_put_control ('need_request', need_request)
      call pc_put_control ('twosets',           twosets)
 
!<execute_only>
 
      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      allocate (obj%sect(obj%ntsw,obj%nb22),stat=ier)
      if (ier/=0) call pc_error ('Error creating FKTMIG: sect array')   

      allocate (obj%tmac(201),stat=ier)
      if (ier/=0) call pc_error ('Error creating FKTMIG: tmac array')
 
      allocate (obj%vmac(201),stat=ier)
      if (ier/=0) call pc_error ('Error creating FKTMIG: vmac array') 

      allocate (obj%cdelta(300),stat=ier)
      if (ier/=0) call pc_error ('Error creating FKTMIG: cdelta array')
 
      allocate (obj%fdcur(100),stat=ier)
      if (ier/=0) call pc_error ('Error creating FKTMIG: fdcur array') 

      allocate (obj%fdout(100),stat=ier)
      if (ier/=0) call pc_error ('Error creating FKTMIG: fdout array')

      allocate (obj%binlim(3),stat=ier)
      if (ier/=0) call pc_error ('Error creating FKTMIG: binlim array') 

      allocate (obj%tr_stor(ndpt_max+2*obj%nwih+1),stat=ier)
      if (ier/=0) call pc_error ('Error creating FKTMIG: binlim array')

      ier =  fft_create (obj%fftcc_pos_obj, 1, obj%nt2, 'ctoc')
      if (ier/=0) call pc_error ('Error creating rtoc FFT object')
      ier =  fft_create (obj%fftcc_neg_obj, -1, obj%nt2, 'ctoc')
      if (ier/=0) call pc_error ('Error creating rtoc FFT object')

      ier =  fft_create (obj%fftrc_xobj, -1, obj%nb2, 'rtoc')
      if (ier/=0) call pc_error ('Error creating rtoc FFT object')
      ier =  fft_create (obj%fftcr_xobj, +1, obj%nb2, 'ctor')
      if (ier/=0) call pc_error ('Error creating ctor FFT object')


      !  negative tstart not allowed 

      if (obj%tstrt < 0.0) then                  
          call pc_error ('FKTMIG: ERROR IN GLOBALS (negative TSTART)' )
      else if (obj%tstrt > 0.0) then
          call pc_error ('FKTMIG: ERROR IN GLOBALS (TSTART > 0)' )
      end if

     !   initialize variables
                                      
        obj%nlive = 0 
        obj%nline = 0 
        obj%icnt = 1 
        obj%velpan = 1 
        obj%ndump = 0 
        obj%binlim(1) = 2*obj%fast_tot 
        obj%binlim(2) = -1 
        obj%binlim(3) = 0 
        obj%bcnt = 0 
        obj%lineold = -67 
        obj%linenew = 0 
        obj%iflag = 1 
        obj%mute = 400.0 

        obj%fswp = max(obj%fswp,0.) 
        obj%fswp = min(obj%fswp,1.) 
        obj%dap = max(obj%dap,0) 
        obj%num_panels = max(1,obj%num_panels) 
        if (obj%mult_inc == 0.) obj%num_panels = 1 
 
        if (obj%pathname_cas /= PATHCHECK_EMPTY) then
            obj%wfac = 1.0
            !  read in Casaded velocity function
             ier = fktmig_0 (obj)
        else
           obj%nvmac = 1 
           obj%tmac(1) = 0.0 
           obj%vmac(1) = obj%vel_const  
        end if

       do i = 2, obj%nvmac

         if ( obj%vmac(i) < obj%vmac(i-1) ) then
            write(obj%print_lun, *) ' Interval Velocity function  '
            do n = 1, obj%nvmac
               write(obj%print_lun, "(' time-intvel ',i6,3x,2f12.4)" ) &
               n, obj%tmac(n), obj%vmac(n)
            end do
            call pc_error( 'FKTMIG: Velocity inversion is not allowed ' )
         end if
       end do


      !   compute freq-interpolation coeficients 
        L = 10 
        if (fktmig_op(obj, L) /= 0) then 
          call pc_error( 'FKTMIG: ERROR-FKTMIGOP CALL FAILED' )   
        endif 
 
        if (obj%stretch == 'NO') then 
           obj%istretch = 0 
            write(obj%print_lun, *) 'FKTMIG: NO STRETCHING APPLIED ' 
        else 
           obj%stretch = 'YES' 
           obj%istretch = 1
            write(obj%print_lun, *) 'FKTMIG: STRETCHING APPLIED '    
         endif

        obj%fdcur = -1 
        obj%fdout = -1 
        obj%fdhdr = -1 

       !   open and/or initialize disk areas for caching data 

!       write(obj%print_lun,*)     &
!            ' *** obj%nb2, obj%nblks, obj%ntsw, obj%nt2***',  &
!            obj%nb2, obj%nblks, obj%ntsw, obj%nt2

       !  cache input traces
        tfile = tempname('fktmig_input_data')                      
        n = fktmig_open(tfile, obj%fdcur, obj%nblks) 
        if (n <= 0) then 
          call pc_error( 'FKTMIG: ERROR IN OPENING INPUT CASHE FILE = ')   
        endif 
 
        tfile=tempname('fktmig_output_data')      
        n= fktmig_open(tfile, obj%fdout, obj%nblks) 
        if (n <= 0) then 
          call pc_error( 'FKTMIG: ERROR IN OPENING OUTPUT CASHE FILE = ')   
        endif 


        obj%nb22 = obj%nb2 + 2 
        n = 1 
        if (obj%num_panels > 1) n = 2 
        call fktmig_dinit (obj, obj%fdcur, obj%nb22*obj%ntsw, n) 

       !   provide a disk file to cache obj%fast_tot trace headers. 
        tfile = tempname('fktmig_header_data')
       !  create  header data file 
        n = fktmig_open(tfile, obj%fdhdr, 1) 
        if (n <= 0) then 
          call pc_error( 'FKTMIG: ERROR IN OPENING OUTPUT FILE = ')   
        endif

       !   initialize header disk file for each new group

        if (fktmig_init_hdrs(obj, obj%fdhdr(1)) == 0) then  
          call pc_error( 'FKTMIG: ERROR IN OPENING HEADER FILE = ') 
          return  
        endif 

     if (obj%fast_inc /= 1.) then
        write(obj%print_lun, *) ' '
        write(obj%print_lun, *) ' ******************************* '
        write(obj%print_lun, *) 'FKTMIG_Info: Migration spacing = ',  &
           obj%dist_fast*obj%fast_inc
        write(obj%print_lun, *) ' ******************************* '
      end if

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine fktmig_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!! Upon input, NTR will have one of these values:
!!   NTR >= 1              means to process the input traces.
!!   NTR == NO_MORE_TRACES means there are no more imput traces.
!!   NTR == NEED_TRACES    means someone from below needs more traces.
!!   NTR == NEED_TRACES    might mean this is a trace-supplying process.
!!   NTR == NEED_TRACES    will not occur unless this process has a label.
!!
!! Upon output, NTR must have one of these values:
!!   NTR >= 1              if you are outputting traces.
!!   NTR == NO_MORE_TRACES if there are no more traces to output.
!!   NTR == FATAL_ERROR    if you have a fatal error.
!!   NTR == NEED_TRACES    if you need another trace before passing any out.
!!   NTR == NEED_TRACES    must not occur unless you specified that you
!!                           might need to request more traces.
!!
!<execute_only>

      subroutine fktmig (obj,ntr,hd,tr)
      implicit none

      type(fktmig_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout)    :: hd(:,:)               ! arguments
      real             ,intent(inout)    :: tr(:,:)               ! arguments


      integer :: i, k          , bin, nperl                   


      double precision  :: ht(obj%nwih,1) 
      real              :: tro(obj%nt2*2,1) 
      real              :: perc 
      integer           :: ifirst, ilast
      integer           :: ib
      integer,save      :: bin_prev
      integer,save      :: init = 0
      double precision  :: ht2(obj%nwih,1) 
      real              :: tro2(obj%nt2*2,1)       
! 
!---what is the current mode for this module 
!   obj%iflag.....1 collecting and storing a group-line  (initial state) 
!   obj%iflag.....2 processing traces in a group-line. n.ne.0 
!   obj%iflag.....3 processing traces in a group-line. ntr = 0 
!   obj%iflag.....4 dumping traces in a group-line to output. ntr.ne.0 
!   obj%iflag.....5 dumping traces in a group-line to output. ntr = 0 
!   obj%iflag.....6 obj%slow_tot pattern was completed. wait for ntr=0 
!   obj%iflag.....7 ntr=0 was detected and dumps finished.return 
! 
  
      if (obj%iflag==4 .or. obj%iflag==5) go to 250 

      if (obj%iflag == 7 .or. ntr == FATAL_ERROR) then 
        ntr = 0 
        call fktmig_wrapup (obj)   
        return  
      endif 

      if (ntr == 0) then 
        if (obj%nline >= obj%slow_tot) then 
          obj%iflag = 7  
        else if (obj%nline < obj%slow_tot) then 
          obj%iflag = 3 
          obj%t0c = obj%tstrt_fin 
          obj%ntc = obj%ndpt_fin
          obj%dtc = obj%dt_fin
          if (obj%istretch == 1) then          
            obj%t0c = obj%tstrt_fout 
            obj%ntc = obj%ndpt_fout
            obj%dtc = obj%dt_fout
          endif 
          if (obj%nlive >= 1) go to 85 
        endif 
        return  
      else if (ntr > 1) then 
        write(obj%print_lun, *) 'FKTMIG: INPUT ONLY 1 TRACE AT A TIME' 
        ntr = FATAL_ERROR  
        return  
      endif 
      if (obj%iflag == 6) then 
        ntr = NEED_TRACES 
        return  
      endif 
 
!                  collection mode 
! compute group-basement bin of input trace. purge dead traces, 
! and basements out of range. store the header and trace values 
! on disk until a group is complete. 
! obj%binlim(1).....bin with 1st  live trace 
! obj%binlim(1).....bin with last live trace 
! obj%nlive......no. of live traces collected 


      if ( init == 0) then
         init = 1
         ht2(obj%nwih,1) = 0.0
         tro2(obj%nt2*2,1) = 0.0
      end if
      
      if (obj%istretch == 1) then 
        tro(1:obj%ndpt, 1) = tr(:obj%ndpt, 1)
        ht(1:obj%nwih, 1)  = hd(:obj%nwih, 1)
        call stretch (obj%stretch_for, ntr, ht, tro)  
        obj%t0c = obj%tstrt_fout 
        obj%ntc = obj%ndpt_fout 
        obj%dtc = obj%dt_fout
      else 
        obj%t0c = obj%tstrt_fin 
        obj%ntc = obj%ndpt_fin 
        obj%dtc = obj%dt_fin 
        tro(:obj%ntc, 1) = tr(:obj%ntc,1) 
        ht(:obj%nwih, 1) = hd(:obj%nwih,1) 
      endif 
 
   61 continue 

       bin = nint((ht(obj%hdr_fast,1)-obj%fast_init)/obj%fast_inc) + 1      
       obj%linenew = nint((ht(obj%hdr_slow,1)-obj%slow_init)/obj%slow_inc) + 1
       ht(30,1) = 1.
       
     if (bin < 1 .or. bin > obj%fast_tot) then  
        ntr = NEED_TRACES  
        return  
      endif 
      
      if (obj%nlive == 0) then
        obj%binlim(1) = bin
        bin_prev = bin
      end if
      
      if (obj%lineold == obj%linenew) then 
        obj%nlive = obj%nlive + 1 
        if (obj%nlive > obj%nb2) then 
          write(obj%print_lun, *) 'FKTMIG: NLIVE INPUT TRACES=', obj%nlive, & 
            ', EXCEEDES MAXIMUM ALLOWED   NB2=', obj%nb2 
          ntr = FATAL_ERROR  
          return  
        endif 
        k = obj%ntsw*obj%nb2
        
        ! Fill missing traces 
        if ( abs( bin-bin_prev) > 1) then

          if ( bin > bin_prev) then
            ifirst = bin_prev+1
            ilast  = bin-1
          else 
            ifirst = bin_prev-1
            ilast  = bin+1
          end if 
                    
          do ib = ifirst, ilast
             obj%nlive = obj%nlive + 1                        
             call fktmig_save_input (obj, obj%fdcur, obj%fdhdr(1), obj%ntc, & 
                 ib, ht2(:,1), tro2(:,1), k, obj%sect)
          end do
        end if               
                             
        call fktmig_save_input (obj, obj%fdcur, obj%fdhdr(1), obj%ntc,    & 
            bin, ht(:,1), tro(:,1), k, obj%sect) 
          
        bin_prev = bin
                                  
        ntr = NEED_TRACES
        return  
      else 
        obj%iflag = 1 
        if (obj%nlive == 0) then 
          obj%lineold = obj%linenew 
          go to 61 
        endif
         
        obj%tr_stor(1+obj%ntc:obj%nwih+obj%ntc) = ht(:obj%nwih, 1) 
        obj%tr_stor(1:obj%ntc) = tro(1:obj%ntc, 1) 
        tro(1:obj%ntc, 1) = 0.0 
        obj%iflag = 2 
      endif  
 
   85 continue 
!     migrate the collected traces
      if (obj%nlive == 0) then                        
        write(obj%print_lun, *) 'FKTMIG: There are no traces to migrate??' 
        ntr = FATAL_ERROR  
        return  
      else 
        write(obj%print_lun, *) 'FKTMIG: GROUP #=', obj%lineold,  & 
          ' TRACES IN=', obj%nlive, ', 1ST LIVE BIN=', obj%binlim(1), &
          ', LAST LIVE BIN=', obj%binlim(2) 
      endif 

      !   will flush to disk any traces remaining in buffer(i.e.obj%bcnt > 0) 
      i = fktmig_flush_inbuf(obj, obj%fdcur, obj%ntsw*obj%nb2, obj%sect)  

! initialize headers for missing traces. cached on disk(fdhdr) 

      call fktmig_fill_hdrs (obj, obj%fdhdr(1), obj%hdr_fast, obj%ndpt,     &   
         obj%fast_tot, obj%fast_init,  obj%fast_inc) 
 
! fft over basements to the wavenumber domain qx. 
! the section may be cached to a file with descriptor obj%fdcur. 

      if (fktmig_xtokx(obj, obj%fdcur) == 0) then 
        call pc_error(' FKTMIG: Forward FFT to wavenumber failed!' )
        ntr = FATAL_ERROR  
        return  
      endif  

! transform from time to frequency then perform a frequency 
! stretch on each trace to transform(i.e. migrate) to vertical 
! wavenumber. the grid for vertical wavenumbers is dqz=dw*(2/v) 
! (a scale factor times the frequency grid). there is a maximum 
! allowed vertical wave number for every horizontal wavenumber qx. 
! then inverse fft from vertical wavenumber to pseudo-z. 
 
  199 continue 

      if (fktmig_migrate(obj,obj%fdcur) == 0) then                        
         write(obj%print_lun, *) 'FKTMIG: Time FFTs or migration failed!' 
        ntr = FATAL_ERROR  
        return  
      endif 

! inverse spatial fft. output the final data to a file(obj%fdout) 
! in a form ready to dump to output.  

      ifirst = obj%binlim(1)
      ilast  = obj%binlim(2)
      if (fktmig_kxtox(obj, obj%fdcur, obj%fdout,                    & 
               ifirst, ilast  ) == 0) then              
        write(obj%print_lun, *) 'FKTMIG: KXTOX transform failed' 
        ntr = FATAL_ERROR  
        return  
      endif 

! reset flags and variables for dumping the migrated traces to 
! output. set appropriate dump mode  

      nperl = abs(obj%binlim(2)-obj%binlim(1)) + 1 
      perc = obj%mult_init + (obj%velpan - 1)*obj%mult_inc 
      write(obj%print_lun, *) ' ' 
      write(obj%print_lun, *) 'FKTMIG: GROUP #=', obj%lineold,    & 
        ' VELOCITY PANEL=', obj%velpan, ' PERCENTAGE=', perc,    &
        ', # INPUT=', obj%nlive, ', # FOR OUTPUT=', nperl 

!     reset dump counter to 1

      obj%icnt = 1                                    
      if (obj%iflag==2 .or. obj%iflag==4) obj%iflag = 4 
      if (obj%iflag==3 .or. obj%iflag==5) obj%iflag = 5 

 
!---dump the traces for the group 
!---no. to dump=num_panels*abs(obj%binlim(1)-obj%binlim(2)+1) 
!---iflag...4 or 5 for dump mode 
 
  250 continue 
      nperl = abs(obj%binlim(2)-obj%binlim(1)) + 1 
      if (obj%icnt > nperl) then                   !is the dump done? 
        if (obj%velpan < obj%num_panels) then 
          obj%velpan = obj%velpan + 1 
          go to 199 
        endif 

        obj%lineold = obj%linenew 
        obj%nlive = 0 
        obj%nline = obj%nline + 1 
               
!       ntr=0 not seen yet 
        if (obj%iflag == 4) then                     
          ntr = 1 
!         revert to collection mode
          obj%iflag = 1 
!         is obj%slow_tot satisfied?                              
          if (obj%nline >= obj%slow_tot) then                 
            obj%iflag = 6 
            ntr = NEED_TRACES 
            return  
          endif 

          if (fktmig_init_hdrs(obj, obj%fdhdr(1)) == 0) then  
!           re-initialize a dummy header array and disk area. 
            ntr = FATAL_ERROR  
            return  
          endif 
! 
! re-initialize disk area for traces so missing ones won't be trash 
          obj%nb22 = obj%nb2 + 2 
          ntr = 1 
          if (obj%num_panels > 1) ntr = 2 
          call fktmig_dinit (obj, obj%fdcur, obj%nb22*obj%ntsw, ntr) 
! 
!   restore the obj%original header and trace saved in tr_stor
          ht(:obj%nwih, 1)  = obj%tr_stor(obj%ntc+1:obj%nwih+obj%ntc) 
          tro(1:obj%ntc, 1) = obj%tr_stor(1:obj%ntc) 
! 
!  return to start accumulating the next line 
          go to 61 
        else if (obj%iflag == 5) then            !ntr=0 was seen 
          ntr = 0 
          obj%iflag = 7 
          call fktmig_close (obj%fdcur, obj%nblks) 
          call fktmig_close (obj%fdout, obj%nblks) 
          call fktmig_close (obj%fdhdr, 1)
 
          return  
        endif 
      endif 
! 
!---get the trace & header for output 

 110  continue
 
      if (fktmig_get_output(obj, nperl, obj%ntc, obj%hdr_fast,      & 
          obj%fast_init,  obj%fast_inc, ht(:,1), tro(:,1)) == 0) then 
 
          call pc_error(' FKTMIG: PROBLEM FOR OUTPUT TRACE ',obj%icnt) 
        ntr = FATAL_ERROR 
        return  
      endif 

!---unstretch the traces if stretching is used 
      if (obj%istretch == 1) then 
        ntr = 1
        call stretch (obj%stretch_inv, ntr, ht, tro)
      endif 

      hd(:obj%nwih,1) = ht(:obj%nwih, 1) 
      tr(:obj%ndpt,1) = tro(1:obj%ndpt, 1)
      
      if (hd(30,1) >= obj%ndpt .and. obj%opt_mdt =='KILL') then
        tr(1:obj%ndpt,1) = 0.0
        hd(2,1) = obj%ndpt
        hd(64,1) = 1
      else if (hd(30,1) >= obj%ndpt .and. obj%opt_mdt =='DEL') then 
        ntr = 1 
        obj%icnt = obj%icnt + ntr
        go to 110
      end if 
      
!---reapply the head and tail mutes (no taper) 
      call mutehw (hd(:,1), tr(:,1), obj%ndpt, 0.0, -3) 

!      recompute Lav of the trace
       call lav_set_hdr (hd, tr, obj%ndpt, 1)

      obj%ndump = obj%ndump + 1 
      hd(1,1) = float(obj%ndump) 
      hd(3,1) = (obj%icnt - 1)/nperl + 1              
      hd(4,1) = nperl 
      ntr = 1 
      obj%icnt = obj%icnt + ntr      
      
      return  

      end subroutine fktmig 


      integer function fktmig_mem (obj, ndpt, mem_use, memsto, memscr) 
      implicit none 

      type(fktmig_struct),intent(inout) :: obj               ! arguments
      integer,     intent(out)  :: memsto                   ! arguments
      integer,     intent(out)  :: memscr                   ! arguments
      integer,     intent(in)   :: ndpt                     ! arguments
      integer,     intent(in)   :: mem_use                  ! arguments

      integer ::  nth, mem_min, mem_max, memxt, msect       ! local
         
      !  Compute the permanent and scratch memory for fktmig. 

      fktmig_mem = 0 
      nth = obj%nt2/2 
      obj%nb22 = obj%nb2 + 2 
      mem_min = obj%nb22*1024 
      mem_max = obj%nb22*obj%ndpt 
      obj%ntsw = 1024 
 
      memxt = ndpt + obj%nwih 
      msect = mem_use - memxt 
      if (msect>=mem_max .and. obj%num_panels==1) then 
        msect = mem_max 
        obj%ntsw = ndpt 
      else if (msect >= mem_min) then 
        msect = mem_min 
        obj%ntsw = 1024 
      else 
        obj%mem_max =  nint(1.0*mem_min/obj%ndpt) + 1
        call pc_info(' FKTMIG: Reset MEM_MAX to required minimum memory ')
      endif 

      memsto = memxt + msect

      memscr = 2*obj%nt2 +2*obj%nt2*obj%nb22 + obj%nb22 + 2*obj%nt2 + 2*obj%nt2 
      obj%nblks = (ndpt - 1)/obj%ntsw + 1
 
      write(obj%print_lun,*)' FKTMIG:------------------------------------------'
      if (obj%nblks == 1) then 
        write(obj%print_lun, *) 'FKTMIG: MIGRATION IS DONE IN-CORE' 
      else if (obj%nblks <= 52) then 
        write(obj%print_lun, *) 'FKTMIG: MIGRATION IS DONE OUT-OF-CORE' 
      else 
        write(obj%print_lun, *) 'FKTMIG: TOO MANY TIME SWATHS NEEDED' 
        fktmig_mem = 1 
        return  
      endif 
      write(obj%print_lun, *) 'FKTMIG: NO. OF DISK-TIME  SWATHS=', obj%nblks 
      write(obj%print_lun, *) 'FKTMIG: NO. OF SAMPLES PER SWATH=', obj%ntsw 
      write(obj%print_lun, *) 'FKTMIG:--------------------------------------' 
      write(obj%print_lun, *) ' ' 

      return  
      end function fktmig_mem 


      integer function fktmig_grid (obj, ndpt) 

      implicit none 
      type(fktmig_struct),intent(inout) :: obj                ! arguments  
      integer,     intent(in)  :: ndpt                       ! arguments

      integer      :: nxfft, ntfft                           ! local

      fktmig_grid = 0 

      !  grid parameters for basement grid 

      nxfft = fktmig_npow2(obj%fast_tot + 200) 
      obj%nb2 = 2**nxfft 
      obj%nbnyq = obj%nb2/2 + 1 
      obj%nb22 = obj%nb2 + 2 
      if (obj%nb2 > 32768) then 
        call pc_error (' FKTMIG: ERROR, # OF BASEMENTS IS > 32768 ' ) 
        fktmig_grid = 1 
        return  
      endif 
 
      !   grid parameters for time grid 

      ntfft = min(fktmig_npow2(ndpt + 200),30) 
      obj%nt2 = 2**ntfft 

      write(obj%print_lun, *) 'FKTMIG:----------------------------------------' 
      if (obj%nb2 > 2**fktmig_npow2(obj%fast_tot)) then 
        write(obj%print_lun, *) ' FKTMIG: HORIZONTAL FFT SIZE = ',         & 
           obj%nb2, ' (padded).' 
      else 
        write(obj%print_lun, *) ' FKTMIG: HORIZONTAL FFT SIZE = ', obj%nb2 
      endif 
      if (obj%nt2 > 2**fktmig_npow2(ndpt)) then 
        write(obj%print_lun, *) ' FKTMIG: VERTICAL FFT SIZE = ',           &
           obj%nt2, ' (padded).' 
      else 
        write(obj%print_lun, *) ' FKTMIG: VERTICAL FFT SIZE = ', obj%nt2 
      endif 
      write(obj%print_lun, *) 'FKTMIG:---------------------------------------' 
      write(obj%print_lun, *) ' ' 

      return  
      end function fktmig_grid 


      integer function fktmig_npow2 (n) 
      integer, intent(in) :: n 
      fktmig_npow2 = int(0.99999 + alog(float(n))/alog(2.)) 
      return  
      end function fktmig_npow2

      integer function fktmig_open (filename, fd, nblk) 

      ! the basic storage block is n2 vectors of n1 samples. 
      ! the traces are stored as nblk swaths of n1*n2 words. 
      ! the total space needed is nv*nblk*n1*n2 

      implicit none 

      integer ,    intent(in)         :: nblk              ! arguments
      character(len=*),intent(inout)  :: filename          ! arguments 
      integer,     intent(inout)      :: fd(:)             ! arguments
  
      character(len=FILENAME_LENGTH) :: tfile              ! local
      character(len=FILENAME_LENGTH) :: tfile1             ! local
      logical                        :: fscratch           ! local
      character(len=60)              :: pid                ! local
      character(len=2)               :: ext                ! local
      integer                        :: nchar      , j ! local
 
      fktmig_open = -1

       fscratch = .true.
       nchar = 2
       pid = string_ii2ss(getsys_pid())
       fscratch = .true. 
       tfile1 = trim(filename)//'_'//trim(pid)

       do j = 1, nblk  
         call string_ii2cc( j, ext, nchar) 
         tfile = trim(tfile1)//'_'//ext
         fd(j) = cio_fopen( tfile, "w+", fscratch)
         if (fd(j) < 0) then 
            call pc_error( ' FKTMIG: OPEN FAILURE FOR CACHE FILE' )
         end if
         fktmig_open = fd(j) 
       end do 


        return  
        end function fktmig_open 

 
      subroutine fktmig_dinit(obj, fd, n1, n2) 

      implicit none 
      type(fktmig_struct),intent(inout) :: obj           ! arguments
      integer , intent(in) :: n1                         ! arguments
      integer , intent(in) :: n2                         ! arguments
      integer , intent(in) :: fd(:)                      ! arguments

      integer :: status                                  ! local
      integer :: j, k, n, off, nby                       ! local 
      real,pointer    :: work(:)                         ! local
 
      allocate (work(n1))

      if (n1<=0 .or. n2<=0) return  
      work(:n1) = 0.0 
 
      do j = 1, max(obj%nblks,1) 
        do k = 1, max(n2,1) 
          nby = n1 
          off = (k - 1)*nby*obj%byt_size 
          if (fd(j) <= 0) cycle
          status = cio_fseek(fd(j), off, obj%origin)
          n = cio_fwrite(work, obj%byt_size, nby, fd(j))
          if (n == nby) cycle  
          call pc_error(' FKTMIG: ERROR INITIALIZING DISK'  ) 
        end do 
      end do 

      deallocate (work)

      return  
      end subroutine fktmig_dinit 

 
      subroutine fktmig_close(fd, nblk)  

      implicit none 

      integer , intent(in)    :: nblk            ! arguments
      integer , intent(inout) :: fd(:)           ! arguments 
      integer :: n, status                       ! local
 
      do n = 1, nblk 
        if (fd(n) <= 1) cycle  
        status = cio_fclose (fd(n), .true.) 
        if ( status < 0 ) then
          call pc_error(' FKTMIG: ERROR CLOSING DISK FILES'  )
        end if
        fd(n) = -1 
      end do 
      return  
      end subroutine fktmig_close 

 
      subroutine fktmig_save_input(obj, fd, fdh, n1, bin, hd, tr,    & 
       bufsiz, buf)  

      implicit none 
      type(fktmig_struct),intent(inout) :: obj             ! arguments
      integer  :: fdh                                     ! arguments
      integer , intent(in) :: n1                          ! arguments
      integer , intent(in) :: bin                         ! arguments
      integer , intent(in) :: bufsiz                      ! arguments 
      integer, intent(in)  :: fd(:)                       ! arguments 
      double precision , intent(in)  :: hd(:)             ! arguments
      real , intent(inout) :: tr(:)                       ! arguments
      real  , intent(inout) :: buf(obj%ntsw,*)            ! arguments 

      integer ::    j, k   , nby, off ! local
      integer ::        gulp, bpos      , status ! local
      real    :: spwrfac, rfold                           ! local
 
      obj%mute = min(1.d0*obj%mute, hd(2)) 
      obj%binlim(2) = bin
      !  fold of stack weighting 
      rfold = hd(5)
      spwrfac = max(rfold,1.)**obj%fswp               
      tr(:n1) = tr(:n1)*spwrfac 
      tr(n1+1:obj%ntsw*obj%nblks) = 0.0 
 
      gulp = bufsiz/(obj%ntsw*obj%nblks)
      !  keep track of 1st trace in buffer 
      if (obj%bcnt == 0) then                         
        bpos = 1 
        obj%binlim(3) = bin 
      endif 
      bpos = abs(bin - obj%binlim(3)) + 1 

      if (bpos > gulp) then 
        j = fktmig_flush_inbuf(obj,fd,bufsiz,buf) 
        bpos = 1 
        obj%binlim(3) = bin 
      endif 
      bpos = abs(bin - obj%binlim(3)) + 1 
 
      j = 1 
      off = bpos 
      !  store in swaths 

      do k = 1, obj%nblks                            
        if (obj%ntsw > 0) then 
          buf(:obj%ntsw,off) = tr(j:obj%ntsw-1+j) 
          j = obj%ntsw + j 
        endif 
        off = off + gulp 
      end do 

      obj%bcnt = obj%bcnt + 1        
              
      nby = obj%nwih
      off = abs(bin - obj%binlim(1))*nby* obj%dbyt_size
      status = cio_fseek(fdh, off, obj%origin)
      status = cio_fwrite( hd(1:nby), obj%dbyt_size, nby, fdh) 

      return  
      end subroutine fktmig_save_input 

 
      integer function fktmig_flush_inbuf (obj, fd, bufsiz, buf)
 
      ! flush the input buffer to disk and reset bcnt & buf
      implicit none

      type(fktmig_struct),intent(inout) :: obj            ! arguments 
      integer , intent(in) :: bufsiz                     ! arguments
      integer , intent(in) :: fd(:)                      ! arguments 
      real, intent(inout)  :: buf(obj%ntsw,*)            ! arguments 
 
      integer :: k, istat, gulp, nby, off                ! local
 
      gulp = bufsiz/(obj%ntsw*obj%nblks) 
      fktmig_flush_inbuf = 1 
      if (obj%bcnt == 0) return 

      do k = 1, obj%nblks 
        nby = obj%bcnt*obj%ntsw 
        off = abs(obj%binlim(1)-obj%binlim(3))*obj%ntsw*obj%byt_size 
        istat = cio_fseek(fd(k), off, obj%origin)
        istat = cio_fwrite(buf(1:,(k-1)*gulp+1:k*gulp),obj%byt_size, &
                            nby,fd(k)) 
        if (istat == nby) cycle  
        call pc_error( ' FKTMIG_FLUSH_INBUF: ERROR ' ) 
        fktmig_flush_inbuf = 0 
        return  
      end do 
      obj%bcnt = 0 
      buf(:bufsiz,1) = 0.0 
      return  
      end function fktmig_flush_inbuf 


      integer function fktmig_get_output (obj, gsize,  & 
            n1, h2,  o2, d2, hd, tr) 

      implicit none 
      type(fktmig_struct),intent(inout) :: obj                  ! arguments

      integer , intent(in) :: gsize                             ! arguments
      integer , intent(in) :: n1                                ! arguments
      integer , intent(in) :: h2                                ! arguments 
      real ,    intent(in) :: o2                                ! arguments
      real ,    intent(in) :: d2                                ! arguments
      double precision, intent(inout) :: hd(:)                  ! arguments 
      real, intent(inout)             :: tr(:)                  ! arguments
 
      integer   ::          n, i   , i1, i2, ib ! local
      integer   ::       nby, off, gpos  ! local
      real      :: x1, x2                                       ! local
      real      :: tr_tmp(obj%ntsw)                             ! local

 
      fktmig_get_output = 0 
 
      !  get the trace for output. 

      do ib = 1, obj%nblks 
 
        nby =  obj%ntsw
        off = (obj%icnt-1)* obj%ntsw * obj%byt_size
        i = cio_fseek(obj%fdout(ib), off, obj%origin)
        i = cio_fread(tr_tmp, obj%byt_size, nby, obj%fdout(ib))
        i1 = (ib - 1)*obj%ntsw + 1 
        i2 = min(n1,ib*obj%ntsw)
        n = i2 - i1 + 1
        tr(i1:i2) = tr_tmp(1:n) 
      end do 


      !  get the header for output. 
      !  position of trace in current group
      gpos = mod(obj%icnt - 1,gsize) + 1           
 
      nby = obj%nwih 
      off = (gpos - 1)*nby*obj%dbyt_size 

      i = cio_fseek( obj%fdhdr(1), off, obj%origin)
      i = cio_fread( hd, obj%dbyt_size, nby, obj%fdhdr(1))

      if ( i  < 0) then 
         write(obj%print_lun, *)                            &
         'FKTMIG==> *Warning* error reading header file. ',  & 
          'Minimal header created.' 
        hd(:obj%nwih) = 0.0 
        x1 = o2 + (obj%binlim(1)-1)*d2 
        x2 = o2 + (obj%binlim(2)-1)*d2 
        if (obj%binlim(1) /= obj%binlim(2)) then 
          hd(h2) = x1 + (x2 - x1)                           &
                      *((gpos - 1)/abs(obj%binlim(1)-obj%binlim(2))) 
        else 
          hd(h2) = x1 
        endif 
        hd(2) = 1 
      endif 
      fktmig_get_output = 1
 
      return  
      end function fktmig_get_output 
 
!     initialize interpolation filter coefficients 

      integer function fktmig_op (obj, L) 

      implicit none 
      type(fktmig_struct),intent(inout) :: obj                  ! arguments
      integer, intent(inout)           :: L                    ! arguments

      integer                :: j, m, lenop1, lenoph, ipt      ! local
      real, dimension(300)   :: delta                          ! local
      real                   :: pi, delsh, rm, denom           ! local
      complex                :: carg                           ! local
      complex, dimension(10) :: cdel2                          ! local 
 
      fktmig_op = 0 
      lenop1 =  L
      if (mod(lenop1,2) /= 0) then 
        write(obj%print_lun, *)                                &
            'FKTMIGOP: OPERATOR WAS NOT EVEN, LENOP=', lenop1 
        lenop1 = lenop1 + 1 
        fktmig_op = 1 
      endif 
      if (lenop1 > 20) then 
        fktmig_op = fktmig_op + 2 
        write(obj%print_lun, *) 'FKTMIGOP: LENOP WAS > 20, LENOP=', lenop1 
      endif 
      if (lenop1 < 2) then 
        fktmig_op = fktmig_op + 5 
        write(obj%print_lun, *) 'FKTMIGOP: LENOP WAS <2, LENOP=', lenop1 
      endif 
      if (fktmig_op /= 0) return  
      L = lenop1 
      write(obj%print_lun, *)                                  &
        'FKTMIG:===========================================' 
      write(obj%print_lun, *)                                  & 
        'FKTMIG: CONVOLUTIONAL INTERPOLATION FOR FREQUENCY SHIFT' 
      pi = 4.0 * atan(1.0)
      lenoph = lenop1/2 

!     shift interpolation window 10 per cent of the trace length 

      delsh = 0.1 
      do m = 1, lenop1 
        carg = cmplx(0.,-2*pi*m*delsh) 
        cdel2(m) = cexp(carg) 
      end do 
! 
      do j = 1, 21 
        rm = (j - 1)*0.05 
        carg = cmplx(0.,(-rm*pi) + 2*pi*delsh*(rm + lenoph)) 
        ipt = (j - 1)*lenop1 
        do m = 1, lenop1 
          denom = lenoph - m + rm 
          delta(m) = 0.0 
          if (denom/=0 .and. j/=1 .and. j/=21) then 
            delta(m) = sin(pi*rm)*(6.0 - abs(denom))/(6.0*denom*pi) 
          else if (j==1 .and. m==lenoph) then 
            delta(m) = 1.0 
          else if (j==21 .and. m==lenoph+1) then 
            delta(m) = -1.0 
          endif 
          obj%cdelta(ipt+m) = delta(m)*cexp(carg)*cdel2(m) 
        end do 
      end do 
      write(obj%print_lun, *)                             &
        'FKTMIG:================================================' 
      write(obj%print_lun, *) ' ' 
      return  
      end function fktmig_op 
 

      !   initialize a dummy header array and disk area. 
      integer function fktmig_init_hdrs (obj, fd) 

      implicit none
      type(fktmig_struct),intent(inout) :: obj             ! arguments
      integer, intent(in)   :: fd                          ! arguments
      double precision      :: hd(obj%nwih)                ! local
      integer               :: k, nby, off, n              ! local
      integer               :: status                      ! local

      fktmig_init_hdrs = 0

      hd(:obj%nwih) = 0.0 
      hd(2) = obj%ndpt + 1 
      if (fd <= (-1)) return  
      do k = 1, obj%fast_tot 
        hd(obj%hdr_fast) = obj%fast_init + (k - 1)*obj%fast_inc 
        nby = obj%nwih 
        off = (k - 1)*nby*obj%dbyt_size
        status = cio_fseek(fd, off, obj%origin)
        n = cio_fwrite (hd, obj%dbyt_size, nby, fd)
        if (n == nby) cycle
        call pc_error ('FKTMIG: ERROR IN CIO_FWRITE FOR HEADER INIT' ) 
        return  
      end do 
      fktmig_init_hdrs = 1 
      return  
      end function fktmig_init_hdrs  
        

      subroutine fktmig_fill_hdrs(obj, fdh, hd2, n1, n2, o2, d2) 

      implicit none 
      type(fktmig_struct),intent(inout) :: obj           ! arguments

      integer, intent(in)  :: fdh                       ! arguments
      integer, intent(in)  :: hd2                       ! arguments
      integer, intent(in)  :: n1                        ! arguments
      integer, intent(in)  :: n2                        ! arguments 
      real, intent(in)     :: o2                        ! arguments
      real, intent(in)     :: d2                        ! arguments 
 
      integer ::   ifill1, ifill2, ifstart, ifstop,  nby        ! local
      integer ::   off                             , status ! local
          
      double precision  :: work1(obj%nwih)
      double precision  :: work2(obj%nwih)
      double precision  :: work3(obj%nwih)
 
      !   fill in headers on missing traces 
      ifill1 = 1                           ! the 1st live trace 
      nby = obj%nwih 
      off = (ifill1 - 1)*nby*obj%dbyt_size 

      status = cio_fseek(fdh, off, obj%origin)
      status = cio_fread(work1,obj%dbyt_size, nby, fdh)
      if (status < 0) then  
         call pc_error(' FKTMIG: ERROR IN CIO_FREAD'  ) 
         return 
      end if 

      ifill2 = ifill1 + 1                  ! the 2nd live trace 
 
      !  fill missing bins between binlim(1) and binlim(2)

   72 continue 

      off = (ifill2 - 1)*nby*obj%dbyt_size 
      status = cio_fseek(fdh, off, obj%origin) 
      status = cio_fread(work2, obj%dbyt_size, nby, fdh)
      if (status < 0) then  
         call pc_error(' FKTMIG: ERROR IN CIO_FWRITE'  ) 
         return 
      end if 

      if (work2(1) == 0.0) then 
        ifill2 = ifill2 + 1 
        if (ifill2 <= abs(obj%binlim(2)-obj%binlim(1)) + 1) go to 72 
      else
 
        ifstart = ifill1 + 1 
        ifstop = ifill2 - 1 
        if (ifstop >= ifstart) call fktmig_hdcalc (obj, fdh,    & 
          ifstart, ifstop, ifill1, ifill2, work1, work2, work3, n1) 
 
        ifill1 = ifill2 
        ifill2 = ifill2 + 1 
        !  shift headers 
        work1(:obj%nwih) = work2(:obj%nwih) 
        if (ifill2 < obj%binlim(2)) go to 72 
      endif 
      return  
      end subroutine fktmig_fill_hdrs 


      subroutine fktmig_hdcalc(obj, fdh, b1, b2, x1, x2, hd1, hd2, hd, n1) 

      !  fdh-file descriptor for header i/o 
      !  extrapolate and interpolate from hd1 and hd2 to hd 
      !  b1 = bin position of first header to determine 
      !  b2 = bin position of last header to determine 
      !  x1 = bin position of 1st reference header 
      !  x2 = bin position of 2nd reference header 
      !  hd1= 1st reference header array 
      !  hd2= 2nd reference header array 
      !  hd = extrapolated set of header words 
      !  n1 = number of samples in trace (needed for dead obj%mute) 

      implicit none
      type(fktmig_struct),intent(inout) :: obj            ! arguments 

      integer               :: fdh                       ! arguments   
      integer , intent(in)  :: b1                        ! arguments
      integer , intent(in)  :: b2                        ! arguments
      integer , intent(in)  :: x1                        ! arguments
      integer , intent(in)  :: x2                        ! arguments  
      integer , intent(in)  :: n1                        ! arguments
      double precision , intent(in)     :: hd1(:)        ! arguments 
      double precision , intent(in)     :: hd2(:)        ! arguments 
      double precision , intent(inout)  :: hd(:)         ! arguments

      integer :: i   , off, nby    , status ! local
      real :: fac                                        ! local
 
      if (fdh <= 0) return  
      nby = obj%nwih 
      do i = b1, b2 
        fac = float(i - x1)/float(x2 - x1) 
        fac = min(fac,1.0) 
        hd(:obj%nwih) = hd1(:obj%nwih) + fac*(hd2(:obj%nwih)-hd1(:obj%nwih)) 
        !  flag dead
        hd(30) = float(n1 + 1)                     
        off = (i - 1)*nby*obj%dbyt_size 
        status = cio_fseek(fdh, off, obj%origin)
        status = cio_fwrite(hd, obj%dbyt_size, nby, fdh) 
                                  
        if (status < 0) then  
          call pc_error(' FKTMIG: ERROR IN CIO_FWRITE'  ) 
          return 
        end if 
 
      end do 
      return  
      end subroutine fktmig_hdcalc 

 
      integer function fktmig_0 (obj) 
  
      implicit none  
      type(fktmig_struct),intent(inout) :: obj                  ! arguments

      integer                  :: i, j, n, nvfun, nwtv         ! local
      integer                  :: ierr, ix, iy, match          ! local
      real                     :: x, y                         ! local
      character(len=8)         :: velname                      ! local
      character(len=4)         :: veltype                      ! local
      character(len=MAX_LINE)  :: msg                          ! local

      real                     :: t_out(1:obj%ndpt)            ! local
      real                     :: v_int(1:obj%ndpt)            ! local


        fktmig_0 = 0
        nwtv = 200
  
!    read in the rms velocity functions used for casaded migration. 

!       retrieve cascaded velocity function 

       if (obj%pathname_cas(1:4) /= PATHCHECK_EMPTY) then 

          write(obj%print_lun, *)               &
            'FKTMIG: ========  CASCADED FK MIGRATION ======== ' 
 
!         open and read the cascaded velocity file
          call velio_open_read(obj%velio, obj%pathname_cas, nvfun,  &
            ierr, msg, ix, iy)
          if ( ierr /= 0) then
             call pc_error(' FKTMIG: Error to retrieve CASCADED velocity')  
             return
          end if  

!        compute the DIX interval velocity 
          if ( obj%name_funct == 'FIRST') then                        
            obj%tmac = 0.0
            obj%vmac = 0.0
            call velio_read_velfun(obj%velio, x, y, n, obj%tmac, obj%vmac,    &
               ierr, msg, velname, veltype ) 
            if ( ierr /= 0) then
              call pc_error ('FKTMIG: ERROR READ V file', trim(msg) )
              return
            end if
          else 
!           read the right velocity function
            match = -1
            do j = 1, nvfun
              obj%tmac = 0.0
              obj%vmac = 0.0
              call velio_read_velfun(obj%velio, x, y, n, obj%tmac,     &
                 obj%vmac, ierr, msg, velname, veltype ) 
              if ( obj%name_funct == velname ) then
                 match = 0
                 exit
              end if
            end do
            if ( match /= 0) then
             call pc_error(' STRETCH: NAME_FUNCT = ',obj%name_funct ,   & 
                           ' is not found on file ', obj%pathname_cas) 
             call pc_info(' NAME_FUNCT is found on file  = ', velname)   

               return
            end if
        end if
 
 
         if (n==0 .or. n>nwtv) then 
           call pc_error ( 'FKTMIG: ERROR VEL-function length= ', n) 
           return
         end if

!          convert from input type to DIX velocity
!          VTIN -- interval velocity 2-way time

           call string_to_upper (veltype)
           if (veltype /= 'VTIN' ) then
              call velutil_convert(veltype, n, obj%tmac, obj%vmac,  &
                'VTIN', t_out, v_int, ierr)

              if (ierr /= 0) then
                call pc_error(' FKTMIG: Velocity conversion error in VELGEN ') 
                return
              end if 
           else
              t_out(1:n) = obj%tmac(1:n)
              v_int(1:n) = obj%vmac(1:n)
           end if

          if (n > 1) then 
            t_out(n) = t_out(n) + 1.0 
          else 
            t_out(n) = obj%ndpt*obj%dt + 1.0 
          endif 
          if (t_out(n) < (obj%ndpt + 1)*obj%dt)   &
              t_out(n) = obj%ndpt*obj%dt + 1.0 
 
!         clip to end of trace
          do i = 1, n - 1                             
            if (t_out(i+1) <= (obj%ndpt - 1)*obj%dt + 1.1) cycle  
            n = i + 1 
            exit  
          end do 
          if (n > 1 .and. t_out(1) > obj%dt) n = n + 1

          obj%nvmac     = n
          obj%tmac(1:n) = t_out(1:n) 
          obj%vmac(1:n) = v_int(1:n)

          call velio_close (obj%velio)

      else
          obj%nvmac = 1 
          obj%tmac(1) = 0.0 
          obj%vmac(1) = obj%vel_const
      endif


      return  
      end function fktmig_0



      integer function fktmig_xtokx (obj, fd ) 

      implicit none 
      type(fktmig_struct),intent(inout) :: obj                ! arguments
      integer, intent(inout)       :: fd(:)                  ! arguments

      real     :: swrki(obj%nb22)                            ! local
      complex  :: swrko(obj%nbnyq)                           ! local
      integer :: status                                      ! local
      integer :: i, j, k    , i2, ib    , off, nby  ! local
 
      fktmig_xtokx = 0

      do ib = 1, obj%nblks 
 
        !  read in time swath file(s) and pack in buffer obj%sect 
        i2 = min(obj%ntsw, obj%ntc - (ib - 1)*obj%ntsw) 
        !  initialize memory for t-swath 
        obj%sect = 0.0 
 
        !  read in a time swath
 
        nby = obj%nb22*obj%ntsw 
        off = 0                                   
        if (fd(ib) <= 0) return         

        status = cio_fseek( fd(ib), off, obj%origin)
        status = cio_fread(obj%sect,obj%byt_size, nby, fd(ib))

        if (status < 0) then 
          write(obj%print_lun, *) 'FKTMIG: ERROR IN CIO_FREAD-XTOKX' 
          fktmig_xtokx = 0 
          return  
        endif 

        !  loop over times. 1)get a row 2)fft 3)put the row back 
 
        do j = 1, i2                           
          swrki(obj%nb2+1) = 0.0 
          swrki(obj%nb2+2) = 0.0 
          !  fill the fft work buffer 
          swrki(:obj%nb2) = obj%sect(j,:obj%nb2) 

          !  fft from x to kx for time j of the swath 

           call fft_rc_transform(obj%fftrc_xobj, swrki, swrko)
 
          !  repack the row j 
          do i = 1, obj%nbnyq
             k = (i-1)*2 + 1
             obj%sect(j,k) = real(swrko(i))          
             obj%sect(j,k+1) = aimag(swrko(i))   
          end do 
        end do 

        !  write transformed time swath back to disk 
        if (obj%nblks == 1) cycle 

        status = cio_fseek( fd(ib), off, obj%origin)
        status = cio_fwrite(obj%sect,obj%byt_size, nby, fd(ib))

        if (status >= 0) cycle 
        write(obj%print_lun, *) 'FKTMIG: ERROR IN CIO_FWRITE' 
        fktmig_xtokx = 0 
        return  
      end do 
      fktmig_xtokx = 1 
      return  
      end function fktmig_xtokx 


      integer function fktmig_kxtox (obj, fd, fdout, fir, las) 
          
      implicit none 

      type(fktmig_struct),intent(inout) :: obj                 ! arguments
      integer,  intent(in)       :: fd(:)                      ! arguments
      integer,  intent(in)       :: fdout(:)                   ! arguments 
      integer , intent(in)       :: fir                        ! arguments
      integer , intent(in)       :: las                        ! arguments
 
      integer            :: i, j, k    , i2 ! local
      integer            ::  ib    , off, nby ! local
      complex            :: swrki(obj%nbnyq)                   ! local
      real               :: swrko(obj%nb22)                    ! local 
      integer            :: n, status                          ! local
      real               :: fft_scale                          ! local

   
      fktmig_kxtox = 0 
      fft_scale = 1./float(obj%nb2)

      do ib = 1, obj%nblks 

        !  read in time swath file(s) and fft the rows 

        if (obj%nblks>1 .or. obj%num_panels>1) then 
          nby = obj%nb22*obj%ntsw 
          off = 0
          if (obj%num_panels > 1) off = nby*obj%byt_size 
          if (fd(ib) <= 0) then 
            write(obj%print_lun, *) 'FKTMIG: ERROR IN CIO_FREAD-KXTOX, FD<0' 
            return  
          endif 

          status = cio_fseek( fd(ib), off, obj%origin)
          status = cio_fread(obj%sect,obj%byt_size, nby, fd(ib))

          if (status < 0) then 
            write(obj%print_lun, *) 'FKTMIG: ERROR IN CIO_FREAD-KXTOX' 
            return  
          endif 
        endif 

        !  loop over time points in the swath 
        i2 = min(obj%ntsw, obj%ntc - (ib - 1)*obj%ntsw) 

        do i = 1, i2 

          do j = 1, obj%nbnyq
             k = (j-1)*2 + 1 
             swrki(j) = cmplx(obj%sect(i,k),obj%sect(i,k+1) )
          end do 

          !  fft from kx to x for time j 
          call fft_cr_transform(obj%fftcr_xobj, swrki, swrko, fft_scale)

          !  place the j'TH TIME ROW BACK INTO SECT

          do j = 1,obj%nb22                 
             obj%sect(i,j) = swrko(j)     
          end do                           

        end do 
 
        !  write traces back to disk file(s) 
        !  in the same order we want them output 
        !   keep only 1st to last active bin

         off = 0.0
         nby = obj%ntsw* (abs(fir - las) + 1)
         status = cio_fseek(fdout(ib), off, obj%origin)
         n = cio_fwrite(obj%sect, obj%byt_size, nby, fdout(ib))
      end do 
 
      fktmig_kxtox = 1 
      return  
      end function fktmig_kxtox 
 

      integer function fktmig_migrate (obj, fdcur)  

      implicit none 

      type(fktmig_struct),intent(inout) :: obj             ! arguments
      integer  :: fdcur(:)                                ! arguments

      complex , allocatable       :: p(:)                 ! local 
      real , allocatable          :: work(:,:)            ! local
  
      integer :: add      , off, nby, i1, nrd 
      integer :: nwr    , col, ir, i, j, k   , n  
      integer :: ibh, k1, k2, j0, gulp, status 
      real :: pi, dw, dqx         , qx               , perc  
  
      !  load cbuf in subblocks--pad to obj%nt2-- do time fft 
      !   allocate temporary arrays
        
      allocate  (p(obj%nt2))
      allocate  (work(obj%ntsw,2))         
                    
      fktmig_migrate = 0 
      pi = 3.1415927
      !  frequency grid spacing 
      dw = 2.0*pi/(obj%nt2*obj%dtc) 
      !  qx wavenumber grid spacing                      
      dqx = 2.0*pi/(obj%nb2*obj%dist_fast*obj%fast_inc)
                      
      if (obj%nblks==1 .and. obj%ntsw<obj%ntc) then 
         write(obj%print_lun, *) 'FKTMIG_migrate: ARRAY DIMENSIONS ARE BAD' 
        return  
      endif 

       !  2*obj%nblks*gulp <= obj%nb22
      gulp = obj%nb22/(2*obj%nblks)                       
      gulp = min(gulp,obj%nbnyq) 
      if (obj%nblks == 1) gulp = obj%nbnyq 

!     write(obj%print_lun,*) 'GULP=',gulp 
 
      !  wavenumber loop
      nrd = 0
      do ibh = 1, obj%nbnyq, gulp                     
      
       !  get the time trace for wavenumber ibh 
        k1 = ibh 
        k2 = obj%nbnyq 
        if (obj%nblks > 1) then 
          k1 = ibh 
          k2 = k1 + gulp - 1 
          k2 = min(k2,obj%nbnyq) 
          do n = 1, obj%nblks 
            nby = obj%ntsw 
            off = 2*(ibh - 1)*nby*obj%byt_size                 
            nrd = 2*nby*(k2 - k1 + 1) 

            status = cio_fseek(fdcur(n), off, obj%origin)
            status = cio_fread(obj%sect(1:,2*(n-1)*gulp+1:), &
                         obj%byt_size, nrd, fdcur(n))

            if (status < 0) then  
               write(obj%print_lun, *) 'FKTMIG: ERROR IN CIO_FREAD' 
               return  
            end if
          end do 
        endif 

        do k = k1, k2 
          col = 2*k - 1 
          qx = (k - 1)*dqx 
          p(:obj%nt2) = 0. 
 
          ir=1
          do j = 1,obj%nblks
            i1 = (j-1)*obj%ntsw + 1
            j0 = 2*(j-1)*gulp + 2*(k-k1)+1
            do i = 1, min(obj%ntsw,obj%ntc-i1+1)
              p(ir) = cmplx( obj%sect(i,j0), obj%sect(i,j0+1) )
              ir = ir+1
            enddo
          enddo
 
         !  migrate the complex trace p and place the result back to p 
          perc = obj%mult_init + (obj%velpan - 1)*obj%mult_inc 
          add = 0 
          if (obj%ree(1:1) == 'Y') add = 1 
 
         i = fktmig_kernel(obj, perc,obj%nvmac,obj%tmac,obj%vmac,p,qx,add)
 
          p(obj%ntc+1:obj%nt2) = 0.0

          if (obj%num_panels>1 .and. obj%nblks==1) then 
          ir=1
          !   reorder complex vector
          do j=1, obj%nblks  
            do i=1, obj%ntsw
             work(i,1) = real(p(ir))
             work(i,2) = aimag(p(ir))
             ir=ir+1
            enddo
          enddo

            nby = obj%ntsw 
            off = (obj%nb22 + 2*(k - 1))*nby*obj%byt_size 
            nwr = 2*nby
 
            status = cio_fseek(fdcur(1), off, obj%origin)
            status = cio_fwrite(work,obj%byt_size, nwr, fdcur(1))

            if (status < 0) then 
              write(obj%print_lun, *) 'FKTMIG: ERROR IN CIO_FWRITE' 
              return  
            endif 
          else 

            ir = 1
            do j = 1, obj%nblks
              i1 = (j-1)*obj%ntsw + 1
              j0 = 2*(j-1)*gulp + 2*(k-k1)+1
              do i=1,min(obj%ntsw, obj%ntc-i1+1)
                obj%sect(i,j0) = real(p(ir))
                obj%sect(i,j0+1) = aimag(p(ir))
                ir = ir+1
              enddo
            enddo
          endif 
        end do 
 
!   problem if data is in mem and v-analysis 
!   save the pseudo-z trace for wavenumber ibh here

        if (obj%nblks <= 1) cycle  
        ir = 1 
        do n = 1, obj%nblks 
          nby = obj%ntsw 
          off = 2*(ibh - 1)*nby  
          if (obj%num_panels > 1) off = (obj%nb22 + 2*(ibh - 1))*nby 
          off = off * obj%byt_size
          nwr = 2*nby*(k2 - k1 + 1)

          status = cio_fseek(fdcur(n), off, obj%origin)
          status = cio_fwrite(obj%sect(1:,2*(n-1)*gulp+1:), &
                   obj%byt_size, nwr, fdcur(n))
 
          if (status < 0) then  
            write(obj%print_lun, *) 'FKTMIG: ERROR IN CIO_FWRITE' 
            return 
          end if 
        end do 
      end do
 
      fktmig_migrate = 1 

      deallocate  (p)
      deallocate  (work)
 
      return  
      end function fktmig_migrate 

      integer function fktmig_kernel (obj, perc, nL, tc, vc, p, qx, add)  

!   p - the complex input trace at wavenumber qx. 
!   rmdr,ind,fac dimensioned obj%nt2/2 
!   cbuf,pkz dimensioned obj%nt2 complex 
!   obj%fftrc_tobj,obj%fftcr_tobj - 5*obj%nt2 
!   fshift - obj%nt2/2 complex 
!   obj%cdelta - 21*10 complex elements  

      implicit none 
      type(fktmig_struct),intent(inout) :: obj                ! arguments

      integer , intent(in) :: nL                             ! arguments
      integer , intent(in) :: add                            ! arguments
      real , intent(in):: perc                               ! arguments
      real , intent(in) :: qx                                ! arguments
      real, intent(in)  :: tc(:)                             ! arguments 
      real, intent(in)  :: vc(:)                             ! arguments
      complex , intent(inout) :: p(:)                        ! arguments 

      integer, allocatable     :: ind(:)                     ! local
      real , allocatable       :: rmdr(:)                    ! local 
      real , allocatable       :: fac(:)                     ! local
      complex, allocatable     :: cbuf(:)                    ! local
      complex, allocatable     :: pkz(:)                     ! local
      complex, allocatable     :: fshift(:)                  ! local
      complex, allocatable     :: cbuf1(:)                   ! local
      complex, allocatable     :: pkz1(:)                    ! local

      integer     :: nth, nt2_2      ! local
      integer     :: jfout, jfinp, jtrig                     ! local
      integer     :: i, j, k, l, iv  ! local
      integer     :: jtstart, jtstop, jstartp, jstartn       ! local      
      integer     :: is0, idx, ltest, ny, Li                 ! local
      integer     :: nt1     , nbuf ! local
      real        :: w2, w2i, w1w2i, rind, facc, hv, hvsq    ! local
      real        :: dwi, omega, omega1, omega2              ! local
      real        :: pi, dw, qt, qxsq, vv, arg, wnyq         ! local
      real        :: fft_scale                               ! local

! 
      ! allocate temporary arrays
      nth = obj%nt2/2 + 1
      nt2_2 =  obj%nt2 * 2 
      nbuf = max( nt2_2, obj%nb22)
 
      allocate    (ind(nth))                    
      allocate    (rmdr(nth))          
      allocate    (fac(nth))         
      allocate    (cbuf(nbuf))                     
      allocate    (pkz(nt2_2))                      
      allocate    (fshift(obj%nt2)) 

      allocate    (cbuf1(nbuf))                     
      allocate    (pkz1(nt2_2)) 
! 
!    loop over the layers. conventional f-k migration has 
!    only 1 layer so nl should=1, and the velocity will be 
!    vc(1). 

      fktmig_kernel = 0 
      pi = 3.1415927 
      dw = 2.0*pi/(obj%nt2*obj%dtc)           !  frequency grid spacing 
      wnyq = pi/obj%dtc                       !  nyquist frequency 
 
      jtstop = 0 
      do iv = 1, max(1,nl - 1) 
        if (nL == 1) then 
          vv = perc*vc(1) 
          jtstart = 1 
          jtstop = obj%ntc 
        else 
          call fktmig_cascade_v (iv, nL, tc, vc, perc, obj%ntc,     & 
            obj%dtc, vv, jtstart, jtstop) 
        endif 
        if (vv<=0. .or. jtstart>obj%ntc) cycle  
! 
!   if this is the first pass of the migration, the 
!   data from jt = 1 to obj%ntc is put into cbuf from 1 to obj%ntc 
!   if this is the second or later stage of a cascaded migration 
!   we start at time jt = jtstart and transfer jtstart to obj%ntc 
!   into cbuf(1...obj%ntc- jtstart + 1) 
!   note we divide by obj%nt2 to normalize for the t - w and kz - t 
!     fft'S 

       fft_scale = 1./ obj%nt2
 
       cbuf1(:obj%ntc-jtstart+1) = p(jtstart:obj%ntc)*fft_scale 

        p(jtstart:obj%ntc) = 0.0 

        nt1 = obj%ntc-jtstart+2
        cbuf1( nt1:obj%nt2 ) = 0.0
 
        !  transform the data from time to frequency. p(kx,t) -> p(kx,w) 
 
         call fft_cc_transform(obj%fftcc_neg_obj, cbuf1, cbuf)
 
        !   compute frequency stretch and migration operator 

        hv = 0.5*vv 
        hvsq = hv*hv 
        qxsq = qx*qx 
        arg = wnyq*wnyq - qx*qx*hvsq 
        if (arg > 0.) then 
          jfout = sqrt(arg)/dw + 1               !compute max allowed qt 
        else 
          jfout = 0 
        endif 
        jfout = min(jfout,obj%nt2/2) 
        jfinp = hv*qx/dw + 1                     !min allowed freq. in input 
        jfinp = min(jfinp,obj%nt2/2 + 1) 
        dwi = 1.0/dw 
        jtrig = max(1,jfout - 20) 
        w2 = 2.0 - obj%wfac 
        w2i = 1.0/w2 
        w1w2i = (1.0 - obj%wfac)/(2.0 - obj%wfac) 
        do j = 2, jfout 
          qt = (j - 1)*dw 
          omega1 = qt*w1w2i 
          omega2 = sqrt(qt*qt + w2*hvsq*qxsq) 
          omega = omega1 + w2i*omega2 
          rind = omega*dwi + 1.0005              !  point on fine grid 
          ind(j) = rind 
          rmdr(j) = rind - ind(j) 
          fac(j) = (1.0 - 0.05*max(0,j - jtrig))*(qt/(w2*omega2))**obj%dap 
          !   include obj%origin shift and amplitude factor in fshift 
          fshift(j) = cexp(cmplx(0.,(-obj%tstrt*(omega - qt))))*fac(j)
        end do 
 
!     rezero column of section,perform shift 
!     interpolate from cbuf to pkz to do the migration 
! 
         pkz(1:2*obj%nt2) = 0.0
! 
!    convolutional sums to do sinc interpolation for output frequencies 
        do l = 1, 10 
! 
!   find loop limits for positive freq. so is0 stays in range 1 to ny 

          ny = obj%nt2/2 + 1 
          jstartp = 2 
   27     continue 
          ltest = ind(jstartp) - 5 + l 
          if (ltest < 2) then 
            jstartp = jstartp + 1 
            if (jstartp < ny) go to 27 
            write(obj%print_lun, *) 'ERROR IN JSTARTP LOGIC - ABORT'
            return 
          endif 

!   find loop limits for neg. freq. so is0 stays in range 1 to ny 
          jstartn = 2 
   29     continue
 
          ltest = obj%nt2 - ind(jstartn) - 4 + l 
          if (ltest > obj%nt2) then 
            jstartn = jstartn + 1 
            if (jstartn < ny) go to 29 
            write(obj%print_lun, *) 'ERROR IN JSTARTN LOGIC - ABORT' 
            return 
          endif 
 
          idx = l - 5 
          do j = jstartp, jfout         !  interpolate the positive freq 
            is0 = ind(j) + idx 
            k = nint(rmdr(j)/0.05)*10 
            pkz(j) = pkz(j) + obj%cdelta(k+l)*cbuf(is0) 
          end do 

          idx = obj%nt2 + l - 4 
          do j = jstartn, jfout          !  interpolate the negative freq 
            is0 = idx - ind(j) 
            k = nint((1.0 - rmdr(j))/0.05)*10 
            pkz(obj%nt2-j+2) = pkz(obj%nt2-j+2) + obj%cdelta(k+l)*cbuf(is0) 
          end do 
        end do 
 
!   frequency is shifted - now perform scaling by fac and if tstrt is 
!   nonzero do phase shifting for the time origin 
 
        !   positive frequencies 
        pkz(2:jfout) = pkz(2:jfout)*fshift(2:jfout) 
! 
        do i = 2, jfout     !   negative frequencies 
          pkz(obj%nt2-i+2) = pkz(obj%nt2-i+2)*conjg(fshift(i)) 
        end do 
 
        facc = fac(min(jfinp,jfout)) 
        facc = min(1.0,facc) 
        facc = max(0.2,facc) 
 
!   for ree option add back the original spectrum from the evanescent 
!   region of the f-k plane 
 
        if (add == 1) then 
          do i = 2,jfinp
             pkz(i) = pkz(i) + cbuf(i)*facc 
          end do
          do i = 2,jfinp 
            Li = obj%nt2 + 2 - (i-1) - 1 
            pkz(obj%nt2-i+2) = pkz(obj%nt2-i+2) + cbuf(Li)*facc              
          end do 
        endif 
 
!  transform the migrated data from kz to z 
 
        call fft_cc_transform(obj%fftcc_pos_obj, pkz, pkz1) 
         
!  transfer the migrated data from pkz back to p 
!  note we transfer from jtstart to nt 
 
        p(jtstart:obj%ntc) = pkz1(:obj%ntc-jtstart+1) 
 
      end do 
      fktmig_kernel = 1 

      deallocate    (ind)                    
      deallocate    (rmdr)          
      deallocate    (fac)         
      deallocate    (cbuf)                     
      deallocate    (pkz)                      
      deallocate    (fshift)
      deallocate    (cbuf1)                     
      deallocate    (pkz1)

!     end cascaded migration loop 
      return  
      end function fktmig_kernel 
 


      subroutine fktmig_cascade_v(ith, n, t, v, f, n1, d1, vo, jstrt, jstop) 
 
      implicit none 

      integer , intent(in) :: ith                              ! arguments
      integer , intent(in) :: n                                ! arguments
      integer , intent(in) :: n1                               ! arguments
      integer , intent(out) :: jstrt                           ! arguments
      integer , intent(out) :: jstop                           ! arguments
      real , intent(in) :: f                                   ! arguments
      real , intent(in) :: d1                                  ! arguments
      real , intent(out) :: vo                                 ! arguments
      real , intent(in) :: t(:)                                ! arguments
      real , intent(in) :: v(:)                                ! arguments
!
      integer :: i, j                                          ! local 
      real :: vsqsum, vsc                                      ! local
                                    
! given the dix velocity function defined by t(n),v(n), n>0 
! return the velocity vo for the ith layer of a cascaded migration. 
! f is a multiplicative scale factor to apply to the input velocity. 
! n1,d1 define the grid for the migration calculation. 
! jstrt,jstop are the grid interval where vo applies 
! note: by convention v(i+1) is from t(i) to t(i+1) 

      vo = 0.0 
      jstrt = 1 
      jstop = n1 
      if (n <= 0) return  
      if (n == 1) then 
        vo = v(1) 
        return  
      endif 
      jstop = 0 
      vsqsum = 0.0 
      if (t(1)>d1 .and. n>1) then 
        if (v(1) /= v(2)) then 
          do i = 1, max(1,n - 1) 
            j = i 
            jstrt = jstop + 1                  !  head to tail intervals 
            if (i == 1) then 
              jstop = jstrt + t(j)/d1 
            else 
              jstop = jstrt + (t(j)-t(j-1))/d1 
            endif 
            jstop = min(jstop,n1) 
            vsc = f*v(j) 
            vo = max(0.,vsc**2 - vsqsum) 
            if (vo > 0.) then 
              vo = sqrt(vo) 
            else 
              return  
            endif 
            vsqsum = vsqsum + vo*vo 
            if (i /= ith) cycle  
            return  
          end do 
        else 
          do i = 1, max(1,n - 1) 
            j = i + 1 
            jstrt = jstop + 1                  !  head to tail intervals 
            if (i == 1) then 
              jstop = jstrt + t(j)/d1 
            else 
              jstop = jstrt + (t(j)-t(j-1))/d1 
            endif 
            jstop = min(jstop,n1) 
            vsc = f*v(j) 
            vo = max(0.,vsc**2 - vsqsum) 
            if (vo > 0.) then 
              vo = sqrt(vo) 
            else 
              return  
            endif 
            vsqsum = vsqsum + vo*vo 
            if (i /= ith) cycle  
            return  
          end do 
        endif 
      else 
        do i = 1, max(1,n - 1) 
          j = i + 1 
          jstrt = jstop + 1                    !  head to tail intervals 
          if (i == 1) then 
            jstop = jstrt + t(j)/d1 
          else 
            jstop = jstrt + (t(j)-t(j-1))/d1 
          endif 
          jstop = min(jstop,n1) 
          vsc = f*v(j) 
          vo = max(0.,vsc**2 - vsqsum) 
          if (vo > 0.) then 
            vo = sqrt(vo) 
          else 
            return  
          endif 
          vsqsum = vsqsum + vo*vo 
          if (i /= ith) cycle  
          return  
        end do 
      endif 
      return  
      end subroutine fktmig_cascade_v 


!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine fktmig_wrapup (obj)
      implicit none
      type(fktmig_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

  if (associated(obj%stretch_for)) call stretch_wrapup (obj%stretch_for)
  if (associated(obj%stretch_inv)) call stretch_wrapup (obj%stretch_inv)

      return
      end subroutine fktmig_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module fktmig_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
