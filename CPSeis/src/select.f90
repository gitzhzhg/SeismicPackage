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
! Name       : SELECT
! Category   : headers
! Written    : 1988-07-08   by: Tom Stoeckley
! Revised    : 2006-10-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Select traces to kill, delete, etc. by header word criteria.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! SELECT is an extremely flexible process that can save, delete, kill, flag or
! reverse the polarity of selected traces.  Traces are selected through a
! process that uses the set theory concepts of complement, intersection and
! union.
!
!
! Conditional Process Execution
!
! SELECT can be used to produce conditional execution of processes within a
! job by two methods.  (This is currently the only means of producing
! conditional process execution within CPS.)
!
! 1. Traces can be directly deleted from the flow by SELECT.
!
! 2. Traces may be flagged and a subsequent process or processes can be made to
! operate on flagged traces only.  Different instances of SELECT within a job
! can establish independent trace flags by using different header words.
!
!
! Trace Selection Logic
!
! Traces are selected for one of the five ACTIONS (SAVE, DELETE, FLAG, KILL or
! REVERSE POLARITY) in a two-step process.  In the first step traces are
! selected by masks, each of which specifies a simple selection procedure.  Up
! to four individual masks are allowed; they are labeled by letter as A, B, C
! and D.
!
! In the second step the final trace selection is made by combining the
! individual selections made by the masks.  Those traces finally selected will
! have been selected either by every mask used or by at least one of the masks
! used.  The specified ACTION will be performed on those traces finally
! selected.
!
!
! Masks
!
! In the first step each mask uses one of four selection METHODS (DO_SKIP,
! PATTERN, RANGES or VALUES) to define selection rules (unless DISABLED).
! (The DO_SKIP method is
! based on sequential trace number; the others are based on the value of the
! user-specified header word HDR_SEL.)  Masks have the additional flexibility
! of allowing rules that define traces selected or traces not selected.  If
! COMPLEMENT = NO, then the mask rules define those traces that are selected,
! if COMPLEMENT = YES, then mask rules define those traces that are NOT
! selected.
!
!
! Final Trace Selection
!
! Traces will be finally selected by the process if they are selected by
! every mask (COMBINE = MEETS_ALL) or if they are selected by at least one
! mask (COMBINE = MEETS_ANY).  The specified ACTION will be performed on those
! traces finally selected.  Traces not finally selected by the process will
! pass through unchanged.
!
!
! Note on Set Theory Concepts
!
! SELECT uses the set theory concepts of complement, intersection and union.
! The COMBINE = MEETS_ALL final selection is the INTERSECTION of the mask
! selections; the COMBINE = MEETS_ANY final selection is the UNION of the mask
! selections.  Switching from a rule defining traces selected to a rule
! defining traces not selected is taking the COMPLEMENT of the mask rule.
!
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
! If only one mask is specified (not DISABLED), COMBINE has no effect and
! traces are selected according to the mask.
!
! Consecutive SELECT Processes
!
! Two or more consecutive SELECT processes in the same job, with the same action
! chosen, function as though each process is a single (possibly complicated)
! mask with traces being selected overall that are selected by EVERY individual
! SELECT process.  They can also be thought of as not selecting traces overall
! that are not selected by AT LEAST one individual SELECT process.
!
!
! Tolerance in VALUES Mask
!
! A tolerance is provided when METHOD = VALUES.  Values of header word HDR_SEL
! need not exactly equal a value in ENTRIES to be selected.  If it is within
! 1.0E-6 times the ENTRIES value it will be selected.
!
!
! EXAMPLES:
!
!
! 1.  METHOD_A = RANGES, HDR_SEL_A = 14, COMPLEMENT_A = NO, ENTRIES_A = (125,
!     135, 247, 248, 180, 180)
!       means that all traces whose header word 14 contains a value within the
!       ranges of 125-135, 247-248, or 180-180 will be selected by mask "A".
!       (Up to 100 pairs of number are allowed in ENTRIES.)
!
!     METHOD_A = RANGES, HDR_SEL_A = 14, COMPLEMENT_A = YES, ENTRIES_A = (125,
!     135, 247, 248, 180, 180)
!       means that all traces whose header word 14 does NOT contain a value
!       within the ranges of 125-135, 247-248, or 180-180 will be selected by
!       mask "A".  (Up to 100 pairs of number are allowed in ENTRIES.)
!
! 2.  METHOD_B = VALUES, HDR_SEL_B = 11, COMPLEMENT_B = NO, ENTRIES_B = (0)
!       means that all traces whose header word 11 value is zero will be
!       selected by mask "B".  (Up to 200 values are allowed in ENTRIES.)
!
!     METHOD_B = VALUES, HDR_SEL_B = 11, COMPLEMENT_B = YES, ENTRIES_B = (0)
!       means that all traces whose header word 11 value is NOT zero will be
!       selected by mask "B".  (Up to 200 values are allowed in ENTRIES.)
!
! 3.  METHOD_C = PATTERN, HDR_SEL_C = 4, COMPLEMENT_C = NO, ENTRIES_C = (27.7,
!     2.4, 10.0, 127.7, 11)
!       means that all traces whose header word 4 contains a value within the
!       ranges 26.5-28.9, 36.5-38.9, 46.5-48.9,... will be selected by mask "C".
!
!     METHOD_C = PATTERN, HDR_SEL_C = 4, COMPLEMENT_C = YES, ENTRIES_C = (27.7,
!     2.4, 10.0, 127.7, 11)
!       means that all traces whose header word 4 contains a value OUTSIDE the
!       ranges 26.5-28.9, 36.5-38.9, 46.5-48.9,... will be selected by mask "C".
!
! 4.  METHOD_D = DO_SKIP, HDR_SEL_D = 4, COMPLEMENT_D = NO, ENTRIES_D = (12,
!     24, 41, 999)
!       means that all traces with sequential trace number within the ranges
!       13-36, 78-101, 143-166,... will be selected by mask "D".  (HDR_SEL is
!       unused when METHOD = DO_SKIP.)
!
!     METHOD_D = DO_SKIP, HDR_SEL_D = 4, COMPLEMENT_D = YES, ENTRIES_D = (12,
!     24, 41, 999)
!       means that all traces with sequential trace number OUTSIDE the ranges
!       13-36, 78-101, 143-166,... will be selected by mask "D".  (HDR_SEL is
!       unused when METHOD = DO_SKIP.)
!
! 5.  A trace will be Saved, Deleted, Flagged, Killed or Polarity Reversed if:
!       ALL active masks select the trace and COMBINE = MEETS_ALL, or
!       ANY active mask selects the trace and COMBINE = MEETS_ANY.
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.

! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process may alter input traces.
! This process outputs the same traces as it receives (possibly altered).
! This process outputs traces with same gather status as the input traces.
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
!
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 1       sequential trace number    reset as needed
! *       HDR_FLAG                   may be set
! *       any header word            may be used in mask
! 2       head mute                  may be set
! 64      tail mute                  may be set
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
! 55. 2006-10-31 Stoeckley  Add calls to pc_register_array_names for SeisSpace.
!054. 2006-01-10  B. Menger   Removed Unused Variables.
! 53. 2002-07-29 Stoeckley  Change PATTERN method to use the MTH module.
! 52. 2002-04-15 Baumel     Fixed bug in input trace count for DO_SKIP when
!                           input traces arrive in groups.
! 51. 2001-12-20 Selzler    Changed selection logic to use double precision.
! 50. 2001-04-02 Selzler    Added error message if bin width >= increment
!                           for the pattern method.
! 49. 2001-01-02 Selzler    Improved ergonomics for RANGE ENTRIES.
! 48. 2000-12-11 Selzler    Changed wrapup logic to use skip_wrapup
! 47. 2000-11-14 Selzler    Fixed pattern prevent updates of increment
! 46. 2000-11-01 Selzler    Fixed pattern avoid updates of increment
! 45. 2000-10-05 Selzler    Fixed gui def for new ezgui conventions.
! 44. 2000-09-27 Selzler    Corrected bug in select_mask and DO_SKIP
! 43. 2000-08-16 Selzler    Fixed problems with values near zero
! 42. 2000-07-07 Selzler    Fixed problems found by CPS Fortran Code Review.
! 41. 2000-05-11 Selzler    Corrected header mute when killing traces
! 40. 2000-04-25 Selzler    Corrected bug in delete logic.
! 39. 2000-04-20 Selzler    Corrected bug in select logic.
! 38. 2000-04-17 Selzler    Changed EVERY and AT_LEAST to MEETS_ALL/ANY.
! 37. 2000-03-30 Selzler    Added SAVE to list of supported actions
! 36. 2000-03-27 Selzler    Improved comments displayed in GUI
! 35. 2000-03-13 Selzler    Corrected bugs in complement logic.
! 34. 2000-03-10 Selzler    Corrected bugs in GUI interface
! 33. 2000-02-24 Selzler    Improved GUI interface
! 32. 2000-02-21 Selzler    Corrected bug in PATTERN increment validation
! 31. 2000-02-18 Selzler    Improved messages for GUI
! 30. 2000-02-16 Selzler    Enhanced GUI with ENTRIES comments
! 29. 2000-02-11 Selzler    Updated documentation for _B, _C, _D
! 28. 2000-02-10 Selzler    Corrected bug in wrapup logic
! 27. 2000-02-09 Selzler    synchronized source with CIB's latest newdoc.
! 26. 2000-02-08 Selzler    improved gui support
! 25. 2000-02-07 Selzler    improved gui support
! 24. 2000-02-04 Selzler    Corrected bug in combo box option count
! 23. 2000-02-02 Selzler    Added support for GUI and general cleanup
! 22. 2000-01-19 Selzler    Corrected various bugs
! 21. 1999-12-16 Selzler    Debugged do-skip loop processing
! 20. 1999-11-23 Selzler    Incorporated pattern_verify into pattern primitive
! 19. 1999-11-19 Selzler    Added RCS "Id" strings to tag executeable
! 18. 1999-09-13 Selzler    Updated skip_wrapup and print_lun usage
! 17. 1999-08-23 Selzler    change headers to double precision
! 16. 1999-08-05 Selzler    Conversion to fortran 90 compiler.
!                           Fix bugs when killing traces and setting hd(64)
! 15. 1998-11-10 Goodger    Begin using the f90 compiler.
! 14. 1992-04-02 Troutt     Set HW64=NDPT+1 for killed traces.
! 13. 1992-01-22 Troutt     Fix to update header word #1 on output.
! 12. 1990-11-08 Stoeckley  Fix bug which occurs when selecting by an
!                           overlapping pattern.
! 11. 1990-06-22 Stoeckley  Allow pairs of values to be in either order.
! 10. 1989-10-25 Troutt     Set mute index (HW2) to NDPT+1 for traces
!                           killed.
! 9.  1989-08-02 Lorimor    Revised SLOP value to 1.0E-06.
! 8.  1989-07-18 Lorimor    Added SLOP variable when selecting by VALUE.
! 7.  1989-05-30 Lorimor    Increase arrays to allow more select values.
! 6.  1989-05-19 Howard     Fix bug in KILL and REVPOL.
! 5.  1989-05-10 Stoeckley  Change ACTION parameters to improve clarity,
!                           remove calls to CLEAR,MOVHAT,REVPOL, redo
!                           screen layout, and add use of ISAVE and KSAVE.
! 4.  1989-04-17 Adams      Add Forward,Reverse, to Offset selection.
! 3.  1989-03-22 Lorimor    Add REVPOL option and DOSKIP option.
!                           Redo screen layout.
! 2.  1989-02-22 Lorimor    Change parameter HFN to NHF, changed BEGINNING
!                           of bin to CENTER of bin in PATTERN card.
! 1.  1988-10-24 Lorimor    NWIH and LTR conversion.
!                           Add KILL and FLAG options.
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
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NEED_TRACES    if this process needs more traces
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
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
! The 'A', 'B', 'C', 'D' notation for the front-end gui corresponds to
! subscripts 1, 2, 3, 4 in process variables.
! For example, 'METHOD_A' and 'METHOD_B' ... 'METHOD_D' correspond to
! obj%method(1), obj%method(2), ... obj%method(4).
!
! The functionality has changed slightly from the original select process.
! The simplifications were mutually agreed to by Tom Stoeckley, Chuck I.
! Burch and Randy Selzler on 29 July 1999.
! The following summarizes the changes:
! 1) 'COMPLEMENT' and 'COMBINE' parameters replaced a more complicated
!   'MODE' and 'SAVE/DELETE/ALSOSAVE/MAYBEDELETE/KILL/MAYBEKILL/...'.
! 2) Special functionality associated with header word 6 ('6F' and '6R')
!    was eliminated.
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS SELECT Process/NC=80>
!
!          Select traces to kill, delete, etc.
!
!      ACTION=`CCCCC          HDR_FLAG=`II
!
!   `--------------------------   `--------------------------
!    METHOD_A=`CCCCCC              METHOD_B=`CCCCCC
!    HDR_SEL_A=`II                 HDR_SEL_B=`II
!    ENTRIES_A   COMMENT_A         ENTRIES_B   COMMENT_B
!    `FFFFFFFFFFF`SSSSSSSSS        `FFFFFFFFFFF`SSSSSSSSS
!    `FFFFFFFFFFF`SSSSSSSSS        `FFFFFFFFFFF`SSSSSSSSS
!    `FFFFFFFFFFF`SSSSSSSSS        `FFFFFFFFFFF`SSSSSSSSS
!    `FFFFFFFFFFF`SSSSSSSSS        `FFFFFFFFFFF`SSSSSSSSS
!    COMPLEMENT_A=`CC              COMPLEMENT_B=`CC
!   `--------------------------   `--------------------------
!
!
!   `--------------------------   `--------------------------
!    METHOD_C=`CCCCCC              METHOD_D=`CCCCCC
!    HDR_SEL_C=`II                 HDR_SEL_D=`II
!    ENTRIES_C   COMMENT_C         ENTRIES_D   COMMENT_D
!    `FFFFFFFFFFF`SSSSSSSSS        `FFFFFFFFFFF`SSSSSSSSS
!    `FFFFFFFFFFF`SSSSSSSSS        `FFFFFFFFFFF`SSSSSSSSS
!    `FFFFFFFFFFF`SSSSSSSSS        `FFFFFFFFFFF`SSSSSSSSS
!    `FFFFFFFFFFF`SSSSSSSSS        `FFFFFFFFFFF`SSSSSSSSS
!    COMPLEMENT_C=`CC              COMPLEMENT_D=`CC
!   `--------------------------   `--------------------------
!
!     COMBINE=`CCCCCCC
!<PARMS ENTRIES_A_ARRAYSET[/XST/YST]>
!<PARMS ENTRIES_B_ARRAYSET[/XST/YST]>
!<PARMS ENTRIES_C_ARRAYSET[/XST/YST]>
!<PARMS ENTRIES_D_ARRAYSET[/XST/YST]>
!<PARMS COMMENT_A[/ML=20/XST]
!<PARMS COMMENT_B[/ML=20/XST]
!<PARMS COMMENT_C[/ML=20/XST]
!<PARMS COMMENT_D[/ML=20/XST]
!</gui_def>
!<HelpSection>
!
!<Help KEYWORD="ACTION">
!<Tip> Action to take on selected traces. </Tip>
! Default = SAVE
! Allowed = SAVE    (Selected traces are saved.)
! Allowed = DELETE  (Selected traces are deleted.)
! Allowed = FLAG    (Selected traces are flagged.)
! Allowed = KILL    (Selected traces are killed.)
! Allowed = RP      (Selected traces are polarity reversed.)
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word to use for flagged traces. </Tip>
! Default = 48
! Allowed = 1 - NWIH
! SELECT will insert a flag in header word HDR_FLAG that can be recognized by
! subsequent processes.  This allows conditional process execution without
! deleting traces from the trace flow.  Operates only when ACTION = FLAG.
!
!</Help>
!
!<Help KEYWORD="METHOD_A">
!<Tip> Method for mask "A" to use in selecting traces.</Tip>
! Default = RANGES
! Allowed = DISABLED (This mask is not used)
! Allowed = RANGES  (Select traces by ranges of header value.)
! Allowed = VALUES  (Select traces by specific values.)
! Allowed = PATTERN (Select traces by bin pattern.)
! Allowed = DO_SKIP (Select traces by do - skip method.)
!</Help>
!
!<Help KEYWORD="HDR_SEL_A">
!<Tip> Header word for mask "A" to use in trace selection. </Tip>
! Default = 1
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="ENTRIES_A">
!<Tip> Array of values defining selection rules for mask "A". </Tip>
! Default =  -
! Allowed = double precision array
!
! If METHOD = RANGES, the ENTRIES array consists of a sequence of start and end
! values defining ranges. Traces will be selected if the values of header word
! HDR_SEL fall within any specified range.  Range start and end values are
! inclusive.  Up to 100 ranges are permitted.
!
! If METHOD = VALUES, the ENTRIES array consists of a sequence of values.
! Traces will be selected if the values of header word HDR_SEL equal the values
! in the ENTRIES array.  (A tolerance is allowed of 1.0E-6 times the value in
! the ENTRIES array.)  Up to 200 values are permitted.
!
! If METHOD = PATTERN, the ENTRIES array consists of 5 values that specify a
! bin pattern.  Traces will be selected if the values of header word HDR_SEL
! fall within any bin.  Values in the ENTRIES array are:  center of first bin,
! width of first bin, increment between bins, center of last bin and total
! number of bins.
!
! If METHOD = DO_SKIP, the ENTRIES array consists of four values: number of
! traces to skip initially, number of traces to select at a time, number of
! traces to skip at a time, and total number of traces to select.  (HDR_SEL is
! unused when METHOD = DO_SKIP.)
!</Help>
!
!<Help KEYWORD="COMMENT_A" TYPE= "DISPLAY_ONLY">
!<Tip> Comment only.  Brief description of associated ENTRIES_A usage.</Tip>
! Comment only.  Brief description of associated ENTRIES_A usage.
!</Help>
!
!<Help KEYWORD="COMPLEMENT_A">
!<Tip> Select traces according to the complement of the METHOD chosen? </Tip>
! Default = NO
! Allowed = YES/NO
! If  COMPLEMENT = NO, then mask is criteria for traces being selected (the
! default and usual choice).
!
! If COMPLEMENT = YES, then mask is criteria for traces not being selected (that
! is, use the method specified to define traces NOT selected and select all
! others.)
!</Help>
!

!<Help KEYWORD="METHOD_B">
!<Tip> Method for mask "B" to use in selecting traces.</Tip>
! Default = DISABLED
! Allowed = DISABLED (This mask is not used)
! Allowed = RANGES  (Select traces by ranges of header value.)
! Allowed = VALUES  (Select traces by specific values.)
! Allowed = PATTERN (Select traces by bin pattern.)
! Allowed = DO_SKIP (Select traces by do - skip method.)
!</Help>
!
!<Help KEYWORD="HDR_SEL_B">
!<Tip> Header word for mask "B" to use in trace selection. </Tip>
! Default = 1
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="ENTRIES_B">
!<Tip> Array of values defining selection rules for mask "B". </Tip>
! Default =  -
! Allowed = double precision array
! If METHOD = RANGES, the ENTRIES array consists of a sequence of start and end
! values defining ranges. Traces will be selected if the values of header word
! HDR_SEL fall within any specified range.  Range start and end values are
! inclusive.  Up to 100 ranges are permitted.
!
! If METHOD = VALUES, the ENTRIES array consists of a sequence of values.
! Traces will be selected if the values of header word HDR_SEL equal the values
! in the ENTRIES array.  (A tolerance is allowed of 1.0E-6 times the value in
! the ENTRIES array.)  Up to 200 values are permitted.
!
! If METHOD = PATTERN, the ENTRIES array consists of 5 values that specify a
! bin pattern.  Traces will be selected if the values of header word HDR_SEL
! fall within any bin.  Values in the ENTRIES array are:  center of first bin,
! width of first bin, increment between bins, center of last bin and total
! number of bins.
!
! If METHOD = DO_SKIP, the ENTRIES array consists of four values: number of
! traces to skip initially, number of traces to select at a time, number of
! traces to skip at a time, and total number of traces to select.  (HDR_SEL is
! unused when METHOD = DO_SKIP.)
!</Help>
!
!<Help KEYWORD="COMMENT_B" TYPE= "DISPLAY_ONLY">
!<Tip> Comment only.  Brief description of associated ENTRIES_B usage.</Tip>
! Comment only.  Brief description of associated ENTRIES_B usage.
!</Help>
!
!<Help KEYWORD="COMPLEMENT_B">
!<Tip> Select traces according to the complement of the METHOD chosen? </Tip>
! Default = NO
! Allowed = YES/NO
! If  COMPLEMENT = NO, then mask is criteria for traces being selected (the
! default and usual choice).
!
! If COMPLEMENT = YES, then mask is criteria for traces not being selected (that
! is, use the method specified to define traces NOT selected and select all
! others.)
!</Help>

!<Help KEYWORD="METHOD_C">
!<Tip> Method for mask "C" to use in selecting traces.</Tip>
! Default = DISABLED
! Allowed = DISABLED (This mask is not used)
! Allowed = RANGES  (Select traces by ranges of header value.)
! Allowed = VALUES  (Select traces by specific values.)
! Allowed = PATTERN (Select traces by bin pattern.)
! Allowed = DO_SKIP (Select traces by do - skip method.)
!</Help>
!
!<Help KEYWORD="HDR_SEL_C">
!<Tip> Header word for mask "C" to use in trace selection. </Tip>
! Default = 1
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="ENTRIES_C">
!<Tip> Array of values defining selection rules for mask "C". </Tip>
! Default =  -
! Allowed = double precision array
! If METHOD = RANGES, the ENTRIES array consists of a sequence of start and end
! values defining ranges. Traces will be selected if the values of header word
! HDR_SEL fall within any specified range.  Range start and end values are
! inclusive.  Up to 100 ranges are permitted.
!
! If METHOD = VALUES, the ENTRIES array consists of a sequence of values.
! Traces will be selected if the values of header word HDR_SEL equal the values
! in the ENTRIES array.  (A tolerance is allowed of 1.0E-6 times the value in
! the ENTRIES array.)  Up to 200 values are permitted.
!
! If METHOD = PATTERN, the ENTRIES array consists of 5 values that specify a
! bin pattern.  Traces will be selected if the values of header word HDR_SEL
! fall within any bin.  Values in the ENTRIES array are:  center of first bin,
! width of first bin, increment between bins, center of last bin and total
! number of bins.
!
! If METHOD = DO_SKIP, the ENTRIES array consists of four values: number of
! traces to skip initially, number of traces to select at a time, number of
! traces to skip at a time, and total number of traces to select.  (HDR_SEL is
! unused when METHOD = DO_SKIP.)
!</Help>
!
!<Help KEYWORD="COMMENT_C" TYPE= "DISPLAY_ONLY">
!<Tip> Comment only.  Brief description of associated ENTRIES_C usage.</Tip>
! Comment only.  Brief description of associated ENTRIES_C usage.
!</Help>
!
!<Help KEYWORD="COMPLEMENT_C">
!<Tip> Select traces according to the complement of the METHOD chosen? </Tip>
! Default = NO
! Allowed = YES/NO
! If  COMPLEMENT = NO, then mask is criteria for traces being selected (the
! default and usual choice).
!
! If COMPLEMENT = YES, then mask is criteria for traces not being selected (that
! is, use the method specified to define traces NOT selected and select all
! others.)
!</Help>

!<Help KEYWORD="METHOD_D">
!<Tip> Method for mask "D" to use in selecting traces.</Tip>
! Default = DISABLED
! Allowed = DISABLED (This mask is not used)
! Allowed = RANGES  (Select traces by ranges of header value.)
! Allowed = VALUES  (Select traces by specific values.)
! Allowed = PATTERN (Select traces by bin pattern.)
! Allowed = DO_SKIP (Select traces by do - skip method.)
!</Help>
!
!<Help KEYWORD="HDR_SEL_D">
!<Tip> Header word for mask "D" to use in trace selection. </Tip>
! Default = 1
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="ENTRIES_D">
!<Tip> Array of values defining selection rules for mask "D". </Tip>
! Default =  -
! Allowed = double precision array
! If METHOD = RANGES, the ENTRIES array consists of a sequence of start and end
! values defining ranges. Traces will be selected if the values of header word
! HDR_SEL fall within any specified range.  Range start and end values are
! inclusive.  Up to 100 ranges are permitted.
!
! If METHOD = VALUES, the ENTRIES array consists of a sequence of values.
! Traces will be selected if the values of header word HDR_SEL equal the values
! in the ENTRIES array.  (A tolerance is allowed of 1.0E-6 times the value in
! the ENTRIES array.)  Up to 200 values are permitted.
!
! If METHOD = PATTERN, the ENTRIES array consists of 5 values that specify a
! bin pattern.  Traces will be selected if the values of header word HDR_SEL
! fall within any bin.  Values in the ENTRIES array are:  center of first bin,
! width of first bin, increment between bins, center of last bin and total
! number of bins.
!
! If METHOD = DO_SKIP, the ENTRIES array consists of four values: number of
! traces to skip initially, number of traces to select at a time, number of
! traces to skip at a time, and total number of traces to select.  (HDR_SEL is
! unused when METHOD = DO_SKIP.)
!</Help>
!
!<Help KEYWORD="COMMENT_D" TYPE= "DISPLAY_ONLY">
!<Tip> Comment only.  Brief description of associated ENTRIES_D usage.</Tip>
! Comment only.  Brief description of associated ENTRIES_D usage.
!</Help>
!
!<Help KEYWORD="COMPLEMENT_D">
!<Tip> Select traces according to the complement of the METHOD chosen? </Tip>
! Default = NO
! Allowed = YES/NO
! If  COMPLEMENT = NO, then mask is criteria for traces being selected (the
! default and usual choice).
!
! If COMPLEMENT = YES, then mask is criteria for traces not being selected (that
! is, use the method specified to define traces NOT selected and select all
! others.)
!</Help>

!<Help KEYWORD="COMBINE">
!<Tip> Logic to use in combining mask selections (A, B, C, and D). </Tip>
! Default = MEETS_ALL
! Allowed = MEETS_ALL  (Select traces meeting criteria of all masks uded.)
! Allowed = MEETS_ANY  (Select traces meeting criteria of any mask used.)
! Final trace selection is determined by combining up to four mask schemes (A, 
! B, C, and D).
!
! If COMBINE = MEETS_ALL is chosen, those traces finally selected must meet the
! criteria of all the masks used.  (Logical intersection.)  
!
! If COMBINE = MEETS_ANY is chosen, those traces finally selected must meet the
! criterion of any of the masks used.  (Logical union.)
!
! If only one mask was used (only "A"), then COMBINE has no effect.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module select_module
      use pc_module
      use named_constants_module
      use mth_module
      implicit none
      private
      public :: select_create     ! uses the parameter cache.
      public :: select_initialize
      public :: select_update     ! uses the parameter cache.
      public :: select_delete
      public :: select            ! main execution (trace processing) routine.
      public :: select_wrapup

      interface select
        module procedure select_private
      end interface

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      integer, parameter           :: max_method = 4   ! Max number of
                                      ! methods (masks, hdrs, complements)
                                      ! Variable subscripts corresponds to
                                      ! 'A', 'B', 'C', 'D' keywords.
      integer, parameter           :: max_entry = 200   ! Max number of
                                      ! select mask 'ENTRIES' values.

      type,public :: select_struct
      private
      logical                         :: skip_wrapup   ! dependent parameter

      character(len=6)                :: action        ! process parameter
      integer                         :: hdr_flag      ! process parameter
      character(len=8), dimension(max_method)  :: method  ! process parameter
      double precision, dimension(max_entry, max_method)  :: entries
      integer, dimension(max_method)  :: hdr_sel       ! process parameter
      logical, dimension(max_method)  :: complement    ! process parameter
      character(len=9)                :: combine       ! process parameter

      integer                         :: nwih          ! global parameter
      integer                         :: ndpt          ! global parameter

      character(len=20), dimension(max_entry, max_method) :: comment
                                         ! context message for entries
      integer, dimension(max_method)  :: entry_cnt     ! dependent parameter
                                         ! Actual number of entry values.
      integer, dimension(max_method)  :: mask_input    ! dependent parameter
                                         ! Actual number of masked input.
      integer, dimension(max_method)  :: mask_output   ! dependent parameter
                                         ! Actual number of masked output
      integer                         :: cnt_input     ! dependent parameter
                                         ! Input trace counter
      integer                         :: cnt_output    ! dependent parameter
                                         ! Output trace counter
      integer                         :: cnt_saved     ! dependent parameter
                                         ! saved trace counter
      integer                         :: cnt_deleted   ! dependent parameter
                                         ! Deleted trace counter
      integer                         :: cnt_killed    ! dependent parameter
                                         ! Killed trace counter
      integer                         :: cnt_flagged   ! dependent parameter
                                         ! Flagged trace counter
      integer                         :: cnt_reversed  ! dependent parameter
                                         ! Reversed Polarity trace counter
      end type select_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(select_struct),pointer,save :: object      ! needed for traps.

      integer :: print_lun = 0                        ! state variable
                 ! default = pc_get_lun()
                 ! valid = 0, do not print
                 !       > 0, Fortran print LUN for system output

      character(len=100),public :: SELECT_IDENT = &
"$Id: select.f90,v 1.55 2006/10/30 14:01:44 Stoeckley prod sps $"

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine select_create (obj)
      type(select_struct),pointer :: obj       ! arguments

!!??? debugging 2000-02-17 Selzler
!call pc_info('   SELECT_CREATE called')
      allocate (obj)

      call select_initialize (obj)

      return
      end subroutine select_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine select_delete (obj)
      type(select_struct),pointer :: obj       ! arguments

!!??? debugging 2000-02-17 Selzler
!call pc_info('   SELECT_DELETE called')

      call select_wrapup (obj)

      deallocate(obj)

      return
      end subroutine select_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine select_initialize (obj)
      type(select_struct),pointer :: obj       ! arguments

      integer :: method_do ! local

!!??? debugging 2000-02-17 Selzler
!call pc_info('   SELECT_INITIALIZE called')
      obj%action = 'SAVE'
      obj%hdr_flag = 48

      do method_do = 1, MAX_METHOD
        obj%method(method_do) = 'DISABLED'
        obj%entry_cnt(method_do) = 0
        obj%entries(:,method_do) = DNIL
        obj%hdr_sel(method_do) = 1
        obj%complement(method_do) = .false.
        obj%mask_input(method_do) = 0
        obj%mask_output(method_do) = 0
      end do

      obj%method(1) = 'RANGES'

      obj%combine = 'MEETS_ALL'
      obj%nwih = 0
      obj%ndpt = 0
      obj%cnt_input = 0
      obj%cnt_output = 0
      obj%cnt_saved = 0
      obj%cnt_deleted = 0
      obj%cnt_killed = 0
      obj%cnt_flagged = 0
      obj%cnt_reversed = 0

      print_lun = 0

      call select_update (obj)

      return
      end subroutine select_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine select_update (obj)
      type(select_struct),target,intent(inout) :: obj        ! arguments

      integer                               :: method_do     ! local
      character(len=1)                      :: suffix        ! local
      integer                               :: mask_cnt      ! local
      integer                               :: entry_do      ! local

      integer                               :: indx          ! local
      integer                               :: action        ! local
      integer                               :: state         ! local
      double precision                      :: total         ! local
      logical                               :: verify_element ! local
      logical                               :: verify_entries ! local


!!??? BEGIN GUI DEBUG CODE... SELZLER 2000-02-08
!character(len=PC_DATACARD_LENGTH) :: cards(100),errmsg
!integer                                                    :: ncards,i
!
!select case(pc_get_update_state())
!case (PC_FRONTEND)
!  call pc_info('SELECT_UPDATE CALLED, state= PC_FRONTEND')
!case (PC_GUI)
!  call pc_info('SELECT_UPDATE CALLED, state= PC_GUI')
!case (PC_BACKEND)
!  call pc_info('SELECT_UPDATE CALLED, state= PC_BACKEND')
!case (PC_EXECUTE)
!  call pc_info('SELECT_UPDATE CALLED, state= PC_EXECUTE')
!case default
!  call pc_info('SELECT_UPDATE CALLED, state= <illegal>')
!end select
!
!call pc_info('    dump gui cards...')
!
!call pc_get_gui_cards (cards,ncards,errmsg)
!do i = 1,ncards
!    call pc_info ('    ' // cards(i))
!end do
!!??? END GUI DEBUG CODE... SELZLER 2000-02-08

      object => obj         ! needed for traps.
      obj%skip_wrapup = .true.

      print_lun = pc_get_lun()

!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!

      call pc_register_array_names &
                        ("entries_a_arrayset", (/ "COMMENT_A", "ENTRIES_A" /))
      call pc_register_array_names &
                        ("entries_b_arrayset", (/ "COMMENT_B", "ENTRIES_B" /))
      call pc_register_array_names &
                        ("entries_c_arrayset", (/ "COMMENT_C", "ENTRIES_C" /))
      call pc_register_array_names &
                        ("entries_d_arrayset", (/ "COMMENT_D", "ENTRIES_D" /))

      call pc_get_global ('nwih'  , obj%nwih)
      call pc_get_global ('ndpt'  , obj%ndpt)

      call pc_get ('action', obj%action)
      call string_to_upper(obj%action)
      call pc_get ('hdr_flag', obj%hdr_flag)

      do method_do = 1, max_method
        suffix = achar(iachar('A') + method_do - 1)
        call pc_get ('METHOD_' // suffix, obj%method(method_do))
        call string_to_upper(obj%method(method_do))
        call pc_get ('ENTRIES_' // suffix, obj%entries(:,method_do), &
          obj%entry_cnt(method_do))
        call pc_get ('HDR_SEL_' // suffix, obj%hdr_sel(method_do))
        call pc_get ('COMPLEMENT_' // suffix, obj%complement(method_do))
      end do

      call pc_get ('combine', obj%combine)
      call string_to_upper(obj%combine)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      select case (obj%action(1:1))
      case ('S')
        obj%action = 'SAVE'
      case ('D')
        obj%action = 'DELETE'
      case ('F')
        obj%action = 'FLAG'
      case ('K')
        obj%action = 'KILL'
      case ('R')
        obj%action = 'RP'
      case default
        call pc_error( &
          'ACTION must be Save, Delete, Flag, Kill or Reverse Polarity')
        obj%action = 'SAVE'
      end select

      if(obj%hdr_flag <= 0 .or. obj%hdr_flag >  obj%nwih) then
        call pc_error('HDR_FLAG must be > zero and <= NWIH')
      end if

      state = pc_get_update_state()

      method_loop: &
      do method_do = 1, max_method
        suffix = achar(iachar('A') + method_do - 1)

!!??? debugging aid, 2000-02-16 Selzler
!if(pc_verify_scalar('METHOD_' // suffix)) then
!  verify_method = '.true.'
!else
!  verify_method = '.false.'
!end if

!???    if(pc_verify_array('ENTRIES_' // suffix) .or. &
!???      pc_verify_arrayset('ENTRIES_' // suffix // '_ARRAYSET') .or. &
!???      pc_verify_scalar('METHOD_' // suffix) .or. &
        if( &
          state == PC_FRONTEND .or. &
          state == PC_BACKEND) then
          verify_entries = .true.
!!??? debugging aid, 2000-02-16 Selzler
!call pc_info('    METHOD_' // suffix // '=' // obj%method(method_do) // &
!  ', verify_method= ' // verify_method // &
!  ', verify_entries= .true., count=', obj%entry_cnt(method_do))
        else
          verify_entries = .false.
!!??? debugging aid, 2000-02-16 Selzler
!call pc_info('    METHOD_' // suffix // '=' // obj%method(method_do) // &
!  ', verify_method= ' // verify_method // &
!  ', verify_entries= .false., count=', obj%entry_cnt(method_do))
        end if

!!??? debugging aid, 2000-02-16 Selzler
!if(pc_verify_arrayset('ENTRIES_' // suffix // '_ARRAYSET')) then
!  call pc_info('        pc_verify_arrayset("ENTRIES_' // suffix // &
!    '_ARRAYSET' // '") == .true.')
!else
!  call pc_info('        pc_verify_arrayset("ENTRIES_' // suffix // &
!    '_ARRAYSET' // '") == .false.')
!end if

!!??? debugging aid, 2000-02-16 Selzler
!if(pc_verify_array('ENTRIES_' // suffix)) then
!  call pc_info('        pc_verify_array("ENTRIES_' // suffix // &
!    '") == .true.')
!else
!  call pc_info('        pc_verify_array("ENTRIES_' // suffix // &
!    '") == .false.')
!end if

        indx = -1
        action = -1
        verify_element = pc_verify_element('ENTRIES_' // suffix, indx, action)

!!??? debugging aid, 2000-02-16 Selzler
!if(verify_element) then
!  call pc_info('        pc_verify_element("ENTRIES_' // suffix // &
!    '",... = .true., indx=', indx, ', action=', action)
!else
!  call pc_info('        pc_verify_element("ENTRIES_' // suffix // &
!    '",... = .false., indx=', indx, ', action=', action)
!end if

        method_choice: &
        select case (obj%method(method_do)(1:2))
        case ('DI') ! method_choice
          obj%method(method_do) = 'DISABLED'

          obj%entry_cnt(method_do) = 0

        case ('RA') ! method_choice
          obj%method(method_do) = 'RANGES'

          ! Start/End entry pairs are required by the RANGE method

          ! Note: regarding an obscure programming trick...
          ! The "if" to force initial values contains an odd test,
          ! i.e. "if( ... .not. pc_verify_scalar('action')))) then"
          ! This allows the values to be forced when the GUI's METHOD_x
          ! is changed by the user but prevents it from being
          ! forced when the parameters are copied by the GUI from the
          ! old process list (one process at a time, not "COPY ALL").

          if(obj%entry_cnt(method_do) < 2 .or. &
            (state == PC_GUI .and. &
              pc_verify_scalar('METHOD_' // suffix) .and. &
              .not. pc_verify_scalar('action'))) then
            ! force a minimum of two entries
!!??? debugging aid, 2000-02-16 Selzler
!call pc_info('SELECT: RANGES, force a minimum of two entries')
            obj%entry_cnt(method_do) = 2
            obj%entries(1, method_do) = DNIL
            obj%entries(2, method_do) = DNIL
          else if(0 /= mod(obj%entry_cnt(method_do), 2)) then
            ! odd count implies insert or delete
!!??? debugging aid, 2000-02-16 Selzler
!call pc_info('SELECT: RANGES, odd count implies insert or delete')
            if(verify_element) then
              if(action == PC_INSERT) then
                ! An element was inserted into the array.
                ! Push any following element one index further into
                ! the array, i.e. always insert a pair of elements.
                obj%entries(indx + 2:max_entry, method_do) = &
                  obj%entries(indx + 1:max_entry - 1, method_do)

                if(0 /= mod(indx, 2)) then
                  ! odd index implies insert *after* original pair
                  if(indx /= obj%entry_cnt(method_do)) then
                    ! This entry was inserted somewhere before the last.
                    ! Make sure both elements of the pair are DNIL
                    obj%entries(indx    , method_do) = DNIL
                  end if

                  obj%entries(indx + 1, method_do) = DNIL
                else
                  ! even index implies insert *before* original pair
                  obj%entries(indx + 1, method_do) = &
                    obj%entries(indx - 1, method_do)
                  obj%entries(indx    , method_do) = DNIL
                  obj%entries(indx - 1, method_do) = DNIL
                end if

                obj%entry_cnt(method_do) = obj%entry_cnt(method_do) + 1
              else if(action == PC_REMOVE) then
                ! an element was removed from the array
                if(0 /= mod(indx, 2)) then
                  ! odd index implies delete *start* for original pair
                  obj%entries(indx:max_entry - 1, method_do) = &
                    obj%entries(indx + 1:max_entry, method_do)
                else
                  ! even index implies delete *end* for original pair
                  obj%entries(indx - 1:max_entry - 1, method_do) = &
                    obj%entries(indx:max_entry, method_do)
                end if

                obj%entry_cnt(method_do) = obj%entry_cnt(method_do) - 1
              else
                call pc_warning('ENTRIES_' // suffix // &
                  ': odd array size, but action is not insert or remove')
              end if
            end if

            ! stay at the current element, i.e. don't jump back to the top.
            call pc_jump_array_element('ENTRIES_' // suffix, indx)
          end if

!!??? debugging aid, 2000-02-16 Selzler
!call pc_info('SELECT_UPDATE RANGE ENTRIES_' // suffix // ', entry_cnt=', &
!  obj%entry_cnt(method_do))
!cycle method_loop
          do entry_do = 1, obj%entry_cnt(method_do), 2
            obj%comment(entry_do, method_do) = 'Start of range'
            obj%comment(entry_do + 1, method_do) = 'End of range'

            if(verify_entries) then
              if(obj%entries(entry_do,method_do) == DNIL .or. &
                obj%entries(entry_do + 1,method_do) == DNIL) then
                call pc_error('RANGES ENTRIES_' // suffix // &
                  ': start/end required for ', entry_do, ' & ', entry_do + 1)
              else if(obj%entries(entry_do,method_do) > &
                obj%entries(entry_do + 1,method_do)) then
                call pc_error('RANGES ENTRIES_' // suffix // &
                  ': start > end invalid for ', entry_do, ' & ', entry_do + 1)
              end if
            end if
          end do
        case ('VA') ! method_choice
          obj%method(method_do) = 'VALUES'

          if(obj%entry_cnt(method_do) < 1 .or. &
            (state == PC_GUI .and. &
              pc_verify_scalar('METHOD_' // suffix) .and. &
              .not. pc_verify_scalar('action'))) then
            ! force a minimum of one entry
            obj%entry_cnt(method_do) = 1
            obj%entries(1,method_do) = DNIL
          end if

          do entry_do = 1, obj%entry_cnt(method_do)
            obj%comment(entry_do, method_do) = 'Value to match'

            if(verify_entries) then
              if(obj%entries(entry_do,method_do) == DNIL) then
                call pc_error('VALUES ENTRIES_' // suffix // &
                  ': match value required for ', entry_do)
              end if
            end if
          end do
        case ('PA') ! method_choice
          obj%method(method_do) = 'PATTERN'
!!??? debugging aid, 2000-02-16 Selzler
!call pc_info('   PATTERN and method_do=', method_do)
!go to 1234

          if(obj%entry_cnt(method_do) /= 5 .or. &
            (state == PC_GUI .and. &
              pc_verify_scalar('METHOD_' // suffix) .and. &
              .not. pc_verify_scalar('action'))) then
            ! force exactly five entries
            obj%entry_cnt(method_do) = 5
            obj%entries(1, method_do) = DNIL
            obj%entries(2, method_do) = DNIL
            obj%entries(3, method_do) = DNIL
            obj%entries(4, method_do) = DNIL
            obj%entries(5, method_do) = DNIL
          end if

          obj%comment(1,method_do) = 'Center of first bin'
          obj%comment(2,method_do) = 'Bin width'
          obj%comment(3,method_do) = 'Bin increment'
          obj%comment(4,method_do) = 'Center of last bin'
          obj%comment(5,method_do) = 'Total number of bins'

!!??? debugging aid, 2000-02-16 Selzler
!???go to 1234
          ! total must always be an integer
          obj%entries(5,method_do) = nint(obj%entries(5,method_do))

          if(verify_element) then
            ! One or more elements have changed.
            if(obj%entries(3,method_do) /= DNIL .and. &
               obj%entries(3,method_do) /= 0.0) then
              ! The increment is defined.
              ! If increment isn't defined, then nothing can be computed.
              if(obj%entries(2,method_do) >= obj%entries(3,method_do)) then
                ! Bin width is greater than or equal to bin increment.
                call pc_error('PATTERN ENTRIES_' // suffix // &
                  ', Bin width >= increment, pattern invalid.')
              else if((indx == 1 .or. indx == 3 .or. indx == 4) .and. &
                  obj%entries(4,method_do) /= DNIL) then
                ! Attempt to recompute 'Total' bin count.
                if(obj%entries(1,method_do) == DNIL) &
                  obj%entries(1,method_do) = 0.0

                total = nint(1.0 + &
                  (obj%entries(4,method_do) - obj%entries(1,method_do)) / &
                  obj%entries(3,method_do))

                if(total < 1.0) then
                  ! Computed total is not valid... make the problem obvious.
                  obj%entries(5,method_do) = 0.0
                else
                  obj%entries(5,method_do) = total

                  ! Rationalize the Last bin position.
                  obj%entries(4,method_do) = obj%entries(1,method_do) + &
                    obj%entries(3,method_do) * (total - 1.0)
                end if
              else if((indx == 1 .or. indx == 3 .or. indx == 5) .and. &
                  obj%entries(5,method_do) /= 0.0) then
                if(obj%entries(5,method_do) < 1.0) then
                  ! Specified total is not valid... make the problem obvious.
                  obj%entries(5,method_do) = 0.0
                else
                  ! Recompute Last bin value.
                  if(obj%entries(1,method_do) == DNIL) &
                    obj%entries(1,method_do) = 0.0

                  obj%entries(4,method_do) = obj%entries(1,method_do) + &
                    obj%entries(3,method_do) * &
                    (obj%entries(5,method_do) - 1.0)
                end if
              end if
            end if
          end if

          if(verify_entries) then
            if(obj%entries(1, method_do) == DNIL) then
              call pc_error('PATTERN ENTRIES_' // suffix // &
                ', Center of first bin must be defined')
            end if

            if(obj%entries(2, method_do) == DNIL) then
              call pc_error('PATTERN ENTRIES_' // suffix // &
                ', Width of first bin must be defined')
            else if(obj%entries(2, method_do) <= 0.0) then
              call pc_error('PATTERN ENTRIES_' // suffix // &
                ', Width of first bin must be > 0.0')
            else if(obj%entries(2, method_do) >= obj%entries(3,method_do)) then
              call pc_error('PATTERN ENTRIES_' // suffix // &
                ', Width of first bin must be < increment between bins')
            end if

            if(obj%entries(3, method_do) == DNIL) then
              call pc_error('PATTERN ENTRIES_' // suffix // &
                ', Increment between bins must be defined')
            else if(obj%entries(3, method_do) == 0.0) then
              call pc_error('PATTERN ENTRIES_' // suffix // &
                ', Increment between bins must be non-zero')
            end if

            if(obj%entries(4, method_do) == DNIL) then
              call pc_error('PATTERN ENTRIES_' // suffix // &
                ', Center of last bin must be defined')
            end if

            if(obj%entries(5, method_do) < 1.0) then
              call pc_error('PATTERN ENTRIES_' // suffix // &
                ', Total number of bins must be >= 1.0')
            end if
          end if

        case ('DO') ! method_choice
          obj%method(method_do) = 'DO_SKIP'

          if(obj%entry_cnt(method_do) /= 4 .or. &
            (state == PC_GUI .and. &
              pc_verify_scalar('METHOD_' // suffix) .and. &
              .not. pc_verify_scalar('action'))) then
            ! force exactly four entries
            obj%entry_cnt(method_do) = 4
            obj%entries(1, method_do) = DNIL
            obj%entries(2, method_do) = DNIL
            obj%entries(3, method_do) = DNIL
            obj%entries(4, method_do) = DNIL
          end if

          obj%comment(1,method_do) = 'Initial skip'
          obj%comment(2,method_do) = 'Number to do'
          obj%comment(3,method_do) = 'Number to skip'
          obj%comment(4,method_do) = 'Total number to do'

          if(verify_entries) then
            if(obj%entries(1, method_do) == DNIL) then
              call pc_error('DO_SKIP ENTRIES_' // suffix // &
                ', initial skip must be defined')
            else if(obj%entries(1, method_do) < 0) then
              call pc_error('DO_SKIP ENTRIES_' // suffix // &
                ', initial skip must not be negative')
            end if

            if(obj%entries(2, method_do) == DNIL) then
              call pc_error('DO_SKIP ENTRIES_' // suffix // &
                ', traces per group must be defined')
            else if(obj%entries(2, method_do) < 1) then
              call pc_error('DO_SKIP ENTRIES_' // suffix // &
                ', traces per group must be greater than zero')
            end if

            if(obj%entries(3, method_do) == DNIL) then
              call pc_error('DO_SKIP ENTRIES_' // suffix // &
                ', trace skip per group must be defined')
            else if(obj%entries(3, method_do) < 0) then
              call pc_error('DO_SKIP ENTRIES_' // suffix // &
                ', trace skip per group must not be negative')
            end if

            if(obj%entries(4, method_do) == DNIL) then
              call pc_error('DO_SKIP ENTRIES_' // suffix // &
                ', total number of traces must be defined')
            else if(obj%entries(4, method_do) < 1) then
              call pc_error('DO_SKIP ENTRIES_' // suffix // &
                ', total number of traces must be greater than zero')
            end if
          end if
        case default ! method_choice
          call pc_error('METHOD_' // suffix // &
            ' must be Ranges, Values, Pattern or Do_skip')
          obj%method(method_do) = 'RANGES'
        end select method_choice
      end do method_loop

!!??? debugging aid, 2000-02-16 Selzler
!call pc_info('   Exiting method_loop')

      mask_cnt = 0

      do method_do = 1, max_method
        if(obj%method(method_do) /= 'DISABLED') mask_cnt = mask_cnt + 1
      end do

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        if(mask_cnt == 0) then
          call pc_error('None of the masks are valid')
        end if
      end if
!??? debugging aid, 2000-02-16 Selzler
!???1234 continue

!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!


!!??? debugging aid, 2000-02-16 Selzler
!call pc_info('   write data cards')

      call pc_put_options_field('action', &
        (/ "SAVE  ", "DELETE", "FLAG  ", "KILL  ", "RP    " /), 5)
      call pc_put ('action', obj%action)
      call pc_put ('hdr_flag', obj%hdr_flag)

      if(obj%action == 'FLAG') then
        call pc_put_sensitive_field_flag('hdr_flag', .true.)
      else
        call pc_put_sensitive_field_flag('hdr_flag', .false.)
      end if

      do method_do = 1, max_method
        suffix = achar(iachar('A') + method_do - 1)

        call pc_put_options_field('METHOD_' // suffix, &
          (/ "DISABLED", "RANGES  ", "VALUES  ", "PATTERN ", "DO_SKIP " /), 5)

        call pc_put ('METHOD_' // suffix, obj%method(method_do))

!!??? debugging aid, 2000-02-16 Selzler
!call pc_info('   call pc_put("ENTRIES_' // suffix // ', ..., count=', &
!  obj%entry_cnt(method_do))

        call pc_put ('ENTRIES_' // suffix, obj%entries(:,method_do), &
          obj%entry_cnt(method_do))

!!??? debugging aid, 2000-02-16 Selzler
!call pc_info('   return from pc_put')

        call pc_put_gui_only ('COMMENT_' // suffix, obj%comment(:,method_do), &
          obj%entry_cnt(method_do))

        call pc_put ('HDR_SEL_' // suffix, obj%hdr_sel(method_do))

        call pc_put_options_field('COMPLEMENT_' // suffix, &
          (/ "YES", "NO " /), 2)

        call pc_put ('COMPLEMENT_' // suffix, obj%complement(method_do))

        if(obj%method(method_do) == 'DISABLED') then
          call pc_put_sensitive_field_flag('HDR_SEL_' // suffix, .false.)
          call pc_put_sensitive_arrayset_flag( &
            'ENTRIES_' // suffix // '_ARRAYSET', .false.)
          call pc_put_sensitive_array_flag('ENTRIES_' // suffix, .false.)
          call pc_put_sensitive_field_flag('COMPLEMENT_' // suffix, .false.)
        else
          if(obj%method(method_do) == 'DO_SKIP') then
            call pc_put_sensitive_field_flag('HDR_SEL_' // suffix, .false.)
          else
            call pc_put_sensitive_field_flag('HDR_SEL_' // suffix, .true.)
          end if

          call pc_put_sensitive_arrayset_flag( &
            'ENTRIES_' // suffix // '_ARRAYSET', .true.)
          call pc_put_sensitive_array_flag('ENTRIES_' // suffix, .true.)
          call pc_put_sensitive_field_flag('COMPLEMENT_' // suffix, .true.)
        end if

        call pc_put_sensitive_array_flag('COMMENT_' // suffix, .false.)
      end do

      call pc_put_options_field('combine', (/ "MEETS_ALL", "MEETS_ANY" /), 2)
      call pc_put ('combine', obj%combine)

      if(mask_cnt > 1) then
        call pc_put_sensitive_field_flag('combine', .true.)
      else
        call pc_put_sensitive_field_flag('combine', .false.)
      end if

      call pc_put_control ('need_request', .true.)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!!??? debugging aid, 2000-02-16 Selzler
!call pc_info('   prepare for execution')

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine select_update

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine select_private (obj,ntr,hd,tr)
      type(select_struct),intent(inout) :: obj            ! arguments
      integer,intent(inout)             :: ntr            ! arguments
      double precision,intent(inout)    :: hd(:,:)        ! arguments
      real   ,intent(inout)             :: tr(:,:)        ! arguments

      INTEGER :: trace_out, trace_in, item
!
!----------PROCESS A GROUP OF TRACES.
!
      IF (ntr <= 0) THEN
        ! all done
        call select_wrapup(obj)

        return
      end if

      trace_out = 0

      ntr_loop: &
      DO trace_in = 1, ntr
        ! process all traces
        obj%cnt_input = obj%cnt_input + 1
        if(select_mask(obj, hd(:,trace_in))) then
          ! mask does select this trace, positive action

          action_choice_yes: &
          select case (obj%action(1:1))
          case ('S')  ! 'SAVE' action
            trace_out = trace_out + 1
            if(trace_in /= trace_out) then
              tr(:obj%ndpt, trace_out) = tr(:obj%ndpt, trace_in)
              hd(:obj%nwih, trace_out) = hd(:obj%nwih, trace_in)
            end if
            obj%cnt_saved = obj%cnt_saved + 1
          case ('D')  ! 'DELETE' action
            obj%cnt_deleted = obj%cnt_deleted + 1
            CYCLE
          case ('K')  !  'KILL' action
            trace_out = trace_out + 1
            TR(:obj%ndpt,trace_out) = 0.
            hd(25,trace_in) = 0.0
            obj%cnt_killed = obj%cnt_killed + 1
          case ('F') !  'FLAG' action
            trace_out = trace_out + 1
            HD(obj%hdr_flag,trace_in) = 1.  ! set header flag
            obj%cnt_flagged = obj%cnt_flagged + 1
          case ('R') !  'RP' reverse polarity action
            trace_out = trace_out + 1
            TR(:obj%ndpt,trace_out) = -TR(:obj%ndpt,trace_in)
            obj%cnt_reversed = obj%cnt_reversed + 1
          end select action_choice_yes
        else
          ! mask does not select this trace, negative action
          action_choice_no: &
          select case (obj%action(1:1))
          case ('S')  ! 'SAVE' action
            cycle
          case ('D')  ! 'DELETE' action
            trace_out = trace_out + 1

            if(trace_in /= trace_out) then
              tr(:obj%ndpt, trace_out) = tr(:obj%ndpt, trace_in)
              hd(:obj%nwih, trace_out) = hd(:obj%nwih, trace_in)
            end if
          case ('F')  ! 'FLAG' action
            trace_out = trace_out + 1
            HD(obj%hdr_flag,trace_in) = 0.  ! reset header flag
          case default
            trace_out = trace_out + 1
          end select action_choice_no

        end if
      END DO ntr_loop

      IF (trace_out == 0) THEN
        ntr = need_traces
      else
        ntr = trace_out
        DO item = 1, ntr
          obj%cnt_output = obj%cnt_output + 1
          HD(1,item) = obj%cnt_output  ! Make sure header 1 is correct
        END DO
      ENDIF

      return
      end subroutine select_private

!!--------------------------- select_mask -------------------------------!!
!!--------------------------- select_mask -------------------------------!!
!!--------------------------- select_mask -------------------------------!!

      logical function select_mask (obj, hd)
      ! return .true., iff trace is selected by masking methods
      type(select_struct),intent(inout)  :: obj            ! arguments
      double precision,intent(inout)     :: hd(:)          ! arguments

      integer :: method_do                ! local, method do loop
      integer :: item                     ! local, generic do loop
      double precision :: hdr_value       ! local, header value
      logical :: match                    ! local, one basic mask result
      integer :: initial_skip, loop_select, loop_skip, select_max
      integer :: input_offset, output_offset, loop_stride
      integer :: remainder, quotient

      real, parameter :: SLOP = 0.000001  ! epsilon factor for 'VALUES'

      select_mask = .false. ! assume masks will fail, unless refuted

      method_loop: &
      do method_do = 1, max_method
        ! loop over all masking methods
        if(obj%entry_cnt(method_do) == 0) then
          ! disabled, this mask method is not applicable
          cycle method_loop ! try next mask method, if any
        end if

        method_choice: &
        select case (obj%method(method_do)(1:2))
        case ('DI')  ! 'DISABLED' mask method

        case ('VA')  ! 'VALUES' mask method
          hdr_value = hd(obj%hdr_sel(method_do))
          match = .false.

          value_loop: &
          do item = 1, obj%entry_cnt(method_do)
            ! loop over all values, looking for a header match
            if(abs(hdr_value - obj%entries(item, method_do)) <= &
              SLOP * ABS(obj%entries(item, method_do))) then
              ! header and mask value are (almost) equal
              match = .true.
              exit value_loop
            end if
          end do value_loop

          if(obj%complement(method_do)) then
            if(match) then
              match = .false.
            else
              match = .true.
            end if
          end if

        case ('RA')  ! 'RANGES' mask method
          hdr_value = hd(obj%hdr_sel(method_do))
          match = .false.  ! assume failure, unless refuted

          range_loop: &
          do item = 1, obj%entry_cnt(method_do), 2
            ! loop over all value pairs, looking for a header match
            if(hdr_value >= dmin1(obj%entries(item    , method_do), &
                                  obj%entries(item + 1, method_do)) .and. &
               hdr_value <= dmax1(obj%entries(item    , method_do), &
                                  obj%entries(item + 1, method_do))) then
              ! header and mask value pair intersect
              match = .true.
              exit range_loop
            end if
          end do range_loop

          if(obj%complement(method_do)) then
            if(match) then
              match = .false.
            else
              match = .true.
            end if
          end if

        case ('PA')  ! 'PATTERN' mask method
          hdr_value = hd(obj%hdr_sel(method_do))
          match = mth_included (hdr_value,obj%entries(1,method_do),  &
                                          obj%entries(2,method_do),  &
                                          obj%entries(3,method_do),  &
                                     nint(obj%entries(5,method_do)))

        case ('DO')  ! 'DO_SKIP' mask method
          match = .false.  ! assume failure, unless refuted

          initial_skip = int(obj%entries(1, method_do))

          if(obj%cnt_input <= initial_skip) then
            ! intersects initial skip zone
            if(obj%complement(method_do)) match = .true.
          else
            ! beyond initial skip zone
            input_offset = obj%cnt_input - 1 - initial_skip
            loop_select = obj%entries(2, method_do)
            loop_skip = obj%entries(3, method_do)
            loop_stride = loop_select + loop_skip
            remainder = mod(input_offset, loop_stride)

            if(remainder < loop_select) then
              ! loop select zone
              quotient = int(input_offset / loop_stride)
              output_offset = int(quotient * loop_select + remainder)
              select_max = obj%entries(4, method_do)

              if(output_offset < select_max) then
                ! less than the total number of traces to select
                if(.not. obj%complement(method_do)) match = .true.
              else
                ! greater than the total number of traces to select
                if(obj%complement(method_do)) match = .true.
              end if
            else
              ! loop skip zone
              if(obj%complement(method_do)) match = .true.
            end if
          end if
        end select method_choice

        if(obj%combine == 'MEETS_ALL') then
          ! 'AND'/'MEETS_ALL'/intersection
          if(.not. match) return
        else
          ! 'OR'/'MEETS_ANY'/union
          if(match) then
            select_mask = .true.
            return
          end if
        end if
      end do method_loop

      if(obj%combine == 'MEETS_ALL') then
        ! 'AND'/'MEETS_ALL'/intersection, all masks succeeded
        select_mask = .true.
      end if

      return
      end function select_mask


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine select_wrapup (obj)
      type(select_struct), intent(inout) :: obj       ! arguments

!!??? debugging 2000-02-17 Selzler
!call pc_info('   SELECT_WRAPUP called, print_lun=', print_lun)
!return
      if(obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      WRITE (print_lun, *) 'SELECT PROCESS WRAPUP'
      WRITE (print_lun, *) obj%cnt_input, &
        ' TRACES LOOKED AT WITH SELECTION CRITERIA'

      IF (obj%action(1:1) == 'S') WRITE (print_lun, *) obj%cnt_saved, &
        ' TRACES SAVED BY SELECTION CRITERIA'

      IF (obj%action(1:1) == 'D') WRITE (print_lun, *) obj%cnt_deleted, &
        ' TRACES DELETED BY SELECTION CRITERIA'

      IF (obj%action(1:1) == 'K') WRITE (print_lun, *) obj%cnt_killed, &
        ' TRACES KILLED BY SELECTION CRITERIA'

      IF (obj%action(1:1) == 'F') WRITE (print_lun, *) obj%cnt_flagged, &
        ' TRACES FLAGGED BY SELECTION CRITERIA'

      IF (obj%action(1:1) == 'R') WRITE (print_lun, *) obj%cnt_reversed, &
        ' TRACES REVERSED BY SELECTION CRITERIA'

      WRITE (print_lun, *) obj%cnt_output, ' TRACES OUTPUT FROM SELECT'

      return
      end subroutine select_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module select_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

