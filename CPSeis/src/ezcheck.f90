!<CPS_v1 type="PROCESS"/>
!!---------------------------- ezcheck.f90 ------------------------------!!
!!---------------------------- ezcheck.f90 ------------------------------!!
!!---------------------------- ezcheck.f90 ------------------------------!!

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
! Name       : ezcheck
! Category   : miscellaneous
! Written    : 2001-02-14   by: Randy L. Selzler, Data-Warp, Inc.
! Revised    : 2007-01-03   by: D. Glover
! Maturity   : production
! Purpose    : Report differences between input and test dataset
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This process reports discrepancies between job input and test data.
! Its used for regression testing of changes in the system and software.
! For example, does process "X" still produce the same results after it
! has been recompiled with optimization or run on different hardware.
!
! Checks are made on global parameters, trace headers and samples.
! Tolerances may be chosen for each item individually.
! Its important to choose them carefully for each test suite.
! If too loose, problems will be missed and testing becomes pointless.
! If too strict, people will be frustrated and tests will be ignored.
!
! Regression testing can increase confidence in the overall system.
! It can be used to validate different hardware platforms,
! compiler versions, OS upgrades, application enhancements and bug "fixes".
!
! Test suites should be maintained and supplemented as needed.
! Each CPS process should have a test suite to exercise its major options.
! Test suites should also be provided for major primitives too.
!
! Test job setups must include:
!     1) process to be tested.
!         A) input parameters and seismic data.
!         B) output seismic (passed to ezcheck).
!         C) printout (for manual inspection).
!     2) ezcheck process.
!         A) input parameters (tolerances and options).
!         B) input seismic, provided by (1) above.
!         C) test seismic, containing expected results (pre-existing).
!             Associated job printout is useful for comparison.
!         D) output seismic (for further analysis).
!         E) printout (regression test report).
!
! Other processes are often needed in a test job.
! For example, an input step and perhaps a gather step may be needed
! to provide data to the process being tested.
!
! The ezcheck process can output seismic that is a copy of the input data,
! test data or various statistical relations between them.
!
! The input data or the known test data can be passed through a sequence
! of regression tests in one job to evaluate standard processing flows.
! The statistical output from ezcheck can be used to diagnose problems.
!
! NOTE: a statistical algorithm can be applied to the trace samples AND
! ALL HEADER WORDS, except header word 1 (trace sequence number).
! The header statistics can be dumped or displayed to reveal problems,
! but are essentially useless for traditional seismic processing purposes.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process outputs the same traces as it receives (possibly altered).
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! GATHERED  whether traces are a legitimate gather  If OUT_DATA is INPUT then
!                                                   GATHERED is unchanged,
!                                                   otherwise its .false.
! all       all globals                             compared (INPUT and TEST).
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 1       Sequential Trace Count     compared and unchanged.
! other   Other header words         compared between INPUT and TEST
!                                    If OUT_DATA=INPUT headers do not change,
!                                    else if OUT_DATA=TEST headers may change,
!                                    otherwise (i.e. DIFF, MEAN or VARIANCE)
!                                    most headers will change.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author                 Description
!     ----        ------                 -----------
! 12. 2007-01-03  D. Glover              Added relaxed comparisons for samples within epsilon of zero
!                                        New keywords smp_0_eps_state and smp_0_eps
!                                        Fixed logic bug in compare samples EQUAL block
! 11. 2006-06-06  Stoeckley              Add call to pc_register_array_names for
!                                         SeisSpace.
! 10. 2003-06-16  Karen Goodger          Add use getlun_module.
!  9. 2002-07-01  Karen Goodger          Change trcio extension to trc.
!  8. 2002-06-12  Karen Goodger          Split into three screens for better
!                                        readability.
!  7. 2002-05-20  C. C. Burch            Increased #digits in verbose prints.
!  6. 2002-03-18  Randy L. Selzler       Clarified VERBOSE description.
!                                        Corrected bug in trace loop logic
!                                        when JOB and TEST trace counts differ.
!                                        Enhanced VERBOSE print out with
!                                        smp_err and detailed header printing.
!  5. 2002-01-17  Randy L. Selzler       Corrected bug in global comparison.
!                                        Add file selection box for test_data.
!                                        Corrected bug in VERBOSE column hdrs.
!  4. 2001-11-09  Randy L. Selzler       Corrected bug in do variable usage.
!  3. 2001-03-30  Randy L. Selzler       Improved print formatting.
!  2. 2001-03-08  Randy L. Selzler       Initial check-in.
!  1. 2001-02-14  Randy L. Selzler       begin development.
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
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.      
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
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
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES              
!
! Automated regression testing requires a simple PASS/FAIL response.
! Any future enhancements should keep this PRIMARY OBJECTIVE in mind.
! Two secondary objectives should be retained as well:
! 1) report discrepancies in the printout in a meaningful way.
! 2) output seismic data that can help diagnose discrepancies.
!
! Two sets of control tables are needed for both global and header checking.
! The first set consists of those referenced in the GUI.
! They are designed to be convenient for the end user.
! The second set is derived automatically from the first set and
! from information obtained from the TEST_DATA.
! The second set is optimized for processing convenience and efficiency.
!
! The keyword/variable names for the first set begin with "GLOB_" and "HDR_".
! They are related to variables named "GLOBAL_" and "HEADER_" in the
! second set of control tables.
!
! The first set is effectively documented by the GUI help information.
! The second set is documented below.
!
! GLOBAL_COUNT: Integer: scalar:
!     Number of entries in GLOBAL_NAME, GLOBAL_MODE, GLOBAL_TOLERANCE,
!     and GLOBAL_RESULT.
! GLOBAL_NAME: Character: dimension (GLOBAL_COUNT):
!     One entry for unique global name in GLOB_NAME, job input and TEST_DATA.
! GLOBAL_MODE: Integer: dimension (GLOBAL_COUNT):
!     The integer value corresponds to the GLOB_MODE_OPTIONS number:
!     1=IGNORE, 2=EQUAL, 3=SAME, 4=ABS, 5=REL.
!     The value is provided by GLOB_MODE if GLOB_NAME matches a GLOBAL_NAME
!     and GLOB_MODE is not DEFAULT.
!     Otherwise, GLOB_DEF_MODE provides a default value.
! GLOBAL_TOLERANCE: real: dimension (GLOBAL_COUNT):
!     The value is provided by GLOB_TOL if GLOB_NAME matches a GLOBAL_NAME
!     and GLOB_MODE is not DEFAULT.
!     Otherwise, GLOB_DEF_TOL provides a default value.
! GLOBAL_RESULT: Character: dimension (GLOBAL_COUNT):
!     PASS implies that the globals provided by the job input and TEST_DATA
!     pass the conditions specified by GLOBAL_MODE and GLOBAL_TOLERANCE.
!     PASS is assumed if the MODE is IGNORE, regardless of whether the
!     parameter exists in either the job or TEST_DATA.
!     The NO-JOB, NO-TEST and NO-GLOBAL suffix implies the global is not
!     in the job input, TEST_DATA or neither respectively.
!     Defined values are:
!         PASS, PASS-NO-JOB, PASS-NO-TEST, PASS-NO-GLOBAL,
!         FAIL, FAIL-NO-JOB, FAIL-NO-TEST, FAIL-NO-GLOBAL
! GLOBAL_SUMMARY: Integer: scalar
!     Count of globals that FAIL a global test.
!
! HEADER_COUNT: Integer: scalar:
!     Number of entries in HEADER_MODE, HEADER_TOLERANCE, HEADER_RESULT,
!     HEADER_EXPLICIT and VERBOSE_HEADERS.
!     Count is the maximum NWIH from the job input and TEST_DATA.
! HEADER_MODE: Integer: dimension (HEADER_COUNT):
!     The number is needed for efficiency when checking many traces.
!     The integer value corresponds to the HDR_MODE_OPTIONS number:
!     1=IGNORE, 2=EQUAL, 3=SAME, 4=ABS, 5=REL.
!     The value is provided by HDR_MODE if HDR_NUM matches a header number
!     and HDR_MODE is not DEFAULT.
!     Otherwise, HDR_DEF_MODE provides a default value.
! HEADER_TOLERANCE: real: dimension (HEADER_COUNT):
!     The value is provided by HDR_TOL if HDR_NUM matches a header number
!     and HDR_MODE is not DEFAULT.
!     Otherwise, HDR_DEF_TOL provides a default value.
! HEADER_RESULT: Integer: dimension (HEADER_COUNT):
!     Zero implies that the headers provided by the job input and TEST_DATA
!     pass the conditions specified by HEADER_MODE and HEADER_TOLERANCE.
!     Zero is assumed if the MODE is IGNORE, regardless of whether the
!     header exists in either the job input or TEST_DATA.
!     Positive results count the number of comparisons that FAIL.
!     Negative results indicate the header word does not exist:
!     -1 header word does not exist in INPUT.
!     -2 header word does not exist in TEST_DATA.
!     -3 header word does not exist in INPUT or TEST_DATA.
!     If negative, PASS is assumed iff MODE is IGNORE, otherwise FAIL.
! HEADER_EXPLICIT: Logical: dimension (HEADER_COUNT)
!     .true. if this header was explicitly listed in HDR_NUM.
!     Used to force synopsis print out for this header.
! HEADER_ICOUNT: Integer:
!     Number of entries in HEADER_ICHECK.
! HEADER_ICHECK: Integer: dimension (HEADER_ICOUNT):
!     The value is an index into HEADER_MODE/TOLERANCE/RESULT.
!     Only these entries are checked on a trace by trace basis.
!     Unchecked entries have MODE=IGNORE or FAIL with a -1, -2 or -3 RESULT.
! HEADER_SUMMARY: Integer: scalar
!     Running count of traces that FAIL any header test.
! SAMPLE_MASK: Integer: dimension (NDPT_JOB):
!     Zero implies that the sample provided by the job input and TEST_DATA
!     pass the conditions specified by SMP_MODE and SMP_TOLERANCE.
!     One implies the sample failed.
!     Zero is assumed if the MODE is IGNORE, regardless of whether the
!     sample exists in either the job input or TEST_DATA.
! SAMPLE_RESULT: Integer: dimension (NDPT_JOB):
!     Zero implies all samples provided by the job input and TEST_DATA
!     pass the conditions specified by SMP_MODE and SMP_TOLERANCE.
!     Positive results count the number of comparisons that FAIL.
! SAMPLE_SUMMARY: Integer: scalar
!     Running count of samples that FAIL any sample test.
! TRACE_SUMMARY: Integer: scalar
!     Running count of traces that FAIL any sample test.
!
! VERBOSE_SUMMARY: Integer: scalar
!     Running count of traces with VERBOSE details printed.
! VERBOSE_HEADERS: Integer: dimension (HEADER_COUNT):
!     Index of failing headers for verbose printout.
!
! Other noteworthy variables are:
!
! TRCIO_FILE: type(trcio_struct): scalar
!     Control structure for access to TEST_DATA (seismic traces).
! DUMP_LUN: Integer: scalar:
!     Logical unit for accumulating trace discrepancies when REPORT= VERBOSE.
! TST_HDR: double precision: dimension(nwih_tst):
!     Trace header buffer for TEST_DATA.
! TST_SMP: real: dimension(ndpt_tst):
!     Trace sample buffer for TEST_DATA.
! TMP_HDR: double precision: dimension(nwih_tst):
!     Trace header buffer for intermediate results.
! TMP_SMP: real: dimension(ndpt_tst):
!     Trace sample buffer for intermediate results.
! JOB_GATHER_CNT: Integer: scalar:
!     Running count of input gathers.
! JOB_TRC_CNT: Integer: scalar:
!     Running count of input traces.
! COM_TRC_CNT: Integer: scalar:
!     Running count of traces compared (input and test).
! TST_TRC_CNT: Integer: scalar:
!     Running count of TEST_DATA traces.
! TST_TRC_MAX: Integer: scalar:
!     Maximum number of TEST_DATA traces reported by trcio.
! JOB_EOD: logical: scalar
!     .true. iff job input End-Of-Data has been seen, otherwise .false.
! TST_EOD: logical: scalar
!     .true. iff job TEST_DATA End-Of-Data has been seen, otherwise .false.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS ezcheck Process/NC=80>
! EZCHECK: Report differences between input and test dataset
!
!Select TEST_DATA[TEST_DATA]=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                [TEST_DATA_INFO]=`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! OUT_DATA=`CCCCC        REPORT=`CCCCCCCCCC     N_LIMIT=`IIIII
!   `--------------------------------------------   Modes
!    Global parameter tolerances                    D  DEFAULT
!                                                   I  IGNORE
!    GLOB_NAME     GLOB_MODE   GLOB_TOL             E  EQUAL
!    `SSSSSSSSSSSSS`SSSSSSSSSSS`FFFFFFFFFFF         S  SAME
!    `SSSSSSSSSSSSS`SSSSSSSSSSS`FFFFFFFFFFF         A  ABS
!    `SSSSSSSSSSSSS`SSSSSSSSSSS`FFFFFFFFFFF         R  REL
!    `SSSSSSSSSSSSS`SSSSSSSSSSS`FFFFFFFFFFF
!    `SSSSSSSSSSSSS`SSSSSSSSSSS`FFFFFFFFFFF
!    `SSSSSSSSSSSSS`SSSSSSSSSSS`FFFFFFFFFFF
!    `SSSSSSSSSSSSS`SSSSSSSSSSS`FFFFFFFFFFF
!    `SSSSSSSSSSSSS`SSSSSSSSSSS`FFFFFFFFFFF
!
!    GLOB_DEF_MODE=`CCCCCCCCCCC
!
!    GLOB_DEF_TOL=`FFFFFFFFFFF
!   `--------------------------------------------
!<PARMS TEST_DATA[/ML=128/XST]>
!<PARMS GLOB_NAME_ARRAYSET[/ML=18/XST/YST]>
!<NS Header Parameters/NC=80>
!   `----------------------------------------   Modes
!    Trace header tolerances                    D  DEFAULT
!                                               I  IGNORE
!    HDR_NUM    HDR_MODE    HDR_TOL             E  EQUAL
!    `IIIIIIIIII`SSSSSSSSSSS`FFFFFFFFFFF        S  SAME
!    `IIIIIIIIII`SSSSSSSSSSS`FFFFFFFFFFF        A  ABS
!    `IIIIIIIIII`SSSSSSSSSSS`FFFFFFFFFFF        R  REL
!    `IIIIIIIIII`SSSSSSSSSSS`FFFFFFFFFFF
!    `IIIIIIIIII`SSSSSSSSSSS`FFFFFFFFFFF
!    `IIIIIIIIII`SSSSSSSSSSS`FFFFFFFFFFF
!    `IIIIIIIIII`SSSSSSSSSSS`FFFFFFFFFFF
!    `IIIIIIIIII`SSSSSSSSSSS`FFFFFFFFFFF
!
!    HDR_DEF_MODE=`CCCCCCCC
!
!    HDR_DEF_TOL=`FFFFFFFFFFF
!   `----------------------------------------
!<PARMS HDR_NUM_ARRAYSET[/XST/YST]>
!<NS Sample Parameters/NC=80>
!   `------------------------------------   Modes
!    Trace sample tolerances                I  IGNORE
!                                           E  EQUAL
!    SMP_MODE=`CCCCCCCCCCC                  S  SAME
!                                           A  ABS
!    SMP_TOL=`FFFFFFFFFFF                   R  REL
!
!    SMP_0_EPS_STATE=`CCCCCCCCCCC           States
!                                           ON
!    SMP_0_EPS=`FFFFFFFFFFF                 OFF
!   `------------------------------------
!</gui_def>
!
!-------------------------------------------------------------------------------
!
!<HelpSection>
!<Help KEYWORD="TEST_DATA_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of TEST_DATA. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_TEST_DATA">
!<Tip> Choose TEST_DATA using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="TEST_DATA">
!<Tip> Pathname for test data containing expected results. </Tip>
! Default = NONE
! Allowed = char
! Path and file name for seismic data to be compared with job input data.
!
! The test data is normally created using the same process and input
! parameters as the one providing input to EZCHECK in this job.
! The test data may have used another version of a library, process,
! compiler, optimization flags, hardware platform or operating system.
! These variations are not supposed to change the expected results and
! is the premise upon which EZCHECK regression testing is based.
!</Help>
!
!<Help KEYWORD="OUT_DATA">
!<Tip> Select the desired output seismic data . </Tip>
! Default = INPUT
! Allowed = INPUT copy input data to output
! Allowed = TEST  copy test  data to output
! Allowed = DIFF  output difference, (INPUT - TEST)
! Allowed = MEAN  output mean, 0.5 * (INPUT + TEST)
! Allowed = VARIANCE output variance, 0.5 * ((INPUT-MEAN)**2 + (TEST-MEAN)**2)
! INPUT and TEST can be used to pass EZCHECK input data or known test data
! through a sequence of regression tests in one job to evaluate
! standard processing flows.
! DIFF, MEAN and VARIANCE output can be used to further diagnose problems.
!</Help>
!
!<Help KEYWORD="REPORT">
!<Tip> Level of detail in printout. </Tip>
! Default = NORMAL
! Allowed = TERSE   simple PASS/FAIL message.
! Allowed = NORMAL  statistical results of regression test.
! Allowed = VERBOSE include details of first N_LIMIT trace anomalies.
!
! Specifies level of detail in printout.
!
! If REPORT=VERBOSE the following trace anomalies are printed:
!   First print line
!     Printed with the following column headings:
!       Failure:     sequential failure number of this anomaly.
!       SeqTrcNum:   sequential trace  number of failing trace.
!       Gather:      sequential gather number of failing trace.
!       NTR:         number of traces within this gather.
!       Trace:       trace number within gather of failing trace.
!       Bad-Hdrs:    number of headers that failed, if any.
!       Bad-Smps:    number of samples that failed, if any.
!       Smp_Err:     sample error statistic 
!   Second print line (potentially very long):
!     Printed with the following column headings:
!       HDR:    index of failing header
!       JOB:    value of JOB  trace header at failing index
!       TEST:   value of TEST trace header at failing index
!
! Smp_Err attempts to measure the significance of the failed samples
! within the non-mute range defined by the TEST trace.
! If smp_err is much greater than 1.0, one or more samples are very bad.
! If much less than 1.0, differences may be relatively benign.
!
! CAUTION: the algorithm is hard to justify and may be very misleading.
! cbyte should be used to examine statistical variations if unsure.
!
!             max absolute difference of non-muted failing samples
! smp_err = ---------------------------------------------------------
!               average absolute value of non-muted TEST samples
!
!</Help>
!
!<Help KEYWORD="N_LIMIT">
!<Tip> Maximum number of trace anomalies to be printed (VERBOSE only). </Tip>
! Default = 1000
! Allowed = 1-20000
! This option is only used when REPORT=VERBOSE.
!</Help>
!
!<Help KEYWORD="GLOB_NAME">
!<Tip> List of global parameters that are explicitly named. </Tip>
! Default = -
! Allowed = char
! The test mode and tolerance may be individually controlled for
! any global parameter that is explicitly named.
! Checks for globals not listed use GLOB_DEF_MODE and GLOB_DEF_TOL.
!</Help>
!
!<Help KEYWORD="GLOB_MODE">
!<Tip> Test mode for explicitly named global parameters. </Tip>
! Default = SAME
! Allowed = D[EFAULT] check global using GLOB_DEF_MODE and GLOB_DEF_TOL.
! Allowed = I[GNORE]  PASS all values, do not test.
! Allowed = E[QUAL]   PASS iff all bits are identical, otherwise FAIL.
! Allowed = S[AME]    PASS iff floats are approximately equal.
! Allowed = A[BS]     PASS iff absolute difference is < GLOB_TOL
! Allowed = R[EL]     PASS iff relative difference is < GLOB_TOL
! EQUAL is most appropriate for globals with whole integer values
! and SAME is most appropriate for floating point values.
!</Help>
!
!<Help KEYWORD="GLOB_TOL">
!<Tip> Numerical tolerance, used when GLOB_MODE is ABS or REL. </Tip>
! Default = 0.10 
! Allowed = greater than or equal to zero required.
! If GLOB_MODE=ABS then PASS iff absolute value of (INPUT-TEST)
! is less than GLOB_TOL.
! If GLOB_MODE=REL then PASS iff absolute value of (INPUT-TEST)/(INPUT+TEST)
! is less than GLOB_TOL, where (INPUT+TEST) is non-zero.
!
! GLOB_TOL is only used when GLOB_MODE is ABS or REL for the entry.
!</Help>
!
!<Help KEYWORD="GLOB_DEF_MODE">
!<Tip> Default test mode for global parameters. </Tip>
! Default = SAME
! Allowed = I[GNORE]  PASS all values, do not test.
! Allowed = E[QUAL]   PASS iff all bits are identical, otherwise FAIL.
! Allowed = S[AME]    PASS iff floats are approximately equal.
! Allowed = A[BS]     PASS iff absolute difference is < GLOB_DEF_TOL
! Allowed = R[EL]     PASS iff relative difference is < GLOB_DEF_TOL
! EQUAL is most appropriate for globals with whole integer values
! and SAME is most appropriate for floating point values.
!
! The "global default mode" is used for any global parameter not explicitly
! mentioned in the GLOB_NAME list and for named parameters that have
! GLOB_MODE=DEFAULT.
!</Help>
!
!<Help KEYWORD="GLOB_DEF_TOL">
!<Tip> Numerical tolerance, used when GLOB_DEF_MODE is ABS or REL. </Tip>
! Default = 0.10 
! Allowed = greater than or equal to zero required.
! If GLOB_MODE=ABS then PASS iff absolute value of (INPUT-TEST)
! is less than GLOB_TOL.
! If GLOB_MODE=REL then PASS iff absolute value of (INPUT-TEST)/(INPUT+TEST)
! is less than GLOB_TOL, where (INPUT+TEST) is non-zero.
!
! GLOB_DEF_TOL is only used when GLOB_DEF_MODE is ABS or REL.
!</Help>
!
!
!<Help KEYWORD="HDR_NUM">
!<Tip> List of header words that are explicitly named. </Tip>
! Default = -
! Allowed = 1 - NWIH
! The test mode and tolerance may be individually controlled for
! any header word that is explicitly named.
! Checks for headers not listed use HDR_DEF_MODE and HDR_DEF_TOL.
!</Help>
!
!<Help KEYWORD="HDR_MODE">
!<Tip> Test mode for explicitly listed header words. </Tip>
! Default = SAME
! Allowed = D[EFAULT] check header using HDR_DEF_MODE and HDR_DEF_TOL.
! Allowed = I[GNORE]  PASS all values, do not test.
! Allowed = E[QUAL]   PASS iff all bits are identical, otherwise FAIL.
! Allowed = S[AME]    PASS iff floats are approximately equal.
! Allowed = A[BS]     PASS iff absolute difference is < HDR_TOL
! Allowed = R[EL]     PASS iff relative difference is < HDR_TOL
! EQUAL is most appropriate for headers with whole integer values
! and SAME is most appropriate for floating point values.
!</Help>
!
!<Help KEYWORD="HDR_TOL">
!<Tip> Numerical tolerance, used when HDR_MODE is ABS or REL. </Tip>
! Default = 0.10 
! Allowed = greater than or equal to zero required.
! If HDR_MODE=ABS then PASS iff absolute value of (INPUT-TEST)
! is less than HDR_TOL.
! If HDR_MODE=REL then PASS iff absolute value of (INPUT-TEST)/(INPUT+TEST)
! is less than HDR_TOL, where (INPUT+TEST) is non-zero.
!
! HDR_TOL is only used when HDR_MODE is ABS or REL for the entry.
!</Help>
!
!<Help KEYWORD="HDR_DEF_MODE">
!<Tip> Default test mode for header words. </Tip>
! Default = SAME
! Allowed = I[GNORE]  PASS all values, do not test.
! Allowed = E[QUAL]   PASS iff all bits are identical, otherwise FAIL.
! Allowed = S[AME]    PASS iff floats are approximately equal.
! Allowed = A[BS]     PASS iff absolute difference is < HDR_DEF_TOL
! Allowed = R[EL]     PASS iff relative difference is < HDR_DEF_TOL
! EQUAL is most appropriate for headers with whole integer values
! and SAME is most appropriate for floating point values.
!
! The "header default mode" is used for any header word not explicitly
! mentioned in the HDR_NUM list and for named headers that have
! HDR_MODE=DEFAULT.
!</Help>
!
!<Help KEYWORD="HDR_DEF_TOL">
!<Tip> Numerical tolerance, used when HDR_DEF_MODE is ABS or REL. </Tip>
! Default = 0.10 
! Allowed = greater than or equal to zero required.
! If HDR_MODE=ABS then PASS iff absolute value of (INPUT-TEST)
! is less than HDR_TOL.
! If HDR_MODE=REL then PASS iff absolute value of (INPUT-TEST)/(INPUT+TEST)
! is less than HDR_TOL, where (INPUT+TEST) is non-zero.
!
! HDR_DEF_TOL is only used when HDR_DEF_MODE is ABS or REL.
!</Help>
!
!<Help KEYWORD="SMP_MODE">
!<Tip> Test mode for trace samples. </Tip>
! Default = SAME
! Allowed = I[GNORE  PASS all values, do not test.
! Allowed = E[QUAL]   PASS iff all bits are identical, otherwise FAIL.
! Allowed = S[AME]    PASS iff floats are approximately equal.
! Allowed = A[BS]     PASS iff absolute difference is < SMP_TOL
! Allowed = R[EL]     PASS iff relative difference is < SMP_TOL
! SAME is most appropriate for trace samples (floating point values).
!</Help>
!
!<Help KEYWORD="SMP_TOL">
!<Tip> Numerical tolerance, used when SMP_MODE is ABS or REL. </Tip>
! Default = 0.10 
! Allowed = greater than or equal to zero required.
! If SMP_MODE=ABS then PASS iff absolute value of (INPUT-TEST)
! is less than SMP_TOL.
! If SMP_MODE=REL then PASS iff absolute value of (INPUT-TEST)/(INPUT+TEST)
! is less than SMP_TOL, where (INPUT+TEST) is non-zero.
!
! SMP_TOL is only used when SMP_MODE is ABS or REL for the entry.
!</Help>
!
!<Help KEYWORD="SMP_0_EPS_STATE">
!<Tip> Sample Zero Epsilon State, either ON or OFF. </Tip>
! Default = OFF  
! Allowed = OFF   samples close to zero are not treated differently
! Allowed = ON    samples close to zero are considered zero
!
! If two samples are close to zero consider them to be zero,
! and therefore equal to each other.
!</Help>
!
!<Help KEYWORD="SMP_0_EPS">
!<Tip> Numerical Sample Zero Epsilon Value, samples within epsilon of zero are considered zero . </Tip>
! Default = 0.000001
! Allowed = greater than or equal to zero required.
!
! If two samples are close to zero consider them to be zero,
! and therefore equal to each other.
!</Help>
!</HelpSection>
!
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module ezcheck_module
      use pc_module
      use named_constants_module
      use pathcheck_module
      use pathchoose_module
      use mem_module
      use mth_module
      use trcio_module
      use cpsio_module
      use getlun_module
      implicit none
      private
      public :: ezcheck_create
      public :: ezcheck_initialize
      public :: ezcheck_update
      public :: ezcheck_delete
!<execute_only>
      public :: ezcheck            ! main execution (trace processing) routine.
      public :: ezcheck_wrapup
!</execute_only>


      character(len=100),public,save :: ezcheck_IDENT = &
'$Id: ezcheck.f90,v 1.12 2007/01/03 14:01:38 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      ! Maximum number of characters in one print line
      integer, parameter :: MAX_LINE = 132

      ! Maximum number of characters in one global name
      integer, parameter :: MAX_NAME = 18 !!! PC_LENGTH !!! = 160

      ! Maximum number of characters in one global scalar value
      integer, parameter :: MAX_VALUE = 18 !!! PC_LENGTH !!! = 160

      type,public :: ezcheck_struct              
        private
        logical                    :: skip_wrapup      ! wrapup flag.

        character(len=FILENAME_LENGTH) :: test_data    ! process parameter
        character(len=8)           :: out_data         ! process parameters.
        character(len=7)           :: report           ! process parameters.
        integer                    :: n_limit          ! process parameters.
        character(len=MAX_NAME),dimension(:),pointer :: glob_name
        character(len=7),dimension(:),pointer  :: glob_mode
        real,dimension(:),pointer  :: glob_tol         ! process parameters.
        character(len=7)           :: glob_def_mode    ! process parameters.
        real                       :: glob_def_tol     ! process parameters.
        integer,dimension(:),pointer :: hdr_num        ! process parameters.
        character(len=7),dimension(:),pointer  :: hdr_mode
        real,dimension(:),pointer  :: hdr_tol          ! process parameters.
        character(len=7)           :: hdr_def_mode     ! process parameters.
        real                       :: hdr_def_tol      ! process parameters.
        character(len=7)           :: smp_mode         ! process parameters.
        real                       :: smp_tol          ! process parameters.

        ! jpa+
        character(len=7)           :: smp_0_eps_state  ! process parameters.
        real                       :: smp_0_eps        ! process parameters.
        ! jpa-

        integer                    :: ndpt_job         ! global parameters.
        integer                    :: nwih_job         ! global parameters.
        logical                    :: gathered_job     ! global parameters.

        integer                    :: ndpt_tst         ! global parameters.
        integer                    :: nwih_tst         ! global parameters.

        integer                    :: nwih_min         ! global parameters.

        integer                    :: glob_name_cnt    ! derived parameters.
        integer                    :: hdr_num_cnt      ! derived parameters.

        integer                                :: global_count
        character(len=MAX_NAME),dimension(:),pointer :: global_name
        integer,dimension(:),pointer           :: global_mode
        real,dimension(:),pointer              :: global_tolerance
        character(len=14),dimension(:),pointer :: global_result
        integer                                :: global_summary
        integer                                :: header_count
        integer,dimension(:),pointer           :: header_mode
        real,dimension(:),pointer              :: header_tolerance
        integer,dimension(:),pointer           :: header_result
        logical,dimension(:),pointer           :: header_explicit
        integer                                :: header_icount
        integer,dimension(:),pointer           :: header_icheck
        integer                                :: header_summary
        integer,dimension(:),pointer           :: sample_mask
        integer,dimension(:),pointer           :: sample_result
        integer                                :: sample_summary
        integer                                :: trace_summary
        integer                                :: verbose_summary
        integer,dimension(:),pointer           :: verbose_headers

        type(trcio_struct),pointer             :: trcio_file
        integer                                :: dump_lun
        double precision,dimension(:),pointer  :: tst_hdr
        real,dimension(:),pointer              :: tst_smp
        double precision,dimension(:),pointer  :: tmp_hdr
        real,dimension(:),pointer              :: tmp_smp
        type(pathchoose_struct),pointer        :: pathchoose
        integer                                :: job_gather_cnt
        integer                                :: job_trc_cnt
        integer                                :: com_trc_cnt
        integer                                :: tst_trc_cnt
        integer                                :: tst_trc_max
        logical                                :: job_eod
        logical                                :: tst_eod

      end type ezcheck_struct

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(ezcheck_struct),pointer,save :: object      ! needed for traps.

      character(len=8),dimension(5),parameter :: out_data_options = &
        (/'INPUT   ','TEST    ','DIFF    ','MEAN    ','VARIANCE'/)

      character(len=7),dimension(3),parameter :: report_options = &
        (/'TERSE  ','NORMAL ','VERBOSE'/)

      character(len=7),dimension(6),parameter :: glob_mode_options = &
        (/'DEFAULT','IGNORE ','EQUAL  ','SAME   ','ABS    ','REL    '/)
      !!!    N/A       1         2         3        4         5
      !!! canonical number associated with global_mode

      character(len=7),dimension(5),parameter :: glob_def_mode_options = &
        (/          'IGNORE ','EQUAL  ','SAME   ','ABS    ','REL    '/)
      !!!              1         2         3        4         5
      !!! canonical number associated with global_mode

      character(len=7),dimension(6),parameter :: hdr_mode_options = &
        (/'DEFAULT','IGNORE ','EQUAL  ','SAME   ','ABS    ','REL    '/)
      !!!    N/A       1         2         3        4         5
      !!! canonical number associated with header_mode

      character(len=7),dimension(5),parameter :: hdr_def_mode_options = &
        (/          'IGNORE ','EQUAL  ','SAME   ','ABS    ','REL    '/)
      !!!              1         2         3        4         5
      !!! canonical number associated with header_mode

      character(len=7),dimension(5),parameter :: samp_mode_options = &
        (/          'IGNORE ','EQUAL  ','SAME   ','ABS    ','REL    '/)

      character(len=3),dimension(2),parameter :: samp_0_eps_states = &
        (/          'ON ','OFF'/)

      integer :: print_lun = 0             ! state variable
                                           ! = pc_get_lun()

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine ezcheck_create (obj)
      implicit none
      type(ezcheck_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify(obj%glob_name)
      nullify(obj%glob_mode)
      nullify(obj%glob_tol)
      nullify(obj%hdr_num)
      nullify(obj%hdr_mode)
      nullify(obj%hdr_tol)
      nullify(obj%global_name)
      nullify(obj%global_mode)
      nullify(obj%global_tolerance)
      nullify(obj%global_result)
      nullify(obj%header_mode)
      nullify(obj%header_tolerance)
      nullify(obj%header_result)
      nullify(obj%header_explicit)
      nullify(obj%header_icheck)
      nullify(obj%sample_mask)
      nullify(obj%sample_result)
      nullify(obj%verbose_headers)
      nullify(obj%trcio_file)
      nullify(obj%tst_hdr)
      nullify(obj%tst_smp)
      nullify(obj%tmp_hdr)
      nullify(obj%tmp_smp)
      nullify(obj%pathchoose)

      call pathchoose_create(obj%pathchoose, 'test_data', 'trc')

      call ezcheck_initialize (obj)
      return
      end subroutine ezcheck_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine ezcheck_delete (obj)
      implicit none
      type(ezcheck_struct),pointer :: obj       ! arguments

      integer status

!<execute_only>
      call ezcheck_wrapup (obj)
!</execute_only>

      call mem_free(obj%glob_name)
      call mem_free(obj%glob_mode)
      call mem_free(obj%glob_tol)
      call mem_free(obj%hdr_num)
      call mem_free(obj%hdr_mode)
      call mem_free(obj%hdr_tol)
      call mem_free(obj%global_name)
      call mem_free(obj%global_mode)
      call mem_free(obj%global_tolerance)
      call mem_free(obj%global_result)
      call mem_free(obj%header_mode)
      call mem_free(obj%header_tolerance)
      call mem_free(obj%header_result)
      call mem_free(obj%header_explicit)
      call mem_free(obj%header_icheck)
      call mem_free(obj%sample_mask)
      call mem_free(obj%sample_result)
      call mem_free(obj%verbose_headers)

      if(associated(obj%trcio_file)) status = trcio_close(obj%trcio_file)

      if(associated(obj%pathchoose)) call pathchoose_delete(obj%pathchoose)

      call mem_free(obj%tst_hdr)
      call mem_free(obj%tst_smp)
      call mem_free(obj%tmp_hdr)
      call mem_free(obj%tmp_smp)

      deallocate(obj)
      return
      end subroutine ezcheck_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine ezcheck_initialize (obj)
      implicit none
      type(ezcheck_struct),intent(inout) :: obj       ! arguments

      call pc_get_global('ndpt', obj%ndpt_job)
      call pc_get_global('nwih', obj%nwih_job)
      call pc_get_global('gathered', obj%gathered_job)

      obj%ndpt_tst = 0
      obj%nwih_tst = 0
      obj%nwih_min = 0

      obj%test_data = PATHCHECK_EMPTY
      obj%out_data = 'INPUT'
      obj%report = 'NORMAL'
      obj%n_limit = 1000

      obj%glob_name_cnt = 7

      call mem_alloc(obj%glob_name, obj%glob_name_cnt)
      call mem_alloc(obj%glob_mode, obj%glob_name_cnt)
      call mem_alloc(obj%glob_tol, obj%glob_name_cnt)

      obj%glob_name(1) = 'NUMTR'
      obj%glob_mode(1) = 'IGNORE'
      obj%glob_tol(1) = 0.10

      obj%glob_name(2) = 'GATHERED'
      obj%glob_mode(2) = 'IGNORE'
      obj%glob_tol(2) = 0.10

      obj%glob_name(3) = 'NWIH'
      obj%glob_mode(3) = 'EQUAL'
      obj%glob_tol(3) = 0.10

      obj%glob_name(4) = 'NDPT'
      obj%glob_mode(4) = 'EQUAL'
      obj%glob_tol(4) = 0.10

      obj%glob_name(5) = 'TSTRT'
      obj%glob_mode(5) = 'SAME'
      obj%glob_tol(5) = 0.10

      obj%glob_name(6) = 'DT'
      obj%glob_mode(6) = 'SAME'
      obj%glob_tol(6) = 0.10

      obj%glob_name(7) = 'GRID'
      obj%glob_mode(7) = 'SAME'
      obj%glob_tol(7) = 0.10

      obj%glob_def_mode = 'SAME'
      obj%glob_def_tol = 0.10

      obj%hdr_num_cnt = 3

      call mem_alloc(obj%hdr_num, obj%hdr_num_cnt)
      call mem_alloc(obj%hdr_mode, obj%hdr_num_cnt)
      call mem_alloc(obj%hdr_tol, obj%hdr_num_cnt)

      obj%hdr_num(1) = HDR_SEQUENCE         ! HW 1
      obj%hdr_mode(1) = 'EQUAL'
      obj%hdr_tol(1) = 0.10

      obj%hdr_num(2) = HDR_CURRENT_GROUP    ! HW 3
      obj%hdr_mode(2) = 'EQUAL'
      obj%hdr_tol(2) = 0.10

      obj%hdr_num(3) = HDR_CURRENT_CHANNEL  ! HW 4
      obj%hdr_mode(3) = 'EQUAL'
      obj%hdr_tol(3) = 0.10

!??? since the default is 'SAME', why bother explicitly naming these
!???  obj%hdr_num(4) = HDR_TOP_MUTE         ! HW 2
!???  obj%hdr_mode(4) = 'SAME'
!???  obj%hdr_tol(4) = 0.10

!???  obj%hdr_num(5) = HDR_BOTTOM+MUTE      ! HW 64
!???  obj%hdr_mode(5) = 'SAME'
!???  obj%hdr_tol(5) = 0.10

!???  obj%hdr_num(6) = HDR_LAV              ! HW 25
!???  obj%hdr_mode(6) = 'SAME'
!???  obj%hdr_tol(6) = 0.10

      obj%hdr_def_mode = 'SAME'
      obj%hdr_def_tol = 0.10

      obj%smp_mode = 'SAME'
      obj%smp_tol = 0.10

      ! jpa+
      obj%smp_0_eps_state = 'OFF'
      obj%smp_0_eps = 0.000001
      ! jpa-

      obj%global_count = 0
      obj%global_summary = 0
      obj%header_count = 0
      obj%header_icount = 0
      obj%header_summary = 0
      obj%sample_summary = 0
      obj%trace_summary = 0
      obj%verbose_summary = 0
      obj%dump_lun = -1
      obj%job_gather_cnt = 0
      obj%job_trc_cnt = 0
      obj%com_trc_cnt = 0
      obj%tst_trc_cnt = 0
      obj%tst_trc_max = 0
      obj%job_eod = .false.
      obj%tst_eod = .false.

      call ezcheck_update (obj)
      return
      end subroutine ezcheck_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine ezcheck_update (obj)
      implicit none
      type(ezcheck_struct),intent(inout),target :: obj             ! arguments

      integer :: state
      logical :: verify
      integer :: status
      integer :: glob_name_cnt2, glob_name_cnt3
      integer :: hdr_num_cnt2, hdr_num_cnt3
      integer :: do_glob_name, do_hdr_num

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      state = pc_get_update_state()

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        verify = .true.
      else
        verify = .false.
      end if

      if (pathchoose_update(obj%pathchoose, obj%test_data)) return

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_register_array_names ("glob_name_arrayset", (/  &
                                    "glob_name",              &
                                    "glob_mode",              &
                                    "glob_tol " /))

      call pc_register_array_names ("hdr_num_arrayset", (/  &
                                    "hdr_num ",             &
                                    "hdr_mode",             &
                                    "hdr_tol " /))

      call pc_get('test_data', obj%test_data)

      call pc_get('out_data', obj%out_data)
      call string_to_upper(obj%out_data)

      call pc_get('report', obj%report)
      call string_to_upper(obj%report)

      call pc_get('n_limit', obj%n_limit)

      glob_name_cnt2 = obj%glob_name_cnt
      glob_name_cnt3 = obj%glob_name_cnt
      call pc_alloc('glob_name', obj%glob_name, obj%glob_name_cnt)
      call pc_alloc('glob_mode', obj%glob_mode, glob_name_cnt2)
      call pc_alloc('glob_tol', obj%glob_tol, glob_name_cnt3)

      call pc_get('glob_def_mode', obj%glob_def_mode)
      call string_to_upper(obj%glob_def_mode)

      call pc_get('glob_def_tol', obj%glob_def_tol)

      hdr_num_cnt2 = obj%hdr_num_cnt
      hdr_num_cnt3 = obj%hdr_num_cnt
      call pc_alloc('hdr_num', obj%hdr_num, obj%hdr_num_cnt)
      call pc_alloc('hdr_mode', obj%hdr_mode, hdr_num_cnt2)
      call pc_alloc('hdr_tol', obj%hdr_tol, hdr_num_cnt3)

      call pc_get('hdr_def_mode', obj%hdr_def_mode)
      call string_to_upper(obj%hdr_def_mode)

      call pc_get('hdr_def_tol', obj%hdr_def_tol)

      call pc_get('smp_mode', obj%smp_mode)
      call string_to_upper(obj%smp_mode)

      call pc_get('smp_tol', obj%smp_tol)

      ! jpa+
      call pc_get('smp_0_eps_state', obj%smp_0_eps_state)
      call string_to_upper(obj%smp_0_eps_state)
      call pc_get('smp_0_eps', obj%smp_0_eps)
      ! jpa-

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(verify .or. pc_verify_scalar('test_data')) then
        call pathcheck('test_data', obj%test_data, ext='trc', &
          required=.true., status=status, show=PATHCHECK_INFO_INPUT)

        if(status /= PATHCHECK_VALID) then
          call pc_error('valid TEST_DATA is required')
        end if
      end if

      if(all(out_data_options /= obj%out_data)) then
        call pc_error( &
          'Invalid OUT_DATA value. Valid values are INPUT, TEST, DIFF, ' // &
          'MEAN and VARIANCE')
        obj%out_data = 'INPUT'
      end if

      if(all(report_options /= obj%report)) then
        call pc_error( &
          'Invalid REPORT value. Valid values are TERSE, NORMAL, VERBOSE')
        obj%report = 'NORMAL'
      end if

      if(obj%n_limit < 1 .or. obj%n_limit > 20000) then
        call pc_error('Invalid N_LIMIT. Must be >= 1 and <= 20000')
        obj%n_limit = 1000
      end if

      if(obj%glob_name_cnt /= glob_name_cnt2 .or. &
         obj%glob_name_cnt /= glob_name_cnt3) then
        call pc_error('Arrays not linked, GLOB_NAME, GLOB_MODE, GLOB_TOL')
        obj%glob_name_cnt = &
          min(obj%glob_name_cnt, glob_name_cnt2, glob_name_cnt3)
      end if

      if(all(glob_def_mode_options /= obj%glob_def_mode)) then
        call pc_error( &
          'Invalid GLOB_DEF_MODE value.  ' // &
          'Valid values are IGNORE, EQUAL, SAME, ABS and REL')
        obj%glob_def_mode = 'SAME'
      end if

      if(obj%glob_def_tol < 0.0) then
        call pc_error( &
          'Invalid GLOB_DEF_TOL value.  ' // &
          'Negative not allowed')
        obj%glob_def_tol = 0.10
      end if

      do do_glob_name = 1, obj%glob_name_cnt
        if(obj%glob_mode(do_glob_name) == CNIL) then
          obj%glob_mode(do_glob_name) = obj%glob_def_mode
        else
          call string_to_upper(obj%glob_mode(do_glob_name))

          select case(obj%glob_mode(do_glob_name)(1:1))
          case ('D')
            obj%glob_mode(do_glob_name) = 'DEFAULT'
          case ('I')
            obj%glob_mode(do_glob_name) = 'IGNORE'
          case ('E')
            obj%glob_mode(do_glob_name) = 'EQUAL'
          case ('S')
            obj%glob_mode(do_glob_name) = 'SAME'
          case ('A')
            obj%glob_mode(do_glob_name) = 'ABS'
          case ('R')
            obj%glob_mode(do_glob_name) = 'REL'
          case default
            call pc_error( &
              'Invalid GLOB_MODE(', do_glob_name, ') value.  ' // &
              'Valid values are DEFAULT, IGNORE, EQUAL, SAME, ABS and REL')
            obj%glob_mode(do_glob_name) = 'SAME'
          end select
        end if
      end do

      if(verify .or. pc_verify_arrayset('glob_name_arrayset')) then
        do do_glob_name = 1, obj%glob_name_cnt
          call string_to_upper(obj%glob_name(do_glob_name))

          if(obj%glob_name(do_glob_name) == CNIL) then
            call pc_error( &
              'Invalid GLOB_NAME(', do_glob_name, ') value.  ' // &
              'Non-blank name required (or delete the entry)')
          end if

          if(obj%glob_tol(do_glob_name) == FNIL) then
            obj%glob_tol(do_glob_name) = obj%glob_def_tol
          else if(obj%glob_tol(do_glob_name) < 0.0) then
            call pc_error( &
              'Invalid GLOB_TOL(', do_glob_name, ') value.  ' // &
              'Negative not allowed')
            obj%glob_tol(do_glob_name) = 0.10
          end if
        end do
      end if

      if(obj%hdr_num_cnt /= hdr_num_cnt2 .or. &
         obj%hdr_num_cnt /= hdr_num_cnt3) then
        call pc_error('Arrays not linked, HDR_NUM, HDR_MODE, HDR_TOL')
        obj%hdr_num_cnt = &
          min(obj%hdr_num_cnt, hdr_num_cnt2, hdr_num_cnt3)
      end if

      if(all(hdr_def_mode_options /= obj%hdr_def_mode)) then
        call pc_error( &
          'Invalid HDR_DEF_MODE value.  ' // &
          'Valid values are IGNORE, EQUAL, SAME, ABS and REL')
        obj%hdr_def_mode = 'SAME'
      end if

      if(obj%hdr_def_tol < 0.0) then
        call pc_error( &
          'Invalid HDR_DEF_TOL value.  ' // &
          'Negative not allowed')
        obj%hdr_def_tol = 0.10
      end if

      do do_hdr_num = 1, obj%hdr_num_cnt
        if(obj%hdr_mode(do_hdr_num) == CNIL) then
          obj%hdr_mode(do_hdr_num) = obj%hdr_def_mode
        else
          call string_to_upper(obj%hdr_mode(do_hdr_num))

          select case(obj%hdr_mode(do_hdr_num)(1:1))
          case ('D')
            obj%hdr_mode(do_hdr_num) = 'DEFAULT'
          case ('I')
            obj%hdr_mode(do_hdr_num) = 'IGNORE'
          case ('E')
            obj%hdr_mode(do_hdr_num) = 'EQUAL'
          case ('S')
            obj%hdr_mode(do_hdr_num) = 'SAME'
          case ('A')
            obj%hdr_mode(do_hdr_num) = 'ABS'
          case ('R')
            obj%hdr_mode(do_hdr_num) = 'REL'
          case default
            call pc_error( &
              'Invalid HDR_MODE(', do_hdr_num, ') value.  ' // &
              'Valid values are DEFAULT, IGNORE, EQUAL, SAME, ABS and REL')
            obj%hdr_mode(do_hdr_num) = 'SAME'
          end select
        end if
      end do

      if(verify .or. pc_verify_arrayset('hdr_name_arrayset')) then
        do do_hdr_num = 1, obj%hdr_num_cnt
          if(obj%hdr_num(do_hdr_num) < 1 .or. &
             obj%hdr_num(do_hdr_num) == INIL) then
            call pc_error( &
              'Invalid HDR_NUM(', do_hdr_num, ') value.  ' // &
              'Greater than zero required (or delete the entry)')
          end if

          if(obj%hdr_tol(do_hdr_num) == FNIL) then
            obj%hdr_tol(do_hdr_num) = obj%hdr_def_tol

          else if(obj%hdr_tol(do_hdr_num) < 0.0) then
            call pc_error( &
              'Invalid HDR_TOL(', do_hdr_num, ') value.  ' // &
              'Negative not allowed')
            obj%hdr_tol(do_hdr_num) = 0.10
          end if
        end do
      end if

      if(all(samp_mode_options /= obj%smp_mode)) then
        call pc_error( &
          'Invalid SMP_MODE value.  ' // &
          'Valid values are IGNORE, EQUAL, SAME, ABS and REL')
        obj%smp_mode = 'SAME'
      end if

      if(obj%smp_tol < 0.0) then
        call pc_error( &
          'Invalid SMP_TOL value.  ' // &
          'Negative not allowed')
        obj%smp_tol = 0.10
      end if

      ! jpa+
      if(all(samp_0_eps_states /= obj%smp_0_eps_state)) then
        call pc_error( &
          'Invalid SMP_0_EPS_STATE value.  ' // &
          'Valid values are ON or OFF')
        obj%smp_0_eps_state = 'OFF'
      end if

      if(obj%smp_0_eps < 0.0) then
        call pc_error( &
          'Invalid SMP_0_EPS value.  ' // &
          'Negative not allowed')
        obj%smp_0_eps = 0.000001
      end if
      ! jpa-

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put('test_data', obj%test_data)

      call pc_put_options_field('out_data', out_data_options, 5)
      call pc_put('out_data', obj%out_data)

      call pc_put_options_field('report', report_options, 3)
      call pc_put('report', obj%report)

      call pc_put('n_limit', obj%n_limit)

      call pc_put('glob_name', obj%glob_name, obj%glob_name_cnt)
!???  call pc_put_options_array('glob_mode', glob_mode_options, 6)
      call pc_put('glob_mode', obj%glob_mode, obj%glob_name_cnt)
      call pc_put('glob_tol', obj%glob_tol, obj%glob_name_cnt)

      call pc_put_options_field('glob_def_mode', glob_def_mode_options, 5)
      call pc_put('glob_def_mode', obj%glob_def_mode)

      call pc_put('glob_def_tol', obj%glob_def_tol)

      call pc_put('hdr_num', obj%hdr_num, obj%hdr_num_cnt)
!???  call pc_put_options_array('hdr_mode', hdr_mode_options, 6)
      call pc_put('hdr_mode', obj%hdr_mode, obj%hdr_num_cnt)
      call pc_put('hdr_tol', obj%hdr_tol, obj%hdr_num_cnt)

      call pc_put_options_field('hdr_def_mode', hdr_def_mode_options, 5)
      call pc_put('hdr_def_mode', obj%hdr_def_mode)

      call pc_put('hdr_def_tol', obj%hdr_def_tol)

      call pc_put_options_field('smp_mode', samp_mode_options, 5)
      call pc_put('smp_mode', obj%smp_mode)

      call pc_put('smp_tol', obj%smp_tol)

      ! jpa+
      call pc_put_options_field('smp_0_eps_state', samp_0_eps_states, 2)
      call pc_put('smp_0_eps_state', obj%smp_0_eps_state)
      call pc_put('smp_0_eps', obj%smp_0_eps)
      ! jpa-

      call pc_put_control('need_request',.true.)
      call pc_put_control('need_label',.true.)

      if(obj%out_data == 'TEST') then
        call pc_put_global('gathered',.false.)
      end if

      if(obj%out_data == 'DIFF' .or. &
         obj%out_data == 'MEAN' .or. &
         obj%out_data == 'VARIANCE') then
        call pc_put_global  ('gathered'    , .false.)
      end if

      if(obj%report == 'VERBOSE') then
        call pc_put_sensitive_field_flag('n_limit', .true.)
      else
        call pc_put_sensitive_field_flag('n_limit', .false.)
      end if

      if(obj%glob_def_mode == 'ABS' .or. &
         obj%glob_def_mode == 'REL') then
        call pc_put_sensitive_field_flag('glob_def_tol', .true.)
      else
        call pc_put_sensitive_field_flag('glob_def_tol', .false.)
      end if

      if(obj%hdr_def_mode == 'ABS' .or. &
         obj%hdr_def_mode == 'REL') then
        call pc_put_sensitive_field_flag('hdr_def_tol', .true.)
      else
        call pc_put_sensitive_field_flag('hdr_def_tol', .false.)
      end if

      if(obj%smp_mode == 'ABS' .or. &
         obj%smp_mode == 'REL') then
        call pc_put_sensitive_field_flag('smp_tol', .true.)
      else
        call pc_put_sensitive_field_flag('smp_tol', .false.)
      end if

      ! jpa+
      if(obj%smp_0_eps_state == 'ON') then
        call pc_put_sensitive_field_flag('smp_0_eps', .true.)
      else
        call pc_put_sensitive_field_flag('smp_0_eps', .false.)
      end if
      ! jpa-

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      print_lun = pc_get_lun()

      ! attempt to get logical unit for temporary dump file
      call getlun(obj%dump_lun, status)

      if (status == 0) then
         open(unit = obj%dump_lun, action = 'READWRITE', iostat = status, &
           status = 'SCRATCH', recl = MAX_LINE)
         if(status /= 0) then
           call pc_error('iostat=',status,', opening scratch dump file')
           return
         end if
      else
        call pc_error('getlun failed, can not open scratch dump file')
        return
      end if

      obj%trcio_file => trcio_open(obj%test_data, 'r')

      if(.not. associated(obj%trcio_file)) then
        call pc_error("trcio_open: error, test_data= " // obj%test_data)
        return
      end if

      obj%ndpt_tst = obj%trcio_file%num_values
      obj%nwih_tst = obj%trcio_file%nwih
      obj%tst_trc_max = obj%trcio_file%num_traces

      obj%nwih_min = min(obj%nwih_job, obj%nwih_tst)

      if(obj%ndpt_job /= obj%ndpt_tst) then
        call pc_error('NDPT in job input and TEST_DATA disagree')
      end if

      call ezcheck_global_setup(obj)

      call ezcheck_header_setup(obj)

      call ezcheck_sample_setup(obj)

      ! allocate one header and samples array for TEST_DATA input
      call mem_alloc(obj%tst_hdr, obj%nwih_tst)
      call mem_alloc(obj%tst_smp, obj%ndpt_tst)

      ! allocate one header and samples array for intermediate results
      call mem_alloc(obj%tmp_hdr, obj%nwih_tst)
      call mem_alloc(obj%tmp_smp, obj%ndpt_tst)

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine ezcheck_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine ezcheck (obj,ntr,hd,tr)
      implicit none
      type(ezcheck_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

      integer :: nwih_min, nwih, ndpt, do_ntr, do_header, do_smp, icheck
      integer :: status, i
      integer :: header_failure
      integer :: sample_failure
      real :: smp_err

      nwih_min = obj%nwih_min
      nwih = obj%nwih_job
      ndpt = obj%ndpt_job

      if(ntr > 0) then
        obj%job_gather_cnt = obj%job_gather_cnt + 1
      else if(ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
        obj%job_eod = .true.
      end if

      if(obj%tst_eod .and. ntr > 0) then
        ! EOF encountered on TEST data, input processing is trivial
        obj%job_trc_cnt = obj%job_trc_cnt + ntr

        if(obj%out_data == 'INPUT') then
          ! zero pad trace headers and output this gather
          hd(nwih_min+1:obj%nwih_job,:ntr) = 0.0
        else
          ! consume input traces but do not output anymore
          ntr = NEED_TRACES
        end if
      else if(obj%job_eod .and. obj%out_data == 'TEST' .and. &
        ntr /= FATAL_ERROR) then
        ! read TEST traces and them pass out
        status = trcio_read_trace(obj%trcio_file,hd(:,1),tr(:,1))

        if(status == TRCIO_OK) then
          ntr = 1
          obj%tst_trc_cnt = obj%tst_trc_cnt + 1
        else
          obj%tst_eod = .true.

          if(status == TRCIO_EOF) then
            ntr = NO_MORE_TRACES
          else
            call pc_error("ezcheck: error reading TEST_DATA")
            ntr = FATAL_ERROR
          end if
        end if
      else if(obj%job_eod .and. ntr /= FATAL_ERROR) then
        ! read all TEST traces and discard them
        do
          status = trcio_read_trace(obj%trcio_file,hd(:,1),tr(:,1))

          if(status == TRCIO_OK) then
            obj%tst_trc_cnt = obj%tst_trc_cnt + 1
          else
            obj%tst_eod = .true.

            if(status == TRCIO_EOF) then
              ntr = NO_MORE_TRACES
            else
              call pc_error("ezcheck: error reading TEST_DATA")
              ntr = FATAL_ERROR
            end if

            exit
          end if
        end do
      else if(ntr > 0) then
        ! read TEST data and compare to JOB input traces
        ntr_loop: &
        do do_ntr = 1, ntr
          ! attempt to read TEST data for comparison to this JOB trace
          obj%job_trc_cnt = obj%job_trc_cnt + 1

          status = trcio_read_trace(obj%trcio_file,obj%tst_hdr,obj%tst_smp)

          if(status == TRCIO_OK) then
            obj%tst_trc_cnt = obj%tst_trc_cnt + 1
          else
            obj%tst_eod = .true.

            if(status /= TRCIO_EOF) then
              call pc_error("ezcheck: error reading TEST_DATA")
              ntr = FATAL_ERROR
            else if(obj%out_data == 'INPUT') then
              ! zero pad trace headers and output this gather
              hd(nwih_min+1:obj%nwih_job,do_ntr:ntr) = 0.0
            else
              if(do_ntr == 1) then
                ! consume input traces but do not output anymore
                ntr = NEED_TRACES
              else
                ! output the traces that have already been compared
                ntr = do_ntr - 1
              end if
            end if

            exit
          end if

          obj%com_trc_cnt = obj%com_trc_cnt + 1

          header_failure = 0

          ! compare headers
          header_loop: &
          do do_header = 1, obj%header_icount
            icheck = obj%header_icheck(do_header)
          
            choose_header_mode: &
            select case(obj%header_mode(icheck))
            case (2)
              ! MODE is EQUAL
              if(hd(icheck,do_ntr) /= obj%tst_hdr(icheck)) then
                header_failure = header_failure + 1
                obj%verbose_headers(header_failure) = do_header
                obj%header_result(icheck) = obj%header_result(icheck) + 1
              end if
            case (3)
              ! MODE is SAME
              if(0 /= mth_compare(hd(icheck,do_ntr),obj%tst_hdr(icheck))) then
                header_failure = header_failure + 1
                obj%verbose_headers(header_failure) = do_header
                obj%header_result(icheck) = obj%header_result(icheck) + 1
              end if
            case (4)
              ! MODE is ABS
              if(abs(hd(icheck,do_ntr) - obj%tst_hdr(icheck)) >= &
                obj%header_tolerance(icheck)) then
                header_failure = header_failure + 1
                obj%verbose_headers(header_failure) = do_header
                obj%header_result(icheck) = obj%header_result(icheck) + 1
              end if
            case (5)
              ! MODE is REL
              if(abs((hd(icheck,do_ntr) - obj%tst_hdr(icheck)) / &
                     (hd(icheck,do_ntr) + obj%tst_hdr(icheck))) >= &
                obj%header_tolerance(icheck)) then
                header_failure = header_failure + 1
                obj%verbose_headers(header_failure) = do_header
                obj%header_result(icheck) = obj%header_result(icheck) + 1
              end if
            end select choose_header_mode
          end do header_loop

          if(header_failure > 0) then
            obj%header_summary = obj%header_summary + 1
          end if

          ! compare samples
          choose_sample_mode: &
          select case(obj%smp_mode(1:1))
          case ('I')  ! IGNORE
            ! no-op
          case ('E')  ! EQUAL
            where(tr(:ndpt,do_ntr) /= obj%tst_smp)
              obj%sample_mask = 1
            else where
              obj%sample_mask = 0
            end where
          case ('S')  ! SAME
            do do_smp = 1, ndpt
              if(0 /= mth_compare(tr(do_smp,do_ntr),obj%tst_smp(do_smp))) then
                obj%sample_mask(do_smp) = 1
              else
                obj%sample_mask(do_smp) = 0
              end if
            end do
          case ('A')  ! ABS
            where(abs(tr(:ndpt,do_ntr) - obj%tst_smp) > obj%smp_tol)
              obj%sample_mask = 1
            else where
              obj%sample_mask = 0
            end where
          case ('R')  ! REL
            obj%tmp_smp = tr(:ndpt,do_ntr) + obj%tst_smp

            where(obj%tmp_smp /= 0.0 .and. &
              (abs(tr(:ndpt,do_ntr) - obj%tst_smp) / obj%tmp_smp) > obj%smp_tol)
                obj%sample_mask = 1
            else where
              obj%sample_mask = 0
            end where
          end select choose_sample_mode

          ! jpa+
          ! If the two samples are close to zero consider them to be zero,
          ! and therefore equal to each other.

          if (obj%smp_0_eps_state == 'ON') then
            do do_smp = 1, ndpt
              if (-obj%smp_0_eps <= tr(do_smp,do_ntr) .and. &
                  tr(do_smp,do_ntr) <= obj%smp_0_eps .and. &
                  -obj%smp_0_eps <= obj%tst_smp(do_smp) .and. &
                  obj%tst_smp(do_smp) <= obj%smp_0_eps) then
                obj%sample_mask(do_smp) = 0
              end if
            end do
          end if
          ! jpa-

          sample_failure = sum(obj%sample_mask)

          if(sample_failure > 0) then
            obj%trace_summary = obj%trace_summary + 1
            obj%sample_result = obj%sample_result + obj%sample_mask
          end if

          if(obj%report == 'VERBOSE' .and. &
             obj%verbose_summary < obj%n_limit .and. &
             (header_failure > 0 .or. sample_failure > 0)) then
            ! write a verbose message (headers or samples failed)
            if(0 == mod(obj%verbose_summary, 10)) then
              ! write column headers after every 10 lines
              write(obj%dump_lun, *) &
                " Failure     SeqTrcNum      Gather" // &
                "     NTR   Trace      Bad-Hdrs  Bad-Smps" // &
                "         Smp_Err"
            end if

            obj%verbose_summary = obj%verbose_summary + 1

            if(header_failure > 0 .and. sample_failure > 0) then
              call ezcheck_smp_err(obj, tr(:,do_ntr), smp_err)

              write(obj%dump_lun, 111) &
                obj%verbose_summary, obj%job_trc_cnt, obj%job_gather_cnt, &
                ntr, do_ntr, header_failure, sample_failure, smp_err

    111       format(2x,i7,2x,i12,2x,i10,2x,i6,2x,i6,2x,i12,2x,i12,2x,g14.6)

              write(obj%dump_lun, 120)
              write(obj%dump_lun, 122) &
                (obj%verbose_headers(i), &
                 hd(obj%verbose_headers(i),do_ntr), &
                 obj%tst_hdr(obj%verbose_headers(i)), &
                  i=1,header_failure)
            else if(header_failure > 0) then
              write(obj%dump_lun, 112) &
                obj%verbose_summary, obj%job_trc_cnt, obj%job_gather_cnt, &
                ntr, do_ntr, header_failure

    112       format(2x,i7,2x,i12,2x,i10,2x,i6,2x,i6,2x,i12,2x,12x,2x,14x)

              write(obj%dump_lun, 120)
              write(obj%dump_lun, 122) &
                (obj%verbose_headers(i), &
                 hd(obj%verbose_headers(i),do_ntr), &
                 obj%tst_hdr(obj%verbose_headers(i)), &
                  i=1,header_failure)
            else if(sample_failure > 0) then
              call ezcheck_smp_err(obj, tr(:,do_ntr), smp_err)

              write(obj%dump_lun, 113) &
                obj%verbose_summary, obj%job_trc_cnt, obj%job_gather_cnt, &
                ntr, do_ntr, sample_failure, smp_err

    113       format(2x,i7,2x,i12,2x,i10,2x,i6,2x,i6,2x,12x,2x,i12,2x,g14.6)
            end if

    120     format(2('  HDR --------JOB--------- --------TEST--------'))
    122     format(2(1x,i4,1x,g20.13,1x,g20.13))
          end if

          choose_out_data: &
          select case(obj%out_data)
          case ('INPUT')
            ! no-op
          case ('TEST')
            hd(1:nwih_min,do_ntr) = obj%tst_hdr(1:nwih_min)
            tr(1:ndpt    ,do_ntr) = obj%tst_smp(1:ndpt)
          case ('DIFF')
            hd(2:nwih_min,do_ntr) = &
              hd(2:nwih_min,do_ntr) - obj%tst_hdr(2:nwih_min)
            tr(1:ndpt,do_ntr) = &
              tr(1:ndpt,do_ntr)     - obj%tst_smp(1:ndpt)
            ! Note: output LAV does NOT represent the trace sample values.
          case ('MEAN')
            hd(2:nwih_min,do_ntr) = 0.5 * &
              (hd(2:nwih_min,do_ntr) + obj%tst_hdr(2:nwih_min))
            tr(1:ndpt,do_ntr) = 0.5 * &
              (tr(1:ndpt,do_ntr)     + obj%tst_smp(1:ndpt))
            ! Note: output LAV does NOT represent the trace sample values.
          case ('VARIANCE')
            ! compute MEAN
            obj%tmp_hdr(2:nwih_min) = 0.5 * &
              (hd(2:nwih_min,do_ntr) + obj%tst_hdr(2:nwih_min))
            obj%tmp_smp(1:ndpt) = 0.5 * &
              (tr(1:ndpt,do_ntr)     + obj%tst_smp(1:ndpt))
            ! compute variance about MEAN
            hd(2:nwih_min,do_ntr) = 0.5 * &
              ((hd(2:nwih_min,do_ntr)   - obj%tmp_hdr(2:nwih_min)) ** 2 + &
               (obj%tst_hdr(2:nwih_min) - obj%tmp_hdr(2:nwih_min)) ** 2)
            tr(2:nwih_min,do_ntr) = 0.5 * &
              ((tr(2:nwih_min,do_ntr)   - obj%tmp_smp(2:nwih_min)) ** 2 + &
               (obj%tst_smp(2:nwih_min) - obj%tmp_smp(2:nwih_min)) ** 2)
            ! Note: output LAV does NOT represent the trace sample values.
          end select choose_out_data

          ! zero pad trace header
          hd(nwih_min+1:obj%nwih_job,ntr) = 0.0
        end do ntr_loop
      end if

      if(ntr == FATAL_ERROR .or. ntr == NO_MORE_TRACES) then
        call ezcheck_wrapup(obj)
      end if

      return
      end subroutine ezcheck

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine ezcheck_wrapup (obj)
      implicit none
      type(ezcheck_struct),intent(inout) :: obj       ! arguments

      integer status, i, j, match
      CHARACTER(LEN=MAX_LINE) :: LINE

      if (obj%skip_wrapup) return

      obj%skip_wrapup = .true.

      obj%sample_summary = sum(obj%sample_result)

      write(print_lun,*)
      write(print_lun,*) '**********************************************'

      if(obj%global_summary /= 0 .or. &
         obj%header_summary /= 0 .or. &
         obj%sample_summary /= 0 .or. &
         obj%job_trc_cnt /= obj%tst_trc_cnt .or. &
         obj%job_trc_cnt /= obj%tst_trc_max) then
        write(print_lun,*) '*** EZCHECK: Regression test summary = FAIL'
        write(print_lun,*) '***     GLOBAL failures=', obj%global_summary
        write(print_lun,*) '***     HEADER failures=', obj%header_summary
        write(print_lun,*) '***     TRACE  failures=', obj%trace_summary
        write(print_lun,*) '***     SAMPLE failures=', obj%sample_summary
      else
        write(print_lun,*) '*** EZCHECK: Regression test summary = PASS'
      end if

      write(print_lun,*) '***     Gathers received =', obj%job_gather_cnt
      write(print_lun,*) '***     Input traces read=', obj%job_trc_cnt
      write(print_lun,*) '***     Test  traces read=', obj%tst_trc_cnt
      write(print_lun,*) '***     Test  traces max =', obj%tst_trc_max
      write(print_lun,*) '***     Traces compared  =', obj%com_trc_cnt

      write(print_lun,*) '**********************************************'
      write(print_lun,*)

      if(obj%report == 'TERSE') return

      write(print_lun,*) 'EZCHECK: GLOBAL synopsis'
      write(print_lun,*) '    default mode= ', obj%glob_def_mode, &
        'tolerance=', obj%glob_def_tol
      write(print_lun,*) '    NAME              MODE        TOLERANCE  RESULT'

      do i = 1, obj%global_count
        write(print_lun,1000) &
          obj%global_name(i), &
          glob_mode_options(1 + obj%global_mode(i)), &
          obj%global_tolerance(i), &
          obj%global_result(i)
 1000   format(7x,a16,2x,a7,2x,g12.3,a)
      end do

      write(print_lun,*)

      write(print_lun,*) 'EZCHECK: HEADER synopsis'
      write(print_lun,*) '    default mode= ', obj%hdr_def_mode, &
        'tolerance=', obj%hdr_def_tol
      write(print_lun,*) '    HDR  MODE        TOLERANCE        FAILURES'

      do i = 1, obj%header_count
        if(.not. obj%header_explicit(i) .and. &
           obj%header_result(i) == 0 .and. &
           obj%hdr_def_mode(1:1) == &
              hdr_mode_options(1 + obj%header_mode(i))(1:1)) then
           ! nothing interesting to report
           cycle
        end if

        if(obj%header_explicit(i)) then
          write(print_lun,2000) &
            i, &
            hdr_mode_options(1 + obj%header_mode(i)), &
            obj%header_tolerance(i), &
            obj%header_result(i)
 2000     format(5x,i3,4x,a7,2x,g12.3,i12)
        else
          write(print_lun,2001) &
            i, &
            hdr_mode_options(1 + obj%header_mode(i)), &
            obj%header_tolerance(i), &
            obj%header_result(i)
 2001     format(5x,i3,4x,a7,2x,g12.3,i12,2x,'Default testing')
        end if
      end do

      write(print_lun,*)

      write(print_lun,*) 'EZCHECK: SAMPLE synopsis'
      write(print_lun,*) '    mode= ', obj%smp_mode, 'tolerance=', obj%smp_tol
      ! jpa+
      write(print_lun,*) '    zero epsilon state= ', obj%smp_0_eps_state, &
      'zero epsilon= ', obj%smp_0_eps
      ! jpa-

      if(obj%sample_summary == 0) then
        write(print_lun,*) '    No sample failures detected'
      else
        write(print_lun,*) '    SAMPLE  ENDING      FAILURES'

        i = 1
        do while(i <= obj%ndpt_job)
          match = obj%sample_result(i)

          do j = i + 1, obj%ndpt_job
            if(match /= obj%sample_result(j)) then
              exit
            end if
          end do

          if(i == j - 1) then
            write(print_lun,3000) i, match
 3000       format(5x,i6,2x,6x,2x,i12)
          else
            write(print_lun,3001) i, j - 1, match
 3001       format(5x,i6,2x,i6,2x,i12)
          end if

          i = j
        end do
      end if

      if(obj%report == 'NORMAL') return

      write(print_lun,*)

      write(print_lun,*) 'EZCHECK: VERBOSE details of trace failures'

      if(obj%dump_lun >= 0) then
        ! copy temporary dump file to standard out

        REWIND obj%dump_lun
        LINE = ' '

        DO
          READ (obj%dump_lun, '(A)', IOSTAT = STATUS) LINE
          IF (STATUS .ne. 0) EXIT
          WRITE(print_lun, '(A)') LINE
        END DO

        close(unit = obj%dump_lun)

        obj%dump_lun = -1
      end if

      write(print_lun,*)


      return
      end subroutine ezcheck_wrapup

!</execute_only>

!!------------------------ ezcheck_smp_err  -----------------------------!!
!!------------------------ ezcheck_smp_err  -----------------------------!!
!!------------------------ ezcheck_smp_err  -----------------------------!!


!<execute_only>

      subroutine ezcheck_smp_err (obj, tr, smp_err)
      implicit none
      type(ezcheck_struct),intent(inout) :: obj          ! arguments
      real             ,intent(in) :: tr(:)            ! arguments

      integer :: mute_top, mute_bot, mute_count
      integer :: ndpt
      real :: max_abs_diff, ave_abs_value, smp_err

! This subroutine is called when REPORT=VERBOSE and some samples failed.
! It computes smp_err which attempts to measure the significance of
! failed samples within the non-mute range defined by the TEST trace.
! If smp_err is much greater than 1.0, one or more samples are very bad.
! If much less than 1.0, differences may be relatively benign.
!
! CAUTION: the algorithm is hard to justify and may be very misleading.
! cbyte should be used to examine statistical variations if unsure.
!
!             max absolute difference of non-muted failing samples
! smp_err = ---------------------------------------------------------
!               average absolute value of non-muted TEST samples
!

      ndpt = obj%ndpt_job

      ! only consider samples within the mute defined by TEST input
      mute_top = max(1,int(obj%tst_hdr(2)))
      mute_bot = min(ndpt,max(mute_top,int(obj%tst_hdr(64))))
      mute_count = mute_bot - mute_top + 1

      ! absolute value of the difference for failed samples
      where(obj%sample_mask(mute_top:mute_bot) == 1)
        obj%tmp_smp(:mute_count) = &
          abs(tr(mute_top:mute_bot) - obj%tst_smp(mute_top:mute_bot))
      else where
        obj%tmp_smp(:mute_count) = 0.0
      end where

      ! maximum absolute value of the difference for failed samples
      max_abs_diff = maxval(obj%tmp_smp(:mute_count))

      if(max_abs_diff == 0.0) then
        smp_err = 0.0
      else
        ! average of the sum of absolute values from TEST samples
        ave_abs_value = sum(abs(obj%tst_smp(mute_top:mute_bot))) / mute_count
        smp_err = max_abs_diff / ave_abs_value
      end if

      return
      end subroutine ezcheck_smp_err

!</execute_only>

!!----------------------- ezcheck_global_setup  ----------------------------!!
!!----------------------- ezcheck_global_setup  ----------------------------!!
!!----------------------- ezcheck_global_setup  ----------------------------!!


!<execute_only>

      subroutine ezcheck_global_setup (obj)
      implicit none
      type(ezcheck_struct),intent(inout) :: obj       ! arguments

      integer :: tst_lun
      integer :: global_cnt_job, global_cnt_tst, global_cnt_max
      integer :: status, idx
      character(len=MAX_NAME),dimension(:),pointer :: tst_keywordlist
      character(len=MAX_NAME) :: global_name
      character(len=MAX_NAME) :: str_job, str_tst
      character(len=7) :: global_mode
      integer :: global_idx
      real :: global_tolerance
      logical :: present_job, present_tst
      integer :: elements_job, elements_tst
      integer :: nature_job

      ! initialize
      nullify(tst_keywordlist)

      tst_lun = obj%trcio_file%lun

      ! get counts for job, test and glob_name_cnt
      ! sum is maximum possible global_count for allocation purposes

      global_cnt_job = pc_num_global_keywords()
      global_cnt_tst = cpsio_number_keywords(tst_lun,'JOBGLOBALS')

      if(global_cnt_tst < 0) then
        call pc_warning('TEST_DATA global keyword count is negative')
        global_cnt_tst = 0
      else
        call mem_alloc(tst_keywordlist,global_cnt_tst)

        if (pc_do_not_process_traces()) return   ! in case of allocation errors.

        status = cpsio_get_keywordlist(tst_lun,tst_keywordlist,'JOBGLOBALS')

        if(status /= CPSIO_OK) then
          call pc_error('TEST_DATA global keywordlist get failed')
          global_cnt_tst = 0
        end if
      end if

      global_cnt_max = obj%glob_name_cnt + global_cnt_job + global_cnt_tst

      ! allocate global_name/mode/tolerance/result
      call mem_alloc(obj%global_name,      global_cnt_max)
      call mem_alloc(obj%global_mode,      global_cnt_max)
      call mem_alloc(obj%global_tolerance, global_cnt_max)
      call mem_alloc(obj%global_result,    global_cnt_max)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      ! add unique global names from glob_name
      scan_glob_table: &
      do idx = 1, obj%glob_name_cnt
        global_name = obj%glob_name(idx)
        global_idx = ezcheck_global_find(obj,global_name)

        if(global_idx == 0) then
          ! this global_name is unique, add it to known globals
          obj%global_count = obj%global_count + 1
          global_idx = obj%global_count

          obj%global_name(global_idx) = global_name

          if(obj%glob_mode(idx) == 'DEFAULT') then
            global_mode = obj%glob_def_mode
            global_tolerance = obj%glob_def_tol
          else
            global_mode = obj%glob_mode(idx)
            global_tolerance = obj%glob_tol(idx)
          end if

          ! UNKNOWN will be changed to PASS/FAIL later.
          obj%global_result(global_idx) = 'UNKNOWN'

          select case(global_mode)
          case ('IGNORE')
            obj%global_mode(global_idx) = 1
            obj%global_tolerance(global_idx) = 0.0
          case ('EQUAL')
            obj%global_mode(global_idx) = 2
            obj%global_tolerance(global_idx) = 0.0
          case ('SAME')
            obj%global_mode(global_idx) = 3
            obj%global_tolerance(global_idx) = 0.0
          case ('ABS')
            obj%global_mode(global_idx) = 4
            obj%global_tolerance(global_idx) = global_tolerance
          case ('REL')
            obj%global_mode(global_idx) = 5
            obj%global_tolerance(global_idx) = global_tolerance
          end select
        else
          ! quietly ignore duplicates ?
        end if
      end do scan_glob_table

      ! add unique global names from job globals
      scan_job_table: &
      do idx = 1, global_cnt_job
        global_name = pc_get_global_keyword(idx)
        global_idx = ezcheck_global_find(obj,global_name)

        if(global_idx == 0) then
          ! this global_name is unique, add it to known globals
          obj%global_count = obj%global_count + 1
          global_idx = obj%global_count

          obj%global_name(global_idx) = global_name

          global_mode = obj%glob_def_mode
          global_tolerance = obj%glob_def_tol

          ! UNKNOWN will be changed to PASS/FAIL later.
          obj%global_result(global_idx) = 'UNKNOWN'

          select case(global_mode)
          case ('IGNORE')
            obj%global_mode(global_idx) = 1
            obj%global_tolerance(global_idx) = 0.0
          case ('EQUAL')
            obj%global_mode(global_idx) = 2
            obj%global_tolerance(global_idx) = 0.0
          case ('SAME')
            obj%global_mode(global_idx) = 3
            obj%global_tolerance(global_idx) = 0.0
          case ('ABS')
            obj%global_mode(global_idx) = 4
            obj%global_tolerance(global_idx) = global_tolerance
          case ('REL')
            obj%global_mode(global_idx) = 5
            obj%global_tolerance(global_idx) = global_tolerance
          end select
        end if
      end do scan_job_table


      ! add unique global names from TEST_DATA
      scan_tst_table: &
      do idx = 1, global_cnt_tst
        global_name = tst_keywordlist(idx)
        global_idx = ezcheck_global_find(obj,global_name)

        if(global_idx == 0) then
          ! this global_name is unique, add it to known globals
          obj%global_count = obj%global_count + 1
          global_idx = obj%global_count

          obj%global_name(global_idx) = global_name

          global_mode = obj%glob_def_mode
          global_tolerance = obj%glob_def_tol

          ! Any unique globals from TEST_DATA will FAIL (name does not
          ! exist in GLOB or job) unless the MODE is IGNORE
          obj%global_result(global_idx) = 'FAIL-NO-JOB'

          select case(global_mode)
          case ('IGNORE')
            obj%global_mode(global_idx) = 1
            obj%global_tolerance(global_idx) = 0.0
            obj%global_result(global_idx) = 'PASS-NO-JOB'
          case ('EQUAL')
            obj%global_summary = obj%global_summary + 1
            obj%global_mode(global_idx) = 2
            obj%global_tolerance(global_idx) = 0.0
          case ('SAME')
            obj%global_summary = obj%global_summary + 1
            obj%global_mode(global_idx) = 3
            obj%global_tolerance(global_idx) = 0.0
          case ('ABS')
            obj%global_summary = obj%global_summary + 1
            obj%global_mode(global_idx) = 4
            obj%global_tolerance(global_idx) = global_tolerance
          case ('REL')
            obj%global_summary = obj%global_summary + 1
            obj%global_mode(global_idx) = 5
            obj%global_tolerance(global_idx) = global_tolerance
          end select
        end if
      end do scan_tst_table

      ! validate all GLOBAL names where the RESULT is still UNKNOWN
      scan_global_table: &
      do idx = 1, obj%global_count
        if(obj%global_result(idx) /= 'UNKNOWN') then
          ! The global_result has already been determined.
          cycle
        end if

        global_name = obj%global_name(idx)

        present_job = pc_global_keyword_present(global_name)

        elements_tst = &
          cpsio_get_keyword_nelem(tst_lun,global_name,'JOBGLOBALS')

        if(elements_tst >= 0) then
          !??? better way to determine if TEST_DATA global is present
          present_tst = .true.
        else
          present_tst = .false.
        end if

        ! global_result will be set after exiting this "if".

        if(obj%global_mode(idx) == 1) then
          ! IGNORE this global
          if(present_job .and. present_tst) then
            obj%global_result(idx) = 'PASS'
          else if(present_job) then
            obj%global_result(idx) = 'PASS-NO-TEST'
          else if(present_tst) then
            ! This should never happend, because its caught above.
            obj%global_result(idx) = 'PASS-NO-JOB'
          else
            obj%global_result(idx) = 'PASS-NO-GLOBAL'
          end if
        else if(present_job .and. present_tst) then
          ! compare the actual values (scalar or array)
          nature_job = pc_nature_global(global_name)

          if(nature_job == CARDSET_SCALAR .and. elements_tst == 1) then
            ! compare scalar values
            call pc_get_global(global_name, str_job)

            status = cpsio_get_keyword(tst_lun,global_name, &
              str_tst,'JOBGLOBALS')

            if(ezcheck_compare_global(str_job, str_tst, &
              obj%global_mode(idx), obj%global_tolerance(idx))) then
              obj%global_result(idx) = 'PASS'
            else
              obj%global_summary = obj%global_summary + 1
              obj%global_result(idx) = 'FAIL'
            end if
          else
            ! assume nature_job is CARDSET_ARRAY
            elements_job = pc_num_elements_global(global_name)

            if(elements_job == elements_tst) then
              ! compare array values
!??? deferred, comparison of array globals
              obj%global_result(idx) = 'PASS'
            else
              obj%global_summary = obj%global_summary + 1
              obj%global_result(idx) = 'FAIL'
            end if
          end if
        else if(present_job) then
          obj%global_summary = obj%global_summary + 1
          obj%global_result(idx) = 'FAIL-NO-TEST'
        else if(present_tst) then
          obj%global_summary = obj%global_summary + 1
          obj%global_result(idx) = 'FAIL-NO-JOB'
        else
          obj%global_summary = obj%global_summary + 1
          obj%global_result(idx) = 'FAIL-NO-GLOBAL'
        end if
      end do scan_global_table

      call mem_free(tst_keywordlist)

      return
      end subroutine ezcheck_global_setup

!</execute_only>

!!----------------------- ezcheck_global_find  ----------------------------!!
!!----------------------- ezcheck_global_find  ----------------------------!!
!!----------------------- ezcheck_global_find  ----------------------------!!

!<execute_only>

      function ezcheck_global_find(obj,name)
      implicit none
      integer :: ezcheck_global_find
      type(ezcheck_struct),intent(inout) :: obj       ! arguments

      ! return index of global name or zero if not found

      character(len=*) :: name

      integer :: idx

      do idx = 1, obj%global_count
        if(name == obj%global_name(idx)) then
          ezcheck_global_find = idx
          return
        end if
      end do

      ezcheck_global_find = 0
      return

      end function ezcheck_global_find

!</execute_only>

!!--------------------- ezcheck_compare_global  ----------------------------!!
!!--------------------- ezcheck_compare_global  ----------------------------!!
!!--------------------- ezcheck_compare_global  ----------------------------!!

!<execute_only>

      function ezcheck_compare_global(str_job, str_tst, mode, tolerance)
      implicit none
      logical ezcheck_compare_global
      character(len=*) :: str_job, str_tst
      integer :: mode
      real :: tolerance

      ! return .true. if values pass comparison, otherwise .false.

      integer :: int_job, int_tst
      double precision :: dbl_job, dbl_tst
      integer :: status_job, status_tst

      if(str_job == str_tst) then
        ! trivial case
        ezcheck_compare_global = .true.
      else if(0 == verify(str_job,'+-0123456789 .eE') .and. &
              0 == verify(str_tst,'+-0123456789 .eE')) then
        ! the string smells like a number
        if(0 == scan(str_job,'.eE') .and. &
           0 == scan(str_tst,'.eE')) then
          ! the number smells like an integer
          int_job = string_ss2ii(str_job,status_job)
          int_tst = string_ss2ii(str_tst,status_tst)

          if(status_job /= 0 .or. status_tst /= 0) then
            ! problem parsing one of the numbers as an integer ?
            ezcheck_compare_global = .false.
          else if(mode == 2 .or. mode == 3) then
            ! MODE is EQUAL or SAME
            if(int_job == int_tst) then
              ezcheck_compare_global = .true.
            else
              ezcheck_compare_global = .false.
            end if
          else if(mode == 4) then
            ! MODE is ABS
            if(abs(int_job - int_tst) < tolerance) then
              ezcheck_compare_global = .true.
            else
              ezcheck_compare_global = .false.
            end if
          else if(mode == 5) then
            ! MODE is REL
            if(abs(real(int_job - int_tst) / real(int_job + int_tst)) &
              < tolerance) then
              ezcheck_compare_global = .true.
            else
              ezcheck_compare_global = .false.
            end if
          end if
        else
          ! the number smells like a float
          dbl_job = string_ss2dd(str_job,status_job)
          dbl_tst = string_ss2dd(str_tst,status_tst)

          if(status_job /= 0 .or. status_tst /= 0) then
            ! problem parsing one of the numbers as a double ?
            ezcheck_compare_global = .false.
          else if(mode == 2) then
            ! MODE is EQUAL
            if(dbl_job == dbl_tst) then
              ezcheck_compare_global = .true.
            else
              ezcheck_compare_global = .false.
            end if
          else if(mode == 3) then
            ! MODE is SAME
            if(0 == mth_compare(dbl_job,dbl_tst)) then
              ezcheck_compare_global = .true.
            else
              ezcheck_compare_global = .false.
            end if
          else if(mode == 4) then
            ! MODE is ABS
            if(abs(dbl_job - dbl_tst) < tolerance) then
              ezcheck_compare_global = .true.
            else
              ezcheck_compare_global = .false.
            end if
          else if(mode == 5) then
            ! MODE is REL
            if(abs((dbl_job - dbl_tst) / (dbl_job + dbl_tst)) &
              < tolerance) then
              ezcheck_compare_global = .true.
            else
              ezcheck_compare_global = .false.
            end if
          end if
        end if
      else
        ! the string smells like a string
        ! Note: if EQUAL then an earlier test would have passed it,
        ! otherwise SAME, ABS and REL are all treated the same.
        ! Assume case is significant ?
        if(trim(str_job) == trim(str_tst)) then
          ezcheck_compare_global = .true.
        else
          ezcheck_compare_global = .false.
        end if
      end if

      return

      end function ezcheck_compare_global

!</execute_only>

!!----------------------- ezcheck_header_setup  ----------------------------!!
!!----------------------- ezcheck_header_setup  ----------------------------!!
!!----------------------- ezcheck_header_setup  ----------------------------!!

!<execute_only>

      subroutine ezcheck_header_setup (obj)
      implicit none
      type(ezcheck_struct),intent(inout) :: obj       ! arguments

      integer :: do_hdr_num, do_header, hdr_num
      character(len=7) :: hdr_mode
      real :: hdr_tol

      ! initialize header control tables

      obj%header_count = max(obj%hdr_num_cnt, obj%nwih_job, obj%nwih_tst)

      call mem_alloc(obj%header_mode,      obj%header_count)
      call mem_alloc(obj%header_tolerance, obj%header_count)
      call mem_alloc(obj%header_result,    obj%header_count)
      call mem_alloc(obj%header_explicit,  obj%header_count)
      call mem_alloc(obj%header_icheck,    obj%header_count)
      call mem_alloc(obj%verbose_headers,  obj%header_count)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      ! initialize all entries to default (may be explicitly overridden)
      select case (obj%hdr_def_mode)
      case ('IGNORE')
        obj%header_mode = 1
      case ('EQUAL')
        obj%header_mode = 2
      case ('SAME')
        obj%header_mode = 3
      case ('ABS')
        obj%header_mode = 4
      case ('REL')
        obj%header_mode = 5
      end select

      do do_hdr_num = 1, obj%header_count
        obj%header_tolerance(do_hdr_num) = obj%hdr_def_tol
      end do

!      obj%header_tolerance = obj%hdr_def_tol
      obj%header_result = 0
      obj%header_explicit = .false.
      obj%header_icheck = 0

      scan_hdr_table: &
      do do_hdr_num = 1, obj%hdr_num_cnt
        ! for each explicitly listed header...
        hdr_num = obj%hdr_num(do_hdr_num)
        hdr_mode = obj%hdr_mode(do_hdr_num)
        hdr_tol = obj%hdr_tol(do_hdr_num)

        if(hdr_mode == 'DEFAULT') then
          ! header explicitly requests the default.
          ! same as if the header was not explicitly listed.
          hdr_mode = obj%hdr_def_mode
          hdr_tol = obj%hdr_def_tol
        end if

        if(hdr_mode == 'IGNORE') then
          ! ignore this header
          obj%header_mode(hdr_num) = 1
          obj%header_tolerance(hdr_num) = hdr_tol
        else
          ! check is not ignored for this header
          obj%header_tolerance(hdr_num) = hdr_tol

          select case (hdr_mode)
          case ('EQUAL')
            obj%header_mode(hdr_num) = 2
          case ('SAME')
            obj%header_mode(hdr_num) = 3
          case ('ABS')
            obj%header_mode(hdr_num) = 4
          case ('REL')
            obj%header_mode(hdr_num) = 5
          end select
        end if

        obj%header_explicit(hdr_num) = .true.

      end do scan_hdr_table

      ! initialize header_icount, header_icheck and update header_result
      scan_header_table: &
      do do_header = 1, obj%header_count
        ! for each possible header word...
        if(do_header <= obj%nwih_min) then
          ! header word exists in both job input and TEST_DATA
          if(obj%header_mode(do_header) /= 1) then
            ! check is not IGNORE.
            ! add header to list of those checked trace by trace.
            obj%header_icount = obj%header_icount + 1
            obj%header_icheck(obj%header_icount) = do_header
          end if
        else if(do_header > obj%nwih_job .and. do_header <= obj%nwih_tst) then
          ! header does not exist in INPUT
          obj%header_result(do_header) = -1
        else if(do_header > obj%nwih_tst .and. do_header <= obj%nwih_job) then
          ! header does not exist in TEST_DATA
          obj%header_result(do_header) = -2
        else
          ! header does not exist in either INPUT or TEST_DATA
          obj%header_result(do_header) = -3
        end if
      end do scan_header_table

      return
      end subroutine ezcheck_header_setup

!</execute_only>

!!----------------------- ezcheck_sample_setup  ----------------------------!!
!!----------------------- ezcheck_sample_setup  ----------------------------!!
!!----------------------- ezcheck_sample_setup  ----------------------------!!

!<execute_only>

      subroutine ezcheck_sample_setup (obj)
      implicit none
      type(ezcheck_struct),intent(inout) :: obj       ! arguments

      ! allocate one samples array for comparison RESULT
      call mem_alloc(obj%sample_mask, obj%ndpt_job)
      call mem_alloc(obj%sample_result, obj%ndpt_job)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      obj%sample_result = 0

      return
      end subroutine ezcheck_sample_setup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module ezcheck_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

