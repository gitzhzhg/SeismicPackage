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
! Name       : COLOR    (COLOR plotting)
! Category   : plot
! Written    : 1989-10-16   by: Karen Goodger
! Revised    : 2006-12-04   by: D. Glover
! Maturity   : production
! Purpose    : Generate a color plot file.
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
! COLOR, when used in conjunction with a color plotter, will generate a color
! plot of the input seismic trace data.
!
!
! Color Bars
!
! COLOR must map input trace sample amplitudes to colors to be plotted. 
! Normally instructions for this mapping are provided by a file containing a 
! "color bar" which is a list of colors, defined by red, green, blue 
! percentages and amplitudes associated with transitions between adjacent 
! colors.  Thus, COLOR can associate a color with any input sample amplitude.  
! Options are also provided to allow the user to override the amplitude values
! in the color bar and specify, by minimum and maximum, a range of evenly 
! spaced amplitudes (VMIN, VMAX).  The user can also define these minimum and 
! maximum amplitudes by specifying percentiles of input sample amplitudes (PNC,
! PPC, PPLC).
!
! Color bars must be built with the BBAR utility.
! 
!
! Tone Bars
!
! A 2D color bar, known as a "tone bar," can be used with COLOR.  When using a
! tone bar, COLOR expects pairs of traces with the odd traces considered to be 
! attribute 1 and the even traces attribute 2.  COLOR reads the trace pairs and
! maps the pairs of attribute samples to colors on the tone bar.  The first 
! attribute refers to the odd traces and the horizontal axis of the tone bar 
! and the second attribute refers to the even traces and the vertical axis of 
! the tone bar.  An option is provided for the user to override the amplitude 
! values in the tone bar.  COLOR will estimate the distribution functions of 
! the attributes and set tone bar amplitudes so that color patch occupation 
! percentages approximate the estimated distribution functions.
! 
! The discrete method (DSCR) uses the normal binning method to estimate the 
! distribution function.  The continuous method (CONT) creates a smoother but 
! less accurate estimate.
!
! Tone bars must be built with the TBAR utility.
!
!
! Length Units
!
! Depending on the choice of English or metric units on the Project_data screen,
! COLOR will automatically use inches or miles where appropriate (English) or
! centimeters or kilometers where appropriate (metric).  Exceptions to this rule
! are documented.  Plotting limitations or restrictions are typically
! documented only in inches.
!
!
! Limitations
!
! Maximum plot length is 200 inches for CONPLOT.  Maximum plot height is 30 
! inches for HP plotters and 35 inches for Versatec plotters.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! COLOR is a highly versatile process that produces color trace plots.  Because
! the number of options relating to trace plotting are very numerous and the 
! options relating to annotation are even more numerous, COLOR is a process for
! the expert user.  (There are over 130 parameters.)
!
! Parameters are located in functionally related groups on the parameter entry
! screens; these groups are listed below.  The first group consists of the 
! 12 essential parameters required to produce a basic plot.  The second group 
! consists of basic annotation parameters.  Users with modest requirements will
! not need to visit additional parameter groups.
!
! COLOR Parameter Categories
! 
!       Automatic Annotation Positioning
!       Basic Annotation
!       Basic Plot
!       Block Boundaries and Line-ties
!       Color Bar and Amplitude
!       Dashed Line Plot
!       Hardcopy Related
!       Manual Shotpoint-style Annotation
!       Panel and Blank Trace
!       Semblance Panel Plot
!       Side Label
!       Timing Lines
!       Tone Plot
!       Trace Overlay
!
!
! Automatic Annotation Location
!
! Setting CALD=YES adjusts parameters SCAS, SDAS,LTDA and ARDA in order to 
! prevent annotation from overlapping vertically.  This calculation is made at 
! workfile creation time. 
!-------------------------------------------------------------------------------
!</advice_doc>
!
!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a single-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process outputs the same traces as it receives unaltered.
!
! This process outputs traces with same gather status as the input traces.
!
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name            Description                       Action taken
! ----            -----------                       ------------
! SURVEY_UNITS    meters or feet                    used but not changed
! NDPT            number of sample values in trace  used but not changed
! TSTRT           starting time on trace            used but not changed
! DT              trace sample interval             used but not changed
! 
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#    Description                Action taken
! ----    -----------                ------------
!         HDR_LAB                    Used
!         HDR_BT                     Used
!         HDR_DLP                    Used
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!              REVISION HISTORY                    
! 
!     Date         Author        Description
!     ----         ------        -----------
!15. 2006-12-04    D. Glover     Added NULLIFY statements for Intel compiler.
!14. 2006-06-06    Stoeckley     Add pc_register_array_names for SeisSpace.
!13. 2006-01-17  B. Menger     - Changed declarations as needed.
!12. 2006-01-10  B. Menger     - Removed Unused Variables.
!11. 2002-06-11 - Goodger      - Fix bug with trace overlay being half length.
!                                Change default arrow size to calculated rather
!                                than .5.
!10. 2002-04-15 - Goodger      - Fix bug with opt_calc parameter checking for
!                                erroneous option.  Fix warnings detected by
!                                intel compiler.  Add option to output to a
!                                cgm file.
!09. 2001-10-18 - Goodger      - GUI change. Set Ystretch to true for LAB_CB
!                                and CS_CBL parameters.
!08. 2001-08-27 - Goodger      - Call new title block routine, ATBLK.
!07. 2001-02-15 - Goodger      - Getting thrown out of cfetest when change
!                                nttp to zero and hitting apply.
!06. 2001-02-06 - Goodger      - Fix problem with metric panel label characters.
!05. 2001-02-02 - Goodger      - Fix problem with trace overlay when paneling.
!04. 2001-01-30 - Goodger      - Problem with 3rd plot in job only plotting
!                                one trace.  Needed to reset trace counter to
!                                zero.
!03. 2001-01-29 - Goodger      - Fix bug which was not picking up a user
!                                specified color bar.  Fix problem with
!                                TPI_OVL help, and change its default to zero.
!02. 2001-01-25 - Goodger      - Call pathcheck primitive on all path 
!                                parameters.  Fix problem with metric sid
!                                size.  Upcase scale yes and no answers.        
!01. 2001-01-15 - Goodger/Korn - Conversion        
!
!                   OLD REVISION HISTORY
!48. 1999-11-05 - K.Goodger - Change doc on node=poplot, queue A.        
!47. 1998-11-11 - K.Goodger - Add documentation about Aberdeen nodes.    
!46. 1998-08-06 - K.Goodger - Remove node HOWWE.  Front end only.        
!45. 1998-07-28 - K.Goodger - Add NJOB parameter to ATB call.            
!44. 1998-04-06 - K.Goodger - Fix problem in SEMB routine with picks not 
!                             honoring tstrt.
!43. 1998-03-16 - K.Goodger - Fix problem in SEMB routine not recognizing
!                             keyword NOPICKS.
!42. 1998-02-16 - K.Goodger - Initialize GLAV to zero in COLRTRCE.    
!41. 1998-01-22 - K.Goodger - Read from unit number returned by dcodet
!                             routine rather than unit 5.
!40. 1998-01-19 - K.Goodger - Fix problem in routine SHOT with getarg.
!39. 1998-01-14 - K.Goodger - Fix problem with velocity file name in
!                             colrsemb
!38. 1997-12-24 - K.Goodger - Add parameters DLPH,DLREF,DLU100.
!37. 1997-12-15 - K.Goodger - Convert to Fortran90 free form. Add keywords
!                             for certain color bars which are stored in
!                             [cfe.colr_bars].
!36. 1997-08-04 - K.Goodger - Allow 16 characters for LWDA and LWDB labels
!                             Use settxt to justify these labels.
!35. 1997-05-27 - K.Goodger - Use settxt routine to justify scle label.
!34. 1997-02-27 - K.Goodger - Add FLPT parameter.
!33. 1997-02-05 - K.Goodger - Make plcs parameter honor the metric flag.
!32. 1997-01-20 - K.Goodger - Node POPLOTB changed to node PO4001.
!31. 1996-12-30 - K.Goodger - Add parameters SCLE,SCDA,and SCCS as in
!                             SPLT.
!30. 1996-12-19 - K.Goodger - Fix problem in SHOT routine.  Was skipping
!                             setting the correct y-coordinate when
!                             encountered a negative shot.
!29. 1996-12-05 - K.Goodger - Documentation change only.  New names for
!                             Lafayette plot nodes.
!28. 1996-10-07 - K.Goodger - Make distribution numbers overlayed on TONE
!                             color bar go to two decimal places, and
!                             reduce character size so it will fit.
!27. 1996-09-03 - K.Goodger - Label shot points between panels when using
!                             shot point cards.
!26. 1996-06-20 - K.Goodger - Remove unused call to tclr in routine logo.
!25. 1996-05-06 - K.Goodger - Remove node HOCOS1.  Documentation change
!                             only.
!24. 1996-04-22 - K.Goodger - Utilize settxt routine for justifying text
!                             strings in TLNE routine.
!                           - Add nodes ABERD1,ABERD2,ABERD3,ABERD4,AEOPA,
!                             AEOPB,IIHP0C to documentation.
!23. 1996-04-17 - K.Goodger - Set line width to .005 in the TIE routine.
!                             The line width effects the thinkness of the
!                             font.
!22. 1996-04-08 - K.Goodger - Add node POHP01.  Documentation change only.
!21. 1996-03-20 - K.Goodger - Move up Y coordinate on TONE color bar in
!                             case where the height of the color bar is
!                             greater than the height of the data.
!20. 1996-03-12 - K.Goodger - Increase TONE to maximum of 21 X 21.
!19. 1996-03-07 - K.Goodger - Remove check in routine TLNE which prevented
!                             timing lines from being plotted when only
!                             one row of panels and TISTR arrays used for
!                             labels.
!18. 1996-03-04 - K.Goodger - Check for string 'atbm' (lower case) when
!                             determining to do ATB.  OPNFIL is now
!                             changing the string to lower case.
!17. 1995-12-21 - K.Goodger - Remove corpus node, doc change only.
!16. 1995-08-28 - K.Goodger - Ability to read new and old block label
!                             formats.
!15. 1995-08-09 - K.Goodger - Add node POENV1.
!14. 1995-08-03 - K.Goodger - Use separate files for ATB as with tie
!                             files etc.  Starting to get a problem with
!                             Unicos 8.0 with not being able to read and
!                             then rewrite the same file name.  Does not
!                             happen with all jobs.
!13. 1995-07-24 - K.Goodger - Add keyword SEMF=NOPICKS for doing
!                             semblance panels without the velocity picks
!                             overlayed.
!12. 1995-07-13 - K.Goodger - Fix call to COLRTRCE when DPUC=NO.  There
!                             was a misspelled variable.
!11. 1995-07-12 - K.Goodger - Fix a problem when doing only one semb.
!                             panel.  Some information gets set when
!                             program encounters the first panel change
!                             and this was not getting done.
!10. 1995-06-12 - K.Goodger - Replace function SPLTCLT with function AMEQ.
!09. 1995-05-16 - K.Goodger - Fix problem introduced by change 6. Was
!                             returning before parameters were being
!                             saved and caused a problem when more than
!                             1 colr in a job.
!08. 1995-05-15 - K.Goodger - Fix problem with automatic reduction of
!                             title block.  Was not working if the user
!                             supplied reduction (FRTB) did not reduce the
!                             title block small enough to fit the paper.
!07. 1995-05-12 - K.Goodger - Remove diagnostic print.
!06. 1995-05-08 - K.Goodger - Fix problem encountered when NUMTRC > NT.
!                             Routine changed is COLR.
!                           - Fix problem with SKPT parameter.  If
!                             paneling, plot was stopping short by SKPT
!                             traces.  Routine changed is TRCE.
!05. 1995-05-03 - K.Goodger - Add title block field OWNER. Routines
!                             changed are TBLK and THIT.
!04. 1995-02-24 - K.Goodger - Add node NO6000, Norway, Zeh graphics.
!03. 1995-01-26 - K.Goodger - In routine COLRPLOT change check for .GT.
!                             39.5 to .GT. 39.50001.
!02. 1995-01-25 - K.Goodger - Rewind the STROT file after VMIN,VMAX cal-
!                             culation.
!01. 1995-01-03 - K.Goodger - Move a block of code so that the color will
!                             be done before the annotation.  This is
!                             necessary for the move to CGM files which
!                             will wipe out annotation if color is done
!                             after.
!**********************************************************************
!28. 1994-11-09 - K.Goodger - Base number of overlay traces in a panel to
!                             number of panels rather than ratio to over-
!                             lay traces.
!27. 1994-11-07 - K.Goodger - Fix problem in routine TBLK.  Another almost
!                             zero problem.  Getting an extremely small
!                             negative number rather than zero which
!                             caused an abort due to a less than zero
!                             check.
!26. 1994-11-03 - K.Goodger - Fix problem in routine TRCE.  Was not re-
!                             versing polarity of overlay trace if
!                             paneling.
!25. 1994-10-18 - K.Goodger - Reset IDOT variable back to zero in routine
!                             TLNE so that timing line parameters will be
!                             reset for next row of panels.
!24. 1994-10-05 - K.Goodger - Add check for POPLOT when determining wheth-
!                             er to use NRNP for route name.
!23. 1994-10-03 - K.Goodger - Add a controlled abort if all overlay traces
!                             are dead.
!                           - Increase the number of digits which can be
!                             plotted on a color bar label from 8 to 16.
!                             Routines INKS,F2CHAR
!22. 1994-09-21 - K.Goodger - Insure line drawn at top of section on all
!                             panels when TSTRT is not zero.
!                             Pass NTRPH to routine PLBL. It needed to
!                             take grading into consideration when
!                             determining horizontal distance between
!                             panels.
!21. 1994-09-12 - K.Goodger - Problem with Tie marks falling short on
!                             panel plot corrected.
!20. 1994-08-23 - K.Goodger - Add panel label option, new parameters are
!                             PLCS,PROW,PCOL,PLBL
!19. 1994-08-11 - K.Goodger - Fix problem skipping around some needed code
!                             when encounter a dead trace.  Was causing
!                             the displayed basement interval to be set
!                             incorrectly.
!18. 1994-08-10 - K.Goodger - Fix misspelled variable in WRDA routine.
!17. 1994-07-28 - K.Goodger - Add routine COLR3DBL for plotting block
!                             labels from a 3D file.  Routines changed are
!                             COLR, COLRCKH,COLR3DBL.
!16. 1994-06-30 - K.Goodger - Reset timing line number parameters when
!                             doing 400 mil lines only. Routine TLNE.
!15. 1994-06-28 - K.Goodger - Insure Y-coordinate passed to the TBLK rou-
!                             tine does not exceed 39.5 inches.
!14. 1994-06-27 - K.Goodger - Add parameter TD400.
!                             Write a new routine to calculate title block
!                             height - COLRTHIT.  This is nearly a dupli-
!                             cation to TBLK but without plotting.  It
!                             gives a more accurate height calculation
!                             which is necessary in order to reduce the
!                             title block to fit the paper before TBLK is
!                             actually called.
!13. 1994-06-14 - K.Goodger - Replace some code in COLRTBLK. LOGO was not
!                             plotting.
!12. 1994-06-08 - K.Goodger - Documentation change - add node POPLOTB.
!11. 1994-05-26 - K.Goodger - Documentation change - remove HOPLT1.
!10. 1994-05-13 - K.Goodger - Documentation change on plotting nodes. Add
!                             checks for POPLOT to checks for POSP03.
!09. 1994-04-28 - K.Goodger - Add 2 source 3 streamer cable diagram.
!08. 1994-04-12 - K.Goodger - Add argument IROT to COLRSETX routine for
!                             rotating the plot.
!07. 1994-03-17 - K.Goodger - Fix problem with only 2 colors in a color
!                             bar.
!06. 1994-03-16 - K.Goodger - Fix problem in PIXL routine calculating
!                             space between panels.
!05. 1994-03-15 - K.Goodger - Look at every other trace in WRDA routine if
!                             doing TONE processing.
!04. 1994-02-18 - K.Goodger - Add AXIS routine.
!03. 1994-01-31 - K.Goodger - Add parameters FVELX, and LVELX for select-
!                             ing a range of semblance panels.
!02. 1994-01-17 - K.Goodger - Pass TIMBTWN to routine SDLB.  It was still
!                             using .1 between panels.
!01. 1994-01-03 - K.Goodger - Changed local name of tie file 2.  It was
!                             using the same name as SPLT uses.
!                           - Further changes to restarting horizontal
!                             grade after blanks.  Routines MAIN,SHOT,
!                             TRCE,2GAB.
!***********************************************************************
!68. 1993-12-09 - K.Goodger - Restart horizontal grade after blank traces
!                             are inserted.  Routines changed BLID,SHOT,
!                             TRCE,WRDA,MAIN.  New C routine COLR2GAB -
!                             "to graded after blanks".
!67. 1993-10-19 - K.Goodger - Added reverse polarity options RPC and RPO.
!66. 1993-09-23 - K.Goodger - Changed NVRT option so that it inverts the
!                             overlay trace as well as the color trace.
!65. 1993-09-09 - K.Goodger - Fix misspelling of NTRPV when passed to TLNE
!                             for panel plots.
!64. 1993-09-01 - K.Goodger - Initialize NTRPV variable to 1.
!                             Do not call TLNE routine if all timing line
!                             dot parameters are set to zero.
!63. 1993-08-26 - K.Goodger - Add FRTB parameter.
!                           - Fix problem with ISKPT parameter routine
!                             TRCE.
!62. 1993-08-26 - K.Goodger - Fix problem with white space between panels.
!                             It was not taking vertical interpolation
!                             into consideration.
!61. 1993-08-24 - K.Goodger - Check INC-DEC flag on first trace block
!                             label check. Skip symbol call if block label
!                             is blank.
!                           - Fix labeling problem in TLNE involving
!                             paneled plots when tstrt global is not zero.
!60. 1993-05-18 - K.Goodger - Add node HOPLT1 for WES-H.
!59. 1993-05-03 - K.Goodger - Add node ACMV1.
!58. 1993-04-20 - K.Goodger - Fix problem in TLNE routine determining
!                             whole number.
!57. 1993-04-06 - K.Goodger - Subtract grade bias on panel plots in TLNE
!                             routine.
!56. 1993-03-08 - K.Goodger - Fix problem in TLNE which was plotting
!                             numbers down to 1.0 sec when there was less
!                             than a second plotted.
!55. 1993-02-10 - K.Goodger - Add Ybias for TSTRT on 10,50,&100 mil lines
!                             as was already done for 500 & 1000.
!54. 1993-01-27 - K.Goodger - Add second row of tie lines.
!                           - Fix problem when TSTRT is not zero. Whole
!                             numbers are now plotted in larger characters
!53. 1993-01-21 - K.Goodger - Fix problem in TBLK routine, underline
!                             extending past title block width.
!52. 1993-01-13 - K.Goodger - Fix problem in TRCE routine, was not setting
!                             interpolation factor to 1 if not paneling in
!                             call to ABT.
!51. 1993-01-11 - K.Goodger - Add DOC for node PFPLOT.
!                             Panel the overlay traces.
!***********************************************************************
!51. 1992-12-21 - K.Goodger - Add parameters DPUC,NTUC.
!                           - In SEMB routine, determine if data is incre-
!                             menting or decrementing and match up with
!                             functions.
!50. 1992-11-23 - K.Goodger - Correct problem with shot point array.
!49. 1992-11-16 - K.Goodger - Let SMED be YES, NO, or a number.
!48. 1992-11-12 - K.Goodger - Update documentation on plotting nodes.
!48. 1992-11-05 - K.Goodger - Fix problem in WRDA routine which was losing
!                             the tic mark below the shot point.
!47. 1992-10-29 - K.Goodger - Fix problem with shot point array index.
!46. 1992-10-28 - K.Goodger - Shot point labels not honoring blank traces.
!45. 1992-10-28 - K.Goodger - Add option to plot Semblance output from the
!                             VELF program.
!44. 1992-08-04 - K.Goodger - Screen order of parameters, print out time
!                             before paneling.
!43. 1992-07-29 - K.Goodger - Correct timing line problem with panel.
!43. 1992-06-30 - D.Peterson- Correct pointer name IPVLBL -> IPVROW.
!42. 1992-06-12 - K.Goodger - Fix problem with TMSH lines in WRDA routine.
!41. 1992-06-10 - K.Goodger - Label WRDA and WRDB labels on all panels.
!                           - Get NRNP and IDNP into route information if
!                             not poesp2 node.
!41. 1992-05-18 - K.Goodger - Use white between panels. Add parameters
!                             WRDA,WRDB,STRT,INTB,LWDA,LWDB,and WBCS.
!                             Add node NOM400 for Norway, 400 dot.
!40. 1992-04-21 - K.Goodger - Remove TIEH and BLKH parameters.
!39. 1992-04-14 - K.Goodger - Fix problem with TLNE routine. Checking for
!                             equal of US param when s-b not equal.
!38. 1992-04-08 - K.Goodger - Fix problem with TLNE routine and RNUM
!                             parameter.
!37. 1992-04-07 - K.Goodger - Fix problem with VLBL routine repeating a
!                             label.
!36. 1992-03-31 - K.Goodger - Add panel option.
!                           - Screen order of parameters.
!35. 1992-03-11 - K.Goodger - Do not call Symbol routine on blank SID
!                             field.
!34. 1992-02-13 - K.Goodger - Fix problem with the blank trace pattern not
!                             honoring the total when GRDH=YES.
!33. 1992-02-12 - K.Goodger - Add node NWMV2 for New Orleans.
!32. 1992-03-03 - K.Goodger - Pass node name job built on thourgh SETX
!                             routine.
!31. 1992-01-16 - K.Goodger - Fix misspelling of VECF parameter when
!                             passed through SETX routine.
!29. 1992-01-06 - K.Goodger - Send Cray NQS as vector file name.
!28. 1991-11-18 - K.Goodger - Remove GRADE user trap.
!                           - Add Casper node - CPMV1.
!27. 1991-11-07 - K.Goodger - Trap GRADE users.
!26. 1991-10-30 - K.Goodger - Pass no history processing on internal
!                             call to STRIN.
!25. 1991-10-29 - K.Goodger - Remove the custom code message.
!24. 1991-10-21 - K.Goodger - Documentation correction to the CBAR
!                             parameter.
!23. 1991-09-30 - K.Goodger - Fix problem in routine HILO which prevented
!                             negative VMIN.
!22. 1991-09-09 - K.Goodger - Add node NOMV1 for Norway.
!21. 1991-08-15 - K.Goodger - Change limit checks for logo from 39.5 to
!                             40.0.
!                           - Fix divide by zero problem when calculating
!                             VLINC parameter.
!20. 1991-07-31 - K.Goodger - Routine HILO - check for all negative data.
!                             Set VMAX to zero and insure negative VMIN.
!19. 1991-07-24 - K.Goodger - Routine COLRPLT - get the IFRST flag out of
!                             data statement and into replacement state-
!                             ment. It was not set properly for the second
!                             COLR in a job.
!18. 1991-07-23 - K.Goodger - Get repeated colors into the standard color
!                             bar for use with GRADE=YES option.
!17. 1991-07-22 - K.Goodger - Update RGB array per Val Rogers.
!16. 1991-07-16 - K.Goodger - For GRADE parameter, increase number of
!                             colors in color bar in an attempt to get
!                             a smoother interpolation.
!15. 1991-07-02 - K.Goodger - Fix trace counter in overlay routine. Was
!                             counting skipped traces and should not. This
!                             caused the overlay trace to start in the
!                             wrong place.
!14. 1991-06-28 - K.Goodger - Close ATBF file.
!                             Add UNICOS with replot information.
!13. 1991-06-18 - K.Goodger - Get dublogo from UNICOS account.
!12. 1991-06-05 - K.Goodger - Remove RDEOF's after DCODE.
!11. 1991-06-03 - K.Goodger - Add extra code before and after color bar
!                             for conplot interpolation. Adjust pixplt
!                             X-coordinate for conplot interpolation.
!                             Update a few more Conplot color codes.
!                             Add parameter GRADE for users to access new
!                             interpolation.
!                             Use plotter 4 for UKNOW node.
!10. 1991-04-25 - K.Goodger - Mickey mouse in TLNE routine. Reference
!                             THETA variable to insure storage.
!                           - Get entire parameter block into one
!                             common block. Under Unicos, it is not
!                             assured that common blocks falling
!                             sequentially in the code will fall sequen-
!                             tially in memory.
!                           - Add node UKNOW for London.
! 199. 1991-04-18 - K.Goodger - Fix problem in SHOT routine. First shotpoint
!                             actually zero, and not a flag for negative.
!                             Needed to blank fill the shot number
!                             variable.
!                           - Unicos change - Skip over code which
!                             checks an array which has not been
!                             allocated.
!                           - Add node HOIESO for Houston WES-H.
! 8. 1991-04-16 - K.Goodger - Add node MIMV1 for Midland.
!                           - Change defaults MXPR to 50 and NJOB to 25.
! 7. 1991-04-15 - K.Goodger - Fix calculation of XEND parameter for TONE
!                             jobs. Divide number of input traces by 2.
!                           - Fix problem converting vertical labels to
!                             English.
! 6. 1991-04-10 - K.Goodger - Add linked array parameters SLTIM,SLBL,SLSZ
!                             for Side LaBeLs. This is a time, annotation
!                             option which is plotted at the left side of
!                             the section.
!                           - Calculate ending X-coordinage for annotation
!                             on all traces rather than live traces.
! 5. 1991-03-25 - K.Goodger - Calculate ending coordinate based on actual
!                             number traces plotted rather than NT
!                             parameter.
!                           - Add documentation on NODE=VAXSV.
!                           - Make TSTR labels a linked array.
! 4. 1991-03-19 - K.Goodger - Use only every other trace for determining
!                             labels from headers on TONE jobs.
! 3. 1991-03-06 - K.Goodger - Put check in routine COLRPIXL to prevent
!                             indexing outside array.
! 2. 1991-01-24 - K.Goodger - UNICOS changes.
! 1. 1991-01-21 - K.Goodger - Change calc for num of decimal points on
!                             TSTR labels.
!***********************************************************************
! 1990-12-26 - K.Goodger - Recompile.
! 1990-12-11 - K.Goodger - Insure pixplt interpolation flag is 1. Was set
!                          zero in TONE routine.
! 1990-12-06 - K.Goodger - Put in calculation for number of decimal points
!                          to plot on color bar label. Routine COLRINKS.
! 1990-11-15 - K.Goodger - Use Conplot color interpolation if .RGB files.
!                          Interpolate on Cray if not.
!                        - Passing rectangle floating point zero when
!                          should be integer zero. Caused CFT77 problems.
! 1990-10-25 - K.Goodger - Remove some unneeded variables detected by
!                          CFT77.
! 1990-10-16 - K.Goodger - Fix index calculation to get to proper color
!                          code when VMIN-VMAX is being used. Program
!                          has been using an index of 1 less than it
!                          should.
!                          Fix problem with calculation of YO-YEND.
!                          Problem occurrs on LR plots when LTAB=yes.
!                          Yend of off .1 from 8-20 change.
! 1990-08-28 - K.Goodger - Compensate for Conplot Font 19 giving only
!                          85 % of character height.
! 1990-08-22 - K.Goodger - Reduce space between data and title block.
! 1990-08-21 - K.Goodger - Add node HOWWE.
! 1990-08-21 - K.Goodger - Return title block file after use.
! 1990-08-20 - K.Goodger - Define colors with DEFRGB routine to provide
!                          a color table for C.C. Burch's interpolation
!                          routine.
!                        - Read in files from BRGB program.
!                        - Move LR plots up to .1 rather than zero. Pre-
!                          vents fold over problem.
! 1990-08-14 - K.Goodger - Getting junk in 3rd and 4th lines of SID on
!                          title block portion.
! 1990-08-13 - K.Goodger - Do not plot NRNP information if POSP03.
!                          Add 2 more lines of SID.
! 1990-07-19 - K.Goodger - Correct calculation of area fill on overlay
!                          traces.
! 1990-06-20 - K.Goodger - Add node LAMV2 for Lafayette.
! 1990-06-07 - K.Goodger - Have TSTR parameter honor TSTRT global.
! 1990-05-11 - K.Goodger - Add node PO8600.
! 1990-05-08 - K.Goodger - Increase NPARM. NPWRDS was not getting saved.
! 1990-04-30 - K.Goodger - Insure Y-origin of title block is at least .1.
! 1990-04-10 - K.Goodger - Fix bug - not plotting first trace if GRDH=NO.
! 1990-04-09 - K.Goodger - Call STREND after doing trace overlay. Allows
!                          second COLR in job with same overlay file.
! 1990-03-27 - K.Goodger - Put replot information on plot.
! 1990-03-22 - K.Goodger - Fix problem with X-coord calculation on trace
!                          overlay.
! 1990-03-20 - K.Goodger - Add blank trace option.
! 1990-03-06 - K.Goodger - Add charge code to conplot call. Add node
!                          POESP2.
! 1990-02-15 - K.Goodger - Change TIO parameter to alpha so may have an
!                          R to be a ratio to TI.
!                          Fix block label problem on second COLR in job.
!                          Change CT calculation to be based on overlay
!                          traces rather than Color traces
! 1990-02-08 - K.Goodger - Change number of inches to fill peaks on trace
!                          overlay to 100% of LAV.
! 1990-02-06 - K.Goodger - Fix capsule of Dubai Logo.
!                          Convert ARDA and TIDA to inches when MTRC=YES.
! 1990-02-02 - K.Goodger - Allow up to 100 words for TEST library calls.
!                          Insure IS is in inches before NTPV calculation
! 1990-01-30 - K.Goodger - Fix problem with tie lines and block labels
!                          being off 1 trace.
! 1990-01-15 - K.Goodger - Add save vector file on Vax in Node=Vaxsv.
! 1990-01-08 - K.Goodger - Change OWN parameter from 1 word to 2. STRIN
!                          expects it to be 2 words.
!                          Add node AVALON to check out avalon board.
! 1990-01-03 - K.Goodger - Fix conversion from metric system on some
!                          parameters.
! 1989-12-27 - K.Goodger - Fix problem with GRDH=NO not setting trace
!                          width.
! 1989-12-27 - K.Goodger - Save vector file on Cray disk.
! 1989-12-20 - K.Goodger - Update NSTOR calculation.
! 1989-12-14 - K.Goodger - Add ability to network plots to different
!                          nodes.
! 1989-12-11 - K.Goodger - Add trace overlay.
!                        - Correct TONE labels when DCM1 or 2 = FILE.
!                        - Move GETSCR calls to N=0 area.
! 1989-11-30 - K.Goodger - Fix problem with not converting IS parameter to
!                          inches when MTRC=YES.
! 1989-11-27 - K.Goodger - Add tie lines.
! 1989-11-21 - K.Goodger - Change name of block label file so won't
!                          be the same as COLR.
! 1989-11-20 - K.Goodger - Add factor to reduce title block FRTB.
!                          Add direction arrows. Add Block labels.
! 1989-11-10 - K.Goodger - Add Initial skip SKPC.
! 1989-11-10 - K.Goodger - Add TITL BLOCK.
! 1989-11-06 - K.Goodger - Fix Y-coordinate on 2nd line of Sid.
! 1989-11-03 - K.Goodger - Change STROT file name to 6 char.
! 1989-10-27 - K.Goodger - Add TONE processing.
! 1989-10-26 - K.Goodger - Fix bad arg in GETP and PUTP calls. Correct
!                          NPARM.
! 1989-10-23 - K.Goodger - Fix problem with GRDH=NO plotting extra trace.
! 1989-10-19 - K.Goodger - Fix problem with one of the calls to COLRPIXL
!                          not having enough arguments.
! 1989-10-17 - K.Goodger - Default ATB1 and ATB2 parameters.
! 1989-10-11 - K.Goodger - Add NVRT parameter. Invert data before
!                          plotting.
! 1989-10-11 - K.Goodger - Fix problem calculating number of words to
!                          multiple of 64.
! 1989-10-10 - K.Goodger - Add SMED parameter. Finds the median value
!                          and divides all sample points by it, forcing
!                          1 to be the median.
! 1989-10-09 - K.Goodger - Remove decimal point from US labels and right
!                          justify.
! 1989-10-06 - K.Goodger - First version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS               
!
! No known limitations.
!
! 
! 
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS             
!
! No special requirements.
!
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
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
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

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS         
!
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

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
!<NS COLOR Process/NC=80>
!
!                  COLOR (COLOR PLOT)  
!
! TIME=`FFFFFFFFFFF  IPS=~~~`FFFFFFFFFFF  TPI=`FFFFFFFFFFF 
!
! LRRL=`CC           NUM_TR=`IIIIIIII  
!
! SID   
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS          CS_SID=`FFFFF
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS  
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS  
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS  
!
!  GRADE_HOR=`CC    INVERT=`CC    RP=`CC     SKIP_INIT=`IIIIIIIII 
!
!  GRADE_VRT=`CC 
!<PARMS SID[/XST/YST]>
!<NS Basic_Annotation_Parameters/NC=80>
!
!    SCALE=`SS     CS_SCALE=`FFFF  
! 
!
! HDR_LAB         NAM_LAB               CS_LAB 
! `IIIIIIII       `SSSSSSSSSSSSSSS      `FFFFFFF    LAB_INIT=~~`SSSS  
! `IIIIIIII       `SSSSSSSSSSSSSSS      `FFFFFFF     
!                                                    LAB_INC=~~`IIIIIIII  
!                                                     
! TRCE   SHOT    INTV  INCR  TOTL  
! `FFFFFF`SSSSSSS`IIIII`IIIII`IIIII     CS_SPL=~~~`FFFFF 
! `FFFFFF`SSSSSSS`IIIII`IIIII`IIIII 
! `FFFFFF`SSSSSSS`IIIII`IIIII`IIIII 
! `FFFFFF`SSSSSSS`IIIII`IIIII`IIIII 
!
!
!
! LTAB=`CC    OPT_VL=`CCC     WID_VL=`FFFFFFFFFFF 
!
! AR_BEG=`SS  AR_END=`SS       AR_HT=`FFFFFFFFFFF  AR_LEN=`FFFFFFFFFFF 
!
! PATH_ATBM=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
!
!   FRTB=`FFFFFFFFFFF 
!<PARMS HDR_LAB[/XST/YST]>
!<PARMS NAM_LAB[/XST/YST]>
!<PARMS CS_LAB[/XST/YST]>
!<PARMS TRCE_ARRAYSET[/XST/YST]>
!<PARMS PATH_ATBM[/ML=128/XST]>
!<NS Hardcopy_Related_Parameters/NC=80>
!
!       Hardcopy Parameters  
!   
!
!
! DEV_LOC=`CCCCCC    QUALITY= `CCCC     OPT_DEV=`CCCCC 
!
!                     COPIES= `IIIIIIII    INIT=`SS 
!
! FOLD=`CC  
!
!<NS Color Bar and Amplitude Parameters/NC=80>
!
!          Color Bar and Amplitude Related Parameters  
!
! PATH_CBAR=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
!
! BARS=`CCCCCCCCC   OPT_CBAR=`CCCCCCCCCC  VMIN=`IIIIIIII  VMAX=`IIIIIIII 
!
!  PNC=`FFFFFFFF        PPC =`FFFFFFFFF   PPLC=`FFFFFFF   FRCB=`FFFFFFF 
!
! LAB_CB[/YST]                                      CS_CBL[/YST]  
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS          `FFFFFFFFFFF  
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS          `FFFFFFFFFFF  
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS          `FFFFFFFFFFF   
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS          `FFFFFFFFFFF   
!
!  OPT_SCALE=`CCCCC  AMPL_USER=`FFFFF  OPT_CALC=`CCCCC  NTR_CALC=`IIIIIII 
!<NS Panel Parameters/NC=80>
!
!   Panel Parameters  
!
! NTPP=`IIIIIIII  NROW=`IIIIIIII 
!
! PCOL     PROW     PLBL 
! `IIIIIIII`IIIIIIII`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
! `IIIIIIII`IIIIIIII`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
! `IIIIIIII`IIIIIIII`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
! `IIIIIIII`IIIIIIII`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
! `IIIIIIII`IIIIIIII`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
!
! CS_PL=~~~`FFFFFF   
!<PARMS PCOL_ARRAYSET[/XST/YST]>
!<NS Timing_Line_Parmeters/NC=80>
!
!            Timing Line Parameters  
!
!MILS TLDOTS     CS_TL=`FFFFFFFFFFF     OPT_TICS=`CCCC  
!10   `IIIII 
!50   `IIIII      
!100  `IIIII 
!400  `IIIII 
!500  `IIIII 
!1000 `IIIII 
!<PARMS TLDOTS[/XST/YST]>
!<NS Block Boundaries,Tie Lines/NC=80>
!
!                 Block Boundaries, Tie Lines  
!
! PATH_BLK=~`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
!
! CS_BL=~~~~~`FFFFFF 
!
! PATH_TIE1=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
!
! PATH_TIE2=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
!
! CS_LT=~~~~~`FFFFFFFFFFF 
!
!
!
!<PARMS PATH_BLK[/ML=128/XST]>
!<PARMS PATH_TIE1[/ML=128/XST]>
!<PARMS PATH_TIE2[/ML=128/XST]>
!<NS Dashed Line Plot/NC=80>
!
!    Dashed Line Plot  
!
!     HDR_DL=~~`I    DLREF=~~~`FFF    DLU100=~~~`FFF 
!
!<NS Vertical and Side Labels/NC=80>
!
!         Vertical and Side Labels  ARE NOT YET IMPLEMENTED
!
!    UPS=`FFFFFFFF 
!
! TIM_INIT VL_INIT  TIM_INC  VL_INC   VL_TOT   CS_VL    VL_ROW  
! `FFFFFFFF`FFFFFFFF`FFFFFFFF`FFFFFFFF`IIIIIIII`FFFFFFFF`IIIIIIII 
! `FFFFFFFF`FFFFFFFF`FFFFFFFF`FFFFFFFF`IIIIIIII`FFFFFFFF`IIIIIIII 
! `FFFFFFFF`FFFFFFFF`FFFFFFFF`FFFFFFFF`IIIIIIII`FFFFFFFF`IIIIIIII 
! `FFFFFFFF`FFFFFFFF`FFFFFFFF`FFFFFFFF`IIIIIIII`FFFFFFFF`IIIIIIII 
! `FFFFFFFF`FFFFFFFF`FFFFFFFF`FFFFFFFF`IIIIIIII`FFFFFFFF`IIIIIIII 
! `FFFFFFFF`FFFFFFFF`FFFFFFFF`FFFFFFFF`IIIIIIII`FFFFFFFF`IIIIIIII 
!
!
!       SLTIM SLBL                    SLSZ  SLROW  
!       `FFFFF`SSSSSSSSSSSSSSSSSSSSSSS`FFFFF`IIII 
!       `FFFFF`SSSSSSSSSSSSSSSSSSSSSSS`FFFFF`IIII 
!<PARMS TIM_INIT_ARRAYSET[/XST/YST]>
!<PARMS SLTIM_ARRAYSET[/XST/YST]>
!<NS Tone Parameters/NC=80>
!
!              Tone Plot Parameters  ARE NOT YET IMPLEMENTED
!
! NCL_AT1=`IIIIII    DIST_OVL=`CC   OPT_TBAR1=`CCC 
!
! NCL_AT2=`IIIIII                   OPT_TBAR2=`CCC 
!
!    LAB1=`SSSSSSSSSSSSSSSSSSSS 
!
!    LAB2=`SSSSSSSSSSSSSSSSSSSS 
!                                                  
!<NS Trace Overlay Parameters/NC=80>
!
!     Trace Overlay Parameters  
!
! PATH_OVL=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
!
! CT_OVL=`FFFFF  NTR_OVL=`IIIIIIII    TPI_OVL=`FFFFFFFF  RATIO_TPI=`IIIIII 
!
!  NCTFO=`IIIIII  WT_OVL=`CC    SKIP_INIT_OVL=`IIIIIII      RP_OVL=`CC 
!
!                 VA_OVL=`CC           ZT_OVL=`FFFFFFF 
!
!<PARMS PATH_OVL[/ML=128/XST]>
!<NS Semblace Panel/NC=80>
!
!            Semblance Panel Parameters  ARE NOT YET IMPLEMENTED
!
! OPT_SEMB=`CCCCC 
!
! PATH_VEL=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
!
! COOR_BEG=`FFFFFFF  COOR_END=`FFFFFFF  SCALE_VEL=`FFFFFFF 
!<PARMS PATH_VEL[/ML=128/XST]>
!<NS Automatic Positioning of Annotation/NC=80>
!
!               Automatic Positioning of Annotation  
!
! CALD=`CCC           SDAS=`FFFFFFFFFFF    SCDA=`FFFFFFFFFFF  
!
! TIDA=`FFFFFFFFFFF   ARDA=`FFFFFFFFFFF   
!</gui_def>
!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!
!<HelpSection>
!
!     -------------------  Basic Plot Parameters  ------------------------
!
!    Basic plot parameters are those required to produce the simplest plots. 
!
! 
!<Help KEYWORD="TIME">
!<Tip> Maximum trace time to plot. </Tip>
! Default = 5.0
! Allowed = real > 0.0
! TIME*IPS + SDAS cannot exceed the maximum plot height.  Maximum plot height 
! is 30 inches for HP plotters and 35 inches for Versatec plotters.
!</Help>
!
!<Help KEYWORD="IPS">
!<Tip> Vertical trace scaling - increment (in. or cm) per second. </Tip>
! Default = 5.0
! Allowed = real > 0.0
! The maximum value of IPS is 
!
!         (number of samples per second)*(65535+0.01)/(RES*4096.0),
!
! where RES is the resolution (either 200 or 400 lines per inch).
!</Help>
!
!<Help KEYWORD="TPI">
!<Tip> Lateral trace spacing - traces per increment (in. or cm). </Tip>
! Default = 10.0
! Allowed = real > 0.0
! TPI cannot be less than 2.0 traces per inch.
! TPI cannot exceed 0.5*(plot resolution).
!</Help>
!
!<Help KEYWORD="LRRL">
!<Tip> Plot traces left to right (LR) or right to left (RL). </Tip>
! Default = RL
! Allowed = LR, RL
!</Help>
!
!<Help KEYWORD="NUM_TR">
!<Tip> Maximum number of traces to plot. </Tip>
! Default = 100
! Allowed = 12000 >= int > 0
! NUM_TR/TPI cannot exceed the maximum plot length.  Maximum plot length is 200
! inches for CONPLOT.
!</Help>
!
!<Help KEYWORD="SID">
!<Tip> Plot annotation text fields (Section IDentification). </Tip>
! Default = -
! Allowed = char(40) array of 4
! The characters ", = * ( )" cannot be used in a SID entry.  An array of four 
! identical SID fields is provided.
!</Help>
!
!<Help KEYWORD="CS_SID">
!<Tip> Character size for SID labels, in inches or cm. </Tip>
! Default = 0.27 inch
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="GRADE_HOR">
!<Tip> Grade horizontally to 100 traces per inch? </Tip>
! Default = YES
! Allowed = YES/NO
! The GRD_HOR option interpolates input data in the horizontal direction to 100
! samples per inch.
!</Help>
!
!<Help KEYWORD="GRADE_VRT">
!<Tip> Grade vertically to 100 samples per inch? </Tip>
! Default = YES
! Allowed = YES/NO
! The GRD_VRT option interpolates input data in the vertical direction to 100
! samples per inch.
!</Help>
!
!<Help KEYWORD="INVERT">
!<Tip> Invert plotted traces so time increases from bottom to top. </Tip>
! Default = NO
! Allowed = YES/NO
! If INVERT = YES, timing lines and timing line numbers will also increase from 
! bottom to top.
!</Help>
!
!<Help KEYWORD="RP">
!<Tip> Reverse polarity option - multiply samples by -1 before plotting? </Tip>
! Default = NO
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="SKIP_INIT">
!<Tip> Number of traces to skip initially for plotting. </Tip>
! Default = 0
! Allowed = int >= 0
!</Help>
!
!
!   -------------------  Basic Annotation Parameters  ------------------------
!
!<Help KEYWORD="SCALE">
!<Tip> Lateral distance scale marker (mile or kilometer marker). </Tip>
! Default = NO
! Allowed = YES/NO or real
! SCALE = YES plots a mile or kilometer marker above the trace plot using the 
! difference between header word 17 values of the first two traces to determine
! the size of the marker. 
!
! SCALE = NO doesn't plot distance scale marker.
!
! SCALE = a number interprets the number as the difference between header word 
! 17 values of two adjacent traces (in feet or meters).
!</Help> 
!
!<Help KEYWORD="CS_SCALE">
!<Tip> Character size for distance scale marker, in inches or cm. </Tip>
! Default = 0.13 inch
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="HDR_LAB">
!<Tip> Header word to plot as a trace label above section. </Tip>
! Default = 7
! Allowed = 0 - NWIH, array of 2
! Two automatic trace labels are allowed, HDR_LAB(A) and HDR_LAB(B).  Label(A) 
! is plotted nearest the trace plot and label(B) is plotted above label(A).
!
! If either of HDR_LAB(A, B) = 0, do not plot that label. 
!</Help> 
!
!<Help KEYWORD="LAB_INIT">
!<Tip> Trace number for the first automatic trace label. </Tip>
! Default = 50
! Allowed = int (1 to NUM_TR)
! Allowed = SHOT
! If LAB_INIT = an integer, the first automatic trace label will be plotted 
! above that trace number.
!
! If LAB_INIT = SHOT, HDR_LAB(A) must be zero and HDR_LAB(B) labels are plotted
! above manual shotpoint labels.
!</Help>
!
!<Help KEYWORD="LAB_INC">
!<Tip> Trace number increment between automatic trace labels. </Tip>
! Default = 50
! Allowed = int > 0
! LAB_INIT and LAB_INC have the same function on each panel if the panel option
! is chosen, i.e. the pattern starts over for each new panel.
!</Help>
!
!<Help KEYWORD="NAM_LAB">
!<Tip> Name to plot adjacent to automatic or manual trace labels. </Tip>
! Default = -
! Allowed = char(16) array of 2
! NAM_LAB is an array of names to plot adjacent to automatic or manual trace 
! labels to identify them.  The array has two elements.
!</Help>
!
!<Help KEYWORD="CS_LAB">
!<Tip> Character size for trace label(A-B), in inches or cm. </Tip>
! Default = 0.13 inch
! Allowed = real > 0.0 array of 2
!</Help>
!
!<Help KEYWORD="LTAB">
!<Tip> Plot automatic or manual trace labels on top and bottom of plot. </Tip>
! Default = NO
! Allowed = YES/NO
! If LTAB = NO, then plot labels on top of section only.
!</Help>
!
!<Help KEYWORD="OPT_VL">
!<Tip> Plot vertical lines under labels. </Tip>
! Default = NO
! Allowed = YES  (Plot a vertical line under the trace labels.)
! Allowed = NO   (No marking under the trace labels.)
!</Help>
!
!<Help KEYWORD="WID_VL">
!<Tip> Width of vertical lines under labels, in inches or cm. </Tip>
! Default = 0.005 inch
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="AR_BEG">
!<Tip> Label for the arrow pointing toward the start of the plot. </Tip>
! Default = -
! Allowed = char(3)
! Normally strings such as N, S, SE are used. If AR_BEG is blank, no arrow is 
! plotted.
!</Help>
!
!<Help KEYWORD="AR_END">
!<Tip> Label for the arrow pointing toward the end of the plot. </Tip>
! Default = -
! Allowed = char(3)
! Normally strings such as N, S, SE are used.  If AR_END is blank, no arrow is 
! plotted.
!</Help>
!
!<Help KEYWORD="AR_HT">
!<Tip> Height of the direction arrow head. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! If AR_HT = 0.0, then arrow head height will be calculated as 0.035 * IPS.
!</Help>
!
!<Help KEYWORD="AR_LEN">
!<Tip> Length of the direction arrow. </Tip>
! Default = 2.5 inch
! Allowed = real > 0.0
! Typical values are 2.5 inches or 6.35 cm.
!</Help>
!
!<Help KEYWORD="PATH_ATBM">
!<Tip> Pathname for file containing the automatic title block master. </Tip>
! Default = NONE
! Allowed = char
! If PATH_ATBM /= NONE and the filename has the extension .atbm, COLOR will 
! automatically plot a title block based on the history file and the automatic 
! title block master file.  If the extension is not .atbm, the contents of the 
! file will be plotted as text in the title block space. 
!</Help>
!
!<Help KEYWORD="FRTB">
!<Tip> Factor for reducing the title block size. </Tip>
! Default = 1.0
! Allowed = 1.0 >= real > 0.0
! If your title block is too long to be plotted, COLOR will automatically reduce
! the size to avoid running off the bottom of the plot.
!</Help>
! 
!     --------------- Manual Shotpoint-style Annotation -------------------
!
!      TRCE,SHOT,INTV,INCR,TOTL are linked arrays 
!      for shotpoint labels. There are 50 labels available.
!
!<Help KEYWORD="TRCE">
!<Tip> Manual shot point labels  </Tip>
! Default = -
! Allowed = int linked array(50)
! Trace number to shart shot point label.
! Numbers may range from 1 to NUM_TR
!</Help>
!
!<Help KEYWORD="SHOT">
!<Tip> Manual shot point labels  </Tip>
! Default = -
! Allowed = char linked array(50)
! First shot point to label
! 8 characters may be used per shot.  Do not use , = ( ) or *.
!</Help>
!
!<Help KEYWORD="INTV">
!<Tip> Manual shot point labels.  </Tip>
! Default = -
! Allowed = integer linked array(50)
! The number of trace intervals between shot point labels.
!</Help>
!
!<Help KEYWORD="INCR">
!<Tip> Manual shot point labels.  </Tip>
! Default = -
! Allowed = integer linked array(50)
! The shot point increment to the next label.
!</Help>
!
!<Help KEYWORD="TOTL">
!<Tip> Manual shot point labels.  </Tip>
! Default = -
! Allowed = integer linked array(50)
! The total number of shot points to do.
!</Help>
!
!<Help KEYWORD="CS_SPL">
!<Tip> Shotpoint label character size.  </Tip>
! Default = 0.15 inch
! Allowed = real > 0.0
!</Help>
!
!
!   -------------------  Hardcopy Related Parameters  ------------------------
!
!<Help KEYWORD="DEV_LOC">
!<Tip> Local device to use for plotting. </Tip>
! Default = HP5000A(HP 5000 )
! Allowed = HP     (HP 650)
! Allowed = TEST   (Run COLOR but write .rpt file only and don't make a plot.)
! Allowed = SAVECGM(Save cgm file in current directory; do not plot.) 
!</Help>
!
!<Help KEYWORD="QUALITY">
!<Tip> Quality for HP5000 plots.  </Tip>
! Default = PROD
! Allowed = PROD   
! Allowed = MAX
! Allowed = FAST    
! Active only if DEV_LOC = HP5000. PROD = production quality, normal speed.
! MAX = Maximum quality, slowest speed.  FAST = poorest quality, fast speed.
!</Help>
!
!<Help KEYWORD="OPT_DEV">
!<Tip> Options for HP5000. </Tip>
! Default = PAPER
! Allowed = PAPER  
! Allowed = FILM    
! Allowed = GLOSS  (GLOSS PAPER)
! Active only if DEV_LOC = HP5000A.
!</Help>
!
!<Help KEYWORD="COPIES">
!<Tip> Number of copies to make of this plot. </Tip>
! Default = 1
! Allowed = 1 - 99
!</Help>
!
!<Help KEYWORD="INIT">
!<Tip> Processor or technician initials for plot identification. </Tip>
! Default = -
! Allowed = char(3)
! Initials are plotted at the end of the plot in the identification section so
! operators can determine which are your plots.
!</Help>
!
!<Help KEYWORD="FOLD">
!<Tip> Whether operators should fold this plot. </Tip>
! Default = NO
! Allowed = YES/NO
!</Help>
!
!
!  -------------  Color Bar and Amplitude Related Parameters  ------------------
!
!<Help KEYWORD="PATH_CBAR">
!<Tip> Pathname for file containing a color bar or tone bar. </Tip>
! Default = NONE
! Allowed = char      (To designate a pathname.)
! Colorbar files contain color codes and corresponding amplitude values and must
! be built by the BBAR utility.  Specified amplitude values are the amplitudes
! associated with transitions between adjacent colors in the colorbar.
!
! For TONE plots (color plots using a two-dimensional color bar) the color bar
! (tone bar) must be built by the TBAR utility.
!</Help>
!
!<Help KEYWORD="BARS">
!<Tip> Choice of color bar stored within COLOR. </Tip>
! Default = STAN
! Allowed = AVO133    (Standard AVA color bar.)
! Allowed = BLUERED   (Blue and red color bar, a.k.a. duochrome.)
! Allowed = BLUEWHT   (Blue and white color bar.)
! Allowed = MEDRAM    (Standard median RAM color bar.)
! Allowed = MEDRAM6   (Standard non-Conoco median RAM color bar.)
! Allowed = STAN      (Standard pseudo-spectrum color bar.)
! Allowed = SVA       (Standard semblance velocity color bar.)
! Active only if PATH_CBAR = NONE.
!</Help>
!
!<Help KEYWORD="OPT_CBAR">
!<Tip> Method to use for specifying amplitude values for color bar. </Tip>
! Default = CBAR
! Allowed = CBAR        (Use the amplitude values in the color bar.)
! Allowed = VMIN_VMAX   (Use the VMIN, VMAX parameters.)
! Allowed = PCTL        (Use the percentile parameters: PNC, PPC, PPLC.)
!
! If OPT_CBAR = VMIN_VMAX or PCTL, then the parameters OPT_CALC and NTR_CALC 
! may constrain the data used in the percentile calculation.
!</Help>
!
!<Help KEYWORD="VMIN">
!<Tip> Minimum amplitude value for colorbar. </Tip>
! Default = -
! Allowed = real < VMAX
! VMIN and VMAX specify the amplitude values that will be used instead of the 
! smallest and the largest amplitude values in the colorbar.  Intermediate 
! amplitude values are automatically chosen so that the amplitude increments 
! are equal.
!</Help>
!
!<Help KEYWORD="VMAX">
!<Tip> Maximum amplitude value for colorbar. </Tip>
! Default = -
! Allowed = real > VMIN
! VMIN and VMAX specify the amplitude values that will be used instead of the 
! smallest and the largest amplitude values in the colorbar.  Intermediate 
! amplitude values are automatically chosen so that the amplitude increments 
! are equal.
!</Help>
!
!<Help KEYWORD="PNC">
!<Tip> Set VMIN at the PNC percentile of the negative amplitude range. </Tip>
! Default = -
! Allowed = real 0.0 - 100.0
! If PNC = 90.0, then VMIN will be set so that 90% of the negative samples are 
! greater algebraically than VMIN.
!</Help>
!
!<Help KEYWORD="PPC">
!<Tip> Set VMAX at the PPC percentile of the positive amplitude range. </Tip>
! Default = -
! Allowed = real 0.0 - 100.0
! If PPC = 90.0, then VMAX will be set so that 90% of the positive samples are 
! smaller algebraically than VMAX.
!</Help>
!
!<Help KEYWORD="PPLC">
!<Tip> Set VMIN at the PPLC percentile of the positive amplitude range. </Tip>
! Default = -
! Allowed = real 0.0 - 100.0
! If PPLC = 30.0, then VMIN will be set so that 30% of the positive samples are 
! smaller algebraically than VMIN.  
!
! PPLC is active only if PNC = 0.0
!</Help>
!
!<Help KEYWORD="FRCB">
!<Tip> Factor for reducing the plotted colorbar size. </Tip>
! Default = 1.0
! Allowed = 1.0 >= real > 0.0
! If FRCB = 1.0, each color patch in the plotted colorbar will 1/2 inch by 1/4
! inch.
!</Help>
!
!<Help KEYWORD="LAB_CB">
!<Tip> Labels to plot above the colorbar; four are available. </Tip>
! Default = NONE
! Allowed = char (20), array of 4
! Four labels are available above the colorbar with LAB_CB(1) the highest and 
! LAB_CB(4) the lowest.  Any or all may be used.  Normally the name of the 
! colorbar is the entry.
!</Help>
!
!<Help KEYWORD="CS_CBL">
!<Tip> Character size for colorbar labels (1-4), in inches or cm. </Tip>
! Default = 0.13 inch
! Allowed = real > 0.0, array of 4
!</Help>
!
!<Help KEYWORD="OPT_SCALE">
!<Tip> Option to use for median scaling of the input data. </Tip>
! Default = NONE
! Allowed = NONE  (Do not scale the input data.)
! Allowed = CALC  (Do a calculated median scale.)
! Allowed = USER  (Do a scale based on AMPL_USER.)
! If OPT_SCALE = CALC, then calculate the median of the absolute amplitude of 
! the input dataset and scale the dataset to make the median absolute amplitude
! equal to unity.  (That is, divide by the calculated median absolute 
! amplitude.)
!
! If OPT_SCALE = USER, then scale the dataset by dividing by AMPL_USER.
!
! Parameters OPT_CALC and NTR_CALC may constrain the data used in the median
! calculation.
!</Help>
!
!<Help KEYWORD="AMPL_USER">
!<Tip> User specified amplitude value for OPT_SCALE = USER. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="OPT_CALC">
!<Tip> Option to use for calculations such as percentile and scaling. </Tip>
! Default = PLOT
! Allowed = PLOT  (Use input traces from TSTRT to TIME for calculations.)
! Allowed = ALL   (Use full length of input traces for calculations.)
! Option to use for percentile, scaling and distribution calculations.
!
! If OPT_CALC = PLOT only the input traces samples in the time range from TSTRT
! to TIME will be used in the percentile and scaling calculations.  Otherwise 
! the entire trace length will be used.
!
! In all cases samples in the mute zones will be excluded from the calculations.
!</Help>
!
!<Help KEYWORD="NTR_CALC">
!<Tip> Number of traces to use for calculations. </Tip>
! Default = 0
! Allowed = NUM_TR >= int > 0
! Number of traces to use for percentile, scaling and distribution calculations.
! Must answer if OPT_CALC=ALL.
!
! If NTR_CALC < NUM_TR, then the first NTR_CALC traces will be used for 
! percentile and scaling calculations.
!</Help>
!
!
!    ---------------  Panel Parameters  --------------------
!
!<Help KEYWORD="NTPP">
!<Tip> Number of traces per panel.  NTPP = 0 means do not use panels. </Tip>
! Default = 0
! Allowed = int < NUM_TR
!</Help>
!
!<Help KEYWORD="NROW">
!<Tip> Number of rows of panels to plot.  </Tip>
! Default = 1
! Allowed = 1 - 26
!</Help>
!
!        PCOL,PROW,PLBL are linked array prompts for panel labels.
!          These may be used only if NTPP > 0.
!          100 labels are available.
!
!<Help KEYWORD="PCOL">
!<Tip> Column number of the panel you want to label.  </Tip>
! Default = -
! Allowed = int
!</Help>
!
!<Help KEYWORD="PROW">
!<Tip> Row number of the panel you want to label.  </Tip>
! Default = -
! Allowed = int
!</Help>
!
!<Help KEYWORD="PLBL">
!<Tip> Label for panel.  </Tip>
! Default = -
! Allowed = char (32)
! If PLBL begins with a period followed by a blank (. ), then that label will 
! not be plotted.
!</Help>
!
!<Help KEYWORD="CS_PL">
!<Tip> Panel label character size.  </Tip>
! Default = 0.3
! Allowed = 0.05 - 0.3 inches
! Suggested values are 0.3 inches, 0.75 cm.
!</Help>
!
!
!      ------------------- Timing Line Parameters -----------------------
!
!<Help KEYWORD="TLDOTS">
!<Tip> Number of dots to use for as many as 6 levels of timing lines.  </Tip>
! Default = 0, 0, 1, 0, 0, 2
! Allowed = 0 - 4, array of 6
! A zero entered for any level causes that level to be omitted in plotting.
!
!            Line Spacing       Default
!       10 mil      0 dots
!       50      0
!       100     1
!       400     0
!       500     0
!       1000        2
!</Help>
!
!<Help KEYWORD="CS_TL">
!<Tip> Timing line character size.  </Tip>
! Default = .236
! Allowed = real
!</Help>
!
!<Help KEYWORD="OPT_TICS">
!<Tip> Whether to plot tics rather than timing lines.  </Tip>
! Default = NO
! Allowed = YES  (Plot tics.)
! Allowed = NO   (Plot timing lines.)
!</Help>
!
!
!      --------------- Block Boundaries and Line-ties -------------------
!
!<Help KEYWORD="PATH_BLK">
!<Tip> Pathname of file containing block boundary information.  </Tip>
! Default = -
! Allowed = char
! File must be built with the BBLK utility.
!</Help>
!
!<Help KEYWORD="CS_BL">
!<Tip> Block label character size. </Tip>
! Default = 0.3
! Allowed = 0.05 - 0.3 inches
! Suggested size is 0.3 inches or 0.762 cm.
!</Help>
!
!<Help KEYWORD="PATH_TIE1">
!<Tip> Pathname of file containing line-tie information.  </Tip>
! Default = -
! Allowed = char
! File must be built with the BTIE utility.
!</Help>
!
!<Help KEYWORD="PATH_TIE2">
!<Tip> Pathname of the second file containing line-tie information.  </Tip>
! Default = -
! Allowed = char
! File must be built with the BTIE utility.  Information in this file will be 
! plotted above that from PATH_TIE1.
!</Help>
!
!<Help KEYWORD="CS_LT">
!<Tip> Line-tie character size.  </Tip>
! Default = 0.07
! Allowed = real > 0.0
! Suggested size is 0.07 inches or 0.18 cm.
!</Help>
!
!
!      --------------- Dashed Line Plot Parameters -------------------
!
!<Help KEYWORD="HDR_DL">
!<Tip> Header word to use for dashed line plot across top of section.  </Tip>
! Default = 0    
! Allowed = 0 - NWIH
! If HDR_DL = 0, then do not make a dashed line plot.
! Word 19 (elevation) is recommended.
!</Help>
!
!<Help KEYWORD="DLREF">
!<Tip> Reference number to plot at zero time or top of section for HDR_DL.</Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="DLU100">
!<Tip> Header word value increment to associate with 100 ms for HDR_DL.  </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!
!
!          --------------- Vertical Label Annotation -------------------
!
!
!<Help KEYWORD="UPS">
!<Tip> Units per seconds for labeling section in units other than time.  </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! If UPS = 0.0, then label in seconds.
!</Help>
!
!
!  Parameters TIM_INIT,VL_INIT,TIM_INC,VL_INC,VL_TOT,CS_VL and VL_ROW are 
!  linked arrays to be used if you wish to build your own vertical labels
!  where the time or US numbers would ordinarily be plotted.
!
!<Help KEYWORD="TIM_INIT">
!<Tip> Time in seconds for the first label.  </Tip>
! Default = 0.0
! Allowed = 0.0 - TIME
!</Help>
!
!<Help KEYWORD="VL_INIT">
!<Tip> Number to use for the first label.  </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="TIM_INC">
!<Tip> Time Increment, in seconds, between labels.  </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="VL_INC">
!<Tip> Number increment between labels.  </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="VL_TOT">
!<Tip> Total number of vertical labels to plot.  </Tip>
! Default = 10
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="CS_VL">
!<Tip> Vertical label character size.  </Tip>
! Default = 0.24 inch
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="VL_ROW">
!<Tip> Row number for vertical labels on paneled plots.  </Tip>
! Default = 1
! Allowed = int =< NROW
!</Help>
!
!  Parameters SLTIM,SLBL,SLSZ, and SLROW are linked arrays for side
!  labels. These labels are plotted to the left side of the data,
!  between the color bar and data, at times you specify.
!
!<Help KEYWORD="SLTIM">
!<Tip> Time associated with the side label.  </Tip>
! Default = 1.0
! Allowed = 0.0 - TIME
!</Help>
!
!<Help KEYWORD="SLBL">
!<Tip> Text for the side label.  </Tip>
! Default = -
! Allowed = char (24)
! You cannot use "," or "()".
!</Help>
!
!<Help KEYWORD="SLSZ">
!<Tip> Side label character sizw.  </Tip>
! Default = 0.15 inch
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="SLROW">
!<Tip> Row number for side label on paneled plots.  </Tip>
! Default = 1
! Allowed = int =< NROW
!</Help>
!
!
!       ------------------ Tone Plot Parameters -------------------
!
!<Help KEYWORD="NCL_AT1">
!<Tip> Number of colors in the tone bar for the first attribute.  </Tip>
! Default = 0
! Allowed = 0 - 21
! The first attribute refers to the odd traces and the horizontal axis of the 
! tone bar (2D color bar).  NCL_AT1 is the number of colors in the horizontal
! direction of the tonebar.  The tone bar must be built by the TBAR utility.
!
! NCL_AT1 must be greater than 0 for COLOR to use a tone bar.
!</Help>
!
!<Help KEYWORD="NCL_AT2">
!<Tip> Number of colors in the tone bar for the second attribute.  </Tip>
! Default = 1
! Allowed = 1 - 21
! The second attribute refers to the even traces and the vertical axis of the 
! tone bar (2D color bar).  NCL_AT2 is the number of colors in the vertical
! direction of the tonebar.  The tone bar must be built by the TBAR utility.
!</Help>
!
!<Help KEYWORD="DIST_OVL">
!<Tip> Overlay attribute occupation percentages on the tonebar?  </Tip>
! Default = YES
! Allowed = YES/NO
! COLOR will analyze the occupation of tone bar color patches by attribute 
! pairs.  These occupation percentages can be overlaid on the plotted tone bar 
! itself. 
!</Help>
!
!<Help KEYWORD="OPT_TBAR1">
!<Tip> Method to use for specifying attribute 1 amplitudes for tone bar. </Tip>
! Default = CONT
! Allowed = CONT        (Fit distribution function by continuous method.)
! Allowed = DSCR        (Fit distribution function by discrete method.)
! Allowed = TBAR        (Use the amplitude values in the tone bar.)
! If OPT_TBAR1 = CONT or DSCR, then COLOR will estimate the distribution 
! function of attribute 1 and set tone bar amplitudes so that color patch 
! occupation percentages approximate the estimated distribution function.
! 
! The discrete method (DSCR) uses the normal binning method to estimate the 
! distribution function.  The continuous method (CONT) creates a smoother but 
! less accurate estimate.
!
! If OPT_TBAR1 = CONT or DSCR, then the parameters OPT_CALC and NTR_CALC 
! may constrain the data used in the distribution calculation.
!</Help>
!
!<Help KEYWORD="OPT_TBAR2">
!<Tip> Method to use for specifying attribute 2 amplitudes for tone bar. </Tip>
! Default = CONT
! Allowed = CONT        (Fit distribution function by continuous method.)
! Allowed = DSCR        (Fit distribution function by discrete method.)
! Allowed = TBAR        (Use the amplitude values in the tone bar.)
! If OPT_TBAR2 = CONT or DSCR, then COLOR will estimate the distribution 
! function of attribute 2 and set tone bar amplitudes so that color patch 
! occupation percentages approximate the estimated distribution function.
! 
! The discrete method (DSCR) uses the normal binning method to estimate the 
! distribution function.  The continuous method (CONT) creates a smoother but 
! less accurate estimate.
!
! If OPT_TBAR2 = CONT or DSCR, then the parameters OPT_CALC and NTR_CALC 
! may constrain the data used in the distribution calculation.
!</Help>
!
!<Help KEYWORD="LAB1">
!<Tip> Label for attribute 1 (horizontal axis) on the tonebar.  </Tip>
! Default = -
! Allowed = char (21) 
!</Help>
!
!<Help KEYWORD="LAB2">
!<Tip> Label for attribute 2 (vertical axis) on the tonebar.  </Tip>
! Default = -
! Allowed = char (21) 
!</Help>
!
!
!       ---------------- Trace Overlay Parameters -------------------
!
!<Help KEYWORD="PATH_OVL">
!<Tip> Pathname for file containing traces for an overlay plot.  </Tip>
! Default = NONE
! Allowed = char
! If PATH_OVL = NONE, then do not make a trace overlay plot.
!</Help>
!
!<Help KEYWORD="CT_OVL">
!<Tip> Plotting gain - LAV is plotted as CT color trace channels. </Tip>
! Default = 5.0
! Allowed = real > 0.0
! The largest absolute value (LAV) of the overlay traces is plotted as CT/2 
! channels, where a channel is the plotted lateral color trace spacing.  (Since
! it is presumed that the sample with the LAV is near another with similar 
! amplitude and opposite polarity, the pair plot as CT channels.)
!</Help>
!
!<Help KEYWORD="NTR_OVL">
!<Tip> Number of overlay traces to be plotted. </Tip>
! Default = 1
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="TPI_OVL">
!<Tip> Number of traces per increment (inches or cm) for overlay traces. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! Overlay trace spacing can be specified either by TPI_OVL or RATIO_TPI.
! RATIO_TPI is active only when TPI_OVL = 0.0
!</Help>
!
!<Help KEYWORD="RATIO_TPI">
!<Tip> Ratio of color trace spacing to overlay trace spacing. </Tip>
! Default = 4
! Allowed = integer > 0
! If RATIO_TPI = 4, then the overlay traces are 4 times farther apart than 
! the color traces.
!
! Active only if TPI_OVL = 0.0.
!</Help>
!
!<Help KEYWORD="NCTFO">
!<Tip> Number of color trace to plot the first overlay trace. </Tip>
! Default = 1
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="WT_OVL">
!<Tip> Whether to plot a wiggle trace as an overlay trace. </Tip>
! Default = YES
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="VA_OVL">
!<Tip> Whether to plot a variable area trace as an overlay trace. </Tip>
! Default = YES
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="SKIP_INIT_OVL">
!<Tip> Number of overlay traces to skip before plotting. </Tip>
! Default = 0
! Allowed = int < NTR_OVL
!</Help>
!
!<Help KEYWORD="RP_OVL">
!<Tip> Whether to reverse the polarity of overlay traces. </Tip>
! Default = NO
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="ZT_OVL">
!<Tip> Time on overlay traces to be plotted at zero time of color traces. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!
!       --------------- Semblance Panel Parameters -------------------
!
!
!<Help KEYWORD="OPT_SEMB">
!<Tip>Whether input traces are normal seismic traces or semblance traces. </Tip>
! Default = NORMAL
! Allowed = NORMAL   (Input traces are normal seismic traces.)
! Allowed = SEMB     (Input traces are semblance traces.)
! If OPT_SEMB = SEMB, then input traces are interpreted as semblance traces 
! from the SVA process and a special semblance panel plot is produced.
!</Help>
!
!<Help KEYWORD="PATH_VEL">
!<Tip> Pathname of the velocity file containing functions to plot.  </Tip>
! Default = NONE
! Allowed = char
! Velocity functions contained in the velocity file will be plotted as a 
! velocity pick overlay over the semblance plot.  
!
! If PATH_VEL = NONE, then do not overlay velocity picks on the semblance plot.
!
! Active only if OPT_SEMB = SEMB.
!</Help>
!
!<Help KEYWORD="COOR_BEG">
!<Tip> First value of velocity file x-coordinate to use for overlay. </Tip>
! Default = 0.0
! Allowed = real
! If COOR_BEG = 0.0, then use the first x-coordinate value in the file.
!
! Active only if OPT_SEMB = SEMB.
!</Help>
!
!<Help KEYWORD="COOR_END">
!<Tip> Last value of velocity file x-coordinate to use for overlay. </Tip>
! Default = 0.0
! Allowed = real > COOR_BEG
! If COOR_END = 0.0, then use the last x-coordinate value in the file.
!
! Active only if OPT_SEMB = SEMB.
!</Help>
!
!<Help KEYWORD="SCALE_VEL">
!<Tip> Scale to use for velocity axis annotation. </Tip>
! Default = 2000.0
! Allowed = real > 0.0
! SCALE_VEL is defined as the velocity increment (in feet/sec or meters/sec) 
! per display unit (inches or cm).  Use of English or metric system is based on
! the Project_Data parameter.  Labels are located wherever the velocity axis 
! value is an even multiple of 1000.
!
! Active only if OPT_SEMB = SEMB.
!</Help>


!    --------------- Automatic Positioning of Annotation -------------------
!
!<Help KEYWORD="CALD">
!<Tip> Automatic calculation of annotation location above section. </Tip>
! Default = YES
! Allowed = YES, NO
! If CALD = YES, COLOR calculates the location of annotation elements above the 
! section automatically.
!
! If CALD = NO, you will be prompted for individual location values.
!
!</Help>
!
!<Help KEYWORD="SCDA">
!<Tip> Distance above zero time to plot the scale marker (in. or cm).  </Tip>
! Default = -
! Allowed = real > 0.0
! Active if CALD = NO.
!</Help>
!
!<Help KEYWORD="SDAS">
!<Tip> Distance above zero time to plot the first SID line (in. or cm).  </Tip>
! Default = -
! Allowed = real > 0.0
! Active if CALD = NO.
!</Help>
!
!<Help KEYWORD="TIDA">
!<Tip> Distance above the section to plot the line tie (in. or cm).  </Tip>
! Default = -
! Allowed = real >= 0.1 inch
! Active if CALD = NO.
!</Help>
!
!<Help KEYWORD="ARDA">
!<Tip> Distance of the direction arrow above the section. </Tip>
! Default = 2.5
! Allowed = real > 0.1 inch
! Typical values are 2.5 (English) or 6.35 (metric).
! Active if CALD = NO.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module color_module
      use ameq_module
      use atblk_module
      use colrplot_module
      use getlun_module
      use mem_module
      use mth_module
      use named_constants_module
      use pathcheck_module
      use pc_module
      use string_module
      use trcio_module
      implicit none
      private
      public :: color_create
      public :: color_initialize
      public :: color_update
      public :: color_delete
!<execute_only>
      public :: color            ! main execution (trace processing) routine.
      public :: color_wrapup
!</execute_only>


      character(len=100),public,save :: color_IDENT = &
       '$Id: color.f90,v 1.15 2006/12/04 13:29:51 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: color_struct              
 
        private
        logical                    :: skip_wrapup      ! wrapup flag.
        integer                    :: copies           ! process parameter
        integer                    :: tldots(6)        ! process parameter
        integer                    :: ups              ! process parameter
        integer                    :: ncl_at1          ! process parameter
        integer                    :: ncl_at2          ! process parameter
        integer                    :: skip_init        ! process parameter
        integer                    :: skip_init_ovl    ! process parameter
        integer                    :: ntr_ovl          ! process parameter
        integer                    :: nctfo            ! process parameter
        integer                    :: num_tr           ! process parameter
        integer                    :: ntpp             ! process parameter
        integer                    :: nrow             ! process parameter
        integer                    :: nhdr_lab         ! dependent var
        integer                    :: hdr_lab(2)       ! process parameter
        integer                    :: lab_inc          ! process parameter
        integer                    :: ntr_calc         ! process parameter
        integer                    :: hdr_dl           ! process parameter
        real                       :: time             ! process parameter
        real                       :: ips              ! process parameter
        real                       :: ips_eng          ! dependent var
        real                       :: cs_sid           ! process parameter
        real                       :: cs_sid_eng       ! dependent var
        real                       :: cs_spl           ! process parameter
        real                       :: cs_spl_eng       ! dependent var
        real                       :: vmin             ! process parameter
        real                       :: vmax             ! process parameter
        real                       :: cs_cbl(4)        ! process parameter
        real                       :: cs_cbl_eng(4)    ! process parameter
        real                       :: fit              ! dependent var
        real                       :: tpi              ! process parameter
        real                       :: tpi_eng          ! dependent var
        real                       :: sdas             ! process parameter
        real                       :: sdas_eng         ! dependent var
        real                       :: cs_tl            ! process parameter
        real                       :: cs_tl_eng        ! dependent var
        real                       :: wid_vl           ! process parameter
        real                       :: wid_vl_eng       ! process parameter
        real                       :: pnc              ! process parameter
        real                       :: ppc              ! process parameter
        real                       :: pplc             ! process parameter
        real                       :: cs_bl            ! process parameter
        real                       :: cs_bl_eng        ! dependent var
        real                       :: frtb             ! process parameter
        real                       :: ar_ht            ! process parameter
        real                       :: ar_ht_eng        ! dependent var
        real                       :: ar_len           ! process parameter
        real                       :: ar_len_eng       ! dependent var
        real                       :: arda             ! process parameter
        real                       :: arda_eng         ! dependent var
        real                       :: tida             ! process parameter
        real                       :: tida_eng         ! dependent var
        real                       :: cs_lt            ! process parameter
        real                       :: cs_lt_eng        ! dependent var
        real                       :: ct_ovl           ! process parameter
        real                       :: zt_ovl           ! process parameter
        real                       :: tpi_ovl          ! process parameter
        real                       :: tpi_ovl_eng      ! dependent var
        real                       :: cs_lab(2)        ! process parameter
        real                       :: cs_lab_eng(2)    ! dependent var
        real                       :: scale_vel        ! process parameter
        real                       :: frcb             ! process parameter
        real                       :: coor_beg         ! process parameter
        real                       :: coor_end         ! process parameter
        real                       :: cs_pl            ! process parameter
        real                       :: cs_pl_eng        ! dependent var
        real                       :: scda             ! process parameter
        real                       :: scda_eng         ! dependent var
        real                       :: cs_scale         ! process parameter
        real                       :: cs_scale_eng     ! dependent var
        real                       :: dlref            ! process parameter
        real                       :: dlu100           ! process parameter
        real                       :: ampl_user        ! process parameter
        character(len=FILENAME_LENGTH) :: path_cbar    ! process parameter
        character(len=FILENAME_LENGTH) :: path_atbm    ! process parameter
        character(len=FILENAME_LENGTH) :: path_blk     ! process parameter
        character(len=FILENAME_LENGTH) :: path_tie1    ! process parameter
        character(len=FILENAME_LENGTH) :: path_tie2    ! process parameter
        character(len=FILENAME_LENGTH) :: path_vel     ! process parameter
        character(len=FILENAME_LENGTH) :: path_ovl     ! process parameters.
        character(len=FILENAME_LENGTH) :: path_vect    ! process parameters.
        character(len=40)          :: sid(4)           ! process parameter
        character(len=40)          :: lab1             ! process parameter
        character(len=40)          :: lab2             ! process parameter
        character(len=32)          :: idnp             ! process parameter
        character(len=20)          :: lab_cb(4)        ! process parameter
        character(len=16)          :: nam_lab(2)       ! process parameter
        character(len=8)           :: cald             ! process parameter
        character(len=8)           :: lrrl             ! process parameter
        character(len=8)           :: init             ! process parameter
        character(len=8)           :: grade_hor        ! process parameter
        character(len=8)           :: grade_vrt        ! process parameter
        character(len=8)           :: ltab             ! process parameter
        character(len=8)           :: opt_tics         ! process parameter
        character(len=8)           :: opt_vl           ! process parameter
        character(len=8)           :: opt_scale        ! process parameter
        character(len=8)           :: opt_tbar1        ! process parameter
        character(len=8)           :: opt_tbar2        ! process parameter
        character(len=8)           :: dist_ovl         ! process parameter
        character(len=8)           :: invert           ! process parameter
        character(len=8)           :: ar_beg           ! process parameter
        character(len=8)           :: ar_end           ! process parameter
        character(len=8)           :: wt_ovl           ! process parameter
        character(len=8)           :: va_ovl           ! process parameter
        character(len=8)           :: dev_loc          ! process parameter
        character(len=8)           :: quality
        character(len=8)           :: dev_rem          ! process parameter
        character(len=32)          :: nrnp             ! process parameter
        integer                    :: ratio_tpi        ! process parameter
        character(len=8)           :: bars             ! process parameter
        character(len=8)           :: lab_init         ! process parameter
        character(len=8)           :: opt_calc         ! process parameter
        character(len=8)           :: rp               ! process parameter
        character(len=8)           :: rp_ovl           ! process parameter
        character(len=8)           :: scale            ! process parameter
        character(len=8)           :: fold             ! process parameter
        character(len=8)           :: opt_semb         ! process parameter
        character(len=12)          :: opt_cbar         ! process parameter
        character(len=8)           :: opt_dev          ! process parameter
        character(len=8)           :: irtbp            ! dependent var
        integer                    :: nshots           ! dependent var
        real,pointer               :: trce(:)          ! process parameter
        character(len=8),pointer   :: shot(:)          ! process parameter
        integer,pointer            :: intv(:)          ! process parameter
        integer,pointer            :: incr(:)          ! process parameter
        integer,pointer            :: totl(:)          ! process parameter
        integer                    :: ntistr           ! dependent var
        real,pointer               :: tim_init(:)      ! process parameter
        real,pointer               :: vl_init(:)       ! process parameter
        real,pointer               :: tim_inc(:)       ! process parameter
        real,pointer               :: vl_inc(:)        ! process parameter
        integer,pointer            :: vl_tot(:)        ! process parameter
        real,pointer               :: cs_vl(:)         ! process parameter
        real,pointer               :: cs_vl_eng(:)     ! dependent var
        integer,pointer            :: vl_row(:)        ! process parameter
        integer                    :: nslbl            ! dependent var
        real,pointer               :: sltim(:)         ! process parameter
        character(len=24),pointer  :: slbl(:)          ! process parameter
        real,pointer               :: slsz(:)          ! process parameter
        real,pointer               :: slsz_eng(:)      ! dependent var
        integer,pointer            :: slrow(:)         ! process parameter
        integer,pointer            :: pcol(:)          ! process parameter
        integer,pointer            :: prow(:)          ! process parameter
        character(len=32),pointer  :: plbl(:)          ! process parameter
        integer                    :: n64s             ! dependent var ???
        integer                    :: ntppsem          ! dependent var
        integer                    :: kntall           ! dependent var
        integer                    :: kbtskp           ! dependent var
        integer                    :: kbtdo            ! dependent var
        integer                    :: kbtbtwn          ! dependent var
        integer                    :: kbttot           ! dependent var
        character(len=8)           :: irspp            ! dependent var
        integer                    :: ntppset          ! dependent var
        integer                    :: ktpanl           ! dependent var
        integer                    :: ncol             ! dependent var
        integer                    :: iuserbt          ! dependent var
        integer                    :: kntcolr          ! dependent var
        integer                    :: irepeat          ! dependent var
        integer                    :: ncolro           ! dependent var
        integer                    :: ilfs             ! dependent var
        integer                    :: nplbl            ! dependent var
        integer                    :: interp           ! dependent var
        integer                    :: ktin             ! dependent var
        integer                    :: ntread           ! dependent var
        integer                    :: nbox             ! dependent var
        integer                    :: ntrpv            ! dependent var
        integer                    :: nvi              ! dependent var
        integer                    :: ncolr            ! dependent var
        integer                    :: ktrnum           ! dependent var
        integer                    :: ipnt             ! dependent var
        integer                    :: nsamp            ! dependent var
        integer                    :: nsamporig        ! dependent var
        integer                    :: icf              ! dependent var
        integer                    :: rgbflg           ! dependent var
        integer                    :: nrlc             ! dependent var
        integer                    :: nbts             ! dependent var
        integer                    :: nwrdsmult_double ! dependent var
        integer                    :: nwrdsmult        ! dependent var
        integer                    :: nsampmult        ! dependent var
        integer                    :: ncharpack        ! dependent var
        integer                    :: ipn              ! dependent var
        integer,pointer            :: kolors(:)        ! dependent var
        integer,pointer            :: btskp(:)         ! dependent var
        integer,pointer            :: btdo  (:)        ! dependent var
        integer,pointer            :: btbtwn(:)        ! dependent var
        integer,pointer            :: bttot(:)         ! dependent var
        real                       :: twidk            ! dependent var
        real                       :: axis1            ! dependent var
        real                       :: axis2            ! dependent var
        real                       :: hdr6lst          ! dependent var
        real                       :: otim             ! dependent var
        real                       :: ftr1             ! dependent var
        real                       :: bsmt1            ! dependent var
        real                       :: bsmt2            ! dependent var
        real                       :: phc              ! dependent var
        real                       :: plc              ! dependent var
        real                       :: vlinc            ! dependent var
        real                       :: glav,glav_ovl    ! dependent var
        real                       :: timbtwn          ! dependent var
        integer                    :: nsbtwn           ! dependent var
        integer                    :: knttr            ! dependent var
        integer                    :: iunit            ! dependent var
        real,pointer               :: vals(:)          ! dependent var
        real,pointer               :: trcb(:)          ! dependent var
        real,pointer               :: hgneg(:)         ! dependent var
        real,pointer               :: hgpos(:)         ! dependent var
        type(trcio_struct),pointer :: isn,isn2,idn     ! dependent var
        integer                    :: nwih             ! global
        integer                    :: ndpt             ! global
        real                       :: dt               ! global
        real                       :: tstrt            ! global
      end type color_struct

      integer   :: print_lun,nbits,nbytes
      real      :: cti=2.54,red(256),green(256),blue(256)
      character(len=8) :: survey_units,mtrc
      character(len=10):: user_name
      character(len=16):: routing_name,address,jobname
      character(len=24):: account


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(color_struct),pointer,save :: object      ! needed for traps.

!    setup combo boxs

      integer,parameter     :: lrrl_noptions = 2
      character(len=4),save :: lrrl_options(lrrl_noptions)      &
                               =(/ 'RL','LR' /)

      integer,parameter     :: opt_tbar1_noptions = 3
      character(len=4),save :: opt_tbar1_options(opt_tbar1_noptions)      &
                               =(/ 'CONT','DSCR','TBAR' /)

      integer,parameter     :: opt_tbar2_noptions = 3
      character(len=4),save :: opt_tbar2_options(opt_tbar2_noptions)      &
                               =(/ 'CONT','DSCR','TBAR' /)

      integer,parameter     :: opt_calc_noptions = 2
      character(len=4),save :: opt_calc_options(opt_calc_noptions)      &
                               =(/ 'PLOT','ALL ' /)

      integer,parameter     :: opt_dev_noptions = 3
      character(len=8),save :: opt_dev_options(opt_dev_noptions)      &
                               =(/ 'PAPER   ','FILM    ' ,'GLOSS   ' /)

      integer,parameter     :: opt_semb_noptions = 2
      character(len=8),save :: opt_semb_options(opt_semb_noptions)      &
                               =(/ 'NORMAL  ','SEMB    ' /)

      integer,parameter     :: opt_scale_noptions = 3
      character(len=4),save :: opt_scale_options(opt_scale_noptions)      &
                               =(/ 'NONE','CALC','USER' /)

      integer,parameter     :: opt_cbar_noptions = 3
      character(len=12),save :: opt_cbar_options(opt_cbar_noptions)      &
                               =(/ 'CBAR        ','VMIN_VMAX   ',&
                                   'PCTL        ' /)

      integer,parameter     :: dev_loc_noptions = 4
      character(len=8),save :: dev_loc_options(dev_loc_noptions)      &
                               =(/ 'HP5000A ','HP      ',&
                                   'TEST    ',&
                                   'SAVECGM '/)

      integer    ,parameter :: quality_noptions = 3
      character(len=8)      :: quality_options(quality_noptions)            &
                               = (/ 'PROD','MAX ','FAST'/)

      integer,parameter     :: bars_noptions = 7
      character(len=8),save :: bars_options(bars_noptions)      &
                               =(/ 'STAN    ','AVO133  ','BLUERED ','BLUEWHT ',&
                                   'MEDRAM  ','MEDRAM6 ','SVA     '/)

      integer    ,parameter :: yesno_noptions = 2
      character(len=4)      :: yesno_options(yesno_noptions)            &
                               = (/ 'YES ', 'NO  ' /)

      integer    ,parameter :: noyes_noptions = 2
      character(len=4)      :: noyes_options(noyes_noptions)            &
                               = (/ 'NO  ', 'YES ' /)

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine color_create (obj)
      implicit none
      type(color_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%isn)
      nullify (obj%isn2)
      nullify (obj%idn)

      nullify (obj%trce)
      nullify (obj%shot)
      nullify (obj%intv)
      nullify (obj%incr)
      nullify (obj%totl)
      nullify (obj%tim_init)
      nullify (obj%vl_init)
      nullify (obj%tim_inc)
      nullify (obj%vl_inc)
      nullify (obj%vl_tot)
      nullify (obj%cs_vl)
      nullify (obj%cs_vl_eng)
      nullify (obj%vl_row)
      nullify (obj%sltim)
      nullify (obj%slbl)
      nullify (obj%slsz)
      nullify (obj%slsz_eng)
      nullify (obj%slrow)
      nullify (obj%pcol)
      nullify (obj%prow)
      nullify (obj%plbl)
      nullify (obj%kolors)
      nullify (obj%vals)
      nullify (obj%trcb)
      nullify (obj%hgneg)
      nullify (obj%hgpos)
      nullify (obj%btskp)
      nullify (obj%btdo)
      nullify (obj%btbtwn)
      nullify (obj%bttot)

      call color_initialize (obj)
      return
      end subroutine color_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine color_delete (obj)
      implicit none
      type(color_struct),pointer :: obj       ! arguments

      integer :: istat

!<execute_only>
      call color_wrapup (obj)
!</execute_only>


      if (associated(obj%isn ))then
         istat=trcio_close(obj%isn)
       endif
      if (associated(obj%isn2 ))then
         istat=trcio_close(obj%isn2)
       endif
      if (associated(obj%idn ))then
        istat=trcio_close(obj%idn)
       endif

      if (associated(obj%trce )) deallocate        (obj%trce)
      if (associated(obj%shot )) deallocate        (obj%shot)
      if (associated(obj%intv )) deallocate        (obj%intv)
      if (associated(obj%incr )) deallocate        (obj%incr)
      if (associated(obj%totl )) deallocate        (obj%totl)
      if (associated(obj%tim_init )) deallocate        (obj%tim_init)
      if (associated(obj%vl_init ))  deallocate        (obj%vl_init)
      if (associated(obj%tim_inc ))  deallocate        (obj%tim_inc)
      if (associated(obj%vl_inc ))   deallocate        (obj%vl_inc)
      if (associated(obj%vl_tot ))   deallocate        (obj%vl_tot)
      if (associated(obj%cs_vl ))    deallocate        (obj%cs_vl)
      if (associated(obj%cs_vl_eng)) deallocate        (obj%cs_vl_eng)
      if (associated(obj%vl_row ))   deallocate        (obj%vl_row)
      if (associated(obj%sltim ))    deallocate        (obj%sltim)
      if (associated(obj%slbl ))     deallocate        (obj%slbl)
      if (associated(obj%slsz ))     deallocate        (obj%slsz)
      if (associated(obj%slsz_eng )) deallocate        (obj%slsz_eng)
      if (associated(obj%slrow ))    deallocate        (obj%slrow)
      if (associated(obj%pcol ))     deallocate        (obj%pcol)
      if (associated(obj%prow ))     deallocate        (obj%prow)
      if (associated(obj%plbl ))     deallocate        (obj%plbl)
      if (associated(obj%kolors ))   deallocate        (obj%kolors)
      if (associated(obj%vals ))     deallocate        (obj%vals)
      if (associated(obj%trcb ))     deallocate        (obj%trcb)
      if (associated(obj%hgneg ))     deallocate        (obj%hgneg)
      if (associated(obj%hgpos ))     deallocate        (obj%hgpos)
      if (associated(obj%btskp ))     deallocate        (obj%btskp)
      if (associated(obj%btdo ))     deallocate        (obj%btdo)
      if (associated(obj%btbtwn ))     deallocate        (obj%btbtwn)
      if (associated(obj%bttot ))     deallocate        (obj%bttot)

      deallocate(obj)
      return
      end subroutine color_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine color_initialize (obj)
      implicit none
      type(color_struct),intent(inout) :: obj       ! arguments



      call pc_get_jdata('TRACE_LENGTH',obj%time)
      obj%ips_eng      = 5.0
      obj%tpi_eng      = 10.0
      obj%cs_sid_eng   = .27
      obj%cs_scale_eng = .13
      obj%cs_lab_eng   = .13
      obj%wid_vl_eng   = .005
      obj%ar_ht_eng    = 0.0
      obj%ar_len_eng   = 2.5
      obj%cs_cbl_eng   = .13
      obj%cs_pl_eng    = .3
      obj%cs_tl_eng    = .236
      obj%cs_bl_eng    = .3
      obj%cs_lt_eng    = .07
      obj%cs_spl_eng   = .15
      obj%tpi_ovl_eng  = 0.0  
      obj%scda_eng     = 0.0
      obj%sdas_eng     = 0.0
      obj%tida_eng     = 0.0
      obj%arda_eng     = 0.0
      


      obj%lrrl         = 'RL'
      obj%num_tr       = 100
      obj%sid          = ' '
      obj%grade_hor    = 'YES'
      obj%grade_vrt    = 'YES'
      obj%invert       = 'NO'
      obj%rp           = 'NO'
      obj%skip_init    = 0
      obj%scale        = 'NO'
      obj%nhdr_lab     = 2
      obj%hdr_lab(1)   = 7
      obj%hdr_lab(2)   = 0
      obj%lab_init     = '50'
      obj%lab_inc      = 50
      obj%nam_lab      = ' '
      obj%ltab         = 'NO'
      obj%opt_vl       = 'NO'
      obj%ar_beg       = ' '
      obj%ar_end       = ' '
      obj%path_atbm    = PATHCHECK_EMPTY
      obj%frtb         = 1.0
      obj%dev_loc      = 'HP5000A'
      obj%quality      = 'PROD'
      obj%opt_dev      = 'PAPER'
      obj%dev_rem      = 'ABERD1'
      obj%copies       = 1
      obj%init         = ' '
      obj%fold         = 'NO'
      obj%nrnp         = ' '
      obj%idnp         = ' '
      obj%path_vect    = PATHCHECK_EMPTY
      obj%path_cbar    = PATHCHECK_EMPTY
      obj%bars         = 'STAN'
      obj%opt_cbar     = 'CBAR'
      obj%vmin         = 0.0
      obj%vmax         = 0.0
      obj%pnc          = 0.0
      obj%ppc          = 0.0
      obj%pplc         = 0.0
      obj%frcb         = 1.0
      obj%lab_cb       = ' '
      obj%opt_scale    = 'NONE'
      obj%ampl_user    = 1.0
      obj%opt_calc     = 'PLOT'
      obj%ntr_calc     = 0
      obj%ntpp         = 0
      obj%nrow         = 1
      obj%tldots       = 0
      obj%tldots(3)    = 1
      obj%tldots(6)    = 2
      obj%opt_tics     = 'NO'
      obj%path_blk     = PATHCHECK_EMPTY
      obj%path_tie1    = PATHCHECK_EMPTY
      obj%path_tie2    = PATHCHECK_EMPTY
      obj%hdr_dl       = 0
      obj%dlref        = 0.0
      obj%dlu100       = 1.0
      obj%ups          = 0.0
      obj%ncl_at1      = 0
      obj%ncl_at2      = 0
      obj%dist_ovl     = 'YES'
      obj%opt_tbar1    = 'CONT'
      obj%opt_tbar2    = 'CONT'
      obj%lab1         = ' '
      obj%lab2         = ' '
      obj%path_ovl     = PATHCHECK_EMPTY
      obj%ct_ovl       = 5.0
      obj%ratio_tpi    = 4
      obj%nctfo        = 1
      obj%wt_ovl       = 'YES'
      obj%va_ovl       = 'YES'
      obj%skip_init_ovl = 0
      obj%rp_ovl        = 'NO'
      obj%zt_ovl        = 0.0
      obj%opt_semb      = 'NORMAL'
      obj%path_vel      = PATHCHECK_EMPTY
      obj%coor_beg      = 0.0
      obj%coor_end      = 0.0
      obj%scale_vel     = 2000.0
      obj%cald          = 'YES'
      obj%nbts          = 0

      nbits=bit_size(obj%nbts)
      nbytes=nbits/8
      
      call pc_get_pdata('account',account)
      call pc_get_pdata('routing_name',routing_name) 
      call pc_get_pdata('user_name',user_name)  
      call pc_get_jdata('jobname',jobname)   

      call pc_get_pdata('survey_units',survey_units)  
!  Convert the displayed defaults to metric is necessary
      if(survey_units.eq.'METERS')then 
        mtrc='YES' 
        obj%ips=obj%ips_eng*cti
        obj%tpi=obj%tpi_eng*cti
        obj%cs_sid=obj%cs_sid_eng*cti
        obj%cs_scale=obj%cs_scale_eng*cti
        obj%cs_lab=obj%cs_lab_eng*cti
        obj%wid_vl=obj%wid_vl_eng*cti
        obj%ar_ht=obj%ar_ht_eng*cti
        obj%ar_len=obj%ar_len_eng*cti
        obj%cs_cbl=obj%cs_cbl_eng*cti
        obj%cs_pl=obj%cs_pl_eng*cti
        obj%cs_tl=obj%cs_tl_eng*cti
        obj%cs_bl=obj%cs_bl_eng*cti
        obj%cs_lt=obj%cs_lt_eng*cti
        obj%cs_spl=obj%cs_spl_eng*cti
        obj%tpi_ovl=obj%tpi_ovl_eng*cti
        obj%scda=obj%scda_eng*cti
        obj%sdas=obj%sdas_eng*cti
        obj%tida=obj%tida_eng*cti
        obj%arda=obj%arda_eng*cti

      else

        mtrc='NO'
        obj%ips=obj%ips_eng 
        obj%tpi=obj%tpi_eng
        obj%cs_sid=obj%cs_sid_eng 
        obj%cs_scale=obj%cs_scale_eng 
        obj%cs_lab=obj%cs_lab_eng 
        obj%wid_vl=obj%wid_vl_eng 
        obj%ar_ht=obj%ar_ht_eng 
        obj%ar_len=obj%ar_len_eng 
        obj%cs_cbl=obj%cs_cbl_eng 
        obj%cs_pl=obj%cs_pl_eng 
        obj%cs_tl=obj%cs_tl_eng 
        obj%cs_bl=obj%cs_bl_eng 
        obj%cs_lt=obj%cs_lt_eng 
        obj%cs_spl=obj%cs_spl_eng 
        obj%tpi_ovl=obj%tpi_ovl_eng 
        obj%scda=obj%scda_eng 
        obj%sdas=obj%sdas_eng 
        obj%tida=obj%tida_eng 
        obj%arda=obj%arda_eng 

      endif

!           initialize the dependent var in the structure      

      obj%fit    = 0
      obj%nshots = 0
      obj%ntistr = 0
      obj%nslbl  = 0
      obj%nplbl   = 0
      obj%n64s   = 0  !!!   needed???????
      obj%ntppsem = 0
      obj%kntall  = 0
      obj%kbtskp  = 0
      obj%kbtdo   = 0
      obj%kbtbtwn = 0
      obj%kbttot   = 0
      obj%irspp   = 'YES'
      obj%ntppset = 0
      obj%ktpanl  = 0
      obj%ncol    = 0
      obj%iuserbt = 0
      obj%irtbp   = 'YES'
      obj%kntcolr = 0
      obj%irepeat = 0
      obj%ncolro  = 0
      obj%ilfs    = 0
      obj%nplbl   = 0
      obj%interp  = 0
      obj%ktin    = 0
      obj%ntread  = 0
      obj%nbox    = 0
      obj%ntrpv   = 0
      obj%nvi     = 0
      obj%ncolr   = 0
      obj%ktrnum  = 0
      obj%ipnt    = 0
      obj%nsamp   = 0
      obj%twidk   = 0.0
      obj%axis1   = 0.0
      obj%axis2   = 0.0
      obj%hdr6lst = 0.0
      obj%otim    = 0.0
      obj%ftr1    = 0.0
      obj%bsmt1   = 0.0
      obj%bsmt2   = 0.0
      obj%phc     = 0.0
      obj%plc     = 0.0
      obj%vlinc   = 0.0
      obj%glav    = 0.0
      obj%knttr   = 0
      obj%iunit   = 0

!      This stuff is not done yet, so turn it off
      call pc_put_sensitive_array_flag('NRNP', .false.)
      call pc_put_sensitive_array_flag('IDNP', .false.)
      call pc_put_sensitive_array_flag('PATH_VECT', .false.)
      call pc_put_sensitive_array_flag('UPS', .false.)
      call pc_put_sensitive_array_flag('TIM_INIT_ARRAYSET', .false.)
      call pc_put_sensitive_array_flag('SLTIM_ARRAYSET', .false.)
      call pc_put_sensitive_array_flag('NCL_AT1', .false.)
      call pc_put_sensitive_array_flag('NCL_AT2', .false.)
      call pc_put_sensitive_array_flag('DIST_OVL', .false.)
      call pc_put_sensitive_array_flag('OPT_TBAR1', .false.)
      call pc_put_sensitive_array_flag('OPT_TBAR2', .false.)
      call pc_put_sensitive_array_flag('LBA1', .false.)
      call pc_put_sensitive_array_flag('LBA2', .false.)
      call pc_put_sensitive_array_flag('OPT_SEMB', .false.)
      call pc_put_sensitive_array_flag('PATH_VEL', .false.)
      call pc_put_sensitive_array_flag('COOR_BEG', .false.)
      call pc_put_sensitive_array_flag('COOR_END',.false.)
      call pc_put_sensitive_array_flag('SCALE_VEL', .false.)

      print_lun=pc_get_lun()
   
      call color_update (obj)
      return
      end subroutine color_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine color_update (obj)
      implicit none
      type(color_struct),intent(inout),target :: obj         


      integer      :: ntldots=6,ncs_cbl=4,ncs_lab=2,nsid=4,nlab_cb=4,nnam_lab=2
      integer      :: i,j,k,m,ii,kk
      integer      :: idm,istat
      integer      :: nscr,nstor,ntmp,ntrp
      integer      :: kflag,kolorst(100)
      integer     :: istan(32)=(/45,43,57,64,63,62,61,60,70,78,87,103, 102,&
                                101,100,116,120,128,131,143,144,153,159,160,&
                                167,165,187,183,184,193,209,223 /)

!          probably don't need - vars for pack
      integer     :: nsamp_double,nsmpmultd                       
      

      real        ::   b,add,dif,f 
      real        :: fdm,frac
      real        :: space,spi,spin,start,temp
      real        :: valst(100)

!           Local array counters
      integer :: ntrce,nintv,nincr,ntotl,nvl_init,ntim_inc,nvl_inc,ncs_vl
      integer :: nvl_tot,nvl_row,nsltim,nslsz,nslrow,npcol,nprow

      integer :: nheader,nprocess_list

      character(len=80) :: cmds(1),card
      character(len=20),pointer :: process_list(:)
      character(len=8)  :: itype


      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_register_array_names ("trce_arrayset", (/  &
                                    "trce",              &
                                    "shot",              &
                                    "intv",              &
                                    "incr",              &
                                    "totl" /))

      call pc_register_array_names ("lab_cb_arrayset", (/  &
                                    "lab_cb",              &
                                    "cs_cbl" /))

      call pc_register_array_names ("pcol_arrayset", (/  &
                                    "pcol",              &
                                    "prow",              &
                                    "plbl" /))

      call pc_register_array_names ("tim_init_arrayset", (/  &
                                    "tim_init",              &
                                    "vl_init ",              &
                                    "tim_inc ",              &
                                    "vl_inc  ",              &
                                    "vl_tot  ",              &
                                    "cs_vl   ",              &
                                    "vl_row  " /))

      call pc_register_array_names ("sltim_arrayset", (/  &
                                    "sltim",              &
                                    "slbl ",              &
                                    "slsz ",              &
                                    "slrow" /))

      call pc_get_global ('nwih'    , obj%nwih)  ! number of header words.
      call pc_get_global ('ndpt'    , obj%ndpt)  ! number of trace samples.
      call pc_get_global ('dt'      , obj%dt)    ! trace sample interval (sec).
      call pc_get_global ('tstrt'   , obj%tstrt) !time of 1st trace sample(sec).

!------------Check that globals are set:
      if (obj%nwih == 0) call pc_error ("NWIH global hasn't been set.")
      if (obj%ndpt == 0) call pc_error ("NDPT global hasn't been set.")
      if (obj%tstrt == fnil) call pc_error ("TSTRT global hasn't been set.")
      if (obj%dt   == 0.0) call pc_error ("DT global hasn't been set.")

      obj%ipn=pc_get_ipn()

!            Set local array counters
      ntrce=obj%nshots
      nintv=obj%nshots
      nincr=obj%nshots
      ntotl=obj%nshots

      nvl_init=obj%ntistr
      ntim_inc=obj%ntistr
      nvl_inc=obj%ntistr
      nvl_tot=obj%ntistr
      ncs_vl=obj%ntistr
      nvl_row=obj%ntistr

      nsltim=obj%nslbl
      nslsz=obj%nslbl
      nslrow=obj%nslbl



      call pc_get('copies',        obj%copies,           color_copies_trap)
      call pc_get('tldots',        obj%tldots, ntldots,  color_tldots_trap)
      call pc_get('ups',           obj%ups,              color_ups_trap)
      call pc_get('ncl_at1',       obj%ncl_at1,          color_ncl_at1_trap)
      call pc_get('ncl_at2',       obj%ncl_at2,          color_ncl_at2_trap)
      call pc_get('skip_init',     obj%skip_init,        color_skip_init_trap)
      call pc_get('skip_init_ovl', obj%skip_init_ovl,  color_skip_init_ovl_trap)
      call pc_get('ntr_ovl',       obj%ntr_ovl,        color_ntr_ovl_trap)
      call pc_get('nctfo',         obj%nctfo,          color_nctfo_trap)
      call pc_get('num_tr',        obj%num_tr,         color_num_tr_trap)
      call pc_get('ntpp',          obj%ntpp,           color_ntpp_trap)
      call pc_get('nrow',          obj%nrow,           color_nrow_trap)
      call pc_get('hdr_lab',       obj%hdr_lab, obj%nhdr_lab)
      call pc_alloc('trce',        obj%trce,    ntrce)
      call pc_alloc('shot',      obj%shot,      obj%nshots)
      call pc_alloc('intv',      obj%intv,      nintv)
      call pc_alloc('incr',      obj%incr,      nincr)
      call pc_alloc('totl',      obj%totl,      ntotl,  color_totl_element_trap)
      call pc_call_array_trap('hdr_lab',color_hdr_lab_trap)
      call pc_get('lab_inc',       obj%lab_inc,        color_lab_inc_trap)
      call pc_get('ntr_calc',      obj%ntr_calc,       color_ntr_calc_trap)
      call pc_get('hdr_dl',        obj%hdr_dl,         color_hdr_dl_trap)
      call pc_get('time',          obj%time,           color_time_trap)
      call pc_get('ips',           obj%ips,            color_ips_trap)
      call pc_get('cs_sid',        obj%cs_sid,         color_cs_sid_trap)
      call pc_get('cs_spl',        obj%cs_spl,         color_cs_spl_trap)
      call pc_get('opt_cbar',      obj%opt_cbar,       color_opt_cbar_trap)
      call pc_get('vmin',          obj%vmin,           color_vmin_trap)
      call pc_get('vmax',          obj%vmax,           color_vmax_trap)
      call pc_get('cs_cbl',        obj%cs_cbl, ncs_cbl,color_cs_cbl_trap)
      call pc_get('tpi',           obj%tpi,            color_tpi_trap)
      call pc_get('sdas',          obj%sdas,           color_sdas_trap)
      call pc_get('cs_tl',         obj%cs_tl,          color_cs_tl_trap)
      call pc_get('wid_vl',        obj%wid_vl,         color_wid_vl_trap)
      call pc_get('pnc',           obj%pnc,            color_pnc_trap)
      call pc_get('ppc',           obj%ppc,            color_ppc_trap)
      call pc_get('pplc',          obj%pplc,           color_pplc_trap)
      call pc_get('cs_bl',         obj%cs_bl,          color_cs_bl_trap)
      call pc_get('frtb',          obj%frtb,           color_frtb_trap)
      call pc_get('ar_ht',         obj%ar_ht,          color_ar_ht_trap)
      call pc_get('ar_len',        obj%ar_len,         color_ar_len_trap)
      call pc_get('arda',          obj%arda,           color_arda_trap)
      call pc_get('tida',          obj%tida,           color_tida_trap)
      call pc_get('cs_lt',         obj%cs_lt,          color_cs_lt_trap)
      call pc_get('ct_ovl',        obj%ct_ovl,         color_ct_ovl_trap)
      call pc_get('zt_ovl',        obj%zt_ovl,         color_zt_ovl_trap)
      call pc_get('tpi_ovl',       obj%tpi_ovl,        color_tpi_ovl_trap)
      call pc_get('cs_lab',        obj%cs_lab, ncs_lab,color_cs_lab_trap)
      call pc_get('scale_vel',     obj%scale_vel,      color_scale_vel_trap)
      call pc_get('frcb',          obj%frcb,           color_frcb_trap)
      call pc_get('coor_beg',      obj%coor_beg,       color_coor_beg_trap)
      call pc_get('coor_end',      obj%coor_end,       color_coor_end_trap)
      call pc_get('cs_pl',         obj%cs_pl,          color_cs_pl_trap)
      call pc_get('scda',          obj%scda,           color_scda_trap)
      call pc_get('cs_scale',      obj%cs_scale,       color_cs_scale_trap)
      call pc_get('dlref',         obj%dlref,          color_dlref_trap)
      call pc_get('dlu100',        obj%dlu100,         color_dlu100_trap)
      call pc_get('path_cbar',     obj%path_cbar,      color_path_cbar_trap)
      call pc_get('path_atbm',     obj%path_atbm,      color_path_atbm_trap)
      call pc_get('path_blk',      obj%path_blk,       color_path_blk_trap)
      call pc_get('path_tie1',     obj%path_tie1,      color_path_tie1_trap)
      call pc_get('path_tie2',     obj%path_tie2,      color_path_tie2_trap)
      call pc_get('path_vel',      obj%path_vel,       color_path_vel_trap)
      call pc_get('path_ovl',      obj%path_ovl,       color_path_ovl_trap)
      call pc_get('path_vect',     obj%path_vect,      color_path_vect_trap)
      call pc_get('sid',           obj%sid,  nsid,     color_sid_trap)
      call pc_get('lab1',          obj%lab1,           color_lab1_trap)
      call pc_get('lab2',          obj%lab2,           color_lab2_trap)
      call pc_get('idnp',          obj%idnp,           color_idnp_trap)
      call pc_get('lab_cb',        obj%lab_cb,nlab_cb, color_lab_cb_trap)
      call pc_get('nam_lab',       obj%nam_lab,nnam_lab,color_nam_lab_trap)
      call pc_get('cald',          obj%cald,           color_cald_trap)
      call pc_get('lrrl',          obj%lrrl,           color_lrrl_trap)
      call pc_get('init',          obj%init,           color_init_trap)
      call pc_get('grade_hor',     obj%grade_hor,      color_grade_hor_trap)
      call pc_get('grade_vrt',     obj%grade_vrt,      color_grade_vrt_trap)
      call pc_get('ltab',          obj%ltab,           color_ltab_trap)
      call pc_get('opt_tics',      obj%opt_tics,       color_opt_tics_trap)
      call pc_get('opt_vl',        obj%opt_vl,         color_opt_vl_trap)
      call pc_get('opt_scale',     obj%opt_scale,      color_opt_scale_trap)
      call pc_get('opt_tbar1',     obj%opt_tbar1,      color_opt_tbar1_trap)
      call pc_get('opt_tbar2',     obj%opt_tbar2,      color_opt_tbar2_trap)
      call pc_get('dist_ovl',      obj%dist_ovl,       color_dist_ovl_trap)
      call pc_get('invert',        obj%invert,         color_invert_trap)
      call pc_get('ar_beg',        obj%ar_beg,         color_ar_beg_trap)
      call pc_get('ar_end',        obj%ar_end,         color_ar_end_trap)
      call pc_get('wt_ovl',        obj%wt_ovl,         color_wt_ovl_trap)
      call pc_get('va_ovl',        obj%va_ovl,         color_va_ovl_trap)
      call pc_get('dev_loc',       obj%dev_loc,        color_dev_loc_trap)
      call pc_get ('quality',      obj%quality)
      call pc_get('opt_dev',       obj%opt_dev)
!      call pc_get('dev_rem',       obj%dev_rem,        color_dev_rem_trap)
      call pc_get('nrnp',          obj%nrnp,           color_nrnp_trap)
      call pc_get('ratio_tpi',     obj%ratio_tpi,      color_ratio_tpi_trap)
      call pc_get('bars',          obj%bars,           color_bars_trap)
      call pc_get('lab_init',      obj%lab_init,       color_lab_init_trap)
      call pc_get('opt_calc',      obj%opt_calc,       color_opt_calc_trap)
      call pc_get('rp',            obj%rp,             color_rp_trap)
      call pc_get('rp_ovl',        obj%rp_ovl,         color_rp_ovl_trap)
      call pc_get('scale',         obj%scale,          color_scale_trap)
      call pc_get('fold',          obj%fold,           color_fold_trap)
      call pc_get('opt_semb',      obj%opt_semb,       color_opt_semb_trap)
      call pc_alloc('tim_init',  obj%tim_init,  obj%ntistr)
      call pc_alloc('vl_init',   obj%vl_init,   nvl_init)
      call pc_alloc('tim_inc',   obj%tim_inc,   ntim_inc)
      call pc_alloc('vl_inc',    obj%vl_inc,    nvl_inc)
      call pc_alloc('vl_tot',    obj%vl_tot,    nvl_tot)
      call pc_alloc('cs_vl',     obj%cs_vl,     ncs_vl)
      call pc_alloc('vl_row',    obj%vl_row,    nvl_row)
      call pc_alloc('sltim',     obj%sltim,     nsltim)
      call pc_alloc('slbl',      obj%slbl,      obj%nslbl)
      call pc_alloc('slsz',      obj%slsz,      nslsz)
      call pc_alloc('slrow',     obj%slrow,     nslrow)
      npcol=obj%nplbl
      nprow=obj%nplbl
      call pc_alloc('pcol',      obj%pcol,      npcol)
      call pc_alloc('prow',      obj%prow,      nprow)
      call pc_alloc('plbl',      obj%plbl,      obj%nplbl)

     call pc_call_arrayset_trap('TRCE_ARRAYSET',color_trce_arrayset_trap)
     call pc_call_arrayset_trap('TIM_INIT_ARRAYSET',&
                                 color_tim_init_arrayset_trap)
     call pc_call_arrayset_trap('SLTIM_ARRAYSET',color_sltim_arrayset_trap)
     if(obj%nplbl.gt.0)then
       call pc_call_arrayset_trap('PCOL_ARRAYSET',color_pcol_arrayset_trap)
     endif

!        Insure linked array counters have the same lengths
      if(ntrce.ne.obj%nshots.or.nintv.ne.obj%nshots.or.nincr.ne.obj%nshots&
         .or.ntotl.ne.obj%nshots)then
        call pc_error('Shot card arrays do not have the same length')
        obj%nshots=min(obj%nshots,ntrce,nintv,nincr,ntotl)
      endif

      if(nvl_init.ne.obj%ntistr.or.ntim_inc.ne.obj%ntistr.or.nvl_inc.ne.&
         obj%ntistr.or.ncs_vl.ne.obj%ntistr.or.nvl_row.ne.obj%ntistr)then
        call pc_error('Vertical label card arrays do not have the same length')
        obj%ntistr=min(obj%ntistr,ntim_inc,nvl_inc,ncs_vl,nvl_row)
      endif

      if(nsltim.ne.obj%nslbl.or.nslsz.ne.obj%nslbl.or.nslrow.ne.obj%nslbl)then
        call pc_error('Side label card arrays do not have the same length')
        obj%nslbl=min(obj%nslbl,nsltim,nslsz,nslrow)
      endif

      if(npcol.ne.obj%nplbl.or.nprow.ne.obj%nplbl)then
        call pc_error('Panel label card arrays do not have the same length')
        obj%nplbl=min(obj%nplbl,npcol,nprow)
      endif

      call pc_call_end_trap(color_end_trap)




!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!




!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!




!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


!          Write out the cpst card if this is the first color
      nullify(process_list)
      nprocess_list=0
      call pc_alloc_jdata('PROCESS_LIST',process_list,nprocess_list)
      DO i=1,nprocess_list
        j=i
        if(process_list(i).eq.'COLOR')exit
      ENDDO 
      if(obj%dev_loc.ne.'TEST'.and.j.eq.obj%ipn)then
        cmds(1)='/usr/app/bin/color2cgm >> online'
!!!        cmds(1)='/u/poepsn03g/goodger/src/color/cgm/color2cgm >> online'
        call pc_put_control ('CMD_AFTER_BSCRIPT',cmds,1)
      endif

      call pc_put_options_field ('LTAB', noyes_options, noyes_noptions)
      call pc_put_options_field ('GRADE_HOR', yesno_options, yesno_noptions)
      call pc_put_options_field ('GRADE_VRT', yesno_options, yesno_noptions)
      call pc_put_options_field ('CALD', yesno_options, yesno_noptions)
      call pc_put_options_field ('INVERT', noyes_options, noyes_noptions)
      call pc_put_options_field ('RP', noyes_options, noyes_noptions)
      call pc_put_options_field ('LTAB', yesno_options, yesno_noptions)
      call pc_put_options_field ('OPT_VL', noyes_options, noyes_noptions)
      call pc_put_options_field ('LRRL', lrrl_options, lrrl_noptions)
      call pc_put_options_field ('DEV_LOC', dev_loc_options,dev_loc_noptions)
      call pc_put_options_field ('QUALITY', quality_options,quality_noptions)
      call pc_put_options_field ('OPT_DEV', opt_dev_options,opt_dev_noptions)
      call pc_put_options_field ('BARS', bars_options,bars_noptions)
      call pc_put_options_field ('OPT_CBAR', opt_cbar_options,opt_cbar_noptions)
      call pc_put_options_field ('OPT_CALC', opt_calc_options,opt_calc_noptions)
      call pc_put_options_field ('OPT_SEMB', opt_semb_options,opt_semb_noptions)
      call pc_put_options_field ('OPT_TBAR1',opt_tbar1_options,&
                                  opt_tbar1_noptions)
      call pc_put_options_field ('OPT_TBAR2',opt_tbar2_options,&
                                  opt_tbar2_noptions)
      call pc_put_options_field ('OPT_SCALE',opt_scale_options,&
                                  opt_scale_noptions)
      call pc_put_options_field ('FOLD', noyes_options, noyes_noptions)
      call pc_put_options_field ('OPT_TICS', noyes_options, noyes_noptions)
      call pc_put_options_field ('DIST_OVL', yesno_options, yesno_noptions)
      call pc_put_options_field ('WT_OVL', yesno_options, yesno_noptions)
      call pc_put_options_field ('VA_OVL', yesno_options, yesno_noptions)
      call pc_put_options_field ('RP_OVL', noyes_options, noyes_noptions)

      call pc_put('copies',        obj%copies)
      call pc_put('tldots',        obj%tldots, ntldots)
      call pc_put('ups',           obj%ups)
      call pc_put('ncl_at1',       obj%ncl_at1)
      call pc_put('ncl_at2',       obj%ncl_at2)
      call pc_put('skip_init',     obj%skip_init)
      call pc_put('skip_init_ovl', obj%skip_init_ovl)
      call pc_put('ntr_ovl',       obj%ntr_ovl)
      call pc_put('nctfo',         obj%nctfo)
      call pc_put('num_tr',        obj%num_tr)
      call pc_put('ntpp',          obj%ntpp)
      call pc_put('nrow',          obj%nrow)
      call pc_put('hdr_lab',       obj%hdr_lab, obj%nhdr_lab)
      call pc_put('lab_inc',       obj%lab_inc)
      call pc_put('ntr_calc',      obj%ntr_calc)
      call pc_put('hdr_dl',        obj%hdr_dl)
      call pc_put('time',          obj%time)
      call pc_put('ips',           obj%ips)
      call pc_put('cs_sid',        obj%cs_sid)
      call pc_put('cs_spl',        obj%cs_spl)
      call pc_put('opt_cbar',      obj%opt_cbar)
      call pc_put('vmin',          obj%vmin)
      call pc_put('vmax',          obj%vmax)
      call pc_put('cs_cbl',        obj%cs_cbl, ncs_cbl)
      call pc_put('tpi',           obj%tpi)
      call pc_put('sdas',          obj%sdas)
      call pc_put('cs_tl',         obj%cs_tl)
      call pc_put('wid_vl',        obj%wid_vl)
      call pc_put('pnc',           obj%pnc)
      call pc_put('ppc',           obj%ppc)
      call pc_put('pplc',          obj%pplc)
      call pc_put('cs_bl',         obj%cs_bl)
      call pc_put('frtb',          obj%frtb)
      call pc_put('ar_ht',         obj%ar_ht)
      call pc_put('ar_len',        obj%ar_len)
      call pc_put('arda',          obj%arda)
      call pc_put('tida',          obj%tida)
      call pc_put('cs_lt',         obj%cs_lt)
      call pc_put('ct_ovl',        obj%ct_ovl)
      call pc_put('zt_ovl',        obj%zt_ovl)
      call pc_put('tpi_ovl',       obj%tpi_ovl)
      call pc_put('cs_lab',        obj%cs_lab, ncs_lab)
      call pc_put('scale_vel',     obj%scale_vel)
      call pc_put('frcb',          obj%frcb)
      call pc_put('coor_beg',      obj%coor_beg)
      call pc_put('coor_end',      obj%coor_end)
      call pc_put('cs_pl',         obj%cs_pl)
      call pc_put('scda',          obj%scda)
      call pc_put('cs_scale',      obj%cs_scale)
      call pc_put('dlref',         obj%dlref)
      call pc_put('dlu100',        obj%dlu100)
      call pc_put('path_cbar',     obj%path_cbar)
      call pc_put('path_atbm',     obj%path_atbm)
      call pc_put('path_blk',      obj%path_blk)
      call pc_put('path_tie1',     obj%path_tie1)
      call pc_put('path_tie2',     obj%path_tie2)
      call pc_put('path_vel',      obj%path_vel)
      call pc_put('path_ovl',      obj%path_ovl)
      call pc_put('path_vect',     obj%path_vect)
      call pc_put('sid',           obj%sid,  nsid)
      call pc_put('lab1',          obj%lab1)
      call pc_put('lab2',          obj%lab2)
      call pc_put('idnp',          obj%idnp)
      call pc_put('lab_cb',        obj%lab_cb,nlab_cb)
      call pc_put('nam_lab',       obj%nam_lab,nnam_lab)
      call pc_put('cald',          obj%cald)
      call pc_put('lrrl',          obj%lrrl)
      call pc_put('init',          obj%init)
      call pc_put('grade_hor',     obj%grade_hor)
      call pc_put('grade_vrt',     obj%grade_vrt)
      call pc_put('ltab',          obj%ltab)
      call pc_put('opt_tics',      obj%opt_tics)
      call pc_put('opt_vl',        obj%opt_vl)
      call pc_put('opt_scale',     obj%opt_scale)
      call pc_put('opt_tbar1',     obj%opt_tbar1)
      call pc_put('opt_tbar2',     obj%opt_tbar2)
      call pc_put('dist_ovl',      obj%dist_ovl)
      call pc_put('invert',        obj%invert)
      call pc_put('ar_beg',        obj%ar_beg)
      call pc_put('ar_end',        obj%ar_end)
      call pc_put('wt_ovl',        obj%wt_ovl)
      call pc_put('va_ovl',        obj%va_ovl)
      call pc_put('dev_loc',       obj%dev_loc)
      call pc_put ('quality',      obj%quality)
      call pc_put('opt_dev',       obj%opt_dev)
!      call pc_put('dev_rem',       obj%dev_rem)
!      call pc_put('nrnp',          obj%nrnp)
      call pc_put('ratio_tpi',     obj%ratio_tpi)
      call pc_put('bars',          obj%bars)
      call pc_put('lab_init',      obj%lab_init)
      call pc_put('opt_calc',      obj%opt_calc)
      call pc_put('rp',            obj%rp)
      call pc_put('rp_ovl',        obj%rp_ovl)
      call pc_put('scale',         obj%scale)
      call pc_put('fold',          obj%fold)
      call pc_put('opt_semb',      obj%opt_semb)

      call pc_put('trce',      obj%trce,      ntrce)
      call pc_put('shot',      obj%shot,      obj%nshots)
      call pc_put('intv',      obj%intv,      nintv)
      call pc_put('incr',      obj%incr,      nincr)
      call pc_put('totl',      obj%totl,      ntotl)
      call pc_put('tim_init',  obj%tim_init,  obj%ntistr)
      call pc_put('vl_init',   obj%vl_init,   nvl_init)
      call pc_put('tim_inc',   obj%tim_inc,   ntim_inc)
      call pc_put('vl_inc',    obj%vl_inc,    nvl_inc)
      call pc_put('vl_tot',    obj%vl_tot,    nvl_tot)
      call pc_put('cs_vl',     obj%cs_vl,     ncs_vl)
      call pc_put('vl_row',    obj%vl_row,    nvl_row)
      call pc_put('sltim',     obj%sltim,     nsltim)
      call pc_put('slbl',      obj%slbl,      obj%nslbl)
      call pc_put('slsz',      obj%slsz,      nslsz)
      call pc_put('slrow',     obj%slrow,     nslrow)
      call pc_put('pcol',      obj%pcol,      npcol)
      call pc_put('prow',      obj%prow,      nprow)
      call pc_put('plbl',      obj%plbl,      obj%nplbl)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!



!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup=.false.
      call mem_alloc(obj%btskp,1)
      call mem_alloc(obj%btdo,1)
      call mem_alloc(obj%btbtwn,1)
      call mem_alloc(obj%bttot,1)
      obj%btskp=0
      obj%btdo=0
      obj%btbtwn=0
      obj%bttot=0
      if(obj%ntistr.gt.0)call mem_alloc(obj%cs_vl_eng,obj%ntistr)
      if(obj%nslbl.gt.0)call mem_alloc(obj%slsz_eng,obj%nslbl)
      if(survey_units.eq.'METERS')then
        do i=1,obj%ntistr
          obj%cs_vl_eng(i)=obj%cs_vl(i)/cti
        enddo
        do i=1,obj%nslbl
          obj%slsz_eng(i)=obj%slsz(i)/cti
          if(obj%slsz_eng(i).le.0.00001)obj%slsz_eng(i)=.15
        enddo
      endif
!
!                  BEGIN OLD BACKEND SETUP ENTRY
      write(print_lun,*)' BEGIN         - COLOR SETUP'
!          check input parameters, initialize variables and tables
!
      obj%hdr6lst = - 999.0
      obj%ntppset = 0
      obj%ncol = 1
      if(obj%opt_semb.eq.'NORMAL')obj%ntppset=1
      obj%ktin = 0
      obj%ntread = obj%num_tr
      if (obj%ncl_at1.gt.0) obj%ntread = obj%ntread * 2
      if (obj%pnc.ne.0.0) obj%pplc = - 1.0
      obj%otim = obj%time
      obj%irtbp = 'NO'
      obj%nsamp = obj%otim / obj%dt + .0001
      obj%nvi = obj%nsamp
      if (obj%ntpp.gt.0.or.obj%opt_semb.eq.'SEMB') then
         obj%nsbtwn = .1 / obj%dt + .0001
         obj%timbtwn = real (obj%nsbtwn) * obj%dt
!          put enough samples between panels so that timing line number
!            will fit - also wrda and wrdb labels
         temp = obj%timbtwn * obj%ips_eng
         space = 0.0
         if (obj%hdr_lab(1).gt.0.or.obj%lab_init.eq.'SHOT') then
            space = obj%cs_spl_eng * 1.5 + .3
            if (obj%hdr_lab(2).gt.0) space = space+obj%cs_lab_eng(1) + .1
         endif
         if (obj%cs_tl_eng.gt.space) space = obj%cs_tl_eng
         if (temp.lt.space) then
            obj%timbtwn = space / obj%ips_eng
            obj%nsbtwn = obj%timbtwn / obj%dt + 1.0001
            obj%timbtwn = real (obj%nsbtwn) * obj%dt
            write(print_lun,*)' COLOR=> obj%time BETWEEN PANELS = ', obj%timbtwn
         endif
         obj%nsamp = obj%nvi * obj%nrow + obj%nsbtwn * (obj%nrow - 1)
      endif
!
!          if paneling - reset obj%time
      if (obj%ntpp.gt.0.or.obj%opt_semb.eq.'SEMB') then
         obj%time = obj%time * real (obj%nrow)+obj%timbtwn*(real(obj%nrow) - 1)
         write(print_lun,*)' PANELED time = ', obj%time
         obj%irtbp = 'YES'
      endif
!
!          set up rgb default toner colors
!
      red (1) = 0.0
      red (2) = 0.0
      red (3) = 1.00
      red (4) = 1.0
      red (5) = 0.00
      red (6) = 1.0
      red (7) = 0.0
      red (8) = 0.0
      red (9) = 1.0
      red (10) = .7
      red (11) = .6230
      red (12) = .4970
      red (13) = .1300
      red (14) = .0750
      red (15) = .0000
      red (16) = .7487
      red (17) = .4973
      red (18) = .2500
      red (19) = .1396
      red (20) = 0.000
      red (21) = .0000
      red (22) = .0017
      red (23) = .0012
      red (24) = 0.000
      red (25) = .4973
      red (26) = .3185
      red (27) = .1396
      red (28) = .0000
      red (29) = .0000
      red (30) = .0000
      red (31) = .0000
      red (32) = .4973
      red (33) = .3185
      red (34) = .1396
      red (35) = .0000
      red (36) = .0000
      red (37) = .0000
      red (38) = .0000
      red (39) = .4973
      red (40) = .3185
      red (41) = .1396
      red (42) = .0000
      red (43) = .0000
      red (44) = .0000
      red (45) = .0000
      red (46) = .4973
      red (47) = .3185
      red (48) = .1396
      red (49) = .0000
      red (50) = .0000
      red (51) = .0000
      red (52) = .0000
      red (53) = .4973
      red (54) = .3185
      red (55) = .1396
      red (56) = .0000
      red (57) = .0000
      red (58) = 0.00
      red (59) = .0000
      red (60) = .7487
      red (61) = .4973
      red (62) = .3185
      red (63) = .1396
      red (64) = .0000
      red (65) = .0000
      red (66) = .0000
      red (67) = .0000
      red (68) = .0000
      red (69) = .4973
      red (70) = .3185
      red (71) = .1396
      red (72) = .0000
      red (73) = .0000
      red (74) = .0000
      red (75) = .0000
      red (76) = .4973
      red (77) = .3185
      red (78) = .1396
      red (79) = .0000
      red (80) = .0000
      red (81) = .0000
      red (82) = 0.0
      red (83) = .7487
      red (84) = .4973
      red (85) = .3185
      red (86) = .1396
      red (87) = .0000
      red (88) = .0672
      red (89) = .0000
      red (90) = .0000
      red (91) = .0000
      red (92) = .6230
      red (93) = .3951
      red (94) = .2418
      red (95) = .1396
      red (96) = .0000
      red (97) = .0000
      red (98) = .0000
      red (99) = .6858
      red (100) = .4973
      red (101) = .3951
      red (102) = .2500
      red (103) = .2460
      red (104) = .0000
      red (105) = .0000
      red (106) = .7487
      red (107) = .6230
      red (108) = .5602
      red (109) = .4973
      red (110) = .3140
      red (111) = .0000
      red (112) = .0000
      red (113) = .8110
      red (114) = .6858
      red (115) = .6510
      red (116) = .6230
      red (117) = .3717
      red (118) = .1203
      red (119) = .0181
      red (120) = .8743
      red (121) = .8115
      red (122) = .7801
      red (123) = .7487
      red (124) = .4974
      red (125) = .2460
      red (126) = .1438
      red (127) = 1.000
      red (128) = 1.000
      red (129) = 1.000
      red (130) = 1.000
      red (131) = 1.000
      red (132) = .7480
      red (133) = .4973
      red (134) = .3951
      red (135) = .3185
      red (136) = 1.000
      red (137) = 1.000
      red (138) = 1.000
      red (139) = 1.000
      red (140) = .7487
      red (141) = .4973
      red (142) = .3951
      red (143) = 1.000
      red (144) = 1.000
      red (145) = 1.000
      red (146) = 1.000
      red (147) = .7487
      red (148) = .4973
      red (149) = .3951
      red (150) = 1.000
      red (151) = 1.000
      red (152) = 1.000
      red (153) = 1.000
      red (154) = .7480
      red (155) = .2200
      red (156) = .5000
      red (157) = 1.000
      red (158) = 1.000
      red (159) = 1.000
      red (160) = 1.000
      red (161) = .7480
      red (162) = .2000
      red (163) = .3950
      red (164) = 1.000
      red (165) = 1.000
      red (166) = 1.000
      red (167) = 1.000
      red (168) = .8000
      red (169) = .2000
      red (170) = .1400
      red (171) = 1.000
      red (172) = 1.000
      red (173) = 1.000
      red (174) = 1.000
      red (175) = 1.000
      red (176) = .7480
      red (177) = .4973
      red (178) = .1300
      red (179) = .3185
      red (180) = 1.000
      red (181) = 1.000
      red (182) = 1.000
      red (183) = 1.000
      red (184) = .7480
      red (185) = .4973
      red (186) = .3140
      red (187) = 1.000
      red (188) = 1.000
      red (189) = 1.000
      red (190) = 1.000
      red (191) = 1.0
      red (192) = .7480
      red (193) = .5000
      red (194) = .3140
      red (195) = .3185
      red (196) = 0.8743
      red (197) = .8115
      red (198) = .7801
      red (199) = .7487
      red (200) = .4974
      red (201) = .2500
      red (202) = .2520
      red (203) = 0.8115
      red (204) = .6850
      red (205) = .6850
      red (206) = .6230
      red (207) = .4630
      red (208) = .1550
      red (209) = .2500
      red (210) = .7487
      red (211) = .6230
      red (212) = .5602
      red (213) = .4973
      red (214) = .2460
      red (215) = .2500
      red (216) = .0250
      red (217) = .6858
      red (218) = .4973
      red (219) = .3951
      red (220) = .3185
      red (221) = .0672
      red (222) = .0160
      red (223) = .1250
      red (224) = .6230
      red (225) = .3951
      red (226) = .2418
      red (227) = .1396
      red (228) = .0000
      red (229) = .0575
      red (230) = .0000
      red (231) = .7487
      red (232) = .6230
      red (233) = .2200
      red (234) = .3185
      red (235) = .1396
      red (236) = .1250
      red (237) = .4973
      red (238) = .4690
      red (239) = .0899
      red (240) = .246
      red (241) = .4973
      red (242) = .4973
      red (243) = .0780
      red (244) = .1250
      red (245) = .2200
      red (246) = .0100
      red (247) = .9372
      red (248) = .8743
      red (249) = .8115
      red (250) = .7487
      red (251) = .7500
      red (252) = .3951
      red (253) = .2418
      red (254) = 1.000
      red (255) = .6250
      red (256) = .1500
!
      green (1) = 0.0
      green (2) = 1.000
      green (3) = 0.000
      green (4) = 1.000
      green (5) = 0.000
      green (6) = 0.000
      green (7) = 1.000
      green (8) = 0.000
      green (9) = 1.000
      green (10) = .7
      green (11) = .6230
      green (12) = .4970
      green (13) = .1300
      green (14) = .0750
      green (15) = .0000
      green (16) = .7487
      green (17) = .4973
      green (18) = .2500
      green (19) = 0.1396
      green (20) = 0.0672
      green (21) = 0.0246
      green (22) = 0.000
      green (23) = 0.000
      green (24) = 0.000
      green (25) = 0.6230
      green (26) = .3951
      green (27) = 0.2418
      green (28) = 0.1396
      green (29) = .1438
      green (30) = .0006
      green (31) = .0005
      green (32) = .6858
      green (33) = .4973
      green (34) = .3951
      green (35) = .3185
      green (36) = .0672
      green (37) = .0033
      green (38) = .0025
      green (39) = .7487
      green (40) = .6230
      green (41) = .5602
      green (42) = .4973
      green (43) = .2460
      green (44) = .0090
      green (45) = .0000
      green (46) = .8115
      green (47) = .6858
      green (48) = .6858
      green (49) = .6230
      green (50) = .3717
      green (51) = .1203
      green (52) = .0181
      green (53) = .8743
      green (54) = .8115
      green (55) = .7801
      green (56) = .7487
      green (57) = .4974
      green (58) = 0.210
      green (59) = .157
      green (60) = 1.000
      green (61) = 1.000
      green (62) = 1.000
      green (63) = 1.000
      green (64) = 1.000
      green (65) = .7487
      green (66) = .4973
      green (67) = .3951
      green (68) = .3185
      green (69) = 1.000
      green (70) = 1.00
      green (71) = 1.000
      green (72) = 1.000
      green (73) = .7487
      green (74) = .3185
      green (75) = .2500
      green (76) = 1.000
      green (77) = 1.000
      green (78) = 1.000
      green (79) = 1.000
      green (80) = .7487
      green (81) = .5000
      green (82) = .3950
      green (83) = 1.000
      green (84) = 1.000
      green (85) = 1.000
      green (86) = 1.000
      green (87) = 1.000
      green (88) = .7487
      green (89) = .0400
      green (90) = .0300
      green (91) = .3185
      green (92) = 1.000
      green (93) = 1.000
      green (94) = 1.000
      green (95) = 1.000
      green (96) = .1400
      green (97) = .4973
      green (98) = .3951
      green (99) = 1.000
      green (100) = 1.000
      green (101) = 1.000
      green (102) = 1.000
      green (103) = .7487
      green (104) = .0800
      green (105) = .0360
      green (106) = 1.000
      green (107) = 1.000
      green (108) = 1.000
      green (109) = 1.000
      green (110) = .6400
      green (111) = .1200
      green (112) = .0860
      green (113) = 1.000
      green (114) = 1.000
      green (115) = 1.000
      green (116) = 1.000
      green (117) = .7487
      green (118) = .4973
      green (119) = .3951
      green (120) = 1.000
      green (121) = 1.000
      green (122) = 1.000
      green (123) = 1.000
      green (124) = .7487
      green (125) = .4973
      green (126) = .3951
      green (127) = 1.000
      green (128) = 1.000
      green (129) = 1.000
      green (130) = 1.000
      green (131) = 1.000
      green (132) = .7487
      green (133) = .4973
      green (134) = .3951
      green (135) = .3185
      green (136) = .8743
      green (137) = .8115
      green (138) = .7801
      green (139) = .7487
      green (140) = .4974
      green (141) = .2460
      green (142) = .1438
      green (143) = .7800
      green (144) = .6850
      green (145) = .6858
      green (146) = .6230
      green (147) = .3717
      green (148) = .1203
      green (149) = .0181
      green (150) = .7480
      green (151) = .6230
      green (152) = .5600
      green (153) = .4973
      green (154) = .2463
      green (155) = .0367
      green (156) = .2500
      green (157) = .6858
      green (158) = .4973
      green (159) = .3951
      green (160) = .3185
      green (161) = .2460
      green (162) = .2500
      green (163) = .1430
      green (164) = .6230
      green (165) = .3951
      green (166) = .2418
      green (167) = .1396
      green (168) = .2500
      green (169) = .0050
      green (170) = .0035
      green (171) = .7480
      green (172) = .4970
      green (173) = .2410
      green (174) = .1396
      green (175) = 0.000
      green (176) = 0.067
      green (177) = 0.000
      green (178) = 0.000
      green (179) = 0.000
      green (180) = 0.497
      green (181) = 0.3180
      green (182) = 0.139
      green (183) = 0.000
      green (184) = 0.000
      green (185) = 0.000
      green (186) = 0.064
      green (187) = 0.750
      green (188) = 0.497
      green (189) = 0.318
      green (190) = 0.139
      green (191) = 0.000
      green (192) = 0.000
      green (193) = 0.000
      green (194) = 0.064
      green (195) = 0.000
      green (196) = 0.4793
      green (197) = 0.3185
      green (198) = 0.1396
      green (199) = 0.000
      green (200) = 0.000
      green (201) = 0.000
      green (202) = 0.015
      green (203) = 0.4973
      green (204) = 0.318
      green (205) = 0.139
      green (206) = 0.000
      green (207) = 0.000
      green (208) = 0.000
      green (209) = 0.000
      green (210) = 0.4973
      green (211) = 0.3185
      green (212) = 0.1396
      green (213) = 0.000
      green (214) = 0.000
      green (215) = 0.000
      green (216) = 0.000
      green (217) = 0.4973
      green (218) = 0.3185
      green (219) = 0.1396
      green (220) = 0.000
      green (221) = 0.000
      green (222) = 0.000
      green (223) = 0.000
      green (224) = 0.4973
      green (225) = 0.3185
      green (226) = 0.1396
      green (227) = 0.000
      green (228) = 0.0000
      green (229) = 0.0000
      green (230) = 0.0000
      green (231) = 0.7487
      green (232) = 0.623
      green (233) = 0.220
      green (234) = 0.3185
      green (235) = 0.1396
      green (236) = 0.250
      green (237) = 0.0899
      green (238) = 0.3740
      green (239) = 0.4973
      green (240) = 0.4973
      green (241) = 0.4973
      green (242) = 0.4973
      green (243) = 0.0452
      green (244) = 0.2500
      green (245) = 0.2200
      green (246) = 0.000
      green (247) = 0.7487
      green (248) = 0.623
      green (249) = 0.5602
      green (250) = 0.2460
      green (251) = 0.2500
      green (252) = 0.0000
      green (253) = 0.0000
      green (254) = 0.623
      green (255) = 0.2500
      green (256) = 0.0000
!
      blue (1) = 0.0
      blue (2) = 1.000
      blue (3) = 1.000
      blue (4) = 0.0000
      blue (5) = 1.0000
      blue (6) = 0.0000
      blue (7) = 0.0000
      blue (8) = 1.0000
      blue (9) = 1.0000
      blue (10) = 0.7
      blue (11) = 0.6230
      blue (12) = 0.4970
      blue (13) = 0.1300
      blue (14) = 0.0750
      blue (15) = 0.0000
      blue (16) = 1.0000
      blue (17) = 1.0000
      blue (18) = 0.5000
      blue (19) = 1.0000
      blue (20) = 0.7487
      blue (21) = 0.4973
      blue (22) = 0.0500
      blue (23) = 0.0360
      blue (24) = 0.3185
      blue (25) = 1.0000
      blue (26) = 1.0000
      blue (27) = 1.0000
      blue (28) = 1.0000
      blue (29) = 0.3951
      blue (30) = 0.0700
      blue (31) = 0.0560
      blue (32) = 1.0000
      blue (33) = 1.0000
      blue (34) = 1.0000
      blue (35) = 1.0000
      blue (36) = 0.7487
      blue (37) = 0.0800
      blue (38) = 0.0600
      blue (39) = 1.0000
      blue (40) = 1.0000
      blue (41) = 1.0000
      blue (42) = 1.0000
      blue (43) = 0.4787
      blue (44) = 0.0900
      blue (45) = 0.2500
      blue (46) = 1.0000
      blue (47) = 1.0000
      blue (48) = 1.0000
      blue (49) = 1.0000
      blue (50) = 0.7487
      blue (51) = 0.4973
      blue (52) = 0.3951
      blue (53) = 1.0000
      blue (54) = 1.0000
      blue (55) = 1.0000
      blue (56) = 1.0000
      blue (57) = 0.7487
      blue (58) = 0.3160
      blue (59) = 0.2370
      blue (60) = 1.0000
      blue (61) = 1.0000
      blue (62) = 1.0000
      blue (63) = 1.0000
      blue (64) = 1.0000
      blue (65) = 0.7487
      blue (66) = 0.4973
      blue (67) = 0.3951
      blue (68) = 0.3185
      blue (69) = 0.7487
      blue (70) = 0.6230
      blue (71) = 0.5602
      blue (72) = 0.4973
      blue (73) = 0.2460
      blue (74) = 0.0000
      blue (75) = 0.0000
      blue (76) = 0.6230
      blue (77) = 0.4462
      blue (78) = 0.3185
      blue (79) = 0.2500
      blue (80) = 0.0000
      blue (81) = 0.0000
      blue (82) = 0.0000
      blue (83) = 0.7487
      blue (84) = 0.4973
      blue (85) = 0.3185
      blue (86) = 0.1396
      blue (87) = 0.0000
      blue (88) = 0.0000
      blue (89) = 0.0187
      blue (90) = 0.0140
      blue (91) = 0.0000
      blue (92) = 0.4973
      blue (93) = 0.3185
      blue (94) = 0.1396
      blue (95) = 0.0000
      blue (96) = 0.0560
      blue (97) = 0.0000
      blue (98) = 0.0000
      blue (99) = 0.4973
      blue (100) = 0.2500
      blue (101) = 0.1396
      blue (102) = 0.0000
      blue (103) = 0.0000
      blue (104) = 0.0253
      blue (105) = 0.0114
      blue (106) = 0.4973
      blue (107) = 0.3185
      blue (108) = 0.1396
      blue (109) = 0.0000
      blue (110) = 0.0000
      blue (111) = 0.0140
      blue (112) = 0.0100
      blue (113) = 0.4970
      blue (114) = 0.3185
      blue (115) = 0.1590
      blue (116) = 0.0000
      blue (117) = 0.0000
      blue (118) = 0.0000
      blue (119) = 0.0000
      blue (120) = 0.4973
      blue (121) = 0.3185
      blue (122) = 0.1396
      blue (123) = 0.0000
      blue (124) = 0.0000
      blue (125) = 0.0000
      blue (126) = 0.0000
      blue (127) = 0.7487
      blue (128) = 0.4973
      blue (129) = 0.3185
      blue (130) = 0.1396
      blue (131) = 0.2500
      blue (132) = 0.0000
      blue (133) = 0.0000
      blue (134) = 0.0000
      blue (135) = 0.0000
      blue (136) = 0.4973
      blue (137) = 0.3185
      blue (138) = 0.1396
      blue (139) = 0.0000
      blue (140) = 0.0000
      blue (141) = 0.0000
      blue (142) = 0.0000
      blue (143) = 0.1390
      blue (144) = 0.1390
      blue (145) = 0.1396
      blue (146) = 0.0000
      blue (147) = 0.0000
      blue (148) = 0.0000
      blue (149) = 0.0000
      blue (150) = 0.4970
      blue (151) = 0.3180
      blue (152) = 0.1390
      blue (153) = 0.0000
      blue (154) = 0.0000
      blue (155) = 0.0000
      blue (156) = 0.0000
      blue (157) = 0.4973
      blue (158) = 0.3185
      blue (159) = 0.1396
      blue (160) = 0.0000
      blue (161) = 0.0670
      blue (162) = 0.0000
      blue (163) = 0.0000
      blue (164) = 0.4973
      blue (165) = 0.3185
      blue (166) = 0.1396
      blue (167) = 0.0000
      blue (168) = 0.0000
      blue (169) = 0.0000
      blue (170) = 0.0000
      blue (171) = 0.7480
      blue (172) = 0.3180
      blue (173) = 0.1390
      blue (174) = 0.1396
      blue (175) = 0.0000
      blue (176) = 0.0000
      blue (177) = 0.0000
      blue (178) = 0.0022
      blue (179) = 0.0000
      blue (180) = 0.7480
      blue (181) = 0.6230
      blue (182) = 0.5600
      blue (183) = 0.4970
      blue (184) = 0.4630
      blue (185) = 0.0000
      blue (186) = 0.0000
      blue (187) = 1.0000
      blue (188) = 1.0000
      blue (189) = 1.0000
      blue (190) = 1.0000
      blue (191) = 1.0
      blue (192) = 0.7480
      blue (193) = 0.3120
      blue (194) = 0.0000
      blue (195) = 0.3185
      blue (196) = 1.0000
      blue (197) = 1.0000
      blue (198) = 1.0000
      blue (199) = 1.0000
      blue (200) = 0.4787
      blue (201) = 0.1550
      blue (202) = 0.0000
      blue (203) = 1.0000
      blue (204) = 1.0000
      blue (205) = 1.0000
      blue (206) = 1.0000
      blue (207) = 0.7480
      blue (208) = 0.2500
      blue (209) = 0.0000
      blue (210) = 1.0000
      blue (211) = 1.0000
      blue (212) = 1.0000
      blue (213) = 1.0000
      blue (214) = 0.4973
      blue (215) = 0.0000
      blue (216) = 0.0500
      blue (217) = 1.0000
      blue (218) = 1.0000
      blue (219) = 1.0000
      blue (220) = 1.0000
      blue (221) = 0.7487
      blue (222) = 0.0600
      blue (223) = 0.0000
      blue (224) = 1.0000
      blue (225) = 1.0000
      blue (226) = 1.0000
      blue (227) = 1.0000
      blue (228) = 0.7487
      blue (229) = 0.0000
      blue (230) = 0.3951
      blue (231) = 0.7487
      blue (232) = 0.6230
      blue (233) = 0.2200
      blue (234) = 0.3185
      blue (235) = 0.1396
      blue (236) = 0.1250
      blue (237) = 0.4973
      blue (238) = 0.2800
      blue (239) = 0.4973
      blue (240) = 0.0000
      blue (241) = 0.0899
      blue (242) = 0.4973
      blue (243) = 0.0479
      blue (244) = 0.0000
      blue (245) = 0.1500
      blue (246) = 0.0050
      blue (247) = 0.5602
      blue (248) = 0.5601
      blue (249) = 0.4345
      blue (250) = 0.0672
      blue (251) = 0.0000
      blue (252) = 0.0000
      blue (253) = 0.0000
      blue (254) = 0.3951
      blue (255) = 0.0000
      blue (256) = 0.0000
!          irgbflg = 0 if no rgb color codes
!          irgbflg = 1 if rgb codes color interpolation
!          irgblfg = 2 if rgb codes conplot interpolation
!              igrbflg = 2 and 3 disabled - no conplot interpolation
!          irgbflg = 3 if conplot interpolation and codes converted
!                      from conplot table
      obj%rgbflg = 0
      obj%interp = 1
      obj%irspp = 'NO'
!
      if (obj%path_cbar.eq.PATHCHECK_EMPTY) then
         select case (obj%bars)
         case ("AVO133")
           obj%path_cbar='/usr/app/user/sps/color_bars/avo133.rgb'
         case ("BLUERED")
           obj%path_cbar='/usr/app/user/sps/color_bars/bluered.rgb'
         case ("BLUEWHT")
           obj%path_cbar='/usr/app/user/sps/color_bars/bluewht.rgb'
         case ("MEDRAM")
           obj%path_cbar='/usr/app/user/sps/color_bars/medram.rgb'
         case ("MEDRAM6")
           obj%path_cbar='/usr/app/user/sps/color_bars/medram6.rgb'
         case ("STAN")
           obj%path_cbar='/usr/app/user/sps/color_bars/stan.rgb'
         case ("VELC")
           obj%path_cbar='/usr/app/user/sps/color_bars/velc.rgb'
         end select
       endif
       call getlun(obj%icf,istat)
       if(istat.ne.0)then
          call pc_error('COLOR-->Unable to get a unit number for color bar')
       endif
       open(obj%icf,iostat=istat,status='OLD',file=obj%path_cbar)
       if(istat.ne.0)then
         call pc_error('COLOR-->Unable to open color bar file')
       endif         
       if (obj%ncl_at1.gt.0) goto 8965
       k = 1
       kflag=0
!          read color codes in from a file
       DO 
         read(obj%icf,'(A)',iostat=istat)card
         if(istat.lt.0)exit
         call string_cc2ii(card(2:4),kflag)
         if(kflag.eq.999.or.kflag.eq.998)go to 8942
         read(card,9051)kolorst(k),valst(k)
         k=k+1
       ENDDO
       obj%ncolr = k - 1
!!      else
!!         obj%ncolr = 32
!!         goto 8945
!!      endif ! obj%path_cbar.eq.PATHCHECK_EMPTY
 8942 continue
!          colors in file are in red,green,blue format
      k = 101
      kk = 1
      if (kflag .eq.999.or.kflag .eq.998) then
         obj%rgbflg = 1
 8944    continue
         if (kflag.eq.999) then
            read(obj%icf,9053,end=8943)red(k),green(k),blue(k),valst(kk)
         else
            read(obj%icf,9054,end=8943)red(k),green(k),blue(k),valst(kk)
         endif
         kolorst(kk)=k
         kk = kk + 1
         k = k + 1
         goto 8944
 8943    continue
         obj%ncolr = kk - 1
      endif
      obj%irepeat = 1
      obj%ncolro = obj%ncolr
      call mem_alloc(obj%kolors,obj%ncolr)
      call mem_alloc(obj%vals,obj%ncolr)
      if (obj%path_cbar.ne.PATHCHECK_EMPTY) then
         dif = valst (2) - valst (1)
         add = dif / real (obj%irepeat)
         start = valst (1) - add * real (obj%irepeat)
         valst (obj%ncolro) = valst (obj%ncolro - 1) + add
         k = 1
         do 8950 i = 1, obj%ncolro
            obj%kolors (k) = kolorst (i)
            obj%vals (k) = start + add
            k = k + 1
            do 8949 ii = k, k + obj%irepeat - 2
               obj%kolors (ii) = kolorst (i)
               obj%vals (ii) = obj%vals (ii - 1) + add
 8949       end do
            k = k + obj%irepeat - 1
            dif = valst (i + 1) - valst (i)
            add = dif / real (obj%irepeat)
            start = valst (i + 1) - add * real (obj%irepeat)
 8950    end do
      else
         k = 1
         do 8960 i = 1, obj%ncolro
            obj%kolors (k) = istan (i)
            k = k + 1
            do 8959 ii = k, k + obj%irepeat - 2
               obj%kolors (ii) = istan (i)
 8959       end do
            k = k + obj%irepeat - 1
 8960    end do
      endif
 8965 obj%ktrnum = 1
      obj%glav = 0.0
      obj%nrlc = 1
      if (obj%ntpp.gt.0) then
         j = obj%ntpp * obj%nrow
         k = mod (obj%num_tr, j)
         obj%ncol = obj%num_tr / j + .0001
         if (k.gt.0) then
            m = obj%num_tr - (obj%ncol * j)
            obj%nrlc = m / obj%ntpp
            obj%ncol = obj%ncol + 1
         endif
      endif
!          if paneling and user has not set blank traces - set it here
      if (obj%ntpp.gt.0.and.obj%nbts.eq.0) then
         obj%btskp (1) = obj%ntpp
!             make btdo large enough to plot timing line number
         b = obj%cs_tl_eng * 3.0
         if (obj%ups.gt.0) then
            f = log10 (real (obj%ups) ) + 1.5
            b = obj%cs_tl_eng * f
         endif
         k = b * obj%tpi_eng + 2.500001
         obj%btdo (1) = k
         obj%btbtwn (1) = obj%ntpp
         obj%bttot (1) = obj%ncol - 1
         obj%nbts = 1
         if (obj%ncol.gt.1) then
            obj%irtbp = 'YES'
         else
            obj%irtbp = 'NO'
         endif
         write(print_lun,*) ' COLOR-BLANK TRACE PATTERN SET AS BTSKP = ', &
                              obj%btskp(1), &
                            ' BTDO = ', obj%btdo (1) , ' BTBTWN = ',&
                              obj%btbtwn (1) , ' BTTOT = ',&
                              obj%bttot (1)
         obj%iuserbt = 0
      elseif (obj%nbts.gt.0) then
         write(print_lun,*)' COLOR=> USER HAS SET THEIR OWN BLANK TRACE PATTERN'
         obj%iuserbt = 1
             !  (nttp.gt.0.and.obj%nbts.eq.0)
      endif
!
      obj%nsamporig=obj%nsamp
      spi = obj%nsamp / (obj%time * obj%ips_eng)
      if (spi.ge.100.0) obj%grade_vrt = 'NO'
      obj%ntrpv = 1
      if (obj%grade_vrt.eq.'NO') goto 2300
!          determine vertical interpolation
      ntrp = 1
      spin = spi
      if (obj%rgbflg.ge.2) obj%interp = 2
 1900 if (spin.le.200.0) goto 2000
!
!          need to cut samples
      ntrp = ntrp + 1
      spin = spi / ntrp
      goto 1900
 2000 obj%nsamp = obj%nsamp / ntrp
!cc   nsampt=nsampt/ntrp
      if (obj%grade_vrt.eq.'NO'.or.obj%rgbflg.ge.2) goto 2300
 2100 if (spin.ge.100) goto 2200
!
!          need to increase samples
      spin = spi
      obj%ntrpv = obj%ntrpv + 1
      spin = spi * obj%ntrpv
      goto 2100
 2200 obj%nsamp = obj%nsamp * obj%ntrpv
!cc   nsampt=obj%nsamp+nsampz*obj%ntrpv
 2300 continue
!
!        determine number of percentage boxes to use for pnc-ppc-pplc
!         opts
      obj%nbox = 0
      if (obj%vmin.ne.0.0.or.obj%vmax.ne.0.0) goto 2400
      if (obj%pnc.eq.0.0.and.obj%ppc.eq.0.0) goto 2400
      obj%nbox = 100
      idm = obj%pnc + .0001
      fdm = idm
      frac = obj%pnc - fdm
      if (frac.ne.0.0) obj%nbox = 1000
      obj%plc = obj%pnc / 100.
      if (obj%nbox.gt.100) goto 2380
!
!          determine number of boxes needed for high cut
      idm = obj%ppc + .0001
      fdm = idm
      frac = obj%ppc - fdm
      if (frac.ne.0.0) obj%nbox = 1000
 2380 obj%phc = obj%ppc / 100.
      if (obj%pplc.ne. - 1.0) obj%pplc = obj%pplc / 100.
!
 2400 continue
!
!           call get storage here
      call mem_alloc(obj%trcb,obj%nsamp)
      if (obj%nbox.gt.0) then
         call mem_alloc (obj%hgneg, obj%nbox)
         call mem_alloc(obj%hgpos, obj%nbox)
      endif
      if (obj%ntpp.gt.0.or.obj%opt_semb.eq.'SEMB')then
!-----   Need a direct access trace file (old dtrot)
!!        call gets (ipdtrot, 12528)
      endif
!
!          calculate samples to multiple of bits in a word for pack routine
      nsamp_double = obj%nsamp * 2
      nsmpmultd = nsamp_double
      obj%nsampmult = obj%nsamp
      obj%nwrdsmult = obj%nsamp / nbytes
      obj%nwrdsmult_double = nsamp_double / nbytes
      k = mod (obj%nsamp, nbits)
      if (k.ne.0) then
         obj%nsampmult = obj%nsamp / nbits + 1
         obj%nsampmult = obj%nsampmult * nbits
!          number of words needed to hold packed trace (nbytes values per
!             word)
         k = mod (obj%nsamp, nbytes)
         obj%nwrdsmult = obj%nsampmult / nbytes
      endif
      k = mod (nsamp_double, nbits)
      if (k.ne.0) then
         nsmpmultd = nsamp_double / nbits + 1
         nsmpmultd = nsmpmultd * nbits
!          number of words needed to hold 2 packed traces (nbytes values per
!             word)
         k = mod (nsamp_double, nbytes)
         obj%nwrdsmult_double = nsmpmultd / nbytes
         obj%nwrdsmult = obj%nwrdsmult_double
      endif
      obj%ncharpack=obj%nwrdsmult_double / nbytes
!
!
!
      nstor = obj%nshots*5+obj%ncolr*2+obj%nwih*obj%nrow+obj%nsamp+12336 + 48 &
      + obj%nbts * 4
      nstor = nstor+obj%nslbl*4+768+obj%ntistr*7+obj%nplbl*6
      if (obj%dev_rem.ne.'POEPLT') nstor = nstor + 2
      if (obj%path_ovl.ne.' ') nstor = nstor + 2
      if (obj%ncl_at1.gt.0) nstor = nstor + 8
      if (obj%nbox.gt.0) nstor = nstor + obj%nbox * 2
      nscr= 290+obj%nsamp*2.0+obj%nwih*obj%nrow+obj%nsampmult&
            +obj%nwrdsmult_double
      if (obj%opt_scale.eq.'CALC') nscr = nscr + 5001
      if (obj%ncl_at1.gt.0) nscr = nscr + 81 + 18

!          Initialize colrplot
       call getlun(obj%iunit,istat)
       if(istat.ne.0)then
         call pc_error('COLOR--> Unable to get unit number for plot file')
         return
       endif
       itype='NEW'
       call colrplot_init(itype,istat,obj%iunit)
       if(istat.ne.0)then
         write(print_lun,*)'COLOR-->Status from colrplot_init = ',istat
         call pc_error('COLOR-->Unable to initialize colrplot')
       endif
!
!          set up trcio files
        obj%isn => trcio_open('%COLRA','w')
        if(.not.associated(obj%isn))then
          call pc_error('COLOR Abort - unable to open trcio file isn')
        else
!               if paneling - use the number of paneled values
          ntmp = obj%ndpt
          nheader=obj%nwih
          if (obj%ntpp.gt.0.or.obj%opt_semb.eq.'SEMB') then
            obj%nsbtwn = obj%timbtwn / obj%dt + .0001
            ntmp = obj%nvi * obj%nrow + obj%nsbtwn * (obj%nrow - 1)
            nheader=obj%nwih*obj%nrow
          endif
          obj%isn%nwih = nheader
          obj%isn%num_values = ntmp
          obj%isn%nbits = nbits
          obj%isn%nbits_hd = 64
          obj%isn%dt=obj%dt
          obj%isn%tmin=obj%tstrt
          if(nbits.eq.32)then
            obj%isn%wtype='IEEE'
          else
            obj%isn%wtype='INT'
          endif
          istat=trcio_writeheader(obj%isn)
          if(istat.ne.TRCIO_OK)then
            call pc_error('COLOR-->Error in trcio_writeheader file isn')
          endif
        endif
!
!          set up second trace file if needed when opt_calc=all
      if(obj%opt_calc.eq.'ALL'.and.(obj%opt_scale.eq.'CALC'.or.obj%nbox.gt.0))&
         then
        obj%isn2 => trcio_open('%CLR2A','w')
        if(.not.associated(obj%isn2))then
          call pc_error('COLOR Abort - unable to open trcio file isn2')
        else
          obj%isn2%nwih = obj%nwih
          obj%isn2%num_values = obj%ndpt
          obj%isn2%nbits = nbits
          obj%isn2%nbits_hd = 64
          obj%isn2%dt=obj%dt
          obj%isn2%tmin=obj%tstrt
          if(nbits.eq.32)then
            obj%isn2%wtype='IEEE'
          else
            obj%isn2%wtype='INT'
          endif
          istat=trcio_writeheader(obj%isn2)
          if(istat.ne.TRCIO_OK)then
            call pc_error('COLOR-->Error in trcio_writeheader file isn2')
          endif
        endif
      endif
!
!          set up 
      if (obj%ntpp.gt.0.or.obj%opt_semb.eq.'SEMB') then
        obj%idn => trcio_open('COLRDA','w')
        if(.not.associated(obj%idn))then
          call pc_error('COLOR Abort - unable to open trcio file idn')
        else
          obj%idn%nwih = obj%nwih
          obj%idn%num_values = obj%ndpt
          obj%idn%nbits = nbits
          obj%idn%nbits_hd = 64
          obj%idn%dt=obj%dt
          obj%idn%tmin=obj%tstrt
          if(nbits.eq.32)then
            obj%idn%wtype='IEEE'
          else
            obj%idn%wtype='INT'
          endif
          istat=trcio_writeheader(obj%idn)
          if(istat.ne.TRCIO_OK)then
            call pc_error('COLOR-->Error in trcio_writeheader file idn')
          endif
        endif
      endif
!
!
      write(print_lun,*)' END           - COLOR SETUP'

!!! Allocate your permanent memory like this:
!!! This code can be modified from your old back-end GETS and HPALLOC calls.
!!! Be sure to test the returned status and register an error message if not 0.
!!!
!!!   allocate(obj%hdup(obj%nfold,obj%nwih), stat=ier1)
!!!   allocate(obj%tdup(obj%nfold,obj%ndpt), stat=ier2)
!!!   if (ier1 /= 0) call pc_error  &
!!!           ('error allocating HDUP to',obj%nfold*obj%nwih,'array elements')
!!!   if (ier2 /= 0) call pc_error  &
!!!           ('error allocating TDUP to',obj%nfold*obj%ndpt,'array elements')
!!!
!!! or more simply:
!!!
!!!   call mem_alloc (obj%hdup, obj%nfold, obj%nwih)
!!!   call mem_alloc (obj%tdup, obj%nfold, obj%ndpt)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

!!! Then put any additional code needed before starting execution here.
!!! This could be code to open files, initialize allocated arrays, etc.
!!! This code can be copied from your old back-end setup code.

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return 
 9051 format(1x,i3,1x,f13.3)
 9053 format(4(1x,f13.3))
 9054 format(4(1x,f13.4))
      end subroutine color_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


      subroutine color_copies_trap(keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_copies_trap

      subroutine color_tldots_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_tldots_trap

      subroutine color_vl_tot_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_vl_tot_trap

      subroutine color_ups_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_ups_trap

      subroutine color_ncl_at1_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_ncl_at1_trap

      subroutine color_ncl_at2_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_ncl_at2_trap

      subroutine color_skip_init_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_skip_init_trap

      subroutine color_skip_init_ovl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_skip_init_ovl_trap

      subroutine color_ntr_ovl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_ntr_ovl_trap

      subroutine color_nctfo_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%nctfo > object%num_tr) then 
        call pc_error('NCTFO cannot exceed NT') 
      endif 
      return
      end subroutine color_nctfo_trap

      subroutine color_num_tr_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_num_tr_trap

      subroutine color_ntpp_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      integer :: nplbl,nrow

      if (object%ntpp<0 .or. object%ntpp>object%num_tr) then 
        call pc_error('NTPP must be between 1 and NUM_TR') 
      endif 
      if (object%ntpp == 0) then 
        nplbl = 0 
        call pc_put_sensitive_arrayset_flag('PCOL_ARRAYSET',.false.)
        object%pcol=0
        object%prow=0
        object%plbl=' '
        object%nplbl=0
        object%nrow=1
        call pc_put_sensitive_field_flag('CS_PL',.false.)
        nrow = 1 
        call pc_put_sensitive_field_flag('NROW',.false.)
      else 
        call pc_put_sensitive_field_flag('CS_PL',.true.)
        if (object%cs_pl <= 0.0001) then 
           object%cs_pl = 0.3 
        endif 
        call pc_put_sensitive_arrayset_flag('PCOL_ARRAYSET',.true.)
        call pc_put_sensitive_field_flag('NROW',.true.)
      endif                                      ! nttp.eq.0 
      call color_english
      return
      end subroutine color_ntpp_trap

      subroutine color_nrow_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%nrow.lt.1)then
        object%nrow=1
        call pc_warning('NROW has been reset to 1')
      endif
      if (object%nrow > 26) then 
        object%nrow = 26 
        call pc_warning('Number of rows is limited to 26') 
      endif 
      return
      end subroutine color_nrow_trap

      subroutine color_hdr_lab_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      integer :: i
      character(len=80) :: card
      do i=1,object%nhdr_lab
        if(object%hdr_lab(i) < 0 .or.object%hdr_lab(i) > object%nwih)then
          write(card,*)'hdr_lab ',i,' is out of the range of allowable &
     &                  header words'
          call pc_error(card)
        endif
      enddo
!            HDR_LAB
      if(object%hdr_lab(1).eq.0 .and. object%hdr_lab(2).eq.0)then
        call pc_put_sensitive_field_flag('LAB_INIT',.false.)
        call pc_put_sensitive_field_flag('LAB_INC',.false.)
        call pc_put_sensitive_field_flag('NAM_LAB',.false.)
        call pc_put_sensitive_field_flag('CS_LAB',.false.)
      else if(object%hdr_lab(1).gt.0)then
        call pc_put_sensitive_field_flag('LAB_INIT',.true.)
        call pc_put_sensitive_field_flag('LAB_INC',.true.)
        call pc_put_sensitive_field_flag('NAM_LAB',.true.)
        call pc_put_sensitive_field_flag('CS_LAB',.true.)
      endif
      if(object%hdr_lab(1).eq.0)then
        call pc_put_sensitive_arrayset_flag('TRCE_ARRAYSET',.true.)
      else
        call pc_put_sensitive_arrayset_flag('TRCE_ARRAYSET',.false.)
        object%trce=0
        object%shot=' '
        object%intv=0
        object%incr=0
        object%totl=0
        object%nshots=0
      endif
      call color_shot_sensitive
      return
      end subroutine color_hdr_lab_trap

      subroutine color_shot_sensitive
      if (object%trce(1)<=0 .and. object%hdr_lab(1)==0) then
        call pc_put_sensitive_field_flag('LTAB',.false.)
        object%ltab = 'NO'
      else
        call pc_put_sensitive_field_flag('LTAB',.true.)
      endif
      if(object%nshots.gt.0.or.object%hdr_lab(1).gt.0)then
        call pc_put_sensitive_array_flag('NAM_LAB', .true.)
      else
        call pc_put_sensitive_array_flag('NAM_LAB', .false.)
      endif

      end subroutine color_shot_sensitive

      subroutine color_lab_inc_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_lab_inc_trap

      subroutine color_ntr_calc_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%opt_calc.eq.'PLOT')return
      if (object%ntr_calc <= 0.and.object%opt_calc.eq.'ALL') then 
        call pc_error('You must answer NTR_CALC if OPT_CALC is ALL') 
        return  
      endif 
      if (object%ntr_calc < object%num_tr)then
        call pc_warning('WARNING - THE NTR_CALC parameter is smaller than &
                        &NUM_TR') 
      endif
      return
      end subroutine color_ntr_calc_trap

      subroutine color_hdr_dl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%hdr_dl > 0) then 
        call pc_put_sensitive_array_flag('DLREF', .true.)
        call pc_put_sensitive_array_flag('DLU100', .true.)
        if (object%hdr_dl > object%nwih) then 
          call pc_error('HDR_DL must be between 0 and NWIH') 
          return  
        endif 
      else 
        call pc_put_sensitive_array_flag('DLREF', .false.)
        call pc_put_sensitive_array_flag('DLU100', .false.)
        object%dlref = 0 
        object%dlu100 = 0 
      endif 
      return
      end subroutine color_hdr_dl_trap

      subroutine color_time_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      real :: gtime
      integer :: n
      gtime = object%ndpt*object%dt - object%dt
      if(object%time.gt.(gtime+0.00001))then
        call pc_warning('TIME cannot exceed global time - TIME reset to &
     &                  global time')
        object%time = gtime
      endif
!          check for max allowed points
      n = object%time/object%dt 
      if (object%ntpp > 0) n = (object%time+object%timbtwn)&
                                *object%nrow/object%dt-object%timbtwn/object%dt 
      if (object%path_ovl/=PATHCHECK_EMPTY.and. n>4096) then 
        call pc_error('Number of sample points cannot exceed 4096 when &
                      &plotting overlay')
      endif 
      if (n > 8192) then 
        call pc_error('Number of sample points cannot exceed 8192') 
      endif 
      return
      end subroutine color_time_trap

      subroutine color_ips_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english

      return
      end subroutine color_ips_trap

      subroutine color_cs_sid_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      return
      end subroutine color_cs_sid_trap

      subroutine color_cs_spl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      return
      end subroutine color_cs_spl_trap

      subroutine color_opt_cbar_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call pc_put_sensitive_field_flag('VMIN',.false.)
      call pc_put_sensitive_field_flag('VMAX',.false.)
      call pc_put_sensitive_field_flag('PPC',.false.)
      call pc_put_sensitive_field_flag('PNC',.false.)
      call pc_put_sensitive_field_flag('PPLC',.false.)
      select case (object%opt_cbar)
      case ('CBAR')
        object%vmin=0
        object%vmax=0
        object%ppc=0
        object%pnc=0
        object%pplc=0
      case ('VMIN_VMAX')
        call pc_put_sensitive_field_flag('VMIN',.true.)
        call pc_put_sensitive_field_flag('VMAX',.true.)                
        object%ppc=0
        object%pnc=0
        object%pplc=0
      case ('PCTL')        
        object%vmin=0
        object%vmax=0
        call pc_put_sensitive_field_flag('PPC',.true.)
        call pc_put_sensitive_field_flag('PNC',.true.)
        call pc_put_sensitive_field_flag('PPLC',.true.)
      end select
      return
      end subroutine color_opt_cbar_trap

      subroutine color_vmin_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_vmin_trap

      subroutine color_vmax_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_vmax_trap

      subroutine color_cs_cbl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      return
      end subroutine color_cs_cbl_trap

      subroutine color_tpi_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      return
      end subroutine color_tpi_trap

      subroutine color_sdas_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      return
      end subroutine color_sdas_trap

      subroutine color_cs_tl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
       if (survey_units.eq.'METERS') then
         if (object%cs_tl > 2.54) then
           call pc_warning('WARNING - CS_TL exceeds 2.54 centimeters')
         endif
       else
         if (object%cs_tl > 1.0)then
            call pc_warning('WARNING - CS_TL exceeds 1 inch')
         endif
       endif
       if (object%cs_tl <= 0.0) then
         call pc_error ('CS_TL must be greater than zero')
       endif
      return
      end subroutine color_cs_tl_trap

      subroutine color_wid_vl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      return
      end subroutine color_wid_vl_trap

      subroutine color_pnc_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%pnc>100.0 .or. object%pnc<0.0) then 
        call pc_error('Range of PNC is 0 - 100 percent') 
      endif 
      if (object%pnc /= 0.0) then 
        call pc_put_sensitive_field_flag('PPLC',.false.)
      else 
        if(object%opt_cbar.eq.'PCTL')then
          call pc_put_sensitive_field_flag('PPLC',.true.)
        endif
      endif 
      return
      end subroutine color_pnc_trap

      subroutine color_ppc_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%ppc>100.0 .or. object%ppc<0.0) then 
        call pc_error('Range of PPC is 0 - 100 percent') 
      endif 
      if (object%ppc == 0.0) then 
        call pc_put_sensitive_field_flag('OPT_SCALE',.true.)
      else 
        call pc_put_sensitive_field_flag('OPT_SCALE',.false.)
        call pc_put_sensitive_field_flag('OPT_SCALE',.true.)
        object%opt_scale='NONE'
      endif 
      return
      end subroutine color_ppc_trap

      subroutine color_pplc_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%pplc>100.0 .or. object%pplc<0.0) then 
        call pc_error('Range of PPLC is 0 - 100 percent') 
      endif 
      return
      end subroutine color_pplc_trap

      subroutine color_cs_bl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
       if (survey_units.eq.'METERS') then
         if (object%cs_bl > 2.54) then
           call pc_warning('WARNING - CS_BL exceeds 2.54 centimeters')
         endif
       else
         if (object%cs_bl > 1.0)then
            call pc_warning('WARNING - CS_BL exceeds 1 inch')
         endif
       endif
       if (object%cs_bl <= 0.0) then
         call pc_error ('CS_BL must be greater than zero')
       endif
      return
      end subroutine color_cs_bl_trap

      subroutine color_frtb_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%frtb > 1.0) then 
        call pc_error('ERROR - FRTB must be 1.0 or less') 
      endif 
      return
      end subroutine color_frtb_trap

      subroutine color_ar_ht_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      if (survey_units == 'METERS') then
        if (object%ar_ht > 2.54)then
          call pc_warning('AR_HT exceeds 2.54 centimeters')
        endif
      else
        if (object%ar_ht > 1.0)then
           call pc_warning('WARNING - AR_HT exceeds 1 inch')
        endif
      endif
      return
      end subroutine color_ar_ht_trap

      subroutine color_ar_len_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      if (survey_units == 'METERS') then
        if (object%ar_len < 2.54) then
          call pc_warning('WARNING - AR_LEN shorter than 2.54 centimeters')
        endif
      else
        if(object%ar_len < 1.0)then
           call pc_warning('WARNING - AR_LEN shorter than 1 inch')
        endif
      endif
      if (object%ar_len <= 0.0) then
        call pc_error('AR_LEN must be greater than zero')
      endif
      return
      end subroutine color_ar_len_trap

      subroutine color_arda_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      return
      end subroutine color_arda_trap

      subroutine color_tida_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      return
      end subroutine color_tida_trap

      subroutine color_cs_lt_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      return
      end subroutine color_cs_lt_trap

      subroutine color_ct_ovl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_ct_ovl_trap

      subroutine color_zt_ovl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      logical :: almost_equal
      real :: gtime
      gtime = object%ndpt*object%dt - object%dt 
      almost_equal = ameq(gtime,object%zt_ovl,0.00001) 
      if (object%zt_ovl>gtime .and. .not.almost_equal) then 
        call pc_error('ZT_OVL may not exceed the time you specified on the &
                      &initial job screen') 
      endif 
      return
      end subroutine color_zt_ovl_trap

      subroutine color_tpi_ovl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      logical :: zero
      zero=ameq(object%tpi_ovl,0.0,.00001)
      if(zero)then
        call pc_put_sensitive_field_flag('RATIO_TPI',.true.)
      else
        call pc_put_sensitive_field_flag('RATIO_TPI',.false.)
      endif      
      call color_english
      return
      end subroutine color_tpi_ovl_trap

      subroutine color_cs_lab_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      integer :: i
      call color_english
      DO i=1,2
        if (object%cs_lab_eng(i) > 1.0)then
          call pc_warning ('WARNING - CS_LAB exceeds 1 inch')
        endif
        if (object%cs_lab_eng(i) <= 0.0) then
          call pc_error('CS_LAB must be greater than zero')
        endif
      ENDDO
      return
      end subroutine color_cs_lab_trap

      subroutine color_scale_vel_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_scale_vel_trap

      subroutine color_frcb_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_frcb_trap

      subroutine color_coor_beg_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_coor_beg_trap

      subroutine color_coor_end_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_coor_end_trap

      subroutine color_cs_pl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      return
      end subroutine color_cs_pl_trap

      subroutine color_scda_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      return
      end subroutine color_scda_trap

      subroutine color_cs_scale_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      if (mtrc == 'YES') then 
        if (object%cs_scale > 2.54)then
           call pc_warning('WARNING - CS_SCALE exceeds 2.54 centimeters')
        endif
        if (object%cs_scale <= 0.0) object%cs_scale = 0.33 
      else 
        if (object%cs_scale > 1.0)then
           call pc_warning('WARNING - CS_SCALE exceeds 1 inch') 
        endif
        if (object%cs_scale <= 0.0) object%cs_scale = 0.13 
      endif 
      return
      end subroutine color_cs_scale_trap

      subroutine color_dlref_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_dlref_trap

      subroutine color_dlu100_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_dlu100_trap

      subroutine color_path_cbar_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%path_cbar.eq.PATHCHECK_EMPTY)return
      call pathcheck('path_cbar',object%path_cbar)
      return
      end subroutine color_path_cbar_trap

      subroutine color_path_atbm_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      logical :: there
      if (object%path_atbm /= PATHCHECK_EMPTY) then 
         call pathcheck('path_atbm',object%path_atbm)
         call pc_put_sensitive_field_flag('FRTB',.true.)
         inquire(file=object%path_atbm,exist=there) 
         if (.not.there) call pc_warning('Title block file does not exist') 
       else 
         call pc_put_sensitive_field_flag('FRTB',.false.)
       endif 
       return
       end subroutine color_path_atbm_trap

      subroutine color_path_blk_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%path_blk.eq.PATHCHECK_EMPTY)return
      call pathcheck('path_blk',object%path_blk,'blk')

!          BLOCK LABELS
      if(object%path_blk.ne.PATHCHECK_EMPTY)then
        call pc_put_sensitive_field_flag('CS_BL',.true.)
      else
        call pc_put_sensitive_field_flag('CS_BL',.false.)
      endif
      return
      end subroutine color_path_blk_trap

      subroutine color_path_tie1_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
       if(object%path_tie1.eq.PATHCHECK_EMPTY)return
       call pathcheck('path_tie1',object%path_tie1,'tie')
       call color_tie_sensitive
      return
      end subroutine color_path_tie1_trap

      subroutine color_path_tie2_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
       if(object%path_tie2.eq.PATHCHECK_EMPTY)return
       call pathcheck('path_tie2',object%path_tie2,'tie')
       call color_tie_sensitive
      return
      end subroutine color_path_tie2_trap

      subroutine color_tie_sensitive
      if(object%path_tie1.ne.PATHCHECK_EMPTY)then
        call pc_put_sensitive_field_flag('CS_TL',.true.)
        if(object%cald.eq.'NO')call pc_put_sensitive_field_flag('TIDA',.true.)
      else
        call pc_put_sensitive_field_flag('CS_TL',.false.)
        call pc_put_sensitive_field_flag('TIDA',.false.)
      endif
      end subroutine color_tie_sensitive

      subroutine color_path_vel_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%path_vel.eq.PATHCHECK_EMPTY)return
      call pathcheck('path_vel',object%path_vel,'vel')
      return
      end subroutine color_path_vel_trap

      subroutine color_path_ovl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      integer :: n
      if (object%path_ovl == PATHCHECK_EMPTY) then 
        call pc_put_sensitive_field_flag ('WT_OVL',.false.) 
        call pc_put_sensitive_field_flag ('VA_OVL',.false.) 
        call pc_put_sensitive_field_flag ('CT_OVL',.false.) 
        call pc_put_sensitive_field_flag ('TPI_OVL',.false.) 
        call pc_put_sensitive_field_flag ('ZT_OVL',.false.) 
        call pc_put_sensitive_field_flag ('SKIP_INIT_OVL',.false.) 
        call pc_put_sensitive_field_flag ('NTR_OVL',.false.) 
        call pc_put_sensitive_field_flag ('NCTFO',.false.) 
        call pc_put_sensitive_field_flag ('RP_OVL',.false.) 
        object%rp_ovl = 'NO' 
        object%zt_ovl = 0.0 
        object%skip_init_ovl = 0 
        object%ntr_ovl = 0 
        object%nctfo = 1 
      else 
        call pathcheck('path_ovl',object%path_ovl)
        call pc_put_sensitive_field_flag ('WT_OVL',.true.) 
        call pc_put_sensitive_field_flag ('VA_OVL',.true.) 
        call pc_put_sensitive_field_flag ('CT_OVL',.true.) 
        call pc_put_sensitive_field_flag ('TPI_OVL',.true.) 
        call pc_put_sensitive_field_flag ('ZT_OVL',.true.) 
        call pc_put_sensitive_field_flag ('SKIP_INIT_OVL',.true.) 
        call pc_put_sensitive_field_flag ('NTR_OVL',.true.) 
        call pc_put_sensitive_field_flag ('NCTFO',.true.) 
        call pc_put_sensitive_field_flag ('RP_OVL',.true.) 
!          check for max allowed points
        n = object%time/object%dt 
        if (n > 4096) then 
          call pc_warning('WARNING - Number of sample points cannot exceed &
                         &4096 when plotting overlay')
          return  
        endif 
      endif 
      return
      end subroutine color_path_ovl_trap

      subroutine color_path_vect_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%path_vect.eq.PATHCHECK_EMPTY)return
      call pathcheck('path_vect',object%path_vect)
      return
      end subroutine color_path_vect_trap

      subroutine color_sid_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_sid_trap

      subroutine color_lab1_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_lab1_trap

      subroutine color_lab2_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_lab2_trap

      subroutine color_idnp_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_idnp_trap

      subroutine color_lab_cb_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_lab_cb_trap

      subroutine color_nam_lab_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_nam_lab_trap

      subroutine color_cald_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_cald_trap

      subroutine color_lrrl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_lrrl_trap

      subroutine color_init_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_init_trap

      subroutine color_grade_hor_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_grade_hor_trap

      subroutine color_grade_vrt_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_grade_vrt_trap

      subroutine color_ltab_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_ltab_trap

      subroutine color_opt_tics_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_opt_tics_trap

      subroutine color_tics_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_english
      return
      end subroutine color_tics_trap

      subroutine color_opt_vl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_opt_vl_trap

      subroutine color_opt_scale_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%opt_scale.eq.'USER')then
        call pc_put_sensitive_field_flag('AMPL_USER',.true.)
      else
        call pc_put_sensitive_field_flag('AMPL_USER',.false.)
      endif
      return
      end subroutine color_opt_scale_trap

      subroutine color_opt_tbar1_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_opt_tbar1_trap

      subroutine color_opt_tbar2_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_opt_tbar2_trap

      subroutine color_dist_ovl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_dist_ovl_trap

      subroutine color_invert_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_invert_trap

      subroutine color_ar_beg_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_arrows_sensitive
      return
      end subroutine color_ar_beg_trap

      subroutine color_ar_end_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call color_arrows_sensitive
      return
      end subroutine color_ar_end_trap

      subroutine color_arrows_sensitive
!           ARROWS
      call pc_put_sensitive_field_flag('AR_HT',.true.)
      call pc_put_sensitive_field_flag('AR_LEN',.true.)
      call pc_put_sensitive_field_flag('ARDA',.true.)
      if(object%ar_beg.eq.' '.and.object%ar_end.eq.' ')then
        call pc_put_sensitive_field_flag('AR_HT',.false.)
        call pc_put_sensitive_field_flag('AR_LEN',.false.)
        call pc_put_sensitive_field_flag('ARDA',.false.)
      endif
      return
      end subroutine color_arrows_sensitive

      subroutine color_wt_ovl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_wt_ovl_trap

      subroutine color_va_ovl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_va_ovl_trap

      subroutine color_dev_loc_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%dev_loc.eq.'HP5000A')then
         call pc_put_sensitive_field_flag ('OPT_DEV',.true.)
         call pc_put_sensitive_field_flag ('QUALITY',.true.)
      else
         call pc_put_sensitive_field_flag ('OPT_DEV',.false.)
         call pc_put_sensitive_field_flag ('QUALITY',.false.)
         object%opt_dev='PAPER'
         object%quality='PROD'
      endif
      return
      end subroutine color_dev_loc_trap

      subroutine color_dev_rem_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_dev_rem_trap

      subroutine color_nrnp_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_nrnp_trap

      subroutine color_ratio_tpi_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%ratio_tpi.le.0)then
        call pc_error('RATIO_TPI must be greater than zero')
      endif
      return
      end subroutine color_ratio_tpi_trap

      subroutine color_bars_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_bars_trap

      subroutine color_lab_init_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      integer :: nstrt,istat
      call string_cc2ii(object%lab_init,nstrt,istat)
      if(istat.eq.1)then
        if(nstrt.le.0 .or.nstrt .gt. object%num_tr)then
          if(object%num_tr.le.0)then
            call pc_error('You must answer NUM_TR')
          else
            call pc_error('LAB_INIT is out of range')
          endif
        endif
      else
        call string_to_upper(object%lab_init)
        if (object%lab_init /='SHOT' .and. object%lab_init/='HDR') then
          call pc_error ('LAB_INIT must be a number or SHOT or HDR')
        endif
      endif
      return
      end subroutine color_lab_init_trap

      subroutine color_opt_calc_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_opt_calc_trap

      subroutine color_rp_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_rp_trap

      subroutine color_rp_ovl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_rp_ovl_trap

      subroutine color_scale_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      real :: fval,val
      integer :: istat
       if(object%scale(1:1).eq.'y')object%scale='YES'
       if(object%scale(1:1).eq.'n')object%scale='NO'
       if(object%scale.eq.'NO'.or.object%scale.eq.'YES')return
       call string_cc2ff(object%scale,fval,istat)
       if(istat.eq.1)then
         if(fval.le.0.0)then
           call pc_error('SCALE value must be greater than zero')
         endif
       else
         if(object%scale.ne.'YES'.and.object%scale.ne.'NO')then
           call pc_error('SCALE must be YES, NO, or a number')
         endif
       endif
      call string_cc2ff(object%scale,val,istat) 
      if(istat.ne.1)go to 100
      if (val <= 0.0) then 
        call pc_error('SCALE value must be greater than zero') 
      endif 
      return  
  100 continue 
      if (object%scale(1:1)=='Y' .or. object%scale(1:1)=='y') then 
        object%scale(1:3) = 'YES' 
        object%scale(4:8) = ' ' 
        call pc_put_sensitive_field_flag('CS_SCALE',.true.) 
        if (object%cald == 'NO')call pc_put_sensitive_field_flag('SCDA',.true.) 
      else 
        object%scale(1:2) = 'NO' 
        object%scale(3:8) = ' ' 
        call pc_put_sensitive_field_flag('SCCS',.false.) 
      endif 
      return
      end subroutine color_scale_trap

      subroutine color_fold_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_fold_trap

      subroutine color_opt_semb_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_opt_semb_trap

      subroutine color_trce_arrayset_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      integer :: i
      real    :: ftmp
      character(len=80) :: card
!          check range of shot point cards
      do i=1,object%nshots
        if (object%trce(i)<=0 .or. object%trce(i)>object%num_tr) then
          call pc_error('TRCE number is out of range')
        endif
!          calculate the number of traces this card covers
        ftmp=object%intv(i)*(object%totl(i)-1)+object%trce(i)
        if(ftmp.gt.object%num_tr)then
          call pc_error('Range of labels exceeds total number of traces')
          ftmp=(object%num_tr-object%trce(i))/object%intv(i) 
          write(card,*)' Maximum totl = ',nint(ftmp)
          call pc_error(card)
        endif
        if (object%intv(i)<=0 .or. object%intv(i)>object%num_tr) then
          call pc_error ('The number of intervals, parameter INTV is out of &
                         &range')
        endif
        if (object%totl(i) <= 0) then
          call pc_error('TOTL must be greater than zero')
        endif
      enddo
      call color_shot_sensitive
      return
      end subroutine color_trce_arrayset_trap

      subroutine color_totl_element_trap (keyword,indx,action)
      implicit none
      character(len=*),intent(in) :: keyword
      integer :: indx,action
      return
      end subroutine color_totl_element_trap

      subroutine color_tim_init_arrayset_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_tim_init_arrayset_trap

      subroutine color_pcol_arrayset_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      integer :: i,j,k,m,nkol,nrlc
      if(object%ntpp.le.0)return
      j = object%ntpp*object%nrow 
      k = mod(object%num_tr,j) 
      nkol = object%num_tr/j + 0.0001 
      DO i=1,object%nplbl
        nrlc = 0 
        if (k > 0) then 
          m = object%num_tr - nkol*j 
          nrlc = m/object%ntpp 
          nkol = nkol + 1 
        endif 
        if (object%pcol(i) > nkol) then 
          call pc_error('PCOL number is out of range') 
        endif 
        j = object%ntpp*object%nrow 
        k = mod(object%num_tr,j) 
        nkol = object%num_tr/j + 0.0001 
        nrlc = 0 
        if (k > 0) then 
          m = object%num_tr - nkol*j 
          nrlc = m/object%ntpp 
          nkol = nkol + 1 
        endif 
        if(object%prow(i).gt.object%nrow)then
          call pc_error('PROW is out of range')
        endif
      ENDDO
      return
      end subroutine color_pcol_arrayset_trap

      subroutine color_sltim_arrayset_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine color_sltim_arrayset_trap

      subroutine color_end_trap                     
      implicit none

      real :: a,dist,f,fmaxin,fitmax,ftio,ftmp,hite,r,spi,spin,tdt    ,whole 

      integer :: i,ispprob,j,k,kntr,maxpts
      integer :: n,nc,ncl,ncol,nl,np,nsamp,nsampo,nsampg
      integer :: nsbtwn      ,ntppo,ntrpv 
      integer :: temp,timbtwn

      logical :: almost_equal

      character(len=80) :: cline

!          check plot height
      fmaxin=41.0
      if(object%dev_loc.eq.'HP')fmaxin=35.0
      hite=object%time*object%ips_eng
      fitmax=fmaxin-hite
      if(hite.gt.fmaxin)then
        write(cline,*)'Total plot height exceeds ',fmaxin,' inches -'
        call pc_error(cline)
        call pc_error('Change TIME or IPS ')
      endif
!          figure plot height if paneling
      if(object%ntpp.gt.0)then
!          calculate time between panels
        nsbtwn=.1/object%dt+.0001
        timbtwn=real(nsbtwn)*object%dt
!          put enough samples between panels so that timing line number
!            will fit - also hdr_lab labels
        temp=timbtwn*object%ips_eng
        if(temp.lt.object%cs_tl)then
          timbtwn=object%cs_tl/object%ips_eng
          nsbtwn=timbtwn/object%dt+.0001
          timbtwn=real(nsbtwn)*object%dt
        endif
        hite=(object%time+timbtwn)*object%nrow*object%ips_eng
        fitmax=fmaxin-hite
        if(hite.gt.fmaxin)then
          write(cline,*)'Total plot height exceeds ',fmaxin,' inches - '
          call pc_error(cline)
          call pc_error('Change TIME, IPS, or NROW')
          if(object%ntpp.gt.0)then
            write(cline,*)' Time between panels = ',timbtwn
            call pc_error(cline)
          endif
        endif
      endif
!          calculate distance above section parameters
      ispprob=0
      object%fit=0.0
      if(object%cald.eq.'YES'.or.object%cald.eq.'YREV')then
         call pc_put_sensitive_field_flag ('SDAS',.false.)
         call pc_put_sensitive_field_flag ('ARDA',.false.)
         call pc_put_sensitive_field_flag ('SCDA',.false.)
         object%scda_eng=0.0
         object%sdas_eng=0.0
         object%arda_eng=0.0
         object%tida_eng=.55
!               shot points
         if(object%nshots.gt.0.or.object%hdr_lab(1).gt.0.or.&
          object%hdr_lab(2).gt.0)then
           f=object%cs_lab_eng(1)*2.0+object%cs_lab_eng(1)
           object%fit=f
         endif
!               wrdb
         if(object%hdr_lab(2).gt.0)then
           f=object%fit+.1+object%cs_lab_eng(2)
           object%fit=f
         endif
!               tie lines
         if(object%path_tie1.ne.PATHCHECK_EMPTY)then
           if(object%hdr_lab(2).ne.0.0)object%tida_eng=.835
           nc=40
           a=nc*object%cs_lt_eng*.342021+object%tida_eng+object%cs_lt_eng*1.5
           if(a.gt.object%fit)then
             object%fit=a
           endif
           if(object%fit.gt.fitmax)object%fit=fitmax
           object%tida=object%tida_eng
         endif
!               tie lines - second row
         if(object%path_tie2.ne.PATHCHECK_EMPTY)then
           nc=40
           a=nc*object%cs_lt_eng*.342021+object%tida_eng+object%cs_lt_eng*2.5
           if(a.gt.object%fit)then
             object%fit=a
           endif
           if(object%fit.gt.fitmax)object%fit=fitmax
         endif
!               arrows
         if(object%ar_beg.ne.' '.or.object%ar_end.ne.' ')then
           object%arda_eng=object%fit+.2
           if(object%arda_eng.lt.0.8)object%arda_eng=0.8
!              add on height of label
           a=object%ar_ht_eng
           dist=object%arda_eng+.1+a+a/2.5
           object%fit=dist
           if(object%fit.gt.fitmax)then
             object%arda_eng=fitmax-(.1+a+a/2.5)
             object%fit=fitmax
             ispprob=1
           endif
           object%arda=object%arda_eng
         endif
!               scale
         if(object%scale.ne.'NO')then
           object%scda_eng=object%fit+0.5
           object%fit=object%scda_eng+object%cs_scale_eng
           if(object%fit.gt.fitmax)then
             object%scda_eng=fitmax-2.0*object%cs_scale_eng
             object%fit=fitmax
             ispprob=1
           endif
           object%scda=object%scda_eng
         endif
!               velocity functions
!     if(velfile(1:10).ne.' ')then
!       if(object%scda_eng.le.0.0)object%scda_eng=object%fit+0.5
!       object%fit=object%scda_eng + object%cs_scale_eng + 2.9
!       if(object%fit.gt.fitmax)then
!         object%scda_eng=fitmax-2*object%cs_scale_eng-3.0
!         object%fit=fitmax
!         ispprob=1
!       endif
!       scda=object%scda_eng
!     endif
!               sid
         if(object%sid(1).ne.' '.or.object%sid(2).ne.' '.or.&
           object%sid(3).ne.' '.or.object%sid(4).ne.' ')then
           nl=5
           if(object%sid(1).eq.' ')nl=4
           if(object%sid(2).eq.' ')nl=3
           if(object%sid(3).eq.' ')nl=2
           f=object%fit+nl*object%cs_sid_eng+4*.15
           object%fit=f
           if(object%fit.lt.5.0.and.fitmax.ge.5.0)object%fit=5.0
           if(object%fit.gt.fitmax)then
             object%fit=fitmax
             ispprob=1
           endif
           object%sdas_eng=object%fit-object%cs_sid_eng
           object%sdas=object%sdas_eng
         endif
         if(survey_units.eq.'METERS')then
           object%sdas=object%sdas_eng*2.54
           object%scda=object%scda_eng*2.54
           object%tida=object%tida_eng*2.54
           object%arda=object%arda_eng*2.54
         endif
!
!          must take away from it if ltab is being done
         if(object%ltab.eq.'YES')then
           object%fit=object%fit-0.5-object%cs_lab_eng(1)*1.5
         endif
      endif
!
!              give a space problem warning message
      if(ispprob.eq.1)then
        call pc_warning('Annotation has been squeezed into the limited &
                        &space available. you may wish to')
        call pc_warning('review your selections. Check the calculated SDAS &
                        &and ')
        call pc_warning('IT in your work file. IT is the total amount of &
                         &space available for all ')
        call pc_warning('annotation - in inches.')
      endif
      if(object%cald.eq.'NO')then
         call pc_put_sensitive_field_flag ('SDAS',.true.)
         call pc_put_sensitive_field_flag ('ARDA',.true.)
         if(fitmax.ge.5.0)object%fit=5.0
         if(object%fit.lt.object%sdas_eng)object%fit=object%sdas_eng
!            tie lines
         if(object%path_tie1.ne.PATHCHECK_EMPTY)then
           a=nc*object%cs_lt_eng*.342021+object%tida_eng+object%cs_lt_eng*1.5
           if(a.gt.object%fit)object%fit=a
         endif
!            tie lines - second row
         if(object%path_tie2.ne.PATHCHECK_EMPTY)then
           a=nc*object%cs_lt_eng*.342021+object%tida_eng+object%cs_lt_eng*2.5
           if(a.gt.object%fit)object%fit=a
         endif
!            scle
        if(object%scale.eq.'YES')then
          dist=object%scda_eng+object%cs_scale_eng
          if(dist.gt.object%fit)object%fit=dist
        endif
!            arrows
         if(object%ar_beg.ne.' '.or.object%ar_end.ne.' ')then
           a=object%ips_eng*.035
           dist=object%arda_eng+.1+a+a/2.5
           if(dist.gt.object%fit)object%fit=dist
         endif
!            velocity functs
!        if(velfile.ne.' ')then
!          dist=object%scda_eng+object%cs_scale_eng+2.9
!          if(dist.gt.object%fit)object%fit=dist
!        endif
         if(object%fit.lt.5.0.and.fitmax.ge.5.0)object%fit=5.0
         if(object%fit.gt.fitmax)object%fit=fitmax
         if((object%sid(1).ne.' '.or.object%sid(2).ne.' '.or.&
             object%sid(3).ne.' '.or.object%sid(4).ne.' ')  &
             .and.object%sdas_eng.eq.0.0.and.fitmax.gt.0.0)then
           call pc_error('You need to set the SDAS parameter')
         endif
!          compare fit to distance parameters
        f=object%fit
        if(survey_units.eq.'METERS')f=object%fit*2.54
        if(object%sdas_eng.gt.object%fit)then
          call pc_error('The SDAS parameter exceeds space available for&
                        & annotation')
          write(cline,*)' SDAS = ',object%sdas_eng,' Space available = ',f
          call pc_error(cline)
          call pc_jump_field('SDAS')
        endif
        if(object%arda_eng.gt.object%fit)then
          call pc_error('The ARDA parameter exceeds space available for&
                        & annotation')
          write(cline,*)' ARDA = ',object%arda_eng,' Space available = ',f
          call pc_error(cline)
          call pc_jump_field('ARDA')
        endif
        if(object%tida_eng.gt.object%fit)then
          call pc_error('The TIDA parameter exceeds space available for&
                        & annotation')
          write(cline,*)' TIDA = ',object%tida_eng,' Space available = ',f
          call pc_error(cline)
          call pc_jump_field('TIDA')
        endif
      endif

!                  Paneling
!        if the number of overlay traces are divisible by the number
!         of panels, and ti/tio is a whole number...next check can be
!         skipped
!
        if(object%ratio_tpi.le.1)then
          ftio=object%tpi_eng
        else
          ftio=object%tpi_eng/object%ratio_tpi
        endif
        j=1
        if(object%ntpp.gt.0.and.object%path_ovl.ne.PATHCHECK_EMPTY)then
          k=mod(object%num_tr,object%ntpp)
          r=object%tpi_eng/ftio
          whole=anint(r)
          almost_equal=ameq(r,whole,0.00001)
          if(.not.almost_equal)k=1
          if(k.eq.0)then
            np=object%num_tr/object%ntpp
            j=mod(object%ntr_ovl,np)
            if(j.eq.0)go to 40
          endif
        endif
!         if paneling - insure that the number of traces in an overlay
!          panel are a multiple of the number of traces in a color panel
      if(object%nrow.gt.1)then
        ncol=object%num_tr/(object%ntpp*object%nrow)
        n=ncol*object%ntpp
      endif
      if(object%ntpp.gt.0.and.object%path_ovl.ne.PATHCHECK_EMPTY.and.&
         ncol.gt.1)then
        r=ftio/object%tpi_eng
        ntppo=object%ntpp*r
        k=mod(object%ntpp,ntppo)
        if(k.ne.0)then
          call pc_error('Number of traces in an overlay panel must be&
                        & a multiple of ntpp')
          call pc_error('Change NTPP,TPI, or TPI_OVL')
          call pc_jump_field('TPI_OVL')
          return
        endif
      endif
 40   continue
!         insure shot points do not exceed number of traces
      if(object%ntpp.gt.0)then
        j=object%ntpp*object%nrow
        k=mod(object%num_tr,j)
        ncl=object%num_tr/j+.0001
        if(k.gt.0)then
          ncl=ncl+1
        endif
        kntr=ncl*object%ntpp
      endif
      do i=1,object%nshots
        ftmp=object%intv(i)*(object%totl(i)-1)+object%trce(i)
        if(ftmp.gt.object%num_tr)then
          call pc_error('Range of shot point labels exceeds total numbe&
                         &r of traces')
          call pc_jump_field('NUM_TR')
          return
        endif
        if(object%ntpp.gt.0.and.ftmp.gt.kntr)then
          call pc_error('Range of labels exceeds total number of traces')
          write(cline,*)'Number of paneled traces = ',kntr
          call pc_error(cline)
          call pc_jump_field('NUM_TR')
          return
        endif
      end do
      if(object%invert.eq.'YES'.and.object%ntistr.gt.0)then
        call pc_error('INVERT not allowed with manual labels')
        object%invert='NO'
        call pc_error('INVERT has be reset to NO')
      endif

!            Check vertical limits
      nsamp=object%time/object%dt+.001
      ntrpv=1
      spi=nsamp/(object%time*object%ips_eng)
      if(object%grade_vrt.eq.'YES')then
        spin=nsamp/(object%time*object%ips_eng)
 65     if(spi.ge.150.0)go to 70
        ntrpv=ntrpv+1
        spi=spin*ntrpv
        go to 65
      endif
 70   continue
      nsampo=nsamp
      nsampg=nsamp*ntrpv
      if(nsampg.gt.8192)then
        tdt=object%dt/ntrpv
        f=8192*tdt
        call pc_error('You have exceeded the maximum of 8192 points i&
                      &n the vertical direction')
        write(cline,*)'Reduce TIME to ',f,' or set GRDV=NO'
        call pc_error(cline)
        if(object%ntpp.gt.0)then
          write(cline,*)' Number of samples between panels = ',nsbtwn
          call pc_error(cline)
        endif
        call pc_jump_field('TIME')
        return
      endif
!          Check vertical limits if overlay
!!      f = (object%time + timbtwn)*object%ips_eng 
!!      if (object%nrow <= 1) object%nrow = 34.5/f 
!!      if(object%nrow.lt.1)object%nrow=1
!          check for max num points - 4096 if doing overlay, otherwise
!                                     8192
      maxpts = 8192 
      if (object%path_ovl /= PATHCHECK_EMPTY) maxpts = 4096 
      n = object%time/object%dt 
      if (object%ntpp > 0)then
         n = (object%time + timbtwn)*object%nrow/object%dt - timbtwn/object%dt 
      endif
      if (n > maxpts) then 
        call pc_error('WARNING You may plot only 8192 points, 4096 if doing &
                      &overlay') 
        return  
      endif 

      return
      end subroutine color_end_trap
!<execute_only>

      subroutine color (obj,ntr,header,trace)
      implicit none
      type(color_struct),intent(inout) :: obj                   ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: header(:,:)            ! arguments
      real             ,intent(inout) :: trace(:,:)             ! arguments

      double precision :: hdr(obj%nwih*obj%nrow),hdra(obj%nwih*obj%nrow)
      double precision :: hdrb(obj%nwih*obj%nrow),rdheader(obj%nwih)
      real :: trcx(obj%nsamp),rdtrace(obj%nsamp)
      real :: a,addon,alpha,b,basint,blhd1,blhd2,blline,dlhite
      real :: f,facth,fblkhd,ftiehd,ftiehd2,fmed,ftra,ftr2
      real :: g,gtrce,gtrce1,gtrce2,gtwid,ht2,reduce
      real :: oblhd2,oftr2,panwid,qtwid,r,shite,shiteo,sladd
      real :: tht,tie1hd,tie2hd,tinch,tiold,tmpx,tmpy,tnnw,tnnw1,tnnw2,tra
      real :: trmax,tti,twid,twidp
      real :: whole,x,xend,xo,xtrce,y,yend,yepan,yinc,yo,ytemp,yy

      real,pointer :: panl(:),axisval(:),tloc(:),vloc(:),trca(:)

      integer,allocatable :: iafter(:),ibefor(:),ipdf(:),jafter(:)

      integer,pointer :: iaskp(:),iado(:),iabtwn(:),iaintv(:),kodtone(:)

      integer :: kodes(obj%nsamp)
      integer :: i3bl,iadd,iatb,iatbf,ibf,iblkhdl,iblkh1=1,ibl3d
      integer ::      idum,ifhd1,iiii,intvg,istat,itf,itf2 
      integer :: itieh1=1,itieh2=1,itrlab
      integer :: kncr,kntcol,knthead,knttone,knttrin=0,ktotl,lsttrce
      integer :: lun_cgmcc
      integer :: match,method
      integer :: nafter,nc,ncinc,ndxaft,nedtie1,nedtie2
      integer :: needblk,np,npan2lab,nspan
      integer :: ntg,ntplot,ntppflg,ntppg,ntrph        ,ntppo,numbt,numpan,nval 
      integer :: trciostat
      integer :: i,i1,j,j1,k,k1,k2,kk,ko,l,l2,m,n


      character(len=1)  :: letter
      character(len=8)  :: ctemp,ctmp,ctmp2,ctmp3,blidfmt,blinc
      character(len=8)  :: cunknown='unknown',icgmcc='%CCGMCA'
      character(len=8)  :: i3blf='%I3DBLA',kshot,tf1inc,tf2inc,uname
      character(len=10) :: project
      character(len=16) :: ctmpl,ctmpl2
      character(len=40) :: tie1txt,tie2txt
      character(len=80) ::        jobcrd,prjcrd,bltxt,card 

      logical :: almost_equal

      save :: ntplot

      data jobcrd/'JOB = 123456789012345 USER = 1234567890  ROUTE = '/
      data prjcrd/'PROJECT = 1234567890 '/

!
      nullify (panl) ! jpa
      nullify (axisval) ! jpa
      nullify (tloc) ! jpa
      nullify (vloc) ! jpa
      nullify (trca) ! jpa
      nullify (iaskp) ! jpa
      nullify (iado) ! jpa
      nullify (iabtwn) ! jpa
      nullify (iaintv) ! jpa
      nullify (kodtone) ! jpa
!
!-------------------------NO MORE TRACES HERE------------------------
!
      if (ntr == NO_MORE_TRACES)then
      obj%ktrnum = obj%ktrnum - 1
      write(print_lun,*) ' ***  COLOR -- ', obj%ktrnum, ' TRACES HAVE BEEN &
                          &OUTPUT TO DISK'
      write(print_lun,*) ' LARGEST ABSOLUTE VALUE = ', obj%glav
      if (obj%glav.eq.0.0) then
         write(print_lun,*) ' ALL TRACES ARE DEAD '
         ntr=FATAL_ERROR
         return
      endif
!
!07/12/95 start
      if (obj%ntppset.le.0) then
!         apparently only one semblance panel - must set parameters
!          now
!          calculate tpi_eng, twid --- assume 1 unit = 1 trace
         obj%axis2 = obj%hdr6lst
         obj%tpi_eng = obj%scale_vel
         obj%ntppsem = (obj%axis2 - obj%axis1) + .0001
         twid = 1 / obj%tpi_eng
         gtwid = twid
!          set the shot point array...
!            make labels even increments of 1000
         idum = obj%axis1 + .0001
         j = mod (idum, 1000)
         if (j.eq.0) then
            obj%trce (1) = 1.0
            call string_ff2cc(obj%axis1,ctmp)
            nc=len_trim(ctmp)
            obj%shot(1)(1:nc)=ctmp(1:nc)
            obj%totl (1) = (obj%axis2 - obj%axis1) / obj%scale_vel + 1
         else
            iadd = 1000 - j
            idum = idum + iadd
            obj%trce (1) = real (idum - obj%axis1)
            call string_ii2cc(idum,ctmp)
            nc=len_trim(ctmp)
            obj%shot(1)(1:nc)=ctmp(1:nc)
            obj%totl(1) = (obj%axis2 - idum) / obj%scale_vel + 1
         endif
         f = obj%scale_vel
         obj%intv(1) = 1000
         obj%incr(1) = 1000
         obj%totl(1) = (obj%axis2 - idum) / obj%intv (1) + 1
         obj%opt_vl = 'YES'
         write(print_lun,*) ' COLOR => SHOT POINT ARRAY SET AS TRACE = ', &
                              obj%trce (1), &
                              ' SHOT = ', ctmp, ' INTV = ', obj%intv (1),&
                              ' INCR = ', obj%incr (1),&
                              ' TOTL = ', obj%totl (1)
         obj%lab_inc = obj%ntpp
         k = obj%ntpp / 2
         call string_ii2cc(k,ctmp)
         nc=len_trim(ctmp)
         obj%lab_init(1:nc)=ctmp(1:nc)
         obj%ntppset = 1
         obj%ncol = 1
         obj%irspp = 'YES'
         if (obj%ntpp.gt.0) then
            j = obj%ntpp * obj%nrow
            k = mod (obj%num_tr, j)
            obj%ncol = obj%num_tr / j + .0001
            obj%nrlc = 1
            if (k.gt.0) then
               m = obj%num_tr - (obj%ncol * j)
               obj%nrlc = m / obj%ntpp
               obj%ncol = obj%ncol + 1
            endif
         endif
!          if paneling and user has not set blank traces - set it here
!        if blank trace pattern not set - set it based on obj%ntpp
         if (obj%nbts.eq.0) then
            obj%btskp(1) = obj%ntppsem
            obj%kbtskp = obj%ntpp
            panwid = obj%ntppsem / obj%tpi_eng
            tiold = obj%ntpp / panwid
            obj%twidk = 1 / tiold
!             make btdo large enough to plot timing line number
            b = obj%cs_tl_eng * 3.0
            if (obj%ups.gt.0) then
               f = log10 (real (obj%ups) ) + 1.5
               b = obj%cs_tl_eng * f
            endif
            k = b * obj%tpi_eng + 2.500001
            ko = b * tiold+2.50001
            obj%btdo (1) = k
            obj%btbtwn (1) = obj%ntppsem
            obj%kbtbtwn = obj%ntpp
            obj%bttot (1) = obj%ncol - 1
            obj%kbttot = obj%ncol - 1
            obj%nbts = 1
            if (obj%ncol.gt.1) then
               obj%irtbp = 'YES'
            else
               obj%irtbp = 'NO'
            endif
            write(print_lun,*) ' COLOR-BLANK TRACE PATTERN SET AS BTSKP = ',&
                                 obj%btskp(1), ' BTDO = ', obj%btdo(1),&
                                 ' BTBTWN = ', obj%btbtwn(1),&
                                 ' BTTOT = ', obj%bttot (1)
            obj%iuserbt = 0
         elseif (obj%nbts.gt.0) then
      write(print_lun,*) ' COLOR=> USER HAS SET THEIR OWN BLANK TRACE PATTERN'
            obj%iuserbt = 1
                 !  (obj%nbts.eq.0)
         endif
      endif
!07/12/95  end
      ntppflg = obj%ntpp
!
!          set up pens for black and white lines
!
!cc   call colrplot_nclr(1,1)   ! set pen 1 to black
!cc   call colrplot_nclr(9,9)   ! set pen 9 to white
!
!          start with black
!cc   call colrplot_npen(1)
!
      if (obj%wid_vl_eng.le.0.0) obj%wid_vl_eng = .005
      if (obj%cs_spl_eng.le.0.0) obj%cs_spl_eng = 0.13
!
      if (obj%opt_semb.eq.'NORMAL') twid = 1.0 / obj%tpi_eng
      ntppg = obj%ntpp
!        calculate horizontal grade factor
      if (obj%tpi_eng.ge.100) obj%grade_hor = 'NO'
      ntrph = 1
      if (obj%grade_hor.eq.'NO') then
         twidp = twid
         goto 578
      endif
      tti = obj%tpi_eng
  575 if (tti.ge.100) goto 576
      ntrph = ntrph + 1
      tti = obj%tpi_eng * ntrph
      goto 575
  576 continue
      facth = 1.0 / real (ntrph)
      twidp = 1.0 / tti
!
  578 continue
      knttrin = 0
!
      if (obj%ntpp.gt.0) then
         if (obj%grade_hor.eq.'YES') then
            gtwid = twidp
            ntppg = obj%ntpp * ntrph - (ntrph - 1)
         endif
         if (obj%ncol.eq.1.and.obj%ktpanl.lt.obj%ntpp) then
            obj%ktpanl = obj%ntpp
            ntplot = obj%ktpanl
               !if(obj%ncol.eq.1.and.obj%ktpanl.lt.obj%ntpp)
         endif
!          now that we know the true number of traces - recalculate
!           nrlc (number of rows in last column) and blank trace
!           pattern unless user defined
         j = obj%ntpp * obj%nrow
         k = mod (obj%ktrnum, j)
         obj%ncol = obj%ktrnum / j + .0001
         obj%nrlc = obj%nrow
         if (k.gt.0) then
            m = obj%ktrnum - (obj%ncol * j)
            obj%nrlc = m / obj%ntpp
            obj%ncol = obj%ncol + 1
               !if(k.gt.0)
         endif
         npan2lab = obj%ktrnum / obj%ntpp
         write(print_lun,*) ' NUMBER OF INPUT TRACES PLOTTED = ', obj%ktrnum
         write(print_lun,*) ' NUMBER OF PANELED TRACES = ', obj%ncol * obj%ntpp
         write(print_lun,*) ' THERE ARE ', obj%ncol, ' COLUMNS OF PANELS'
         if (obj%nrlc.gt.0) then
            write(print_lun,*) ' LAST COLUMN HAS ', obj%nrlc, ' ROWS '
         endif
         if (obj%bttot (1) .gt. (obj%ncol - 1) .and.obj%iuserbt.eq.0) then
            obj%bttot (1) = obj%ncol - 1
            write(print_lun,*) ' BTTOT RESET TO ', obj%bttot(1),&
                               ' BASED ON ',obj%ncol, ' COLUMNS'
               !(obj%bttot(1).gt.(obj%ncol-1).and.obj%iuserbt.eq.0)
         endif
             !if(obj%ntpp.gt.0)
      endif
!
!          if paneling - make the paneled strot file from the dtrot
!            file. add .1 sec or more between each panel
      if (obj%opt_calc.eq.'ALL'.and. (obj%opt_scale.eq.'CALC'.or.obj%nbox.gt.0)&
           ) then
         if(associated(panl))deallocate(panl)
         allocate(panl(obj%ndpt),stat=istat)
         if(istat.ne.0)then
           call pc_error('COLOR-->Unable to allocate array panl')
           ntr=FATAL_ERROR
           return
         endif
      else
         if(associated(panl))deallocate(panl)
         allocate(panl(obj%nvi),stat=istat)
         if(istat.ne.0)then
           call pc_error('COLOR-->Unable to allocate array panl')
           ntr=FATAL_ERROR
           return
         endif
      endif
      nval = obj%nvi
      if (obj%ntpp.gt.0) then
         obj%ktrnum = 0
         nspan = obj%nvi * obj%nrow + obj%nsbtwn * (obj%nrow - 1)
         nval = nspan
         if (obj%ndpt.gt.nspan) then
           if(associated(panl))deallocate(panl)
           allocate(panl(obj%ndpt),stat=istat)
           if(istat.ne.0)then
             call pc_error('COLOR-->Unable to allocate array panl')
             ntr=FATAL_ERROR
             return
           endif
         else
           if(associated(panl))deallocate(panl)
           allocate(panl(nspan),stat=istat)
           if(istat.ne.0)then
             call pc_error('COLOR-->Unable to allocate array panl')
             ntr=FATAL_ERROR
             return
           endif
         endif
         k1 = 1
         n = (k1 - 1) + obj%nrow * obj%ntpp
         k2 = 1
         i1 = 1
         j1 = 1
         kntcol = 1
         numpan = obj%ncol * obj%ntpp
  504    continue
         panl=0
                              ! one column
         do j = 1, obj%ntpp
            j1 = 1
                              ! one trace
            do i = k1, n, obj%ntpp
               trciostat=trcio_read_trace(obj%idn,rdheader,rdtrace,i)
               if(trciostat.ne.TRCIO_OK)then
                 call pc_error('COLOR-->Error reading from trace file idn')
                 call pc_error('        COLOR abort')
                 ntr=FATAL_ERROR
                 return
               endif
!             keep headers of all rows
               do k=1,obj%nwih
                 hdr(j1)=rdheader(k)
                 j1=j1+1
               enddo
               l2=i1
               do l=1,obj%nvi
                 panl(l2)=rdtrace(l) 
                 l2=l2+1
               enddo
               i1 = i1 + obj%nvi + obj%nsbtwn
                        ! end one paneled trace
            end do
            k1 = k1 + 1
            i1 = 1
            trciostat=trcio_write_trace(obj%isn,hdr,panl)
            if(trciostat.eq.TRCIO_ERROR)then
              call pc_error('COLOR--> Error in trcio')
              ntr=FATAL_ERROR
              return
            endif
            obj%ktrnum = obj%ktrnum + 1
                    ! end one column of panels
         end do
         if (obj%ktrnum.lt.numpan) then
            i1 = 1
            k2 = k2 + obj%ntpp * obj%nrow
            k1 = k2
            n = (k1 - 1) + obj%nrow * obj%ntpp
            kntcol = kntcol + 1
            if (kntcol.eq.obj%ncol) then
               n = (k1 - 1) + obj%nrlc * obj%ntpp
            endif  ! kntcol.eq.obj%ncol
            goto 504
         endif   ! obj%ktrnum.le.numpan
      endif   ! nttp.gt.0
!
      if (obj%opt_scale.eq.'CALC')then
         allocate(ipdf(5001),stat=istat)
         if(istat.ne.0)then
           call pc_error('COLOR-->Unable to allocate array for opt_scale=calc')
           ntr=FATAL_ERROR
           return
         endif
      endif
!           trca needs to hold 2 traces if grade =yes
      n = obj%nsamp
!      if (igrade.eq.'YES') n = n * 2
      call mem_alloc(trca,n)
!!!      call getscr (ippack, obj%nwrdsmult_double) ???????????? --array kpack
!
!          for each blank trace card - determine trace number to insert
!           traces after
      nafter = 0
      ntg = 0
      if (obj%ntpp.eq.0.and.obj%nbts.gt.0.and.ntrph.gt.1) then
         ndxaft = 0
         do i = 1, obj%nbts
           nafter = nafter + obj%bttot (i)
         enddo
         ntg = obj%num_tr * ntrph - (nafter + 1) * (ntrph - 1)
         allocate(iafter(nafter),stat=istat)
         if(istat.ne.0)then
           call pc_error('COLOR--> Unable to allocate array iafter')
           ntr=FATAL_ERROR
           return
         endif
         allocate(ibefor(nafter),stat=istat)
         if(istat.ne.0)then
           call pc_error('COLOR--> Unable to allocate array ibefor')
           ntr=FATAL_ERROR
           return
         endif
         ibefor (1) = obj%btskp (1)
         do i = 1, obj%nbts
           ndxaft = ndxaft + 1
           iafter (ndxaft) = obj%btskp (i)
           if (ndxaft.gt.1) then
              ibefor (ndxaft) = iabs (iafter (ndxaft) - iafter (ndxaft -  &
              1) )
           endif
           do j = 1, obj%bttot (i) - 1
             ndxaft = ndxaft + 1
             iafter (ndxaft) = iafter (ndxaft - 1) + obj%btbtwn (i)
             ibefor (ndxaft) = obj%btbtwn (i)
           enddo
         enddo
         ntppflg = 1
         ndxaft = 1
!           sort the iafter array
         if (nafter.gt.1) call color_sbwa (iafter, ibefor, nafter)
!          convert iafter array to graded trace number
         if (nafter.gt.0) then
            allocate(jafter(nafter),stat=istat)
            if(istat.ne.0)then
              call pc_error('COLOR--> Unable to allocate array jafter')
              ntr=FATAL_ERROR
              return
            endif
            jafter (1) = (iafter (1) - 1) * ntrph + 1
            do i = 2, nafter
              jafter (i) = (ibefor (i) - 1) * ntrph + 1 + jafter (i - 1)
            enddo
         endif
!
!          convert blank trace array to graded trace numbers
         if (nafter.gt.0.or.obj%ntpp.gt.0) then
            call mem_alloc(iaskp,obj%nbts)
            call mem_alloc(iado,obj%nbts)
            call mem_alloc(iabtwn,obj%nbts)
            if (obj%nshots.gt.0)then
              call mem_alloc(iaintv,obj%nshots)
            endif
            do i = 1, obj%nbts
              a = real (obj%btskp (i) )
              f = color_2gab (a, iafter, jafter, nafter, ntrph, ntg)
              iaskp (i) = nint (f)
              iado (i) = obj%btdo (i) * ntrph
              iabtwn (i) = obj%btbtwn (i) * ntrph - (ntrph - 1)
            enddo
            do i = 1, obj%nshots
              iaintv (i) = obj%intv (i) * ntrph
            enddo
         endif
      endif  ! obj%ntpp.eq.0.and.obj%nbts.gt.0.and.ntrph.gt.1
!
!            if calculating vmin-vmax from pnc-ppc-pplc parameters,
!             must make separate pass through the data
      if (obj%nbox.eq.0) goto 520
      if (obj%opt_scale.eq.'CALC') then
        write(print_lun,*) 'COLOR ABORT - FOR opt_scale=calc OPTION VALUES MUST&
                           & BE READ IN FROM A FILE'
      endif

!          rewind the strot file     
      istat=trcio_seek_trace(obj%isn,1) 

      if(istat.eq.TRCIO_OK)then
        write(print_lun,*)'COLOR-->isn file rewound'
      else
        write(print_lun,*)'COLOR-->Problem rewinding trcio file'
        write(print_lun,*)'        Status = ',istat 
      endif
!
      obj%hgneg=0.0
      obj%hgpos=0.0
      do i = 1, obj%ktrnum
         if (obj%opt_calc.eq.'PLOT') then
            istat=trcio_read_trace(obj%isn,hdr,panl)
            call color_gram(obj%nbox-1,nval,obj%glav,panl,obj%hgneg,obj%hgpos)
         else
            istat=trcio_read_trace(obj%isn2,hdr,panl)
            call color_gram(obj%nbox-1,obj%ndpt,obj%glav,panl,obj%hgneg,&
                            obj%hgpos)
         endif
      end do
      istat=trcio_seek_trace(obj%isn,1)
!
      call color_hilo(obj%phc,obj%plc,obj%pplc,obj%vmin,obj%vmax,obj%glav,&
                      obj%nbox,obj%hgneg,obj%hgpos)
!
      write(print_lun,*)' CALCULATED vmin-vmax = ', obj%vmin, obj%vmax

!
  520 continue
!          scale to median option
       fmed = 1.0
      if (obj%opt_scale.eq.'CALC') then
         if (obj%opt_calc.eq.'PLOT') then
            call color_med(obj%isn,obj%glav,fmed,ipdf,obj%nwih,nval,istat) 
            if(istat.ne.0)go to 9501
         else
            call color_med(obj%isn2,obj%glav,fmed,ipdf,obj%nwih,obj%ndpt,&
                           istat)
            if(istat.ne.0)go to 9501
         endif
      endif
      if(obj%opt_scale.eq.'USER')fmed=obj%ampl_user
      tinch = obj%time * obj%ips_eng
      obj%vlinc = 10.0
      if (obj%ncolr.gt.2) obj%vlinc = (obj%vmax - obj%vmin) / (obj%ncolr - 2)
      shite = tinch / real (obj%nsamp)
      shiteo = tinch / real (obj%nvi)
!           set x and y origin of section (trace 1, sample 1)
!             find length of longest side label first
      sladd = 0.0
      if (obj%nslbl.gt.0) then
         j = 1
         sladd = 0.0
         do i = 1, obj%nslbl
            nc=len_trim(obj%slbl(j))
            almost_equal = ameq (f, 0.0, .0001)
            if(almost_equal)f = 0.15
            a = real (nc) * obj%slsz (i) * .85 + 1.0
            sladd = amax1 (sladd, a)
            j = j + 3
         end do             
      endif ! if(obj%nslbl.gt.0)
      if (obj%lrrl.eq.'LR') then
         xo = 9.5 + sladd
         if (obj%ncl_at1.ne.0) xo = xo + real (obj%ncl_at1) * 0.5
         if (obj%path_atbm.ne.PATHCHECK_EMPTY)xo = xo + 8.0 * obj%frtb
         yo = obj%time * obj%ips_eng + .1
         yend = 0.1
         alpha = 0.0
         if (obj%ltab.eq.'YES') then
            yo = yo + 0.28 + obj%cs_spl_eng
            yend = yend+0.28 + obj%cs_spl_eng
         endif
      else
         xo = 4.5
         yo = obj%fit
         yend = yo + obj%time * obj%ips_eng
         alpha = 180.0
      endif
      qtwid = twid/4.0
      if(obj%dlu100.gt.0.00001)dlhite=obj%ips_eng/(obj%dlu100*10)
      if (obj%dev_rem.ne.'POSP03'.and.obj%dev_rem.ne.'POPLOT')xo = xo + 0.5
      numbt = 0
      do i = 1, obj%nbts
         k = obj%btdo(i) * obj%bttot(i)
         numbt = numbt + k
      end do
      k = obj%ktrnum
      if (obj%opt_semb.eq.'SEMB') k = obj%ntppsem * obj%ncol
      if (obj%ncl_at1.gt.0) k = k / 2
      xend = xo + (k + numbt) / obj%tpi_eng
      if (obj%grade_hor.eq.'YES'.and.nafter.eq.0) xend = xend-twid
      if (obj%ntpp.gt.0.and.obj%grade_hor.eq.'YES') then
         k = ntppg * obj%ncol
         k = k + numbt * ntrph
         xend = xo + k * gtwid
      endif
      if (nafter.gt.0.and.ntrph.gt.1) then
         xend = xend+twidp * nafter - twid * nafter
      endif
!
      write(print_lun,*) ' COLOR => XO = ', xo, ' XEND = ', xend
      write(print_lun,*) '          YO = ', yo, ' YEND = ', yend
!
      ncinc = 1
      if (obj%ncolr.ge.16) ncinc = obj%ncolr / 8
!          fill up vals array from vmin and vmax parameters
      if (obj%vmin.eq.0.0.and.obj%vmax.eq.0.0) goto 552
      if (obj%irepeat.gt.1) then
         if (obj%ncolro.gt.2) then
            g = (obj%vmax - obj%vmin) / (obj%ncolro - 2)
         else
            g = obj%vmax - obj%vmin
         endif
         f = g / real (obj%irepeat)
         obj%vals (1) = obj%vmin - g + f
      else
         if (obj%ncolr.gt.2) then
            f = (obj%vmax - obj%vmin) / (obj%ncolr - 2)
            obj%vals (1) = obj%vmin
         else
            obj%vals (1) = obj%vmin
            obj%vals (2) = obj%vmax
            goto 551
         endif
      endif
      do i = 2, obj%ncolr
         obj%vals (i) = obj%vals (i - 1) + f
      end do
  551 obj%vlinc = obj%vals (2) - obj%vals (1)
!
!
!           initialize the color plot with call to conplot
  552 continue
!          
!!      ctmp=account(1:8)   
      ctmpl2=' '
      ctmpl(1:7)=routing_name(1:7)
      ctmpl(9:15)=address(1:7)
      if(routing_name(1:7).eq.' ')ctmpl(1:7)=user_name(1:7) 
      if(obj%nrnp.ne.' ')ctmpl(1:7)=obj%nrnp(1:7)
      if(obj%idnp.ne.' ')ctmpl(9:15)=obj%idnp(1:7) 
      ctmp2=jobname(1:8) 
      ctmp3=' '   ! job identifier   
      ctemp=obj%dev_loc
      if(obj%opt_dev.eq.'PREM')then
        ctemp=trim(obj%dev_loc) // 'Q'
      endif
      call colrplot_setx (obj%iunit,ctemp,ctmpl2(1:12),ctmpl,obj%copies,ctmp2,&
                          ctmp3,obj%path_vect,cunknown,0,obj%fold)
      call colrplot_tflg(obj%iunit,-1)  
!
!          set to overlay mode
      call colrplot_olay (obj%iunit,1)
!
!          set up a color table if rgb file
      if (obj%rgbflg.gt.0) then
         write(print_lun,*) ' COLOR TABLE '
         l = obj%kolors (1)
         m = 100
!          set up white as color 99 if paneling
         if (obj%ntpp.gt.0) call colrplot_rgb (obj%iunit,99, 1.0, 1.0, 1.0)
         call colrplot_rgb (obj%iunit,m,red(l),green(l),blue(l))
         write(print_lun,'(" ",I3,3(1X,F6.4))')m,red(l),green(l),blue(l)
         do i = 1, obj%ncolr
            l = obj%kolors (i)
            m = m + 1
            call colrplot_rgb (obj%iunit,m, red (l), green (l), blue (l) )
            write(print_lun,'(" ",I3,1X,I3,3(1X,F6.4))')l,m,red(l),green(l),&
                                                        blue(l)
         end do
         l = obj%kolors (obj%ncolr)
         m = m + 1
         call colrplot_rgb (obj%iunit,m, red (l), green (l), blue (l) )
         write(print_lun,'(" ",I3,3(1X,F6.4))')m,red(l),green(l),blue (l)
         m = 101
         do i = 1, obj%ncolr
            obj%kolors (i) = m
            m = m + 1
         end do
      endif
!
!          plot the job and project card
!
      call colrplot_fnt (obj%iunit,19)
      if (obj%fold.eq.'YES') then
         call colrplot_sym (obj%iunit,0.5, 1.0, 0.25, 'FOLD', 90.0, 4)
      else
         call colrplot_sym (obj%iunit,0.5, 1.0, 0.25, 'ROLL', 90.0, 4)
      endif
      call pc_get_pdata('USER_NAME',uname)
      jobcrd(7:21)=jobname
      jobcrd(30:39)=uname
      call pc_get_pdata('PROJECT',project)
      prjcrd(11:20) = project
      call string_to_upper(jobcrd)
      call string_to_upper(prjcrd)      
      call colrplot_sym (obj%iunit,1.0, 1.0, 0.25, jobcrd, 90.0, 80)
      call colrplot_sym (obj%iunit,1.5, 1.0, 0.25, prjcrd, 90.0, 80)
!      if (obj%dev_rem.ne.'POEPLT') then
!         netloc=' '
!         netloc(1:16)=obj%nrnp(1:16)
!         netloc(17:32)=obj%idnp(1:16)
!         call colrplot_sym (obj%iunit,2.0, 1.0, 0.25, netloc, 90.0, 80)
!      endif
!
!          plot replot information --- was in old job file
!!      if (replot (1:1) .ne.' ') then
!!         call colrplot_sym (obj%iunit,2.5, 1.0, 0.1, replot, 90.0, 80)
!!      endif
!
!
      if (obj%path_atbm.ne.PATHCHECK_EMPTY) then

!          if encounter string 'ATBM' generate title block with atb
         iatb = 0
         k=index(obj%path_atbm,'ATBM')
         if (k.eq.0) then
!         check for lower case
            k=index(obj%path_atbm,'atbm')
         endif
         if (k.ne.0) then
            basint = abs (obj%bsmt2 - obj%bsmt1)
            if (obj%scale.ne.'NO'.and.obj%scale.ne.'YES') then
               call string_cc2ff(obj%scale,basint,istat)
               if(istat.ne.1)then
                 call pc_error('COLOR-->Error converting the SCALE parameter')
                 ntr=FATAL_ERROR
                 return
               endif
            endif
            call atblk(obj%path_atbm,obj%ipn,jobname,basint,.true.,istat,&
                       print_lun)
            if(istat.ne.0)then
               call pc_warning('COLOR--> ATB failed.  No title block will be &
                               &done')
               obj%path_atbm=PATHCHECK_EMPTY
               iatb=0
               go to 560
            endif
            call getlun(iatbf,istat)
            if(istat.ne.0)then
              call pc_error('COLOR--> Unable to get unit number for title &
                            &block file')
              ntr=FATAL_ERROR
              return
            endif
            iatb = 1
         endif
!             open the title block file
         open(iatbf,iostat=istat,status='OLD',file='atbf')
!          Read through once to print file to online
          write(print_lun,*)'        APPEARANCE OF GENERATED TITLE BLOCK'
          write(print_lun,*)'*************************************************'
          DO
            read(iatbf,'(A)',iostat=istat)card
            if(istat.lt.0)exit
            write(print_lun,*)card
          ENDDO
          write(print_lun,*)'*************************************************'
          rewind iatbf
!             plot the title block
         reduce = obj%frtb
         call color_thit (reduce,tht,obj%path_atbm,iatb,iatbf,ht2)
         if (tht.gt.39.5) then
            reduce = 39.5 / tht * obj%frtb
!cc        tht=tht*reduce
            tht = 39.5
         endif
         if (obj%lrrl.eq.'LR') then
            x = 3.0
            y = yo + obj%sdas_eng
            if (y.gt.39.5) y = 39.5
            if (y.lt.tht) y = tht
         else
            x = xend+6.5 + sladd+8.0 * reduce
            if (obj%ncl_at1.ne.0) x = x + real (obj%ncl_at1) * 0.5
            y = yo - obj%sdas_eng
            if (39.5 - abs (y) .lt.tht) y = .1
         endif
         if (y.le.0.0) y = .1
         write(print_lun,*) ' COLOR -> TITLE BLOCK HEIGHT = ', tht
         call color_tblk(obj%iunit,x,y,obj%sid(1),obj%sid(2),obj%sid(3),&
                         obj%sid(4),&
                         reduce,alpha,iatbf,ht2)
!            get rid of the title block file
         if(iatb.eq.1)close(iatbf,status='delete')
      endif
!
!
  560 continue
!c
!          generate ink scale if lr plot
!
      if (obj%lrrl.eq.'LR'.and.obj%ncl_at1.eq.0) then
         call color_inks(obj,xo-sladd,tinch)
      endif
!
!
!          plot the section id
      if (obj%lrrl.eq.'RL') then
         x = xend
         y = yo - obj%sdas_eng
         alpha = 180.0
      else
         y = yo + obj%sdas_eng
         x = xo
         alpha = 0.0
      endif
      call color_sid(obj%iunit,x,y,obj%cs_sid_eng,obj%sid(1),obj%sid(2),&
                     obj%sid(3),&
                     obj%sid(4),alpha)
!start - 01-03-95
      trciostat=trcio_seek_trace(obj%isn,1)
  561 continue
!
      if (obj%ncl_at1.gt.0) goto 3000
      if (obj%opt_semb.eq.'SEMB') goto 563
!!      if (igrade.eq.'YES'.and.obj%rgbflg.ge.2) goto 4000
!
!          read the first trace
      trciostat=trcio_read_trace(obj%isn,hdr,panl)
      if (trciostat.eq.TRCIO_EOF) goto 755
      if(trciostat.eq.TRCIO_ERROR)go to 9501
      knttrin = knttrin + 1
      if (obj%grade_hor.eq.'NO') goto 680
!          copy to trace a buffer
      do i = 1, nval
         trca (i) = panl (i) * fmed
      end do
      do i = 1, obj%nwih
         hdra (i) = hdr (i)
      end do
      if(obj%grade_vrt.eq.'YES'.and.obj%rgbflg.lt.2)then
         call color_grdv (trca,trcx,nval, obj%ntrpv - 1, obj%nsamp)
      endif
!
!          plot the data
  600 continue
      if (obj%ncl_at1.gt.0) goto 3000
      trciostat=trcio_read_trace(obj%isn,hdr,panl)
 605  continue
      if (trciostat.eq.TRCIO_EOF) then
        if (obj%grade_hor.eq.'NO') goto 1000
         goto 755
      else if(trciostat.eq.TRCIO_ERROR)then
        go to 9501
      endif
      knttrin = knttrin + 1
      if(knttrin.gt.obj%ktrnum)then
        trciostat=TRCIO_EOF
        go to 605
      endif
!          copy to trace b buffer
  680 do i = 1, nval
        obj%trcb (i) = panl (i) * fmed
      end do
      if(obj%grade_vrt.eq.'YES'.and.obj%rgbflg.lt.2)then
         call color_grdv (obj%trcb,trcx,nval,obj%ntrpv-1,obj%nsamp)
      endif
      do i = 1, obj%nwih
         hdrb(i)=hdr(i)
      end do
!
!
      if (obj%grade_hor.eq.'NO') goto 800
      obj%knttr = obj%knttr + 1
      tra = obj%knttr
      if (nafter.gt.0) then
         call color_abt(tra,tnnw,obj%nbts,iaskp,iado,iabtwn,obj%bttot,1)
      else
         call color_abt(tra,tnnw,obj%nbts,obj%btskp,obj%btdo,obj%btbtwn,&
                        obj%bttot,ntrph,ntppflg)
      endif
      k = tnnw
      itrlab=k
      call color_pixl(obj,trca,&
                      xo,yo,shite,twidp,k,kodes,&
                      yend,ncinc,&
                      ntppg,&
                      istat)
      if(istat.ne.0)then
        call pc_error('COLOR-->Abort - problem in pixl')
        ntr=FATAL_ERROR
        return
      endif

!            dashed line plot
      if(obj%dlu100.gt.0.00001)then
        ytemp=yo
        if(obj%tstrt.lt.0.0001)then
          ytemp=yo+obj%tstrt*obj%ips_eng
        endif
        if(obj%lrrl.eq.'LR')then
          yy=ytemp+((hdr(obj%hdr_dl)-obj%dlref)*dlhite)
        else
          yy=ytemp-((hdr(obj%hdr_dl)-obj%dlref)*dlhite)
        endif
        call colrplot_lwid(obj%iunit,.02)
        xtrce=real(itrlab)*twidp+xo
        call colrplot_plot(obj%iunit,xtrce-qtwid, yy, 3)
        call colrplot_plot(obj%iunit,xtrce+qtwid, yy, 2)
      endif
!
      trcx(1:obj%nsamp)=trca(1:obj%nsamp)
      do i = 1, ntrph - 1
         do j = 1, obj%nsamp
            addon = (obj%trcb (j) - trca (j) ) * facth
            trcx (j) = trcx (j) + addon
         end do
         obj%knttr = obj%knttr + 1
         tra = obj%knttr
         if (nafter.gt.0) then
            call color_abt(tra,tnnw,obj%nbts,iaskp,iado,iabtwn,obj%bttot,1)
         else
            call color_abt (tra,tnnw,obj%nbts,obj%btskp,obj%btdo,obj%btbtwn,&
                            obj%bttot,ntrph,ntppflg)
         endif
         k = tnnw
         call color_pixl(obj,trcx,&
                         xo,yo,shite,twidp,k,kodes,&
                         yend,ncinc,     &
                         ntppg,     &
                         istat)
         if(istat.ne.0)then
           call pc_error('COLOR-->Abort - problem in pixl')
           ntr=FATAL_ERROR
           return
         endif
      end do  !loop 750 
      if (trciostat.eq.TRCIO_OK) then
         trca(1:obj%nsamp)=obj%trcb(1:obj%nsamp)
!          if paneling - start grade over for each panel
         if (obj%ntpp.gt.0) then
            m = mod (knttrin, obj%ntpp)
            if (m.eq.0) goto 755
         elseif (nafter.gt.0) then
            if (knttrin.eq.iafter (ndxaft) ) then
               ndxaft = ndxaft + 1
               goto 755
            endif
         endif
         goto 600
      endif
!
!          plot the last trace
  755 obj%knttr = obj%knttr + 1
      tra = obj%knttr
      if (nafter.gt.0) then
         call color_abt(tra,tnnw,obj%nbts,iaskp,iado,iabtwn,obj%bttot,1)
      else
         call color_abt(tra,tnnw,obj%nbts,obj%btskp,obj%btdo,obj%btbtwn,&
                        obj%bttot, &
                        ntrph, ntppflg)
      endif
      k = tnnw
      call color_pixl(obj,trca,&
                      xo, yo,   &
                      shite,twidp,k,kodes,yend,&
                      ncinc, &
                      ntppg,istat)
      if(istat.ne.0)then
        call pc_error('COLOR-->Abort - problem in pixl')
        ntr=FATAL_ERROR
        return
      endif
      if((obj%ntpp.gt.0.or.nafter.gt.0).and.trciostat.eq.TRCIO_OK)goto 561
      goto 1000
!
!
!          color the trace
  800 continue
      obj%knttr =obj%knttr+1
      tra = obj%knttr
      call color_abt(tra,tnnw,obj%nbts,obj%btskp,obj%btdo,obj%btbtwn,&
                     obj%bttot,ntrph,ntppflg)
      itrlab = nint(tnnw)
      call color_pixl(obj,obj%trcb,&
                      xo, yo,   &
                      shite,twidp,itrlab,kodes,yend,&
                      ncinc,  &
                      ntppg,istat)
      if(istat.ne.0)then
        call pc_error('COLOR-->Abort - problem in pixl')
        ntr=FATAL_ERROR
        return
      endif
!
!            dashed line plot
      if(obj%dlu100.gt.0.00001)then
        ytemp=yo
        if(obj%tstrt.lt.0.0001)then
          ytemp=yo+obj%tstrt*obj%ips_eng
        endif
        if(obj%lrrl.eq.'LR')then
          yy=ytemp+((hdr(obj%hdr_dl)-obj%dlref)*dlhite)
        else
          yy=ytemp-((hdr(obj%hdr_dl)-obj%dlref)*dlhite)
        endif
        call colrplot_lwid(obj%iunit,.02)
        xtrce=real(itrlab)*twid+xo
        call colrplot_plot(obj%iunit,xtrce-qtwid, yy, 3)
        call colrplot_plot(obj%iunit,xtrce+qtwid, yy, 2)
      endif
      if(trciostat.eq.TRCIO_OK) goto 600
!
 1000 continue
      call colrplot_lwid(obj%iunit,.005)
      write(print_lun,*) ' NUM TRACES PLOTTED = ', obj%knttr
      goto 5000
!
 3000 continue
!              *****   tone processing  ***********
!
      call mem_alloc(kodtone,441)
      call mem_alloc(axisval,44)
!
!      call color_tone(obj%ncl_at1,obj%ncl_at2,nval,obj%lrrl,obj%invert,&
!                      obj%isn,obj%num_tr,obj%opt_tbar1,&
!                      obj%opt_tbar2,hdr,panl,hdrb,obj%trcb,kodes,&
!                      obj%ncharpack,kodtone,&
!                      axisval,obj%nsampmult,&
!                      xo,yo,yend,twid,obj%path_cbar,istat,obj%nbts,&
!                      obj%btskp,obj%btdo,    &
!                      obj%btbtwn,obj%bttot)
!      if(istat.ne.0)then
!        call pc_error('COLOR-->Abort - problem in tone')
!        ntr=FATAL_ERROR
!        return
!      endif
!
!          generate the tone ink scale
!
      if (obj%lrrl.eq.'LR') then
         x = xo - sladd
      else
         x = xend+sladd
      endif
!      call color_tink(x,yo,obj%lab1,obj%lab2,obj%lrrl,obj%ncl_at1,obj%ncl_at2,&
!                      kodtone,axisval,obj%dist_ovl,obj%lab_cb(1),&
!                      obj%lab_cb(2),&
!                      obj%lab_cb(3),obj%lab_cb(4),obj%cs_cbl_eng(1),&
!                      obj%cs_cbl_eng(2),obj%cs_cbl_eng(3),obj%cs_cbl_eng(4),&
!                      obj%opt_tbar1,obj%opt_tbar2)
      call mem_free(kodtone)
      call mem_free(axisval)
      goto 5000
!
!
 5000 continue
  563 continue
!end  -  01-03-95
!
!
!          plot the timing lines
      if (obj%lrrl.eq.'LR') then
         yepan = yo - obj%otim * obj%ips_eng
      else
         yepan = yo + obj%otim * obj%ips_eng
      endif
      if((obj%tldots(1).ne.0.or.obj%tldots(2).ne.0.or.obj%tldots(3).ne.0.or.&
          &obj%tldots(5).ne.0.or.obj%tldots(6).ne.0.or.obj%tldots(4).ne.0)&
          &.and.(obj%opt_semb.eq.'NORMAL'))then
         call color_tlne(obj%iunit,xo,xend,yo,obj%tldots(1),obj%tldots(2),&
                         obj%tldots(3),&
                         obj%tldots(5),     &
                         obj%tldots(6),obj%ups,obj%otim,obj%invert,obj%lrrl,&
                         obj%opt_tics,obj%cs_tl_eng,obj%ntistr,obj%ips_eng, &
                         alpha,yepan,obj%tstrt,twid,obj%grade_hor,obj%nrow,&
                         obj%nbts,obj%btskp,obj%btdo,   &
                         obj%btbtwn,obj%bttot,obj%irtbp,ntrph,obj%timbtwn,&
                         obj%nrlc,obj%tldots(4))
      endif
!
!          determine the number of decimal points to plot if doing
!              vertical labels
      if(obj%ntistr.ne.0)then
!        call color_vdec (obj%vl_init, obj%vl_inc, obj%ntistr, idec)
      endif
!
      do i = 1, obj%ntistr
!         call color_vlbl((obj%tim_init(i)-obj%tstrt),obj%vl_init(i),&
!                         obj%tim_inc(i),&
!                         obj%vl_inc(i),obj%vl_tot(i),obj%lrrl,obj%cs_vl(i),&
!                         obj%vl_row(i),obj%ips_eng,xo,    &
!                         xend,yo,idec,obj%otim,obj%irtbp,obj%nrow,obj%nbts,&
!                         obj%btskp,obj%btdo,obj%btbtwn,obj%bttot,twid,yend, &
!                         'NO',obj%timbtwn,obj%nrlc)
      end do
!
!
!          plot the shot points
      call colrplot_fnt(obj%iunit,19)
      kshot = obj%shot(1)
      kncr = obj%incr(1)
      ktotl = obj%totl(1)
      i = 1
      if (obj%opt_semb.eq.'NORMAL') then
         do j = 1, obj%ncol
            do i = 1, obj%nshots
               if(obj%irspp.eq.'NO')then
                  kshot = obj%shot(i)
                  kncr = obj%incr(i)
                  ktotl = obj%totl(i)
               endif
               gtrce = obj%trce(i)
               intvg = obj%intv (i)
               gtwid = twid
               ntppg = obj%ntpp
               if (obj%ntpp.gt.0.and.ntrph.gt.1) then
                  call color_t2gp(obj%trce(i),gtrce,ntrph,obj%ntpp)
                  intvg = obj%intv (i) * ntrph
                  gtwid = twidp
                  ntppg = obj%ntpp * ntrph - (ntrph - 1)
               endif
               if(nafter.gt.0.and.ntrph.gt.1)then
                  call color_shot (obj%iunit,xo,yo,yend,obj%trce(i),&
                                   obj%shot(i),     &
                                   obj%intv(i),obj%incr(i),ktotl,&
                                   obj%cs_spl_eng,0.2,obj%opt_vl,&
                                   obj%wid_vl_eng,    &
                                   alpha,twidp,obj%ltab,istat,obj%nbts,iaskp,&
                                   iado,iabtwn,&
                                   obj%bttot,ntrph,0,obj%timbtwn,obj%nrow,&
                                   obj%otim,obj%ips_eng,iafter,   &
                                   jafter,nafter,ntg)
                  if(istat.ne.0)then
                    call pc_error('COLOR-->ABORT - problem in shot')
                    ntr=FATAL_ERROR
                    return
                  endif
               else
                  call color_shot(obj%iunit,xo,yo,yend,gtrce,kshot,intvg,     &
                                  kncr,ktotl,obj%cs_spl_eng,0.2,obj%opt_vl,&
                                  obj%wid_vl_eng,alpha,gtwid,    &
                                  obj%ltab,istat,obj%nbts,obj%btskp,obj%btdo,&
                                  obj%btbtwn,obj%bttot,   &
                                  ntrph,ntppg,obj%timbtwn,obj%nrow,obj%otim,&
                                  obj%ips_eng)
                  if(istat.ne.0)then
                    call pc_error('COLOR-->ABORT - problem in shot')
                    ntr=FATAL_ERROR
                    return
                  endif
               endif
            end do
         end do
      endif
!
!          rewind the trace file
      istat=trcio_seek_trace(obj%isn,1)
!start 12-20-96
!          calculate basement interval for scale
      if (obj%scale.ne.'NO') then
         if (obj%scale.eq.'YES') then
            basint = abs (obj%bsmt2 - obj%bsmt1)
            if (basint.le.0.0) then
              write(print_lun,*) ' COLOR -  CALCULATED BASEMENT INTERVAL IS ', &
                                 &basint
              write(print_lun,*) ' COLOR -  NO SCALE WILL BE DONE'
              goto 5349
            endif
         else
            call string_cc2ff(obj%scale,basint,istat)
            if(istat.ne.1)then
              write(print_lun,*)'COLOR-->Error converting scale'
              write(print_lun,*)'        No scale will be done'
              go to 5349
            endif
         endif
         call color_scl(obj%iunit,xo,yo,obj%scda_eng,obj%cs_scale_eng,&
                        twid,mtrc,basint,&
                        alpha,ntplot)
      endif
 5349 continue
!end 12-20-96
!
!          plot automatic shot labels
      if (obj%hdr_lab(1).ne.0.or.obj%hdr_lab(2).ne.0) then
         if (nafter.gt.0) then
            l = obj%lab_inc
            if (obj%lab_init.eq.'SHOT') l = obj%lab_inc * ntrph
            call color_wrda(obj%iunit,obj%hdr_lab(1),obj%hdr_lab(2),&
                            xo,yo,yend,obj%nshots, obj%trce,    &
                            iaintv,obj%totl,obj%cs_spl_eng,obj%cs_lab(1),&
                            obj%opt_vl,obj%wid_vl_eng,alpha,obj%ltab,obj%nbts,&
                            iaskp,iado,iabtwn,obj%bttot,ntrph,0,obj%isn,&
                            obj%nwih,panl,&
                            nval,obj%lab_init,l,twidp,twidp,obj%nam_lab(1),&
                            obj%nam_lab(2), xend, obj%nrow, obj%otim, &
                            obj%timbtwn,obj%ips_eng,obj%ncol,obj%nrlc,istat,&
                            iafter,jafter,nafter,   &
                            ntg, obj%ncl_at1)
            if(istat.ne.0)then
              call pc_error('COLOR--Abort - problem with hdr_lab(1)')
              ntr=FATAL_ERROR
              return
            endif
         else
            call color_wrda(obj%iunit,obj%hdr_lab(1),obj%hdr_lab(2),&
                            xo,yo,yend,obj%nshots, obj%trce,    &
                            obj%intv,obj%totl,obj%cs_spl_eng,obj%cs_lab(2),&
                            obj%opt_vl,obj%wid_vl_eng,alpha,obj%ltab,obj%nbts,&
                            obj%btskp,obj%btdo,obj%btbtwn,obj%bttot,ntrph,&
                            obj%ntpp,obj%isn,obj%nwih,  &
                            panl,nval,obj%lab_init,obj%lab_inc,twid,twidp,&
                            obj%nam_lab(1),obj%nam_lab(2),xend,     &
                            obj%nrow,obj%otim,obj%timbtwn,obj%ips_eng,&
                            obj%ncol,obj%nrlc,istat,iafter,       &
                            jafter,nafter,ntg,obj%ncl_at1)
            if(istat.ne.0)then
              call pc_error('COLOR--Abort - problem with hdr_lab(1)')
              ntr=FATAL_ERROR
              return
            endif
         endif
      endif
!
      obj%knttr = 0
      if(obj%path_blk.ne.PATHCHECK_EMPTY)then
!              block labels
         blidfmt=' '
         call getlun(ibf,istat)
         if(istat.ne.0)then
           call pc_error('COLOR-->Unable to get unit number for block labels')
           ntr=FATAL_ERROR
           return
         endif
!              initialize block labels
         open(ibf,status='OLD',file=obj%path_blk,iostat=istat)
         if(istat.ne.0)then
           write(print_lun,*)'COLOR-->Unable to open block label file'
           write(print_lun,*)'        No block labels will be done'
           go to 588
         endif
         blinc = 'INC'
         read (ibf, 9055, iostat=istat) ctmp2
         if(istat.lt.0)then
           write(print_lun,*)'COLOR-->Block label file is empty'
           write(print_lun,*)'        No block labels will be done'
           go to 588
         endif
         rewind ibf
         if (ctmp2.eq.'X') then
            blidfmt = 'X'
         endif
         if (blidfmt.eq.' ') then
            read(ibf,9005,end=587)blhd1,blhd2,bltxt,blline,ctmp2,ctmp
         else
            read(ibf,9056,end= 587)blhd1,blhd2,blline,bltxt,ctmp2,ctmp,ctmp3
         endif
         call string_cc2ii(ctmp,iblkh1,istat)
         if(istat.ne.1)then
           write(print_lun)'COLOR-->Error encountered getting header from &
                           &block label file'
           write(print_lun)'        No block labels will be done'
           obj%path_blk=PATHCHECK_EMPTY
           go to 588
         endif
         call string_cc2ii(ctmp,iblkh1,istat)
         if(istat.ne.1)then
           write(print_lun,*)'COLOR-->Error encountered getting header from &
                             &block label file'
           write(print_lun,*)'        No block labels will be done'
           obj%path_blk=PATHCHECK_EMPTY
           go to 588
         endif
         if (obj%path_blk.ne.PATHCHECK_EMPTY) then
            write(print_lun,*)' COLOR=> HEADER WORD USED FOR BLOCK LABELS = ',&
                                iblkh1
            if (ctmp2.eq.'2'.or.ctmp2.eq.'DEC') blinc = 'DEC'
            if (ctmp2.eq.' 2') blinc = 'DEC'
            needblk = 0
            ifhd1 = 0
            oblhd2 = - 1.0
            obj%ftr1 = 0.0
            ibl3d = 0
            almost_equal = ameq (blline, 0.0, 0.00001)
            if(.not.almost_equal)ibl3d = 1
            if(ibl3d.eq.1) then
               k = mod (iblkh1, 2)
               if (k.eq.0) then
                  iblkhdl = iblkh1 - 1
               else
                  iblkhdl = iblkh1 + 1
               endif
               rewind ibf
               close (unit = ibf, status = 'KEEP')
            endif
         endif
      endif
      goto 588
  587 write(print_lun,*) ' COLOR-->ERROR FETCHING BLOCK LABEL FILE - &
                         &NO BLOCK LABELS WILL be done'
      obj%path_blk=PATHCHECK_EMPTY
  588 continue
      if(obj%path_tie1.ne.PATHCHECK_EMPTY)then
!              tie lines
         f = 2 * obj%cs_spl_eng
         call getlun(itf,istat)
         if(istat.ne.0)then
           write(print_lun,*)'COLOR-->Unable to get unit number for tie line &
                             &file'
           write(print_lun,*)'        No tie lines will be done'
           obj%path_tie1=PATHCHECK_EMPTY
           go to 595
         endif
!              initialize tie lines
         open(itf,status='OLD',file=obj%path_tie1,iostat=istat)
         if(istat.ne.0)then
           write(print_lun,*)'COLOR-->Unable to open tie line file'
           write(print_lun,*)'        No tie lines will be done'
           obj%path_tie1=PATHCHECK_EMPTY
           go to 595
         endif
         tf1inc = 'INC'
         read (itf, 9007, iostat=istat) tie1hd, tie1txt, ctemp, ctmp
         if(istat.lt.0)then
           write(print_lun,*)'COLOR-->Tie line file is empty'
           write(print_lun,*)'        No tie lines will be done'
           obj%path_tie1=PATHCHECK_EMPTY
           go to 595
         endif
         call string_cc2ii(ctmp,itieh1,istat)
         if(istat.ne.1)then
           write(print_lun,*)'COLOR-->Error encountered getting tie line &
                             &header from tie line file'
           write(print_lun,*)'        No tie lines will be done'
           obj%path_tie1=PATHCHECK_EMPTY
           go to 595
         endif
         if(obj%path_tie1.ne.PATHCHECK_EMPTY)then
            write(print_lun,*)' COLOR=> HEADER WORD USED FOR TIE FILE = ',itieh1
            if (ctemp.eq.'2'.or.ctemp.eq.'DEC')tf1inc = 'DEC'
            if (ctemp.eq.' 2') tf1inc = 'DEC'
            nedtie1 = 0
         endif
      endif
      if (obj%path_tie2.ne.PATHCHECK_EMPTY)then
!              second row of tie lines
         f = 2 * obj%cs_spl_eng
         call getlun(itf2,istat)
         if(istat.ne.0)then
           write(print_lun,*)'COLOR-->Unable to get unit number for tie line &
                             &file 2'
           write(print_lun,*)'        Tie file 2 will not be plotted'
           obj%path_tie2=PATHCHECK_EMPTY
           go to 595
         endif
!              initialize tie lines
         open(itf2,status='OLD',file=obj%path_tie2,iostat=istat)
         if(istat.ne.0)then
           write(print_lun,*)'COLOR-->Unable to open tie line file2'
           write(print_lun,*)'        No tie lines will be done'
           obj%path_tie2=PATHCHECK_EMPTY
           go to 595
         endif
         tf2inc = 'INC'
         read (itf2, 9007, iostat=istat) tie2hd, tie2txt, ctemp, ctmp
         if(istat.lt.0)then
           write(print_lun,*)'COLOR-->Tie line file2 is empty'
           write(print_lun,*)'        No tie lines will be done'
           obj%path_tie2=PATHCHECK_EMPTY
           go to 595
         endif
         call string_cc2ii(ctmp,itieh2,istat)
         if(istat.ne.1)then
           write(print_lun,*)'COLOR-->Error encountered getting tie line &
                             &header from tie2 file'
           write(print_lun,*)'        Tie file 2 will not be plotted'
           obj%path_tie2=PATHCHECK_EMPTY
           go to 595
         endif
         if (obj%path_tie2.ne.PATHCHECK_EMPTY) then
            write(print_lun,*)'COLOR=> HEADER WORD USED FOR TIE2 FILE = ',itieh2
            if (ctemp.eq.'2'.or.ctemp.eq.'DEC')tf2inc = 'DEC'
            if (ctemp.eq.' 2')tf2inc = 'DEC'
            nedtie2 = 0
         endif
      endif
  595 continue
!
      if (obj%opt_semb.eq.'SEMB') goto 7000
!          generate ink scale if rl plot
!
      if (obj%lrrl.ne.'LR'.and.obj%ncl_at1.eq.0) then
         call color_inks(obj,xend-0.5-sladd,tinch)

      endif
!
!          side labels
!      if (obj%nslbl.gt.0) then
!         if (obj%lrrl.eq.'LR') then
!            x = xo
!         else
!            x = xend
!         endif
!         call color_sdlb(x,yo,alpha,obj%sltim,obj%slbl,obj%slsz,obj%slrow,&
!                         obj%nslbl,  &
!                         sladd,obj%ips_eng,obj%tstrt,obj%otim,obj%timbtwn)
!      endif
!
!                                  plot direction arrows
      if (obj%ar_ht_eng.eq.0.0) obj%ar_ht_eng = (obj%ips_eng * .035)
      x = xo + twid
      if (obj%lrrl.eq.'LR') then
         y = yo + obj%arda_eng
      else
         y = yo - obj%arda_eng
      endif
      if (obj%ar_beg.ne.' ')then
!                                  plot arrow pointing to first trace in
         call color_arow(obj%iunit,x,y,obj%ar_ht_eng,obj%ar_len_eng,&
                         obj%ar_beg,0,alpha,&
                         istat)
         if(istat.ne.0)go to 5080
      endif
      if(obj%ar_end.ne.' ')then
         x = xend-twid-obj%ar_len_eng
!                        plot arrow pointing to last trace in
         call color_arow(obj%iunit,x,y,obj%ar_ht_eng,obj%ar_len_eng,&
                         obj%ar_end,1,alpha,&
                         istat)
      endif
 5080 continue
      if(obj%path_blk.eq.PATHCHECK_EMPTY.and.obj%path_tie1.eq.PATHCHECK_EMPTY&
         .and.obj%path_tie2.eq.PATHCHECK_EMPTY)go to 7000 
!
!             reread data for annotation from headers
      istat=trcio_seek_trace(obj%isn,1)
      knttrin = 0
      knttone = 0
      knthead = 0
!        open temporary file if doing 3d block labels
      if(ibl3d.eq.1)then
        call getlun(i3bl,istat)
        if(istat.ne.0)then
          write(print_lun,*)'COLOR-->Unable to get unit number for 3d block &
                            &labels'
          write(print_lun,*)'        Block labels not plotted'
          ibl3d=0
          obj%path_blk=PATHCHECK_EMPTY
        endif
        open(i3bl,status='REPLACE',file=i3blf,iostat=istat)
        if(istat.ne.0)then
          write(print_lun,*)'COLOR-->Unable to open file for 3d block labels'
          write(print_lun,*)'        Block labels will not be done'
          ibl3d=0
          obj%path_blk=PATHCHECK_EMPTY
        endif
      endif
 6000 continue
      trciostat=trcio_read_trace(obj%isn,hdr,panl)
      if(trciostat.eq.TRCIO_ERROR)go to 9501
      knttrin = knttrin + 1
      knthead = knttrin
      if (obj%ncl_at1.gt.0) then
         kk = mod (knttrin, 2)
         if (kk.eq.0) goto 6000
         knttone = knttone+1
         knthead = knttone
      endif
      if (ibl3d.eq.1) then
         tmpx = hdr (iblkh1)
         tmpy = hdr (iblkhdl)
         k = nint (tmpy)
         tmpy = k
         write (i3bl) knttrin, tmpx, tmpy
      endif
      if (knthead.eq.1) then
         fblkhd = hdr(iblkh1)
         ftiehd = hdr(itieh1)
         ftiehd2 = hdr(itieh2)
      endif
      if (trciostat.eq.TRCIO_EOF) goto 7000
      if (obj%path_blk.eq.PATHCHECK_EMPTY) goto 6620
      call colrplot_fnt(obj%iunit,19)
      lsttrce = obj%num_tr + numbt
!          block labels
!
 6550 continue
      if (obj%path_blk.ne.PATHCHECK_EMPTY.and.needblk.lt.2.and.ibl3d.eq.0)then
         if (needblk.eq.1) then
            if (blidfmt.eq.' ') then
               read (ibf, 9006, end = 6600) blhd1, blhd2, bltxt
            else
               read (ibf, 9056, end = 6600) blhd1, blhd2, blline, bltxt,&
                                           ctemp, ctmp, ctmp2
            endif
            needblk = 0
         endif
!              determine if blhd1 plotted on this trace
         if (ifhd1.eq.0) then
!                first trace is special case
            if (knthead.eq.1) then
               if((blhd1.lt.hdr(iblkh1).and.blinc.eq.'INC').or. &
                  (blhd1.gt.hdr(iblkh1).and.blinc.eq.'DEC'))then
                  obj%ftr1 = 0.0
                  ifhd1 = 1
                  goto 6555
               endif
            endif
            almost_equal = ameq (blhd1, oblhd2, .001)
            if (almost_equal) then
               obj%ftr1 = oftr2
               ifhd1 = 1
               goto 6555
            endif
            call color_ckh(hdr(iblkh1),blhd1,blinc,fblkhd,match)
            if (match.eq.1) then
               obj%ftr1 = knthead
               ifhd1 = 1
            endif
         endif
 6555    continue

!              determine if blhd2 plotted on this trace
!                last trace is special case
         if (knttrin.eq.obj%num_tr.or.knttone.eq.(obj%num_tr-1))then
            if((blinc.eq.'INC'.and.blhd2.gt.hdr(iblkh1)).or.&
               (blinc.eq.'DEC'.and.blhd2.lt.hdr(iblkh1)))then
               ftr2 = obj%num_tr + 1
               match = 1
               if (nafter.gt.0) then
                  gtrce1=color_2gab(obj%ftr1,iafter,jafter,nafter,ntrph,ntg)
                  gtrce2=color_2gab(ftr2,iafter,jafter,nafter,ntrph,ntg)
                  call color_abt(gtrce1,tnnw1,obj%nbts,iaskp,iado,iabtwn,&
                                 obj%bttot,1)                  
                  call color_abt(gtrce2,tnnw2,obj%nbts,iaskp,iado,iabtwn,&
                                 obj%bttot,1)                  
                  call color_blid(obj%iunit,xo,yo,tnnw1,tnnw2,bltxt,&
                                  obj%cs_bl_eng,&
                                  twidp,ntg,alpha)
               else

                  call color_abt(obj%ftr1,tnnw1,obj%nbts,obj%btskp,obj%btdo,&
                                 obj%btbtwn,obj%bttot,1)
                  call color_abt(ftr2,tnnw2,obj%nbts,obj%btskp,obj%btdo,&
                                 obj%btbtwn,obj%bttot,1)
                  call color_blid(obj%iunit,xo,yo,tnnw1,tnnw2,bltxt,&
                                  obj%cs_bl_eng,&
                                  twid,lsttrce,alpha)
               endif
               nc=len_trim(bltxt)
               write(print_lun,*)'BLOCK LABEL ',bltxt (1:nc) ,' PLOTTED &
                                 &BETWEEN TRACES ',obj%ftr1,' and ',FTR2
               needblk = 2
               goto 6610
            endif
         endif
         call color_ckh(hdr(iblkh1),blhd2,blinc,fblkhd,match)
         if (match.eq.1) then
!          reference obj%ftr1 to prevent losing it'S VALUE (MICKEY MOUSE)
!            write (card, '(F8.0)') obj%ftr1
            ftr2 = knthead
            if (nafter.gt.0) then
               gtrce1=color_2gab(obj%ftr1,iafter,jafter,nafter,ntrph,ntg)
               gtrce2=color_2gab(ftr2,iafter,jafter,nafter,ntrph,ntg)
               call color_abt(gtrce1,tnnw1,obj%nbts,iaskp,iado,iabtwn,&
                              obj%bttot,1)
               call color_abt(gtrce2,tnnw2,obj%nbts,iaskp,iado,iabtwn, &
                              obj%bttot,1)
               call color_blid(obj%iunit,xo,yo,tnnw1,tnnw2,bltxt,&
                               obj%cs_bl_eng,twidp,&
                               ntg,alpha)
            else
               call color_abt(obj%ftr1,tnnw1,obj%nbts,obj%btskp,obj%btdo,&
                              obj%btbtwn,obj%bttot,1)
               call color_abt(ftr2,tnnw2,obj%nbts,obj%btskp,obj%btdo,&
                              obj%btbtwn,obj%bttot,1)
               call color_blid(obj%iunit,xo,yo,tnnw1,tnnw2,bltxt,&
                               obj%cs_bl_eng,twid, &
                               lsttrce,alpha)
            endif
            nc=len_trim(bltxt)
            write(print_lun,*)' BLOCK LABEL ',bltxt(1:nc),' PLOTTED BETWEEN &
                              &TRACES ',obj%ftr1,' and ',FTR2
            needblk = 1
            ifhd1 = 0
            oblhd2 = blhd2
            oftr2 = ftr2
         endif
         if(match.eq.-1)then
            needblk = 1
            goto 6550
         endif
         goto 6610
 6600    needblk = 2
      endif
 6610 continue

!           tie lines
 6620 continue
!                 tie lines from a file
      if(obj%path_tie1.ne.PATHCHECK_EMPTY.and.nedtie1.lt.2)then
         call colrplot_fnt(obj%iunit,0)
         if(nedtie1.eq.1)then
            read(itf,9008,end=6630)tie1hd,tie1txt
            nedtie1 = 0
         endif
!            determine if tie plotted on this trace
         call color_ckh(hdr(itieh1),tie1hd,tf1inc,ftiehd,match)
         if(match.eq.1)then
            ftra = knthead
            if(nafter.gt.0)then
               gtrce = color_2gab(ftra,iafter,jafter,nafter,ntrph,ntg)
               call color_abt(gtrce,tnnw,obj%nbts,iaskp,iado,iabtwn,&
                              obj%bttot,1)
               k = tnnw
               call color_tie(obj%iunit,xo,yo,obj%cs_lt_eng,obj%tida_eng,&
                              twidp,k,&
                              tie1txt,obj%cs_spl_eng,alpha,twidp)
            else
               call color_abt(ftra,tnnw,obj%nbts,obj%btskp,obj%btdo,obj%btbtwn,&
                              obj%bttot,1)
               k = tnnw
               call color_tie(obj%iunit,xo,yo,obj%cs_lt_eng,obj%tida_eng,&
                              twid,k,tie1txt,obj%cs_spl_eng,alpha,twid) 
              
            endif
            write(print_lun,*)' TIE LINE ',tie1txt,' PLOTTED AT TRACE ',knthead
            nedtie1 = 1
         endif
         if(match.eq.- 1)then
            nedtie1 = 1
            goto 6620
         endif
         goto 6635
 6630    nedtie1 = 2
      endif
 6635 continue
!
!                 second set of tie lines
      if(obj%path_tie2.ne.PATHCHECK_EMPTY.and.nedtie2.lt.2)then
         if(nedtie2.eq.1)then
            read(itf2,9008,end=6640)tie2hd,tie2txt
            nedtie2 = 0
         endif
!            determine if tie plotted on this trace
         call color_ckh(hdr(itieh2),tie2hd,tf2inc,ftiehd2,match)
         if(match.eq.1)then
            ftra = knthead
            if(nafter.gt.0)then
               gtrce = color_2gab(ftra,iafter,jafter,nafter,ntrph,ntg)
               call color_abt(gtrce,tnnw,obj%nbts,iaskp,iado,iabtwn,obj%bttot,1)
               k = tnnw
               call color_tie(obj%iunit,xo,yo,obj%cs_lt_eng,obj%tida_eng,&
                              twidp,k,tie1txt,&
                              obj%cs_spl_eng,alpha,twidp)
            else
               call color_abt(ftra,tnnw,obj%nbts,obj%btskp,obj%btdo,obj%btbtwn,&
                              obj%bttot,1)
               k = tnnw
               call color_tie(obj%iunit,xo,yo,obj%cs_lt_eng,&
                              obj%tida_eng+1.0,twid,k,&
                              tie2txt,obj%cs_spl_eng,alpha,twid)
            endif
            write(print_lun,*)'COLOR -TIE2 LINE ',tie2txt,' PLOTTED AT TRACE ',&
                               knthead
            nedtie2 = 1
         endif
         if(match.eq.- 1)then
            nedtie2 = 1
            goto 6635
         endif
         goto 6645
 6640    nedtie2 = 2
      endif
 6645 continue

      goto 6000
!
 7000 continue
!      if(ibl3d.ne.0)then
!         endfile i3bl
!         rewind i3bl
!         close(unit=i3bl,status = 'KEEP')
!         call color_3dbl(i3blf,ibf,obj%num_tr,xo,yo,obj%cs_bl_eng,twid,alpha,&
!                         obj%nbts,obj%btskp,obj%btdo,obj%btbtwn,obj%bttot,&
!                         nafter,iafter,&
!                         jafter,ntg,ntrph,iaskp,iado,iabtwn,twidp,lsttrce)
!      endif
      call colrplot_fnt(obj%iunit,19)
      if(obj%opt_semb.eq.'NORMAL')goto 8000
      call mem_alloc(axisval,obj%ntpp)
      call mem_alloc(tloc,100)
      call mem_alloc(vloc,100)
      call mem_alloc(trca,obj%nsamp)
      call mem_alloc(obj%trcb,obj%nsamp)
!
!      call color_semb(obj%isn,obj%ntpp,axisval,header,trca,nval,obj%path_vel, &
!                      tloc,vloc,shite,xo,yo,obj%dt,alpha,twid,obj%nbts,&
!                      obj%btskp,obj%btdo, &
!                      obj%btbtwn,obj%bttot,ntrph,obj%grade_vrt,obj%scale_vel,&
!                      obj%ntrpv,obj%trcb,trcx,obj%vals,&
!                      obj%kolors,obj%ncolr,obj%vlinc,obj%lrrl,kodes,&
!                      obj%ncharpack,&
!                      yend,obj%nsampmult,ncinc,&
!                      obj%invert,obj%ncol,obj%nrlc,obj%time,obj%nsamp,shiteo,&
!                      mtrc,obj%trce(1),obj%shot(1),&
!                      obj%intv(1),obj%incr(1),obj%totl(1),obj%cs_spl_eng,&
!                      obj%coor_beg,obj%coor_end,istat)
!       if(istat.ne.0)then
!         write(print_lun,*)'COLOR-->Abort - Problem in semb routine'
!         ntr=FATAL_ERROR
!         return
!       endif
!
      if(obj%tldots(1).ne.0.or.obj%tldots(2).ne.0.or.obj%tldots(3).ne.0.or.&
         obj%tldots(5).ne.0.or.obj%tldots(6).ne.0.or.obj%tldots(4).ne.0)then
         obj%bttot(1)= obj%ncol - 1
         numbt = obj%btdo(1)* obj%bttot(1)
         k = obj%ntppsem * obj%ncol
         xend = xo +(numbt + k)/ obj%tpi_eng
         if(obj%ntpp.eq.0)obj%nrow = 0
!               Semblance plot timing lines
         call color_tlne(obj%iunit,xo,xend,yo,obj%tldots(1),obj%tldots(2),&
                         obj%tldots(3),obj%tldots(5),    &
                         obj%tldots(6),obj%ups,obj%otim,obj%invert,obj%lrrl,&
                         obj%opt_tics,obj%cs_tl_eng,obj%ntistr,obj%ips_eng,&
                         alpha,yepan,obj%tstrt,twid,obj%grade_hor,obj%nrow,&
                         obj%nbts,obj%btskp,obj%btdo,&
                         obj%btbtwn,obj%bttot,obj%irtbp,ntrph,obj%timbtwn,&
                         obj%nrlc,obj%tldots(4))
      endif
!
      call mem_free(axisval)
      call mem_free(tloc)
      call mem_free(vloc)
      call mem_free(trca)
      call mem_free(obj%trcb)
 8000 continue
!
!          trace overlay
!          
      method = 1
      if(obj%path_ovl.ne.PATHCHECK_EMPTY)then
!               set tpi_ovl_eng if not already set
        almost_equal=ameq(obj%tpi_ovl,0.0,.00001)
        if(almost_equal)then
          if(obj%ratio_tpi.le.0)then
            write(print_lun,*)'COLOR ERROR -->TPI_OVL is zero'
            write(print_lun,*)'               Trace overlay will not be done'
            go to 8800
          endif
          obj%tpi_ovl_eng=obj%tpi_eng/real(obj%ratio_tpi)
        endif
!          two ways to calculate the number of overlay traces in a
!          panel.  method 2 divides the number of overlay traces by the
!          number of panels.  method 1 insures that the number of
!          overlay traces in a panel has a correspondence to the number
!          of color traces in a panel (2 to 1, 3 to 1, etc).  method 1
!          is the original method.  method 2 will be used if proper
!          conditions are met, otherwise, we will revert back to original
!          method.
         j = 1
         if(obj%ntpp.gt.0)then
            k = mod(obj%num_tr,obj%ntpp)
            r = obj%tpi_eng / obj%tpi_ovl_eng
!!               don't use method 2 unless tpi_eng/tio_eng is a whole number
            whole = anint(r)
            almost_equal = ameq(r,whole,0.00001)
            if(.not.almost_equal)k = 1
            if(k.eq.0)then
               np = obj%num_tr / obj%ntpp
               j = mod(obj%ntr_ovl,np)
            endif
         endif
         if(j.eq.0)then
            n = obj%ntr_ovl / np
            method = 2
         else
            r = obj%tpi_ovl_eng / obj%tpi_eng
            ntppo = obj%ntpp * r
            n = ntppo
         endif
         if(obj%opt_calc.eq.'PLOT')then
            i=obj%otim/obj%dt + 1
            call color_trce(obj,twid,i,xo,yo,yend,istat,n,nspan,&
                            ntrph,iafter,jafter,nafter,&
                            ntg,iaskp,iado,iabtwn,method)
            if(istat.ne.0)go to 8050
         else
            i=obj%ndpt
            call color_trce(obj,twid,i,xo,yo,yend,istat,n,nspan,&
                            ntrph,iafter,jafter,nafter,&
                            ntg,iaskp,iado,iabtwn,method)
            if(istat.ne.0)go to 8050
         endif
      endif
      goto 8800
 8050 write(print_lun,*)' COLOR--> PROBLEM ENCOUNTERED WITH TRACE OVERLAY'
 8800 continue
!
      yinc =(obj%otim + obj%timbtwn)* obj%ips_eng
      if(obj%nplbl.lt.npan2lab)then
         k = obj%nplbl
      else
         k = npan2lab
      endif
      call color_plbl(obj%iunit,k,obj%plbl,obj%prow,obj%pcol,obj%btskp,&
                      obj%btdo,xo,yo,obj%cs_pl_eng,twid,yinc,alpha,ntrph)
!
      call colrplot_plot(obj%iunit,0.0,0.0,999)
!
!          Write out control card file for color2cgm
!             the cgm control card file
      call getlun(lun_cgmcc,istat)
      if(istat.ne.0)then
         call pc_error('Unable to get unit number for cgm control file')
      endif
      open(lun_cgmcc,status='NEW',iostat=istat,form='UNFORMATTED',&
           file=icgmcc)
      if(istat.ne.0)then
        call pc_error('Unable to open control file ',icgmcc)
      endif
      letter=icgmcc(7:7)
      k=ichar(letter)
      k=k+1
      letter=char(k)
      write(print_lun,*)' Control card file ' // trim(icgmcc)
      icgmcc(7:7)=letter
      write(print_lun,*)jobname,xend,obj%lrrl,obj%dev_loc,obj%quality,&
                        obj%copies,obj%opt_dev,yo,yend,obj%nsamporig,&
                        obj%ct_ovl,obj%tpi_ovl_eng,obj%glav_ovl
                        
      write(lun_cgmcc)jobname,xend,obj%lrrl,obj%dev_loc,obj%quality,obj%copies,&
                      obj%opt_dev,yo,yend,obj%nsamporig,obj%ct_ovl,&
                      obj%tpi_ovl_eng,obj%glav_ovl
                      
!
      if(associated(panl))deallocate(panl)
      if(obj%opt_scale.eq.'CALC')deallocate(ipdf)
      call mem_free(trca)
      if(obj%ntpp.eq.0.and.obj%nbts.gt.0.and.ntrph.gt.1)then
        deallocate(iafter)
        deallocate(ibefor)
        if(nafter.eq.0)then
          deallocate(jafter)
        endif
      endif
      call mem_free(iaskp)
      call mem_free(iado)
      call mem_free(iabtwn)
      call mem_free(iaintv)
      return
 9501 call pc_error('COLOR--> Problem with trcio')
      ntr=FATAL_ERROR
      return
      else if(ntr == FATAL_ERROR)then
           call color_wrapup(obj)
      end if
!
!-------------------TRACE PROCESSING HERE----------------------------
!        write out all data input
      if(obj%opt_calc.eq.'ALL'.and.(obj%opt_scale.eq.'CALC'.or.obj%nbox.gt.0))&
        then
         do iiii = 1,ntr
            obj%kntall = obj%kntall + 1
            if(obj%rp.eq.'YES')then
              call color_revpol(trace(1:obj%ndpt,iiii),obj%ndpt)
            endif
            istat=trcio_write_trace(obj%isn2,header(1:obj%ndpt,iiii),&
                                    trace(1:obj%ndpt,iiii))
            if(istat.eq.TRCIO_ERROR)then
               write(print_lun,*)'COLOR-->Abort - trcio write error A'
               ntr=FATAL_ERROR
               return
            endif
         end do
      endif
      if(obj%opt_calc.eq.'ALL')then
         do iiii = 1,ntr
            k = mth_isamax(obj%ndpt,trace(1:obj%ndpt,iiii),1)
            trmax = abs(trace(k,iiii))
            if(trmax.ne.0)obj%glav = amax1(obj%glav,trmax)
         end do
      endif
!
!
      if(obj%ktrnum.gt.obj%ntread)then
         goto 410
      endif
!
      do 400 iiii = 1,ntr
!
         obj%ktin = obj%ktin + 1
         if(obj%ktin.le.obj%skip_init)cycle
!          copy to trace work buffer
         do 100 i = 1,obj%nvi
            obj%trcb(i)= trace(i,iiii)
  100    end do
         if(obj%rp.eq.'YES')call color_revpol(obj%trcb,obj%nvi)
         do 101 i = 1,obj%nwih
            hdrb(i)= header(i,iiii)
  101    end do
!
!          save data to trcio file
         if(obj%ntpp.gt.0.or.obj%opt_semb=='SEMB')then
           istat=trcio_write_trace(obj%idn,hdrb,obj%trcb)
         else
           istat=trcio_write_trace(obj%isn,hdrb,obj%trcb)
         endif
         if(istat.ne.0)then
           write(print_lun,*)'COLOR Abort - error writing to trcio file B'
           ntr=FATAL_ERROR
           return
         endif
!
!
!          obj%ntppset = -1 => need to set obj%ntpp and have read first trace
!          obj%ntppset = 0  => need to set obj%ntpp
!          obj%ntppset = 1  => obj%ntpp has already been set
         if(obj%ntppset.le.0)then
!          set obj%ntpp based on header word 6
            if(hdrb(6).gt.obj%hdr6lst)then
               obj%ntpp = obj%ntpp + 1
               obj%hdr6lst = hdrb(6)
               if(obj%ntppset.eq.0)then
                  obj%axis1 = hdrb(6)
                  obj%ntppset = - 1
               endif
            else
!          calculate obj%tpi_eng,twid --- assume 1 unit = 1 trace
               obj%axis2 = obj%hdr6lst
               obj%tpi_eng = obj%scale_vel
               obj%ntppsem =(obj%axis2 - obj%axis1)+ .0001
               twid = 1 / obj%tpi_eng
               gtwid = twid
!          set the shot point array...
!            make labels even increments of 1000
               idum = obj%axis1 + .0001
               j = mod(idum,1000)
               if(j.eq.0)then
                  obj%trce(1)= 1.0
                  call string_ff2cc(obj%axis1,ctmp)
                  obj%shot(1)=ctmp
                  obj%totl(1)=(obj%axis2 - obj%axis1)/ obj%scale_vel + 1
               else
                  iadd = 1000 - j
                  idum = idum + iadd
                  obj%trce(1)= real(idum - obj%axis1)
                  call string_ii2cc(idum,ctmp)
                  obj%shot(1)=ctmp
                  obj%totl(1)=(obj%axis2 - idum)/ obj%scale_vel + 1
               endif
               f = obj%scale_vel
               obj%intv(1)= 1000
               obj%incr(1)= 1000
               obj%totl(1)=(obj%axis2 - idum)/ obj%intv(1)+ 1
               obj%opt_vl = 'YES'
               write(print_lun,*)' COLOR => SHOT POINT ARRAY SET AS trce = ',&
                                   obj%trce(1),' shot = ',CTMP,' obj%intv = ',&
                                   obj%intv(1),' obj%incr = ',obj%incr(1),&
                                 ' totl = ',obj%totl(1)
               obj%lab_inc = obj%ntpp
               k = obj%ntpp / 2
               call string_ii2cc(k,ctmp)
               obj%lab_init=ctmp
               obj%ntppset = 1
               obj%ncol = 1
               obj%irspp = 'YES'
               if(obj%ntpp.gt.0)then
                  j = obj%ntpp * obj%nrow
                  k = mod(obj%num_tr,j)
                  obj%ncol = obj%num_tr / j + .0001
                  obj%nrlc = 1
                  if(k.gt.0)then
                     m = obj%num_tr -(obj%ncol * j)
                     obj%nrlc = m / obj%ntpp
                     obj%ncol = obj%ncol + 1
                  endif
               endif
!          if paneling and user has not set blank traces - set it here
!        if blank trace pattern not set - set it based on obj%ntpp
               if(obj%nbts.eq.0)then
                  obj%btskp(1)= obj%ntppsem
                  obj%kbtskp = obj%ntpp
                  panwid = obj%ntppsem / obj%tpi_eng
                  tiold = obj%ntpp / panwid
                  obj%twidk = 1 / tiold
!             make btdo large enough to plot timing line number
                  b = obj%cs_tl_eng * 3.0
                  if(obj%ups.gt.0)then
                     f = log10(real(obj%ups))+ 1.5
                     b = obj%cs_tl_eng * f
                  endif
                  k = b * obj%tpi_eng + 2.500001
                  ko = b * tiold+2.50001
                  obj%btdo(1)= k
                  obj%btbtwn(1)= obj%ntppsem
                  obj%kbtbtwn = obj%ntpp
                  obj%bttot(1)= obj%ncol - 1
                  obj%kbttot = obj%ncol - 1
                  obj%nbts = 1
                  if(obj%ncol.gt.1)then
                     obj%irtbp = 'YES'
                  else
                     obj%irtbp = 'NO'
                  endif
                  write(print_lun,*)'COLOR-BLANK TRACE PATTERN SET AS &
                                    &BTSKP = ',&
                                     obj%btskp(1),' BTDO = ',obj%btdo(1),&
                                    ' BTBTWN = ',   &
                                     obj%btbtwn(1),' BTTOT = ',obj%bttot(1)
                  obj%iuserbt = 0
               elseif(obj%nbts.gt.0)then
                  write(print_lun,*)' COLOR=> USER HAS SET THEIR OWN BLANK &
                                    &TRACE PATTERN'
                  obj%iuserbt = 1
                 ! (obj%nbts.eq.0)
               endif  !(hdrb(6).gt.obj%hdr6lst)
            endif     !(obj%ntppset.eq.0)
         endif

!         find the largest absolute value
         if(obj%opt_calc.eq.'PLOT')then
            trmax = abs(obj%trcb(mth_isamax(obj%nvi,obj%trcb,1)))
            if(trmax.eq.0)goto 385
            obj%glav = amax1(obj%glav,trmax)
         endif
  385    continue
         if(obj%ktrnum.eq.1)then
            obj%bsmt1 = header(17,iiii)
         endif
         if(obj%ktrnum.eq.2)then
            obj%bsmt2 = header(17,iiii)
         endif
!
!
!
         obj%ktrnum = obj%ktrnum + 1
         ntplot = obj%ktrnum
         if(obj%ntpp.gt.0)then
            obj%ktpanl = obj%ktpanl + 1
            ntplot = obj%ktpanl
         endif
         if(obj%ktrnum.gt.obj%ntread)then
            exit
         endif
  400 end do
  410  continue
 9005 format(2f10.0,a10,f8.0,32x,a3,t77,a2)
 9006 format(2f10.0,a10)
 9007 format(f10.0,a40,20x,a3,t77,a2)
 9008 format(f10.0,a40)
 9055 format(t79,a1)
 9056 format(3f8.0,a51,a1,a2,a1)
      end subroutine color
      subroutine color_abt (tra, tnnw, nbts, iskp, ido, ibtwn, itot, &
        ntrp, ntpp)
!
!          adjusts trace number to be annotated for blank traces
!
!         tra  = trace # to mark
!         tnnw = new trace # ajusted for blank traces
!         nbts = the number of blank trace patterns.
!         iskp = number of traces to skip before blanks.
!         ido  = number of blanks to insert.
!        ibtwn = number of traces between blanks.
!         itot = total number of sets this pattern
!         ntrp = multiplier to use for interpolated trace numbers
!                when horizontal grade needs to be considered
!         ntpp = number of traces per panel (optional)
!                0 = no paneling done
!
      real,                  intent(in)  :: tra
      real,                  intent(out) :: tnnw
      integer,               intent(in)  :: nbts
      integer, dimension(*), intent(in)  :: iskp
      integer, dimension(*), intent(in)  :: ido
      integer, dimension(*), intent(in)  :: ibtwn
      integer, dimension(*), intent(in)  :: itot
      integer,               intent(in)  :: ntrp
      integer, optional,     intent(in)  :: ntpp
!
      integer :: i,itra,j,ltra,m,nblk,ntpp_tmp
!
      if (present(ntpp)) then
        ntpp_tmp = ntpp
      else
        ntpp_tmp = 0
      endif
      nblk = 0
      itra = tra + .0001
      do 400 i = 1, nbts
!
         if (ido (i) .eq.0) goto 500
!
         ltra = iskp (i) * ntrp
         if (ntpp_tmp.gt.0) ltra = ltra - (ntrp - 1)
         if (ltra.ge.itra) goto 500
         m = itot (i) * ntrp
         do 100 j = 1, m
            nblk = nblk + ido (i) * ntrp
            if ( (j * ntrp) .eq.m) goto 400
            ltra = ltra + ibtwn (i) * ntrp
            if (ntpp_tmp.gt.0) ltra = ltra - (ntrp - 1)
            if (ltra.ge.itra) goto 500
  100    end do
  400 end do
  500 continue
      tnnw = tra + real (nblk)
      return
!
      end subroutine color_abt
      subroutine color_arc (iunit,x, y, r1, r2, theta, istat)
!
!          draws an arc of thickness specified by giving radius r1
!            and r2 as outside and inside radius of an open cirlce
!
!       x   = x coordinate for center of circle
!       y   = y coordinate for center of circle
!       r1  = outside radius
!       r2  = inside radius
!     theta = portion of circle to keep for making arc specified in
!             degrees
!          theta must be 0,90,180, or 270
!     istat = return status (0 = normal)
!
      integer,  intent(in) :: iunit
      real,     intent(in) :: x
      real,     intent(in) :: y
      real,     intent(in) :: r1
      real,     intent(in) :: r2
      real,     intent(in) :: theta
      integer, intent(out) :: istat
!
      real :: xpts(5), ypts(5), rad
!
!!!      data pi / 3.141592653 /   ! use named_constants
!
!          convert theta to radians
      rad = theta * pi / 180.0
!
!          draw red circle of ourside radius
      call colrplot_tclr (iunit,6)
      call colrplot_circ (iunit,x, y, r1, 0)
!c
!          white out inside of circle with inside radius
      call colrplot_tclr (iunit,9)
      call colrplot_circ (iunit,x, y, r2, 0)
!
!          white out part of circle not specified by drawing
!             white polygon
!
      xpts (5) = r1 * cos (rad) + x
      xpts (1) = xpts (5)
      ypts (5) = r1 * sin (rad) + y
      ypts (1) = ypts (5)
!
      rad = (theta + 180.0) * pi / 180.0
!
      xpts (2) = r1 * cos (rad) + x
      ypts (2) = r1 * sin (rad) + y
!
!
      if (theta.eq.0.0) then
         xpts (3) = x - r1
         ypts (3) = y - r1
         xpts (4) = x + r1
         ypts (4) = y - r1
      endif
!
      if (theta.eq.90.0) then
         xpts (3) = x + r1
         ypts (3) = y - r1
         xpts (4) = x + r1
         ypts (4) = y + r1
      endif
!
      if (theta.eq.180.0) then
         xpts (3) = x + r1
         ypts (3) = y + r1
         xpts (4) = x - r1
         ypts (4) = y + r1
      endif
!
!
      if (theta.eq.270.0) then
         xpts (3) = x - r1
         ypts (3) = y + r1
         xpts (4) = x - r1
         ypts (4) = y - r1
      endif
!
      call colrplot_poly (iunit,xpts, ypts, 5, 1, istat)
      if (istat .ne. 0) then
        write(print_lun,*)' ERROR ENCOUNTERED IN COLOR_ARC ROUTINE WITH &
                          &COLRPLOT_POLY'
        write(print_lun,*)' X = ', x, ' Y = ', y, ' R1 = ', r1, ' R2 = ', &
                            r2, ' Theta = ', THETA
      end if
      return
      end subroutine color_arc
!
      subroutine color_arow (iunit,x1,y1,arrowh, arrowl, label, nenda, alpha, &
        istat)
! this routine plots and labels an arrow
!   x1     = the extreme left x-coordinate of the arrow
!   y1     = the extreme left y-coordinate of the arrow
!   arrowh = the height of the arrow
!   arrowl = the length of the arrow
!   label  = 3 character label of the arrow
!   nenda  = first arrow if 0 last arrow if 1
!   alpha  = angle to plot annotation
!   istat  = return status (0 = normal)
!
      integer,  intent(in) :: iunit
      real,                intent(in)  :: x1
      real,                intent(in)  :: y1
      real,                intent(in)  :: arrowh
      real,                intent(in)  :: arrowl
      character (len = *), intent(in)  :: label
      integer,             intent(in)  :: nenda
      real,                intent(in)  :: alpha
      integer,             intent(out) :: istat
!
      integer :: nchar, npts
      real :: xarrow (5), yarrow (5), perh, perw, percw, unitah, unital, &
        headl, warrow, hlabel, with, space, btwn, unitsp, xs, ys, xx
      character(len=2) :: lrrl,just

!
      call colrplot_fnt (iunit,19)
      if (alpha.eq.0.0) then
         lrrl = 'LR'
      else
         lrrl = 'RL'
      endif
      nchar=len_trim(label)
      npts = 5
      perh = 4.0
      perw = 8.0
      percw = 3.0
      unitah = arrowh
      unital = arrowl
      headl = unitah * perh
      warrow = unitah / perw
      hlabel = (unitah + (unitah / 2.5) )
      with = hlabel * 0.7
      space = hlabel * 0.9
      btwn = space-with
      unitsp = space
      if (nenda.eq.0) goto 50
!
!          first arrow
      xarrow (1) = x1
      xarrow (2) = x1
      xarrow (3) = x1 + unital + (warrow * percw) - headl
      xarrow (4) = x1 + unital - headl
      xarrow (5) = x1 + unital
      yarrow (1) = y1
      yarrow (5) = y1
      if (alpha.eq.0.0) then
         yarrow (2) = y1 + warrow
         yarrow (4) = y1 + unitah
         xs = xarrow (4) - (nchar + .5) * unitsp + btwn
      else
         yarrow (2) = y1 - warrow
         yarrow (4) = y1 - unitah
         xs = xarrow (4) - unitsp * .5
      endif
      yarrow (3) = yarrow (2)
      goto 100
   50 continue
!
!          last arrow
      xarrow (1) = x1
      xarrow (2) = x1 + headl
      xarrow (3) = xarrow (2) - (warrow * percw)
      xarrow (4) = x1 + unital
      xarrow (5) = xarrow (4)
      yarrow (1) = y1
      if (alpha.eq.0.0) then
         yarrow (2) = y1 + unitah
         yarrow (3) = y1 + warrow
      else
         yarrow (2) = y1 - unitah
         yarrow (3) = y1 - warrow
         xs = xarrow (2) + (nchar + .5) * unitsp - btwn
      endif
      yarrow (4) = yarrow (3)
      yarrow (5) = y1
  100 continue
! go plot the arrow
      call colrplot_poly (iunit,xarrow, yarrow, npts, 1, istat)
      if (istat .ne. 0) go to 500
      if (alpha.eq.0.0) then
         ys = y1 + hlabel / 2.0 + 2.0 * warrow
      else
         ys = y1 - (hlabel / 2.0 + 2.0 * warrow)
      endif
      if (nenda.eq.0) then
         if (alpha.eq.0.0) then
            xs = x1 + headl + .1
         else
            xx = x1 + headl + .1
            just = 'RJ'
            call color_just (xx, xs, nchar, 19, just, hlabel, lrrl)
         endif
      else
         if (alpha.eq.0.0) then
            xx = xarrow (5) - headl - .1
            just = 'RJ'
            call color_just (xx, xs, nchar, 19, just, hlabel, lrrl)
         else
            xs = xarrow (5) - headl - .1
         endif
      endif
      call colrplot_sym (iunit,xs, ys, hlabel, label, alpha, nchar)
!
  500 continue
      if (istat .ne. 0) then
       write(print_lun)' ERROR ENCOUNTERED IN COLOR_AROW ROUTINE WITH &
                       &COLRPLOT_POLY'
      end if
      return
!
      end subroutine color_arow
      subroutine color_blid (iunit,xo,yo,ftr,ltr,idbl,blcs,tunt,nt,theta)
!
!     plot block id
!
!     xo    = x origin of section
!     yo    = y origin of section
!     ftr   = first trace boundary
!     ltr   = last trace boundary
!     idbl  = id of the block
!     blcs  =  character size
!     tunt  = trace units
!     nt    = # of traces in section
!     theta = angle to plot text
!
      integer,  intent(in) :: iunit
        real,                intent(in) :: xo
        real,                intent(in) :: yo
        real,                intent(in) :: ftr
        real,                intent(in) :: ltr
        character (len = *), intent(in) :: idbl
        real,                intent(in) :: blcs
        real,                intent(in) :: tunt
        integer,             intent(in) :: nt
        real,                intent(in) :: theta
!
      integer :: nbkc
      real :: ftr1, ltr1, ylocl, yloch, wth, trlab, xloc, ds
      character iperod*1
!
      data iperod / '.' /
!
      call colrplot_fnt (iunit,19)
      ftr1 = ftr
      ltr1 = ltr
!
      if (theta.eq.0.0) then
         ylocl = yo - .2
         yloch = yo + .1
      else
         ylocl = yo + .2
         yloch = yo - .1
      endif
!
!          calculate line width based on blcs - with .3 being
!            100% blcs and .05 being 100% line width
!
      wth = blcs * .05 / .3
      if (wth.lt.0.025) wth = 0.025
      if (wth.gt.0.05) wth = .05
      call colrplot_lwid(iunit,wth)
!
!  check for block id left of last trace
      if (ftr1.le.0.) goto 100
      trlab = ftr1
      xloc = trlab * tunt + xo - tunt
      call colrplot_plot(iunit,xloc, ylocl, 3)
      call colrplot_plot(iunit,xloc, yloch, 2)
  100 continue
!
      if (ltr1.gt.nt) goto 200
      trlab = ltr1
      xloc = trlab * tunt + xo - tunt
      call colrplot_plot(iunit,xloc, ylocl, 3)
      call colrplot_plot(iunit,xloc, yloch, 2)
  200 continue
      if (idbl (1:1) .eq.iperod) goto 8000
      if (ftr1.le.0.) ftr1 = 1.
      if (ltr1.gt.nt) ltr1 = nt
      nbkc=len_trim(idbl)
      if (nbkc.eq.0) goto 8000
      xloc = tunt * (ftr1 + (ltr1 - ftr1) / 2.) + xo - tunt
      ds = blcs * (nbkc / 2.)
      if (theta.eq.0.0) then
         xloc = xloc - ds
      else
         xloc = xloc + ds
      endif
      if (theta.eq.0.0) then
         ylocl = ylocl - blcs
      else
         ylocl = ylocl + blcs
      endif
      call colrplot_sym (iunit,xloc, ylocl, blcs, idbl, theta, nbkc)
!
!
 8000 continue
      call colrplot_lwid(iunit,.005)
      return
      end subroutine color_blid
      subroutine color_brac (card, mcd, nca, nci)
!
!    routine to fix master card by removing ([])
!    from card
!    saves indexes of info inside brackets in nci and nca arrays
!     this info is plotted at a smaller character size
!
!     arguments
!      card = card array
!      mcd  = array to return fixed card
!      nca  = array to return # of char. per field
!      nci  = array to return # of char. in front of field
!
      character (len = 80),   intent(in)  :: card
      character (len = 80),   intent(out) :: mcd
      integer, dimension(20), intent(out) :: nca
      integer, dimension(20), intent(out) :: nci
!
      integer :: ib, i, nb, icol, nc, jj
!
      ib = 0
      mcd = card
      do 35 i = 1, 20
         nca(i) = 0
   35 end do
      ib = 0
      nb = 0
      icol = 0
      do 50 i = 1, 20
         ib = index (mcd(icol+1:), '[') + icol
         if (ib .eq. icol) goto 100
         icol = ib + 1
         nb = index (mcd(icol+1:), ']') + icol
         if (nb .eq. icol) goto 500
         nc = nb - ib - 1
         nca(i) = nc
         nci(i) = ib + 1
         do 45 jj = ib, nb
            mcd(jj:jj) = ' '
   45    end do
   50 end do
  100 continue
      return
  500 continue
      write(print_lun,*) 'COLR - ERROR IN TITLE BLOCK CARD WAS '
      write(print_lun,*) mcd
!
      return
      end subroutine color_brac
      subroutine color_brc1 (ia, card, ans)
!
!          gets out parameter between brackets.  ia is updated to 1st
!           character after right bracket
!
!          ia    = char to begin looking for left bracket
!          card  = input character array to search
!          ans   = information between brackets to return
!
      integer,              intent(inout) :: ia
      character (len = 80), intent(in)    :: card
      character (len = 10), intent(out)   :: ans
!
      integer :: k
      character l*1, r*1, cblk*10
!
      data l /'['/, r /']'/, cblk /'          '/
!
!
      k = 1
  100 if (card(ia:ia) .eq. l) goto 150
      ia = ia + 1
      goto 100
!
  150 ia = ia + 1
      ans = cblk
  170 ans(k:k) = card(ia:ia)
      k = k + 1
      ia = ia + 1
      if (card(ia:ia) .eq. r) goto 200
      goto 170
!
  200 ia = ia + 1
      return
      end subroutine color_brc1
      subroutine color_cabl (iunit,x, y, nci, nca, ht, ang, reduce, inp)
!
! *** routine to plot cable diagram
!
!     aurguments:
!      x   = x cordinate- bow of boat
!      y   = y coordinate - bow of boat
!      td  = total distance
!      nci = # of char. in front of field
!      nca = # of char. in field
!      ht  = character height
!      ang = angle to plot annotation
!    reduce= reduction factor
!     inp  = color input logical unit #
!
      integer,  intent(in) :: iunit
      real,                   intent(in)    :: x
      real,                   intent(in)    :: y
      integer, dimension(20), intent(inout) :: nci
      integer, dimension(20), intent(inout) :: nca
      real,                   intent(in)    :: ht
      real,                   intent(in)    :: ang
      real,                   intent(in)    :: reduce
      integer,                intent(in)    :: inp
!
      integer :: i, ia, icol, k, j, nc, istat
      real :: xb(9), yb(9), xp(4), yp(4), xfb, yfb, xan, yan, xcr1, xcr2, &
        ycr, xc1, yc1, yc2, yc3, xc4, xcc, ycc, ytmp, ch, chs, xc2, &
        xch, xc5, xend, xc3, ych, yc4, xc6, xsr, ysr1, ysr2, xso, obias, ycl
      character(len=80) :: inf,mcd
      character(len=16) :: icf
      character(len=10) :: idist,irec,cblk
      character(len=1)  :: carr(4),iblk,ich
!
      data cblk / '          ' /
!
      iblk = achar (0) ! had been => data iblk / "" /
      xfb = x
      read (inp, 9001) inf
      call color_brac (inf, mcd, nca, nci)
!
      yfb = y
!
! *** points for boat
!
      xb (1) = xfb
      yb (1) = yfb
      if (ang.eq.0.0) then
         xb (2) = xfb + .375 * reduce
      else
         xb (2) = xfb - .375 * reduce
      endif
      yb (2) = yfb
!    front top
      xb (3) = xb (2)
      if (ang.eq.0.0) then
         yb (3) = yfb + .08 * reduce
      else
         yb (3) = yfb - .08 * reduce
      endif
!    top of boat
      if (ang.eq.0.0) then
         xb (4) = xfb + .625 * reduce
      else
         xb (4) = xfb - .625 * reduce
      endif
      yb (4) = yb (3)
!    back of top
      xb (5) = xb (4)
      if (ang.eq.0.0) then
         yb (5) = yfb - .06 * reduce
      else
         yb (5) = yfb + .06 * reduce
      endif
!    back deck
      if (ang.eq.0.0) then
         xb (6) = xfb + 1.25 * reduce
      else
         xb (6) = xfb - 1.25 * reduce
      endif
      yb (6) = yb (5)
!    back of boat
      xb (7) = xb (6)
      if (ang.eq.0.0) then
         yb (7) = yfb - .19 * reduce
      else
         yb (7) = yfb + .19 * reduce
      endif
!    bottom of boat
      if (ang.eq.0.0) then
         xb (8) = xfb + .125 * reduce
      else
         xb (8) = xfb - .125 * reduce
      endif
      yb (8) = yb (7)
!    front of boat
      xb (9) = xb (1)
      yb (9) = yb (1)
!
      call colrplot_plot(iunit,xb (1), yb (1), 3)
      do 20 i = 2, 9
         call colrplot_plot(iunit,xb (i), yb (i), 2)
   20 end do
!
! *** antenna
!
      if (ang.eq.0.0) then
         xan = xfb + .375 * reduce
      else
         xan = xfb - .375 * reduce
      endif
      if (ang.eq.0.0) then
         yan = yfb + .43 * reduce
      else
         yan = yfb - .43 * reduce
      endif
      call colrplot_plot(iunit,xan, yb (3), 3)
      call colrplot_plot(iunit,xan, yan, 2)
      if (ang.eq.0.0) then
         xcr1 = xan - .02 * reduce
         xcr2 = xan + .02 * reduce
         ycr = yan - .02 * reduce
      else
         xcr1 = xan + .02 * reduce
         xcr2 = xan - .02 * reduce
         ycr = yan + .02 * reduce
      endif
      call colrplot_plot(iunit,xcr1, ycr, 3)
      call colrplot_plot(iunit,xcr2, ycr, 2)
      if (ang.eq.0.0) then
         xcr1 = xan - .035 * reduce
         xcr2 = xan + .035 * reduce
         ycr = yan - .05 * reduce
      else
         xcr1 = xan + .035 * reduce
         xcr2 = xan - .035 * reduce
         ycr = yan + .05 * reduce
      endif
      call colrplot_plot(iunit,xcr1, ycr, 3)
      call colrplot_plot(iunit,xcr2, ycr, 2)
!
! *** center lines
!
      if (ang.eq.0.0) then
         xc1 = xfb + 1.065 * reduce
         yc1 = yfb + .43 * reduce
         yc2 = yfb - .5 * reduce
         yc3 = yc1 - .065 * reduce
      else
         xc1 = xfb - 1.065 * reduce
         yc1 = yfb - .43 * reduce
         yc2 = yfb + .5 * reduce
         yc3 = yc1 + .065 * reduce
      endif
      xc4 = xc1
! *** source center line
      call colrplot_plot(iunit,xc1, yc1, 3)
      call colrplot_plot(iunit,xc1, yc2, 2)
      if (ang.eq.0.0) then
         ycl = yc2 - .05 * reduce
         xcc = xc1 - .05 * reduce
         ycc = ycl - .05 * reduce
         ytmp = ycl - ch
      else
         ycl = yc2 + .05 * reduce
         xcc = xc1 + .05 * reduce
         ycc = ycl + .05 * reduce
         ytmp = ycl + ch
      endif
      ch = .1 * reduce
      chs = .08 * reduce
      call colrplot_sym (iunit,xcc, ycc, ch, 'C', ang, 1)
      call colrplot_sym (iunit,xc1, ytmp, ch, 'L', ang, 1)
      if (ang.eq.0.0) then
         xc2 = xc1 + 1 * reduce
      else
         xc2 = xc1 - 1 * reduce
      endif
      call colrplot_plot(iunit,xc1, yc3, 3)
      call colrplot_plot(iunit,xc2, yc3, 2)
      icf=' '
      call colrplot_mvc (inf(1:1), nci(4), icf(1:1), 1, nca(4))
      if (ang.eq.0.0) then
         xch = xc2 + ch
      else
         xch = xc2 - ch
      endif
! *** field 4
      call colrplot_sym (iunit,xch, yc3, ch, icf, ang, nca(4))
      xp (1) = xc1
      yp (1) = yc3
      xp (4) = xc1
      yp (4) = yc3
      yp (2) = yc3
      if (ang.eq.0.0) then
         xp (2) = xc1 + .06 * reduce
         xp (3) = xc1 + .07 * reduce
         yp (3) = yc3 - .02 * reduce
      else
         xp (2) = xc1 - .06 * reduce
         xp (3) = xc1 - .07 * reduce
         yp (3) = yc3 + .02 * reduce
      endif
      call colrplot_poly (iunit,xp, yp, 4, 1, istat)
      if (istat .ne. 0) go to 5000
      xc5 = xc1
!
      if (ang.eq.0.0) then
         xc1 = xc1 + 3.065 * reduce
         xcc = xc1 - .05 * reduce
         ycc = ycl - .05 * reduce
      else
         xc1 = xc1 - 3.065 * reduce
         xcc = xc1 + .05 * reduce
         ycc = ycl + .05 * reduce
      endif
      xend = xc1
      xc3 = xc1
! *** end of cable
      call colrplot_plot(iunit,xc1, yc1, 3)
      call colrplot_plot(iunit,xc1, yc2, 2)
      call colrplot_sym (iunit,xcc, ycc, ch, 'C', ang, 1)
      call colrplot_sym (iunit,xc1, ytmp, ch, 'L', ang, 1)
      carr(1) = 'n'
      carr(2) = '0'
      carr(3) = '.'
      call colrplot_mvc (carr, 1, icf(1:1), 1, 3)
      call colrplot_mvc (inf(1:1), nci(8), icf(1:1), 4, nca(8))
      if (ang.eq.0.0) then
         xch = xc1 - 3 * ch
         ych = yc2 - 4 * ch
      else
         xch = xc1 + 3 * ch
         ych = yc2 + 4 * ch
      endif
! *** field 8
      call colrplot_sym (iunit,xch, ych, ch, icf, ang, nca (8) + 4)
      call colrplot_lwid(iunit,0.015)
      call colrplot_plot(iunit,xc1 - .19, yb (8), 3)
      call colrplot_plot(iunit,xc1 + .19, yb (8), 2)
      call colrplot_lwid(iunit,0.005)
      if (ang.eq.0.0) then
         xc2 = xc1 - 1. * reduce
      else
         xc2 = xc1 + 1. * reduce
      endif
      call colrplot_plot(iunit,xc2, yc3, 3)
      call colrplot_plot(iunit,xc1, yc3, 2)
      xp (1) = xc1
      yp (1) = yc3
      if (ang.eq.0.0) then
         xp (2) = xc1 - .06 * reduce
         yp (3) = yc3 - .02 * reduce
         xp (3) = xc1 - .07 * reduce
      else
         xp (2) = xc1 + .06 * reduce
         yp (3) = yc3 + .02 * reduce
         xp (3) = xc1 + .07 * reduce
      endif
      yp (2) = yc3
      xp (4) = xp (1)
      yp (4) = yp (1)
      call colrplot_poly (iunit,xp, yp, 4, 1, istat)
      if (istat .ne. 0) go to 5000
      if (ang.eq.0.0) then
         xc1 = xfb + 2. * reduce
         yc1 = yfb + .25 * reduce
         xcc = xc1 - .05 * reduce
         ycc = ycl - .05 * reduce
      else
         xc1 = xfb - 2. * reduce
         yc1 = yfb - .25 * reduce
         xcc = xc1 + .05 * reduce
         ycc = ycl + .05 * reduce
      endif
! *** start of cable
      call colrplot_plot(iunit,xc1, yc1, 3)
      call colrplot_plot(iunit,xc1, yc2, 2)
      call colrplot_sym (iunit,xcc, ycc, ch, 'C', ang, 1)
      call colrplot_sym (iunit,xc1, ytmp, ch, 'L', ang, 1)
      carr(1) = 'n'
      carr(2) = '0'
      carr(3) = '.'
      call colrplot_mvc (carr, 1, icf(1:1), 1, 3)
      call colrplot_mvc (inf(1:1), nci(6), icf(1:1), 4, nca(6))
      if (ang.eq.0.0) then
         xch = xc1 - 3 * ch
         ych = yc2 - 4 * ch
      else
         xch = xc1 + 3 * ch
         ych = yc2 + 4 * ch
      endif
! *** field 6
      call colrplot_sym (iunit,xch, ych, ch, icf, ang, nca(6)+4)
      call colrplot_lwid(iunit,0.015)
      call colrplot_plot(iunit,xc1 - .19, yb (8), 3)
      call colrplot_plot(iunit,xc1 + .19, yb (8), 2)
      call colrplot_lwid(iunit,0.005)
      if (ang.eq.0.0) then
         xc2 = xan + .09 * reduce
         yc3 = yc1 - .065 * reduce
      else
         xc2 = xan - .09 * reduce
         yc3 = yc1 + .065 * reduce
      endif
! *** antenna to source
      call colrplot_plot(iunit,xan, yc3, 3)
      call colrplot_plot(iunit,xc2, yc3, 2)
      icf=' '
      call colrplot_mvc (inf(1:1), nci(1), icf(1:1), 1, nca(1))
      if (ang.eq.0.0) then
         xch = xc2 + chs / 2.
      else
         xch = xc2 - chs / 2.
      endif
! *** field 1
      call colrplot_sym (iunit,xch, yc3, chs, icf, ang, nca(1))
      xp (1) = xan
      yp (1) = yc3
      if (ang.eq.0.0) then
         xp (2) = xan + .06 * reduce
      else
         xp (2) = xan - .06 * reduce
      endif
      yp (2) = yc3
      if (ang.eq.0.0) then
         yp (3) = yc3 - .02 * reduce
         xp (3) = xan + .07 * reduce
      else
         xp (3) = xan - .07 * reduce
         yp (3) = yc3 + .02 * reduce
      endif
      xp (4) = xp (1)
      yp (4) = yp (1)
      call colrplot_poly (iunit,xp, yp, 4, 1, istat)
      if (istat .ne. 0) go to 5000
      if (ang.eq.0.0) then
         xc2 = xc5 - .09 * reduce
      else
         xc2 = xc5 + .09 * reduce
      endif
      call colrplot_plot(iunit,xc2, yc3, 3)
      call colrplot_plot(iunit,xc5, yc3, 2)
      xp (1) = xc5
      yp (1) = yc3
      if (ang.eq.0.0) then
         xp (2) = xc5 - .06 * reduce
         xp (3) = xc5 - .07 * reduce
         yp (3) = yc3 - .02 * reduce
      else
         xp (2) = xc5 + .06 * reduce
         xp (3) = xc5 + .07 * reduce
         yp (3) = yc3 + .02 * reduce
      endif
      yp (2) = yc3
      xp (4) = xp (1)
      yp (4) = yp (1)
      call colrplot_poly (iunit,xp, yp, 4, 1, istat)
      if (istat .ne. 0) go to 5000
!
      if (ang.eq.0.0) then
         xc2 = xc4 + .25 * reduce
         yc4 = yc3 - .15 * reduce
      else
         xc2 = xc4 - .25 * reduce
         yc4 = yc3 + .15 * reduce
      endif
! *** source to cable
      call colrplot_plot(iunit,xc4, yc4, 3)
      call colrplot_plot(iunit,xc2, yc4, 2)
      icf=' '
      call colrplot_mvc (inf(1:1), nci(2), icf(1:1), 1, nca(2))
      xch = xc2 + chs / 2.
! *** field 2
      call colrplot_sym (iunit,xch, yc4, chs, icf, ang, nca(2))
      xp (1) = xc4
      yp (1) = yc4
      if (ang.eq.0.0) then
         xp (2) = xc4 + .06 * reduce
         xp (3) = xc4 + .07 * reduce
         yp (3) = yc4 - .02 * reduce
      else
         xp (2) = xc4 - .06 * reduce
         xp (3) = xc4 - .07 * reduce
         yp (3) = yc4 + .02 * reduce
      endif
      yp (2) = yc4
      xp (4) = xp (1)
      yp (4) = yp (1)
      call colrplot_poly (iunit,xp, yp, 4, 1, istat)
      if (istat .ne. 0) go to 5000
      if (ang.eq.0.0) then
         xc2 = xc1 - .25 * reduce
      else
         xc2 = xc1 + .25 * reduce
      endif
      call colrplot_plot(iunit,xc2, yc4, 3)
      call colrplot_plot(iunit,xc1, yc4, 2)
      xp (1) = xc1
      yp (1) = yc4
      if (ang.eq.0.0) then
         xp (2) = xc1 - .06 * reduce
         xp (3) = xc1 - .07 * reduce
         yp (3) = yc4 - .02 * reduce
      else
         xp (2) = xc1 + .06 * reduce
         xp (3) = xc1 + .07 * reduce
         yp (3) = yc4 + .02 * reduce
      endif
      yp (2) = yc4
      xp (4) = xp (1)
      yp (4) = yp (1)
      call colrplot_poly (iunit,xp, yp, 4, 1, istat)
      if (istat .ne. 0) go to 5000
      if (ang.eq.0.0) then
         xc2 = xc1 + .5 * reduce
      else
         xc2 = xc1 - .5 * reduce
      endif
      call colrplot_plot(iunit,xc1, yc3, 3)
      call colrplot_plot(iunit,xc2, yc3, 2)
! *** field 5
      icf=' '
      call colrplot_mvc (inf(1:1), nci(5), icf(1:1), 1, nca(5))
      if (ang.eq.0.0) then
         xch = xc2 + ch
      else
         xch = xc2 - ch
      endif
      call colrplot_sym (iunit,xch, yc3, ch, icf, ang, nca(5))
      xp (1) = xc1
      yp (1) = yc3
      if (ang.eq.0.0) then
         xp (2) = xc1 + .06 * reduce
         xp (3) = xc1 + .07 * reduce
         yp (3) = yc3 - .02 * reduce
      else
         xp (2) = xc1 - .06 * reduce
         xp (3) = xc1 - .07 * reduce
         yp (3) = yc3 + .02 * reduce
      endif
      yp (2) = yc3
      xp (4) = xp (1)
      yp (4) = yp (1)
      call colrplot_poly (iunit,xp, yp, 4, 1, istat)
      if (istat .ne. 0) go to 5000
      xc6 = xc1
      if (ang.eq.0.0) then
         xc1 = xc3 - .5 * reduce
      else
         xc1 = xc3 + .5 * reduce
      endif
      call colrplot_plot(iunit,xc1, yc3, 3)
      call colrplot_plot(iunit,xc3, yc3, 2)
      xp (1) = xc3
      yp (1) = yc3
      if (ang.eq.0.0) then
         xp (2) = xc3 - .06 * reduce
         xp (3) = xc3 - .07 * reduce
         yp (3) = yc3 - .02 * reduce
      else
         xp (2) = xc3 + .06 * reduce
         xp (3) = xc3 + .07 * reduce
         yp (3) = yc3 + .02 * reduce
      endif
      yp (2) = yc3
      xp (4) = xp (1)
      yp (4) = yp (1)
      call colrplot_poly (iunit,xp, yp, 4, 1, istat)
      if (istat .ne. 0) go to 5000
      if (ang.eq.0.0) then
         xc1 = xfb + 2.625 * reduce
         xc2 = xc6 + .09 * reduce
      else
         xc1 = xfb - 2.625 * reduce
         xc2 = xc6 - .09 * reduce
      endif
      call colrplot_plot(iunit,xc6, yc4, 3)
      call colrplot_plot(iunit,xc2, yc4, 2)
      icf=' '
      call colrplot_mvc (inf(1:1), nci(3), icf(1:1), 1, nca(3))
      if (ang.eq.0.0) then
         xch = xc2 + chs
      else
         xch = xc2 - chs
      endif
! *** field 3
      call colrplot_sym (iunit,xch, yc4, chs, icf, ang, nca (3) )
      xp (1) = xc6
      yp (1) = yc4
      if (ang.eq.0.0) then
         xp (2) = xc6 + .06 * reduce
         xp (3) = xc6 + .07 * reduce
         yp (3) = yc4 - .02 * reduce
      else
         xp (2) = xc6 - .06 * reduce
         xp (3) = xc6 - .07 * reduce
         yp (3) = yc4 + .02 * reduce
      endif
      yp (2) = yc4
      xp (4) = xp (1)
      yp (4) = yp (1)
      call colrplot_poly (iunit,xp, yp, 4, 1, istat)
      if (istat .ne. 0) go to 5000
      if (ang.eq.0.0) then
         xc2 = xc1 - .09 * reduce
      else
         xc2 = xc1 + .09 * reduce
      endif
      call colrplot_plot(iunit,xc2, yc4, 3)
      call colrplot_plot(iunit,xc1, yc4, 2)
      xp (1) = xc1
      yp (1) = yc4
      if (ang.eq.0.0) then
         xp (2) = xc1 - .06 * reduce
         xp (3) = xc1 - .07 * reduce
         yp (3) = yc4 - .02 * reduce
      else
         xp (2) = xc1 + .06 * reduce
         xp (3) = xc1 + .07 * reduce
         yp (3) = yc4 + .02 * reduce
      endif
      yp (2) = yc4
      xp (4) = xp (1)
      yp (4) = yp (1)
      call colrplot_poly (iunit,xp, yp, 4, 1, istat)
      if (istat .ne. 0) go to 5000
!
      yc1 = yb (3)
! *** center line from start to next receiver
      call colrplot_plot(iunit,xc1, yc1, 3)
      call colrplot_plot(iunit,xc1, yc2, 2)
      if (ang.eq.0.0) then
         xcc = xc1 - .05 * reduce
         ycc = ycl - .05 * reduce
      else
         xcc = xc1 + .05 * reduce
         ycc = ycl + .05 * reduce
      endif
      call colrplot_sym (iunit,xcc, ycc, ch, 'C', ang, 1)
      call colrplot_sym (iunit,xc1, ytmp, ch, 'L', ang, 1)
      carr(1) = 'n'
      carr(2) = '0'
      carr(3) = '.'
      call colrplot_mvc (carr, 1, icf(1:1), 1, 3)
      call colrplot_mvc (inf(1:1), nci(7), icf(1:1), 4, nca(7))
      if (ang.eq.0.0) then
         xch = xc1 - 3 * ch
         ych = yc2 - 4 * ch
      else
         xch = xc1 + 3 * ch
         ych = yc2 + 4 * ch
      endif
! *** field 7
      call colrplot_sym (iunit,xch, ych, ch, icf, ang, nca(7)+4)
      call colrplot_lwid(iunit,0.015)
      call colrplot_plot(iunit,xc1 - .19, yb (8), 3)
      call colrplot_plot(iunit,xc1 + .19, yb (8), 2)
      call colrplot_lwid(iunit,0.005)
!
!          test for continuation card
      ich = iblk
      call colrplot_mvc (inf(1:1), 80, ich, 1, 1)
      if (ich.eq.iblk) goto 100
!
      read (inp, 9001, end = 10000) inf
10000 idist = cblk
      irec = cblk
      ia = index (inf, ']')
      call colrplot_mvc (inf(1:1), 2, idist(1:1), 1, ia-1)
      icol = ia
      k = index (inf (icol + 1:) , '[') + icol
      icol = k
      k = k + 1
      j = index (inf (icol + 1:) , ']') + icol
      nc = j - k
      call colrplot_mvc (inf(1:1), k, irec(1:1), 1, nc)
!
      if (idist.eq.cblk.and.irec.eq.cblk) goto 100
!
!          center line from end to next receiver
      xc1 = xfb + 3.5
      call colrplot_plot(iunit,xc1, yc1, 3)
      call colrplot_plot(iunit,xc1, yc2, 2)
      if (ang.eq.0.0) then
         xcc = xc1 - .05 * reduce
         ycc = ycl - .05 * reduce
      else
         xcc = xc1 + .05 * reduce
         ycc = ycl + .05 * reduce
      endif
      call colrplot_sym (iunit,xcc, ycc, ch, 'C', ang, 1)
      call colrplot_sym (iunit,xc1, ytmp, ch, 'L', ang, 1)
      carr(1) = 'n'
      carr(2) = '0'
      carr(3) = '.'
      icf= ' '
      irec=adjustl(irec)
      call colrplot_mvc (irec(1:1), 1, icf(1:1), 5, 5)
      if (ang.eq.0.0) then
         xch = xc1 - 3 * ch
      else
         xch = xc1 + 3 * ch
      endif
!
!          receiver label
      call colrplot_sym (iunit,xch, ych, ch, icf, ang, 6)
      call colrplot_plot(iunit,xc1 - .19, yb (8), 3)
      call colrplot_plot(iunit,xc1 + .19, yb (8), 2)
!
!          label between end and next receiver
!             left arrow
      if (ang.eq.0.0) then
         xc2 = xc1 + .09 * reduce
      else
         xc2 = xc1 - .09 * reduce
      endif
      call colrplot_plot(iunit,xc1, yc4, 3)
      call colrplot_plot(iunit,yc2, yc4, 2)
      xp (1) = xc1
      yp (1) = yc4
      if (ang.eq.0.0) then
         xp (2) = xc1 + .06 * reduce
         xp (3) = xc1 + .07 * reduce
         yp (3) = yc4 - .02 * reduce
      else
         xp (2) = xc1 - .06 * reduce
         xp (3) = xc1 - .07 * reduce
         yp (3) = yc4 + .02 * reduce
      endif
      yp (2) = yc4
      xp (4) = xp (1)
      yp (4) = yp (1)
      call colrplot_poly (iunit,xp, yp, 4, 1, istat)
      if (istat .ne. 0) go to 5000
!
!            label
      if (ang.eq.0.0) then
         xch = xc2 + chs
      else
         xch = xc2 - chs
      endif
      call colrplot_sym (iunit,xch, yc4, chs, idist, ang, 4)
!
!            right arow
      if (ang.eq.0.0) then
         xc2 = xend-.09 * reduce
      else
         xc2 = xend+.09 * reduce
      endif
      call colrplot_plot(iunit,xc2, yc4, 3)
      call colrplot_plot(iunit,xend, ycr, 2)
      xp (1) = xend
      yp (1) = yc4
      if (ang.eq.0.0) then
         xp (2) = xend-.05 * reduce
         xp (3) = xend-.07 * reduce
         yp (3) = yc4 - .02 * reduce
      else
         xp (2) = xend+.05 * reduce
         xp (3) = xend+.07 * reduce
         yp (3) = yc4 + .02 * reduce
      endif
      yp (2) = yc4
      xp (4) = yp (1)
      yp (4) = yp (1)
      call colrplot_poly (iunit,xp, yp, 4, 1, istat)
      if (istat .ne. 0) go to 5000
!
! *** source
!
  100 continue
      if (ang.eq.0.0) then
         xsr = xfb + .94 * reduce
         ysr2 = yfb - .38 * reduce
      else
         xsr = xfb - .94 * reduce
         ysr2 = yfb + .38 * reduce
      endif
      ysr1 = yb (6)
      call colrplot_plot(iunit,xsr, ysr1, 3)
      call colrplot_plot(iunit,xsr, ysr2, 2)
!
      if (ang.eq.0.0) then
         xso = xsr - .03 * reduce
      else
         xso = xsr + .03 * reduce
      endif
      if (ang.eq.0.0) then
         obias = 0.0
      else
         obias = 0.063
      endif
      call colrplot_sym (iunit,xso, ysr2 + obias, .063, 'O', ang, 1)
      if (ang.eq.0.0) then
         xsr = xfb + 1.19 * reduce
         xso = xsr - .03 * reduce
      else
         xsr = xfb - 1.19 * reduce
         xso = xsr + .03 * reduce
      endif
      ysr1 = yb (7)
      call colrplot_plot(iunit,xsr, ysr1, 3)
      call colrplot_plot(iunit,xsr, ysr2, 2)
      call colrplot_sym (iunit,xso, ysr2 + obias, .063, 'O', ang, 1)
!
 9001 format(a)
!
      return
 5000 continue
      write(print_lun) ' ERROR ENCOUNTERED IN COLOR_CABL ROUTINE WITH &
                       &COLRPLOT_POLY'
      write(print_lun) ' X = ', x, ' Y = ', y, ' NCI = ', nci, ' NCA = ', nca, &
      ' HT = ', ht, ' ANG = ', ang
      return
      end subroutine color_cabl
      subroutine color_ckh (trchedr_arg,filhedr,incdec,fhedr,match,bias_arg, &
        exact)
!
!
!            compares header of trace to header of other
!             data to determine if other header is close enough
!             match to the trace header
!
!
!         trchedr = trace header
!         filhedr = header to compare to trace header
!         incdec  = hollerith flag to indicate if header values are
!                   incrementing or decrementing
!                       1 = incrementing
!                      -1 = decrementing
!         fhedr   = header of the first trace
!         match   = integer value returned to indicate if this is an
!                   appropriate match.
!                      0   = does not match
!                      1   = matches
!                     -1   = skip this file entry and read next
!         bias    = bias to use in determining equality
!         exact   = flag to indigate exact match is required
!                   true or false (optional)
!
      double precision,  intent(in)  :: trchedr_arg
      real,              intent(in)  :: filhedr
      character(len=*),  intent(in)  :: incdec
      real,              intent(in)  :: fhedr
      integer,           intent(out) :: match
      real,optional,     intent(in)  :: bias_arg
      logical, optional, intent(in)  :: exact
!
      real :: bias,tbias,trchedr
      logical :: exact_tmp,almost_equal
!
      trchedr=trchedr_arg
      if (present(bias_arg))then
        bias=bias_arg
      else
        bias=0.0
      endif
      if (present(exact)) then
        exact_tmp = exact
      else
        exact_tmp = .false.
      endif
      tbias = 0.1
      if (bias .gt. 0) tbias = bias
!
      match = 0
      almost_equal = ameq(trchedr, filhedr, bias)
      if (almost_equal) then
         match = 1
         go to 100
      endif
!
      if (incdec .eq. 'DEC') goto 3660
      if (filhedr .lt. fhedr) then
         match = - 1
         go to 100
      endif
!
      if (trchedr .lt. filhedr) go to 100
      if (trchedr .gt. filhedr) then
         if (exact_tmp) then
            match = - 1
            go to 100
         endif
         write(print_lun) ' FILE HEADER ', filhedr, ' NOT FOUND. HEADER ',     &
         trchedr, ' USED.'
         match = 1
         go to 100
      endif
!
 3660 continue
!               decrementing headers
      if (filhedr .gt. fhedr) then
         match = - 1
         go to 100
      endif
!
      if (trchedr .gt. filhedr) go to 100
      if (trchedr .lt. filhedr)write(print_lun,*)' FILE HEADER ', filhedr,&
                               ' NOT found.  header ', TRCHEDR, ' used.'
      match = 1
 100  return
      end subroutine color_ckh
      subroutine color_dubi (iunit,yc, xc, ang, istat)
!
!     istat  = return status 0 is normal return
!
      integer,  intent(in) :: iunit
      real,    intent(in)  :: yc
      real,    intent(in)  :: xc
      real,    intent(in)  :: ang
      integer, intent(out) :: istat
!
      integer :: inp, i, npts, knt, junk
      real :: x(15), y(15), size, xx, a1, yy, r1, r2
      character inpf*8
!
      data inpf /'COLORDUB'/
!
!      cmd = 'assign -a /u/poepsn03e/goodger/keep/dublogo.dat COLORDUB'
!      call putsys_cmd (cmd, istat)
!      write(print_lun,*) ' COLOR - STATUS ACCESSING DUBAI LOGO FILE = ', istat
!
!      if (istat .ne. 0) go to 200
!
!          set color to red
      call colrplot_tclr (iunit,6)
      size = .037
!          draw right and left curve of capsule
      if (ang .eq. 0.0) then
         xx = 11.0 * size+xc
         a1 = 90.0
         yy = 11.0 * size+yc
      else
         xx = xc - 11.0 * size
         a1 = 270.0
         yy = yc - 11.0 * size
      endif
      r1 = 10. * size
      r2 = 8. * size
!
      call color_arc (iunit,xx, yy, r1, r2, a1, istat)
      if (istat .ne. 0) go to 5001
!
      if (ang.eq.0.0) then
         xx = 54.0 * size+xc
         a1 = 270.0
      else
         xx = xc - 54.0 * size
         a1 = 90.0
      endif
      call color_arc (iunit,xx, yy, r1, r2, a1, istat)
      if (istat .ne. 0) go to 5001
!
      call colrplot_tclr (iunit,6)
!
      call getlun (inp, istat)
      if (istat .ne. 0) then
        write(print_lun,*)' COLOR - ERROR GETTING LUN FOR DUBAI LOGO FILE = ',&
                            istat
        go to 200
      end if
!
      open (unit=inp, file=inpf, iostat=istat, status='old', action='read')
      if (istat .ne. 0) then
        write(print_lun,*) ' COLOR - ERROR OPENING DUBAI LOGO FILE = ', istat
        go to 200
      end if
!
   40 read (inp, 9002, end = 100) npts
      knt = knt + 1
      do 50 i = 1, npts
         read (inp, 9001, end = 100) x (i), y (i)
         knt = knt + 1
         if (ang.eq.0.0) then
            x (i) = x (i) * size+xc
            y (i) = y (i) * size+yc
         else
            x (i) = xc - x (i) * size
            y (i) = yc - y (i) * size
         endif
   50 end do
      call colrplot_poly (iunit,x, y, npts, 1, istat)
      if (istat .ne. 0) go to 5000
      goto 40
!
  100 continue
!
!          set color back to black
      call colrplot_tclr (iunit,1)
!
      junk = 0
      call colrplot_mvc ('DN', 1, junk, 1, 2)
      return
 5000 continue
      write(print_lun,*)' ERROR ENCOUNTERED IN COLOR_DUBI ROUTINE WITH &
                        &COLRPLOT_POLY'
      write(print_lun,*) ' YC = ', yc, ' XC = ', xc, ' ANG = ', ang
      return
 5001 continue
      write(print_lun,*)' ERROR ENCOUNTERED IN COLOR_DUBI WITH COLOR_ARC'
      write(print_lun,*)' COLOR_ARC ARGS - XX = ', xx, ' YY = ', yy,&
                        ' R1 = ', r1,' R2 = ', r2
      
 200  return
!
 9001 format (bz,2f10.0)
 9002 format (bz,i10)
!
      end subroutine color_dubi
      subroutine color_english
      if(survey_units.eq.'METERS')then
        object%ips_eng=object%ips*.3937008
        object%tpi_eng=object%tpi*cti
        object%cs_sid_eng=object%cs_sid/cti
        object%cs_scale_eng=object%cs_scale/cti 
        object%cs_lab_eng=object%cs_lab/cti 
        object%wid_vl_eng=object%wid_vl/cti 
        object%ar_ht_eng=object%ar_ht/cti 
        object%ar_len_eng=object%ar_len/cti 
        object%cs_cbl_eng=object%cs_cbl/cti 
        object%cs_pl_eng=object%cs_pl/cti 
        object%cs_tl_eng=object%cs_tl/cti 
        object%cs_bl_eng=object%cs_bl/cti 
        object%cs_lt_eng=object%cs_lt/cti 
        object%cs_spl_eng=object%cs_spl/cti 
        object%tpi_ovl_eng=object%tpi_ovl*cti 
        object%scda_eng=object%scda*cti 
        object%sdas_eng=object%sdas/cti 
        object%tida_eng=object%tida/cti 
        object%arda_eng=object%arda/cti

      else

        object%ips_eng=object%ips
        object%tpi_eng=object%tpi
        object%cs_sid_eng=object%cs_sid
        object%cs_scale_eng=object%cs_scale 
        object%cs_lab_eng=object%cs_lab 
        object%wid_vl_eng=object%wid_vl 
        object%ar_ht_eng=object%ar_ht 
        object%ar_len_eng=object%ar_len 
        object%cs_cbl_eng=object%cs_cbl 
        object%cs_pl_eng=object%cs_pl 
        object%cs_tl_eng=object%cs_tl 
        object%cs_bl_eng=object%cs_bl 
        object%cs_lt_eng=object%cs_lt 
        object%cs_spl_eng=object%cs_spl 
        object%tpi_ovl_eng=object%tpi_ovl 
        object%scda_eng=object%scda
        object%sdas_eng=object%sdas 
        object%tida_eng=object%tida 
        object%arda_eng=object%arda

      endif 
      end subroutine color_english
      subroutine color_fkpl (iunit,yc, dly, ht, ang, inp)
!
!        inp= color input logical unit #
!
      integer,  intent(in) :: iunit
      real,    intent(inout) :: yc
      real,    intent(in)    :: dly
      real,    intent(in)    :: ht
      real,    intent(in)    :: ang
      integer, intent(in)    :: inp
!
      integer :: ia, istat, ixft, i
      real :: xcor(4), ycor(4), siz1, siz2, units, f, xft, fmax, x1, y1, y2, &
        ytmp, x0, x2, y, yinc, xinc, x
      character card*80, cxft*10, cfmax*10, cy*10, cx*10, tfsz*20, cblk*10
!
      data tfsz / 'TRANSFORM SIZE =    ' / , cblk / '          ' /
!
      siz1 = .09
      siz2 = .08
!
      units = 1.0
      if (ht.lt.0.1) then
         f = ht / .1
         units = units * f
      endif
!
!
      read (inp, 9001, end = 800) card
!          get coordinates for scale
      ia = 1
      call color_brc1 (ia, card, cxft)
      call string_cc2ff(cxft,xft,istat)
      if (istat .ne. 1) go to 7081
!
      call color_brc1 (ia, card, cfmax)
      call string_cc2ff(cfmax,fmax,istat)
      if (istat .ne. 1)go to 7080
!
!          allow 2 inches for vertical scale
!                3 inches for horizontal scale
      x1 = 3.3 * units
      y1 = yc
      if (ang.eq.0.0) then
         y2 = yc - 2.0 * units
      else
         y2 = yc + 2.0 * units
      endif
!
!          vertical scale
      cfmax (5:6) = 'HZ'
      if (ang.eq.0.0) then
         ytmp = y1 + .01
      else
         ytmp = y1 - .01
      endif
      call colrplot_sym (iunit,x1, ytmp, 0.1, cfmax, ang, 6)
      call colrplot_lwid(iunit,0.01)
      call colrplot_plot(iunit,x1, y1, 3)
      call colrplot_plot(iunit,x1, y2, 2)
      call colrplot_lwid(iunit,0.005)
!
!          horizontal scale
      ixft = xft / 2.0 + .0001
      x1 = 1.8 * units
      if (ang.eq.0.0) then
         x0 = x1 + 1.5 * units
         x2 = x1 + 3.0 * units
      else
         x0 = x1 - 1.5 * units
         x2 = x1 - 3.0 * units
      endif
      call colrplot_plot(iunit,x1, y2, 3)
      call colrplot_plot(iunit,x2, y2, 2)
!
      if (ang.eq.0.0) then
         x1 = x1 - .8 * units
         y = y2 - .03 * units
      else
         x1 = x1 + .8 * units
         y = y2 + .03 * units
      endif
      call colrplot_sym (iunit,x1, y, siz1, '-.5 CY/TR', ang, 9)
      call colrplot_sym (iunit,x1, y, siz1, '-.5 CY/TR', ang, 9)
      call colrplot_sym (iunit,x0, y, siz1, '.5 CY/TR', ang, 8)
      call colrplot_sym (iunit,x0, y, siz1, '0', ang, 1)
!
!          determine units to the inch
      yinc = 1.0 / (fmax / 2.0)
      xinc = 1.0 / (ixft / 1.5)
!
!          0,0 of fk scale is x0,y2
!
!          read and plot coordinates
  400 read (inp, 9001, end = 800) card
      if (card (1:9) .eq.'  ENDPLOT') goto 800
      ia = 1
      do 500 i = 1, 4
         if (i.eq.0) goto 8000
         call color_brc1 (ia, card, cy)
         if (cy.eq.cblk) goto 400
         call string_cc2ff(cy,y,istat)
         if (istat .ne. 1) go to 7082
         if (ang.eq.0.0) then
            ycor (i) = y2 + y * yinc * units
         else
            ycor (i) = y2 - y * yinc * units
         endif
!
         call color_brc1 (ia, card, cx)
         call string_cc2ff(cx,x,istat)
         if (istat .ne. 1) go to 7083
         if (ang.eq.0.0) then
            xcor (i) = x0 + x * xinc * units
         else
            xcor (i) = x0 - x * xinc * units
         endif
  500 end do
      call colrplot_poly (iunit,xcor, ycor, 4, 1, istat)
      if (istat .ne. 0) go to 8001
      goto 400
!
  800 yc = y2 - dly
      tfsz (17:20) = cxft (1:4)
      if (ang.eq.0.0) then
         x1 = x1 + 1.4 * units
      else
         x1 = x1 - 1.4 * units
      endif
      call colrplot_sym (iunit,x1, yc, siz2, tfsz, ang, 20)
      if (ang.eq.0.0) then
         yc = yc - dly * 1.5
      else
         yc = yc + dly * 1.5
      endif
      go to 8002
!
 7080 write(print_lun,*) ' COLR - CFMAX = ', cfmax, ' FMAX = ', fmax
      goto 8000
 7081 write(print_lun,*) ' COLR - CXFT = ', cxft, ' XFT = ', xft
      goto 8000
 7082 write(print_lun,*) ' COLR - CY = ', cy, ' Y = ', y
      goto 8000
 7083 write(print_lun,*) ' COLR - CX = ', cx, ' X = ', x
 8000 write(print_lun,*) ' COLOR_FKPL - ERROR IN TCON'
      go to 8002
 8001 write(print_lun,*) ' ERROR ENCOUNTERED IN COLOR_FKPL WITH COLRPLOT_POLY'
 8002 return
!
 9001 format(a)
!
      end subroutine color_fkpl
      subroutine color_f2char (fnum, inum, idec, jusflg, ipldec)
!
! where:
! name    type*  valid  description         *type: i=in, o=out, b=both
! ----    ----   -----  -----------
!
! fnum     i          = the floating point number given
! inum     o          = the character string returned
! idec     i          = the number of places to keep after the decimal
!                       point
! jusflg   i          = flag to left or right justify
!                        1 = left justify
!                        0 = right justify
! ipldec   i          = flag to indicate if decimal point plotted when
!                       idec = 0
!                        1 = plot
!                        0 = do not plot
!
      real,                intent(in)  :: fnum
      character (len = *), intent(out) :: inum
      integer,             intent(in)  :: idec
      integer,             intent(in)  :: jusflg
      integer, optional,   intent(in)  :: ipldec
!
      integer :: nlen, nc, nxt, ipldec_tmp
      character fm1*8, ctmp*8 
      character(len=8) :: kdec
!
      fm1= '(F     )'
!
      nlen = len (inum)
      call string_ii2cc(nlen,ctmp)
      nc=len_trim(ctmp)
      fm1(3:3+nc-1)=ctmp(1:nc)
      nxt = nc + 3
      if (present(ipldec))then
        ipldec_tmp = ipldec
      else
        ipldec_tmp = 1
      endif
      if (idec .gt. 8) then
         write(print_lun) ' COLOR_F2CHAR - IDEC MUST BE 8 OR LESS'
      endif
!
!          create the format statement for conversion
!
      call string_ii2cc(idec,kdec)
      fm1(nxt:nxt)='.'
      nxt = nxt + 1
      fm1(nxt:nxt)=kdec(1:1)
      write (inum, fm1) fnum
      inum=adjustr(inum)
      if (idec .eq. 0 .and. ipldec_tmp .eq. 0) then
         inum (nlen:nlen) = ' '
         inum=adjustr(inum)
      endif
      if (jusflg .eq. 1)then
         inum=adjustl(inum)
      endif
!
      return
      end subroutine color_f2char
      subroutine color_just (xold, xnew, nchar, ifnt, just, size, lrrl)
!
!
!          calculate the x-coordinate of the left-most character
!            of a text string in order to center or right justify
!            the text string at the given x-coordinate for use with
!            conplot symbol routine
!
!          xold    = given x-coordinate
!          xnew    = calculated x-coordinate of left-most character
!          nchar   = the number of characters in the text string
!          ifnt    = the conplot font being used to determine spacing
!                    0  = 100% spacing
!                    19 = 85% spacing
!          just    = right or center justify
!                    'RJ' = right justify
!                    'CJ' = center justify
!          size    = character size
!          lrrl    = left to right or right to left plot
!                    'LR' = left to right
!                    'RL' = right to left
!
      real,                intent(in)  :: xold
      real,                intent(out) :: xnew
      integer,             intent(in)  :: nchar
      integer,             intent(in)  :: ifnt
      character (len = *), intent(in)  :: just
      real,                intent(in)  :: size
      character (len = *), intent(in)  :: lrrl
!
      integer :: nc
      real :: pcnt
!
      pcnt = 1
      if (ifnt .ne. 0) pcnt = 0.85
      nc = nchar
      if (just(:2) .eq. 'CJ') nc = nchar / 2
!          justify
      if (lrrl(:2) .eq. 'LR') then
         xnew = xold - real(nc) * size * pcnt
      else
         xnew = xold + real(nc) * size * pcnt
      endif
!
      return
      end subroutine color_just
      subroutine color_gram (nbx, nval, histv, trace, tmp, tmpe)
!
!*** routine to compute histogram
!
!          nbx   = the number of percentage boxes to use less 1
!          nval  = the number of sample points in a trace
!          histv = the largest absolute value of the data
!          trace = input trace buffer
!          tmp   = histogram of negative values
!          tmpe  = histogram of positive values
!
      integer,            intent(in)  :: nbx
      integer,            intent(in)  :: nval
      real,               intent(in)  :: histv
      real, dimension(*), intent(in)  :: trace
      real, dimension(*), intent(inout) :: tmp
      real, dimension(*), intent(inout) :: tmpe
!
      integer :: i, j
!
!*** l00p to sum min and max into boxs
!
      do 225 i = 1, nval
         if (trace (i) .gt.0.) goto 210
         if (trace (i) .eq.0.) goto 225
         j = (nbx * (abs (trace (i) ) ) / histv) + 1.
         tmp (j) = tmp (j) + 1.
         goto 225
  210    continue
         j = (nbx * (trace (i) ) / histv) + 1
         tmpe (j) = tmpe (j) + 1
  225 end do
      return
      end subroutine color_gram
      subroutine color_grdv (trace, tmp, nvi, ndo, nsamp)
!
!          grades from sample to sample to plotter resolution between
!            100 and 200
!
!
!          trace  =  input-output array
!          tmp    =  work array
!          nvi    =  original number of values
!          ndo    =  number of values to interpolate between original
!                     values
!          nsamp  =  number of values after interpolation
!
      integer,            intent(in)    :: nvi
      integer,            intent(in)    :: ndo
      integer,            intent(in)    :: nsamp
      real, dimension(*), intent(inout) :: trace
      real, dimension(*), intent(out)   :: tmp
!
      integer :: n, i, j
      real :: dif, fact, addon
!
      n = 0
      dif = trace(nvi) - trace(nvi-1)
      trace(nvi+1) = trace(nvi) + dif
      fact = 1.0 / (real(ndo) + 1.0)
!
!
      do 100 i = 1, nvi
         n = n + 1
         tmp(n) = trace(i)
         dif = trace(i+1) - trace(i)
         addon = dif * fact
         do 80 j = 1, ndo
            n = n + 1
            tmp(n) = tmp(n-1) + addon
   80    end do
  100 end do

      do i=1,nsamp
        trace(i)=tmp(i)
      enddo
!
      return
      end subroutine color_grdv
      subroutine color_hilo (ppc, pnc, pplc, vmin, vmax, histv, nbox, tmp,&
      tmpe)
!
!          routine to determine minimum & maximum values
!
!          ppc   = percent of positive range
!          pnc   = percent of negative range
!          pplc  = percent of positive range on low end if doing
!                  positive only
!          vmin  = minimum value to use for color scale - returned
!          vmax  = maximum value to use for color scale - returned
!          histv = the largest absolute value of the data
!          nbox  = the number of percentage boxes used (100 or 1000)
!          tmp   = array holding histogram of negative values
!          tmpe  = array holding histogram of positive values
!
      real,               intent(in)  :: ppc
      real,               intent(in)  :: pnc
      real,               intent(in)  :: pplc
      real,               intent(out) :: vmin
      real,               intent(out) :: vmax
      real,               intent(in)     :: histv
      integer,            intent(in)     :: nbox
      real, dimension(*), intent(inout)  :: tmp
      real, dimension(*), intent(inout)  :: tmpe
!
      integer :: i1, i2, k, i, j
      real :: ampl, amph, rangn, rangx, clow, chigh
      logical :: pnczero, ppczero, negval, posval, pplczro, pplcneg
!
      logical :: almost_equal
!
      pnczero = .false.
      ppczero = .false.
      pplczro = .false.
      pplcneg = .false.
      negval = .true.
      posval = .true.
      almost_equal = ameq (pnc, 0.0, .0001)
      if (almost_equal) pnczero = .true.
      almost_equal = ameq (ppc, 0.0, .0001)
      if (almost_equal) ppczero = .true.
      almost_equal = ameq (pplc, 0.0, .0001)
      if (almost_equal) pplczro = .true.
      almost_equal = ameq (pplc, - 1.0, .0001)
      if (almost_equal) pplcneg = .true.
!
!
!
!          get print of boxes before sum
      write(print_lun) ' NEGATIVE'
      i1 = 1
      i2 = 15
  472 write(print_lun,9001) i1, (tmp(k), k = i1, i2)
      i1 = i1 + 15
      i2 = i2 + 15
      if (i2.le.nbox) goto 472
!
      write(print_lun) ' POSITIVE'
      i1 = 1
      i2 = 15
  475 write(print_lun,9001) i1, (tmpe(k), k = i1, i2)
      i1 = i1 + 15
      i2 = i2 + 15
      if (i2 .le. nbox) goto 475
!
!
!*** sum all boxs
!
      do 490 i = 1, nbox - 1
         tmp(i+1) = tmp(i+1) + tmp(i)
         tmpe (i+1) = tmpe(i+1) + tmpe(i)
  490 end do
      almost_equal = ameq (tmp(nbox), 0.0, .0001)
      if (almost_equal) then
         write(print_lun) ' COLR-HILO=> There are no negative values'
         negval = .false.
      endif
      almost_equal = ameq (tmpe(nbox), 0.0, .0001)
      if (almost_equal) then
         write(print_lun) ' COLR-HILO=> There are no positive values'
         posval = .false.
      endif
!
!*** find low and high cut
!
      ampl = pnc * tmp(nbox)
!cc   if (pplc .ne. -1.) ampl=pplc*tmpe(nbox)
      if (.not.pplcneg) ampl = pplc * tmpe(nbox)
      amph = ppc * tmpe(nbox)
!
!
!
!
      do 520 i = 1, nbox - 1
         j = i
!cc     if (pplc .ne. -1.) go to 500
         if (.not.pplcneg) goto 500
         if (tmp(nbox) .ne.0.) goto 510
  500    continue
         if (tmpe(i) .gt. ampl) goto 530
         goto 520
  510    continue
         if (tmp(i) .ge. ampl) goto 530
  520 end do
!
!
  530 continue
      k = j
!
!
      do 540 i = 1, nbox
         j = i
         if (tmpe(i) .ge. amph) goto 550
  540 end do
!
  550 continue
      rangn = histv / nbox
      rangx = histv / nbox
      clow = rangn * (k - 1) + (rangn / 2.)
      almost_equal = ameq (tmp(nbox), 0.0, .0001)
!            9-30-91
      if ((.not.almost_equal) .and. pplcneg) then
         clow = - clow
      endif
      chigh = rangx * (j - 1) + (rangx / 2.)
      if (chigh .gt. 0.0 .and. (.not.posval .or. ppczero)) chigh = 0.0
!
      if (.not.pplcneg) goto 560
      if (pnc .eq. 0.) clow = 0.
      goto 570
  560 continue
      if (pplczro) clow = 0.
!
!
  570 continue
      vmin = clow
      vmax = chigh
!
      if ( (.not.pplcneg) .or. (ppc .ne. pnc) ) goto 575
      if (.not.pplcneg) goto 575
      if (vmin .ge. 0) goto 575
      clow = abs (vmin)
      chigh = abs (vmax)
      if (clow.gt.chigh) goto 572
      vmin = - vmax
      goto 575
  572 vmax = - vmin
!
  575 write(print_lun) ' '
      write(print_lun) ' NEGATIVE--CUMULATED'
      i1 = 1
      i2 = 10
  580 write(print_lun,9002) i1, (tmp(k), k = i1, i2)
      i1 = i1 + 10
      i2 = i2 + 10
      if (i2 .le. nbox) goto 580
!
      write(print_lun) ' POSITIVE--CUMULATED'
      i1 = 1
      i2 = 10
  590 write(print_lun,9002) i1, (tmpe(k), k = i1, i2)
      i1 = i1 + 10
      i2 = i2 + 10
      if (i2 .le. nbox) goto 590
!
      return
!
 9001 format(i4,3x,15f8.0)
 9002 format(i4,3x,10(f9.0,3x))
      end subroutine color_hilo
      subroutine color_inks (obj,xs,tinch)
!
!         plots the ink scale and annotation above ink scale
!
!         xs    = x-end of section if rl plot
!                 x-start of section if lr plot
!                   after being adjusted for side labels
!         tinch = total inches in data
!
      type(color_struct),intent(inout) :: obj       ! arguments
      real,                  intent(in)  :: xs
      real,                  intent(in)  :: tinch
!
      integer :: nlab, lencnum, ncolrp, j, nc1, nc, nc2, nc3, nc4, &
        i, idec, maxnc, ii, kd, m, jusflg
      real :: theta, x, xsize, ysize, asiz, hitemax, tsize, y, yinc, siz, &
        xx, yy, f, hite, xtic, ticlen, a
      character cnum*16, lr*2, iblk*1

      logical :: almost_equal
!
!
      data lr /'LR'/
      save nlab
!
      iblk = achar (0)
      lencnum = len (cnum)
      call colrplot_lwid(obj%iunit,.005)
      ncolrp = obj%ncolr / (obj%irepeat)
      nlab = ncolrp - 1
      theta = 180.0
      if (obj%lrrl(:2) .eq. lr(:2)) theta = 0.0
!          determine coordinates for ink scale
      if (obj%lrrl(:2) .eq. lr(:2)) then
         x = xs - 4.0
      else
         x = xs + 4.0
      endif
      xsize = .5 * obj%frcb
      ysize = .25 * obj%frcb
      asiz = 0.08 * obj%frcb
      j = ncolrp
      hitemax = 40.0
      if (obj%lab_cb(1) .ne. ' ') hitemax = 38.0
      if (ncolrp .le. 16) ysize = .5
      if (ncolrp .le.  8) ysize = 1.0
      if (ncolrp .le.  4) ysize = 2.0
 6300 tsize = real(j) * ysize + .7
      if (tsize .le. hitemax) goto 6400
      ysize = ysize - 0.05
      goto 6300
 6400 if (obj%lrrl(:2) .eq.lr(:2)) goto 6500
      y = obj%fit + 2.0
      if ((tsize+y) .gt. hitemax) y = 40.0 - hitemax - 0.1
      goto 6530
 6500 y = tinch - 2.0 - ysize
      if (y .lt. tsize) y = tsize
!
 6530 continue
!
!!      if (obj%lab_cb(1) .eq. ' ') goto 6600
!          plot labels above color bar
      yinc = 0.4
      nc1=len_trim(obj%lab_cb(1))
      nc = nc1
      siz = obj%cs_cbl_eng(1)
      nc2=len_trim(obj%lab_cb(2))
      if (nc2 .gt. nc) nc = nc2
      if (obj%cs_cbl_eng(2) .gt. siz) siz = obj%cs_cbl_eng(2)
      nc3=len_trim(obj%lab_cb(3))
      if (nc3 .gt. nc) nc = nc3
      if (obj%cs_cbl_eng(3) .gt. siz) siz = obj%cs_cbl_eng(3)
      nc4=len_trim(obj%lab_cb(4))
      if (nc4 .gt. nc) nc = nc4
      if (obj%cs_cbl_eng(4) .gt. siz) siz = obj%cs_cbl_eng(4)

      if (obj%lrrl(:2) .eq. lr(:2)) then
         xx = x
         yy = y + ysize + 1.68
         yinc = -yinc
      else
         xx = x + xsize
         yy = y - 1.68
      endif
      call colrplot_fnt (obj%iunit,0)
      if(obj%lab_cb(1) .ne. ' ')then
        call colrplot_sym(obj%iunit,xx,yy,obj%cs_cbl_eng(1),obj%lab_cb(1),&
                          theta,nc1)
      endif
      yy = yy + yinc
!
      if(obj%lab_cb(2) .ne. ' ')then
         call colrplot_sym (obj%iunit,xx,yy,obj%cs_cbl_eng(2),obj%lab_cb(2),&
                            theta,nc2)
      endif
      yy = yy + yinc
!
      if (obj%lab_cb(3) .ne. ' ')then
         call colrplot_sym (obj%iunit,xx,yy,obj%cs_cbl_eng(3),obj%lab_cb(3),&
                            theta,nc3)
      endif
      yy = yy + yinc
!
      if (obj%lab_cb(4) .ne. ' ')then
         call colrplot_sym (obj%iunit,xx,yy,obj%cs_cbl_eng(4),obj%lab_cb(4),&
                            theta,nc4)
      endif
!
!          generate ink scale
      yy = y
      f = ysize
      if (obj%lrrl(:2) .eq. lr(:2)) f = -ysize
      do i = obj%irepeat, obj%ncolr, obj%irepeat
         call colrplot_rect (obj%iunit,x, yy, x + xsize, yy + ysize, 0, &
                             obj%kolors (i))
         yy = yy + f
      end do
!
!          set color back to black
      call colrplot_tclr (obj%iunit,1)
      call colrplot_tflg (obj%iunit,1)
!
!          determine # of decimal points to plot on ink scale label
      idec = 0
      if (obj%vlinc .lt. 10.0) idec = 3
      maxnc = lencnum
      almost_equal= ameq (obj%vlinc, 0.0, .0001)
      if (almost_equal) then
         idec = 0
!cc     maxnc=0 ! why?
         ii = obj%irepeat
         do 7300 i = 1, nlab
            kd = 0
            call string_ff2cc(obj%vals(ii),cnum,ndec=idec)
            ii = ii + obj%irepeat
            j = index (cnum, ".")
            if (j .eq. 0) goto 7297
            j = j + 1
            do 7295 m = j, j+2
               if (cnum(m:m) .ne. ' ' .and. cnum(m:m) .ne. '0') kd = kd+1
 7295       end do
            idec = max (idec, kd)
 7297       continue
! 7297     maxnc=max(maxnc,nc)  !why?
 7300    end do
      endif
      if (idec .gt. 3) idec = 3
!
!          tic marks and annotation for ink scale
      hite = 0.1
!
      if (obj%lrrl(:2) .eq. lr(:2)) then
         yy = y + 0.005
         xtic = x
         ticlen = -0.2
         almost_equal = ameq (obj%frcb, 1.0, .0001)
         if (almost_equal) then
            a = 0.2
         else
            a = 0.3
         endif
         x = x - .1 - maxnc * asiz - a * obj%frcb
      else
         yy = y - .005 + ysize
         xtic = x + xsize
         ticlen = 0.2
         x = x + xsize+maxnc * asiz + 0.3 * obj%frcb
!cc     left justify
!cc     jusflg=1
      endif
!       right justify
      jusflg = 0
      call colrplot_fnt (obj%iunit,0)
      ii = obj%irepeat
      do i = 1, nlab
         call colrplot_plot(obj%iunit,xtic, yy, 3)
         call colrplot_plot(obj%iunit,xtic + ticlen, yy, 2)
         call color_f2char(obj%vals(ii),cnum,idec,jusflg,0)
         ii = ii + obj%irepeat
         nc=len_trim(cnum)
         call colrplot_sym (obj%iunit,x, yy, asiz, cnum, theta, nc)
         if (obj%lrrl(:2) .eq. lr) then
            yy = yy - ysize
         else
            yy = yy + ysize
         endif
      end do
!
      return
      end subroutine color_inks
      subroutine color_logo (iunit,x, y, sz, ang)
!
!          plot the conoco logo
!
!        x   = x coordinate of center of logo
!        y   = y coordinate of center of logo
!       sz   = length of logo in inches
!       ang  = angle to plot annotation
!
      integer,  intent(in) :: iunit
      real, intent(in) :: x
      real, intent(in) :: y
      real, intent(in) :: sz
      real, intent(in) :: ang
!
      integer :: is, ie, kbr, i, jbr, js, je, j, k, n, istat
      real :: rc(32), ro(28), xp(11), an(5), px(5), py(5), anrl(5), anlr(5), &
        scn, xsf, csa, ssa, xg, yg, r1, r2, cl, xl, yl, xl2, yl2, xx, yy
      character (len=2) :: lr,irl,lrrl
!
      data scn /43./ , lr /'LR'/ , irl /'RL'/
      data rc / 0., 0., 2.5, 0., 2.5, 1.5, 0., 1.5, 0., 0., 1.5, 0.,    &
      1.5, 2.5, 0., 2.5, 0., 0., 1., 0., 1., 1.5, 0., 1.5, 0., 0., 30.5,&
      0., 30.5, 1.5, 0., 1.5 /
      data ro / - 15., 1., - 15., - 2.5, 9., 1., 9., - 2.5, - 5.5,      &
      - 2.5, - 2., - 2.5, - 9.5, 1.0, - 9.5, - 2.5, 2.5, 1.0, 2.5,      &
      - 2.5, 14.5, 1.0, 14.5, - 2.5, - 15., 4.75, - 15., - 6.25 /
      data anlr / 90., 0., 90., 90., 270. /
      data anrl / 270.0, 180.0, 270.0, 270.0, 90.0 /
      data xp / - 15., - 3.0, 9.0, - 15., 15.5, - 9.5, - 8.5, 2.5, 3.5, &
      14.5, 15.5 /
!
      if (ang.eq.0.0) then
         lrrl = lr
      else
         lrrl = irl
      endif
      xsf = (sz / scn)
      csa = 1.0
      ssa = 0.0
!
      xg = x
      yg = y
      if (lrrl .eq. irl) then
         an=anrl
      else
         an=anlr
      endif
!
      goto 11
!
!          arcs making up c'S & N IN CONOCO
    8 continue
      r1 = 2.5 * xsf
      r2 = 1.0 * xsf
      is = 1
      ie = 3
      kbr = 2
!
    9 do i = is, ie
         cl = xp (i) * xsf
         if (lrrl.eq.irl) cl = - cl
         xl = xg + cl * csa
         yl = yg + cl * ssa
         call color_arc (iunit,xl, yl, r1, r2, an(i), istat)
      enddo
      if (istat .ne. 0) go to 5001
!
      if (kbr .eq. 1) then
        go to 8
      else if (kbr .eq. 2) then
        go to 15
      end if
!
!          arcs making up capsule
   11 continue
      r1 = 6.25 * xsf
      r2 = 4.75 * xsf
      is = 4
      ie = 5
      kbr = 1
      goto 9
!
!cc        set color to red
!cc   call colrplot_tclr(6)
!
!          arcs making up o'S IN CONOCO
   15 continue
      is = 6
      ie = 11
!
      do 17 i = is, ie, 2
         cl = xp (i) * xsf
         if (lrrl .eq. irl) cl = - cl
         xl = xg + cl * csa
         yl = yg + cl * ssa
!
         call colrplot_tclr (iunit,6)
         call colrplot_circ (iunit,xl, yl, r1, 0)
!
         px (1) = xl
         px (2) = xl
         px (5) = xl
         py (1) = yl + r1
         py (5) = py(1)
         py (2) = yl - r1
!
         cl = xp(i+1) * xsf
         if (lrrl .eq. irl) cl = -cl
         xl2 = xg + cl * csa
         yl2 = yg + cl * ssa
!
         call colrplot_circ (iunit,xl2, yl2, r1, 0)
         px (3) = xl2
         px (4) = xl2
         py (3) = yl2 - r1
         py (4) = yl2 + r1
         call colrplot_tclr (iunit,9)
         call colrplot_circ (iunit,xl, yl, r2, 0)
         call colrplot_circ (iunit,xl2, yl2, r2, 0)
         call colrplot_poly (iunit,px, py, 5, 1, istat)
         if (istat .ne. 0) go to 5000
   17 end do
      jbr = 1
      is = 1
      js = 1
      je = 8
!
   20 do 25 j = js, je, 2
!
         do 22 k = 1, 4
            n = 2 * (k - 1)
            xx = (ro(j) + rc(is+n)) * xsf
            yy = (ro(j+1) + rc(is+n+1)) * xsf
            if (lrrl .eq. lr) then
               px(k) = xg + xx * csa - yy * ssa
               py(k) = yg + yy * csa + xx * ssa
            else
               px(k) = xg - xx * csa - yy * ssa
               py(k) = yg - yy * csa + xx * ssa
            endif
   22    end do
         px(5) = px(1)
         py(5) = py(1)
!
         call colrplot_tclr (iunit,6)
         call colrplot_poly (iunit,px, py, 5, 1, istat)
         if (istat .ne. 0) go to 5000
   25 end do
!
      is = is + 8
      if (jbr .eq. 1) then
        go to 31
      else if (jbr .eq. 2) then
        go to 32
      else if (jbr .eq. 3) then
        go to 33
      else if (jbr .eq. 4) then
        go to 34
      end if
!
   31 js = 9
      je = 12
      jbr = 2
      goto 20
!
   32 js = 13
      je = 24
      jbr = 3
      goto 20
!
   33 js = 25
      je = 28
      jbr = 4
      goto 20
!
   34 continue
!          set color back to black
      call colrplot_tclr (iunit,1)
      go to 6000
 5000 continue
      write(print_lun,*)' ERROR ENCOUNTERED IN COLOR_LOGO ROUTINE WITH &
                        &COLRPLOT_POLY'
      go to 6000
 5001 continue
      write(print_lun,*)' ERROR ENCOUNTERED IN COLOR_LOGO WITH COLOR_ARC'
      write(print_lun,*)' COLOR_ARC ARGS - XL = ', xl, ' YL = ', yl,&
                        ' R1 = ', r1,&
                        ' R2 = ', r2, ' ANG = ', an(i) , ' I = ', i
 6000 return
      end subroutine color_logo 
      subroutine color_med (ipnstro, big, fmed, ipdf, nwih,      &
        nsamp, istat)
!
!     purpose:
!     calculate median amplitude. so that the data may be divided by it,
!          forcing 1 to be the median.
!
!     method:
!     read strot file , bin, determine the median.
!     this subroutine uses
!     the method developed by c.i. burch in cyber program ctds.
!
!     parameters:
!     ipnstro = process number of the strot file
!     big     = lav from all traces being plotted
!     fmed    = returned -- scaling factor
!     ipdf    = integer buffer for 5001 bins
!     nwih    = number of words in header
!     nsamp   = number of trace samples
!     istat   = return status (0 = normal return)
!----------------------------------------------------------------------
!
      type(trcio_struct),pointer :: ipnstro
      real,                  intent(in)  :: big
      real,                  intent(out) :: fmed
      integer, dimension(*), intent(out) :: ipdf
      integer,               intent(in)  :: nwih
      integer,               intent(in)  :: nsamp
      integer,               intent(out) :: istat
!
      integer, parameter :: npc = 8
!
      integer :: numbin, i, ibin, k, num, nfind, itot
      real :: ipcnt(npc), amp(npc), fctr

      double precision :: header(nwih)

      real :: trbuf(nsamp)
!
      data numbin /5001/
!
      itot = 0
      fctr = (real(numbin) - 1.0) / big
      do i = 1, numbin
        ipdf (i) = 0
      enddo

!
!          rewind the trace file
      istat=trcio_seek_trace(ipnstro,1)
!       read a trace from the strot file
 20   continue
      istat=trcio_read_trace(ipnstro,header,trbuf)
      if(istat.eq.TRCIO_EOF)go to 80
      if (istat .ne. TRCIO_OK) go to 9501
!     fill pdf bins
      do 60 i = 1, nsamp
         if (trbuf(i) .ne. 0.) then
            ibin = abs (trbuf(i) ) * fctr + 1.0
            ipdf(ibin) = ipdf(ibin) + 1
            itot = itot + 1
         endif
   60 end do
      goto 20
!
!     end of input data
   80 ipcnt(1) = 50
      ipcnt(2) = 60
      ipcnt(3) = 70
      ipcnt(4) = 80
      ipcnt(5) = 90
      ipcnt(6) = 95
      ipcnt(7) = 98
      ipcnt(8) = 99
!
      k = 1
      num = 0
      nfind = real(itot) * real(ipcnt (1)) * 0.01
      do ibin = 1, numbin
         num = num + ipdf(ibin)
  110    continue
         if (num .ge. nfind) then
            amp(k) = (real(ibin) - 0.5) / fctr
            if (k .ge. npc)exit
            k = k + 1
            nfind = real(itot) * real(ipcnt(k)) * 0.01
            goto 110
         endif
      enddo
!
      fmed = amp(1)
      write(print_lun,1020) fmed
 1020 format (5x,'INPUT DATA MEDIAN WAS ',g14.6)
      fmed = 1.0 / fmed
!
      istat=trcio_seek_trace(ipnstro,1)
!
 9501 continue
      if (istat .ne. TRCIO_OK) then
        write(print_lun) ' ERROR READING STROT FILE IN ROUTINE COLOR_MED'
      end if
      return
      end subroutine color_med
      subroutine color_plbl(iunit,NPLBL,lab,row,col,btskp,&
                            btdo,xp, yp,&
                            PLCS,TWID,YINC,ALPHA,NTRPH)

        integer,         intent(in)   :: nplbl,ntrph,iunit
        character(len=32),intent(in)  :: lab(:)
        integer,         intent(in)   :: row(:),col(:),btskp(:),btdo(:)
        real,            intent(in)   :: xp,yp,plcs,twid,yinc,alpha


!        NPLBL = Number of labels
!        lab   = Array containing the labels, each label is 32 characters
!        row   = array containing row number for corresponding label
!        col   = array containing col number for corresponding label
!        btskp = number of traces to skip before blanks
!        btdo  = number of blanks to insert
!        xp    = X-origin 
!        yp    = Y-origin                                  
!        PLCS  = pointer - panel label characer size in inches   
!        TWID  = width of 1 trace in inches
!        YINC  = pointer - Y increment in inches
!        ALPHA = pointer - angle to plot annotation
!        NTRPH = Number of traces in the horzintal interpolation in 1 TWID


      real    :: x   ,xinc,xo,y,yo,panwid  
      integer ::  i    ,nc  
  
      panwid=btskp(1)*twid
      nc=nplbl*32
      xinc=(btskp(1)+btdo(1))*twid
      if(NTRPH > 1)then
        xinc=xinc-(twid/(NTRPH))*(NTRPH-1)
        panwid=panwid-(twid/(NTRPH))*(NTRPH-1)
      endif
      if(ALPHA > 0.001)then !    /* right to left */
        xo=xp-.2
        yo=yp+.2
      else !                      /* left to right */
        xo=xp+.2         
        yo=yp-.2
      endif

      DO i=1,nplbl
        if(ALPHA > 0.000001) then !             /* right to left */
          x=(xo+panwid)+(col(i)-1)*xinc-twid                     
          y=yo+(row(i)-1)*yinc+(PLCS+0.1)
        else !                                  /* left to right */
          x=xo+(col(i)-1)*xinc+twid
          y=yo-(row(i)-1)*yinc-(PLCS+0.1)
        endif
        if(lab(1) == '.')cycle
        nc=len_trim(lab(i))
        call colrplot_sym(iunit,x,y,PLCS,lab(i),ALPHA,nc)
      ENDDO

      end subroutine color_plbl
      subroutine color_pixl(obj,trace,xo,yo,shite,twid,numtr,kodes,yend,incr,&
                            ntpp,istat)
!
!
!          plots a seismic trace as a column of colors using pixplt
!
!
!            trace  = input trace array
!            xo     = x-origin of section (trace 1 , sample 1)
!            yo     = y-origin of section (trace 1 , sample 1)
!            shite  = height of a sample point
!            twid   = width of one trace
!            numtr  = the sequential trace number being plotted
!            kodes  = array containing a color code for each sample
!            nchar = number of characters needed for kpack array
!            yend   = y-coordinate of bottom of section
!            incr   = the increment to use for the first comparison in
!                     a non-linear grade
!            ntpp   = number of traces in one column of panels
!           istat   = return status (0 = normal return)
!
      type(color_struct) :: obj       ! arguments

      real,                dimension(*), intent(in)  :: trace
      real,                              intent(in)  :: xo
      real,                              intent(in)  :: yo
      real,                              intent(in)  :: shite
      real,                              intent(in)  :: twid
      integer,                           intent(in)  :: numtr
      integer,             dimension(*), intent(out) :: kodes
      real,                              intent(in)  :: yend
      integer,                           intent(in)  :: incr
      integer,                           intent(in)  :: ntpp
      integer,                           intent(out) :: istat
!
      integer :: kntcol, k, kndx, i1, i2, i3, iii, ndx, j, istrt, i
      real :: x1, x2, val
      character no*2, iyes*3, lr*2      
      logical :: almost_equal

!!      character(len=1) :: kpack(obj%nsamp)
        integer :: kpack(obj%nsamp)
!
      data no /'NO'/ , iyes /'YES'/ , lr /'LR'/
!
      istat = 0
      if (obj%knttr .eq. 1) kntcol = 1
      if (obj%nrow .gt. 1) then
         k = mod (obj%knttr, ntpp)
         if (k .eq. 0) then
            kntcol = kntcol + 1
         endif
      endif
      kndx = 1
!!      if ((obj%lrrl(:2) .eq. 'LR' .and. obj%invert(:2) .eq. no  ) .or. &
!!          (obj%lrrl(:2) .eq. 'RL' .and. obj%invert(:3) .eq. iyes)       ) then
         i1 = 1
         i2 = obj%nsamp
         i3 = 1
!!      else
!!         i1 = obj%nsamp
!!         i2 = 1
!!         i3 = - 1
!!      endif
      x1 = xo + (numtr - 1) * twid
!          need to adjust x-coordinate if conplot interpolation
      x2 = x1 + twid + .0001
      DO iii = i1, i2, i3
         val = trace(iii)
         if (obj%vlinc .gt. 0.0) then
            ndx = (val - obj%vals(1)) / obj%vlinc + 2.00001
            if (ndx .gt. 1) then
               if (ndx .gt. obj%ncolr) ndx = obj%ncolr
               almost_equal=ameq(obj%vals(ndx-1), val, .0001)
               if (almost_equal) ndx = ndx - 1
            endif
         else
!          find starting point to begin comparisons
!            decreases num of if checks
!
            do j = 1, obj%ncolr, incr
               if (val .le. obj%vals(j))exit
            enddo
            istrt = j - incr
            if (istrt .le. 0) istrt = 1
!
            do ndx = istrt, obj%ncolr
               if (val .le. obj%vals(ndx) )exit
            enddo
         endif
         if (ndx .lt. 1    ) ndx = 1
         if (ndx .gt. obj%ncolr) ndx = obj%ncolr
         kodes(kndx) = obj%kolors(ndx)
         kndx = kndx + 1
      ENDDO
!
!          if paneling - make area between panels white
      if (obj%nrow .gt. 1) then
         i1 = obj%nvi * obj%ntrpv + 1
         do i = 1, obj%nrow - 1
            do j = 1, obj%nsbtwn * obj%ntrpv
               kodes (i1) = 9
               i1 = i1 + 1
            enddo
            i1 = i1 + obj%nvi * obj%ntrpv
         enddo
!        if on last column - make any dead panels white
                                                    ! nrow is total
         if (kntcol.ge.obj%ncol.and.obj%nrlc.lt.obj%nrow) then
            if (obj%lrrl(:2) .eq. lr) then
                                                    ! number of rows
               i1 = (obj%nvi * obj%ntrpv + obj%nsbtwn * obj%ntrpv) * obj%nrlc
               i2 = obj%nsamp
               i3 = 1
            else
               i1 = 1
               i2 = (obj%nvi * obj%ntrpv + obj%nsbtwn * obj%ntrpv) * &
                    (obj%nrow - obj%nrlc)
               i3 = 1
            endif
            do i = i1, i2, i3
               kodes (i) = 9
            enddo
         endif
      endif
!
!     move the kodes integer array into the kpack byte array
      call colrplot_movew2b (kodes, kpack, obj%nsamp)
      call colrplot_pix (obj%iunit,x1,yo,x2,yend,1,obj%nsamp,kpack,obj%interp)
!
      return
      end subroutine color_pixl
      subroutine color_revpol(aray,num)

      real,intent(inout) :: aray(:)
      integer,intent(in) :: num

      integer :: i
      do i=1,num
        aray(i)=aray(i)*(-1.0)
      enddo
      end subroutine color_revpol
      subroutine color_rvse(buf,num)

!          Reverse a trace in the same buffer

      real,intent(inout) :: buf(:)
      integer,intent(in) :: num

      real :: temp,temp2

      integer :: i,j

      temp2=buf(num)
      j=num

      DO i=1,num/2
        temp=buf(i)
        buf(i)=temp2
        buf(j)=temp
        j=j-1
        temp2=buf(j)
      ENDDO

      end subroutine color_rvse
      subroutine color_sbwa(a,b,num)


      integer :: a(:),b(:),num

!  "S"ort array "b" "w"ith array "a"

!     This function uses the insertion sort algorithm to sort array a
!     Array b is maintained parallel to array a 

      integer :: i,j,key,keyb


      do j=2,num
        key  = a(j)
        keyb = b(j)
        i  = j-1
        do
          if(i > 0  .and.   a(i) > key)then 
            a(i + 1)  = a(i)
            b(i + 1)  = b(i)
            i = i - 1
          else
            exit
          endif
        enddo
        a(i+1) = key
        b(i+1) = keyb
      enddo                           
      end subroutine color_sbwa
      subroutine color_scl (iunit,x1, y1, dis, csize, tunit, mtrc, basint, &
        alpha, nt)
!
!           this routine plots a scale on the section
!
!   x1     =  the x coordinate of the section
!   y1     =  the y coordinate of the top of the section
!   dis    =  the distance above the section
!   csize  =  size of the characters
!   tunit  =  units per trace
!    mtrc  =  yes indicates units are metric
!  basint  =  the basement interval between 2 plotted traces
!   alpha  =  angle to plot annotation
!   nt     =  number of traces in section
!
      integer,  intent(in) :: iunit
      real,                intent(in) :: x1
      real,                intent(in) :: y1
      real,                intent(in) :: dis
      real,                intent(in) :: csize
      real,                intent(in) :: tunit
      character (len = *), intent(in) :: mtrc
      real,                intent(in) :: basint
      real,                intent(in) :: alpha
      integer,             intent(in) :: nt
!
      integer :: numch
      real :: tscale, fmid, xl1, xl2, sclmid, fcmid, xs1, width, width1, yl1, &
        ys1, with, thick, beta, gama, mir
      character iyes*3, labsca*16

      logical :: lr
!
!
      data iyes /'YES'/
!
      lr = ameq (alpha, 0.0, 0.00001)
      if (mtrc(:3) .eq. iyes) then
         tscale = 1000.0 / basint
         labsca = 'ONE KILOMETER   '
         numch = 13
      else
         tscale = 5280.0 / basint
         LABSCA = 'ONE MILE        '
         numch = 8
      endif
!
      fmid = real (nt) / 2.0
      xl1 = (fmid-tscale * .5) * tunit + x1
      xl2 = xl1 + tscale * tunit
      if (xl1.lt.x1) then
        write(print_lun,*)'COLOR_SCL  => ERROR --- CALCULATED SCLE IS LARGER &
                          &THAN THE WIDTH OF THE PLOT'
         write(print_lun,*) '           BASEMENT INTERVAL = ', basint
         if (mtrc(:3) .eq. iyes) then
           write(print_lun,*) '         NUMBER OF TRACES PER METER = ', tscale
         else
           write(print_lun,*) '         NUMBER OF TRACES PER FOOT  = ', tscale
         endif
         write(print_lun,*) '           NO SCLE WILL BE DONE'
         return
      endif
      sclmid = (xl2 - xl1) * .5 + xl1
!
!          center label on scale
      fcmid = real (numch) / 2.0
!cc   xs1=sclmid-(fcmid*csize)+csize
      if (alpha .ne. 0.0) xs1 = sclmid + (fcmid * csize) - csize
      width = .025
      width1 = .0125
      if (lr) then
         yl1 = y1 + dis
         ys1 = yl1 + csize
      else
         yl1 = y1 - dis
         ys1 = yl1 - csize
      endif
      with = csize * 0.7
      thick = csize * 0.13
      beta = 0.0
      gama = 0.0
      mir = 0
!             set to center justify
      call colrplot_settxt (iunit,19, 0, 2, 4, 1.0, 0.0, 0)
      call colrplot_sym (iunit,sclmid, ys1, csize, labsca, alpha, numch)
!cc   call colrplot_sym(xs1,ys1,csize,labsca,alpha,numch)
      call colrplot_lwid(iunit,width)
      call colrplot_plot(iunit,xl1, yl1, 3)
      call colrplot_plot(iunit,xl2, yl1, 2)
      call colrplot_lwid(iunit,0.005)
!            set back to left justify
      call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
      return
      end subroutine color_scl
      subroutine color_shot (iunit,x1,y1,y2,ftra,ishot,ndt,incre,num,      &
        csize, skp, itmsh, tmwd, theta, twid, ltab, jstat, nbts, iskp, ido, &
        ibtwn, itot, ntrp, ntpp, timbtwn, nrowarg, timearg, fisarg, iafter, &
        jafter, nafterarg, ntgarg)
!
! this routine plots shot points
!   x1     = the x coordinate of the section
!   y1     = the y coordinate of the top of the section
!   y2     = the y coordinate of the bottom of the section
!   ftra   = the trace to start the shot points on
!   ishot  = first shot point number
!   ndt    = the number of traces between shot points
!   incre  = number to increment shot point by
!   num    = the number of shot points to do
!   csize  = the height of the characters in inches
!   skp    = the length of the line below shot
!   itmsh  = yes--draw a line down section @ shot point labels
!          = tmrk--draw 10 mil timing marks down section @ shot point
!                  labels
!   tmwd   = width in inches of the itmsh lines
!   theta  = angle to plot characters
!   twid   = width of one trace in inches
!   ltab   = label shot points at both top and bottom of section
!   jstat  = return status (0 = normal return)
!   nbts   = number of blank trace cards
!   iskp   = number traces to skip
!   ido    = number blanks to do
!   ibtwn  = number traces between blanks
!   itot   = total in this set
!   ntrp   = multiplier to use for interpolated trace numbers
!            when horizontal grade needs to be considered
!   ntpp   = number of traces per panel
!            0 = no paneling done
!   iafter = array containing the numbers of traces that blanks
!            will be inserted after
!   jafter = array containing the graded number of traces that
!            blanks will be inserted after
!   nafter = the number of elements in iafter and jafter
!     ntg  = the total number of graded traces in the section
!  timbtwn =  time between panels
!   nrow   =  number of rows of panels
!
      integer,  intent(in) :: iunit
      real,                            intent(in)  :: x1
      real,                            intent(in)  :: y1
      real,                            intent(in)  :: y2
      real,                            intent(in)  :: ftra
      character (len = *),             intent(in)  :: ishot
      integer,                         intent(in)  :: ndt
      integer,                         intent(in)  :: incre
      integer,                         intent(in)  :: num
      real,                            intent(in)  :: csize
      real,                            intent(in)  :: skp
      character (len = *),             intent(in)  :: itmsh
      real,                            intent(in)  :: tmwd
      real,                            intent(in)  :: theta
      real,                            intent(in)  :: twid
      character (len = *),             intent(in)  :: ltab
      integer,                         intent(out) :: jstat
      integer,                         intent(in)  :: nbts
      integer,           dimension(*), intent(in)  :: iskp
      integer,           dimension(*), intent(in)  :: ido
      integer,           dimension(*), intent(in)  :: ibtwn
      integer,           dimension(*), intent(in)  :: itot
      integer,                         intent(in)  :: ntrp
      integer,                         intent(in)  :: ntpp
      real,    optional,               intent(in)  :: timbtwn
      integer, optional,               intent(in)  :: nrowarg
      real,    optional,               intent(in)  :: timearg
      real,    optional,               intent(in)  :: fisarg
      integer, optional, dimension(*), intent(in)  :: iafter
      integer, optional, dimension(*), intent(in)  :: jafter
      integer, optional,               intent(in)  :: nafterarg
      integer, optional,               intent(in)  :: ntgarg
!
      integer :: k, iws, nrow, nafter, ntg, nafter_tmp,    &
        iz, lr, nsht, nc, nnn, it3, nshts, it, it2, i,  &
        nchars,ndots
      real :: yy2, fis, time, ydatlen, ypanlen, ty1, ty2, tra, ydel, ysdel, &
        ysdel2, f, xx, xxx, fnc, x, yy1, tnnw
      character iyes*3

      integer :: istat

      integer,pointer :: tmpiafter(:),tmpjafter(:)
      
      character(len=8) :: itemp,ipz,lch
      character(len=8) :: npnt,ns,nstemp
      character(len=1) :: onech,izh

      logical :: almost_equal
!
!
      data iyes /'YES'/
!
      nullify (tmpiafter) ! jpa
      nullify (tmpjafter) ! jpa
!
      jstat=0
      ipz = '0'
!
      yy1 = y1
      yy2 = y2
      if (present(nrowarg)) then
        nrow = nrowarg
      else
        nrow = 1
      end if
      if (present(fisarg)) then
        fis = fisarg
      else
        fis = 5.0
      end if
      if (present(timearg)) then
        time = timearg
      else
        time = 1.0
      end if
      if (present(nafterarg)) then
        nafter = nafterarg
      else
        nafter = 0
      end if
      if (present(ntgarg)) then
        ntg = ntgarg
      else
        ntg = 0
      end if
      if (present(iafter)) then
        nafter_tmp = nafter
      else
        nafter_tmp = 0
      end if
      ns = ishot
      iws = bit_size (k)
      ndots = tmwd / 0.005 + .00001
      izh = ' '
      lch = ' '
      itemp = ' '
      itemp(1:1)=ns(1:1)
      if (itemp(1:1).eq.'-') goto 100
      call string_cc2ii(ns,iz,istat)
      if (istat .ne. 1) go to 50
      almost_equal = ameq (theta, 0.0, 0.0001)
      ydatlen = time * fis
      ypanlen = ydatlen
      if (nrow.gt.1) then
         ypanlen = (time+timbtwn) * fis
         ydatlen = time * fis
         if (almost_equal) then
            yy2 = abs (y1 - ydatlen)
            ypanlen = - ypanlen
            ydatlen = - ydatlen
         else
            yy2 = y1 + ydatlen
         endif
      endif
!
!          test for 1st character zero
      call string_cc2ii(ns,iz,istat)
      if (istat .ne. 1) go to 50
      if (iz.ne.0) goto 150
!          if second character is blank - user really wants
!            a zero shot point rather than a zero representing
!            a minus sign
!
      onech=' '
      onech=ns(1:1)
      if (onech .eq.' ') iz = 9999
      goto 150
!
!          first character in label is hollerith
 50   continue
      izh=ns(1:1)
      nstemp=' '
      nstemp(1:7)=ns(2:8)
      ns=nstemp
      ns(8:8)=' '
      call string_cc2ii(ns,nsht,istat)
      if (istat .ne. 1) go to 60
      goto 207
!
!          last character must be hollerith
   60 continue
      nstemp=' '
      nstemp(2:8)=ns(1:7)
      ns=nstemp
      ns(1:1)=izh
      izh = ' '
      ns=adjustr(ns)
      lch(1:1)=ns(8:8)
      ns=adjustl(ns)
      nc=len_trim(ns)
      ns(1:nc)=' '
      goto 150
!
!
  100 iz = 1
  150 continue
      call string_cc2ii(ns,nsht,istat)
      if (istat .ne. 1) go to 400
!
  207 continue
      ty1 = yy1
      ty2 = yy2
      tra = ftra
      ydel = skp
      ysdel = ydel + .1
      ysdel2 = ysdel + csize
      if (theta.gt.0.0) then
         ydel = - ydel
         ysdel = - ysdel
         ysdel2 = - ysdel2
      endif
!
!
!
      nnn = ntrp
      if (ntpp.eq.0) nnn = 1
      if(nafter_tmp.gt.0)then
        call mem_alloc(tmpiafter,nafter)
        call mem_alloc(tmpjafter,nafter)
      endif
      do it3 = 1, nrow
        nshts = nsht
        do 350 it = 1, num
           tnnw = tra
           if (nafter_tmp.gt.0) then
!                 compiler will not allow passing iafter and jafter because
!                  they are assumed shape arrays
              tmpiafter(1:nafter)=iafter(1:nafter)
              tmpjafter(1:nafter)=jafter(1:nafter)
              f = color_2gab (tra,tmpiafter,tmpjafter,nafter,ntrp,ntg)
              call color_abt (f, tnnw, nbts, iskp, ido, ibtwn, itot, 1)
           else
              call color_abt (tra, tnnw, nbts, iskp, ido, ibtwn, itot, nnn, &
                ntpp)
           endif
           xx = x1 + tnnw * twid-twid
           xxx = xx - 0.01
           do 200 it2 = 1, 5
              call colrplot_plot(iunit,xxx, ty1, 3)
              call colrplot_plot(iunit,xxx, ty1 + ydel, 2)
              if (ltab(:3) .eq. iyes .and. it3.eq.1) then
                 call colrplot_plot(iunit,xxx, y2, 3)
                 call colrplot_plot(iunit,xxx, y2 - ydel, 2)
              endif
              xxx = xxx + .005
  200      end do
!
           if (itmsh(:3) .eq. iyes) then
              xxx = xx - real (ndots) / 2.0 * 0.005
              do 225 i = 1, ndots
                 call colrplot_plot(iunit,xxx, ty1, 3)
                 call colrplot_plot(iunit,xxx, ty2, 2)
                 xxx = xxx + .005
  225         end do
           endif
           nchars = 1
           if (nsht .gt. 9) nchars = 2
           if (nsht .gt. 99) nchars = 3
           if (nsht .gt. 999) nchars = 4
           if (nsht .gt. 9999) nchars = 5
           if (nsht .gt. 99999) nchars = 6
           if (nsht .lt. 0) nchars = 2
           if (nsht .lt. -9) nchars = 3
           if (nsht .lt. -99) nchars = 4
           if (nsht .lt. -999) nchars = 5
           if (nsht .lt. -9999) nchars = 6
           if (nsht .lt. -99999) nchars = 7
           fnc = nchars
           call string_ii2cc(nsht,npnt)
           if (iz .ne. 0 .and. izh .eq. ' ') goto 250
           if (lch .ne. ' ') goto 250
           nstemp=' '
           nstemp(2:8)=npnt(1:7)
           npnt=nstemp
           nchars = nchars + 1
           fnc = nchars
           npnt(1:1)=ipz(1:1)
           if (izh .ne.' ')then
             npnt(1:1)=izh(1:1)
           endif
  250      continue
           if (lch .ne. ' ') then
              npnt(nchars-1:nchars)=lch(1:1)
              nchars = nchars + 1
              fnc = nchars
           endif
!              move x to center justify label
           if (theta .eq. 0.0) x = xx - ((fnc / 2.0 * (csize)))
           if (theta .ne. 0.0) x = xx + ((fnc / 2.0 * (csize)))
           call colrplot_sym (iunit,x, ty1+ysdel, csize, npnt, theta, nchars)
           if (ltab(:3) .eq. iyes .and. it3 .eq. 1) then
             call colrplot_sym (iunit,x, y2-ysdel2, csize, npnt, theta, nchars)
           end if
           nsht = nsht + incre
           tra = tra + ndt
  350   end do
        ty1 = ty1 + ypanlen
        ty2 = ty1 + ydatlen
        if (lr .eq. 1 .and. ty2 .lt. y2) ty2 = y2
        if (lr .eq. 0 .and. ty2 .gt. y2) ty2 = y2
        tra = ftra
        nsht = nshts
      enddo
      if(nafter_tmp.gt.0)then
        call mem_free(tmpiafter)
        call mem_free(tmpjafter)
      endif
!
!
!
  400 continue
      if (istat .ne. 1)then
         write(print_lun,*) 'TROUBLE WITH ALF2NUM IN color_shot      '
         jstat=1
      endif
      return
      end subroutine color_shot
      subroutine color_sid (iunit,x, y, size, isid1, isid2, isid3, isid4, alpha)
!
!
!  x,y     = coordinate for isid1
!  size    = height of letters in inchs
!  sids    = two  28 character fields for section id
!  alpha   = angle to plot characters
!
      integer,  intent(in) :: iunit
      real,                intent(in) :: x
      real,                intent(in) :: y
      real,                intent(in) :: size
      character (len = *), intent(in) :: isid1
      character (len = *), intent(in) :: isid2
      character (len = *), intent(in) :: isid3
      character (len = *), intent(in) :: isid4
      real,                intent(in) :: alpha
!
      integer :: nc, nonbc
      real :: hsid, ys, xx
      character ibnk*1
!
      ibnk = achar (0)
!          conplot gives only 85% of "SIZE" - so compensate
      hsid = size / .85
      nc = 28
      ys = y
      call colrplot_fnt (iunit,19)
      nonbc=len_trim(isid1)
      xx = x
      if (isid1 .eq. ' ') goto 5
      call colrplot_sym (iunit,xx, ys, hsid, isid1, alpha, nonbc)
    5 if (alpha .eq. 0.0) then
         ys = ys - (.15 + size)
      else
         ys = ys + (.15 + size)
      endif
      nonbc=len_trim(isid2)
      if (isid2 .eq. ' ') goto 10
      call colrplot_sym (iunit,x, ys, hsid, isid2, alpha, nonbc)
   10 if (alpha .eq. 0.0) then
         ys = ys - (.15 + size)
      else
         ys = ys + (.15 + size)
      endif
      if (isid3 .eq. ' ') goto 15
      nonbc=len_trim(isid3)
      call colrplot_sym (iunit,x, ys, hsid, isid3, alpha, nonbc)
   15 if (alpha .eq. 0.0) then
         ys = ys - (.15 + size)
      else
         ys = ys + (.15 + size)
      endif
      nonbc=len_trim(isid4)
      if (isid4 .eq. ' ') goto 20
      call colrplot_sym (iunit,x, ys, hsid, isid4, alpha, nonbc)
!
   20 continue
      return
      end subroutine color_sid
      subroutine color_tblk (iunit,x1,y1,isid1,isid2,isid3,isid4,reduce,  &
        ang, inp, ht2)
!
!    routine to plot title block
!
!--------if anything is changed in this routine which would effect
!         the length of the title block - be sure to change the
!         color_thit routrine also------------
!
!    argument
!        x1 = x cordi of upper left corner of title block
!        y1 = y cordi of upper left corner of title block
!       isid1= line 1 of sid
!       isid2= line 2 of sid
!       isid3= line 3 of sid
!       isid4= line 4 of sid
!     reduce= reduction factor for title block
!        ang= angle to plot annotation
!        ht2= height from second type card from calhite routine
!               used to calculate coordinated for lines between
!               sections
!        inp= color input logical unit #
!
      integer,  intent(in) :: iunit
      real,                intent(in) :: x1
      real,                intent(in) :: y1
      character (len = *), intent(in) :: isid1
      character (len = *), intent(in) :: isid2
      character (len = *), intent(in) :: isid3
      character (len = *), intent(in) :: isid4
      real,                intent(in) :: reduce
      real,                intent(in) :: ang
      integer,             intent(in) :: inp
      real,                intent(in) :: ht2
!
      integer :: nca(20), nci(20), ifs, ihalf, knt, k, j, nc1, nc2, nc3, &
        nc, i1, i2, n, istat
      real :: siz1, x, y, xc, yc, xe, xs, dly, ht, ys, cht, yc1,   &
        hti, xx, xus, xue, yu, sz, f, xpl, ydif, xsm1, ysm1, s, xsm2,  &
        ysm2, xcab
      character itype*5, icom*1, ieq*1, type*10, cabl*10, ctemp*10, its*10, &
        logo*10, icard*80, cblk*10, dubi*10, mcd*80, ownera*80, ownerb*160, &
        ownert*320, iblk*8, inf*80
!
!
      data logo /'LOGO'/, cabl /'CABLE'/, itype /'TYPE='/, icom /','/,     &
        ieq /'='/, cblk /'          '/, dubi /'DUBI'/, iblk /'        '/
      data ownera /'THESE DATA ARE OWNED BY AND ARE A TRADE SECRET OF'/
      data ownerb /'. THE USE OF THESE DATA IS RESTRICTED TO PARTIES HO&
        &lding a valid license and is subject to the confidentiality terms &
        &of that license.'/

      logical :: almost_equal


!
!
      call colrplot_fnt (iunit,0)
      call colrplot_tclr (iunit,1)
      siz1 = 0.01
!
      type = cblk
      ctemp = cblk
      if (ang.eq.0.0) then
         x = x1 + .8 * reduce
      else
         x = x1 - .8 * reduce
      endif
      y = y1
      xc = x
      yc = y
!
      if (ang .eq. 0.0) then
         xe = x + 71. * ht2 * reduce
         xs = x - ht2 * reduce
      else
         xe = x - 71. * ht2 * reduce
         xs = x + ht2 * reduce
      endif
      dly = ((1.5 * ht2) * reduce) * 2.
      if (ang .eq. 0.0) dly = - dly
      ifs = 0
      ihalf = 0
!
!
   10 read (inp, 9001, end = 720) icard
      knt = knt + 1
!
!          test for type card
      if (icard(:5) .ne. itype) goto 120
      knt = 0
      type = cblk
      ctemp = cblk
!
!          get type of card & character height
!          ignore old numc parameter
      k = 1
      j = 6
   12 if (icard(j:j) .eq. icom) goto 20
      type(k:k) = icard(j:j)
      if (k .ge. 10) then
         ht = .1 * reduce
         goto 29
      endif
      k = k + 1
      j = j + 1
      goto 12
   20 j = j + 1
      if (icard(j:j) .eq. 'H') goto 25
      goto 20
   25 j = j + 7
      k = 1
   27 ctemp(k:k) = icard(j:j)
      j = j + 1
      if (icard(j:j) .eq. icom .or. icard(j:j) .eq. cblk(1:1)) goto 28
      k = k + 1
      goto 27
 28   continue
      call string_cc2ff(ctemp,ht,istat)
      if (istat .ne. 1) go to 30
      ht = ht * reduce
   29 dly = 1.5 * ht * 2.0
      if (ang .ne. 0.0) dly = - dly
      if (ifs .eq. 0) goto 40
      goto 600
   30 write(print_lun,*)' ERROR ON TYPE CARD'
      write(print_lun,*) ' CTEMP = ', ctemp, ' HT = ', ht
      return
   40 ifs = 1
      ys = y
      if (ang .eq. 0.0) then
         yc = y - (ht * 1.5)
      else
         yc = y + (ht * 1.5)
      endif
      goto 10
  120 its = cblk
      its(1:8) = icard(1:8)
      if (its .eq. logo      ) goto 350
      if (its .eq. dubi      ) goto 360
      if (its .eq. '  FKPLOT') goto 425
      if (its(1:3) .eq. '  #') then
         ihalf = 1
         yc = yc + dly * .5
         icard(1:3) = cblk(1:3)
      endif
      if (type .eq. cabl .or. type .eq. 'CAB2S3S') goto 400
      if (type .eq. 'ID') goto 700
      call color_brac (icard, mcd, nca, nci)
      if (its .eq. 'C0N0') goto 150
      if (its .ne. 'CONO') goto 200
  150 continue
      if (its .eq. 'C0N0C0-') goto 450
      if (its .eq. 'CONOCO-') goto 450
  200 continue
      cht = ht
!          knt will be equal to 1 on the first card read after the
!              type card
      yc1 = yc
      if (knt .eq. 1 .and. type .ne. 'HEAD') then
         cht = ht * 1.5
         if (ang .eq. 0.0) then
            yc1 = yc - 0.05
         else
            yc1 = yc + 0.05
         endif
      endif
!          plot all info on card which is not in brackets
      call colrplot_sym (iunit,xc, yc1, cht, mcd, ang, 70)
      if (type .eq. 'OWNER') then
         yc = yc - dly
         goto 690
      endif
      hti = ht * .8
      inf=' '
!!      call fillb (inf(1:1), iblk(1:1), 64)
      do 250 k = 1, 20
         if (nca(k) .eq. 0) goto 300
         call colrplot_mvc (icard(1:1), nci(k), inf(1:1), 1, nca(k))
         if (ang .eq. 0.0) then
            xx = xc + (ht * nci(k))
         else
            xx = xc - (ht * nci(k))
         endif
         if (ang .eq. 0.0) then
            yc1 = yc + .03
         else
            yc1 = yc - .03
         endif
!            plot information which is inside brackets
         call colrplot_sym (iunit,xx, yc1, hti, inf, ang, nca (k) )
         xus = xx
         if (ang .eq. 0.0) then
            xue = xx + ht * nca(k)
            if (xue .gt. xe) xue = xe - ht
         else
            xue = xx - ht * nca(k)
            if (xue .lt. xe) xue = xe + ht
         endif
         if (ang .eq. 0.0) then
            yu = yc - (ht * .52)
         else
            yu = yc + (ht * .52)
         endif
!            underline information inside brackets with dashed line
         call colrplot_plot(iunit,xus, yu, 3)
         call colrplot_plot(iunit,xue, yu, 2)
!           turn off dashline mode
!       call colrplot_dash(iunit,dshpat,0)
  250 end do
  300 continue
      yc = yc - dly
      if (ihalf .eq. 1) then
         yc = yc + dly * .5
         ihalf = 0
      endif
      if (cht .ne. ht) yc = yc - dly * .5
      goto 10
  350 continue
! *** call color_logo to plot logo
!
      yc = yc - (dly / 2.)
      sz = 2.3125
      if (ht .lt. 0.15) then
         f = ht / .15
         sz = sz * f
      endif
      if (ang .eq. 0.0) then
         xpl = xc + (6. * ht * .9 + sz / 2. + 1.2)
      else
         xpl = xc - (6. * ht * .9 + sz / 2. + 1.2)
      endif
      call color_logo (iunit,xpl, yc, sz, ang)
      yc = yc - dly * 1.5
!
      goto 10
!
  360 continue
      yc = yc - dly * 1.4
      sz = 2.3125 * reduce
      if (ang .eq. 0.0) then
         xpl = xc + (6. * ht * .9 + sz / 2.)
      else
         xpl = xc - (6. * ht * .9 + sz / 2.)
      endif
      call color_dubi (iunit,yc, xpl, ang,istat)
      if (istat .ne. 0) go to 10
      yc = yc - dly * .8
      goto 10
  400 continue
!
! *** call cabl to plot cable digram
!
      cht = ht * 1.5
      if (ang .eq. 0.0) then
         xcab = xc + .95
         yc = yc - 0.1
      else
         xcab = xc - .95
         yc = yc + 0.1
      endif
      call colrplot_sym (iunit,xc, yc, cht, icard, ang, 70)
      if (ang .eq. 0.0) then
         yc = yc - (dly * 1.5 + .5)
      else
         yc = yc + (-1.0 * dly * 1.5 + .5)
      endif
      if (type .eq. 'CAAB2S3S') then
!         read (inp, 9001) icard
!         call sptbbrac (icard, mcd, nca, nci)
!         icard (71:71) = char (0)
!         call colr2s3s (xcab, yc, ang, icard, nca, nci)
      else
         call color_cabl (iunit,xcab, yc, nci, nca, ht, ang, reduce, inp)
      endif
! *** set type so that any card left will be plotted
      type = 'XXX'
      if (ang .eq. 0.0) then
         yc = yc - 1.2
      else
         yc = yc + 1.2
      endif
      goto 10
!
  425 call color_fkpl (iunit,yc, dly, ht, ang, inp)
      goto 10
!
  450 continue
!
! *** plot conoco-partner name
!
      yc = yc - (dly / 2.)
      call colrplot_sym (iunit,xc, yc, ht * 1.5, icard, ang, 70)
      yc = yc - dly * 1.5
      goto 10
  600 continue
!          line between sections
      call colrplot_plot(iunit,xs, yc + dly * .5, 3)
      call colrplot_plot(iunit,xe, yc + dly * .5, 2)
      goto 10
!!      goto 40
!
!          plot owner
  690 continue
      read (inp, 9001, end = 720) icard
      nc1=len_trim(ownera)
      nc2=len_trim(icard)
      nc3=len_trim(ownerb)
      ownert = ownera (1:nc1+1) //icard (1:nc2) //ownerb (1:nc3)
      nc=len_trim(ownert)
      i1 = 1
      i2 = 70
  695 continue
!        make sure line ends in a period or space
      if (ownert(i2:i2) .ne. '.' .and. ownert(i2:i2) .ne. ' ') then
         i2 = i2 - 1
         goto 695
      endif
      n = i2 - i1 + 1
!        move to icard so text will be on word boundaries
      call colrplot_mvc (ownert(1:1), i1, icard(1:1), 1, n)
      call colrplot_sym (iunit,xc, yc, hti, icard, ang, 70)
      yc = yc - dly * .5
      if (i2 .lt. nc) then
         i1 = i1 + n
         i2 = i1 + 70
         goto 695
      endif
      yc = yc - dly * .5
      goto 10
  700 continue
!
!          plot id
      call colrplot_sym (iunit,xc, yc, ht, icard, ang, 70)
      yc = yc - dly * .5
!
!          box around title block
  720 continue
      almost_equal = ameq(yc, 0.0, 0.00001)
      if (almost_equal) yc = 0.0001
      call colrplot_plot(iunit,xs, ys, 3)
      call colrplot_plot(iunit,xs, yc, 2)
      call colrplot_plot(iunit,xe, ys, 3)
      call colrplot_plot(iunit,xe, yc, 2)
      call colrplot_plot(iunit,xs, ys, 3)
      call colrplot_plot(iunit,xe, ys, 2)
      call colrplot_plot(iunit,xs, yc, 3)
      call colrplot_plot(iunit,xe, yc, 2)
      call colrplot_plot(iunit,x1, ys, 3)
      call colrplot_plot(iunit,xs, ys, 2)
      call colrplot_plot(iunit,x1, yc, 3)
      call colrplot_plot(iunit,xs, yc, 2)
      call colrplot_plot(iunit,x1, ys, 3)
      call colrplot_plot(iunit,x1, yc, 2)
      ydif = abs (ys - yc)
      write(print_lun,*) ' COLOR_TBLK -> ACTUAL TITLE BLOCK HEIGHT = ', ydif
      if (ang .eq. 0.0) then
         xsm1 = x1 + .3 * reduce
         ysm1 = y1 - 11.7 * reduce
      else
         xsm1 = x1 - .3 * reduce
         ysm1 = y1 + 11.7 * reduce
      endif
      s = .2 * reduce
      call colrplot_sym (iunit,xsm1, ysm1, s, isid1, ang + 90.0, 28)
      if (ang .eq. 0.0) then
         xsm2 = xsm1 + .3 * reduce
         ysm2 = ysm1 + (29 * .2) * reduce
      else
         xsm2 = xsm1 - .3 * reduce
         ysm2 = ysm1 - (29 * .2) * reduce
      endif
      call colrplot_sym (iunit,xsm1, ysm2, s, isid2, ang + 90.0, 28)
      if (isid3(1:8) .eq. iblk) goto 750
      call colrplot_sym (iunit,xsm2, ysm1, s, isid3, ang + 90.0, 28)
      if (isid4(1:8) .eq. iblk) goto 750
      call colrplot_sym (iunit,xsm2, ysm2, s, isid4, ang + 90.0, 28)
  750 continue
      return
!
 9001 format(a)
      end subroutine color_tblk
      subroutine color_thit (reduce, tht, ifili, iatb, inp, ht2)
!
!    routine to calculate the height of the title block
!
!     reduce= reduction factor for title block  - received
!     tht   = total height in inches returned   - returned
!     ifili = file name to read if not from atb - received
!     iatb  = 1 if generated by atb otherwise 0 - received
!     inp   = color input logical unit #        - changed
!     ht2   = height from second type card      - returned
!               used to calculate coordinated for lines between
!               sections                        - changed
!
      real,                intent(in)    :: reduce
      real,                intent(out)   :: tht
      character (len = *), intent(in)    :: ifili
      integer,             intent(in)    :: iatb
      integer,             intent(inout) :: inp
      real,                intent(inout) :: ht2
!
      integer :: nca(20), nci(20), ifs, ihalf, knt, k, j,  nc1, nc2,     &
        nc3, nc, i1, i2, n, istat
      real :: siz1, ang, x1, y1, x, y, xc, yc, xe, xs, dly, ht,    &
        ys, cht, yc1, hti, xx, xus, xue, yu, sz, f, xpl, xcab, xsm1, ysm1,  &
        s, xsm2, ysm2
      character itype*5, icom*1, ieq*1, type*10, cabl*10, ctemp*10, its*10, &
        logo*10, icard*80, cblk*10, dubi*10, mcd*80, ownera*80, ownerb*160, &
        ownert*320, inf*80, iblk*8
!
      data logo /'LOGO'/, cabl /'CABLE'/, itype /'TYPE='/, icom /','/, &
        ieq /'='/, cblk /'          '/, dubi /'DUBI'/, iblk /'        '/
      data ownera /'THESE DATA ARE OWNED BY AND ARE A TRADE SECRET OF'/
      data ownerb /'. THE USE OF THESE DATA IS RESTRICTED TO PARTIES HO&
        &lding a valid license and is subject to the confidentiality terms &
        &of that license.' /
!
      siz1 = 0.01
      ang = 0.0
      x1 = 0.0
      y1 = 0.0
!
      type = cblk
      ctemp = cblk
      if (ang .eq. 0.0) then
         x = x1 + .8 * reduce
      else
         x = x1 - .8 * reduce
      endif
      y = y1
      xc = x
      yc = y
      if (iatb .eq. 0) then
        call getlun (inp, istat) 
        if (istat .ne. 0) then
          write(print_lun,*)'COLOR_thit - ERROR GETTING LUN FOR COLOR TBK &
                            &FILE = ', istat
          go to 8600
        end if
!
        open (unit=inp, file=ifili, iostat=istat, status='old', &
          action='read')
        if (istat .ne. 0) go to 8506
      endif
!
      if (ang .eq. 0.0) then
         xe = x + 71. * ht2 * reduce
         xs = x - ht2 * reduce
      else
         xe = x - 71. * ht2 * reduce
         xs = x + ht2 * reduce
      endif
      if (ang .eq. 0.0) dly = -dly
      ifs = 0
      ihalf = 0
      rewind inp
!
   10 read (inp, 9001, end = 720) icard
      knt = knt + 1
!
!          test for type card
      if (icard(:5) .ne. itype) goto 120
      knt = 0
      type = cblk
      ctemp = cblk
!
!          get type of card & character height
!          ignore old numc parameter
      k = 1
      j = 6
   12 if (icard(j:j) .eq. icom) goto 20
      type(k:k) = icard(j:j)
      if (k .ge. 10) then
         ht = .1 * reduce
         goto 29
      endif
      k = k + 1
      j = j + 1
      goto 12
   20 j = j + 1
      if (icard(j:j) .eq. 'H') goto 25
      goto 20
   25 j = j + 7
      k = 1
   27 ctemp(k:k) = icard(j:j)
      j = j + 1
      if (icard(j:j) .eq. icom .or. icard(j:j) .eq. cblk(1:1)) goto 28
      k = k + 1
      goto 27
 28   continue
      call string_cc2ff(ctemp,ht,istat)
      if (istat .ne. 1)go to 30
      ht = ht * reduce
   29 dly = 1.5 * ht * 2.0
      if (ang .ne. 0.0) dly = - dly
      if (ifs .eq. 0) goto 40
      goto 600
   30 write(print_lun,*) ' ERROR ON TYPE CARD'
      write(print_lun,*) ' CTEMP = ', ctemp, ' HT = ', ht
      go to 8600
   40 ifs = 1
      ys = y
      if (ang .eq. 0.0) then
         yc = y - (ht * 1.5)
      else
         yc = y + (ht * 1.5)
      endif
      goto 10
  120 its = cblk
      its(1:8) = icard(1:8)
      if (its .eq. logo      ) goto 350
      if (its .eq. dubi      ) goto 360
      if (its .eq. '  FKPLOT') goto 425
      if (its(1:3) .eq. '  #') then
         ihalf = 1
         yc = yc + dly * .5
         icard(1:3) = cblk(1:3)
      endif
      if (type .eq. cabl .or. type .eq. 'CAB2S3S') goto 400
      if (type .eq. 'ID') goto 700
      call color_brac (icard, mcd, nca, nci)
      if (its .eq. 'C0N0') goto 150
      if (its .ne. 'CONO') goto 200
  150 continue
      if (its .eq. 'C0N0C0-') goto 450
      if (its .eq. 'CONOCO-') goto 450
  200 continue
      cht = ht
!          knt will be equal to 1 on the first card read after the
!              type card
      yc1 = yc
      if (knt .eq. 1 .and. type .ne. 'HEAD') then
         ht2 = ht / reduce
         dly = ((1.5 * ht2) * reduce) * 2.
         cht = ht * 1.5
         if (ang .eq. 0.0) then
            yc1 = yc - 0.05
         else
            yc1 = yc + 0.05
         endif
      endif
      if (type .eq. 'OWNER') then
         yc = yc - dly
         goto 690
      endif
!          plot all info on card which is not in brackets
!cc   call colrplot_sym(iunit,xc,yc1,cht,mcd,ang,70)
      hti = ht * .8
      inf=' '
!!      call fillb (inf(1:1), iblk(1:1), 64)
      do 250 k = 1, 20
         if (nca(k) .eq. 0) goto 300
         call colrplot_mvc (icard(1:1), nci(k), inf(1:1), 1, nca(k))
         if (ang .eq. 0.0) then
            xx = xc + (ht * nci(k))
         else
            xx = xc - (ht * nci(k))
         endif
         if (ang .eq. 0.0) then
            yc1 = yc + .03
         else
            yc1 = yc - .03
         endif
!            plot information which is inside brackets
!cc     call colrplot_sym(iunit,xx,yc1,hti,inf,ang,nca(k))
         xus = xx
         if (ang .eq. 0.0) then
            xue = xx + ht * nca(k)
            if (xue .gt. xe) xue = xe - ht
         else
            xue = xx - ht * nca(k)
            if (xue .lt. xe) xue = xe + ht
         endif
         if (ang .eq. 0.0) then
            yu = yc - (ht * .52)
         else
            yu = yc + (ht * .52)
         endif
!            underline information inside brackets with dashed line
!cc     call colrplot_plot(iunit,xus,yu,3)
!cc     call colrplot_plot(iunit,xue,yu,2)
!           turn off dashline mode
!       call colrplot_dash(iunit,dshpat,0)
  250 end do
  300 continue
      yc = yc - dly
      if (ihalf .eq. 1) then
         yc = yc + dly * .5
         ihalf = 0
      endif
      if (cht .ne. ht) yc = yc - dly * .5
      goto 10
  350 continue
! *** call color_logo to plot logo
!
      yc = yc - (dly / 2.)
      sz = 2.3125
      if (ht .lt. 0.15) then
         f = ht / .15
         sz = sz * f
      endif
      if (ang .eq. 0.0) then
         xpl = xc + (6. * ht * .9 + sz / 2. + 1.2)
      else
         xpl = xc - (6. * ht * .9 + sz / 2. + 1.2)
      endif
!cc   call color_logo(iunit,xpl,yc,sz,ang)
      yc = yc - dly * 1.5
!
      goto 10
!
  360 continue
      yc = yc - dly * 1.4
      sz = 2.3125 * reduce
      if (ang .eq. 0.0) then
         xpl = xc + (6. * ht * .9 + sz / 2.)
      else
         xpl = xc - (6. * ht * .9 + sz / 2.)
      endif
!cc   call color_dubi(yc,xpl,ang,icray,istat)
!cc   if (istat .ne. 0) go to 10
      yc = yc - dly * .8
      goto 10
  400 continue
!
! *** call cabl to plot cable digram
!
      cht = ht * 1.5
      if (ang .eq. 0.0) then
         xcab = xc + .95
         yc = yc - 0.1
      else
         xcab = xc - .95
         yc = yc + 0.1
      endif
!cc   call colrplot_sym(iunit,xc,yc,cht,icard,ang,70)
      if (ang .eq. 0.0) then
         yc = yc - (dly * 1.5 + .5)
      else
         yc = yc + (-1.0 * dly * 1.5 + .5)
      endif
      if (type .eq. 'CAB2S3S') then
!         read (inp, 9001) icard
!         call sptbbrac (icard, mcd, nca, nci)
!         icard (71:71) = char (0)
!cc     call colr2s3s(xcab,yc,ang,icard,nca,nci)
      else
!cc     call color_cabl(xcab,yc,nci,nca,ht,ang,reduce,inp)
      endif
! *** set type so that any card left will be plotted
      type = 'XXX'
      if (ang .eq. 0.0) then
         yc = yc - 1.2
      else
         yc = yc + 1.2
      endif
      goto 10
!
  425 continue
!cc5  call color_fkpl(yc,dly,ht,ang,inp)
      goto 10
!
  450 continue
!
! *** plot conoco-partner name
!
      yc = yc - (dly / 2.)
!cc   call colrplot_sym(iunit,xc,yc,ht*1.5,icard,ang,70)
      yc = yc - dly * 1.5
      goto 10
  600 continue
!          line between sections
!cc   call colrplot_plot(iunit,xs,yc+dly*.5,3)
!cc   call colrplot_plot(iunit,xe,yc+dly*.5,2)
      goto 10
!!      goto 40
!          plot owner
  690 continue
      read (inp, 9001, end = 720) icard
      nc1=len_trim(ownera)
      nc2=len_trim(icard)
      nc3=len_trim(ownerb)
      ownert = ownera(1:nc1+1) // icard(1:nc2) // ownerb(1:nc3)
      nc=len_trim(ownert)
      i1 = 1
      i2 = 70
  695 continue
!        make sure line ends in a period or space
      if (ownert(i2:i2) .ne. '.' .and. ownert(i2:i2) .ne. ' ') then
         i2 = i2 - 1
         goto 695
      endif
      n = i2 - i1 + 1
!        move to icard so text will be on word boundaries
      call colrplot_mvc (ownert(1:1), i1, icard(1:1), 1, n)
      yc = yc - dly * .5
      if (i2.lt.nc) then
         i1 = i1 + n
         i2 = i1 + 70
         goto 695
      endif
      yc = yc - dly * .5
      goto 10
  700 continue
!
!          plot id
!cc   call colrplot_sym(iunit,xc,yc,ht,icard,ang,70)
      yc = yc - dly * .5
!
!          box around title block
  720 continue
!cc   call colrplot_plot(xs,ys,3)
!cc   call colrplot_plot(xs,yc,2)
!cc   call colrplot_plot(xe,ys,3)
!cc   call colrplot_plot(xe,yc,2)
!cc   call colrplot_plot(xs,ys,3)
!cc   call colrplot_plot(xe,ys,2)
!cc   call colrplot_plot(xs,yc,3)
!cc   call colrplot_plot(xe,yc,2)
!cc   call colrplot_plot(x1,ys,3)
!cc   call colrplot_plot(xs,ys,2)
!cc   call colrplot_plot(x1,yc,3)
!cc   call colrplot_plot(xs,yc,2)
!cc   call colrplot_plot(x1,ys,3)
!cc   call colrplot_plot(x1,yc,2)
      tht = abs (ys - yc)
      write(print_lun,*) ' COLOR_THIT ->  TITLE BLOCK HEIGHT = ', tht
      if (ang .eq. 0.0) then
         xsm1 = x1 + .3 * reduce
         ysm1 = y1 - 11.7 * reduce
      else
         xsm1 = x1 - .3 * reduce
         ysm1 = y1 + 11.7 * reduce
      endif
      s = .2 * reduce
!cc    call colrplot_sym(xsm1,ysm1,s,isid1,ang+90.0,28)
      if (ang .eq. 0.0) then
         xsm2 = xsm1 + .3 * reduce
         ysm2 = ysm1 + (29 * .2) * reduce
      else
         xsm2 = xsm1 - .3 * reduce
         ysm2 = ysm1 - (29 * .2) * reduce
      endif
!cc   call colrplot_sym(xsm1,ysm2,s,isid2,ang+90.0,28)
!cc   call colrplot_sym(xsm2,ysm1,s,isid3,ang+90.0,28)
!cc   call colrplot_sym(xsm2,ysm2,s,isid4,ang+90.0,28)
      rewind inp
      go to 8600
!
 8506 write(print_lun,*) '***************************************************'
      write(print_lun,*) ' ERROR TRYING TO GET TITLE BLOCK FILE '
 8600 return
 9001 format(a)
      end subroutine color_thit
      subroutine color_tlne (iunit,xo,xend,yo,it10,it50,it100,it500,      &
        it1000, ius, time, irnum, lrrl, itico, tncs, itlbl3, fis, angle,  &
        yends, tstrt, twid, igrdh, nrow, nbts, ibtskp, ibtdo, ibtbtwn,    &
        ibttot, irtbp, ntrph, timbtwn, nrlc, it400)
!
!          this routine plots horizontal timing lines and
!           timing line numbers
!
!
!          xo     = beginning x coordinate of section
!        xend     = ending x coordinate of section
!          yo     = y- origin
!           it10  = number of dots for 10 mil timing lines
!           it50  = number of dots for 50 mil timing lines
!          it100  = number of dots for 100 mil timing lines
!          it500  = number of dots for 500 mil timing lines
!         it1000  = number of dots for 1000 mil timing lines
!          ius    = units per second
!         time    = time in seconds
!        irnum    = reverse timing line numbers - yes or no
!         lrrl    = left to right or right to left plot
!        itico    = plot tic marks only rather than full timing line
!                    yes or no
!          tncs   = timing number character size
!         itlbl3  = time increment used when not plotting regular
!                   timing line numbers.
!          fis    = inches per second
!          angle  = angle in degrees
!          yend   = y-coordinate for end of data
!          tstrt  = label of first timing line number
!          twid   = width of one trace in inches
!          grdh   = horizontal grade yes or no
!          nrow   = number of rows if paneling
!          nbts   = number of blank trace patterns
!         ibtskp  = number of traces to skip before inserting blanks
!          ibtdo  = number of blank traces to do
!         ibtbtwn = number of traces between blanks
!          ibttot = number of times to repeat pattern
!          irtbp  = repeat times in blank trace pattern (yes or no)
!          ntrph  = number of traces interpolated in horizontal
!                   direction
!         timbtwn = time between panels
!           nrlc  = number of rows in last column
!         it400  = number of dots for 400 mil timing lines
!
      integer,  intent(in) :: iunit
      real,                  intent(in) :: xo
      real,                  intent(in) :: xend
      real,                  intent(in) :: yo
      integer,               intent(in) :: it10
      integer,               intent(in) :: it50
      integer,               intent(in) :: it100
      integer,               intent(in) :: it500
      integer,               intent(in) :: it1000
      integer,               intent(in) :: ius
      real,                  intent(in) :: time
      character (len = *),   intent(in) :: irnum
      character (len = *),   intent(in) :: lrrl
      character (len = *),   intent(in) :: itico
      real,                  intent(in) :: tncs
      integer,               intent(in) :: itlbl3
      real,                  intent(in) :: fis
      real,                  intent(in) :: angle
      real,                  intent(in) :: yends
      real,                  intent(in) :: tstrt
      real,                  intent(in) :: twid
      character (len = *),   intent(in) :: igrdh
      integer,               intent(in) :: nrow
      integer,               intent(in) :: nbts
      integer, dimension(*), intent(in) :: ibtskp
      integer, dimension(*), intent(in) :: ibtdo
      integer, dimension(*), intent(in) :: ibtbtwn
      integer, dimension(*), intent(in) :: ibttot
      character (len = *),   intent(in) :: irtbp
      integer,               intent(in) :: ntrph
      real,                  intent(in) :: timbtwn
      integer,               intent(in) :: nrlc
      integer,               intent(in) :: it400
!
      integer :: itldt(6), needtop, k, itmp, milts, mil500, mil400,    &
        mil100, mil50, mil10, n, idot, kntrow, kntr, krow, num, lnum,     &
        ndec, nsec, itnum, ndtmp, m, j, ii, ntlne, i, nsec2, nc, mil200,  &
        mfwhnum, ndots
      real :: yend, f, ybias, g, theta, ftmp, frac, s,    &
        fwhnum, otim, rowylen, sec, us, gap, y1, dotsiz, yy1, &
        xx1, xx2, grdbias, ypanlen, xz, sizl, sizs, x2l, x2s, x1l, x1s,   &
        ftime, ydot, ytop, xxt, xt, secsav, ysize, ysizes, yinc, secinc,  &
        t, sinc, dif, yi, y, yy1sav, xml, xms, ssav, xinc
      character (len=8) :: cdum,knum,knumrj
      character (len=3) :: iyes
      character (len=2) :: lr,no
      character (len=1) :: iblk    
      logical :: wholnum,almost_equal
      real    :: y100b=0.0,y400b=0.0,y500b=0.0,y50b=0.0,y10b=0.0
!
!
      data lr /'LR'/, iyes /'YES'/, no /'NO'/, iblk /' '/
      save theta
!
      needtop = 0
!
      yend = yends
!          make a y_adjustment if tstrt not a whole second
      f = 0.0
      ybias = 0.0
      wholnum = .true.
      g=tstrt
      do while(g.le.(time+tstrt))
!!!!!!      do g = tstrt, time+tstrt
        
        almost_equal = ameq (tstrt, f, .00001)
        if (almost_equal) goto 60
        f = f + 1.0
        g=g+1.0
      enddo
      itmp = tstrt + .0001
      ftmp = itmp
      frac = abs (tstrt-ftmp)
      ybias = frac * fis
      if (tstrt .gt. 0.00001) ybias = (1.0 - frac) * fis
!          find bias to 500 mil timing line
      milts = tstrt * 1000.0
      mil500 = milts / 500 + 1
      mil500 = mil500 * 500
      s = real (mil500) / 1000.0
      y500b = abs (tstrt-s) * fis
!
      milts = tstrt * 1000.0
      mil400 = milts / 400 + 1
      mil400 = mil400 * 400
      s = real (mil400) / 1000.0
      y400b = abs (tstrt-s) * fis
!
      mil100 = milts / 100 + 1
      mil100 = mil100 * 100
      s = real (mil100) / 1000.0
      y100b = abs (tstrt-s) * fis
!
      mil50 = milts / 50 + 1
      mil50 = mil50 * 50
      s = real (mil50) / 1000.0
      y50b = abs (tstrt-s) * fis
!
      mil10 = milts / 10 + 1
      mil10 = mil10 * 10
      s = real (mil10) / 1000.0
      y10b = abs (tstrt-s) * fis
!
      wholnum = .false.
!
   60 continue
      if (wholnum) then
         fwhnum = tstrt
      else
         n = tstrt + 1.0
         fwhnum = real (n)
      endif
      otim = time
      rowylen = (otim + timbtwn) * fis
      if (lrrl(:2) .eq. lr) then
         rowylen = -rowylen
         ybias = -ybias
         y500b = -y500b
         y400b = -y400b
         y100b = -y100b
         y50b = -y50b
         y10b = -y10b
      endif
      if (irnum(:3) .eq. iyes) then
         rowylen = -rowylen
         ybias = -ybias
         y500b = -y500b
         y400b = -y400b
         y100b = -y100b
         y50b = -y50b
         y10b = -y10b
      endif
      theta = angle
      sec = fwhnum
      us = ius
      call colrplot_fnt (iunit,19)
      itldt(1) = it10
      itldt(2) = it50
      itldt(3) = it100
      itldt(4) = it500
      itldt(5) = it1000
      itldt(6) = it400
      gap = 0.12
      y1 = yo + ybias
      idot = 0
      dotsiz = .005
      if (theta .gt. 0.0) dotsiz = -0.005
!
!
      yy1 = yo
      xx1 = xo - gap
      xx2 = xend + gap
      grdbias = 0.0
      kntrow = 0
      kntr = 0
      krow = 1
      if (nrow .gt. 1) then
         kntrow = nrow
         ypanlen = (time + timbtwn) * fis
         if (lrrl(:2) .eq. lr) ypanlen = -ypanlen
      endif
      xz = xx2
      num = 1
      lnum = 0
      if (irtbp(:3) .eq. iyes) then
         if (ntrph .gt. 1) grdbias = twid / ntrph * (ntrph - 1)
         xz = (ibtskp(1) + 1) * twid + xo - grdbias
         num = ibttot(1)
         lnum = 1
      endif
      sizl = tncs
      sizs = sizl / 2.0
      x2l = xx2 + 8.0 * sizl * 0.85
      x2s = xx2 + 8.0 * sizs * 0.85
      x1l = xx1 - 8.0 * sizl * 0.85
      x1s = xx1 - 8.0 * sizs * 0.85

      if (ius .ne. 0) then
         sizs = tncs
         x2s = x2l
         x1s = x1l
      endif
      ndec = 1
      nsec = time + 1.0 + tstrt
      itnum = 0
      ftime = time + tstrt
!
!          reset y-coordinate if plotting numbers in reverse
      if (irnum(:2) .eq. no) goto 220
      yy1 = yend
      y1 = yy1 + ybias
  220 continue
!
!          insure a line drawn at top of section
      almost_equal = ameq (yy1, y1, 0.001)
      if (.not.almost_equal) then
         needtop = 1
         ydot = yy1
         ytop = yy1
         xxt = xx1
         xt = xz
         ndtmp = itldt(5)
         if (ndtmp .eq. 0) then
            do k = 4, 1, - 1
              if (ndtmp .eq. 0) ndtmp = itldt(k)
            enddo
            if (ndtmp .eq. 0) ndtmp = itldt(6)
         endif
         do m = 1, nrow
           do k = 1, num + lnum

              if (k .eq. (num+lnum) .and. m .gt. nrlc)cycle
              do j = 1, ndtmp
                 call colrplot_plot(iunit,xxt, ydot, 3)
                 call colrplot_plot(iunit,xt, ydot, 2)
              ydot = ydot + dotsiz
              end do
              xxt = xt + (ibtdo(1) - 1) * twid
              xt = (ibtbtwn(1) + 1) * twid + xxt - grdbias
              ydot = ytop
           end do
           ytop = ytop + ypanlen
           ydot = ytop
           xxt = xx1
           xt = xz
         enddo
      endif
      if (ius .eq. 0) goto 250
!
!          plot units other than time
      sec = fwhnum * real (ius)
      ftime = ftime * real (ius) + .0001
      ndec = 0

  250 continue
      secsav = sec
      if (num .eq. 0 .and. krow .gt. nrlc .and. nrow .gt. 1) goto 510
      DO 500 ii = 1, 6
         if (itldt(ii) .eq. 0)cycle
         ndots = itldt(ii)
!
         SELECT CASE(ii)
!!         goto (275, 440, 460, 480, 490, 495), ii
!
!          10 mil timing lines
         CASE(1)
         ysize = fis / 100.
         ysizes = ysize
         ntlne = time * 100. + 1.0001
         idot = it10
         ntlne = (time - abs (y10b) / fis) * 100. + 1.0001
         y1 = yy1 + y10b
         go to 280
!
!
!
         CASE(2)
!          50 mil timing lines
         if (it50 .le. idot)go to 500
         ysize = fis / 20.
         ntlne = time * 20. + 1.0001
         y1 = yy1
         idot = it50
         ntlne = (time - abs (y50b) / fis) * 20. + 1.0001
         y1 = yy1 + y50b
         goto 280
!
         CASE(3)
!
!          100 mil timing lines
         if (it100 .le. idot .and. itnum .eq. 1)go to 500
         idot = it100
         ysize = fis / 10.
         y1 = yy1
         ntlne = (time - abs (y100b) / fis) * 10. + 1.0001
         y1 = yy1 + y100b
!
!          set timing line number parameters
         yinc = fis / 5.
         secinc = .2
         if (ius .ne. 0) secinc = us / 5.0
         nsec2 = 4
         itnum = 1
         goto 280
!
         CASE(4)
!
!          500 mil timing lines
         if (it500 .le. idot)go to 500
         ysize = fis / 2.
         idot = it500
         ntlne = (time - abs (y500b) / fis) * 2. + 1.0001
         y1 = yy1 + y500b
!
!            set timing line number parameters if not already set
         if (itnum .eq. 1) goto 280
         nsec2 = 1
         yinc = fis / 2.
         secinc = 0.5
         if (ius .ne. 0) secinc = us / 2.0
         itnum = 1
         goto 280
!
         CASE(5)
!
!            1000 mil timing lines
         if (it1000 .le. idot)go to 500
         ysize = fis
         ntlne = (time - abs (ybias) / fis) + 1.0001
         y1 = yy1 + ybias
!
!            set timing line number parameters if not already set
         if (itnum .eq. 1) goto 280
         yinc = fis
         secinc = 1.0
         if (us .ne. 0.0) secinc = us
         itnum = 1
         goto 280
!
         CASE(6)
!
!            400 mil timing lines
         if (it400 .le. idot)go to 500
         ysize = fis / 2.5
         ntlne = (time - abs (y400b) / fis) * 2.5 + 1.0001
         y1 = yy1 + y400b
!
!            set timing line number parameters if not already set
         if (itnum .eq. 1) goto 280
         yinc = fis / 2.5
         secinc = .4
         if (us .ne. 0.0) secinc = us / 2.5
         nsec = ntlne
         nsec2 = 0
         itnum = 1
         goto 280
        END SELECT
!
!          draw timing lines
  280    continue
         if (lrrl(:2) .eq. lr) then
            ysize = -ysize
         endif
         if (irnum(:3) .eq. iyes) then
            ysize = -ysize
         endif
         do 420 i = 1, ntlne
            if (itico(:3) .eq. iyes) goto 380
            ydot = y1
            do 320 j = 1, ndots
               call colrplot_plot(iunit,xx1, ydot, 3)
               call colrplot_plot(iunit,xz, ydot, 2)
               ydot = ydot + dotsiz
  320       end do
            goto 400
  380       continue
            ydot = y1
            do 390 j = 1, ndots
               call colrplot_plot(iunit,(xx1), ydot, 3)
               call colrplot_plot(iunit,xo, ydot, 2)
               call colrplot_plot(iunit,(xx2), ydot, 3)
               call colrplot_plot(iunit,xend, ydot, 2)
               ydot = ydot + dotsiz
  390       end do
  400       y1 = y1 + ysize
            if (lrrl(:2) .eq. lr) then
               if (y1 .lt. yend)go to 500
            else
               if (y1 .gt. yend) goto 500
            endif
  420    end do
  500 end do
  510 continue
      if (itnum .eq. 0 .and. kntrow .eq. 1) goto 515
      if (itnum .eq. 0 .and. kntrow .eq. 0) goto 8000
      idot = 0
!
      if (irtbp(:2) .eq. no .and. kntrow .gt. 0) kntrow = kntrow - 1
      if (irtbp(:3) .eq. iyes) then
         num = num - 1
!                                               ! does timing lines
                                                !  row by row
         if (num .ge. 0) then
!           reset xz from blank trace array
            xx1 = xz + (ibtdo(1) - 1) * twid
            xz = (ibtbtwn(1) + 1) * twid + xx1 - grdbias
            itnum = 0
            goto 250
               ! num.ge.0
         endif
         kntrow = kntrow - 1
         krow = krow + 1
         if (kntrow .gt. 0) then
            if (lrrl(:2) .eq. lr) then
                                              ! num is -1
               yy1 = yy1 - (timbtwn + otim) * fis
               yend = yy1 - otim * fis
                                              !  go to next row
            else
               yy1 = yy1 + (timbtwn + otim) * fis
               yend = yy1 + otim * fis
                ! (lrrl(:2) .eq. lr)
            endif
            y1 = yy1
            xx1 = xo - gap
            xz = (ibtskp(1) + 1) * twid+xo - grdbias
            num = ibttot(1)
            itnum = 0
            goto 250
              ! (kntrow .ge. 0)
         endif
      elseif (kntrow .gt. 0) then
         if (lrrl(:2) .eq. lr) then
            yy1 = yy1 - (timbtwn + otim) * fis
            yend = yy1 - otim * fis
         else
            yy1 = yy1 + (timbtwn + otim) * fis
            yend = yy1 + otim * fis
         endif
         y1 = yy1
         if (needtop .eq. 1) then
            ydot = yy1
            do j = 1, ndtmp
              call colrplot_plot(iunit,xx1, ydot, 3)
              call colrplot_plot(iunit,xz, ydot, 2)
              ydot = ydot + dotsiz
            enddo
         endif
         goto 250
            ! (irtbp(:3) .eq. iyes)
      endif
!
  515 continue
      if (itlbl3 .gt. 0) goto 8000
!
!          plot timing line number
      xx1 = xo - gap
      if (lrrl(:2) .eq. lr) then
         yinc = -yinc
      endif
      if (irnum(:3) .eq. iyes) then
         yinc = -yinc
      endif
      kntrow = 0
      krow = 1
      if (nrow .gt. 1) kntrow = nrow
      if (irnum(:2) .eq. no) then
         yy1 = yo
      else
         yy1 = yend
      endif
  516 if (.not.wholnum) then
         t = tstrt
         if (ius .ne. 0) t = t * us
         call string_ff2cc(t,knum,ndec=ndec)
         call string_ff2cc(t,knumrj,ndec=ndec)
         knumrj=adjustr(knumrj)
         if (ius .ne. 0) then
            knumrj(8:8)=iblk
            knumrj=adjustr(knumrj)
         endif
         nc=len_trim(knum)
         if (ndec .eq. 0) nc = nc - 1
         if (lrrl(:2) .eq. lr) then
            call colrplot_settxt (iunit,19, 0, 3, 4, 1.0, 0.0, 0)
            call colrplot_sym (iunit,xx1, yy1, sizs, knumrj, theta, 8)
            call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
            if (krow .le. nrlc .or. nrow .eq. 1) &
              call colrplot_sym (iunit,xx2, yy1, sizs, knum, theta, nc)
         else
            call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
            call colrplot_sym (iunit,xx1, yy1, sizs, knum, theta, nc)
            call colrplot_settxt (iunit,19, 0, 3, 4, 1.0, 0.0, 0)
            if (krow .le. nrlc .or. nrow .eq. 1) &
              call colrplot_sym (iunit,xx2, yy1, sizs, knumrj, theta, 8)
         endif
!          label multiples of 200 mils from tstrt to fwhnum
         milts = tstrt * 1000.0
         mil200 = milts / 200 + 1
         mil200 = mil200 * 200
         mfwhnum = fwhnum * 1000.0
         s = real (mil200) / 1000.0
         sinc = 0.2
         dif = abs (tstrt-s) * fis
         if (ius .ne. 0) then
            s = s * us
            sinc = us / 5.0
         endif
         yi = fis / 5.0
         if (lrrl(:2) .eq. lr) then
            y = yy1 - dif
            yi = - yi
            if (irnum(:3) .eq. iyes) y = yy1 + dif
         else
            y = yy1 + dif
            if (irnum(:3) .eq. iyes) y = yy1 - dif
         endif
         if (irnum(:3) .eq. iyes) then
            yi = -yi
         endif
         do 517 j = mil200, mfwhnum-1, 200
            call string_ff2cc(s,knum,ndec=ndec)
            call string_ff2cc(s,knumrj,ndec=ndec)
            knumrj=adjustr(knumrj)
            if (ius .ne. 0) then
               knumrj(8:8)=iblk
               knumrj=adjustr(knumrj)
            endif
            nc=len_trim(knum)
            if (ndec .eq. 0) nc = nc - 1
            if (lrrl(:2) .eq. lr) then
               call colrplot_settxt (iunit,19, 0, 3, 4, 1.0, 0.0, 0)
               call colrplot_sym (iunit,xx1, y, sizs, knumrj, theta, 8)
               call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
               if (krow .le. nrlc .or. nrow .eq. 1) &
                 call colrplot_sym (iunit,xx2, y, sizs, knum, theta, nc)
            else
               call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
               call colrplot_sym (iunit,xx1, y, sizs, knum, theta, nc)
               call colrplot_settxt (iunit,19, 0, 3, 4, 1.0, 0.0, 0)
               if (krow .le. nrlc .or. nrow .eq. 1) &
                 call colrplot_sym (iunit,xx2, y, sizs, knumrj, theta, 8)
            endif
            s = s + sinc
            y = y + yi
            if (lrrl(:2) .eq. lr) then
               if (y .lt. yend) goto 610
            else
               if (y .gt. yend) goto 610
            endif
  517    end do
      endif   ! if(.not.wholnum)
      yy1 = yy1 + ybias
!
      do 550 i = 1, nsec
         call color_f2char(sec,knumrj,ndec,0)
         if (ius .ne. 0) then
            knumrj(8:8)=iblk
            knumrj=adjustr(knumrj)
         endif
         call color_f2char(sec,knum,ndec,1)
         nc=len_trim(knum)
         if (ndec .eq. 0) nc = nc - 1
         if (lrrl(:2) .eq. lr) then
            call colrplot_settxt (iunit,19, 0, 3, 4, 1.0, 0.0, 0)
            call colrplot_sym (iunit,xx1, yy1, sizl, knumrj, theta, 8)
            call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
            if (krow .le. nrlc .or. nrow .eq. 1) &
              call colrplot_sym (iunit,xx2, yy1, sizl, knum, theta, nc)
         else
            call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
            call colrplot_sym (iunit,xx1, yy1, sizl, knum, theta, nc)
            call colrplot_settxt (iunit,19, 0, 3, 4, 1.0, 0.0, 0)
            if (krow .le. nrlc .or. nrow .eq. 1) &
              call colrplot_sym (iunit,xx2, yy1, sizl, knumrj, theta, 8)
         endif
         sec = sec + secinc
         if (sec .ge. ftime)then
           goto 600
         endif
         yy1 = yy1 + yinc
!
         do j = 1, nsec2
            call string_ff2cc(sec,knum,ndec=ndec)
            call string_ff2cc(sec,knumrj,ndec=ndec)
            knumrj=adjustr(knumrj)
            if (ius .ne. 0) then
               knumrj(8:8)=iblk
               knumrj=adjustr(knumrj)
            endif
            nc=len_trim(knum)
            if (ndec .eq. 0) nc = nc - 1
            if (lrrl(:2) .eq. lr) then
               call colrplot_settxt (iunit,19, 0, 3, 4, 1.0, 0.0, 0)
               call colrplot_sym (iunit,xx1, yy1, sizs, knumrj, theta, 8)
               call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
               if (krow .le. nrlc .or. nrow .eq. 1) &
                 call colrplot_sym (iunit,xx2, yy1, sizs, knum, theta, nc)
            else
               call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
               call colrplot_sym (iunit,xx1, yy1, sizs, knum, theta, nc)
               call colrplot_settxt (iunit,19, 0, 3, 4, 1.0, 0.0, 0)
               if (krow .le. nrlc .or. nrow .eq. 1) &
                 call colrplot_sym (iunit,xx2, yy1, sizs, knumrj, theta, 8)
            endif
            sec = sec + secinc
            yy1 = yy1 + yinc
            if (sec .gt. (ftime+.0001))then
               goto 600
            endif
         end do
!          reference theta mickey-mouse
         write (cdum, '(F4.0)') theta
  550 end do
!
  600 continue
      kntrow = kntrow - 1
      kntr = kntr + 1
      krow = krow + 1
      if (kntrow .gt. 0) then
         yy1 = yo + rowylen * kntr
         sec = secsav
         goto 516
      endif
!
!          if paneling - plot times that fall between panels
  610 continue
      if (irtbp(:2) .eq. no) goto 8000
      kntrow = 0
      krow = 1
      kntr = 0
      if (nrow .gt. 0) kntrow = nrow
      if (kntrow .eq. 0) goto 8000
      yy1 = yo
      if (irnum(:3) .eq. iyes) then
         yy1 = yend
         y1 = yy1
      endif
      yy1sav = yy1
      s = real (ius)
      call string_ff2cc(s,knum,ndec=ndec)
      nc=len_trim(knum)
      if (lrrl(:2) .eq. lr) then
         xml = real (ibtskp(1)) * twid + gap - grdbias
         xms = xml
      else
         xml = (real (ibtskp(1)+ibtdo(1))) * twid-gap - grdbias
         xms = xml
      endif
      call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
      s = tstrt
      if (ius .ne. 0) then
         s = s * real (ius)
      else
         xms = xml
      endif
      sec = fwhnum
      ssav = s
      xml = xml + xo
      xms = xms + xo
      xinc = (ibtbtwn(1) + ibtdo(1)) * twid - grdbias
      num = ibttot(1)
!
  620 continue
      if (.not.wholnum) then
         call string_ff2cc(s,knum,ndec=ndec)
         call string_ff2cc(s,knumrj,ndec=ndec)
         knumrj=adjustr(knumrj)
         if (ius .ne. 0) then
            knumrj(8:8)=iblk
            knumrj=adjustr(knumrj)
         endif
         nc=len_trim(knum)
         if (ndec .eq. 0) nc = nc - 1
         call colrplot_sym (iunit,xms, yy1, sizs, knum, theta, nc)
!
!          label multiples of 200 mils from tstrt to fwhnum
         s = real (mil200) / 1000.0
         sinc = 0.2
         dif = abs (tstrt-s) * fis
         if (ius .ne. 0) then
            s = s * us
            sinc = us / 5.0
         endif
         yi = fis / 5.0
         if (lrrl(:2) .eq. lr) then
            y = yy1 - dif
            yi = - yi
            if (irnum(:2) .eq. iyes) y = yy1 + dif
         else
            y = yy1 + dif
            if (irnum(:2) .eq. iyes) y = yy1 - dif
         endif
         if (irnum(:2) .eq. iyes) then
            yi = -yi
         endif
         do j = mil200, mfwhnum - 1, 200
           call string_ff2cc(s,knum,ndec=ndec)
           nc=len_trim(knum)
           if (ndec .eq. 0) nc = nc - 1
           call colrplot_sym (iunit,xms, y, sizs, knum, theta, nc)
           s = s + sinc
           y = y + yi
           if (lrrl(:2) .eq. lr) then
              if (y .lt. yend) goto 655
           else
              if (y .gt. yend) goto 655
           endif
         enddo
                  ! if(.not.wholnum)
      endif
      yy1 = yy1 + ybias
!
      do i = 1, nsec
         call color_f2char(sec,knum,ndec,1)
         nc=len_trim(knum)
         if (ndec .eq. 0) nc = nc - 1
         if (sec .gt. ftime)cycle
         call colrplot_sym (iunit,xml, yy1, sizl, knum, theta, nc)
         sec = sec + secinc
         yy1 = yy1 + yinc
         do j = 1, nsec2
            call string_ff2cc(sec,knum,ndec=ndec)
            nc=len_trim(knum)
            if (ndec .eq. 0) nc = nc - 1
            if (sec .gt. ftime)cycle
            call colrplot_sym (iunit,xms, yy1, sizs, knum, theta, nc)
            sec = sec + secinc
            yy1 = yy1 + yinc
         end do
      end do
  655 continue
      kntrow = kntrow - 1
      krow = krow + 1
      kntr = kntr + 1
      if (kntrow .gt. 0) then
         yy1 = yo + rowylen * kntr
         sec = secsav
         s = ssav
         goto 620
      endif
!
      num = num - 1
      if (num .gt. 0) then
         kntrow = nrow
         krow = 1
         kntr = 0
         yy1 = yo
         sec = secsav
         s = ssav
         if (irnum(:3) .eq. iyes) then
            yy1 = yend
            y1 = yy1
         endif
         xml = xml + xinc
         xms = xms + xinc
         goto 620
      endif
!
 8000 continue
      call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
      return
      end subroutine color_tlne
      subroutine color_tie (iunit,xo, yo, csize, dist,tunit,numt,ltname,     &
       shcs, alpha, twid)
! this routine plots tie lines
!   xo     = the x coordinate of the section
!   yo     = the y coordinate of the top of the section
!   csize  = the height of the characters in inches
!   dist   = the distance above the section in inches
!   tunit  = units per trace
!   numt   = the number of trace that the tie is on
!   ltname = array containing the name of the line tie
!   shcs   = height of numbers
!   alpha  = angle to plot annatation
!    twid  = trace width in inches
!
      real,                intent(in) :: xo
      real,                intent(in) :: yo
      real,                intent(in) :: csize
      real,                intent(in) :: dist
      real,                intent(in) :: tunit
      integer,             intent(in) :: numt,iunit
      character (len = *), intent(in) :: ltname
      real,                intent(in) :: shcs
      real,                intent(in) :: alpha
      real,                intent(in) :: twid
!
      integer :: nc, nbkc
      real :: y1, y2, y4, y3, tang, bias, tnum, angn, cunit, x1, ylen, x5, y5
!
!          set up tie line angle of 20 and 200 degrees in radians
!              (rad=theta*pi/180.0)
      call colrplot_fnt (iunit,0)
      call colrplot_lwid(iunit,0.005)
      if (alpha.ne.0.0) then
         y1 = yo
         y2 = yo - .25
         y4 = yo - dist
         y3 = y4 + 0.1
         tang = 3.491
         bias = - .01
!
      else
!
         y1 = yo
         y2 = yo + .25
         y4 = yo + dist
         y3 = y4 - 0.1
         tang = .3490658
         bias = .01
      endif
!
      tnum = numt
      angn = alpha + 20
      cunit = csize
      nc = 40
      x1 = tunit * tnum + xo - twid
      call colrplot_plot(iunit,x1, y1, 3)
      call colrplot_plot(iunit,x1, y2, 2)
      call colrplot_plot(iunit,x1, y3, 3)
      call colrplot_plot(iunit,x1, y4, 2)
      nbkc=len_trim(ltname)
      ylen = nbkc * csize
      x5 = ylen * cos (tang) + x1
      y5 = ylen * sin (tang) + y4
      call colrplot_plot(iunit,x1, y4, 3)
      call colrplot_plot(iunit,x5, y5, 2)
      call colrplot_sym (iunit,x1, y4 + bias, csize, ltname, angn, nbkc)
      call colrplot_fnt (iunit,19)
      return
      end subroutine color_tie
      subroutine color_trce (obj,ctwid,ltr,xo,yor,yend,errflg,ntpp,  &
                             nspan,ntrph,iafter,jafter,nafter,ntg,iaskp,iado,&
                             iabtwn,method)
!
!
!
!          routine to plot trace overlay
!
!          obj%ntr_ovl      = total number of traces
!          obj%ips_eng      = inches per second
!          obj%tpi_ovl_eng  = traces per inch of the overlay traces
!          obj%otim         = number of seconds to plot
!          obj%lrrl         = left to right or right to left
!          obj%wt_ovl       = wiggle trace
!          obj%path_ovl     = strot file containing overlay traces
!          obj%ct_ovl       = channels per trace (color channels)
!          obj%trcb         = one-dimensional array to hold trace
!          irpt             = reverse polarity
!          obj%skip_init_ovl= number of traces to skip
!         ctwid             = trace width of color traces
!          obj%zt_ovl       = time in seconds of overlay trace to be plotted
!                             at zero time
!         obj%nctfo         = number of the color trace to plot the first
!                             overlay trace
!           hdr             = array holding trace header
!        datain             = array holding trace
!          obj%va_ovl       = variable area -yes or no
!         obj%dt            = sample rate
!          ltr              = number of points to use for lav calculation
!           xo              = x-origin of section
!           yor             = y-origin of section
!          yend             = y-coordinate of end of data
!          errflg           = return status (0 = normal return)
!          obj%nbts         = number of blank trace cards
!          obj%btskp        = number traces to skip
!          obj%btdo         = number blanks to do
!          obj%ibtwn        = number traces between
!          obj%bttot        = total in set
!          obj%opt_calc     = data plotted used in calculation of lav
!                              plot - use only data that is plotted
!                              all - use all data in the file
!          ntpp             = number of traces in a panel
!          obj%nrow         = number of rows of panels
!       obj%idn             = process number of dtrot file
!         nspan             = number of samples on a paneled trace
!        obj%nsbtwn         = number of samples between rows of panels
!         ntrph             = number in horizontal interpolation
!          obj%ncol         = number of columns of panels
!          obj%invert       = invert trace before plotting
!          obj%rp_ovl       = reverse polarity
!        iafter             = array containing the numbers of traces that 
!                             blanks will be inserted after
!        jafter             = array containing the graded number of traces that
!                             blanks will be inserted after
!        nafter             = the number of elements in iafter and ibefor
!        method             = method for determining the color trace for 
!                             placement of the overlay trace
!
      type(color_struct),intent(inout) :: obj       ! arguments

      real,                  intent(in)    :: ctwid
      integer,               intent(in)    :: ltr
      real,                  intent(in)    :: xo
      real,                  intent(in)    :: yor
      real,                  intent(in)    :: yend
      integer,               intent(out)   :: errflg
      integer,               intent(in)    :: ntpp
      integer,               intent(in)    :: nspan
      integer,               intent(in)    :: ntrph
      integer, dimension(nafter), intent(in)    :: iafter
      integer, dimension(nafter), intent(in)    :: jafter
      integer,               intent(in)    :: nafter
      integer,               intent(in)    :: ntg
      integer, dimension(*), intent(in)    :: iaskp
      integer, dimension(*), intent(in)    :: iado
      integer, dimension(*), intent(in)    :: iabtwn
      integer,               intent(in)    :: method
!
      integer :: istrt, nsamp, itr, ntr, inctwid, nctpp, ngctpp(1), igbdo(1), &
        igbtot(1), knt, k, i, idif, iform, ipfill, imfill,  &
        iw, k1, k2, n, i1, numpan, kntcol, ktrnum, j, igctn, igskp(1),     &
        igbtdo(1), igbtwn(1)
      real :: xs, xsav, datlen, twid, tti, gtwid, cgtwid, glav, trmax, scale,  &
        fmaxin, ybtwn, yadd, ys, xpmin, xpmax, xmmin, xmmax, trai, ftrg,    &
        tnnw, tra, gtrce
      character lr*2, iyes*3, no*2

      integer :: istat

      double precision :: hdr(obj%nwih)
      
      real :: datain(obj%ndpt)


      type(trcio_struct),pointer :: ipn1
!
      data lr /'LR'/, iyes /'YES'/, no /'NO'/
!
!
!          set color to black
      call colrplot_tclr (obj%iunit,1)
!          determine first sample point to plot
      istrt = obj%zt_ovl / obj%dt + .0001
      if (istrt .le. 0) istrt = 1
!
!
      xs = xo + (obj%nctfo - 1) * ctwid
      xsav = xs
      if (method .eq. 2) xsav = xo
      datlen = obj%otim * obj%ips_eng
      if (ntpp .eq. 0) then
      else
        istat=trcio_seek_trace(obj%idn,1)
      endif
      nsamp = obj%otim / obj%dt + .0001
      itr = 0
      ntr = obj%ntr_ovl + obj%skip_init_ovl + .0001
      twid = 1.0 / obj%tpi_ovl_eng
      inctwid = nint (twid/ctwid)
      tti = obj%tpi_ovl_eng * ntrph
      gtwid = 1.0 / tti
      cgtwid = ctwid / real (ntrph)
      if (ntpp .gt. 0 .and. ntrph .gt. 1) then
         twid = gtwid
         if (method .ne. 2) itr = 1 - inctwid
         nctpp = ntpp * inctwid
      endif
      ngctpp(1) = (obj%ntpp - 1) * ntrph + 1
      igbdo(1) = obj%btdo(1) * ntrph
!
      glav=0.0
!
!          open overlay file
      ipn1 => trcio_open(obj%path_ovl,'r')
      if(.not.associated(ipn1))then
        call pc_error('COLOR_trce - unable to open trace overlay file')
        errflg=1
        return
      endif
!
!          read through data and calculate lav
      knt = 0
 200  continue
      istat=trcio_read_trace(ipn1,hdr,datain)
      if(istat.eq.TRCIO_EOF)go to 230
      if (istat .ne. TRCIO_OK) go to 9001
      if (ntpp .gt. 0) then
!          write to file
         istat=trcio_write_trace(obj%idn,hdr,datain)
         if (istat .ne. TRCIO_OK) go to 9003
      endif
!         find the largest absolute value
      knt = knt + 1
      if (obj%opt_calc .eq. 'ALL') goto 210
      if (knt .le. obj%skip_init_ovl) goto 200
      if (knt .gt. ntr) goto 230
!          copy to trace work buffer
  210 k = istrt
      if (obj%opt_calc .eq.'ALL') k = 1
      do i = 1, ltr
         obj%trcb(i) = datain(k)
         k = k + 1
      end do
      trmax = abs (obj%trcb(mth_isamax(ltr,obj%trcb,1)))
      if (trmax .eq. 0) goto 200
      glav = amax1 (glav, trmax)
      goto 200
                                                       !start
  230 continue
      if (knt .lt. obj%ntr_ovl) then
         write(print_lun) ' ONLY ', knt, ' TRACES ON OVERLAY FILE'
         idif = obj%ntr_ovl - knt
         ntr = ntr - idif
         obj%ntr_ovl = knt
      endif
!
!
      if (glav .eq. 0.0) then
        write(print_lun,*)'COLOR_TRCE -> ALL OVERLAY TRACES ARE DEAD'
        stop
      end if
      scale = (obj%ct_ovl * 0.5) / (obj%tpi_ovl_eng * glav)  
!!      scale = (obj%ct_ovl * 0.5) / (obj%tpi_eng * glav) documented
                                                       !end
      fmaxin = glav * scale
      write(print_lun,*) ' LAV FROM TRACE OVERLAY DATA = ', glav
      obj%glav_ovl=glav
      write(print_lun,*) ' '
      istat=trcio_seek_trace(ipn1,1)
      if (ntpp .gt. 0)then
        istat=trcio_seek_trace(obj%idn,1)
      endif
!
!
!          set the seismic plotting parameters
!
      ybtwn = obj%nsbtwn * obj%dt * obj%ips_eng
      yadd = ybtwn + datlen
      if (obj%lrrl(:2) .eq. lr) then
         iform = 2
         ys = yend
         if (ntpp .gt. 0) then
            ys = yor - datlen
            yadd = - yadd
         endif
         if (obj%va_ovl(:3) .eq. iyes) then
            xpmin = abs (fmaxin*0.03)
            xpmax = abs (fmaxin*0.73)
            ipfill = 16
         else
            xpmin = 0.0
            xpmax = 0.0
            ipfill = 0.0
         endif
         xmmin = 0.0
         xmmax = 0.0
         imfill = 0
      else
         iform = 1
         ys = yor
         fmaxin = -fmaxin
         scale = -scale
         if (obj%va_ovl(:3) .eq. iyes) then
            xmmin = abs (fmaxin*0.03)
            xmmax = abs (fmaxin*0.73)
            imfill = 16
         else
            xmmin = 0.0
            xmmax = 0.0
            imfill = 0
         endif
         xpmin = 0.0
         xpmax = 0.0
         ipfill = 0
      endif
      if (obj%wt_ovl(:3) .eq. iyes) then
         iw = 1
      else
         iw = 0
      endif
      call colrplot_sets(obj%iunit,xmmin,xmmax,imfill,xpmin,xpmax,ipfill,&
                         iform,iw)
      k1 = obj%skip_init_ovl + 1
      k2 = k1
      n = (k1 - 1) + obj%nrow * ntpp
      i1 = 1
      numpan = obj%ncol * ntpp
      kntcol = 1
      ktrnum = 0
!
!          start first trace at x-origin
      knt = 0
  500 continue
      knt = knt + 1
      if (knt .gt. ntr) goto 2000
!
      if (nafter .gt. 0 .and. ntrph .gt. 1) then
         istat=trcio_read_trace(ipn1,hdr,datain)
         if (istat.eq.TRCIO_EOF) goto 2000
         if (istat .ne. TRCIO_OK) go to 9001
         if (knt .le. obj%skip_init_ovl) goto 500
         itr = itr + 1
         trai = real (itr)
         ftrg = color_2gab (trai, iafter, jafter, nafter, ntrph, ntg)
         call color_abt (ftrg, tnnw, obj%nbts,iaskp,iado,iabtwn,obj%bttot, 1)
         xs = (tnnw - 1.0) * cgtwid + xsav
         if (obj%rp_ovl(:3) .eq. iyes) call color_revpol (datain, nsamp)
         if (obj%invert(:3) .eq. iyes) call color_rvse (datain, nsamp)
         call colrplot_seis (obj%iunit,xs, ys, datain, nsamp, datlen, scale)
         goto 500
      elseif (ntpp .eq. 0) then
         istat=trcio_read_trace(ipn1,hdr,datain)
         if (istat.eq.TRCIO_EOF) goto 2000
         if (istat .ne. TRCIO_OK) go to 9001
         if (knt .le. obj%skip_init_ovl) goto 500
         itr = itr + 1
         tra = itr
         call color_abt (tra,tnnw,obj%nbts,obj%btskp,obj%btdo,obj%btbtwn,&
                         obj%bttot,1,ntpp)
         xs = (tnnw - 1.0) * twid + xsav
         if (obj%rp_ovl(:3) .eq. iyes) call color_revpol (datain, nsamp)
         if (obj%invert(:3) .eq. iyes) call color_rvse (datain, nsamp)
         call colrplot_seis (obj%iunit,xs, ys, datain, nsamp, datlen, scale)
         goto 500
      else
                              ! one column
         do 508 j = 1, ntpp
!           panel one trace
            if (method .eq. 2) then
               itr = itr + 1
               n=ngctpp(1)
               igctn = color_2gpm2(n,obj%ntpp,ntpp,inctwid,ntrph,itr)
               tnnw = igctn
               igbtot(1) = obj%ncol - 1
               call color_abt (tnnw,gtrce,1,ngctpp,igbdo,ngctpp,igbtot,1)
               gtrce = gtrce + (obj%nctfo - 1) * ntrph
               xs = (gtrce - 1.0) * cgtwid + xsav
            else
               itr = itr + inctwid
               tra = itr
               gtrce = tra
               igskp (1) = obj%btskp (1)
               igbtdo(1) = obj%btdo  (1)
               igbtwn(1) = obj%btbtwn(1)
               if (ntpp .gt. 0 .and. ntrph .gt. 1) then
                  call color_t2gp (tra, gtrce, ntrph, nctpp)
                  igskp (1) = obj%btskp (1) * ntrph - (ntrph - 1)
                  igbtdo(1) = obj%btdo  (1) * ntrph
                  igbtwn(1) = obj%btbtwn(1) * ntrph - (ntrph - 1)
               endif
               call color_abt (gtrce, tnnw, obj%nbts, igskp, igbtdo, igbtwn,  &
                 obj%bttot, 1)
               xs = (tnnw - 1.0) * cgtwid + xsav
            endif
                              ! obj%nrow traces
            do 507 i = k1, n, ntpp
               if (i .gt. ntr) goto 507
               istat=trcio_read_trace(obj%idn,hdr,obj%trcb,i)
               if (obj%rp_ovl(:3) .eq. iyes) call color_revpol(obj%trcb,nsamp)
               if (obj%invert(:3) .eq. iyes) call color_rvse(obj%trcb,nsamp)
               call colrplot_seis (obj%iunit,xs,ys,obj%trcb,nsamp,datlen,scale)
               ys = ys + yadd
               ktrnum = ktrnum + 1
  507       end do
            if (obj%lrrl(:2) .eq. lr) then
               ys = yor - datlen
            else
               ys = yor
            endif
            k1 = k1 + 1
  508    end do
         if (ktrnum .lt. obj%ntr_ovl) then
            k2 = k2 + ntpp * obj%nrow
            k1 = k2
            n = (k1 - 1) + obj%nrow * ntpp
            kntcol = kntcol + 1
            goto 500
              ! ktrnum lt obj%ntr_ovl
         endif
              ! nttp eq 0
      endif
!
 2000 continue
      istat=trcio_seek_trace(ipn1,1)
      go to 9005
 9001 continue
      write(print_lun) ' ERROR READING OVERLAY TRACES '
      go to 9005
 9003 write(print_lun) ' COLOR_TRCE => ERROR WRITING DTROT OVERLAY FILE'
 9005 return
      end subroutine color_trce
      subroutine color_t2gp (tra, gptra, ntrp, ntpp)
!
!
!          convert ungraded trace number to graded paneled trace number
!
!
!            tra    =  ungraded trace number
!            gptra  =  graded paneled trace number   - returned
!            ntrp   =  interpolation multiplier
!            ntpp   =  number of ungraded traces in a panel
!
      real,    intent(in)  :: tra
      real,    intent(out) :: gptra
      integer, intent(in)  :: ntrp
      integer, intent(in)  :: ntpp
!
      integer :: itra, icol, k
!
      itra = nint (tra)
      icol = itra / ntpp + 1
      k = mod (itra, ntpp)
      if (k.eq.0) icol = icol - 1
      gptra = itra * ntrp - (ntrp - 1) * icol
!
!
      return
      end subroutine color_t2gp
      subroutine color_wrda (iunit,iwrda, iwrdb, xo, yo,yend,nshots,trce,    &
      intv, itotl, shcs, wbcs, itmsh, tmwd, alpha, ltab, nbts, ibtskp,  &
      ibtdo, ibtbtwn, ibttot, ntrph, ntpp, ipnstro, nwih, panl, nval, &
      kstrt, kntb, twidu, twidg, lwda, lwdb, xend, nrow, time, timbtwn, &
      fis, ncol, nrlc, istat, iafter, jafter, nafter, ntg, iatb1)
!
!
!
!          iwrda  =  header number of wrda label
!          iwrdb  =  header number of wrdb label
!          xo     =  x-origin of section
!          yo     =  y-origin of section
!          yend   =  y-end of section
!          nshots =  number of shot point cards
!          trce   =  trace number to start labeling shot points
!          intv   =  number of trace intervals between shot labels
!          itotl  =  total number of shot points to label
!          shcs   =  wrda character size
!          wbcs   =  wrdb character size
!          itmsh  =  draw vertical line down section at wrda-wrdb
!                    locations (yes or no)
!          tmwd   =  width of itmsh lines in inches
!          alpha  =  angle to plot annotation
!          ltab   =  label both top and bottom of section (yes or no)
!          nbts   =  number of blank trace cards
!          ibtskp =  number of traces to skip before first blank
!          ibtdo  =  number of blanks to do
!        ibtbtwn  =  number of traces between blanks
!         ibttot  =  total number of sets this pattern
!          ntrph  =  horizontal interpolation factor
!          ntpp   =  number of traces per panel
!        ipnstro  =  pointer to trcio file 
!          nwih   =  number of words in trace header
!          panl   =  array to hold trace values
!          nval   =  number of values in panl
!          kstrt  =  hollerith istrt parameter - if = shot, label
!                    wrdb above shot point array labels
!          kntb   =  number of trace intervals between wrda-wrdb labels
!          twidu  =  width of one trace in inches ungraded
!          twidg  =  width of one trace in inches graded
!          lwda   =  label to define wrda plotted above zero time
!          lwdb   =  label to define wrdb plotted above zero time
!          xend   =  x-end of the section
!          nrow   =  number of rows of panels
!          time   =  time in one panel
!         timbtwn =  time between panels
!          fis    =  inches per second
!          ncol   =  number of columns of panels
!          nrlc   =  number of rows in the last column
!          istat  =  return status (0 = normal)
!          iafter =  array containing ungraded trace numbers that blanks
!                    are inserted after
!          jafter =  array containing the graded trace numbers that
!                    blanks are inserted after
!          nafter =  number of elements in iafter and jafter
!          ntg    =  the total number of graded traces
!          iatb1  =  flag to indicate tone processing
!                    0   = not tone
!                    > 0 = tone
!
      integer,               intent(in)  :: iwrda,iunit
      integer,               intent(in)  :: iwrdb
      real,                  intent(in)  :: xo
      real,                  intent(in)  :: yo
      real,                  intent(in)  :: yend
      integer,               intent(in)  :: nshots
      real,    dimension(*), intent(in)  :: trce
      integer, dimension(*), intent(in)  :: intv
      integer, dimension(*), intent(in)  :: itotl
      real,                  intent(in)  :: shcs
      real,                  intent(in)  :: wbcs
      character (len = *),   intent(in)  :: itmsh
      real,                  intent(in)  :: tmwd
      real,                  intent(in)  :: alpha
      character (len = *),   intent(in)  :: ltab
      integer,               intent(in)  :: nbts
      integer, dimension(*), intent(in)  :: ibtskp
      integer, dimension(*), intent(in)  :: ibtdo
      integer, dimension(*), intent(in)  :: ibtbtwn
      integer, dimension(*), intent(in)  :: ibttot
      integer,               intent(in)  :: ntrph
      integer,               intent(in)  :: ntpp
      type(trcio_struct),pointer :: ipnstro
      integer                            :: nwih
      real,                  intent(out) :: panl(nval)
      integer,               intent(in)  :: nval
      character (len = *),   intent(in)  :: kstrt
      integer,               intent(in)  :: kntb
      real,                  intent(in)  :: twidu
      real,                  intent(in)  :: twidg
      character (len = *),   intent(in)  :: lwda
      character (len = *),   intent(in)  :: lwdb
      real,                  intent(in)  :: xend
      integer,               intent(in)  :: nrow
      real,                  intent(in)  :: time
      real,                  intent(in)  :: timbtwn
      real,                  intent(in)  :: fis
      integer,               intent(in)  :: ncol
      integer,               intent(in)  :: nrlc
      integer,               intent(out) :: istat
      integer,               intent(in)  :: iafter(nafter)
      integer,               intent(in)  :: jafter(nafter)
      integer,               intent(in)  :: nafter
      integer,               intent(in)  :: ntg
      integer,               intent(in)  :: iatb1
!
      integer :: ntplot, knttone,igskp(1), igbtdo(1), igbtwn(1), kntcol, &
        intb, ndxwrdb, kntwrdb, iwrbtot, ntppg, ntrpg, nca, ncb, lxabb,    &
        istrt, istrtsv, kendpnl, k, ntplotg, i, ndx, numa, &
        numb, nc
      real :: ydatlen, ypanlen, pcnt, gap, gtwid, ydel, ysdel, yedel, ysdel2, &
        sizlwd, xlaba, xlabb, xlab, ywrda, xlab2, ywrdb, gstrt, gntplot,   &
        y1, y2, tnnw, tra, gtrce, xx, ty1, ty2, fnc, x2, xbias
      character iyes*3, iblk*1, knum*8, ktmp*8

      logical :: lr

      double precision :: header(nwih*nrow)
!
      data iyes /'YES'/, iblk /' '/ 

      istat=trcio_seek_trace(ipnstro,1)
      ntplot = 0
      knttone = 0
      kntcol = 1
      ydatlen = time * fis
      if (kstrt(:4) .eq. 'SHOT') then
         intb = intv(1)
      else
         intb = kntb
      endif
      ndxwrdb = 0
      kntwrdb = 0
      iwrbtot = 0
      lr = ameq(alpha, 0.0, .001)
      if (lr) ydatlen = -ydatlen
      ypanlen = ydatlen
      if (nrow .gt. 1) then
         ypanlen = (time + timbtwn) * fis
         ydatlen = time * fis
         if (lr) then
            ypanlen = -ypanlen
            ydatlen = -ydatlen
         endif
      endif
      pcnt = .85
      gap = .2
      gtwid = twidu
      ntppg = ntpp
      ntrpg = ntrph
      ydel = .2
      ysdel = .3
      yedel = ysdel + (shcs + .1)
      ysdel2 = ysdel + shcs
      if (alpha .gt. 0.0) then
         ydel = -ydel
         ysdel = -ysdel
         ysdel2 = -ysdel2
         yedel = -yedel
      endif
!
      if (lwda(1:1) .ne. iblk .or. lwdb(1:1) .ne. iblk) then
         nca=len_trim(lwda)
         ncb=len_trim(lwdb)
         sizlwd = 0.1
                         !  left to right
         if (lr) then
!               need to right justify first label
            call colrplot_settxt (iunit,19, 0, 3, 4, 1.0, 0.0, 0)
            xlaba = xo - gap
            xlabb = xo - gap
            xlab = xlaba
            ywrda = yo + ysdel
            call colrplot_sym (iunit,xlab, ywrda, sizlwd, lwda, alpha, nca)
                                                ! left justify
            call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
            xlab2 = xend + gap
            call colrplot_sym (iunit,xlab2, ywrda, sizlwd, lwda, alpha, nca)
            if (lwdb(1:1) .ne.iblk) then
               ywrdb = yo + yedel
                                                ! right justify
               call colrplot_settxt (iunit,19, 0, 3, 4, 1.0, 0.0, 0)
               call colrplot_sym (iunit,xlab, ywrdb, sizlwd, lwdb, alpha, ncb)
                                                  ! left justify
               call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
               call colrplot_sym (iunit,xlab2, ywrdb, sizlwd, lwdb, alpha, ncb)
            endif
         endif
                         !  right to left
         if (.not.lr) then
!            need to right justify last label
            call colrplot_settxt (iunit,19, 0, 3, 4, 1.0, 0.0, 0)
            xlaba = xend + gap
            lxabb = xend + gap
            xlab = xlaba
            ywrda = yo + ysdel
            call colrplot_sym (iunit,xlab, ywrda, sizlwd, lwda, alpha, nca)
            xlab2 = xo - gap
!               set back to left justify
            call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
            call colrplot_sym (iunit,xlab2, ywrda, sizlwd, lwda, alpha, nca)
            if (lwdb(1:1) .ne. iblk) then
               ywrdb = yo + yedel
                                                ! right justify
               call colrplot_settxt (iunit,19, 0, 3, 4, 1.0, 0.0, 0)
               call colrplot_sym (iunit,xlab, ywrdb, sizlwd, lwdb, alpha, ncb)
                                                ! left justify
               call colrplot_settxt (iunit,19, 0, 1, 4, 1.0, 0.0, 0)
               call colrplot_sym (iunit,xlab2, ywrdb, sizlwd, lwdb, alpha, ncb)
            endif
         endif
!
      endif
!
!
      if (nafter .gt. 0 .and. ntrph .gt. 1) then
         gtwid = twidg
      endif
      if (ntpp .gt. 0 .and. ntrph .gt. 1) then
         gtwid = twidg
         ntppg = ntpp * ntrph - (ntrph - 1)
         ntrpg = 1
      endif
      if (kstrt(:4) .eq. 'SHOT') then
         istrt = trce(1)
         if (nafter .gt. 0) then
            gstrt = color_2gab (real(istrt), iafter, jafter, nafter,     &
              ntrph, ntg)
            istrt = nint (gstrt)
         endif
      else
         call string_cc2ii(kstrt,istrt)
      endif
      if (iwrda .gt. 0 .or. iwrdb .gt. 0) then
         istrtsv = istrt
         kendpnl = ntpp
      endif
 100  continue
      istat=trcio_read_trace(ipnstro,header,panl)
      if (istat.eq.TRCIO_EOF) goto 8000
      if (istat .ne. 0) go to 9501
!        if tone processing - use header of every other trace
      if (iatb1 .ne. 0) then
         knttone = knttone + 1
         k = mod (knttone, 2)
         if (k .ne. 0) goto 100
      endif
      ntplot = ntplot + 1
      if (nafter .gt. 0 .and. kstrt(:4) .eq. 'SHOT') then
         gntplot = color_2gab (real(ntplot), iafter, jafter, nafter,     &
           ntrph, ntg)
         ntplotg = nint (gntplot)
      else
         ntplotg = ntplot
      endif
      if (iwrda .eq. 0 .and. iwrdb .eq. 0) goto 8000
      if (ntplotg .lt. istrt) goto 100
      if (kstrt(:4) .ne. 'SHOT') istrt = istrt + intb
      if (kstrt(:4) .eq. 'SHOT' .and. ndxwrdb .gt. nshots) goto 8000
      if (kntwrdb .ge. iwrbtot .and. kstrt(:4) .eq. 'SHOT') then
         ndxwrdb = ndxwrdb + 1
         kntwrdb = 0
         istrt = nint (trce(ndxwrdb))
         if (nafter .gt. 0) then
            gstrt=color_2gab(real(istrt),iafter,jafter,nafter,ntrph, ntg)
            istrt = nint (gstrt)
         endif
         if (ndxwrdb .gt. nshots) goto 8000
         intb = intv (ndxwrdb)
         iwrbtot = itotl(ndxwrdb)
         if (ntplotg .lt. istrt) goto 100
      endif
      kntwrdb = kntwrdb + 1
      y1 = yo
      if (lr) then
         y2 = yo + .25
      else
         y2 = yo - .25
      endif
      tnnw = ntplotg
      tra = tnnw
      gtrce = tra
      igskp (1) = ibtskp(1)
      igbtdo(1) = ibtdo(1)
      igbtwn(1) = ibtbtwn(1)
      if (ntpp .gt. 0 .and. ntrph .gt. 1) then
          call color_t2gp (tra, gtrce, ntrph, ntpp)
         igskp (1) = ibtskp (1) * ntrph - (ntrph - 1)
         igbtdo(1) = ibtdo  (1) * ntrph
         igbtwn(1) = ibtbtwn(1) * ntrph - (ntrph - 1)
      endif
      if (nafter .gt. 0 .and. ntrph .gt. 1 .and. kstrt(:4) .ne. 'SHOT') then
         gtrce = color_2gab (tra, iafter, jafter, nafter, ntrph, ntg)
         call color_abt (gtrce, tnnw, nbts, ibtskp, ibtdo, ibtbtwn,       &
           ibttot, 1)
      else
         call color_abt (gtrce, tnnw, nbts, igskp, igbtdo, igbtwn, ibttot,1)
      endif
      xx = xo + tnnw * gtwid - gtwid
      if (kstrt(:4) .ne. 'SHOT') then
         call colrplot_lwid(iunit,.025)
         ty1 = y1
         ty2 = y2
         do 500 i = 1, nrow
            if (kntcol .eq. ncol .and. i .gt. nrlc .and. ntpp .gt. 0) goto 510
            call colrplot_plot(iunit,xx, ty1, 3)
            call colrplot_plot(iunit,xx, ty2, 2)
            ty1 = ty1 + ypanlen
            ty2 = ty2 + ypanlen
  500    end do
  510    continue
         if (ltab(:3) .eq. iyes) then
            call colrplot_plot(iunit,xx, yend, 3)
            call colrplot_plot(iunit,xx, yend-ydel, 2)
         endif
      endif
      if (iwrda .eq. 0) goto 3400
      if (itmsh(:3) .eq. iyes) then
         call colrplot_lwid(iunit,tmwd)
         ty1 = yo
         ty2 = yo + ydatlen
         do 520 i = 1, nrow
            if (kntcol .eq. ncol .and. i .gt. nrlc .and. ntpp .gt. 0) goto 530
            call colrplot_plot(iunit,xx, ty1, 3)
            call colrplot_plot(iunit,xx, ty2, 2)
            ty1 = ty1 + ypanlen
            ty2 = ty1 + ydatlen
  520    end do
      endif
  530 if (lr) then
         ty1 = yo + .3
      else
         ty1 = yo - .3
      endif
      do 540 i = 1, nrow
         if (kntcol .eq. ncol .and. i .gt. nrlc .and. ntpp .gt. 0) goto 3400
         ndx = (i - 1) * nwih + iwrda
         numa = nint (header(ndx) )
         call string_ii2cc(numa,knum)
         nc=len_trim(knum)
         fnc = nc
!            move x to center justify label
         if (lr) then
            x2 = xx - ((fnc / 2.0 * (pcnt * shcs)))
            if (x2 .lt. xo) xbias = xo - x2
         else
            x2 = xx + ((fnc / 2.0 * (pcnt * shcs)))
         endif
!
         call colrplot_sym (iunit,x2, ty1, shcs, knum, alpha, nc)
         ty1 = ty1 + ypanlen
  540 end do
      if (ltab(:3) .eq. 'YES') then
         call colrplot_sym (iunit,x2, yend-ysdel2, shcs, knum, alpha, nc)
      endif
!
!
!          plot wrdb
 3400 if (iwrdb .ne. 0) then
         ty1 = yo + yedel
         do 600 i = 1, nrow
            if (kntcol .eq. ncol .and. i .gt. nrlc .and. ntpp .gt. 0) goto 100
            ndx = (i - 1) * nwih + iwrdb
            numb = nint (header(ndx))
            call string_ii2cc(numb,knum)
!               if wrdb is elevation header - do not plot the minus sign
            k=index(knum,'-')
            if (k .eq. 1) then
               ktmp = knum
               knum = iblk
               knum(1:7)=ktmp(2:8)
            endif
            nc=len_trim(knum)
            fnc = nc
            if (lr) then
               x2 = xx - ((fnc / 2.0 * (pcnt * wbcs)))
            endif
            if (.not.lr) x2 = xx + ((fnc / 2.0 * (pcnt * wbcs)))
            call colrplot_sym (iunit,x2, ty1, wbcs, knum, alpha, nc)
            ty1 = ty1 + ypanlen
  600    end do
         if (kstrt(:4) .eq. 'SHOT' .and. kntwrdb .lt. iwrbtot) then
            istrt = istrt + intb
         endif
      endif
!          if paneling - reset istrt to begin pattern over
!            again for each panel
      if (ntpp .gt. 0) then
         if ((ntplotg+intb) .gt. kendpnl) then
            istrt = kendpnl + istrtsv
            kendpnl = kendpnl + ntpp
            kntcol = kntcol + 1
         endif
      endif
      goto 100
!
!          rewind the trcio file
 8000 continue
      if(ntpp.eq.0)then
        istat=trcio_seek_trace(ipnstro,1)
      else
        istat=trcio_seek_trace(ipnstro,1)
      endif
 9501 continue
      if(istat.ne.0)then
        write(print_lun,*)' COLOR_WRDA ABORT - ERROR WITH TRCIO'
      endif
      return
      end subroutine color_wrda
      real function color_2gab(itr,iafter,jafter,nafter,ntrp,lgt)


      real    :: itr
      integer :: iafter(:),jafter(:),nafter,ntrp,lgt

!    Convert an ungraded trace number to a graded trace number when stopping
!     and restarting grade at blank trace locations 

!         itr      =  ungraded trace number  - received
!         iafter   =  array containing ungraded trace numbers that blanks
!                     are inserted after - received
!         jafter   =  array containing the graded trace numbers that blanks
!                     are inserted after - received
!         nafter   =  number of elements in iafter and jafter - received
!         ntrp     =  The number in the horizontal grade - received
!         lgt      =  The last graded trace number - received
                                                                    

      integer :: i,j,iadd
      real    ::  GITR  

      if(itr <= iafter(1))then
         color_2gab = (itr-1.0)*ntrp + 1.0
         return
      endif

!          find threshhold 

      j=nafter
      do i=1,nafter
        if(itr <= iafter(i))then
          j=i-1
          exit
        endif
      enddo
      if(j < 1)j=1
      iadd=(itr - iafter(j) - 1.0) * ntrp
      GITR = (jafter(j) + 1.0) + iadd
      if(GITR < 0)GITR=0
      if(GITR > lgt)GITR=lgt+1
      color_2gab=GITR
      end function color_2gab
     integer function color_2gpm2(NGCTPP,NTPPC,NTPP,INCTWID,NTRPH,ITRA)
                

!  Trace to graded panel - method 2  
!
!     Method 2 for determining the graded paneled color trace number 
!     which will have an overlay trace plotted on it.  This method does 
!     not require a correspondence of the number of overlay traces in a panel
!     to the number of color traces in a panel.  The number of overlay traces
!     in a panel is determined by dividing the total number of overlay traces
!     (NTO) by the number of panels.  There MUST be a correspondence of traces
!     per increment (TI) and overlay tracer per increment (TIO).  TIO/TI
!     must be a whole number
!
!     NGCTPP  =  number of graded color traces in one panel
!
!     NTPPC   =  number of ungraded color traces in a panel
!
!     NTPP    =  number of overlay traces in one panel
!
!     INCTWID =  The number of color traces to overlay traces 
!
!     NTRPH   =  horizontal grade factor
!
!     ITRA    =  sequential number of the overlay traces 

      integer :: ngctpp,ntppc,ntpp,inctwid,ntrph,itra

      integer :: k,icolt,ifirstc,ifirstgt,ifirstt,iseqt,igctn

      icolt=ITRA/(NTPP) + 1      !which column */
      k=mod(ITRA,NTPP)
      if(k==0)icolt=icolt-1
      ifirstt=(icolt-1)*(NTPP)+1   ! first overlay trace in a panel */
      iseqt=ITRA-ifirstt+1         ! the sequential trace number in a panel */
      ifirstc=(icolt-1)*(NTPPC)+1  ! first ungraded color trace in a panel */
      ifirstgt=(icolt-1)*(NGCTPP)+1 ! first graded color trace in a panel */
      igctn=(iseqt-1)*(NTRPH)*(INCTWID)+ifirstgt 

      color_2gpm2=igctn
      end function color_2gpm2
!</execute_only>



!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine color_wrapup (obj)
      implicit none
      type(color_struct),intent(inout) :: obj       ! arguments

      if(obj%skip_wrapup)return
      obj%skip_wrapup = .true.

!!!
!!!  --> Put any required wrapup code here, including wrapups of
!!!  --> internally-called processes.
!!!
!!!   if(associated(obj%test1))call test1_wrapup(obj%test1)
!!!

      return
      end subroutine color_wrapup

!</execute_only>


!!! All processes must wrap up appropriately after processing traces.
!!! Some processes might not have anything to do.  Other processes might
!!! want to save files or print summary information.  Appropriate wrapup
!!! procedures should be taken when trace processing has terminated
!!! successfully or unsuccessfully.
!!!
!!! The code in this subroutine must function correctly even if trace
!!! processing never began.  In spite of the <execute> flags in this
!!! template, there is no guarantee that this wrapup routine will not
!!! get called from the front end.
!!!
!!! Even if this routine is empty, this routine should be called from the
!!! main trace processing routine before returning NTR == NO_MORE_TRACES
!!! or NTR == FATAL_ERROR, and from the DELETE routine.
!!!
!!! This routine is called from the back-end after some process (including
!!! possibly this one) has reported an error before all traces have been
!!! processed.
!!!
!!! This routine should be called from any other calling program (e.g.
!!! another process module or an interactive program) if processing is
!!! prematurely terminated for any reason.
!!!
!!! This routine is also called from the DELETE routine to make sure the
!!! wrapup is completed.  Therefore, if this object is to be deleted right
!!! after the wrapup is completed, the call to the WRAPUP routine is not
!!! necessary.
!!!
!!! By testing the WRAPPED_UP structure variable, this routine does not
!!! repeat any wrapup previously done in case it is called more than once.
!!!
!!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
      end module color_module
