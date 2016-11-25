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
! Name       : SPLT   (Section PLoT)
! Category   : plot
! Written    : 1989-01-28   by: Karen Goodger
! Revised    : 2007-01-16   by: Karen Goodger
! Maturity   : beta
! Purpose    : Plot a section of seismic traces.
! Portability: 32 bit word size
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! SPLT is a highly versatile process that produces seismic trace plots on a
! variety of plotting devices.  Because the number of options relating to trace
! plotting are very numerous and the options relating to annotation are even
! more numerous, SPLT is a process for the expert user.  (There are over 170
! parameters.)
!
! Parameters are located in functionally related groups on the parameter entry
! screens; these groups are listed below.  The first group consists of the
! seven essential parameters required to produce a basic plot.  The second
! group consists of basic annotation parameters.  Users with modest
! requirements will not need to visit additional parameter groups.
!
!
! SPLT Parameter Categories
!
!   Automatic Annotation Positioning
!   Basic Annotation
!   Basic Plot
!   Block Boundaries and Line-ties
!   Graph-style Plots
!   Hardcopy Related
!   Manual Shotpoint-style Annotation
!   Panel and Blank Trace
!   Side Label
!   Statics Annotation
!   Timing Lines
!   Trace Appearance
!   Trace Selection
!   Velocity Functions
!
!
! Length Units
!
! Depending on the choice of English or metric units on the Project_data screen,
! SPLT will automatically use inches or miles where appropriate (English) or
! centimeters or kilometers where appropriate (metric).  Exceptions to this rule
! are documented.  Plotting limitations or restrictions are typically
! documented only in inches.
!
!
! Limitations
!
! Maximum plot length is 240 inches for INDIGO and 200 inches for CGM.  Maximum
! plot height is 30 inches for HP plotters and 35 inches for Indigo and Versatec
! plotters.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! Basic Plot
!
! Basic plot parameters are those required to produce the simplest plots.
! These parameters are:
!
! TIME      Maximum trace time to plot (time at bottom of plotted trace).
! IPS       Vertical trace scaling - increment (in. or cm) per second.
! TPI       Lateral trace spacing - traces per increment (in. or cm).
! LRRL      Plot traces left to right (LR) or right to left (RL).
! NUM_TR    Maximum number of traces to plot.
! CT        Plotting gain - LAV is plotted as CT channels.
! SID       Plot annotation (Section IDentification).
!
!
! Basic Annotation
!
! Basic annotation parameters are those required for the simplest plot
! annotation.  These parameters are:
!
! CALD      Automatic calculation of annotation location above section.
!
! SCALE     Lateral distance scale marker (mile or kilometer marker).
!
! HDR_LAB(1-4)  Header word to plot as an automatic trace label above section.
! LAB_INIT  Trace number of the first trace label.
! HDR_CHG   Plot automatic trace labels wherever HDR_CHG changes.
! LAB_INC   Trace number increment between trace labels.
! NAM_LAB   Name(s) to identify automatic or manual trace labels.
! LTAB      Plot trace labels on top and bottom of section.
! CS_LAB    Character size for trace label(1-4).

!
! OPT_VL    Whether to plot vertical lines, blanks or timing marks under
!       labels.
! WID_VL    Width of vertical lines under labels.
!
! AR_BEG    Label for the direction arrow pointing to the start of the plot.
! AR_END    Label for the direction arrow pointing to the end of the plot.
! AR_HT     Height of the direction arrowhead.
! AR_LEN    Length of the direction arrow.
! ARDA      Height of the direction arrow above top of plot.
!
! PATH_ATBM Pathname for file containing the automatic title block master.
! FRTB      Factor for reducing the title block size.
!
!
! Automatic Annotation Location
!
! Setting CALD=YES adjusts parameters SDAS,TIDA,ARDA and SEDA in order to
! prevent annotation from overlapping vertically. This calculation is made at
! workfile building time.
!-------------------------------------------------------------------------------
!</advice_doc>

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
! This process does not alter input traces.
! This process outputs the same traces as it receives.
!
! This process outputs traces with same gather status as the input traces.
!
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
! IPN      process number                        used (must not be changed)
! MAXTR    max number of traces input/output     used but not changed
! NWIH     number of words in trace header       used but not changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
! 1       Sequential Trace Count     Renumbered.
! NHF     User-defined header word   Reset to 1 or 0 (trace flag).
! 3       Current gather             Used to group traces into gathers.
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>
!<history_doc>
!
!                             REVISION HISTORY
!
!    date        author      description
!    ----        ------      -----------
!37. 2007-01-16  Goodger     Call velfile_read_file with the 'all' flag set
!                            to true.  Otherwise, needed velocity functions
!                            are rejected.
!36. 2007-01-03  Stoeckley   Add call to pc_register_array_names for SeisSpace;
!                             remove more unused variables.
!35. 2006-01-10  B. Menger   Removed Unused Variables.
!34. 2003-12-09  Goodger     Set ymax to a maximum of 40 inches. This solves
!                            the problem of splt outputting an extra panel.
!33. 2003-08-28  Goodger     Doc changes for Houston plotters.
!32. 2003-08-14  Goodger     Remove hard coded path.
!31. 2002-04-29  k.goodger - Fix overplotting problem with nam_lab parameter.
!                            Fix problem cutting off top line of SID.
!30. 2002-02-22  k.goodger - Add option to output to a cgm file.  Add plotter
!                            HP5000.  Remove indigo and versatec plotters.
!29. 2001-12-10  k.goodger - Fix bug in routine tblk (routine that plots the
!                            title block) which was causing an internal compiler
!                            error on the intel compiler.
!                            Take care of warning messages generated by intel
!                            compiler.
!28. 2001-10-18  k.goodger - Save the unit number for the dplt control cards
!                            to insure it is the same for successive splts.
!                            Build cards for dplttest if testlib and dplt if
!                            prodlib. This change resolves problems with
!                            timing lines, when not the first plot.
!27. 2001-08-14  k.goodger - Write the plotting scale to the history file,
!                            called PLOTSCALE.
!26. 2001-08-09  k.goodger - Call new title block routine, ATBLK.
!25. 2001-08-02  k.goodger - Fix bug in plbl routine. It was sending a null
!                            to the symbol routine.
!24. 2001-04-30  k.goodger - Removed check in skip_init trap against num_tr.
!23. 2001-03-22  k.goodger - Do not allow a zero time.
!22. 2001-02-23  k.goodger - Rename plot files names if jobname is greater
!                            than 9 characters.
!21. 2001-02-15  k.goodger - Fix bug with dev_loc=SAVE.
!20. 2001-02-06  k.goodger - Do not output dplt or spst when opt_loc=test.
!                            Fix bug with arrows when conp.
!19. 2001-01-24  k.goodger - Add HP and Versatec devices.
!18. 2001-01-15  k.goodger - Uppercase tie lines in a file.
!17. 2000-12-12  k.goodger - Add printout of title block file.
!16. 2000-12-08  k.goodger - Wrapup flag changes.  Added an error flag to
!                            splt_vf.
!15. 2000-12-05  k.goodger - Print some dplt control card info to online.
!14. 2000-12-04  k.goodger - Do not plot minus sign on water depths.  Fix
!                            problem with FOLD not coming out on sticker id.
!13. 2000-11-30  k.goodger - Removed calles to hist_init and hist_write. This
!                            fixes the problem of velocity functions
!                            overplotting.
!12. 2000-10-31  k.goodger - Remove some repeated code which was causing
!                            velocity function failure.
!11. 2000-10-27  k.goodger - Add call to ATB.
!10. 2000-10-18  k.goodger - If job name is longer than 10 characters, create
!                            a random name to use for the plot file rather than
!                            the job name.  The vax cannot handle plot file
!                            names longer than 10 characters.
!                            Fix front end problem.  Variable nsid was not
!                            initialized.
!09. 2000-09-27  k.goodger - Fix paneling problem when there is only 1 row.
!08. 2000-09-18  k.goodger - Correct problem with blank trace traps.
!07. 2000-09-18  k.goodger - Fix problem with bthc option.
!                            Fix problem with intermediate tic marks not
!                            showing up.
!                            Implement blank timing lines.
!06. 2000-09-14  k.goodger - Correct ovwd parameter to accept decimal.
!                            Convert scale parameter to upper case.
!                            Convert side labels to upper case.
!                            Make hdr_dl sensitive.
!                            Change VINT OR&I to ORI.
!                            Correct problems with more than 1 plot in a job.
!                            Convert plbl to upper case.
!05. 2000-09-12  k.goodger - Print error message if velocity funcion headers
!                            do not match the data headers.
!                            Reset some array counters back to zero.
!                            Correct metric conversion problems with tics,
!                            tida, and arda.
!04. 2000-09-11  k.goodger - Remove and old array which was not allocated
!                            and causing a job abort with no messages.
!03. 2000-09-10  k.goodger - Fix problem getting into trtic field.
!                            Reposition some screen 2 parameters.
!                            Inform user of maximum total on manual shot
!                            point array.
!                            Add an element trap to the totl field of the
!                            shot point array to check range for that
!                            card.
!                            Correct problem of setting ntpp back to zero.
!02. 2000-09-07  k.goodger - Insure path filled in for path_vel parameter.
!                            Adust CALD parameters.
!                            Correct problem with elevation plot having
!                            incorrect Y coordinate.
!                            Fix velocity label.
!                            Allow lower case in LAB_INIT parameter.
!                            Turn off manual trace array if hdr_lab set.
!                            Fix bug with two plots in a job.
!                            Bug report 25.
!01. 2000-08-17  k.goodger - Initial conversion to new CPS.
!***********************************************************************
!01. 1999-05-04  k.goodger - remove email to node pogun.  removed nesp
!                            from doc.
!***********************************************************************
!09. 1998-11-18  k.goodger - fix arunment in call to splt_vf.  it was
!                            delcared character in the calling routine
!                            but used as hollerith in the receiving
!                            routine.  fix problem in spltckh involving
!                            getarg.
!08. 1998-11-10  k.goodger - put a save statement in each subroutine.
!                            begin using fortran90 compiler.
!07. 1998-09-16  k.goodger - hitemax was set to 70.0 if a half scale
!                            only.  this will not work due to memory
!                            limitations in the indigo.
!06. 1998-08-06  k.goodger - removed node howwe.  front end change only.
!05. 1998-07-27  k.goodger - add the njob parameter to the atb call.
!04. 1998-05-04  k.goodger - update documentation on plotting nodes.
!03. 1998-04-14  k.goodger - make corresponding corrections in thit
!                            routine that was made to tblk routine in
!                            revision 2.  made a note in tblk routine
!                            that thit will also need changes.
!02. 1998-04-01  k.goodger - correct problem with plotting blank cards
!                            in the title block.
!01. 1998-01-22  k.goodger - call dcodet to find out the unit number
!                            being used by dcode.  it may not be unit 5.
!**********************************************************************
!10. 1997-10-08  k.goodger - add aberdeen plotters to documentation.
!09. 1997-09-04  k.goodger - change bias on check for nils. routine cdp.
!                            some values close to zero, but not nil were
!                            not getting plotted.
!08. 1997-09-03  k.goodger - increase xmax when title block falls on a
!                            hsr page.
!07. 1997-08-15  k.goodger - adjust y-coordinate on cdpi plot labels
!                            so that they do not overlap.
!06. 1997-08-04  k.goodger - insure calculated vxbias parameter is
!                            positive.
!05. 1997-06-30  k.goodger - ignore nils on the static plot.  routine
!                            cdp.
!04. 1997-06-18  k.goodger - add norway plotting nodes. remove aberdeen
!                            nodes.
!03. 1997-02-17  k.goodger - add flpt parameter.
!02. 1997-02-03  k.goodger - fix problem with elevation plot not honoring
!                            paneling.
!01. 1997-01-20  k.goodger - documentation change on node po4001.
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
! NSCRATCH        >0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK           >0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
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
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
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
!<gui_def>
!<NS SPLT Process/NC=80>
!
!                  Section PLoT Process
!           Plot a section of seismic traces.
!
! TIME=`FFFFFFFFFFF  IPS=~~~`FFFFFFFFFFF  TPI=`FFFFFFFFFFF
!
! LRRL=`CC           NUM_TR=`IIIIIIII     CT= `FFFFFFFFFFF
!
! SID
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS          CS_SID=`FFFFF
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!<PARMS SID[/XST/YST]>
!<NS Basic_Annotation_Parameters/NC=80>
!
!    SCALE=`SS     CS_SCALE=`FFFF
!
!
! HDR_LAB         NAM_LAB               CS_LAB=~~~`FFFFF
! `IIIIIIII       `SSSSSSSSSSSSSSS
! `IIIIIIII       `SSSSSSSSSSSSSSS      LAB_INIT=~`SSSS        HDR_CHG=`IIIIII
! `IIIIIIII       `SSSSSSSSSSSSSSS
! `IIIIIIII       `SSSSSSSSSSSSSSS      LAB_INC=~~`IIIIIIII
!
! TRCE   SHOT    INTV  INCR  TOTL
! `FFFFFF`SSSSSSS`IIIII`IIIII`IIIII
! `FFFFFF`SSSSSSS`IIIII`IIIII`IIIII
! `FFFFFF`SSSSSSS`IIIII`IIIII`IIIII
! `FFFFFF`SSSSSSS`IIIII`IIIII`IIIII
!
! TRTIC  INTIC  ITTIC
! `FFFFFF`IIIIII`IIIIII
! `FFFFFF`IIIIII`IIIIII
! `FFFFFF`IIIIII`IIIIII
! `FFFFFF`IIIIII`IIIIII
!
!
! LTAB=`CC    OPT_VL=`CCC     WID_VL=`FFFFFFFFFFF
!
! AR_BEG=`SS  AR_END=`SS       AR_HT=`FFFFFFFFFFF  AR_LEN=`FFFFFFFFFFF
!
! PATH_ATBM=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! FRTB=`FFFFFFFFFFF
!<PARMS HDR_LAB[/XST/YST]>
!<PARMS NAM_LAB[/XST/YST]>
!<PARMS CS_LAB[/XST/YST]>
!<PARMS TRCE_ARRAYSET[/XST/YST]>
!<PARMS TRTIC_ARRAYSET[/XST/YST]>
!<PARMS PATH_ATBM[/ML=128/XST]>
!<NS Hardcopy_Related_Parameters/NC=80>
!
!       Hardcopy Parameters
!
!
! DEV_LOC=`CCCCCC    QUALITY= `CCCC     OPT_DEV=`CCCCC
!
!                     COPIES= `IIIIIIII    INIT=`SS
!
! FOLD=`CC    
!
!<NS Trace_Appearance_Parameters/NC=80>
!
!     Trace Appearance Parameters
!
! METH_CCT=`CCCC    CCTF=~~~`FFFFFFFFFFF
!
! NVRT=~~~~`CC      NORM=~~~`CCC    WT=~~~`C
!
! VA=~~~~~~`CC      VASTRT=`FFFFFF  VAI=~~`CC     RP=`CC
!<NS Trace_Selection_Parameters/NC=80>
!
!      Trace Selection Parameters
!      Will use this pattern until NUM_TR (screen 1) traces are selected
!
! SKIP_INIT=`IIIIIIII    NUM_DO=`IIIIIIII
!
! NUM_SKIP= `IIIIIIII
!<NS Panel and Blank Trace Parameters/NC=80>
!
!   Panel and Blank Trace Parameters
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
! PLCS=~~~`FFFFFF    RSPP=`CCC
!
! HDR_BT=`IIIIII  BTHC=~~~`IIIIIIII
!<NS Timing_Line_Parmeters/NC=80>
!
!            Timing Line Parameters
!
!MILS TLDOTS     TLCS=`FFFFFFFFFFF     TLST=`FFFFFFFFFFF
!10   `IIIII
!50   `IIIII     TBST=`FFFFFFFFFFF     TBIB=`IIIIIIII  TBTOT=`IIIIIIII
!100  `IIIII
!400  `IIIII
!500  `IIIII
!1000 `IIIII
!<PARMS TLDOTS[/XST/YST]>
!<PARMS PCOL_ARRAYSET[/XST/YST]>
!<NS Velocity_Function_Paramters/NC=80>
!
!                Velocity Function Parameters
!
! PATH_VEL=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! FRVF=~~~~`FFFFFF    VINT=`CCCCC    VXTOL=`FFFFFFFFFFF
!<PARMS PATH_VEL[/ML=128/XST>
!<NS Block Boundaries_Tie Lines_Side Labels/NC=80>
!
!                 Block Boundaries, Tie Lines, Side Labels
!
! PATH_BLK=~`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! BLCS=~~~~~`FFFFFF
!
! PATH_TIE1=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! PATH_TIE2=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! TICS=~~~~~`FFFFFFFFFFF
!
! TBOT=~~~~~`CC
!
! TITR TIE
! `IIII`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `IIII`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `IIII`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! `IIII`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!
!       SLTIM SLBL                    SLSZ  SLROW
!       `FFFFF`SSSSSSSSSSSSSSSSSSSSSSS`FFFFF`IIII
!       `FFFFF`SSSSSSSSSSSSSSSSSSSSSSS`FFFFF`IIII
!
!                 Block Boundaries, Tie Lines, Side Labels
!
!
!
!<PARMS PATH_BLK[/ML=128/XST]>
!<PARMS PATH_TIE1[/ML=128/XST]>
!<PARMS PATH_TIE2[/ML=128/XST]>
!<PARMS TITR_ARRAYSET[/XST/YST]>
!<PARMS SLTIM_ARRAYSET[/XST/YST]>
!<NS Graph Style Plots/NC=80>
!
!    Graph Style of Plots
!
!     FLST=~~~~`FFF  HDR_FL=~~`I      OVJD=~~~~~`FFFFF   HDR_OFF=`I   OVWD=`FFF
!
!     HDR_DL=~~`I    DLREF=~~~`FFF    DLU100=~~~`FFF
!
!     HDR_PL=~~`I    PLREF=~~~`FFF    PLU100=~~~`FFF
!
!     REZT=~~~~`CC   PMSE=~~~~`CC     PMSE100=~~`FFF
!
!
!<NS Automatic Positioning of Annotation/NC=80>
!
!               Automatic Positioning of Annotation
!
! CALD=`CCC           SDAS=`FFFFFFFFFFF    SCDA=`FFFFFFFFFFF
!
! TIDA=`FFFFFFFFFFF   ARDA=`FFFFFFFFFFF    SEDA=`FFFFFFFFFFF
!</gui_def>

!!
!<HelpSection>
!     -------------------  Basic Plot Parameters  ------------------------
!    Basic plot parameters are those required to produce the simplest plots.
!
!<Help KEYWORD="TIME">
!<Tip> Maximum trace time to plot (time at bottom of plotted trace). </Tip>
! Default = Time on job data screen
! Allowed = real > 0.0
! TIME*IPS + SDAS cannot exceed the maximum plot height.  Maximum plot height
! is 30 inches for HP and 40 inches for HP5000A.
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
! where RES is the resolution (either 200 or 400 dots per inch).
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
! NUM_TR/TPI cannot exceed the maximum plot length.  Maximum plot length is 240
! inches for INDIGO and 200 inches for CONPLOT.
!</Help>
!
!<Help KEYWORD="CT">
!<Tip> Plotting gain - LAV is plotted as CT channels. </Tip>
! Default = 5.0
! Allowed = real > 0.0
! The largest absolute value (LAV) is plotted as CT/2 channels, where a channel
! is the plotted lateral trace spacing.  (Since it is presumed that the sample
! with the LAV is near another with similar amplitude and opposite polarity,
! the pair plot as CT channels.)
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
!<Tip> Character size for SID, in inches or cm. </Tip>
! Default = 0.27 inch
! Allowed = less than 1 inch.
!</Help>
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
! SCALE = NO doesn't plot distance scale marker.
! SCALE = a number interprets the number as the difference between header word
! 17 values of two adjacent traces (in feet or meters).
!</Help>
!
!<Help KEYWORD="CS_SCALE">
!<Tip> Character size for SCALE, in inches or cm. </Tip>
! Default = 0.13 inch
! Allowed = Less than 1 inch.
!</Help>
!
!<Help KEYWORD="HDR_LAB">
!<Tip> Header word to plot as a trace label above section. </Tip>
! Default = 7
! Allowed = 0 - NWIH, array of 4
! Four automatic trace labels are allowed, HDR_LAB(1) - HDR_LAB(4).  Label(1) is
! plotted nearest the trace plot and labels(2-4) are stacked sequentially above
! label(1).
! If any of HDR_LAB(1-4)=0, do not plot that label.
!</Help>
!
!<Help KEYWORD="LAB_INIT">
!<Tip> Trace number for the first automatic trace label. </Tip>
! Default = 50
! Allowed = int (1 to NUM_TR)
! Allowed = SHOT
! Allowed = HDR
! If LAB_INIT = an integer, the first automatic trace label will be plotted
! above that trace number.
! If LAB_INIT = SHOT, HDR_LAB(1) must be zero and HDR_LAB(2) labels are plotted
! above manual shotpoint labels.
! If LAB_INIT = HDR, the HDR_LAB(1) label is plotted above the first trace and
! subsequently wherever the header word HDR_CHG changes.
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
! Allowed = char(16) array of 4
! NAM_LAB is an array of names to plot adjacent to automatic or manual trace
! labels to identify them.  The array has an element for each of the four
! labels.
!</Help>
!
!<Help KEYWORD="HDR_CHG">
!<Tip> Plot automatic trace labels wherever HDR_CHG changes. </Tip>
! Default = 7
! Allowed = 1 - NWIH
! Active only if LAB_INIT = HDR.
!</Help>
!
!<Help KEYWORD="LTAB">
!<Tip> Plot automatic or manual trace labels on top and bottom of plot. </Tip>
! Default = NO
! Allowed = YES/NO
! If LTAB = NO, then plot labels on top of section only.
!</Help>
!
!<Help KEYWORD="CS_LAB">
!<Tip> Character size for trace label(A-D), in inches or cm. </Tip>
! Default = 0.13 inch
! Allowed = real > 0.0 array of 2
!</Help>
!
!<Help KEYWORD="OPT_VL">
!<Tip> Plot vertical lines, blanks or timing marks under labels. </Tip>
! Default = NO
! Allowed = YES  (Plot a vertical line under the trace labels.)
! Allowed = NO   (No marking under the trace labels.)
! Allowed = BTL  (Blank the Timing Lines under the trace labels.)
! Allowed = TM   (Plot 10 mil Timing Marks under the trace labels.)
!</Help>
!
!<Help KEYWORD="WID_VL">
!<Tip> Width of vertical lines under labels, in inches or cm. </Tip>
! Default = 0.005 inch
! Allowed = real > 0.0
! The front-end will insure that WID_VL is at least one dot larger than the WT
! parameter.
!</Help>
!
!<Help KEYWORD="AR_BEG">
!<Tip> Label for the arrow pointing toward the start of the plot. </Tip>
! Default = -
! Allowed = char(3)
! Normally strings such as N, S, SE are used. If AR_BEG = BLANK, no arrow is
! plotted.
!</Help>
!
!<Help KEYWORD="AR_END">
!<Tip> Label for the arrow pointing toward the end of the plot. </Tip>
! Default = -
! Allowed = char(3)
! Normally strings such as N, S, SE are used.  If AR_END = BLANK, no arrow is
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
! Default = 2.5
! Allowed = real > 0.0
! Typical values are 2.5 inches or 6.35 cm.
!</Help>
!
!<Help KEYWORD="PATH_ATBM">
!<Tip> Pathname for file containing the automatic title block master. </Tip>
! Default = -
! Allowed = char
! If PATH_ATBM is non-blank and the filename has the extension .atbm, SPLT will
! automatically plot a title block based on the history file and the automatic
! title block master file.  If the extension is not .atbm, the contents of the
! file will be plotted as text in the title block space.
!</Help>
!
!<Help KEYWORD="FRTB">
!<Tip> Factor for reducing the title block size. </Tip>
! Default = 1.0
! Allowed = 1.0 >= real > 0.0
! If your title block is too long to be plotted, SPLT will automatically reduce
! the size to avoid running off the bottom of the plot.
!</Help>
!
!
!   -------------------  Hardcopy Related Parameters  ------------------------
!
!<Help KEYWORD="DEV_LOC">
!<Tip> Local device to use for plotting. </Tip>
! Default = HP5000A (HP5000 plotter 2nd floor lobo)
! Allowed = HP      (HP     plotter 3rd floor lobo)
! Allowed = TEST    (Run SPLT but write .rpt file only and don't make a plot.)
! Allowed = SAVECGM (Save a CGM file in current directory; do not plot.)
!</Help>
!
!<Help KEYWORD="OPT_DEV">
!<Tip> Options for local Versatec A or B plotters. </Tip>
! Default = PAPER
! Allowed = PAPER   
! Allowed = FILM    
! Active only if DEV_LOC = HP5000.
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
!   -------------------  Trace Appearance Parameters  ------------------------
!
!<Help KEYWORD="METH_CCT">
!<Tip> Method to use for the calculated CT factor. </Tip>
! Default = NONE
! Allowed = NONE
! Allowed = MED
! Allowed = NOMED
! If METH_CCT = NONE, then use the CT parameter, do not use the calculated CT.
!
! If METH_CCT = MED, then use a calculated CT that plots the median absolute
! amplitude of the plotted traces as CCTF channels.  (CT = CCTF * LAV/MEDIAN)
! The median calculation uses only nonzero sample values below the head mute.
!
! If METH_CCT = NOMED, then use a calculated CT that plots unit amplitude of
! the plotted traces as CCTF channels.  This method assumes the traces have
! already been median scaled, i.e. scaled so that the median absolute amplitude
! is 1.0.  (CT = CCTF * LAV)
!</Help>
!
!<Help KEYWORD="CCTF">
!<Tip> Parameter to use in calculated CT factor. </Tip>
! Default = 0.35
! Allowed = real > 0.0
! Recommended values are 0.35 for relative amplitude plots and 0.9 for
! structural (XP) plots.
!</Help>
!
!<Help KEYWORD="NVRT">
!<Tip> Invert plotted traces so that the last sample is plotted on top </Tip>
! Default = NO
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="NORM">
!<Tip> Determine plotting gain using individual trace LAV or dataset LAV. </Tip>
! Default = ALL
! Allowed = ALL
! Allowed = EACH
! If NORM = ALL, then use the same gain for all traces based on the maximum LAV
! of all the plotted traces.  (Old NORM=NO.)
! If NORM = EACH, then calculate a gain for each trace individually based on
! the LAV of that trace.  (Old NORM=YES.)
!</Help>
!
!<Help KEYWORD="WT">
!<Tip> Number of dots to use for the wiggle trace line. </Tip>
! Default = 1
! Allowed = 0, 1, 2
! WT can be 0 only if VA = YES.  WT must be 1 if making dual polarity plots.
!</Help>
!
!<Help KEYWORD="VA">
!<Tip> Variable area option - fill in positive lobe of the wiggle trace? </Tip>
! Default = YES
! Allowed = YES/NO
! If VA = NO, you will get a wiggle trace only plot.
!</Help>
!
!<Help KEYWORD="VASTRT">
!<Tip> Variable area start shading option. </Tip>
! Default = 0.0
! Allowed = 1.0 >= VASTRT >= 0.0
! VA shading will shade the amplitude range from VASTRT*LAV to LAV.
!</Help>
!
!<Help KEYWORD="VAI">
!<Tip> Variable intensity option - plot every other VA raster scan? </Tip>
! Default = NO
! Allowed = YES/NO
! If VAI = YES, every other scan will be plotted in the VA raster area so that
! overlapping trace lobes do no obliterate each other.
!</Help>
!
!<Help KEYWORD="RP">
!<Tip> Reverse polarity option - multiply samples by -1 before plotting. </Tip>
! Default = NO
! Allowed = YES/NO
!</Help>
!
!
!   -------------------  Trace Selection Parameters  ------------------------
!

!<Help KEYWORD="SKIP_INIT">
!<Tip> Number of traces to skip initially in the DO-SKIP selection. </Tip>
! Default = 0
! Allowed = int>=0
! The DO-SKIP trace selection method consists of initially skipping SKIP_INIT
! traces, then sequentially plotting ("doing") NUM_DO consecutive traces
! and skipping NUM_SKIP consecutive traces until NUM_TR total traces are
! processed.
!</Help>
!
!<Help KEYWORD="NUM_DO">
!<Tip> Number of traces to process at a time in the DO-SKIP selection. </Tip>
! Default = 1
! Allowed = int>0
! The DO-SKIP trace selection method consists of initially skipping SKIP_INIT
! traces, then sequentially plotting ("doing") NUM_DO consecutive traces
! and skipping NUM_SKIP consecutive traces until NUM_TR total traces are
! processed.
!</Help>
!
!<Help KEYWORD="NUM_SKIP">
!<Tip> Number of traces to skip at a time in the DO-SKIP selection. </Tip>
! Default = 0
! Allowed = int>=0
! The DO-SKIP trace selection method consists of initially skipping SKIP_INIT
! traces, then sequentially plotting ("doing") NUM_DO consecutive traces
! and skipping NUM_SKIP consecutive traces until NUM_TR total traces are
! processed.
!</Help>
!
!
!    ---------------  Panel and Blank Trace Parameters  --------------------
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
!<Help KEYWORD="PLCS">
!<Tip> Panel label character size.  </Tip>
! Default = 0.3
! Allowed = 0.05 - 0.3 inches
! Suggested values are 0.3 inches, 0.75 cm.
!</Help>
!
!<Help KEYWORD="RSPP">
!<Tip> Repeat trace labeling (automatic or manual) on each panel.  </Tip>
! Default = YES
! Allowed = YES/NO
! If RSPP = YES, then each panel is treated as a separate section.
!</Help>
!
!<Help KEYWORD="HDR_BT">
!<Tip> Insert blank traces when header word HDR_BT changes.  </Tip>
! Default = 3
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="BTHC">
!<Tip> Number of blank traces to insert when header word BTHW changes.  </Tip>
! Default = 0
! Allowed = < NUM_TR
! If BTHC = 0, then blank traces and timing line numbers are automatically
! inserted between panels (if paneling is being done).
! If BTHC > 0, then blank traces are inserted only when header word BTHW
! changes.
!</Help>
!
!
!      ------------------- Timing Line Parameters -----------------------
!
!<Help KEYWORD="TLDOTS">
!<Tip> Number of dots to use for as many as 6 levels of timing lines.  </Tip>
! Default = 0, 0, 1, 0, 0, 2
! Allowed = 0 - 4 array(6)
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
!<Help KEYWORD="TLCS">
!<Tip> Timing line character size.  </Tip>
! Default = 0.0
! Allowed = real
! If TLCS = 0.0, then character size will be 0.035*IPS.
! If TLCS = -1, then no timing lines will be plotted.
!</Help>
!
!<Help KEYWORD="TLST">
!<Tip> Timing line starting position, in mils from top of section.  </Tip>
! Default = 0
! Allowed = integer > 0
! Annotation will always be referenced to top of section (TSTRT).
!</Help>
!
!<Help KEYWORD="OVJD">
!<Tip> Velocity to use for OVJD tics.  </Tip>
! Default = 0.0
! Allowed = real > 0.0
! Plots a tic mark at time = offset/(OVJD velocity).  Used to verify field
! geometry.
! If OVJD = 0.0, then do not plot tics.
!</Help>
!
!<Help KEYWORD="HDR_OFF">
!<Tip> Header word carrying offset to use for OVJD tics.  </Tip>
! Default = 6
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="OVWD">
!<Tip> Thickness of OVJD tic marks.  </Tip>
! Default = 0.01 inches
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="TBST">
!<Tip> Trace number to start blanking timing lines.  </Tip>
! Default = -
! Allowed = int < NUM_TR
!</Help>
!
!<Help KEYWORD="TBIB">
!<Tip> Trace number increment between blank timing lines.  </Tip>
! Default = -
! Allowed = int < 0
!</Help>
!
!<Help KEYWORD="TBTOT">
!<Tip> Total number of times to blank timing lines.  </Tip>
! Default = -
! Allowed = int < 0
!</Help>
!
!
!       --------------- Velocity Function Parameters -------------------
!
!<Help KEYWORD="PATH_VEL">
!<Tip> Pathname of the file containing velocity functions to plot.  </Tip>
! Default = -
! Allowed = char
! Velocity function names within the file must be distinct.
!</Help>
!
!<Help KEYWORD="FRVF">
!<Tip> Factor for reducing plotted velocity function size.  </Tip>
! Default = 1.0
! Allowed = 1.0 >= FRVF > 0.0
!</Help>
!
!<Help KEYWORD="VINT">
!<Tip> Options for velocity function interpolation.  </Tip>
! Default = TINT5
! Allowed = TINT5  (Time interpolation to 5.5 sec.)
! Allowed = TINT7  (Time interpolation to 7.0 sec.)
! Allowed = TINT15 (Time interpolation to 15.0 sec.)
! Allowed = ORI   (Original function + interpolated times every 0.5 sec. )
! Allowed = VELO   (Velocity interpolation.)
! Allowed = NONE   (Plot function without change.)
!</Help>
!
!<Help KEYWORD="VXTOL">
!<Tip> Tolerance for determining equality of coordinate values.  </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! SPLT looks at the value of the velocity file coordinate header for the first
! two traces to determine whether the file should be read in increasing or
! decreasing order.  This may be difficult if the coordinate header does not
! monotonically increase or decrease.  In this case you should set VXTOL
! non-zero so that if the header value of the two traces falls within VXTOL of
! each other, SPLT will look at more traces to determine the order.
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
!<Help KEYWORD="BLCS">
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
!<Help KEYWORD="TICS">
!<Tip> Line-tie character size.  </Tip>
! Default = 0.07
! Allowed = real > 0.0
! Suggested size is 0.07 inches or 0.18 cm.
!</Help>
!
!<Help KEYWORD="TBOT">
!<Tip> Plot line-tie marks on bottom of section as well as the top.  </Tip>
! Default = NO
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="TITR">
!<Tip> Trace numbers for manual line-tie labels.  </Tip>
! Default = -
! Allowed = int linked array(50)
! Manual lie-ties cannot be used if PATH_TIE is specified.
!</Help>
!
!<Help KEYWORD="TIE">
!<Tip> Manual line-tie labels.  </Tip>
! Default = -
! Allowed = char linked array(50)
! 40 characters may be used per label.  Do not use , = ( ) or *.
!</Help>
!!
!<Help KEYWORD="PMSE">
!<Tip> Plot midpoint surface elevation in box above section?  </Tip>
! Default = NO
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="PMSE100">
!<Tip> Units representing 100 mils for PMSE  </Tip>
! Default = 1.0
! Allowed = 1.0 to 1000.0
!</Help>
!
!<Help KEYWORD="FLST">
!<Tip> Fold of stack indicator.    </Tip>
! Default = 0
! Allowed = integer
! Plots a fold of stack indicator as an open circle between zero time and
! 100 mils with 100% of FLST plotted at zero time.  FLST=maximum fold.  Zero =
! no fold of stack indicator plotted.  Fold will be plotted between the top
! of the section and the first 100 mils.
!</Help>

!<Help KEYWORD="HDR_FL">
!<Tip> Header word to use for FLST.  </Tip>
! Default = 5
! Allowed = 1 - NWIH
! Word 5 is the fold of stack which is standard for the FLST parameter.
!</Help>
!
!<Help KEYWORD="HDR_DL">
!<Tip> Header word to use for dashed line plot.  </Tip>
! Default = 0
! Allowed = 1 - NWIH
! Word 19 is the elevation which would be the standard use for this parameter.
!</Help>
!
!<Help KEYWORD="DLREF">
!<Tip> Reference number to be plotted at zero time for HDR_DL  </Tip>
! Default = -
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="DLU100">
!<Tip> Units representing 100 mils for HDR_DL  </Tip>
! Default = -
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="HDR_PL">
!<Tip> Header word to use for plus sign plot.  </Tip>
! Default = 0
! Allowed = 1 - NWIH
! Works the same as the HDR_DL option.
!</Help>
!
!<Help KEYWORD="PLREF">
!<Tip> Reference number to be plotted at zero time for HDR_PL  </Tip>
! Default = -
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="PLU100">
!<Tip> Units representing 100 mils for HDR_PL  </Tip>
! Default = -
! Allowed = real
!</Help>
!
!<Help KEYWORD="REZT">
!<Tip> Reference elevation to zero time  </Tip>
! Default = YES
! Allowed = YES/NO
! Reference elevation (HDR_DL and HDR_PL) and fold of stack (FLST) to zero
! time.  If there is a negative tstrt, zero time will be below the top of the
! section.  REZT=NO will reference elevation and flst to the top of the section
! rather than zero.
!</Help>
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
!<Help KEYWORD="TRTIC">
!<Tip> Manual shot tic marks  </Tip>
! Default = -
! Allowed = integer linked array(50)
! The trace number to start shot point tics.
!</Help>
!
!<Help KEYWORD="INTIC">
!<Tip> Manual shot tic marks.  </Tip>
! Default = -
! Allowed = integer linked array(50)
! The number of trace intervals between shot point tics.
!</Help>
!
!<Help KEYWORD="ITTIC">
!<Tip> Manual shot tic marks.  </Tip>
! Default = -
! Allowed = integer linked array(50)
! The total number of tics to do.
!</Help>
!
!<Help KEYWORD="SLTIM">
!<Tip> Manual side labels.  </Tip>
! Default = -
! Allowed = real linked array(50)
! The time in seconds to position the label.
!</Help>
!
!<Help KEYWORD="SLBL">
!<Tip> Manual side labels.  </Tip>
! Default = -
! Allowed = char linked array(50)
! The annotation for the side label.  24 characters available.
!</Help>
!
!<Help KEYWORD="SLSZ">
!<Tip> Manual side labels.  </Tip>
! Default = .15
! Allowed = real linked array(50)
! The character size of the side label in inches or cm.  A zero here will be
! plotted at .15 inch.
!</Help>
!
!<Help KEYWORD="SLROW">
!<Tip> Manual side labels  </Tip>
! Default = 1
! Allowed = integer linked array(50)
! The row you are labeling refers to paneled plots.  If you are not paneling
! use 1.
!</Help>
!
!    --------------- Automatic Positioning of Annotation -------------------
!
!<Help KEYWORD="CALD">
!<Tip> Automatic calculation of annotation location above section. </Tip>
! Default = YES
! Allowed = YES, NO, YREV
! If CALD = YES, SPLT calculates the location of annotation elements above the
! section automatically.
! If CALD = NO, you will be prompted for individual location values.
! If CALD = YREV, you must review the values SPLT calculates automatically.
!
! You cannot exit the front-end until CALD has been set to YES or NO.
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
!<Help KEYWORD="SCDA">
!<Tip> Distance of scale above the section. </Tip>
! Default = -
! Allowed = real > 0.0
! Active if CALD = NO.
!</Help>
!<Help KEYWORD="SEDA">
!<Tip> Distance of the survace elevation plot above the section. </Tip>
! Default = -
! Allowed = real > 0.0
! Active if CALD = NO.
!</Help>
!
!</HelpSection>

!
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module splt_module
      use ameq_module
      use atblk_module
      use cio_module
      use cpsplot_module
      use getlun_module
      use getsys_module
      use hist_module
      use mem_module
      use mth_module
      use named_constants_module
      use pathcheck_module
      use pc_module
      use pkutil_module
      use string_module
      use velfile_module
      use wrdc_module
      implicit none
      private
      public :: splt_create     ! uses the parameter cache.
      public :: splt_initialize
      public :: splt_update     ! uses the parameter cache.
      public :: splt_delete
!<execute_only>
      public :: splt            ! main execution (trace processing) routine.
      public :: splt_wrapup
!</execute_only>


      character(len=100),public,save :: splt_IDENT = &
       '$Id: splt.f90,v 1.37 2007/01/17 14:15:30 Goodger beta sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: splt_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.
        logical                    :: allopen
        integer                    :: tldots(6)        ! process parameters.
        integer                    :: num_tr           ! process parameters.
        integer                    :: hdr_lab(4)       ! process parameters.
        character(len=8)           :: lab_init         ! process parameters.
        integer                    :: hdr_chg          ! process parameters.
        integer                    :: lab_inc          ! process parameters.
        integer                    :: copies           ! process parameters.
        integer                    :: wt               ! process parameters.
        integer                    :: ntpp             ! process parameters.
        integer                    :: nrow             ! process parameters.
        integer,pointer            :: pcol(:)          ! process parameters.
        integer,pointer            :: prow(:)          ! process parameters.
        integer                    :: hdr_bt           ! process parameters.
        integer                    :: bthc             ! process parameters.
        integer                    :: tlst             ! process parameters.
        integer                    :: hdr_off          ! process parameters.
        integer                    :: tbst             ! process parameters.
        integer                    :: tbib             ! process parameters.
        integer                    :: tbtot            ! process parameters.
        integer,pointer            :: titr(:)          ! process parameters.
        integer,pointer            :: intv(:)          ! process parameters.
        integer,pointer            :: incr(:)          ! process parameters.
        integer,pointer            :: totl(:)          ! process parameters.
        integer,pointer            :: intic(:)         ! process parameters.
        integer,pointer            :: ittic(:)         ! process parameters.
        integer,pointer            :: ttotl(:)         ! process parameters.
        integer,pointer            :: slrow(:)         ! process parameters.
        integer                    :: us               ! process parameters.
        integer                    :: flst             ! process parameters.
        integer                    :: hdr_fl           ! process parameters.
        integer                    :: hdr_dl           ! process parameters.
        integer                    :: hdr_pl           ! process parameters.
        integer                    :: skip_init        ! process parameters.
        integer                    :: skip             ! dependent variable.
        integer                    :: num_do           ! process parameters.
        integer                    :: num_skip         ! process parameters.
        integer                    :: istrt            ! dependant variable.
        integer                    :: nshots           ! dependant variable.
        integer                    :: nbts             ! dependant variable.
        integer,pointer            :: btskp(:)         ! dependant variable.
        integer,pointer            :: btdo(:)          ! dependant variable.
        integer,pointer            :: btbtwn(:)        ! dependant variable.
        integer,pointer            :: bttot(:)         ! dependant variable.
        integer                    :: btf              ! dependant variable.
        integer                    :: btadd            ! dependant variable.
        integer                    :: ibf              ! dependant variable.
        integer                    :: nct              ! dependant variable.
        integer                    :: nsamp            ! dependant variable.
        integer                    :: itif2            ! dependant variable.
        integer                    :: ian              ! dependant variable.
        integer                    :: itn              ! dependant variable.
        integer                    :: lun_cgmcc        ! dependant variable.
        integer                    :: ktrnum           ! dependant variable.
        integer                    :: ktdo             ! dependant variable.
        integer                    :: inten            ! dependant variable.
        integer                    :: ipan             ! dependant variable.
        character(len=4)           :: ndots            ! dependant variable.
        integer                    :: fhd1             ! dependant variable.
        integer                    :: ktpanl           ! dependant variable.
        integer                    :: kpan             ! dependant variable.
        integer                    :: npack            ! dependant variable.
        integer                    :: nparm            ! dependant variable.
        integer                    :: numbt            ! dependant variable.
        integer                    :: needvf           ! dependant variable.
        integer                    :: ivelx            ! dependant variable.
        integer                    :: ively            ! dependant variable.
        integer                    :: needblk          ! dependant variable.
        integer                    :: npairs           ! dependant variable.
        integer                    :: maxpairs         ! dependant variable.
        integer                    :: nvfid            ! dependant variable.
        integer                    :: kntvf            ! dependant variable.
        integer                    :: ktin             ! dependant variable.
        integer                    :: ktrdo            ! dependant variable.
        integer                    :: kdo              ! dependant variable.
        integer                    :: kgr              ! dependant variable.
        integer                    :: ktrskp           ! dependant variable.
        integer                    :: itf1             ! dependant variable.
        integer                    :: elevlun          ! dependant variable.
        integer                    :: knttie           ! dependant variable.
        integer                    :: kntrow           ! dependant variable.
        integer                    :: ntplot           ! dependant variable.
        character(len=8)           :: ipansav,ipanname ! dependant variable.
        integer                    :: nrlc             ! dependant variable.
        integer                    :: kntcol           ! dependant variable.
        integer                    :: ncol             ! dependant variable.
        integer                    :: ml10             ! dependant variable.
        integer                    :: iwrbtot          ! dependant variable.
        integer                    :: kntwrdb          ! dependant variable.
        integer                    :: ndxwrdb          ! dependant variable.
        integer                    :: kvfndx           ! dependant variable.
        integer                    :: kvfinc           ! dependant variable.
        integer                    :: ntvel            ! dependant variable.
        integer                    :: ivxid            ! dependant variable.
        integer                    :: iblinc           ! dependant variable.
        integer                    :: itf1inc          ! dependant variable.
        integer                    :: itf2inc          ! dependant variable.
        integer                    :: istrtsv          ! dependant variable.
        integer                    :: kendpnl          ! dependant variable.
        integer                    :: iuserbt          ! dependant variable.
        integer                    :: i3d              ! dependant variable.
        integer                    :: ifirst           ! dependant variable.
        integer                    :: i3dt             ! dependant variable.
        integer                    :: ibl3d            ! dependant variable.
        integer                    :: i3blf            ! dependant variable.
        integer                    :: mxpr             ! dependant variable.
        integer                    :: njob             ! dependant variable.
        integer                    :: icmin            ! dependant variable.
        integer                    :: icmax            ! dependant variable.
        integer                    :: icint            ! dependant variable.
        integer                    :: ntie             ! dependant variable.
        integer                    :: nstic            ! dependant variable.
        integer                    :: nslbl            ! dependant variable.
        integer                    :: nsltim           ! dependant variable.
        integer                    :: nplbl            ! dependant variable.
        integer                    :: tieh1            ! dependant variable.
        integer                    :: tieh2            ! dependant variable.
        integer                    :: nedtie1          ! dependant variable.
        integer                    :: nedtie2          ! dependant variable.
        integer                    :: blkhd            ! dependant variable.
        integer                    :: ipf1             ! dependant variable.
        integer                    :: blkhdl           ! dependant variable.
        integer                    :: pageflg          ! dependant variable.
        integer                    :: nhdr_lab         ! dependant variable.
        integer                    :: ntistr           ! dependant variable.
        integer                    :: ntrtic           ! dependant variable.
        integer                    :: ipn              ! parameter cache
        real                       :: time             ! process parameters.
        real                       :: ips              ! process parameters.
        real                       :: tpi              ! process parameters.
        real                       :: vxtol            ! process parameters.
        real                       :: ct               ! process parameters.
        real                       :: cs_lab           ! process parameters.
        real                       :: wid_vl           ! process parameters.
        real                       :: ar_ht            ! process parameters.
        real                       :: ar_len           ! process parameters.
        real                       :: frtb             ! process parameters.
        real                       :: cctf             ! process parameters.
        real                       :: vastrt           ! process parameters.
        real                       :: plcs             ! process parameters.
        real                       :: tlcs             ! process parameters.
        real                       :: tlcs_eng         ! process parameters.
        real                       :: ovjd             ! process parameters.
        real                       :: ovwd             ! process parameters.
        real                       :: frvf             ! process parameters.
        real                       :: blcs             ! process parameters.
        real                       :: tics             ! process parameters.
        real,pointer               :: trce(:)          ! process parameters.
        real,pointer               :: trtic(:)         ! process parameters.
        real,pointer               :: tistr(:)         ! process parameters.
        real,pointer               :: stlbl(:)         ! process parameters.
        real,pointer               :: tiinc(:)         ! process parameters.
        real,pointer               :: lbinc(:)         ! process parameters.
        real,pointer               :: szlbl(:)         ! process parameters.
        real,pointer               :: sltim(:)         ! process parameters.
        real,pointer               :: slsz(:)          ! process parameters.
        real                       :: sdas             ! process parameters.
        real                       :: tida             ! process parameters.
        real                       :: arda             ! process parameters.
        real                       :: scda             ! process parameters.
        real                       :: seda             ! process parameters.
        real                       :: cs_scale         ! process parameters.
        real                       :: cs_sid           ! process parameters.
        real                       :: dlref            ! process parameters.
        real                       :: plref            ! process parameters.
        real                       :: dlu100           ! process parameters.
        real                       :: plu100           ! process parameters.
        real                       :: pmse100          ! process parameters.
        real                       :: fintop           ! dependent variable
        real                       :: fintoptitl       ! dependent variable
        real                       :: tie1hd           ! dependent variable
        real                       :: tie2hd           ! dependent variable
        real                       :: ftiehd1          ! dependent variable
        real                       :: ftiehd2          ! dependent variable
        real                       :: fblkhd           ! dependent variable
        real                       :: cbth             ! dependent variable
        real                       :: pbth             ! dependent variable
        real                       :: halfs            ! dependent variable
        real                       :: fldsz            ! dependent variable
        real                       :: dlhite           ! dependent variable
        real                       :: plhite           ! dependent variable
        real                       :: fldht            ! dependent variable
        real                       :: fin100m          ! dependent variable
        real                       :: dlhalf           ! dependent variable
        real                       :: tis              ! dependent variable
        real                       :: ois              ! dependent variable
        real                       :: theta            ! dependent variable
        real                       :: whsec            ! dependent variable
        real                       :: scan             ! dependent variable
        real                       :: tunit            ! dependent variable
        real                       :: xmin             ! dependent variable
        real                       :: ymin             ! dependent variable
        real                       :: ymax             ! dependent variable
        real                       :: xmax             ! dependent variable
        real                       :: x1               ! dependent variable
        real                       :: y1               ! dependent variable
        real                       :: tmpx(5)          ! dependent variable
        real                       :: tmpy(5)          ! dependent variable
        real                       :: glav             ! dependent variable
        real                       :: flbkhd           ! dependent variable
        real                       :: xo               ! dependent variable
        real                       :: yo               ! dependent variable
        real                       :: oyo              ! dependent variable
        real                       :: xcort            ! dependent variable
        real                       :: ysdel            ! dependent variable
        real                       :: xend             ! dependent variable
        real                       :: otim             ! dependent variable
        real                       :: yend             ! dependent variable
        real                       :: yedel            ! dependent variable
        real                       :: bsmt1            ! dependent variable
        real                       :: bsmt2            ! dependent variable
        real                       :: basint           ! dependent variable
        real                       :: surmin           ! dependent variable
        real                       :: surmax           ! dependent variable
        real,pointer               :: vloc(:)          ! dependent variable
        real,pointer               :: tloc(:)          ! dependent variable
        double precision           :: fvelhd1          ! dependent variable
        double precision           :: fvelhd2          ! dependent variable
        double precision           :: fvelyd1          ! dependent variable
        double precision           :: fvelyd2          ! dependent variable
        real                       :: sclskp           ! dependant variable.
        real                       :: xbd              ! dependant variable.
        real                       :: ybd              ! dependant variable.
        real                       :: blhd1            ! dependent variable
        real                       :: blhd2            ! dependent variable
        real                       :: ftr1             ! dependent variable
        real                       :: ftr2             ! dependent variable
        real                       :: oblhd2           ! dependent variable
        real                       :: oftr2            ! dependent variable
        real                       :: xsav             ! dependent variable
        real                       :: dy10             ! dependent variable
        real                       :: ydif             ! dependent variable
        real                       :: xhist            ! dependent variable
        real                       :: xbias            ! dependent variable
        real                       :: ycdel            ! dependent variable
        real                       :: yddel            ! dependent variable
        real                       :: pcnt             ! dependent variable
        real                       :: blhead           ! dependent variable
        real                       :: sladd            ! dependent variable
        real                       :: pwrh             ! dependent variable
        real                       :: grpint           ! dependent variable
        real                       :: group1           ! dependent variable
        real                       :: group2           ! dependent variable
        real                       :: yodif            ! dependent variable
        real                       :: hitemax          ! dependent variable
        real                       :: tieskp           ! dependent variable
        real                       :: twid             ! dependent variable
        real                       :: yt               ! dependent variable
        real                       :: y2               ! dependent variable
        real                       :: xtrce            ! dependent variable
        real                       :: tht              ! dependent variable
        real                       :: skp              ! dependent variable
        real                       :: ips_eng
        real                       :: tpi_eng
        real                       :: cs_lab_eng
        real                       :: wid_vl_eng
        real                       :: ar_ht_eng
        real                       :: ar_len_eng
        real                       :: plcs_eng
        real                       :: ovwd_eng
        real                       :: blcs_eng
        real                       :: tics_eng
        real,pointer               :: szlbl_eng(:)
        real,pointer               :: slsz_eng(:)
        real                       :: sdas_eng
        real                       :: tida_eng
        real                       :: arda_eng
        real                       :: scda_eng
        real                       :: seda_eng
        real                       :: cs_sid_eng
        real                       :: cs_scale_eng
        character(len=FILENAME_LENGTH) :: path_atbm    ! process parameters.
        character(len=FILENAME_LENGTH) :: path_vel     ! process parameters.
        character(len=FILENAME_LENGTH) :: path_blk     ! process parameters.
        character(len=FILENAME_LENGTH) :: path_tie2    ! process parameters.
        character(len=FILENAME_LENGTH) :: path_tie1    ! process parameters.
        character(len=40)          :: sid(4)           ! process parameters.
        character(len=40),pointer  :: tie(:)           ! process parameters.
        character(len=32),pointer  :: plbl(:)          ! process parameters.
        character(len=24),pointer  :: slbl(:)          ! process parameters.
        character(len=16)          :: nam_lab(4)       ! process parameters.
        character(len=8)           :: opt_dev          ! process parameters.
        character(len=8)           :: dev_loc          ! process parameters.
        character(len=8)           :: quality          ! process parameters.
        character(len=8)           :: dev              ! dependent variable
        character(len=8)           :: scale            ! process parameters.
        character(len=8)           :: vint             ! process parameters.
        character(len=8)           :: meth_cct         ! process parameters.
        character(len=8),pointer   :: shot(:)          ! process parameters.
        character(len=8)           :: cald             ! process parameters.
        character(len=8)           :: lrrl             ! process parameters.
        character(len=8)           :: norm             ! process parameters.
        character(len=8)           :: rp               ! process parameters.
        character(len=8)           :: va               ! process parameters.
        character(len=8)           :: vai              ! process parameters.
        character(len=8)           :: opt_vl           ! process parameters.
        character(len=8)           :: ltab             ! process parameters.
        character(len=8)           :: ar_beg           ! process parameters.
        character(len=8)           :: ar_end           ! process parameters.
        character(len=8)           :: init             ! process parameters.
        character(len=8)           :: fold             ! process parameters.
        character(len=8)           :: hscl             ! process parameters.
        character(len=8)           :: dulp             ! process parameters.
        character(len=8)           :: nvrt             ! process parameters.
        character(len=8)           :: rspp             ! process parameters.
        character(len=8)           :: tbot             ! process parameters.
        character(len=8)           :: pmse             ! process parameters.
        character(len=8)           :: rezt             ! process parameters.
        character(len=8)           :: rtbp             ! process parameters.
        character(len=8)           :: alch             ! process parameters.
        character(len=8)           :: kfunc            ! process parameters.
        character(len=40)          :: tie1txt,tie2txt  ! dependent variable.
        character(len=8)           :: ksym             ! dependent variable.
        character(len=8)           :: blidfmt          ! dependent variable.
        character(len=56)          :: bltxt            ! dependent variable.
        integer                    :: nwih             ! global
        integer                    :: ndpt             ! global
        real                       :: dt               ! global
        real                       :: tstrt            ! global
        type(velfile_struct),pointer :: velfile

      end type splt_struct


      integer  :: print_lun,ntldots,nnam_lab
      integer,save :: kntsplt=1
      real     :: cti=2.54,scal=200.0
      character(len=8) :: survey_units
      character(len=16) :: plotname
      character(len=32) :: jobname
      logical  :: swap



!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(splt_struct),pointer,save :: object      ! needed for traps.



      integer,parameter     :: lrrl_noptions = 2
      character(len=4),save :: lrrl_options(lrrl_noptions)      &
                               =(/ 'RL','LR' /)

      integer    ,parameter :: yesno_noptions = 2
      character(len=4)      :: yesno_options(yesno_noptions)            &
                               = (/ 'YES ', 'NO  ' /)

      integer    ,parameter :: opt_vl_noptions = 4
      character(len=4)      :: opt_vl_options(opt_vl_noptions)            &
                               = (/ 'NO ', 'YES','BTL','TM ' /)

      integer    ,parameter :: dev_loc_noptions = 4
      character(len=8)      :: dev_loc_options(dev_loc_noptions)            &
                               = (/ 'HP5000A ','HP      ',&
                                    'TEST    ',&
                                    'SAVECGM '/)

      integer    ,parameter :: quality_noptions = 3
      character(len=8)      :: quality_options(quality_noptions)            &
                               = (/ 'PROD','MAX ','FAST'/)

      integer    ,parameter :: opt_dev_noptions = 2
      character(len=8)      :: opt_dev_options(opt_dev_noptions)            &
                               = (/ 'PAPER   ', 'FILM    ' /)

      integer    ,parameter :: meth_cct_noptions = 3
      character(len=8)      :: meth_cct_options(meth_cct_noptions)            &
                               = (/ 'NONE    ','MED     ','NOMED   ' /)

      integer    ,parameter :: norm_noptions = 2
      character(len=8)      :: norm_options(norm_noptions)            &
                               = (/ 'ALL     ','EACH    ' /)

      integer    ,parameter :: wt_noptions = 3
      integer               :: wt_options(wt_noptions)            &
                               = (/ 1, 2, 0 /)

      integer    ,parameter :: vint_noptions = 6
      character(len=8)      :: vint_options(vint_noptions)            &
                               = (/ 'TINT5   ','TINT7   ','TINT15  ',&
                                    'ORI     ',                      &
                                    'VELO    ','NONE    ' /)

      integer    ,parameter :: cald_noptions = 3
      character(len=8)      :: cald_options(cald_noptions)            &
                               = (/ 'YES     ','NO      ','YREV    '/)



      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine splt_create (obj)
      type(splt_struct),pointer :: obj       ! arguments


      allocate (obj)


      nullify  (obj%pcol)
      nullify  (obj%prow)
      nullify  (obj%titr)
      nullify  (obj%intv)
      nullify  (obj%incr)
      nullify  (obj%totl)
      nullify  (obj%intic)
      nullify  (obj%ittic)
      nullify  (obj%ttotl)
      nullify  (obj%slrow)
      nullify  (obj%btskp)
      nullify  (obj%btdo)
      nullify  (obj%btbtwn)
      nullify  (obj%bttot)
      nullify  (obj%trce)
      nullify  (obj%trtic)
      nullify  (obj%tistr)
      nullify  (obj%stlbl)
      nullify  (obj%lbinc)
      nullify  (obj%szlbl)
      nullify  (obj%sltim)
      nullify  (obj%slsz)
      nullify  (obj%szlbl_eng)
      nullify  (obj%slsz_eng)
      nullify  (obj%tie)
      nullify  (obj%plbl)
      nullify  (obj%slbl)
      nullify  (obj%shot)
      nullify  (obj%velfile)
      nullify  (obj%tloc)
      nullify  (obj%vloc)



!

      call splt_initialize (obj)
      return
      end subroutine splt_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine splt_delete (obj)
      type(splt_struct),pointer :: obj       ! arguments

!<execute_only>
      call splt_wrapup (obj)
!</execute_only>

      if (associated(obj%pcol   )) deallocate      (obj%pcol)
      if (associated(obj%prow   )) deallocate      (obj%prow)
      if (associated(obj%titr   )) deallocate      (obj%titr)
      if (associated(obj%intv   )) deallocate      (obj%intv)
      if (associated(obj%incr   )) deallocate      (obj%incr)
      if (associated(obj%totl   )) deallocate      (obj%totl)
      if (associated(obj%intic  )) deallocate      (obj%intic)
      if (associated(obj%ittic  )) deallocate      (obj%ittic)
      if (associated(obj%ttotl  )) deallocate      (obj%ttotl)
      if (associated(obj%slrow    )) deallocate      (obj%slrow)
      if (associated(obj%btskp  )) deallocate      (obj%btskp)
      if (associated(obj%btdo   )) deallocate      (obj%btdo)
      if (associated(obj%btbtwn )) deallocate      (obj%btbtwn)
      if (associated(obj%bttot  )) deallocate      (obj%bttot)
      if (associated(obj%trce   )) deallocate      (obj%trce)
      if (associated(obj%trtic  )) deallocate      (obj%trtic)
      if (associated(obj%tistr  )) deallocate      (obj%tistr)
      if (associated(obj%stlbl  )) deallocate      (obj%stlbl)
      if (associated(obj%tiinc  )) deallocate      (obj%tiinc)
      if (associated(obj%lbinc  )) deallocate      (obj%lbinc)
      if (associated(obj%szlbl  )) deallocate      (obj%szlbl)
      if (associated(obj%sltim  )) deallocate      (obj%sltim)
      if (associated(obj%slsz   )) deallocate      (obj%slsz)
      if (associated(obj%slsz_eng)) deallocate     (obj%slsz_eng)
      if (associated(obj%szlbl_eng)) deallocate    (obj%szlbl_eng)
      if (associated(obj%tie    )) deallocate      (obj%tie)
      if (associated(obj%plbl   )) deallocate      (obj%plbl)
      if (associated(obj%slbl   )) deallocate      (obj%slbl)
      if (associated(obj%shot   )) deallocate      (obj%shot)
      if (associated(obj%velfile  )) call velfile_delete(obj%velfile)
      if (associated(obj%tloc  )) deallocate       (obj%tloc)
      if (associated(obj%vloc  )) deallocate       (obj%vloc)



      deallocate(obj)
      return
      end subroutine splt_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine splt_initialize (obj)
      type(splt_struct),intent(inout) :: obj       ! arguments








      call pc_get_jdata('TRACE_LENGTH',obj%time)
      obj%ips_eng          = 5.0
      obj%tpi_eng          = 10.0
      obj%lrrl         = 'RL'
      obj%num_tr       = 100
      obj%ct           = 5.0
      obj%sid          = ' '
      obj%scale        = 'NO'
      obj%hdr_lab(1)   = 7
      obj%hdr_lab(2)   = 0
      obj%hdr_lab(3)   = 0
      obj%hdr_lab(4)   = 0
      obj%nhdr_lab     = 4
      obj%lab_init     = '50'
      obj%hdr_chg      = 7
      obj%lab_inc      = 50
      obj%nam_lab      = ' '
      obj%ltab         = 'NO'
      obj%cs_lab_eng       = 0.13
      obj%cs_sid_eng       = 0.27
      obj%cs_scale_eng = 0.13
      obj%opt_vl       = 'NO'
      obj%wid_vl_eng       = .005
      obj%shot         = ' '
      obj%ar_beg       = ' '
      obj%ar_end       = ' '
      obj%ar_ht_eng    = 0.0
      obj%ar_len_eng   = 2.5
      obj%path_atbm    = PATHCHECK_EMPTY
      obj%frtb         = 1.0
      obj%dev_loc      = 'HP5000A'
      obj%quality      = 'PROD'
      obj%opt_dev      = 'PAPER'
      obj%copies       = 1
      obj%init         = ' '
      obj%fold         = 'NO'
      obj%hscl         = 'NO'
      obj%dulp         = 'NO'
      obj%meth_cct     = 'NONE'
      obj%cctf         = 0.35
      obj%nvrt         = 'NO'
      obj%norm         = 'ALL'
      obj%wt           = 1
      obj%va           = 'YES'
      obj%vastrt       = 0.0
      obj%vai          = 'NO'
      obj%rp           = 'NO'
      obj%skip_init    = 0
      obj%skip         = 0
      obj%num_do       = 1
      obj%num_skip     = 0
      obj%ntpp         = 0
      obj%nrow         = 1
      obj%plcs_eng         = 0.3
      obj%rspp         = 'YES'
      obj%hdr_bt       = 3
      obj%bthc         = 0
      obj%tldots       = 0
      obj%tldots(3)    = 1
      obj%tldots(6)    = 2
      obj%tlcs_eng         = 0.0
      obj%tlst         = 0
      obj%ovjd         = 0.0
      obj%hdr_off      = 6
      obj%ovwd_eng     = 0.01
      obj%tbst         = 0
      obj%tbib         = 0
      obj%tbtot        = 0
      obj%path_vel     = PATHCHECK_EMPTY
      obj%frvf         = 1.0
      obj%vint         = 'TINT5'
      obj%vxtol        = 0.0
      obj%path_blk     = PATHCHECK_EMPTY
      obj%blcs_eng         = 0.3
      obj%path_tie1    = PATHCHECK_EMPTY
      obj%path_tie2    = PATHCHECK_EMPTY
      obj%tics_eng         = 0.07
      obj%tbot         = 'NO'
      obj%pmse         = 'NO'
      obj%pmse100      = 1.0
      obj%rezt         = 'YES'
      obj%us           = 0
      obj%cald         = 'YES'
      obj%rtbp         = 'NO'
      obj%sdas_eng         = 0.0
      obj%tida_eng         = 0.0
      obj%arda_eng         = 2.5
      obj%scda_eng         = 0.0
      obj%seda_eng         = 2.0
      obj%flst         = 0
      obj%hdr_fl       = 5
      obj%hdr_dl       = 0
      obj%hdr_pl       = 0
      obj%dlref        = 0.0
      obj%plref        = 0.0
      obj%dlu100       = 0.0
      obj%plu100       = 0.0
      obj%ipn          = 0
      obj%twid         = 0.0
      obj%ndots        = '400'
      obj%tht          = 0.0
      obj%ivelx        = 37
      obj%ively        = 38
      obj%allopen      = .false.




!          The backend requires these parameters in the English system,
!           so keep a copy which will always be in that system
!  --  For defaults only here -- also done in traps

      call pc_get_pdata('survey_units',survey_units)
      if(survey_units.eq.'METERS')then
        obj%ips=obj%ips_eng*cti
        obj%tpi=obj%tpi_eng/cti
        obj%cs_lab=obj%cs_lab_eng*cti
        obj%cs_sid=obj%cs_sid_eng*cti
        obj%wid_vl=obj%wid_vl_eng*cti
        obj%ar_ht=obj%ar_ht_eng*cti
        obj%ar_len=obj%ar_len_eng*cti
        obj%plcs=obj%plcs_eng*cti
        obj%ovwd=obj%ovwd_eng*cti
        obj%blcs=obj%blcs_eng*cti
        obj%tics=obj%tics_eng*cti
        obj%sdas=obj%sdas_eng*cti
        obj%tida=obj%tida_eng*cti
        obj%arda=obj%arda_eng*cti
        obj%scda=obj%scda_eng*cti
        obj%seda=obj%seda_eng*cti
        obj%cs_scale=obj%cs_scale_eng*cti
        obj%tlcs=obj%tlcs_eng*cti


      else

        obj%ips=obj%ips_eng
        obj%tpi=obj%tpi_eng
        obj%cs_lab=obj%cs_lab_eng
        obj%cs_sid=obj%cs_sid_eng
        obj%wid_vl=obj%wid_vl_eng
        obj%ar_ht=obj%ar_ht_eng
        obj%ar_len=obj%ar_len_eng
        obj%plcs=obj%plcs_eng
        obj%ovwd=obj%ovwd_eng
        obj%blcs=obj%blcs_eng
        obj%tics=obj%tics_eng
        obj%sdas=obj%sdas_eng
        obj%tida=obj%tida_eng
        obj%arda=obj%arda_eng
        obj%scda=obj%scda_eng
        obj%seda=obj%seda_eng
        obj%cs_scale=obj%cs_scale_eng
        obj%tlcs=obj%tlcs_eng
      endif

      if(obj%cs_scale_eng.lt.0.00001)obj%cs_scale_eng=0.13



      obj%istrt        = 0
      obj%nbts         = 0
      obj%btf          = 0
      obj%btadd        = 0
      obj%ibf          = 0
      obj%nct          = 0
      obj%nsamp        = 0
      obj%itif2        = 0
      obj%ian          = 0
      obj%itn          = 0
      obj%ktrnum       = 0
      obj%inten        = 0
      obj%ipan         = 0
      obj%fhd1         = 0
      obj%ktpanl       = 0
      obj%kpan         = 0
      obj%npack        = 0
      obj%nparm        = 0
      obj%numbt        = 0
      obj%needvf       = 0
      obj%needblk      = 0
      obj%npairs       = 0
      obj%nvfid        = 0
      obj%kntvf        = 0
      obj%ktin         = 0
      obj%ktrdo        = 0
      obj%kdo          = 0
      obj%kgr          = 0
      obj%ktrskp       = 0
      obj%itf1         = 0
      obj%elevlun      = 0
      obj%knttie       = 0
      obj%kntrow       = 0
      obj%ntplot       = 0
      obj%ipansav      = ' '
      obj%ipanname     ='%PANLAA'
      obj%nrlc         = 0
      obj%kntcol       = 0
      obj%ncol         = 0
      obj%ml10         = 0
      obj%iwrbtot      = 0
      obj%kntwrdb      = 0
      obj%ndxwrdb      = 0
      obj%kvfndx       = 0
      obj%kvfinc       = 0
      obj%ntvel        = 0
      obj%ivxid        = 0
      obj%iblinc       = 0
      obj%itf1inc      = 0
      obj%itf2inc      = 0
      obj%istrtsv      = 0
      obj%kendpnl      = 0
      obj%iuserbt      = 0
      obj%i3d          = 0
      obj%ifirst       = 0
      obj%i3dt         = 0
      obj%ibl3d        = 0
      obj%i3blf        = 0
      obj%mxpr         = 0
      obj%njob         = 0
      obj%icmin        = 0
      obj%icmax        = 0
      obj%icint        = 0
      obj%ntie         = 0
      obj%nstic        = 0
      obj%nslbl        = 0
      obj%tieh1        = 0
      obj%tieh2        = 0
      obj%blkhd        = 0
      obj%ipf1         = 0
      obj%blkhdl       = 0
      obj%pageflg      = 0
      obj%fintop       = 0.0
      obj%fintoptitl   = 0.0
      obj%tie1hd       = 0.0
      obj%ftiehd1      = 0.0
      obj%ftiehd2      = 0.0
      obj%fblkhd       = 0.0
      obj%cbth         = 0.0
      obj%pbth         = 0.0
      obj%halfs        = 0.0
      obj%fldsz        = 0.0
      obj%dlhite       = 0.0
      obj%plhite       = 0.0
      obj%fldht        = 0.0
      obj%fin100m      = 0.0
      obj%dlhalf       = 0.0
      obj%tis          = 0.0
      obj%ois          = 0.0
      obj%theta        = 0.0
      obj%whsec        = 0.0
      obj%scan         = 0.0
      obj%xmin         = 0.0
      obj%ymin         = 0.0
      obj%ymax         = 0.0
      obj%xmax         = 0.0
      obj%x1           = 0.0
      obj%y1           = 0.0
      obj%tmpx(5)      = 0.0
      obj%tmpy(5)      = 0.0
      obj%glav         = 0.0
      obj%flbkhd       = 0.0
      obj%xo           = 0.0
      obj%yo           = 0.0
      obj%xcort        = 0.0
      obj%ysdel        = 0.0
      obj%xend         = 0.0
      obj%otim         = 0.0
      obj%yend         = 0.0
      obj%yedel        = 0.0
      obj%bsmt1        = 0.0
      obj%bsmt2        = 0.0
      obj%basint       = 0.0
      obj%surmin       = 999999999.0
      obj%surmax       = -999999999.0
      obj%fvelhd1      = 0.0
      obj%fvelyd1      = 0.0
      obj%fvelyd2      = 0.0
      obj%sclskp       = 0.0
      obj%xbd          = 0.0
      obj%ybd          = 0.0
      obj%blhd1        = 0.0
      obj%blhd2        = 0.0
      obj%ftr1         = 0.0
      obj%oblhd2       = 0.0
      obj%oftr2        = 0.0
      obj%xsav         = 0.0
      obj%dy10         = 0.0
      obj%ydif         = 0.0
      obj%xhist        = 0.0
      obj%xbias        = 0.0
      obj%ycdel        = 0.0
      obj%yddel        = 0.0
      obj%pcnt         = 0.0
      obj%blhead       = 0.0
      obj%sladd        = 0.0
      obj%pwrh         = 0.0
      obj%grpint       = 0.0
      obj%group1       = 0.0
      obj%group2       = 0.0
      obj%yodif        = 0.0
      obj%hitemax      = 0.0
      obj%alch         = ' '
      obj%ksym         = ' '
      obj%blidfmt      = ' '
      obj%bltxt        = ' '
!
!          globals
      obj%nwih         = 0
      obj%ndpt         = 0
      obj%dt           = 0.0
      obj%tstrt        = fnil

      obj%pageflg=0
      obj%pwrh=99999999.0


      print_lun=pc_get_lun()


!  Initialize array counters
      obj%nstic=0
      obj%ntrtic=0
      obj%ntistr=0
      obj%nplbl=0
      obj%nslbl=0
      obj%nshots=0
      ntldots=6
      obj%ntie=0
      nnam_lab=0




      call splt_update (obj)
      return
      end subroutine splt_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine splt_update (obj)
      type(splt_struct),intent(inout),target :: obj             ! arguments




      integer     :: length,istat,k,nsid=4
      integer     :: iscan,kntr,nc,nstor,nscr,ostype         
      integer     :: i,j,m,stickerknt=1

!          Local array counters
      integer :: npcol,nprow,ntitr,nintv,nincr,ntotl,nintic,nittic,nslnrow
      integer ::        krow,nslsz,ntrce                     ,nszlbl  

      logical     :: almost_equal,found_error
      real        :: htwid,qtwid,tmp                   ,blline  
      real        :: b,f,x,y
      integer     :: nprocess_list  

      character(len=20),pointer :: process_list(:)

      character(len=1)  :: letter
      character(len=8)  :: ctmp,ctmp2,itemp,ipfname='%PRT1FA'
      character(len=8)  :: elevname='%ELEVXA'  
      character(len=8)  :: iannaxa='%ANNAXA',itracxa='%TRACXA'
      character(len=8)  :: icpname='%SPLTXA',icptrce='%SPLTTA'
      character(len=8)  :: icgmname='%SCGMXA',icgmtrce='%SCGMTA'
      character(len=8)  :: icgmcc='%SCGMCA'

      character(len=10) :: uname,project
      character(len=80) :: card,jobcrd,prjcrd,sticker,cmds(1)
      character(len=160):: lcard

      data jobcrd/'JOB = 123456789012345 USER = 1234567890  ROUTE = '/
      data prjcrd/'PROJECT = 1234567890 '/
      data sticker/'STICKER ID = SPLT   COPIES = 99  ROLL '/



      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_register_array_names ("hdr_lab_arrayset", (/  &
                                    "hdr_lab",              &
                                    "nam_lab" /))

      call pc_register_array_names ("trce_arrayset", (/  &
                                    "trce",              &
                                    "shot",              &
                                    "intv",              &
                                    "incr",              &
                                    "totl" /))

      call pc_register_array_names ("trtic_arrayset", (/  &
                                    "trtic",              &
                                    "intic",              &
                                    "ittic" /))

      call pc_register_array_names ("pcol_arrayset", (/  &
                                    "pcol",              &
                                    "prow",              &
                                    "plbl" /))

      call pc_register_array_names ("titr_arrayset", (/  &
                                    "titr",              &
                                    "tie " /))

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

!       Set local array counters
      ntrce=obj%nshots
      nintv=obj%nshots
      nintv=obj%nshots
      nincr=obj%nshots
      ntotl=obj%nshots

      nintic=obj%ntrtic
      nittic=obj%ntrtic

      obj%nsltim=obj%nslbl
      nslsz=obj%nslbl
      nslnrow=obj%nslbl

      npcol=obj%nplbl
      nprow=obj%nplbl

      ntitr=obj%ntie

      call pc_get ('tldots'  , obj%tldots,     ntldots,  splt_tldots_trap)
      call pc_get ('num_tr'  , obj%num_tr,               splt_num_tr_trap)
      call pc_get ('hdr_lab' , obj%hdr_lab,obj%nhdr_lab)
      call pc_alloc ('trce',     obj%trce,   ntrce)
      call pc_alloc ('shot',     obj%shot,   obj%nshots)
      call pc_alloc ('intv',     obj%intv,   nintv)
      call pc_alloc ('incr',     obj%incr,   nincr)
      call pc_alloc ('totl',     obj%totl,   ntotl, splt_totl_element_trap)
      call pc_call_array_trap('hdr_lab',splt_hdr_lab_trap)
      call pc_get ('lab_init', obj%lab_init,             splt_lab_init_trap)
      call pc_get ('hdr_chg',  obj%hdr_chg,              splt_hdr_chg_trap)
      call pc_get ('lab_inc',  obj%lab_inc,              splt_lab_inc_trap)
      call pc_get ('copies',   obj%copies,               splt_copies_trap)
      call pc_get ('skip_init',obj%skip_init,            splt_skip_init_trap)
      call pc_get ('num_do',   obj%num_do,               splt_num_do_trap)
      call pc_get ('num_skip',   obj%num_skip,               splt_num_skip_trap)
      call pc_get ('wt',       obj%wt,                   splt_wt_trap)
      call pc_get ('ntpp',     obj%ntpp,                 splt_ntpp_trap)
      call pc_get ('nrow',     obj%nrow,                 splt_nrow_trap)
      call pc_alloc ('pcol',     obj%pcol,   npcol)
      call pc_alloc ('prow',     obj%prow,   nprow)
      call pc_alloc ('plbl',     obj%plbl,   obj%nplbl,  splt_plbl_element_trap)
      call pc_get ('hdr_bt' ,  obj%hdr_bt,               splt_hdr_bt_trap)
      call pc_get ('bthc',     obj%bthc,                 splt_bthc_trap)
      call pc_get ('tlst',     obj%tlst,                 splt_tlst_trap)
      call pc_get ('hdr_off',  obj%hdr_off,              splt_hdr_off_trap)
      call pc_get ('tbst',     obj%tbst,                 splt_tbst_trap)
      call pc_get ('tbib',     obj%tbib,                 splt_tbib_trap)
      call pc_get ('tbtot',    obj%tbtot,                splt_tbtot_trap)
      call pc_alloc ('titr',     obj%titr,   ntitr)
      call pc_alloc ('intic',    obj%intic,  nintic)
      call pc_alloc ('ittic',    obj%ittic,  nittic)
!!      call pc_alloc ('ttotl',    obj%ttotl,  nttotl,       splt_ttotl_trap)
      call pc_alloc ('slrow',      obj%slrow,    krow)
!!      call pc_get ('us',       obj%us,                   splt_us_trap)
      call pc_get ('flst',     obj%flst,                 splt_flst_trap)
      call pc_get ('hdr_dl',   obj%hdr_dl,               splt_hdr_dl_trap)
      call pc_get ('hdr_fl' ,  obj%hdr_fl,               splt_hdr_fl_trap)
      call pc_get ('hdr_pl',   obj%hdr_pl,               splt_hdr_pl_trap)
      call pc_get ('time',     obj%time,                 splt_time_trap)
      call pc_get ('ips',      obj%ips,                  splt_ips_trap)
      call pc_get ('tpi',      obj%tpi,                  splt_tpi_trap)
      call pc_get ('vxtol',    obj%vxtol,                splt_vxtol_trap)
      call pc_get ('ct',       obj%ct,                   splt_ct_trap)
      call pc_get ('cs_lab',   obj%cs_lab,               splt_cs_lab_trap)
      call pc_get ('wid_vl',   obj%wid_vl,               splt_wid_vl_trap)
      call pc_get ('ar_ht',    obj%ar_ht,                splt_ar_ht_trap)
      call pc_get ('ar_len',   obj%ar_len,               splt_ar_len_trap)
      call pc_get ('frtb',     obj%frtb,                 splt_frtb_trap)
      call pc_get ('cctf',     obj%cctf,                 splt_cctf_trap)
      call pc_get ('vastrt',   obj%vastrt,               splt_vastrt_trap)
      call pc_get ('plcs',     obj%plcs,                 splt_plcs_trap)
      call pc_get ('tlcs',     obj%tlcs,                 splt_tlcs_trap)
      call pc_get ('ovjd',     obj%ovjd,                 splt_ovjd_trap)
      call pc_get ('ovwd',     obj%ovwd,                 splt_ovwd_trap)
      call pc_get ('frvf',     obj%frvf,                 splt_frvf_trap)
      call pc_get ('blcs',     obj%blcs,                 splt_blcs_trap)
      call pc_get ('tics',     obj%tics,                 splt_tics_trap)
      call pc_get ('sdas',     obj%sdas,                 splt_sdas_trap)
      call pc_get ('tida',     obj%tida,                 splt_tida_trap)
      call pc_get ('arda',     obj%arda,                 splt_arda_trap)
      call pc_get ('scda',     obj%scda,                 splt_scda_trap)
      call pc_get ('seda',     obj%seda,                 splt_seda_trap)
      call pc_get ('cs_scale', obj%cs_scale,             splt_cs_scale_trap)
      call pc_get ('cs_sid',   obj%cs_sid,               splt_cs_sid_trap)
      call pc_get ('dlref',    obj%dlref,                splt_dlref_trap)
      call pc_get ('plref',    obj%plref,                splt_plref_trap)
      call pc_get ('dlu100',   obj%dlu100,               splt_dlu100_trap)
      call pc_get ('plu100',   obj%plu100,               splt_plu100_trap)
      call pc_get ('path_atbm',obj%path_atbm,            splt_path_atbm_trap)
      call pc_get ('path_vel', obj%path_vel,             splt_path_vel_trap)
      call pc_get ('path_blk', obj%path_blk,             splt_path_blk_trap)
      call pc_get ('path_tie2',obj%path_tie2,            splt_path_tie2_trap)
      call pc_get ('path_tie1',obj%path_tie1,            splt_path_tie1_trap)
      call pc_get ('sid',      obj%sid,   nsid,          splt_sid_trap)
      call pc_alloc ('tie',    obj%tie,   obj%ntie)
      call pc_get ('nam_lab',  obj%nam_lab,nnam_lab,     splt_nam_lab_trap)
      call pc_get ('opt_dev',  obj%opt_dev,              splt_opt_dev_trap)
      call pc_get ('dev_loc',  obj%dev_loc,              splt_dev_loc_trap)
      call pc_get ('quality',  obj%quality)
      call pc_get ('scale',    obj%scale,                splt_scale_trap)
      call pc_get ('vint',     obj%vint,                 splt_vint_trap)
      call pc_get ('meth_cct', obj%meth_cct,             splt_meth_cct_trap)
      call pc_get ('cald',     obj%cald,                 splt_cald_trap)
      call pc_get ('lrrl',     obj%lrrl,                 splt_lrrl_trap)
      call pc_get ('norm',     obj%norm,                 splt_norm_trap)
      call pc_get ('rp',       obj%rp,                   splt_rp_trap)
      call pc_get ('va',       obj%va,                   splt_va_trap)
      call pc_get ('vai',      obj%vai,                  splt_vai_trap)
      call pc_get ('opt_vl',   obj%opt_vl,               splt_opt_vl_trap)
      call pc_get ('ltab',     obj%ltab,                 splt_ltab_trap)
      call pc_get ('ar_beg',   obj%ar_beg,               splt_ar_beg_trap)
      call pc_get ('init',     obj%init,                 splt_init_trap)
      call pc_get ('ar_end',   obj%ar_end,               splt_ar_end_trap)
      call pc_get ('fold',     obj%fold,                 splt_fold_trap)
      call pc_get ('nvrt',     obj%nvrt,                 splt_nvrt_trap)
      call pc_get ('rspp',     obj%rspp,                 splt_rspp_trap)
      call pc_get ('tbot',     obj%tbot,                 splt_tbot_trap)
      call pc_get ('pmse',     obj%pmse,                 splt_pmse_trap)
      call pc_get ('pmse100',  obj%pmse100,              splt_pmse100_trap)
      call pc_get ('rezt',     obj%rezt,                 splt_rezt_trap)
!!      call pc_get ('rtbp',     obj%rtbp,                 splt_rtbp_trap)
!!      call pc_get ('alch',     obj%alch,                 splt_alch_trap)
      call pc_alloc ('trtic',    obj%trtic,  obj%ntrtic)
!!      call pc_alloc ('tistr',    obj%tistr,  obj%ntistr,   splt_tistr_trap)
!!      call pc_alloc ('stlbl',    obj%stlbl,  nstlbl,       splt_stlbl_trap)
!!      call pc_alloc ('tiinc',    obj%tiinc,  ntiinc,       splt_tiinc_trap)
!!      call pc_alloc ('lbinc',    obj%lbinc,  nlbinc,       splt_lbinc_trap)
!!      call pc_alloc ('szlbl',    obj%szlbl,  nszlbl,       splt_szlbl_trap)
      call pc_alloc ('sltim',    obj%sltim,  obj%nsltim)
      call pc_alloc ('slsz',     obj%slsz,   nslsz)
      call pc_alloc ('slrow',      obj%slrow,    nslnrow)
      call pc_alloc ('slbl',     obj%slbl,   obj%nslbl)

      call pc_call_arrayset_trap('TRCE_ARRAYSET',splt_trce_arrayset_trap)
      call pc_call_arrayset_trap('TRTIC_ARRAYSET',splt_trtic_arrayset_trap)
      call pc_call_arrayset_trap('SLTIM_ARRAYSET',splt_sltim_arrayset_trap)
      call pc_call_arrayset_trap('PCOL_ARRAYSET',splt_pcol_arrayset_trap)
      call pc_call_arrayset_trap('TITR_ARRAYSET',splt_titr_arrayset_trap)




!      Insure linked array counters have the same lengths
      if(ntrce.ne.obj%nshots.or.nintv.ne.obj%nshots.or.nintv.ne.obj%nshots&
         .or.nincr.ne.obj%nshots.or.ntotl.ne.obj%nshots)then
         call pc_error('Shot card arrays do not have the same length')
         obj%nshots=min(obj%nshots,ntrce,nintv,nincr,ntotl)
      endif

      if(nintic.ne.obj%ntrtic.or.nittic.ne.obj%ntrtic)then
        call pc_error('Tic mark arrays do not have the save length')
        obj%ntrtic=min(obj%ntrtic,nintic,nittic)
      endif

      if(obj%nsltim.ne.obj%nslbl.or.nslsz.ne.obj%nslbl.or.nslnrow.ne.obj%nslbl)&
        then
        call pc_error('Side label arrays do not have the same length')
        obj%nslbl=min(obj%nslbl,obj%nsltim,nslsz,nslnrow)
      endif

      if(obj%nplbl.ne.0)then
        if(npcol.ne.obj%nplbl.or.nprow.ne.obj%nplbl)then
          call pc_error('Panel label arrays do not have the same length')
          write(card,*)'npcol = ',npcol,' nprow = ',nprow,' nplbl = ',&
                        obj%nplbl
          call pc_error(card)
          obj%nplbl=min(obj%nplbl,npcol,nprow)
        endif
      endif

      if(ntitr.ne.obj%ntie)then
        call pc_error('Tie line arrays do not have the same length')
        obj%ntie=min(obj%ntie,ntitr)
      endif



!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      call pc_call_end_trap(splt_end_trap)




!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

!          Write out the splt2cgm card if this is the first splt
      nullify(process_list)
      nprocess_list=0
      call pc_alloc_jdata('PROCESS_LIST',process_list,nprocess_list)
      DO i=1,nprocess_list
        j=i
        if(process_list(i).eq.'SPLT')exit
      ENDDO 
      cmds(1)='NONE'
      if(obj%dev_loc(1:2).eq.'VE'&
              .or.obj%dev_loc.eq.'SAVE')then
        cmds(1)='spst >> online'
        obj%dev='CONP'
      else if(obj%dev_loc.eq.'SAVECGM'.or.obj%dev_loc(1:2).eq.'HP')then
        cmds(1)='splt2cgm >> online'
        obj%dev='CGM'
      else
        obj%dev='CGM'
      endif
      if(cmds(1)(1:4).ne.'NONE'.and.j.eq.obj%ipn)then
        call pc_put_control ('CMD_AFTER_BSCRIPT',cmds,1)
      endif


      call pc_put_options_field ('LTAB', yesno_options, yesno_noptions)
      call pc_put_options_field ('LRRL', lrrl_options, lrrl_noptions)
      call pc_put_options_field ('DEV_LOC', dev_loc_options,dev_loc_noptions)
      call pc_put_options_field ('QUALITY', quality_options,quality_noptions)
      call pc_put_options_field ('OPT_VL', opt_vl_options, opt_vl_noptions)
      call pc_put_options_field ('OPT_DEV', opt_dev_options,opt_dev_noptions)
      call pc_put_options_field ('FOLD', yesno_options, yesno_noptions)
      call pc_put_options_field ('METH_CCT',meth_cct_options,meth_cct_noptions)
      call pc_put_options_field ('NVRT', yesno_options, yesno_noptions)
      call pc_put_options_field ('NORM', norm_options, norm_noptions)
      call pc_put_options_field ('WT', wt_options, wt_noptions)
      call pc_put_options_field ('VA', yesno_options, yesno_noptions)
      call pc_put_options_field ('VAI', yesno_options, yesno_noptions)
      call pc_put_options_field ('RP', yesno_options, yesno_noptions)
      call pc_put_options_field ('RSPP', yesno_options, yesno_noptions)
      call pc_put_options_field ('VINT',vint_options,vint_noptions)
      call pc_put_options_field ('TBOT', yesno_options, yesno_noptions)
      call pc_put_options_field ('PMSE', yesno_options, yesno_noptions)
      call pc_put_options_field ('REZT', yesno_options, yesno_noptions)
      call pc_put_options_field ('CALD', cald_options, cald_noptions)


      call pc_put ('tldots'  , obj%tldots, ntldots)
      call pc_put ('num_tr'  , obj%num_tr)
      call pc_put ('hdr_lab' , obj%hdr_lab,obj%nhdr_lab)
      call pc_put ('lab_init', obj%lab_init)
      call pc_put ('hdr_chg',  obj%hdr_chg)
      call pc_put ('lab_inc',  obj%lab_inc)
      call pc_put ('copies',   obj%copies)
      call pc_put ('skip_init',obj%skip_init)
      call pc_put ('num_do',   obj%num_do)
      call pc_put ('num_skip',   obj%num_skip)
      call pc_put ('wt',       obj%wt)
      call pc_put ('ntpp',     obj%ntpp)
      call pc_put ('nrow',     obj%nrow)
      call pc_put ('pcol',     obj%pcol,   obj%nplbl)
      call pc_put ('prow',     obj%prow,   obj%nplbl)
      call pc_put ('hdr_bt' ,  obj%hdr_bt)
      call pc_put ('bthc',     obj%bthc)
      call pc_put ('tlst',     obj%tlst)
      call pc_put ('hdr_off',  obj%hdr_off)
      call pc_put ('tbst',     obj%tbst)
      call pc_put ('tbib',     obj%tbib)
      call pc_put ('tbtot',    obj%tbtot)
      call pc_put ('titr',     obj%titr,   ntitr)
      call pc_put ('intv',     obj%intv,   obj%nshots)
      call pc_put ('incr',     obj%incr,   obj%nshots)
      call pc_put ('totl',     obj%totl,   obj%nshots)
      call pc_put ('trtic',    obj%trtic,  obj%ntrtic)
      call pc_put ('intic',    obj%intic,  obj%ntrtic)
      call pc_put ('ittic',    obj%ittic,  obj%ntrtic)
!!      call pc_put ('ttotl',    obj%ttotl,  nttotl)
!!      call pc_put ('us',       obj%us)
      call pc_put ('flst',     obj%flst)
      call pc_put ('hdr_dl',   obj%hdr_dl)
      call pc_put ('hdr_fl' ,  obj%hdr_fl)
      call pc_put ('hdr_pl',   obj%hdr_pl)
      call pc_put ('time',     obj%time)
      call pc_put ('ips',      obj%ips)
      call pc_put ('tpi',      obj%tpi)
      call pc_put ('vxtol',    obj%vxtol)
      call pc_put ('ct',       obj%ct)
      call pc_put ('cs_lab',   obj%cs_lab)
      call pc_put ('wid_vl',   obj%wid_vl)
      call pc_put ('ar_ht',    obj%ar_ht)
      call pc_put ('ar_len',   obj%ar_len)
      call pc_put ('frtb',     obj%frtb)
      call pc_put ('cctf',     obj%cctf)
      call pc_put ('vastrt',   obj%vastrt)
      call pc_put ('plcs',     obj%plcs)
      call pc_put ('tlcs',     obj%tlcs)
      call pc_put ('ovjd',     obj%ovjd)
      call pc_put ('ovwd',     obj%ovwd)
      call pc_put ('frvf',     obj%frvf)
      call pc_put ('blcs',     obj%blcs)
      call pc_put ('tics',     obj%tics)
      call pc_put ('sdas',     obj%sdas)
      call pc_put ('tida',     obj%tida)
      call pc_put ('arda',     obj%arda)
      call pc_put ('scda',     obj%scda)
      call pc_put ('seda',     obj%seda)
      call pc_put ('cs_scale', obj%cs_scale)
      call pc_put ('cs_sid',   obj%cs_sid)
      call pc_put ('dlref',    obj%dlref)
      call pc_put ('plref',    obj%plref)
      call pc_put ('dlu100',   obj%dlu100)
      call pc_put ('plu100',   obj%plu100)
      call pc_put ('path_atbm',obj%path_atbm)
      call pc_put ('path_vel', obj%path_vel)
      call pc_put ('path_blk', obj%path_blk)
      call pc_put ('path_tie2',obj%path_tie2)
      call pc_put ('path_tie1',obj%path_tie1)
      call pc_put ('sid',      obj%sid   ,nsid)
      call pc_put ('tie',      obj%tie   ,obj%ntie)
      call pc_put ('nam_lab',  obj%nam_lab,nnam_lab)
      call pc_put ('opt_dev',  obj%opt_dev)
      call pc_put ('dev_loc',  obj%dev_loc)
      call pc_put ('quality',  obj%quality)
      call pc_put ('scale',    obj%scale)
      call pc_put ('vint',     obj%vint)
      call pc_put ('meth_cct', obj%meth_cct)
      call pc_put ('cald',     obj%cald)
      call pc_put ('lrrl',     obj%lrrl)
      call pc_put ('norm',     obj%norm)
      call pc_put ('rp',       obj%rp)
      call pc_put ('va',       obj%va)
      call pc_put ('vai',      obj%vai)
      call pc_put ('opt_vl',   obj%opt_vl)
      call pc_put ('ltab',     obj%ltab)
      call pc_put ('ar_beg',   obj%ar_beg)
      call pc_put ('init',     obj%init)
      call pc_put ('ar_end',   obj%ar_end)
      call pc_put ('fold',     obj%fold)
      call pc_put ('nvrt',     obj%nvrt)
      call pc_put ('rspp',     obj%rspp)
      call pc_put ('tbot',     obj%tbot)
      call pc_put ('pmse',     obj%pmse)
      call pc_put ('pmse100',  obj%pmse100)
      call pc_put ('rezt',     obj%rezt)
!!      call pc_put ('rtbp',     obj%rtbp)
!!      call pc_put ('alch',     obj%alch)
      call pc_put ('trce',     obj%trce,   obj%nshots)
!!      call pc_put ('tistr',    obj%tistr,  obj%ntistr)
!!      call pc_put ('stlbl',    obj%stlbl,  nstlbl)
!!      call pc_put ('tiinc',    obj%tiinc,  ntiinc)
!!      call pc_put ('lbinc',    obj%lbinc,  nlbinc)
!!      call pc_put ('szlbl',    obj%szlbl,  nszlbl)
      call pc_put ('sltim',    obj%sltim,  obj%nslbl)
      call pc_put ('slsz',     obj%slsz,   obj%nslbl )
      call pc_put ('slrow ',     obj%slrow,    obj%nslbl )
      call pc_put ('plbl',     obj%plbl,   obj%nplbl)
      call pc_put ('slbl',     obj%slbl,   obj%nslbl)
      call pc_put ('shot',     obj%shot,   obj%nshots)


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
      if(obj%ntistr.gt.0)call mem_alloc(obj%szlbl_eng,obj%ntistr)
      if(obj%nsltim.gt.0)call mem_alloc(obj%slsz_eng,obj%nsltim)
      if(survey_units.eq.'METERS')then
        do i=1,object%ntistr
          object%szlbl_eng(i)=object%szlbl(i)/cti
        enddo
        do i=1,object%nsltim
          object%slsz_eng(i)=object%slsz(i)/cti
          if(object%slsz_eng(i).le.0.00001)object%slsz_eng(i)=.15
        enddo
      else
        do i=1,object%ntistr
          object%szlbl_eng(i)=object%szlbl(i)
        enddo
        do i=1,object%nsltim
          object%slsz_eng(i)=object%slsz(i)
          if(object%slsz_eng(i).le.0.00001)object%slsz_eng(i)=.15
        enddo
      endif


      nstor=nszlbl+nslsz
      nscr =obj%nsamp+obj%npack

      swap=.false.
      ostype=getsys_ostype()
      if(ostype.eq.GETSYS_LINUX)swap=.true.


!      istat=hist_init(obj%ipn,'SPLT')


!               BEGIN OLD BACKEND SETUP ENTRY
      write(print_lun,*)' BEGIN           - SPLT SETUP '
!        ********  file names **************
!          print file

      call getlun(obj%ipf1,istat)
      if(istat.ne.0)then
         call pc_error('Unable to get unit number for splt print file')
      endif
      open(obj%ipf1,iostat=istat,status='NEW',file=ipfname)
      if(istat.ne.0)then
        call pc_error('Unable to open splt print file',trim(ipfname))
      endif
      letter=ipfname(7:7)
      k=ichar(letter)
      k=k+1
      letter=char(k)
      ipfname(7:7)=letter

      if(obj%pmse.eq.'YES')then
        call getlun(obj%elevlun,istat)
        if(istat.ne.0)then
           call pc_error('Unable to get unit number for splt pmse file')
        endif
        open(obj%elevlun,iostat=istat,status='NEW',file=elevname,&
             form='UNFORMATTED')
        if(istat.ne.0)then
          call pc_error('Unable to open elevation file')
        endif
        letter=elevname(7:7)
        k=ichar(letter)
        k=k+1
        letter=char(k)
        elevname(7:7)=letter
      endif

!    DON'T USE A SCRATCH FILE
!          obj%bthc option - trace number file
      call getlun(obj%btf,istat)
      if(istat.ne.0)then
         call pc_error('Unable to get unit number for splt bthc file')
      endif
      open(obj%btf,status='SCRATCH',form='UNFORMATTED')



!          panel option
      if(obj%ntpp.gt.0)then
        obj%ipansav=obj%ipanname
        call getlun(obj%ipan,istat)
        if(istat.ne.0)then
           call pc_error('Unable to get unit number for splt panl file')
        endif
        open(obj%ipan,status='NEW',iostat=istat,form='UNFORMATTED',&
             file=obj%ipanname)
        if(istat.ne.0)then
          call pc_error('Unable to open splt panl file ',trim(obj%ipanname))
        endif
        letter=obj%ipanname(7:7)
        k=ichar(letter)
        k=k+1
        letter=char(k)
        obj%ipanname(7:7)=letter
      endif


!
!
 8903 continue
        if(obj%hdr_lab(1).gt.0)then
          call string_cc2ii(obj%lab_init,obj%istrt)
          obj%istrtsv=obj%istrt
          obj%kendpnl=obj%ntpp
        endif

      obj%otim=obj%time
!          if paneling - reset time
      if(obj%ntpp.gt.0)then
        obj%time=obj%time*real(obj%nrow)+.1*(real(obj%nrow)-1)
        write(print_lun,*)' PANELED TIME = ',obj%time
      endif

!
!          set maximum height for device
      obj%hitemax=40.0
      if(obj%dev_loc.eq.'HP')obj%hitemax=35.0
!               here fintop = max inches needed for annotation
        obj%yt=obj%time*obj%ips_eng+obj%fintop
!!!        if(obj%yt.le.28.0)obj%hitemax=28.0
!!!      endif
      obj%fintop=obj%hitemax-obj%time*obj%ips_eng
!          For title block use 35.0 as maximum height
      obj%fintoptitl=35.0-obj%time*obj%ips_eng
!
      if(obj%lab_init.eq.'SHOT')then
         obj%istrt=0
      else
         call string_cc2ii(obj%lab_init,obj%istrt)
      endif
      if(obj%opt_vl.eq.'BTL'.and.obj%hdr_lab(1).gt.0)then
        obj%tbst=obj%istrt
        obj%tbib=obj%lab_inc
        obj%tbtot=(obj%num_tr-obj%istrt)/obj%lab_inc + 1
      endif
!
!
      if(obj%norm.eq.'EACH'.and.obj%ntpp.gt.0)then
        write(print_lun,*)&
     &  ' EACH INPUT TRACE WILL BE scaled to lav=1.0 before paneling'
      endif
!
!          save the job name
      call pc_get_jdata('JOBNAME',jobname)
!
      if(obj%dev.eq.'NETP')then
        write(print_lun,9002)iannaxa,itracxa
!            setup file names for trace and annotation files
        call getlun(obj%ian,istat)
        if(istat.ne.0)then
           call pc_error('Unable to get unit number for splt annotation file')
        endif
        open(obj%ian,status='NEW',iostat=istat,form='UNFORMATTED',file=iannaxa)
        if(istat.ne.0)then
          call pc_error('Unable to open annotation file ',trim(iannaxa))
        endif
        letter=iannaxa(7:7)
        k=ichar(letter)
        k=k+1
        letter=char(k)
        iannaxa(7:7)=letter



        call getlun(obj%itn,istat)
        if(istat.ne.0)then
           call pc_error('Unable to get unit number for splt trace file')
        endif
        open(obj%itn,status='NEW',iostat=istat,form='UNFORMATTED',file=itracxa)
        if(istat.ne.0)then
          call pc_error('Unable to open trace file ',trim(itracxa))
        endif
        letter=itracxa(7:7)
        k=ichar(letter)
        k=k+1
        letter=char(k)
        itracxa(7:7)=letter
 
      else if(obj%dev.eq.'CGM')then
        write(print_lun,9002)icgmname,icgmtrce
!            setup file names for trace,annotation, and control card files
        call getlun(obj%ian,istat)
        if(istat.ne.0)then
           call pc_error('Unable to get unit number for splt annotation file')
        endif
        open(obj%ian,status='NEW',iostat=istat,form='UNFORMATTED',file=icgmname)
        if(istat.ne.0)then
          call pc_error('Unable to open cgm annotation file ',icgmname)
        endif
        letter=icgmname(7:7)
        k=ichar(letter)
        k=k+1
        letter=char(k)
        icgmname(7:7)=letter

        call getlun(obj%itn,istat)
        if(istat.ne.0)then
           call pc_error('Unable to get unit number for splt trace file')
        endif
        open(obj%itn,status='NEW',iostat=istat,form='UNFORMATTED',file=icgmtrce)
        if(istat.ne.0)then
          call pc_error('Unable to open cgm trace file ',icgmtrce)
        endif
        letter=icgmtrce(7:7)
        k=ichar(letter)
        k=k+1
        letter=char(k)
        icgmtrce(7:7)=letter

!             the cgm control card file
        call getlun(obj%lun_cgmcc,istat)
        if(istat.ne.0)then
           call pc_error('Unable to get unit number for cgm control file')
        endif
        open(obj%lun_cgmcc,status='NEW',iostat=istat,form='UNFORMATTED',&
             file=icgmcc)
        if(istat.ne.0)then
          call pc_error('Unable to open control file ',icgmcc)
        endif
        letter=icgmcc(7:7)
        k=ichar(letter)
        k=k+1
        letter=char(k)
        icgmcc(7:7)=letter

      else 

        write(print_lun,9002)icpname,icptrce
!            setup file names for trace and annotation files if conplot
        call getlun(obj%ian,istat)
        if(istat.ne.0)then
           call pc_error('Unable to get unit number for splt annotation file')
        endif
        open(obj%ian,status='NEW',iostat=istat,form='UNFORMATTED',file=icpname)
        if(istat.ne.0)then
          call pc_error('Unable to open annotation file ',icpname)
        endif
        letter=icpname(7:7)
        k=ichar(letter)
        k=k+1
        letter=char(k)
        icpname(7:7)=letter



        call getlun(obj%itn,istat)
        if(istat.ne.0)then
           call pc_error('Unable to get unit number for splt trace file')
        endif
        open(obj%itn,status='NEW',form='UNFORMATTED',iostat=istat,file=icptrce)
        if(istat.ne.0)then
          call pc_error('Unable to open trace file ',icptrce)
        endif
        letter=icptrce(7:7)
        k=ichar(letter)
        k=k+1
        letter=char(k)
        icptrce(7:7)=letter
      endif     ! obj%dev.eq.'NETP'

!
      obj%pcnt=.85
      obj%xbias=0.0
      obj%glav=0.0
      obj%ktrnum=0
      obj%ntplot=0
      obj%ntvel =2
      obj%ktin =0
      obj%kntvf=0
      obj%knttie=1
      obj%kntwrdb=0
      obj%iwrbtot=0
      obj%ndxwrdb=2
      obj%tunit=200.0/obj%tpi_eng
      if(obj%lab_init.eq.'SHOT')then
        obj%istrt=obj%trce(1) + .5000001
        obj%lab_inc=obj%intv(1)
        obj%iwrbtot=obj%totl(1)
      endif
      obj%inten=15
      obj%kntrow=1
      obj%kntcol=1
      obj%ktpanl=0
      obj%halfs=200.0/obj%tpi_eng/2.0
!
      if(obj%wid_vl.le.0.0)obj%wid_vl_eng=.005
      if(obj%cs_lab.le.0.0)obj%cs_lab_eng=0.13
      if(obj%ovwd.le.0.0)obj%ovwd_eng=0.01
!
!          compensate for conplot solid font being .85 character height
      obj%cs_sid_eng=obj%cs_sid_eng/.85
      obj%cs_lab_eng=obj%cs_lab_eng/.85
      obj%cs_scale=obj%cs_scale/.85
      obj%blcs_eng=obj%blcs_eng/.85
      obj%tlcs_eng=obj%tlcs_eng/.85
      obj%sclskp=obj%scda_eng+obj%cs_scale_eng*2.3
      if(obj%ar_beg.ne.' '.or.obj%ar_end.ne.' ')obj%sclskp=obj%sclskp+.05
      if (obj%pmse.eq.'YES') then
        if(obj%scda.eq.0.0)obj%sclskp=obj%sclskp+5.0
      endif
!
      f=2 * obj%cs_lab_eng
      obj%tieskp=f + obj%cs_lab_eng * 1.5
      if(obj%hdr_lab(2).ne.0)then
        obj%tieskp=obj%tieskp+obj%cs_lab_eng+.15
        if(obj%tida_eng.le.0.6)obj%tida_eng=obj%tida_eng+obj%cs_lab_eng+.15
      endif
      if(obj%hdr_lab(3).ne.0)then
        obj%tieskp=obj%tieskp+obj%cs_lab_eng+.15
        if(obj%tida_eng.le.0.6)obj%tida_eng=obj%tida_eng+obj%cs_lab_eng+.15
      endif
      if(obj%hdr_lab(4).ne.0)then
        obj%tieskp=obj%tieskp+obj%cs_lab_eng+.15
        if(obj%tida_eng.le.0.6)obj%tida_eng=obj%tida_eng+obj%cs_lab_eng+.15
      endif
!
!          set obj%num_skip for skpm or sltm
!      if(islct.eq.'SKPM'l)then
!        obj%ktrskp=obj%num_skip*obj%num_do
!        obj%num_skip=(igrp1-1)*obj%num_do
!        obj%ktrdo=obj%num_do*obj%tot_do
!      endif
!      if(islct.eq.'SLTM'l)then
!        obj%num_skip=(igrps(1)-1)*obj%num_do
!        obj%ktrdo=obj%num_do
!      endif
!
!          set sizes for elevation, fold of stack
      obj%twid=1.0/obj%tpi_eng
      htwid=obj%twid/2.0
      qtwid=obj%twid/4.0
      obj%fldsz=htwid
      if(obj%tpi_eng.gt.50.0)obj%fldsz=obj%twid
      if(obj%dlu100.gt.0.0)obj%dlhite=obj%ips_eng/(obj%dlu100*10)
      if(obj%plu100.gt.0.0)obj%plhite=obj%ips_eng/(obj%plu100*10)
      obj%fin100m=obj%ips_eng/10.
      if(obj%flst.gt.0)obj%fldht=obj%fin100m/real(obj%flst)
      obj%dlhalf=qtwid*scal
      if(obj%twid.gt.0.025)obj%dlhalf=.025*scal
!
!             compute timing line no. char. size
      almost_equal=ameq(obj%tlcs_eng,0.0,.000001)
      if(almost_equal)obj%tlcs_eng   =   obj%ips_eng * .035
      if(obj%tlcs_eng.lt.-0.000000001)obj%tlcs_eng=0.0
      obj%tlcs_eng=obj%tlcs_eng/0.85
!
!             compute tm variables
      obj%ml10=obj%time*100
      obj%dy10 =scal*obj%ips_eng/100
!
!
!
      if(obj%lrrl.eq.'LR')then
        obj%theta=0.0
      else
        obj%theta=180.0
      endif
!
!         compute whole seconds
      k=obj%otim
      obj%whsec=obj%otim-k
!
!          number of samples to plot
      obj%nsamp=obj%otim/obj%dt + .0001
      if(obj%nsamp.gt.obj%ndpt)then
        call pc_error(" SPLT - ABORT TIME REQUESTED IS GREATER THAN TRACE &
     &TRACE LENGTH")
      endif
!
!          number of words needed to hold packed trace
      obj%npack=pkutil_npack8(obj%nsamp)
      obj%npack=obj%npack-1
!
!            channels per trace
      obj%nct=obj%ct
      k=obj%ct
      f=k
      if((obj%ct-f).ne.0.0)obj%nct=k+1
      obj%nct=obj%nct/2.0
      if(obj%nct.eq.0)obj%nct=1
!
!          number of scans per trace (always based on 200 dot)
      obj%scan=(200/obj%tpi_eng) + .05
      iscan=obj%scan
      obj%tunit=200/obj%tpi_eng
!                                  compute break for shot point
      obj%skp    =  2.0 * obj%cs_lab_eng
      obj%xmin    =  0.0
      obj%ymin    =  0.0
      kntr=obj%num_tr
      if(obj%ntpp.gt.0)then
        j=obj%ntpp*obj%nrow
        k=mod(obj%num_tr,j)
        obj%ncol=obj%num_tr/j+.0000001
        obj%nrlc=0
        if(k.gt.0)then
          m=obj%num_tr-(obj%ncol*j)
          obj%nrlc=m/obj%ntpp
          obj%ncol=obj%ncol+1
        endif
        kntr=obj%ncol*obj%ntpp
      endif
!          if paneling and user has not set blank traces - set it here
      if(obj%ntpp.gt.0.and.obj%nbts.eq.0)then
        obj%btskp(1)=obj%ntpp
!             make btdo large enough to plot timing line number
        b=obj%tlcs_eng*3.0
!!!        f=obj%stlbl(1)
        if(obj%ntistr.gt.0)then
          b=f
          do  i=1,obj%ntistr
            do j=1,obj%ttotl(i)
              f=f+obj%lbinc(i)
              b=amax1(f,b)
            enddo
          enddo
          f=log10(b)+1.5
          b=obj%tlcs_eng*f
        endif
!!        if(obj%us.gt.0)then
!!          f=log10(real(obj%us))+1.5
!!          b=obj%tlcs_eng*f
!!        endif
        k=b*obj%tpi_eng+2.500001
        obj%btdo(1)=k
        obj%btbtwn(1)=obj%ntpp
        obj%bttot(1)=obj%ncol-1
        obj%nbts=1
        if(obj%ncol.gt.1)obj%rtbp='YES'
        write(print_lun,*)' SPLT-BLANK TRACE PATTERN SET AS BTSKP = ',&
                            obj%btskp(1),   &
     &' BTDO = ',obj%btdo(1),' BTBTWN = ',obj%btbtwn(1),' BTTOT = ',&
     &  obj%bttot(1)
        obj%iuserbt=0
      else
        write(print_lun,*)' SPLT=> USER HAS SET THEIR OWN BLANK TRACE PATTERN'
        obj%iuserbt=1
      endif
!
!          count the number of blank traces
      obj%btadd=0
      obj%numbt=0
      do i=1,obj%nbts
        k=obj%btdo(i)*obj%bttot(i)
        obj%numbt=obj%numbt+k
      enddo
 8950 continue
!
      obj%x1      =  - ((3.0 * 200 + iscan * (obj%nct)) - iscan/2.0)
!
!          set ymax
      obj%ymax=40.0
!!      if(obj%dev_loc.eq.'SAVECGM')obj%ymax=35.0
!
!            set the origin to the lower left
!             if right to left plot - make y-coordinate decrease
      obj%sladd=0.0
      if(obj%nsltim.gt.0)then
        j=1
        obj%sladd=0.0
        do i=1,obj%nsltim
          nc=len_trim(obj%slbl(j))
!  f apparently is set from paneling ??
!!          almost_equal=ameq(f,0.0,.0000001)
!!          if(almost_equal)f=0.15
          f=0.15
          b=real(nc)*obj%slsz_eng(i)*obj%pcnt+1.0
          obj%sladd=amax1(obj%sladd,b)
          j=j+1
        enddo
        obj%sladd=obj%sladd*scal
      endif  ! if(obj%nsltim.gt.0)
      obj%xo=0.0
      tmp=200.0
      if(obj%lrrl.eq.'RL')tmp=-200.0
      card(11:16)='      '
      nc=len_trim(jobname)
      ctmp=obj%dev_loc
      if(obj%opt_dev.eq.'PREM')ctmp=trim(ctmp) // 'Q'
      if(obj%dev.eq.'CONP')call cpsplot_setx(obj%ian,ctmp)
        write(print_lun,*)jobname,obj%xend,obj%ymax          

!
      obj%ysdel=.3*scal
      obj%yedel=obj%ysdel+(obj%cs_lab_eng+.1)*scal
      obj%ycdel=obj%yedel+(obj%cs_lab_eng+.1)*scal
      obj%yddel =obj%ycdel+(obj%cs_lab_eng+.1)*scal

!
!
!
!             plot job card

       call cpsplot_fnt(obj%ian,19)

       call pc_get_pdata('USER_NAME',uname)
       jobcrd(7:21)=jobname
       jobcrd(30:39)=uname
       x=1.0*scal
       y=1.0*scal
       if(obj%lrrl.eq.'RL')y=-y
       call string_to_upper(jobcrd)
       call cpsplot_xsymbol (obj%ian,x,y,jobcrd,80,.2,15,2,90.0)


       x=x+.5*scal
!              plot the line and project
       call pc_get_pdata('PROJECT',project)
       prjcrd(11:20) = project
       call string_to_upper(prjcrd)
       call cpsplot_xsymbol (obj%ian,x,y,prjcrd,80,.2,15,2,90.0)

!            plot the sticker identification
       if(obj%fold.eq.'YES')then
         ctmp='FOLD'
       else
         ctmp='ROLL'
       endif
       sticker(34:37)=ctmp(1:4)
       call string_ii2cc((stickerknt),sticker(19:20),2)
       stickerknt=stickerknt+1
       call string_ii2cc(obj%copies,sticker(30:31),2)
       obj%xsav=x+.5*scal
       call string_to_upper(sticker)
       call cpsplot_xsymbol (obj%ian,obj%xsav,y,sticker,80,.2,15,2,90.0)

!
!      obj%xhist=0.0
 1111 format(1x,10a8)
!
!
!
!
        if(obj%path_vel.ne.PATHCHECK_EMPTY)then
!               initialize velocity functions
          call velfile_create(obj%velfile,obj%path_vel,obj%ivelx,obj%ively,&
                              found_error,lcard)
          if(found_error)then
            call pc_print(lcard)
            call pc_error('SPLT-->error encountered getting velocity functions')
          endif
          obj%needvf=1
        endif

       if(obj%path_blk.ne.PATHCHECK_EMPTY)then
!               initialize block labels
         obj%blidfmt=' '
         obj%ibf=cio_fopen(obj%path_blk,'r')
         if(obj%ibf.eq.CIO_ERROR)then
           call pc_error('SPLT--> ERROR encountered getting block label file')
         endif
         obj%iblinc=1
         length=cio_fgetline(card,80,obj%ibf)
         if(length.eq.CIO_EOF)then
          call pc_error('SPLT--> EOF encountered on first card in block file')
        endif
         read(card,9011)ctmp2
         if(ctmp2.eq.'X')then
           obj%blidfmt='X'
         endif
         if(obj%blidfmt.eq.' ')then
           read(card,9005)obj%blhd1,obj%blhd2,obj%bltxt,blline,&
                                      itemp,ctmp
         else
           read(card,9010)obj%blhd1,obj%blhd2,blline,obj%bltxt,&
                                      itemp,ctmp,ctmp2
         endif
         call string_cc2ii(ctmp,obj%blkhd,istat)
         if(istat.ne.1)then
           write(print_lun,*)' SPLT=> ERROR ENCOUNTERED GETTING HEADER &
      &FROM BLOCK LABEL FILE'
           write(print_lun,*)'        NO BLOCK LABELS WILL BE DONE'
           write(print_lun,*)' CTMP = ',ctmp
           obj%path_blk=PATHCHECK_EMPTY
         endif
         if(obj%path_blk.ne.PATHCHECK_EMPTY)then
           write(print_lun,*)' SPLT=> HEADER WORD USED FOR BLOCK LABELS = '&
                                   ,obj%blkhd
           if(itemp(1:1).eq.'2'.or.itemp(1:3).eq.'DEC')obj%iblinc=0
           if(itemp(1:2).eq.' 2')obj%iblinc=0
           obj%needblk=0
           obj%fhd1=0
           obj%oblhd2=-1.0
           obj%ibl3d=0
           almost_equal=ameq(blline,0.0,0.000001)
           if(.not.almost_equal)obj%ibl3d=1
           if(obj%ibl3d.eq.1)then
             k=mod(obj%blkhd,2)
             if(k.eq.0)then
               obj%blkhdl=obj%blkhd-1
             else
               obj%blkhdl=obj%blkhd+1
             endif
             rewind obj%ibf
           endif
         endif
       endif
!
!
      if(obj%path_tie1.ne.PATHCHECK_EMPTY)then
!              initialize first tie line file
        obj%itf1=cio_fopen(obj%path_tie1,'r')
        if(obj%itf1.eq.CIO_ERROR)then
           call pc_error('SPLT--> ERROR encountered getting tie file 1')
        endif
        obj%itf1inc=1
        length=cio_fgetline(card,80,obj%itf1)
        if(length.eq.CIO_EOF)then
          call pc_error('SPLT--> EOF encountered on first card in tie file 1')
        endif
        read(card,9007)obj%tie1hd,obj%tie1txt,itemp,ctmp
        call string_to_upper(obj%tie1txt)

        call string_cc2ii(ctmp,obj%tieh1,istat)
        if(istat.ne.1)go to 8965
        go to 8963
 8965   continue
        write(print_lun,*)' SPLT=> ERROR ENCOUNTERED GETTING TIE LINE &
     &HEADER FROM TIE FILE'
        write(print_lun,*)'      NO TIE LINES WILL BE DONE FROM TIE FILE 1'
        write(print_lun,*)' CTMP = ',ctmp
        obj%path_tie1=PATHCHECK_EMPTY
 8963   continue
        if(obj%path_tie1.ne.PATHCHECK_EMPTY)then
          write(print_lun,*)' SPLT=> HEADER WORD USED FOR TIE FILE 1 = ',&
                                obj%tieh1
          if(itemp(1:1).eq.'2'.or.itemp(1:2).eq.'DEC')obj%itf1inc=0
          obj%nedtie1=0
        endif
      endif


      if(obj%path_tie2.ne.PATHCHECK_EMPTY)then
!              initialize second tie line file
        obj%itif2=cio_fopen(obj%path_tie2,'r')
        if(obj%itif2.eq.CIO_ERROR)then
          call pc_error('SPLT--> ERROR opening tie file 2')
        endif
        obj%itf2inc=1
        length=cio_fgetline(card,80,obj%itif2)
        if(length.eq.CIO_EOF)then
          call pc_error('SPLT--> EOF on first card of tie file 2')
        endif
        read(card,9007)obj%tie2hd,obj%tie2txt,itemp,ctmp
        call string_to_upper(obj%tie2txt)
        call string_cc2ii(ctmp,obj%tieh2,istat)
        if(istat.ne.1)go to 8967
        go to 8966
 8967   continue
        write(print_lun,*)' SPLT=> ERROR ENCOUNTERED GETTING TIE LINE &
     &HEADER FROM TIE FILE'
        write(print_lun,*)'      NO TIE LINES WILL BE DONE FROM TIE FILE 2'
        write(print_lun,*)' CTMP = ',ctmp
        obj%path_tie2=PATHCHECK_EMPTY
 8966   continue
        if(obj%path_tie2.ne.PATHCHECK_EMPTY)then
          write(print_lun,*)' SPLT=> HEADER WORD USED FOR TIE FILE 2 = ',&
                                  obj%tieh2
          if(itemp(1:1).eq.'2'.or.itemp(1:3).eq.'DEC')obj%itf2inc=0
          obj%nedtie2=0
        endif
      endif

!
!
!
!
 8980 continue

!
!
!
!
      write(print_lun,*)' END             - SPLT SETUP '
      return
 9002 format(' ANNOTATION FILE = ',a7,' TRACE FILE = ',a7)
 9003 format(a80)
 9004 format(10x,a8,2x,a8,2x,2a8)
 9005 format(2f10.0,a10,f8.0,32x,a3,t77,a2)
 9006 format(2f10.0,a10,f8.0)
 9007 format(f10.0,a40,20x,a3,t77,a2)
 9008 format(f10.0,a40)
 9010 format(3f8.0,a51,a1,a2,a1)
 9011 format(t79,a1)
 9930 format(a132)
 9931 format(' INVALID TYPPLT IN SECTION CALL - WT = ',i5,' VA = ',a4,  &
     &' VAI = ',a4)
10050 format(f6.2,2x,f6.2,2x,a8)
!               END OLD BACKEND SETUP ENTRY

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine splt_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


      subroutine splt_num_tr_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
!       No checks made here
      return
      end subroutine splt_num_tr_trap

      subroutine splt_lab_init_trap (keyword)
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
!            LAB_INIT
      if(object%lab_init .eq. 'HDR')then
        call pc_put_sensitive_field_flag('HDR_CHG',.true.)
      else
        call pc_put_sensitive_field_flag('HDR_CHG',.false.)
      endif
      if(object%lab_init.eq.'SHOT'.or.object%lab_init.eq.'HDR')then
!!        call pc_put_sensitive_field_flag('LAB_INIT',.false.)
      endif

      return
      end subroutine splt_lab_init_trap

      subroutine splt_hdr_chg_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine splt_hdr_chg_trap

      subroutine splt_lab_inc_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
!         done in verify parameter section
      return
      end subroutine splt_lab_inc_trap

      subroutine splt_copies_trap (keyword)
      implicit none
       character(len=*),intent(in) :: keyword
      if (object%copies > 9) then
        object%copies = 9
        call pc_error('Number of copies is presently limited to 9')
      endif
      return
      end subroutine splt_copies_trap

      subroutine splt_skip_init_trap (keyword)
      implicit none
       character(len=*),intent(in) :: keyword
      return
      end subroutine splt_skip_init_trap

      subroutine splt_num_do_trap (keyword)
      implicit none
       character(len=*),intent(in) :: keyword
      return
      end subroutine splt_num_do_trap

      subroutine splt_num_skip_trap (keyword)
      implicit none
       character(len=*),intent(in) :: keyword
      return
      end subroutine splt_num_skip_trap

      subroutine splt_wt_trap (keyword)
      implicit none
       character(len=*),intent(in) :: keyword

       return
       end subroutine splt_wt_trap

      subroutine splt_ntpp_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%ntpp<0 .or. object%ntpp>object%num_tr) then
        call pc_error('NTPP must be between 1 and NT')
      endif

!          Panel Labels
      if(object%ntpp.eq.0)then
        call pc_put_sensitive_field_flag('NROW',.false.)
        call pc_put_sensitive_field_flag('PLCS',.false.)
        call pc_put_sensitive_field_flag('RSPP',.false.)
        call pc_put_sensitive_field_flag('BTHC',.true.)
        call pc_put_sensitive_field_flag('HDR_BT',.true.)
        call pc_put_sensitive_arrayset_flag('PCOL_ARRAYSET',.false.)
        object%pcol=0
        object%prow=0
        object%plbl=' '
        object%nplbl=0
      else
        call pc_put_sensitive_field_flag('NROW',.true.)
        call pc_put_sensitive_field_flag('PLCS',.true.)
        call pc_put_sensitive_field_flag('RSPP',.true.)
        call pc_put_sensitive_field_flag('BTHC',.false.)
        call pc_put_sensitive_field_flag('HDR_BT',.false.)
        call pc_put_sensitive_arrayset_flag('PCOL_ARRAYSET',.true.)
      endif

      return
      end subroutine splt_ntpp_trap

      subroutine splt_nrow_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%nrow > 26) then
        object%nrow = 26
        call pc_error('Number of rows is limited to 26')
      endif
      return
      end subroutine splt_nrow_trap

      subroutine splt_pcol_arrayset_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword

      integer :: i,j,k,nkol,nrlc,m

      do i=1,object%nplbl
        call string_to_upper(object%plbl(i))
        j = object%ntpp*object%nrow
        k = mod(object%num_tr,j)
        nkol = object%num_tr/j + 0.0000001
        nrlc = 0
        if (k > 0) then
          m = object%num_tr - nkol*j
          nrlc = m/object%ntpp
          nkol = nkol + 1
        endif
        if (object%pcol(i) > nkol) then
          call pc_error('PCOL number is out of range')
        endif
        if (object%prow(i) > object%nrow) then
          call pc_error('PROW number is out of range')
        endif
      enddo
      return
      end subroutine splt_pcol_arrayset_trap

      subroutine splt_hdr_bt_trap (keyword)
      implicit none
       character(len=*),intent(in) :: keyword

       return
       end subroutine splt_hdr_bt_trap
      subroutine splt_bthc_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_bthc_trap
      subroutine splt_tlst_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_tlst_trap
      subroutine splt_hdr_off_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_hdr_off_trap

      subroutine splt_tbst_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%tbst<0 .or. object%tbst>object%num_tr) then
        if (object%num_tr <= 0) then
          call pc_error('You must answer NUM_TR')
        else
          call pc_error('TBST is out of range')
        endif
      endif
      if (object%tbst == 0) then
        call pc_put_sensitive_field_flag('TBIB',.false.)
        call pc_put_sensitive_field_flag('TBTOT',.false.)
        object%tbib = 0
        object%tbtot = 0
      else
        call pc_put_sensitive_field_flag('TBIB',.true.)
        call pc_put_sensitive_field_flag('TBTOT',.true.)
      endif
      return
      end subroutine splt_tbst_trap

      subroutine splt_tbib_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%tbst.le.0)return
      if (object%tbib<=0 .or. object%tbib>object%num_tr) then
        if (object%num_tr <= 0) then
          call pc_error('You must answer NUM_TR')
        else
          call pc_error('TBIB is out of range')
        endif
      endif
      return
      end subroutine splt_tbib_trap

      subroutine splt_tbtot_trap (keyword)              ! scalar trap.
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments
      if(object%tbst.le.0)return
      if (object%tbtot<=0 .or. object%tbtot>object%num_tr) then
        if (object%num_tr <= 0) then
          call pc_error('You must answer NUM_TR')
        else
          call pc_error('TBTOT is out of range')
        endif
      endif
      return
      end subroutine splt_tbtot_trap

      subroutine splt_us_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_us_trap

      subroutine splt_flst_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
!          FOLD OF STACK INDICATOR
      if (object%flst > 0) then
        call pc_put_sensitive_field_flag('HDR_FL',.true.)
      else
        call pc_put_sensitive_field_flag('HDR_FL',.false.)
      endif
      call splt_rezt_sensitive
      return
      end subroutine splt_flst_trap

      subroutine splt_rezt_sensitive
      if((object%flst.gt.0.or.object%hdr_dl.gt.0.or.object%hdr_pl.gt.0)&
         .and.object%tstrt.lt.0.0)then
        call pc_put_sensitive_field_flag('REZT',.true.)
      else
        call pc_put_sensitive_field_flag('REZT',.false.)
      endif
      end subroutine splt_rezt_sensitive

      subroutine splt_hdr_dl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
!          DASHED LINE PLOT
      if (object%hdr_dl > 0) then
        call pc_put_sensitive_field_flag('DLU100',.true.)
      else
        call pc_put_sensitive_field_flag('DLU100',.false.)
        object%dlref = 0
        object%dlu100 = 0
      endif
      call splt_rezt_sensitive
      return
      end subroutine splt_hdr_dl_trap

      subroutine splt_hdr_fl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%hdr_fl<0 .or. object%hdr_fl>object%nwih) then
        call pc_error('HDR_FL out of range of allowable header words')
      endif
      end subroutine splt_hdr_fl_trap

      subroutine splt_hdr_pl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
!          PLUS SIGN PLOT
      if (object%hdr_pl > 0) then
        call pc_put_sensitive_field_flag('PLREF',.true.)
        call pc_put_sensitive_field_flag('PLU100',.true.)
      else
        call pc_put_sensitive_field_flag('PLREF',.false.)
        call pc_put_sensitive_field_flag('PLU100',.false.)
        object%plref = 0
        object%plu100 = 0
      endif
      call splt_rezt_sensitive
      end subroutine splt_hdr_pl_trap

      subroutine splt_time_trap (keyword)
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
      if(object%time.le.0.0)then
        call pc_error('TIME must be greater than zero - TIME reset to &
                      &global time')
        object%time=gtime
      endif
!          if conp - check for 4096 points - dplt 4080 points
      n = object%time/object%dt
      if (object%ntpp > 0)then
         n = (object%time + 0.1)*object%nrow/object%dt - 0.1/object%dt
      endif
      if (n > 4096) then
        call pc_error('You may plot only 4096 points on this device')
      endif
      return
      end subroutine splt_time_trap

      subroutine splt_ips_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
      return
      end subroutine splt_ips_trap

      subroutine splt_tpi_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
      return
      end subroutine splt_tpi_trap

      subroutine splt_vxtol_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%vxtol < 0.0) then
        call pc_error('VXTOL cannot be negative')
      endif
      return
      end subroutine splt_vxtol_trap

      subroutine splt_ct_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%ct.lt.0.0)then
        call pc_error('CT cannot be negative')
      endif
      return
      end subroutine splt_ct_trap

      subroutine splt_cs_lab_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
      if (object%cs_lab_eng > 1.0)then
        call pc_warning ('WARNING - CS_LAB exceeds 1 inch')
      endif
      if (object%cs_lab_eng <= 0.0) then
        call pc_error('CS_LAB must be greater than zero')
      endif
      return
      end subroutine splt_cs_lab_trap

      subroutine splt_wid_vl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
      return
      end subroutine splt_wid_vl_trap

      subroutine splt_ar_ht_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
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
      end subroutine splt_ar_ht_trap


      subroutine splt_ar_len_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
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
      end subroutine splt_ar_len_trap

      subroutine splt_frtb_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%frtb > 1.0) then
        call pc_warning('WARNING - FRTB must be 1.0 or less')
      endif
      return
      end subroutine splt_frtb_trap

      subroutine splt_cctf_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%cctf.lt.0.0)then
        call pc_error('CCTF cannot be negative')
      endif
      return
      end subroutine splt_cctf_trap

      subroutine splt_vastrt_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_vastrt_trap

      subroutine splt_plcs_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
      return
      end subroutine splt_plcs_trap

      subroutine splt_tlcs_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
      return
      end subroutine splt_tlcs_trap

      subroutine splt_ovjd_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword

!          OVJD
      if (object%ovjd /= 0) then
        call pc_put_sensitive_field_flag('HDR_OFF',.true.)
        call pc_put_sensitive_field_flag('OVWD',.true.)
      else
        call pc_put_sensitive_field_flag('HDR_OFF',.false.)
        call pc_put_sensitive_field_flag('OVWD',.false.)
      endif

      return
      end subroutine splt_ovjd_trap

      subroutine splt_ovwd_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
      return
      end subroutine splt_ovwd_trap

       subroutine splt_frvf_trap (keyword)
       implicit none
       character(len=*),intent(in) :: keyword
       if (object%frvf > 1.0) then
         call pc_warning('WARNING - FRVF must be 1.0 or less')
       endif
       return
       end subroutine splt_frvf_trap

       subroutine splt_blcs_trap (keyword)
       implicit none
       character(len=*),intent(in) :: keyword
       call splt_english
       if (survey_units.eq.'METERS') then
         if (object%blcs > 2.54) then
           call pc_warning('WARNING - BLCS exceeds 2.54 centimeters')
         endif
       else
         if (object%blcs > 1.0)then
            call pc_warning('WARNING - BLCS exceeds 1 inch')
         endif
       endif
       if (object%blcs <= 0.0) then
         call pc_error ('BLCS must be greater than zero')
       endif


       return
       end subroutine splt_blcs_trap

       subroutine splt_tics_trap (keyword)
       implicit none
       character(len=*),intent(in) :: keyword
       call splt_english
       if (survey_units.eq.'METERS') then
         if (object%tics > 2.54) then
           call pc_warning('WARNING - TICS exceeds 2.54 centimeters')
         endif
       else
         if (object%tics > 1.0)then
            call pc_warning('WARNING - TICS exceeds 1 inch')
         endif
       endif
       if (object%tics <= 0.0) then
         call pc_error ('TICS must be greater than zero')
       endif
       return
       end subroutine splt_tics_trap

      subroutine splt_sdas_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
      return
      end subroutine splt_sdas_trap

       subroutine splt_tida_trap (keyword)
       implicit none
       character(len=*),intent(in) :: keyword
       call splt_english
       return
       end subroutine splt_tida_trap
!
      subroutine splt_arda_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
      return
      end subroutine splt_arda_trap

      subroutine splt_scda_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
      return
      end subroutine splt_scda_trap

      subroutine splt_seda_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      real :: fmaxin
      character(len=80) :: cline
      call splt_english
      fmaxin = 40.0
      if (object%opt_dev=='HP'.or.object%opt_dev.eq.'SAVECGM') fmaxin = 35.0
      if (object%seda_eng>=0.0 .and. object%seda_eng<=fmaxin) return
      write (cline, *) 'SEDA must be between 0.0 and ', fmaxin
      call pc_error(cline)
      return
      end subroutine splt_seda_trap

      subroutine splt_cs_scale_trap (keyword)
      implicit none
       character(len=*),intent(in) :: keyword
       if(survey_units.eq.'METERS')then
         if(object%cs_scale.gt.2.54)then
           call pc_warning('CS_SCALE exceeds 2.54 centimeters')
         endif
       else
         if(object%cs_scale.gt.1.0)then
           call pc_warning('CS_SCALE exceeds 1 inch')
         endif
       endif
       if(object%cs_scale.lt.0.0)then
          call pc_error('You must set CS_SCALE')
       endif
       return
       end subroutine splt_cs_scale_trap


      subroutine splt_cs_sid_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
      if (survey_units == 'METERS') then
        if (object%cs_sid > 2.54)then
           call pc_warning('CS_SID exceeds 2.54 centimeters')
        endif
      else
        if (object%cs_sid > 1.0)then
          call pc_warning('WARNING - CS_SID exceeds 1 inch')
        endif
      endif
      if (object%cs_sid <= 0.0) then
        call pc_error('ERROR - You must set CS_SID')
      endif
      return
      end subroutine splt_cs_sid_trap

      subroutine splt_dlref_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_dlref_trap
      subroutine splt_plref_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_plref_trap
      subroutine splt_dlu100_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_dlu100_trap
      subroutine splt_plu100_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_plu100_trap

      subroutine splt_path_atbm_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword

      if(object%path_atbm.eq.PATHCHECK_EMPTY)return
      call pathcheck('path_atbm',object%path_atbm)
      if(object%path_atbm.ne.PATHCHECK_EMPTY)then
        call pc_put_sensitive_field_flag('FRTB',.true.)
      else
        call pc_put_sensitive_field_flag('FRTB',.false.)
        object%frtb = 1.0
      endif

      return
      end subroutine splt_path_atbm_trap

      subroutine splt_path_vel_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
!          VELOCITY FUNCTIONS
      if(object%path_vel.ne.PATHCHECK_EMPTY)then
        call pathcheck('path_vel',object%path_vel,'vel')
        call pc_put_sensitive_field_flag('FRVF',.true.)
        call pc_put_sensitive_field_flag('VINT',.true.)
        call pc_put_sensitive_field_flag('VXTOL',.true.)
      else
        call pc_put_sensitive_field_flag('FRVF',.false.)
        call pc_put_sensitive_field_flag('VINT',.false.)
        call pc_put_sensitive_field_flag('VXTOL',.false.)
      endif
      return
      end subroutine splt_path_vel_trap

      subroutine splt_path_blk_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%path_blk.eq.PATHCHECK_EMPTY)return
      call pathcheck('path_blk',object%path_blk,'blk')

!          BLOCK LABELS
      if(object%path_blk.ne.PATHCHECK_EMPTY)then
        call pc_put_sensitive_field_flag('BLCS',.true.)
      else
        call pc_put_sensitive_field_flag('BLCS',.false.)
      endif
      return

      end subroutine splt_path_blk_trap

       subroutine splt_path_tie2_trap (keyword)
       implicit none
       character(len=*),intent(in) :: keyword
       if(object%path_tie2.eq.PATHCHECK_EMPTY)return
       call pathcheck('path_tie2',object%path_tie2,'tie')
       return
       end subroutine splt_path_tie2_trap

       subroutine splt_path_tie1_trap (keyword)
       implicit none
       character(len=*),intent(in) :: keyword
       if(object%path_tie1.eq.PATHCHECK_EMPTY)return
       call pathcheck('path_tie1',object%path_tie1,'tie')
       call splt_tie_sensitive
       return
       end subroutine splt_path_tie1_trap

       subroutine splt_sid_trap (keyword)
       implicit none
       character(len=*),intent(in) :: keyword
       call string_to_upper(object%sid(1))
       call string_to_upper(object%sid(2))
       call string_to_upper(object%sid(3))
       call string_to_upper(object%sid(4))
       return
       end subroutine splt_sid_trap

      subroutine splt_nam_lab_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%nam_lab(1))
      call string_to_upper(object%nam_lab(2))
      call string_to_upper(object%nam_lab(3))
      call string_to_upper(object%nam_lab(4))
      return
      end subroutine splt_nam_lab_trap

      subroutine splt_opt_dev_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_opt_dev_trap

      subroutine splt_dev_loc_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%dev_loc(1:4).eq.'HP50')then
        call pc_put_sensitive_field_flag('OPT_DEV',.true.)
        call pc_put_sensitive_field_flag('QUALITY',.true.)
      else
        call pc_put_sensitive_field_flag('OPT_DEV',.false.)
        call pc_put_sensitive_field_flag('QUALITY',.false.)
        object%opt_dev='PAPER'
        object%quality='PROD'
      endif
      return
      end subroutine splt_dev_loc_trap

      subroutine splt_scale_trap (keyword)
      implicit none
       character(len=*),intent(in) :: keyword
       real    :: fval
       integer :: istat
       character(len=80) :: card
       call string_to_upper(object%scale)
       if(object%scale.eq.'NO'.or.object%scale.eq.'YES')return
       call string_cc2ff(object%scale,fval,istat,card)
       if(istat.eq.1)then
         if(fval.le.0.0)then
           call pc_error('SCALE value must be greater than zero')
         endif
       else
         if(object%scale.ne.'YES'.and.object%scale.ne.'NO')then
           call pc_error('SCALE must be YES, NO, or a number')
         endif
       endif
       return
      end subroutine splt_scale_trap

      subroutine splt_vint_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%vint.eq.'OR&I')object%vint='ORI'
      if (object%vint /= 'TINT5') then
        if (object%vint /= 'TINT7') then
          if (object%vint /= 'TINT15') then
            if (object%vint /= 'ORI') then
              if (object%vint /= 'VELO') then
                if (object%vint /= 'NONE') then
                  call pc_error('Valid answers for VINT are TINT5,TINT7,&
   &                             TINT15,ORI,VELO and NONE')
                endif
              endif
            endif
          endif
        endif
      endif
      return
      end subroutine splt_vint_trap
      subroutine splt_meth_cct_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword


      if(object%meth_cct.eq.'NONE'.or.object%meth_cct.eq.'MED'.or.&
         object%meth_cct.eq.'NOMED')return
      call pc_error('METH_CCT must be NONE, MED, or NOMED')

!           CT
      if(object%meth_cct.eq.'NONE')then
        call pc_put_sensitive_field_flag('CCTF',.false.)
      else
        call pc_put_sensitive_field_flag('CCTF',.true.)
      endif
      return
      end subroutine splt_meth_cct_trap

      subroutine splt_english
      if(survey_units.eq.'METERS')then
        object%ips_eng=object%ips*.3937008
        object%tpi_eng=object%tpi*cti
        object%cs_lab_eng=object%cs_lab/cti
        object%cs_sid_eng=object%cs_sid/cti
        object%wid_vl_eng=object%wid_vl/cti
        object%ar_ht_eng=object%ar_ht/cti
        object%ar_len_eng=object%ar_len/cti
        object%plcs_eng=object%plcs/cti
        object%ovwd_eng=object%ovwd/cti
        object%blcs_eng=object%blcs/cti
        object%tics_eng=object%tics/cti
        object%sdas_eng=object%sdas/cti
        object%tida_eng=object%tida/cti
        object%arda_eng=object%arda/cti
        object%scda_eng=object%scda/cti
        object%seda_eng=object%seda/cti
        object%tlcs_eng=object%tlcs/cti

      else

        object%ips_eng=object%ips
        object%tpi_eng=object%tpi
        object%cs_lab_eng=object%cs_lab
        object%cs_sid_eng=object%cs_sid
        object%wid_vl_eng=object%wid_vl
        object%ar_ht_eng=object%ar_ht
        object%ar_len_eng=object%ar_len
        object%plcs_eng=object%plcs
        object%ovwd_eng=object%ovwd
        object%blcs_eng=object%blcs
        object%tics_eng=object%tics
        object%sdas_eng=object%sdas
        object%tida_eng=object%tida
        object%arda_eng=object%arda
        object%scda_eng=object%scda
        object%seda_eng=object%seda
      endif
      object%tlcs_eng=object%tlcs/cti
      end subroutine splt_english

      subroutine splt_cald_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(.not.(object%cald=='YES'.or.object%cald=='NO'.or.object%cald=='YREV'))&
         then
        call pc_error('CALD must be YES, NO, or YREV')
      endif
      if(object%cald.eq.'NO')then
        call pc_put_sensitive_field_flag('SDAS',.true.)
        call pc_put_sensitive_field_flag('SCDA',.true.)
        call pc_put_sensitive_field_flag('TIDA',.true.)
        call pc_put_sensitive_field_flag('ARDA',.true.)
        call pc_put_sensitive_field_flag('SEDA',.true.)
      endif
      if(object%cald.eq.'YES'.or.object%cald.eq.'YREV')then
        call pc_put_sensitive_field_flag('SDAS',.false.)
        call pc_put_sensitive_field_flag('SCDA',.false.)
        call pc_put_sensitive_field_flag('TIDA',.false.)
        call pc_put_sensitive_field_flag('ARDA',.false.)
        call pc_put_sensitive_field_flag('SEDA',.false.)
      endif
      return
      end subroutine splt_cald_trap

      subroutine splt_lrrl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%lrrl)
      if(object%lrrl.ne.'LR'.and.object%lrrl.ne.'RL')then
        call pc_error('LRRL must be LR or RL')
      endif
      return
      end subroutine splt_lrrl_trap

      subroutine splt_norm_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_norm_trap

      subroutine splt_rp_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%rp(1:1)=='Y' .or. object%rp(1:1)=='y') then
        object%rp = 'YES'
      else
        object%rp = 'NO'
      endif
      return
      end subroutine splt_rp_trap

      subroutine splt_va_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_va_trap
      subroutine splt_vai_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_vai_trap
      subroutine splt_opt_vl_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_opt_vl_trap
      subroutine splt_ltab_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments
       return
       end subroutine splt_ltab_trap

      subroutine splt_ar_beg_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%ar_beg)
      call splt_arrows_sensitive

      return
      end subroutine splt_ar_beg_trap
      subroutine splt_arrows_sensitive
!           ARROWS
      if(object%ar_beg.eq.' '.and.object%ar_end.eq.' ')then
        call pc_put_sensitive_field_flag('AR_HT',.false.)
        call pc_put_sensitive_field_flag('AR_LEN',.false.)
        call pc_put_sensitive_field_flag('ARDA',.false.)
      else
        call pc_put_sensitive_field_flag('AR_HT',.true.)
        call pc_put_sensitive_field_flag('AR_LEN',.true.)
        call pc_put_sensitive_field_flag('ARDA',.true.)
      endif
      return
      end subroutine splt_arrows_sensitive

      subroutine splt_init_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_init_trap

      subroutine splt_ar_end_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call string_to_upper(object%ar_end)
      call splt_arrows_sensitive
      return
      end subroutine splt_ar_end_trap

      subroutine splt_fold_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine splt_fold_trap


      subroutine splt_nvrt_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%nvrt(1:1)=='Y' .or. object%nvrt(1:1)=='y') then
        object%nvrt = 'YES'
      else
        object%nvrt = 'NO'
      endif
      return
      end subroutine splt_nvrt_trap

      subroutine splt_rspp_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_rspp_trap

       subroutine splt_tbot_trap (keyword)
       implicit none
       character(len=*),intent(in) :: keyword
       if (object%tbot(1:1)=='Y' .or. object%tbot(1:1)=='y') then
         object%tbot = 'YES'
       else
         object%tbot = 'NO'
       endif
       return
       end subroutine splt_tbot_trap

      subroutine splt_pmse_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%pmse.eq.'YES')then
        call pc_put_sensitive_array_flag('PMSE100', .true.)
      else
        call pc_put_sensitive_array_flag('PMSE100', .false.)
      endif
      return
      end subroutine splt_pmse_trap

      subroutine splt_pmse100_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if (object%pmse100>=1.0 .and. object%pmse100<=1000.0) return
      call pc_error('PMSE100 must be between 1.0 and 1000.0.')
      return
      end subroutine splt_pmse100_trap

      subroutine splt_rezt_trap (keyword)
      implicit none
       character(len=*),intent(in) :: keyword

       return
       end subroutine splt_rezt_trap

      subroutine splt_rtbp_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_rtbp_trap
      subroutine splt_alch_trap (keyword)              ! scalar trap.
      implicit none
       character(len=*),intent(in) :: keyword           ! arguments

       return
       end subroutine splt_alch_trap

      subroutine splt_totl_element_trap (keyword,indx,action)

      implicit none
      character(len=*),intent(in) :: keyword           ! arguments
      integer         ,intent(in) :: indx              ! arguments
      integer         ,intent(in) :: action            ! arguments
      real    :: ftmp
      character(len=80) :: card
       ! INDX is a Fortran-style index (=1 for the first array element).
       ! ACTION is PC_INSERT or PC_REMOVE or PC_MODIFY.
       ! INDX refers to the array element inserted or removed or modified.

!          calculate the number of traces this card covers
        ftmp=object%intv(indx)*(object%totl(indx)-1)+object%trce(indx)
        if(ftmp.gt.object%num_tr)then
          call pc_error('Range of labels exceeds total number of traces')
          ftmp=(object%num_tr-object%trce(indx))/object%intv(indx)
          write(card,*)' Maximum totl = ',nint(ftmp)
          call pc_error(card)
        endif
      return
      end subroutine splt_totl_element_trap
      subroutine splt_plbl_element_trap (keyword,indx,action)

      implicit none
      character(len=*),intent(in) :: keyword           ! arguments
      integer         ,intent(in) :: indx              ! arguments
      integer         ,intent(in) :: action            ! arguments


       ! INDX is a Fortran-style index (=1 for the first array element).
       ! ACTION is PC_INSERT or PC_REMOVE or PC_MODIFY.
       ! INDX refers to the array element inserted or removed or modified.

      call string_to_upper(object%plbl(indx))

      return
      end subroutine splt_plbl_element_trap

      subroutine splt_trce_arrayset_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      integer :: i
      real    :: ftmp
      character(len=80) :: card
!          check range of shot point cards
      do i=1,object%nshots
        if (object%trce(i)<=0 .or. object%trce(i)>object%num_tr) then
          call pc_error('Trace number is out of range')
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
          call pc_error ('The number of intervals is out of range')
        endif
        if (object%totl(i) <= 0) then
          call pc_error('TOTL must be greater than zero')
        endif
      enddo
      call splt_shot_sensitive
      return
      end subroutine splt_trce_arrayset_trap

      subroutine splt_shot_sensitive
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

      end subroutine splt_shot_sensitive

      subroutine splt_trtic_arrayset_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword

      integer :: i
      real    :: ftmp
      do i=1,object%ntrtic
        if (object%trtic(i)<=0 .or. object%trtic(i)>object%num_tr) then
          call pc_error('Trace number is out of range')
        endif
        if (object%intic(i)<=0 .or. object%intic(i)>object%num_tr) then
          call pc_error('The number of intervals is out of range')
        endif
        if (object%ittic(i) <= 0) then
          call pc_error('ITTIC must be greater than zero')
        endif
!          calculate the number of traces this card covers
        ftmp = object%intic(i)*(object%ittic(i)-1) + object%trtic(i)
        if (ftmp > object%num_tr) then
          call pc_error ('Range of tics exceeds total number of traces')
        endif
      enddo
      return
      end subroutine splt_trtic_arrayset_trap

      subroutine splt_tldots_trap (keyword)            ! array trap.
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments
      return
      end subroutine splt_tldots_trap

      subroutine splt_hdr_lab_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      character(len=80) :: card
      integer :: i
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
        call pc_put_sensitive_field_flag('HDR_CHG',.false.)
        call pc_put_sensitive_arrayset_flag('TRCE_ARRAYSET',.true.)
      else
        call pc_put_sensitive_field_flag('LAB_INIT',.true.)
        call pc_put_sensitive_field_flag('LAB_INC',.true.)
        call pc_put_sensitive_field_flag('NAM_LAB',.true.)
        call pc_put_sensitive_field_flag('CS_LAB',.true.)
        call pc_put_sensitive_field_flag('HDR_CHG',.true.)
        call pc_put_sensitive_arrayset_flag('TRCE_ARRAYSET',.false.)
        object%trce=0
        object%shot=' '
        object%intv=0
        object%incr=0
        object%totl=0
      endif
      call splt_shot_sensitive

      return
      end subroutine splt_hdr_lab_trap

      subroutine splt_ttotl_trap (keyword)            ! array trap.
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments
      return
      end subroutine splt_ttotl_trap


      subroutine splt_titr_arrayset_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      integer :: i
      do i=1,object%ntie
        call string_to_upper(object%tie(i))
        if (object%titr(i)<=0 .or. object%titr(i)>object%num_tr) then
          call pc_error('Trace number is out of range')
        endif
      enddo
      call splt_tie_sensitive
      return
      end subroutine splt_titr_arrayset_trap

      subroutine splt_tie_sensitive
      if(object%ntie.gt.0 .or. object%path_tie1.ne.PATHCHECK_EMPTY)then
        call pc_put_sensitive_field_flag('TICS',.true.)
        call pc_put_sensitive_field_flag('TBOT',.true.)
        if(object%cald.eq.'NO')call pc_put_sensitive_field_flag('TIDA',.true.)
      else
        call pc_put_sensitive_field_flag('TICS',.false.)
        call pc_put_sensitive_field_flag('TIDA',.false.)
        call pc_put_sensitive_field_flag('TBOT',.false.)
        object%tbot='NO'
      endif
      end subroutine splt_tie_sensitive

      subroutine splt_tistr_trap (keyword)            ! array trap.
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments
      return
      end subroutine splt_tistr_trap
      subroutine splt_stlbl_trap (keyword)            ! array trap.
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments
      return
      end subroutine splt_stlbl_trap
      subroutine splt_tiinc_trap (keyword)            ! array trap.
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments
      return
      end subroutine splt_tiinc_trap
      subroutine splt_lbinc_trap (keyword)            ! array trap.
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments
      return
      end subroutine splt_lbinc_trap

      subroutine splt_szlbl_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      call splt_english
      return
      end subroutine splt_szlbl_trap

      subroutine splt_sltim_arrayset_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword

      real :: gtime

      integer :: nline

      character(len=80) :: card

      call splt_english
      gtime = object%ndpt*object%dt + object%tstrt
      do nline=1,object%nsltim
        call string_to_upper(object%slbl(nline))
        if (object%sltim(nline) > gtime) then
          call pc_error('SLTIM may not exceed the time you specified on the &
                        &initial job screen. SLTIM reset to global time')
          object%sltim(nline) = gtime
        endif
        if (object%sltim(nline) < object%tstrt) then
          call pc_error('SLTIM may not be less that the TSTRT global &
                        &SLTIM reset to TSTRT')
          object%sltim(nline) = object%tstrt
        endif
        if (object%slsz(nline) < 0.0) then
          call pc_error('SLSZ must be greater than zero')
        endif
        if (object%slrow(nline) <= 0) object%slrow(nline) = 1
        if (object%slrow(nline) > object%nrow) then
          write (card, *) 'There are only ', object%nrow, ' rows of panels'
          call pc_error(card)
        endif
      enddo
      return
      end subroutine splt_sltim_arrayset_trap

      subroutine splt_end_trap                         ! end trap.
      implicit none
      real :: hite,fitmax,a,f,fmaxin,fit,ftida,farda,dist,fscda,fsdas,fseda

      integer :: i35flg,maxit,i,j,n,ispprob,nc,nl

      character(len=160) :: lcard
      character(len=80)  :: card


      if(object%cald.eq.'NO')then
        if(object%sdas.le.0.0)then
          call pc_error('You must set SDAS')
          call pc_jump_field('SDAS')
        endif
        if(object%scda.le.0.0.and.object%scale.ne.'NO')then
          call pc_error('You must set SCDA')
          call pc_jump_field('SCDA')
        endif
        if(object%tida.le.0.0.and.(object%ntie.gt.0.or.object%path_tie1.ne.&
           PATHCHECK_EMPTY))then
          call pc_error('You must set TIDA')
          call pc_jump_field('TIDA =')
        endif
        if(object%arda_eng.le.0.0.and.(object%ar_beg.ne.' '.or.&
           object%ar_end.ne.' '))then
          call pc_error('You must set ARDA')
          call pc_jump_field('ARDA =')
        endif
      endif

!                PLOT HEIGHT
      fmaxin=40.0
      i35flg=0
      if(object%dev_loc.eq.'HP')then
        fmaxin=35.0
        i35flg=1
      endif
      hite=object%time*object%ips_eng
      fitmax=fmaxin-hite
      if(object%tbot.eq.'YES'.or.object%ltab.eq.'YES')then
        fitmax=fitmax-0.5-object%cs_lab_eng*1.5
      else if(object%tbot.eq.'YES')then
        fitmax=fitmax-0.28
      endif
!          truncate fitmax to 2 decimal places
      maxit=fitmax*100.0
      fitmax=real(maxit)/100.0
      if(hite.gt.fmaxin)then        !chere
        write(lcard,*)'Total plot height exceeds ',fmaxin,' inches - &
     & Change TIME or IPS'
        call pc_error(lcard)
      endif
      if(fitmax.lt.4.0)then
        if(i35flg.eq.1)then
            call pc_warning('WARNING - HP 650 plotter is only &
     &35.0 inches wide')
        endif
        write(card,*)'WARNING - There are only ',fitmax,' inches availa&
     &ble for annotation'
        call pc_warning(card)
      endif
     if (object%lab_init == 'SHOT') then
        do i=1,object%nshots
          f = mod(object%trce(i),1.0)
          if (f /= 0.0) then
            call pc_error('When using LAB_INIT=SHOT you cannot label between &
     &                     traces')

          endif
        enddo
      endif
!
!          figure plot height if paneling
      if(object%ntpp.gt.0)then
        hite=(object%time+.1)*object%nrow*object%ips_eng
        fitmax=fmaxin-hite
        if(fitmax.lt.4.0)then
          write(card,*)'WARNING - There are only ',fitmax,' inches avai&
     &lable for annotation'
          call pc_warning(card)
        endif
        if(hite.gt.fmaxin)then
          write(lcard,*)'Total plot height exceeds ',fmaxin,' inches - &
                       & Change TIME, IPS, or NROW'
          call pc_error(lcard)
        endif
      endif

!          calculate distance above section parameters
      ispprob=0
      fit=0.0
      if(object%cald.eq.'YES'.or.object%cald.eq.'YREV')then
         fscda=0.0
         fsdas=0.0
         ftida=0.0
         farda=0.0
         fseda=0.0
         object%scda=0.0
         object%sdas=0.0
         object%fintop=0.0
         ftida=.55
!               shot points
         if(object%nshots.gt.0.or.object%ntrtic.gt.0.or.object%hdr_lab(1).gt.0&
            .or.object%hdr_lab(2).gt.0)then
           fit=object%cs_lab_eng*2.0+object%cs_lab_eng
           if(fit.gt.fitmax)then
             call pc_error('There is not enough room to plot shot points.')
             call pc_error('Change TIME or IPS')
             call pc_jump_field('TIME =')
           endif
         endif
!               hdr_lab(2)
         if(object%hdr_lab(2).gt.0)then
           fit=fit+.1+object%cs_lab_eng
           if(fit.gt.fitmax)then
             call pc_error('There is not enough room available to plo&
     &t hdr_lab(2).  Change TIME or IPS')
             call pc_jump_field('TIME')
           endif
         endif
!               hdr_lab(3)
         if(object%hdr_lab(3).gt.0)then
           fit=fit+.1+object%cs_lab_eng
           if(fit.gt.fitmax)then
             call pc_error('There is not enough room available to plo&
     &t hdr_lab(3). Change TIME or IS')
             call pc_jump_field('TIME')
           endif
         endif
!               hdr_lab(4)
         if(object%hdr_lab(4).gt.0)then
           fit=fit+.1+object%cs_lab_eng
           if(fit.gt.fitmax)then
             call pc_error('There is not enough room available to plo&
     &t hdr_lab(4). Change TIME or IS')
             call pc_jump_field('TIME')
           endif
         endif
!               tie lines
         if(object%path_tie1.ne.PATHCHECK_EMPTY.or.object%ntie.gt.0)then
           if(object%hdr_lab(2).ne.0)ftida=.835
           if(object%hdr_lab(3).ne.0)&
             ftida=ftida+.1+object%cs_lab_eng
           if(object%hdr_lab(4).ne.0)&
             ftida=ftida+.1+object%cs_lab_eng
!             if doing a dashed elevation plot set tida to 1.0
           if(object%dlu100.gt.0.0.and.ftida.lt.1.0)&
              ftida=1.0
           nc=40
           if(object%ntie.gt.0)then
             nc=0
!                find longest tie line
             do i=1,object%ntie
               j=len_trim(object%tie(i))
               if(j.gt.nc)nc=j
             enddo
           endif
           a=nc*object%tics_eng*.342021+ftida+object%tics_eng*1.5
           if(object%path_tie2.ne.PATHCHECK_EMPTY)a=a+1.5
           if(a.gt.fit)then
             fit=a
           endif
           if(fit.gt.fitmax)fit=fitmax
           object%tida_eng=ftida
         endif
!               pmse  information
         if(object%pmse.eq.'YES')then
           fseda=fit+1.5
           fit=fseda
           if(fit.gt.fitmax)then
             fit=fitmax
             ispprob=1
           endif
!                  Estimate 1 inch for elevation plot
           fit=fit+1.0
           object%seda_eng=fseda
         endif
!               arrows
         if(object%ar_beg.ne.' '.or.object%ar_end.ne.' ')then
           farda=fit+.2
           if(farda.lt.0.8)farda=0.8
!              add on height of label
           a=object%ar_ht_eng
           dist=farda+.1+a+a/2.5
           fit=dist
           if(fit.gt.fitmax)then
             farda=fitmax-(.1+a+a/2.5)
             fit=fitmax
             ispprob=1
           endif
           if((farda+object%ar_ht).gt.fitmax)then
              farda=fitmax-(.1+object%ar_ht)
           endif
           object%arda_eng=farda
         endif
!               scale
         if(object%scale.ne.'NO')then
           fscda=fit+0.5
           fit=fscda+object%cs_scale
           if(fit.gt.fitmax)then
             fscda=fitmax-2.0*object%cs_scale
             fit=fitmax
             ispprob=1
           endif
           object%scda_eng=fscda
         endif
!               velocity functions
        if(object%path_vel.ne.PATHCHECK_EMPTY)then
          if(fscda.le.0.0)fscda=fit+0.5
          fit=fscda + object%cs_scale + 2.9*object%frvf
          if(fit.gt.fitmax)then
            fscda=fitmax-2*object%cs_scale-3.0
            fit=fitmax
            ispprob=1
          endif
          object%scda_eng=fscda
        endif
!               sid
         if(object%sid(1)(1:10).ne.' '.or.object%sid(2)(1:10).ne.' '&
           .or.object%sid(3)(1:10).ne.' '.or.object%sid(4)(1:10).ne.' ')then
!          determine number of lines including blanks needed for sids
         nl=2
         if(object%sid(2)(1:10).ne.' '.and.object%sid(3)(1:10).eq.' '&
            .and.object%sid(4)(1:10).eq.' ')nl=3
         if(object%sid(3)(1:10).ne.' '.and.object%sid(4)(1:10).eq.' ')nl=4
         if(object%sid(4)(1:10).ne.' ')nl=5
!
!
           fit=fit+nl*object%cs_sid_eng+4*.15
           if(fit.lt.5.0.and.fitmax.ge.5.0)fit=5.0
           if(fit.gt.fitmax)then
             fit=fitmax
             ispprob=1
           endif
           fsdas=fit-object%cs_sid_eng
           object%sdas_eng=fsdas
         endif
         if(survey_units.eq.'METERS')then
           object%sdas=fsdas*2.54
           object%scda=fscda*2.54
           object%tida=ftida*2.54
           object%arda=farda*2.54
           object%seda=fseda*2.54
           write(print_lun,*)' Units are in centimeters'
         else
           object%sdas=fsdas
           object%scda=fscda
           object%tida=ftida
           object%arda=farda
           object%seda=fseda
           write(print_lun,*)' Units are in inches'
         endif
!
         object%fintop=fit
         write(print_lun,*)'SPLT-->Calculated SDAS = ',object%sdas
         write(print_lun,*)'SPLT-->Calculated SCDA = ',object%scda
         write(print_lun,*)'SPLT-->Calculated ARDA = ',object%arda
         write(print_lun,*)'SPLT-->Calculated SEDA = ',object%seda
         write(print_lun,*)'SPLT-->Calculated TIDA = ',object%tida
         write(print_lun,*)'SPLT-->Inches at the top = ',fit
      endif   ! cald.eq.yes

!        Manual shot point labels
      if (object%rspp=='YES' .and. object%ntpp>0) then
        do i=1,object%nshots
          if (object%trce(i) > object%ntpp) then
            call pc_error ('If paneling and RSPP=YES, trace number must be &
      &                     less than NTPP')
          endif
        enddo
      endif

!        Intermediate tic marks

      if (object%lab_init == 'SHOT') then
        f = mod(object%trtic(i),1.0)
        if (f /= 0.0) then
          call pc_error('When using STRT=SHOT you cannot plot tics between &
     &                   traces')

        endif
      endif

!        Panel Labels
!!      if(object%nrow.eq.1)object%ntpp=0
      if(object%ntpp.gt.0)then
        object%bthc=0
        f = (object%time + 0.1)*object%ips_eng
        if (object%nrow <= 0) object%nrow = 34.5/f
!          if conp - check for 4096 points - dplt 4080
        n = object%time/object%dt
        if (object%ntpp > 0)then
           n = (object%time + 0.1)*object%nrow/object%dt - 0.1/object%dt
        endif
!             This needs to be tested for new HPs
        if (n > 4080) then
          call pc_error('Maximum number of points you may plot is 4080')
        endif
        if (survey_units == 'METERS') then
          if (object%plcs > 2.54)then
            call pc_warning('WARNING - PLCS exceeds 2.54 centimeters')
          endif
        else
          if (object%plcs > 1.0)then
             call pc_warning('WARNING - PLCS exceeds 1 inch')
          endif
        endif
      endif


!            LAB_INC
      if (object%lab_inc<=0 .or. object%lab_inc>object%num_tr) then
        if (object%num_tr <= 0) then
          call pc_error ('You must answer NUM_TR')
        else
          call pc_error('LAB_INC is out of range')
        endif
      endif

      if(object%hdr_lab(1).ne.0 .or. object%hdr_lab(2).ne.0)then
        if((object%lab_init.eq.'0'.or.object%lab_inc.eq.0).and.&
             (object%lab_init.ne.&
            'SHOT'.and.object%lab_init.ne.'HDR'))then
          call pc_error('You must answer LAB_INIT and LAB_INC')
        endif
      endif

      if(object%lab_init.eq.'SHOT'.and.object%nshots.eq.0)then
        call pc_error('You must build shot points if lab_init=SHOT')
      endif
      if((object%lab_init.eq.'0'.or.object%lab_inc.eq.0).and.&
          (object%lab_init.ne.&
          'SHOT'.and.object%lab_init.ne.'HDR'))then
        call pc_error('You must answer LAB_INIT and LAB_INC')
      endif

!          Elevation plot  - pmse
      if(object%pmse.eq.'YES'.and.object%ntpp.gt.0)then
        call pc_error('PMSE is not allowed if paneling')
        call pc_jump_field('PMSE')
      endif





      return
      end subroutine splt_end_trap


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine splt (obj,ntr,header,trace)
      implicit none
      type(splt_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: header(:,:)            ! arguments
      real             ,intent(inout) :: trace(:,:)             ! arguments
!
      real      :: work(obj%nsamp)
      integer   :: ibuf(obj%nsamp)
      integer   :: i,j,k,l,m  ,iiii,mask1,mask2,numa,numb,length,iatb 
      integer   :: itypplt,irec,idone,need                             ,ndx  
      integer   :: idir,incdec,iscale,nc             ,istat,nrtmp  
      integer   :: ibtacum,iatbf,kflg,ibt,nsamct,ns,nspan=0,nsbtwn,iend,npwrds
      integer   :: iatbf1,jntplot,num
      real      :: trapxu,spyu,rrvs,olav,ampsc,yn,vaend=1.0,trlab,oldt,gap
      real      :: xs,xe,delx,flen,tmp,y0,cwrh,fnc,xtrce,skp2,tht,reduce
      real      :: trmax,scl,factor,fvelhd2,ym,fmax,secend,blline,sec,yon
      real      :: a  ,f,x,y,x1,x2,xtemp,ytemp,yy,yyy,yyyy,x3,ylst,ht2,ydif 
      real      :: filhedr,frsthedr,dum,trmaxs(26),facnew,fac,ftmp,yinc,xinc

      integer,allocatable :: jbuf(:)

      integer,save :: lun_ccname,match


      double precision :: thedr

      logical   :: there,almost_equal,found_error
      character(len=1)  :: iplus='+',letter
      character(len=4)  :: ireps
      character(len=8)  :: iccname='%DPLTCC',ctmp,ctmp2,itemp,knum,ktmp
      character(len=16) :: uname,subl
      character(len=24) :: com2
      character(len=40) :: com1,hcard
      character(len=80) :: card

      data mask1/Z'000000FF'/, mask2/Z'FFFFFFF0'/


!
! ---------------------- NO MORE TRACES HERE ------------------------------
!

      IF (ntr == NO_MORE_TRACES)THEN

!          if didn'T GET LAST BLOCK LABEL PLOTTED - DO IT NOW
      if(obj%needblk.lt.2.and.obj%path_blk.ne.PATHCHECK_EMPTY)then
          if((obj%iblinc.eq.1.and.obj%blhd2.gt.obj%blhead).or. &
     &       (obj%iblinc.eq.0.and.obj%blhd2.lt.obj%blhead))then
            obj%ftr2=obj%ntplot+1
            match=1
            call splt_blid(obj%xo,obj%yo,obj%ftr1,obj%ftr2,obj%bltxt,obj%blcs,&
                           obj%tunit,             &
     &                     obj%ntplot,obj%theta,obj%nbts,obj%btskp,obj%btdo,&
                           obj%btbtwn,       &
     &                     obj%bttot,obj%ian)
            nc=len_trim(obj%bltxt)
            write(print_lun,*)' SPLT - BLOCK LABEL ',obj%bltxt(1:nc),' PLOTTED &
                              &BETWEEN traces ',obj%FTR1,' and ',OBJ%FTR2
          endif
      endif

      if(obj%ntpp.eq.0)then
        endfile obj%itn
        rewind obj%itn
      else
        if(obj%ncol.eq.1.and.obj%ktpanl.lt.obj%ntpp)then
          obj%ktpanl=obj%ntpp
          obj%ntplot=obj%ktpanl
        endif
        obj%ipanname=obj%ipansav
!          now that we know the true number of traces - recalculate
!           nrlc (number of rows in last column) and blank trace
!           pattern unless user defined
        j=obj%ntpp*obj%nrow
        k=mod(obj%ktrnum,j)
        obj%ncol=obj%ktrnum/j+.0000001
        obj%nrlc=0
        if(k.gt.0)then
          m=obj%ktrnum-(obj%ncol*j)
          obj%nrlc=m/obj%ntpp
          obj%ncol=obj%ncol+1
        endif
        write(print_lun,*)' NUMBER OF INPUT TRACES PLOTTED = ',obj%ktrnum
        write(print_lun,*)' NUMBER OF PANELED TRACES = ',obj%ncol*obj%ntpp
        write(print_lun,*)' THERE ARE ',obj%ncol,' COLUMNS OF PANELS'
        if(obj%nrlc.gt.0)write(print_lun,*)' LAST COLUMN HAS ',obj%nrlc,' ROWS '
        if(obj%bttot(1).gt.(obj%ncol-1).and.obj%iuserbt.eq.0)then
          obj%bttot(1)=obj%ncol-1
          write(print_lun,*)' BTTOT RESET TO ',obj%bttot(1),' BASED ON ',&
                            &obj%ncol,' COLUMNS'
          obj%numbt=0
          do 5045 i=1,obj%nbts
            k=obj%btdo(i)*obj%bttot(i)
            obj%numbt=obj%numbt+k
 5045     continue
        endif

           do i=1,obj%nrow
             inquire(file=obj%ipanname,number=j)
             rewind j
             letter=obj%ipanname(7:7)
             k=ichar(letter)
             k=k+1
             letter=char(k)
             obj%ipanname(7:7)=letter
           enddo

      endif
      if(obj%bthc.ne.0)then
        rewind obj%btf
        obj%numbt=obj%btadd
      endif

!          if paneling - make trace file from the temporary files
!           add .1 sec between each row of panels
      nspan=0
      nsbtwn=0
      if(obj%ntpp.gt.0)then
!          number of samples to plot
        nsbtwn=.1/obj%dt+.0000001
        nspan=obj%nsamp*obj%nrow+nsbtwn*(obj%nrow-1)
        allocate (jbuf(nspan),stat=istat)
        if(istat.ne.0)then
           call pc_error('SPLT-->Unable to allocate array for paneling')
           ntr=FATAL_ERROR
        endif
        obj%npack=pkutil_npack8(nspan)
!
!
        obj%ipanname=obj%ipansav
        inquire(file=obj%ipanname,number=obj%ipan)
        f=0.0
        ndx=1
        nrtmp=obj%nrow
        iend=0
        ibuf=0
        jbuf=0
 5080   do 5100 l=1,nrtmp
          read(obj%ipan,end=5095)npwrds
          read(obj%ipan,end=5095)trmax,(ibuf(j),j=1,npwrds)
          trmaxs(l)=trmax
          f=amax1(f,trmax)
          istat=wrdc_unpack(ibuf,obj%nsamp,1,4,swap)
          do m=1,obj%nsamp
            jbuf(ndx)=ibuf(m)
            ndx=ndx+1
          enddo
          ndx=ndx+nsbtwn
          letter=obj%ipanname(7:7)
          k=ichar(letter)
          k=k+1
          letter=char(k)
          obj%ipanname(7:7)=letter
          inquire(file=obj%ipanname,number=obj%ipan)
          go to 5100
 5095   continue
!             end of file encountered - but may not be finished yet
          if(obj%nrlc.eq.0)go to 5125
          if(iend.eq.1)go to 5125
          iend=1
          nrtmp=obj%nrlc
          go to 5115
 5100   continue
!
 5102   if(obj%norm.eq.'ALL'.and.f.ne.0.0)then
!               rescale as one trace
          ndx=1
          facnew=127.0/f
          do 5110 l=1,nrtmp
            fac=trmaxs(l)/127.0
            do 5105 m=1,obj%nsamp
              if(jbuf(ndx).eq.255)then
                jbuf(ndx)=-1
              else if(jbuf(ndx).gt.127)then
                jbuf(ndx) = ior(jbuf(ndx),mask1)
              endif
              ftmp=jbuf(ndx)*fac
              jbuf(ndx)=nint(ftmp*facnew)
              jbuf(ndx)=iand(jbuf(ndx),mask1)
              ndx=ndx+1
 5105       continue
            ndx=ndx+nsbtwn
 5110     continue
        endif
        istat=wrdc_pack(jbuf,nspan,4,1,swap)
        write(obj%itn)obj%npack
        write(obj%itn)f,(jbuf(j),j=1,obj%npack)
        obj%ipanname=obj%ipansav
        inquire(file=obj%ipanname,number=obj%ipan)
        ndx=1
        f=0.0
        jbuf=0
        go to 5080
 5115   continue
!            last column of panels is not full - nrlc rows in last col
        ndx=obj%nrlc*(obj%nsamp+nsbtwn)+1
        jbuf(ndx:nspan-ndx)=0
        go to 5102
 5125   continue
        endfile obj%itn
        rewind obj%itn
        f=0.0
        obj%ipanname=obj%ipansav
        do j=1,obj%nrow
          inquire(file=obj%ipanname,number=obj%ipan)
          close(unit=obj%ipan,status='delete')
          letter=obj%ipanname(7:7)
          k=ichar(letter)
          k=k+1
          letter=char(k)
          obj%ipanname(7:7)=letter
        enddo
        deallocate(jbuf)
      endif


!          Calculate end of plot
      obj%xend=obj%xo+(obj%ntplot+obj%numbt)*obj%tunit

!          side labels
      if(obj%nsltim.gt.0)then
        if(obj%lrrl.eq.'LR')then
          x=obj%xo
        else
          x=obj%xend
        endif
        call splt_sidelabel(x,obj)
      endif  ! if(obj%nsltim.gt.0)
!

      if(obj%path_atbm.ne.PATHCHECK_EMPTY)then
!
!          if encounter string 'ATBM' generate title block with atb
        iatb=0
        k=index(obj%path_atbm,'ATBM')
        if(k.eq.0)then
!          try lower case
           k=index(obj%path_atbm,'atbm')
        endif
        if(k.ne.0)then
          obj%basint=abs(obj%bsmt2-obj%bsmt1)
          if(obj%scale.ne.'NO'.and.obj%scale.ne.'YES')then
             call string_cc2ff(obj%scale,obj%basint)
          endif
          call atblk(obj%path_atbm,obj%ipn,jobname,obj%basint,.true.,istat,&
                     print_lun)
          write(print_lun,*)'Status calling ATBLK = ',istat
          if(istat.ne.0)then
             call pc_warning('SPLT: Fatal error in ATBLK')
             call pc_warning('      No title block will be done')
             obj%path_atbm=PATHCHECK_EMPTY
             go to 5140
          endif
          call getlun(iatbf,istat)
          if(istat.ne.0)then
            call pc_error('Unable to get unit number for title block file')
          endif
          open(iatbf,status='OLD',file='atbf',iostat=istat)
          if(istat.ne.0)then
            call pc_error('SPLT: unable to open output file from ATBLK')
            call pc_error('      No title block will be done')
            obj%path_atbm=PATHCHECK_EMPTY
            go to 5140
          endif
!         call getlun here for the file created by atb
          iatb=1
        else ! Path is already ready to be plotted
          call getlun(iatbf,istat)
          if(istat.ne.0)then
            call pc_error('Unable to get unit number for title block file')
          endif
          open(iatbf,status='OLD',file=obj%path_atbm,iostat=istat)
          if(istat.ne.0)then
            call pc_error('Unable to open title block file')
          endif
        endif
!             plot the title block
        call splt_thit(tht,iatbf,ht2)
        write(print_lun,*)' SPLT - TITLE BLOCK HEIGHT = ',tht
         reduce=1.0
         ym=obj%time*obj%ips_eng+obj%fintoptitl
         if(ym.le.28.0.and.tht.gt.28.0)then
           reduce=27.5/tht
         else if(ym.le.35.0.and.tht.gt.35.0)then
           reduce=34.8/tht
         else if((tht+.05).gt.35.0)then
           reduce=35.0/(tht+.05)
         endif
         if(reduce.gt.obj%frtb)reduce=obj%frtb
         tht=tht*reduce
         if(obj%lrrl.eq.'LR')then
           x=obj%xsav+1.0*scal
           y=obj%yo/200.0+obj%sdas_eng
           if(y.gt.35.0.and.ym.le.35.0)y=35.0
           if(y.lt.tht)y=tht + .05
         else
           x=obj%xend+10.0*scal + obj%sladd
           y=obj%yo/200.0+obj%sdas_eng
           if(obj%hitemax-abs(y).lt.tht)y=(obj%hitemax-tht-.05)*(-1.0)
         endif
         y=y*scal
         xtemp=abs(x/200.0)
         if(obj%lrrl.eq.'RL')xtemp=xtemp+1.0
         ytemp=abs(y/200.0)
!             set to conplot font
         call cpsplot_fnt(obj%ian,0)
         call cpsplot_lwid(obj%ian,.005)
!  Try reg tblk routine to see what it does
         call splt_tblk(x,y,obj%sid(1),obj%sid(2),obj%sid(3),obj%sid(4), &
                        reduce,obj%theta,obj%dev,obj%pageflg,iatbf,ht2,&
                        obj%ian)
         call cpsplot_fnt(obj%ian,19)
         close(iatbf,status='DELETE')
      endif
!
 5140 continue

!         Old n=0 stuff here
        write(print_lun,*)' BEGIN           - SPLT END OF DATA'
        write(print_lun,*)' SPLT number = ',kntsplt
        write(print_lun,*)' ***  SPLT -- ALL TRACES HAVE BEEN READ'
        write(print_lun,*)' Number of traces plotted = ',obj%ktrnum
        if(obj%glav.eq.0.0)then
          write(print_lun,*)' ALL TRACES ARE DEAD '
          ntr = FATAL_ERROR
        endif
        write(print_lun,*)' XO OF SECTION = ',obj%xo/200.0,' XEND OF &
       &SECTION = &
       &',obj%xend/200.0
        fmax=obj%xend/200.0+3.0
!          call to section has info about plotting the trace
!
!
        if(obj%dev_loc.eq.'HP')obj%ymax=35.0
        tmp=200.0
        if(obj%lrrl.eq.'RL')then
          tmp=-200
          if(obj%path_atbm.ne.PATHCHECK_EMPTY)fmax=fmax+7.0
          idir=1
        else
          idir=0
        endif
!
!             Getting cut off in dplt - add 3 inches
        fmax=fmax+3.0
        if(obj%dev.eq.'NETP')then
          call cpsplot_xsetup(obj%ian,0,0.0,0.0,scal,tmp,obj%xmin,&
                              obj%ymin,fmax,obj%ymax,10,1)
          write(print_lun,*)' SPLT - Maximum X reset to ',fmax,&
     &                      ' Maximum Y reset to ',obj%ymax
        endif
!           itypplt is flag for info about variable area, wiggle trace
!              and pseudo intensity
        itypplt=0
        if(obj%wt.eq.1.and.obj%va.eq.'NO')itypplt=1
        if(obj%wt.eq.2.and.obj%va.eq.'NO')itypplt=2
        if(obj%wt.eq.0)itypplt=4
        if(obj%wt.eq.1.and.obj%va.eq.'YES')itypplt=5
        if(obj%wt.eq.2.and.obj%va.eq.'YES')itypplt=6
        if(obj%vai.eq.'YES')itypplt=itypplt+8
        if(itypplt.eq.0)then
          ntr=FATAL_ERROR
          return
        endif
        trapxu=obj%tpi_eng/200.0
        spyu=obj%ips_eng*200.0
        rrvs=0.0
        if(obj%rp.eq.'YES')rrvs=-1.0
        olav=0.0

!          calculate median obj%ct
        if(obj%meth_cct.ne.'NONE')THEN
!            calculate obj%ct
          nsamct= max(obj%nsamp,nspan)
!
          call splt_ct(obj%itn,obj%glav,obj%cctf,obj%ct,dum,   &
     &                 work,nsamct,obj%meth_cct)
        endif


        k=obj%ct/obj%tpi_eng/2.0*200.0 +.5000001
        ampsc=real(k)/obj%glav
        irec=obj%nsamp*5
        y0=obj%yo
        yn=-1.0*obj%ips_eng*obj%time*200.0 + obj%yo
        if(obj%dev.eq.'NETP')then
          call cpsplot_section(obj%ian,0,idir,itypplt,0,0,0,0,trapxu,&
     &                         spyu,rrvs,obj%dt,obj%glav,   &
     &                         ampsc,obj%nsamp,0.0,obj%time,y0,yn,obj%vastrt,&
     &                         vaend)
        else
          if(obj%ntpp.eq.0)then
            ns=obj%nsamp
          else
            ns=nspan
          endif
          call cpsplot_seis(obj%ian,obj%glav,ns,obj%lrrl,obj%yo/200.0,&
                            obj%yend/200.0,obj%ct,obj%tpi_eng,obj%va,obj%wt,&
                            obj%vastrt,vaend)
        endif
!          write out the trace vectors
        k=0
        idone=0
        need=1
        rewind obj%btf
        do 5300 i=1,obj%ntplot
          trlab=real(i+k)
          oldt=trlab
          if(obj%bthc.ne.0)then
            if (idone.eq.1) go to 5201
            if(need.eq.1)then
              read(obj%btf,end=5200)m
              need=0
            endif
            if(m.eq.i)then
              k=k+obj%bthc
              need=1
            endif
            go to 5201
 5200       idone=1
            rewind obj%btf
 5201       trlab=real(i+k)
          else
             if(obj%nbts.gt.0)then
               call splt_abt(oldt,trlab,obj%nbts,obj%btskp,obj%btdo,obj%btbtwn,&
      &                      obj%bttot)
             endif
          endif
          obj%xcort=trlab*obj%tunit+obj%xo
          call cpsplot_xtrace(obj%ian,i,obj%xcort,0.0,15)
 5300   continue

!          plot the section id
      ireps='NO'
      if(obj%sdas_eng.gt.obj%fintop)obj%sdas_eng=obj%fintop-obj%cs_sid_eng*0.5
      y=obj%yo+obj%sdas_eng*scal
      x=obj%xo
      if(obj%lrrl.eq.'RL')x=obj%xend
      call cpsplot_fnt(obj%ian,19)
       call splt_sid (obj%ian,x,y,obj%cs_sid_eng,obj%sid(1),obj%sid(2),&
     &               obj%sid(3),obj%sid(4),&
     &               obj%ntplot+obj%numbt,obj%tpi_eng,   &
     &               ireps,obj%theta,obj%ipf1,obj%yo)

!                                  plot shot point numbers
      j=0
      idone=0
      f=0.0
      need=1
      ibtacum=0
      do 5500 k = 1,obj%nshots
        if(obj%trce(k).eq.0.0)go to 5505
        call splt_sho (obj%xo,obj%yo,obj%yend,obj%trce(k),f,obj%shot(k),&
                       obj%intv(k),&
                       obj%incr(k),    &
                       obj%totl(k),obj%cs_lab_eng,obj%tunit,obj%opt_vl,&
                       obj%wid_vl_eng,obj%theta,&
                       obj%nbts,obj%btskp,obj%btdo,        &
                       obj%btbtwn,obj%bttot,istat,obj%ml10,obj%dy10 ,&
                       obj%tldots(1),obj%bthc,&
                       ibtacum,idone,obj%btf, &
                       need,obj%dev,obj%ltab,obj%rspp,obj%ntpp,obj%ncol,&
                       obj%ian)
        if(istat.ne.0)then
          ntr=FATAL_ERROR
        endif
 5500 continue
 5505 continue
!
!
!                          plot tics at intermediate shot point numbers
      j=0
      idone=0
      f=0.0
      need=1
      skp2=obj%skp/2.
      ibtacum=0
      do 5510 k = 1,obj%ntrtic
        if(obj%trtic(k).eq.0.0)go to 5515
        call splt_stic(obj%xo,obj%yo,obj%trtic(k),f,obj%intic(k),&
                       obj%ittic(k),  &
                       obj%tunit,skp2,obj%nbts,obj%btskp,obj%btdo,     &
                       obj%btbtwn,obj%bttot,obj%bthc,ibtacum,idone,obj%btf,&
                       need,obj%ian)
 5510 continue
 5515 continue

!          calculate basement interval for scale
      if(obj%scale.ne.'NO')then
        if(obj%scale.eq.'YES')then
          obj%basint=abs(obj%bsmt2-obj%bsmt1)
          if(obj%basint.le.0.0)then
            write(print_lun,*)' SPLT -  CALCULATED BASEMENT INTERVAL IS ',&
                                obj%basint
            write(print_lun,*)' SPLT -  NO SCALE WILL BE DONE'
            go to 5350
          endif
        else
          call string_cc2ff(obj%scale,obj%basint)
        endif
          call splt_scl(obj%xo,obj%yo,obj%scda_eng,obj%cs_scale_eng,&
                        obj%tunit,survey_units,obj%basint,obj%theta, &
                        obj%ntplot+obj%numbt,obj%ipf1,obj%ian)
      endif
 5350 continue
!
!                                  plot direction arrows
      x      =  0.0
      y      = 0.0
      if (obj%ar_ht_eng .eq. 0.0 ) obj%ar_ht_eng =  (obj%ips_eng * .035)
      x      =  x  +  obj%tunit + obj%xo
      y      =  obj%arda_eng * scal + obj%yo
      if(obj%ar_beg.ne.' ')then
!                                  plot arrow pointing to first trace in
        call splt_arow(x,y,obj%ar_ht_eng,obj%ar_len_eng,obj%ar_beg,0,obj%theta,&
     &                 obj%dev,obj%yo,obj%ipf1,obj%ian)
      endif
      if(obj%ar_end.ne.' ')then
        x=obj%tunit*(obj%ntplot+obj%numbt)-obj%tunit -(obj%ar_len_eng*scal)&
                     +obj%xo
!                        plot arrow pointing to last trace in
        call splt_arow (x,y,obj%ar_ht_eng,obj%ar_len_eng,obj%ar_end,1,&
                        obj%theta,&
     &                  obj%dev,obj%yo,obj%ipf1,obj%ian)
      endif
!          plot a legend explaining elevation plot and fold of stack
!           indicator
      if(obj%flst.gt.0.or.(obj%dlu100.gt.0.0.and.obj%hdr_dl.eq.19).or.&
        (obj%plu100.gt.0.0.and.obj%hdr_pl.eq.19))then
        if(obj%lrrl.eq.'LR')then
          x1=obj%xo-(3.0*obj%tlcs_eng+.3)*scal - 1.0*scal
        else
          x1=obj%xo-(3.0*obj%tlcs_eng+.3)*scal
        endif
        call cpsplot_fnt(obj%ian,0)
        call splt_eleg(x1,obj%yo,obj%dlu100,obj%plu100,obj%flst,&
                       obj%theta,obj%ndots,obj%dev,obj%ian)
        if(obj%lrrl.eq.'LR')then
          x2=obj%xend+(3.0*obj%tlcs_eng+.3)*scal
        else
          x2=obj%xend+(3.0*obj%tlcs_eng+.3)*scal + 1.2*scal
        endif
        call splt_eleg(x2,obj%yo,obj%dlu100,obj%plu100,obj%flst,&
                       obj%theta,obj%ndots,obj%dev,obj%ian)
        call cpsplot_fnt(obj%ian,19)
      endif
      k=obj%otim
      secend=obj%otim
      tmp=obj%otim
      if(obj%tstrt.ne.0.0)then
        tmp=tmp+obj%tstrt
        k=tmp
      endif
      secend=tmp
      y=obj%yo
      if(obj%tlst.ne.0)y=obj%yo-real(obj%tlst)/1000.0*scal*obj%ips_eng
!
!          plot the timing line numbers
        if(obj%ntistr.eq.0)then
          if(obj%us.eq.0)then
            call splt_tml (obj%xo,y,obj%yend,obj%tlcs_eng,obj%tstrt,k,&
     &                     obj%ips_eng,&
     &                     obj%whsec, &
     &                     obj%lrrl,1,obj%ysdel,obj%yedel,obj%ycdel,&
     &                     obj%yddel ,&
     &                     obj%nrow,obj%otim,&
     &                     obj%nam_lab(1),obj%nam_lab(2),obj%nam_lab(3),&
     &                     obj%nam_lab(4),&
     &                     obj%tunit,secend,obj%xbias,obj%dev,&
     &                     obj%twid,0.0,&
     &                     obj%cs_lab_eng,obj%ian)
!
            call splt_tml (obj%xend,y,obj%yend,obj%tlcs_eng,obj%tstrt,k,&
     &                     obj%ips_eng,&
                           obj%whsec,&
                           obj%lrrl,2,&
     &                     obj%ysdel,obj%yedel,obj%ycdel,obj%yddel ,obj%nrow,&
     &                     obj%otim,&
     &                     obj%nam_lab(1),obj%nam_lab(2),obj%nam_lab(3),&
     &                     obj%nam_lab(4),&
     &                     obj%tunit,secend,0.0,obj%dev,&
     &                     obj%twid,0.0,&
     &                     obj%cs_lab_eng,obj%ian)
             obj%nam_lab=' '
          else
            call splt_us(obj%xo,y,obj%tlcs_eng,obj%us,obj%ips_eng,obj%time,&
                         obj%tstrt,obj%nrow,obj%otim,obj%theta,'F',&
     &                   obj%nam_lab(1),obj%nam_lab(2),obj%dev,obj%xbias,&
     &                   obj%ysdel,obj%twid,obj%yedel,obj%nam_lab(3),    &
     &                   obj%nam_lab(4),obj%ycdel,obj%yddel,obj%ian )
            call splt_us(obj%xend,y,obj%tlcs_eng,obj%us,obj%ips_eng,obj%time,&
     &                   obj%tstrt,&
     &                   obj%nrow,obj%otim,        &
     &                   obj%theta,'L',obj%nam_lab(1),obj%nam_lab(2),&
     &                   obj%dev,0.0,obj%ysdel,obj%twid,obj%yedel, &
     &                   obj%nam_lab(3),obj%nam_lab(4),obj%ycdel,obj%yddel,&
     &                   obj%ian )
            obj%nam_lab=' '
          endif
        endif  !  (obj%ntistr.eq.0)

!          repeat timing line numbers in blank trace pattern
      if(obj%rtbp.eq.'YES')then
        if(obj%lrrl.eq.'LR')then
          if(obj%us.ne.0)then
            x=(real(obj%btskp(1)+obj%btdo(1)/2))/obj%tpi_eng-obj%tlcs_eng* 1.1
          else
            x=(real(obj%btskp(1)+obj%btdo(1)/2))/obj%tpi_eng-obj%tlcs_eng*.5
          endif
          xs=(real(obj%btskp(1)+obj%btdo(1)/2))/obj%tpi_eng
        else
          x=(real(obj%btskp(1)+obj%btdo(1)/2))/obj%tpi_eng+obj%tlcs_eng*1.5
          xs=(real(obj%btskp(1)+obj%btdo(1)/2))/obj%tpi_eng
          if(obj%us.eq.0)xs=xs+obj%tlcs_eng/2*1.5
        endif   ! (obj%lrrl.eq.lr)
        x=x*200.0 + obj%xo
        xs=xs*200.0 + obj%xo
        xinc=(obj%btbtwn(1)+obj%btdo(1))/obj%tpi_eng*scal
        num=obj%bttot(1)
        DO i=1,num
          if(obj%us.ne.0)then
            call splt_us(x,y,obj%tlcs_eng,obj%us,obj%ips,obj%time,obj%tstrt,&
     &                   obj%nrow,obj%otim,obj%theta,   &
     &                   'M',obj%nam_lab(1),obj%nam_lab(2),obj%dev,0.0,&
     &                    obj%ysdel,obj%twid,obj%yedel,obj%nam_lab(3),     &
     &                    obj%nam_lab(4),obj%ycdel,obj%yddel,obj%ian)
          else
            call splt_tml (x,y,obj%yend,obj%tlcs_eng,obj%tstrt,k,obj%ips_eng,&
                           obj%whsec,&
                           obj%lrrl,3,&
     &                     obj%ysdel,obj%yedel,obj%ycdel,obj%yddel,obj%nrow,&
     &                     obj%otim,&
     &                     obj%nam_lab(1),&
     &                     obj%nam_lab(2),obj%nam_lab(3),obj%nam_lab(4),&
     &                     obj%tunit,secend,0.0,obj%dev,&
     &                     obj%twid,xs,obj%cs_lab,obj%ian)
          endif
          x=x+xinc
          xs=xs+xinc
        ENDDO
      endif   ! (obj%rtbp.eq.iyes)

!          May need to  draw timing lines here
      if(obj%tlst.ne.0.or.obj%tbtot.gt.0.or.obj%rtbp.eq.'YES'.or.&
         obj%opt_vl.eq.'BTL'&
         .or.obj%tstrt.ne.0.0.or.obj%nrow.gt.1.or.obj%dev.eq.'CONP'&
         .or.obj%tldots(4).ne.0.or.obj%dev.eq.'CGM')then
        sec=obj%time-real(obj%tlst)/1000.0
        yon=obj%yo-real(obj%tlst)/1000.0*scal*obj%ips_eng
        write(print_lun,*)' TIMING LINE VECTORS CREATED IN SPLT'
          call splt_tlne(yon,sec,obj)
        obj%tldots=0
      endif
!          plot the panel labels
      if(obj%ntpp.gt.0)then
        yinc=(obj%otim+.1)*obj%ips_eng
        call splt_plbl(yinc,obj)
      endif

!          elevation plot
      if(obj%pmse.eq.'YES')then
        call splt_pmse(obj)
      endif

      call cpsplot_xfinish(obj%ian,2)

      if(kntsplt.eq.1.and.obj%dev.eq.'CONP')then
!         Generate a random name for the plot file if job name is longer than
!          10 characters

        plotname=' '
        nc=len_trim(jobname)
        if(nc.gt.9)then
          call splt_randname(plotname)
          write(print_lun,*)' Job name is greater than 9 characters'
          write(print_lun,*)' Name ',plotname,' will be used for the plot'
          write(print_lun,*)' file name'
        else
          plotname=jobname
        endif
      endif

!            write to control card file
        if(obj%dev.eq.'CONP')go to 8000
        if(obj%dev.eq.'CGM')then
          write(print_lun,*)'CGM control card file...'
          write(print_lun,*)obj%xend/200.0,obj%ymax+1.0,jobname,obj%lrrl,&
                            obj%dev_loc,obj%quality,obj%copies,obj%opt_dev
          write(obj%lun_cgmcc)obj%xend/200.0,obj%ymax+1.0,jobname,obj%lrrl,&
                            obj%dev_loc,obj%quality,obj%copies,obj%opt_dev
        else
          xs=obj%tunit-((obj%tunit/2.0)+obj%tunit*obj%nct)
          xe=(obj%ntplot+obj%numbt)*obj%tunit+obj%tunit*obj%nct+obj%tunit/2
          delx=abs(xs/200.0)
          flen=(xe-xs)/scal-delx
          gap=1.0/obj%tpi_eng
          inquire(file=iccname,exist=there)
          if(.not.there)then
            call getlun(lun_ccname,istat)
            open(lun_ccname,file=iccname,iostat=istat,status='NEW',&
                 form='UNFORMATTED')
            write(print_lun,*)' control file written to unit ',lun_ccname
            if(istat.ne.0)then
              ntr=fatal_error
              return
            endif
          endif
          write(print_lun,*)' CONTROL CARD FILE ...'
          write(lun_ccname)obj%dev,gap,delx,flen,obj%ndots,obj%tldots(1),&
     &                     obj%tldots(2),obj%tldots(3)
          write(lun_ccname)obj%tldots(5),obj%tldots(6),obj%dev,plotname
          hcard=obj%sid(1)
          write(lun_ccname)hcard
          hcard=obj%sid(2)
          write(lun_ccname)hcard
          write(print_lun,*)obj%dev,gap,delx,flen,obj%ndots,obj%tldots(1),&
     &                      obj%tldots(2),obj%tldots(3)
          write(print_lun,*)obj%tldots(5),obj%tldots(6),obj%dev,plotname
          write(print_lun,*)obj%sid(1)
          write(print_lun,*)obj%sid(2)
!
          if (obj%dev.eq.'NETP')then
            call getsys_username(uname)
            if(obj%fold.eq.'YES')then
              ctmp='FOLD'
            else
              ctmp='ROLL'
            endif
            com1='                 LABEL 1  OF 1 '
            com1(1:4)=ctmp(1:4)
            com2='            X     '
            k=nint(obj%ymax)                               ! Plot height
            call string_ii2cc(k,ctmp)
            com2(15:16)=ctmp(1:2)
            k=nint(fmax)                                   ! Plot length
            call string_ii2cc(k,ctmp)
            com2(9:11)=ctmp(1:3)
!              Half Scale
            if(obj%hscl.eq.'ONLY')then
              com2(5:8)='HALF'
            else if(obj%dulp.eq.'ONLY')then
              com2(5:8)='DUAL'
            else
              com2(1:7)='400 DOT'
            endif
            write(lun_ccname)uname,obj%copies
            write(lun_ccname)com1
            write(lun_ccname)com2
            write(print_lun,*)uname,obj%copies
            write(print_lun,*)com1
            write(print_lun,*)com2
          endif
        endif
 8000   continue
        write(print_lun,*)' '
        write(print_lun,*)' '
        write(print_lun,*)' XORIGIN OF PLOT = ',obj%xo/200.0,' YORIGIN = ',&
     &                      obj%yo/200.0
        write(print_lun,*)' XEND    OF PLOT = ',obj%xend/200.0,' YEND    = ',&
     &                      obj%yend/200.0
        write(print_lun,*)
        if(olav.eq.0.0)then
          write(print_lun,*)' LARGEST ABSOLUTE VALUE = ',obj%glav
        endif
        write(print_lun,*)
 8050   continue
        write(print_lun,*)'################################################'
        write(print_lun,*)' END             - SPLT END OF DATA'
        write(print_lun,*)' SPLT number = ',kntsplt
        kntsplt=kntsplt+1

          return
      ELSE IF(ntr == FATAL_ERROR)THEN
           return
      ENDIF

!-------------------TRACE PROCESSING HERE---------------------------------

      if(obj%ktrnum.ge.obj%num_tr)return
      DO iiii=1,ntr
!          determine if this trace is to be plotted
!           ktin =counter for traces in
!           ktrnum=counter for traces plotted
!           kdo=counter for number of traces to do in skip-do pattern
!           ktpanl=counter for number of paneled traces plotted
!           kntrow=counter for current row if paneling
!           kntcol=counter for current column if paneling
        obj%ktin =obj%ktin+1
        if(obj%ktin .le.obj%skip_init)cycle
        obj%kdo=obj%kdo+1
        if(obj%kdo.ge.obj%num_do.and.obj%num_skip.gt.0)then
          obj%kdo=0
          obj%skip_init=obj%ktin+obj%num_skip
        endif
        obj%ktrnum=obj%ktrnum+1
        obj%ntplot=obj%ktrnum
        if(obj%ntpp.gt.0)then
          obj%ktpanl=obj%ktpanl+1
          obj%ntplot=obj%ktpanl
        endif
!            copy trace to working buffer
        k=obj%nsamp
        do i=1,obj%nsamp
          if(obj%nvrt.eq.'NO')then
            work(i)=trace(i,iiii)
          else
            work(k)=trace(i,iiii)
            k=k-1
          endif
        enddo
!
!         find the largest absolute value
        trmax=abs(work(mth_isamax(obj%nsamp,work,1)))
        if(trmax.eq.0)go to 120
        if(obj%norm.eq.'EACH'.and.(obj%ntpp.gt.0.or.obj%dev.eq.'CONP'))then
          scl=1.0/trmax
          do i=1,obj%nsamp
            work(i)=work(i)*scl
          enddo
          trmax=1.0
        endif
        if(obj%rp.eq.'YES'.and.(obj%dev.eq.'CONP'.or.obj%dev.eq.'CGM'))then
          do i=1,obj%nsamp
            work(i)=-1.0*work(i)
          enddo
        endif
        obj%glav=amax1(obj%glav,trmax)

!          Compute Scale Factor
        if(trmax.ne.0)factor=127.0/trmax
!
 120    continue
        do I=1,obj%nsamp
          ibuf(I)=nint(work(I)*factor)
          ibuf(I)=iand(ibuf(I),mask1)
        enddo

!           Pack 4 bytes per word
        istat=wrdc_pack(ibuf,obj%nsamp,4,1,swap)

!
!
!          if paneling--write to temp files (ipan)
        obj%kpan=obj%itn
        if(obj%ntpp.gt.0)obj%kpan=obj%ipan
!
        write(obj%kpan)obj%npack
        write(obj%kpan)trmax,(ibuf(j),j=1,obj%npack)
!
!
!
!
        if(obj%ntplot.eq.1.and.obj%kntrow.le.1)then
          obj%bsmt1=header(17,iiii)
          obj%fblkhd=header(obj%blkhd,iiii)
          obj%ftiehd1=header(obj%tieh1,iiii)
          obj%ftiehd2=header(obj%tieh2,iiii)
          obj%fvelhd1=header(obj%ivelx,iiii)
          obj%fvelyd1=header(obj%ively,iiii)
          obj%group1=header(3,iiii)
!             set x and y origin of section (trace 1, sample 1)
          obj%ydif=0.0
          obj%yodif=0.0
          ym=obj%time*obj%ips_eng+obj%fintop
          if(obj%lrrl.eq.'LR')then
            obj%xo=obj%xo+7.0*scal + obj%sladd
            if(obj%path_atbm.ne.PATHCHECK_EMPTY)obj%xo=obj%xo+8.0*scal*obj%frtb
            obj%yo=obj%time*obj%ips_eng*scal+.05*scal
            if(obj%tbot.eq.'YES'.or.obj%ltab.eq.'YES')then
              obj%yo=obj%yo+obj%cs_lab_eng*1.5*scal+80.0
            else if(obj%tbot.eq.'YES')then
              obj%yo=obj%yo+.25*scal
            endif ! itbot.eq.'YES'.or.ltab.eq.'YES'
!            obj%oyo=obj%yo
!              need to see if title block is longer than data
            if(obj%path_atbm.eq.PATHCHECK_EMPTY)go to 200
            iatb=0
            k=index(obj%path_atbm,'ATBM')
            if(k.eq.0)then
! -            try lower case
               k=index(obj%path_atbm,'atbm')
            endif
!                 calculate title block height in order to adjust
!                  the y-origin if necessary
            if(k.ne.0)then
              call getlun(iatbf,istat)
              if(istat.ne.0)then
                call pc_error('SPLT--> Unable to get unit number for title&
                             &block file')
              endif
              call atblk(obj%path_atbm,obj%ipn,jobname,obj%basint,.false.,&
                         istat,print_lun)
              write(print_lun,*)'SPLT: Status after first call to ATB = ',istat
              if(istat.ne.0)then
                write(print_lun,*)'SPLT: Fatal error encountered in ATBLK'
                write(print_lun,*)'      No title block will be done'
                obj%path_atbm=PATHCHECK_EMPTY
                go to 200
              endif
              open(iatbf1,status='OLD',file='atbf',iostat=istat)
              if(istat.ne.0)then
                write(print_lun,*)'SPLT: Unable to open ATBLK output file'
                write(print_lun,*)'      No title block will be done'
              endif
              call splt_thit(tht,iatbf1,ht2)
              close(iatbf1,status='DELETE')
            endif ! k.ne.0
 160        continue
            if(tht.gt.ym)then  !
              if(tht.gt.41.0)tht=41.0
              if(ym.le.35.0)tht=35.0
              if(ym.le.28.0)tht=27.5
!                adjust y-origin so title block will line up with sid
              ydif=tht-ym
              if(ydif.le.0.0)ydif=0.0
              if(ydif.gt.0.000001)then
                ydif=ydif*200.0
                obj%yo=obj%yo+ydif
                write(print_lun,*)'SPLT-->  Y-ORIGIN ADJUSTED TO DUE TO TITLE &
     &                             BLOCK HEIGHT - FROM ',obj%oyo/200.0,' TO ',&
     &                             obj%yo/200.0

              endif
            endif! tht.gt.ym
          else    !   right to left
            obj%xo=obj%xo+7.0*scal
            obj%yo=-1.0*obj%fintop*scal
            if(obj%ltab.eq.'YES')then
              obj%oyo=obj%yo
              obj%yo=obj%yo+obj%cs_lab_eng*1.5*scal+60.0
              obj%yodif=abs(obj%oyo-obj%yo)/200.0
            endif  ! ltab.eq.'YES'.and.obj%dev.ne.iconp
!               allow for maximum room for static plots if use of 30 inch
!               plotter not a factor
!               the 30-inch films have a different orientation so cannot
!                adjust in the same manner as other plots
            if(ym.gt.28.0.and.obj%pmse.eq.'YES')then
              obj%fintop=obj%hitemax-obj%time*obj%ips_eng
              obj%yo=-1.0*obj%fintop*scal
              if(obj%tbot.eq.'YES'.and.obj%ltab.eq.'YES')then
                obj%yo=obj%yo+obj%cs_lab_eng*1.5*scal+60.0
              else if(obj%tbot.eq.'YES')then
                obj%yo=obj%yo+0.25*200.0
              else if(obj%ltab.eq.'YES')then
                obj%yo=obj%yo+obj%cs_lab_eng*1.5*scal+60.0
              endif  ! tbot.eq.'YES'.and.ltab.eq.'YES'
            endif    ! ym.gt.28.0.and.obj%pmse.eq.'YES'
          endif      ! lrrl.eq.lr
 200      continue
          obj%yend=obj%yo-obj%time*obj%ips_eng*scal

        endif ! obj%ntplot.eq.1.and.obj%kntrow.le.1
      if(obj%ntplot.eq.2.and.obj%kntrow.le.1)then
        obj%bsmt2=header(17,iiii)
        obj%basint=obj%bsmt2-obj%bsmt1
        obj%group2=header(3,iiii)
        obj%grpint=obj%group2-obj%group1
!!          almost_equal=ameq(obj%grpint,0.0,.0000001)
!!          if(.not.almost_equal)then
!!            obj%vxtol=abs((header(obj%ivelx,iiii)-obj%fvelhd1)/3.0)
!!          endif
        obj%fvelhd2=header(obj%ivelx,iiii)
        obj%fvelyd2=header(obj%ively,iiii)
        incdec=1
        if(obj%fvelhd1.gt.(fvelhd2+obj%vxtol))incdec=0
        if(obj%nvfid.eq.1)obj%ivxid=incdec
        almost_equal=ameq(obj%fvelhd1,fvelhd2,obj%vxtol)
        if(almost_equal)then
           obj%ntvel =3
        endif
        if(incdec.ne.obj%ivxid)then
          obj%kvfndx=obj%nvfid+1
          obj%kvfinc=-1
          obj%ivxid=incdec
        else
          obj%kvfndx=0
          obj%kvfinc=1
        endif
        if(obj%path_vel.ne.PATHCHECK_EMPTY)then


          call velfile_read_file(obj%velfile,obj%fvelhd1,obj%fvelyd1,&
                                 obj%fvelhd2,obj%fvelyd2,obj%nvfid,&
                                 obj%maxpairs,found_error,card,.true.)
          write(print_lun,*)' reading velfile - X1 = ',obj%fvelhd1,' X2 = ',&
                              obj%fvelhd2
          write(print_lun,*)'                 - Y1 = ',obj%fvelyd1,' Y2 = ',&
                              obj%fvelyd2
          write(print_lun,*)' header for X = ',obj%ivelx,' header for Y = ',&
                              obj%ively

          if(found_error.or.obj%nvfid.le.0)then
            write(print_lun,'(A)')card
            obj%path_vel=PATHCHECK_EMPTY
            obj%needvf=0
            if(obj%nvfid.le.0)then
              write(print_lun,*)'SPLT--> ERROR - No velocity functions were &
                                &selected'
              write(print_lun,*)' '
            endif
          else
            call mem_alloc(obj%tloc,obj%maxpairs)
            call mem_alloc(obj%vloc,obj%maxpairs)
!             Get the first function
            call velfile_get_velfun(obj%velfile,obj%xbd,obj%ybd,obj%npairs,&
                                    obj%tloc,obj%vloc,obj%kfunc)
            obj%kntvf=obj%kntvf+1
            obj%needvf=0
!              set 3d flag 0 = 2d, 1= 3d
            obj%i3d=0
            almost_equal=ameq(obj%fvelyd1,obj%fvelyd2,0.000001)
            if(.not.almost_equal)then
              obj%i3d=1
            endif
!              set velocity flags - 1 = incrementing, 0 = decrementing
            obj%ivxid=1
            if(obj%i3d.eq.0)then
              if(obj%fvelhd1.gt.obj%fvelhd2)obj%ivxid=0
            else
              if(obj%fvelyd1.gt.obj%fvelyd2)obj%ivxid=0
            endif
          endif
        endif
        if(survey_units.eq.'METERS')then
          iscale=nint(obj%tpi*obj%basint*100)
        else
          iscale=nint(obj%tpi_eng*obj%basint*12)
        endif
        call string_ii2cc(iscale,ctmp)
! 350    continue
        write(card,9936)ctmp
         istat=hist_write(obj%ipn,card)
      endif ! obj%ntplot.eq.2.and.obj%kntrow.le.1

!            check for blank trace when header changes
      if(obj%bthc.gt.0)then
        obj%cbth=header(obj%hdr_bt,iiii)
        almost_equal=ameq(obj%cbth,obj%pbth,.001)
        if(.not.almost_equal)then
          obj%btadd=obj%btadd+obj%bthc
          write(obj%btf)obj%ntplot
          obj%pbth=obj%cbth
        endif
      endif

!          adjust trace number for blank traces
        trlab=obj%ntplot
        oldt=obj%ntplot+.00001
        if(obj%bthc.eq.0)then
          if(obj%nbts.gt.0)                                             &
     &      call splt_abt(oldt,trlab,obj%nbts,obj%btskp,obj%btdo,obj%btbtwn,&
     &                    obj%bttot)
          else
            trlab = trlab + real(obj%btadd)
          endif
!
!          set the x-coordinate for this trace
        xtrce=trlab*obj%tunit + obj%xo

      if(obj%kntrow.ne.1)go to 3950
!              plot word hdr_lab as header labels
      if(obj%hdr_lab(1).eq.0.and.obj%hdr_lab(2).eq.0)go to 3500
      if(obj%lab_init.eq.'HDR')then
        cwrh=header(obj%hdr_chg,iiii)
        almost_equal=ameq(cwrh,obj%pwrh,.001)
        if(.not.almost_equal)then
          obj%pwrh=cwrh
          go to 3300
        else
          go to 3500
        endif
      endif
      if(obj%ntplot.lt.obj%istrt)go to 3500
      if(obj%lab_init.ne.'SHOT')obj%istrt=obj%istrt+obj%lab_inc
      if(obj%kntwrdb.ge.obj%iwrbtot.and.obj%lab_init.eq.'SHOT')then
        if(obj%ndxwrdb.gt.obj%nshots)go to 3500
        obj%istrt=obj%trce(obj%ndxwrdb) +.5000001
        obj%lab_inc=obj%intv(obj%ndxwrdb)
        obj%iwrbtot=obj%totl(obj%ndxwrdb)
        obj%kntwrdb=0
        obj%ndxwrdb=obj%ndxwrdb+1
        if(obj%ntplot.lt.obj%istrt)go to 3500
      endif
      obj%kntwrdb=obj%kntwrdb+1
 3300 continue
      obj%y1=obj%yo
      obj%y2=obj%yo+.25*scal
      call cpsplot_xline(obj%ian,xtrce,obj%y1,xtrce,obj%y2,.025,15,201)
      if(obj%ltab.eq.'YES')then
        obj%y2=obj%yend-obj%cs_lab_eng*1.5*scal
        call cpsplot_xline(obj%ian,xtrce,obj%yend,xtrce,obj%y2,.025,15,201)
      endif
      if(obj%hdr_lab(1).eq.0)go to 3400
      if(obj%opt_vl.eq.'YES')then
         call cpsplot_xline (obj%ian,xtrce,obj%yo,xtrce,obj%yend,&
                             obj%wid_vl_eng,15,201)
      endif
      if(obj%opt_vl.eq.'TM')then
        call splt_tmrk(xtrce,obj%yo,obj%tldots(1),obj%tunit,obj%ml10,&
     &                 obj%dy10,obj%wid_vl_eng,obj%ian)
      endif
      numa=nint(header(obj%hdr_lab(1),iiii))
      call string_ii2cc(numa,knum)
      nc=len_trim(knum)
      fnc=nc
!          move x to center justify label
      if(obj%lrrl.eq.'LR')then
        x2=xtrce-((fnc/2.0*(obj%pcnt*obj%cs_lab_eng)))*scal
        if(x2.lt.obj%xo)obj%xbias=obj%xo-x2
      endif
      if(obj%lrrl.eq.'RL')x2=xtrce+((fnc/2.0*(obj%pcnt*obj%cs_lab_eng)))*scal
!
      y=obj%yo+60.0
      call cpsplot_xsymbol(obj%ian,x2,y,knum,nc,obj%cs_lab_eng,15,1,obj%theta)
      if(obj%ltab.eq.'YES')then
        y=obj%yend-60.0-obj%cs_lab_eng*1.5*200.0
        call cpsplot_xsymbol(obj%ian,x2,y,knum,nc,obj%cs_lab_eng,15,1,&
                             obj%theta)
      endif
!
!          if paneling - reset obj%istrt to begin pattern over
!            again for each panel
      if(obj%ntpp.gt.0)then
        if((obj%ntplot+obj%lab_inc).gt.obj%kendpnl)then
          obj%istrt=obj%kendpnl+obj%istrtsv
          obj%kendpnl=obj%kendpnl+obj%ntpp
        endif
      endif
!
!          plot hdr_lab 2,3,and 4
 3400 if(obj%hdr_lab(2).ne.0)then
        numb=nint(header(obj%hdr_lab(2),iiii))
        call string_ii2cc(numb,knum)
!             if wrdb is elevation header - do not plot the minus sign
        k=index(knum,'-')
        if(k.eq.1)then
          ktmp=knum
          knum=' '
          knum(1:7)=ktmp(2:8) !chere
        endif
        nc=len_trim(knum)
        fnc=nc
        if(obj%lrrl.eq.'LR')then
          x2=xtrce-((fnc/2.0*(obj%pcnt*obj%cs_lab_eng)))*scal
        endif
        if(obj%lrrl.eq.'RL')x2=xtrce+((fnc/2.0*(obj%pcnt*obj%cs_lab_eng)))&
                            *scal
        y=obj%yo+obj%yedel
        call cpsplot_xsymbol(obj%ian,x2,y,knum,nc,obj%cs_lab_eng,15,1,&
                             obj%theta)

        if(obj%hdr_lab(3).ne.0)then
          numb=nint(header(obj%hdr_lab(3),iiii))
          call string_ii2cc(numb,knum)
          nc=len_trim(knum)
          fnc=nc
          if(obj%lrrl.eq.'LR')then
            x2=xtrce-((fnc/2.0*(obj%pcnt*obj%cs_lab_eng)))*scal
          endif
          if(obj%lrrl.eq.'RL')x2=xtrce+((fnc/2.0*(obj%pcnt*obj%cs_lab_eng)))*&
                              scal
          y=obj%yo+obj%ycdel
          call cpsplot_xsymbol(obj%ian,x2,y,knum,nc,obj%cs_lab_eng,15,1,&
                               obj%theta)
        endif
        if(obj%hdr_lab(4).ne.0)then
          numb=nint(header(obj%hdr_lab(4),iiii))
          call string_ii2cc(numb,knum)
          nc=len_trim(knum)
          fnc=nc
          if(obj%lrrl.eq.'LR')then
            x2=xtrce-((fnc/2.0*(obj%pcnt*obj%cs_lab_eng)))*scal
          endif
          if(obj%lrrl.eq.'RL')x2=xtrce+((fnc/2.0*(obj%pcnt*obj%cs_lab_eng)))*&
                              scal
          y=obj%yo+obj%yddel
          call cpsplot_xsymbol(obj%ian,x2,y,knum,nc,obj%cs_lab_eng,15,1,&
                               obj%theta)
        endif
        if(obj%lab_init.eq.'SHOT'.and.obj%kntwrdb.lt.obj%iwrbtot)then
           obj%istrt=obj%istrt+obj%lab_inc
        endif
      endif
!          velocity functions
 3500 continue

      if(obj%path_vel.ne.PATHCHECK_EMPTY.and.obj%needvf.lt.2.and.&
         obj%ntplot.ge.obj%ntvel)then
        if(obj%i3d.eq.0)then
        endif
        if(obj%needvf.eq.1.and.obj%kntvf.lt.obj%nvfid)then
          obj%kntvf=obj%kntvf+1
          obj%kvfndx=obj%kvfndx+obj%kvfinc
          obj%tloc=0
          obj%vloc=0
          call velfile_get_velfun(obj%velfile,obj%xbd,obj%ybd,obj%npairs,&
                                  obj%tloc,obj%vloc,obj%kfunc)
          write(print_lun,3520)obj%kfunc,obj%xbd,obj%kntvf
 3520 format(' SPLT - FUNCTION ',a8,' VELX = ',f10.3,' NUMBER = ',i5)
          obj%needvf=0
        endif
!            determine if function plotted on this trace
      if(obj%ntplot.eq.obj%ntvel )then
!            compare to trace 1 first
        if(obj%i3d.eq.1)then
          thedr=obj%fvelyd1
          filhedr=obj%ybd
          frsthedr=obj%fvelyd1
        else
          thedr=obj%fvelhd1
          filhedr=obj%xbd
          frsthedr=obj%fvelhd1
        endif
        call splt_ckh(thedr,filhedr,obj%ivxid,frsthedr,match,&
                      obj%vxtol)
        kflg=0
        if(match.eq.1)then
          kflg=1
          go to 3522
        endif
      endif
      if(obj%i3d.eq.1)then
        thedr=header(obj%ively,iiii)
        filhedr=obj%ybd
        frsthedr=obj%fvelyd1
      else
        thedr=header(obj%ivelx,iiii)
        filhedr=obj%xbd
        frsthedr=obj%fvelhd1
      endif
      call splt_ckh(thedr,filhedr,obj%ivxid,frsthedr,match,&
                    obj%vxtol)
 3522 continue
        if(match.eq.1)then
          subl=' '
          jntplot=obj%ntplot
          f=header(obj%ivelx,iiii)
          if(kflg.eq.1)then
            jntplot=1
            f=obj%fvelhd1
          endif
          kflg=0
          call cpsplot_fnt(obj%ian,0)
!          if obj%vxtol = parts - obj%vxtol may not have been calculated yet
!           when checked first trace - recheck for it now
!         *********** match case ***************
          ibt=obj%btadd
          if(jntplot.eq.1.and.obj%btadd.gt.0)ibt=0
          call splt_vf(obj,jntplot,subl,ibt,istat)
          if(istat.ne.0)then
            write(print_lun,*)'SPLT-->Fatal error encountered in splt_vf'
            ntr=FATAL_ERROR
            return
          endif
          call cpsplot_fnt(obj%ian,19)
          write(print_lun,3525)obj%kfunc,jntplot,obj%xbd,f
 3525 format(' SPLT - FUNCTION ',a8,' PLOTTED AT TRACE ',i6,' XBD = ',  &
     &f8.2,' HEADER = ',f8.2)
          obj%needvf=1
          if(obj%kntvf.ge.obj%nvfid)obj%needvf=2
        endif
        if(match.eq.-1)then
          obj%needvf=1
          if(obj%kntvf.ge.obj%nvfid)obj%needvf=2
          go to 3500
        endif
      endif


 3550 continue
!          block labels
!
      obj%blhead=header(obj%blkhd,iiii)
      if(obj%path_blk.ne.PATHCHECK_EMPTY.and.obj%needblk.lt.2.and.&
         obj%ibl3d.eq.0)then
        if(obj%needblk.eq.1)then
         length=cio_fgetline(card,80,obj%ibf)
         if(length.eq.CIO_EOF)go to 3600
          if(obj%blidfmt.eq.' ')then
            read(card,9006)obj%blhd1,obj%blhd2,obj%bltxt,blline
          else
            read(card,9010)obj%blhd1,obj%blhd2,blline,obj%bltxt,&
                                       itemp,&
                                       ctmp,ctmp2
          endif
          obj%needblk=0
        endif
!              determine if obj%blhd1 plotted on this trace
        if(obj%fhd1.eq.0)then
!                first trace is special case
          if(obj%ntplot.eq.1)then
            if((obj%blhd1.lt.header(obj%blkhd,iiii).and.obj%iblinc.eq.1).or.&
               (obj%blhd1.gt.header(obj%blkhd,iiii).and.obj%iblinc.eq.0))then
               obj%ftr1=0.0
               obj%fhd1=1
               go to 3555
            endif
          endif
          almost_equal=ameq(obj%blhd1,obj%oblhd2,.001)
          if(almost_equal)then
            obj%ftr1=obj%oftr2
            obj%fhd1=1
            go to 3555
          endif
          call splt_ckh(header(obj%blkhd,iiii),obj%blhd1,obj%iblinc,&
                        obj%fblkhd,match,   &
                        0.0)
          if(match.eq.1)then
            obj%ftr1=obj%ntplot
            obj%fhd1=1
          endif
        endif
 3555   continue
!
!              determine if obj%blhd2 plotted on this trace
!                last trace is special case
        if(obj%ntplot.eq.obj%num_tr)then
          if((obj%iblinc.eq.1.and.obj%blhd2.gt.header(obj%blkhd,iiii)).or. &
             (obj%iblinc.eq.0.and.obj%blhd2.lt.header(obj%blkhd,iiii)))then
            obj%ftr2=obj%num_tr+1
            match=1
            call splt_blid(obj%xo,obj%yo,obj%ftr1,obj%ftr2,obj%bltxt,obj%blcs,&
                           obj%tunit,             &
                           obj%num_tr,obj%theta,obj%nbts,obj%btskp,obj%btdo,&
                           obj%btbtwn,        &
                           obj%bttot,obj%ian)
            nc=len_trim(obj%bltxt)
            write(print_lun,*)' SPLT - BLOCK LABEL ',obj%bltxt(1:nc),&
                              ' PLOTTED BETWEEN traces ',obj%FTR1,' and ',&
                                OBJ%FTR2
            obj%needblk=2
            go to 3610
          endif
        endif
        call splt_ckh(header(obj%blkhd,iiii),obj%blhd2,obj%iblinc,obj%fblkhd,&
                      match,0.0)
          if(match.eq.1)then
            obj%ftr2=obj%ntplot
            call splt_blid(obj%xo,obj%yo,obj%ftr1,obj%ftr2,obj%bltxt,obj%blcs,&
                           obj%tunit,             &
     &                     obj%num_tr,obj%theta,obj%nbts,obj%btskp,obj%btdo,&
                           obj%btbtwn,        &
     &                     obj%bttot,obj%ian)
            nc=len_trim(obj%bltxt)

            write(print_lun,*)' SPLT - BLOCK LABEL ',obj%bltxt(:nc),&
                              ' PLOTTED BETWEEN Traces  ',OBJ%FTR1,' and ',&
                                OBJ%FTR2
            obj%needblk=1
            obj%fhd1=0
            obj%oblhd2=obj%blhd2
            obj%oftr2=obj%ftr2
          endif
          if(match.eq.-1)then
            obj%needblk=1
            go to 3550
          endif
        go to 3610
 3600   obj%needblk=2
      endif
 3610 continue

!                 tie lines
!
        if(obj%ntie.eq.0)go to 3620
        if(obj%ntplot.eq.obj%titr(obj%knttie))then
           call splt_tie(obj%xo,obj%yo,obj%tieskp,obj%tics_eng,&
                         obj%tida_eng,obj%tunit,&
                         trlab,obj%tie(obj%knttie), &
                         obj%cs_lab_eng,obj%theta,obj%dev,obj%yend,obj%tbot,&
                         obj%ian)
           obj%knttie=obj%knttie+1
           if(obj%knttie.gt.obj%ntie)obj%ntie=0
        endif
 3620   continue
!
!                   tie lines from a file
        if(obj%path_tie1.ne.PATHCHECK_EMPTY.and.obj%nedtie1.lt.2)then
          if(obj%nedtie1.eq.1)then
            istat=cio_fgetline(card,80,obj%itf1)
            if(istat.eq.CIO_EOF)go to 3630
            read(card,9008)obj%tie1hd,obj%tie1txt
            call string_to_upper(obj%tie1txt)
            obj%nedtie1=0
          endif
!              determine if tie plotted on this trace
          call splt_ckh(header(obj%tieh1,iiii),obj%tie1hd,obj%itf1inc,&
                        obj%ftiehd1,match,0.0)
            if(match.eq.1)then
              call splt_tie(obj%xo,obj%yo,obj%tieskp,obj%tics_eng,obj%tida_eng,&
                            obj%tunit,trlab,obj%tie1txt,&
     &                      obj%cs_lab_eng,obj%theta,obj%dev,obj%yend,obj%tbot,&
                            obj%ian)
              write(print_lun,*)'SPLT-TIE LINE ',obj%tie1txt,' PLOTTED AT &
     &                           TRACE ',trlab
              obj%nedtie1=1
            endif
            if(match.eq.-1)then
              obj%nedtie1=1
              go to 3620
            endif
          go to 3635
 3630     obj%nedtie1=2
        endif
 3635   continue

!
!                 second set of tie lines
        if(obj%path_tie2.ne.PATHCHECK_EMPTY.and.obj%nedtie2.lt.2)then
          if(obj%nedtie2.eq.1)then
            istat=cio_fgetline(card,80,obj%itif2)
            if(istat.eq.CIO_EOF)go to 3640
            read(card,9008)obj%tie2hd,obj%tie2txt
            call string_to_upper(obj%tie2txt)
            obj%nedtie2=0
          endif
!              determine if tie plotted on this trace
          call splt_ckh(header(obj%tieh2,iiii),obj%tie2hd,obj%itf2inc,&
                        obj%ftiehd2,        &
                        match,0.0)
            if(match.eq.1)then
              call splt_tie(obj%xo,obj%yo,obj%tieskp,obj%tics_eng,&
                            obj%tida_eng+1.0,&
                            obj%tunit,trlab,&
                            obj%tie2txt,obj%cs_lab_eng,obj%theta,obj%dev,&
                            obj%yend,&
                            obj%tbot,obj%ian)
              write(print_lun,*)' SPLT -TIE2 LINE ',obj%tie2txt,' PLOTTED AT &
     &                             TRACE ',trlab
              obj%nedtie2=1
            endif
            if(match.eq.-1)then
              obj%nedtie2=1
              go to 3635
            endif
          go to 3645
 3640     obj%nedtie2=2
        endif
 3645   continue

!              dashed line plot
        if(obj%dlu100.ne.0.0)then
          ytemp=obj%yo
          if(obj%tstrt.lt.0.0.and.obj%rezt.eq.'YES')then
            ytemp=obj%yo+obj%tstrt*obj%ips_eng*200.0
          endif
          yy=ytemp+((header(obj%hdr_dl,iiii)-obj%dlref)*obj%dlhite)*scal
          call cpsplot_xline(obj%ian,xtrce-obj%dlhalf,yy,xtrce+obj%dlhalf,yy,&
                             .02,15)
        endif
!
!             plus sign plot
        if(obj%plu100.ne.0)then
          ytemp=obj%yo
          if(obj%tstrt.lt.0.0.and.obj%rezt.eq.'YES')then
            ytemp=obj%yo+obj%tstrt*obj%ips_eng*200.0
          endif
          yyy=ytemp+((header(obj%hdr_pl,iiii)-obj%plref)*obj%plhite)*scal
          call cpsplot_xsymbol(obj%ian,xtrce,yyy,iplus,1,obj%fldsz*2,15,0,&
                               obj%theta)
        endif

!          plot fold of stack
        if(obj%flst.ne.0)then
          ytemp=obj%yo
          if(obj%tstrt.lt.0.0.and.obj%rezt.eq.'YES')then
            ytemp=obj%yo+obj%tstrt*obj%ips_eng*200.0
          endif
          yyyy=ytemp-obj%fin100m*scal+(header(obj%hdr_fl,iiii)*&
      &        obj%fldht)*scal
          x3=xtrce
          call cpsplot_xsymbol(obj%ian,x3,yyyy,'0',1,obj%fldsz,15,0,obj%theta)
        endif

!            plot ovjd marks
 3950   continue
        if(obj%ovjd.ne.0.0)then
          y=obj%yo-(header(obj%hdr_off,iiii)/obj%ovjd*200.0*obj%ips_eng)
          if(obj%kntrow.gt.1)then
            y=y-(obj%otim+.1)*obj%ips_eng*200*(obj%kntrow-1)
          endif
!               don'T GO PASSED END OF PANEL
          ylst=obj%yo-obj%otim*obj%ips_eng*200.0*obj%kntrow
          if(obj%kntrow.gt.1)ylst=ylst-.1*obj%ips_eng*200.0*(obj%kntrow-1)
          if(y.gt.ylst)call cpsplot_xline(obj%ian,xtrce-obj%halfs,y,&
                                          xtrce+obj%halfs,y,obj%ovwd_eng,15)
        endif
!
!            write out elevations for pmse routine
        if(obj%pmse.eq.'YES')then
          a=header(19,iiii)  ! don't need double precision
          write(obj%elevlun)a
          obj%surmin=min(a,obj%surmin)
          obj%surmax=max(a,obj%surmax)
        endif
!
        if(obj%ntpp.gt.0)then
!            see if need to start new panel
          j=mod(obj%ktrnum,obj%ntpp)
          if(j.eq.0)then
            if(.not.obj%allopen)then
              call getlun(obj%ipan,istat)
              if(istat.ne.0)then
                call pc_error('Unable to get unit number for splt panl file')
              endif
              open(obj%ipan,status='NEW',form='UNFORMATTED',file=obj%ipanname,&
                   iostat=istat)
              if(istat.ne.0)then
                call pc_error('Unable to open ',obj%ipanname)
              endif
            else
              inquire(file=obj%ipanname,number=obj%ipan)
            endif
            letter=obj%ipanname(7:7)
            k=ichar(letter)
            k=k+1
            letter=char(k)
            obj%ipanname(7:7)=letter
            obj%kntrow=obj%kntrow+1
            obj%ktpanl=obj%ntpp*(obj%kntcol-1)
            if(obj%kntrow.ge.obj%nrow)obj%allopen=.true.
            if(obj%kntrow.gt.obj%nrow)then
              obj%kntrow=1
              obj%kntcol=obj%kntcol+1
              obj%ktpanl=obj%ntpp*(obj%kntcol-1)
              obj%ipanname =obj%ipansav
              inquire(file=obj%ipanname,number=obj%ipan)
              letter=obj%ipanname(7:7)
              k=ichar(letter)
              k=k+1
              letter=char(k)
              obj%ipanname(7:7)=letter
            endif
          endif
        endif
!
      ENDDO
      return
 9006 format(2f10.0,a10,f8.0)
 9008 format(f10.0,a40)
 9010 format(3f8.0,a51,a1,a2,a1)
 9936 FORMAT(' PLOTSCALE =',A8)
      end subroutine splt
      subroutine splt_abt(tra, tnnw, nbts, iskp, ido, ibtwn, itot)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nbts
      real , intent(in) :: tra
      real , intent(out) :: tnnw
      integer , intent(in) :: iskp(*)
      integer , intent(in) :: ido(*)
      integer , intent(in) :: ibtwn(*)
      integer , intent(in) :: itot(*)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: nblk, itra, i, ltra, m, j

      save nblk, itra, i, ltra, m, j
!-----------------------------------------------
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
!
!
!
      nblk = 0
      itra = tra + 0.0001
      l400: do i = 1, nbts
!
         if (ido(i) == 0) exit  l400
!
         ltra = iskp(i)
         if (ltra >= itra) exit  l400
         m = itot(i)
         do j = 1, m
            nblk = nblk + ido(i)
            if (j == m) cycle  l400
            ltra = ltra + ibtwn(i)
            if (ltra < itra) cycle
            exit  l400
         end do
      end do l400
      tnnw = tra + real(nblk)
      return
      end subroutine splt_abt
      subroutine splt_arc (iunit,x, y, r1, r2, theta)
!
!          draws an arc of thickness specified by giving radius r1
!            and r2 as outside and inside radius of an open cirlce
!           This routine is used when doing conplot
!
!       x   = x coordinate for center of circle
!       y   = y coordinate for center of circle
!       r1  = outside radius
!       r2  = inside radius
!     theta = portion of circle to keep for making arc specified in
!             degrees
!          theta must be 0,90,180, or 270
!
      integer,  intent(in) :: iunit
      real,     intent(in) :: x
      real,     intent(in) :: y
      real,     intent(in) :: r1
      real,     intent(in) :: r2
      real,     intent(in) :: theta
!
      real xpts(5), ypts(5), rad
!


!
!          convert theta to radians
      rad = theta * pi / 180.0
!
!          draw red circle of outside radius
      call cpsplot_tclr (iunit,6)
      call cpsplot_circ (iunit,x, y, r1, 0)
!c
!          white out inside of circle with inside radius
      call cpsplot_tclr (iunit,9)
      call cpsplot_circ (iunit,x, y, r2, 0)
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
      call cpsplot_xpoly (iunit,xpts, ypts, 5, 1)
      return
      end subroutine splt_arc

      subroutine splt_arow(x1,y1,arrowh,arrowl,label,nenda,alpha,idev,yo,&
                          ipf1,iunit)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*)  :: label
      integer , intent(in) :: nenda,iunit
      character(len=*) , intent(in) :: idev
      integer , intent(in) :: ipf1
      real , intent(in) :: x1
      real , intent(in) :: y1
      real , intent(in) :: arrowh
      real , intent(in) :: arrowl
      real  :: alpha
      real , intent(in) :: yo
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i,nchar, npts, mir
      real , dimension(5) :: xarrow, yarrow
      real :: perh, perw, percw, unitah, unital, headl, warrow, hlabel, with, &
         thick, space, btwn, beta, gama, unitwt, unitsp, xs, ys, yprint1, &
         yprint2
      character(len=8)lab,iconp

      save
!-----------------------------------------------
! this routine plots and labels an arrow
!   x1     = the extreme left x-coordinate of the arrow
!   y1     = the extreme left y-coordinate of the arrow
!   arrowh = the height of the arrow
!   arrowl = the length of the arrow
!   label  = 3 character label of the arrow
!   nenda  = left arrow if 0 right arrow if 1
!   alpha  = angle to plot annotation
!   idev   = device to plot on
!   yo     = yo of plot
!   ipf1   = print file
!   iunit  = plot file unit number
!
      data iconp/ 'CONP'/
      nchar=len_trim(label)
      npts = 5
      perh = 4.0
      perw = 8.0
      percw = 3.0
      unitah = 200.0*arrowh
      unital = 200.0*arrowl
      headl = unitah*perh
      warrow = unitah/perw
      hlabel = (unitah + unitah/2.5)/200.0
      with = hlabel*0.7
      thick = hlabel*0.13
      space = hlabel*0.85
      btwn = (space - with)*200.0
      beta = 0.0
      gama = 0.0
      mir = 0
      unitwt = with*200.0
      unitsp = space*200.0
      if (nenda /= 0) then
!
!          first arrow
         xarrow(1) = x1
         yarrow(1) = y1
         xarrow(2) = x1
         yarrow(2) = y1 + warrow
         xarrow(3) = x1 + unital + warrow*percw - headl
         yarrow(3) = yarrow(2)
         xarrow(4) = x1 + unital - headl
         yarrow(4) = y1 + unitah
         xarrow(5) = x1 + unital
         yarrow(5) = y1
         xs = xarrow(4) - (nchar + 0.5)*unitsp + btwn
         if (alpha /= 0.) xs = xarrow(4) - unitsp*0.5
      else
         xarrow(1) = x1
         yarrow(1) = y1
         xarrow(2) = x1 + headl
         yarrow(2) = y1 + unitah
         yarrow(3) = y1 + warrow
         xarrow(3) = xarrow(2) - warrow*percw
         xarrow(4) = x1 + unital
         yarrow(4) = yarrow(3)
         xarrow(5) = xarrow(4)
         yarrow(5) = y1
         xs = xarrow(2) + unitsp*0.5
         if (alpha /= 0.) xs = xarrow(2) + (nchar + 0.5)*unitsp - btwn
      endif
      DO i=1,npts
        xarrow(i)=xarrow(i)/200.0
        yarrow(i)=yarrow(i)/200.0
      ENDDO
      call cpsplot_xpoly (iunit,xarrow(1), yarrow(1), npts, 15, 400)
      ys = y1 + (hlabel*200.0)/2.0 + 2.0*warrow
      call cpsplot_xsymbol (iunit,xs, ys, label, nchar, hlabel, 15, 2, alpha)
      yprint1 = y1/200.0 - yo/200.0
      yprint2 = ys/200.0 + hlabel - yo/200.0
      lab = 'ARROWS'
      write (ipf1, 9001) yprint1, yprint2, lab
      return
 9001 format(f6.2,2x,f6.2,2x,a8)
      return
      end subroutine splt_arow

      subroutine splt_blid(xo,yo,ftr,ltr,idbl,blcs,tunt,nt,theta,nbts, &
                          ibtskp,ibtdo,ibtbtwn,ibttot,iunit)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nt
      integer  :: nbts,iunit
      real , intent(in) :: xo
      real , intent(in) :: yo
      real , intent(in) :: ftr
      real , intent(in) :: ltr
      real  :: blcs
      real , intent(in) :: tunt
      real  :: theta
      character  :: idbl*(*)
      integer  :: ibtskp(*)
      integer  :: ibtdo(*)
      integer  :: ibtbtwn(*)
      integer  :: ibttot(*)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: idum, nbkc
      real :: ltr1, units, ftr1, ylocl, yloch, wth, trlab, oldt, xloc, dum1, &
         dum2, dum3, dum4, f1, f2, ds
      character(len=1) :: iperod='.'

      save ltr1, iperod, units, ftr1, ylocl, yloch, wth, trlab, oldt, xloc, &
         idum, dum1, dum2, dum3, dum4, nbkc, f1, f2, ds
!-----------------------------------------------
!
!
!     plot block id
!
!     ftr   = first trace boundary
!     ltr   = last trace boundary
!     idbl  = id of the block
!     blcs  =  character size
!     tunt  = trace units
!     nt    = # of traces in section
!     theta = angle to plot text
!      nbts = the number of blank trace patterns.
!   ibtskp  = number of traces to skip before blanks.
!   ibtdo   = number of blanks to insert.
!  ibtbtwn  = number of traces between blanks.
!   ibttot  = total number of sets this pattern
!   iunit   = unit number for plot file
!
      units = 200.0
!
!
      ftr1 = ftr
      ltr1 = ltr
!
      ylocl = (-0.2*units) + yo
      yloch = 0.1*units + yo
!
!          calculate line width based on blcs - with .3 being
!            100% blcs and .05 being 100% line width
!
      wth = blcs*0.05/0.3
      wth = amax1(0.025,wth)
      if (wth > 0.05) wth = 0.05
!
!  check for block id left of last trace
      if (ftr1 > 0.) then
         trlab = ftr1
         oldt = ftr1
         call splt_abt (oldt, trlab, nbts, ibtskp, ibtdo, ibtbtwn, ibttot)
         xloc = trlab*tunt + xo
         call cpsplot_xline (iunit,xloc, ylocl, xloc, yloch, wth, 15, 1000)
      endif
      idum = ltr1
!
      if (ltr1 <= nt) then
         trlab = ltr1
         oldt = ltr1
         call splt_abt (oldt, trlab, nbts, ibtskp, ibtdo, ibtbtwn, ibttot)
         xloc = trlab*tunt + xo
!          reference arguments to prevent loseing their values
         dum1 = xloc
         dum2 = ylocl
         dum3 = yloch
         dum4 = wth
         call cpsplot_xline (iunit,xloc, ylocl, xloc, yloch, wth, 15, 1010)
      endif
      if (idbl(1:) /= iperod) then
         if (ftr1 <= 0.) ftr1 = 1.
         if (ltr1 > nt) ltr1 = nt
         nbkc=len_trim(idbl)
         call splt_abt (ftr1, f1, nbts, ibtskp, ibtdo, ibtbtwn, ibttot)
         call splt_abt (ltr1, f2, nbts, ibtskp, ibtdo, ibtbtwn, ibttot)
         xloc = tunt*(f1 + (f2 - f1)/2.) + xo
         ds = blcs*(nbkc/2.)*units
         xloc = xloc - ds
         if (theta /= 0.) xloc = xloc + 2.*ds
         ylocl = ylocl - 0.6*blcs*units
         call cpsplot_xsymbol (iunit,xloc,ylocl,idbl,nbkc,blcs,15,2,theta)
      endif
!
!
      return
      end subroutine splt_blid

      subroutine splt_brac(card, mcd, nca, nci,nfield)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=80) , intent(in) :: card
      character(len=80) , intent(out) :: mcd
      integer , intent(out) :: nca(20)
      integer , intent(out) :: nci(20),nfield
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: ib,i,nb,icol,nc,jj

!-----------------------------------------------
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
!      nci  = array to return # of char. infront of field
!
      ib = 0
      mcd = card
      nca = 0
      ib = 0
      nb = 0
      icol = 0
      do i = 1, 20
         ib = index(mcd(icol+1:),'[') + icol
         if (ib == icol) exit
         icol = ib + 1
         nb = index(mcd(icol+1:),']') + icol
         if (nb == icol) go to 500
         nc = nb - ib - 1
         nca(i) = nc
         nci(i) = ib + 1
         do jj = ib, nb
            mcd(jj:jj) = ' '
         end do
         nfield = i
      end do
      return
  500 continue
      write (print_lun, *) 'SPLT_BRAC-->ERROR IN TITLE BLOCK CARD WAS '
      write (print_lun, *) mcd
!
      return
      end subroutine splt_brac

      subroutine splt_brc1(ia, card, ans)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(inout) :: ia
      character , intent(in) :: card*80
      character , intent(out) :: ans*10
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: k
      character :: l, r, cblk*10

      save
!-----------------------------------------------
!
!          gets out parameter between brackets.  ia is updated to 1st
!           character after right bracket
!
!          ia    = char to begin looking for left bracket
!          card  = input character array to search
!          ans   = information between brackets to return
!
!
!
      data l/ '['/
      data r/ ']'/
      data cblk/ '          '/
!
!
      k = 1
      do while(card(ia:ia) /= l)
         ia = ia + 1
      end do
!
      ia = ia + 1
      ans = cblk
      ans(k:k) = card(ia:ia)
      k = k + 1
      ia = ia + 1
      if (card(ia:ia) == r) go to 200
      do while(card(ia:ia) /= r)
         ans(k:k) = card(ia:ia)
         k = k + 1
         ia = ia + 1
      end do
!
  200 continue
      ia = ia + 1
      return
      end subroutine splt_brc1


      subroutine splt_cabl(x,y,nci,nca,ht,ang,idev,inp,iunit)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*), intent(in) :: idev
      real , intent(in) :: x
      real , intent(in) :: y
      real , intent(in) :: ht
      real  :: ang
      integer  :: nci(20)
      integer  :: nca(20)
      integer,intent(in) :: inp,iunit
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      character(len=80) :: mcd
      character(len=16) :: icf
      character(len=1)  :: ich
      integer :: ia, icol, k, j, nc ,nfield
      real , dimension(9) :: xb, yb
      real , dimension(3) :: xp, yp
      real :: scal, siz1, siz2, htfac, units, xfb, yfb, xan, yan, xcr1, xcr2, &
              ycr, xc1, xc4, yc1, yc2, yc3, ycl, ch, chs, chr, chsr, yadj, &
              xadj, xc2,&
              xch, xc5, xend, xc3, ych, x6end, yc4, xc6, xsr, ysr1, ysr2, &
              wts, tks
      character :: inf*80, idist*10, irec*10, cblk*10

      save

!-----------------------------------------------!
! *** routine to plot cable diagram
!
!     aurguments:
!      x   = x cordinate
!      y   = y coordinate
!      nci = # of char. in front of field
!      nca = # of char. in field
!      ht  = character height
!      ang = angle to plot annotation
!     idev = device to plot on
!      inp = The unit number of the title block file
!
      data cblk/ '          '/
!
      scal = 200.0
      siz1 = 0.005
      siz2 = 0.01
      htfac = 1.0
!
      units = scal
      if (ht < 0.1) then
         htfac = ht/0.1
         units = units*htfac
      endif
!
      xfb = x
      read (inp, 9001) inf
      call splt_brac (inf, mcd, nca, nci,nfield)
      if (nfield /= 8) then
         write (print_lun, *) ' '
         write (print_lun, *) '***********************************************&
     &                         ********'
         write (print_lun, *) &
            ' SPLT => ERROR - Cable diagram card must contain 8 fields'
         write (print_lun, *) '                 Number of fields found = ', &
                               nfield
         write (print_lun, *) '                 Cable diagram not done'
         write (print_lun, *) ' '
         write (print_lun, *) '***********************************************&
     &                         ********'
         return
      endif
!
      yfb = y
!
! *** points for boat
!
      xb(1) = xfb
      yb(1) = yfb
      if (ang == 0.0) then
         xb(2) = xfb + 0.375*units
      else
         xb(2) = xfb - 0.375*units
      endif
      yb(2) = yfb
!    front top
      xb(3) = xb(2)
      yb(3) = yfb + 0.08*units
!    top of boat
      if (ang == 0.0) then
         xb(4) = xfb + 0.625*units
      else
         xb(4) = xfb - 0.625*units
      endif
      yb(4) = yb(3)
!    back of top
      xb(5) = xb(4)
      yb(5) = yfb - 0.06*units
!    back deck
      if (ang == 0.0) then
         xb(6) = xfb + 1.25*units
      else
         xb(6) = xfb - 1.25*units
      endif
      yb(6) = yb(5)
!    back of boat
      xb(7) = xb(6)
      yb(7) = yfb - 0.19*units
!    bottom of boat
      if (ang == 0.0) then
         xb(8) = xfb + 0.125*units
      else
         xb(8) = xfb - 0.125*units
      endif
      yb(8) = yb(7)
!    front of boat
      xb(9) = xb(1)
      yb(9) = yb(1)
!

      call cpsplot_xlines (iunit,xb, yb, 9, siz2, 15)
!
! *** antenna
!
      if (ang == 0.0) then
         xan = xfb + 0.375*units
      else
         xan = xfb - 0.375*units
      endif
      yan = yfb + 0.43*units
      call cpsplot_xline (iunit,xan, yb(3), xan, yan, siz1, 15)
      if (ang == 0.0) then
         xcr1 = xan - 0.02*units
         xcr2 = xan + 0.02*units
      else
         xcr1 = xan + 0.02*units
         xcr2 = xan - 0.02*units
      endif
      ycr = yan - 0.02*units
      call cpsplot_xline (iunit,xcr1, ycr, xcr2, ycr, siz1, 15)
      if (ang == 0.0) then
         xcr1 = xan - 0.035*units
         xcr2 = xan + 0.035*units
      else
         xcr1 = xan + 0.035*units
         xcr2 = xan - 0.035*units
      endif
      ycr = yan - 0.05*units
      call cpsplot_xline (iunit,xcr1, ycr, xcr2, ycr, siz1, 15)
!
! *** center lines
!
      if (ang == 0.0) then
         xc1 = xfb + 1.065*units
      else
         xc1 = xfb - 1.065*units
      endif
      xc4 = xc1
      yc1 = yfb + 0.43*units
      yc2 = yfb - 0.5*units
      yc3 = yc1 - 0.065*units
! *** source center line
      call cpsplot_xline (iunit,xc1, yc1, xc1, yc2, siz1, 15)
      ycl = yc2 - 0.05*units
      ch = 0.1
      chs = 0.08
      chr = ch*htfac
      chsr = chs*htfac
      yadj = -1.0*ch/2.0*units
      if (ang == 0.0) then
         xadj = -1.0*ch/2.0*units
      else
         xadj = chs/2.0*units
      endif
      call cpsplot_xsymbol (iunit,xc1+xadj,ycl+yadj,'C',1,chr,15,4,ang)
      call cpsplot_xsymbol (iunit,xc1,ycl -ch*units,'L',1,chr,15,2,ang)
      if (ang == 0.0) then
         xc2 = xc1 + 1*units
      else
         xc2 = xc1 - 1*units
      endif
      call cpsplot_xline (iunit,xc1, yc3, xc2, yc3, siz1, 15)
      icf=' '
      icf(1:nca(4))=inf(nci(4):nci(4)+nca(4)-1)
      if (ang == 0.0) then
         xch = xc2 + ch*units
      else
         xch = xc2 - ch*units
      endif
! *** field 4
      call cpsplot_xsymbol (iunit,xch, yc3, icf, nca(4), chr, 15, 2, ang)
      xp(1) = xc1
      yp(1) = yc3
      if (ang == 0.0) then
         xp(2) = xc1 + 0.06*units
      else
         xp(2) = xc1 - 0.06*units
      endif
      yp(2) = yc3
      if (ang == 0.0) then
         xp(3) = xc1 + 0.07*units
      else
         xp(3) = xc1 - 0.07*units
      endif
      yp(3) = yc3 - 0.02*units
!!      call cpsplot_xpoly (iunit,xp, yp, 3, 15)
      xc5 = xc1
!
      if (ang == 0.0) then
         xc1 = xc1 + 3.065*units
      else
         xc1 = xc1 - 3.065*units
      endif
      xend = xc1
      xc3 = xc1
! *** end of cable
      call cpsplot_xline (iunit,xc1, yc1, xc1, yc2, siz1, 15)
      call cpsplot_xsymbol (iunit,xc1+xadj,ycl+yadj,'C',1,chr,15,4,ang)
      call cpsplot_xsymbol (iunit,xc1,ycl-ch*units,'L',1,chr,15,2,ang)
      icf(1:3) = 'N0.'
      icf(4:4+nca(8)-1)=inf(nci(8):nci(8)+nca(8)-1)
      if (ang == 0.0) then
         xch = xc1 - 3*ch*units
      else
         xch = xc1 + 3*ch*units
      endif
      ych = yc2 - 4*ch*units
! *** field 8
      call cpsplot_xsymbol (iunit,xch, ych, icf, nca(8) + 4, chr, 15, 2, ang)
!
      call cpsplot_xline(iunit,xc1-0.19*units,yb(8),xc1+0.19*units,yb(8),&
                         0.015,15)
      if (ang == 0.0) then
         xc2 = xc1 - 1.*units
      else
         xc2 = xc1 + 1.*units
      endif
      call cpsplot_xline (iunit,xc2, yc3, xc1, yc3, siz1, 15)
      xp(1) = xc1
      yp(1) = yc3
      if (ang == 0.0) then
         xp(2) = xc1 - 0.06*units
      else
         xp(2) = xc1 + 0.06*units
      endif
      yp(2) = yc3
      xp(3) = xc1 - 0.07*units
      yp(3) = yc3 - 0.02*units
!!      call cpsplot_xpoly (iunit,xp, yp, 3, 15)
      if (ang == 0.0) then
         xc1 = xfb + 2.*units
      else
         xc1 = xfb - 2.*units
      endif
      yc1 = yfb + 0.25*units
! *** start of cable
      call cpsplot_xline (iunit,xc1, yc1, xc1, yc2, siz1, 15)
      call cpsplot_xsymbol (iunit,xc1+xadj,ycl+yadj,'C',1,chr,15,4,ang)
      call cpsplot_xsymbol (iunit,xc1,ycl-ch*units,'L',1,chr,15,2,ang)
      icf(1:3) = 'NO.'
      icf(4:4+nca(6)-1)=inf(nci(6):nci(6)+nca(6)-1)
      if (ang == 0.0) then
         xch = xc1 - 3*ch*units
         x6end = xch + (nca(6)+4)*ch*units
      else
         xch = xc1 + 3*ch*units
         x6end = xch - (nca(6)+4)*ch*units
      endif
      ych = yc2 - 4*ch*units
! *** field 6
      call cpsplot_xsymbol (iunit,xch, ych, icf, nca(6) + 4, chr, 15, 2, ang)
      call cpsplot_xline (iunit,xc1-0.19*units,yb(8),xc1+0.19*units,yb(8),&
                          0.015, 15)
      yc3 = yc1 - 0.065*units
      if (ang == 0.0) then
         xc2 = xan + 0.09*units
      else
         xc2 = xan - 0.09*units
      endif
! *** antenna to source
      call cpsplot_xline (iunit,xan, yc3, xc2, yc3, siz1, 15)
      icf=' '
      icf(1:nca(1))=inf(nci(1):nci(1)+nca(1)-1)
      if (ang == 0.0) then
         xch = xc2 + chs*units/2.
      else
         xch = xc2 - chs*units/2.
      endif
! *** field 1
      call cpsplot_xsymbol (iunit,xch, yc3, icf, nca(1), chsr, 15, 2, ang)
      xp(1) = xan
      yp(1) = yc3
      if (ang == 0.0) then
         xp(2) = xan + 0.06*units
      else
         xp(2) = xan - 0.06*units
      endif
      yp(2) = yc3
      if (ang == 0.0) then
         xp(3) = xan + 0.07*units
      else
         xp(3) = xan - 0.07*units
      endif
      yp(3) = yc3 - 0.02*units
!!      call cpsplot_xpoly (iunit,xp, yp, 3, 15)
      if (ang == 0.0) then
         xc2 = xc5 - 0.09*units
      else
         xc2 = xc5 + 0.09*units
      endif
      call cpsplot_xline (iunit,xc2, yc3, xc5, yc3, siz1, 15)
      xp(1) = xc5
      yp(1) = yc3
      if (ang == 0.0) then
         xp(2) = xc5 - 0.06*units
      else
         xp(2) = xc5 + 0.06*units
      endif
      yp(2) = yc3
      if (ang == 0.0) then
         xp(3) = xc5 - 0.07*units
      else
         xp(3) = xc5 + 0.07*units
      endif
      yp(3) = yc3 - 0.02*units
!!      call cpsplot_xpoly (iunit,xp, yp, 3, 15)
!
      yc4 = yc3 - 0.15*units
      if (ang == 0.0) then
         xc2 = xc4 + 0.25*units
      else
         xc2 = xc4 - 0.25*units
      endif
! *** source to cable
      call cpsplot_xline (iunit,xc4, yc4, xc2, yc4, siz1, 15)
      icf=' '
      icf(1:nca(2))=inf(nci(2):nci(2)+nca(2)-1)
      xch = xc2 + chs*units/2.
! *** field 2
      call cpsplot_xsymbol (iunit,xch, yc4, icf, nca(2), chsr, 15, 2, ang)
      xp(1) = xc4
      yp(1) = yc4
      if (ang == 0.0) then
         xp(2) = xc4 + 0.06*units
      else
         xp(2) = xc4 - 0.06*units
      endif
      yp(2) = yc4
      if (ang == 0.0) then
         xp(3) = xc4 + 0.07*units
      else
         xp(3) = xc4 - 0.07*units
      endif
      yp(3) = yc4 - 0.02*units
!!      call cpsplot_xpoly (iunit,xp, yp, 3, 15)
      if (ang == 0.0) then
         xc2 = xc1 - 0.25*units
      else
         xc2 = xc1 + 0.25*units
      endif
      call cpsplot_xline (iunit,xc2, yc4, xc1, yc4, siz1, 15)
      xp(1) = xc1
      yp(1) = yc4
      if (ang == 0.0) then
         xp(2) = xc1 - 0.06*units
      else
         xp(2) = xc1 + 0.06*units
      endif
      yp(2) = yc4
      if (ang == 0.0) then
         xp(3) = xc1 - 0.07*units
      else
         xp(3) = xc1 + 0.07*units
      endif
      yp(3) = yc4 - 0.02*units
!!      call cpsplot_xpoly (iunit,xp, yp, 3, 15)
      if (ang == 0.0) then
         xc2 = xc1 + 0.5*units
      else
         xc2 = xc1 - 0.5*units
      endif
      call cpsplot_xline (iunit,xc1, yc3, xc2, yc3, siz1, 15)
! *** field 5
      icf=' '
      icf(1:nca(5))=inf(nci(5):nci(5)+nca(5)-1)
      if (ang == 0.0) then
         xch = xc2 + ch*units
      else
         xch = xc2 - ch*units
      endif
      call cpsplot_xsymbol (iunit,xch, yc3, icf, nca(5), chr, 15, 2, ang)
      xp(1) = xc1
      yp(1) = yc3
      if (ang == 0.0) then
         xp(2) = xc1 + 0.06*units
      else
         xp(2) = xc1 - 0.06*units
      endif
      yp(2) = yc3
      if (ang == 0.0) then
         xp(3) = xc1 + 0.07*units
      else
         xp(3) = xc1 - 0.07*units
      endif
      yp(3) = yc3 - 0.02*units
!!      call cpsplot_xpoly (iunit,xp, yp, 3, 15)
      xc6 = xc1
      if (ang == 0.0) then
         xc1 = xc3 - 0.5*units
      else
         xc1 = xc3 + 0.5*units
      endif
      call cpsplot_xline (iunit,xc1, yc3, xc3, yc3, siz1, 15)
      xp(1) = xc3
      yp(1) = yc3
      if (ang == 0.0) then
         xp(2) = xc3 - 0.06*units
      else
         xp(2) = xc3 + 0.06*units
      endif
      yp(2) = yc3
      if (ang == 0.0) then
         xp(3) = xc3 - 0.07*units
      else
         xp(3) = xc3 + 0.07*units
      endif
      yp(3) = yc3 - 0.02*units
!!      call cpsplot_xpoly (iunit,xp, yp, 3, 15)
      if (ang == 0.0) then
         xc1 = xfb + 2.625*units
         xc2 = xc6 + 0.09*units
      else
         xc1 = xfb - 2.625*units
         xc2 = xc6 - 0.09*units
      endif
      call cpsplot_xline (iunit,xc6, yc4, xc2, yc4, siz1, 15)
      icf=' '
      icf(1:nca(3))=inf(nci(3):nci(3)+nca(3)-1)
      if (ang == 0.0) then
         xch = xc2 + chs*units
      else
         xch = xc2 - chs*units
      endif
! *** field 3
      call cpsplot_xsymbol (iunit,xch, yc4, icf, nca(3), chsr, 15, 2, ang)
      xp(1) = xc6
      yp(1) = yc4
      if (ang == 0.0) then
         xp(2) = xc6 + 0.06*units
      else
         xp(2) = xc6 - 0.06*units
      endif
      yp(2) = yc4
      if (ang == 0.0) then
         xp(3) = xc6 + 0.07*units
      else
         xp(3) = xc6 - 0.07*units
      endif
      yp(3) = yc4 - 0.02*units
!!      call cpsplot_xpoly (iunit,xp, yp, 3, 15)
      if (ang == 0.0) then
         xc2 = xc1 - 0.09*units
      else
         xc2 = xc1 + 0.09*units
      endif
      call cpsplot_xline (iunit,xc2, yc4, xc1, yc4, siz1, 15)
      xp(1) = xc1
      yp(1) = yc4
      if (ang == 0.0) then
         xp(2) = xc1 - 0.06*units
      else
         xp(2) = xc1 + 0.06*units
      endif
      yp(2) = yc4
      if (ang == 0.0) then
         xp(3) = xc1 - 0.07*units
      else
         xp(3) = xc1 + 0.07*units
      endif
      yp(3) = yc4 - 0.02*units
!!      call cpsplot_xpoly (iunit,xp, yp, 3, 15)
!
      yc1 = yb(3)
! *** center line from start to next receiver
      call cpsplot_xline (iunit,xc1, yc1, xc1, yc2, siz1, 15)
      call cpsplot_xsymbol (iunit,xc1 + xadj, ycl + yadj,'C',1,chr,15,4,ang)
      call cpsplot_xsymbol (iunit,xc1, ycl - ch*units, 'L', 1, chr, 15, 2,ang)
      icf(1:3) = 'NO.'
      icf(4:4+nca(7)-1)=inf(nci(7):nci(7)+nca(7)-1)
      if (ang == 0.0) then
         xch = xc1 - 3*ch*units
      else
         xch = xc1 + 3*ch*units
      endif
      ych = yc2 - 4*ch*units
!          make sure fields 6 and 7 do not overlap
      if (ang==0.0 .and. xch<x6end .or. ang/=0.0 .and. xch>x6end)then
         ych = yc2 - 6*ch*units
      endif

! *** field 7
      call cpsplot_xsymbol (iunit,xch, ych, icf, nca(7) + 4, chr, 15, 2, ang)
      call cpsplot_xline (iunit,xc1 - 0.19*units, yb(8), xc1 + 0.19*units, &
                          yb(8), 0.015, 15)
!
!          test for continuation card
      ich = ' '
      ich(1:1)=inf(80:80)
      if (ich /= ' ') then
!
         read (inp, 9001, end=10000) inf
10000    continue
         idist = cblk
         irec = cblk
         ia = index(inf,']')
         idist(1:ia-1)=inf(2:2+ia-1-1)
         icol = ia
         k = index(inf(icol+1:),'[') + icol
         icol = k
         k = k + 1
         j = index(inf(icol+1:),']') + icol
         nc = j - k
         irec(1:nc)=inf(k:k+nc-1)
!
         if (idist/=' ' .or. irec/=' ') then
!
!          center line from end to next receiver
            xc1 = xfb + 3.5*units
            call cpsplot_xline (iunit,xc1, yc1, xc1, yc2, siz1, 15)
            call cpsplot_xsymbol(iunit,xc1+xadj,ycl+yadj,'C',1,chr,15, 4, ang)
            call cpsplot_xsymbol(iunit,xc1,ycl-ch*units,'L',1,chr,15,2, ang)
            icf(1:3) = 'N0.'
            irec=adjustl(irec)
            icf(5:9)=irec(1:5)
            if (ang == 0.0) then
               xch = xc1 - 3*ch*units
            else
               xch = xc1 + 3*ch*units
            endif
!
!          receiver label
            call cpsplot_xsymbol (iunit,xch, ych, icf, 6, chr, 15, 2, ang)
            call cpsplot_xline (iunit,xc1 - 0.19*units, yb(8), &
                                xc1 + 0.19*units, yb(8), 0.015,15)
!
!          label between end and next receiver
!             left arrow
            if (ang == 0.0) then
               xc2 = xc1 + 0.09*units
            else
               xc2 = xc1 - 0.09*units
            endif
            call cpsplot_xline (iunit,xc1, yc4, xc2, yc4, siz1, 15)
            xp(1) = xc1
            yp(1) = yc4
            if (ang == 0.0) then
               xp(2) = xc1 + 0.06*units
            else
               xp(2) = xc1 - 0.06*units
            endif
            yp(2) = yc4
            if (ang == 0.0) then
               xp(3) = xc1 + 0.07*units
            else
               xp(3) = xc1 - 0.07*units
            endif
            yp(3) = yc4 - 0.02*units
!!            call cpsplot_xpoly (iunit,xp, yp, 3, 15)
!
!            label
            if (ang == 0.0) then
               xch = xc2 + chs*units
            else
               xch = xc2 - chs*units
            endif
            call cpsplot_xsymbol (iunit,xch, yc4, idist, 4, chsr, 15, 2, ang)
!
!            right arow
            if (ang == 0.0) then
               xc2 = xend - 0.09*units
            else
               xc2 = xend + 0.09*units
            endif
            call cpsplot_xline (iunit,xc2, yc4, xend, yc4, siz1, 15)
            xp(1) = xend
            yp(1) = yc4
            if (ang == 0.0) then
               xp(2) = xend - 0.05*units
            else
               xp(2) = xend + 0.05*units
            endif
            yp(2) = yc4
            if (ang == 0.0) then
               xp(3) = xend - 0.07*units
            else
               xp(3) = xend + 0.07*units
            endif
            yp(3) = yc4 - 0.02*units
!!            call cpsplot_xpoly (iunit,xp, yp, 3, 15)
!
! *** source
!
         endif
      endif
      if (ang == 0.0) then
         xsr = xfb + 0.94*units
      else
         xsr = xfb - 0.94*units
      endif
      ysr1 = yb(6)
      ysr2 = yfb - 0.38*units
      call cpsplot_xline (iunit,xsr, ysr1, xsr, ysr2, siz1, 15)
!
      wts = 0.063*0.7*htfac
      tks = 0.063*0.13*htfac
      call cpsplot_xsymbol(iunit,xsr+xadj,ysr2+yadj,'O',1,0.063*htfac,15,4,&
                           ang)
      if (ang == 0.0) then
         xsr = xfb + 1.19*units
      else
         xsr = xfb - 1.19*units
      endif
      ysr1 = yb(7)
      call cpsplot_xline (iunit,xsr + xadj, ysr1 + yadj, xsr, ysr2, siz1, 15)
      call cpsplot_xsymbol (iunit,xsr, ysr2, 'O', 1, 0.063*htfac, 15, 4, ang)
!
 9001 format(a)
!
      return
      end subroutine splt_cabl



      subroutine splt_ckh(trchedr, filhedr, incdec, fhedr, match, bias, &
         exact_arg)
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: incdec
      integer , intent(out) :: match
      double precision  :: trchedr
      real  :: filhedr
      real , intent(in) :: fhedr
      real  :: bias
      logical ,optional, intent(in) :: exact_arg
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: kdec   
      logical :: exact,almost_equal

      save exact, kdec
!-----------------------------------------------
!
!
!            compares header of trace to header of other
!             data to determine if other header is close enough
!             match to the trace header
!
!
!         trchedr = trace header
!         filhedr = header to compare to trace header
!         incdec  = integer flag to indicate if header values are
!                   incrementing or decrementing
!                      1 = incrementing
!                      0 = decrementing
!         fhedr   = header of the first trace
!         match   = integer value returned to indicate if this is an
!                   appropriate match.
!                      0   = does not match
!                      1   = matches
!                     -1   = skip this file entry and read next
!         bias    = bias to use in determining equality
!
      data kdec/ 0/
!
      if(present(exact_arg))then
        exact = exact_arg
      else
        exact=.false.
      endif
      match = 0
      almost_equal = ameq(trchedr,filhedr,bias)
      if (almost_equal) then
         match = 1
         return
      endif
!
      if (incdec /= kdec) then
         if (filhedr < fhedr) then
            match = -1
            return
         endif
!
         if (trchedr < filhedr) return
         if (trchedr > filhedr) then
            if (exact) then
               match = -1
               return
            endif
            write(print_lun,*) ' FILE HEADER ', filhedr, &
                               ' NOT FOUND.  HEADER ', &
                                 trchedr, ' USED.'
            match = 1
            return
         endif
      endif
!

!
      if (filhedr > fhedr) then
         match = -1
         return
      endif
!
      if (trchedr > filhedr) return
      if (trchedr < filhedr) write(print_lun,*) ' FILE HEADER ', filhedr, &
         ' NOT FOUND.  HEADER ', trchedr, ' USED.'
      match = 1
      return
      end subroutine splt_ckh

      subroutine splt_ct(iun,big,cam,ct,fmed, &
                         trbuf,nsamp,meth_cct)
!
!     purpose:
!     calculate a ct value which causes the median amplitude
!     to be plotted with a specified number of plot channels.
!
!     method:
!     read the byte file created by splt, unpack the 8 bit
!     integer values, re-scale, bin the results,
!     determine the median, calculate the ct.
!     the byte file 'IUN' is not changed. this subroutine uses
!     the method developed by c.i. burch in cyber program ctds.
!
!     parameters:
!     iun     = unit number of the byte file
!     big     = lav from all traces being plotted
!     cam     = user specified factor in ct calculation
!     ct      = returned -- calculated ct
!     fmed    = returned -- scaling factor for possible
!               use with color plotting. ex: tr(i)/fmed
!     ibuf    = integer buffer to unpack nsamp values
!     ipdf    = integer buffer for 1001 bins
!     trbuf   = trace buffer to re-scale and float the trace
!     nsamp   = number of trace samples
!     meth_cct= Method to use for calculated ct factor
!               MED  = cam*lav/median
!               NOMED= cam*lav
!----------------------------------------------------------------------
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iun
      integer , intent(in) :: nsamp
      real , intent(in) :: big
      real , intent(in) :: cam
      real , intent(out) :: ct
      real , intent(out) :: fmed
      real , intent(inout) :: trbuf(*)
      character(len=*) :: meth_cct
!-----------------------------------------------
!   l o c a l   p a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: npc = 8
      integer, parameter :: numbin = 1001
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer , dimension(npc) :: ipcnt   
      integer :: ipdf(1001),ibuf(nsamp)
      integer :: ict, itot, i, j, ibin, k, num, nfind,npwrds,istat,mask1
      real , dimension(npc) :: amp
      real :: fctr, trmax, strmax

      data mask1/Z'FFFFFFF0'/

      save

!-----------------------------------------------
!
      if(meth_cct.eq.'NOMED')then
         ct = abs(cam)*big
         ict = nint(ct*10.0)
         ct = float(ict)/10.0
         write(print_lun,*) ' SPLT=> CT HAS BEEN SET TO ', ct
         go to 8000
      endif
      itot = 0
      fctr = (float(numbin) - 1.0)/big
      ipdf(:numbin) = 0
!
      rewind iun
!       read a trace from the byte file
   20 continue
      read (iun, end=80) npwrds
      read (iun, end=80) trmax, (ibuf(j),j=1,npwrds)
!       unpack (8 values per word)
!!!      call unpack (kbuf, 8, ibuf, nsamp)
        istat=wrdc_unpack(ibuf,nsamp,1,4,swap)
!       de-code and re-scale
      strmax = trmax/127.0
      do i = 1, nsamp
         if (ibuf(i) == 255) then
            ibuf(i) = -1
         else if (ibuf(i) > 127) then
            ibuf(i) = ior(ibuf(i),mask1)
         endif
         trbuf(i) = ibuf(i)*strmax
      end do
!
!     fill pdf bins
      do i = 1, nsamp
         if (trbuf(i) == 0.) cycle
         ibin = abs(trbuf(i))*fctr + 1.0
         ipdf(ibin) = ipdf(ibin) + 1
         itot = itot + 1
      end do
      go to 20
!
!     end of input data
   80 continue
      ipcnt(1) = 50
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
      nfind = float(itot)*float(ipcnt(1))*0.01
      do ibin = 1, numbin
         num = num + ipdf(ibin)
  110    continue
         if (num < nfind) cycle
         amp(k) = (float(ibin) - 0.5)/fctr
         if (k >= npc) exit
         k = k + 1
         nfind = float(itot)*float(ipcnt(k))*0.01
         go to 110
      end do
!
      fmed = amp(1)
!     calculate ct
      ct = cam*big/fmed
      ict = nint(ct*10.0)
      ct = float(ict)/10.0
!
!     median scale the %ile amplitudes
      amp(:npc) = amp(:npc)/fmed
!
!     print distribution diagnostics
      write(print_lun,*) '          FROM SPLT '
      write(print_lun,*) '   CT HAS BEEN SET TO ', ct
      write(print_lun,*) '   MEDIAN SCALED PERCENTILE AMPLITUDES '
      write (print_lun, 1000) (ipcnt(i),i=1,npc)
 1000 format(5x,'PERCENTILES',8i8)
      write (print_lun, 1010) (amp(i),i=1,npc)
 1010 format(5x,'AMPLITUDES  ',8f8.4)
      write(print_lun,*) '     NON ZERO SAMPLES= ', itot
      write (print_lun, 1020) fmed
 1020 format(5x,'INPUT DATA MEDIAN WAS ',g14.6)
!
      rewind iun
 8000 continue
      return
      end subroutine splt_ct


      subroutine splt_dubi(yc, xc, ang,iunit)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      real , intent(in) :: yc
      real , intent(in) :: xc
      real , intent(in) :: ang

      integer,intent(in) :: iunit
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer ::             npts, i   
      real , dimension(15) :: x, y
      real :: scal, size, xx, a1, yy, r1, r2


      character(len=80) :: dublogo(594)

      integer :: ndx=0

      save

!-----------------------------------------------

data dublogo(1  )/'         4          TOP OF CAPSULE'/
data dublogo(2  )/'   11.0      21.0'/
data dublogo(3  )/'   11.0      19.0'/
data dublogo(4  )/'   54.0      19.0'/
data dublogo(5  )/'   54.0      21.0'/
data dublogo(6  )/'      4          BOTTOM OF CAPSULE '/
data dublogo(7  )/'   11.0       3.0'/
data dublogo(8  )/'   11.0       1.0'/
data dublogo(9  )/'   54.0       1.0'/
data dublogo(10 )/'   54.0       3.0'/
data dublogo(11 )/'      8          FIRST LETTER UPPER TOP HALF--POLY1'/
data dublogo(12 )/'   10.0      14.5'/
data dublogo(13 )/'    9.9      14.8'/
data dublogo(14 )/'    9.6      15.0'/
data dublogo(15 )/'    9.0      15.0'/
data dublogo(16 )/'    9.0     13.75'/
data dublogo(17 )/'    9.5      13.8'/
data dublogo(18 )/'    9.7      13.9'/
data dublogo(19 )/'    9.9      14.2'/
data dublogo(20 )/'      5          FIRST CHARACHER UPPER BOTTOM HALF--POLY2'/
data dublogo(21 )/'    9.0      15.0'/
data dublogo(22 )/'    8.5      14.9'/
data dublogo(23 )/'    7.6      14.4'/
data dublogo(24 )/'    8.5      13.5'/
data dublogo(25 )/'    9.0     13.75'/
data dublogo(26 )/'      7          FIRST CHARACHER UPPER MIDDLE--POLY3 '/
data dublogo(27 )/'    6.7      13.5'/
data dublogo(28 )/'    6.1      11.5'/
data dublogo(29 )/'    7.4      11.5'/
data dublogo(30 )/'    7.5      12.0'/
data dublogo(31 )/'    7.8      12.8'/
data dublogo(32 )/'    8.5      13.5'/
data dublogo(33 )/'    7.6      14.4'/
data dublogo(34 )/'      6          FIRST CHARACTER --POLY5 '/
data dublogo(35 )/'    6.2      10.1'/
data dublogo(36 )/'    6.4       9.2'/
data dublogo(37 )/'    6.7       8.5'/
data dublogo(38 )/'   7.25      8.25'/
data dublogo(39 )/'    7.9       9.4'/
data dublogo(40 )/'    7.5      10.1'/
data dublogo(41 )/'      6          FIRST CHARACTER LOWER MIDDLE--POLY4 '/
data dublogo(42 )/'    6.2      10.1'/
data dublogo(43 )/'    7.5      10.1'/
data dublogo(44 )/'    7.4      11.0'/
data dublogo(45 )/'    7.4      11.5'/
data dublogo(46 )/'    6.1      11.5'/
data dublogo(47 )/'    6.1      11.0'/
data dublogo(48 )/'      5          FIRST CHARACTER BOTTOM LEFT HALF--POLY6 '/
data dublogo(49 )/'   7.25      8.25'/
data dublogo(50 )/'    8.0       8.1'/
data dublogo(51 )/'    8.5       8.0'/
data dublogo(52 )/'    8.5       9.3'/
data dublogo(53 )/'    7.9       9.4'/
data dublogo(54 )/'      5          FIRST HCARACTER BOTTOM RIGHT HALF--POLY7'/
data dublogo(55 )/'    8.5       8.0'/
data dublogo(56 )/'   12.5       8.0'/
data dublogo(57 )/'   11.0       9.5'/
data dublogo(58 )/'   10.0       9.3'/
data dublogo(59 )/'    8.5       9.3'/
data dublogo(60 )/'      7          SECOND CHARACTER LOWER LEFT--POLY2'/
data dublogo(61 )/'   10.5      10.5'/
data dublogo(62 )/'   11.0       9.5'/
data dublogo(63 )/'   12.5       8.0'/
data dublogo(64 )/'   12.7       9.0'/
data dublogo(65 )/'   12.0      10.6'/
data dublogo(66 )/'   11.8      10.7'/
data dublogo(67 )/'   11.7      10.9'/
data dublogo(68 )/'     10          SECOND CHARACTER UPPER LEFT--POLY3'/
data dublogo(69 )/'   11.0      13.0'/
data dublogo(70 )/'   10.6      12.9'/
data dublogo(71 )/'   10.3      12.4'/
data dublogo(72 )/'  10.25     11.75'/
data dublogo(73 )/'   10.3     11.25'/
data dublogo(74 )/'   10.5      10.5'/
data dublogo(75 )/'   11.7      10.9'/
data dublogo(76 )/'   11.5      11.3'/
data dublogo(77 )/'   11.5      11.6'/
data dublogo(78 )/'   11.6      11.8'/
data dublogo(79 )/'     11          SECOND CHARACTER RIGHT UPPER--POLY4 '/
data dublogo(80 )/'   11.0      13.0'/
data dublogo(81 )/'   11.5      13.1'/
data dublogo(82 )/'   11.9      13.1'/
data dublogo(83 )/'   12.5      12.9'/
data dublogo(84 )/'   12.8      12.5'/
data dublogo(85 )/'   13.2      11.8'/
data dublogo(86 )/'   12.2      11.2'/
data dublogo(87 )/'   12.1      11.4'/
data dublogo(88 )/'   11.9      11.7'/
data dublogo(89 )/'   11.8      11.8'/
data dublogo(90 )/'   11.6     11.82'/
data dublogo(91 )/'      8          SECOND CHARACTER LOWER RIGHT--POLY1 '/
data dublogo(92 )/'   12.2      11.2'/
data dublogo(93 )/'   12.3      10.9'/
data dublogo(94 )/'   12.3      10.7'/
data dublogo(95 )/'   12.2      10.6'/
data dublogo(96 )/'   12.0      10.6'/
data dublogo(97 )/'   12.7       9.0'/
data dublogo(98 )/'   12.9       9.4'/
data dublogo(99 )/'   13.2      11.8'/
data dublogo(100)/'     12          THIRD CHARACTER LEFT--POLY1 '/
data dublogo(101)/'   12.9       9.4'/
data dublogo(102)/'   13.2       9.3'/
data dublogo(103)/'   13.4       9.0'/
data dublogo(104)/'   13.6       8.3'/
data dublogo(105)/'   13.8       8.1'/
data dublogo(106)/'   14.0       8.0'/
data dublogo(107)/'   16.0       8.0'/
data dublogo(108)/'   15.4       9.0'/
data dublogo(109)/'   14.6       9.0'/
data dublogo(110)/'   14.5       9.1'/
data dublogo(111)/'   14.3       9.4'/
data dublogo(112)/'   13.2      11.8'/
data dublogo(113)/'      9          THIRD CHARACTER RIGHT--POLY2'/
data dublogo(114)/'   15.4       9.0'/
data dublogo(115)/'   15.6       9.2'/
data dublogo(116)/'  15.75       9.4'/
data dublogo(117)/'  15.75      11.0'/
data dublogo(118)/'   16.8      11.0'/
data dublogo(119)/'  16.75      10.4'/
data dublogo(120)/'  16.75       9.0'/
data dublogo(121)/'   16.5       8.5'/
data dublogo(122)/'   16.0       8.0'/
data dublogo(123)/'     10          THIRD CHARACTER RIGHT --POLY3 '/
data dublogo(124)/'  15.75      11.0'/
data dublogo(125)/'   15.7      11.2'/
data dublogo(126)/'   15.6      11.3'/
data dublogo(127)/'   15.5      11.5'/
data dublogo(128)/'   15.5      11.7'/
data dublogo(129)/'   15.6      11.8'/
data dublogo(130)/'   16.9      11.8'/
data dublogo(131)/'   17.0      11.7'/
data dublogo(132)/'   16.9      11.3'/
data dublogo(133)/'   16.8      11.0'/
data dublogo(134)/'     10          THRID CHARACTER TAIL'/
data dublogo(135)/'   16.0       8.0'/
data dublogo(136)/'  15.75       7.5'/
data dublogo(137)/'   15.6      7.25'/
data dublogo(138)/'  15.75       7.0'/
data dublogo(139)/'   16.0       6.5'/
data dublogo(140)/'   16.0       6.0'/
data dublogo(141)/'   14.5       6.0'/
data dublogo(142)/'   14.6       6.0'/
data dublogo(143)/'   15.0       7.0'/
data dublogo(144)/'   15.0       8.0'/
data dublogo(145)/'     11          FOURTH CHARACTER UPPER--POLY1 '/
data dublogo(146)/'   18.7      11.5'/
data dublogo(147)/'   20.0      11.3'/
data dublogo(148)/'   21.0      11.0'/
data dublogo(149)/'   21.6      10.7'/
data dublogo(150)/'   22.0     10.25'/
data dublogo(151)/'   22.5      9.25'/
data dublogo(152)/'   21.0       9.4'/
data dublogo(153)/'   21.0       9.6'/
data dublogo(154)/'   20.9       9.8'/
data dublogo(155)/'   20.7       9.9'/
data dublogo(156)/'   18.5       9.9'/
data dublogo(157)/'      9          FOURTH CHARACTER LOWER'/
data dublogo(158)/'   18.0      9.25'/
data dublogo(159)/'   17.6       8.0'/
data dublogo(160)/'  22.75       8.0'/
data dublogo(161)/'   22.5      9.25'/
data dublogo(162)/'   21.0       9.5'/
data dublogo(163)/'   20.9       9.2'/
data dublogo(164)/'   20.8       9.1'/
data dublogo(165)/'   20.5       9.0'/
data dublogo(166)/'   19.0       9.0'/
data dublogo(167)/'     10          FIFTH CHARACTER UPPER LEFT--POLY1 '/
data dublogo(168)/'  24.25      10.5'/
data dublogo(169)/'  24.25      14.5'/
data dublogo(170)/'   24.3      14.7'/
data dublogo(171)/'   24.4      14.9'/
data dublogo(172)/'   24.6      15.0'/
data dublogo(173)/'   25.4      15.0'/
data dublogo(174)/'   25.6      14.9'/
data dublogo(175)/'   25.7      14.7'/
data dublogo(176)/'  25.75      14.5'/
data dublogo(177)/'  25.75      11.2'/
data dublogo(178)/'     12          FIFTH CHARACTER LOWER LEFT--POLY2 '/
data dublogo(179)/'  24.25      10.5'/
data dublogo(180)/'   24.0       9.5'/
data dublogo(181)/'  23.75       9.0'/
data dublogo(182)/'   23.0       8.0'/
data dublogo(183)/'   26.1       8.0'/
data dublogo(184)/'   25.8       9.0'/
data dublogo(185)/'   25.5       9.1'/
data dublogo(186)/'   25.4       9.2'/
data dublogo(187)/'   25.3       9.4'/
data dublogo(188)/'   25.3       9.6'/
data dublogo(189)/'   25.4       9.8'/
data dublogo(190)/'   25.5       9.9'/
data dublogo(191)/'     12          5TH CHARACTER RIGHT TOP POLY5 '/
data dublogo(192)/'  25.75      11.2'/
data dublogo(193)/'   28.0      11.4'/
data dublogo(194)/'   29.5      11.5'/
data dublogo(195)/'   29.8      11.3'/
data dublogo(196)/'   30.0      11.0'/
data dublogo(197)/'   30.0      10.5'/
data dublogo(198)/'   28.4       9.8'/
data dublogo(199)/'   28.3       9.9'/
data dublogo(200)/'   28.0      10.0'/
data dublogo(201)/'   25.8      10.0'/
data dublogo(202)/'   25.4       9.9'/
data dublogo(203)/'  24.25      10.5'/
data dublogo(204)/'      7          5TH CHARACTER BOTTOM RIGHT--POLY4 '/
data dublogo(205)/'   29.8       9.5'/
data dublogo(206)/'   30.0       9.3'/
data dublogo(207)/'   30.2       9.3'/
data dublogo(208)/'   31.0       9.3'/
data dublogo(209)/'   31.5       8.0'/
data dublogo(210)/'   28.4       9.2'/
data dublogo(211)/'   28.5       9.4'/
data dublogo(212)/'      7          5TH CHARACTER RIGHT--POLY6'/
data dublogo(213)/'   30.0      10.5'/
data dublogo(214)/'   29.8      10.0'/
data dublogo(215)/'   29.7       9.7'/
data dublogo(216)/'   29.8       9.5'/
data dublogo(217)/'   28.5       9.4'/
data dublogo(218)/'   28.5       9.6'/
data dublogo(219)/'   28.4       9.8'/
data dublogo(220)/'      6          FIFTH CHARACTER BOTTOM--POLY3 '/
data dublogo(221)/'   25.8       9.0'/
data dublogo(222)/'   26.1       8.0'/
data dublogo(223)/'   31.5       8.0'/
data dublogo(224)/'   28.4       9.2'/
data dublogo(225)/'   28.3       9.1'/
data dublogo(226)/'   28.0       9.0'/
data dublogo(227)/'      7          6TH CHARACTER TOP LEFT--POLY1 '/
data dublogo(228)/'   31.0      10.6'/
data dublogo(229)/'   31.1      10.8'/
data dublogo(230)/'   31.3      11.0'/
data dublogo(231)/'   31.5      11.2'/
data dublogo(232)/'   32.4      10.0'/
data dublogo(233)/'   32.3       9.9'/
data dublogo(234)/'   32.2       9.8'/
data dublogo(235)/'      6          6TH CHARACTER LEFT--POLY5 '/
data dublogo(236)/'   31.0       9.3'/
data dublogo(237)/'   31.1       9.5'/
data dublogo(238)/'   31.0       9.8'/
data dublogo(239)/'   31.0      10.6'/
data dublogo(240)/'   32.2       9.8'/
data dublogo(241)/'  32.20      9.40'/
data dublogo(242)/'     11          6TH CHARACTER TOP RIGHT--POLY2'/
data dublogo(243)/'   31.5      11.2'/
data dublogo(244)/'   31.9      11.4'/
data dublogo(245)/'   32.4      11.5'/
data dublogo(246)/'   32.6      11.5'/
data dublogo(247)/'   33.1      11.4'/
data dublogo(248)/'   33.7      11.0'/
data dublogo(249)/'   33.9      10.8'/
data dublogo(250)/'   34.0      10.6'/
data dublogo(251)/'  32.70      9.90'/
data dublogo(252)/'   32.6      10.0'/
data dublogo(253)/'  32.40      10.0'/
data dublogo(254)/'     11          6TH CHARACTER BOTTOM--POLY4 '/
data dublogo(255)/'   31.0       9.3'/
data dublogo(256)/'   31.5       8.0'/
data dublogo(257)/'   35.0       8.0'/
data dublogo(258)/'   35.0       8.9'/
data dublogo(259)/'   34.5       8.9'/
data dublogo(260)/'   34.1       9.0'/
data dublogo(261)/'   34.0       9.5'/
data dublogo(262)/'  32.80      9.40'/
data dublogo(263)/'   32.6       9.3'/
data dublogo(264)/'  32.40      9.30'/
data dublogo(265)/'   32.2       9.4'/
data dublogo(266)/'      5          6TH CHARACTER RIGHT--POLY3'/
data dublogo(267)/'   34.0      10.6'/
data dublogo(268)/'   34.0       9.5'/
data dublogo(269)/'  32.80      9.40'/
data dublogo(270)/'   32.8       9.8'/
data dublogo(271)/'   32.7       9.9'/
data dublogo(272)/'     11          DOT ABOVE 6TH CHARACTER TOP--POLY3'/
data dublogo(273)/'   31.7      15.0'/
data dublogo(274)/'   32.0     15.05'/
data dublogo(275)/'   32.3     15.07'/
data dublogo(276)/'   32.6     15.05'/
data dublogo(277)/'   32.9     15.07'/
data dublogo(278)/'  33.25      15.1'/
data dublogo(279)/'  33.35      14.9'/
data dublogo(280)/'   33.4      14.5'/
data dublogo(281)/'   33.4      14.3'/
data dublogo(282)/'   31.6      14.2'/
data dublogo(283)/'   31.6      14.6'/
data dublogo(284)/'      7          DOT ABOVE 6TH CHARACTER BOTTOM--POLY1 '/
data dublogo(285)/'   31.6      14.2'/
data dublogo(286)/'   31.7     13.71'/
data dublogo(287)/'  31.85      13.7'/
data dublogo(288)/'   32.0     13.75'/
data dublogo(289)/'   32.3     13.76'/
data dublogo(290)/'   32.5     13.75'/
data dublogo(291)/'   32.5      14.3'/
data dublogo(292)/'      8          DOTS ABOVE 6TH CHARACTER BOTTOM--POLY2'/
data dublogo(293)/'   32.5     13.75'/
data dublogo(294)/'   32.7      13.7'/
data dublogo(295)/'   33.0     13.55'/
data dublogo(296)/'  33.25      13.5'/
data dublogo(297)/'   33.3      13.7'/
data dublogo(298)/'  33.35      14.0'/
data dublogo(299)/'   33.4      14.3'/
data dublogo(300)/'   32.5      14.3'/
data dublogo(301)/'     11          7TH CHARACTER UPPER--POLY2'/
data dublogo(302)/'   36.0      11.6'/
data dublogo(303)/'   36.1      11.6'/
data dublogo(304)/'   36.4      11.5'/
data dublogo(305)/'   36.6      11.3'/
data dublogo(306)/'   36.7      11.0'/
data dublogo(307)/'   36.6      10.0'/
data dublogo(308)/'   35.4       9.4'/
data dublogo(309)/'   35.4      10.0'/
data dublogo(310)/'   35.3      10.5'/
data dublogo(311)/'   35.3      11.0'/
data dublogo(312)/'   35.5      11.3'/
data dublogo(313)/'     10          7TH CHARACTER LOWER--POLY1'/
data dublogo(314)/'   36.6      10.0'/
data dublogo(315)/'  36.50       9.5'/
data dublogo(316)/'   36.4       8.3'/
data dublogo(317)/'   36.2       8.2'/
data dublogo(318)/'   35.9       8.1'/
data dublogo(319)/'   35.5       8.0'/
data dublogo(320)/'   35.0       8.0'/
data dublogo(321)/'   35.0       8.9'/
data dublogo(322)/'   35.3       9.1'/
data dublogo(323)/'   35.4       9.4'/
data dublogo(324)/'     11          DOT ABOVE 7TH CHARACTER TOP '/
data dublogo(325)/'   35.2      15.0'/
data dublogo(326)/'   35.6      14.9'/
data dublogo(327)/'   36.3      14.9'/
data dublogo(328)/'   36.3      15.0'/
data dublogo(329)/'  36.43      15.0'/
data dublogo(330)/'  36.45     14.82'/
data dublogo(331)/'   36.4      14.6'/
data dublogo(332)/'  36.38      14.5'/
data dublogo(333)/'  35.25      14.4'/
data dublogo(334)/'   35.2      14.6'/
data dublogo(335)/'  35.15     14.85'/
data dublogo(336)/'     12          DOT ABOVE 7TH CHARACTER BOTTOM'/
data dublogo(337)/'  36.38      14.5'/
data dublogo(338)/'   36.4      14.3'/
data dublogo(339)/'   36.4      14.1'/
data dublogo(340)/'  36.43     13.95'/
data dublogo(341)/'  36.45      13.7'/
data dublogo(342)/'  36.23     13.83'/
data dublogo(343)/'   35.8      13.7'/
data dublogo(344)/'   35.6     13.73'/
data dublogo(345)/'   35.4     13.75'/
data dublogo(346)/'  35.28      13.7'/
data dublogo(347)/'  35.25      14.0'/
data dublogo(348)/'  35.25      14.4'/
data dublogo(349)/'      8          8TH CHARACTER LEFT--POLY2 '/
data dublogo(350)/'  39.55      9.35'/
data dublogo(351)/'   37.6       9.8'/
data dublogo(352)/'   37.9      10.3'/
data dublogo(353)/'   38.2      10.6'/
data dublogo(354)/'   39.0      11.1'/
data dublogo(355)/'  39.55      9.95'/
data dublogo(356)/'   39.5      9.80'/
data dublogo(357)/'   39.5      9.60'/
data dublogo(358)/'     11          8TH CHARACTER RIGHT  UPPER--POLY3 '/
data dublogo(359)/'   39.0      11.1'/
data dublogo(360)/'   39.6      11.5'/
data dublogo(361)/'   40.3      11.8'/
data dublogo(362)/'   40.8      12.0'/
data dublogo(363)/'   40.9      12.0'/
data dublogo(364)/'   41.0      11.9'/
data dublogo(365)/'   40.0      9.80'/
data dublogo(366)/'  39.95      9.95'/
data dublogo(367)/'   39.8      10.0'/
data dublogo(368)/'  39.70      10.0'/
data dublogo(369)/'  39.55      9.95'/
data dublogo(370)/'      8          CHARACTER 8 RIGHT--POLY4'/
data dublogo(371)/'   41.0      11.9'/
data dublogo(372)/'   41.0      8.00'/
data dublogo(373)/'  39.55      9.35'/
data dublogo(374)/'   39.6      9.30'/
data dublogo(375)/'   39.7      9.30'/
data dublogo(376)/'  39.95      9.35'/
data dublogo(377)/'   40.0      9.50'/
data dublogo(378)/'   40.0      9.80'/
data dublogo(379)/'      8          8TH CHARACTER LOWER--POLY1'/
data dublogo(380)/'  39.55      9.35'/
data dublogo(381)/'   41.0       8.0'/
data dublogo(382)/'  37.13       8.0'/
data dublogo(383)/'  37.08       8.1'/
data dublogo(384)/'  37.25      8.55'/
data dublogo(385)/'   37.4      9.00'/
data dublogo(386)/'   37.4       9.0'/
data dublogo(387)/'  37.60      9.80'/
data dublogo(388)/'     12          9TH CHARACTER MIDDLE'/
data dublogo(389)/'   43.1      13.0'/
data dublogo(390)/'   43.4      12.7'/
data dublogo(391)/'   43.6      12.5'/
data dublogo(392)/'   43.8      12.2'/
data dublogo(393)/'   44.2      11.6'/
data dublogo(394)/'   44.5      11.0'/
data dublogo(395)/'   44.8      10.5'/
data dublogo(396)/'   45.0      10.0'/
data dublogo(397)/'   46.5      10.1'/
data dublogo(398)/'   46.2      10.6'/
data dublogo(399)/'  45.25      12.5'/
data dublogo(400)/'   45.0      13.0'/
data dublogo(401)/'     10          9TH CHARACTER TOP--POLY1'/
data dublogo(402)/'   43.1      13.0'/
data dublogo(403)/'   42.9      13.3'/
data dublogo(404)/'   43.5      15.0'/
data dublogo(405)/'   46.8      15.0'/
data dublogo(406)/'   47.0      14.8'/
data dublogo(407)/'   47.0      13.9'/
data dublogo(408)/'   46.8     13.75'/
data dublogo(409)/'   45.2     13.75'/
data dublogo(410)/'   44.8      13.5'/
data dublogo(411)/'   45.0      13.0'/
data dublogo(412)/'     11          9TH CHARACTER BOTTOM'/
data dublogo(413)/'   41.0      9.25'/
data dublogo(414)/'   44.0      9.25'/
data dublogo(415)/'   45.1       9.5'/
data dublogo(416)/'   45.0      10.0'/
data dublogo(417)/'   46.5      10.1'/
data dublogo(418)/'   46.7       9.5'/
data dublogo(419)/'   46.9       9.0'/
data dublogo(420)/'   47.0       8.5'/
data dublogo(421)/'   46.8      8.25'/
data dublogo(422)/'   46.5       8.0'/
data dublogo(423)/'   41.0       8.0'/
data dublogo(424)/'     11          10TH CHARACTER BOTTOM TAIL'/
data dublogo(425)/'   46.5       7.2'/
data dublogo(426)/'   46.3       6.9'/
data dublogo(427)/'   46.1       6.6'/
data dublogo(428)/'   46.0       6.3'/
data dublogo(429)/'   46.0       6.0'/
data dublogo(430)/'   49.0       6.0'/
data dublogo(431)/'   49.3       6.2'/
data dublogo(432)/'   49.5       6.5'/
data dublogo(433)/'   49.7       6.8'/
data dublogo(434)/'   49.9       7.1'/
data dublogo(435)/'   48.3       7.2'/
data dublogo(436)/'     12          10TH CHARACTER LEFT '/
data dublogo(437)/'   48.3       7.2'/
data dublogo(438)/'   48.5       7.3'/
data dublogo(439)/'   48.6       7.5'/
data dublogo(440)/'   48.7       7.7'/
data dublogo(441)/'   48.7      10.8'/
data dublogo(442)/'   48.8      11.0'/
data dublogo(443)/'   49.9      11.0'/
data dublogo(444)/'   50.0      10.8'/
data dublogo(445)/'   50.0       9.5'/
data dublogo(446)/'   50.0       8.1'/
data dublogo(447)/'   50.0       7.5'/
data dublogo(448)/'   49.9       7.1'/
data dublogo(449)/'     12          10TH CHARACTER RIGHT'/
data dublogo(450)/'   50.2       8.0'/
data dublogo(451)/'   50.0       8.1'/
data dublogo(452)/'   50.0       9.5'/
data dublogo(453)/'   50.2       9.3'/
data dublogo(454)/'   51.1       9.3'/
data dublogo(455)/'   51.3       9.5'/
data dublogo(456)/'   51.3      10.8'/
data dublogo(457)/'   51.4      11.0'/
data dublogo(458)/'   52.5      11.0'/
data dublogo(459)/'   52.6      10.8'/
data dublogo(460)/'   52.6       9.5'/
data dublogo(461)/'   52.6       8.0'/
data dublogo(462)/'     12          11TH CHARACTER LEFT '/
data dublogo(463)/'   52.6       9.5'/
data dublogo(464)/'   53.2       9.3'/
data dublogo(465)/'   53.7       9.3'/
data dublogo(466)/'   53.9       9.5'/
data dublogo(467)/'   53.9      10.8'/
data dublogo(468)/'   54.1      11.0'/
data dublogo(469)/'   55.0      11.0'/
data dublogo(470)/'   55.2      10.8'/
data dublogo(471)/'   55.2       9.5'/
data dublogo(472)/'   55.4       9.3'/
data dublogo(473)/'   55.4       8.0'/
data dublogo(474)/'   52.6       8.0'/
data dublogo(475)/'      9          11TH CHARACTER TOP RIGHT'/
data dublogo(476)/'   55.4       9.3'/
data dublogo(477)/'   56.3       9.3'/
data dublogo(478)/'   56.5       9.5'/
data dublogo(479)/'   56.5      10.8'/
data dublogo(480)/'   56.7      11.0'/
data dublogo(481)/'   57.6      11.0'/
data dublogo(482)/'   57.8      10.8'/
data dublogo(483)/'   57.8       9.2'/
data dublogo(484)/'   57.7       8.7'/
data dublogo(485)/'      7          11TH CHARACTER BOTTOM RIGHT '/
data dublogo(486)/'   55.4       9.3'/
data dublogo(487)/'   57.7       8.7'/
data dublogo(488)/'   57.5       8.5'/
data dublogo(489)/'   57.2       8.2'/
data dublogo(490)/'   57.0       8.1'/
data dublogo(491)/'   56.6       8.0'/
data dublogo(492)/'   55.4       8.0'/
data dublogo(493)/'      8          DOTS ABOVE CHARACTER 8 LEFT TOP '/
data dublogo(494)/'   38.0      15.0'/
data dublogo(495)/'   38.3     15.07'/
data dublogo(496)/'   38.5     15.08'/
data dublogo(497)/'   38.6     15.09'/
data dublogo(498)/'   38.5      14.5'/
data dublogo(499)/'  37.95     14.45'/
data dublogo(500)/'   38.9      14.0'/
data dublogo(501)/'   38.0      14.0'/
data dublogo(502)/'      7          DOTS ABOVE CHAR 8 LEFT BOTTOM--POLY1'/
data dublogo(503)/'   38.5      14.5'/
data dublogo(504)/'   38.4      13.9'/
data dublogo(505)/'   38.2      13.9'/
data dublogo(506)/'   38.0      14.0'/
data dublogo(507)/'  37.89     14.08'/
data dublogo(508)/'   37.9      14.2'/
data dublogo(509)/'  37.95     14.45'/
data dublogo(510)/'      9          DOTS ABOVE CHARACTER 8  MIDDLE RIGHT'/
data dublogo(511)/'   39.6      14.8'/
data dublogo(512)/'   40.0      15.0'/
data dublogo(513)/'  40.15      15.1'/
data dublogo(514)/'   40.3      15.1'/
data dublogo(515)/'   40.5     15.08'/
data dublogo(516)/'   40.5      13.9'/
data dublogo(517)/'   40.2      14.0'/
data dublogo(518)/'   40.0      14.0'/
data dublogo(519)/'   39.6      14.2'/
data dublogo(520)/'     11          DOTS ABOVE CHARACTER 8  MIDDLE LEFT '/
data dublogo(521)/'   38.6     15.09'/
data dublogo(522)/'   38.8     15.05'/
data dublogo(523)/'   39.0     15.02'/
data dublogo(524)/'   39.4      14.8'/
data dublogo(525)/'   39.6      14.8'/
data dublogo(526)/'   39.6      14.2'/
data dublogo(527)/'   39.4      14.2'/
data dublogo(528)/'   39.0      14.0'/
data dublogo(529)/'  38.85     13.93'/
data dublogo(530)/'   38.7     13.93'/
data dublogo(531)/'   38.4      13.9'/
data dublogo(532)/'      7          DOTS ABOVE CHARACTER 8  RIGHT '/
data dublogo(533)/'   40.5     15.08'/
data dublogo(534)/'   41.0      15.0'/
data dublogo(535)/'   41.1      14.9'/
data dublogo(536)/'   41.1      14.6'/
data dublogo(537)/'   41.0      14.0'/
data dublogo(538)/'   40.8      13.9'/
data dublogo(539)/'   40.5      13.9'/
data dublogo(540)/'     10          DOTS ABOVE LAST CHARACTER  LEFT '/
data dublogo(541)/'   53.1      12.2'/
data dublogo(542)/'   52.6      13.6'/
data dublogo(543)/'   52.7     13.75'/
data dublogo(544)/'   53.5      13.7'/
data dublogo(545)/'   54.7      12.5'/
data dublogo(546)/'   54.7      12.3'/
data dublogo(547)/'   54.6      12.2'/
data dublogo(548)/'   54.3     12.25'/
data dublogo(549)/'   54.0     12.25'/
data dublogo(550)/'   53.5      12.3'/
data dublogo(551)/'     12          DOTS ABOVE LAST CHARACTER TOP LEFT'/
data dublogo(552)/'   53.5     13.72'/
data dublogo(553)/'   53.7      13.8'/
data dublogo(554)/'   53.7      14.0'/
data dublogo(555)/'   53.5      14.8'/
data dublogo(556)/'  53.45     14.95'/
data dublogo(557)/'   53.3      15.0'/
data dublogo(558)/'   53.1      15.0'/
data dublogo(559)/'   53.1      15.2'/
data dublogo(560)/'   53.2      15.3'/
data dublogo(561)/'   54.4      13.6'/
data dublogo(562)/'   54.4      13.4'/
data dublogo(563)/'   54.7      12.5'/
data dublogo(564)/'     11          DOTS ABOVE LAST CHARACTER - TOP LEFT'/
data dublogo(565)/'   54.4      13.6'/
data dublogo(566)/'   54.5      13.7'/
data dublogo(567)/'   54.8      13.8'/
data dublogo(568)/'   55.0     13.85'/
data dublogo(569)/'   55.2      13.8'/
data dublogo(570)/'   55.4      13.8'/
data dublogo(571)/'   55.5     13.75'/
data dublogo(572)/'   55.6      14.1'/
data dublogo(573)/'  55.05      15.2'/
data dublogo(574)/'   54.9      15.3'/
data dublogo(575)/'   53.2      15.3'/
data dublogo(576)/'      8       DOTS ABOVE LAST CHARACTER LEFT PART OF RIGHT'/
!                                 OF RIGHT DOT'/
data dublogo(577)/'   55.6      14.1'/
data dublogo(578)/'   55.7      14.0'/
data dublogo(579)/'   57.0      14.0'/
data dublogo(580)/'   57.1      13.9'/
data dublogo(581)/'   56.0      12.9'/
data dublogo(582)/'   55.7      13.5'/
data dublogo(583)/'   55.6     13.65'/
data dublogo(584)/'   55.5     13.75'/
data dublogo(585)/'      9       DOTS ABOVE LAST CHARACTER RIGHT SIDE OF RIGHT'/
!                                 SIDE OF DOT'/
data dublogo(586)/'   57.1      13.9'/
data dublogo(587)/'   57.2      13.7'/
data dublogo(588)/'   57.8      12.4'/
data dublogo(589)/'   57.7      12.3'/
data dublogo(590)/'   57.4      12.3'/
data dublogo(591)/'   57.0     12.45'/
data dublogo(592)/'   56.6      12.3'/
data dublogo(593)/'   56.3      12.3'/
data dublogo(594)/'   56.0      12.9'/
!
!
!
      scal = 200.0
      size = 0.037
!
!
!
!
   40 continue
      ndx=ndx+1
      read (dublogo(ndx), 9002) npts
      if (ang == 0.0) then
         do i = 1, npts
            ndx=ndx+1
            read (dublogo(ndx), 9001) x(i), y(i)
            x(i) = x(i)*scal*size + xc
            y(i) = y(i)*scal*size + yc
         end do
      else
         do i = 1, npts
            ndx=ndx+1
            read (dublogo(ndx), 9001) x(i), y(i)
            x(i) = xc - x(i)*scal*size
            y(i) = y(i)*scal*size + yc
         end do
      endif
      call cpsplot_xpoly (iunit,x, y, npts, 15)
      if(ndx.lt.594)go to 40
!
      if (ang == 0.0) then
         xx = 11.0*scal*size + xc
         a1 = 90.0
      else
         xx = xc - 11.0*scal*size
         a1 = 270.0
      endif
      yy = 11.0*scal*size + yc
      r1 = 10.*scal*size
      r2 = 8.*scal*size
!
      call cpsplot_xsector (iunit,xx, yy, r1, r2, a1, 180.0, 1.0, 15)
!
      if (ang == 0.0) then
         xx = 54.0*scal*size + xc
         a1 = 270.0
      else
         xx = xc - 54.0*scal*size
         a1 = 90.0
      endif
      call cpsplot_xsector (iunit,xx, yy, r1, r2, a1, 180.0, 1.0, 15)
!
!
      return
!
!
 9001 format(bz,2f10.0)
 9002 format(bz,i10)
      return
!
      end subroutine splt_dubi


      subroutine splt_eleg (x,y,dlu100,plu100,flst,alpha,ndots,&
     &                     idev,iunit)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*),intent(in) :: idev,ndots
      integer , intent(in) :: iunit
      real  :: x
      real , intent(in) :: y
      real  :: dlu100
      real  :: plu100
      integer  :: flst,k
      real  :: alpha
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: nc
      real :: units, y1, high, wid, thi, space       

      character(len=3) :: idash='(-)',iplus='(+)'
      character(len=8) :: lab1='SCALE:  ',lab2='100 MS  ',lab3='EQUALS  '
      character(len=8) :: cflst,cu100  
      character(len=16) :: lab4='     FT.        '
      character(len=16) :: lab8='     FOLD       '
!
! this routine plots a legend explaining elevation and fold of stack
!  plots
!   x      =  the x coordinate
!   y      =  the y coordinate
!  dlu100  =  units representing 100 mils for dash line plot
!  plu100  =  units representing 100 mils for plus sign plot
!  flst    =  fold of stack scale
!  alpha   =  angle to plot annotation
!  ndots   =  plot resolution
! idev     =  device to plot on
      save
      units=200.0
      if(survey_units.eq.'METERS')then
        lab4(6:6)=lab2(5:5)
        lab4(7:8)=lab2(7:8)
      endif
      y1   =  y
      high   =  0.09
      wid    =  high * 0.7
      thi    =  high * 0.13
      call string_cc2ii(ndots,k)
      if(k.ge.400)thi=high*0.18
      space=high*.85
      call string_ii2cc(flst,cflst)
      lab8(1:3)=cflst(1:3)
      call string_ff2cc(dlu100,cu100)
      lab4(1:4)=cu100(1:4)
      if((dlu100.gt.0.0).or.((dlu100.gt.0.0.and.plu100 &
     &.gt.0.0).or.flst.gt.0))then
          call cpsplot_xsymbol (iunit,x,y1,lab1,6,high,15,2,alpha,wid)
          y1   = y1  - (2 * high) * units
          call cpsplot_xsymbol (iunit,x,y1,lab2,8,high,15,2,alpha)
          y1   =  y1 - (2 * high) * units
          call cpsplot_xsymbol (iunit,x,y1,lab3,6,high,15,2,alpha)
          y1   =  y1 - (2 * high) * units
        endif
      if(dlu100.eq.0.0)go to 70
      lab4(10:12)=idash(1:3)
      nc=12
   70 if((dlu100.gt.0.0).or.(dlu100.gt.0.0.and.plu100.gt. &
     &0.0))then
        call cpsplot_xsymbol (iunit,x,y1,lab4,nc,high,15,2,alpha)
        y1=y1-(2*high)*units
      endif
      if(plu100.gt.0.0)then
        call string_ff2cc(plu100,cu100)
        lab4(1:4)=cu100(1:4)
        lab4(10:12)=iplus(1:3)
        nc=12
        call cpsplot_xsymbol (iunit,x,y1,lab4,nc,high,15,2,alpha)
        y1=y1-(2*high)*units
      endif
!
      if(flst.ne.0)then
        call cpsplot_xsymbol(iunit,x,y1,lab8,9,high,15,2,alpha)
      endif
      return
      end subroutine splt_eleg

      subroutine splt_fkpl(yc, dly, ht, ang,iunit,inp)
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      real  :: yc
      real , intent(in) :: dly
      real , intent(in) :: ht
      real  :: ang
      integer,intent(in) :: iunit,inp

!     iunit = unit number of plot file
!     inp   = unit number of title block file
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: ia, ixft, i ,istat
      real , dimension(4) :: xcor, ycor
      real :: scal, siz1, siz2, units, f, xft, fmax, x1, y1, y2, x0, x2, y, &
              wid1, thk1, yinc, xinc, x, wid2, thk2
      character :: card*80, cxft*10, cfmax*10, cy*10, cx*10, tfsz*20, cblk*10

      save
!-----------------------------------------------
!
!
!
!
!
      data tfsz/ 'TRANSFORM SIZE =    '/
      data cblk/ '          '/
!
      scal = 200.0
      siz1 = 0.09
      siz2 = 0.08
!
      units = scal
      if (ht < 0.1) then
         f = ht/0.1
         units = units*f
      endif
!
!
      read (inp, 9001, end=800) card
!          get coordinates for scale
      ia = 1
      call splt_brc1 (ia, card, cxft)
      call string_cc2ff(cxft,xft,istat)
      if(istat.ne.1)go to 7081
!
      call splt_brc1 (ia, card, cfmax)
      call string_cc2ff(cfmax,fmax,istat)
      if(istat.ne.1)go to 7080
!
!          allow 2 inches for vertical scale
!                3 inches for horizontal scale
      x1 = 3.3*units
      y1 = yc
      y2 = yc - 2.0*units
!
!          vertical scale
      cfmax(5:6) = 'HZ'
      call cpsplot_xsymbol (iunit,x1, y1 + 0.01, cfmax, 6, 0.1, 15, 1, ang)
      call cpsplot_xline (iunit,x1, y1, x1, y2, 0.01, 15)
!
!          horizontal scale
      ixft = xft/2.0 + 0.0000001
      x1 = 1.8*units
      if (ang == 0.0) then
         x0 = x1 + 1.5*units
         x2 = x1 + 3.0*units
      else
         x0 = x1 - 1.5*units
         x2 = x1 - 3.0*units
      endif
      call cpsplot_xline (iunit,x1, y2, x2, y2, 0.0, 15)
      y = y2 - 0.03*units
!
      if (ang == 0.0) then
         x1 = x1 - 0.8*units
      else
         x1 = x1 + 0.8*units
      endif
      call cpsplot_xsymbol(iunit,x1,y,'-.5 CY/TR',9,siz1,15,3,ang,wid1, thk1)
      call cpsplot_xsymbol(iunit,x2, y,'.5 CY/TR',8,siz1,15,3,ang,wid1, thk1)
      call cpsplot_xsymbol(iunit,x0, y, '0', 1, siz1, 15, 4, ang, wid1, thk1)
!
!          determine units to the inch
      yinc = 1.0/(fmax/2.0)
      xinc = 1.0/(ixft/1.5)
!
!          0,0 of fk scale is x0,y2
!
!          read and plot coordinates
  400 continue
      read (inp, 9001, end=800) card
      if (card(1:9) == '  ENDPLOT') go to 800
      ia = 1
      do i = 1, 4
         if (i == 0) go to 8000
         call splt_brc1 (ia, card, cy)
         if (cy == cblk) go to 400
         call string_cc2ff(cy,y,istat)
         if(istat.ne.1)go to 7082
         ycor(i) = y2 + y*yinc*units
!
         call splt_brc1 (ia, card, cx)
         call string_cc2ff(cx,x,istat)
         if(istat.ne.1)go to 7083
         if (ang == 0.0) then
            xcor(i) = x0 + x*xinc*units
         else
            xcor(i) = x0 - x*xinc*units
         endif
      end do
      call cpsplot_xpoly (iunit,xcor, ycor, 4, 15)
      go to 400
!
  800 continue
      yc = y2 - dly
      tfsz(17:20) = cxft(1:4)
      if (ang == 0.0) then
         x1 = x1 + 1.4*units
      else
         x1 = x1 - 1.4*units
      endif
      call cpsplot_xsymbol(iunit,x1,yc,tfsz,20,siz2,15,4,ang,wid2,thk2)
      yc = yc - dly*1.5
      return
!
 7080 continue
      write (print_lun, *) ' CFMAX = ', cfmax, ' FMAX = ', fmax
      go to 8000
 7081 continue
      write (print_lun, *) ' CXFT = ', cxft, ' XFT = ', xft
      go to 8000
 7082 continue
      write (print_lun, *) ' CY = ', cy, ' Y = ', y
      go to 8000
 7083 continue
      write (print_lun, *) ' CX = ', cx, ' X = ', x
 8000 continue
      write (print_lun, *) ' ERROR IN TCON'
      return
!
 9001 format(a)
      return
!
      end subroutine splt_fkpl


      subroutine splt_logo(x,y,sz,ang,itn,iunit)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer,intent(in):: iunit,itn
      real , intent(in) :: x
      real , intent(in) :: y
      real , intent(in) :: sz
      real , intent(in) :: ang
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: is, ie, kbr, i, jbr, js, je, j, k, n
      real , dimension(32) :: rc
      real , dimension(28) :: ro
      real , dimension(11) :: xp, an
      real , dimension(4) :: px, py
      real :: scn, units, xsf, ran, csa, ssa, xg, yg, arc, r1, r2, a2, a1, &
              cl
      real :: xl, flip, xx, yy
      save

!-----------------------------------------------
      data scn/ 43./
      data rc/ 0.,0.,2.5,0.,2.5,1.5,0.,1.5,0.,0.,1.5,0.,1.5,2.5, &
               0.,2.5,0.,0.,1.,0.,1.,1.5,0.,1.5,0.,0.,30.5,0.,30.5,1.5,&
               0.,1.5/
      data ro/ -15.,1.,-15.,-2.5,9.,1.,9.,-2.5,-5.5,-2.5,-2.,-2.5, &
               -9.5,1.0,-9.5,-2.5,2.5,1.0,2.5,-2.5,14.5,1.0,14.5,-2.5, &
               -15.,4.75,-15.,-6.25/
      data an/ 90.,90.,270.,0.,90.,270.,90.,90.,270.,90.,270./
      data xp/ -15.,-9.5,-8.5,-3.0,2.5,3.5,9.0,14.5,15.5,-15.,15.5/
!
!          plots the conoco logo
!
!            numbers in data statements above are based on a 43 inch
!            logo. (the x-distance of the capsule). numbers are scaled
!            based on the sz parameter.
!
!          x    = x-coordinate of center of capsule
!          y    = y-coordinate of center of capsule
!          sz   = length of capsule
!          ang  = angle to plot logo ( 0 or 180)
!          itn  = intensity
!         iunit = unit number of plot file
!
      units = 200.0
      xsf = (sz/scn)*units
      ran = ang*0.01745329
      csa = cos(ran)
      ssa = sin(ran)
!
!
      xg = x
      yg = y
!
      arc = 2./sz
      arc = amin1(2.,arc)
      arc = amax1(1.,arc)
!
      r1 = 2.5*xsf
      r2 = 1.0*xsf
      a2 = 180.
!
      is = 1
      ie = 9
      kbr = 1
!
    9 continue
      do i = is, ie
         a1 = amod(an(i)+ang,360.)
         cl = xp(i)*xsf
         xl = xg + cl*csa
!!!         call splt_arc(iunit,xl/200.0,yg/200.0,r1/200.0,r2/200.0,arc)
         call cpsplot_xsector (iunit,xl, yg, r1, r2, a1, a2, arc, itn)
      end do
!
      select case (kbr)
!
      case default
         r1 = 6.25*xsf
         r2 = 4.75*xsf
         is = 10
         ie = 11
         kbr = 2
         go to 9
!
      case (2)
         jbr = 1
         is = 1
         js = 1
         je = 8
!          need to flip the polygons that go with the 180 degree
!           half circle if this is a rl plot (the n in conoco)
         flip = 1.0
      end select
!
   20    continue
         do j = js, je, 2
!
            do k = 1, 4
               n = 2*(k - 1)
               xx = (ro(j)+rc(is+n))*xsf
               yy = (ro(j+1)+rc(is+n+1))*xsf
               px(k) = xg + xx*csa
               py(k) = yg + yy*csa*flip
            end do
!
            call cpsplot_xpoly (iunit,px, py, 4, itn)
         end do
!
         is = is + 8
         select case (jbr)
!
         case default
            js = 9
            je = 12
            jbr = 2
            if (y < 0.0) flip = -1.0
            go to 20
!
         case (2)
            js = 13
            je = 24
            jbr = 3
            flip = 1.0
            go to 20
!
         case (3)
            js = 25
            je = 28
            jbr = 4
            go to 20
!
         case (4)
            return
         end select
      end subroutine splt_logo
      subroutine splt_logo_conp (iunit,x, y, sz, ang)
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
      integer is, ie, kbr, i, jbr, js, je, j, k, n
      real rc(32), ro(28), xp(11), an(5), px(5), py(5), anrl(5), anlr(5), &
        scn, xsf, csa, ssa, xg, yg, r1, r2, cl, xl, yl, xl2, yl2, xx, yy
      character(len=2) :: lr, irl, lrrl
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
         call splt_arc (iunit,xl, yl, r1, r2, an(i))
      enddo
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
         call cpsplot_tclr (iunit,6)
         call cpsplot_circ (iunit,xl, yl, r1, 0)
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
         call cpsplot_circ (iunit,xl2, yl2, r1, 0)
         px (3) = xl2
         px (4) = xl2
         py (3) = yl2 - r1
         py (4) = yl2 + r1
         call cpsplot_tclr (iunit,9)
         call cpsplot_circ (iunit,xl, yl, r2, 0)
         call cpsplot_circ (iunit,xl2, yl2, r2, 0)
         call cpsplot_xpoly (iunit,px, py, 5, 1)
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
!    Set color to red
         call cpsplot_tclr (iunit,6)
         call cpsplot_xpoly (iunit,px, py, 5, 1)
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
      call cpsplot_tclr (iunit,1)
      go to 6000
 5000 continue
      write(print_lun,*)' ERROR ENCOUNTERED IN COLOR_LOGO_CONP ROUTINE WITH &
                        &CPSPLOT_POLY'
      go to 6000
 5001 continue
      write(print_lun,*)' ERROR ENCOUNTERED IN COLOR_LOGO_CONP WITH COLOR_ARC'
      write(print_lun,*)' COLOR_ARC ARGS - XL = ', xl, ' YL = ', yl,&
                        ' R1 = ', r1,&
                        ' R2 = ', r2, ' ANG = ', an(i) , ' I = ', i
 6000 return
      end subroutine splt_logo_conp

  subroutine splt_plbl(yinc,obj)

!        nplbl = total number of labels
!        plbl  = Array containing the labels, each label is 32 characters
!        prow  = array containing row number for corresponding label
!        pcol  = array containing col number for corresponding label
!        btskp = number of traces to skip before blanks
!        btdo  = number of blanks to insert
!        xo    = X-origin
!        yo    = Y-origin
!        PLCS  = panel label characer size in inches
!        TWID  = width of 1 trace in inches
!        YINC  = Y increment in inches
!        ALPHA = angle to plot annotation
!        iunit = unit number of the plot file

!  dummy arguments

      real   , intent(in) :: yinc
      type(splt_struct),intent(in) :: obj       ! arguments



!   local variables

  real    :: x   ,xinc,xo,y,yo 
  real    :: panwid     ,units=200. 
  integer :: i

  integer :: nc,inten=15,posn=1


  xo=obj%xo+.2
  if(obj%theta > 0.000001)xo=obj%xo-.2
  yo=obj%yo-.2
  panwid=obj%btskp(1)*obj%twid*units
  xinc=(obj%btskp(1)+obj%btdo(1))*obj%twid*units


  do i=1,obj%nplbl

    if(obj%theta > 0.000001)then
      x=(xo+panwid)+(obj%pcol(i)-1)*xinc-obj%twid*units
    else
      x=xo+(obj%pcol(i)-1)*xinc+obj%twid*units
    endif
    y=yo-(obj%prow(i)-1)*yinc*units-(obj%plcs_eng+0.1)*units
    if(obj%plbl(i)(1:1).eq.'.'.and.obj%plbl(i)(2:2).eq.' ')cycle
    nc=len_trim(obj%plbl(i))
    if(nc.eq.0)cycle
    call cpsplot_xsymbol(obj%ian,x,y,obj%plbl(i),nc,obj%plcs_eng,&
                         inten,posn,obj%theta)
  enddo

  end subroutine splt_plbl

      subroutine splt_pmse(obj)

      type(splt_struct),intent(inout) :: obj

      real :: x1,x2,y1,y2,yor,elevht,elevation,feet,eldif,elmin,elmax
      real ::         cxl1,cxl2,siz1=0.075,thk1=0.015  

      integer :: knt=0,i,j,minel,maxel
      rewind obj%elevlun

      knt=0
      x1=0.0
      x2=0.0
      y1=0.0
      y2=0.0

      yor=obj%yo+obj%seda_eng*scal
      if(obj%pmse100.le.0.000001)obj%pmse100=200.0
      elevht=obj%ips_eng/(obj%pmse100*10)

!        Elevation scale info
      i=obj%surmin
      j=obj%surmax
      elmin=i
      elmax=j

!          Use at least .5 inch
      feet= 0.5/elevht
      eldif=elmax-elmin
      if(eldif.lt.feet)elmax=elmax+(feet-eldif)
!
!          Round off elevation scales to next 100
      minel=elmin+0.000001
      maxel=elmax+0.000001
      j=mod(minel,100)
      minel=minel-j
      elmin=minel
      j=mod(maxel,100)
      maxel=maxel+100-j
      y1=yor
      y2=(maxel-minel)*elevht*scal + yor

      call cpsplot_xline(obj%ian,obj%xo,y1,obj%xend,y1,0.01,15)
      call cpsplot_xline(obj%ian,obj%xo,y2,obj%xend,y2,0.01,15)
      call cpsplot_xline(obj%ian,obj%xo,y1,obj%xo,y2,0.01,15)
      call cpsplot_xline(obj%ian,obj%xend,y1,obj%xend,y2,0.01,15)

      if(obj%lrrl.eq.'LR')then
        cxl1 = obj%xo-0.5*scal
        cxl2 = obj%xend+siz1*scal
      else
        cxl1 = obj%xo-siz1*scal
        cxl2 = obj%xend+0.5*scal
      endif
      call cpsplot_xfixno(obj%ian,cxl1,y1, 4,minel,siz1,15,obj%theta,&
                          20,thk1)
      call cpsplot_xfixno(obj%ian,cxl2,y1,-4,minel,siz1,15,obj%theta,&
                          21,thk1)
      call cpsplot_xfixno(obj%ian,cxl1,y2,4,maxel,siz1,15,obj%theta,&
                          22,thk1)
      call cpsplot_xfixno(obj%ian,cxl2,y2,4,maxel,siz1,15,obj%theta,&
                          23,thk1)
      read(obj%elevlun,end=8000)elevation
      knt=knt+1
      y1=(elevation-minel)*elevht*scal + yor
      x1=real(knt)*obj%tunit + obj%xo

      DO
        read(obj%elevlun,end=8000)elevation
        knt=knt+1
        y2=(elevation-minel)*elevht*scal + yor
        x2=real(knt)*obj%tunit + obj%xo
        call cpsplot_xline(obj%ian,x1,y1,x2,y2,.01,15)
        x1=x2
        y1=y2
      ENDDO


8000  continue

      end subroutine splt_pmse

      subroutine splt_randname(name)
!======================================================================
! --- create a file name based on time of day
!
! ---
!
!=====================================================================
      implicit none
      character(len=*) :: name
      character(len=10) :: ctim
      integer :: ihrs,mins,isecs  ,i,j   ,i2  
       call date_and_time(TIME=ctim)
       read(ctim,'(3I2)')ihrs,mins,isecs
       i=(ihrs*3600+mins*60+isecs)*1000
       write(print_lun,*)' IHRS = ',ihrs,' MINS = ',mins,' ISECS = ',isecs
       write(print_lun,*)' JOB NAME BASED ON TIME ',i,' MILLISECONDS'
       DO j=5,1,-1                       !form base-36 time(in ms)
         i2=mod(i,36)
         if(i2.lt.10) then
           name(j:j)=char(i2+48)   !0-9
          else
           name(j:j)=char(i2+55)   !a-z
          endif
         i=i/36
       ENDDO

      return
      end subroutine splt_randname

      subroutine splt_scl(x1,y1,scda,cs_scale,tunit,survey_units,basint,&
                          alpha, nt,ipf1,iunit)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=8) :: survey_units
      integer , intent(in) :: nt
      integer , intent(in) :: ipf1,iunit
      real , intent(in) :: x1
      real , intent(in) :: y1
      real , intent(in) :: scda
      real  :: cs_scale
      real , intent(in) :: tunit
      real , intent(in) :: basint
      real  :: alpha
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: numch, mir
      real :: units=200.0, tscale,fmid,xl1,xl2,sclmid,fcmid,xs1,width,width1&
         , yl1, ys1, with, thick, beta, gama, yl2, a, b
      character(len=8) :: yes='YES',l
      character(len=16) :: labsca

      save yes, units, tscale, numch, fmid, xl1, xl2, sclmid, fcmid, &
         xs1, width, width1, yl1, ys1, with, thick, beta, gama, mir, yl2, l, a&
         , b
!-----------------------------------------------
!
!           this routine plots a scale on the section
!
!   x1     =  the x coordinate of the section
!   y1     =  the y coordinate of the top of the section
!   scda    =  the distance above the section
!   cs_scale  =  size of the characters
!   tunit  =  units per trace
!   survey_units  =  meters or feet
!  basint  =  the basement interval between 2 plotted traces
!   alpha  =  angle to plot annotation
!   nt     =  number of traces in section
!   ipf1   =  print file
!
!

!
      if (survey_units == 'METERS') then
         tscale = 1000.0/basint
         labsca = 'ONE KILOMETER'
         numch = 13
!
      else
!
         tscale = 5280.0/basint
         labsca = 'ONE MILE'
         numch = 8
      endif
!
      fmid = real(nt)/2.0
      xl1 = (fmid - tscale*0.5)*tunit + x1
      xl2 = xl1 + tscale*tunit
      if (xl1 < x1) then
         write (print_lun, *) &
          'SPLT_SCL  => ERROR --- CALCULATED SCALE IS LARGER THAN THE WIDTH &
      &   OF THE PLOT'
         write (print_lun, *) '            BASEMENT INTERVAL = ', basint
         if (survey_units == 'METERS') then
            write (print_lun, *) '            NUMBER OF TRACES PER METER = ',&
                                              tscale
         else
            write (print_lun, *) '            NUMBER OF TRACES PER FOOT  = ',&
                                              tscale
         endif
         write (print_lun, *) '            NO SCALE WILL BE DONE'
         return
      endif
      sclmid = (xl2 - xl1)*0.5 + xl1
!
!          center label on scale
      fcmid = real(numch)/2.0
      xs1 = sclmid - (fcmid*cs_scale)*units + cs_scale*units
      if (alpha /= 0.0) xs1 = sclmid + (fcmid*cs_scale)*units - cs_scale*units
      width = 0.025
      width1 = 0.0125
      yl1 = y1 + scda*units
      ys1 = yl1 + cs_scale*units
      with = cs_scale*0.7
      thick = cs_scale*0.13
      beta = 0.0
      gama = 0.0
      mir = 0
      call cpsplot_xsymbol (iunit,xs1, ys1, labsca, numch, cs_scale,15,2,alpha)
      call cpsplot_xline (iunit,xl1, yl1, xl2, yl1, width, 15, 510)
      yl2 = yl1 + units/35.0
      yl1 = yl1 - units/35.0
      call cpsplot_xline (iunit,xl1, yl1, xl1, yl2, width1, 15, 520)
      call cpsplot_xline (iunit,xl2, yl1, xl2, yl2, width1, 15, 530)
      l = 'SCALE'
      a = (yl1 - y1)/200.0
      b = (ys1 - y1)/200.0 + cs_scale
      write (ipf1, 9001) a, b, l
      return
 9001 format(f6.2,2x,f6.2,2x,a8)
      return
      end subroutine splt_scl

      subroutine splt_sidelabel(x,obj)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------

      real , intent(in) :: x
      type(splt_struct),intent(in) :: obj       ! arguments
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: j, i   
      real :: rowlen, x1, y1, sz

      logical :: almost_equal

      save
!-----------------------------------------------
!
!        plot side labels between data and title block at user
!         provided times
!
!        x           = xo of section if lr plot (obj%theta=0)
!                     xend of section if rl plot (obj%theta=180)
!        obj%yo      = yo of section
!        obj%theta   = angle to plot annotation
!        obj%sltim   = array of times in seconds
!        obj%slbl    = array of labels to annotate - 24 char per label
!        obj%slsz    = array of character sizes
!        obj%row     = the row number for paneled plots
!        obj%nsltim  = the number of entries in obj%sltim,slbl,obj%slsz,&
!                         obj%row
!        obj%sladd   = the length of the longest side label in inches
!        obj%ips_eng = inches per second
!        obj%tstrt   = starting time of data
!        obj%dev     = device to plot on
!
!
      rowlen = (obj%otim + 0.1)*200.0*obj%ips_eng
      call cpsplot_fnt (obj%ian,19)
      almost_equal=ameq(obj%theta,0.0,0.000001)
      if (almost_equal) then
!         lr-plot
         x1 = x - obj%sladd
      else
!         rl-plot
         x1 = x + obj%sladd
      endif
!
      j = 1
      do i = 1, obj%nsltim
         y1 = obj%yo - (obj%sltim(i)-obj%tstrt)*obj%ips_eng*200.0
         y1 = y1 - (obj%slrow(i)-1)*rowlen
         sz = obj%slsz(i)
         almost_equal=ameq(sz,0.0,0.000001)
         if (almost_equal) sz = 0.15
         call cpsplot_xsymbol (obj%ian,x1,y1,obj%slbl(j),24,sz,15,1,&
                               obj%theta)
         j = j + 1
      end do
!
      call cpsplot_fnt (obj%ian,0)
      return
      end subroutine splt_sidelabel


      subroutine splt_sho (x1,y1,y2,ftra,f,ns,ndt,incre,num,csize,&
                          tunit,opt_vl,tmwd,theta,nbts,ibtskp,ibtdo,ibtbtwn,&
                          ibttot,errstat,&
                          ml10,dy10,i10,ibthc,ibtacum,idone,ibtf,need,dev,&
                          ltab,irspp,ntpp,ncol,iunit)

      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*)  :: ns,opt_vl,ltab,irspp,dev
      integer , intent(in) :: ndt
      integer , intent(in) :: incre
      integer , intent(in) :: num
      integer  :: nbts
      integer  :: ml10
      integer  :: i10
      integer , intent(in) :: ibthc,iunit
      integer , intent(inout) :: ibtacum
      integer , intent(inout) :: idone
      integer , intent(in) :: ibtf
      integer , intent(inout) :: need
      integer , intent(in) :: ntpp
      integer , intent(in) :: ncol
      integer , intent(out):: errstat
      real , intent(in) :: x1
      real  :: y1
      real  :: y2
      real , intent(in) :: ftra
      real , intent(inout) :: f
      real  :: csize
      real  :: tunit
      real  :: tmwd
      real  :: theta
      real  :: dy10
      integer  :: ibtskp(*)
      integer  :: ibtdo(*)
      integer  :: ibtbtwn(*)
      integer  :: ibttot(*)
!-----------------------------------------------
!   l o ! a l   v a r i a b l e s
!-----------------------------------------------
      integer ::                     kendpnl,kntcol,&  
                 iz, nsht, nc, nshts, it, m, nchars
      integer :: istat
      real    :: start, pcnt, units,width,cunit,tra,oldt,trlab,xdel,xx,fnc,x
      character(len=8) :: izh,ctemp,lch,npnt
      character(len=1) :: ipz='0'


! this routine plots shot points
!   x1     = the x coordinate of the section
!   y1     = the y coordinate of the top of the section
!   y2     = the y coordinate of the bottom of the section
!   ftra   = the trace to start the shot points on
!   f      = current trace count where header word changes
!   ns     = first shot point number
!   ndt    = the number of traces between shot points
!   incre  = number to increment shot point by
!   num    = the number of shot points to do
!   csize  = the height of the characters in inches
!   tunit  = units per trace
!   opt_vl  = yes--draw a line down section @ shot point labels
!           = tm--draw 10 mil timing marks down section @ shot point
!                  labels
!   tmwd   = width in inches of the opt_vl lines
!   theta  = angle to plot characters
!     nbts = the number of blank trace patterns.
!   ibtskp = number of traces to skip before blanks.
!   ibtdo  = number of blanks to insert.
!   ibttwn = number of traces between blanks.
!   ibttot = total number of sets this pattern
!     ml10 = the number of 10 mil marks to draw
!     dy10 = the distance between 10 mil marks
!      i10 = number of dots in 10 mil timing line
!    ibthc = number of blanks when header changes
!  ibtacum = accumulated blank traces
!    idone = flag to indicate finished with blank trace file
!     ibtf = file containing blank trace locations
!    need  = flag to indicate need to read another blank trace
!    dev   = device to plot on
!    ltab  = label both top and bottom
!    irspp = repeat shot pattern for each panel - yes or no
!    ntpp  = number of traces per panel
!    ncol  = number of columns of panels
!
      save
!
      errstat=0
      kendpnl=ntpp
      kntcol=0
      start=ftra
      pcnt=.85
      units=200.0
      izh=' '
      lch=' '
      ctemp=' '
      ctemp(1:1)=ns(1:1)
      if(ctemp(1:1).eq.'-')go to 100
      call string_cc2ii(ns,iz,istat) ! istat=1 if successful
      if(istat.ne.1)go to 50
!
!          test for 1st character zero
      if(ns(1:1).ne.'0')go to 150
!          if second character is blank - user really wants
!            a zero shot point rather than a zero representing
!            a minus sign
!
      if(ns(2:2).eq.' ')iz=9999
      go to 150
!
!          first character in label is type character
 50   continue
      izh=' '
      izh(1:1)=ns(1:1)
      ctemp=ns
      ns(1:7)=ctemp(2:8)
      ns(8:8)=' '
      call string_cc2ii(ns,nsht,istat) ! istat=1 if OK
      if(istat.ne.1)go to 60
      go to 207
!
!          last character must be type character
 60   continue
      ctemp=ns
      ns(2:8)=ctemp(1:7)
      ns(1:1)=izh(1:1)
      izh=' '
      ctemp=ns
      ns=adjustr(ctemp)
      lch(1:1)=ns(8:8)
      ctemp=ns
      ns=adjustl(ctemp)
      nc=len_trim(ns)
      ns(nc:nc)=' '
      go to 150
!
!
 100  iz=1
 150  continue
      call string_cc2ii(ns,nsht,istat)
 207  continue
      if(istat.ne.1)go to 400
      nshts=nsht
!
      width   =   (units/units) /40.0
      cunit   =   csize * units
 230  continue
      tra = start
!
!
!
      do it = 1,num
        oldt=tra
        trlab=tra
        if(ibthc.ne.0)then
 5150     if (idone.eq.1) go to 5201
          if(need.eq.1)then
            read(ibtf,end=5200)m
            f=real(m)
            need=0
          endif
          if(trlab.ge.f)then
            ibtacum=ibtacum+ibthc
            need=1
            go to 5150
          endif
          go to 5201
 5200     idone=1
          rewind ibtf
 5201     trlab=trlab+real(ibtacum)
        else
          call splt_abt(oldt,trlab,nbts,ibtskp,ibtdo,ibtbtwn,ibttot)
        endif
        xdel = trlab * tunit
        xx=x1+xdel
        call cpsplot_xline (iunit,xx,y1,xx,y1+50.0,width,15,200)
        if(ltab.eq.'YES')then
          call cpsplot_xline (iunit,xx,y2,xx,y2-50.0,width,15,200)
        endif
!
        if(opt_vl.eq.'YES')call cpsplot_xline (iunit,x1+xdel,y1,x1+xdel,y2,&
                                              tmwd,15,201)
        if(opt_vl.eq.'TM')call splt_tmrk(x1+xdel,y1,i10,tunit,ml10,&
                                         dy10,tmwd,iunit)
        nchars = 1
        if (nsht .gt. 9) nchars = 2
        if (nsht .gt. 99) nchars = 3
        if (nsht .gt. 999) nchars = 4
        if (nsht .gt. 9999) nchars = 5
        if(nsht.gt.99999)nchars=6
        if(nsht.lt.0)nchars=2
        if(nsht.lt.-9)nchars=3
        if(nsht.lt.-99)nchars=4
        if(nsht.lt.-999)nchars=5
        if(nsht.lt.-9999)nchars=6
        if(nsht.lt.-99999)nchars=7
        fnc=nchars
        call string_ii2cc(nsht,npnt)
        if (iz.ne.0.and.izh.eq.' ') go to 250
        if(lch.ne.' ')go to 250
        ctemp=npnt
        npnt(2:8)=ctemp(1:7)
        nchars = nchars + 1
        fnc=nchars
        npnt(1:1)=ipz(1:1)
        if(izh.ne.' ')npnt(1:1)=izh(1:1)
 250    continue
        if(lch.ne.' ')then
          npnt(nchars-1:nchars-1)=lch(1:1)
          nchars=nchars+1
          fnc=nchars
        endif
!            move x to center justify label
        if(theta.eq.0.0)x=xx-((fnc/2.0*(pcnt*csize)))*units
        if(theta.ne.0.0)x=xx+((fnc/2.0*(pcnt*csize)))*units
        call cpsplot_xsymbol(iunit,x,y1+60.0,npnt,nchars,csize,15,1,theta)
        if(ltab.eq.'YES')then
          call cpsplot_xsymbol(iunit,x,y2-60.0-csize*200.0,npnt,nchars,&
                               csize,15,1,theta)
        endif
        nsht    =  nsht + incre
        tra = tra + ndt
      enddo
!
!          if paneling - reset istrt to begin pattern over
!            again for each panel
      if(ntpp.gt.0.and.irspp.eq.'YES')then
        kntcol=kntcol+1
        if(kntcol.ge.ncol)go to 375
        start=start+ntpp
        nsht=nshts
        go to 230
      endif

!
!
 375  continue
!
      return
 400  continue
      write(print_lun,*)'TROUBLE WITH string_cc2ii IN SPLT_SHO      '
      errstat=1
      return
      end subroutine splt_sho


      subroutine splt_sid(iunit,x,y,hsid,isid1,isid2,isid3,isid4,nt,ti,ireps,&
                          alpha,ipf1,yo)
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iunit
      integer , intent(in) :: nt
      integer , intent(in) :: ipf1
      real  :: x
      real , intent(in) :: y
      real  :: hsid
      real , intent(in) :: ti
      real  :: alpha
      real , intent(in) :: yo
      character(len=*)  :: isid1
      character(len=*)  :: isid2
      character(len=*)  :: isid3
      character(len=*)  :: isid4
      character(len=*)  :: ireps
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: nonbc, n14, i
      real :: wth, ys, thk, tlen, xx, bigy, smaly, a, b
      character(len=4) :: iyes='YES'

      save iyes, wth,ys, thk, nonbc, tlen, n14, xx, i, bigy, &
         smaly, a, b
!-----------------------------------------------
!  x,y     = coordinate for isid1
!  hsid    = height of letters in inchs
!  sids    = four 28 character fields for section id
!   nt     = number of traces
!   ti     = traces to the inch
!  ireps   = repeat sid every 14.0 inches if yes
!  alpha   = angle to plot characters
      wth = hsid*0.7
      ys = y
      thk = hsid*0.13
      nonbc=len_trim(isid1)
      tlen = nt/ti
      n14 = 1
      if (ireps == iyes) n14 = tlen/14.0 + 0.5
      n14 = max0(1,n14)
      xx = x
      do i = 1, n14
         call cpsplot_xsymbol(iunit,xx,ys,isid1,nonbc,hsid,15,2,alpha,wth,thk)
         xx = xx + 14.0*scal
         bigy = ys/200.0 + hsid/2.0
         smaly = ys
      end do
      ys = ys - (0.15 + hsid)*scal
      nonbc=len_trim(isid2)
      if (isid2 /= ' ') then
         call cpsplot_xsymbol (iunit,x,ys,isid2,nonbc,hsid,15,2,alpha,wth,thk)
         smaly = amin1(ys,smaly)
      endif
!
      ys = ys - (0.15 + hsid)*scal
      nonbc=len_trim(isid3)
      if (isid3 /= ' ') then
         call cpsplot_xsymbol (iunit,x,ys,isid3,nonbc,hsid,15,2,alpha,wth,thk)
         smaly = amin1(ys,smaly)
      endif
!
      ys = ys - (0.15 + hsid)*scal
      nonbc=len_trim(isid4)
      if (isid4 /= ' ') then
        call cpsplot_xsymbol(iunit,x,ys,isid4,nonbc,hsid,15,2,alpha,&
                                     wth,thk)
        smaly = amin1(ys,smaly)
      endif
      a = (smaly - yo)/200.0 - hsid/2.0
      b = bigy - yo/200.0
!!      write (ipf1, 9001) a, b, 'SID'
      return
      return
      end subroutine splt_sid

      subroutine splt_stic(x1,y1,ftra,f,ndt,num,tunit,skp,nbts,ibtskp, &
                           ibtdo,ibtbtwn,ibttot,ibthc,ibtacum,idone,ibtf,&
                           need,iunit)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: ndt,iunit
      integer , intent(in) :: num
      integer  :: nbts
      integer , intent(in) :: ibthc
      integer , intent(inout) :: ibtacum
      integer , intent(inout) :: idone
      integer , intent(in) :: ibtf
      integer , intent(inout) :: need
      real , intent(in) :: x1
      real  :: y1
      real , intent(in) :: ftra
      real , intent(inout) :: f
      real , intent(in) :: tunit
      real , intent(in) :: skp
      integer  :: ibtskp(8)
      integer  :: ibtdo(*)
      integer  :: ibtbtwn(*)
      integer  :: ibttot(*)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: it, m
      real :: units, width, tra, ydel, oldt, trlab, xdel, xx

      save units, width, tra, ydel, it, oldt, trlab, m, xdel, xx
!-----------------------------------------------
! this routine plots tics at intermediate shot points
!   x1     = the x coordinate of the section.
!   y1     = the y coordinate of the top of the section.
!   ftra   = the trace to start the shot points on.
!   f      = current trace count where header word changes.
!   ndt    = the number of traces between shot points.
!   num    = the number of shot points to do.
!   tunit  = units per trace.
!   skp    = the length of the tic line above zero time.
!   nbts   = the number of blank trace patterns.
!   ibtskp = number of traces to skip before blanks.
!   ibtdo  = number of blanks to insert.
!   ibttwn = number of traces between blanks.
!   ibttot = total number of sets this pattern.
!   ibthc  = number of blanks when header changes.
!   ibtacum= accumulated blank traces.
!   idone  = flag to indicate finished with blank trace file.
!   ibtf   = file containing blank trace locations.
!   need   = flag to indicate need to read another blank trace.
!   iunit  = unit number of plot file
!
!
      units = 200.
!
      width = 0.02
      tra = ftra
      ydel = skp*units
!
!
      do it = 1, num
         oldt = tra
         trlab = tra
         if (ibthc /= 0) then
 5150       continue
            if (idone /= 1) then
               if (need == 1) then
                  read (ibtf, end=5200) m
                  f = real(m)
                  need = 0
               endif
               if (trlab >= f) then
                  ibtacum = ibtacum + ibthc
                  need = 1
                  go to 5150
               endif
               go to 5201
 5200          continue
               idone = 1
               rewind ibtf
            endif
 5201       continue
            trlab = trlab + real(ibtacum)
         else
            call splt_abt (oldt, trlab, nbts, ibtskp, ibtdo, ibtbtwn, ibttot)
         endif
         xdel = trlab*tunit
         xx = x1 + xdel
         call cpsplot_xline (iunit,xx, y1, xx, y1 + ydel, width, 15, 200)
         tra = tra + ndt
      end do
      return
      end subroutine splt_stic

      subroutine splt_tblk(x1,y1,isid1,isid2,isid3,isid4,reduce,ang,idev,&
                          pageflg,inp,ht2,iunit)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(out) :: pageflg
      integer, intent(in)   :: inp,iunit
      real  :: x1,ht2
      real , intent(in) :: y1
      real , intent(in) :: reduce
      real  :: ang
      character(len=*)  :: isid1,isid2,isid3,isid4,idev
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer ::            nfield  
      character(len=80):: inf
      integer , dimension(20) :: nca, nci
      character(len=24) :: iussr='CONOCO/TYUMEN GEOLOGY   '
      character(len=24) :: itpc='TEXAS PETROLEUM COMPANY '
      integer :: k,kk,j,ifs,ihalf,knt,nc,nc1,nc2,nc3,i1,i2,n,istat
      real :: sidsz, smht, units, runits, siz1, x, y, xc, yc, ht, pcnt,xe,xs&
         , boxmid, xb, dly, ys, cht, wts, tks, hti, wts2, tks2, sps,xx,yc1, &
         xus, xue, yu, sz, f, xpl, ypl, cablen, xcab, xsm1, ysm1, xsm2, ysm2
      character :: itype*5, icom, ieq, type*10, cabl*10,ctemp*10,its*10,logo&
         *10, icard*80, dubi*10, mcd*80, ownera*80,&
         ownerb*160, ownert*320
      logical right_to_left,almost_equal
      data logo/'LOGO'/,cabl/'CABLE'/, itype/'TYPE='/, icom/','/
      data ieq/'='/, dubi/'DUBI'/
!
      data ownera/'THESE DATA ARE OWNED BY AND ARE A TRADE SECRET OF'/
      data ownerb/'. THE USE OF THESE DATA IS RESTRICTED TO PARTIES HOLD&
     &ing a valid license and is subject to the confidentiality terms of&
     & that license.'/
!
!
!
!    routine to plot title block
!
!***********************************************************************
!   changes made to this routine must also be made in splt_thit         *
!***********************************************************************
!
!    argument
!        x1 = x cordi of upper left corner of title block
!        y1 = y cordi of upper left corder of title block
!       sid1= line 1 of sid
!       sid2= line 2 of sid
!       sid3= line 3 of sid
!       sid4= line 4 of sid
!        ht2= height from second type card from calhite routine
!               used to calculate coordinated for lines between
!               sections
!     reduce= reduction factor for title block
!        ang= angle to plot annotation
!       idev= device to plot on
!    pageflg= flag to indicate an rl plots title block has landed on
!             an hsr page boundary, and thus xmax will need to be
!             increased.
!    inp    = unit number of title block file
!   iunit   = unit number of plot file

!
!
!
      nca=0
      nci=0
      sidsz=.2*reduce
      smht=99999.0
      units=200.0
      write(print_lun,*)' REDUCTION FACTOR FOR TITLE BLOCK = ',reduce
      runits=units*reduce
      almost_equal=ameq(ang,0.0,0.00001)
      if(.not.almost_equal)then
        right_to_left=.true.
      else
        right_to_left=.false.
      endif
!
      siz1=0.01
!
      type=' '
      ctemp=' '
      if(ang.eq.0.0)then
        x = x1 + .8*runits
      else
        x = x1 - .8*runits
      endif
      y = y1
      xc=x
      yc=y
      rewind inp
!          Read through once to print file to online
      write(print_lun,*)'        APPEARANCE OF GENERATED TITLE BLOCK'
      write(print_lun,*)'****************************************************'
      DO
        read(inp,9001,iostat=istat)icard
        if(istat.lt.0)exit
        write(print_lun,*)icard
      ENDDO
      write(print_lun,*)'****************************************************'
      rewind inp
!          find the smallest character height
    5 read(inp,9001,end=8)icard
!          test for type card
      if(icard(:5).ne.itype)go to 5
      ctemp=' '
      kk=index(icard,'HEIGHT=')
      if(kk.eq.0)go to 5
      kk=kk+7
      j=1
    6 ctemp(j:j)=icard(kk:kk)
      j=j+1
      kk=kk+1
      if(icard(kk:kk).ne.icom.and.icard(kk:kk).ne.' ')go to 6
      call string_cc2ff(ctemp,ht,istat)
      if(istat.ne.1)go to 30
      if(ht.le.0.0)ht=0.1
      ht=ht*reduce
      smht=amin1(smht,ht)
      go to 5
    8 rewind inp
!
      pcnt=1.0
      if(smht.lt.0.1.and.smht.gt.0.09)smht=0.09
      if(ang.eq.0.0)then
        xe= x + (71.*ht2*pcnt)*runits
        xs= x - (ht2*pcnt)*runits
        boxmid=xc + (abs(xe-xc))/2.0
      else
        xe= x - (71.*ht2*pcnt)*runits
        xs= x + (ht2*pcnt)*runits
!          if coordinates fall across an hsr page (65535 rasters), +
!           fudge - adjust x so will be all on same page
        if(xe.lt.32767.5.and.xs.gt.32767.5)then
          xb=32767.5-xe + 100.0
          xe=xe+xb
          xs=xs+xb
          xc=xc+xb
          x1=x1+xb
          if(right_to_left)pageflg=1
        endif
        boxmid=xc - (abs(xe-xc))/2.0
      endif
      dly=((1.5*ht2)*runits)*2.
      ifs=0
      ihalf=0
!
!
   10 read(inp,9001,end=720)icard
      knt=knt+1
!
!          test for type card
      if(icard(:5).ne.itype)go to 120
      knt=0
      type=' '
      ctemp=' '
!
!          get type of card & character height
!          ignore old numc parameter
      k=1
      j=6
   12 if(icard(j:j).eq.icom)go to 20
      type(k:k)=icard(j:j)
      if(k.ge.10)then
        ht=.1*reduce
        go to 29
      endif
      k=k+1
      j=j+1
      go to 12
   20 j=j+1
      if(icard(j:j).eq.'H')go to 25
      go to 20
   25 j=j+7
      k=1
   27 ctemp(k:k)=icard(j:j)
      j=j+1
      if(icard(j:j).eq.icom.or.icard(j:j).eq.' ')go to 28
      k=k+1
      go to 27
 28   continue
      call string_cc2ff(ctemp,ht,istat)
      if(istat.ne.1)go to 30
      ht=ht*reduce
      pcnt=1.0
      if(ht.lt.0.1.and.ht.gt.0.09)ht=0.09
   29 dly=1.5*ht*units*2.0
      if(ifs.eq.0)go to 40
      go to 600
   30 write(print_lun,*)' ERROR ON TYPE CARD'
      write(print_lun,*)' CTEMP = ',ctemp,' HT = ',ht
      return
   40 ifs=1
      ys=y
      yc= y - (ht*1.5)*units
      go to 10
  120 its=' '
      its(1:8)=icard(1:8)
      if(its.eq.logo) go to 350
      if(its.eq.dubi)go to 360
      if(its.eq.'USSR')go to 370
      if(its.eq.'TPC')go to 380
      if(its.eq.'  FKPLOT')go to 425
      if(its(1:3).eq.'  #')then
      ihalf=1
      yc=yc+dly*.5
      icard(1:3)=' '
      endif
      if(type.eq.cabl.or.type.eq.'CAB2S3S')go to 400
      if (type.eq.'ID') go to 700
      call splt_brac(icard,mcd,nca,nci,nfield)
      if (its .eq. 'C0N0') go to 150
      if (its .ne. 'CONO')go to 200
  150 continue
      if (its .eq. 'C0N0C0-') go to 450
      if (its .eq. 'CONOCO-') go to 450
  200 continue
      cht = ht
!          knt will be equal to 1 on the first card read after the
!              type card
      if(knt.eq.1.and.type.ne.'HEAD') cht=ht*1.5
      wts=cht*.7
      tks=cht*.13
!          plot all info on card which is not in brackets
      nc=len_trim(mcd)
      if(mcd.ne.' ')then
        call cpsplot_xsymbol(iunit,xc,yc,mcd,nc,cht,15,2,ang)
      endif
      if (type.eq.'OWNER')then
         yc=yc-dly
         go to 690
      endif
      hti = ht*.8
      wts2 = hti*.7
      tks2 = hti*.13
      sps = hti*1.01
      inf=' '
      do 250 k=1,20
        if (nca(k).eq.0) go to 300
        if((nci(k)+nca(k)-1).gt.80)go to 900
        inf(1:nca(k))=icard(nci(k):nci(k)+nca(k)-1)
        if(ang.eq.0.0)then
          xx=xc + (ht* nci(k))* units*pcnt
        else
          xx=xc - (ht* nci(k))* units*pcnt
        endif
        yc1 = yc + .03*runits
!            plot information which is inside brackets
        call cpsplot_xsymbol (iunit,xx,yc1,inf,nca(k),    &
     &                        hti,15,2,ang)
        xus=xx
        if(ang.eq.0.0)then
          xue= xx + (hti*pcnt*nca(k))*units
        else
          xue= xx - (hti*pcnt*nca(k))*units
        endif
        yu= yc - (ht*.52)*units
!            underline information inside brackets
        call cpsplot_xdash(iunit,xus,yu,xue,yu,.07,.02,.005,15)
  250 continue
  300 continue
      yc = yc - dly
      if(ihalf.eq.1)then
        yc=yc+dly*.5
        ihalf=0
      endif
      if (cht .ne. ht) yc = yc - dly*.5
      go to 10
  350 continue
! *** call spltlogo to plot logo
!
      yc= yc - (dly/2.)
      sz=2.3125
      if(ht.lt.0.15)then
        f=ht/.15
        sz=sz*f
      endif
      xpl=boxmid
      call splt_logo_conp(iunit,xpl/200.0,abs(yc/200.0),sz,ang)
      yc= yc - dly*1.5
!
      go to 10
!
  360 continue
      yc=yc-dly*1.4
      sz=2.3125*reduce
      if(ang.eq.0.0)then
        xpl=xc+(6.*ht*pcnt+sz/2.)*units
      else
        xpl=xc-(6.*ht*pcnt+sz/2.)*units
      endif
      call splt_dubi(yc,xpl,ang,istat)
      if(istat.eq.1)go to 10
      yc=yc-dly*.8
      go to 10
  370 continue
      yc=yc-dly*1.4
      sz=0.3*reduce
      if(ang.eq.0.0)then
        xpl=xc+.2*units
      else
        xpl=xc-.2*units
      endif
      ypl=yc+.2*runits
      call cpsplot_xsymbol(iunit,xpl,ypl,iussr,24,sz,15,2,ang)
      yc=yc-dly*.8
      go to 10
  380 continue
      yc=yc-dly*1.4
      sz=0.3*reduce
      if(ang.eq.0.0)then
        xpl=xc+.2*units
      else
        xpl=xc-.2*units
      endif
      ypl=yc+.2*runits
      call cpsplot_xsymbol(iunit,xpl,ypl,itpc,24,sz,15,2,ang)
      yc=yc-dly*.8
      go to 10
  400 continue
!
! *** call cabl to plot cable digram
!
      cht = ht*1.5
      cablen=4.3*runits
      if(ang.eq.0.0)then
!cc     xcab = xc + .95*units
        xcab = boxmid-cablen/2.0
      else
!cc     xcab = xc - .95*units
        xcab = boxmid+cablen/2.0
      endif
      call cpsplot_xsymbol(iunit,xc,yc,icard,70,cht,15,2,ang)
      yc = yc - (dly*1.5 + .5*runits)
      if(type.eq.cabl)then
        call splt_cabl(xcab,yc,nci,nca,ht,ang,idev,inp,iunit)
!!!      else if(type.eq.'CAB2S3S')then
!!!        read(inp,9001,end=720)icard
!!!        call splt_brac(icard,mcd,nca,nci,nfield)
!!!        icard(71:71)=char(0)
!!!        call splt_2s3s(xcab,yc,ang,icard,nca,nci)
      endif
! *** set type so that any card left will be plotted
      type = 'XXX'
      yc = yc - 1.2*runits
      go to 10
!
  425 call splt_fkpl(yc,dly,ht,ang,iunit,inp)
      go to 10
!
  450 continue
!
! *** plot conoco-partner name
!
      yc = yc - (dly/2.)
      call cpsplot_xsymbol(iunit,xc,yc,icard,40,ht*1.5,15,2,ang)
      yc = yc - dly*1.5
      go to 10
  600 continue
!          line between sections
      call cpsplot_xline(iunit,xs,yc+dly*.5,xe,yc+dly*.5,siz1,15)
      go to 10
!
!          plot owner
  690 continue
      read(inp,9001,end=720)icard
      nc1=len_trim(ownera)
      nc2=len_trim(icard)
      nc3=len_trim(ownerb)
      ownert=ownera(1:nc1+1) // icard(1:nc2) // ownerb(1:nc3)
      nc=len_trim(ownert)
      i1=1
      i2=70
  695 continue
!        make sure line ends in a period or space
      if(ownert(i2:i2).ne.'.'.and.ownert(i2:i2).ne.' ')then
        i2=i2-1
        go to 695
      endif
      n=i2-i1+1
!        move to icard so text will be on word boundaries
      icard(1:n)=ownert(i1:i1+n-1)
      call cpsplot_xsymbol(iunit,xc,yc,icard,n,hti,15,2,ang)
      yc = yc - dly*.5
      if(i2.lt.nc)then
        i1=i1+n
        i2=i1+70
        go to 695
      endif
      yc = yc - dly*.5
      go to 10
!
  700 continue
!
!          plot id
      call cpsplot_xsymbol(iunit,xc,yc,icard,70,ht,15,2,ang)
      yc = yc - dly*.5
!
!          box around title block
  720 continue
      call cpsplot_xline(iunit,xs,ys,xs,yc,siz1,15)
      call cpsplot_xline(iunit,xe,ys,xe,yc,siz1,15)
      call cpsplot_xline(iunit,xs,ys,xe,ys,siz1,15)
      call cpsplot_xline(iunit,xs,yc,xe,yc,siz1,15)
      call cpsplot_xline(iunit,x1,ys,xs,ys,siz1,15)
      call cpsplot_xline(iunit,x1,yc,xs,yc,siz1,15)
      call cpsplot_xline(iunit,x1,ys,x1,yc,siz1,15)
      write(print_lun,*)' Actual title block height = ',abs((yc-ys)/200.0)
      write(print_lun,*)' SPLTTBLK-> ystart = ',ys/200.0,' yend = ',yc/200.0
       if(ang.eq.0.0)then
         xsm1 = x1 + .2*runits
       else
         xsm1 = x1 - .2*runits
       endif
       ysm1 = y1 - 11.7*runits
       call cpsplot_xsymbol(iunit,xsm1,ysm1,isid1,28,sidsz,15,2,ang+90.)
      if(ang.eq.0.0)then
        xsm2 = xsm1 + .3*runits
      else
        xsm2 = xsm1 - .3*runits
      endif
      ysm2 = ysm1 + (28*.2*pcnt)*runits
      call cpsplot_xsymbol(iunit,xsm1,ysm2,isid2,28,sidsz,15,2,ang+90.)
      call cpsplot_xsymbol(iunit,xsm2,ysm1,isid3,28,sidsz,15,2,ang+90.)
      call cpsplot_xsymbol(iunit,xsm2,ysm2,isid4,28,sidsz,15,2,ang+90.)
      close(inp)
      return
  900 continue
      write(print_lun,*)' '
      write(print_lun,*)'SPLTTBLK-> Title block abort - The following card '
      write(print_lun,*)'goes beyond 80 characters'
      write(print_lun,*)icard
      call pc_error('SPLT aborted doing title block')
!
 9001 format(a)
      end subroutine splt_tblk

      subroutine splt_thit(tht,inp,ht2)
      implicit none
!
!    calculate height of title block

      integer,intent(in) :: inp
      real,intent(out)   :: tht
      real               :: ht2
!
!    argument
!        tht = height in inches - returned
!        inp = unit number of title block file
!        ht2 = character height - returned
!
      character(len=320):: ownert
      character(len=160):: ownerb
      character(len=80) :: icard,ctemp,ownera,inf
      character(len=10) :: type
      character(len=8)  ::      dubi,logo,cabl,its 
      character(len=5)  :: itype

      character(len=1)  :: icom,ich
      integer :: nca(20),nci(20),kk,j,istat,knt=0,ifs,n,i1,i2,ihalf,k,nc1
      integer :: nc2,nc3,nc
      real    :: y,ys,yc,units,runits,ht,cht,dly,sz,f,reduce,ang,xpl
      real    :: cablen,sidsz,smht,siz1,pcnt,wts,tks,hti,wts2,tks2,sps,yc1
      real    :: yu,ypl,xc,y2
      data logo/'LOGO'/,cabl/'CABLE'/, itype/'TYPE='/, icom/','/
      data dubi/'DUBI'/
      data ownera/'THESE DATA ARE OWNED BY AND ARE A TRADE SECRET OF'/
      data ownerb/'. THE USE OF THESE DATA IS RESTRICTED TO PARTIES HOLD&
     &ing a valid license and is subject to the confidentiality terms of&
     & that license.'/
!
!
      nca=0
      nci=0
      reduce=1.0
      sidsz=.2*reduce
      smht=99999.0
      units=200.0
      runits=units*reduce
!
      siz1=0.01
!
      type=' '
      ctemp=' '
      y = 0.0
      yc=y
      rewind inp
!          find the smallest character height
  5   read(inp,9001,end=8)icard
!          test for type card
      if(icard(:5).ne.itype)go to 5
      ctemp=' '
      kk=index(icard,'HEIGHT=')
      if(kk.eq.0)go to 5
      kk=kk+7
      j=1
 6    ctemp(j:j)=icard(kk:kk)
      j=j+1
      kk=kk+1
      if(icard(kk:kk).ne.icom.and.icard(kk:kk).ne.' ')go to 6
      call string_cc2ff(ctemp,ht,istat)
      if(istat.ne.1)go to 30
      if(ht.le.0.0)ht=0.1
      ht=ht*reduce
      smht=amin1(smht,ht)
      go to 5
 8    rewind inp
!
      pcnt=.9
      if(smht.lt.0.07)pcnt=0.96
      if(smht.lt.0.1.and.smht.gt.0.09)smht=0.09
      dly=((1.5*ht2)*runits)*2.
      ifs=0
      ihalf=0
!
!
 10   read(inp,9001,end=720)icard
      knt=knt+1
!
!          test for type card
      if(icard(:5).ne.itype)go to 120
      knt=0
      type=' '
      ctemp=' '
!
!          get type of card & character height
!          ignore old numc parameter
      k=1
      j=6
 12   if(icard(j:j).eq.icom)go to 20
      type(k:k)=icard(j:j)
      if(k.ge.10)then
        ht=.1*reduce
        go to 29
      endif
      k=k+1
      j=j+1
      go to 12
 20   j=j+1
      if(icard(j:j).eq.'H')go to 25
      go to 20
 25   j=j+7
      k=1
 27   ctemp(k:k)=icard(j:j)
      j=j+1
      if(icard(j:j).eq.icom.or.icard(j:j).eq.' ')go to 28
      k=k+1
      go to 27
 28   continue
      call string_cc2ff(ctemp,ht,istat)
      if(istat.ne.1)go to 30
      ht2=ht
      ht=ht*reduce
      pcnt=0.9
      if(ht.lt.0.07)pcnt=0.96
      if(ht.lt.0.1.and.ht.gt.0.09)ht=0.09
 29   dly=1.5*ht*units*2.0
      if(ifs.eq.0)go to 40
      go to 600
 30   write(print_lun,*)' SPLT_THIT-->ERROR ON TYPE CARD'
      write(print_lun,*)' CTEMP = ',ctemp,' HT = ',ht
      return
 40   ifs=1
      ys=y
      yc= y - (ht*1.5)*units
      go to 10
 120  its=' '
      its(1:8)=icard(1:8)
      if(its.eq.logo) go to 350
      if(its.eq.dubi)go to 360
      if(its.eq.'USSR')go to 370
      if(its.eq.'TPC')go to 380
      if(its.eq.'  FKPLOT')go to 425
      if(its(1:3).eq.'  #')then
        ihalf=1
        yc=yc+dly*.5
        icard(1:3)=' '
      endif
      if(type.eq.cabl)go to 400
      if (type.eq.'ID') go to 700
      if (its .eq. 'C0N0') go to 150
      if (its .ne. 'CONO')go to 200
 150  continue
      if (its .eq. 'C0N0C0-') go to 450
      if (its .eq. 'CONOCO-') go to 450
 200  continue
      cht = ht
!          knt will be equal to 1 on the first card read after the
!              type card
      if(knt.eq.1.and.type.ne.'HEAD') cht=ht*1.5
      if(type.eq.'OWNER')then
        yc=yc-dly
        go to 690
      endif
      wts=cht*.7
      tks=cht*.13
      hti = ht*.8
      wts2 = hti*.7
      tks2 = hti*.13
      sps = hti*1.01
      do k=1,20
        if (nca(k).eq.0)exit
        inf(1:nca(k))=icard(nci(k):nci(k)+nca(k)-1) 
        yc1 = yc + .03*units
        yu= yc - (ht*.52)*units
      enddo
      yc = yc - dly
      if(ihalf.eq.1)then
        yc=yc+dly*.5
        ihalf=0
      endif
      if (cht .ne. ht) yc = yc - dly*.5
      go to 10
 350  continue
! *** call splt_logo to plot logo
!
      yc= yc - (dly/2.)
      sz=2.3125
      if(ht.lt.0.15)then
        f=ht/.15
        sz=sz*f
      endif
!      logo
      yc= yc - dly*1.5
!
      go to 10
!
 360  continue
      yc=yc-dly*1.4
      sz=2.3125*reduce
!        dubai logo
      yc=yc-dly*.8
      go to 10
 370  continue
      yc=yc-dly*1.4
      sz=0.3*reduce
      ypl=yc+.2*units
      yc=yc-dly*.8
      go to 10
 380  continue
      yc=yc-dly*1.4
      sz=0.3*reduce
      if(ang.eq.0.0)then
        xpl=xc+.2*units
      else
        xpl=xc-.2*units
      endif
      ypl=yc+.2*runits
      yc=yc-dly*.8
      go to 10
 400  continue
!
! ***  cable digram
      read(inp,9001)icard
!          test for continuation card
      ich=' '
      ich=icard(80:80)
      if(ich.eq.' ')go to 405
      read(inp,9001,end=405)icard
 405  continue
!
      cht = ht*1.5
      cablen=4.3*runits
      yc = yc - (dly*1.5 + .5*units)
! *** set type so that any card left will be plotted
      type = 'XXX'
      yc = yc - 1.2*units
      go to 10
!
 425  continue
!        fkplot
      y2=yc-2.0*units
      yc=y2-dly
      yc=yc-dly*1.5
      go to 10
!
 450  continue
!
! *** plot conoco-partner name
!
      yc = yc - (dly/2.)
      yc = yc - dly*1.5
      go to 10
 600  continue
      go to 10
!          plot owner
  690 continue
      read(inp,9001,end=720)icard
      nc1=len_trim(ownera)
      nc2=len_trim(icard)
      nc3=len_trim(ownerb)
      ownert=ownera(1:nc1+1) // icard(1:nc2) // ownerb(1:nc3)
      nc=len_trim(ownert)
      i1=1
      i2=70
  695 continue
!        make sure line ends in a period or space
      if(ownert(i2:i2).ne.'.'.and.ownert(i2:i2).ne.' ')then
        i2=i2-1
        go to 695
      endif
      n=i2-i1+1
!        move to icard so text will be on word boundaries
      icard(1:n)=ownert(i1:i1+n-1)
      yc = yc - dly*.5
      if(i2.lt.nc)then
        i1=i1+n
        i2=i1+70
        go to 695
      endif
      yc = yc - dly*.5
      go to 10
  700 continue
!
!           id
      yc = yc - dly*.5
!
 720  continue
      tht=abs(yc/200.0)
      return
!
 9001 format(a)
      end subroutine splt_thit


      subroutine splt_tie(x1,y1,skp,tics,dist,tunit,tnum,ltname,cs_lab, &
                          alpha,idev,yend,tbot,iunit)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*) , intent(in) :: idev
      integer , intent(in) :: iunit
      real , intent(in) :: x1
      real  :: y1
      real , intent(in) :: skp
      real  :: tics
      real , intent(in) :: dist
      real , intent(in) :: tunit
      real , intent(in) :: tnum
      real , intent(in) :: cs_lab
      real , intent(in) :: alpha
      real  :: yend
      character(len=*)  :: ltname,tbot
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer ::       nc, nbkc 
      real , dimension(4) :: xarray, yarray
      real :: addang, units, space, ang, angn, width, y2, cunit, xsym, ydel, &
         ydele, delti, xx, ytl

      save xarray, yarray, addang, units, space, ang, angn, width, y2, &
         cunit, nc, xsym, ydel, ydele, nbkc, delti, xx, ytl
!-----------------------------------------------
! this routine plots tie lines
!   x1     = the x coordinate of the section
!   y1     = the y coordinate of the top of the section
!   skp    = number of inches up (to allow for shot points) from top
!   tics  = the height of the characters in inches
!   dist   = the distance above the section in inches
!   tunit  = units per trace
!   tnum   = the number of trace that the tie is on
!   ltname = array containing the name of the line tie
!   cs_lab   = height of numbers
!   idev   = device to plot on
!   yend   = the y coordinate of the end of the section
!   tbot  = label tie tic at bottom of section as well as top
!  iunit   = unit number of plot file
!
!
!
!
!          mickey mouse due to logic sciences character set
      if (tics>0.07 .and. tics<0.075) tics = 0.07
      addang = 20.0
      units = 200.0
      call cpsplot_fnt (iunit,0)
      space = tics*0.85
      ang = alpha + 20.0
      angn = alpha + addang
      width = 0.025
      y2 = y1 + (skp - cs_lab*1.7)*units           ! not used
      cunit = tics*units
      nc = 40
      xarray(1) = tunit*tnum + x1
      xsym = xarray(1)
      if (skp /= 0.0) then
         ydel = 0.25*units + y1
         ydele = yend - 0.25*units
         call cpsplot_xline (iunit,xarray(1),y1,xarray(1),ydel,0.01,15,100)
         if (tbot == 'YES') call cpsplot_xline (iunit,xarray(1), yend, &
                             xarray(1), ydele, 0.01, 15, 100)
      endif
      xarray(2) = xarray(1)
      yarray(2) = y1 + dist*units
      yarray(1) = yarray(2) - 0.1*units
      call cpsplot_xlines (iunit,xarray,yarray,2,0.01,15,1,1,110)
      nbkc=len_trim(ltname)
      if (nbkc == 0) nbkc = nc
      delti = (space*nbkc/2.0)*units
      xarray(3) = xarray(1)
      xarray(4) = xarray(1) + 2.*delti
      yarray(3) = yarray(2)
      yarray(4) = (xarray(4)-xarray(3))*tan(angn*0.01745329) + yarray(2)
      if (alpha /= 0) then
         xarray(4) = xarray(1) - 2.*delti
         yarray(4) = (xarray(3)-xarray(4))*tan(angn*0.01745329) + yarray(2)
      endif
      call cpsplot_xlines (iunit,xarray(3:), yarray(3:), 2, 0.01, 15, 1, 1, 115)
      xx = xsym
      ytl = yarray(2) + (width*units)*2 + cunit/2.0
      call cpsplot_xsymbol (iunit,xx, ytl, ltname, nbkc, tics, 15, 2, ang)
      call cpsplot_fnt (iunit,19)
      return
      end subroutine splt_tie

      subroutine splt_tlne(yyy,sec,obj)
      real , intent(in) :: yyy
      real , intent(inout) :: sec
      type(splt_struct),intent(in) :: obj       ! arguments
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer ::    idot, itlsts   , itmp, itb, kntrow, nsh, nvdscn,&  
                 ml10, ml50, ml100, ml500, ml1000, ml400, num, ij, i, ii
      real , dimension(3) :: tb

      real :: wt, units, ftmp, frac, ybias, secadd, yepan, ys, yyyx, yendx, &
              yepanx, x1, xz, dy, dy10, dy50, dy100,dy500,dy1000,dy400,ymax, &
              trnw, yt, yz

      logical :: almost_equal

      save
!-----------------------------------------------
!
!          routine to draw horizontal timing lines
!
!
!        obj%xo   = beginning x-coordinate  of the section
!      obj%xend   = ending x-coordinate  of the section
!               yyy  = beginning y-coordinate
!       obj%tunit = trace units
!               sec  = number of seconds
!   obj%ips_eng   = inches per second
!  obj%tldots(1)  = num dots for 10 mil line
!      obj%scan   = number of scans per trace (res/obj%tpi_eng+.05)
!        obj%dt   = sample rate
!    obj%opt_vl   = blank timimg line at shot point locations if
!                      obj%opt_vl=btl
!      obj%rtbp   = repeat times in blank trace pattern
!   obj%tpi_eng   = traces to the inch
! obj%yo   = y-origin of section before timing line shift
!      obj%tbst   = trace to start blank timing lines
!      obj%tbib   = trace intervals between blank timing lines
!     obj%tbtot   = total number of blanks
!      obj%nbts   = number of blank trace patterns
!     obj%btskp   = number of traces to skip before inserting blanks
!      obj%btdo   = number of blank traces to do
!     obj%btbtwn  = number of traces between blanks
!     obj%bttot   = number of times to repeat pattern
!      obj%trce   = trace to start for shot points
!      obj%intv   = the number of trace intervals to the next shot label
!      obj%totl   = the number of shot points to label
!      ojbect%time   = number of seconds to plot
!      obj%nrow   = number of rows if paneling
!     obj%tstrt   = starting ojbect%time of section
!      obj%twid   = trace width in inches
!      obj%yend   = ending y-coordinate of the section
!obj%hdr_lab(1)   = header used to label shot points
!     obj%nshots  = the number of entries in obj%trce,obj%intv,
!                        obj%totl arrays
!      obj%tlst   = timing line starting position in mils from the start
!                       of the section.
!
!          if rtbp = yes  - blank timing lines are not allowed
!
      data wt/ 0.005/
      data units/ 200.0/
!
      idot = 1
      itlsts = obj%tlst
      if (obj%tstrt < 0.0) itlsts = abs(obj%tstrt*1000.0)
!
!          make a y-adjustment if obj%tstrt not at whole second
      almost_equal = ameq(obj%tstrt,0.0,0.0001)
      if (.not.almost_equal) then
         itmp = obj%tstrt + 0.0000001
         ftmp = itmp
         frac = obj%tstrt - ftmp
         ybias = frac*obj%ips_eng*units
         secadd = frac
      else
         frac = 0.0
         ybias = 0.0
         secadd = 0.0
      endif
!
!          initially set if blank timing line parameters come in
!             from blank timing line array
      tb(1) = obj%tbst
      tb(2) = obj%tbib
      itb = obj%tbtot
!
!
      yepan = obj%yend
      ys = yyy
      kntrow = 0
      if (obj%nrow > 1) then
         sec = (obj%time + 0.1)/obj%nrow - 0.1
         kntrow = obj%nrow
         yepan = obj%yo - sec*obj%ips_eng*units
      endif
      yyyx = yyy
      yendx = obj%yend
      yepanx = yepan
    5 continue
      if (obj%opt_vl == 'BTL') then
!
         nsh = 1
         if (obj%hdr_lab(1)==0 .and. obj%nshots>0) then
            tb(1) = obj%trce(1)
            tb(2) = obj%intv(1)
            if (obj%nshots > 0) itb = obj%totl(1)
         else
            tb(1) = obj%tbst
            tb(2) = obj%tbib
            itb = obj%tbtot
         endif
      endif
      x1 = obj%xo
      xz = obj%xend + obj%twid*units
      nvdscn = obj%scan/2.0 + 0.0000001
!     if (tl(1) .ne. 0) obj%tldots(1) = 0
      dy = units*obj%ips_eng
      dy10 = (units*obj%ips_eng)/100
      dy50 = (units*obj%ips_eng)/20
      dy100 = (units*obj%ips_eng)/10
      dy500 = (units*obj%ips_eng)/2.
      dy1000 = units*obj%ips_eng
      dy400 = (units*obj%ips_eng)/2.5
   20 continue
      ml10 = (sec + secadd)*100.
      ml50 = (sec + secadd)*20.
      ml100 = (sec + secadd)*10.
      ml500 = (sec + secadd)*2.
!                select mode -no--610
      ml1000 = (sec + secadd) + 1. - obj%dt
      ml400 = (sec + secadd)*2.5
      if (ybias /= 0.0) ml1000 = ml1000 + 1
      ymax = ys - (sec*obj%ips_eng)*units
      if (ys - ml10*dy10 <= ymax) ml10 = ml10 - 1
      if (ys - ml50*dy50 <= ymax) ml50 = ml50 - 1
      if (ys - ml100*dy100 <= ymax) ml100 = ml100 - 1
      if (ys - ml500*dy500 <= ymax) ml500 = ml500 - 1
      if (ys - ml400*dy400 <= ymax) ml400 = ml400 - 1
!
!          if rtbp option - do not draw lines in blank trace
!             pattern
      if (obj%rtbp /= 'NO') then
         x1 = obj%xo
         xz = (obj%btskp(1)+1)/obj%tpi_eng*units + obj%xo
         num = obj%bttot(1)
         go to 50
      endif
!
!          check for blank timing lines
      if (itb == 0) go to 50
      trnw = tb(1)
      call splt_abt (tb(1),trnw,obj%nbts,obj%btskp,obj%btdo,&
                     obj%btbtwn, obj%bttot)
      xz = trnw*obj%tunit - nvdscn + obj%xo
      go to 50
   25 continue
      trnw = tb(1)
      call splt_abt (tb(1),trnw,obj%nbts,obj%btskp,obj%btdo,&
                     obj%btbtwn,obj%bttot)
      x1 = trnw*obj%tunit + nvdscn + obj%xo
      if (x1 >= obj%xend) go to 620
      tb(1) = tb(1) + tb(2)
      call splt_abt (tb(1),trnw,obj%nbts,obj%btskp,obj%btdo,&
                     obj%btbtwn, obj%bttot)
      xz = trnw*obj%tunit - nvdscn + obj%xo
      xz = amin1(obj%xend,xz)
      if (xz<=0.0 .or. trnw<=0.0) xz = obj%xend
!
   50 continue
      yt = ys - dy10
      almost_equal  = ameq(ys,obj%yo,0.0001)
      if (.not.almost_equal) call cpsplot_xline (obj%ian,x1,obj%yo,xz,&
                                                 obj%yo,0.01,15,909)
      if (obj%tstrt /= 0.0)then
         call cpsplot_xline (obj%ian,x1, obj%yo,xz,obj%yo,wt,15, 911)
      endif
!            draw a timing line at the beginning of each panel
      if (kntrow /= obj%nrow)then
         call cpsplot_xline (obj%ian,x1, ys, xz, ys, wt, 15, 911)
      endif
      if (obj%tldots(1) /= 0) then
         ij = obj%tldots(1)
         do i = 1, ml10
            yz = yt + ybias
            if (.not.(yz>yyyx .or. yz<yendx .or. yz<yepanx)) then
               do ii = 1, ij
                  call cpsplot_xline (obj%ian,x1, yz, xz, yz, wt, 15, 910)
                  yz = yz - idot
               end do
            endif
            yt = yt - dy10
         end do
      endif
      yt = ys - dy50
      if (obj%tldots(2) /= 0) then
         ij = obj%tldots(2)
         do i = 1, ml50
            yz = yt + ybias
            if (.not.(yz>yyyx .or. yz<yendx .or. yz<yepanx)) then
               do ii = 1, ij
                  call cpsplot_xline (obj%ian,x1, yz, xz, yz, wt, 15, 920)
                  yz = yz - idot
               end do
            endif
            yt = yt - dy50
         end do
      endif
      yt = ys - dy100
      if (obj%tldots(3) /= 0) then
         ij = obj%tldots(3)
         do i = 1, ml100
            yz = yt + ybias
            if (.not.(yz>yyyx .or. yz<yendx .or. yz<yepanx)) then
               do ii = 1, ij
                  call cpsplot_xline (obj%ian,x1, yz, xz, yz, wt, 15, 930)
                  yz = yz - idot
               end do
            endif
            yt = yt - dy100
         end do
      endif
      yt = ys - dy500
      if (obj%tldots(5) /= 0) then
         ij = obj%tldots(5)
         do i = 1, ml500
            yz = yt + ybias
            if (yz > yyyx) cycle
            do ii = 1, ij
               call cpsplot_xline (obj%ian,x1, yz, xz, yz, wt, 15, 940)
               yz = yz - idot
            end do
            yt = yt - dy500
         end do
      endif
      yt = ys - dy400
      if (obj%tldots(4) /= 0) then
         ij = obj%tldots(4)
         do i = 1, ml400
            yz = yt + ybias
            if (yz > yyyx) cycle
            do ii = 1, ij
               call cpsplot_xline (obj%ian,x1, yz, xz, yz, wt, 15, 940)
               yz = yz - idot
            end do
            yt = yt - dy400
         end do
      endif
      yt = ys
      if (obj%tldots(6) /= 0) then
         ij = obj%tldots(6)
         do i = 1, ml1000
            yz = yt + ybias
            if (.not.(yz>yyyx .or. yz<yendx .or. yz<yepanx)) then
               do ii = 1, ij
                  call cpsplot_xline (obj%ian,x1, yz, xz, yz, wt, 15, 950)
                  yz = yz - idot
               end do
            endif
            yt = yt - dy1000
         end do
      endif
      if (obj%rtbp == 'NO') go to 612
      num = num - 1
      if (num < 0) go to 620
!          reset x1 and xz from blank trace array
      x1 = xz + (obj%btdo(1)-1)/obj%tpi_eng*units
      xz = (obj%btbtwn(1)+1)/obj%tpi_eng*units + x1
      go to 50
!
  612 continue
      if (tb(1) == 0.) go to 620
      itb = itb - 1
      if (itb > 0) go to 25
      if (obj%opt_vl/='BTL' .and. itb==0) go to 25
      if (obj%opt_vl == 'BTL') then
         if (itb == 0) go to 900
         nsh = nsh + 1
         if (nsh <= 20) then
            if (obj%trce(nsh) /= 0.0) then
               if (obj%hdr_lab(1)==0 .and. obj%nshots>0) then
                  tb(1) = obj%trce(nsh)
                  tb(2) = obj%intv(nsh)
                  itb = obj%totl(nsh)
                  itb = itb - 1
!          determine intervals between shot point cards
                  if (itb == 0) tb(2) = obj%trce(nsh+1) - obj%trce(nsh)
                  if (tb(1) /= 0.) go to 25
               endif                             ! obj%hdr_lab(1).eq.0
            endif
         endif
      endif
      if (xz >= obj%xend) go to 620
      x1 = xz
      xz = obj%xend
      go to 50
  620 continue
      kntrow = kntrow - 1
      if (kntrow > 0) then
         ys = ys - (0.1 + sec)*obj%ips_eng*units
         if (obj%rtbp == 'NO') then
            if (obj%hdr_lab(1)==0 .and. obj%nshots>0) then
               tb(1) = obj%trce(1)
               tb(2) = obj%intv(1)
               itb = obj%totl(1)
            else
               tb(1) = obj%tbst
               tb(2) = obj%tbib
               itb = obj%tbtot
            endif
            if (itb == 0) go to 880
            trnw = tb(1)
            call splt_abt(tb(1),trnw,obj%nbts,obj%btskp,obj%btdo,&
                          obj%btbtwn,obj%bttot)
            xz = trnw*obj%tunit - nvdscn + obj%xo
         endif
!
!
         if (itb /= 0) then
            trnw = tb(1)
            call splt_abt(tb(1),trnw,obj%nbts,obj%btskp,obj%btdo,&
                          obj%btbtwn, obj%bttot)
            x1 = trnw*obj%tunit + nvdscn + obj%xo
            if (x1 < obj%xend) then
               tb(1) = tb(1) + tb(2)
               call splt_abt (tb(1),trnw,obj%nbts,obj%btskp,&
                              obj%btdo, obj%btbtwn, obj%bttot)
               xz = trnw*obj%tunit - nvdscn + obj%xo
               xz = amin1(obj%xend,xz)
               if (xz<=0.0 .or. trnw<=0.0) xz = obj%xend
               if (tb(1) /= 0.) itb = itb - 1
            endif
         endif
  880    continue
         yepanx = ys - sec*obj%ips_eng*units
         go to 5
      endif                                      ! kntrow.gt.0
!
!         draw lines above zero obj%time if tlst set or have negative tstrt
      if (itlsts > 0) then
         ys = yyy
         sec = real(itlsts)/1000.0
         yyyx = obj%yo
         yendx = yyy
         yepan = yyy
         idot = -idot
         dy10 = -dy10
         dy100 = -dy100
         dy500 = -dy500
         dy1000 = -dy1000
         ybias = 0.0
         secadd = 0.0
         itlsts = 0
         go to 20
      endif
      return
  900 continue
      if (obj%trce(nsh+1) == 0.) go to 25
      if (obj%hdr_lab(1)==0 .and. obj%nshots>0) then
         tb(2) = obj%trce(nsh+1)
         tb(2) = tb(2) - tb(1)
      endif
      go to 25
      end subroutine splt_tlne


      subroutine splt_tml(xx,yyy,yend,tlcs,tstrt,numsec,ips,fsect,lrrl,&
                          lorf,ysdel,yedel,y3del,y4del,nrow,otim,lwda, &
                          lwdb,lwdc,lwdd,tunit,secend,xbias,idev,&
                          twid,xs,cs_lab,iunit)
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: numsec
      character(len=8), intent(in) :: lrrl
      integer , intent(in) :: lorf
      integer , intent(in) :: nrow
      integer , intent(in) :: iunit
      character(len=*) , intent(in) :: idev
      real , intent(in) :: xx
      real , intent(in) :: yyy
      real , intent(in) :: yend
      real , intent(in) :: cs_lab
      real  :: tlcs
      real  :: tstrt
      real , intent(in) :: ips
      real  :: fsect
      real , intent(in) :: ysdel
      real , intent(in) :: yedel
      real , intent(in) :: y3del
      real , intent(in) :: y4del
      real , intent(in) :: otim
      real  :: tunit
      real , intent(in) :: secend
      real , intent(in) :: xbias
      real , intent(in) :: twid
      real , intent(in) :: xs
      character(len=16)  :: lwda
      character(len=16)  :: lwdb
      character(len=16)  :: lwdc
      character(len=16)  :: lwdd
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer ::    kntrow, itmp, nca, ncb, ncc, & 
         ncd, nchar, ndec, ibias, kbias, istrs, istrs2, nums, i, idx1, idx2
      real :: increm, hsize, pcnt, pcnth, units, y1, pcntadj, fnc, x1, x1s, &
         theta, strsec, ftmp, frac, xlab, xlaba, xlabb, xlabc, xlabd, ywrda, &
         ywrdb, ywrdc, ywrdd, yy, thick, thk2
      logical ::  almost_equal
      character (len=8) :: irl,lr

      save increm,irl, lr, hsize, pcnt,pcnth,units,y1, &
         pcntadj, fnc, x1, x1s, theta, kntrow, strsec, itmp, ftmp, frac, &
         xlab, nca, ncb, ncc, ncd, xlaba, xlabb, xlabc, xlabd, ywrda, ywrdb, &
         ywrdc, ywrdd, yy, nchar, ndec, thick, thk2, ibias, kbias, istrs, &
         istrs2, nums, i, idx1, idx2
!-----------------------------------------------
! this routine is use to number timing lines
!  (xx,yyy)= the x,y of the section (first or last trace) already scaled
!  yend    = y-coordinate of end of the section
!  tlcs    = height of numbers in inches
!  tstrt, = number of the first(second) timing line.
!  numsec  = number of the last(whole second) timing line.
!  ips    = number of inches/second of data
!  fsect = odd tenths of seconds
!  lrrl  = lr = left to right
!          rl = right to left
!  lorf  = last ,first, middle
!         1 = first
!         2 = last
!         3 = middle
!  ysdel = y-coordinate for header label
!  yedel = y-coordinate for elevation label
!  y3del = y-coordinate for 3rd label (above elevation label)
!  y4del = y-coordinate for 4th label (above 3rd label)
!  nrow  = number of rows if paneling
!  otim  = old time before paneling
!  lwda  = label for header word a (wrda)
!  lwdb  = label for header word b (wrdb)
!  lwdc  = label for header word c (wrdc)
!  lwdd  = label for header word d (wrdd)
!  tunit = trace unit
!  secend= number of the last timing line
!  xbias = difference between x-origin and x-coordinate of first shot
!          point label
!  idev  = device to plot on
!  twid  = width between traces in inches
!  xs    = x- coordinate to use for small numbers when lorf=3
!  iunit = unit number of plot annotation file.
!
!
      data irl/ 'RL'/
      data lr/ 'LR'/
!
!
!
      hsize = tlcs/2.0
      pcnt = 0.85
      pcnth = 0.85
      if (hsize<0.1) pcnth = 1.0
      units = 200.
      y1 = yyy
!
!          set x1 to lower left of first timing line number
!           3 characters in number
!           xflotno routine is plotting the number at just left
!            of center of the first character - trying to adjust
!            for this by using .35 tlcs
!            leave .15 inch from end of timing line to number
!
      pcntadj = 1.0
      fnc = 3.0
      if (numsec > 9.9999999) fnc = 4.0
      if (lrrl==lr .and. lorf==1) then
         x1 = xx - (fnc*tlcs*pcnt + pcntadj*tlcs)*units
         x1s = xx - (fnc*hsize*pcnth + pcntadj*hsize)*units
         if (tlcs <= 0.1000001) then
            x1 = x1 - 0.15*units
            x1s = x1s - 0.15*units
         endif
      endif
      if (lrrl==lr .and. lorf==2) then
         x1 = xx + pcntadj*tlcs*units + twid*units
         x1 = x1 + 0.15*units
         x1s = xx + pcntadj*hsize*units + twid*units
         x1s = x1s + 0.15*units
      endif
      if (lrrl==irl .and. lorf==1) then
         x1 = xx - 0.15*units
         x1s = x1
      endif
      if (lrrl==irl .and. lorf==2) then
         x1 = xx + (fnc*tlcs*pcnt + pcntadj*tlcs)*units + twid*units
         x1s = xx + (fnc*hsize*pcnth + pcntadj*hsize)*units + twid*units
         if (tlcs <= 0.100001) then
            x1 = x1 + 0.15*units
            x1s = x1s + 0.15*units
         endif
      endif
!
      if (lrrl == lr) then
         theta = 0.0
      else
         theta = 180.0
      endif
      almost_equal = ameq(tstrt,0.0,0.00001)
      if (almost_equal) tstrt = 0.0
      kntrow = nrow
      if (lwda /= ' ') then
         xlab = x1
         nca=len_trim(lwda)
         ncb=len_trim(lwdb)
         ncc=len_trim(lwdc)
         ncd=len_trim(lwdd)
         if (lrrl==lr .and. lorf==1) then
!               need to right justify first label
            xlaba = xx - nca*0.1*pcnt*units - 0.3*units
            xlabb = xx - ncb*0.1*pcnt*units - 0.3*units
            xlabc = xx - ncc*0.1*pcnt*units - 0.3*units
            xlabd = xx - ncd*0.1*pcnt*units - 0.3*units
            xlab = amin1(xlaba,xlabb,xlabc,xlabd)
            xlab = xlab - xbias
         endif
         if (lrrl==irl .and. lorf==2) then
!            need to right justify last label
            xlaba = xx + nca*0.1*pcnt*units + 0.3*units + twid*units
            xlabb = xx + ncb*0.1*pcnt*units + 0.3*units + twid*units
            xlabc = xx + ncc*0.1*pcnt*units + 0.3*units + twid*units
            xlabd = xx + ncd*0.1*pcnt*units + 0.3*units + twid*units
            xlab = amax1(xlaba,xlabb,xlabc,xlabd)
         endif
!
         ywrda = yyy + ysdel
         call cpsplot_xsymbol (iunit,xlab,ywrda,lwda,nca,cs_lab,&
                                            15,1,theta)
      endif
!
      if (lwdb /= ' ') then
         ywrdb = yyy + yedel
         call cpsplot_xsymbol(iunit,xlab,ywrdb,lwdb,ncb,cs_lab,&
                                           15,1,theta)
      endif
      if (lwdc /= ' ') then
         ywrdc = yyy + y3del
         call cpsplot_xsymbol(iunit,xlab,ywrdc,lwdc,ncc,cs_lab,&
                                           15,1,theta)
      endif
      if (lwdd /= ' ') then
         ywrdd = yyy + y4del
         call cpsplot_xsymbol(iunit,xlab,ywrdd,lwdd,ncd,cs_lab,&
                                           15,1,theta)
      endif
    2 continue
      strsec = tstrt
      itmp = strsec + 0.0000001
      ftmp = itmp
      frac = strsec - ftmp
      almost_equal = ameq(frac,0.0,0.00001)
      if (almost_equal) frac = 0.0
      if (lorf == 3) then
         x1 = xx
         x1s = xs
      endif
      almost_equal = ameq(tlcs,0.0,0.00001)
      if (almost_equal) return
!
      yy = y1
      nchar = -3
      ndec = 1
      increm = ips*(units*0.2)
      thick = hsize*0.13
      thk2 = tlcs*0.13
      if (ips<=5.0) thick = hsize*0.18
      if (ips<=2.5) then
         thick = hsize*0.21
         thk2 = tlcs*0.18
      endif
      ibias = (strsec + 0.01)*10.
      kbias = ((ibias + 1)/2)*2
      ibias = (ibias/2)*2
      istrs = 0
      istrs2 = 0
      nums = numsec + 1
      if (strsec /= 0.) then
         if (frac /= 0.0) then
            if (ibias /= kbias) then
               yy = y1 - units*(ips*0.1)
               strsec = strsec + 0.1
            endif
            istrs = strsec + 1
            istrs2 = istrs
            if (istrs == 0) istrs2 = 1
!          for very small characters use default font
            if (hsize<0.1) call cpsplot_fnt (iunit,0)
            do i = 1, 5
               if (strsec + 0.05 > istrs) exit
               if (strsec > 9.9) nchar = -4
               if (strsec>=0.0 .and. yy>yend)then
                  call cpsplot_xflotno (iunit,x1s,yy,nchar,&
                                        ndec,strsec,hsize,15,theta)
               endif
               strsec = strsec + 0.2
               yy = yy - increm
            end do
         endif
      endif
      if (hsize<0.1) call cpsplot_fnt (iunit,19)
l100: do idx1 = istrs2, nums
         if (strsec + 0.05 > secend) exit  l100
         if (strsec > 9.9) nchar = -4
         if (strsec < 0.0) then
            almost_equal = ameq(strsec,0.0,0.00001)
            if (almost_equal) strsec = 0.0
         endif
         if (yy > yend)then
             call cpsplot_xflotno(iunit,x1,yy,nchar,ndec,strsec,tlcs,15,theta)
         endif
!          for very small characters use default font
         if (hsize<0.1) call cpsplot_fnt (iunit,0)
         do idx2 = 1, 4
            yy = yy - increm
            strsec = strsec + 0.2
            if (strsec + 0.05 > secend) exit  l100
            if (strsec > 9.9) nchar = -4
            if (strsec<=0.0 .or. yy<=yend) cycle
            call cpsplot_xflotno (iunit,x1s,yy,nchar,ndec,strsec,hsize,15,&
                                  theta)
         end do
!           set back to solid font
         if (hsize<0.1) call cpsplot_fnt (iunit,19)
         yy = yy - increm
         strsec = strsec + 0.2
      end do l100
      kntrow = kntrow - 1
      if (kntrow > 0) then
         y1 = y1 - (0.1 + otim)*ips*units
         go to 2
      endif
!           set back to solid font
      if (hsize<0.1) call cpsplot_fnt (iunit,19)
      return
      end subroutine splt_tml

      subroutine splt_tmrk(x, y, i10, tunit, ml10, dy10, wt,iunit)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: i10,iunit
      integer , intent(in) :: ml10
      real , intent(in) :: x
      real , intent(in) :: y
      real , intent(in) :: tunit
      real , intent(in) :: dy10
      real  :: wt
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: ij, i, ii
      real :: x1, xz, yt, yz

      save x1, xz, yt, ij, i, yz, ii
!-----------------------------------------------
!
!
!          ten mill timing marks
!
!
!            x = x-coordinate
!            y = y-coordinate
!          i10 = number of dots for 10 mil timing lines
!        tunit = width of one trace
!         ml10 = the number of 10 mil marks to draw
!         dy10 = the distance between 10 mil marks
!         wt   = width of lines in inches
!
      x1 = x - tunit/2.0
      xz = x + tunit/2.0
      yt = y - dy10
      ij = i10
      do i = 1, ml10
         yz = yt
         if (ij == 0) ij = 1
         do ii = 1, ij
            call cpsplot_xline (iunit,x1, yz, xz, yz, wt, 15)
            yz = yz - 1
         end do
         yt = yt - dy10
      end do
!
      return
      end subroutine splt_tmrk



      subroutine splt_us(x,yyy,tlcs,us,ips,time,tstrt,nrow,otim,theta, &
                         lorf,lwda,lwdb,dev,xbias,ysdel,twid,&
                         yedel,lwdc,lwdd,ycdel,yddel,iunit)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: us
      integer , intent(in) :: nrow
      integer , intent(in) :: iunit
      character(len=*),intent(in) :: lorf
      character(len=*),intent(in) :: dev
      real , intent(in) :: x
      real , intent(in) :: yyy
      real  :: tlcs
      real , intent(in) :: ips
      real  :: time
      real , intent(in) :: tstrt
      real , intent(in) :: otim
      real  :: theta
      real , intent(in) :: xbias
      real , intent(in) :: ysdel
      real , intent(in) :: twid
      real , intent(in) :: yedel
      real , intent(in) :: ycdel
      real , intent(in) :: yddel
      character(len=*)  :: lwda
      character(len=*)  :: lwdb
      character(len=*)  :: lwdc
      character(len=*)  :: lwdd
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!----------------------------------------------- 
     integer :: kntrow,nch,ndec,iopf,i  ,ifpoi,nca,ncb, & 
                 ncc,ncd
      real :: units,fs,y,pcnt,xp,dely,fpic,fpoi,totin,yknt,yp,sfpoi,&
              fnum,xlab,xlaba,xlabb,xlabc,xlabd,ywrda,ywrdb,ywrdc,ywrdd

      character (len=1) :: kf,kl

      logical :: iangzro,almost_equal

      save
!-----------------------------------------------
!
!     routine to number units per second
!
!          x    = x-coordinate of section
!          yyy  = y-coordinate of section
!          tlcs = tlcs of characters
!          us  = units per second
!          ips   = inches per second
!          time = length
!          tstrt = starting time
!          nrow  = the number of rows if paneling
!          otim  = the old time before paneling
!          theta = angle to plot annotation
!          lorf  = last or first
!                  f = labels off first trace end
!                  l = labels off last trace end
!                  m = labels between panels
!          lwda  = label for header word a (wrda)
!          lwdb  = label for header word b (wrdb)
!          dev  = device to plot on
!         xbias  = difference between x-origin and x-coordinate of 1st
!                  shot point label
!         ysdel  = y-coordinate for shot point label
!          twid  = width between traces in inches
!          yedel = y-coordinate for elevation label
!          lwdc  = label for header word c (wrdc)
!          lwdd  = label for header word d (lwdd)
!         ycdel  = y-coordinate for wrdc label
!         yddel  = y-coordinate for wrdd label
!         iunit  = unit number of plot annotation file
!
!
      data kf/ 'F'/
      data kl/ 'L'/
      data units/ 200.0/
!
      fs = us
      y = yyy
      kntrow = nrow
      pcnt = 0.85
!
!
   20 continue
      xp = x
      dely = 2.*(ips/10.)*units
      fpic = fs/5.             ! this makes the increment every 200 mills
      fpoi = abs(fs)*tstrt
      totin = otim*ips*units
      yknt = 0.0
      nch = 3
      if (fpoi >= 1000.) nch = 4
      ndec = -1
      iopf = fpoi + 0.01
      yp = y
      sfpoi = fpoi
      do i = 1, 1000
         if (sfpoi > 999) nch = 4
         if (sfpoi > 9999) nch = 5
         almost_equal = ameq(yknt,totin,0.001)
         if (almost_equal .or. yknt>=totin) exit
         ifpoi = (sfpoi + 0.001)*100.
         fnum = ifpoi/100. + 0.0001
         if (theta/=0.0 .and. lorf==kf) xp = x - 0.3*units
         if (theta/=0.0 .and. lorf==kl) xp = x + (nch*tlcs + 0.3)*units + twid*&
            units
         if (theta==0.0 .and. lorf==kf) xp = x - (nch*tlcs + 0.3)*units
         if (theta==0.0 .and. lorf==kl) xp = x + 0.3*units + twid*units
         call cpsplot_xflotno (iunit,xp,yp,(-nch),ndec,fnum,tlcs,15,theta)
         yp = yp - dely
         yknt = yknt + dely
         sfpoi = sfpoi + fpic
      end do
      kntrow = kntrow - 1
      if (kntrow > 0) then
         y = y - (0.1 + otim)*ips*units
         go to 20
      endif
!
      if (lwda /= ' ') then
         xlab = xp
         nca=len_trim(lwda)
         ncb=len_trim(lwdb)
         ncc=len_trim(lwdc)
         ncd=len_trim(lwdd)
         iangzro = ameq(theta,0.0,0.0000001)
         if (iangzro .and. lorf==kf) then
!           need to right justify first label
            xlaba = x - nca*0.1*pcnt*units - 0.3*units
            xlabb = x - ncb*0.1*pcnt*units - 0.3*units
            xlabc = x - ncc*0.1*pcnt*units - 0.3*units
            xlabd = x - ncd*0.1*pcnt*units - 0.3*units
            xlab = amin1(xlaba,xlabb,xlabc,xlabd)
            xlab = xlab - xbias
         endif
         if (.not.iangzro .and. lorf==kl) then
!            need to right justify last label
            xlaba = x + nca*0.1*pcnt*units + 0.3*units + twid*units
            xlabb = x + ncb*0.1*pcnt*units + 0.3*units + twid*units
            xlabc = x + ncc*0.1*pcnt*units + 0.3*units + twid*units
            xlabd = x + ncd*0.1*pcnt*units + 0.3*units + twid*units
            xlab = amax1(xlaba,xlabb,xlabc,xlabd)
         endif
         ywrda = yyy + ysdel
         call cpsplot_xsymbol (iunit,xlab, ywrda, lwda, nca, 0.1, 15, 1, theta)
      endif
!
      if (lwdb /= ' ') then
         ywrdb = yyy + yedel
         call cpsplot_xsymbol (iunit,xlab, ywrdb, lwdb, ncb, 0.1, 15, 1, theta)
      endif
      if (lwdc /= ' ') then
         ywrdc = yyy + ycdel
         call cpsplot_xsymbol (iunit,xlab, ywrdc, lwdc, ncc, 0.1, 15, 1, theta)
      endif
      if (lwdd /= ' ') then
         ywrdd = yyy + yddel
         call cpsplot_xsymbol (iunit,xlab, ywrdd, lwdd, ncd, 0.1, 15, 1, theta)
      endif
      return
      end subroutine splt_us
      subroutine splt_vf(obj,ivftr,isubl,ibtadd,errorflag)
!
!          routine to plot velocity functions
!
!
!          obj%tunit   = units per trace
!          ivftr   = trace number of function
!          obj%kfunc   = velocity function name
!          isubl   =  sub-label for velocity function
!          obj%sclskp  = skip for scal
!          obj%vint   = velocity interpolation parameter
!                  timi   = time interpolation
!                  velo   = velocity interpolation
!                  plot   = velocity interpolation and iso plots
!                  dpth   = convert vel to depth and iso depth plots
!                  vcol   = velocity coutour overlayed on section
!                  dcol   = depth coutour overlayed on section
!                  blank  = no interpolation
!          ivi     = label interval velocities
!          obj%ips_eng     = inches per second
!          iftmt   = convert feet to meters
!          obj%theta   = angle to plot annotation
!          obj%tloc    = array of function times
!          obj%vloc    = array of functin velocities
!          obj%xo      = x-coordinate of section (trace 1, sample 1)
!          obj%yo      = y-coordinate of section (trace 1, sample 1)
!          obj%npairs  = number of time-velocity pairs in function
!             obj%nbts = the number of blank trace patterns.
!          obj%btskp  = number of traces to skip before blanks.
!          obj%btdo   = number of blanks to insert.
!         obj%btbtwn  = number of traces between blanks.
!          obj%bttot  = total number of sets this pattern
!          ibtadd  = the number of blank traces to add to the trace
!                    number if doing automatic blank traces
!          obj%dev    = the device to plot on
!          obj%frvf  = factor to reduce velocity functions
!          obj%ipf1    = print file
!          obj%ian   = unit number of the plot file
!          errorflag = 0 if all OK


!
!
!
!
!
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      type(splt_struct),intent(inout) :: obj       ! arguments

      integer , intent(in) :: ivftr,ibtadd
      integer , intent(inout) :: errorflag
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------

      integer , dimension(18) :: iast

      integer :: lll,iseq,indx,ii,nbkc,ic,j,k, m, n
      real , dimension(65) :: veli, veln
      real , dimension(20) :: tmn


      real , dimension(65) :: velim

      real , dimension(20) :: ravg

      real , dimension(20) :: tmn2, tmn3, orii
      real , dimension(18) :: rvels, rvelsn, tmnn, velnn
      real :: units, oldt, trlab, vftr, siz1, wid1, thk1, pnt3, twtyp3, pnt1, &
              pnt5,pnt15,pnt2,pnt25,pnt06,pnt45,pnt075,pnt37,pnt09,pnt11, &
              pnt05,y3l1,y3l2,xt,x1l1,x1l2,x1l3,y4l1,x2l1,x2l2,x2l3,x3l3,&
              x3l4,x3l5,y5l1,wid,thi,y3s1,fnbkc,dch,x5s1,y4s1,y2s1,x2s1,&
              y5s1,y6s1,x6s1,xast,x7s1,x8s1,y,y2,a,rvel,tdif,vdif,&
              roc,vdif1,tdif1,y1

      character(len=1) :: kd
      character(len=8) :: ctime,cvel
      character(len=8) :: lc,itimi,itim2,itim3,iorii,ivi='NO',iftmt='NO'
      character(len=16):: isubl,itv

      integer :: istat,l

      save

!
!
!
      data itv/'T      V    VI  '/
!
      data tmn/0.0,.2,.4,.6,.8,1.,1.2,1.4,1.6,1.8,2.0,2.5,3.,3.5,4.,    &
     &        4.5,5.0,5.5,6.0,8.0/
      data tmn2/0.0,0.2,0.4,0.6,1.0,1.4,1.8,2.2,2.6,3.0,3.5,4.0,4.5,    &
     &5.0,5.5,6.0,6.5,7.0,7.5,8.0/
      data tmn3/0.0,1.5,2.0,2.5,3.0,3.5,4.0,5.0,6.0,7.0,8.0,9.0,10.0,   &
     &11.0,12.0,13.0,14.0,15.0,16.0,17.0/
      data orii/0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,        &
     &6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5/
      data ravg/5100.,5400.,5700.,6000.,6300.,                          &
     &          6600.,7000.,7500.,8000.,8500.,9000.,10000.,             &
     &         11000.,12000.,13000.,14000.,15000.,16000.,               &
     &17000,18000/
      data veli/5000.,5200.,5400.,5600.,5800.,6000.,6200.,6400.,        &
     &           6600.,6800.,7000.,7200.,7400.,7600.,7800.,8000.,       &
     &         8200.,8400.,8600.,8800.,9000.,9200.,9400.,9600.,         &
     &         9800.,10000.,10200.,10400.,10600.,10800.,11000.,         &
     &         11200.,11400.,11600.,11800.,12000.,12200.,12400.,        &
     &         12600.,12800.,13000.,13200.,13400.,13600.,13800.,        &
     &         14000.,14200.,14400.,14600.,14800.,15000.,15200.,        &
     &         15400.,15600.,15800.,16000.,16200.,16400.,16600.,        &
     &         16800.,17000.,17200.,17400.,17600.,17800./
!
      data velim/1500.,1550.,1600.,1700.,1750.,1800.,1900.,1950.,       &
     &2000.,2050.,2100.,2150.,2200.,2300.,2350.,2400.,2500.,2550.,      &
     &2600.,2700.,2750.,2800.,2850.,2900.,3000.,3050.,3100.,3200.,      &
     &3250.,3300.,3350.,3400.,3500.,3550.,3600.,3650.,3700.,3800.,      &
     &3850.,3900.,3950.,4000.,4150.,4200.,4250.,4300.,4400.,4450.,      &
     &4500.,4550.,4600.,4700.,4750.,4800.,4850.,4900.,5000.,5050.,      &
     &5100.,5200.,5250.,5300.,5400.,5450,5500/
!
      data itimi/'TINT5'/
      data itim2/'TINT7'/,itim3/'TINT15'/, iorii/'ORI'/, kd/'D'/
      data units/200.0/
!
!
      errorflag=0
      oldt=ivftr
      trlab=ivftr
      vftr=trlab
      if(ibtadd.eq.0)then
        call splt_abt(oldt,vftr,obj%nbts,obj%btskp,obj%btdo,obj%btbtwn,&
                      obj%bttot)
      else
        vftr=vftr + real(ibtadd)
      endif
!
!
      lll=1
!
!
      if(iftmt.eq.'YES'.or.survey_units.eq.'METERS')then
        veli=velim
      endif
      if(obj%vint.eq.'TINT7')then
        tmn=tmn2
        obj%vint='TINT5'
      endif
!
      if(obj%vint.eq.'TINT15')then
        tmn=tmn3
        obj%vint='TINT5'
      endif
!
      if(obj%vint.eq.iorii)then
        tmn=orii
      endif
!
      siz1=.08*obj%frvf
      wid1=siz1*.7*obj%frvf
      thk1=siz1*.13*obj%frvf
      pnt3=0.3*obj%frvf
      twtyp3=20.3*obj%frvf
      pnt1=0.1*obj%frvf
      pnt5=0.5*obj%frvf
      pnt15=0.15*obj%frvf
      pnt2=0.2*obj%frvf
      pnt25=0.25*obj%frvf
      pnt06=0.06*obj%frvf
      pnt45=0.45*obj%frvf
      pnt075=0.075*obj%frvf
      pnt37=0.37*obj%frvf
      pnt09=0.09*obj%frvf
      pnt11=0.11*obj%frvf
      pnt05=0.05*obj%frvf
!
      iseq=0
      indx=1
      y3l1=obj%sclskp*units +obj%yo
      y3l2=y3l1+20.3*(pnt1*units)
!
      if(iftmt.ne.'YES')go to 450
!
!          convert feet to meters
      do 175 ii=1,obj%npairs
      obj%vloc(ii)=obj%vloc(ii)*.30480064
  175 continue
!
!          draw scale
  450 xt=vftr*obj%tunit + obj%xo
      call cpsplot_xline(obj%ian,xt,y3l1,xt,y3l2,.025,15,620)
      x1l1=xt-units*pnt3
      x1l2=xt+units*pnt3
      x1l3=x1l2+units*pnt5
      if(obj%theta.ne.0.0)x1l3=x1l1-units*pnt5
      call cpsplot_xline(obj%ian,x1l1,y3l2,x1l2,y3l2,.025,15,625)
!
      y4l1=y3l2+units*pnt15
      x2l1=xt-units*pnt5
      x2l2=xt+units*pnt5
      x2l3=x2l2+units*pnt5
      x3l3=xt
      x3l4=xt
      x3l5=xt
      if(ivi.ne.'YES')go to 500
      x3l3=x2l2
      x3l4=x2l2
      if(obj%theta.eq.0.0)go to 500
      x3l3=x2l1
      x3l4=xt
      x3l5=x2l1
  500 continue
!
      if(obj%theta.ne.0.0)x2l3=x2l1-units*pnt5
      call cpsplot_xline(obj%ian,x2l1,y4l1,x2l2,y4l1,.025,15,630)
      y5l1=y4l1+units*pnt2
      call cpsplot_xline(obj%ian,x2l1,y5l1,x2l2,y5l1,.025,15,635)
!
!          plot labels for scale
      wid=.084
      thi=.0156
      y3s1=y3l2+units*pnt25
      nbkc=len_trim(isubl)
      fnbkc=nbkc-1.0
      dch=fnbkc*pnt06*units
      x5s1=xt-dch
      if(obj%theta.ne.0.0)x5s1=xt+dch
      call cpsplot_xsymbol(obj%ian,x5s1,y3s1,isubl,16,pnt09,15,2,obj%theta,&
                           istat=istat)
      if(istat.ne.0)then
        write(print_lun,*)'SPLT_VF ERROR A encountered in xsymbol routine'
        errorflag=1
        return
      endif
!
      y4s1=y3l2+units*pnt45
      nbkc=len_trim(obj%kfunc)
      fnbkc=nbkc-1.0
      dch=fnbkc*pnt06*units
      x5s1=xt-dch
      if(obj%theta.ne.0.0)x5s1=xt+dch
!
      call cpsplot_fnt(obj%ian,19)
      call cpsplot_xsymbol(obj%ian,x5s1,y4s1,obj%kfunc,8,.12*obj%frvf,15,2,&
                           obj%theta,istat=istat)
      if(istat.ne.0)then
        write(print_lun,*)'SPLT_VF ERROR B encountered in xsymbol routine'
        errorflag=1
        return
      endif
      lc='VEL FUNS'
      write(obj%ipf1,9010)(y3l1-obj%yo)/200.0,(y4s1-obj%yo)/200.0+.12,l
      call cpsplot_fnt(obj%ian,0)
!
!          plot interval velocity scale
      ic=8
      if(ivi.ne.'YES')go to 550
      ic=16
      call cpsplot_xline(obj%ian,x1l1,y3l2,x1l3,y3l2,.025,15,625)
      call cpsplot_xdash(obj%ian,x3l3,y3l1,x3l3,y3l2,pnt25,.025,.025,15,620)
      call cpsplot_xline(obj%ian,x2l1,y4l1,x2l3,y4l1,.025,15,630)
      call cpsplot_xline(obj%ian,x2l1,y5l1,x2l3,y5l1,.025,15,635)
  550 continue
!
      wid=.07
      thi=.013
      y2s1=y3l2+units*pnt075
      x2s1=xt-units*pnt37
      if(obj%theta.ne.0.0)x2s1=xt+units*pnt37
      call cpsplot_xsymbol(obj%ian,x2s1,y2s1,itv,ic,pnt09,15,0,obj%theta,&
                           istat=istat)
      if(istat.ne.0)then
        write(print_lun,*)'SPLT_VF ERROR C encountered in xsymbol routine'
        errorflag=1
        return
      endif
!
!          coordinates for function
      y5s1=y3l2-pnt09*units
      y6s1=y5s1-(units*pnt11*pnt5)
      x6s1=xt-units*6*siz1-pnt1*units
      xast=x6s1-pnt05*units
      x7s1=xt+units*pnt1
      x8s1=x3l3+units*pnt1
      if(obj%theta.eq.0.0)go to 600
      x6s1=xt+units*6*siz1+pnt1*units
      xast=x6s1+pnt05*units
      x7s1=xt-units*pnt1
      x8s1=x3l3-units*pnt1
  600 continue
!
!
      if(obj%vint.eq.itimi)go to 1150
      rvels=0
      if(obj%vint.eq.'VELO')go to 900
!
!          plot the raw function
      y=y5s1
      j=2
      y2=y6s1
      k=0
      if(obj%vint.eq.iorii.and.ivi.ne.'YES')go to 1150
!
      do 650 ii=1,18
        if(nint(obj%vloc(ii)).eq.0)go to 655
        if(obj%vint.eq.iorii)go to 630
        call cpsplot_xflotno(obj%ian,x6s1,y,-6,3,obj%tloc(ii),siz1,15,&
                             obj%theta)
        call cpsplot_xflotno(obj%ian,x7s1,y,-5,-1,obj%vloc(ii),siz1,15,&
                             obj%theta)
        y=y-pnt11*units
        if(ivi.ne.'YES')go to 650
  630   continue
        a=(((((obj%vloc(j-1)**2)*obj%tloc(j-1))-&
     &     ((obj%vloc(j)**2)*obj%tloc(j)))/ &
     &     (obj%tloc(j-1)-obj%tloc(j))))

        if(a.lt.0.0)then
          write(print_lun,*)' FUNCTION NOT INCREMENTING'
          go to 640
        endif
        rvel=sqrt(a)
        k=k+1
        if(obj%vint.eq.iorii)then
          rvels(k)=rvel
        else
          call cpsplot_xflotno(obj%ian,x8s1,y2,-5,-1,rvel,siz1,15,obj%theta)
        endif
  640   y2=y2-pnt11*units
        j=j+1
  650 continue
!
  655 continue
      if(obj%vint.eq.iorii)go to 1150
!
!
      go to 6000
!          velo option--interpolate times to specified velocities
 900  continue
      do 950 ii=1,20
        tmn(ii)=0.0
        veln(ii)=0.0
 950  continue
!
      l=2
!
!          save first two t-v pairs
      veln(1)=obj%vloc(1)
      veln(2)=obj%vloc(2)
      tmn(2)=obj%tloc(2)
!
      do 1050 j=2,obj%npairs
        do 1000 k=1,20
          if(obj%vloc(j+1).eq.0.0)go to 1100
          if(ravg(k).lt.obj%vloc(j))go to 1000
          if(ravg(k).gt.obj%vloc(j+1))go to 1050
          tdif=obj%tloc(j+1)-obj%tloc(j)
          vdif=obj%vloc(j+1)-obj%vloc(j)
          vdif1=ravg(k)-obj%vloc(j)
          roc=tdif/vdif
          if(vdif.le.0.0.or.vdif1.le.0.0)go to 1000
          if(veln(l).eq.ravg(k))go to 1000
          if(l.gt.19)go to 1000
          l=l+1
!
          tmn(l)=roc*vdif1+obj%tloc(j)
          veln(l)=ravg(k)
 1000   continue
 1050 continue
 1100 continue
!
      l=l+1
      if(l.gt.18)l=18
!
!          save last t-v pair if last value not set
      if(obj%vloc(j).eq.veln(l-1))go to 1350
      tmn(l)=obj%tloc(j)
      veln(l)=obj%vloc(j)
      go to 1350
!
!
!          timi option--interpolate velocities to specified times
 1150 continue
      do 1200 k=1,20
        veln(k)=0.0
 1200 continue
!
      veln(1)=obj%vloc(1)
!
      do 1300 k=2,18
          do 1250 j=1,obj%npairs
          if(tmn(k).lt.obj%tloc(j))go to 1250
          if(tmn(k).gt.obj%tloc(j+1))go to 1250
          tdif=obj%tloc(j+1)-obj%tloc(j)
          tdif1=tmn(k)-obj%tloc(j)
          vdif=obj%vloc(j+1)-obj%vloc(j)
          veln(k)=obj%vloc(j)+vdif*tdif1/tdif
 1250   continue
 1300 continue
      if(obj%vint.eq.iorii)go to 1500
!
!          plot the interpolated function
 1350 continue
      y1=y5s1
      y2=y6s1
      do k=1,18
        if(veln(k).eq.0.0)cycle
        write(ctime,'(F6.3)')tmn(k)
        write(cvel,'(I5)')nint(veln(k))
        call cpsplot_xsymbol(obj%ian,x6s1,y1,ctime,6,siz1,15,1,obj%theta)
        call cpsplot_xsymbol(obj%ian,x7s1,y1,cvel,6,siz1,15,1,obj%theta)
!!!        call cpsplot_xflotno(obj%ian,x6s1,y1,-6,3,tmn(k),siz1,15,obj%theta)
!!!        call cpsplot_xflotno(obj%ian,x7s1,y1,-5,-1,veln(k),siz1,15,obj%theta)
        y1=y1-pnt11*units
        if(ivi.ne.'YES')cycle
        if(veln(k+1).eq.0.0)cycle
        a=(((((veln(k)**2)*tmn(k))-((veln(k+1)**2)*tmn(k+1)))/            &
     &    (tmn(k)-tmn(k+1))))
        if(a.lt.0.0)then
          write(print_lun,*)' FUNCTION NOT INCREMENTING'
          go to 1390
        endif
        rvel=sqrt(a)
!
        call cpsplot_xflotno(obj%ian,x8s1,y2,-5,-1,rvel,siz1,15,obj%theta)
 1390   y2=y2-pnt11*units
      enddo
      go to 6000
!
 1500 continue
!
!          plot the vint=or&i option - the original function
!            plus some interpolated times @ .5 marks
!
!          merge functions together by incrementing time
      j=2
      m=2
      l=2
      tmnn=0
      velnn=0
      rvelsn=0
      iast=0
      tmnn(1)=tmn(1)
      velnn(1)=veln(1)
      rvelsn(1)=rvels(1)
!
      do 1550 n=2,18
        if(tmn(m).ge.obj%tloc(j))go to 1530
        tmnn(n)=tmn(m)
        velnn(n)=veln(m)
        m=m+1
        go to 1550
!
 1530   tmnn(n)=obj%tloc(j)
        velnn(n)=obj%vloc(j)
        rvelsn(n)=rvels(l)
        iast(n)=1
        if(tmn(m).eq.obj%tloc(j))m=m+1
        l=l+1
        j=j+1
 1550 continue
!
!
      y1=y5s1
      y2=y6s1
      do 1600 k=1,18
        if(nint(velnn(k)).eq.0)go to 1600
        if(iast(k).eq.1)then
          call cpsplot_xsymbol(obj%ian,xast,y1,'*',1,siz1,15,0,&
                               obj%theta,istat=istat)
          if(istat.ne.0)then
            write(print_lun,*)'SPLT_VF ERROR C encountered in xsymbol routine'
            errorflag=1
            return
          endif
        endif
        call cpsplot_xflotno(obj%ian,x6s1,y1,-6,3,tmnn(k),siz1,15,obj%theta)
        call cpsplot_xflotno(obj%ian,x7s1,y1,-5,-1,velnn(k),siz1,15,obj%theta)
        y1=y1-pnt11*units
        if(ivi.ne.'YES')go to 1600
        if(rvelsn(k).eq.0.0)go to 1590
        call cpsplot_xflotno(obj%ian,x8s1,y2,-5,-1,rvelsn(k),siz1,15,obj%theta)
 1590   y2=y2-pnt11*units
 1600 continue
!
!
!
 6000 continue
!
      return
!
 9010 format(f6.2,2x,f6.2,2x,a8)
!
      end subroutine splt_vf



!</execute_only>



!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine splt_wrapup (obj)
      implicit none
      type(splt_struct),intent(inout) :: obj       ! arguments


      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.


      return
      end subroutine splt_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
      end module splt_module
