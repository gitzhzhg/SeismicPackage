!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- atblk.f90 --------------------------------!!
!!------------------------------- atblk.f90 --------------------------------!!
!!------------------------------- atblk.f90 --------------------------------!!
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
!                        C P S   P R I M I T I V E
!
! Name       : atblk
! Category   : miscellaneous
! Written    : 1989-05-18   by: Karen Goodger
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Generate a title block file from history information.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! the atblk program determines what histories are to be used to
! generate title block information by tracking the inputs and outputs
! of each history. atblk first looks at history number 1 to get the first
! output reel number or file name.  it then searches to see where that
! reel or file is input. it keeps tracking through the histories in this
! manner until the current history is reached. if atblk is unable to reach
! the current history, it will begin with history 2 and track from
! there. atblk will keep searching until it finds a path to the current
! history. atblk uses only the first input and the last output of each
! history to do the tracking. if you have multiple input processes in
! a job, atblk will not track the second input. therefore, multiple input
! processes in the same job should be avoided. you should never use the
! same output file name or reel number in more that one job. atblk will be
! unable to track if this occurs.
!
! each job you build creates a history record. a record must have a
! unique identifier, and the history file uses the job name for this
! purpose. never use the same job name for different jobs. the history
! file will keep only one of these and you will lose information that
! should go in your title block, and perhaps the entire title block
! since you will also be destroying information needed for tracking.
!
!format of the atblk instruction file is as follows...
!
!columns 1 and 2 of the master file.
!
!code               type
! **      a process name. atblk looks at 8 characters of a process name.
!
! ##      a parameter card. a list of parameters to search for. the
!         parameter names are separated by commas.
!         parameter names may be followed by a subscript. a subscripted
!         parameter will handle three types of formats out of a history
!         file, arrays, linked arrays, nth occurrence.
!
!          arrays -
!
!          information appearing in the history as a set of information
!          in parenthesis separated by commas as in the following
!          example:
!
!            off =(0.000 , 2200.0 , 15663  )
!
!          parameter name off(2) would pick up the 2200.0 out of the
!          history.
!
!          linked arrays -
!
!          information appearing in the history as a table with
!          parameter names enclosed in brackets and answers to those
!          parameters in columns directly below the parameter name
!          as follows:
!
!            [twin,bwin,off]=
!            (0.5 ,3.5 ,0  ),
!            (1   ,4   ,9  ),
!
!          parameter name bwin(2) would pick up the 4 out of the
!          history.
!
!          nth occurrence -
!
!          parameter names which appear more than once as part of a
!          process as follows:
!
!            ifq=  8, ffq= 80, itap= 4
!            ifq=  5, ffq= 50, itap= 4
!
!          parameter name ifq(1) would pick up the 8 out of the history.
!
!         parameter names cannot appear in a parameter list more than
!         once unless they are subscripted.
!
! #l      the number of characters you would like to pick up out of
!         the history file for a parameter answer. this card is
!         optional. if used, atblk will pick up the first non-blank
!         character after the equal sign as character 1 and continue to
!         the length specified. if the length specified is longer than
!         the history file answer, atblk will blank fill this answer.
!         there must be a 1 to 1 correspondence between the parameters
!         on the ## card and the lengths requested on the #l card. if
!         the length does not matter for a particular parameter, use a
!         zero for the length. lengths are separated by commas like the
!         ## card. if you wish to specify lengths for parameters at the
!         beginning of the parameter card only, you do not have to fill
!         out the rest of the length card with zeros. however, if a
!         length is needed for the last parameter, a length must be
!         specified for all parameters.
!
!         if the #l card is not used, atblk will pick up the first non-
!         blank character after the equal sign to the last non-blank
!         character before the comma.
!
! !!      a print card. print this card in the atblk block.
!
! !?      a print card. print this card if all parameters on the card
!         are non-blank.
!
! !/      a conditional print card.
!         examples:
!         !/1=" BULK"/bulk shift
!             if parameter 1 equals  bulk, print bulk shift in the
!             title block.
!         !/5^"O"/cdp static allignment
!             if parameter 5 does not equal o print cpd static
!             allignment in the title block.
!
!         brackets denote parameter fill in on all print cards.
!         example:
!         !!    type [2]   [1] passes    [3] cdp
!             print this card replacing [2] with answer to parameter 2,
!             [1] with answer to parameter 1, and [3] with answer to
!             parameter 3.
!
!         1/2 line increment print cards.
!            if the first character in the printed information portion
!            of a print card is a pound sign (#), this line will be
!            vertically spaced 1/2 the normal increment from the line
!            above it.
!         examples:
!         !!#         bulk shift
!         !/1=" BULK"/#    bulk shift
!
!         multiconditional print cards - use & for logical and
!                                        use | for logical or
!              limited to two conditions
!         examples:
!           !/8="DIP"&52="0"/  dip filter
!               if parameter 8 equals dip and parameter 52 equals 0
!                print dip filter in the title block.
!                both conditions must be true for the card to be
!                printed in the title block.
!
!           !/8="DIP"|52="0"/  dip filter
!               if parameter 8 equals dip or parameter 52 equals 0
!                print dip filter in the title block
!                if either condition is true the card will be printed
!                 in the title block.
!
! *g      a group sequence process card. lists multiple processes as one
!         process.
!            *g________\________\________(sq
!            ^ ^        ^        ^
!   col      1 3        11       19
!
!
!                 *g       - code for group sequence in cols 1 and 2
!         cols    3 -10    - the process names follow in 8 column
!                11 -18       fields, left-justified.
!                19 -26
!                 etc
!
!                 (        - left parenthesis is the delimiter which
!                             follows the last process in the group
!                             sequence
!
!                 sq or    - sequential or sparse
!                 sp           sequential = flag this series of
!                                           processes as a group
!                                           sequence when they appear
!                                           sequentially in the history
!                                           without any processes
!                                           between.
!                              sparse     = flag this series as a group
!                                           sequence. ignore processes
!                                           which fall between.
!
! #g      group sequence parameter card.
!            #g(__-__)(__-__)(__-__)
!                 #g       - code for group process parameter card.
!                            the portion enclosed in parenthesis is
!                            considered 1 parameter. the parameter con-
!                            sists of two numbers separated by a hyphen.
!                            the first number refers to the sequential
!                            process from the *g card. the second number
!                            refers back to the parameter cards
!                            associated with this same process.
!                            following the ** process name. use that
!                            sequential parameter number.
!
!                  print cards are the same format as normal title
!                  processes. the parameter number you enclose in
!                  brackets refers to the sequential number from the
!                  #g card.
!
!            example:
!            *gnmo     gathr   fkdip   nmo     (sq
!            #g(1-2)
!            !!dmult : f-k de-multiple process   ** cray **
!            !!          ratio = [1]
!
!               if the above example were using the sparse option, the
!               first card would also need a replacement process as...
!
!               *gnmo     gathr   fkdip   nmo     (sp)gathr
!
!               the processes that fall in the sparse sequence nmo,
!               gathr,fkdip,nmo will be replaced by the above sparse
!               group sequence in the position where gathr
!               was. any processes that fall between the group sequence
!               processes will be printed in their normal position.
! #g      group sequence parameter card.
!            #g(__-__)(__-__)(__-__)
!                 #g       - code for group process parameter card.
!                            the portion enclosed in parenthesis is
!                            considered 1 parameter. the parameter con-
!                            sists of two numbers separated by a hyphen.
!                            the first number refers to the sequential
!                            process from the *g card. the second number
!                            refers back to the parameter cards
!                            associated with this same process.
!                            following the ** process name. use that
!                            sequential parameter number.
!
!                  print cards are the same format as normal title
!                  processes. the parameter number you enclose in
!                  brackets refers to the sequential number from the
!                  #g card.
!
!            example:
!            *gnmo     gathr   fkdip   nmo     (sq
!            #g(1-2)
!            !!dmult : f-k de-multiple process   ** cray **
!            !!          ratio = [1]
!
!               if the above example were using the sparse option, the
!               first card would also need a replacement process as...
!
!               *gnmo     gathr   fkdip   nmo     (sp)gathr
!
!               the processes that fall in the sparse sequence nmo,
!               gathr,fkdip,nmo will be replaced by the above sparse
!               group sequence in the position where gathr
!               was. any processes that fall between the group sequence
!               processes will be printed in their normal position.
!
!
! $$      a print card to be used if there are no titl cards found in
!         history file.
!
!         the $$ cards consist of sub-fields as follows: head, field,
!         digital, other, cable, cab2s3s, owner and id. the first card
!         in all sub-fields must be a type card in the form shown below:
!              type=head,height=.15
!                  where:
!                           type     = one of the 7 sub-field names
!                           height   = the height of the characters
!                                      (inches) in the sub-field
!
!         in sub-fields field, other, and cable the second card must
!         be a sub-field title card. i.e.
!           type=field,height=.1
!           acquisition
!
!         sub-field owner prints the following message...
!         these data are owned by and are a trace secret of (owner
!         inserted here).  the use of these data is restricted to
!         parties holding a valid license and is subject to the
!         confidentiality terms of that license.
!
!         in sub-field owner, the second card must be a sub-field titl
!         card. the third card is the name of the owner. i.e.
!           type=owner,height=.1
!           owner
!           conoco
!
!           the head sub-field must be first and the id sub-field last.
!           all other sub-fields may be included or left out as deemed
!           appropriate.
!
!           special consideration:
!
!           head           sub-field
!           ____
!
!           the maximum number of characters for head cards is 45.
!           the word logo in the first 4 characters of a card will cause
!           the conoco logo to be plotted. the word dubi will cause the
!           dubai petroleum company logo to be plotted.
!           if the first 7 characters in a card are conoco, the card
!           will be plotted with characters 1.5 times the height called
!           for.
!
!           fkplot beginning in col 3 indicates cards to follow have
!           parameters for doing an fkplot. the first card has 2 fields.
!           the remaiining cards have 8 fields. end plot beginning in
!           col 3 is the terminator for fkplot.
!              the fields for card 1 are:
!                 (1) # of points in the transform
!                 (2) maximum frequency
!              the fields for remaining cards are 4 frequency, wave
!              number pairs.
!
!
!
!           cable          sub-field
!           _____
!
!           the information card for the 1 streamer cable diagram
!           must have 8 fields. the fields from left to right are:
!               (1) antenna to source
!               (2) source to near group (min offset)
!               (3) group interval (rec interval)
!               (4) total length (max offset)
!               (5) cable length (spread)
!               (6) last trace number
!               (7) next to last trace number
!               (8) far trace number
!           an x in col 80 indicates there is another cable information
!           card. card 2:
!               (1) receiver interval from far trace to next receiver.
!               (2) trace number of receiver next to far trace.
!           fields are enclosed in brackets.
!
!
!           cab2s3s        sub-field
!           _______
!
!           the information card for the 2 source 3 streamer cable
!           diagram must have 7 fields. the fields from left to right
!           are:
!               (1) distance between the two sources
!               (2) distance between the gun and cable
!               (3) near trace number
!               (4) far trace number
!               (5) antenna to gun distance
!               (6) gun to cable distance
!               (7) cable length
!           fields are enclosed in brackets.
!
! $p      a print card which is not associated with a process. these
!         cards will print as part of the digital processing sequence
!         right after the displayed trace interval.
!
! $*      option cards.
!         $*02 = print ve and ref from last occurrance of fgd.
!         $*03 = print all occurrances of splt and colr. otherwise,
!                prints only current splt and colr.
!         $*04 = do not print basement interval.
!         $*05 = make two passes at histories to pick up histories not
!                used in first pass (not implemented)
!         $*06 = use the current history only for title block.
!         $*07 = convert basement interval to meters and display in
!                both meters and feet.
!         $*08 = print only that portion of a parameter answer that
!                comes after a right bracket
!         $*09 = get heading information for digital processing sequence
!                after the type=digital card in the master file. if
!                omitted, title will use a left-justified heading of
!                "DIGITAL PROCESSING SEQUENCE"
!         $*10 = process histories sequentially rather than tracking
!                by reel number.
!
!
!
!
!        example of the instruction file
!
! $$TYPE=HEAD,HEIGHT=.15
! $$AREA [                              ]
! $$QUAD [                                      ]
! $$LINE [        ] TYPE DATA [   ] CDP
! $$LOGO
! $$NORTH AMERICAN EXPLORATION SERVICES
! $$EXPLORATION PROCESSING
! $$LAFAYETTE DIVISION
! $$TYPE=FIELD,HEIGHT=.1
! $$FIELD RECORDING
! $$PURCHASED FROM [                                   ]
! $$RECORDED BY [               ] DATE [               ]
! $$MAX CDP [    ] ANALOG [ ] DIGITAL [X] SAMPLE RATE [ 4 MS]
! $$SOURCE INTERVAL [     FT] SEIS GROUP INTERVAL [     FT]
! $$SPREAD [                                      ]
! $$ENERGY SOURCE [VIBROSEIS ] NUMBER OF VIBRATORS [  ]
! $$SWEEP [               ] SWEEP LENGTH [     ] SEC
! $$RECORDER UNIT [       ]  FILTER [            ]
! $$SOURCE:    SAMPLES [    ] DIMENSIONS [   FT]
! $$RECEPTORS: SAMPLES [    ] DIMENSIONS [   FT]
! $$TYPE=DIGITAL
! $*02
! **TVF
! ##F1(1),F2(1),PHASE(1),TLAST(1)
! ##F1(2),F2(2),TLAST(2)
! ##F1(3),F2(3),TLAST(3)
! ##F1(4),F2(4),TLAST(4)
! #L4,4,4,4,4,4,4,4,4,4,4,4,4
! !/1^"      "/TVF :  DIGITAL FILTER   PHASE[3]  ** CPS TVF **
! !/1^"      "/       ["0.00-",4]SEC  [1,2]HZ
! !?       [4,7]SEC  [5,6]HZ
! !?       [7,10]SEC  [8,9]HZ
! !?       [10,13]SEC  [11,12]HZ
! **XP
! ##DBRI,WLN,WINC
! !/1^"   "/XP  :  EXPANSION, TRACE BY TRACE  ** CPS XP **
! **MUTE
! ##OPT
! !/1^" "/MUTE   ** CPS MUTE **
! !/1="5"/          INSIDE MUTE
! !/1="6"/          INSIDE MUTE
! **SORT
! ##HG,HO,GINC
! !/1^"   "/SORT:  TRACE SORT  BIN SIZE [3]  ** CPS SORT **
! !/1^"   "/          PRIMARY SORT [1]  SECONDARY SORT [2]
! **DTMW
! ##OVLP,TWIN(1),BWIN(1),OPLEN(1),GAP(1),TWIN(2),BWIN(2),OPLEN(2),GAP(2)
! ##TWIN(3),BWIN(3),OPLEN(3),GAP(3),TWIN(4),BWIN(4),OPLEN(4),GAP(4)
! !!DTMW:  DECONVOLUTION  OVERLAP [1] SEC     ** CPS DTMW **
! !!           TWIN      BWIN    OPERATOR    GAP    (SECONDS)
! !!         [2]  [4]  [4]  [5]
! !?         [6]  [7]  [8]  [9]
! !?         [10]  [11]  [12]  [13]
! !?         [14]  [15]  [16]  [17]
! **DECON
! ##OPLEN,GAP,DLD
! !!DECO:  DECONVOLUTION  ** CPS DECON **
! !!                              OPERATOR [1]SEC
! !/2^" 0.000"/            GAP [2]SEC
! **GSTK
! ##SCALING
! !!GSTK:  CDP STACK  ** CPS GSTK **
! !?            [1] MUTE SCALING
! **SHIF
! ##SHIFT
! !!SHIF:  SEA LEVEL CORRECTION [1]SEC  ** CPS SHIF **
! **NMO
! ##NMOT,RATO
! !!NMO :  NORMAL MOVEOUT CORRECTION  ** CPS NMO **
! !/1^"FORWARD "/       REVERSE NMO
! **GATHR
! ###TPG,HG#,HS#
! !!GATH:  GATHER GROUPS    ** CPS GATHR **
! !!       HEADER WORD TOGATHER [2]  GROUP SIZE [1]  SORT ON [3]
! **FKMAS
! !!FKMA:  FK MIGRATION    ** CPS FKMAS **
! **FKDIPF
! !!FKDI:  FK DIP FILTER  ** CPS FKDIPF **
! **EZSORT
! !!SORT: EASY SORT
! $$
! $$TYPE=OTHER,HEIGHT=.1
! $$OTHER PROCESSES
! $$[     ] [                                       ]
! $$[     ] [                                       ]
! $$[     ] [                                       ]
! $$TYPE=NOTES,HEIGHT=.1
! $$NOTES
! $$[                                               ]
! $$[                                               ]
! $$TYPE=ID,HEIGHT=.1
! $$        TOTAL MILES [      ]         [SAW]
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS
!
!
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS
!
!
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
!
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!                 i     i   i   i    i      o     i       
!      call atblk(ititl,ipn,job,bsmt,jprint,error,print_lun)
!
! character(len=*) ititl    name of instruction file
! integer          ipn      the cps process number of the calling process
! character(len=*) job      name of job currently being executed
! integer          bsmt     the basement interval between two traces
! integer          abort    abort the job
! integer          error    abort atblk but continue with job
! logical          jprint   print flag  false=supress atblk printout
!                                        true=normal printout
! integer          error    error flag
!                             0=OK
!                             1=abort atblk but continue with job
!                             2=abort job
! integer         print_lun unit number for printed output
!
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author         Description
!     ----        ------         -----------
!005. 2006-09-18  D. Glover      Added NULLIFY statements for Intel compiler.
!  4. 2006-01-10  B. Menger      Removed Unused Variables.
!  3. 2001-10-18  Karen Goodger  Insure current history done when there are
!                                no previous histories 
!  2. 2001-08-27  Karen Goodger  Pass option MODEL to manhist when getting 
!                                history information.
!  1. 2001-08-09  Karen Goodger  Converted from old system atb primitive.
!
!
!                 OLD REVISION HISTORY
!
!
!     date     author    description
!     ----     ------    -----------
! 09. 12-08-98 k.goodger change some close statements containing
!                        a character name to a hollerith name to
!                        satisfy fortran90.  begin using the
!                        fortran90 compiler.
! 08. 10-07-98 k.goodger add qtrin and qtrot as valid i/o programs.
! 07. 07-28-98 k.goodger use numjob=1 as flag to allocate memory for
!                        only one history.  the splt and colr front
!                        ends set their njob parameter to 1 if they
!                        find a $*06 in the master file.
! 06. 07-23-98 k.goodger set current history number to nhist rather
!                        than nhist+1.
! 05. 05-19-98 k.goodger cleared the igsflg array upon entering gseq.
! 04. 04-01-98 k.goodger increased the number of inputs/outputs allowed
!                        from 20 to 100.
! 03. 03-23-98 k.goodger added rtrin as valid input.  made fixes based
!                        on parameter changes in rtrot.
! 02. 02-16-98 k.goodger wrote a routine called atblk_cljus which should
!                        be used in place of atblkljus when character
!                        data is being passed.
! 01. 02-02-98 k.goodger change abort message in bld to an informative
!                        message when unable to find starting point
!                        for merge.  add a save statement to the
!                        beginning of each subroutine so that the
!                        static compile option will no longer be
!                        necessary.
!*********************************************************************
! 40. 11-21-97 k.goodger fix problem with similar paramter recognition
!                        concerning linked arrays in routine load.
! 39. 10-09-97 k.goodger added rtrot as a valid output process. look
!                        down 9 cards for ttrin reel number rather than
!                        5.
! 38. 06-23-97 k.goodger set diag flag back to zero.
! 37. 06-20-97 k.goodger fix bug with option 05 not merging in all the
!                        new processes.
! 36. 05-27-97 k.goodger special option $*05 to handle two processing
!                        streams.
! 35. 12-26-96 k.goodger add dint and dout as valid i/o processes.
!                        routines changed are io and gans.
! 34. 08-03-95 k.goodger change to accept a file name to write to.
!                        using common block titletitlef.
!                        encountering problems writing to same file
!                        name under unicos 8.0.
! 33. 05-03-95 k.goodger add type=owner field. all changes were made
!                        to splt and colr.  documentation only change
!                        in title.
! 32. 03-29-95 k.goodger abort title block if output card exceeds 80
!                        characters. routine titleprcrd.
! 31. 04-27-94 k.goodger document 2 source 3 streamer cable diagram.
! 30. 01-06-94 k.goodger increase keys array from 380 to 1000.
! 29. 10-21-93 k.goodger add special option flag $*08 which will print
!                        only that part of a parameter answer that comes
!                        after the right bracket.
! 28. 05-24-93 k.goodger new format for ttrin.
! 27. 09-23-92 k.goodger correct tracking problem in routine bld.
! 26. 08-19-92 k.goodger recompile for tracking problem.
! 25. 08-13-92 k.goodger get *   titl cards out of history and replace
!                        the $$ cards in the master file with these
!                        cards.
!                        fix problem tracking histories. bld routine.
! 24. 06-10-92 k.goodger add and and or operators to conditional print
!                        cards.
!                        increase number of reels checked from 10 to 20.
! 23. 05-14-92 k.goodger get back ljus routine which had been lost.
! 22. 04-14-92 k.goodger increase number of parameters allowed inside
!                        brackets from 5 to 20.
!                        add process pulse to titleio routine.
! 21. 03-20-92 k.goodger remove 100 card limit.
! 20. 03-03-92 k.goodger fix problem in gseq routine handling processes
!                        which occur both as group and non-group
!                        processes.
! 19. 02-20-92 k.goodger code to handle tcat problem. tcat was getting
!                        added to the history file erroneously prior to
!                        12-17-91 10:09.
! 18. 02-06-92 k.goodger add avost as valid output process.
! 17. 01-27-92 k.goodger make sure group sequence counter gets
!                        incremented for locating a second occurrence of
!                        a group sequence.
! 16. 01-08-92 k.goodger closed the master file. was never using the
!                        second master file in a job.
! 15. 01-06-92 k.goodger remove trap of gseq users.
! 14. 12-30-91 k.goodger redo gseq routine to allow more parameters
!                        per group sequence.
! 13. 10-28-91 k.goodger add stka as a valid output program.
! 12. 10-21-91 k.goodger correct search for history number rather than
!                        index in routine jst.
! 11. 08-26-91 k.goodger getting vertical bars interpolated as a blank
!                        on short answers as well as long.
! 10. 06-18-91 k.goodger set print flag if it does not come in as an
!                        argument.
!  9. 05-21-91 k.goodger increase keys array from 200 to 380 to be
!                        compatible with max number of histories.
!  8. 04-26-91 k.goodger increase keys array from 100 to 200.
!  7. 04-08-91 k.goodger undo revision 5. this causes a problem with
!                        custom code only. putting quotes around the
!                        \eof will resolve the problem on unicos
!                        jobs with custom code.
!  6. 02-18-91 k.goodger print only last occurrence of colr unless
!                        special option 3 is flagged.
!  5. 01-22-91 k.goodger use ishell rm command if unicos job.
!                        put \ in front of $ char for unicos job. this
!                        is a special character interpreted by the
!                        shell.
!                        add endfile after write to itlf. add rewind
!                        before read itlf.
!  4. 01-14-91 k.goodger routine bld, do a search to find proper index
!                        jst array rather than assuming sequential
!                        history numbers.
!  3. 01-09-91 k.goodger routine bld, start with index equal to 1 rather
!                        than istart.
!                        ignore histories begining with program sonic.
!                        allow up to 20 answers per line.
!  2. 01-08-91 k.goodger routine bld figures out what histories will be
!                        used in the title block and saves in array
!                        ipath. determining what history to use next
!                        now comes from this array.
!  1. 01-03-91 k.goodger add code to routine bld for determining history
!                        to start. previously always started with first
!                        history but could not always find a path to the
!                        current history. this code attempts to find the
!                        first history it can use which will produce a
!                        path to the current history.
! 12-20-90  k.goodger    routine load, fix problem indexing to last
!                        array parameter.
! 12-18-90  k.goodger    ignore history beginning with rnsyn.
! 12-06-90  k.goodger    use only 1st 10 pdn's on TTROT.
! 11-06-90  k.goodger    add special option $*04 to supress printing
!                        the trace interval.
! 10-25-90  k.goodger    fix to look for second strin to see if merge=
!                        yes option is being used. also look for ttrin
!                        followed by strin for merge=add or yes.
! 10-25-90  k.goodger    make changes due to cft77 compiler.
! 10-11-90  k.goodger    fix to look for second strin to see if merge=
!                        add option is being used.
! 09-06-90  k.goodger    fix problem occurring when array is last
!                        variable in history. (card not ending in a
!                        comma)
! 08-28-90  k.goodger    new type of print card with code which will
!                        increment 1/2 space. implemented by putting a
!                        pound sign in 1st space of print information.
!                        documentation change only.
! 08-02-90  k.goodger    look at only 1st 10 reels in ttrin, routine
!                        titleio.
! 07-24-90  k.goodger    look for ve and ref from fgd program as well
!                        as jd.
! 07-23-90  k.goodger    blank out parameter number on print card when
!                        can find no answer in history. routine prcrd.
! 06-11-90  k.goodger    check pdn field to determine end of reels
!                        on ttrin entries.
! 06-06-90  k.goodger    add paste as valid input program.
! 04-19-90  k.goodger    increase history buffer to 3980.
! 04-11-90  k.goodger    remove print flag on message before error
!                        return.
! 03-28-90  k.goodger    remove 50 process limitation from group
!                        sequence option.
! 01-15-90  k.goodger    add no print argument.
! 01-10-90  k.goodger    allow more than 1 length card.
! 01-08-90  k.goodger    if don't find equal sign after parameter name
!                        assume this is some other character string and
!                        go on searching for parameter name. routine
!                        load.
! 12-27-89  k.goodger    increase group sequence process fields from 4
!                        to 8 characters.
!                        add special option 10 which processes histories
!                        sequentially rather than tracks them by reel
!                        number.
! 12-11-89  k.goodger    increase process names from 4 to 8 characters.
! 12-11-89  k.goodger    print field recording information to online.
! 12-05-89  k.goodger    in routine gseq fix statement which had been
!                        commented out.
! 11-09-89  k.goodger    abort title in no history file available.
! 11-01-89  k.goodger    replace brackets in cvarans array with blanks.
! 10-13-89  k.goodger    add stkf as valid output process.
! 10-04-89  k.goodger    increase first call to getscr from 4000 to
!                        10160. bhist now uses this much scratch.
! 09-21-89  k.goodger    correct problem with group sequence parameter
!                        number being destroyed. routine gseq.
! 09-20-89  k.goodger    go back and look for another occurrence of
!                        a group sequence after one has been rejected
!                        for not being sequential. routine gseq.
! 09-13-89  k.goodger    fix problem caused by new format of ttrot.
! 09-07-89  k.goodger    fix problem when only one history record.
! 08-22-89  k.goodger    fill cvanans array with blanks. routine load.
! 08-21-89  k.goodger    fix problem with not reading all of the
!                        histories in the titleref routine.
! 08-19-89  k.goodger    increase long answer array to from 3 to 10.
!                        scan for vertical bar in long answer array
!                        and replace it with a blank.
! 08-19-89  k.goodger    fix problem with last parameter in history
!                        being longer than 24 characters. routine load.
! 08-14-89  k.goodger    release master file on controlled abort.
! 08-13-89  k.goodger    fix problem indexing outside jobchk array.
!                        routine bld.
! 08-10-89  k.goodger    fix problem finding nth occurrence. counter
!                        not always getting incremented. routine load.
! 08-09-89  k.goodger    fix bug caused by history keys no longer
!                        beginning with 1 due to keeping only cray
!                        histories.
! 08-02-89  k.goodger    keeps only cray histories.
! 08-01-89  k.goodger    allow more than one occurrence of the same
!                        group sequence. routine titlegseq.
! 07-28-89  k.goodger    fix problem with getting the last parameter in
!                        a process which is not terminated by a comma.
! 07-26-89  k.goodger    fix problem with counting group sequence print
!                        cards which solves a memory allocation problem.
! 07-25-89  k.goodger    add additional checks to determine if a
!                        group sequence is sequential. routine titlegseq.
! 07-25-89  k.goodger    insure that ncard never exceeds the mxpr
!                        parameter set in splt. add some additional
!                        abort messages.
! 07-23-89  k.goodger    add code to prevent indexing outside normal
!                        array. routine titleload.
! 07-22-89  k.goodger    add code to determine the last card in a linked
!                        array to prevent indexing passed it. routine
!                        titleload.
! 07-21-89  k.goodger    fix problem finding the nth occurrence of a
!                        string. routine titleload.
! 07-19-89  k.goodger    fix index problem on isp histories. routine
!                        titleio.
!                        limit number of allowable input reels to 10.
!                        was set to 100. routine titleio.
! 07-18-89  k.goodger    increased hard-dimensioned array cvarans in
!                        roution titleload from 20 to 100. this allows
!                        100 possible parameter answers from a single
!                        process.
! 06-27-89  k.goodger    allow for another type of vci format when
!                        looking for output reel. routine titleio.
! 06-21-89  k.goodger    clear out history buffer before each read.
!                        add 20 word array to gseq routine to save
!                        whether a group sequence is sequential or
!                        sparce.
! 06-15-89  k.goodger    reduce memeory allocated by maxproc.
! 06-08-89  k.goodger    additional checking on linked array parameters.
! 06-06-89  k.goodger    ignore input's on SEG demultiplex jobs.
!                        print only current splt unless special option
!                        03 is flagged.
! 05-31-89  k.goodger    allow two title's to be run in a job.
! 05-30-89  k.goodger    update documentation on sparce option of group
!                        sequence.
! 05-30-89  k.goodger    enable group sequence option.
!                        correct problem getting information from last
!                        process in a history.
! 05-17-89  k.goodger    first version
!
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
!                      SPECIAL COMPILING REQUIREMENTS
!
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
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



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module atblk_module
      use array_module
      use getlun_module
      use hist_module
      use histprim_module
      use manhist_module
      implicit none

      private
      public :: atblk

      character(len=100),public,save :: atblk_IDENT = &
'$Id: atblk.f90,v 1.5 2006/09/18 13:32:36 Glover prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!!
!!! If a data structure is needed, it can be defined here.
!!! The data structure can be made public or private.
!!! OTHERWISE OMIT THIS SECTION.




!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


!!! If any interface blocks are needed, they can be placed here.
!!! OTHERWISE OMIT THIS SECTION.


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      integer, parameter :: cardlen=80
      integer :: maxcards,plun
      integer :: kntproc,kntpar,kntprt,kntgseq,kntgskd,kntgprt,kntgpar
      integer :: iatbf=0,idiag,ifrst,igsf=0,iprint,iproc
      integer :: ispcrd(20),ispf=0
      integer :: mstrf=0,ncard,nprocc=0
      integer :: numproc                           !number of processes
      integer :: whichcard

      character(len=cardlen),pointer :: ibuf(:) !array to hold 1 history record

      integer,pointer :: nireel(:)
      integer,pointer :: noreel(:)
      integer,pointer :: njst(:)
      integer,pointer :: njstt(:)
      integer,pointer :: reel(:,:)
      integer,pointer :: oreel(:,:)
      character(len=80),pointer :: output_names(:)
      character(len=8) :: histype,mach,version
      integer,pointer :: jst(:,:)
      integer,pointer :: jbchk(:)
      character(len=8),pointer :: proc(:)      !process names from master file
      integer,pointer :: mvar(:)               !beginning indexes for variables
      integer,pointer :: nvar(:)               !ending indexes for variables
      integer,pointer :: mpc(:)                !starting index for print cards
      integer,pointer :: npc(:)                !ending index for print cards
      integer,pointer :: ivar(:)               !lengths of variables
      character(len=24),pointer :: cvar(:)
      character(len=1),pointer :: pcom(:)      !second character of code
      character(len=16) :: jobname
      character(len=32) :: vol,asc_time
      character(len=80),pointer :: pcard(:)    !print cards
      character(len=680),pointer :: lcard(:)
      character(len=680),pointer :: lcard2(:)
      integer,pointer :: lproc(:)
      integer,pointer :: lproc2(:)
      character(len=1),pointer :: flag(:)      !array to flag inserted processes
      character(len=1),pointer :: pflag(:)
      integer,pointer :: whathist(:) !array parallel to lproc which indicates
                                     !what history that process came from

      integer,pointer :: whathist2(:)!array parallel to lproc2 which indicates
                                     !what history that process came from

      character(len=680),pointer :: tcard(:)
      character(len=680),pointer :: tproc(:)
      character(len=680),pointer :: tvarf(:)
      character(len=80),pointer :: pgcrd(:)
      integer,pointer :: mgpc(:)
      integer,pointer :: ngpc(:)
      integer,pointer :: stgrp(:)
      integer,pointer :: endgp(:)
      integer,pointer :: mgvar(:)
      integer,pointer :: ngvar(:)
      integer,pointer :: rpprc(:)
      character(len=304),pointer :: lgcard(:)
      integer,pointer :: gsflg(:,:)
      integer,pointer :: lgprc(:)
      integer,pointer :: igvar(:,:)
      integer,pointer :: tpflg(:,:)
      integer,pointer :: path(:)
      integer         :: nhist



      contains



!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

      subroutine atblk(ititl,ipn,job,bsmt,jprint,error,&
                       print_lun)

!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*), intent(in) :: ititl,job
      integer , intent(in) :: ipn,print_lun
      integer, intent(out) :: error
      real, intent(in)     :: bsmt
      logical, intent(in)  :: jprint
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------



      integer :: i,icol,igrpflg,inhist,irgsflg,istat,j
      integer :: lasthist,lgseq,maxproc,mxprc2,n,nw,n1,n2,n3,n4
      integer :: k 
      character(len=2) :: code
      character(len=4) ::      cno='NO' 

      character(len=78) :: card

      numproc=0
      whichcard=0
      iproc=0
      nprocc=0
      igsf=0
      ispf=0
      mstrf=0
      ifrst=0
      ncard=0
      plun=print_lun   ! Set output message unit number to global var
      error = 0
      if(ispf.eq.0)then
        call getlun(ispf,istat)
        if(istat.ne.0)then
          write(plun,*)'ATBLK-->Unable to get unit number for spf file'
          error=2
          return
        endif
        open(ispf,file='ATBSPRT',status='replace',iostat=istat)
        if(istat.ne.0)then
          write(plun,*)'ATBLK-->Unable to open file ATBSPRT'
          error=2
          return
        endif
      endif
      if(igsf.eq.0)then
        call getlun(igsf,istat)
        if(istat.ne.0)then
          write(plun,*)'ATBLK-->Unable to get unit number for gsf file'
          error=2
          return
        endif
        open(igsf,file='ATBGSF',status='replace',iostat=istat)
        if(istat.ne.0)then
          write(plun,*)'ATBLK-->Unable to open file ATBGSF'
          error=2
          return
        endif
      endif
      if(mstrf.eq.0)then
        call getlun(mstrf,istat)
        if(istat.ne.0)then
          write(print_lun)'ATBLK-->Unable to get unit number for master file'
          error=2
          return
        endif
      endif
      open(mstrf,file=ititl,status='old',iostat=istat)
      if(istat.ne.0)then
        write(print_lun,*)'ATBLK-->Unable to open master file'
        error=2
        return
      endif
!
!           open 'atbf' output file
      if(iatbf.eq.0)then
        call getlun(iatbf,istat)
        if(istat.ne.0)then
          write(print_lun)'ATBLK-->Unable to get unit number for atbf file'
          error=2
          return
        endif
      endif
      open(iatbf,file='atbf',status='replace',iostat=istat)
      if(istat.ne.0)then
        write(print_lun,*)'ATBLK-->Unable to open atbf file'
        error=2
        return
      endif
!
!          set diagnostic print flag
      idiag = 0
!
!          set print/no print flag
      iprint=0
      if(jprint)iprint=1
!
!
!          count the number of histories
!           8 arrays needed dimensioned by number of histories
!           5 - one dimensional
!             nireel
!             noreel
!             njst
!             njstt
!             jobchk
!           3 - 100 x nhist (100 input & output reels per history - 100
!                columns in job-step table)
!             reel
!             oreel
!             jst
!
!
      call manhist_track
      call manhist_get_info(nhist,maxcards,'MODEL')

      call array_alloc(ibuf,maxcards)

      inhist = nhist
      lasthist = nhist
!
      if (nhist <= 0) then
        write(plun,*) ' ATBLK - NO HISTORIES AVAILABLE'
        write(plun,*) '         Only current history will be done'
!!        write(plun,*) ' NO TITLE BLOCK WILL BE DONE'
!!        error = 2
!!        return
      endif
!          calculate number of words needed for arrays dimensioned
!          by nhist
!
      nw = nhist*4 + nhist*20*3
!
!          also allocate memory based on master file information
      kntproc = 0
      kntpar = 0
      kntprt = 0
      kntgprt = 0
      kntgpar = 0
      kntgseq = 0
      kntgskd = 0
      lgseq = 0
!
      if (idiag == 1) write(plun,*) &
        ' PROCESS AND CORRESPONDING PROCESS NUMBERS ARE...'
  200 continue
      read (mstrf, 9001, end=1000) code, card
  205 continue
      if (code == '*G') go to 400
      if (code == '**') then
        kntproc = kntproc + 1
        if (idiag == 1) write(plun,*) ' ', kntproc, ' ', card(1:8)
      endif
      if (code == '##') then
        icol = 0
        kntpar = kntpar + 1
  250   continue
        k = index(card(icol+1:),',') + icol
        if (k == icol) go to 200
        icol = k
        kntpar = kntpar + 1
        go to 250
      endif
!
      if (code(1:1) /= '!') go to 200
      kntprt = kntprt + 1
      go to 200
!
  400 continue
      kntgskd = kntgskd + 1
      k = index(card,'(')
      n = (k - 1)/8
      lgseq = max0(lgseq,n)
      kntgseq = kntgseq + n
  450 continue
      read (mstrf, 9001, end=1000) code, card
      if (code(1:1) == '*') go to 205
      if (code(1:1) == '!') kntgprt = kntgprt + 1
      if (code(1:1) == '#') then
        do i = 1, 80
          if (card(i:i) /= '(') cycle
          kntgpar = kntgpar + 1
        end do
      endif
      go to 450
!
!
 1000 continue
      maxproc=200
      mxprc2 = 20
      if (maxproc < 20) mxprc2 = maxproc
      if (iprint == 1) then
        write(plun,*) ' THERE ARE ', kntproc, ' PROCESSES '
        write(plun,*) ' THERE ARE ', kntpar, ' PARAMETERS'
        write (plun,*) ' THERE ARE ', kntprt, ' PRINT CARDS'
        write (plun,*) ' THERE ARE ', kntgprt, ' GROUP SEQUENCE PRINT CARDS'
        write (plun,*) ' PROCESS NUMBER IS ', ipn
        write (plun,*) ' MXPRC2 = ', mxprc2
        write (plun,*) ' NUMBER OF HISTORIES IS ', nhist
        write (plun,*) ' THERE ARE ', kntgskd, ' GROUP SEQUENCES'
        write (plun,*) ' THERE ARE ', kntgseq, &
          ' TOTAL PROCESSES USED IN GROUP SEQUENCES'
        write (plun,*) ' THERE ARE ', kntgpar, &
          ' TOTAL PARAMETERS USED IN GROUP SEQUENCES'
      endif
      rewind mstrf
!          need 5 arrays dimensioned by kntproc
!              5 one-dimensional
!                proc
!                mvar
!                nvar
!                mpc
!                npc
      n1 = 5*kntproc
!
!          need 2 arrays dimensioned by kntpar
!              1  one-dimensional
!                 ivar
!              1 x 24 characters (3 words)
!                 cvar
      n2 = 1*kntpar + 3*kntpar
!
!
!          need 3 arrays dimensioned by kntprt
!            2  one dimensional
!                pcom
!                pcard
!            1  x 80 characters (10 words)
!                pcard
      n3 = 2*kntprt
      n4 = 10*kntprt
!
      nw = nw + n1 + n2 + n3 + n4
!
!           need 8 arrays dimensioned by maxproc
!               6  one-dimensional
!                   lproc
!                   lproc2
!                   flag
!                   pflag
!                   whathist
!                   whathist2
!               2  x 680 characters
!                   lcard
!                   lcard2
      n1 = maxproc*6
      n2 = maxproc*85*2
      nw = nw + n1 + n2
!
!
!           need 3 arrays dimension by 20 or maxproc if less than 20
!               3 x 85 (680 characters)
!                  tcard
!                  tproc
!                  tvarf
      nw = nw + mxprc2*85*3
!
!          allocate memory for group sequence option
      if (kntgseq /= 0) then
!
!          need 7 arrays dimensioned by kntgskd  (the number of group
!                                                 sequenceses)
!              7 one-dimensional
!               mgpc
!               ngpc
!               stgrp
!               endgp
!               mgvar
!               ngvar
!               rpprc
        n1 = kntgskd*7
        nw = nw + n1
!
!          need 1 array dimensioned by kntgprt (the number of group
!                                               sequence print cards)
!            1 x 80 char (10 words)
!               pccrd   ?????????
        n1 = kntgprt*10
        nw = nw + n1
!
!          need 1 arrays dimensioned by kntgseq (the total number of
!                                                process names in all
!                                                group sequences)
!              1 one-dimensional
!               lgprc
        n1 = kntgseq
        nw = nw + n1
!
!          need 1 array dimensioned by kntgpar ( the total number of
!                 parameter names in all group sequences)
!                     kntgpar rows by 3 cols
!                igvar
        n1 = kntgpar*3
        nw = nw + n1
!          need 3 arrays dimensioned by maxproc
!              1 x 304 characters
!               lgcard
!                2 two-dimensional
!              maxproc x 3 cols
!               gsflg,tmpflg
        n1 = maxproc*38
        n2 = maxproc*3*2
        nw = nw + n1 + n2
!
!          need 1 array dimensined by lgseq (the largest group sequence)
!              1 x 2
!               tpflg
        n1 = lgseq*2
        nw = nw + n1
      endif
!
!
      call array_alloc (nireel, nhist)
      call array_alloc (noreel, nhist)
      call array_alloc (output_names,nhist)
      call array_alloc (njst, nhist)
      call array_alloc (njstt, nhist)
      call array_alloc (reel, nhist,100)
      call array_alloc (oreel, nhist,100)
      call array_alloc (jst,100,nhist)
      call array_alloc (jbchk, nhist)
      call array_alloc (proc, kntproc)
      call array_alloc (mvar, kntproc)
      call array_alloc (nvar, kntproc)
      call array_alloc (mpc, kntproc)
      call array_alloc (npc, kntproc)
      call array_alloc (ivar, kntpar)
      call array_alloc (cvar, kntpar*3)
      call array_alloc (pcom, kntprt)
      call array_alloc (pcard,kntprt)
      call array_alloc (lcard, maxproc*85)
      call array_alloc (lcard2, maxproc*85)
      call array_alloc (lproc, maxproc)
      call array_alloc (lproc2, maxproc)
      call array_alloc (flag, maxproc)
      call array_alloc (pflag, maxproc)
      call array_alloc (whathist, maxproc)
      call array_alloc (whathist2, maxproc)
      call array_alloc (tcard, mxprc2*85)
      call array_alloc (tproc, mxprc2*85)
      call array_alloc (tvarf, mxprc2*85)
      if (kntgseq /= 0) then
        call array_alloc (pgcrd, kntgprt)
        call array_alloc (mgpc, kntgskd)
        call array_alloc (ngpc, kntgskd)
        call array_alloc (stgrp, kntgskd)
        call array_alloc (endgp, kntgskd)
        call array_alloc (mgvar, kntgskd)
        call array_alloc (ngvar, kntgskd)
        call array_alloc (rpprc, kntgskd)
        call array_alloc (lgcard, maxproc)
        call array_alloc (gsflg, maxproc,3)
        call array_alloc (lgprc, kntgseq)
        call array_alloc (igvar, kntgpar,3)
        call array_alloc (tpflg, maxproc,3)
!
      endif
      nullify (nireel) ! jpa
      nullify (noreel) ! jpa
      output_names=' '
      nullify (njst) ! jpa
      nullify (njstt) ! jpa
      nullify (reel) ! jpa
      nullify (oreel) ! jpa
      nullify (jst) ! jpa
      nullify (jbchk) ! jpa
      proc=' '
      nullify (mvar) ! jpa
      nullify (nvar) ! jpa
      nullify (mpc) ! jpa
      nullify (npc) ! jpa
      nullify (ivar) ! jpa
      cvar=' '
      pcom=' '
      pcard=' '
      nullify (lcard) ! jpa
      nullify (lcard2) ! jpa
      nullify (lproc) ! jpa
      nullify (lproc2) ! jpa
      nullify (flag) ! jpa
      pflag=' '
      nullify (whathist) ! jpa
      nullify (whathist2) ! jpa
      tcard=' '
      tproc=' '
      tvarf=' '
      if (kntgseq > 0)then
        pgcrd=' '
        nullify (mgpc) ! jpa
        nullify (ngpc) ! jpa
        nullify (stgrp) ! jpa
        nullify (endgp) ! jpa
        nullify (mgvar) ! jpa
        nullify (ngvar) ! jpa
        nullify (rpprc) ! jpa
        nullify (lgcard) ! jpa
        nullify (gsflg) ! jpa
        nullify (lgprc) ! jpa
        nullify (igvar) ! jpa
        nullify (tpflg) ! jpa
      endif

!    read and interpret title block master file
      call atblk_gmas (igrpflg,error)
      if (error .ne. 0)return
      if (idiag == 1) then
        write(plun,*) ' VARIABLE NAMES SET IN GMAS ARE...'
        do i = 1, kntproc
          write(plun,*) ' VARIABLES FOR PROCESS ', i, ' ARE...'
          do j = mvar(i), nvar(i)
            write(plun,*) ' ', j, ' ', cvar(j)
          end do
        end do
      endif
!    Get the history numbers to use
      call array_alloc(path,nhist)
      call manhist_histsused(path)
!
!    get title cards of title block
      call atblk_titl (mstrf, inhist)

!    set up special, user-specified cards in title block output
      call atblk_spec (job, bsmt,cno)
      if (ispcrd(10) == 1)then
!!!        call array_alloc(path,nhist)
        do i=1,nhist
          path(i)=i
        enddo
        go to 50
      endif
 50   continue
!          build the processing sequence
      call atblk_bld(maxproc,error)
      if(error.ne.0)return

!
!          Show multiple processes as one process where necessary
      irgsflg=ncard
      if(igrpflg.eq.1)call atblk_gseq(irgsflg,error)
      if(error.ne.0)return
!
!          Generate output of title block from built sequence
      call atblk_gtbk

      close(iatbf,status='keep')
      close(mstrf)
      close(ispf)
      close(igsf)

!
      call array_free(ibuf)
      call array_free (nireel)
      call array_free (noreel)
      call array_free (output_names)
      call array_free (njst)
      call array_free (njstt)
      call array_free (reel)
      call array_free (oreel)
      call array_free (jst)
      call array_free (jbchk)
      call array_free (proc)
      call array_free (mvar)
      call array_free (nvar)
      call array_free (mpc)
      call array_free (npc)
      call array_free (ivar)
      call array_free (cvar)
      call array_free (pcom)
      call array_free (pcard)
      call array_free (lcard)
      call array_free (lcard2)
      call array_free (lproc)
      call array_free (lproc2)
      call array_free (flag)
      call array_free (pflag)
      call array_free (whathist)
      call array_free (whathist2)
      call array_free (tcard)
      call array_free (tproc)
      call array_free (tvarf)
      call array_free (path)
      if (kntgseq /= 0) then
        call array_free (pgcrd)
        call array_free (mgpc)
        call array_free (ngpc)
        call array_free (stgrp)
        call array_free (endgp)
        call array_free (mgvar)
        call array_free (ngvar)
        call array_free (rpprc)
        call array_free (lgcard)
        call array_free (gsflg)
        call array_free (lgprc)
        call array_free (igvar)
        call array_free (tpflg)
!
      endif
      return
!
!          abort return
 4090 continue
      write (plun,*) ' ERROR TRYING TO GET TITLE BLOCK MASTER FILE'
 5000 continue
      error=1
      return
!
 9001 format(a,a)
      return
!
      end subroutine atblk

      subroutine atblk_alen(card,errorflag)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*) , intent(in) :: card
      integer,intent(out) :: errorflag
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: j2, idone, n1, icol, i1, k, n2
      character :: comma, ctemp*10, cblk*10

!-----------------------------------------------
!
!          routine to add lengths of parameter answers to look-
!           up tables. optional
!
!           card   = card containing the lengths
!           ivar  = array to hold the lengths corresponding to the
!                        parameter names
!           mvar  = array containing starting indexes
!           nvar  = array containing ending indexes
!
!
!
!
!
!
      data comma/ ','/
      data cblk/ ' '/
      errorflag = 0
!
!
      idone = 0
      n1 = 1
      icol = 0
      i1 = i1 + 1
!
      if (ifrst == 1) go to 100
      ifrst = 1
      i1 = mvar(numproc)
      go to 200
!
  100 continue
      if (i1 > nvar(numproc)) go to 8000
  200 continue
      k = index(card(icol+1:),comma) + icol
      if (k == icol) then
!          there should be one more
        n1 = icol + 1
        n2 = index(card(n1:),' ') + n1
        idone = 1
        go to 300
      endif
      n2 = k - 1
  300 continue
      ctemp = cblk
      ctemp = card(n1:n2)
!
      call string_cc2ii(ctemp,ivar(i1),j2)
      if (j2 .ne. 1) go to 7000
      icol = k
      if (idone == 1) go to 8000
      i1 = i1 + 1
      n1 = icol + 1
      go to 100
!
!
 7000 continue
      write(plun, *) ' PROBLEM WITH PARAMETER LENGTH CARD'
      errorflag = 1
      return
!
 8000 continue
      return
      end subroutine atblk_alen


      subroutine atblk_apar(card)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*) , intent(in) :: card
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: icol, n1, ndx, k, n2
      character :: comma

      save comma, icol, n1, ndx, k, n2
!-----------------------------------------------
!
!          add cray parameters to look up tables
!
!            called by gmas
!
!
!           card   = card from master file containing parameters
!           nvar  = array containing ending indexes for cvar
!           cvar   = array containing the cray variable names
!
!
!
!
!
      data comma/ ','/
!
!
      icol = 0
      n1 = 1
!
  100 continue
      nvar(numproc) = nvar(numproc) + 1
      ndx = nvar(numproc)
      k = index(card(icol+1:),comma) + icol
      if (k == icol) then
!          there should be one more parameter
        n1 = icol + 1
        n2 = index(card(n1:),' ') + n1
        cvar(ndx) = card(n1:n2)
        go to 8000
      endif
!
      n2 = k - 1
      cvar(ndx) = card(n1:n2)
      icol = k
      n1 = icol + 1
      go to 100
!
 8000 continue
      return
!
      end subroutine atblk_apar


      subroutine atblk_aprc(card,status)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*),intent(in) :: card
      integer,intent(out) :: status
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
!-----------------------------------------------
!
!          add a process name to the list of processes
!
!             called by gmas
!
!
!
!           card   = the card with the process name
!           mpc   = array containing starting indexes for print cards
!           npc   = array containing ending indexes for print cards
!           proc  = array containing the names of the cray process from
!                     the master file
!           mvar  = array containing beginning indexes for variables
!           nvar  = array containing ending indexs for variables
!
!
!
!
!
!
!
!
!
      status=0
      numproc = numproc + 1
      if(numproc.gt.kntproc)then
        write(plun,*)'ATBLK_aprc: Too many processes encountered'
        write(plun,*)'            No title block will be done'
        status=1
      endif
      ifrst = 0
      nprocc = numproc
      proc(numproc) = card(1:8)
!
!          set starting and ending indexes for print cards
      mpc(numproc) = npc(numproc-1) + 1
      if (numproc > 1) npc(numproc) = npc(numproc-1)
!
!          set starting and ending indexes for variable list
      mvar(numproc) = nvar(numproc-1) + 1
      if (numproc > 1) nvar(numproc) = nvar(numproc-1)
!
      return
      end subroutine atblk_aprc

      subroutine atblk_aprt(code, card,status)
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*) , intent(in) :: code
      character(len=*) , intent(in) :: card
      integer,intent(out) :: status
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: ndx
!-----------------------------------------------
!
!          add print cards to look-up tables
!
!
!           card   = the print card
!           code   = card code
!           npc   = array containing ending indexes for pcom and
!                    pcard
!           pcom  = array containing second character of codes
!           pcard = array containing print cards
!
!
!
!
!
!
      status=0
      npc(numproc) = npc(numproc) + 1
      ndx = npc(numproc)
      if(ndx.gt.kntprt)then
        write(plun,*)'ATBLK_aprt: Error indexing into pcom and pcard arrays'
        write(plun,*)'            ndx = ',ndx,' kntprt = ',kntprt
        write(plun,*)'            No title block will be done'
        status=1
      endif
      pcom(ndx) = code(2:2)
      pcard(ndx) = card
!
      return
!
      end subroutine atblk_aprt

      subroutine atblk_aspc(card)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*) , intent(in) :: card
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: idum
      character :: cbuf*80

!-----------------------------------------------
!cc
!    routine set flags for printing special cards
!
!    interpret special print request
      write (cbuf, 16) card
   16 format(a2)
      read (cbuf, 17) idum
   17 format(i2)
      ispcrd(idum) = 1
      if (iprint == 1) write(plun, *) 'FIRM WIRED CARD #', idum, &
        ' REQUESTED IN MASTER'
!    finished
      return
      end subroutine atblk_aspc

      subroutine atblk_bld(maxproc,status)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer,intent(in)   :: maxproc
      integer,intent(out)  :: status
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer ::       i           ,key,ncard1                               

      integer :: istat  

!-----------------------------------------------
!cc
!    routine to build title block processing sequence
!          called by atblk
!
!          maxproc = The maximum number of processes allowed in the title
!                    block
!
!            nhist = the total number of history records
!           proc  = array containing the names of all the process in
!                    the master file
!
!
!
      status=0
      do i = 1, nhist
!
        key = path(i)
        call atblk_load (key,maxproc,istat)
        if (istat == 1) go to 5000
        ncard1 = ncard
        if (ncard <= maxproc) cycle
        ncard = maxproc
        return
      end do
!           Now load the current history
      call atblk_load(-9999,maxproc,istat)
      if(istat.eq.1)go to 5000
      return
!
!!!  The code below is for making a second pass as the histories and
!!!    determining if there is another path - may want to implement later
!     if (ispcrd(5) == 0) then
!       go to 990
!     else
!       ncard = 0
!     endif
!
!          determine first history which was not used
!     istart = 0
!     do i = 1, nhist - 1
!       if (path(i) + 1 == path(i+1)) cycle
!       istart = path(i) + 1
!       go to 350
!     end do
!
!
!
!     if (istart == 0) then
!       ncard = ncard1
!       return
!     endif
!
! 350 continue
!     path(:nhist) = 0
!     ipass = ipass + 1
!     go to 56
!
! 400 continue
!     do i = 1, nhist
!       do j = 1, ncard1
!         if (whathist(j) == path(i)) go to 420
!       end do
!     end do
!     go to 425
! 420 continue
!     nhist = i - 1
! 425 continue
!     do i = 1, nhist
!       key = path(i)
!       call atblk_load(lproc2,lcard2,key,maxproc,proc,cvar,whathist2,&
!                       istat)
!
!       if (istat == 1) go to 5000
!     end do
!     ncard2 = ncard
!
!          merge differences into lproc and lcard
!     do i = 1, ncard1
!       if (whathist(i) > istart) go to 450
!     end do
!     write (plun, *) ' istart = ', istart
!     write (plun, *) ' ATBLK_BLD-> Unable to find starting position for merge'
!     ispcrd(5) = 0
!     ncard = ncard1
!     go to 990
! 450 continue
!     lproc_strt = i
!     j = lproc_strt
!     ifirst_yes = 0
!     do i = 1, ncard2
!       if (lproc2(i) == lproc(j)) then
!         flag(i) = 'N'
!         j = j + 1
!       else
!         k = i
!         if (ncard2 - i + 1 > 0) then
!           flag(i:ncard2) = 'Y'
!           do k = 1, ncard2 - i + 1
!             if (ifirst_yes == 0) ifirst_yes = i + k - 1
!           end do
!           k = ncard2 + 1
!         endif
!         numi = ncard2 - i + 1
!         lproc_strt = j
!         exit
!       endif
!     end do
!
!     ncard = ncard1 + numi
!     j = ncard
!     do i = ncard1, lproc_strt, -1
!       lproc(j) = lproc(i)
!       lcard(j) = lcard(i)
!       whathist(j) = whathist(i)
!       j = j - 1
!     end do
!     j = ifirst_yes
!     i = lproc_strt
!     if (numi + 1 > 0) then
!       where (flag(j:numi+j) == 'Y')
!         lproc(lproc_strt:numi+lproc_strt) = lproc2(j:numi+j)
!         lcard(lproc_strt:numi+lproc_strt) = lcard2(j:numi+j)
!         whathist(lproc_strt:numi+lproc_strt) = whathist2(j:numi+j)
!       end where
!       i = lproc_strt + numi + 1
!       j = numi + 1 + j
!     endif
!
!
! 990 continue
!     if (idiag /= 0) then
!       if (ispcrd(5) == 1) then
!         if (ispcrd(6) == 1) call cpsabort (&
!           'ATBLK--> You can''t use both special options 5 and 6')
!         write (plun, *) ' lproc2 '
!         do i = 1, ncard2
!           write (plun, 9001) i, lproc2(i), flag(i)
!         end do
!       endif
!       write (plun, *) ' LPROC IN ROUTINE ATBLK_BLD'
!       do i = 1, ncard
!         write (plun, 9001) i, lproc(i), flag(i)
!         write (plun, *) ' '
!         write (plun, *) ' '
!       end do
!
!       write (plun, *) ' LCARD IN ROUTINE ATBLK_BLD'
!       do i = 1, ncard
!         write (plun, *) i, lcard(i)
!       end do
!     endif
!     return
 9001 format(i4,i4,a)
!
!          error return
 5000 continue
      write (plun, *) '-->ATBLK - ERROR ENCOUNTERED IN ROUTINE ATBLK_BLD'
      status=-2
      return
      end subroutine atblk_bld


      subroutine atblk_bsmt(bsmt, mtrcflg)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*) , intent(in) :: mtrcflg
      real , intent(in) :: bsmt
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      real :: bsmtm

!-----------------------------------------------
!cc
!    routine to print basement interval to title block
!          called by atbspec
!
!          bsmt  = the basement interval between two traces
!       mtrcflg  = flag to indicate if this is the metric system
!                   (yes or no)
!
!

!    user may specify feet or meters for the displayed trace interval
      if (mtrcflg=='NO' .and. ispcrd(7)==1) then
        bsmtm = bsmt*0.30480064
        write (iatbf, 9006) bsmt, bsmtm
        if (iprint == 1) write (plun, 9005) bsmt, bsmtm
!
      else if (mtrcflg == 'NO') then
        write (iatbf, 9002) bsmt
        if (iprint == 1) write (plun, 9001) bsmt
!
      else if (mtrcflg == 'YES') then
!
        write (iatbf, 9004) bsmt
        if (iprint == 1) write (plun, 9003) bsmt
      endif
 9001 format(1x,'DISPLAYED TRACE INTERVAL:',f6.1,' FEET')
 9002 format('DISPLAYED TRACE INTERVAL:',f6.1,' FEET')
 9003 format(1x,'DISPLAYED TRACE INTERVAL:',f6.1,' METERS')
 9004 format('DISPLAYED TRACE INTERVAL:',f6.1,' METERS')
 9005 format(1x,'DISPLAYED TRACE INTERVAL:',f6.1,' FEET (',f6.1,' METERS)')
 9006 format('DISPLAYED TRACE INTERVAL:',f6.1,' FEET (',f6.1,' METERS)')
!
      return
      end subroutine atblk_bsmt

      subroutine atblk_load(job,maxproc,status)
!
!
!
!          gather title block information from a cray history file
!           builds the lproc, lcard and whathist arrays
!
!
!          lproc  =  array of process numbers in the order they were run
!          lcard  =  array of parameter answers from the history file
!          job    =  the history number to read
!          nhist  =  the total number of history records
!          maxproc=  maximum number of processes in this title block.
!          proc  =  array containing the names of the process from the
!                     master file.
!          cvar   =  array contain the variable names from the master
!                    file
!         whathist=  array parallel to lproc which indicates the history
!                    that process came from
!
!
      integer,intent(in)  :: job,maxproc
      integer,intent(out) :: status
!
      character(len=680)::        longans(10) 
      character cvarans(100)*24
      character(len=1) :: comma,lpar,rpar,lbrac
      character ctemp*24,braceq*2
!
      integer :: i,id,icol,ii,iplus,ipn,istat,it,itmp,i2,j,jj,j1
      integer :: k,kbias,kc,knt,kntlnk,kk,kkk,kp,kstat,kt,k1,k2
      integer :: l,lbcol,lc,lcs,len,lendif,lenp,ll,lll,lndx,lpcol,m,mm
      integer :: n,ncrds,ndx        ,nc,nm80,noc,nn,nt,ntmp,num,n1,n2,n80 


      character(len=4)  :: version
      character(len=24) :: procname,tempname
!
!          ncard  =  index for lproc and lcard arrays
!
!
!
!
      data comma/','/, lpar/'('/, rpar/')'/, braceq/']='/, lbrac/'['/
!

     status=0
!    read proper history file
      istat=0
      num=0
      if(job.eq.-9999)then
        ipn=hist_maxipn()
        j=1
        do i=1,ipn
          if((j+num-1).gt.maxcards)then
            write(plun,*)'ATBLK_load: Number of cards gt maxcards'
            write(plun,*)'            Number of cards = ',j+num-1
            write(plun,*)'            maxcards = ',maxcards
            status=1
            return
          endif
          k=hist_read(i,num,ibuf(j:))
          j=j+num
        enddo
        version='2.0'
        ncrds=j-1
      else
        call manhist_gethistoryrec(job,ibuf)
        call manhist_get_table_info(job,istat,version,numbercards=ncrds)
        if(istat.ne.0)then
          write(plun,*)'ATBLK_load: Bad history number requested'
          return
        endif
      endif
!
      ctemp=' '
      do id=1,100
        cvarans(id)=' '
      enddo
!
!          begin reading history until find a process
      i=0
 100  i=i+1
      if(i.gt.ncrds)go to 1100
      if(version.eq.'0.0')then
        if(ibuf(i)(1:1).eq.' ')go to 100
        procname=ibuf(i)(1:8)
      else
        call histprim_process_name(ibuf(i),procname)
        if(procname.eq.' ')go to 100
      endif
!
!          see if this process is in the master
      do j=1,nprocc
        jj=j
        if(procname(1:8).eq.proc(j))go to 200
      enddo
      go to 100
!
 200  continue
!
!          if splt or colr - print only the one we are on -
!                     unless special option 3 flagged
      if(ispcrd(3).eq.1)go to 215
      if(proc(jj).eq.'SPLT'.or.proc(jj).eq.'COLR'.or.proc(jj).eq.'COLOR')then
        if(job.ne.-9999)go to 100
        do ii=ncrds,1,-1
          if(version.eq.'0.0')then
            procname=ibuf(ii)(1:4)
          else
            call histprim_process_name(ibuf(ii),procname)
          endif
          if(procname.ne.'SPLT'.and.procname.ne.'COLR'.and.procname.ne.&
            'COLOR')cycle
          if(ii.ne.i)go to 100
          go to 215
        enddo
      endif
!
!          process found
 215  ncard=ncard+1
      if(ncard.gt.maxproc)then
        if(iprint.eq.1)then
        write(plun,*)' '
        write(plun,*)' NUMBER OF PROCESSES CUT OFF AT ',maxproc, ' AS SPECIFIE&
                     &d by the mxpr parameter in splt'
        endif
      return
      endif
!
      lproc(ncard)=jj
      lcard(ncard)=' '
      whathist(ncard)=job
!
!          flag lcard that this is cray format
!!!      lcard(ncard)(1:4)='CRAY'
!
!          find the card number in ibuf of the next process so can
!           look for parameters for this process only
      ii=i+1
      i2=0
 250  continue
      if(version.eq.'0.0')then
        tempname=' '
        tempname=ibuf(ii)(1:1)
      else
        call histprim_process_name(ibuf(ii),tempname)
      endif
      if(tempname.ne.' ')then
        i2=ii
        go to 300
      endif
!
      ii=ii+1
      if(ii.le.ncrds)go to 250
 300  continue
      if(i2.lt.i)then
        lc=ncrds*80
      else
        lc=(i2-i)*80
      endif
!
!
!          look for parameters
      kbias=mvar(jj)-1
      lndx=1
!
!          set i to 1st parameter card
      i=i+1
      do 750 k=mvar(jj),nvar(jj)
!
!          if parameter has a parenthesis - this is an array of answers
        ndx=0
        knt=0
        n=index(cvar(k),lpar)
        if(n.eq.0)go to 350
!
!            get the subscript
        n1=n+1
        n2=index(cvar(k),rpar)
        if(n2.eq.0)go to 1000
        n2=n2-1
        if((n2-n1).gt.1)go to 1000
        ctemp=cvar(k)(n1:n2)
        call string_cc2ii(ctemp,ndx,kstat)
        if(kstat.ne.1)go to 1000
 350    continue
        len=len_trim(cvar(k))
        if(ndx.gt.0)len=n1-2
!
        do 700 m=1,lc
          if(cvar(k)(1:len).ne.ibuf(i)(m:m+len-1))go to 700
!
!
!          set n80 to last character this card
          ntmp=m/80+.0000001
          n80=ntmp*80+80
          lcs=n80
          if(ndx.gt.0)lcs=lc
!
!            found the variable - save the answer in cvarans
!
!           if this is an array parameter--determine if its a
!            linked array
          if(ndx.eq.0)go to 370
          it=index(ibuf(i)(m:n80),braceq)
          if(it.eq.0)go to 370
!           find column of left bracket
          iplus=m/80
          lbcol=index(ibuf(i+iplus)(1:80),lbrac)
!
!            make sure this is actually the linked array parameter
!            we are after.  if there are characters between len-1
!            and a common or bracket, it is not.
!
          lll=mod(m,80)
          l=index(ibuf(i+iplus)(lll+len:),comma)+lll
          do ll=lll+len,l
            if(ibuf(i+iplus)(ll:ll).ne.' ')go to 700
          enddo
!
!            determine the last card of the linked array
          kntlnk=0
          itmp=lbcol
 360      itmp=itmp+80
          kntlnk=kntlnk+1
          if(ibuf(i+iplus)(itmp:itmp).eq.lpar)go to 360
          kntlnk=kntlnk-1
          if(ndx.gt.kntlnk)go to 700
!
!            the answer will be on the ndx'D CARD DOWN BEGINNING IN
!             column m. terminating delimiter could be a comma or a
!             right parenthesis.
          k1=m+80*ndx
          ntmp=k1/80+.0000001
          n80=ntmp*80+80
!            if left parenthesis not in same column as left bracket -
!              this parameter does not exist in history
          iplus=k1/80
          lpcol=index(ibuf(i+iplus),lpar)
          if(lpcol.ne.lbcol)go to 700
          kt=index(ibuf(i)(k1:n80),rpar)+k1-1
!            if right paren missing - this parameter does not exist in
!              history
          if(kt.eq.(k1-1))go to 700
          k2=index(ibuf(i)(k1:n80),comma)+k1-1
          if(k2.eq.(k1-1))k2=kt
          if(kt.lt.k2)k2=kt
          k2=k2-1
          go to 470
!
 370      n=index(ibuf(i)(m:n80),'=')+m-1
          if(n.eq.(m-1))then
!              for nth occurrence - won'T HAVE EQUAL SIGN YET
            if(knt.ne.ndx)go to 380
!              if no equal sign, probably found some string other than
!               the parameter name - look for another occurrence of this
!               string
            knt=knt+1
            go to 700
          endif
!
!          if there is anything but blanks between the parameter
!           name and the equal sign - this is not the correct name
 380      j1=m+len-1
          if((n-j1).gt.1)then
            ctemp=' '
            ctemp=ibuf(i)(j1+1:n-1)
            if(ctemp.ne.' ')then
              knt=knt+1
              go to 700
            endif
          endif
          if(ndx.eq.0)go to 500
!
!            find the ndx'D SEQUENTIAL ANSWER
          nn=index(ibuf(i)(n+1:n80),lpar)+n
          if(nn.eq.n)then
!            not an array parameter - find the nth occurrence of this
!             parameter name
            knt=knt+1
            if(knt.eq.ndx)go to 500
            go to 700
          endif
!
          icol=nn+1
!            determine ending column of array
          kp=index(ibuf(i)(icol+1:lc),rpar)+icol
          if(kp.eq.icol)then
            write(plun,*)' CLOSEING RIGHT PARENTHESIS NOT FOUND SEARCHING FOR&
                         & parameter ',CVAR(K),' process ',PROC(J)
            write(plun,*)' ROUTINE ATBLK_LOAD'
            status=1
            return
          endif
          noc=ndx-1
          if(noc.eq.0)then
            k1=nn+1
            go to 450
          endif
!
          do 400 mm=1,noc
            kc=index(ibuf(i)(icol+1:lcs),comma)+icol
            if(kc.ge.kp)go to 700
            if(kc.eq.icol)then
!              did not find comma - check for right paren this card
              nm80=icol
              kkk=mod(icol,80)
              if(kkk.ne.0)then
                nm80=icol/80+1
                nm80=nm80*80
              endif
              if(kp.gt.nm80.or.kp.lt.kc)then
                go to 1050
              endif
            endif
            icol=kc
 400      continue
          k1=icol+1
 450      kp=index(ibuf(i)(icol+1:lc),rpar)+icol
          if(kp.eq.icol)then
            write(plun,*)' CLOSEING RIGHT PARENTHESIS NOT FOUND SEARCHING FOR &
                         &parameter ',CVAR(K),' process ',PROC(J)
            write(plun,*)' ROUTINE ATBLK_LOAD'
            status=1
            return
          endif
          kc=index(ibuf(i)(icol+1:lcs),comma)+icol
          if(kc.eq.icol)then
!          did not find comma - check for right paren this card
            nm80=icol
            kkk=mod(icol,80)
            if(kkk.ne.0)then
              nm80=icol/80+1
              nm80=nm80*80
            endif
            if(kp.gt.nm80.or.kp.lt.kc)then
              go to 1050
            endif
          endif
          if(kc.lt.kp.and.kc.gt.k1)then
            k2=kc-1
          else
            k2=kp-1
          endif
          if((k2-k1).gt.19)k2=k1+19
 470      continue
          if(k2.lt.k1)then
            write(plun,*)' PROBLEM ENCOUNTERED WITH ARRAY PARAMETER ROUTINE&
                         & ATBLK_load'
            write(plun,*)' HISTORY = ',job,' PROCESS = ',proc(j),' K1 = ',k1,&
                         ' K2 = ',k2,' CARD NUM = ',i
            write(plun,*)' CARD = ',ibuf(i)
            status=1
            return
          endif
          ctemp=ibuf(i)(k1:k2)
          nc=k2-k1+1
          ctemp=adjustl(ctemp)
          n=k-kbias
          if(n.gt.100)then
              write(plun,*)' MORE THAN 100 PARAMETER ANSWERS IN A SINGLE&
                           & PROCESS.'
              write(plun,*)' ROUTINE ATBLK_LOAD'
              status=1
              return
            endif
          cvarans(n)=ctemp
          ctemp=' '
          go to 700
!
!
 500      continue
          n1=n+1
!
!          set n1 to the first non-blank character after the
!            equal sign
          do 550 mm=n1,n80
            if(ibuf(i)(mm:mm).ne.' ')go to 600
 550      continue
 600      n1=mm
          if(n1.gt.n80)n1=n80
          n=index(ibuf(i)(m:n80),comma)+m-1
          if(n.eq.(m-1))then
!              comma not found - make n2 80th character on this card
            n2=n80
            n=n2
          endif
!          if there is a right bracket between n1 and n - reset n1 to
!           first character after the bracket
          if(ispcrd(8).ne.0)then
            it=index(ibuf(i)(n1:n),']')+n1-1
            if(it.ne.(n1-1))n1=it+1
          endif
          if(n.gt.lc)n=lc
          n2=n-1
          if(n1.gt.n2)n1=n2
          len=n-n1
          lendif=0
          lenp=len
 650      if(ivar(k).ne.0)len=ivar(k)
          if(len.gt.lenp)lendif=len-lenp
          n2=n1+len-1
          n=k-kbias
          if(len.gt.24)then
!
!              length greather than 24
!
!            flag cvarans - and put answer in long answer array
            if(lndx.gt.10)then
                write(plun,*)' MORE THAN 10 ANSWERS LONGER THAN 24 CHARACTERS'
                status=1
                return
              endif
            cvarans(n)(1:4)='LONG'
            longans(lndx)=ibuf(i)(n1:n2)
!               scan for vertical bar put into the history by ncode and
!                replace it with a blank
!                also check for []
            do 660 lll=1,680
              if(longans(lndx)(lll:lll).eq.'|'.or.longans(lndx)(lll:lll)&
                 .eq.'['.or.longans(lndx)(lll:lll).eq.']')&
                 longans(lndx)(lll:lll)=' '
 660        continue
            lndx=lndx+1
            go to 750
          endif
!
 670      cvarans(n)=ibuf(i)(n1:n2)
!          replace brackets with a blank
          do 680 lll=1,24
            if(cvarans(n)(lll:lll).eq.'['.or.cvarans(n)(lll:lll).eq.']')&
               cvarans(n)(lll:lll)=' '
 680      continue
          if(lendif.gt.0)then
            nt=len-lendif+1
            cvarans(n)(nt:len)=' '
          endif
          go to 750
 700    continue
!
 750  continue
!
      lndx=1
!
!          put answers from cvarans into lcard. separate them by commas
      n1=5
      kk=1
      do 900 k=mvar(j),nvar(j)
        len=len_trim(cvarans(kk))
        if(len.eq.0)go to 850
        if(ivar(k).ne.0)len=ivar(k)
!
!            get from long answer array if flag set
        if(cvarans(kk).eq.'LONG')then
          len=len_trim(longans(lndx))
          lcard(ncard)(n1:n1+len-1)=longans(lndx)(1:len)
          lndx=lndx+1
          go to 800
        endif
!           scan for vertical bar put into the history by ncode and
!           replace it with a blank
        do 790 lll=1,len
          if(cvarans(kk)(lll:lll).eq.'|')cvarans(kk)(lll:lll)=' '
 790    continue
        lcard(ncard)(n1:n1+len-1)=cvarans(kk)(1:len)
 800    n1=n1+len
 850    lcard(ncard)(n1:n1)=comma
        n1=n1+1
        kk=kk+1
 900  continue
!
      do 950 id=1,20
        cvarans(id)=' '
 950  continue
!
      go to 100
!
 1000 continue
        write(plun,*)' PROBLEM WITH SUBSCRIPTED PARAMETER - PROCESS = ',&
                       proc(j),' PARAMETER = ',cvar(k)
      status=1
      return
 1050 continue
        write(plun,*)' COMMA NOT FOUND IN HISTORY FILE WHILE SEARCHING FOR&
                     & PARameter ',CVAR(K),' in process ',PROC(JJ)
        write(plun,*)' AREA BEING SEARCHED IS ',ibuf(i)(icol+1:lcs)
        write(plun,*)' I = ',i,' ICOL = ',icol,' LCS = ',lcs
      status=1
      return
!
 1100 continue
      return
      end subroutine atblk_load


      subroutine atblk_gans(iparm, ians, ibuf, icard, nc)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: icard
      integer , intent(in) :: nc
      character(len=*)  :: iparm
      character , intent(out) :: ians*(*)
      character(len=80),intent(in) :: ibuf(*)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: n, i, k, ndx, icol, i1, i2

      save n, i, k, ndx, icol, i1, i2
!-----------------------------------------------
!
!
!          looks for the name iparm in the history file buffer
!          returns the information following iparm between the
!          equal sign and comma or end of card
!
!
!          iparm = the parameter to look for in ibuf
!          ians  = the answer returned
!          ibuf  = the history file buffer
!         icard  = the card to begin looking in ibuf
!            nc  = total number of cards in ibuf
!
!
!
!
      n=len_trim(iparm)
      do i = icard, nc
        k = index(ibuf(i),iparm(1:n))
        if (k /= 0) go to 150
      end do
!
      return
!
  150 continue
      ndx = i
      icol = k
      icol = index(ibuf(ndx)(icol+1:),'=') + icol
      i1 = icol + 1
      k = index(ibuf(ndx)(icol+1:),',') + icol
      i2 = k - 1
      if (k == icol) i2 = i1 + 7
!
      i2 = min0(80,i2)
!
      ians = ibuf(ndx)(i1:i2)
!
!
!
      return
      end subroutine atblk_gans


      subroutine atblk_gmas(grpseqflg,errorflag)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(out) :: errorflag,grpseqflg
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: istat
      character :: card*78, cdum*2

!-----------------------------------------------
!
!          routine to read and interpret the cray master file
!
!
!          nvar   = array containing ending indexes of array cvar
!          mvar   = array containing beginning indexes of array cvar
!          proc   = array containing the names of the process from the
!                     master file
!          npc    = array containing ending indexes of pcard and pcom
!          pcom   = array containing second characters of codes
!        grpseqflg   = flag returned to indicate a group sequence
!          mstrf   = the name of the master file
!
!
!
!
!
      errorflag = 0
      ispcrd=0
      nprocc = 0
      grpseqflg = 0
!
!          read master file cards and interpret
!           ** process name
!           ## parameter card
!           #l optional lengths of parameter answers
!           !! print card
!           !? print if all parameters on the card are non-blank
!           !/ conditional print card
!    '$$' represents a title card, ignored
!    '$P' represents a print card  not associated with a process.
!          prints as part of digital processing sequence after the
!          displayed trace interval
!    '$*' represents special option cards
!
!
      rewind mstrf
!
  100 continue
      read (mstrf, 9001, end=200) cdum, card
  110 continue
      if (cdum == '**')then
        call atblk_aprc (card,errorflag)
        if(errorflag.ne.0)go to 5000
      endif
      if (cdum == '##') call atblk_apar (card)
      if (cdum == '#L') then
        call atblk_alen (card,istat)
        if (istat.ne.0) go to 5000
      endif
      if (cdum(1:1) == '!')then
        call atblk_aprt (cdum,card,errorflag)
        if(errorflag.ne.0)go to 5000
      endif
      if (cdum == '$P') write (ispf, 9002) card
      if (cdum == '$*') call atblk_aspc (card)
      if (cdum == '*G') then
        grpseqflg = 1
  150   continue
        write (igsf, 9001) cdum, card
!            read and write until hit next process
        read (mstrf, 9001, end=200) cdum, card
        if (cdum=='**' .or. cdum=='$$') go to 110
        go to 150
      endif
      go to 100
!
  200 continue
      endfile igsf
      rewind igsf
      endfile ispf
      rewind ispf
!
      if (idiag == 1) then
        write(plun,*) ' '
        write(plun,*) ' NUMBER OF PROCESSES IN MASTER = ', nprocc
        write(plun,*) ' NUMBER OF PARAMETERS = ', nvar(nprocc)
        write(plun,*) ' NUMBER OF CRAY PRINT CARDS = ', npc(nprocc)
      endif
!
      return
 5000 continue
      if (iprint == 1) then
        write(plun,*) ' ATBLK_gmas: ERROR ENCOUNTERED INTERPRETING MASTER FILE'
        write(plun,*) '             NO TITLE BLOCK WILL BE DONE'
      endif
      errorflag = 1
      return
 9001 format(a,a)
 9002 format(a)
!
      end subroutine atblk_gmas

      subroutine atblk_gnprt(card,lprint,lgndx,status)
!
!    routine to generate the output from a print card
!          called by atbgtbk
!
      character(len=*),intent(inout) :: card
      integer,intent(out) :: status
      logical,intent(inout) :: lprint
      integer,intent(inout) :: lgndx

      logical :: ldum

      status=0

!    initialize
      lprint=.false.
!    check for type of print card, act accordingly
      if(card(1:2).eq.'!!'.or.card(1:2).eq.'!H')then
        call atblk_prcrd(card,ldum,lgndx,status)
        if(status.ne.0)go to 5000
        lprint=.true.
      else if(card(1:2).eq.'!?')then
         call atblk_prcrd(card,lprint,lgndx,status)
         if(status.ne.0)go to 5000
      else if(card(1:2).eq.'!/')then
         call atblk_gtcdx(card(3:80),lprint,lgndx,status)
         if(status.ne.0)go to 5000
         if(lprint)then
           call atblk_prcrd(card,ldum,lgndx,status)
           if(status.ne.0)go to 5000
         endif
      endif
!
      return
!
 5000 continue
      write(plun,*)' IN ROUTINE GNPRT BEFORE RETURN 1'
      return
      end subroutine atblk_gnprt


      subroutine atblk_gseq(irgsflg,status)
!
!
!          routine which indicates multiple processes as one process.
!          ( *g cards in the master file)
!          cards were saved in tape53 by routines getmas and getcmas
!
!          proc = array containing the names of the processes in the
!                  master file
!
!
      integer,intent(in) ::  irgsflg
      integer,intent(out) :: status
!
      character(len=8) :: ctmp
      character(len=80) :: kard,ctmp2(50)
!
      integer :: i,ia,icol,ii,iii,ifound,ipar,ipnum,irp,iseq(50),i1,i2,j,jj
      integer :: k,kard1,kk,kntgs,l,ll
      integer :: m,maxgs,mm,n,nc,ndx,npnum,ntmp,num
!
!
      integer :: itmpflg(irgsflg,3)
!
!
!             ********** arrays used ****************
!
!          lgprc   =  array containing the process numbers included in
!              a group sequence. lgprc is indexed by stgrp and
!              endgp arrays.
!
!          stgrp   =  array containing the index number for the first
!    process in a group sequence. it is indexed by the group process
!              number.
!
!          endgp  =  array containing the index number for the last
!                      process
!              in a group sequence. it is indexed by the group process
!              number.
!
!          machine  =  character array indicateing what machine this
!                      group process comes from. it is indexed by the
!                      group process number.
!
!          rpprc  =  array containing the process number which will
!                      be replaced by the new group sequence. necessary
!                      for "SPARSE" group sequences. subscript is the
!                      group process number.
!
!          igvar    =  group variable array. two dimensioal. the row is
!              subscripted by the mgvar and ngvar arrays. col1 contains
!              the process number for this sequence. col2 indicates
!              the parameter number from that process. col3 contains
!              the length of the parameter answer.
!
!          mgvar    =  array containing index for first group variable
!              for group sequence. its index is the group process number
!
!          ngvar    =  array containing index for last group variable
!              for group sequence.
!
!
!          pgcrd   =  array containing group sequence print cards.
!                      subscripted by mgpc and ngpc arrays.
!
!          mgpc     =  contains index of first group sequence print card
!                      subscripted by the group process number.
!
!          ngpc     =  contains index of last group sequence print card.
!             it is subscripted by the process number.
!
!          gsflg   =  array to flag if processes are part of a group
!                      sequence.
!                      two-dimensional. rows correspond to the lproc-
!                       lcard arrays. 3 columns
!                      col1 indicates the group sequence number. col2
!                      indicates the instruction code. col3 is a
!                      counter for the nth occurrence of this group
!                      sequence.
!                       col 2 instruction codes...
!                        1  =  replace this process with the new group
!                              sequence.
!                        2  =  delete this process.
!
!         itmpflg  =  temporary 2-dim array to use for gsflg while
!                     looking for group sequences. information is stored
!                     in gsflg after entire group sequence is found.
!                     col1 contains index in lproc, col2 contains code.
!                     col3 the nth occurrence
!
!          lproc    =  array containing process numbers wanted in the
!                      order they appear in the history file.
!
!         lgcard     =  array containing answers to group process
!                       variables. all answers are put on one card. the
!                       card number corresponds to the gsflg array with
!                       a 1 instruction code.
!
!
!          lcard    =  array containing answers to variables. all
!                      answers are put
!              on one card and the card number corresponds to the lproc
!              array. the columns for these answers are determined by
!              ivar array.
!
!          ivar     =  array contains the lengths of the parameter
!                      answers. the ivar array is subscript
!                      by the mvar and nvar arrays.
!
!          mvar      =  starting subscript array for ivar. its
!                       subscript is the process number.
!
!          nvar      =  ending subscript array for ivar. its
!                       subscript is the process number.
!
!          irgsflg   =  number of rows in gsflg array.
!
!          irgvar    =  number of rows in gvar array.
!
!
!
!          m = index for row in igvar - saved in mgvar and ngvar
!          n = index for pgcrd - saved in mgpc and ngpc arrays
!          k = group process number
!          l = index for lgprc
!          j = column index on *g card for process names
!
!
      status=0
      do i=1,irgsflg
        gsflg(i,1)=0
        gsflg(i,2)=0
        gsflg(i,3)=0
      end do
      m=1
      l=1
      n=1
      k=0
      j=3
      do 10 ii=1,kntgskd
        igvar(ii,1)=0
        igvar(ii,2)=0
        igvar(ii,3)=0
        iseq(ii)=0
 10   continue
!
 20   read(igsf,9001,end=300)kard
 22   if(kard(1:2).eq.'*G')go to 25
      if(kard(1:2).eq.'#G')go to 100
      if(kard(1:1).eq.'!')then
        go to 200
      endif
!
      write(plun,*)' ILLEGAL CARD ENCOUNTERED IN GROUP SEQUENCE'
      write(plun,*)' KARD = ',kard
      status=1
      return
!
 25   continue
!          interpret group sequences
      k=k+1
      mgpc(k)=n
      ngpc(k)=n-1
      stgrp(k)=l
      j=3
!
 30   ctmp=kard(j:j+7)
      if(ctmp(1:1).eq.'(')go to 70
!
      do 50 i=1,kntproc
        if(proc(i).ne.ctmp)go to 50
        lgprc(l)=i
        go to 60
 50   continue
!
      write(plun,*)' GSEG PROCESS ',ctmp,' DOES NOT EXIST IN MASTER FILE'
      status=1
      return
!
 60   l=l+1
      j=j+8
      go to 30
!
 70   continue
      endgp(k)=l-1
      if(ctmp(2:3).eq.'SQ')then
        rpprc(k)=lgprc(stgrp(k))
        iseq(k)=1
      endif
      if(ctmp(2:3).ne.'SP')go to 20
!
!          sparse sequence - read process name for substitution
      j=j+4
      ctmp=kard(j:j+7)
      do 80 i=1,kntproc
        if(proc(i).ne.ctmp)go to 80
        rpprc(k)=i
        go to 20
 80   continue
      if(iprint.eq.1)then
      write(plun,*)' ATBLK_gseq: SUBSTITUTION PROCESS NAME ',ctmp,' NOT FOUND &
                   &IN MASTER FILE'
      endif
      status=1
      return
!
!          interpret group sequence parameter card
 100  continue
      icol=0
      ll=1
      mgvar(k)=m
 110  continue
!
      ia=index(kard(icol+1:),'(') + icol
      if(ia.eq.icol)go to 150
      icol=ia
      ia=ia+1
      kk=index(kard(icol+1:),'-') + icol
      icol=kk
      nc=kk-ia
      ctmp='    '
!
!          get process
      ctmp(1:nc)=kard(ia:ia+nc-1)
      ia=kk+1
      call string_cc2ii(ctmp,ntmp)
      num=stgrp(k)+ntmp-1
      igvar(m,1)=lgprc(num)
!
!          get parameter number
      ctmp='    '
      icol=kk
      kk=index(kard(icol+1:),')') + icol
      nc=kk-ia
      icol=kk
      ctmp(1:nc)=kard(ia:ia+nc-1)
      call string_cc2ii(ctmp,ntmp)
      igvar(m,2)=ntmp
      m=m+1
      ll=ll+1
      go to 110
!
 150  continue
      ngvar(k)=m-1
      go to 20
!
 200  continue
!
!          save the print cards
      if(n.gt.kntgprt)go to 20
      ngpc(k)=ngpc(k)+1
      pgcrd(n)=kard
      n=n+1
      go to 20
!
 300  continue
      kk=1
 350  continue
      if(iprint.eq.1)then
        write(plun,*)' GROUP SEQUENCE ',kk
        do ii=stgrp(kk),endgp(kk)
          write(plun,*)lgprc(ii)
        enddo
      endif
      kk=kk+1
      if(kk.le.k)go to 350
!
 500  continue
      maxgs=k
!          flag group sequences
!
!          j  =  counter for itmpflg
!
      kard1=1
      k=1
      kntgs=1
 700  j=1
      i1=stgrp(k)
      i2=endgp(k)
      do i=1,ncard
        itmpflg(i,1)=0
        itmpflg(i,2)=0
        itmpflg(i,3)=0
      enddo
!
      irp=0
!          look for group sequence k
      do 750 i=kard1,ncard
        if(lgprc(i1).ne.lproc(i))go to 750
        if(j.gt.1.and.iseq(k).eq.1)then
!         check for sequential before going on
          if(i.gt.(itmpflg(j-1,1)+1))then
!            it'S NOT SEQUENTIAL
            kard1=itmpflg(j-1,1)+1
            go to 700
          endif
        endif
        itmpflg(j,1)=i
        itmpflg(j,2)=2
        itmpflg(j,3)=kntgs
!            insure only one replace process
        if(irp.ne.0)go to 720
        if(rpprc(k).eq.lgprc(i1))then
          itmpflg(j,2)=1
          irp=1
        endif
 720    continue
        j=j+1
        i1=i1+1
        if(i1.le.i2)go to 750
!
!          entire sequence found - update gsflg array
        j=1
!          make sure sequential if sq option
        if(iseq(k).eq.0)go to 740
        do 735 mm=stgrp(k),endgp(k)-1
          if(itmpflg(j+1,1)-itmpflg(j,1).gt.1)go to 760
          if(itmpflg(j+1,1)-itmpflg(j,1).le.0)go to 760
          j=j+1
 735    continue
        irp=0
        j=1
 740    ii=itmpflg(j,1)
        gsflg(ii,1)=k
        gsflg(ii,2)=itmpflg(j,2)
        gsflg(ii,3)=kntgs
        j=j+1
        if(itmpflg(j,1).eq.0)then
          i1=stgrp(k)
          irp=0
          do 745 iii=1,ncard
            itmpflg(i,1)=0
            itmpflg(i,2)=0
            itmpflg(i,3)=0
 745      continue
          j=1
          kntgs=kntgs+1
          go to 750
        endif
        go to 740
 750  continue
      k=k+1
      kntgs=1
      kard1=1
      if(k.le.maxgs)go to 700
      go to 785
 760  continue
!          look for another occurrence of this group sequence
      kntgs=kntgs+1
      do i=1,ncard
        if(itmpflg(i,1).ne.0)go to 775
      enddo
      k=k+1
      if(k.le.maxgs)go to 700
 775  continue
      kard1=itmpflg(i,1)+1
      if(kard1.lt.ncard)go to 700
 785  continue
!
!
!          look for parameters belonging to nth occurrence of group
!           sequence k
!      (the gsflg array is complete and cols 1 and 2 of the igvar
!        arrays are complete)
      kntgs=1
      ifound=0
      k=1
 900  continue
      do i=1,ncard
        if(gsflg(i,1).ne.k.or.gsflg(i,3).ne.kntgs)cycle
        ipnum=lproc(i)
        do j=mgvar(k),ngvar(k)
          npnum=j-mgvar(k)+1
          if(ipnum.ne.igvar(j,1))cycle
          ifound=1
          ipar=igvar(j,2)
          call atblk_gsprm(ipar,lcard(i)(5:80),ctmp2(npnum),igvar(j,3))
        enddo
      enddo
        if(ifound.eq.0)go to 1400
!
!        found all parameters for nth occurrence of sequence k
!         find the replace process for this sequence. save this index
      do j=1,ncard
        if(gsflg(j,1).eq.k.and.gsflg(j,2).eq.1.and.gsflg(j,3).eq.kntgs)&
           go to 1300
      enddo
      if(iprint.eq.1)then
        write(plun,*)' NO REPLACE PROCESS FOUND FOR GROUP SEQUENCE ',k
        write(plun,*)' GROUP SEQUENCE gsflg ARRAY...'
        do j=1,ncard
          write(plun,9002)j,(gsflg(j,jj),jj=1,3)
        enddo
      endif
      status=1
      return
 1300 continue
      ndx=j
!
!          move ctmp2 to lgcard
      npnum=0
      i1=1
      i2=0
      do i=mgvar(k),ngvar(k)
        npnum=npnum+1
        if(igvar(i,3).eq.0)go to 1330
        lgcard(ndx)(i1:i2+igvar(i,3))=ctmp2(npnum)(1:igvar(i,3))
        i1=i1+igvar(i,3)
 1330   lgcard(ndx)(i1:i1)=','
        i1=i1+1
        i2=i1-1
      enddo
!
!          look for another occurrence of group sequence k
      kntgs=kntgs+1
      ifound=0
      ctmp2=' '
      go to 900
!
 1400 continue
!          look for next group sequence
      k=k+1
      kntgs=1
      if(k.le.maxgs)go to 900
!
!
!
      if(idiag.eq.0)go to 1500
      write(plun,*)'gsflg ARRAY AND LGCARD ARRAY'
      do i=1,ncard
        write(plun,9003)i,gsflg(i,1),gsflg(i,2),gsflg(i,3),lgcard(i)(1:60)
      enddo

      write(plun,*)' IGVAR ARRAY'
      do i=1,maxgs
        write(plun,*)' GROUP SEQUENCE ',i
        do ii=mgvar(i),ngvar(i)
          write(plun,9002)ii,(igvar(ii,iii),iii=1,3)
        enddo
      enddo
 1500 return
 9001 format(a)
 9002 format(1x,i3,1x,3i3)
 9003 format(4i4,a)
      end subroutine atblk_gseq


      subroutine atblk_gsprm(ipnum, kard, str, len)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: ipnum
      integer , intent(out) :: len
      character , intent(in) :: kard*(*)
      character , intent(out) :: str*(*)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: icol, noc, k1, i, k, k2
      character(len=1) :: comma=','

!-----------------------------------------------
!
!          get sequential parameter from a text card where parameters
!           are spearated by commas
!
!          **given**
!          ipnum = the parameter number to retrieve
!          kard  = the card of text with parameters separated by commas
!
!          **returning**
!          str   = the parameter
!          len   = the number of characters in str
!
!
!
!
!
!
      icol = 0
      noc = ipnum - 1
      if (noc == 0) then
        k1 = 1
        go to 350
      endif
!
      do i = 1, noc
        k = index(kard(icol+1:),comma) + icol
        if (k == icol) go to 400
        icol = k
      end do
      k1 = k + 1
!
  350 continue
      k = index(kard(icol+1:),comma) + icol
      if (k /= icol) then
        k2 = k - 1
        if (k1 <= k2) then
          str = kard(k1:k2)
          len = k2 - k1 + 1
!
!
          return
!
        endif
      endif
  400 continue
      str = ' '
      len = 0
      return
      end subroutine atblk_gsprm

      subroutine atblk_gtbk
!
!    routine to generate output of title block from built sequence
!
!
      character(len=1) :: tcode
      character(len=80) :: card,tcard
      character(len=80) :: ocard(500)
!
      logical :: lprint
      integer :: i,idum,istat,i1,i2,j,k   ,lgndx,npcard 
!

      idum=0
      if(idiag.eq.0)go to 25
      write(plun,*)' ENTERING GTBK...'
      write(plun,*)' PCARD...'
      do i=1,kntprt
        write(plun,*)' ',i,pcard(i)
      enddo
      write(plun,*)' MPC,NPC...'
      do i=1,kntproc
        write(plun,*)' ',i,mpc(i),npc(i)
      enddo
      write(plun,*)' LPROC...'
      do i=1,ncard
        write(plun,*)i,lproc(i)
      enddo
      write(plun,*)' LCARD...'
      do i=1,ncard
        write(plun,*)i,lcard(i)
      enddo
      if(kntgseq.gt.0)then
        write(plun,*)' gsflg...'
        do i=1,ncard
          write(plun,*)i,gsflg(i,1),gsflg(i,2)
        enddo
        write(plun,*)' FLAG...'
        do i=1,kntproc
          write(plun,*)i,flag(i)
        enddo
        write(plun,*)' PFLAG...'
        do i=1,kntproc
          write(plun,*)i,pflag(i)
        enddo
      endif
      write(plun,*)' MGPC,NGPC ARRAYS...'
      write(plun,*)' KNTGSKD = ',kntgskd
!!      kk=loc(mgpc)
!!      write(plun,*)' LOC OF MGPC = ',kk
!!      kk=loc(ngpc)
!!      write(plun,*)' LOC OF NGPC = ',kk
      do i=1,kntgskd
        write(plun,*)i,' ',mgpc(i)
      enddo
      do i=1,kntgskd
        write(plun,*)i,' ',ngpc(i)
      enddo
      write(plun,*)' IVAR ARRAY...'
      do i=1,kntpar
        write(plun,*)i,ivar(i)
      enddo
      write(plun,*)' MVAR,NVAR ARRAYS...'
      do i=1,kntproc
        write(plun,*)i,mvar(i),nvar(i)
      enddo
!
!    initialize
 25   npcard=0

!    for each process in lproc, evaluate and generate print cards
      do 200 whichcard=1,ncard
!
        iproc=lproc(whichcard)
        pflag(npcard+1)=flag(whichcard)
!
!            determine if part of group sequence
        lgndx=0
        if(kntgseq.eq.0)go to 100
        if(gsflg(whichcard,1).eq.0)go to 100
!            is it a delete?
        if(gsflg(whichcard,2).eq.2)go to 200
!
!            insert group sequence here
        k=gsflg(whichcard,1)
        lgndx=whichcard
!
        do 50 j=mgpc(k),ngpc(k)
          card=pgcrd(j)(1:78)
          call atblk_gnprt(card,lprint,lgndx,istat)
          if(istat.ne.0)go to 5000
          if(lprint)then
            npcard=npcard+1
            if(npcard.gt.500)then
              write(plun,*)'ATBLK_gtbk: Title block truncated at 500 cards'
              npcard=500
              go to 300
            endif
            card(1:2)='  '
            ocard(npcard)=card
          endif
 50     continue
        go to 200
!            looks thru conditional cards in master for this process
!              and picks which ones to print
!
 100    continue
!
        i1=mpc(iproc)
        i2=npc(iproc)
!
        do 150 j=i1,i2
          card(1:1)='!'
          tcode=pcom(j)
          tcard=pcard(j)
          card(2:2)=tcode
          card(3:)=tcard
          call atblk_gnprt(card,lprint,idum,istat)
          if(istat.ne.0)go to 5000
          if(lprint)then
            npcard=npcard+1
            card(1:2)='  '
            ocard(npcard)=card
          endif
 150    continue
 200  continue
      go to 300
!
 250  continue
      if(iprint.eq.1)write(plun,*)' TITLE BLOCK TRUNCATED AT 100 PRINT CARDS'
!
 300  continue

!    write digital processing sequence of title block to title block fil
      if(idiag.eq.1)then
        write(plun,*)'********** APPEARANCE OF GENERATED TITLE BLOCK &
                     &**********'
      endif
        do i=1,npcard
          if(ispcrd(4).eq.1)then
            write(iatbf,450)pflag(i),ocard(i)
          else
            write(iatbf,'(A80)')ocard(i)
          endif
          if(idiag.eq.1)print 400,pflag(i),ocard(i)
        enddo
        if(idiag.eq.1)then
          write(plun,*)'********** END OF GENERATED TITLE BLOCK **********'
        endif
!
 400  format(1x,a1,a80)
 450  format(a1,a80)
      return
!
 5000 continue
      write(plun,*)' IN ROUTINE GTBK - BEFORE RETURN 1'
      istat=1
      return
      end subroutine atblk_gtbk

      subroutine atblk_gtcdx(condx,totcnd,lgndx,status)

      character(len=*),intent(inout) :: condx
      logical,intent(out)         :: totcnd
      integer,intent(inout)       :: lgndx
      integer,intent(out)         :: status
!
!    routine to determine whether a complex conditional is true or false
!          called by atbgnprt
!
!            condx  =  the condition - received
!           totcnd  =  boolean answer for condition - returned
!
      character(len=80) :: cond,cond1,cond2,temp
      logical :: con,con1,con2
      integer :: kslash, kand, k, j, kor

      status=0
!    initialize
      totcnd=.false.

       kslash=index(condx,'/')
      if(kslash.eq.0)then
        write(plun,*)' SLASH MISSING FROM CONDITION ',condx
        status=1
        return
      endif
!    break up into simple conditions if & or | present
      cond=condx(1:kslash-1)
      kand=index(cond,'&')
      if(kand.ne.0)then
        k=index(cond(kand:),'=')
        j=index(cond(kand:),'^')
        if(k.eq.0.and.j.eq.0)kand=0
      endif
      kor=index(cond,'|')
      if(kand.gt.0)then
        cond1=cond(1:kand-1)
        cond2=cond(kand+1:kslash-1)
        call atblk_gtcnd(con1,cond1,lgndx,status)
        if(status.ne.0)go to 5000
        call atblk_gtcnd(con2,cond2,lgndx,status)
        if(status.ne.0)go to 5000
        totcnd=con1.and.con2
!
      else if(kor.gt.0)then
        cond1=cond(1:kor-1)
        cond2=cond(kor+1:kslash-1)
        call atblk_gtcnd(con1,cond1,lgndx,status)
        if(status.ne.0)go to 5000
        call atblk_gtcnd(con2,cond2,lgndx,status)
        if(status.ne.0)go to 5000
        totcnd=con1.or.con2
!
      else
!
!          compare parameter answer in history file to
!            parameter answers in title block master
         call atblk_gtcnd(con,cond,lgndx,status)
         if(status.ne.0)go to 5000
         totcnd=totcnd.or.con
      endif


!
!          remove conditional string from rest of condx to line up
!           print cards properly
      temp=condx(kslash+1:)
      condx(1:)=temp
      return
!
 5000 continue
      write(plun,*)' ROUTINE GTCDX STMT 5000 BEFORE RETURN 1'
      return
      end subroutine atblk_gtcdx

      subroutine atblk_gtcnd(con,cond,lgndx,status)


      logical,intent(out)   :: con
      character(len=*),intent(in)    :: cond
      integer,intent(inout) :: lgndx
      integer,intent(out)   :: status
!
!    routine to evaluate a simple conditional
!          called by atblk_gtcdx
!
!
!          con - returned
!
!          cond,kgpnum - received
!
      character(len=80) :: temp,str,val1,val2
      integer :: kl, keq, kne, ke, kq1,kq2, iprms, idum, istrt, iend, icol, i


      kl=len(cond)
!
      con=.false.

!    look for key symbols in condition - equal or not equal
!          keq = position of first = (equal)
!          kne = position of first ^ (not equal)
!          kq1 = position of first "
!          kq2 = position of second "
!          ke  = starting index (equal or not equal)
!
      keq=index(cond,'=')
      kne=index(cond,'^')
      ke=keq
      if(ke.le.1)ke=kne
      kq1=index(cond(ke+1:),'"')+ke
      kq2=index(cond(kq1+1:),'"')+kq1

!         check for "PARAMETER EQUAL/NOT EQUAL VALUE" condition
      if(kq1.lt.ke)go to 50
      if(kq2.le.kq1)go to 50
      temp=cond(1:ke-1)
      iprms=0
      call string_cc2ii(temp,iprms)
      if(iprms.le.0)go to 8000

!
      call atblk_gtprm(iprms,str,idum,lgndx,status)
      if(status.ne.0)return
!
!          val1 = answer to parameter from history file
!          val2 = answer in title block master to compare
      val1=str
      val2=cond(kq1+1:kq2-1)
      go to 200
!
!
!
!
!           check for "PARAMETER EQUAL/NOT EQUAL PARAMTER" condition
 50   if(ke.le.1)go to 100
      temp=cond(1:ke-1)
      iprms=0
      call string_cc2ii(temp,iprms)
      if(iprms.le.0)go to 8000
       if(lgndx.ne.0)go to 65
!          see if this parameter was found in history
      istrt=mvar(iproc)
      iend=istrt+iprms-2
      icol=1
      do i=istrt,iend
        icol=icol+ivar(i)
      enddo
 65   continue
      if(iprms.le.0)go to 8000
      call atblk_gtprm(iprms,str,idum,lgndx,status)
      if(status.ne.0)return
      val1=str
      temp=cond(ke+1:kl)
      iprms=0
      call string_cc2ii(temp,iprms)
      if(iprms.le.0)go to 8000
      call atblk_gtprm(iprms,str,idum,lgndx,status)
      if(status.eq.0)return
      val2=str
      go to 200
!              check for "IS PARAMETER NON BLANK" condition
 100  continue
      iprms=0
      call string_cc2ii(temp,iprms)
      if(iprms.le.0)go to 8000
 150  continue
      call atblk_gtprm(iprms,str,idum,lgndx,status)
      if(status.ne.0)return
      val1=str
 200  continue

!    determine whether condition is true or false
      if(keq.gt.1.and.val1.eq.val2)con=.true.
      if(kne.gt.1.and.val1.ne.val2)con=.true.
      if(ke.le.1.and.val1.ne.' ')con=.true.

 9999 continue
      return
 8000 continue
      if(iprint.eq.1)then
        write(plun,*)' PARAMETER NUMBER SET TO ',iprms,' IN ROUTINE GTCND. PRO&
     &cess number = ',IPROC,' card = ',whichcard,' condition = ',TEMP
      endif
!
!
      end subroutine atblk_gtcnd

      subroutine atblk_gtprm(iprm,str,kstr,lgndx,status)

      integer,intent(in) :: iprm,lgndx
      character(len=*),intent(out) :: str
      integer,intent(out) :: kstr,status

!
!          extract history file answer for a particular parameter
!            number
!
!          called by getcnd and atbinrpt
!
!          iprm  = the parmeter number - received
!          str   = the history file answer - returned
!          kstr  = the number of characters in str - returned
!
!          whichcard = corresponding index in parameter array for process
!                   array
!
!         lgndx  = index for lgcard array - received
!
      character(len=680) :: kard

      integer :: i,icol,k,k1,k2,lcol,noc
!
!
      status=0
!               get the iprm sequential parameter from lcard or
!                 lgcard if group sequence
      kard=lcard(whichcard)
      lcol=680
      if(lgndx.ne.0)then
        kard=lgcard(lgndx)
        lcol=304
      endif
      icol=0
      noc=iprm-1
      if(noc.eq.0)then
        k1=5
        if(lgndx.ne.0)k1=1
        go to 350
      endif
      do 315 i=1,noc
        k=index(kard(icol+1:lcol),',')+icol
        icol=k
 315  continue
      k1=k+1
 350  continue
      k=index(kard(icol+1:lcol),',')+icol
!          if k equals k1 this parameter did not exist in the
!           history file
      if(k.eq.k1)then
        str=' '
        kstr=0
        go to 9999
      endif
      k2=k-1
      if(k2.lt.k1)then
        if(iprint.eq.1)then
        write(plun,*)' ERROR TRYING TO GET PARAMETER NUMBER ',iprm,&
                     ' FOR PROCESS number ',IPROC,' routine atblk_gtprm abort'
        write(plun,*)' K1 = ',k1,' K2 = ',k2,' ICOL = ',icol,' whichcard = ',&
                       whichcard
        write(plun,*)' LGNDX = ',lgndx
        write(plun,*)' KARD = ',kard
        endif
        status=1
      endif
      str=kard(k1:k2)
      kstr=k2-k1+1
 9999 continue
      return
      end subroutine atblk_gtprm


      subroutine atblk_inrpt(prmlst,fulstr,ndxto,lgndx,status)
!
!    routine to interpret parameter list and convert to text
!          called by atbprcrd
!
!
!          prmlst = the parameters bewteen one set of brackets
!                   from the master file after the brackets
!                   have been removed
!
!          fulstr = the text string converted from prmlst
!
!          ndxto  = the starting index to move in text in array
!                   fulstr. it will be the number of characters
!                   in fulstr when finished
!
!
      character(len=*),intent(in)  :: prmlst
      character(len=*),intent(out) :: fulstr
      integer,intent(out) :: ndxto,status
      integer,intent(inout) :: lgndx


      character(len=80) :: lst,str,sublst
      integer :: i,icol,idum,iprms(20),iq,iquots,istat,k,kend,kq1,kq2,nc,ndxfrom
!

!    initialize
      fulstr=' '
      sublst=' '
      str=' '
      ndxto=1
      ndxfrom=1
      kend=index(prmlst,'  ')
!
      icol=0
!          see if prmlst has an item in quotes
 55   iq=index(prmlst(icol+1:),'"') + icol
      if(iq.eq.icol)then
        iquots=0
        sublst=prmlst(ndxfrom:)
!          terminate sublst with a comma
        k=index(sublst,'   ')
        sublst(k:k)=','
        go to 60
      endif
      iquots=1
!
      kq1=iq
!
      icol=iq
      iq=index(prmlst(icol+1:),'"') + icol
      if(iq.eq.icol)then
        if(iprint.eq.1)then
        write(plun,*)' ATBLK_inrpt: CLOSING QUOTE NOT FOUND-PROCESS = ',&
                       iproc,' PARAMETER LIST = ',PRMLST
        endif
        status=1
        return
      endif
!
      kq2=iq
      icol=iq
!
!
!          are there parameter numbers in front of the quote
      if(kq1.le.ndxfrom)go to 210
!
!
      sublst=prmlst(ndxfrom:kq1-1)
      ndxfrom=kq1
!
 60   continue
      do i=1,20
        iprms(i)=0
        call histprim_gsprm(i,sublst,lst,nc)
        if(nc.eq.0)cycle
        call string_cc2ii(lst,iprms(i),istat)
        if(istat.ne.1)go to 400
      enddo
!
!          convert these parameter numbers to history file entries
      do 150 i=1,20
        if(iprms(i).eq.0)go to 150
        call atblk_gtprm(iprms(i),str,nc,lgndx,status)
        if(status.ne.0)go to 500
        if(nc.eq.0)go to 150
        fulstr(ndxto:)=str(1:nc)
        ndxto=ndxto+nc+1
        if(str(1:nc).eq.' ')go to 150
        fulstr(ndxto-1:)='-'
 150  continue
!          reduce index to remove last hyphen
 160  ndxto=ndxto-1
!
!          move in the string between quotes
 200  if(iquots.eq.0)go to 300
 210  nc=kq2-kq1-1
      fulstr(ndxto:ndxto+nc)=prmlst(kq1+1:kq2-1)
      ndxfrom=kq2+1
      if(prmlst(ndxfrom:ndxfrom).eq.',')ndxfrom=ndxfrom+1
      ndxto=ndxto+nc
      if(ndxfrom.ge.kend)go to 300
      go to 55
!
!
 300  ndxto=ndxto-1
      return
!
 400  continue
      if(iprint.eq.1)then
        write(plun,*)' ERROR CONVERTING ',lst(1:8),' TO AN INTEGER. ROUTINE &
                     &IS ATBLK_inrpt. process is ',IPROC,' icard = ',IDUM
        write(plun,*)' PARAMETER LIST = ',prmlst
      endif
!
 500  continue
      status=1
      return
      end subroutine atblk_inrpt


      subroutine atblk_prcrd(card,lprint,lgndx,status)
!c
!    routine to process a title block master print card
!          called by atbgnprt when codes are !! or !? or !/
!           !! cards are always printed - lprint is set to true in routi
!               atbgnprt
!           !? cards are printed if parameters in brackets are non-blank
!               lprint is set is this routine
!
!           !/ cards are printed upon conditions being true
!          card = ! print card from master
!          lprint = set to true if card is to be printed
!
      character(len=*),intent(inout) :: card
      logical,intent(inout) :: lprint
      integer,intent(inout) :: lgndx
      integer,intent(out)   :: status
!
      character(len=80) :: fulstr,temp
      character(len=20) :: prmlst
!

      integer :: i,itrue(20),knt,ks1,kt,k1,k2,nc
!

!    initialize
      k1=0
      lprint=.false.
      itrue=0
      knt=0

!    do while - brackets exist that need to be filled with parameter val
      do 50 i=1,20
        kt=index(card(k1+1:),'[')+k1
        k2=index(card(kt+1:),']')+kt
        if(kt.eq.k1.or.kt+1.ge.k2)go to 100
        knt=knt+1
        if(knt.gt.20)then
          write(plun,*)' ROUTINE PRCRD BEFORE RETURN 1'
          write(plun,*)' MORE THAN 20 ANSWERS IN ONE LINE'
          status=1
          return
        endif
!      convert data between brackets into string
!            prmlst = the parameter numbers between brackets
!            fulstr = the paramter answers from the history file
        prmlst=card(kt+1:k2-1)
        call atblk_inrpt(prmlst,fulstr,ks1,lgndx,status)
        if(status.ne.0)go to 500
!      fill space between brackets with history file entries
        if(ks1.le.0)ks1=1
          if(fulstr(1:ks1).ne.' ')itrue(knt)=1
          temp=fulstr(1:ks1)//card(k2:)
          card(kt+1:)=temp
      nc=len_trim(temp)
      if((nc+kt+1).gt.80)then
        write(plun,*)' *** ERROR *** More than 80 characters on card...'
        write(plun,*)card
        status=1
        return
      endif
        k1=kt
 50   continue
 100  continue
!
!          if all parameters on card are not blank-
!             set lprint to true
      do i=1,knt
        if(itrue(i).eq.0)go to 160
      enddo
      lprint=.true.
!
 160  continue
      return
!
 500  continue
      write(plun,*)' ROUTINE PRCRD STMT 500 BEFORE RETURN 1'
      return
      end subroutine atblk_prcrd


      subroutine atblk_process_name(card,procname)

      character(len=*),intent(in) :: card
      character(len=*),intent(out) :: procname

      integer :: k1,k2



!         Parse out the process name from card
!         The process name is between PROCESS:blank and blank*





      k1=index(card,'PROCESS:')
      k2=index(card,' **********')

      k1=k1+9
      k2=k2-1

      procname=card(k1:k2)


      return
      end subroutine atblk_process_name

      subroutine atblk_ref(mtrcflg)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*) , intent(in) :: mtrcflg
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: job, istat, nc                                           
      integer :: i
      character :: ve*5, elev*4   
      character(len=4) :: cve='VE',cvel='VEL',cref='REF'
      character(len=8) :: version
      character(len=24) :: process_name
!-----------------------------------------------
!cc
!    routine to determine and print the reference elevation and velocity
!          called by atbspec
!
!            nhist = the total number of history records
!          mtrcflg = the metric flag (yes or no)
!
!
!    initialize
      ve = ' '
      elev = ' '
      job = 0

!    search all history files for last occurance of process jd or fgd
  210 continue

      DO job=nhist,1,-1
        call manhist_gethistoryrec(path(job),ibuf)
        call manhist_get_table_info(path(job),istat,version,nc)
        do i = 1, nc
          if(version.eq.'0.0')then
            if (ibuf(i)(1:4)/='JD  ' .and. ibuf(i)(1:4)/='FGD ') cycle
            process_name=ibuf(i)(1:4)
          else
            if(ibuf(i)(1:8).ne.'*HISTORY')cycle
            call atblk_process_name(ibuf(i),process_name)
            if(process_name.ne.'FGD')cycle
          endif
          ve = ' '
          elev = ' '
          if (process_name == 'FGD ') then
            call atblk_gans (cve, ve, ibuf, i, nc)
          else
            call atblk_gans (cvel, ve, ibuf, i, nc)
          endif
          call atblk_gans (cref, elev, ibuf, i, nc)
          go to 230
        end do
      ENDDO
!
  230 continue
      if (mtrcflg == 'YES') then
        write (iatbf, 9003) elev, ve
        if (iprint == 1) write (plun,9004) elev, ve
!
      else
!
        write (iatbf, 9001) elev, ve
        if (iprint == 1) write (plun,9001) elev, ve
      endif
!
      return

!
 9001 format('REFERENCE ',a6,' FT       VE ',a6,' FT/SEC')
 9002 format(1x,'REFERENCE ',a6,' FT       VE ',a6,' FT/SEC')
 9003 format('REFERENCE ',a6,' M        VE ',a6,'  M/SEC')
 9004 format(1x,'REFERENCE ',a6,' M        VE ',a6,'  M/SEC')
      return
      end subroutine atblk_ref

      subroutine atblk_spec(job, bsmt, mtrc)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*)  :: mtrc
      real  :: bsmt
      character(len=*),intent(in) :: job
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      character(len=10) :: idt
      character(len=78) :: card

!-----------------------------------------------
!cc
!    routine to generate and write special, user-specified cards
!          called by atblk
!
!          nhist = total number of history records
!          job   = name of job currently executing
!          bsmt  = the basement interval between two traces
!          mtrc  = the metric flag (yes or no)
!
!


!           write out digital processing sequence heading card if
!             not in master file
      if (ispcrd(9) == 0) write (iatbf, '(A)') 'DIGITAL PROCESSING SEQUENCE'

!    put together date/job number card
      call string_date(idt)
      write (iatbf, *) 'DATE ', idt, '     JOB NUMBER ', job
      if (iprint == 1) write (plun, *) 'DATE ', idt, '     JOB NUMBER ', job
!    print calculated basement interval
      if (ispcrd(4) /= 1) call atblk_bsmt (bsmt, 'NO')
!    print reference elevation and velocity, if requested
      if (ispcrd(2) == 1) call atblk_ref ('NO')
!
!          ispf is print cards to be part of digital processing
!           sequence
      rewind ispf
  400 continue
      read (ispf, 9002, end=500) card
      write (iatbf, 9002) card
      go to 400
  500 continue
      return
!
 9002 format(a)
      return
      end subroutine atblk_spec

      subroutine atblk_titl(mstrf, inhist)
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: mstrf
      integer , intent(in) :: inhist
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: itlf=0,i80,idone,i,injob,job,istat,nc,nhc
      logical :: found



      character(len=80) :: card,tcard

!-----------------------------------------------
!cc
!    routine to get title cards for the non dig proc seq part of title
!    block
!          called by atbmain
!
!            nhist = total number of history cray records
!            ibuf   = buffer to read in one history record
!            mstrf = unit number for master file
!           inhist = total number of input history records -
!                       cray + other
!
!
!
!
!           open 'atbtitl' file
      if(itlf.eq.0)then
        call getlun(itlf,istat)
        if(istat.ne.0)then
          write(plun)'ATBLK_titl-->Unable to get unit number for atbtitl&
                          & file'
          return
        endif
      endif
      open(itlf,file='ATBTITL',status='replace',iostat=istat)
      if(istat.ne.0)then
        write(plun,*)'ATBLK_titl-->Unable to open atbtitl file'
        return
      endif

!
      i80 = 80
      idone = 0
      found = .false.

!    extract '$$' cards from title block master
      rewind itlf
      rewind mstrf
      do i = 1, 1000
        read (mstrf, '(A)', end=75) card
        if (card(1:2) /= '$$') cycle
        write (itlf, '(A)') card(3:80)
      end do
   75 continue
      rewind itlf
      injob = inhist + 1
      job = 1

!    search histories for '*   TITL' cards.  if found, replace '$$' card
!      search backwards to get latest version
  100 continue
      injob = injob - 1
      istat = 0
      if (job<=nhist .and. injob>=1) then
        call manhist_gethistory(injob,ibuf,nc,nhc,'MODEL')
        nc=nc+nhc
        job = job + 1
        rewind itlf
        do i = 1, nc
          if (ibuf(i)(1:8) /= '*   TITL') cycle
          card = ibuf(i)(1:i80)
          if (card(1:5)/='*   $' .and. card(9:10)/='$$') cycle
          if (card(7:9) == 'END') go to 250
          tcard = card
          card = tcard(11:i80)
          write (itlf, '(A80)') card
          found = .true.
        end do
        if (.not.found) go to 100
        write (plun, *) '  TITL CARDS FOUND IN HISTORY ', injob
      endif
  250 continue
      rewind itlf
      if (idiag == 1) write (plun, *) &
        ' ***************** FIELD RECORDING INFORMATION ********'
      if (idiag == 1) then
        do
          read (itlf, '(A)', end=400) card
          write (iatbf, '(A)') card
          write (plun, *) ' ', card
          if (idone == 1) go to 450
          if (card(1:10) /= 'TYPE=DIGIT') cycle
          if (ispcrd(9) == 0) go to 450
!              set flag to read 1 more card which should be the
!                heading for the digital processing sequence
          idone = 1
        end do
      else
        do
          read (itlf, '(A)', end=400) card
          write (iatbf, '(A)') card
          if (idone == 1) go to 450
          if (card(1:10) /= 'TYPE=DIGIT') cycle
          if (ispcrd(9) == 0) go to 450
!              set flag to read 1 more card which should be the
!                heading for the digital processing sequence
          idone = 1
        end do
      endif
  400 continue
  450 continue
      close(itlf)
      return
!
      return
      end subroutine atblk_titl

      integer function atblk_update_name(name)

      character(len=*),intent(in) :: name

      integer :: i

      do i=1,nhist
        if(name.eq.output_names(i))then
           atblk_update_name=i
           return
        endif
        if(output_names(i).eq.' ')then
          output_names(i)=name
          atblk_update_name=i
          return
        endif
      enddo

      end function atblk_update_name
      end module atblk_module



!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
