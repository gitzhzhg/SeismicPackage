!<CPS_v1 type="PRIMITIVE"/>
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
! Name       : cfestruct
! Category   : cfe
! Written    : 1999-08-25   by: Donna K. Vunderink
! Revised    : 2004-08-23   by: SMCook
! Maturity   : production
! Purpose    : Structure Module.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This primitive is used by cfe.
!
!-------------------------------------------------------------------------------
!</descript_doc>


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
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 25. 2004-08-23  SMCook       Incorporated mfilebox (multi-file selection).
! 24. 2004-05-26  SMCook       Added custom_path variable to store path for
!                               custom executables e.g. icps.  Currently it's
!                               parsed from the prefs files and taken to be the
!                               same as the BackEndLibPath -- no new prefs file
!                               variable was added.
!                                 Example: /home/cooksm/mar8/linuxab80.
! 23. 2004-03-10  SMCook       Added prog_icps variable.
! 22. 2003-08-12  Stoeckley    Changed name from cfe_struct to cfestruct;
!                               changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
! 21. 2002-09-23  Vunderink    Added parameter STDOUT.
! 20. 2002-09-13  Vunderink    Added SJlist_dialog, SJjobsList, SJcountList and
!                                SJnextList for new Submit screen buttons.
! 19. 2002-09-11  Vunderink    Added selectCurrentDialog for Select Current
!                                Workfile popup.
! 18. 2002-07-25  Vunderink    Removed CFE_DIRECTAPP and CFE_CSAPP
! 17. 2002-02-26  Vunderink    Added ALPHALIB constant.
! 16. 2002-01-04  Vunderink    Made mwb_buffer to listbuffer name changes.
! 15. 2001-09-10  Vunderink    Changes required to rename TESTLIB to BETALIB.
! 14. 2001-04-11  Vunderink    Added SJindependent
! 13. 2001-03-12  Vunderink    Added SJlock_file, SJseries_locked and
!                                SJseries_dialog and increased size of SJseries
! 12. 2001-02-01  Vunderink    Added exec_node and exec_node_temp
! 11. 2000-12-05  Vunderink    Made changes for MWB redesign.
! 10. 2000-10-18  Vunderink    Added userid
!  9. 2000-10-08  Vunderink    Removed SJqueue
!  8. 2000-08-15  Vunderink    Added cfe contants, changed character variables
!                                back to using PC_LENGTH, and added multi-
!                                workfile builder variables
!  7. 2000-05-26  Vunderink    Added exemode.
!  6. 2000-05-09  Vunderink    Removed NSLJobs, NSLBuilt, NSLSubmitted,
!                                NSLSeries, NSLCount, NSLNext and added SLTotal.
!  5. 2000-04-24  Vunderink    Changed character variables to use named constant
!                                CFE_LENGTH, increased length of prog_cfebld and
!                                prog_cfesub, added last_jobfile,
!                                last_reportfile, last_wb_action_mb3, editor,
!                                editor_tmp, NsourceProcessId, and
!                                NdestinationProcessId, removed modifyCurrent,
!                                modifyOld, modifySubset and SJmachine.
!  4. 2000-03-08  Vunderink    Added prog_cfebld and prog_cfesub to structure.
!  3. 2000-02-18  Vunderink    Added lnklib to structure
!  2. 2000-02-03  Vunderink    Maded changes required by cfesj.f90
!  1. 1999-08-25  Vunderink    Initial version.
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
! No special requirements.
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


      module cfestruct_module

      use pc_module
      use cardset_module
      use process_module
      use cfelistbuffer_module
      use worklist_module
      use workfile_module
      use filebox_module
      use mfilebox_module
      use incjobname_module

      implicit none

      private

      character(len=100),public,save :: cfestruct_ident = &
       '$Id: cfestruct.f90,v 1.25 2004/08/23 13:15:19 SMCook prod sps $'

      integer,parameter,public    :: STDOUT               = 6

      integer,parameter,public    :: SERIES_NAME_LEN      = 15

      integer,parameter,public    :: CFE_UNKNOWN          =  0
      integer,parameter,public    :: CFE_PRODLIB          =  1
      integer,parameter,public    :: CFE_BETALIB          =  2
      integer,parameter,public    :: CFE_ALPHALIB         =  3

      integer,parameter,public    :: CFE_NAME_INVALID     = -1
      integer,parameter,public    :: CFE_NAME_VALID       =  1

      integer,parameter,public    :: CFE_WORKFILE         =  1
      integer,parameter,public    :: CFE_JOBFILE          =  2


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: cfestruct

        integer                                      :: lnklib
        integer                                      :: exemode
        character(len=181)                           :: prog_cfebld
        character(len=181)                           :: prog_cfesub
        character(len=181)                           :: prog_icps
        character(len=181)                           :: custom_path
        integer                                      :: mainWindowID
        integer                                      :: ScreenId
        character(len=PC_LENGTH)                     :: last_jobfile
        character(len=PC_LENGTH)                     :: last_reportfile
        logical                                      :: last_wb_action_mb3
        character(len=16)                            :: userid

!!------------------------- build job file ---------------------------------!!
        character(len=PC_LENGTH)                     :: buildjobfile
        type(filebox_struct),pointer                 :: buildjob_dialog

!!------------------------- edit work file ---------------------------------!!
        character(len=PC_LENGTH)                     :: editworkfile
        type(filebox_struct),pointer                 :: editwork_dialog

!!-------------------------- edit job file ---------------------------------!!
        character(len=PC_LENGTH)                     :: editjobfile
        type(filebox_struct),pointer                 :: editjob_dialog

!!---------------------------- edit file -----------------------------------!!
        character(len=PC_LENGTH)                     :: editfile
        type(filebox_struct),pointer                 :: editfile_dialog

!!------------------------- view work file ---------------------------------!!
        character(len=PC_LENGTH)                     :: viewworkfile
        type(filebox_struct),pointer                 :: viewwork_dialog

!!-------------------------- view job file ---------------------------------!!
        character(len=PC_LENGTH)                     :: viewjobfile
        type(filebox_struct),pointer                 :: viewjob_dialog

!!------------------------- view report file -------------------------------!!
        character(len=PC_LENGTH)                     :: viewreportfile
        type(filebox_struct),pointer                 :: viewreport_dialog

!!---------------------------- view file -----------------------------------!!
        character(len=PC_LENGTH)                     :: viewfile
        type(filebox_struct),pointer                 :: viewfile_dialog

!!-------------------- change working directory ----------------------------!!
        character(len=PC_LENGTH)                     :: working_dir
        character(len=PC_LENGTH)                     :: working_dir_temp
        character(len=PC_LENGTH) ,pointer            :: cwd_dirs(:)
        integer                                      :: ncwd_dirs

!!------------------------ user preferences --------------------------------!!
        character(len=8)                             :: editor_temp
        character(len=8)                             :: editor
        character(len=16)                            :: exec_node_temp
        character(len=16)                            :: exec_node

!!--------------------- programmer preferences -----------------------------!!
        character(len=16)                            :: process_maturity_temp
        integer                                      :: process_maturity
        character(len=PC_LENGTH) ,pointer            :: custom_xml_temp(:)
        integer                                      :: ncustom_xml_temp
        character(len=PC_LENGTH) ,pointer            :: custom_xml(:)
        integer                                      :: ncustom_xml

!!------------------------ workfile builder --------------------------------!!
        character(len=24)                            :: processDefaults
        logical                                      :: insertButton
        logical                                      :: replaceButton
        logical                                      :: appendButton
        logical                                      :: deleteButton
        type(filebox_struct),pointer                 :: selectJobDialog
        type(mfilebox_struct),pointer                :: mselectJobDialog
        character(len=WORKFILE_NAME_LEN)             :: oldJobName
        character(len=PC_LENGTH)                     :: oldJobComment
        integer                                      :: NoldProcessList
        character(len=PROCESS_NAME_LEN),pointer      :: oldProcessList(:)
        type(filebox_struct),pointer                 :: selectCurrentDialog
        character(len=WORKFILE_NAME_LEN)             :: currentJobName
        integer                                      :: NcurrentProcessList
        character(len=PROCESS_NAME_LEN),pointer      :: currentProcessList(:)
        character(len=PC_LENGTH)                     :: processSubset
        character(len=PROCESS_NAME_LEN)              :: subsetProcessListControl
        integer                                      :: NsubsetProcessList
        character(len=PROCESS_NAME_LEN),pointer      :: subsetProcessList(:)

        integer                                      :: NprocessDefaultsMenu
        character(len=24)                            :: processDefaultsMenu(3)
        integer                                      :: NprocessSubsetMenu
        character(len=PC_LENGTH)                     :: processSubsetMenu(17)

        type(workfile_struct),pointer                :: oldWorkfileDisplayed
        type(worklist_struct),pointer                :: oldWorkfileList
        type(workfile_struct),pointer                :: currentWorkfile
        character(len=10)                            :: undo_action
        integer,pointer                              :: undo_indexes(:,:)
        integer                                      :: undo_replace1
        integer                                      :: undo_replace2
        character(len=PC_LENGTH)                     :: sourceKeyword
        integer,pointer                              :: sourceProcessId(:)
        integer                                      :: NsourceProcessId
        integer,pointer                              :: destinationProcessId(:)
        integer                                      :: NdestinationProcessId

!!--------------------------- submit job -----------------------------------!!
        character(len=PC_LENGTH)                     :: SJseries
        character(len=PC_LENGTH)                     :: SJlock_file
        logical                                      :: SJseries_locked
        logical                                      :: SJindependent
        type(mfilebox_struct),pointer                :: SJjobs_dialog
        type(filebox_struct),pointer                 :: SJseries_dialog
        type(filebox_struct),pointer                 :: SJlist_dialog
        type(incjobname_struct),pointer              :: SJincjobname_dialog
        integer                                      :: SJtotal
        character(len=WORKFILE_NAME_LEN),pointer     :: SJjobs(:)
        integer,pointer                              :: SJcount(:)
        character(len=SERIES_NAME_LEN),pointer       :: SJnext(:)
        character(len=24)                            :: SJdate
        character(len=24)                            :: SJtime
        integer                                      :: SJtotal_temp
        character(len=WORKFILE_NAME_LEN),pointer     :: SJjobs_temp(:)
        integer,pointer                              :: SJcount_temp(:)
        character(len=SERIES_NAME_LEN),pointer       :: SJnext_temp(:)
        character(len=PC_LENGTH)                     :: SJjobsList
        character(len=PC_LENGTH)                     :: SJcountList
        character(len=PC_LENGTH)                     :: SJnextList

!!--------------------------- session log ----------------------------------!!
        integer                                      :: SLTotal
        character(len=WORKFILE_NAME_LEN),pointer     :: SLJobs(:)
        character(len=20),pointer                    :: SLBuilt(:)
        character(len=20),pointer                    :: SLSubmitted(:)
        character(len=SERIES_NAME_LEN),pointer       :: SLSeries(:)
        character(len=20),pointer                    :: SLCount(:)
        character(len=SERIES_NAME_LEN),pointer       :: SLNext(:)

!!---------------------- multi-workfile builder ----------------------------!!
        character(len=WORKFILE_NAME_LEN)             :: templateJobName
        character(len=PROCESS_NAME_LEN),pointer      :: templateProcessList(:)
        integer                                      :: NtemplateProcessList
        character(len=WORKFILE_NAME_LEN),pointer     :: jobnameList(:)
        integer                                      :: NjobnameList
        integer                                      :: totalToBuild
        character(len=4),pointer                     :: MWBipn(:)
        character(len=PROCESS_NAME_LEN),pointer      :: MWBprocess(:)
        character(len=PC_LENGTH),pointer             :: MWBkeyword(:)
        integer                                      :: MWBtotal
        character(len=PC_LENGTH),pointer             :: MWBlist(:)
        integer                                      :: MWBnlist

        type(filebox_struct),pointer                 :: templateJobDialog
        type(workfile_struct),pointer                :: templateWorkfile
        type(incjobname_struct),pointer              :: incjobnameDialog
        type(incjobname_struct),pointer              :: submitDialog
        character(len=WORKFILE_NAME_LEN),pointer     :: jobnameList_temp(:)
        integer                                      :: NjobnameList_temp
        type(cfelistbuffer_struct),pointer              :: masterBuffer
        type(cardset_struct),pointer                 :: masterCardset
        integer                                      :: MWBselected
        integer                                      :: MWBparmselected
        logical                                      :: writeReport
        logical                                      :: allTraps
        logical                                      :: writeWorkfiles
        logical                                      :: buildJobs

      end type cfestruct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(cfestruct),pointer,public,save :: object         ! needed for traps.


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module cfestruct_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

