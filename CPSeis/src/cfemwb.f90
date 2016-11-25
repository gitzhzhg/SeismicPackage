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
! Name       : cfemwb
! Category   : cfe
! Written    : 2000-08-15   by: Donna K. Vunderink
! Revised    : 2006-10-31   by: D. Glover
! Maturity   : production
! Purpose    : Multi-Workfile Builder Module.
! Portability: No known limitations, but see note below.
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
!031. 2006-10-31  D. Glover    Added NULLIFY statements for Intel compiler.
!030. 2006-01-10  B. Menger    Removed Unused Variables.
! 29. 2004-08-23  Goodger      Trap jobnames which do not begin with an alpha.
! 28. 2003-11-18  Stoeckley    Provide workarounds for Portland Group compiler.
! 27. 2003-09-15  Stoeckley    Changed name from cfe_mwb to cfemwb;
!                               changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
! 26. 2002-09-27  Vunderink    Fixed array parameter bug in work_traps_and_write
! 25. 2002-09-23  Vunderink    Changed prints to writes for checkc.
! 24. 2002-08-28  Vunderink    Fixed path_parameters to handle arrays.
! 23. 2002-04-18  Vunderink    Display any errors in template workfile
! 22. 2002-04-17  Vunderink    Added shell specification to script that builds
!                                jobs.
! 21. 2002-03-07  Vunderink    Remove file lock from write opens of files.
! 20. 2002-01-04  Vunderink    Made changes for buildlist tool.
! 19. 2001-11-02  Vunderink    Make sure only parameters to change are in cards
!                                when GUI update.
! 18. 2001-08-28  Vunderink    Fixed problem caused by process parameters 
!                                setting other process parameters but only one
!                                is in list of parameters to change.
! 17. 2001-08-08  Vunderink    Set packing option after clearing cardset
! 16. 2001-04-30  Vunderink    Update .mwb file after looking for all path
!                                parameters
! 15. 2001-01-09  Vunderink    Fixed clearing all parameters.
! 14. 2001-01-08  Vunderink    Run traps and build workfiles before submit.
! 13. 2001-01-07  Vunderink    Fixed problem saving masterCardset when template
!                                job is replaced.
! 12. 2000-12-19  Vunderink    Improve setting the alltraps, writeworkfiles and
!                                buildjobs flags, fixed the RESET button on
!                                the build jobnames screen and made argument
!                                changes for mwb_buffer_insert_list.
! 11. 2000-12-11  Vunderink    Added support for RESET button on parameter
!                                screens and remove RUNTRAPS from jobname screen
! 10. 2000-12-05  Vunderink    Redesigned screen.
!  9. 2000-10-20  Vunderink    Fixed writing changed array elements to new
!                                workfiles
!  8. 2000-10-20  Vunderink    Fixed Clear MWB Session button, modified to check
!                                length of jobs to build entries, and fixed
!                                to save lists.
!  7. 2000-10-19  Vunderink    Fixed bug in work_traps_and_write causing abort
!  6. 2000-10-18  Vunderink    Allow vundedk and wardrcj access to MWB screen.
!  5. 2000-10-16  Vunderink    Fix problem runing traps from parameter window
!  4. 2000-10-08  Vunderink    Removed SJqueue
!  3. 2000-09-14  Vunderink    Remove debug print 
!  2. 2000-09-04  Vunderink    Release all memory on delete to assist in finding
!                                memory leaks.
!  1. 2000-08-15  Vunderink    Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! The Portland Group compiler incorrectly executes code such as the following:
!
!       subroutine glotch_get_next (aaa,next)
!       type(xxxx_struct),pointer :: aaa,next
!       next => aaa%next
!       end subroutine
!
!       call glotch_get_next (ccc,ccc)
!
! To work around the bug, calls such as the above (in this file and several
! others) have been changed to:
!
!       call glotch_get_next (ccc,next)
!       ccc => next
!
! Changing the subroutine like this does not fix the problem:
!
!       subroutine glotch_get_next (aaa,next)
!       type(xxxx_struct),pointer :: aaa,next
!       type(xxxx_struct),pointer :: temp
!       temp => aaa%next
!       next => temp
!       end subroutine
!
! The Portland Group compiler required modifications in the code as follows:
!
!         Replaced reverse INDEX intrinsic function calls with STRING_INDEX.
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


      module cfemwb_module

      use buildlist_module
      use cardset_module
      use cfebuildlist_module
      use cfegui_module
      use cfeprocess_module
      use cfesl_module
      use cfestruct_module
      use cfeutil_module
      use cfewindow_module
      use cio_module
      use filebox_module
      use finquire_module
      use getsys_module
      use incjobname_module
      use cfelistbuffer_module
      use cfelist_module
      use path_module
      use pc_module
      use process_module
      use putsys_module
      use string_module
      use workfile_module

      implicit none

      private
      public :: cfemwb_create
      public :: cfemwb_initialize
      public :: cfemwb_delete
      public :: cfemwb_update
      public :: cfemwb_clear
      public :: cfemwb_parameter_update
      public :: cfemwb_parameter_run_traps
      public :: cfemwb_execute_submitjobs

      character(len=100),public,save :: cfemwb_ident = &
       '$Id: cfemwb.f90,v 1.31 2006/10/30 14:01:44 Glover prod sps $'

      contains


!!-------------------------------- create ----------------------------------!!
!!-------------------------------- create ----------------------------------!!
!!-------------------------------- create ----------------------------------!!


      subroutine cfemwb_create (obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      nullify(obj%templateProcessList)
      nullify(obj%jobnameList)
      nullify(obj%MWBipn)
      nullify(obj%MWBprocess)
      nullify(obj%MWBkeyword)
      nullify(obj%MWBlist)
      nullify(obj%templateJobDialog)
      nullify(obj%templateWorkfile)
      nullify(obj%incjobnameDialog)
      nullify(obj%submitDialog)
      nullify(obj%jobnameList_temp)
      nullify(obj%masterCardset)
      nullify(obj%masterBuffer)

      return
      end subroutine cfemwb_create


!!------------------------------ initialize --------------------------------!!
!!------------------------------ initialize --------------------------------!!
!!------------------------------ initialize --------------------------------!!


      subroutine cfemwb_initialize (obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      obj%ScreenId              = 1
      obj%templateJobName       = ' '
      obj%NtemplateProcessList  = 0
      obj%totalToBuild          = 0
      obj%NjobnameList          = 0
      obj%NjobnameList_temp     = 0
      obj%MWBtotal              = 0
      obj%MWBnlist              = 0
      obj%MWBselected           = 0
      obj%MWBparmselected       = 0
      obj%writeReport           = .true.
      obj%allTraps              = .true.
      obj%writeWorkfiles        = .true.
      obj%buildJobs             = .true.

      call getsys_current_dir (obj%working_dir)
      call cfelistbuffer_create  (obj%masterBuffer)

      allocate(obj%templateProcessList(1))
      allocate(obj%jobnameList(1))
      allocate(obj%MWBipn(1))
      allocate(obj%MWBprocess(1))
      allocate(obj%MWBkeyword(1))
      allocate(obj%MWBlist(1))

      return
      end subroutine cfemwb_initialize


!!-------------------------------- delete ----------------------------------!!
!!-------------------------------- delete ----------------------------------!!
!!-------------------------------- delete ----------------------------------!!


      subroutine cfemwb_delete (obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      call filebox_delete    (obj%templateJobDialog)
      call workfile_delete   (obj%templateWorkfile)
      call incjobname_delete (obj%incjobnameDialog)
      call incjobname_delete (obj%submitDialog)
      call cfelistbuffer_delete (obj%masterBuffer)
      if (associated(obj%masterCardset)) call cardset_delete (obj%masterCardset)

      if (associated(obj%templateProcessList))  &
                                             deallocate(obj%templateProcessList)
      if (associated(obj%jobnameList))       deallocate(obj%jobnameList)
      if (associated(obj%jobnameList_temp))  deallocate(obj%jobnameList_temp)
      if (associated(obj%MWBipn))            deallocate(obj%MWBipn)
      if (associated(obj%MWBprocess))        deallocate(obj%MWBprocess)
      if (associated(obj%MWBkeyword))        deallocate(obj%MWBkeyword)
      if (associated(obj%MWBlist))           deallocate(obj%MWBlist)

      return
      end subroutine cfemwb_delete


!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!


      subroutine cfemwb_update (obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      character(len=PC_LENGTH)             :: ctemp                    !local
      character(len=12)                    :: arrays(3)                !local
      integer                              :: narrays = 3              !local
      integer                              :: nitemp                   !local
      integer,pointer                      :: itemp(:)                 !local

!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!

      nullify(itemp)

      if (pc_gui_action_present('templateProcessList','ItemSelected')) then
        call pc_alloc_gui ('templateProcessList','ItemSelected',itemp,nitemp)
        if (nitemp .eq. 0) return
        if (itemp(nitemp) .eq. INIL) return
        if (itemp(nitemp) .le. obj%NtemplateProcessList) then
          obj%MWBselected = itemp(nitemp)
        endif
        return
      endif

      if (pc_gui_action_present('MWBipn','ItemSelected')) then
        call pc_alloc_gui ('MWBipn','ItemSelected',itemp,nitemp)
        if (nitemp .eq. 0) return
        if (itemp(nitemp) .eq. INIL) return
        if (itemp(nitemp) .le. obj%MWBtotal) then
          obj%MWBparmselected = itemp(nitemp)
        endif
        return
      endif

      if (pc_gui_action_present('MWBprocess','ItemSelected')) then
        call pc_alloc_gui ('MWBprocess','ItemSelected',itemp,nitemp)
        if (nitemp .eq. 0) return
        if (itemp(nitemp) .eq. INIL) return
        if (itemp(nitemp) .le. obj%MWBtotal) then
          obj%MWBparmselected = itemp(nitemp)
        endif
        return
      endif

      if (pc_gui_action_present('MWBkeyword','ItemSelected')) then
        call pc_alloc_gui ('MWBkeyword','ItemSelected',itemp,nitemp)
        if (nitemp .eq. 0) return
        if (itemp(nitemp) .eq. INIL) return
        if (itemp(nitemp) .le. obj%MWBtotal) then
          obj%MWBparmselected = itemp(nitemp)
        endif
        return
      endif

      if (pc_gui_action_present('Proceed2Parms','ButtonPress')) then
        if (obj%MWBselected .eq. 0) then
          call cfegui_beep()
          call pc_error ('No process selected')
          return
        endif
        if (len_trim(object%templateJobName) .eq. 0) then
          call pc_error ('Template workfile must be entered first')
          return
        endif
        if (object%NjobnameList .eq. 0) then
          call pc_error ('Workfiles to Build must be entered first')
          return
        endif
        call cfeprocess_create ('TEMPLATEPROCESSLIST',obj%MWBselected)
        return
      endif

      if (pc_gui_action_present('PathParameters','ButtonPress')) then
        call cfemwb_path_parameters (obj)
      endif

      if (pc_gui_action_present('MWBParmShow','ButtonPress')) then
        if (obj%MWBparmselected .eq. 0) then
          call cfegui_beep()
          call pc_error ('No parameter selected')
          return
        endif
        call cfebuildlist_create (obj,obj%MWBipn(obj%MWBparmselected),  &
                                      obj%MWBprocess(obj%MWBparmselected),  &
                                      obj%MWBkeyword(obj%MWBparmselected))
        return
      endif

      if (pc_gui_action_present('WriteReport','ButtonPress')) then
        call cfemwb_write_report_trap ('WriteReport')
      endif

      if (pc_gui_action_present('AllTraps','ButtonPress')) then
        call cfemwb_run_all_traps ('AllTraps')
      endif

      if (pc_gui_action_present('WriteWorkfiles','ButtonPress')) then
        call cfemwb_write_workfiles_trap ('WriteWorkfiles')
      endif

      if (pc_gui_action_present('BuildJobs','ButtonPress')) then
        call cfemwb_build_jobs_trap ('BuildJobs')
      endif

      if (pc_gui_action_present('SubmitJobs','ButtonPress')) then
        call cfemwb_submit_jobs_trap ('SubmitJobs')
        return
      endif

      if (pc_gui_action_present('selectTemplate','ButtonPress')) then
        call cfemwb_select_template_trap ('selectTemplate')
        return
      endif

      if (pc_gui_action_present('MBincJobname','ButtonPress')) then
        if (len_trim(object%templateJobName) .eq. 0) then
          call pc_error ('Template workfile must be entered first')
          return
        endif
        if (obj%totalToBuild .le. 0) then
          call pc_error ('Total number of jobs to build must be entered first')
          return
        endif
        call cfemwb_increment_jobname_trap ('MBincJobname')
        return
      endif

      if (pc_gui_action_present('MWBParmDelete','ButtonPress')) then
        if (obj%MWBparmselected .eq. 0) then
          call cfegui_beep()
          call pc_error ('No parameter selected')
          return
        endif
        call cfemwb_delete_trap (obj,obj%MWBparmselected)
      endif

      if (pc_gui_action_present('NewMWBSession','ButtonPress')) then
        call cfemwb_clear_trap ('NewMWBSession')
      endif

      if (pc_gui_action_present('ClearListBuffer','ButtonPress')) then
        call cfemwb_clear_trap ('ClearListBuffer')
      endif

      if (pc_gui_action_present('MWBParmClear','ButtonPress')) then
        call cfemwb_clear_trap ('MWBParmClear')
      endif

      ctemp = obj%templateJobName
      call pc_get ('templateJobName',obj%templateJobName,cfemwb_template_trap)
      if (len_trim(obj%templateJobName) .eq. 0) then
        if (len_trim(ctemp) .gt. 0) obj%templateJobName = ctemp
      endif
      call pc_get ('totalToBuild',obj%totalToBuild,cfemwb_total_to_build_trap)
      call pc_alloc ('jobnameList',obj%jobnameList,obj%NjobnameList,  &
                      cfemwb_jobname_list_trap)


!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!


      arrays(1) = 'MWBipn'
      arrays(2) = 'MWBprocess'
      arrays(3) = 'MWBkeyword'

      call pc_register_array_names ('MWBArraySet',arrays,narrays)
      call pc_put_minsize_arrayset ('MWBArraySet',obj%MWBtotal)
      call pc_put_maxsize_arrayset ('MWBArraySet',obj%MWBtotal)

      call pc_put_minsize_array ('templateProcessList',obj%NtemplateProcessList)
      call pc_put_maxsize_array ('templateProcessList',obj%NtemplateProcessList)

      call pc_put_minsize_array ('jobnameList'        ,obj%totalToBuild        )
      call pc_put_maxsize_array ('jobnameList'        ,obj%totalToBuild        )

      call pc_put ('templateJobName'      ,obj%templateJobName     )
      call pc_put ('templateProcessList'  ,obj%templateProcessList ,  &
                                           obj%NtemplateProcessList)
      call pc_put ('totalToBuild'         ,obj%totalToBuild        )
      call pc_put ('jobnameList'          ,obj%jobnameList         ,  &
                                           obj%NjobnameList        )
      call pc_put ('MWBipn'               ,obj%MWBipn              ,  &
                                           obj%MWBtotal            )
      call pc_put ('MWBprocess'           ,obj%MWBprocess          ,  &
                                           obj%MWBtotal            )
      call pc_put ('MWBkeyword'           ,obj%MWBkeyword          ,  &
                                           obj%MWBtotal            )

      if (obj%writeReport) then
        call pc_put ('WriteReportFlag' ,' ')
      else
        call pc_put ('WriteReportFlag' ,'X')
      endif

      if (obj%allTraps) then
        call pc_put ('AllTrapsFlag' ,' ')
      else
        call pc_put ('AllTrapsFlag' ,'X')
      endif

      if (obj%writeWorkfiles) then
        call pc_put ('WriteWorkfilesFlag' ,' ')
      else
        call pc_put ('WriteWorkfilesFlag' ,'X')
      endif

      if (obj%buildJobs) then
        call pc_put ('BuildJobsFlag' ,' ')
      else
        call pc_put ('BuildJobsFlag' ,'X')
      endif

      return
      end subroutine cfemwb_update


!!--------------------------- update_cardset -------------------------------!!
!!--------------------------- update_cardset -------------------------------!!
!!--------------------------- update_cardset -------------------------------!!


      subroutine cfemwb_update_cardset (obj)
      implicit none
      type(cfestruct),pointer           :: obj                         !argument

      integer                           :: i                           !local
      integer                           :: nkeys                       !local
      character(len=PC_LENGTH)          :: keyword                     !local
      character(len=PC_LENGTH)          :: name                        !local
      character(len=PC_LENGTH),pointer  :: array(:)                    !local
      character(len=PC_LENGTH)          :: errmsg                      !local
      integer                           :: narray                      !local
      type(cfelist_struct),pointer      :: current,next                !local

      nullify (current) ! jpa
      nullify (next) ! jpa

      if (len_trim(obj%templateJobname) .eq. 0) return

      nkeys = cardset_num_keywords (obj%masterCardset)
      if (nkeys .gt. 0) then
        do i=1,nkeys
          keyword = cardset_get_keyword (obj%masterCardset,i)
          if (keyword(1:8) .eq. 'MWBLIST#') then
            call cardset_remove_keyword (obj%masterCardset,keyword)
          endif
        enddo
      endif

      nullify(array)
      narray = 0
      call cfelistbuffer_get_num_lists (obj%masterBuffer,narray)
      if (narray .gt. 0) then
        call cfelistbuffer_get_first_list (obj%masterBuffer,current)
        do
          if (.not. associated(current)) exit
          narray = 0
          call cfelist_get_name      (current,name)
          call cfelist_alloc_strings (current,array,narray)
          if (narray .gt. 0) then
            call cardset_put_array (obj%masterCardset,'MWBLIST#'//trim(name),  &
                                    array,narray)
          endif
!!!       call cfelist_get_next (current,current)
          call cfelist_get_next (current,next)
          current => next
        enddo
        if (associated(array)) deallocate(array)
      endif

      call cardset_get_scalar (obj%masterCardset,'TEMPLATEJOBNAME',name,errmsg)
!!!   i = index        (name,'.wrk',.TRUE.) - 1
      i = string_index (name,'.wrk',.TRUE.) - 1
      if (i .eq. 0) i = len_trim(name)
      call cfeutil_save_mwb_cardset (obj%masterCardset,  &
                                     trim(obj%working_dir) //  &
                                     name(1:i) // '.mwb')

      return
      end subroutine cfemwb_update_cardset


!!------------------------- write_report_trap ------------------------------!!
!!------------------------- write_report_trap ------------------------------!!
!!------------------------- write_report_trap ------------------------------!!


      subroutine cfemwb_write_report_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           ::  i                          !local
      integer                           ::  j                          !local

      integer                           ::  i2                         !local
      integer                           ::  lun                        !local
      integer                           ::  nstring                    !local
      integer                           ::  narray                     !local
      integer                           ::  istat                      !local

      integer,pointer                   ::  ichars(:)                  !local
      character(len=2)                  ::  mode                       !local
      character(len=PC_LENGTH)          ::  filename                   !local
      character(len=PC_LENGTH)          ::  bigword                    !local
      character(len=PC_LENGTH)          ::  value                      !local
      character(len=PC_LENGTH)          ::  errmsg                     !local
      character(len=20560)              ::  string                     !local
      character(len=PC_LENGTH),pointer  ::  array(:)                   !local
      integer                           ::  temp

      if (len_trim(object%templateJobName) .eq. 0) then
        call pc_error ('Template workfile must be entered first')
        return
      endif

      if (object%NjobnameList .eq. 0) then
        call pc_error ('Workfiles to Build must be entered first')
        return
      endif

      nullify(array)

!!!   i = index        (object%templateJobname,'.wrk',.TRUE.) - 1
      i = string_index (object%templateJobname,'.wrk',.TRUE.) - 1
      if (i .le. 0) then
        filename =trim(object%working_dir)//trim(object%templateJobname)//'.prn'
      else
        filename =trim(object%working_dir)//object%templateJobname(1:i)//'.prn'
      endif
      mode = "w"
      lun = cio_fopen(trim(filename),mode,file_lock=FILE_LOCK_DISABLED)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      allocate(ichars(object%MWBtotal))
      ichars = 0

      do i=1,object%MWBtotal
        bigword = trim(object%MWBprocess(i))//'#'//trim(object%MWBipn(i))//  &
                  '#'//trim(object%MWBkeyword(i))
        narray = 0
        call cardset_alloc_array (object%masterCardset,trim(bigword),array,  &
                                  narray,errmsg)
        ichars(i) = len_trim(bigword)
        do j=1,narray
          ichars(i) = max(len_trim(array(j)),ichars(i))
        enddo
      enddo
      if (associated(array)) deallocate(array)

      string = ''
      i2     = 1
      do i=1,object%MWBtotal
        bigword = trim(object%MWBprocess(i))//'#'//trim(object%MWBipn(i))//  &
                  '#'//trim(object%MWBkeyword(i))
        write(string(i2:i2+ichars(i)),'(A,A)') trim(bigword),' '
        i2 = i2 + ichars(i) + 1
      enddo
      temp = len_trim(string)
      nstring = cio_fputline(string,temp,lun)

      do i=1,object%NjobnameList
        string = ''
        i2     = 1
        do j=1,object%MWBtotal
           bigword = trim(object%MWBprocess(j))//'#'//trim(object%MWBipn(j))// &
                     '#'//trim(object%MWBkeyword(j))
           call cardset_get_element (object%masterCardset,bigword,i,value, &
                                     errmsg)
           write(string(i2:i2+ichars(j)),'(A,A)') trim(value),' '
           i2 = i2 + ichars(j) + 1
        enddo
        temp = len_trim(string)
        nstring = cio_fputline(string,temp,lun)
      enddo

      istat = cio_fclose(lun)

      object%writeReport = .false.

      return
      end subroutine cfemwb_write_report_trap


!!--------------------------- run_all_traps --------------------------------!!
!!--------------------------- run_all_traps --------------------------------!!
!!--------------------------- run_all_traps --------------------------------!!


      subroutine cfemwb_run_all_traps (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument


      if (len_trim(object%templateJobName) .eq. 0) then
        call pc_error ('Template workfile must be entered first')
        return
      endif

      if (object%NjobnameList .eq. 0) then
        call pc_error ('Workfiles to Build must be entered first')
        return
      endif

      if (object%MWBtotal .eq. 0) then
        call pc_error ('No parameters have been changed')
        return
      endif

      if (.not. object%allTraps) then
        call pc_info ('Nothing Changed...Did not run traps')
        return
      endif

      object%writeWorkfiles = .false.
      call cfemwb_work_traps_and_write (object)

      if (object%allTraps .and. .not. pc_update_error()) then
        object%allTraps       = .false.
        object%writeWorkfiles = .true.
      endif

      return
      end subroutine cfemwb_run_all_traps


!!------------------------ write_workfiles_trap ----------------------------!!
!!------------------------ write_workfiles_trap ----------------------------!!
!!------------------------ write_workfiles_trap ----------------------------!!


      subroutine cfemwb_write_workfiles_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument


      if (len_trim(object%templateJobName) .eq. 0) then
        call pc_error ('Template workfile must be entered first')
        return
      endif

      if (object%NjobnameList .eq. 0) then
        call pc_error ('Workfiles to Build must be entered first')
        return
      endif

      if (object%MWBtotal .eq. 0) then
        call pc_error ('No parameters have been changed')
        return
      endif

      if (.not. object%writeWorkfiles) then
        call pc_info ('Nothing Changed...Did not write workfiles')
        return
      endif

      call cfemwb_work_traps_and_write (object)

      if (object%writeWorkfiles .and. .not. pc_update_error()) then
        object%allTraps       = .false.
        object%writeWorkfiles = .false.
        object%buildJobs      = .true.
      endif

      return
      end subroutine cfemwb_write_workfiles_trap


!!------------------------ work_traps_and_write ----------------------------!!
!!------------------------ work_traps_and_write ----------------------------!!
!!------------------------ work_traps_and_write ----------------------------!!


      subroutine cfemwb_work_traps_and_write (obj,spec_ipn,spec_keyword,  &
                                                       spec_array,spec_narray)
      implicit none
      type(cfestruct),pointer           :: obj                         !argument
      integer,optional                  :: spec_ipn                    !argument
      character(len=*),optional         :: spec_keyword                !argument
      character(len=*),pointer,optional :: spec_array(:)               !argument
      integer,optional                  :: spec_narray                 !argument

      integer                           ::  i                          !local
      integer                           ::  j                          !local
      integer                           ::  k                          !local
      integer                           ::  i1                         !local
      integer                           ::  i2                         !local
      integer                           ::  ipn                        !local
      integer                           ::  indx                       !local
      integer                           ::  ipound                     !local
      integer                           ::  is_array                   !local
      integer                           ::  narray                     !local
      character(len=12)                 ::  cnum                       !local
      character(len=PC_LENGTH)          ::  bigword                    !local
      character(len=PC_LENGTH)          ::  value                      !local
      character(len=PC_LENGTH),pointer  ::  array(:)                   !local
      character(len=PC_LENGTH)          ::  errmsg                     !local
      logical                           ::  clear_cards                !local
      type(workfile_struct),pointer     ::  workfile                   !local
      type(process_struct),pointer      ::  process                    !local
      type(process_struct),pointer      ::  previous,next              !local
      type(process_struct),pointer      ::  template_process           !local
      type(process_struct),pointer      ::  start                      !local

      nullify (workfile) ! jpa
      nullify (process) ! jpa
      nullify (previous) ! jpa
      nullify (next) ! jpa

      if (len_trim(object%templateJobName) .eq. 0) then
        call pc_error ('Template workfile must be entered first')
        return
      endif

      if (obj%NjobnameList .eq. 0) then
        call pc_error ('Workfiles to Build must be entered first')
        return
      endif

      if (.not. present(spec_ipn)) then
        if (obj%MWBtotal .eq. 0) then
          call pc_error ('No parameters have been changed')
          return
        endif
      endif

      nullify(array)

      do i=1,obj%NjobnameList
        call workfile_create (workfile)
        call workfile_copy   (obj%templateWorkfile,workfile)
        nullify(start)

        call workfile_get_first_process (workfile,process)
        call workfile_get_first_process (obj%templateWorkfile,template_process)
        do j=1,obj%NtemplateProcessList
          if (.not. associated(process)) exit
          clear_cards = .true.
          if (j .eq. 2) then
            if (clear_cards) then
              call process_clear_process_cards (process)
              call process_clear_gui_cards     (process)
              clear_cards = .false.
            endif
            call process_put_gui_cscalar (process,'JOBNAME#MODIFYFIELD',  &
                                                   trim(obj%jobnameList(i)))
          endif
          if (obj%MWBtotal .gt. 0) then
            do k=1,obj%MWBtotal
              call string_cc2ii (obj%MWBipn(k),ipn)
              if (ipn .eq. j) then
                bigword = trim(obj%MWBprocess(k))//'#'//  &
                          trim(obj%MWBipn(k))    //'#'//  &
                          trim(obj%MWBkeyword(k))
                ipound = index(bigword,'#')
                select case (ipound)
                  case (0); cycle
                  case default
                    value = ''
                    call cardset_get_element (obj%masterCardset,bigword,i,  &
                                              value,errmsg)
                end select
                if (.not.associated(start)) start => process
!!!             is_array = index        (obj%MWBkeyword(k),'#',.true.)
                is_array = string_index (obj%MWBkeyword(k),'#',.true.)
                i2 = len_trim(obj%MWBkeyword(k))
                if (is_array .eq. 0) then
                  bigword = trim(obj%MWBkeyword(k))//'#MODIFYFIELD'
                  if (clear_cards) then
                    call process_clear_process_cards (process)
                    call process_clear_gui_cards     (process)
                    clear_cards = .false.
                  endif
                  call process_put_gui_cscalar (process,bigword,value)
                else
                  bigword =obj%MWBkeyword(k)(1:is_array-1)//'#REPLACEELEMENTS'
                  narray = 0
                  i1     = is_array+1
                  indx   = 0
                  if (i1 .le. i2)  &
                                call string_cc2ii(obj%MWBkeyword(k)(i1:i2),indx)
                  call process_alloc_by_keyword (process,'GUI',bigword,  &
                                                 array,narray)
                  if (narray .eq. 0) then
                    call process_alloc_by_keyword (template_process,'PROCESS', &
                                              obj%MWBkeyword(k)(1:is_array-1), &
                                              array,narray)
                  endif
                  if (indx.gt.0 .and. indx.le.narray) then
                    array(indx) = value
                    if (clear_cards) then
                      call process_clear_process_cards (process)
                      call process_clear_gui_cards     (process)
                      clear_cards = .false.
                    endif
                    call process_put_gui_carray (process,bigword,array,narray)
                    bigword = obj%MWBkeyword(k)(1:is_array-1)//'#MODIFYINDEX'
                    call process_put_gui_iscalar (process,bigword,indx)
                  endif
                endif
              endif
            enddo
          endif
          if (present(spec_ipn)) then
            if (j .eq. spec_ipn) then
              if (i .le. spec_narray) then
                if (.not.associated(start)) start = process
!!!             is_array = index        (spec_keyword,'#',.true.)
                is_array = string_index (spec_keyword,'#',.true.)
                i2 = len_trim(spec_keyword)
                if (is_array .eq. 0) then
                  bigword = spec_keyword(1:i2)//'#MODIFIYFIELD'
                  if (clear_cards) then
                    call process_clear_process_cards (process)
                    call process_clear_gui_cards     (process)
                    clear_cards = .false.
                  endif
                  call process_put_gui_cscalar (process,bigword,spec_array(i))
                else
                  bigword = spec_keyword(1:is_array-1)//'#REPLACEELEMENTS'
                  narray = 0
                  i1     = is_array+1
                  indx   = 0
                  if (i1 .ge. i2) call string_cc2ii(spec_keyword(i1:i2),indx)
                  call process_alloc_by_keyword (process,'GUI',bigword,  &
                                                 array,narray)
                  if (narray .eq. 0) then
                    call process_alloc_by_keyword (template_process,'PROCESS', &
                                                 spec_keyword(1:is_array-1),   &
                                                 array,narray)
                  endif
                  if (indx.gt.0 .and. indx.le.narray) then
                    array(indx) = spec_array(i)
                    if (clear_cards) then
                      call process_clear_process_cards (process)
                      call process_clear_gui_cards     (process)
                      clear_cards = .false.
                    endif
                    call process_put_gui_carray  (process,bigword,array,narray)
                    bigword = spec_keyword(1:is_array-1)//'#MODIFYINDEX'
                    call process_put_gui_iscalar (process,bigword,indx)
                  endif
                endif
              endif
            endif
          endif
          if (clear_cards) then
            call process_clear_process_cards (process)
            call process_clear_gui_cards     (process)
            clear_cards = .false.
          endif
          call process_get_previous      (process,previous)
          call process_copy_global_cards (previous,process)
          call process_update            (process,from_cards=.true.)
!!!       call process_get_next          (process,process)
          call process_get_next          (process,next)
          process => next
!!!       call process_get_next          (template_process,template_process)
          call process_get_next          (template_process,next)
          template_process => next
        enddo
          
        if (associated(array)) deallocate(array)

        if (obj%allTraps) then
          call workfile_run_all_traps (workfile,start,from_cards=.true., &
                                       frontend=.true.,report_errors=.true., &
                                       workfile_indx = i)
        endif

        if (obj%writeWorkfiles) then
          if (len_trim(obj%jobnameList(i)) .gt. 0) then
            call workfile_write (workfile,trim(obj%working_dir)//  &
                                          trim(obj%jobnameList(i))//'.wrk')
          else
            call string_ii2cc (i,cnum)
            call pc_error ('Invalid jobname for workfile #'//trim(cnum)//  &
                         '  Could not write workfile.')
          endif
        endif

        call workfile_delete (workfile)
      enddo

      return
      end subroutine cfemwb_work_traps_and_write


!!-------------------------- build_jobs_trap -------------------------------!!
!!-------------------------- build_jobs_trap -------------------------------!!
!!-------------------------- build_jobs_trap -------------------------------!!


      subroutine cfemwb_build_jobs_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           :: i                           !local
      integer                           :: istat                       !local
      integer                           :: ncmd                        !local
      integer                           :: lun                         !local
      integer                           :: pid                         !local
      integer                           :: ncjn                        !local
      integer                           :: ncards                      !local
      character(len=2)                  :: mode                        !local
      character(len=12)                 :: cnum                        !local
      character(len=10)                 :: cpid                        !local
      character(len=1024)               :: cmd                         !local
      character(len=80)                 :: filename                    !local
      character(len=PC_LENGTH),pointer  :: cards(:)                    !local
      integer                           :: temp


      if (len_trim(object%templateJobName) .eq. 0) then
        call pc_error ('Template workfile must be entered first')
        return
      endif

      if (object%NjobnameList .eq. 0) then
        call pc_error ('Workfiles to Build must be entered first')
        return
      endif

      call cfemwb_work_traps_and_write (object)
      if (pc_update_error()) return

      pid = getsys_pid()
      call string_ii2cc (pid,cpid)

      filename = '/tmp/cfebld'//trim(cpid)//'.csh'
      mode = "w"
      lun = cio_fopen(trim(filename),mode,file_lock=FILE_LOCK_DISABLED)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      cmd = '#!/bin/csh'
      write(STDOUT,*) trim(cmd)
      temp = len_trim(cmd)
      ncmd = cio_fputline(cmd,temp,lun)
      do i=1,object%NjobnameList
        if (len_trim(object%jobnameList(i)) .gt. 0) then
!!!       ncjn = index        (object%jobnameList(i),'.wrk',.TRUE.) - 1
          ncjn = string_index (object%jobnameList(i),'.wrk',.TRUE.) - 1
          if (ncjn .le. 0) ncjn = len_trim(object%jobnameList(i))
          cmd = trim(object%prog_cfebld)//' '//  &
                trim(object%working_dir)//object%jobnameList(i)(1:ncjn)
          write(STDOUT,*) trim(cmd)
          temp = len_trim(cmd)
          ncmd = cio_fputline(cmd,temp,lun)
        else
          call string_ii2cc (i,cnum)
          call pc_error ('Invalid jobname for workfile #'//trim(cnum)//  &
                         '  Could not build jobfile.')
        endif
      enddo
      istat = cio_fclose(lun)
      istat = cio_chmod(trim(filename),'rwx,rwx,rwx')

      cmd = trim(filename)//' >>&! /tmp/cfebld.'//trim(cpid)
      write(STDOUT,*) trim(cmd)
      call putsys_texec (trim(cmd))
      write(STDOUT,*) 'Finished building jobs'
      istat = cio_remove(trim(filename))
      write(STDOUT,*) 'Removed script'

      do i=1,object%NjobnameList
        call cfesl_add (object%jobnameList(i)(1:ncjn),'BUILT')
      enddo

      nullify(cards)
      call cfeutil_read_file ('/tmp/cfebld.'//trim(cpid),cards,ncards,  &
                              'Build Job Output')
      if (ncards .gt. 0) then
        do i=1,ncards
          write(STDOUT,*) trim(cards(i))
        enddo
!       call pc_put_gui ('INFO','INFO',cards,ncards)
        deallocate(cards)
      endif

      if (.not. pc_update_error()) then
        object%last_jobfile = trim(object%working_dir)//  &
                            object%jobnameList(object%NjobnameList)(1:ncjn)//  &
                            '.job'

        object%allTraps       = .false.
        object%writeWorkfiles = .false.
        object%buildJobs      = .false.
      endif

      return
      end subroutine cfemwb_build_jobs_trap


!!-------------------------- submit_jobs_trap ------------------------------!!
!!-------------------------- submit_jobs_trap ------------------------------!!
!!-------------------------- submit_jobs_trap ------------------------------!!


      subroutine cfemwb_submit_jobs_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      character(len=PC_LENGTH)          :: local_key                   !local
      type(cfewindow_struct),pointer    :: window                      !local
      integer                           :: window_id                   !local

      nullify (window) ! jpa

      if (len_trim(object%templateJobName) .eq. 0) then
        call pc_error ('Template workfile must be entered first')
        return
      endif

      if (object%NjobnameList .eq. 0) then
        call pc_error ('Workfiles to Build must be entered first')
        return
      endif

      local_key = keyword
      call string_to_upper (local_key)

      call cfewindow_create           (window)
      call cfewindow_set_keyword      (window ,trim(local_key))
      call cfewindow_set_index        (window ,1)
      call cfewindow_set_window_type  (window ,'INCJOBNAME')
      call cfewindow_get_window_id    (window ,window_id)

      if (.not. associated(object%submitDialog)) then
        call incjobname_create (object%submitDialog)
      else
        call incjobname_update (object%submitDialog)
      endif
      call cfegui_create (window_id,'SELECT JOBS','mwb_submitjobs.xml', &
                           .false.)
      call cfewindow_set_current (window)
      call pc_put_gui ('ROOT'   ,'Visible','False')
      call pc_put_gui ('PREVIEW','Visible','False')

      return
      end subroutine cfemwb_submit_jobs_trap


!!------------------------ select_template_trap ----------------------------!!
!!------------------------ select_template_trap ----------------------------!!
!!------------------------ select_template_trap ----------------------------!!


      subroutine cfemwb_select_template_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      character(len=PC_LENGTH)          :: local_key                   !local
      type(cfewindow_struct),pointer    :: window                      !local
      integer                           :: window_id                   !local

      nullify (window) ! jpa

      local_key = keyword
      call string_to_upper (local_key)

      call cfewindow_create           (window)
      call cfewindow_set_keyword      (window ,'TEMPLATEJOBNAME')
      call cfewindow_set_window_type  (window ,'FILEBOX')
      call cfewindow_get_window_id    (window ,window_id)

      if (.not. associated(object%templateJobDialog)) then
        call filebox_create (object%templateJobDialog,  &
                             trim(object%working_dir) // '*.wrk')
      else
        call filebox_restore (object%templateJobDialog)
      endif
      call cfegui_create (window_id,'FILE SELECTION','filebox.xml',.false.)
      call cfewindow_set_current (window)

      return
      end subroutine cfemwb_select_template_trap


!!----------------------------- clear_trap ---------------------------------!!
!!----------------------------- clear_trap ---------------------------------!!
!!----------------------------- clear_trap ---------------------------------!!


      subroutine cfemwb_clear_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      character(len=PC_LENGTH)          :: local_key                   !local
      character(len=PC_LENGTH)          :: bigword                     !local
      integer                           :: i                           !local
      integer                           :: ntemp                       !local
      integer                           :: build_id                    !local
      integer,pointer                   :: window_ids(:)               !local
      integer                           :: nwindow_ids                 !local

      local_key = keyword
      call string_to_upper (local_key)

      if (trim(local_key) .eq. 'CLEARLISTBUFFER') then
        nullify(window_ids)
        call cfewindow_get_all_of_type ('PARAMETER',window_ids)
        if (associated(window_ids)) then
          nwindow_ids = size(window_ids)
          do i=1,nwindow_ids
              call cfebuildlist_delete_popup (window_ids(i))
          enddo
          deallocate(window_ids)
        endif
        if (associated(object%masterBuffer)) then
          call cfelistbuffer_clear (object%masterBuffer)
        endif
        return
      endif

      if (trim(local_key) .eq. 'NEWMWBSESSION') then
        if (associated(object%jobnameList)) deallocate(object%jobnameList)
        object%NjobnameList = 0
        allocate(object%jobnameList(1))
        object%totalToBuild = 0
        nullify(window_ids)
        call cfewindow_get_all_of_type ('PARAMETER',window_ids)
        if (associated(window_ids)) then
          build_id = cfewindow_match ('PARAMETER','BUILDLIST','BUILDLIST',0)
          nwindow_ids = size(window_ids)
          do i=1,nwindow_ids
            if (window_ids(i) .ne. build_id) then
              call cfebuildlist_delete_popup (window_ids(i))
            endif
          enddo
          deallocate(window_ids)
        endif
        if (associated(object%masterCardset)) then
          call cfemwb_update_cardset     (object)
          call cardset_clear              (object%masterCardset)
          call cardset_set_packing_option (object%masterCardset,CARDSET_PACKED)
        endif
      endif

      if (trim(local_key) .eq. 'NEWTEMPLATE') then
        if (associated(object%jobnameList)) deallocate(object%jobnameList)
        object%NjobnameList = 0
        allocate(object%jobnameList(1))
        object%totalToBuild = 0
      endif

      if (trim(local_key) .eq. 'MWBPARMCLEAR') then
        if (object%MWBtotal .gt. 0) then
          do i=1,object%MWBtotal
            bigword = trim(object%MWBprocess(i))//'#'//trim(object%MWBipn(i))//&
                                               '#'//trim(object%MWBkeyword(i))
            call cardset_remove_keyword (object%masterCardset,trim(bigword))
          enddo
          call cardset_remove_keyword (object%masterCardset,'MWBIPN')
          call cardset_remove_keyword (object%masterCardset,'MWBPROCESS')
          call cardset_remove_keyword (object%masterCardset,'MWBKEYWORD')
          call cfemwb_update_cardset  (object)
        endif
      endif

      if (object%MWBtotal .gt. 0) then
        ntemp = object%MWBtotal
        call cfeutil_clear_array (object%MWBipn    ,ntemp)
        ntemp = object%MWBtotal
        call cfeutil_clear_array (object%MWBprocess,ntemp)
        ntemp = object%MWBtotal
        call cfeutil_clear_array (object%MWBkeyword,ntemp)
        object%MWBtotal = 0
      endif

      object%writeReport    = .true.
      object%allTraps       = .true.
      object%writeWorkfiles = .true.
      object%buildJobs      = .true.

      return
      end subroutine cfemwb_clear_trap


!!-------------------------------- clear -----------------------------------!!
!!-------------------------------- clear -----------------------------------!!
!!-------------------------------- clear -----------------------------------!!


      subroutine cfemwb_clear (obj)
      implicit none
      type(cfestruct),pointer          :: obj                         !argument

      integer                          :: i                           !local
      integer                          :: ntemp                       !local

      integer,pointer                  :: window_ids(:)               !local
      integer                          :: nwindow_ids                 !local
      integer                          :: buildlist_id                !local


      obj%templateJobName      = ' '
      obj%NtemplateProcessList = 0
      if (associated(obj%templateProcessList)) then
        deallocate(obj%templateProcessList)
        allocate(obj%templateProcessList(1))
        obj%templateProcessList(1) = ' '
      endif

      if (associated(obj%templateWorkfile)) then
        call workfile_delete (obj%templateWorkfile)
        nullify(obj%templateWorkfile)
      endif

      if (associated(obj%jobnameList)) deallocate(obj%jobnameList)
      obj%NjobnameList = 0
      allocate(obj%jobnameList(1))
      obj%totalToBuild = 0

      if (obj%MWBtotal .gt. 0) then
        ntemp = obj%MWBtotal
        call cfeutil_clear_array (obj%MWBipn    ,ntemp)
        ntemp = obj%MWBtotal
        call cfeutil_clear_array (obj%MWBprocess,ntemp)
        ntemp = obj%MWBtotal
        call cfeutil_clear_array (obj%MWBkeyword,ntemp)
        obj%MWBtotal = 0
      endif

      nullify(window_ids)
      call cfewindow_get_all_of_type ('PARAMETER',window_ids)
      if (associated(window_ids)) then
        buildlist_id = cfewindow_match ('PARAMETER','BUILDLIST','BUILDLIST',0)
        nwindow_ids = size(window_ids)
        do i=1,nwindow_ids
          if (window_ids(i) .ne. buildlist_id) then
            call cfebuildlist_delete_popup (window_ids(i))
          endif
        enddo
        deallocate(window_ids)
      endif

      obj%writeReport    = .true.
      obj%allTraps       = .true.
      obj%writeWorkfiles = .true.
      obj%buildJobs      = .true.

      return
      end subroutine cfemwb_clear


!!----------------------- increment_jobname_trap ---------------------------!!
!!----------------------- increment_jobname_trap ---------------------------!!
!!----------------------- increment_jobname_trap ---------------------------!!


      subroutine cfemwb_increment_jobname_trap (keyword)
      implicit none
      character(len=*),intent(in)      ::  keyword                    !argument

      character(len=PC_LENGTH)         :: local_key                   !local
      character(len=PC_LENGTH)         :: parameter_name              !local
      character(len=PC_LENGTH)         :: window_type                 !local
      character(len=PC_LENGTH)         :: window_title                !local

      character(len=PC_LENGTH)         :: ctemp                       !local
      integer                          :: i                           !local
      integer                          :: window_id                   !local
      type(cfewindow_struct),pointer   :: window                      !local
      type(buildlist_struct),pointer   :: parameter                   !local

      nullify (window) ! jpa
      nullify (parameter) ! jpa

      if (len_trim(object%templateJobName) .eq. 0) then
        call pc_error ('Template workfile must be entered first')
        return
      endif

      window_type  = 'PARAMETER'
      window_title = 'Build Jobnames'
      local_key = 'jobnameList'
      call string_to_upper (local_key)
      parameter_name = local_key
      window_id=cfewindow_match (window_type,local_key,parameter_name,0)
      if (window_id .ne. -1) then
        call cfegui_jump_window (window_id)
        return
      endif
!!!   i = index        (object%templateJobname,'.wrk',.TRUE.) - 1
      i = string_index (object%templateJobname,'.wrk',.TRUE.) - 1
      if (i .le. 0) then
        ctemp = object%templateJobname
      else
        ctemp = object%templateJobname(1:i)
      endif
      call buildlist_create (parameter,object%working_dir,object%NjobnameList, &
                            trim(parameter_name),trim(ctemp),.true.)
      call buildlist_set_buffer (parameter,object%masterBuffer)
      call buildlist_set_values (parameter,object%jobnameList,  &
                                                            object%NjobnameList)
      call pc_put_gui ('RUNTRAPS'     ,'VISIBLE','FALSE')
      call pc_put_gui ('PARAMETERLIST','VISIBLE','FALSE')
      call cfewindow_create          (window)
      call cfewindow_set_window_type (window ,window_type)
      call cfewindow_set_keyword     (window ,local_key)
      call cfewindow_set_value       (window ,parameter_name)
      call cfewindow_set_parameter   (window ,parameter)
      call cfewindow_get_window_id   (window ,window_id)

      call cfegui_create (window_id,trim(window_title),'mwb_parameter.xml', &
                           .false.)

      call cfewindow_set_current (window)

      return
      end subroutine cfemwb_increment_jobname_trap


!!--------------------------- template_trap --------------------------------!!
!!--------------------------- template_trap --------------------------------!!
!!--------------------------- template_trap --------------------------------!!


      subroutine cfemwb_template_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      character(len=PC_LENGTH)        ::  errmsg                       !local
      character(len=PC_LENGTH)        ::  name                         !local
      character(len=PC_LENGTH)        ::  key                          !local
      character(len=PC_LENGTH)        ::  wcurrent_name                !local
      character(len=PC_LENGTH),pointer :: array(:)                     !local
      character(len=PC_LENGTH),pointer :: listnames(:)                 !local
      integer                         ::  nlistnames                   !local
      integer                         ::  narray                       !local

      integer                         ::  nkeys                        !local
      integer                         ::  i                            !local
      integer                         ::  j                            !local
      integer                         ::  k                            !local

      logical                         ::  not_found                    !local
      type(cfelist_struct),pointer   ::  list                         !local
      type(cfelist_struct),pointer   ::  lastlist                     !local

      nullify (array) ! jpa
      nullify (list) ! jpa
      nullify (lastlist) ! jpa

      if (len_trim(object%templateJobName) .eq. 0) return

!!!   i = index        (trim(object%templateJobName),'/',.true.)
      i = string_index (trim(object%templateJobName),'/',.true.)
      if (i .eq. 0) then
        wcurrent_name = object%templateJobName
        object%templateJobName = trim(object%working_dir) // trim(wcurrent_name)
      endif

!!!   i = index        (object%templateJobName,'.wrk',.TRUE.)
      i = string_index (object%templateJobName,'.wrk',.TRUE.)
      if (i .eq. 0) then
        i = len_trim(object%templateJobName)
        object%templateJobName(i+1:) = '.wrk'
      endif

      if (associated(object%templateWorkfile)) then
        call workfile_delete (object%templateWorkfile)
      endif

      call cfemwb_clear_trap ('NewTemplate')

      call workfile_create   (object%templateWorkfile)
      call workfile_read     (object%templateWorkfile, object%templateJobName, &
                              report_errors=.true.)
      call workfile_get_name (object%templateWorkfile, object%templateJobName)

      call workfile_process_list (object%templateWorkfile,  &
                                  object%templateProcessList,  &
                                  object%NtemplateProcessList)

      if (object%NjobnameList .eq. 0 .and.object%MWBtotal .eq. 0) then
!!!     i = index        (object%templateJobname,'.wrk',.TRUE.) - 1
        i = string_index (object%templateJobname,'.wrk',.TRUE.) - 1
        if (i .le. 0) then
          name = object%templateJobname
        else
          name = object%templateJobname(1:i)
        endif
        if (associated(object%masterCardset)) then
          call cardset_delete(object%masterCardset)
        endif
        call cfeutil_get_mwb_cardset (object%masterCardset,  &
                                      trim(object%working_dir) //  &
                                      trim(name) // '.mwb')

        if (associated(object%masterCardset)) then
          if (cardset_array_matches(object%masterCardset,       &
                                    'TEMPLATEPROCESSLIST',      &
                                    object%templateProcessList, &
                                    object%NtemplateProcessList)) then
            call cardset_alloc_array(object%masterCardset,'JOBNAMELIST',  &
                                            object%jobnameList ,  &
                                            object%NjobnameList,errmsg)
            object%totalToBuild = object%NjobnameList
            call cardset_alloc_array(object%masterCardset,'MWBIPN',  &
                                            object%MWBipn, &
                                            object%MWBtotal,errmsg)
            call cardset_alloc_array(object%masterCardset,'MWBPROCESS',  &
                                            object%MWBprocess, &
                                            object%MWBtotal,errmsg)
            call cardset_alloc_array(object%masterCardset,'MWBKEYWORD',  &
                                            object%MWBkeyword, &
                                            object%MWBtotal,errmsg)
            nullify(listnames)
            call cfelistbuffer_get_names &
                                   (object%masterBuffer,listnames,nlistnames)
            nkeys = cardset_num_keywords (object%masterCardset)
            do i = 1,nkeys
              key = cardset_get_keyword (object%masterCardset,i)
              j = index(key,'MWBLIST#')
              if (j .eq. 1) then
                name = key(9:)
                not_found = .true.
                if (nlistnames .gt. 0) then
                  do k=1,nlistnames
                    if (trim(listnames(k)) .eq. trim(name)) then
                      not_found = .false.
                      call pc_error ('Could not load list '//trim(name)//  &
                                     ' for it already exists in buffer')
                      exit
                    endif
                  enddo
                endif
                call cardset_alloc_array(object%masterCardset,  &
                                         key,array,narray,errmsg)
                if (narray .gt. 0 .and. not_found) then
                  call cfelist_create      (list)
                  call cfelist_set_name    (list,trim(name))
                  call cfelist_put_strings (list,array,narray)
                  call cfelistbuffer_get_last_list &
                                           (object%masterBuffer,lastList)
                  if (associated(lastList)) then
                    call cfelistbuffer_insert_list(object%masterBuffer,list)
                  else
                    call cfelistbuffer_set_first_list (object%masterBuffer,list)
                    call cfelistbuffer_set_last_list  (object%masterBuffer,list)
                  endif
                endif
              endif
            enddo
          else
            if (associated(object%masterCardset)) then
              call cardset_delete (object%masterCardset)
            endif
          endif
        endif
      endif

      if (.not. associated(object%masterCardset)) then
        call cardset_create             (object%masterCardset)
        call cardset_set_packing_option (object%masterCardset,CARDSET_PACKED)
      endif
      call cardset_put_array (object%masterCardset,'TEMPLATEPROCESSLIST', &
                                                  object%templateProcessList,  &
                                                  object%NtemplateProcessList)
      call cardset_put_scalar (object%masterCardset,'TEMPLATEJOBNAME',  &
                               object%templateJobName)
      call cfemwb_update_cardset (object)

      object%writeReport    = .true.
      object%allTraps       = .true.
      object%writeWorkfiles = .true.
      object%buildJobs      = .true.

      return
      end subroutine cfemwb_template_trap


!!------------------------ total_to_build_trap -----------------------------!!
!!------------------------ total_to_build_trap -----------------------------!!
!!------------------------ total_to_build_trap -----------------------------!!


      subroutine cfemwb_total_to_build_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      if (len_trim(object%templateJobName) .eq. 0) then
        call pc_error ('Template workfile must be entered first')
        return
      endif

      if (object%totalToBuild .gt. object%NjobnameList) then
        if (object%NjobnameList .gt. 0) then
          call pc_warning ('You must specify additional jobnames')
          object%allTraps       = .true.
          object%writeWorkfiles = .true.
          object%buildJobs      = .true.
        endif
        call cfeutil_append_array_element (object%jobnameList,       &
                                           object%NjobnameList,' ',  &
                                           object%totalToBuild -     &
                                           object%NjobnameList)
      else if (object%totalToBuild .lt. object%NjobnameList) then
        call cfeutil_remove_array_element (object%jobnameList,       &
                                           object%NjobnameList,      &
                                           object%totalToBuild+1,    &
                                           object%NjobnameList)
      endif

      return
      end subroutine cfemwb_total_to_build_trap


!!------------------------- jobname_list_trap ------------------------------!!
!!------------------------- jobname_list_trap ------------------------------!!
!!------------------------- jobname_list_trap ------------------------------!!


      subroutine cfemwb_jobname_list_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           ::  i,k                        
      integer                           ::  istat                      !local
      integer                           ::  nctemp                     !local
      character(len=PC_LENGTH) ,pointer ::  ctemp(:)                   !local

      if (len_trim(object%templateJobName) .eq. 0) then
        call pc_error ('Template workfile must be entered first')
        return
      endif

      nctemp = 0
      nullify(ctemp)

      do i=1,object%NjobnameList
        istat = finquire_output (trim(object%working_dir) //  &
                                 trim(object%jobnameList(i))//'.wrk')
        if (istat .eq. FINQUIRE_FOUND) then
          call cfeutil_append_array_element (ctemp,nctemp,  &
                                              trim(object%jobnameList(i)))
        endif
        k=ichar(object%jobnameList(i)(1:1))
        if(k.lt.65.or.(k.gt.90.and.k.lt.97).or.k.gt.122)then
          call pc_error ('Workfile name '//trim(object%jobnameList(i))//  &
                         ' MUST begin with an alpha.')
        endif
        if (len_trim(object%jobnameList(i)) .gt. 15) then
          call pc_error ('Workfile name '//trim(object%jobnameList(i))//  &
                         ' is too long. Maximum is 15 characters.')
        endif
      enddo

      if (pc_update_error()) return

      if (nctemp .gt. 0) then
        do i=1,nctemp
          call pc_warning ('Workfile '//trim(ctemp(i))//' already exist')
        enddo
        deallocate(ctemp)
      endif

      if (.not. associated(object%masterCardset)) then
        call cardset_create             (object%masterCardset)
        call cardset_set_packing_option (object%masterCardset,CARDSET_PACKED)
      endif
      call cardset_put_array  (object%masterCardset,'JOBNAMELIST',  &
                                                  object%jobnameList,  &
                                                  object%NjobnameList)
      call cfemwb_update_cardset (object)

      object%writeReport    = .true.
      object%allTraps       = .true.
      object%writeWorkfiles = .true.
      object%buildJobs      = .true.

      return
      end subroutine cfemwb_jobname_list_trap


!!--------------------------- parameter_update -----------------------------!!
!!--------------------------- parameter_update -----------------------------!!
!!--------------------------- parameter_update -----------------------------!!


      subroutine cfemwb_parameter_update (obj,process,parameter)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument
      type(process_struct),pointer        :: process                  !argument
      type(buildlist_struct),pointer      :: parameter                !argument

      integer                              :: i                        !local
      integer                              :: j                        !local
      integer                              :: k                        !local
      integer                              :: indx                     !local
      integer                              :: ipn                      !local
      integer                              :: narray                   !local

      character(len=10)                    :: cipn                     !local
      character(len=PC_LENGTH)             :: bigword                  !local
      character(len=PC_LENGTH)             :: pname                    !local

      character(len=PC_LENGTH)             :: keyword                  !local

      character(len=PC_LENGTH) ,pointer    :: array(:)                 !local




      nullify(array)
      narray = 0

      call buildlist_get_name        (parameter,keyword)
      call buildlist_alloc_valuelist (parameter,array  ,narray)
      call process_get_name          (process  ,pname)
      call process_get_ipn           (process  ,ipn)

      call string_ii2cc (ipn,cipn)
      bigword = trim(pname)//'#'//trim(cipn)//'#'//trim(keyword)
      if (.not. associated(object%masterCardset)) then
        call cardset_create             (object%masterCardset)
        call cardset_set_packing_option (object%masterCardset,CARDSET_PACKED)
      endif
      if (.not. cardset_keyword_present(obj%masterCardset,bigword)) then
        indx = obj%MWBtotal + 1
        do j=1,obj%MWBtotal
          call string_cc2ii (obj%MWBipn(j),k)
          if (ipn .lt. k) then
            indx = j
            exit
          endif
        enddo
        i = obj%MWBtotal
        call cfeutil_insert_array_element (obj%MWBipn,i,trim(cipn),indx)
        i = obj%MWBtotal
        call cfeutil_insert_array_element (obj%MWBprocess,i,trim(pname),indx)
        i = obj%MWBtotal
        call cfeutil_insert_array_element (obj%MWBkeyword,i,trim(keyword),indx)
        obj%MWBtotal = obj%MWBtotal + 1
        call cardset_put_array (obj%masterCardset,'MWBIPN',  &
                                obj%MWBipn,obj%MWBtotal)
        call cardset_put_array (obj%masterCardset,'MWBPROCESS',  &
                                obj%MWBprocess,obj%MWBtotal)
        call cardset_put_array (obj%masterCardset,'MWBKEYWORD',  &
                                obj%MWBkeyword,obj%MWBtotal)
      endif

      call cardset_put_array (obj%masterCardset,bigword,array,narray)
      if (associated(array)) deallocate(array)

      call cfemwb_update_cardset (obj)

      obj%writeReport    = .true.
      obj%allTraps       = .true.
      obj%writeWorkfiles = .true.
      obj%buildJobs      = .true.

      return
      end subroutine cfemwb_parameter_update


!!----------------------------- delete_trap --------------------------------!!
!!----------------------------- delete_trap --------------------------------!!
!!----------------------------- delete_trap --------------------------------!!


      subroutine cfemwb_delete_trap (obj,indx)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument
      integer        ,intent(in)          :: indx                     !argument


      integer                             :: ntemp                    !local
      character(len=PC_LENGTH)            :: bigword                  !local


      bigword = trim(obj%MWBprocess(indx))//'#'//trim(obj%MWBipn(indx))//  &
                '#'//trim(obj%MWBkeyword(indx))
      call cardset_remove_keyword (obj%masterCardset,trim(bigword))

      ntemp = obj%MWBtotal
      call cfeutil_remove_array_element (obj%MWBipn    ,ntemp,indx,indx)
      ntemp = obj%MWBtotal
      call cfeutil_remove_array_element (obj%MWBprocess,ntemp,indx,indx)
      ntemp = obj%MWBtotal
      call cfeutil_remove_array_element (obj%MWBkeyword,ntemp,indx,indx)
      obj%MWBtotal = ntemp

      if (.not. associated(obj%masterCardset)) then
        call cardset_create             (obj%masterCardset)
        call cardset_set_packing_option (obj%masterCardset,CARDSET_PACKED)
      endif
      call cardset_put_array (obj%masterCardset,'MWBIPN',  &
                              obj%MWBipn,obj%MWBtotal)
      call cardset_put_array (obj%masterCardset,'MWBPROCESS',  &
                              obj%MWBprocess,obj%MWBtotal)
      call cardset_put_array (obj%masterCardset,'MWBKEYWORD',  &
                              obj%MWBkeyword,obj%MWBtotal)
      call cfemwb_update_cardset (obj)

      obj%writeReport    = .true.
      obj%allTraps       = .true.
      obj%writeWorkfiles = .true.
      obj%buildJobs      = .true.

      return
      end subroutine cfemwb_delete_trap


!!------------------------- parameter_run_traps ----------------------------!!
!!------------------------- parameter_run_traps ----------------------------!!
!!------------------------- parameter_run_traps ----------------------------!!


      subroutine cfemwb_parameter_run_traps (obj,process,parameter)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument
      type(process_struct),pointer        :: process                  !argument
      type(buildlist_struct),pointer      :: parameter                !argument

      integer                             :: ipn                      !local
      integer                             :: narray  = 0              !local
      character(len=PC_LENGTH)            :: keyword                  !local
      character(len=PC_LENGTH) ,pointer   :: array(:)                 !local


      nullify(array)

      call buildlist_get_name        (parameter,keyword)
      call buildlist_alloc_valuelist (parameter,array  ,narray)
      call process_get_ipn          (process  ,ipn)

      obj%allTraps       = .true.
      obj%writeWorkfiles = .false.
      call cfemwb_work_traps_and_write (obj,ipn,keyword,array,narray)
      obj%allTraps       = .false.
      obj%writeWorkfiles = .false.

      return
      end subroutine cfemwb_parameter_run_traps


!!-------------------------- execute_submitjobs ----------------------------!!
!!-------------------------- execute_submitjobs ----------------------------!!
!!-------------------------- execute_submitjobs ----------------------------!!


      subroutine cfemwb_execute_submitjobs (obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      integer                             :: i                        !local
      integer                             :: itemp                    !local
      integer                             :: nindx2submit = 0         !local
      character(len=12),pointer           :: indx2submit(:)           !local

      nullify(indx2submit)

      call incjobname_selection (obj%submitDialog,indx2submit,nindx2submit)
      if (nindx2submit .eq. 0) return

      call cfemwb_work_traps_and_write (obj)
      if (pc_update_error()) return

      if (object%writeWorkfiles) then
        object%allTraps       = .false.
        object%writeWorkfiles = .false.
        object%buildJobs      = .true.
      endif

      if (associated(obj%SJjobs))        deallocate(obj%SJjobs)
      if (associated(obj%SJcount))       deallocate(obj%SJcount)
      if (associated(obj%SJnext))        deallocate(obj%SJnext)

      do i=1,nindx2submit
        call string_cc2ii (indx2submit(i),itemp)
        if (itemp .gt. obj%NjobnameList) exit
      enddo
      obj%SJtotal = i - 1

      allocate(obj%SJjobs(obj%SJtotal))
      allocate(obj%SJcount(obj%SJtotal))
      allocate(obj%SJnext(obj%SJtotal))

      obj%SJnext  = ''

      do i=1,obj%SJtotal
        call string_cc2ii (indx2submit(i),itemp)
        obj%SJjobs(i)  = obj%jobnameList(itemp)
        obj%SJcount(i) = i
      enddo

      obj%ScreenId = 3
      call pc_jump_screen ('SubmitJobScreen')

      return
      end subroutine cfemwb_execute_submitjobs


!!---------------------------- path_parameters -----------------------------!!
!!---------------------------- path_parameters -----------------------------!!
!!---------------------------- path_parameters -----------------------------!!


      subroutine cfemwb_path_parameters (obj)
      implicit none
      type(cfestruct),pointer              :: obj                      !argument


      integer                              :: i                        !local
      integer                              :: j                        !local
      integer                              :: k                        !local
      integer                              :: ipn                      !local
      integer                              :: ikey                     !local
      integer                              :: indx                     !local
      integer                              :: narray                   !local
      integer                              :: nkeys                    !local
      integer                              :: nature                   !local
      character(len=10)                    :: cipn                     !local
      character(len=PC_LENGTH)             :: pname                    !local
      character(len=PC_LENGTH)             :: bigword                  !local
      character(len=PC_LENGTH)             :: value                    !local
      character(len=PC_LENGTH) ,pointer    :: array(:)                 !local
      character(len=PC_LENGTH) ,pointer    :: keys(:)                  !local
      type(process_struct)     ,pointer    :: process,next             !local

      nullify (array)
      nullify (keys)
      nullify (next) ! jpa
      nullify (process) ! jpa

      if (len_trim(obj%templateJobName) .eq. 0) then
        call pc_error ('Template Workfile must be specified first')
        return
      endif

      if (obj%NjobnameList .eq. 0) then
        call pc_error ('Workfiles to Build must be entered first')
        return
      endif

      if (.not. associated(obj%masterCardset)) then
        call cardset_create             (obj%masterCardset)
        call cardset_set_packing_option (obj%masterCardset,CARDSET_PACKED)
      endif

      call workfile_get_first_process (obj%templateWorkfile,process)
      do
        if (.not. associated(process)) exit
        call process_alloc_keywords (process,'PROCESS',keys,nkeys)
        if (nkeys .gt. 0) then
          call process_get_name (process ,pname)
          call process_get_ipn  (process ,ipn)
          call string_ii2cc     (ipn     ,cipn)
          do ikey=1,nkeys
            j = index(keys(ikey),'PATH')
            if (j .eq. 0) cycle
            nature = process_get_keyword_nature (process,'PROCESS',keys(ikey))
            if (nature .eq. CARDSET_SCALAR) then
              call process_get_by_keyword (process,'PROCESS',keys(ikey),value)
            else if (nature .eq. CARDSET_ARRAY) then
              call process_get_by_keyword (process,'PROCESS',keys(ikey),1,value)
              keys(ikey) = trim(keys(ikey))//'#1'
            else
              cycle
            endif
            if (trim(value) .eq. 'NONE') cycle
            bigword = trim(pname)//'#'//trim(cipn)//'#'//trim(keys(ikey))
            if (.not. cardset_keyword_present(obj%masterCardset,bigword)) then
              indx = obj%MWBtotal + 1
              do j=1,obj%MWBtotal
                call string_cc2ii (obj%MWBipn(j),k)
                if (ipn .lt. k) then
                  indx = j
                  exit
                endif
              enddo
              i = obj%MWBtotal
              call cfeutil_insert_array_element (obj%MWBipn,i,  &
                                                 trim(cipn),indx)
              i = obj%MWBtotal
              call cfeutil_insert_array_element (obj%MWBprocess,i,  &
                                                 trim(pname),indx)
              i = obj%MWBtotal
              call cfeutil_insert_array_element (obj%MWBkeyword,i,  &
                                                 trim(keys(ikey)),indx)
              obj%MWBtotal = obj%MWBtotal + 1
              call cardset_put_array (obj%masterCardset,'MWBIPN',  &
                                      obj%MWBipn,obj%MWBtotal)
              call cardset_put_array (obj%masterCardset,'MWBPROCESS',  &
                                      obj%MWBprocess,obj%MWBtotal)
              call cardset_put_array (obj%masterCardset,'MWBKEYWORD',  &
                                      obj%MWBkeyword,obj%MWBtotal)
            endif
            if (associated(array)) deallocate(array)
            narray = obj%NjobnameList
            allocate(array(narray))
            array = value
            call cardset_put_array (obj%masterCardset,bigword,array,narray)
            obj%writeReport    = .true.
            obj%allTraps       = .true.
            obj%writeWorkfiles = .true.
            obj%buildJobs      = .true.
          enddo
        endif
!!!     call process_get_next (process,process)
        call process_get_next (process,next)
        process => next
      enddo
      call cfemwb_update_cardset (obj)

      if (associated(array)) deallocate(array)
      if (associated(keys))  deallocate(keys)

      return
      end subroutine cfemwb_path_parameters


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module cfemwb_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

