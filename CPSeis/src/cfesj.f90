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
! Name       : cfesj
! Category   : cfe
! Written    : 1999-11-12   by: Donna K. Vunderink
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Allow the user to submit jobs to a batch queue.
! Portability: No known limitations, but see notes below.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! SUBMIT_JOB tabbed screen allows the user to submit jobs to a batch queue and
! to specify job series information about jobs being submitted.
!
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
!031. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
! 30. 2006-01-10  B. Menger    Removed Unused Variables.
! 29. 2004-08-23  SMCook       Incorporated mfilebox (multi-file selection).
! 28. 2004-05-03  SMCook       Filebox default is now *.wrk instead of *.job.
! 27. 2003-11-18  SMCook       Monitor putsys_texec status and call pc_error
!                               upon failure for cfesub case.  Heretofore errors
!                               from cfesub were unmonitored (cfesub.c also
!                               required modification to make this happen).
! 26. 2003-10-03  Stoeckley    Replace reverse INDEX intrinsic functions with
!                               STRING_INDEX to make the Portland Group
!                               compiler happy.
! 25. 2003-09-15  Stoeckley    Changed name from cfe_sj to cfesj;
!                               changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
! 24. 2003-03-05  SMCook       Changed cfesub logic to be more portable by
!                               eliminating use of the cfesub script, which
!                               utilized '--which'.  The cps_config.dat file
!                               now contains the explicit path info needed.
! 23. 2002-09-23  Vunderink    Changed prints to writes for checkc.
! 22. 2002-09-13  Vunderink    Added support for Jobname, Count and Next list
!                                buttons.
! 21. 2002-03-07  Vunderink    Removed unused variable.
! 20. 2001-05-01  Vunderink    Remove blank rows from arrays before submit
! 19. 2001-04-11  Vunderink    Added support for independent series
! 18. 2001-03-15  Vunderink    Removed series requirement for submitting
!                                multiple jobs and added pause between submits
! 17. 2001-03-12  Vunderink    Enhanced support for reloading series file
! 16. 2000-10-09  Vunderink    Fixed bug introduced by removing SJqueue
! 15. 2000-10-08  Vunderink    Removed SJqueue and HelpSection
! 14. 2000-09-04  Vunderink    Eliminate un-necessary calls to string_to_upper
!                                and release all memory on delete to aid in
!                                finding memory leaks.
! 13. 2000-08-15  Vunderink    Removed use of cfe_constants, changed
!                                cfe_incjobname calls to incjobname, and
!                                changed character variables to use PC_LENGTH
! 12. 2000-05-26  Vunderink    Changed fputline call for old way no longer 
!                                worked
! 11. 2000-05-23  Vunderink    Fixed bug in building series without count=1
! 10. 2000-05-11  Vunderink    Added ! to redirection of cfebld and cfesub
!                                for users with noclobber in their .cshrc
!  9. 2000-05-09  Vunderink    Make sure pointer arrays are allocated.
!  8. 2000-04-24  Vunderink    Changed character variables to use named constant
!                                CFE_LENGTH, moved some code from cfe module as
!                                routines execute_filebox, execute_incjobname 
!                                and reset_incjobname, added trap on SJjobs,
!                                added parameter SJReloadSeries, and made
!                                improvements to submit_trap.
!  7. 2000-03-08  Vunderink    Change cfebld and cfesub commands to be built
!                                new variables prog_cfebld and prog_cfesub.
!  6. 2000-02-27  Vunderink    Added support for date and time 
!  5. 2000-02-23  Vunderink    Added submit job help
!  4. 2000-02-16  Vunderink    Modified to route output from cfebld and cfesub
!                                to file and then display file contents in INFO
!                                window.  Fixed series count in call to 
!                                cfesl_add to "count / total"
!  3. 2000-02-06  Vunderink    Create series file even though there are no jobs
!                                to submit and pass next series to cfesl_add.
!  2. 2000-02-03  Vunderink    Continued to complete code needed to support
!                                screen.
!  1. 1999-11-12  Vunderink    Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
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


      module cfesj_module

      use cfestruct_module
      use cfewindow_module
      use cfegui_module
      use cfesl_module
      use cfeutil_module
      use filebox_module
      use mfilebox_module
      use incjobname_module
      use pc_module
      use finquire_module
      use getsys_module
      use putsys_module
      use path_module
      use cardset_module
      use cio_module

      implicit none

      private
      public :: cfesj_create                   ! uses the parameter cache.
      public :: cfesj_initialize
      public :: cfesj_update                   ! uses the parameter cache.
      public :: cfesj_delete
      public :: cfesj_execute_filebox
      public :: cfesj_execute_incjobname
      public :: cfesj_reset_incjobname

      character(len=100),public,save :: cfesj_ident = &
       '$Id: cfesj.f90,v 1.31 2006/09/18 13:32:42 Glover prod sps $'

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine cfesj_create (obj)
      implicit none
      type(cfestruct),pointer        :: obj                           !argument

      nullify(obj%SJjobs_dialog)
      nullify(obj%SJlist_dialog)
      nullify(obj%SJseries_dialog)
      nullify(obj%SJincjobname_dialog)
      nullify(obj%SJjobs)
      nullify(obj%SJcount)
      nullify(obj%SJnext)
      nullify(obj%SJjobs_temp)
      nullify(obj%SJcount_temp)
      nullify(obj%SJnext_temp)

      return
      end subroutine cfesj_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine cfesj_delete (obj)
      implicit none
      type(cfestruct),pointer          :: obj                         !argument
      integer                          :: istat                       !local

      if (object%SJseries_locked) then
        istat = cio_unlock_file(trim(object%SJlock_file))
        object%SJseries_locked = .false.
        object%SJlock_file     = ' '
      endif

      call mfilebox_delete   (obj%SJjobs_dialog)
      call filebox_delete    (obj%SJlist_dialog)
      call filebox_delete    (obj%SJseries_dialog)
      call incjobname_delete (obj%SJincjobname_dialog)

      if (associated(obj%SJjobs))        deallocate(obj%SJjobs)
      if (associated(obj%SJcount))       deallocate(obj%SJcount)
      if (associated(obj%SJnext))        deallocate(obj%SJnext)
      if (associated(obj%SJjobs_temp))   deallocate(obj%SJjobs_temp)
      if (associated(obj%SJcount_temp))  deallocate(obj%SJcount_temp)
      if (associated(obj%SJnext_temp))   deallocate(obj%SJnext_temp)

      return
      end subroutine cfesj_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine cfesj_initialize (obj)
      implicit none
      type(cfestruct),pointer        :: obj                           !argument

      allocate(obj%SJjobs(1))
      allocate(obj%SJcount(1))
      allocate(obj%SJnext(1))

      obj%SJseries        = ' '
      obj%SJindependent   = .false.
      obj%SJlock_file     = ' '
      obj%SJseries_locked = .false.
      obj%SJdate          = ' '
      obj%SJtime          = ' '
      obj%SJjobs(1)       = ' '
      obj%SJcount(1)      = 0
      obj%SJnext(1)       = ' '
      obj%SJjobsList      = ' '
      obj%SJcountList     = ' '
      obj%SJnextList      = ' '

      obj%SJtotal         = 0
      obj%SJtotal_temp    = 0

      return
      end subroutine cfesj_initialize


!!----------------------------- update -------------------------------------!!
!!----------------------------- update -------------------------------------!!
!!----------------------------- update -------------------------------------!!


      subroutine cfesj_update (obj)
      implicit none
      type(cfestruct),target         :: obj                           !argument

      integer                         :: i                             !local


      integer                         :: njobs                         !local
      integer                         :: ncount                        !local
      integer                         :: nnext                         !local
      logical                         :: found_job                     !local

      object => obj                                          ! needed for traps.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      if (pc_gui_action_present('SJJobSelect','ButtonPress')) then
        call cfesj_jobselect_trap ('SJJobSelect')
        return
      endif

      if (pc_gui_action_present('SJReloadSeries','ButtonPress')) then
        call cfesj_reload_trap ('SJReloadSeries')
        return
      endif

      if (pc_gui_action_present('SJJobsList','ButtonPress')) then
        call cfesj_list_trap ('SJJobsList')
      endif

      if (pc_gui_action_present('SJCountList','ButtonPress')) then
        call cfesj_list_trap ('SJCountList')
      endif

      if (pc_gui_action_present('SJNextList','ButtonPress')) then
        call cfesj_list_trap ('SJNextList')
      endif

      if (pc_gui_action_present('SJSubmit','ButtonPress')) then
        call cfesj_submit_trap ('SJSubmit')
      endif

      if (pc_gui_action_present('SJClear','ButtonPress')) then
        call cfesj_clear_trap ('SJClear')
      endif

      if (pc_gui_action_present('SJIncrement','ButtonPress')) then
        call cfesj_increment_trap ('SJIncrement')
        return
      endif

      if (pc_gui_action_present('SJUpdateSeries','ButtonPress')) then
        call cfesj_update_series_trap ('SJUpdateSeries')
      endif

      call pc_get   ('SJjobsList'   ,obj%SJjobsList   ,cfesj_jobslist_trap )
      call pc_get   ('SJcountList'  ,obj%SJcountList  ,cfesj_countlist_trap)
      call pc_get   ('SJnextList'   ,obj%SJnextList   ,cfesj_nextlist_trap )
      call pc_get   ('SJSeries'     ,obj%SJseries     ,cfesj_series_trap   )
      call pc_get   ('SJIndependent',obj%SJindependent)
      call pc_get   ('SJDate'       ,obj%SJdate       ,cfesj_date_trap     )
      call pc_get   ('SJTime'       ,obj%SJtime       ,cfesj_time_trap     )
      njobs  = obj%SJtotal
      ncount = obj%SJtotal
      nnext  = obj%SJtotal
      call pc_alloc ('SJJobs'  ,obj%SJjobs  , njobs ,cfesj_jobs_trap)
      call pc_alloc ('SJCount' ,obj%SJcount , ncount)
      call pc_alloc ('SJNext'  ,obj%SJnext  , nnext )

      if (njobs.ne.ncount .or. njobs.ne.nnext) then
        call pc_error ('Jobs, Count, Next arrays have different lengths')
        obj%SJtotal = min(njobs,ncount,nnext)
      else
        obj%SJtotal = njobs
      endif

      if (pc_gui_action_present('SubmitJobScreen','EnterScreen')) then
        if (len_trim(obj%currentJobName) .gt. 0) then
          found_job = .false.
          if (obj%SJtotal .gt. 0) then
            do i=1,obj%SJtotal
              if (trim(obj%SJjobs(i)) .eq. trim(obj%currentJobName)) then
                found_job = .true.
                exit
              endif
            enddo
          endif
          if (.not. found_job) call cfesj_append_job(object,obj%currentJobName)
        endif
      endif


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!




!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('SJIndependent',(/ 'YES', 'NO ' /),2)

      call pc_put ('SJSeries'     ,obj%SJseries)
      call pc_put ('SJIndependent',obj%SJindependent)
      call pc_put ('SJDate'       ,obj%SJdate  )
      call pc_put ('SJTime'       ,obj%SJtime  )
      call pc_put ('SJJobs'       ,obj%SJjobs  , obj%SJtotal)
      call pc_put ('SJCount'      ,obj%SJcount , obj%SJtotal)
      call pc_put ('SJNext'       ,obj%SJnext  , obj%SJtotal)

      return
      end subroutine cfesj_update


!!----------------------------- series_trap --------------------------------!!
!!----------------------------- series_trap --------------------------------!!
!!----------------------------- series_trap --------------------------------!!


      subroutine cfesj_series_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      integer                           :: i                           !local
      integer                           :: lun                         !local
      integer                           :: istat                       !local
      integer                           :: nstring                     !local
      integer                           :: njobs                       !local
      integer                           :: ncount                      !local
      integer                           :: nnext                       !local
      integer                           :: ntemp                       !local
      character(len=PC_DATACARD_LENGTH) :: string                      !local

      character(len=PC_LENGTH)          :: series_dir                  !local
      character(len=PC_LENGTH)          :: seriesfile                  !local
      character(len=PC_LENGTH)          :: errmsg                      !local
      character(len=2)                  :: mode                        !local
      type(cardset_struct),pointer      :: cardset                     !local


      if (object%SJseries_locked) then
        istat = cio_unlock_file(trim(object%SJlock_file))
        object%SJseries_locked = .false.
        object%SJlock_file     = ' '
      endif

      if (len_trim(object%SJseries) .eq. 0) return

      series_dir = path_get_dir(object%SJseries)
      if (trim(series_dir) .eq. PATH_EMPTY) return
      
      seriesfile = path_get_file(object%SJseries)
!!!   i = index        (seriesfile,'.series',.TRUE.)
      i = string_index (seriesfile,'.series',.TRUE.)
      if (i .eq. 0) then
        object%SJseries = trim(seriesfile)
      else
        object%SJseries = seriesfile(1:i-1)
      endif

      seriesfile = trim(series_dir)//trim(object%SJseries)//'.series'
      object%SJlock_file  = trim(series_dir)//trim(object%SJseries)//'.lock'
      istat      = cio_lock_file(trim(object%SJlock_file),1800)
      object%SJseries_locked = .true.

      istat = finquire_input(trim(seriesfile))
      if (istat .eq. FINQUIRE_FOUND) then
        mode = "r"
        lun = cio_fopen(trim(seriesfile),mode)
        if (lun .lt. 100) then
          call pc_error ('Error opening file '//trim(seriesfile))
          write(STDOUT,*) 'Error opening file '//trim(seriesfile)//'  lun=',lun
          istat = cio_unlock_file(trim(object%SJlock_file))
          object%SJseries_locked = .false.
          object%SJlock_file     = ' '
          return
        endif

        nullify(cardset)
        call cardset_create (cardset)
        call cardset_set_packing_option (cardset,CARDSET_PACKED)

        do
          nstring = cio_fgetline(string,PC_DATACARD_LENGTH,lun)
          if (nstring .lt. 0) exit
          call cardset_add_card (cardset,string(1:nstring))
        enddo
        istat = cio_fclose(lun)

        njobs  = object%SJtotal
        ncount = object%SJtotal
        nnext  = object%SJtotal

        call cardset_get_scalar  (cardset,'Independent',object%SJindependent,  &
                                                                        errmsg)
        call cardset_alloc_array (cardset,'Jobs' ,object%SJjobs ,njobs, errmsg)
        call cardset_alloc_array (cardset,'Count',object%SJcount,ncount,errmsg)
        call cardset_alloc_array (cardset,'Next' ,object%SJnext ,nnext, errmsg)
        call cardset_delete (cardset)

        if (njobs.ne.ncount .or. njobs.ne.nnext) then
          call pc_error ('Jobs, Count, Next arrays have different lengths')
          object%SJtotal = min(njobs,ncount,nnext)
        else
          object%SJtotal = njobs
        endif

        njobs  = object%SJtotal
        ncount = object%SJtotal
        nnext  = object%SJtotal
        ntemp  = object%SJtotal

        i = 1
        do 
          if (i .gt. ntemp) exit
          if (object%SJcount(i) .lt. 1) then
            call cfeutil_remove_array_element (object%SJjobs ,njobs ,i,i)
            call cfeutil_remove_array_element (object%SJcount,ncount,i,i)
            call cfeutil_remove_array_element (object%SJnext ,nnext ,i,i)
            ntemp = ntemp - 1
          else
            i = i + 1
          endif
        enddo

        if (njobs.ne.ncount .or. njobs.ne.nnext) then
          call pc_error ('Jobs, Count, Next arrays have different lengths')
          object%SJtotal = min(njobs,ncount,nnext)
        else
          object%SJtotal = njobs
        endif
      endif

      return
      end subroutine cfesj_series_trap


!!---------------------------- reload_trap ---------------------------------!!
!!---------------------------- reload_trap ---------------------------------!!
!!---------------------------- reload_trap ---------------------------------!!


      subroutine cfesj_reload_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      integer                         :: istat                         !local
      integer                         :: window_id                     !local
      character(len=PC_LENGTH)        :: home_dir                      !local
      character(len=PC_LENGTH)        :: series_dir                    !local
      logical                         :: exists                        !local
      type(cfewindow_struct),pointer  :: window                        !local
      
      nullify (window) ! jpa

      if (object%SJseries_locked) then
        istat = cio_unlock_file(trim(object%SJlock_file))
        object%SJseries_locked = .false.
        object%SJlock_file     = ' '
      endif

      if (object%SJtotal .gt. 0) then
        call pc_error ('You must clear entries first')
        return
      endif

      call cfewindow_create           (window)
      call cfewindow_set_keyword      (window ,'SJSERIES')
      call cfewindow_set_window_type  (window ,'FILEBOX')
      call cfewindow_get_window_id    (window ,window_id)

      call getsys_env ('HOME',home_dir)
      series_dir = trim(home_dir) // '/.cfe_series'
      inquire (file=trim(series_dir),exist=exists)
      if (.not. exists) call putsys_texec ('mkdir '// trim(series_dir),istat)

      if (.not. associated(object%SJseries_dialog)) then
        call filebox_create (object%SJseries_dialog,  &
                             trim(series_dir) // '/*.series')
      else
        call filebox_update (object%SJseries_dialog)
      endif
      call cfegui_create (window_id,'FILE SELECTION','filebox.xml',.false.)
      call cfewindow_set_current (window)

      return
      end subroutine cfesj_reload_trap


!!----------------------------- list_trap ----------------------------------!!
!!----------------------------- list_trap ----------------------------------!!
!!----------------------------- list_trap ----------------------------------!!


      subroutine cfesj_list_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      character(len=PC_LENGTH)        :: local_key                     !local
      type(cfewindow_struct),pointer  :: window                        !local
      integer                         :: window_id                     !local
      integer                         :: istat                         !local

      nullify (window) ! jpa

      if (object%SJseries_locked) then
        istat = cio_unlock_file(trim(object%SJlock_file))
        object%SJseries_locked = .false.
        object%SJlock_file     = ' '
      endif

      local_key = keyword
      call string_to_upper (local_key)

      call cfewindow_create           (window)
      call cfewindow_set_keyword      (window ,trim(local_key))
      call cfewindow_set_window_type  (window ,'FILEBOX')
      call cfewindow_get_window_id    (window ,window_id)

      if (.not. associated(object%SJlist_dialog)) then
        call filebox_create (object%SJlist_dialog,  &
                             trim(object%working_dir) // '*.lst')
      else
        call filebox_update (object%SJlist_dialog)
      endif 
      call cfegui_create (window_id,'FILE SELECTION','filebox.xml',.false.)
      call cfewindow_set_current (window)

      return
      end subroutine cfesj_list_trap


!!--------------------------- jobslist_trap --------------------------------!!
!!--------------------------- jobslist_trap --------------------------------!!
!!--------------------------- jobslist_trap --------------------------------!!


      subroutine cfesj_jobslist_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      integer                         ::  i                            !local
      integer                         ::  njobs                        !local
      integer                         ::  ncount                       !local
      integer                         ::  nnext                        !local

      njobs  = object%Sjtotal
      ncount = object%Sjtotal
      nnext  = object%Sjtotal

      call cfeutil_read_file (object%SJjobsList,object%SJjobs,njobs)

      if (njobs .gt. object%Sjtotal) then
        do i=object%Sjtotal+1,njobs
          call cfeutil_append_array_element (object%SJcount,ncount,i)
        enddo
        call cfeutil_append_array_element (object%SJnext ,nnext ,' ',  &
                                           njobs-object%Sjtotal)
        object%Sjtotal = njobs

      else if (njobs .lt. object%Sjtotal) then
        call cfeutil_remove_array_element (object%SJcount,ncount,njobs+1,  &
                                           object%Sjtotal)
        call cfeutil_remove_array_element (object%SJnext ,nnext ,njobs+1,  &
                                           object%Sjtotal)
        object%Sjtotal = njobs
      endif

      call pc_remove_gui_action ('SJJOBSLIST','MODIFYFIELD')

      return
      end subroutine cfesj_jobslist_trap


!!--------------------------- countlist_trap -------------------------------!!
!!--------------------------- countlist_trap -------------------------------!!
!!--------------------------- countlist_trap -------------------------------!!


      subroutine cfesj_countlist_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument


      integer                         ::  njobs                        !local
      integer                         ::  ncount                       !local
      integer                         ::  nnext                        !local

      njobs  = object%Sjtotal
      ncount = object%Sjtotal
      nnext  = object%Sjtotal

      call cfeutil_read_file (object%SJcountList,object%SJcount,ncount)

      if (ncount .gt. object%Sjtotal) then
        call cfeutil_append_array_element (object%SJjobs ,njobs ,' ',  &
                                           ncount-object%Sjtotal)
        call cfeutil_append_array_element (object%SJnext ,nnext ,' ',  &
                                           ncount-object%Sjtotal)
        object%Sjtotal = ncount

      else if (ncount .lt. object%Sjtotal) then
        call cfeutil_remove_array_element (object%SJjobs,njobs,ncount+1,  &
                                           object%Sjtotal)
        call cfeutil_remove_array_element (object%SJnext,nnext,ncount+1,  &
                                           object%Sjtotal)
        object%Sjtotal = ncount
      endif

      call pc_remove_gui_action ('SJCOUNTLIST','MODIFYFIELD')

      return
      end subroutine cfesj_countlist_trap


!!--------------------------- nextlist_trap --------------------------------!!
!!--------------------------- nextlist_trap --------------------------------!!
!!--------------------------- nextlist_trap --------------------------------!!


      subroutine cfesj_nextlist_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      integer                         ::  i                            !local
      integer                         ::  njobs                        !local
      integer                         ::  ncount                       !local
      integer                         ::  nnext                        !local

      njobs  = object%Sjtotal
      ncount = object%Sjtotal
      nnext  = object%Sjtotal

      call cfeutil_read_file (object%SJnextList,object%SJnext,nnext)

      if (nnext .gt. object%Sjtotal) then
        call cfeutil_append_array_element (object%SJjobs ,njobs ,' ',  &
                                           nnext-object%Sjtotal)
        do i=object%Sjtotal+1,njobs
          call cfeutil_append_array_element (object%SJcount,ncount,i)
        enddo
        object%Sjtotal = nnext

      else if (nnext .lt. object%Sjtotal) then
        call cfeutil_remove_array_element (object%SJjobs ,njobs ,nnext+1,  &
                                           object%Sjtotal)
        call cfeutil_remove_array_element (object%SJcount,ncount,nnext+1,  &
                                           object%Sjtotal)
        object%Sjtotal = nnext
      endif

      call pc_remove_gui_action ('SJNEXTLIST','MODIFYFIELD')

      return
      end subroutine cfesj_nextlist_trap


!!--------------------------- jobselect_trap -------------------------------!!
!!--------------------------- jobselect_trap -------------------------------!!
!!--------------------------- jobselect_trap -------------------------------!!


      subroutine cfesj_jobselect_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      character(len=PC_LENGTH)        :: local_key                     !local
      type(cfewindow_struct),pointer  :: window                        !local
      integer                         :: window_id                     !local
      integer                         :: istat                         !local

      nullify (window) ! jpa

      if (object%SJseries_locked) then
        istat = cio_unlock_file(trim(object%SJlock_file))
        object%SJseries_locked = .false.
        object%SJlock_file     = ' '
      endif

      local_key = keyword
!     call string_to_upper (local_key)

      call cfewindow_create           (window)
      call cfewindow_set_keyword      (window ,'SJJOBS')
      call cfewindow_set_index        (window ,object%SJtotal+1)
      call cfewindow_set_window_type  (window ,'FILEBOX')
      call cfewindow_get_window_id    (window ,window_id)

      if (.not. associated(object%SJjobs_dialog)) then
        call mfilebox_create (object%SJjobs_dialog,  &
                             trim(object%working_dir) // '*.wrk')
      else
        call mfilebox_update (object%SJjobs_dialog)
      endif
      call cfegui_create (window_id,'FILE SELECTION','mfilebox.xml',.false.)
      call cfewindow_set_current (window)

      return
      end subroutine cfesj_jobselect_trap


!!----------------------------- submit_trap --------------------------------!!
!!----------------------------- submit_trap --------------------------------!!
!!----------------------------- submit_trap --------------------------------!!


      subroutine cfesj_submit_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           :: i                           !local
      integer                           :: j                           !local
      integer                           :: n                           !local
      integer                           :: lun                         !local
      integer                           :: istat                       !local
      integer                           :: ncards                      !local
      integer                           :: submit_count                !local
      integer                           :: pid                         !local
      integer                           :: filetype                    !local
      integer                           :: finquire_stat               !local
      integer,allocatable               :: count_tmp(:)                !local
      character(len=2)                  :: mode                        !local
      character(len=PC_LENGTH),pointer  :: cards(:)                    !local
      character(len=PC_LENGTH)          :: seriesfile                  !local
      character(len=PC_LENGTH)          :: home_dir                    !local
      character(len=PC_LENGTH)          :: series_dir                  !local
      character(len=10)                 :: cpid                        !local
      character(len=10)                 :: series_total                !local
      character(len=10)                 :: series_count                !local
      character(len=1024)               :: cmd                         !local
      character(len=20)                 :: qsub_date                   !local
      character(len=PC_LENGTH)          :: file_with_path              !local
      character(len=PC_LENGTH)          :: file_no_path                !local
      character(len=PC_LENGTH)          :: file_path                   !local
      character(len=PC_LENGTH)          :: file_full                   !local
      character(len=10)                 :: requestid                   !local
      logical                           :: exists                      !local
      logical                           :: build_job                   !local
      type(cardset_struct),pointer      :: cardset                     !local
      integer                           :: temp


      if (object%SJseries_locked) then
        istat = cio_unlock_file(trim(object%SJlock_file))
        object%SJseries_locked = .false.
        object%SJlock_file     = ' '
      endif

      if (object%SJtotal .le. 0) then
        call pc_error ('No jobs specified for submit')
        return
      endif

      call cfesj_verify_date (object,istat)
      if (istat .ne. 0) return
      call cfesj_verify_time (object,istat)
      if (istat .ne. 0) return
      call cfesj_reformat_date_time (object,qsub_date)

      call cfesj_remove_blank_rows (object%SJjobs,object%SJcount,  &
                                    object%SJnext,object%SJtotal)

      submit_count = 0
      do i=1,object%SJtotal
        if (len_trim(object%SJseries).eq.0 .and. object%SJcount(i).ne.INIL) then
          call pc_error ('Job Series Name must be specified')
          return
        else if (len_trim(object%SJseries).ne.0.and.object%SJcount(i).eq.INIL) &
                                                                            then
          call pc_error ('Series count must be specified for each job')
          return
        endif
        if (object%SJcount(i) .eq. 1) submit_count = submit_count + 1
      enddo
      if (submit_count .eq. 0 .and. len_trim(object%SJseries) .ne. 0) then
        call pc_warning ('There are no count equal 1 jobs.')
      endif

      if (submit_count .gt. 1 .and. len_trim(object%SJseries) .eq. 0) then
        call pc_warning ('Job Series was not specified')
      endif

      if (len_trim(object%SJseries) .gt. 0) then
        call getsys_env ('HOME',home_dir)
        series_dir = trim(home_dir) // '/.cfe_series'
        inquire (file=trim(series_dir),exist=exists)
        if (.not. exists) call putsys_texec ('mkdir '// trim(series_dir),istat)

        seriesfile = trim(series_dir)//'/'//trim(object%SJseries)//'.series'
        mode = "w"
        lun = cio_fopen(trim(seriesfile),mode)
        if (lun .lt. 100) then
          write(STDOUT,*) 'Error opening file '//trim(seriesfile)//'  lun=',lun
          return
        endif

        nullify(cards)
        nullify(cardset)
        call cardset_create      (cardset)
        call cardset_put_scalar  (cardset,'WORKING_DIR',object%working_dir)
        call cardset_put_scalar  (cardset,'INDEPENDENT',object%SJindependent)
        call cardset_put_array   (cardset,'JOBS' ,object%SJjobs ,object%SJtotal)
        allocate (count_tmp(object%SJtotal))
        count_tmp = object%SJcount
        do i=1,object%SJtotal
          count_tmp(i) = count_tmp(i) - 1
        enddo
        call cardset_put_array   (cardset,'COUNT',count_tmp     ,object%SJtotal)
        deallocate(count_tmp)
        call cardset_put_array   (cardset,'NEXT' ,object%SJnext ,object%SJtotal)
        call cardset_alloc_cards (cardset,cards,ncards)
        call cardset_delete      (cardset)

        do i=1,ncards
          temp  = len_trim(cards(i))
          istat = cio_fputline(cards(i),temp,lun)
        enddo

        istat = cio_fclose(lun)
      endif

      pid = getsys_pid()
      call string_ii2cc (pid,cpid)
      call string_ii2cc (object%SJtotal,series_total)

      submit_count = 0
      do i=1,object%SJtotal
        file_no_path = path_get_file(object%SJjobs(i))
!!!     n = index        (file_no_path,'.wrk',.true.) - 1
        n = string_index (file_no_path,'.wrk',.true.) - 1
        if (n .gt. 0) then
          filetype = CFE_WORKFILE
        else
!!!       n = index        (file_no_path,'.job',.true.) - 1
          n = string_index (file_no_path,'.job',.true.) - 1
          if (n .gt. 0) then
            filetype = CFE_JOBFILE
          else
            n = len_trim(file_no_path)
            filetype = CFE_UNKNOWN
          endif
        endif
        file_path = path_get_dir (object%SJjobs(i))
        if (file_path .eq. PATH_EMPTY) then
          file_with_path = trim(object%working_dir)//file_no_path(1:n)
        else
          file_with_path = trim(file_path)//file_no_path(1:n)
        endif
        if (filetype .eq. CFE_WORKFILE) then
          file_full = trim(file_with_path)//'.wrk'
        else if (filetype .eq. CFE_JOBFILE) then
          file_full = trim(file_with_path)//'.job'
        else
          file_full = trim(file_with_path)//'.wrk'
        endif
        finquire_stat = finquire_input (trim(file_full))
        if (finquire_stat .ne. FINQUIRE_FOUND) then
          call pc_error ('Can not find file '//trim(file_full))
        else
          build_job = .false.
          if (filetype .eq. CFE_WORKFILE) then
            build_job = .true.
          else if (filetype .eq. CFE_UNKNOWN) then
            finquire_stat = finquire_input (trim(file_with_path)//'.job')
            if (finquire_stat .eq. FINQUIRE_FOUND) then
              j = cfeutil_file_datecmp (trim(file_with_path)//'.job',  &
                                        trim(file_with_path)//'.wrk')
              if (j .lt. 0) then     ! Workfile is newer, Rebuild job
                build_job = .true.
              endif
            else
              build_job = .true.
            endif
          endif
          if (build_job) then
            cmd = trim(object%prog_cfebld)//' '//trim(file_with_path)//  &
                  ' >>&! /tmp/submitjob.'//trim(cpid)
            write(STDOUT,*) trim(cmd)
            call putsys_texec (trim(cmd))
            call cfesl_add (trim(file_no_path),'BUILT')
            object%last_jobfile = trim(file_no_path)
          endif
          if (len_trim(object%SJseries) .eq. 0) then
            if (len_trim(qsub_date) .gt. 0) then
              cmd = trim(object%prog_cfesub)//' -a "'//trim(qsub_date)//'"'
            else
              cmd = trim(object%prog_cfesub)
            endif
            cmd = trim(cmd)//' '//trim(file_with_path)//  &
                  '.job >>&! /tmp/submitjob.'//trim(cpid)
            write(STDOUT,*) trim(cmd)
            if (submit_count .gt. 0) call cfeutil_sleep (15)
            call putsys_texec (trim(cmd), istat)
            if(istat /= 0) then
              call pc_error('CFESJ: texec 1 istat = ', istat)
              return
            endif
            if (len_trim(qsub_date) .eq. 0) submit_count = submit_count + 1
            call cfesl_add (trim(file_no_path),'SUBMITTED')
            object%last_reportfile = trim(file_with_path)//'.rpt'
          else
            if (object%SJcount(i) .eq. 1) then
              if (len_trim(qsub_date) .gt. 0) then
                cmd = trim(object%prog_cfesub)//' -a "'//trim(qsub_date)//'"'
              else
                cmd = trim(object%prog_cfesub)
              endif
              if (object%SJindependent) cmd = trim(cmd)//' -i'
              cmd = trim(cmd)//' '//'-s '//trim(seriesfile)//' '//  &
                    trim(file_with_path)//                           &
                    '.job >>&! /tmp/submitjob.'//trim(cpid)
              write(STDOUT,*) trim(cmd)
              if (submit_count .gt. 0) call cfeutil_sleep (15)
              call putsys_texec (trim(cmd), istat)
              if(istat /= 0) then
                call pc_error('CFESJ: texec 2 istat = ', istat)
                return
              endif
              if (len_trim(qsub_date) .eq. 0) submit_count = submit_count + 1
              call string_ii2cc (object%SJcount(i),series_count)
              call cfesl_add (trim(file_no_path),'SUBMITTED',  &
                               object%SJseries,trim(series_count)//' / '//  &
                               trim(series_total),object%SJnext(i))
              object%last_reportfile = trim(file_with_path)//'.rpt'
            else
              call string_ii2cc (object%SJcount(i),series_count)
              call cfesl_add (trim(file_no_path),'SERIES',  &
                               object%SJseries,trim(series_count)//' / '//  &
                               trim(series_total),object%SJnext(i))
            endif

          endif

          call pc_print('CFE_SJ: Submit Job Command = ', cmd);

        endif
      enddo

      object%SJseries  = ' '
      object%SJdate    = ' '
      object%SJtime    = ' '

      call cfeutil_clear_array (object%SJjobs ,object%SJtotal)
      call cfeutil_clear_array (object%SJcount,object%SJtotal)
      call cfeutil_clear_array (object%SJnext ,object%SJtotal)

      if (associated(cards)) deallocate(cards)
      ncards = 0
      call cfeutil_read_file ('/tmp/submitjob.'//trim(cpid),cards,ncards,  &
                              'Submit Job Output',remove=.true.)
      if (ncards .gt. 0) then
        do i=1,ncards
          write(STDOUT,*) trim(cards(i))
        enddo
        call cfeutil_find_last_requestid (cards,ncards,requestid)
        call pc_put_gui ('INFO','INFO',cards,ncards)
        deallocate(cards)
      endif

      if (len_trim(requestid) .ne. 0) then
        object%last_reportfile = trim(object%last_reportfile)//trim(requestid)
      else
        object%last_reportfile = ' '
      endif

      return
      end subroutine cfesj_submit_trap


!!----------------------------- clear_trap ---------------------------------!!
!!----------------------------- clear_trap ---------------------------------!!
!!----------------------------- clear_trap ---------------------------------!!


      subroutine cfesj_clear_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument
      integer                           :: istat                       !local

      if (object%SJseries_locked) then
        istat = cio_unlock_file(trim(object%SJlock_file))
        object%SJseries_locked = .false.
        object%SJlock_file     = ' '
      endif

      object%SJseries  = ' '
      object%SJdate    = ' '
      object%SJtime    = ' '

      call cfeutil_clear_array (object%SJjobs ,object%SJtotal)
      call cfeutil_clear_array (object%SJcount,object%SJtotal)
      call cfeutil_clear_array (object%SJnext ,object%SJtotal)

      return
      end subroutine cfesj_clear_trap


!!--------------------------- increment_trap -------------------------------!!
!!--------------------------- increment_trap -------------------------------!!
!!--------------------------- increment_trap -------------------------------!!


      subroutine cfesj_increment_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      character(len=PC_LENGTH)        :: local_key                     !local
      type(cfewindow_struct),pointer  :: window                        !local
      integer                         :: window_id                     !local
      integer                         :: istat                         !local

      nullify (window) ! jpa

      if (object%SJseries_locked) then
        istat = cio_unlock_file(trim(object%SJlock_file))
        object%SJseries_locked = .false.
        object%SJlock_file     = ' '
      endif

      local_key = keyword
!     call string_to_upper (local_key)

      call cfewindow_create           (window)
      call cfewindow_set_keyword      (window ,'SJJOBS')
      call cfewindow_set_index        (window ,object%SJtotal+1)
      call cfewindow_set_window_type  (window ,'INCJOBNAME')
      call cfewindow_get_window_id    (window ,window_id)

      if (associated(object%SJjobs_temp))  deallocate(object%SJjobs_temp)
      if (associated(object%SJcount_temp)) deallocate(object%SJcount_temp)
      if (associated(object%SJnext_temp))  deallocate(object%SJnext_temp)
      object%SJtotal_temp = object%SJtotal
      if (object%SJtotal_temp .gt. 0) then
        allocate(object%SJjobs_temp(object%SJtotal_temp))
        allocate(object%SJcount_temp(object%SJtotal_temp))
        allocate(object%SJnext_temp(object%SJtotal_temp))
        object%SJjobs_temp  = object%SJjobs
        object%SJcount_temp = object%SJcount
        object%SJnext_temp  = object%SJnext
      else
        allocate(object%SJjobs_temp(1))
        allocate(object%SJcount_temp(1))
        allocate(object%SJnext_temp(1))
        object%SJjobs_temp(1)  = ' '
        object%SJcount_temp(1) = 0
        object%SJnext_temp(1)  = ' '
      endif
      if (.not. associated(object%SJincjobname_dialog)) then
        call incjobname_create (object%SJincjobname_dialog)
      else
        call incjobname_update (object%SJincjobname_dialog)
      endif
      call cfegui_create (window_id,'INCREMENT JOBNAME','incjobname.xml', &
                           .false.)
      call cfewindow_set_current (window)

      return
      end subroutine cfesj_increment_trap


!!------------------------- update_series_trap -----------------------------!!
!!------------------------- update_series_trap -----------------------------!!
!!------------------------- update_series_trap -----------------------------!!


      subroutine cfesj_update_series_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           :: i                           !local
      integer                           :: j                           !local
      integer                           :: n                           !local
      integer                           :: pid                         !local
      integer                           :: istat                       !local
      integer                           :: lun                         !local
      integer                           :: ncards                      !local
      integer                           :: filetype                    !local
      integer                           :: finquire_stat               !local
      character(len=2)                  :: mode                        !local
      character(len=PC_LENGTH)          :: home_dir                    !local
      character(len=PC_LENGTH)          :: series_dir                  !local
      character(len=PC_LENGTH)          :: seriesfile                  !local
      character(len=PC_LENGTH)          :: file_with_path              !local
      character(len=PC_LENGTH)          :: file_no_path                !local
      character(len=PC_LENGTH)          :: file_path                   !local
      character(len=PC_LENGTH)          :: file_full                   !local
      character(len=10)                 :: cpid                        !local
      character(len=10)                 :: series_total                !local
      character(len=10)                 :: series_count                !local
      character(len=1024)               :: cmd                         !local
      character(len=PC_LENGTH),pointer  :: cards(:)                    !local
      logical                           :: exists                      !local
      logical                           :: build_job                   !local
      type(cardset_struct),pointer      :: cardset                     !local
      integer                           :: temp

      if (len_trim(object%SJseries) .eq. 0) return

      call getsys_env ('HOME',home_dir)
      series_dir = trim(home_dir) // '/.cfe_series'
      inquire (file=trim(series_dir),exist=exists)
      if (.not. exists) call putsys_texec ('mkdir '// trim(series_dir),istat)

      seriesfile = trim(series_dir)//'/'//trim(object%SJseries)//'.series'
      mode = "w"
      lun = cio_fopen(trim(seriesfile),mode)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(seriesfile)//'  lun=',lun
        return
      endif

      nullify(cards)
      nullify(cardset)
      call cardset_create      (cardset)
      call cardset_put_scalar  (cardset,'WORKING_DIR',object%working_dir)
      call cardset_put_scalar  (cardset,'INDEPENDENT',object%SJindependent)
      call cardset_put_array   (cardset,'JOBS' ,object%SJjobs ,object%SJtotal)
      call cardset_put_array   (cardset,'COUNT',object%SJcount,object%SJtotal)
      call cardset_put_array   (cardset,'NEXT' ,object%SJnext ,object%SJtotal)
      call cardset_alloc_cards (cardset,cards,ncards)
      call cardset_delete      (cardset)

      if (ncards .gt. 0) then
        do i=1,ncards
          temp  = len_trim(cards(i))
          istat = cio_fputline(cards(i),temp,lun)
        enddo
        deallocate(cards)
      endif

      istat = cio_fclose(lun)

      pid = getsys_pid()
      call string_ii2cc (pid,cpid)
      call string_ii2cc (object%SJtotal,series_total)

      do i=1,object%SJtotal
        file_no_path = path_get_file(object%SJjobs(i))
!!!     n = index        (file_no_path,'.wrk',.true.) - 1
        n = string_index (file_no_path,'.wrk',.true.) - 1
        if (n .gt. 0) then
          filetype = CFE_WORKFILE
        else
!!!       n = index        (file_no_path,'.job',.true.) - 1
          n = string_index (file_no_path,'.job',.true.) - 1
          if (n .gt. 0) then
            filetype = CFE_JOBFILE
          else
            n = len_trim(file_no_path)
            filetype = CFE_UNKNOWN
          endif
        endif
        file_path = path_get_dir (object%SJjobs(i))
        if (file_path .eq. PATH_EMPTY) then
          file_with_path = trim(object%working_dir)//file_no_path(1:n)
        else
          file_with_path = trim(file_path)//file_no_path(1:n)
        endif
        if (filetype .eq. CFE_WORKFILE) then
          file_full = trim(file_with_path)//'.wrk'
        else if (filetype .eq. CFE_JOBFILE) then
          file_full = trim(file_with_path)//'.job'
        else
          file_full = trim(file_with_path)//'.wrk'
        endif
        finquire_stat = finquire_input (trim(file_full))
        if (finquire_stat .ne. FINQUIRE_FOUND) then
          call pc_error ('Can not find file '//trim(file_full))
        else
          build_job = .false.
          if (filetype .eq. CFE_WORKFILE) then
            build_job = .true.
          else if (filetype .eq. CFE_UNKNOWN) then
            finquire_stat = finquire_input (trim(file_with_path)//'.job')
            if (finquire_stat .eq. FINQUIRE_FOUND) then
              j = cfeutil_file_datecmp (trim(file_with_path)//'.job',  &
                                        trim(file_with_path)//'.wrk')
              if (j .lt. 0) then     ! Workfile is newer, Rebuild job
                build_job = .true.
              endif
            else
              build_job = .true.
            endif
          endif
          if (build_job) then
            cmd = trim(object%prog_cfebld)//' '//trim(file_with_path)//  &
                  ' >>&! /tmp/submitjob.'//trim(cpid)
            write(STDOUT,*) trim(cmd)
            call putsys_texec (trim(cmd))
            call cfesl_add (trim(file_no_path),'BUILT')
            object%last_jobfile = trim(file_no_path)
          endif
          call string_ii2cc (object%SJcount(i),series_count)
          call cfesl_add (trim(file_no_path),'UPDATE',  &
                             object%SJseries,trim(series_count)//' / '//  &
                             trim(series_total),object%SJnext(i))
        endif
      enddo
      istat = cio_unlock_file(trim(object%SJlock_file))
      object%SJseries_locked = .false.
      object%SJlock_file     = ' '

      object%SJseries  = ' '
      object%SJdate    = ' '
      object%SJtime    = ' '

      call cfeutil_clear_array (object%SJjobs ,object%SJtotal)
      call cfeutil_clear_array (object%SJcount,object%SJtotal)
      call cfeutil_clear_array (object%SJnext ,object%SJtotal)

      if (associated(cards)) deallocate(cards)
      ncards = 0
      call cfeutil_read_file ('/tmp/submitjob.'//trim(cpid),cards,ncards,  &
                              'Update Series Output',remove=.true.)
      if (ncards .gt. 0) then
        do i=1,ncards
          write(STDOUT,*) trim(cards(i))
        enddo
        call pc_put_gui ('INFO','INFO',cards,ncards)
        deallocate(cards)
      endif

      return
      end subroutine cfesj_update_series_trap


!!------------------------------ date_trap ---------------------------------!!
!!------------------------------ date_trap ---------------------------------!!
!!------------------------------ date_trap ---------------------------------!!


      subroutine cfesj_date_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      call cfesj_verify_date (object)

      return
      end subroutine cfesj_date_trap


!!----------------------------- verify_date --------------------------------!!
!!----------------------------- verify_date --------------------------------!!
!!----------------------------- verify_date --------------------------------!!


      subroutine cfesj_verify_date (obj,ireturn)
      implicit none
      type(cfestruct),pointer          :: obj                         !argument
      integer,optional                 :: ireturn                     !argument

      integer                           :: leap                        !local
      integer                           :: iday                        !local
      integer                           :: imonth                      !local
      integer                           :: iyear                       !local
      integer                           :: istat                       !local
      character(len=80)                 :: msg                         !local

      if (present(ireturn)) ireturn = -1

      call string_strip_blanks (obj%SJdate)
      if (len_trim(obj%SJdate) .eq. 0) then
        if (present(ireturn)) ireturn = 0
        return
      endif

      if (len_trim(obj%SJdate).ne.10 .or. obj%SJdate(5:5).ne.'-' .or.  &
          obj%SJdate(8:8).ne.'-') then
        call pc_error ('Must use format YYYY-MM-DD')
        return
      endif

      call string_cc2ii (obj%SJdate(6:7),imonth,istat,msg)
      if (istat .eq. -1) then
        call pc_error (trim(msg))
        return
      endif
      if (imonth .lt. 1 .or. imonth .gt. 12) then
        call pc_error ('MM must be in the range of 1 - 12')
        return
      endif

      call string_cc2ii (obj%SJdate(9:10),iday,istat,msg)
      if (istat .eq. -1) then
        call pc_error (trim(msg))
        return
      endif

      call string_cc2ii (obj%SJdate(1:4),iyear,istat,msg)
      if (istat .eq. -1) then
        call pc_error (trim(msg))
        return
      endif

      if (imonth.eq.1 .or. imonth.eq.3 .or. imonth.eq.5 .or. imonth.eq.7 .or.  &
          imonth.eq.8 .or. imonth.eq.10 .or. imonth.eq.12) then
        if (iday .lt. 1 .or. iday .gt. 31) then
          call pc_error ('DD must be in the range of 1 - 31')
          return
        endif
      else if (imonth.eq.4 .or. imonth.eq.6 .or. imonth.eq.9 .or.  &
               imonth.eq.11) then
        if (iday .lt. 1 .or. iday .gt. 30) then
          call pc_error ('DD must be in the range of 1 - 30')
          return
        endif
      else if (imonth.eq.2) then
        leap = mod(iyear,4)
        if (leap .ne. 0) then
          if (iday .lt. 1 .or. iday .gt. 28) then
            call pc_error ('DD must be in the range of 1 - 28')
            return
          endif
        else
          if (iday .lt. 1 .or. iday .gt. 29) then
            call pc_error ('DD must be in the range of 1 - 29')
            return
          endif
        endif
      endif
        
      if (present(ireturn)) ireturn = 0

      return
      end subroutine cfesj_verify_date


!!------------------------------ time_trap ---------------------------------!!
!!------------------------------ time_trap ---------------------------------!!
!!------------------------------ time_trap ---------------------------------!!


      subroutine cfesj_time_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      call cfesj_verify_time (object)

      return
      end subroutine cfesj_time_trap


!!------------------------------ jobs_trap ---------------------------------!!
!!------------------------------ jobs_trap ---------------------------------!!
!!------------------------------ jobs_trap ---------------------------------!!


      subroutine cfesj_jobs_trap (keyword,indx,action)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument
      integer         ,intent(in)       ::  indx                       !argument
      integer         ,intent(in)       ::  action                     !argument


      if (len_trim(object%SJseries) .eq. 0) return

      select case (action)

        case (PC_INSERT)
        case (PC_REMOVE)
        case (PC_MODIFY)
          object%SJcount(indx) = indx

      end select


      return
      end subroutine cfesj_jobs_trap


!!----------------------------- verify_time --------------------------------!!
!!----------------------------- verify_time --------------------------------!!
!!----------------------------- verify_time --------------------------------!!


      subroutine cfesj_verify_time (obj,ireturn)
      implicit none
      type(cfestruct),pointer          :: obj                         !argument
      integer,optional                 :: ireturn                     !argument

      integer                           :: i                           !local
      integer                           :: istat                       !local
      character(len=80)                 :: msg                         !local

      if (present(ireturn)) ireturn = -1

      call string_strip_blanks (obj%SJtime)
      if (len_trim(obj%SJtime) .eq. 0) then
        if (present(ireturn)) ireturn = 0
        return
      endif

      if (len_trim(obj%SJtime) .ne. 5 .or. obj%SJtime(3:3) .ne. ':') then
        call pc_error ('Must use format HH:MM')
        return
      endif

      call string_cc2ii (obj%SJtime(1:2),i,istat,msg)
      if (istat .eq. -1) then
        call pc_error (trim(msg))
        return
      endif
      if (i .lt. 0 .or. i .gt. 23) then
        call pc_error ('HH must be in the range of 0 - 23')
        return
      endif

      call string_cc2ii (obj%SJtime(4:5),i,istat,msg)
      if (istat .eq. -1) then
        call pc_error (trim(msg))
        return
      endif
      if (i .lt. 0 .or. i .gt. 59) then
        call pc_error ('MM must be in the range of 0 - 59')
        return
      endif

      if (present(ireturn)) ireturn = 0

      return
      end subroutine cfesj_verify_time


!!-------------------------- reformat_date_time ----------------------------!!
!!-------------------------- reformat_date_time ----------------------------!!
!!-------------------------- reformat_date_time ----------------------------!!


      subroutine cfesj_reformat_date_time (obj,date_time)
      implicit none
      type(cfestruct),pointer          :: obj                         !argument
      character(len=*),intent(out)     :: date_time                   !argument

      integer                          :: imonth                      !local
      character(len=3)                 :: cmonth                      !local

      date_time = ''
      call string_strip_blanks (obj%SJdate)
      call string_strip_blanks (obj%SJtime)
      if (len_trim(obj%SJdate).eq.0 .and. len_trim(obj%SJtime).eq.0) return

      if (len_trim(obj%SJdate) .ne. 0) then
        call string_cc2ii (obj%SJdate(6:7) ,imonth)
        select case (imonth)
          case ( 1) ; cmonth = 'Jan'
          case ( 2) ; cmonth = 'Feb'
          case ( 3) ; cmonth = 'Mar'
          case ( 4) ; cmonth = 'Apr'
          case ( 5) ; cmonth = 'May'
          case ( 6) ; cmonth = 'Jun'
          case ( 7) ; cmonth = 'Jul'
          case ( 8) ; cmonth = 'Aug'
          case ( 9) ; cmonth = 'Sep'
          case (10) ; cmonth = 'Oct'
          case (11) ; cmonth = 'Nov'
          case (12) ; cmonth = 'Dec'
        end select
      endif

      if (len_trim(obj%SJtime) .eq. 0) then
        date_time = obj%SJdate(9:10)//'-'//cmonth// '-'//obj%SJdate(1:4)
      else if (len_trim(obj%SJdate) .eq. 0) then
        date_time = trim(obj%SJtime)
      else
        date_time = obj%SJdate(9:10)//'-'//cmonth// '-'//obj%SJdate(1:4)// &
                    ' ' // trim(obj%SJtime)
      endif

      return
      end subroutine cfesj_reformat_date_time


!!---------------------------- execute_filebox -----------------------------!!
!!---------------------------- execute_filebox -----------------------------!!
!!---------------------------- execute_filebox -----------------------------!!


      subroutine cfesj_execute_filebox (obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      character(len=PC_LENGTH),pointer    :: jobs_to_add(:)           !local
      integer                             :: i, njobs                 !local

      nullify(jobs_to_add)
      call mfilebox_selections(obj%SJjobs_dialog,jobs_to_add,njobs)
      do i=1,njobs
        call cfesj_append_job (obj,jobs_to_add(i))
      enddo

      return
      end subroutine cfesj_execute_filebox


!!------------------------------ append_job --------------------------------!!
!!------------------------------ append_job --------------------------------!!
!!------------------------------ append_job --------------------------------!!


      subroutine cfesj_append_job (obj,job_to_add)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument
      character(len=*),intent(in)         :: job_to_add               !argument

      integer                              :: njobs                    !local
      integer                              :: ncount                   !local
      integer                              :: nnext                    !local


      njobs  = obj%SJtotal
      ncount = obj%SJtotal
      nnext  = obj%SJtotal

      call cfeutil_append_array_element (obj%SJjobs ,njobs ,trim(job_to_add))
      if (ncount .gt. 0) then
        if (obj%SJcount(ncount) .ne. INIL) then
          call cfeutil_append_array_element (obj%SJcount,ncount,  &
                                             obj%SJcount(ncount)+1)
        else
          call cfeutil_append_array_element (obj%SJcount,ncount,INIL)
        endif
      else
        call cfeutil_append_array_element (obj%SJcount,ncount,INIL)
      endif
      call cfeutil_append_array_element (obj%SJnext ,nnext ,' ')

      if (njobs.ne.ncount .or. njobs.ne.nnext) then
        call pc_error ('Jobs, Count, Next arrays have different lengths')
        obj%SJtotal = min(njobs,ncount,nnext)
      else
        obj%SJtotal = njobs
      endif

!     call pc_put_gui ('SJJOBS' ,'ReplaceElements',obj%SJjobs ,obj%SJtotal)
!     call pc_put_gui ('SJCOUNT','ReplaceElements',obj%SJcount,obj%SJtotal)
!     call pc_put_gui ('SJNEXT' ,'ReplaceElements',obj%SJnext ,obj%SJtotal)

      return
      end subroutine cfesj_append_job


!!------------------------------ remove_job --------------------------------!!
!!------------------------------ remove_job --------------------------------!!
!!------------------------------ remove_job --------------------------------!!


      subroutine cfesj_remove_job (obj,indx)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument
      integer,intent(in)                  :: indx                     !argument

      integer                              :: njobs                    !local
      integer                              :: ncount                   !local
      integer                              :: nnext                    !local


      njobs  = obj%SJtotal
      ncount = obj%SJtotal
      nnext  = obj%SJtotal

      call cfeutil_remove_array_element (obj%SJjobs ,njobs ,indx,indx)
      call cfeutil_remove_array_element (obj%SJcount,ncount,indx,indx)
      call cfeutil_remove_array_element (obj%SJnext ,nnext ,indx,indx)

      if (njobs.ne.ncount .or. njobs.ne.nnext) then
        call pc_error ('Jobs, Count, Next arrays have different lengths')
        obj%SJtotal = min(njobs,ncount,nnext)
      else
        obj%SJtotal = njobs
      endif

!     call pc_put_gui ('SJJOBS' ,'ReplaceElements',obj%SJjobs ,obj%SJtotal)
!     call pc_put_gui ('SJCOUNT','ReplaceElements',obj%SJcount,obj%SJtotal)
!     call pc_put_gui ('SJNEXT' ,'ReplaceElements',obj%SJnext ,obj%SJtotal)

      return
      end subroutine cfesj_remove_job


!!-------------------------- execute_incjobname ----------------------------!!
!!-------------------------- execute_incjobname ----------------------------!!
!!-------------------------- execute_incjobname ----------------------------!!


      subroutine cfesj_execute_incjobname (obj)
      implicit none
      type(cfestruct),pointer          :: obj                         !argument

      integer                           :: i                           !local
      integer                           :: i1                          !local
      integer                           :: i2                          !local
      integer                           :: njobs                       !local
      integer                           :: ncount                      !local
      integer                           :: nnext                       !local



      call incjobname_selection (obj%SJincjobname_dialog,  &
                                     obj%SJjobs,njobs)
      ncount = obj%SJtotal
      nnext  = obj%SJtotal

      i = njobs - obj%SJtotal
      if (i .gt. 0) then
        call cfeutil_append_array_element (obj%SJcount,ncount, 0 ,i)
        call cfeutil_append_array_element (obj%SJnext ,nnext ,' ',i)
      else if (i .lt. 0) then
        i1    = njobs + 1
        i2    = obj%SJtotal
        call cfeutil_remove_array_element (obj%SJcount,ncount,i1,i2)
        call cfeutil_remove_array_element (obj%SJnext ,nnext ,i1,i2)
      endif

      do i=1,ncount
        obj%SJcount(i) = i
      enddo

      if (njobs.ne.ncount .or. njobs.ne.nnext) then
        call pc_error ('Jobs, Count, Next arrays have different lengths')
        obj%SJtotal = min(njobs,ncount,nnext)
      else
        obj%SJtotal = njobs
      endif

!     call pc_put_gui ('SJJOBS' ,'ReplaceElements',obj%SJjobs ,obj%SJtotal)
!     call pc_put_gui ('SJCOUNT','ReplaceElements',obj%SJcount,obj%SJtotal)
!     call pc_put_gui ('SJNEXT' ,'ReplaceElements',obj%SJnext ,obj%SJtotal)

      return
      end subroutine cfesj_execute_incjobname


!!--------------------------- reset_incjobname -----------------------------!!
!!--------------------------- reset_incjobname -----------------------------!!
!!--------------------------- reset_incjobname -----------------------------!!


      subroutine cfesj_reset_incjobname (obj)
      implicit none
      type(cfestruct),pointer          :: obj                         !argument


      call pc_put_gui ('SJJOBS' ,'ReplaceElements',obj%SJjobs_temp,   &
                        obj%SJtotal_temp)
      call pc_put_gui ('SJCOUNT','ReplaceElements',obj%SJcount_temp,  &
                        obj%SJtotal_temp)
      call pc_put_gui ('SJNEXT' ,'ReplaceElements',obj%SJnext_temp ,  &
                        obj%SJtotal_temp)

      return
      end subroutine cfesj_reset_incjobname


!!-------------------------- remove_blank_rows -----------------------------!!
!!-------------------------- remove_blank_rows -----------------------------!!
!!-------------------------- remove_blank_rows -----------------------------!!

      subroutine cfesj_remove_blank_rows (jobs,count,next,total)

      character(len=*),pointer             :: jobs(:)                  !argument
      integer         ,pointer             :: count(:)                 !argument
      character(len=*),pointer             :: next(:)                  !argument
      integer         ,intent(inout)       :: total                    !argument

      integer                              :: i                        !local
      character(len=PC_LENGTH),allocatable :: temp_jobs(:)             !local
      integer                 ,allocatable :: temp_count(:)            !local
      character(len=PC_LENGTH),allocatable :: temp_next(:)             !local

      if (total .eq. 0) return
      i = 1
      do
        if (i .gt. total) exit
        if (len_trim(jobs(i)) .ne. 0) then
          i = i + 1
          cycle
        endif
        allocate(temp_jobs(total))
        allocate(temp_count(total))
        allocate(temp_next(total))
        temp_jobs(1:total)  = jobs(1:total)
        temp_count(1:total) = count(1:total)
        temp_next(1:total)  = next(1:total)
        deallocate(jobs)
        deallocate(count)
        deallocate(next)
        allocate(jobs(total-1))
        allocate(count(total-1))
        allocate(next(total-1))
        if (i .gt. 1) then
          jobs(1:i-1)      = temp_jobs(1:i-1)
          count(1:i-1)     = temp_count(1:i-1)
          next(1:i-1)      = temp_next(1:i-1)
          jobs(i:total-1)  = temp_jobs(i+1:total)
          count(i:total-1) = temp_count(i+1:total)
          next(i:total-1)  = temp_next(i+1:total)
        else
          jobs(1:total-1)  = temp_jobs(i+1:total)
          count(1:total-1) = temp_count(i+1:total)
          next(1:total-1)  = temp_next(i+1:total)
        endif
        total = total - 1
        deallocate(temp_jobs)
        deallocate(temp_count)
        deallocate(temp_next)
      enddo

      return
      end subroutine cfesj_remove_blank_rows


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module cfesj_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

