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
! Name       : cfesl 
! Category   : cfe
! Written    : 1999-08-26   by: Donna K. Vunderink
! Revised    : 2003-08-12   by: Tom Stoeckley
! Maturity   : production   2003-09-15
! Purpose    : Display information to the user about the current session.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! SESSION_LOG tabbed screen displays to the user session log information
! relating to the current CFE session.  Information shown is:
!
!       JOBS            Names of workfiles created in the current session,
!
!       BUILT           Time each jobfile was built,
!
!       SUBMITTED       Time each job was submitted, and
!
!       JOB_SERIES      Job series information on each job.
!
!
! Information on each job is shown on one row of the scrollable lists.
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
! 12. 2003-09-15  Stoeckley    Changed name from cfe_sl to cfesl;
!                               changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
! 11. 2001-03-12  Vunderink    Added new action UPDATE to cfesl_add
! 10. 2000-10-08  Vunderink    Removed HelpSection.
!  9. 2000-09-04  Vunderink    Release all memory on delete to aid in finding
!                                memory leaks.
!  8. 2000-08-15  Vunderink    Removed use of cfe_constants
!  7. 2000-05-23  Vunderink    Added parameter cache calls to set minsize and
!                                maxsize of arrays.
!  6. 2000-05-09  Vunderink    Make sure all array pointers are allocated, and
!                                made changes so that the session log is 
!                                actually a day log.
!  5. 2000-04-24  Vunderink    Added new action SERIES to cfesl_add
!  4. 2000-02-23  Vunderink    Added session log help
!  3. 2000-02-16  Vunderink    Changed seriescount argument in cfesl_add to
!                                character so that "count / total" format could
!                                be used.
!  2. 2000-02-03  Vunderink    Fixed bug in sl_add when seriescount present.
!  1. 1999-08-26  Vunderink    Initial version.
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


      module cfesl_module

      use cfestruct_module
      use cfeutil_module
      use pc_module
      use string_module
      implicit none

      private
      public :: cfesl_create
      public :: cfesl_initialize
      public :: cfesl_update
      public :: cfesl_delete
      public :: cfesl_add

      character(len=100),public,save :: cfesl_ident = &
       '$Id: cfesl.f90,v 1.12 2003/09/12 19:28:11 Stoeckley prod sps $'

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine cfesl_create (obj)
      implicit none
      type(cfestruct),pointer :: obj       ! argument

      nullify(obj%SLJobs)
      nullify(obj%SLBuilt)
      nullify(obj%SLSubmitted)
      nullify(obj%SLSeries)
      nullify(obj%SLCount)
      nullify(obj%SLNext)

      return
      end subroutine cfesl_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine cfesl_delete (obj)
      implicit none
      type(cfestruct),pointer :: obj       ! argument

      if (associated(obj%SLJobs)     ) deallocate(obj%SLJobs)
      if (associated(obj%SLBuilt)    ) deallocate(obj%SLBuilt)
      if (associated(obj%SLSubmitted)) deallocate(obj%SLSubmitted)
      if (associated(obj%SLSeries)   ) deallocate(obj%SLSeries)
      if (associated(obj%SLCount)    ) deallocate(obj%SLCount)
      if (associated(obj%SLNext)     ) deallocate(obj%SLNext)

      return
      end subroutine cfesl_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine cfesl_initialize (obj)
      implicit none
      type(cfestruct),pointer :: obj                                  !argument

      object => obj                                          ! needed for traps.

      allocate(obj%SLJobs(1))
      allocate(obj%SLBuilt(1))
      allocate(obj%SLSubmitted(1))
      allocate(obj%SLSeries(1))
      allocate(obj%SLCount(1))
      allocate(obj%SLNext(1))

      obj%SLtotal        = 0
      obj%SLJobs(1)      = ' '
      obj%SLBuilt(1)     = ' '
      obj%SLSubmitted(1) = ' '
      obj%SLSeries(1)    = ' '
      obj%SLCount(1)     = ' '
      obj%SLNext(1)      = ' '

      return
      end subroutine cfesl_initialize


!!------------------------------ update ------------------------------------!!
!!------------------------------ update ------------------------------------!!
!!------------------------------ update ------------------------------------!!


      subroutine cfesl_update (obj)
      implicit none
      type(cfestruct),target :: obj                                   !argument

      integer                 :: narrays = 6                           !local
      character(len=12)       :: arrays(6)                             !local

      object => obj                                          ! needed for traps.

      arrays(1) = 'SLJobs'
      arrays(2) = 'SLBuilt'
      arrays(3) = 'SLSubmitted'
      arrays(4) = 'SLSeries'
      arrays(5) = 'SLCount'
      arrays(6) = 'SLNext'

      call pc_register_array_names ('SessionArraySet',arrays,narrays)

      call pc_put_minsize_arrayset ('SessionArraySet',obj%SLtotal)
      call pc_put_maxsize_arrayset ('SessionArraySet',obj%SLtotal)

      call pc_put ('SLJobs'      ,obj%SLJobs      ,obj%SLtotal)
      call pc_put ('SLBuilt'     ,obj%SLBuilt     ,obj%SLtotal)
      call pc_put ('SLSubmitted' ,obj%SLSubmitted ,obj%SLtotal)
      call pc_put ('SLSeries'    ,obj%SLSeries    ,obj%SLtotal)
      call pc_put ('SLCount'     ,obj%SLCount     ,obj%SLtotal)
      call pc_put ('SLNext'      ,obj%SLNext      ,obj%SLtotal)

      return
      end subroutine cfesl_update


!!-------------------------------- add -------------------------------------!!
!!-------------------------------- add -------------------------------------!!
!!-------------------------------- add -------------------------------------!!


      subroutine cfesl_add (name,action,series,seriescount,nextseries)
      implicit none
      character(len=*),intent(in)          :: name                     !argument
      character(len=*),intent(in)          :: action                   !argument
      character(len=*),intent(in),optional :: series                   !argument
      character(len=*),intent(in),optional :: seriescount              !argument
      character(len=*),intent(in),optional :: nextseries               !argument

      character(len=20)                    :: time_date                !local
      integer                              :: i                        !local
      integer                              :: indx                     !local
      integer                              :: itemp                    !local

      call string_time_date (time_date)

      if (object%SLtotal .eq. 0) then
        indx = 1
        object%SLtotal = 1
!       allocate(object%SLJobs(indx))
!       allocate(object%SLBuilt(indx))
!       allocate(object%SLSubmitted(indx))
!       allocate(object%SLSeries(indx))
!       allocate(object%SLCount(indx))
!       allocate(object%SLNext(indx))
        object%SLJobs(indx) = trim(name)
        object%SLBuilt(indx)     = ' '
        object%SLSubmitted(indx) = ' '
        object%SLSeries(indx)    = ' '
        object%SLCount(indx)     = ' '
        object%SLNext(indx)      = ' '
      else
        indx = 0
        do i=object%SLtotal,1,-1
          if (trim(object%SLJobs(i)) .eq. trim(name)) then
            indx = i
            exit
          endif
        enddo
        if (indx .gt. 0) then
          if (action.eq.'BUILT') then
            if (len_trim(object%SLSubmitted(indx)).eq.0) then
              itemp = object%SLTotal
              call cfeutil_remove_array_element (object%SLJobs      ,  &
                                                 object%SLTotal     ,indx,indx)
              object%SLTotal = itemp
              call cfeutil_remove_array_element (object%SLBuilt     ,      &
                                                 object%SLTotal     ,indx,indx)
              object%SLTotal = itemp
              call cfeutil_remove_array_element (object%SLSubmitted ,      &
                                                 object%SLTotal     ,indx,indx)
              object%SLTotal = itemp
              call cfeutil_remove_array_element (object%SLSeries    ,      &
                                                 object%SLTotal     ,indx,indx)
              object%SLTotal = itemp
              call cfeutil_remove_array_element (object%SLCount     ,      &
                                                 object%SLTotal     ,indx,indx)
              object%SLTotal = itemp
              call cfeutil_remove_array_element (object%SLNext      ,      &
                                                 object%SLTotal     ,indx,indx)
            endif
            indx = 0
          else
            if (len_trim(object%SLSubmitted(indx)).ne.0) indx = 0
          endif
        endif

        if (indx .eq. 0) then
          indx  = object%SLTotal + 1
          itemp = object%SLTotal
          call cfeutil_append_array_element (object%SLJobs      ,      &
                                             object%SLTotal     ,trim(name))
          object%SLTotal = itemp
          call cfeutil_append_array_element (object%SLBuilt     ,      &
                                             object%SLTotal     ,' ')
          object%SLTotal = itemp
          call cfeutil_append_array_element (object%SLSubmitted ,      &
                                             object%SLTotal     ,' ')
          object%SLTotal = itemp
          call cfeutil_append_array_element (object%SLSeries    ,      &
                                             object%SLTotal     ,' ')
          object%SLTotal = itemp
          call cfeutil_append_array_element (object%SLCount     ,      &
                                             object%SLTotal     ,' ')
          object%SLTotal = itemp
          call cfeutil_append_array_element (object%SLNext      ,      &
                                             object%SLTotal     ,' ')
        endif
      endif

      if (action .eq. 'BUILT') then
        object%SLBuilt(indx)     = time_date
      else if (action .eq. 'SUBMITTED') then
        object%SLSubmitted(indx) = time_date
        if (present(series)) then
          object%SLSeries(indx)  = series
        else
          object%SLSeries(indx)  = ' '
        endif
        if (present(seriescount)) then
          object%SLCount(indx)   = seriescount
        else
          object%SLCount(indx)   = ' '
        endif
        if (present(nextseries)) then
          object%SLNext(indx)    = nextseries
        else
          object%SLNext(indx)    = ' '
        endif
      else if (action .eq. 'SERIES') then
        object%SLSubmitted(indx) = 'part of series'
        if (present(series)) then
          object%SLSeries(indx)  = series
        else
          object%SLSeries(indx)  = ' '
        endif
        if (present(seriescount)) then
          object%SLCount(indx)   = seriescount
        else
          object%SLCount(indx)   = ' '
        endif
        if (present(nextseries)) then
          object%SLNext(indx)    = nextseries
        else
          object%SLNext(indx)    = ' '
        endif
      else if (action .eq. 'UPDATE') then
        object%SLSubmitted(indx) = 'update of series'
        if (present(series)) then
          object%SLSeries(indx)  = series
        else
          object%SLSeries(indx)  = ' '
        endif
        if (present(seriescount)) then
          object%SLCount(indx)   = seriescount
        else
          object%SLCount(indx)   = ' '
        endif
        if (present(nextseries)) then
          object%SLNext(indx)    = nextseries
        else
          object%SLNext(indx)    = ' '
        endif
      endif

      call cfeutil_save_session_log (object)
      call cfesl_update             (object)

      return
      end subroutine cfesl_add


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module cfesl_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

