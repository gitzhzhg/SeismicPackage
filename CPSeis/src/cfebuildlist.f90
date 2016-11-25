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
! Name       : cfebuildlist
! Category   : cfe
! Written    : 2000-08-15   by: Donna K. Vunderink
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : CFE List Builder Module.
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
!004. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
!  3. 2006-01-10  B. Menger    Removed Unused Variables.
!  2. 2003-09-15  Stoeckley    Changed name from cfe_buildlist to cfebuildlist;
!                               changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
!  1. 2002-01-04  Vunderink    Initial version.
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


      module cfebuildlist_module

      use buildlist_module
      use cfegui_module
      use cfestruct_module
      use cfewindow_module
      use cfelist_module
      use pc_module
      use process_module
      use string_module
      use workfile_module

      implicit none

      private
      public :: cfebuildlist_create
      public :: cfebuildlist_delete
      public :: cfebuildlist_update
      public :: cfebuildlist_reset
      public :: cfebuildlist_delete_popup
      public :: cfebuildlist_update_popups

      character(len=100),public,save :: cfebuildlist_ident = &
       '$Id: cfebuildlist.f90,v 1.4 2006/09/18 13:32:41 Glover prod sps $'

      interface cfebuildlist_create
        module procedure cfebuildlist_create_1
        module procedure cfebuildlist_create_2
      end interface

      contains


!!------------------------------- create_1 ---------------------------------!!
!!------------------------------- create_1 ---------------------------------!!
!!------------------------------- create_1 ---------------------------------!!


      subroutine cfebuildlist_create_1 (obj,process)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument
      type(process_struct),pointer        :: process                  !argument

      integer                             :: process_id               !local
      character(len=10)                   :: parameter_indx           !local
      character(len=10)                   :: cid                      !local
      character(len=PC_LENGTH)            :: process_name             !local
      character(len=PC_LENGTH)            :: parameter_name           !local

      call process_get_ipn  (process,process_id)
      call process_get_name (process,process_name)
      parameter_name = ' '
      parameter_name = pc_get_gui_keyword(1)
      if (pc_gui_action_present(parameter_name,'ItemClicked')) then
        call pc_get_gui (parameter_name,'ItemClicked',parameter_indx)
        call string_ii2cc (process_id,cid)
        if (len_trim(parameter_indx) .eq. 0) then
          call cfebuildlist_create_private (obj,process,cid,process_name, &
                                            trim(parameter_name),.false., &
                                            .false.)
        else
          call cfebuildlist_create_private (obj,process,cid,process_name, &
                                            trim(parameter_name)//'#'//   &
                                            trim(parameter_indx),.false., &
                                            .false.)
        endif
      endif

      return
      end subroutine cfebuildlist_create_1


!!------------------------------- create_2 ---------------------------------!!
!!------------------------------- create_2 ---------------------------------!!
!!------------------------------- create_2 ---------------------------------!!


      subroutine cfebuildlist_create_2 (obj,cipn,name,parameter,  &
                                             same_window)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument
      character(len=*),intent(in)         :: cipn                     !argument
      character(len=*),intent(in)         :: name                     !argument
      character(len=*),intent(in)         :: parameter                !argument
      logical,optional                    :: same_window              !argument

      integer                             :: i                        !local
      integer                             :: ipn                      !local
      type(process_struct),pointer        :: process                  !local

      nullify (process) ! jpa

      call string_cc2ii (cipn,ipn)
      call workfile_get_first_process (object%templateWorkfile,process)
      do i=1,ipn-1
        if (.not. associated(process)) exit
        call process_get_next (process,process)
      enddo

      if (present(same_window)) then
        call cfebuildlist_create_private (obj,process,cipn,name,parameter,  &
                                          same_window,.true.)
      else
        call cfebuildlist_create_private (obj,process,cipn,name,parameter,  &
                                          .false.,.true.)
      endif

      return
      end subroutine cfebuildlist_create_2


!!---------------------------- create_private ------------------------------!!
!!---------------------------- create_private ------------------------------!!
!!---------------------------- create_private ------------------------------!!


      subroutine cfebuildlist_create_private (obj,process,cipn,process_name,  &
                                              parameter_name,same_window,     &
                                              add_parameter_list)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument
      type(process_struct),pointer        :: process                  !argument
      character(len=*)    ,intent(in)     :: cipn                     !argument
      character(len=*)    ,intent(in)     :: process_name             !argument
      character(len=*)    ,intent(in)     :: parameter_name           !argument
      logical,optional                    :: same_window              !argument
      logical,optional                    :: add_parameter_list       !argument

      character(len=PC_LENGTH)            :: local_key                !local
      character(len=PC_LENGTH)            :: window_type              !local
      character(len=PC_LENGTH)            :: window_title             !local
      character(len=10)                   :: ctemp                    !local
      character(len=PC_LENGTH)            :: bigword                  !local
      character(len=PC_LENGTH)            :: parameter_value          !local
      character(len=PC_LENGTH)            :: errmsg                   !local
      character(len=PC_LENGTH),pointer    :: array(:)                 !local
      character(len=PC_LENGTH),pointer    :: parameterlist(:)         !local
      integer                             :: i                        !local
      integer                             :: indx                     !local
      integer                             :: window_id                !local
      integer                             :: narray                   !local
      type(cfewindow_struct),pointer      :: window                   !local
      type(cardset_struct),pointer         :: cardset                  !local
      type(buildlist_struct),pointer       :: parameter                !local

      nullify (cardset) ! jpa
      nullify (window) ! jpa

      window_type  = 'PARAMETER'
      call string_ii2cc (obj%NtemplateProcessList,ctemp)
      window_title = trim(obj%templateJobName) // '   ----------   ' //  &
                     trim(parameter_name)         // '   ----------   ' //  &
                     'Process ' // trim(cipn) // ' of ' // trim(ctemp)
      local_key = ' '
      bigword = trim(process_name)//'#'//trim(cipn)//'#'//trim(parameter_name)
      window_id=cfewindow_match (window_type,local_key,bigword,0)
      if (window_id .ne. -1) then
        call cfegui_jump_window (window_id)
        return
      endif
      nullify(parameter)
      i = index(parameter_name,'#')
      if (i .eq. 0) then
        call process_get_by_keyword (process,'PROCESS',parameter_name,  &
                                     parameter_value)
      else
        call string_cc2ii (parameter_name(i+1:),indx)
        call process_get_by_keyword (process,'PROCESS',parameter_name(1:i-1),  &
                                     indx,parameter_value)
      endif
      call buildlist_create (parameter,obj%working_dir,obj%NjobnameList,  &
                            trim(parameter_name),trim(parameter_value),.true.)

      call buildlist_set_buffer (parameter,obj%masterBuffer)
      if (cardset_keyword_present(obj%masterCardset,bigword)) then
        nullify(array)
        call cardset_alloc_array (obj%masterCardset,bigword,array,narray,  &
                                  errmsg)
        call buildlist_set_values (parameter,array,narray)
        if (associated(array)) deallocate(array)
      endif

      if (present(add_parameter_list)) then
        if (add_parameter_list) then
          allocate(parameterlist(obj%MWBtotal))
          do i=1,obj%MWBtotal
            call string_ii2cc (i,ctemp)
            parameterlist(i) = trim(ctemp)//'   '//  &
                               trim(obj%MWBipn(i))//'#'//      &
                               trim(obj%MWBprocess(i))//'#'//  &
                               trim(obj%MWBkeyword(i))
          enddo
          call pc_put_options_field ('PARAMETERLIST',parameterlist,obj%MWBtotal)
          call pc_put_gui_only      ('PARAMETERLIST',  &
                                             parameterlist(obj%MWBparmselected))
          deallocate(parameterlist)
        else
          call pc_put_gui ('PARAMETERLIST','VISIBLE','FALSE')
        endif
      else
          call pc_put_gui ('PARAMETERLIST','VISIBLE','FALSE')
      endif

      if (present(same_window)) then
        if (same_window) then
          call cfewindow_get_current (window)
          call cfewindow_get_cardset (window,cardset)
          call cardset_clear         (cardset)
          call cfewindow_set_cardset (window,cardset)
        else
          call cfewindow_create          (window)
        endif
      else
        call cfewindow_create          (window)
      endif
      call cfewindow_set_window_type (window ,window_type)
      call cfewindow_set_keyword     (window ,local_key)
!!!!!!call cfewindow_set_value       (window ,parameter_name)
      call cfewindow_set_value       (window ,trim(bigword))
      call cfewindow_set_process     (window ,process)
      call cfewindow_set_parameter   (window ,parameter)
      call cfewindow_get_window_id   (window ,window_id)

      call cfegui_create (window_id,trim(window_title),'mwb_parameter.xml', &
                           .false.)

      call cfewindow_set_current (window)

      return
      end subroutine cfebuildlist_create_private


!!-------------------------------- delete ----------------------------------!!
!!-------------------------------- delete ----------------------------------!!
!!-------------------------------- delete ----------------------------------!!


      subroutine cfebuildlist_delete (parameter)
      implicit none
      type(buildlist_struct),pointer       :: parameter                !argument

      call buildlist_delete (parameter)

      return
      end subroutine cfebuildlist_delete


!!-------------------------------- update ----------------------------------!!
!!-------------------------------- update ----------------------------------!!
!!-------------------------------- update ----------------------------------!!


      subroutine cfebuildlist_update (parameter)
      implicit none
      type(buildlist_struct),pointer       :: parameter                !argument

      call buildlist_update (parameter)
      call cfebuildlist_update_popups()

      return
      end subroutine cfebuildlist_update


!!--------------------------------- reset ----------------------------------!!
!!--------------------------------- reset ----------------------------------!!
!!--------------------------------- reset ----------------------------------!!


      subroutine cfebuildlist_reset (obj,process,parameter)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument
      type(process_struct),pointer        :: process                  !argument
      type(buildlist_struct),pointer      :: parameter                !argument

      integer                             :: i                        !local


      integer                             :: indx                     !local
      integer                             :: ipn                      !local
      integer                             :: narray                   !local
      character(len=10)                   :: cipn                     !local


      character(len=PC_LENGTH)            :: process_name             !local
      character(len=PC_LENGTH)            :: keyword                  !local
      character(len=PC_LENGTH)            :: bigword                  !local
      character(len=PC_LENGTH)            :: value                    !local
      character(len=PC_LENGTH)            :: errmsg                   !local
      character(len=PC_LENGTH),pointer    :: array(:)                 !local



      nullify(array)

      if (associated(process)) then
        call process_get_name   (process  ,process_name)
        call process_get_ipn    (process  ,ipn         )
        call buildlist_get_name (parameter,keyword     )

        i = index(keyword,'#')
        if (i .eq. 0) then
          call process_get_by_keyword (process,'PROCESS',keyword,value)
        else
          call string_cc2ii (keyword(i+1:),indx)
          call process_get_by_keyword (process,'PROCESS',keyword(1:i-1), &
                                       indx,value)
        endif

        call string_ii2cc (ipn,cipn)
        bigword = trim(process_name)//'#'//trim(cipn)//'#'//trim(keyword)
        if (associated(obj%masterCardset)) then
          if (cardset_keyword_present(obj%masterCardset,bigword)) then
            narray = 0
            call cardset_alloc_array (obj%masterCardset,bigword,array,narray,  &
                                      errmsg)
          else
            narray = obj%NjobnameList
            allocate(array(narray))
            array = value
          endif
        else
          narray = obj%NjobnameList
          allocate(array(narray))
          array = value
        endif
      else
        call buildlist_get_name (parameter,keyword)
        if (trim(keyword) .eq. 'BUILDLIST') then
          value = ' '
          narray = 2
          allocate(array(narray))
          array = ' '
        else if (trim(keyword) .eq. 'JOBNAMELIST') then
          i = index(obj%templateJobname,'.wrk',.TRUE.) - 1
          if (i .le. 0) then
            value = obj%templateJobname
          else
            value = obj%templateJobname(1:i)
          endif
          narray = obj%NjobnameList
          allocate(array(narray))
          array = obj%jobnameList
        endif
      endif

      call buildlist_reset      (parameter,keyword,narray,value)
      call buildlist_set_values (parameter,array,narray)

      if (associated(array)) deallocate(array)

      return
      end subroutine cfebuildlist_reset


!!---------------------------- delete_popup --------------------------------!!
!!---------------------------- delete_popup --------------------------------!!
!!---------------------------- delete_popup --------------------------------!!


      subroutine cfebuildlist_delete_popup (window_id)
      implicit none
      integer,intent(in)               :: window_id                   !argument

      type(buildlist_struct),pointer   :: parameter                   !local
      type(cfewindow_struct),pointer   :: window                      !local

      nullify (window) ! jpa
      nullify (parameter)

      call cfegui_delete           (window_id,'Close')
      call cfewindow_get_pointer   (window_id,window )
      call cfewindow_get_parameter (window   ,parameter)
      call cfebuildlist_delete (parameter)
      call cfewindow_delete        (window)

      return
      end subroutine cfebuildlist_delete_popup


!!--------------------------- update_popups --------------------------------!!
!!--------------------------- update_popups --------------------------------!!
!!--------------------------- update_popups --------------------------------!!


      subroutine cfebuildlist_update_popups()
      implicit none

      integer                          :: i                           !local
      integer,pointer                  :: window_ids(:)               !local
      integer                          :: nwindow_ids                 !local
      integer                          :: current_id                  !local
      type(cfewindow_struct),pointer   :: window                      !local
      type(buildlist_struct),pointer   :: parameter                   !local

      nullify (window) ! jpa
      nullify (window_ids) ! jpa

      call cfewindow_get_all_of_type ('PARAMETER',window_ids)
      if (associated(window_ids)) then
        call cfewindow_get_current (window)
        call cfewindow_get_window_id (window,current_id)
        nwindow_ids = size(window_ids)
        call pc_put_gui ('PARAMETER','UpdateWindow',window_ids,nwindow_ids)
        do i=1,nwindow_ids
          if (window_ids(i) .eq. current_id) cycle
          call cfewindow_get_pointer (window_ids(i),window)
          nullify(parameter)
          call cfewindow_get_parameter (window,parameter)
          call buildlist_update_popup (parameter)
        enddo
        deallocate(window_ids)
      endif

      return
      end subroutine cfebuildlist_update_popups


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module cfebuildlist_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

