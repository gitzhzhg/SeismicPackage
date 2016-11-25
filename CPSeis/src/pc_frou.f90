
!<CPS_v1 type="AUXILIARY_FILE"/>
!!------------------------------ pc_frou.f90 -------------------------------!!
!!------------------------------ pc_frou.f90 -------------------------------!!
!!------------------------------ pc_frou.f90 -------------------------------!!

        ! other files are:  pc.f90  pcw.cc  pcw.hh
        !                   pcw_fortran_capitals.hh
        !                   pcw_fortran_underscores.hh
        !                   pcw_fortran_prototypes.hh
        !                   PC.cpp  PC.java


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
! Name       : PC                  (parameter cache)
! Category   : character
! Written    : 2005-08-11   by: Tom Stoeckley
! Revised    : 2007-09-18   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Parameter cache for passing parameters to/from process modules.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2007-09-18  Stoeckley  Add ability to get variable types.
!  2. 2007-04-24  Stoeckley  Pass -6 to the parameter cache for noprint instead
!                             of 0.
!  1. 2005-08-11  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


     ! Note: Since this parameter cache wrapper will never be called
     ! from inside a Fortran-90 process module, the functions which would
     ! only be called from such a process module are eliminated in this
     ! wrapper.  These include the pc_alloc routines.  In addition,
     ! function arguments which would only be used from such a process
     ! module are also eliminated.  These include trap arguments.

     ! Note: In spite of the above-mentioned eliminations, full
     ! functionality is retained for processes which are written
     ! in C or C++ or java.

     ! Note: Because of the difference between C streams and Fortran 
     ! logical unit numbers, no logical unit numbers are passed through
     ! this wrapper, and all prints will be to standard out.

     ! Note: Jump functions, and multi-argument error/warning/info/print
     ! functions, are omitted from this wrapper because they are unnecessary.


!!----------------------------- module ------------------------------------!!
!!----------------------------- module ------------------------------------!!
!!----------------------------- module ------------------------------------!!


      module pc_frou_module

      use pc_module
      use string_module
      use convert_module
      use grid_frou_module
      use grid_module
      implicit none
      public

      character(len=100),public,save :: PC_FROU_IDENT = &
'$Id: pc_frou.f90,v 1.3 2007/09/19 14:02:25 Stoeckley beta sps $'

      integer,parameter,public :: PC_MESSAGE_LENGTH = 80
      integer,parameter,public :: PC_KEYWORD_LENGTH = 80
      integer,parameter,public :: LUNPRINT = 6

      end module pc_frou_module


!!----------------------------- exists -------------------------------------!!
!!----------------------------- exists -------------------------------------!!
!!----------------------------- exists -------------------------------------!!


      function pc_frou_exists () result (exists)
      use pc_frou_module
      implicit none
      integer :: exists                          ! result
      logical :: exists9                         ! local

      exists9 = pc_exists()
      call convert_ll2ii (exists9,exists)
      end function pc_frou_exists


!!---------------- frontend and backend and gui update --------------------!!
!!---------------- frontend and backend and gui update --------------------!!
!!---------------- frontend and backend and gui update --------------------!!


      subroutine pc_frou_frontend_update
      use pc_frou_module
      implicit none

      call pc_frontend_update (LUNPRINT)
      end subroutine pc_frou_frontend_update



      subroutine pc_frou_backend_update
      use pc_frou_module
      implicit none

      call pc_backend_update (LUNPRINT)
      end subroutine pc_frou_backend_update



      subroutine pc_frou_gui_update
      use pc_frou_module
      implicit none

      call pc_gui_update (LUNPRINT)
      end subroutine pc_frou_gui_update



      subroutine pc_frou_quick_update
      use pc_frou_module
      implicit none

      call pc_quick_update (LUNPRINT)
      end subroutine pc_frou_quick_update


!!----------- frontend and backend and gui update (noprint) ------------!!
!!----------- frontend and backend and gui update (noprint) ------------!!
!!----------- frontend and backend and gui update (noprint) ------------!!


      subroutine pc_frou_frontend_update_noprint
      use pc_frou_module
      implicit none

      call pc_frontend_update (-LUNPRINT)
      end subroutine pc_frou_frontend_update_noprint



      subroutine pc_frou_backend_update_noprint
      use pc_frou_module
      implicit none

      call pc_backend_update (-LUNPRINT)
      end subroutine pc_frou_backend_update_noprint



      subroutine pc_frou_gui_update_noprint
      use pc_frou_module
      implicit none

      call pc_gui_update (-LUNPRINT)
      end subroutine pc_frou_gui_update_noprint



      subroutine pc_frou_quick_update_noprint
      use pc_frou_module
      implicit none

      call pc_quick_update (-LUNPRINT)
      end subroutine pc_frou_quick_update_noprint


!!------------------------------ clear ----------------------------------!!
!!------------------------------ clear ----------------------------------!!
!!------------------------------ clear ----------------------------------!!


      subroutine pc_frou_clear
      use pc_frou_module
      implicit none

      call pc_clear
      end subroutine pc_frou_clear


!!----------------------------- restore ---------------------------------!!
!!----------------------------- restore ---------------------------------!!
!!----------------------------- restore ---------------------------------!!


      subroutine pc_frou_restore
      use pc_frou_module
      implicit none

      call pc_restore
      end subroutine pc_frou_restore


!!----------------------------- next ------------------------------------!!
!!----------------------------- next ------------------------------------!!
!!----------------------------- next ------------------------------------!!


      subroutine pc_frou_next
      use pc_frou_module
      implicit none

      call pc_next
      end subroutine pc_frou_next


!!------------------------ backend execute ------------------------------!!
!!------------------------ backend execute ------------------------------!!
!!------------------------ backend execute ------------------------------!!


      subroutine pc_frou_backend_execute
      use pc_frou_module
      implicit none

      call pc_backend_execute
      end subroutine pc_frou_backend_execute


!!--------------------- continue backend update ----------------------------!!
!!--------------------- continue backend update ----------------------------!!
!!--------------------- continue backend update ----------------------------!!


      subroutine pc_frou_continue_backend_update
      use pc_frou_module
      implicit none

      call pc_continue_backend_update
      end subroutine pc_frou_continue_backend_update


!!-------------------- get and set state variables -------------------------!!
!!-------------------- get and set state variables -------------------------!!
!!-------------------- get and set state variables -------------------------!!


      function pc_frou_get_update_state () result (update_state)
      use pc_frou_module
      implicit none
      integer :: update_state                   ! result

      update_state = pc_get_update_state()
      end function pc_frou_get_update_state



      subroutine pc_frou_set_backend_no_exec
      use pc_frou_module
      implicit none

      call pc_set_backend_no_exec
      end subroutine pc_frou_set_backend_no_exec           



      subroutine pc_frou_set_backend_yes_exec
      use pc_frou_module
      implicit none

      call pc_set_backend_no_exec
      end subroutine pc_frou_set_backend_yes_exec           



      function pc_frou_get_ipn () result (ipn)
      use pc_frou_module
      implicit none
      integer :: ipn                            ! result

      ipn = pc_get_ipn()
      end function pc_frou_get_ipn



      function pc_frou_previous_error () result (error)
      use pc_frou_module
      implicit none
      integer :: error                          ! result
      logical :: error9                         ! local

      error9 = pc_previous_error()
      call convert_ll2ii (error9,error)
      end function pc_frou_previous_error



      subroutine pc_frou_set_ipn (ipn)
      use pc_frou_module
      implicit none
      integer,intent(in) :: ipn                 ! argument

      call pc_set_ipn (ipn)
      end subroutine pc_frou_set_ipn



!!------------------------- do not process traces ------------------------!!
!!------------------------- do not process traces ------------------------!!
!!------------------------- do not process traces ------------------------!!


      function pc_frou_do_not_process_traces () result (stop_now)
      use pc_frou_module
      implicit none
      integer                     :: stop_now        ! result
      logical                     :: stop_now9       ! local

      stop_now9 = pc_do_not_process_traces()
      call convert_ll2ii (stop_now9,stop_now)
      end function pc_frou_do_not_process_traces


!!--------------------- deal with errors and messages --------------------!!
!!--------------------- deal with errors and messages --------------------!!
!!--------------------- deal with errors and messages --------------------!!


      function pc_frou_update_error () result (error)
      use pc_frou_module
      implicit none
      integer :: error                          ! result
      logical :: error9                         ! local

      error9 = pc_update_error()
      call convert_ll2ii (error9,error)
      end function pc_frou_update_error



      subroutine pc_frou_error (msg)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: msg(*)             ! argument
      character(len=PC_MESSAGE_LENGTH) :: msg9               ! local

      call string_hh2cc (msg,msg9)
      call pc_error     (msg9)
      end subroutine pc_frou_error



      subroutine pc_frou_warning (msg)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: msg(*)             ! argument
      character(len=PC_MESSAGE_LENGTH) :: msg9               ! local

      call string_hh2cc (msg,msg9)
      call pc_warning   (msg9)
      end subroutine pc_frou_warning



      subroutine pc_frou_info (msg)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: msg(*)             ! argument
      character(len=PC_MESSAGE_LENGTH) :: msg9               ! local

      call string_hh2cc (msg,msg9)
      call pc_info      (msg9)
      end subroutine pc_frou_info



      subroutine pc_frou_print (msg)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: msg(*)             ! argument
      character(len=PC_MESSAGE_LENGTH) :: msg9               ! local

      call string_hh2cc (msg,msg9)
      call pc_print     (msg9)
      end subroutine pc_frou_print


!!------------------------ print data cards ----------------------------!!
!!------------------------ print data cards ----------------------------!!
!!------------------------ print data cards ----------------------------!!


      subroutine pc_frou_print_process_cards
      use pc_frou_module
      implicit none

      call pc_print_process_cards
      end subroutine pc_frou_print_process_cards



      subroutine pc_frou_print_global_cards
      use pc_frou_module
      implicit none

      call pc_print_global_cards
      end subroutine pc_frou_print_global_cards



      subroutine pc_frou_print_control_cards
      use pc_frou_module
      implicit none

      call pc_print_control_cards
      end subroutine pc_frou_print_control_cards



      subroutine pc_frou_print_pdata_cards
      use pc_frou_module
      implicit none

      call pc_print_pdata_cards
      end subroutine pc_frou_print_pdata_cards



      subroutine pc_frou_print_jdata_cards
      use pc_frou_module
      implicit none

      call pc_print_jdata_cards
      end subroutine pc_frou_print_jdata_cards



      subroutine pc_frou_print_gui_cards
      use pc_frou_module
      implicit none

      call pc_print_gui_cards
      end subroutine pc_frou_print_gui_cards

                              !!!!!!!!!!!!!!!!!!


      subroutine pc_frou_info_process_cards
      use pc_frou_module
      implicit none

      call pc_info_process_cards
      end subroutine pc_frou_info_process_cards



      subroutine pc_frou_info_global_cards
      use pc_frou_module
      implicit none

      call pc_info_global_cards
      end subroutine pc_frou_info_global_cards



      subroutine pc_frou_info_control_cards
      use pc_frou_module
      implicit none

      call pc_info_control_cards
      end subroutine pc_frou_info_control_cards



      subroutine pc_frou_info_pdata_cards
      use pc_frou_module
      implicit none

      call pc_info_pdata_cards
      end subroutine pc_frou_info_pdata_cards



      subroutine pc_frou_info_jdata_cards
      use pc_frou_module
      implicit none

      call pc_info_jdata_cards
      end subroutine pc_frou_info_jdata_cards



      subroutine pc_frou_info_gui_cards
      use pc_frou_module
      implicit none

      call pc_info_gui_cards
      end subroutine pc_frou_info_gui_cards



!!------------------------ num elements -----------------------------------!!
!!------------------------ num elements -----------------------------------!!
!!------------------------ num elements -----------------------------------!!


      function pc_frou_num_elements_process (keyword) result (nelements)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: nelements         ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc                   (keyword,keyword9)
      nelements = pc_num_elements_process (keyword9)
      end function pc_frou_num_elements_process


      function pc_frou_num_elements_global (keyword) result (nelements)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: nelements         ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc                  (keyword,keyword9)
      nelements = pc_num_elements_global (keyword9)
      end function pc_frou_num_elements_global


      function pc_frou_num_elements_control (keyword) result (nelements)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: nelements         ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc                   (keyword,keyword9)
      nelements = pc_num_elements_control (keyword9)
      end function pc_frou_num_elements_control


      function pc_frou_num_elements_gui (keyword, action) result (nelements)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer             ,intent(in)  :: action(*)         ! argument
      integer                          :: nelements         ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local

      call string_hh2cc               (keyword,keyword9)
      call string_hh2cc               (action,action9)
      nelements = pc_num_elements_gui (keyword9,action9)
      end function pc_frou_num_elements_gui


      function pc_frou_num_elements_pdata (keyword) result (nelements)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: nelements         ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc                 (keyword,keyword9)
      nelements = pc_num_elements_pdata (keyword9)
      end function pc_frou_num_elements_pdata


      function pc_frou_num_elements_jdata (keyword) result (nelements)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: nelements         ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc                 (keyword,keyword9)
      nelements = pc_num_elements_jdata (keyword9)
      end function pc_frou_num_elements_jdata


!!--------------------------- nature --------------------------------------!!
!!--------------------------- nature --------------------------------------!!
!!--------------------------- nature --------------------------------------!!


      function pc_frou_nature_process (keyword) result (nature)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: nature            ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      nature = pc_nature_process (keyword9)
      end function pc_frou_nature_process


      function pc_frou_nature_global (keyword) result (nature)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: nature            ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      nature = pc_nature_global (keyword9)
      end function pc_frou_nature_global


      function pc_frou_nature_control (keyword) result (nature)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: nature            ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      nature = pc_nature_control (keyword9)
      end function pc_frou_nature_control


      function pc_frou_nature_gui (keyword, action) result (nature)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)       ! argument
      integer             ,intent(in)  :: action(*)        ! argument
      integer                          :: nature           ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      character(len=PC_KEYWORD_LENGTH) :: action9          ! local

      call string_hh2cc      (keyword,keyword9)
      call string_hh2cc      (action,action9)
      nature = pc_nature_gui (keyword9,action9)
      end function pc_frou_nature_gui


      function pc_frou_nature_pdata (keyword) result (nature)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: nature            ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      nature = pc_nature_pdata (keyword9)
      end function pc_frou_nature_pdata


      function pc_frou_nature_jdata (keyword) result (nature)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: nature            ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      nature = pc_nature_jdata (keyword9)
      end function pc_frou_nature_jdata


!!--------------------------- vartype --------------------------------------!!
!!--------------------------- vartype --------------------------------------!!
!!--------------------------- vartype --------------------------------------!!


      function pc_frou_vartype_process (keyword) result (vartype)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: vartype           ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      vartype = pc_vartype_process (keyword9)
      end function pc_frou_vartype_process


      function pc_frou_vartype_global (keyword) result (vartype)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: vartype           ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      vartype = pc_vartype_global (keyword9)
      end function pc_frou_vartype_global


      function pc_frou_vartype_control (keyword) result (vartype)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: vartype           ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      vartype = pc_vartype_control (keyword9)
      end function pc_frou_vartype_control


      function pc_frou_vartype_gui (keyword, action) result (vartype)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)       ! argument
      integer             ,intent(in)  :: action(*)        ! argument
      integer                          :: vartype          ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      character(len=PC_KEYWORD_LENGTH) :: action9          ! local

      call string_hh2cc      (keyword,keyword9)
      call string_hh2cc      (action,action9)
      vartype = pc_vartype_gui (keyword9,action9)
      end function pc_frou_vartype_gui


      function pc_frou_vartype_pdata (keyword) result (vartype)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: vartype           ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      vartype = pc_vartype_pdata (keyword9)
      end function pc_frou_vartype_pdata


      function pc_frou_vartype_jdata (keyword) result (vartype)
      use pc_frou_module
      implicit none
      integer             ,intent(in)  :: keyword(*)        ! argument
      integer                          :: vartype           ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      vartype = pc_vartype_jdata (keyword9)
      end function pc_frou_vartype_jdata


!!---------------------- get scalars --------------------------------------!!
!!---------------------- get scalars --------------------------------------!!
!!---------------------- get scalars --------------------------------------!!


      subroutine pc_frou_get_gscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
      call string_hh2cc   (keyword,keyword9)
      call pc_get_gscalar (keyword9,obj)
      end subroutine pc_frou_get_gscalar



      subroutine pc_frou_get_iscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc   (keyword,keyword9)
      call pc_get_iscalar (keyword9,scalar)
      end subroutine pc_frou_get_iscalar



      subroutine pc_frou_get_fscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      real             ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc   (keyword,keyword9)
      call pc_get_fscalar (keyword9,scalar)
      end subroutine pc_frou_get_fscalar



      subroutine pc_frou_get_dscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      double precision ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc   (keyword,keyword9)
      call pc_get_dscalar (keyword9,scalar)
      end subroutine pc_frou_get_dscalar



      subroutine pc_frou_get_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(inout)  :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: scalar9           ! local

      call string_hh2cc   (keyword,keyword9)
      call convert_ii2ll  (scalar,scalar9)
      call pc_get_lscalar (keyword9,scalar9)
      call convert_ll2ii  (scalar9,scalar)
      end subroutine pc_frou_get_lscalar



      subroutine pc_frou_get_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(inout)  :: scalar(*)         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: scalar9           ! local

      call string_hh2cc   (keyword,keyword9)
      call string_hh2cc   (scalar,scalar9)
      call pc_get_cscalar (keyword9,scalar9)
      call string_cc2hh   (scalar9,scalar)
      end subroutine pc_frou_get_cscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_process_gscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
      call string_hh2cc           (keyword,keyword9)
      call pc_get_process_gscalar (keyword9,obj)
      end subroutine pc_frou_get_process_gscalar



      subroutine pc_frou_get_process_iscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_get_process_iscalar (keyword9,scalar)
      end subroutine pc_frou_get_process_iscalar



      subroutine pc_frou_get_process_fscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      real             ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_get_process_fscalar (keyword9,scalar)
      end subroutine pc_frou_get_process_fscalar



      subroutine pc_frou_get_process_dscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      double precision ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_get_process_dscalar (keyword9,scalar)
      end subroutine pc_frou_get_process_dscalar



      subroutine pc_frou_get_process_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(inout)  :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: scalar9           ! local

      call string_hh2cc           (keyword,keyword9)
      call convert_ii2ll          (scalar,scalar9)
      call pc_get_process_lscalar (keyword9,scalar9)
      call convert_ll2ii          (scalar9,scalar)
      end subroutine pc_frou_get_process_lscalar



      subroutine pc_frou_get_process_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(inout)  :: scalar(*)         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: scalar9           ! local

      call string_hh2cc           (keyword,keyword9)
      call string_hh2cc           (scalar,scalar9)
      call pc_get_process_cscalar (keyword9,scalar9)
      call string_cc2hh           (scalar9,scalar)
      end subroutine pc_frou_get_process_cscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_global_gscalar (keyword,scalar)
      use pc_frou_module
      implicit none

      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
!print *, 'pc_frou.f90 before get: xorigin = ',grid_get_xorigin(obj)
!print *, 'pc_frou.f90 before get: yorigin = ',grid_get_yorigin(obj)
!print *, 'pc_frou.f90 before get: xwidth = ',grid_get_xgrid_width(obj)
!print *, 'pc_frou.f90 before get: ywidth = ',grid_get_ygrid_width(obj)
!print *, 'pc_frou.f90 before get: angle = ',grid_get_rotation_angle(obj)
!print *, 'pc_frou.f90 before get: handedness = ',grid_get_handedness(obj)
      call string_hh2cc          (keyword,keyword9)
      call pc_get_global_gscalar (keyword9,obj)
!print *, 'pc_frou.f90 after get: xorigin = ',grid_get_xorigin(obj)
!print *, 'pc_frou.f90 after get: yorigin = ',grid_get_yorigin(obj)
!print *, 'pc_frou.f90 after get: xwidth = ',grid_get_xgrid_width(obj)
!print *, 'pc_frou.f90 after get: ywidth = ',grid_get_ygrid_width(obj)
!print *, 'pc_frou.f90 after get: angle = ',grid_get_rotation_angle(obj)
!print *, 'pc_frou.f90 after get: handedness = ',grid_get_handedness(obj)
      end subroutine pc_frou_get_global_gscalar



      subroutine pc_frou_get_global_iscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_global_iscalar (keyword9,scalar)
      end subroutine pc_frou_get_global_iscalar



      subroutine pc_frou_get_global_fscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      real             ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_global_fscalar (keyword9,scalar)
      end subroutine pc_frou_get_global_fscalar



      subroutine pc_frou_get_global_dscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      double precision ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_global_dscalar (keyword9,scalar)
      end subroutine pc_frou_get_global_dscalar



      subroutine pc_frou_get_global_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(inout)  :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: scalar9           ! local

      call string_hh2cc          (keyword,keyword9)
      call convert_ii2ll         (scalar,scalar9)
      call pc_get_global_lscalar (keyword9,scalar9)
      call convert_ll2ii         (scalar9,scalar)
      end subroutine pc_frou_get_global_lscalar



      subroutine pc_frou_get_global_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(inout)  :: scalar(*)         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: scalar9           ! local

      call string_hh2cc          (keyword,keyword9)
      call string_hh2cc          (scalar,scalar9)
      call pc_get_global_cscalar (keyword9,scalar9)
      call string_cc2hh          (scalar9,scalar)
      end subroutine pc_frou_get_global_cscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_control_gscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
      call string_hh2cc           (keyword,keyword9)
      call pc_get_control_gscalar (keyword9,obj)
      end subroutine pc_frou_get_control_gscalar



      subroutine pc_frou_get_control_iscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_get_control_iscalar (keyword9,scalar)
      end subroutine pc_frou_get_control_iscalar



      subroutine pc_frou_get_control_fscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      real             ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_get_control_fscalar (keyword9,scalar)
      end subroutine pc_frou_get_control_fscalar



      subroutine pc_frou_get_control_dscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      double precision ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_get_control_dscalar (keyword9,scalar)
      end subroutine pc_frou_get_control_dscalar



      subroutine pc_frou_get_control_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(inout)  :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: scalar9           ! local

      call string_hh2cc           (keyword,keyword9)
      call convert_ii2ll          (scalar,scalar9)
      call pc_get_control_lscalar (keyword9,scalar9)
      call convert_ll2ii          (scalar9,scalar)
      end subroutine pc_frou_get_control_lscalar



      subroutine pc_frou_get_control_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(inout)  :: scalar(*)         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: scalar9           ! local

      call string_hh2cc           (keyword,keyword9)
      call string_hh2cc           (scalar,scalar9)
      call pc_get_control_cscalar (keyword9,scalar9)
      call string_cc2hh           (scalar9,scalar)
      end subroutine pc_frou_get_control_cscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_gui_gscalar (keyword,action,scalar)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      integer               ,intent(in)    :: action(*)     ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local
      character(len=PC_KEYWORD_LENGTH)     :: action9       ! local

      obj => scalar%obj
      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call pc_get_gui_gscalar (keyword9,action9,obj)
      end subroutine pc_frou_get_gui_gscalar



      subroutine pc_frou_get_gui_iscalar (keyword,action,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call pc_get_gui_iscalar (keyword9,action9,scalar)
      end subroutine pc_frou_get_gui_iscalar



      subroutine pc_frou_get_gui_fscalar (keyword,action,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      real             ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call pc_get_gui_fscalar (keyword9,action9,scalar)
      end subroutine pc_frou_get_gui_fscalar



      subroutine pc_frou_get_gui_dscalar (keyword,action,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      double precision ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call pc_get_gui_dscalar (keyword9,action9,scalar)
      end subroutine pc_frou_get_gui_dscalar



      subroutine pc_frou_get_gui_lscalar (keyword,action,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(inout)  :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local
      logical                          :: scalar9           ! local

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call convert_ii2ll      (scalar,scalar9)
      call pc_get_gui_lscalar (keyword9,action9,scalar9)
      call convert_ll2ii      (scalar9,scalar)
      end subroutine pc_frou_get_gui_lscalar



      subroutine pc_frou_get_gui_cscalar (keyword,action,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(inout)  :: scalar(*)         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local
      character(len=PC_LENGTH)         :: scalar9           ! local

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call string_hh2cc       (scalar,scalar9)
      call pc_get_gui_cscalar (keyword9,action9,scalar9)
      call string_cc2hh       (scalar9,scalar)
      end subroutine pc_frou_get_gui_cscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_pdata_gscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
      call string_hh2cc         (keyword,keyword9)
      call pc_get_pdata_gscalar (keyword9,obj)
      end subroutine pc_frou_get_pdata_gscalar



      subroutine pc_frou_get_pdata_iscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_get_pdata_iscalar (keyword9,scalar)
      end subroutine pc_frou_get_pdata_iscalar



      subroutine pc_frou_get_pdata_fscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      real             ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_get_pdata_fscalar (keyword9,scalar)
      end subroutine pc_frou_get_pdata_fscalar



      subroutine pc_frou_get_pdata_dscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      double precision ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_get_pdata_dscalar (keyword9,scalar)
      end subroutine pc_frou_get_pdata_dscalar



      subroutine pc_frou_get_pdata_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(inout)  :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: scalar9           ! local

      call string_hh2cc         (keyword,keyword9)
      call convert_ii2ll        (scalar,scalar9)
      call pc_get_pdata_lscalar (keyword9,scalar9)
      call convert_ll2ii        (scalar9,scalar)
      end subroutine pc_frou_get_pdata_lscalar



      subroutine pc_frou_get_pdata_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(inout)  :: scalar(*)         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: scalar9           ! local

      call string_hh2cc         (keyword,keyword9)
      call string_hh2cc         (scalar,scalar9)
      call pc_get_pdata_cscalar (keyword9,scalar9)
      call string_cc2hh         (scalar9,scalar)
      end subroutine pc_frou_get_pdata_cscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_jdata_gscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
      call string_hh2cc         (keyword,keyword9)
      call pc_get_jdata_gscalar (keyword9,obj)
      end subroutine pc_frou_get_jdata_gscalar



      subroutine pc_frou_get_jdata_iscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_get_jdata_iscalar (keyword9,scalar)
      end subroutine pc_frou_get_jdata_iscalar



      subroutine pc_frou_get_jdata_fscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      real             ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_get_jdata_fscalar (keyword9,scalar)
      end subroutine pc_frou_get_jdata_fscalar



      subroutine pc_frou_get_jdata_dscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      double precision ,intent(out)    :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_get_jdata_dscalar (keyword9,scalar)
      end subroutine pc_frou_get_jdata_dscalar



      subroutine pc_frou_get_jdata_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(inout)  :: scalar            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: scalar9           ! local

      call string_hh2cc         (keyword,keyword9)
      call convert_ii2ll        (scalar,scalar9)
      call pc_get_jdata_lscalar (keyword9,scalar9)
      call convert_ll2ii        (scalar9,scalar)
      end subroutine pc_frou_get_jdata_lscalar



      subroutine pc_frou_get_jdata_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(inout)  :: scalar(*)         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: scalar9           ! local

      call string_hh2cc         (keyword,keyword9)
      call string_hh2cc         (scalar,scalar9)
      call pc_get_jdata_cscalar (keyword9,scalar9)
      call string_cc2hh         (scalar9,scalar)
      end subroutine pc_frou_get_jdata_cscalar


!!----------------------- get arrays -----------------------------------!!
!!----------------------- get arrays -----------------------------------!!
!!----------------------- get arrays -----------------------------------!!


      subroutine pc_frou_get_iarray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_get_iarray       (keyword9,array,nelements)
      end subroutine pc_frou_get_iarray




      subroutine pc_frou_get_farray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      real             ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_get_farray       (keyword9,array,nelements)
      end subroutine pc_frou_get_farray




      subroutine pc_frou_get_darray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      double precision ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_get_darray       (keyword9,array,nelements)
      end subroutine pc_frou_get_darray




      subroutine pc_frou_get_carray (keyword,nsize,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      integer          ,intent(in)     :: nwords            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: array9(nsize)     ! local
      integer                          :: i

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc_array (array,array9,nwords,nelements)
      call pc_get_carray      (keyword9,array9,nelements)
      call string_cc2hh_array (array9,array,nwords,nelements)
      end subroutine pc_frou_get_carray




      subroutine pc_frou_get_larray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: array9(nsize)     ! local

      call string_hh2cc        (keyword,keyword9)
      call convert_ii2ll_array (array,array9,nelements)
      call pc_get_larray       (keyword9,array9,nelements)
      call convert_ll2ii_array (array9,array,nelements)
      end subroutine pc_frou_get_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_process_iarray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_process_iarray (keyword9,array,nelements)
      end subroutine pc_frou_get_process_iarray




      subroutine pc_frou_get_process_farray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      real             ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_process_farray (keyword9,array,nelements)
      end subroutine pc_frou_get_process_farray




      subroutine pc_frou_get_process_darray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      double precision ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_process_darray (keyword9,array,nelements)
      end subroutine pc_frou_get_process_darray




      subroutine pc_frou_get_process_carray &
                               (keyword,nsize,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      integer          ,intent(in)     :: nwords            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: array9(nsize)     ! local

      call string_hh2cc          (keyword,keyword9)
      call string_hh2cc_array    (array,array9,nwords,nelements)
      call pc_get_process_carray (keyword9,array9,nelements)
      call string_cc2hh_array    (array9,array,nwords,nelements)
      end subroutine pc_frou_get_process_carray




      subroutine pc_frou_get_process_larray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: array9(nsize)     ! local

      call string_hh2cc          (keyword,keyword9)
      call convert_ii2ll_array   (array,array9,nelements)
      call pc_get_process_larray (keyword9,array9,nelements)
      call convert_ll2ii_array   (array9,array,nelements)
      end subroutine pc_frou_get_process_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_global_iarray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_get_global_iarray (keyword9,array,nelements)
      end subroutine pc_frou_get_global_iarray




      subroutine pc_frou_get_global_farray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      real             ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_get_global_farray (keyword9,array,nelements)
      end subroutine pc_frou_get_global_farray




      subroutine pc_frou_get_global_darray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      double precision ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_get_global_darray (keyword9,array,nelements)
      end subroutine pc_frou_get_global_darray




      subroutine pc_frou_get_global_carray &
                                 (keyword,nsize,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      integer          ,intent(in)     :: nwords            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: array9(nsize)     ! local

      call string_hh2cc         (keyword,keyword9)
      call string_hh2cc_array   (array,array9,nwords,nelements)
      call pc_get_global_carray (keyword9,array9,nelements)
      call string_cc2hh_array   (array9,array,nwords,nelements)
      end subroutine pc_frou_get_global_carray




      subroutine pc_frou_get_global_larray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: array9(nsize)     ! local

      call string_hh2cc         (keyword,keyword9)
      call convert_ii2ll_array  (array,array9,nelements)
      call pc_get_global_larray (keyword9,array9,nelements)
      call convert_ll2ii_array  (array9,array,nelements)
      end subroutine pc_frou_get_global_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_control_iarray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_control_iarray (keyword9,array,nelements)
      end subroutine pc_frou_get_control_iarray




      subroutine pc_frou_get_control_farray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      real             ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_control_farray (keyword9,array,nelements)
      end subroutine pc_frou_get_control_farray




      subroutine pc_frou_get_control_darray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      double precision ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_control_darray (keyword9,array,nelements)
      end subroutine pc_frou_get_control_darray




      subroutine pc_frou_get_control_carray &
                                (keyword,nsize,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      integer          ,intent(in)     :: nwords            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: array9(nsize)     ! local

      call string_hh2cc          (keyword,keyword9)
      call string_hh2cc_array    (array,array9,nwords,nelements)
      call pc_get_control_carray (keyword9,array9,nelements)
      call string_cc2hh_array    (array9,array,nwords,nelements)
      end subroutine pc_frou_get_control_carray




      subroutine pc_frou_get_control_larray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: array9(nsize)     ! local

      call string_hh2cc          (keyword,keyword9)
      call convert_ii2ll_array   (array,array9,nelements)
      call pc_get_control_larray (keyword9,array9,nelements)
      call convert_ll2ii_array   (array9,array,nelements)
      end subroutine pc_frou_get_control_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_gui_iarray (keyword,action,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local

      call string_hh2cc      (keyword,keyword9)
      call string_hh2cc      (action,action9)
      call pc_get_gui_iarray (keyword9,action9,array,nelements)
      end subroutine pc_frou_get_gui_iarray




      subroutine pc_frou_get_gui_farray (keyword,action,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(in)     :: nsize             ! argument
      real             ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local

      call string_hh2cc      (keyword,keyword9)
      call string_hh2cc      (action,action9)
      call pc_get_gui_farray (keyword9,action9,array,nelements)
      end subroutine pc_frou_get_gui_farray




      subroutine pc_frou_get_gui_darray (keyword,action,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(in)     :: nsize             ! argument
      double precision ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local

      call string_hh2cc      (keyword,keyword9)
      call string_hh2cc      (action,action9)
      call pc_get_gui_darray (keyword9,action9,array,nelements)
      end subroutine pc_frou_get_gui_darray




      subroutine pc_frou_get_gui_carray &
                           (keyword,action,nsize,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      integer          ,intent(in)     :: nwords            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local
      character(len=PC_LENGTH)         :: array9(nsize)     ! local

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call string_hh2cc_array (array,array9,nwords,nelements)
      call pc_get_gui_carray  (keyword9,action9,array9,nelements)
      call string_cc2hh_array (array9,array,nwords,nelements)
      end subroutine pc_frou_get_gui_carray




      subroutine pc_frou_get_gui_larray (keyword,action,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local
      logical                          :: array9(nsize)     ! local

      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc        (action,action9)
      call convert_ii2ll_array (array,array9,nelements)
      call pc_get_gui_larray   (keyword9,action9,array9,nelements)
      call convert_ll2ii_array (array9,array,nelements)
      end subroutine pc_frou_get_gui_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_pdata_iarray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_get_pdata_iarray (keyword9,array,nelements)
      end subroutine pc_frou_get_pdata_iarray




      subroutine pc_frou_get_pdata_farray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      real             ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_get_pdata_farray (keyword9,array,nelements)
      end subroutine pc_frou_get_pdata_farray




      subroutine pc_frou_get_pdata_darray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      double precision ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_get_pdata_darray (keyword9,array,nelements)
      end subroutine pc_frou_get_pdata_darray




      subroutine pc_frou_get_pdata_carray &
                              (keyword,nsize,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      integer          ,intent(in)     :: nwords            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: array9(nsize)     ! local

      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc_array  (array,array9,nwords,nelements)
      call pc_get_pdata_carray (keyword9,array9,nelements)
      call string_cc2hh_array  (array9,array,nwords,nelements)
      end subroutine pc_frou_get_pdata_carray




      subroutine pc_frou_get_pdata_larray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: array9(nsize)     ! local

      call string_hh2cc        (keyword,keyword9)
      call convert_ii2ll_array (array,array9,nelements)
      call pc_get_pdata_larray (keyword9,array9,nelements)
      call convert_ll2ii_array (array9,array,nelements)
      end subroutine pc_frou_get_pdata_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_jdata_iarray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_get_jdata_iarray (keyword9,array,nelements)
      end subroutine pc_frou_get_jdata_iarray




      subroutine pc_frou_get_jdata_farray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      real             ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_get_jdata_farray (keyword9,array,nelements)
      end subroutine pc_frou_get_jdata_farray




      subroutine pc_frou_get_jdata_darray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      double precision ,intent(out)    :: array(nsize)      ! argument
      integer          ,intent(out)    :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_get_jdata_darray (keyword9,array,nelements)
      end subroutine pc_frou_get_jdata_darray




      subroutine pc_frou_get_jdata_carray &
                                (keyword,nsize,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      integer          ,intent(in)     :: nwords            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: array9(nsize)     ! local

      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc_array  (array,array9,nwords,nelements)
      call pc_get_jdata_carray (keyword9,array9,nelements)
      call string_cc2hh_array  (array9,array,nwords,nelements)
      end subroutine pc_frou_get_jdata_carray




      subroutine pc_frou_get_jdata_larray (keyword,nsize,array,nelements)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: nsize             ! argument
      integer          ,intent(inout)  :: array(*)          ! argument
      integer          ,intent(inout)  :: nelements         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: array9(nsize)     ! local

      call string_hh2cc        (keyword,keyword9)
      call convert_ii2ll_array (array,array9,nelements)
      call pc_get_jdata_larray (keyword9,array9,nelements)
      call convert_ll2ii_array (array9,array,nelements)
      end subroutine pc_frou_get_jdata_larray


!!------------------------- get array element -----------------------------!!
!!------------------------- get array element -----------------------------!!
!!------------------------- get array element -----------------------------!!


      subroutine pc_frou_get_process_ielement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_get_process_ielement (keyword9,indx+1,element)
      end subroutine pc_frou_get_process_ielement



      subroutine pc_frou_get_process_felement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      real             ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_get_process_felement (keyword9,indx+1,element)
      end subroutine pc_frou_get_process_felement



      subroutine pc_frou_get_process_delement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      double precision ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_get_process_delement (keyword9,indx+1,element)
      end subroutine pc_frou_get_process_delement



      subroutine pc_frou_get_process_lelement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: element9          ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_get_process_lelement (keyword9,indx+1,element9)
      call convert_ll2ii           (element9,element)
      end subroutine pc_frou_get_process_lelement



      subroutine pc_frou_get_process_celement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element(*)        ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: element9          ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_get_process_celement (keyword9,indx+1,element9)
      call string_cc2hh            (element9,element)
      end subroutine pc_frou_get_process_celement


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_global_ielement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_get_global_ielement (keyword9,indx+1,element)
      end subroutine pc_frou_get_global_ielement



      subroutine pc_frou_get_global_felement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      real             ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_get_global_felement (keyword9,indx+1,element)
      end subroutine pc_frou_get_global_felement



      subroutine pc_frou_get_global_delement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      double precision ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_get_global_delement (keyword9,indx+1,element)
      end subroutine pc_frou_get_global_delement



      subroutine pc_frou_get_global_lelement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: element9          ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_get_global_lelement (keyword9,indx+1,element9)
      call convert_ll2ii          (element9,element)
      end subroutine pc_frou_get_global_lelement



      subroutine pc_frou_get_global_celement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element(*)        ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: element9          ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_get_global_celement (keyword9,indx+1,element9)
      call string_cc2hh           (element9,element)
      end subroutine pc_frou_get_global_celement


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_control_ielement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_get_control_ielement (keyword9,indx+1,element)
      end subroutine pc_frou_get_control_ielement



      subroutine pc_frou_get_control_felement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      real             ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_get_control_felement (keyword9,indx+1,element)
      end subroutine pc_frou_get_control_felement



      subroutine pc_frou_get_control_delement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      double precision ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_get_control_delement (keyword9,indx+1,element)
      end subroutine pc_frou_get_control_delement



      subroutine pc_frou_get_control_lelement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: element9          ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_get_control_lelement (keyword9,indx+1,element9)
      call convert_ll2ii           (element9,element)
      end subroutine pc_frou_get_control_lelement



      subroutine pc_frou_get_control_celement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element(*)        ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: element9          ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_get_control_celement (keyword9,indx+1,element9)
      call string_cc2hh            (element9,element)
      end subroutine pc_frou_get_control_celement


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_gui_ielement (keyword,action,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local

      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc        (action,action9)
      call pc_get_gui_ielement (keyword9,action9,indx+1,element)
      end subroutine pc_frou_get_gui_ielement



      subroutine pc_frou_get_gui_felement (keyword,action,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(in)     :: indx              ! argument
      real             ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local

      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc        (action,action9)
      call pc_get_gui_felement (keyword9,action9,indx+1,element)
      end subroutine pc_frou_get_gui_felement



      subroutine pc_frou_get_gui_delement (keyword,action,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(in)     :: indx              ! argument
      double precision ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local

      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc        (action,action9)
      call pc_get_gui_delement (keyword9,action9,indx+1,element)
      end subroutine pc_frou_get_gui_delement



      subroutine pc_frou_get_gui_lelement (keyword,action,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local
      logical                          :: element9          ! local

      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc        (action,action9)
      call pc_get_gui_lelement (keyword9,action9,indx+1,element9)
      call convert_ll2ii       (element9,element)
      end subroutine pc_frou_get_gui_lelement


      subroutine pc_frou_get_gui_celement (keyword,action,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: action(*)         ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element(*)        ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_KEYWORD_LENGTH) :: action9           ! local
      character(len=PC_LENGTH)         :: element9          ! local

      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc        (action,action9)
      call pc_get_gui_celement (keyword9,action9,indx+1,element9)
      call string_cc2hh        (element9,element)
      end subroutine pc_frou_get_gui_celement


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_pdata_ielement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_pdata_ielement (keyword9,indx+1,element)
      end subroutine pc_frou_get_pdata_ielement



      subroutine pc_frou_get_pdata_felement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      real             ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_pdata_felement (keyword9,indx+1,element)
      end subroutine pc_frou_get_pdata_felement



      subroutine pc_frou_get_pdata_delement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      double precision ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_pdata_delement (keyword9,indx+1,element)
      end subroutine pc_frou_get_pdata_delement



      subroutine pc_frou_get_pdata_lelement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: element9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_pdata_lelement (keyword9,indx+1,element9)
      call convert_ll2ii         (element9,element)
      end subroutine pc_frou_get_pdata_lelement



      subroutine pc_frou_get_pdata_celement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element(*)        ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: element9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_pdata_celement (keyword9,indx+1,element9)
      call string_cc2hh          (element9,element)
      end subroutine pc_frou_get_pdata_celement


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_get_jdata_ielement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_jdata_ielement (keyword9,indx+1,element)
      end subroutine pc_frou_get_jdata_ielement



      subroutine pc_frou_get_jdata_felement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      real             ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_jdata_felement (keyword9,indx+1,element)
      end subroutine pc_frou_get_jdata_felement



      subroutine pc_frou_get_jdata_delement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      double precision ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_jdata_delement (keyword9,indx+1,element)
      end subroutine pc_frou_get_jdata_delement



      subroutine pc_frou_get_jdata_lelement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: element9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_jdata_lelement (keyword9,indx+1,element9)
      call convert_ll2ii         (element9,element)
      end subroutine pc_frou_get_jdata_lelement



      subroutine pc_frou_get_jdata_celement (keyword,indx,element)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element(*)        ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      character(len=PC_LENGTH)         :: element9          ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_get_jdata_celement (keyword9,indx+1,element9)
      call string_cc2hh          (element9,element)
      end subroutine pc_frou_get_jdata_celement


!!---------------------------- pressed ----------------------------------!!
!!---------------------------- pressed ----------------------------------!!
!!---------------------------- pressed ----------------------------------!!


      function pc_frou_pressed    (keyword) result (pressed)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)        ! argument
      integer                          :: pressed           ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local
      logical                          :: pressed9          ! local

      call string_hh2cc     (keyword,keyword9)
      pressed9 = pc_pressed (keyword9)
      call convert_ll2ii    (pressed9,pressed)
      end function pc_frou_pressed


!!-------------------------- activated --------------------------------!!
!!-------------------------- activated --------------------------------!!
!!-------------------------- activated --------------------------------!!


      subroutine pc_frou_activated (keyword)
      use pc_frou_module
      implicit none
      integer          ,intent(out)    :: keyword(*)        ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9          ! local

      keyword9 = pc_activated ()
      call string_cc2hh       (keyword9,keyword)
      end subroutine pc_frou_activated


!!-------------------------- verify -----------------------------------!!
!!-------------------------- verify -----------------------------------!!
!!-------------------------- verify -----------------------------------!!


      function pc_frou_verify_scalar (keyword) result (verify)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)       ! argument
      integer                          :: verify           ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      logical                          :: verify9          ! local

      call string_hh2cc          (keyword,keyword9)
      verify9 = pc_verify_scalar (keyword9)
      call convert_ll2ii         (verify9,verify)
      end function pc_frou_verify_scalar



      function pc_frou_verify_element (keyword,indx,action) result (verify)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)       ! argument
      integer          ,intent(out)    :: indx             ! argument
      integer          ,intent(out)    :: action           ! argument
      integer                          :: verify           ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      logical                          :: verify9          ! local
      integer                          :: indx9            ! local

      call string_hh2cc           (keyword,keyword9)
      verify9 = pc_verify_element (keyword9,indx9,action)
      call convert_ll2ii          (verify9,verify)
      indx = indx9 - 1;
      end function pc_frou_verify_element



      function pc_frou_verify_array (keyword) result (verify)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)       ! argument
      integer                          :: verify           ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      logical                          :: verify9          ! local

      call string_hh2cc         (keyword,keyword9)
      verify9 = pc_verify_array (keyword9)
      call convert_ll2ii        (verify9,verify)
      end function pc_frou_verify_array



      function pc_frou_verify_arrayset (keyword) result (verify)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)       ! argument
      integer                          :: verify           ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      logical                          :: verify9          ! local

      call string_hh2cc            (keyword,keyword9)
      verify9 = pc_verify_arrayset (keyword9)
      call convert_ll2ii           (verify9,verify)
      end function pc_frou_verify_arrayset



      function pc_frou_verify_screen (keyword) result (verify)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)       ! argument
      integer                          :: verify           ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      logical                          :: verify9          ! local

      call string_hh2cc          (keyword,keyword9)
      verify9 = pc_verify_screen (keyword9)
      call convert_ll2ii         (verify9,verify)
      end function pc_frou_verify_screen



      function pc_frou_verify_end () result (verify)
      use pc_frou_module
      implicit none
      integer                          :: verify           ! result
      logical                          :: verify9          ! local

      verify9 = pc_verify_end ()
      call convert_ll2ii      (verify9,verify)
      end function pc_frou_verify_end


!!------------------------- put scalars -----------------------------------!!
!!------------------------- put scalars -----------------------------------!!
!!------------------------- put scalars -----------------------------------!!


      subroutine pc_frou_put_gscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      integer               ,intent(in)    :: nchar,ndec    ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
      call string_hh2cc   (keyword,keyword9)
      call pc_put_gscalar (keyword9,obj,nchar,ndec)
      end subroutine pc_frou_put_gscalar



      subroutine pc_frou_put_iscalar (keyword,scalar,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc   (keyword,keyword9)
      call pc_put_iscalar (keyword9,scalar,nchar)
      end subroutine pc_frou_put_iscalar



      subroutine pc_frou_put_fscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      real                 ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc   (keyword,keyword9)
      call pc_put_fscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_fscalar



      subroutine pc_frou_put_dscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      double precision     ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc   (keyword,keyword9)
      call pc_put_dscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_dscalar



      subroutine pc_frou_put_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar(*)     ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      character(len=PC_LENGTH)         :: scalar9       ! local

      call string_hh2cc   (keyword,keyword9)
      call string_hh2cc   (scalar,scalar9)
      call pc_put_cscalar (keyword9,scalar9)
      end subroutine pc_frou_put_cscalar



      subroutine pc_frou_put_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)       ! argument
      integer          ,intent(in)     :: scalar           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      logical                          :: scalar9          ! local

      call string_hh2cc   (keyword,keyword9)
      call convert_ii2ll  (scalar,scalar9)
      call pc_put_lscalar (keyword9,scalar9)
      end subroutine pc_frou_put_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_process_gscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      integer               ,intent(in)    :: nchar,ndec    ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
      call string_hh2cc           (keyword,keyword9)
      call pc_put_process_gscalar (keyword9,obj,nchar,ndec)
      end subroutine pc_frou_put_process_gscalar



      subroutine pc_frou_put_process_iscalar (keyword,scalar,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_put_process_iscalar (keyword9,scalar,nchar)
      end subroutine pc_frou_put_process_iscalar



      subroutine pc_frou_put_process_fscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      real                 ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_put_process_fscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_process_fscalar



      subroutine pc_frou_put_process_dscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      double precision     ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_put_process_dscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_process_dscalar



      subroutine pc_frou_put_process_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar(*)     ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      character(len=PC_LENGTH)         :: scalar9       ! local

      call string_hh2cc           (keyword,keyword9)
      call string_hh2cc           (scalar,scalar9)
      call pc_put_process_cscalar (keyword9,scalar9)
      end subroutine pc_frou_put_process_cscalar



      subroutine pc_frou_put_process_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)       ! argument
      integer          ,intent(in)     :: scalar           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      logical                          :: scalar9          ! local

      call string_hh2cc           (keyword,keyword9)
      call convert_ii2ll          (scalar,scalar9)
      call pc_put_process_lscalar (keyword9,scalar9)
      end subroutine pc_frou_put_process_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_global_gscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      integer               ,intent(in)    :: nchar,ndec    ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
      call string_hh2cc          (keyword,keyword9)
!print *, 'pc_frou.f90 put: xorigin = ',grid_get_xorigin(obj)
!print *, 'pc_frou.f90 put: yorigin = ',grid_get_yorigin(obj)
!print *, 'pc_frou.f90 put: xwidth = ',grid_get_xgrid_width(obj)
!print *, 'pc_frou.f90 put: ywidth = ',grid_get_ygrid_width(obj)
!print *, 'pc_frou.f90 put: angle = ',grid_get_rotation_angle(obj)
!print *, 'pc_frou.f90 put: handedness = ',grid_get_handedness(obj)
      call pc_put_global_gscalar (keyword9,obj,nchar,ndec)
      end subroutine pc_frou_put_global_gscalar



      subroutine pc_frou_put_global_iscalar (keyword,scalar,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_put_global_iscalar (keyword9,scalar,nchar)
      end subroutine pc_frou_put_global_iscalar



      subroutine pc_frou_put_global_fscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      real                 ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_put_global_fscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_global_fscalar



      subroutine pc_frou_put_global_dscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      double precision     ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_put_global_dscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_global_dscalar



      subroutine pc_frou_put_global_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar(*)     ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      character(len=PC_LENGTH)         :: scalar9       ! local

      call string_hh2cc          (keyword,keyword9)
      call string_hh2cc          (scalar,scalar9)
      call pc_put_global_cscalar (keyword9,scalar9)
      end subroutine pc_frou_put_global_cscalar



      subroutine pc_frou_put_global_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)       ! argument
      integer          ,intent(in)     :: scalar           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      logical                          :: scalar9          ! local

      call string_hh2cc          (keyword,keyword9)
      call convert_ii2ll         (scalar,scalar9)
      call pc_put_global_lscalar (keyword9,scalar9)
      end subroutine pc_frou_put_global_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_control_gscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      integer               ,intent(in)    :: nchar,ndec    ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
      call string_hh2cc           (keyword,keyword9)
      call pc_put_control_gscalar (keyword9,obj,nchar,ndec)
      end subroutine pc_frou_put_control_gscalar



      subroutine pc_frou_put_control_iscalar (keyword,scalar,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_put_control_iscalar (keyword9,scalar,nchar)
      end subroutine pc_frou_put_control_iscalar



      subroutine pc_frou_put_control_fscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      real                 ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_put_control_fscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_control_fscalar



      subroutine pc_frou_put_control_dscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      double precision     ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_put_control_dscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_control_dscalar



      subroutine pc_frou_put_control_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar(*)     ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      character(len=PC_LENGTH)         :: scalar9       ! local

      call string_hh2cc           (keyword,keyword9)
      call string_hh2cc           (scalar,scalar9)
      call pc_put_control_cscalar (keyword9,scalar9)
      end subroutine pc_frou_put_control_cscalar



      subroutine pc_frou_put_control_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)       ! argument
      integer          ,intent(in)     :: scalar           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      logical                          :: scalar9          ! local

      call string_hh2cc           (keyword,keyword9)
      call convert_ii2ll          (scalar,scalar9)
      call pc_put_control_lscalar (keyword9,scalar9)
      end subroutine pc_frou_put_control_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_gui_gscalar (keyword,action,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      integer               ,intent(in)    :: action(*)     ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      integer               ,intent(in)    :: nchar,ndec    ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local
      character(len=PC_KEYWORD_LENGTH)     :: action9       ! local

      obj => scalar%obj
      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call pc_put_gui_gscalar (keyword9,action9,obj,nchar,ndec)
      end subroutine pc_frou_put_gui_gscalar



      subroutine pc_frou_put_gui_iscalar (keyword,action,scalar,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: action(*)     ! argument
      integer              ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      character(len=PC_KEYWORD_LENGTH) :: action9       ! local

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call pc_put_gui_iscalar (keyword9,action9,scalar,nchar)
      end subroutine pc_frou_put_gui_iscalar



      subroutine pc_frou_put_gui_fscalar (keyword,action,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: action(*)     ! argument
      real                 ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      character(len=PC_KEYWORD_LENGTH) :: action9       ! local

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call pc_put_gui_fscalar (keyword9,action9,scalar,nchar,ndec)
      end subroutine pc_frou_put_gui_fscalar



      subroutine pc_frou_put_gui_dscalar (keyword,action,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: action(*)     ! argument
      double precision     ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      character(len=PC_KEYWORD_LENGTH) :: action9       ! local

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call pc_put_gui_dscalar (keyword9,action9,scalar,nchar,ndec)
      end subroutine pc_frou_put_gui_dscalar



      subroutine pc_frou_put_gui_cscalar (keyword,action,scalar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: action(*)     ! argument
      integer              ,intent(in) :: scalar(*)     ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      character(len=PC_KEYWORD_LENGTH) :: action9       ! local
      character(len=PC_LENGTH)         :: scalar9       ! local

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call string_hh2cc       (scalar,scalar9)
      call pc_put_gui_cscalar (keyword9,action9,scalar9)
      end subroutine pc_frou_put_gui_cscalar



      subroutine pc_frou_put_gui_lscalar (keyword,action,scalar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: action(*)     ! argument
      integer              ,intent(in) :: scalar        ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      character(len=PC_KEYWORD_LENGTH) :: action9       ! local
      logical                          :: scalar9       ! local

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call convert_ii2ll      (scalar,scalar9)
      call pc_put_gui_lscalar (keyword9,action9,scalar9)
      end subroutine pc_frou_put_gui_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_gui_only_gscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      integer               ,intent(in)    :: nchar,ndec    ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
      call string_hh2cc            (keyword,keyword9)
      call pc_put_gui_only_gscalar (keyword9,obj,nchar,ndec)
      end subroutine pc_frou_put_gui_only_gscalar



      subroutine pc_frou_put_gui_only_iscalar (keyword,scalar,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_put_gui_only_iscalar (keyword9,scalar,nchar)
      end subroutine pc_frou_put_gui_only_iscalar



      subroutine pc_frou_put_gui_only_fscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      real                 ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_put_gui_only_fscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_gui_only_fscalar



      subroutine pc_frou_put_gui_only_dscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      double precision     ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_put_gui_only_dscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_gui_only_dscalar



      subroutine pc_frou_put_gui_only_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar(*)     ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      character(len=PC_LENGTH)         :: scalar9       ! local

      call string_hh2cc            (keyword,keyword9)
      call string_hh2cc            (scalar,scalar9)
      call pc_put_gui_only_cscalar (keyword9,scalar9)
      end subroutine pc_frou_put_gui_only_cscalar



      subroutine pc_frou_put_gui_only_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar        ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      logical                          :: scalar9       ! local

      call string_hh2cc            (keyword,keyword9)
      call convert_ii2ll           (scalar,scalar9)
      call pc_put_gui_only_lscalar (keyword9,scalar9)
      end subroutine pc_frou_put_gui_only_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_pdata_gscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      integer               ,intent(in)    :: nchar,ndec    ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
      call string_hh2cc         (keyword,keyword9)
      call pc_put_pdata_gscalar (keyword9,obj,nchar,ndec)
      end subroutine pc_frou_put_pdata_gscalar



      subroutine pc_frou_put_pdata_iscalar (keyword,scalar,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_put_pdata_iscalar (keyword9,scalar,nchar)
      end subroutine pc_frou_put_pdata_iscalar



      subroutine pc_frou_put_pdata_fscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      real                 ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_put_pdata_fscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_pdata_fscalar



      subroutine pc_frou_put_pdata_dscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      double precision     ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_put_pdata_dscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_pdata_dscalar



      subroutine pc_frou_put_pdata_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar(*)     ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      character(len=PC_LENGTH)         :: scalar9       ! local

      call string_hh2cc         (keyword,keyword9)
      call string_hh2cc         (scalar,scalar9)
      call pc_put_pdata_cscalar (keyword9,scalar9)
      end subroutine pc_frou_put_pdata_cscalar



      subroutine pc_frou_put_pdata_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)       ! argument
      integer          ,intent(in)     :: scalar           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      logical                          :: scalar9          ! local

      call string_hh2cc         (keyword,keyword9)
      call convert_ii2ll        (scalar,scalar9)
      call pc_put_pdata_lscalar (keyword9,scalar9)
      end subroutine pc_frou_put_pdata_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_jdata_gscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer               ,intent(in)    :: keyword(*)    ! argument
      type(grid_frou_struct),intent(inout) :: scalar        ! argument
      integer               ,intent(in)    :: nchar,ndec    ! argument
      type(grid_struct)     ,pointer       :: obj           ! local
      character(len=PC_KEYWORD_LENGTH)     :: keyword9      ! local

      obj => scalar%obj
      call string_hh2cc         (keyword,keyword9)
      call pc_put_jdata_gscalar (keyword9,obj,nchar,ndec)
      end subroutine pc_frou_put_jdata_gscalar



      subroutine pc_frou_put_jdata_iscalar (keyword,scalar,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar         ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_put_jdata_iscalar (keyword9,scalar,nchar)
      end subroutine pc_frou_put_jdata_iscalar



      subroutine pc_frou_put_jdata_fscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      real                 ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_put_jdata_fscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_jdata_fscalar



      subroutine pc_frou_put_jdata_dscalar (keyword,scalar,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      double precision     ,intent(in) :: scalar        ! argument
      integer              ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_put_jdata_dscalar (keyword9,scalar,nchar,ndec)
      end subroutine pc_frou_put_jdata_dscalar



      subroutine pc_frou_put_jdata_cscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)    ! argument
      integer              ,intent(in) :: scalar(*)     ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9      ! local
      character(len=PC_LENGTH)         :: scalar9       ! local

      call string_hh2cc         (keyword,keyword9)
      call string_hh2cc         (scalar,scalar9)
      call pc_put_jdata_cscalar (keyword9,scalar9)
      end subroutine pc_frou_put_jdata_cscalar



      subroutine pc_frou_put_jdata_lscalar (keyword,scalar)
      use pc_frou_module
      implicit none
      integer          ,intent(in)     :: keyword(*)       ! argument
      integer          ,intent(in)     :: scalar           ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9         ! local
      logical                          :: scalar9          ! local

      call string_hh2cc         (keyword,keyword9)
      call convert_ii2ll        (scalar,scalar9)
      call pc_put_jdata_lscalar (keyword9,scalar9)
      end subroutine pc_frou_put_jdata_lscalar


!!------------------------- put arrays ---------------------------------!!
!!------------------------- put arrays ---------------------------------!!
!!------------------------- put arrays ---------------------------------!!


      subroutine pc_frou_put_iarray (keyword,array,nelements,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar                 ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc  (keyword,keyword9)
      call pc_put_iarray (keyword9,array,nelements,nchar)
      end subroutine pc_frou_put_iarray


      subroutine pc_frou_put_farray (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      real                 ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc  (keyword,keyword9)
      call pc_put_farray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_farray


      subroutine pc_frou_put_darray (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      double precision     ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc  (keyword,keyword9)
      call pc_put_darray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_darray


      subroutine pc_frou_put_carray (keyword,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: array(*)              ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: nwords                ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_LENGTH)         :: array9(nelements)     ! local
      integer :: i

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc_array (array,array9,nwords,nelements)
      call pc_put_carray      (keyword9,array9,nelements)
      end subroutine pc_frou_put_carray


      subroutine pc_frou_put_larray (keyword,array,nelements)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: array9(nelements)     ! local

      call string_hh2cc        (keyword,keyword9)
      call convert_ii2ll_array (array,array9,nelements)
      call pc_put_larray       (keyword9,array9,nelements)
      end subroutine pc_frou_put_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_process_iarray (keyword,array,nelements,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar                 ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_put_process_iarray (keyword9,array,nelements,nchar)
      end subroutine pc_frou_put_process_iarray


      subroutine pc_frou_put_process_farray (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      real                 ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_put_process_farray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_process_farray


      subroutine pc_frou_put_process_darray (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      double precision     ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_put_process_darray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_process_darray


      subroutine pc_frou_put_process_carray (keyword,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: array(*)              ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: nwords                ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_LENGTH)         :: array9(nelements)     ! local

      call string_hh2cc          (keyword,keyword9)
      call string_hh2cc_array    (array,array9,nwords,nelements)
      call pc_put_process_carray (keyword9,array9,nelements)
      end subroutine pc_frou_put_process_carray


      subroutine pc_frou_put_process_larray (keyword,array,nelements)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: array9(nelements)     ! local

      call string_hh2cc          (keyword,keyword9)
      call convert_ii2ll_array   (array,array9,nelements)
      call pc_put_process_larray (keyword9,array9,nelements)
      end subroutine pc_frou_put_process_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_global_iarray (keyword,array,nelements,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar                 ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_put_global_iarray (keyword9,array,nelements,nchar)
      end subroutine pc_frou_put_global_iarray


      subroutine pc_frou_put_global_farray (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      real                 ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_put_global_farray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_global_farray


      subroutine pc_frou_put_global_darray (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      double precision     ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_put_global_darray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_global_darray


      subroutine pc_frou_put_global_carray (keyword,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: array(*)              ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: nwords                ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_LENGTH)         :: array9(nelements)     ! local

      call string_hh2cc         (keyword,keyword9)
      call string_hh2cc_array   (array,array9,nwords,nelements)
      call pc_put_global_carray (keyword9,array9,nelements)
      end subroutine pc_frou_put_global_carray


      subroutine pc_frou_put_global_larray (keyword,array,nelements)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: array9(nelements)     ! local

      call string_hh2cc         (keyword,keyword9)
      call convert_ii2ll_array  (array,array9,nelements)
      call pc_put_global_larray (keyword9,array9,nelements)
      end subroutine pc_frou_put_global_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_control_iarray (keyword,array,nelements,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar                 ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_put_control_iarray (keyword9,array,nelements,nchar)
      end subroutine pc_frou_put_control_iarray


      subroutine pc_frou_put_control_farray (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      real                 ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_put_control_farray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_control_farray


      subroutine pc_frou_put_control_darray (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      double precision     ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_put_control_darray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_control_darray


      subroutine pc_frou_put_control_carray (keyword,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: array(*)              ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: nwords                ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_LENGTH)         :: array9(nelements)     ! local

      call string_hh2cc          (keyword,keyword9)
      call string_hh2cc_array    (array,array9,nwords,nelements)
      call pc_put_control_carray (keyword9,array9,nelements)
      end subroutine pc_frou_put_control_carray


      subroutine pc_frou_put_control_larray (keyword,array,nelements)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: array9(nelements)     ! local

      call string_hh2cc          (keyword,keyword9)
      call convert_ii2ll_array   (array,array9,nelements)
      call pc_put_control_larray (keyword9,array9,nelements)
      end subroutine pc_frou_put_control_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_gui_iarray (keyword,action,array,nelements,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: action(*)             ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar                 ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_KEYWORD_LENGTH) :: action9               ! local

      call string_hh2cc      (keyword,keyword9)
      call string_hh2cc      (action,action9)
      call pc_put_gui_iarray (keyword9,action9,array,nelements,nchar)
      end subroutine pc_frou_put_gui_iarray


      subroutine pc_frou_put_gui_farray &
                                (keyword,action,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: action(*)             ! argument
      integer              ,intent(in) :: nelements             ! argument
      real                 ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_KEYWORD_LENGTH) :: action9               ! local

      call string_hh2cc      (keyword,keyword9)
      call string_hh2cc      (action,action9)
      call pc_put_gui_farray (keyword9,action9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_gui_farray


      subroutine pc_frou_put_gui_darray &
                                (keyword,action,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: action(*)             ! argument
      integer              ,intent(in) :: nelements             ! argument
      double precision     ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_KEYWORD_LENGTH) :: action9               ! local

      call string_hh2cc      (keyword,keyword9)
      call string_hh2cc      (action,action9)
      call pc_put_gui_darray (keyword9,action9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_gui_darray


      subroutine pc_frou_put_gui_carray (keyword,action,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: action(*)             ! argument
      integer              ,intent(in) :: array(*)              ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: nwords                ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_KEYWORD_LENGTH) :: action9               ! local
      character(len=PC_LENGTH)         :: array9(nelements)     ! local

      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc       (action,action9)
      call string_hh2cc_array (array,array9,nwords,nelements)
      call pc_put_gui_carray  (keyword9,action9,array9,nelements)
      end subroutine pc_frou_put_gui_carray


      subroutine pc_frou_put_gui_larray (keyword,action,array,nelements)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: action(*)             ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_KEYWORD_LENGTH) :: action9               ! local
      logical                          :: array9(nelements)     ! local

      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc        (action,action9)
      call convert_ii2ll_array (array,array9,nelements)
      call pc_put_gui_larray   (keyword9,action9,array9,nelements)
      end subroutine pc_frou_put_gui_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_gui_only_iarray (keyword,array,nelements,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar                 ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_put_gui_only_iarray (keyword9,array,nelements,nchar)
      end subroutine pc_frou_put_gui_only_iarray


      subroutine pc_frou_put_gui_only_farray &
                                    (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      real                 ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_put_gui_only_farray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_gui_only_farray


      subroutine pc_frou_put_gui_only_darray &
                                    (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      double precision     ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_put_gui_only_darray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_gui_only_darray


      subroutine pc_frou_put_gui_only_carray (keyword,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: array(*)              ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: nwords                ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_LENGTH)         :: array9(nelements)     ! local

      call string_hh2cc           (keyword,keyword9)
      call string_hh2cc_array     (array,array9,nwords,nelements)
      call pc_put_gui_only_carray (keyword9,array9,nelements)
      end subroutine pc_frou_put_gui_only_carray


      subroutine pc_frou_put_gui_only_larray (keyword,array,nelements)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: array9(nelements)     ! local

      call string_hh2cc           (keyword,keyword9)
      call convert_ii2ll_array    (array,array9,nelements)
      call pc_put_gui_only_larray (keyword9,array9,nelements)
      end subroutine pc_frou_put_gui_only_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_pdata_iarray (keyword,array,nelements,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar                 ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_put_pdata_iarray (keyword9,array,nelements,nchar)
      end subroutine pc_frou_put_pdata_iarray


      subroutine pc_frou_put_pdata_farray (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      real                 ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_put_pdata_farray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_pdata_farray


      subroutine pc_frou_put_pdata_darray (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      double precision     ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_put_pdata_darray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_pdata_darray


      subroutine pc_frou_put_pdata_carray (keyword,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: array(*)              ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: nwords                ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_LENGTH)         :: array9(nelements)     ! local

      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc_array  (array,array9,nwords,nelements)
      call pc_put_pdata_carray (keyword9,array9,nelements)
      end subroutine pc_frou_put_pdata_carray


      subroutine pc_frou_put_pdata_larray (keyword,array,nelements)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: array9(nelements)     ! local

      call string_hh2cc        (keyword,keyword9)
      call convert_ii2ll_array (array,array9,nelements)
      call pc_put_pdata_larray (keyword9,array9,nelements)
      end subroutine pc_frou_put_pdata_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_jdata_iarray (keyword,array,nelements,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar                 ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_put_jdata_iarray (keyword9,array,nelements,nchar)
      end subroutine pc_frou_put_jdata_iarray


      subroutine pc_frou_put_jdata_farray (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      real                 ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_put_jdata_farray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_jdata_farray


      subroutine pc_frou_put_jdata_darray (keyword,array,nelements,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      double precision     ,intent(in) :: array(nelements)      ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc        (keyword,keyword9)
      call pc_put_jdata_darray (keyword9,array,nelements,nchar,ndec)
      end subroutine pc_frou_put_jdata_darray


      subroutine pc_frou_put_jdata_carray (keyword,array,nelements,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: array(*)              ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: nwords                ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_LENGTH)         :: array9(nelements)     ! local

      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc_array  (array,array9,nwords,nelements)
      call pc_put_jdata_carray (keyword9,array9,nelements)
      end subroutine pc_frou_put_jdata_carray


      subroutine pc_frou_put_jdata_larray (keyword,array,nelements)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: nelements             ! argument
      integer              ,intent(in) :: array(nelements)      ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: array9(nelements)     ! local

      call string_hh2cc        (keyword,keyword9)
      call convert_ii2ll_array (array,array9,nelements)
      call pc_put_jdata_larray (keyword9,array9,nelements)
      end subroutine pc_frou_put_jdata_larray


!!----------------------- put GUI information -----------------------------!!
!!----------------------- put GUI information -----------------------------!!
!!----------------------- put GUI information -----------------------------!!


      subroutine pc_frou_register_array_names (keyword,arrays,narrays,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: arrays(*)             ! argument
      integer              ,intent(in) :: narrays               ! argument
      integer              ,intent(in) :: nwords                ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_LENGTH)         :: arrays9(narrays)      ! local

      call string_hh2cc            (keyword,keyword9)
      call string_hh2cc_array      (arrays,arrays9,nwords,narrays)
      call pc_register_array_names (keyword9,arrays9,narrays)
      end subroutine pc_frou_register_array_names


                        !!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_options_iscalar (keyword,options,noptions,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: noptions              ! argument
      integer              ,intent(in) :: options(noptions)     ! argument
      integer              ,intent(in) :: nchar                 ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_put_options_iscalar (keyword9,options,noptions,nchar)
      end subroutine pc_frou_put_options_iscalar


      subroutine pc_frou_put_options_fscalar &
                                (keyword,options,noptions,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: noptions              ! argument
      real                 ,intent(in) :: options(noptions)     ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_put_options_fscalar (keyword9,options,noptions,nchar,ndec)
      end subroutine pc_frou_put_options_fscalar


      subroutine pc_frou_put_options_dscalar &
                                (keyword,options,noptions,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: noptions              ! argument
      double precision     ,intent(in) :: options(noptions)     ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc           (keyword,keyword9)
      call pc_put_options_dscalar (keyword9,options,noptions,nchar,ndec)
      end subroutine pc_frou_put_options_dscalar


      subroutine pc_frou_put_options_cscalar (keyword,options,noptions,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: options(*)            ! argument
      integer              ,intent(in) :: noptions              ! argument
      integer              ,intent(in) :: nwords                ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_LENGTH)         :: options9(noptions)    ! local

      call string_hh2cc           (keyword,keyword9)
      call string_hh2cc_array     (options,options9,nwords,noptions)
      call pc_put_options_cscalar (keyword9,options9,noptions)
      end subroutine pc_frou_put_options_cscalar


      subroutine pc_frou_put_options_lscalar (keyword,options,noptions)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: options(*)            ! argument
      integer              ,intent(in) :: noptions              ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: options9(noptions)    ! local

      call string_hh2cc           (keyword,keyword9)
      call convert_ii2ll_array    (options,options9,noptions)
      call pc_put_options_lscalar (keyword9,options9,noptions)
      end subroutine pc_frou_put_options_lscalar

                        !!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine pc_frou_put_options_iarray (keyword,options,noptions,nchar)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: noptions              ! argument
      integer              ,intent(in) :: options(noptions)     ! argument
      integer              ,intent(in) :: nchar                 ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_put_options_iarray (keyword9,options,noptions,nchar)
      end subroutine pc_frou_put_options_iarray


      subroutine pc_frou_put_options_farray &
                                (keyword,options,noptions,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: noptions              ! argument
      real                 ,intent(in) :: options(noptions)     ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_put_options_farray (keyword9,options,noptions,nchar,ndec)
      end subroutine pc_frou_put_options_farray


      subroutine pc_frou_put_options_darray &
                                (keyword,options,noptions,nchar,ndec)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: noptions              ! argument
      double precision     ,intent(in) :: options(noptions)     ! argument
      integer              ,intent(in) :: nchar,ndec            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc          (keyword,keyword9)
      call pc_put_options_darray (keyword9,options,noptions,nchar,ndec)
      end subroutine pc_frou_put_options_darray


      subroutine pc_frou_put_options_carray (keyword,options,noptions,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: options(*)            ! argument
      integer              ,intent(in) :: noptions              ! argument
      integer              ,intent(in) :: nwords                ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_LENGTH)         :: options9(noptions)    ! local

      call string_hh2cc          (keyword,keyword9)
      call string_hh2cc_array    (options,options9,nwords,noptions)
      call pc_put_options_carray (keyword9,options9,noptions)
      end subroutine pc_frou_put_options_carray


      subroutine pc_frou_put_options_larray (keyword,options,noptions)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: options(*)            ! argument
      integer              ,intent(in) :: noptions              ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: options9(noptions)    ! local

      call string_hh2cc          (keyword,keyword9)
      call convert_ii2ll_array   (options,options9,noptions)
      call pc_put_options_larray (keyword9,options9,noptions)
      end subroutine pc_frou_put_options_larray


                        !!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_frou_put_sns_field_flag (keyword,sensitive)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: sensitive             ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: sensitive9            ! local

      call string_hh2cc                (keyword,keyword9)
      call convert_ii2ll               (sensitive,sensitive9)
      call pc_put_sensitive_field_flag (keyword9,sensitive9)
      end subroutine pc_frou_put_sns_field_flag


      subroutine pc_frou_put_sns_array_flag (keyword,sensitive)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: sensitive             ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: sensitive9            ! local

      call string_hh2cc                (keyword,keyword9)
      call convert_ii2ll               (sensitive,sensitive9)
      call pc_put_sensitive_array_flag (keyword9,sensitive9)
      end subroutine pc_frou_put_sns_array_flag


      subroutine pc_frou_put_sns_arrayset_flag (keyword,sensitive)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: sensitive             ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: sensitive9            ! local

      call string_hh2cc                   (keyword,keyword9)
      call convert_ii2ll                  (sensitive,sensitive9)
      call pc_put_sensitive_arrayset_flag (keyword9,sensitive9)
      end subroutine pc_frou_put_sns_arrayset_flag


      subroutine pc_frou_put_sns_screen_flag (keyword,sensitive)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: sensitive             ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: sensitive9            ! local

      call string_hh2cc                 (keyword,keyword9)
      call convert_ii2ll                (sensitive,sensitive9)
      call pc_put_sensitive_screen_flag (keyword9,sensitive9)
      end subroutine pc_frou_put_sns_screen_flag

                        !!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine pc_frou_put_visible_flag (keyword,visible)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: visible               ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: visible9              ! local

      call string_hh2cc        (keyword,keyword9)
      call convert_ii2ll       (visible,visible9)
      call pc_put_visible_flag (keyword9,visible9)
      end subroutine pc_frou_put_visible_flag

                        !!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine pc_frou_put_minsize_array (keyword,minsize)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: keyword(*)        ! argument
      integer              ,intent(in)  :: minsize           ! argument
      character(len=PC_KEYWORD_LENGTH)  :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_put_minsize_array (keyword9,minsize)
      end subroutine pc_frou_put_minsize_array


      subroutine pc_frou_put_minsize_arrayset (keyword,minsize)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: keyword(*)        ! argument
      integer              ,intent(in)  :: minsize           ! argument
      character(len=PC_KEYWORD_LENGTH)  :: keyword9          ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_put_minsize_arrayset (keyword9,minsize)
      end subroutine pc_frou_put_minsize_arrayset


      subroutine pc_frou_put_maxsize_array (keyword,maxsize)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: keyword(*)        ! argument
      integer              ,intent(in)  :: maxsize           ! argument
      character(len=PC_KEYWORD_LENGTH)  :: keyword9          ! local

      call string_hh2cc         (keyword,keyword9)
      call pc_put_maxsize_array (keyword9,maxsize)
      end subroutine pc_frou_put_maxsize_array


      subroutine pc_frou_put_maxsize_arrayset (keyword,maxsize)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: keyword(*)        ! argument
      integer              ,intent(in)  :: maxsize           ! argument
      character(len=PC_KEYWORD_LENGTH)  :: keyword9          ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_put_maxsize_arrayset (keyword9,maxsize)
      end subroutine pc_frou_put_maxsize_arrayset


!!------------------------ get and put data cards -------------------------!!
!!------------------------ get and put data cards -------------------------!!
!!------------------------ get and put data cards -------------------------!!


      function pc_frou_num_process_cards () result (ncards)
      use pc_frou_module
      implicit none
      integer                      :: ncards         ! result

      ncards = pc_num_process_cards()
      end function pc_frou_num_process_cards



      subroutine pc_frou_get_process_cards (nsize,cards,ncards,errmsg,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: nsize             ! argument
      integer              ,intent(out) :: cards(*)          ! argument
      integer              ,intent(out) :: ncards            ! argument
      integer              ,intent(out) :: errmsg(*)         ! argument
      integer              ,intent(in)  :: nwords            ! argument
      character(len=PC_DATACARD_LENGTH) :: cards9(nsize)     ! local
      character(len=PC_MESSAGE_LENGTH)  :: errmsg9           ! local

      call pc_get_process_cards (cards9,ncards,errmsg9)
      call string_cc2hh_array   (cards9,cards,nwords,ncards)
      call string_cc2hh         (errmsg9,errmsg)
      end subroutine pc_frou_get_process_cards


      subroutine pc_frou_get_process_card (icard,card,errmsg)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: icard          ! argument
      integer              ,intent(out) :: card(*)        ! argument
      integer              ,intent(out) :: errmsg(*)      ! argument
      character(len=PC_DATACARD_LENGTH) :: card9          ! local
      character(len=PC_MESSAGE_LENGTH)  :: errmsg9        ! local

      call pc_get_process_card (icard,card9,errmsg9)
      call string_cc2hh        (card9,card)
      call string_cc2hh        (errmsg9,errmsg)
      end subroutine pc_frou_get_process_card


      subroutine pc_frou_put_process_cards (cards,ncards,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: cards(*)           ! argument
      integer              ,intent(in)  :: ncards             ! argument
      integer              ,intent(in)  :: nwords             ! argument
      character(len=PC_DATACARD_LENGTH) :: cards9(ncards)     ! local

      call string_hh2cc_array   (cards,cards9,nwords,ncards)
      call pc_put_process_cards (cards9,ncards)
      end subroutine pc_frou_put_process_cards


      subroutine pc_frou_put_process_card (card)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: card(*)            ! argument
      character(len=PC_DATACARD_LENGTH) :: card9              ! local

      call string_hh2cc        (card,card9)
      call pc_put_process_card (card9)
      end subroutine pc_frou_put_process_card


      subroutine pc_frou_add_process_card (card)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: card(*)            ! argument
      character(len=PC_DATACARD_LENGTH) :: card9              ! local

      call string_hh2cc        (card,card9)
      call pc_add_process_card (card9)
      end subroutine pc_frou_add_process_card


      subroutine pc_frou_clear_process_cards
      use pc_frou_module
      implicit none

      call pc_clear_process_cards
      end subroutine pc_frou_clear_process_cards


                         !!!!!!!!!!!!!!!!!!!!!!!


      function pc_frou_num_global_cards () result (ncards)
      use pc_frou_module
      implicit none
      integer                      :: ncards         ! result

      ncards = pc_num_global_cards()
      end function pc_frou_num_global_cards


      subroutine pc_frou_get_global_cards (nsize,cards,ncards,errmsg,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: nsize             ! argument
      integer              ,intent(out) :: cards(*)          ! argument
      integer              ,intent(out) :: ncards            ! argument
      integer              ,intent(out) :: errmsg(*)         ! argument
      integer              ,intent(in)  :: nwords            ! argument
      character(len=PC_DATACARD_LENGTH) :: cards9(nsize)     ! local
      character(len=PC_MESSAGE_LENGTH)  :: errmsg9           ! local

      call pc_get_global_cards (cards9,ncards,errmsg9)
      call string_cc2hh_array  (cards9,cards,nwords,ncards)
      call string_cc2hh        (errmsg9,errmsg)
      end subroutine pc_frou_get_global_cards


      subroutine pc_frou_get_global_card (icard,card,errmsg)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: icard          ! argument
      integer              ,intent(out) :: card(*)        ! argument
      integer              ,intent(out) :: errmsg(*)      ! argument
      character(len=PC_DATACARD_LENGTH) :: card9          ! local
      character(len=PC_MESSAGE_LENGTH)  :: errmsg9        ! local

      call pc_get_global_card (icard,card9,errmsg9)
      call string_cc2hh       (card9,card)
      call string_cc2hh       (errmsg9,errmsg)
      end subroutine pc_frou_get_global_card


      subroutine pc_frou_put_global_cards (cards,ncards,nwords)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: cards(*)           ! argument
      integer               ,intent(in) :: ncards             ! argument
      integer               ,intent(in) :: nwords             ! argument
      character(len=PC_DATACARD_LENGTH) :: cards9(ncards)     ! local

      call string_hh2cc_array  (cards,cards9,nwords,ncards)
      call pc_put_global_cards (cards9,ncards)
      end subroutine pc_frou_put_global_cards


      subroutine pc_frou_put_global_card (card)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: card(*)            ! argument
      character(len=PC_DATACARD_LENGTH) :: card9              ! local

      call string_hh2cc       (card,card9)
      call pc_put_global_card (card9)
      end subroutine pc_frou_put_global_card


      subroutine pc_frou_add_global_card (card)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: card(*)            ! argument
      character(len=PC_DATACARD_LENGTH) :: card9              ! local

      call string_hh2cc       (card,card9)
      call pc_add_global_card (card9)
      end subroutine pc_frou_add_global_card


      subroutine pc_frou_clear_global_cards
      use pc_frou_module
      implicit none

      call pc_clear_global_cards
      end subroutine pc_frou_clear_global_cards


                         !!!!!!!!!!!!!!!!!!!!!!!


      function pc_frou_num_control_cards () result (ncards)
      use pc_frou_module
      implicit none
      integer                      :: ncards         ! result

      ncards = pc_num_control_cards()
      end function pc_frou_num_control_cards



      subroutine pc_frou_get_control_cards (nsize,cards,ncards,errmsg,nwords)
      use pc_frou_module
      implicit none
      integer               ,intent(in)  :: nsize             ! argument
      integer               ,intent(out) :: cards(*)          ! argument
      integer               ,intent(out) :: ncards            ! argument
      integer               ,intent(out) :: errmsg(*)         ! argument
      integer               ,intent(in)  :: nwords            ! argument
      character(len=PC_DATACARD_LENGTH)  :: cards9(nsize)     ! local
      character(len=PC_MESSAGE_LENGTH)   :: errmsg9           ! local

      call pc_get_control_cards (cards9,ncards,errmsg9)
      call string_cc2hh_array   (cards9,cards,nwords,ncards)
      call string_cc2hh         (errmsg9,errmsg)
      end subroutine pc_frou_get_control_cards


      subroutine pc_frou_get_control_card (icard,card,errmsg)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: icard          ! argument
      integer              ,intent(out) :: card(*)        ! argument
      integer              ,intent(out) :: errmsg(*)      ! argument
      character(len=PC_DATACARD_LENGTH) :: card9          ! local
      character(len=PC_MESSAGE_LENGTH)  :: errmsg9        ! local

      call pc_get_control_card (icard,card9,errmsg9)
      call string_cc2hh        (card9,card)
      call string_cc2hh        (errmsg9,errmsg)
      end subroutine pc_frou_get_control_card


      subroutine pc_frou_put_control_cards (cards,ncards,nwords)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: cards(*)           ! argument
      integer               ,intent(in) :: ncards             ! argument
      integer               ,intent(in) :: nwords             ! argument
      character(len=PC_DATACARD_LENGTH) :: cards9(ncards)     ! local

      call string_hh2cc_array   (cards,cards9,nwords,ncards)
      call pc_put_control_cards (cards9,ncards)
      end subroutine pc_frou_put_control_cards


      subroutine pc_frou_put_control_card (card)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: card(*)            ! argument
      character(len=PC_DATACARD_LENGTH) :: card9              ! local

      call string_hh2cc        (card,card9)
      call pc_put_control_card (card9)
      end subroutine pc_frou_put_control_card


      subroutine pc_frou_add_control_card (card)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: card(*)            ! argument
      character(len=PC_DATACARD_LENGTH) :: card9              ! local

      call string_hh2cc        (card,card9)
      call pc_add_control_card (card9)
      end subroutine pc_frou_add_control_card


      subroutine pc_frou_clear_control_cards
      use pc_frou_module
      implicit none

      call pc_clear_control_cards
      end subroutine pc_frou_clear_control_cards


                         !!!!!!!!!!!!!!!!!!!!!!!


      function pc_frou_num_pdata_cards () result (ncards)
      use pc_frou_module
      implicit none
      integer                      :: ncards         ! result

      ncards = pc_num_pdata_cards()
      end function pc_frou_num_pdata_cards




      subroutine pc_frou_get_pdata_cards (nsize,cards,ncards,errmsg,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: nsize             ! argument
      integer              ,intent(out) :: cards(*)          ! argument
      integer              ,intent(out) :: ncards            ! argument
      integer              ,intent(out) :: errmsg(*)         ! argument
      integer              ,intent(in)  :: nwords            ! argument
      character(len=PC_DATACARD_LENGTH) :: cards9(nsize)     ! local
      character(len=PC_MESSAGE_LENGTH)  :: errmsg9           ! local

      call pc_get_pdata_cards (cards9,ncards,errmsg9)
      call string_cc2hh_array (cards9,cards,nwords,ncards)
      call string_cc2hh       (errmsg9,errmsg)
      end subroutine pc_frou_get_pdata_cards


      subroutine pc_frou_get_pdata_card (icard,card,errmsg)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: icard          ! argument
      integer              ,intent(out) :: card(*)        ! argument
      integer              ,intent(out) :: errmsg(*)      ! argument
      character(len=PC_DATACARD_LENGTH) :: card9          ! local
      character(len=PC_MESSAGE_LENGTH)  :: errmsg9        ! local

      call pc_get_pdata_card (icard,card9,errmsg9)
      call string_cc2hh      (card9,card)
      call string_cc2hh      (errmsg9,errmsg)
      end subroutine pc_frou_get_pdata_card


      subroutine pc_frou_put_pdata_cards (cards,ncards,nwords)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: cards(*)           ! argument
      integer               ,intent(in) :: ncards             ! argument
      integer               ,intent(in) :: nwords             ! argument
      character(len=PC_DATACARD_LENGTH) :: cards9(ncards)     ! local

      call string_hh2cc_array (cards,cards9,nwords,ncards)
      call pc_put_pdata_cards (cards9,ncards)
      end subroutine pc_frou_put_pdata_cards


      subroutine pc_frou_put_pdata_card (card)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: card(*)            ! argument
      character(len=PC_DATACARD_LENGTH) :: card9              ! local

      call string_hh2cc      (card,card9)
      call pc_put_pdata_card (card9)
      end subroutine pc_frou_put_pdata_card


      subroutine pc_frou_add_pdata_card (card)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: card(*)            ! argument
      character(len=PC_DATACARD_LENGTH) :: card9              ! local

      call string_hh2cc      (card,card9)
      call pc_add_pdata_card (card9)
      end subroutine pc_frou_add_pdata_card


      subroutine pc_frou_clear_pdata_cards
      use pc_frou_module
      implicit none

      call pc_clear_pdata_cards
      end subroutine pc_frou_clear_pdata_cards


                         !!!!!!!!!!!!!!!!!!!!!!!


      function pc_frou_num_jdata_cards () result (ncards)
      use pc_frou_module
      implicit none
      integer                      :: ncards         ! result

      ncards = pc_num_jdata_cards()
      end function pc_frou_num_jdata_cards



      subroutine pc_frou_get_jdata_cards (nsize,cards,ncards,errmsg,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: nsize             ! argument
      integer              ,intent(out) :: cards(*)          ! argument
      integer              ,intent(out) :: ncards            ! argument
      integer              ,intent(out) :: errmsg(*)         ! argument
      integer              ,intent(in)  :: nwords            ! argument
      character(len=PC_DATACARD_LENGTH) :: cards9(nsize)     ! local
      character(len=PC_MESSAGE_LENGTH)  :: errmsg9           ! local

      call pc_get_jdata_cards (cards9,ncards,errmsg9)
      call string_cc2hh_array (cards9,cards,nwords,ncards)
      call string_cc2hh       (errmsg9,errmsg)
      end subroutine pc_frou_get_jdata_cards


      subroutine pc_frou_get_jdata_card (icard,card,errmsg)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: icard          ! argument
      integer              ,intent(out) :: card(*)        ! argument
      integer              ,intent(out) :: errmsg(*)      ! argument
      character(len=PC_DATACARD_LENGTH) :: card9          ! local
      character(len=PC_MESSAGE_LENGTH)  :: errmsg9        ! local

      call pc_get_jdata_card (icard,card9,errmsg9)
      call string_cc2hh      (card9,card)
      call string_cc2hh      (errmsg9,errmsg)
      end subroutine pc_frou_get_jdata_card


      subroutine pc_frou_put_jdata_cards (cards,ncards,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: cards(*)           ! argument
      integer              ,intent(in)  :: ncards             ! argument
      integer              ,intent(in)  :: nwords             ! argument
      character(len=PC_DATACARD_LENGTH) :: cards9(ncards)     ! local

      call string_hh2cc_array (cards,cards9,nwords,ncards)
      call pc_put_jdata_cards (cards9,ncards)
      end subroutine pc_frou_put_jdata_cards


      subroutine pc_frou_put_jdata_card (card)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: card(*)            ! argument
      character(len=PC_DATACARD_LENGTH) :: card9              ! local

      call string_hh2cc      (card,card9)
      call pc_put_jdata_card (card9)
      end subroutine pc_frou_put_jdata_card


      subroutine pc_frou_add_jdata_card (card)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: card(*)            ! argument
      character(len=PC_DATACARD_LENGTH) :: card9              ! local

      call string_hh2cc      (card,card9)
      call pc_add_jdata_card (card9)
      end subroutine pc_frou_add_jdata_card


      subroutine pc_frou_clear_jdata_cards
      use pc_frou_module
      implicit none

      call pc_clear_jdata_cards
      end subroutine pc_frou_clear_jdata_cards


                         !!!!!!!!!!!!!!!!!!!!!!!


      function pc_frou_num_gui_cards () result (ncards)
      use pc_frou_module
      implicit none
      integer                      :: ncards         ! result

      ncards = pc_num_gui_cards()
      end function pc_frou_num_gui_cards



      subroutine pc_frou_get_gui_cards (nsize,cards,ncards,errmsg,nwords)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: nsize             ! argument
      integer              ,intent(out) :: cards(*)          ! argument
      integer              ,intent(out) :: ncards            ! argument
      integer              ,intent(out) :: errmsg(*)         ! argument
      integer              ,intent(in)  :: nwords            ! argument
      character(len=PC_DATACARD_LENGTH) :: cards9(nsize)     ! local
      character(len=PC_MESSAGE_LENGTH)  :: errmsg9           ! local

      call pc_get_gui_cards   (cards9,ncards,errmsg9)
      call string_cc2hh_array (cards9,cards,nwords,ncards)
      call string_cc2hh       (errmsg9,errmsg)
      end subroutine pc_frou_get_gui_cards


      subroutine pc_frou_get_gui_card (icard,card,errmsg)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: icard          ! argument
      integer              ,intent(out) :: card(*)        ! argument
      integer              ,intent(out) :: errmsg(*)      ! argument
      character(len=PC_DATACARD_LENGTH) :: card9          ! local
      character(len=PC_MESSAGE_LENGTH)  :: errmsg9        ! local

      call pc_get_gui_card (icard,card9,errmsg9)
      call string_cc2hh    (card9,card)
      call string_cc2hh    (errmsg9,errmsg)
      end subroutine pc_frou_get_gui_card


      subroutine pc_frou_put_gui_cards (cards,ncards,nwords)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: cards(*)           ! argument
      integer               ,intent(in) :: ncards             ! argument
      integer               ,intent(in) :: nwords             ! argument
      character(len=PC_DATACARD_LENGTH) :: cards9(ncards)     ! local

      call string_hh2cc_array (cards,cards9,nwords,ncards)
      call pc_put_gui_cards   (cards9,ncards)
      end subroutine pc_frou_put_gui_cards


      subroutine pc_frou_put_gui_card (card)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: card(*)            ! argument
      character(len=PC_DATACARD_LENGTH) :: card9              ! local

      call string_hh2cc    (card,card9)
      call pc_put_gui_card (card9)
      end subroutine pc_frou_put_gui_card


      subroutine pc_frou_add_gui_card (card)
      use pc_frou_module
      implicit none
      integer               ,intent(in) :: card(*)            ! argument
      character(len=PC_DATACARD_LENGTH) :: card9              ! local

      call string_hh2cc    (card,card9)
      call pc_add_gui_card (card9)
      end subroutine pc_frou_add_gui_card


      subroutine pc_frou_clear_gui_cards
      use pc_frou_module
      implicit none

      call pc_clear_gui_cards
      end subroutine pc_frou_clear_gui_cards


!!------------------------ get keyword information -----------------------!!
!!------------------------ get keyword information -----------------------!!
!!------------------------ get keyword information -----------------------!!


      function pc_frou_process_keyword_present (keyword) result (present)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer                          :: present               ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: present9              ! local

      call string_hh2cc                     (keyword,keyword9)
      present9 = pc_process_keyword_present (keyword9)
      call convert_ll2ii                    (present9,present)
      end function pc_frou_process_keyword_present



      function pc_frou_num_process_keywords () result (num)
      use pc_frou_module
      implicit none
      integer                     :: num       ! result

      num = pc_num_process_keywords ()
      end function pc_frou_num_process_keywords



      subroutine pc_frou_get_process_keyword (indx,keyword)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: indx                  ! argument
      integer              ,intent(out) :: keyword(*)            ! argument
      character(len=PC_KEYWORD_LENGTH)  :: keyword9              ! local

      keyword9 = pc_get_process_keyword (indx+1)
      call string_cc2hh                 (keyword9,keyword)
      end subroutine pc_frou_get_process_keyword



      subroutine pc_frou_remove_process_keyword (keyword)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc              (keyword,keyword9)
      call pc_remove_process_keyword (keyword9)
      end subroutine pc_frou_remove_process_keyword


                         !!!!!!!!!!!!!!!!!!!!!!!!!


      function pc_frou_pdata_keyword_present (keyword) result (present)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer                          :: present               ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: present9              ! local

      call string_hh2cc                   (keyword,keyword9)
      present9 = pc_pdata_keyword_present (keyword9)
      call convert_ll2ii                  (present9,present)
      end function pc_frou_pdata_keyword_present



      function pc_frou_num_pdata_keywords () result (num)
      use pc_frou_module
      implicit none
      integer                     :: num       ! result

      num = pc_num_pdata_keywords ()
      end function pc_frou_num_pdata_keywords



      subroutine pc_frou_get_pdata_keyword (indx,keyword)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: indx                  ! argument
      integer              ,intent(out) :: keyword(*)            ! argument
      character(len=PC_KEYWORD_LENGTH)  :: keyword9              ! local

      keyword9 = pc_get_pdata_keyword (indx+1)
      call string_cc2hh               (keyword9,keyword)
      end subroutine pc_frou_get_pdata_keyword



      subroutine pc_frou_remove_pdata_keyword (keyword)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_remove_pdata_keyword (keyword9)
      end subroutine pc_frou_remove_pdata_keyword


                         !!!!!!!!!!!!!!!!!!!!!!!!!


      function pc_frou_jdata_keyword_present (keyword) result (present)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer                          :: present               ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: present9              ! local

      call string_hh2cc                   (keyword,keyword9)
      present9 = pc_jdata_keyword_present (keyword9)
      call convert_ll2ii                  (present9,present)
      end function pc_frou_jdata_keyword_present



      function pc_frou_num_jdata_keywords () result (num)
      use pc_frou_module
      implicit none
      integer                     :: num       ! result

      num = pc_num_jdata_keywords ()
      end function pc_frou_num_jdata_keywords



      subroutine pc_frou_get_jdata_keyword (indx,keyword)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: indx                  ! argument
      integer              ,intent(out) :: keyword(*)            ! argument
      character(len=PC_KEYWORD_LENGTH)  :: keyword9              ! local

      keyword9 = pc_get_jdata_keyword (indx+1)
      call string_cc2hh               (keyword9,keyword)
      end subroutine pc_frou_get_jdata_keyword



      subroutine pc_frou_remove_jdata_keyword (keyword)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc            (keyword,keyword9)
      call pc_remove_jdata_keyword (keyword9)
      end subroutine pc_frou_remove_jdata_keyword


                         !!!!!!!!!!!!!!!!!!!!!!!!!


      function pc_frou_global_keyword_present (keyword) result (present)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer                          :: present               ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: present9              ! local

      call string_hh2cc                    (keyword,keyword9)
      present9 = pc_global_keyword_present (keyword9)
      call convert_ll2ii                   (present9,present)
      end function pc_frou_global_keyword_present



      function pc_frou_num_global_keywords () result (num)
      use pc_frou_module
      implicit none
      integer                     :: num       ! result

      num = pc_num_global_keywords ()
      end function pc_frou_num_global_keywords



      subroutine pc_frou_get_global_keyword (indx,keyword)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: indx                  ! argument
      integer              ,intent(out) :: keyword(*)            ! argument
      character(len=PC_KEYWORD_LENGTH)  :: keyword9              ! local

      keyword9 = pc_get_global_keyword (indx+1)
      call string_cc2hh                (keyword9,keyword)
      end subroutine pc_frou_get_global_keyword



      subroutine pc_frou_remove_global_keyword (keyword)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc             (keyword,keyword9)
      call pc_remove_global_keyword (keyword9)
      end subroutine pc_frou_remove_global_keyword


                         !!!!!!!!!!!!!!!!!!!!!!!!!


      function pc_frou_gui_action_present (keyword,action) result (present)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: action(*)             ! argument
      integer                          :: present               ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_KEYWORD_LENGTH) :: action9               ! local
      logical                          :: present9              ! local

      call string_hh2cc                (keyword,keyword9)
      call string_hh2cc                (action,action9)
      present9 = pc_gui_action_present (keyword9,action9)
      call convert_ll2ii               (present9,present)
      end function pc_frou_gui_action_present



      function pc_frou_num_gui_keywords () result (num)
      use pc_frou_module
      implicit none
      integer                     :: num       ! result

      num = pc_num_gui_keywords ()
      end function pc_frou_num_gui_keywords



      subroutine pc_frou_get_gui_keyword (indx,keyword)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: indx                  ! argument
      integer              ,intent(out) :: keyword(*)            ! argument
      character(len=PC_KEYWORD_LENGTH)  :: keyword9              ! local

      keyword9 = pc_get_gui_keyword (indx+1)
      call string_cc2hh             (keyword9,keyword)
      end subroutine pc_frou_get_gui_keyword



      subroutine pc_frou_get_gui_action (indx,action)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: indx                  ! argument
      integer              ,intent(out) :: action(*)             ! argument
      character(len=PC_KEYWORD_LENGTH)  :: action9               ! local

      action9 = pc_get_gui_action (indx+1)
      call string_cc2hh           (action9,action)
      end subroutine pc_frou_get_gui_action



      subroutine pc_frou_remove_gui_action (keyword,action)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer              ,intent(in) :: action(*)             ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      character(len=PC_KEYWORD_LENGTH) :: action9               ! local

      call string_hh2cc         (keyword,keyword9)
      call string_hh2cc         (action,action9)
      call pc_remove_gui_action (keyword9,action9)
      end subroutine pc_frou_remove_gui_action


                         !!!!!!!!!!!!!!!!!!!!!!!!!


      function pc_frou_control_keyword_present (keyword) result (present)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      integer                          :: present               ! result
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local
      logical                          :: present9              ! local

      call string_hh2cc                     (keyword,keyword9)
      present9 = pc_control_keyword_present (keyword9)
      call convert_ll2ii                    (present9,present)
      end function pc_frou_control_keyword_present



      function pc_frou_num_control_keywords () result (num)
      use pc_frou_module
      implicit none
      integer                     :: num       ! result

      num = pc_num_control_keywords ()
      end function pc_frou_num_control_keywords



      subroutine pc_frou_get_control_keyword (indx,keyword)
      use pc_frou_module
      implicit none
      integer              ,intent(in)  :: indx                  ! argument
      integer              ,intent(out) :: keyword(*)            ! argument
      character(len=PC_KEYWORD_LENGTH)  :: keyword9              ! local

      keyword9 = pc_get_control_keyword (indx+1)
      call string_cc2hh                 (keyword9,keyword)
      end subroutine pc_frou_get_control_keyword



      subroutine pc_frou_remove_control_keyword (keyword)
      use pc_frou_module
      implicit none
      integer              ,intent(in) :: keyword(*)            ! argument
      character(len=PC_KEYWORD_LENGTH) :: keyword9              ! local

      call string_hh2cc              (keyword,keyword9)
      call pc_remove_control_keyword (keyword9)
      end subroutine pc_frou_remove_control_keyword


!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!

