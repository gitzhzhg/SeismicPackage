!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ superproc.f90 -----------------------------!!
!!------------------------------ superproc.f90 -----------------------------!!
!!------------------------------ superproc.f90 -----------------------------!!

  ! other files are:  superproc_frou.f90  superproc_crou.c  superproc_crou.h


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
!                        C P S  P R I M I T I V E
!
! Name       : SUPERPROC
! Category   : cfe
! Written    : 2003-09-11   by: Tom Stoeckley
! Revised    : 2003-11-03   by: Tom Stoeckley
! Maturity   : production
! Purpose    : CFE Super Process Object Module.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This module provides generic access to all CPS processes.  It is
! somewhat analogous to a base class in object-oriented languages.
!
! This module calls the following two files, which are created automatically
! from templates.  The subroutines in these two files are called without USE
! statements to keep superproc.f90 (and the modules which call superproc.f90)
! from having to be recompiled when these two files are modified:
!
!                    superproc_crou.c
!                    superproc_frou.f90
!
! The above two files call functions in the following files, which are also
! created automatically from templates:
!
!                    superwrap.c
!                    xxxx_superwrap.f90 (a file for each CPS process)
!
!-------------------------------------------------------------------------------
!
! All related files:
!
!    this primitive               built using these templates
!    --------------               ---------------------------
!    superproc.f90                N/A
!    superproc_crou.h             N/A
!    superproc_crou.c             superproc_crou_template
!    superproc_frou.f90           superproc_frou_template
!
!    the superwrap primitive      built using these templates
!    -----------------------      ---------------------------
!    superwrap.c                  superwrap_cproc1_template
!                                 superwrap_cproc2_template
!
!    individual wrapper files     built using these templates
!    ------------------------     ---------------------------
!    xxxx_superwrap.f90           fproc_superwrap_template
!    (one for each CPS process)
!
! The program SPS_BUILD_SUPERPROC (called by the MAKE_SUPERPROC script) uses
! the above templates to build the specified files.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!                                   o    i
!    call superproc_create        (obj, name)
!
!                                   b
!    call superproc_initialize    (obj)
!    call superproc_update        (obj)
!    call superproc_wrapup        (obj)
!    call superproc_delete        (obj)
!
!                                   i    o
!    call superproc_get_name      (obj, name)
!
!                                   b    b    b    b
!    call superproc_oneset        (obj, ntr, hd,  tr)
!    call superproc_twosets       (obj, ntr, hd1, tr1, hd2, tr2)
!                                   b    b    b    b    o    o
!
!                                                 opt
!                                   o      o       i
!    call superproc_list          (list, nlist, category)
!
!      o                            i
!    valid = superproc_validate   (name)
!
!                                   i
!    call superproc_set_view      (view)
!
!                                   i      o
!    call superproc_get_rcs_ident (name, ident)
!
!
! type(superproc_struct),pointer      obj = pointer to the SUPERPROC structure.
! character(*)                       name = name of CPS process.
! integer                             ntr = number of traces in and out.
! double precision                hd(:,:) = input and output trace headers.
! real                            tr(:,:) = input and output traces.
! double precision               hd1(:,:) = input trace headers.
! real                           tr1(:,:) = input traces.
! double precision               hd2(:,:) = output trace headers.
! real                           tr2(:,:) = output traces.
! character(*)                   category = name of CPS process category.
! character(*),pointer            list(:) = pointer to list of CPS processes.
! integer                           nlist = number of CPS processes.
! logical                           valid = true if process name is valid.
! integer                            view = index of desired view.
! character(*)                      ident = RCS ident of specified CPS process.
!
! PROCESS_LIST must be nullified before first use.  It will be deallocated
! (if associated) and then reallocated to contain a list of NPROCESS_LIST
! processes.
!
! DESIRED_VIEW must be set to one of the following named constants:
!                         SUPERPROC_PRODUCTION
!                         SUPERPROC_BETA
!                         SUPERPROC_ALPHA
!                         SUPERPROC_RAW
!
! Note: HD1 and TR1 are intent(inout) rather than intent(in) because some
! processes might alter the input traces while working with them.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2003-11-18  Stoeckley  Initial version, made from Donna's super.f90.
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module superproc_module
      use pc_module
      use named_constants_module
      use string_module
      implicit none
      public

      character(len=100),public,save :: SUPERPROC_IDENT = &
'$Id: superproc.f90,v 1.1 2003/11/17 17:02:19 Stoeckley prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: superproc_struct
        private
        character(len=40) :: name
        type(CPOINTER)    :: cpoint
      end type superproc_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


      interface
        subroutine superproc_crou_create (cpoint,h_name,whoops)
        use named_constants_module
        type(CPOINTER),intent(inout) :: cpoint
        integer       ,intent(in)    :: h_name(*)
        integer       ,intent(out)   :: whoops
        end subroutine
      end interface

      interface
        subroutine superproc_crou_delete (cpoint)
        use named_constants_module
        type(CPOINTER),intent(inout) :: cpoint
        end subroutine
      end interface

      interface
        subroutine superproc_crou_initialize (cpoint)
        use named_constants_module
        type(CPOINTER),intent(inout) :: cpoint
        end subroutine
      end interface

      interface
        subroutine superproc_crou_update (cpoint)
        use named_constants_module
        type(CPOINTER),intent(inout) :: cpoint
        end subroutine
      end interface

      interface
        subroutine superproc_crou_wrapup (cpoint)
        use named_constants_module
        type(CPOINTER),intent(inout) :: cpoint
        end subroutine
      end interface

      interface
        subroutine superproc_crou_oneset (cpoint,ntr,hd,tr,lenhd,lentr,num)
        use named_constants_module
        type(CPOINTER)  ,intent(inout) :: cpoint
        integer         ,intent(inout) :: ntr
        integer         ,intent(in)    :: lenhd,lentr,num
        double precision,intent(inout) :: hd(lenhd,num)
        real            ,intent(inout) :: tr(lentr,num)
        end subroutine
      end interface

      interface
        subroutine superproc_crou_twosets (cpoint,ntr,                 &
                                           hd1,tr1,lenhd1,lentr1,num1, &
                                           hd2,tr2,lenhd2,lentr2,num2)
        use named_constants_module
        type(CPOINTER)  ,intent(inout) :: cpoint
        integer         ,intent(inout) :: ntr
        integer         ,intent(in)    :: lenhd1,lentr1,num1
        integer         ,intent(in)    :: lenhd2,lentr2,num2
        double precision,intent(inout) :: hd1(lenhd1,num1)
        double precision,intent(inout) :: hd2(lenhd2,num2)
        real            ,intent(inout) :: tr1(lentr1,num1)
        real            ,intent(inout) :: tr2(lentr2,num2)
        end subroutine
      end interface

      interface
        subroutine superproc_frou_nlist (nprocess_list,category)
        integer         ,intent(out)         :: nprocess_list
        character(len=*),intent(in)          :: category      
        end subroutine
      end interface

      interface
        subroutine superproc_frou_list (process_list,category)
        character(len=*),intent(out)         :: process_list(:)
        character(len=*),intent(in)          :: category        
        end subroutine
      end interface

      interface
        subroutine superproc_frou_validate (name,valid)
        character(len=*),intent(in)      :: name      
        logical         ,intent(out)     :: valid      
        end subroutine
      end interface

      interface
        subroutine superproc_frou_set_view (desired_view)
        integer,intent(in)        :: desired_view       
        end subroutine
      end interface

      interface
        subroutine superproc_frou_get_rcs_ident (name,ident)
        character(len=*)          ,intent(in)    :: name 
        character(len=*)          ,intent(out)   :: ident 
        end subroutine
      end interface


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      integer,parameter,public   :: SUPERPROC_PRODUCTION = 1
      integer,parameter,public   :: SUPERPROC_BETA       = 2
      integer,parameter,public   :: SUPERPROC_ALPHA      = 3
      integer,parameter,public   :: SUPERPROC_RAW        = 4

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine superproc_create (obj,name)
      type(superproc_struct),pointer    :: obj                  ! argument
      character(len=*)      ,intent(in) :: name                 ! argument
      integer                           :: h_name(22)           ! local
      integer                           :: whoops               ! local

      allocate (obj)
      obj%name = name

      call string_cc2hh          (name,h_name)
      call superproc_crou_create (obj%cpoint,h_name,whoops)

      if (whoops /= 0) then
           if (pc_exists()) then
                call pc_error ("Invalid process",obj%name)
           else
                print *, "Invalid process "//trim(obj%name)
           end if
      end if
      end subroutine superproc_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine superproc_delete (obj)
      type(superproc_struct),pointer   :: obj                ! argument

      if (.not. associated(obj)) return

      call superproc_crou_delete (obj%cpoint)

      deallocate(obj)
      end subroutine superproc_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine superproc_initialize (obj)
      type(superproc_struct),pointer :: obj           ! argument

      if (.not. associated(obj)) return

      call superproc_crou_initialize (obj%cpoint)

      end subroutine superproc_initialize


!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!


      subroutine superproc_update (obj)
      type(superproc_struct),pointer :: obj           ! argument

      if (.not. associated(obj)) return

      call superproc_crou_update (obj%cpoint)

      end subroutine superproc_update


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine superproc_wrapup (obj)
      type(superproc_struct),pointer :: obj           ! argument

      if (.not. associated(obj)) return

      call superproc_crou_wrapup (obj%cpoint)

      end subroutine superproc_wrapup


!!------------------------------- oneset -----------------------------------!!
!!------------------------------- oneset -----------------------------------!!
!!------------------------------- oneset -----------------------------------!!


      subroutine superproc_oneset (obj,ntr,hd,tr)
      type(superproc_struct),pointer       :: obj               ! arguments
      integer               ,intent(inout) :: ntr               ! arguments
      double precision      ,intent(inout) :: hd(:,:)           ! arguments
      real                  ,intent(inout) :: tr(:,:)           ! arguments
      integer                              :: lenhd,lentr,num   ! local

      lenhd = size(hd,1)
      lentr = size(tr,1)
      num   = size(tr,2)

      if (.not. associated(obj)) return

      call superproc_crou_oneset (obj%cpoint,ntr,hd,tr,lenhd,lentr,num)

      end subroutine superproc_oneset


!!------------------------------- twosets -----------------------------------!!
!!------------------------------- twosets -----------------------------------!!
!!------------------------------- twosets -----------------------------------!!


      subroutine superproc_twosets (obj,ntr,hd1,tr1,hd2,tr2)
      type(superproc_struct),pointer       :: obj                ! arguments
      integer               ,intent(inout) :: ntr                ! arguments
      double precision      ,intent(inout) :: hd1(:,:)           ! arguments
      real                  ,intent(inout) :: tr1(:,:)           ! arguments
      double precision      ,intent(inout) :: hd2(:,:)           ! arguments
      real                  ,intent(inout) :: tr2(:,:)           ! arguments
      integer                              :: lenhd1,lentr1,num1 ! local
      integer                              :: lenhd2,lentr2,num2 ! local

      lenhd1 = size(hd1,1)
      lentr1 = size(tr1,1)
      num1   = size(tr1,2)

      lenhd2 = size(hd2,1)
      lentr2 = size(tr2,1)
      num2   = size(tr2,2)

      if (.not. associated(obj)) return

      call superproc_crou_twosets (obj%cpoint,ntr,hd1,tr1,lenhd1,lentr1,num1, &
                                                  hd2,tr2,lenhd2,lentr2,num2)

      end subroutine superproc_twosets


!!-------------------------------- list ------------------------------------!!
!!-------------------------------- list ------------------------------------!!
!!-------------------------------- list ------------------------------------!!


      subroutine superproc_list (list,nlist,category)
      implicit none
      character(len=*),pointer             :: list(:)            ! argument
      integer         ,intent(out)         :: nlist              ! argument
      character(len=*),intent(in),optional :: category           ! argument
      character(len=40)                    :: category2          ! local

      if (present(category)) then
           category2 = category
      else
           category2 = ' '
      end if

      call string_to_upper (category2)
      if (category2 == ' ') category2 = 'ALL_PROCESSES'

      call superproc_frou_nlist (nlist,category2)

      if (associated(list)) deallocate (list)
      allocate (list(max(nlist,1)))

      call superproc_frou_list (list,category2)

      end subroutine superproc_list


!!------------------------------ validate ----------------------------------!!
!!------------------------------ validate ----------------------------------!!
!!------------------------------ validate ----------------------------------!!


      function superproc_validate (name) result (valid)
      character(len=*),intent(in)      :: name                 ! argument
      logical                          :: valid                ! result

      call superproc_frou_validate (name,valid)

      end function superproc_validate


!!------------------------------- set view ---------------------------------!!
!!------------------------------- set view ---------------------------------!!
!!------------------------------- set view ---------------------------------!!


      subroutine superproc_set_view (view)
      integer,intent(in)               :: view           ! argument

      call superproc_frou_set_view (view)

      end subroutine superproc_set_view


!!------------------------------- get name ---------------------------------!!
!!------------------------------- get name ---------------------------------!!
!!------------------------------- get name ---------------------------------!!


      subroutine superproc_get_name (obj, name)
      type(superproc_struct),intent(in)    :: obj              ! argument
      character(len=*)      ,intent(out)   :: name             ! argument

      name = obj%name

      end subroutine superproc_get_name


!!------------------------------- get rcs ident ----------------------------!!
!!------------------------------- get rcs ident ----------------------------!!
!!------------------------------- get rcs ident ----------------------------!!


      subroutine superproc_get_rcs_ident (name, ident)
      character(len=*)          ,intent(in)    :: name          ! argument
      character(len=*)          ,intent(out)   :: ident         ! argument

      call superproc_frou_get_rcs_ident (name,ident)

      end subroutine superproc_get_rcs_ident


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module superproc_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

