!<CPS_v1 type="PRIMITIVE"/>
!!-------------------------- tsortparams.f90 --------------------------------!!
!!-------------------------- tsortparams.f90 --------------------------------!!
!!-------------------------- tsortparams.f90 --------------------------------!!


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
! Name       : TSORTPARAMS     (trace sort parameters)
! Category   : sorts
! Written    : 2001-12-28   by: Tom Stoeckley
! Revised    : 2010-02-02   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Maintain trace sort parameters.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive owns and maintains trace sort parameters (and defines
! the associated GUI and context sensitive help) for several processes
! which sort traces, or sort or use directories for reading traces.
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
! Call from the process create routine:
!
!                call tsortparams_create (obj)
!                                          o
!
! Call from the process delete routine:
!
!                call tsortparams_delete (obj)
!                                          b
!
! Call from the process initialize routine:
!
!                call tsortparams_initialize (obj)
!                                              b
!
! Call from the process update routine:
!
!                call tsortparams_update (obj, sense)
!                                          i     i
!                                               opt
!
! Call from anywhere:
!
!                hdr  = tsortparams_get_hdr  (obj)
!                init = tsortparams_get_init (obj)
!                inc  = tsortparams_get_inc  (obj)
!                 o                            i
!
!
! type(tsortparams_struct)  obj = pointer to the data structure.
! logical                 sense = whether to make the parameters sensitive.
! type(triplesort_ints)     hdr = trace header words to sort on.
! type(triplesort_doubles) init = center of first (or any bin) for each header.
! type(triplesort_doubles)  inc = bin increment (and width) for each header.
!
! The default value of SENSE is true.
!
! The sort priority order for HDR, INIT, and INC is the same order as their
! structural components %primary, %secondary, and %tertiary.
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
!     Date        Author     Description
!     ----        ------     -----------
!  4. 2010-02-02  Stoeckley  Make sort option values more general.
!  3. 2005-10-24  Stoeckley  Remove blank lines in gui_def to provide more
!                             room for large menus.
!  2. 2003-12-09  Stoeckley  Use triplesort double precision instead of real.
!  1. 2002-02-04  Stoeckley  Initial version, made from code removed from the
!                             TSORT process for reuse by other processes.
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


!-------------------------------------------------------------------------------
!<gui_def>
!     OPT_SORT=`CCCCCCCCCCCCCC
!     HDR_PRI =`IIIII   PRI_INIT =`FFFFFFFFFF   PRI_INC =`FFFFFFFFFF
!     HDR_SEC =`IIIII   SEC_INIT =`FFFFFFFFFF   SEC_INC =`FFFFFFFFFF
!     HDR_TERT=`IIIII   TERT_INIT=`FFFFFFFFFF   TERT_INC=`FFFFFFFFFF
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OPT_SORT">
!<Tip> Option for selecting common sorts or a custom sort. </Tip>
! Default = CUSTOM
! Allowed = COMMON_MIDPOINT ( 8, 7, 6)
! Allowed = COMMON_SOURCE   ( 9,10, 1)
! Allowed = COMMON_RECEIVER (10, 9, 1)
! Allowed = COMMON_OFFSET   ( 6, 8, 7)
! Allowed = CUSTOM
!</Help>
!
!
!<Help KEYWORD="HDR_PRI">
!<Tip> Header word designating primary sort bins. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!
! HDR_PRI is the least rapidly changing sort header for output traces.
!</Help>
!
!
!<Help KEYWORD="HDR_SEC">
!<Tip> Header word designating secondary sort bins. </Tip>
! Default = 6
! Allowed = 1 - NWIH
!
! HDR_SEC has intermediate rapidity of change for output traces.
!</Help>
!
!
!<Help KEYWORD="HDR_TERT">
!<Tip> Header word designating tertiary sort bins. </Tip>
! Default = 1
! Allowed = 1 - NWIH
!
! HDR_TERT is the most rapidly changing sort header for output traces.
!</Help>
!
!
!<Help KEYWORD="PRI_INIT">
!<Tip> Value for the center of ANY primary sort bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="SEC_INIT">
!<Tip> Value for the center of ANY secondary sort bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="TERT_INIT">
!<Tip> Value for the center of ANY tertiary sort bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="PRI_INC">
!<Tip> Increment for primary header word. </Tip>
! Default = 1.0
! Allowed = real (but not zero)
!
! PRI_INC is the increment between, and the width of, primary sort bins.
! If PRI_INC < 0, traces will be output in descending order of HDR_PRI.
!</Help>
!
!
!<Help KEYWORD="SEC_INC">
!<Tip> Increment for secondary header word. </Tip>
! Default = 1.0
! Allowed = real (but not zero)
!
! SEC_INC is the increment between, and the width of, secondary sort bins.
! If SEC_INC < 0, traces will be output in descending order of HDR_SEC.
!</Help>
!
!
!<Help KEYWORD="TERT_INC">
!<Tip> Increment for tertiary header word. </Tip>
! Default = 1.0
! Allowed = real (but not zero)
!
! TERT_INC is the increment between, and the width of, tertiary sort bins.
! If TERT_INC < 0, traces will be output in descending order of HDR_TERT.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module tsortparams_module
      use pc_module
      use triplesort_module
      use named_constants_module
      implicit none
      public
      private :: tsortparams_fixup

      character(len=100),public,save :: TSORTPARAMS_IDENT = &
'$Id: tsortparams.f90,v 1.3 2005/10/24 11:32:17 Stoeckley prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: tsortparams_struct              

        private
        character(len=32)        :: opt_sort         ! process parameters.
        type(triplesort_ints)    :: hdr              ! process parameters.
        type(triplesort_doubles) :: init             ! process parameters.
        type(triplesort_doubles) :: inc              ! process parameters.

      end type tsortparams_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      integer,parameter      ::  sort_nopt = 5
      character(len=26),save ::  sort_options (sort_nopt)

      data  sort_options /'COMMON_MIDPOINT ( 8, 7, 6)', &
                          'COMMON_SOURCE   ( 9,10, 1)', &
                          'COMMON_RECEIVER (10, 9, 1)', &
                          'COMMON_OFFSET   ( 6, 8, 7)', &
                          'CUSTOM'/

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine tsortparams_create (obj)

      type(tsortparams_struct),pointer :: obj       ! arguments

      allocate (obj)
      call tsortparams_initialize (obj)

      end subroutine tsortparams_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine tsortparams_delete (obj)

      type(tsortparams_struct),pointer :: obj       ! arguments

      if (associated(obj)) deallocate(obj)

      end subroutine tsortparams_delete


!!------------------------- initialize ------------------------------------!!
!!------------------------- initialize ------------------------------------!!
!!------------------------- initialize ------------------------------------!!


      subroutine tsortparams_initialize (obj)

      type(tsortparams_struct),intent(inout) :: obj       ! arguments

      obj%opt_sort       = 'CUSTOM'
      obj%hdr            = (/7,6,1/)
      obj%init           = 1.0D0
      obj%inc            = 1.0D0

      end subroutine tsortparams_initialize


!!---------------------------- update -------------------------------------!!
!!---------------------------- update -------------------------------------!!
!!---------------------------- update -------------------------------------!!


      subroutine tsortparams_update (obj,sense)

      type(tsortparams_struct),intent(inout) :: obj       ! arguments
      logical     ,optional   ,intent(in)    :: sense     ! arguments
      integer                                :: nwih      ! local
      logical                                :: sense2    ! local

!----------get parameters.

      call pc_get_global ('nwih' , nwih)

      call pc_get ('OPT_SORT'  , obj%opt_sort      )
      call pc_get ('HDR_PRI'   , obj%hdr%primary   )
      call pc_get ('HDR_SEC'   , obj%hdr%secondary )
      call pc_get ('HDR_TERT'  , obj%hdr%tertiary  )
      call pc_get ('PRI_INIT'  , obj%init%primary  )
      call pc_get ('SEC_INIT'  , obj%init%secondary)
      call pc_get ('TERT_INIT' , obj%init%tertiary )
      call pc_get ('PRI_INC'   , obj%inc%primary   )
      call pc_get ('SEC_INC'   , obj%inc%secondary )
      call pc_get ('TERT_INC'  , obj%inc%tertiary  )

!----------verify parameters.

      select case (obj%opt_sort(1:15))
!!!!!   case ('COMMON_MIDPOINT ( 8, 7, 6)') ; obj%hdr      = (/ 8, 7, 6/)
!!!!!   case ('COMMON_SOURCE   ( 9,10, 1)') ; obj%hdr      = (/ 9,10, 1/)
!!!!!   case ('COMMON_RECEIVER (10, 9, 1)') ; obj%hdr      = (/10, 9, 1/)
!!!!!   case ('COMMON_OFFSET   ( 6, 8, 7)') ; obj%hdr      = (/ 6, 8, 7/)
        case ('COMMON_MIDPOINT           ') ; obj%hdr      = (/ 8, 7, 6/)
        case ('COMMON_SOURCE             ') ; obj%hdr      = (/ 9,10, 1/)
        case ('COMMON_RECEIVER           ') ; obj%hdr      = (/10, 9, 1/)
        case ('COMMON_OFFSET             ') ; obj%hdr      = (/ 6, 8, 7/)
        case default                        ; obj%opt_sort = 'CUSTOM'
      end select

      call tsortparams_fixup (obj%hdr %primary,  &
                              obj%init%primary,  &
                              obj%inc %primary, nwih, 'PRI')

      call tsortparams_fixup (obj%hdr %secondary,  &
                              obj%init%secondary,  &
                              obj%inc %secondary, nwih, 'SEC')

      call tsortparams_fixup (obj%hdr %tertiary,  &
                              obj%init%tertiary,  &
                              obj%inc %tertiary, nwih, 'TERT')

      if (obj%hdr%primary == 1) then
          call pc_error ('HDR_PRI must be greater than one')
      end if

      if (obj%hdr%secondary == 1 .and. obj%hdr%tertiary /= 1) then
          call pc_error ('HDR_TERT must be one if HDR_SEC is one')
      end if

!----------put parameters.

      call pc_put_options_field ('OPT_SORT', sort_options, sort_nopt)

      call pc_put ('OPT_SORT'  , obj%opt_sort      )
      call pc_put ('HDR_PRI'   , obj%hdr%primary   )
      call pc_put ('HDR_SEC'   , obj%hdr%secondary )
      call pc_put ('HDR_TERT'  , obj%hdr%tertiary  )
      call pc_put ('PRI_INIT'  , obj%init%primary  )
      call pc_put ('SEC_INIT'  , obj%init%secondary)
      call pc_put ('TERT_INIT' , obj%init%tertiary )
      call pc_put ('PRI_INC'   , obj%inc%primary   )
      call pc_put ('SEC_INC'   , obj%inc%secondary )
      call pc_put ('TERT_INC'  , obj%inc%tertiary  )

!----------set sensitivities.

      if (present(sense)) then
           sense2 = sense
      else
           sense2 = .true.
      end if

      if (sense2) then
        call pc_put_sensitive_field_flag ('OPT_SORT' ,.true.                  )
        call pc_put_sensitive_field_flag ('HDR_PRI'  ,obj%opt_sort == 'CUSTOM')
        call pc_put_sensitive_field_flag ('HDR_SEC'  ,obj%opt_sort == 'CUSTOM')
        call pc_put_sensitive_field_flag ('HDR_TERT' ,obj%opt_sort == 'CUSTOM')
        call pc_put_sensitive_field_flag ('PRI_INIT' ,obj%hdr%primary   > 1   )
        call pc_put_sensitive_field_flag ('SEC_INIT' ,obj%hdr%secondary > 1   )
        call pc_put_sensitive_field_flag ('TERT_INIT',obj%hdr%tertiary  > 1   )
        call pc_put_sensitive_field_flag ('PRI_INC'  ,obj%hdr%primary   > 1   )
        call pc_put_sensitive_field_flag ('SEC_INC'  ,obj%hdr%secondary > 1   )
        call pc_put_sensitive_field_flag ('TERT_INC' ,obj%hdr%tertiary  > 1   )
      else
        call pc_put_sensitive_field_flag ('OPT_SORT' ,.false.)
        call pc_put_sensitive_field_flag ('HDR_PRI'  ,.false.)
        call pc_put_sensitive_field_flag ('HDR_SEC'  ,.false.)
        call pc_put_sensitive_field_flag ('HDR_TERT' ,.false.)
        call pc_put_sensitive_field_flag ('PRI_INIT' ,.false.)
        call pc_put_sensitive_field_flag ('SEC_INIT' ,.false.)
        call pc_put_sensitive_field_flag ('TERT_INIT',.false.)
        call pc_put_sensitive_field_flag ('PRI_INC'  ,.false.)
        call pc_put_sensitive_field_flag ('SEC_INC'  ,.false.)
        call pc_put_sensitive_field_flag ('TERT_INC' ,.false.)
      end if

      end subroutine tsortparams_update


!!----------------------------- fixup --------------------------------------!!
!!----------------------------- fixup --------------------------------------!!
!!----------------------------- fixup --------------------------------------!!


   subroutine tsortparams_fixup (hdr,init,inc,nwih,which)

   integer          ,intent(inout) :: hdr                    ! arguments
   double precision ,intent(inout) :: init,inc               ! arguments
   integer          ,intent(in)    :: nwih                   ! arguments
   character(len=*) ,intent(in)    :: which                  ! arguments

   if (hdr <= 1) then
        hdr  = 1
        init = 1.0
        inc  = 1.0
        return
   end if

   if (hdr > nwih) then
        call pc_error ('HDR_'//trim(which)//' cannot be larger than',nwih)
   end if

   if (inc == 0.0) then
        inc = 1.0
        call pc_warning (trim(which)//'_INC cannot be zero. Resetting to 1.0')
   endif

   end subroutine tsortparams_fixup


!!---------------------------- get -------------------------------------!!
!!---------------------------- get -------------------------------------!!
!!---------------------------- get -------------------------------------!!


      function tsortparams_get_hdr (obj) result (hdr)

      type(tsortparams_struct),intent(in) :: obj       ! arguments
      type(triplesort_ints)               :: hdr       ! result

      hdr = obj%hdr

      end function tsortparams_get_hdr



      function tsortparams_get_init (obj) result (init)

      type(tsortparams_struct),intent(in) :: obj       ! arguments
      type(triplesort_doubles)            :: init      ! result

      init = obj%init

      end function tsortparams_get_init



      function tsortparams_get_inc (obj) result (inc)

      type(tsortparams_struct),intent(in) :: obj       ! arguments
      type(triplesort_doubles)            :: inc       ! result

      inc = obj%inc

      end function tsortparams_get_inc


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module tsortparams_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

