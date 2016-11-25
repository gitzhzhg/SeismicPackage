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
! Name       : incjobname 
! Category   : cfe
! Written    : 1999-10-01   by: Donna K. Vunderink
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : Automatically fill the Jobname scrollable list on the Submit_Job
!              screen.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! INCREMENT_JOBNAME pop-up dialog box allows the user to fill the Jobname
! scrollable list on the Submit_Job screen when the names conform to constant
! increment naming conventions.
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
! 10. 2006-06-12  B. Menger    Removed Unused Variables.
!  9. 2003-09-15  Stoeckley    Changed type to primitive; changed category to
!                               cfe.
!  8. 2002-01-04  Vunderink    Made inc_utils to incutils name changes.
!  7. 2000-12-11  Vunderink    Made argument list changes required by inc_utils
!                                module change.
!  6. 2000-12-05  Vunderink    Added a check in selection for 15 character
!                                maximum jobnames.
!  5. 2000-08-15  Vunderink    Changed name from cfe_incjobname to incjobname,
!                                changed character variables back to PC_LENGTH,
!                                and made changes for multi-workfile builder.
!  4. 2000-04-24  Vunderink    Changed character variables to use named constant
!                                CFE_LENGTH, and made changes to support alpha
!                                incrementing of jobnames.
!  3. 2000-02-23  Vunderink    Added screen help
!  2. 2000-02-03  Vunderink    Added some parameter traps.
!  1. 1999-10-01  Vunderink    Initial version.
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


!<HelpSection>
!
!<Help KEYWORD="ROOT">
!<Tip> The constant part of the jobname. (Incrementing part is a suffix.) </Tip>
! Default =
! Allowed = char*15
!</Help>
!
!<Help KEYWORD="START">
!<Tip> Initial value of the incrementing part of the jobname. </Tip>
! Default =
! Allowed = char*15
! START may be numeric or alphabetic.  Examples:  1, 01, A, AA, 101.
!</Help>
!
!<Help KEYWORD="INC">
!<Tip> Amount of increment between successive jobnames. </Tip>
! Default =
! Allowed = int /= 0
! INC = 1 means use the natural increment in numeric or alphabetic sequences.
! Example of sequences with INC = 1:  1, 2, 3 or A, B, C or 08, 09, 10.
!
! INC = 2 means use twice the natural increment in numeric or alphabetic
! sequences.  Example:  1, 3, 5 or A, C, E or 08, 10, 12.
!
! INC = n means use n times the natural increment in numeric or alphabetic
! sequences.
!
! INC < 1 is permitted but may be risky since a "1" or an "A" cannot be
! decremented.
!</Help>
!
!<Help KEYWORD="END">
!<Tip> Last value of the incrementing part of the jobname. </Tip>
! Default =
! Allowed = char*15
!</Help>
!
!<Help KEYWORD="TODO">
!<Tip> Total number of incrementing jobnames to build. </Tip>
! Default =
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="OK">
!<Tip> Enter the jobnames in the Jobnames list and remove pop-up. </Tip>
! Default =
! Allowed =
! Button that enters the incrementing jobnames in the Jobnames list on the
! Submit Job screen and removes pop-up.
!</Help>
!
!<Help KEYWORD="Preview">
!<Tip> Temporarily enter the jobnames in the Jobnames list. </Tip>
! Default =
! Allowed =
! Button that temporarily enters the incrementing jobnames in the Jobnames list
! on the Submit Job screen.  Press OK to make the entries permanent.
!</Help>
!
!<Help KEYWORD="Cancel">
!<Tip> Make no entries in the Jobnames list and remove pop-up. </Tip>
! Default =
! Allowed =
! Button that makes no entries in the Jobnames list on the Submit Job screen
! and removes pop-up.
!</Help>
!
!<Help KEYWORD="Help">
!<Tip> This button accesses the pop-up Help Window. </Tip>
! Default =
! Allowed =
!</Help>
!
!</HelpSection>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module incjobname_module

      use pc_module
      use incutils_module

      implicit none

      private
      public :: incjobname_create
      public :: incjobname_update
      public :: incjobname_delete
      public :: incjobname_selection

      character(len=100),public,save :: incjobname_ident = &
       '$Id: incjobname.f90,v 1.10 2006/06/12 13:03:52 Menger prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: incjobname_struct              
        private
        character(len=PC_LENGTH)                :: root
        character(len=PC_LENGTH)                :: start
        integer                                 :: inc
        character(len=PC_LENGTH)                :: end
        integer                                 :: todo
        logical                                 :: favor_end
        character(len=PC_LENGTH)                :: previous_start
        integer                                 :: previous_inc
        character(len=PC_LENGTH)                :: previous_end
        integer                                 :: previous_todo
      end type incjobname_struct


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      character(len=1)                   ,save :: carray(26)
      type(incjobname_struct),pointer,save     :: object      ! needed for traps

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine incjobname_create (obj)
      implicit none
      type(incjobname_struct),pointer :: obj                     ! arguments

      allocate (obj)

      object => obj                                           ! needed for traps

      obj%root           = ' '
      obj%start          = '1'
      obj%inc            = 1
      obj%end            = '1'
      obj%todo           = 1
      obj%favor_end      = .false.
      obj%previous_start = '1'
      obj%previous_inc   = 1
      obj%previous_end   = '1'
      obj%previous_todo  = 1

      call incjobname_update (obj)

      return
      end subroutine incjobname_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine incjobname_delete (obj)
      implicit none
      type(incjobname_struct),pointer :: obj       ! arguments

      if (.not. associated(obj)) return

      deallocate(obj)

      return
      end subroutine incjobname_delete


!!-------------------------------- update ---------------------------------!!
!!-------------------------------- update ---------------------------------!!
!!-------------------------------- update ---------------------------------!!


      subroutine incjobname_update (obj)
      implicit none
      type(incjobname_struct),pointer :: obj                      ! argument

      if (.not. associated(obj)) return

      object => obj                                          ! needed for traps

      call pc_get   ('ROOT'  , obj%root )
      call pc_get   ('START' , obj%start,incjobname_start_trap)
      call pc_get   ('INC'   , obj%inc  ,incjobname_inc_trap  )
      call pc_get   ('END'   , obj%end  ,incjobname_end_trap  )
      call pc_get   ('TODO'  , obj%todo ,incjobname_todo_trap )

      call pc_put   ('ROOT'  , obj%root )
      call pc_put   ('START' , obj%start)
      call pc_put   ('INC'   , obj%inc  )
      call pc_put   ('END'   , obj%end  )
      call pc_put   ('TODO'  , obj%todo )

      return
      end subroutine incjobname_update


!!------------------------------ start_trap -------------------------------!!
!!------------------------------ start_trap -------------------------------!!
!!------------------------------ start_trap -------------------------------!!


      subroutine incjobname_start_trap (keyword)
      implicit none
      character(len=*),intent(in)         :: keyword                  ! argument







      if (object%start .eq. CNIL) then
        call pc_error ('Start can not be blank')
        object%start = object%previous_start
        return
      else
        object%previous_start = object%start
      endif

      call incutils_start_trap (object%start,object%end,1,object%inc,1,  &
                                 object%todo,object%favor_end)

      return
      end subroutine incjobname_start_trap


!!------------------------------- inc_trap --------------------------------!!
!!------------------------------- inc_trap --------------------------------!!
!!------------------------------- inc_trap --------------------------------!!


      subroutine incjobname_inc_trap (keyword)
      implicit none
      character(len=*),intent(in)         :: keyword                  ! argument






      if (object%inc .eq. 0) then
        call pc_error ('Increment must be non-zero')
        object%inc = object%previous_inc
        return
      else
        object%previous_inc = object%inc
      endif

      call incutils_inc_trap (object%start,object%end,1,object%inc,1,  &
                               object%todo,object%favor_end)

      return
      end subroutine incjobname_inc_trap


!!------------------------------- end_trap --------------------------------!!
!!------------------------------- end_trap --------------------------------!!
!!------------------------------- end_trap --------------------------------!!


      subroutine incjobname_end_trap (keyword)
      implicit none
      character(len=*),intent(in)         :: keyword                  ! argument









      if (object%end .eq. CNIL) then
        call pc_error ('End can not be blank')
        object%end = object%previous_end
        return
      else
        object%previous_end = object%end
      endif

      call incutils_end_trap (object%start,object%end,1,object%inc,1,  &
                               object%todo,object%favor_end)

      return
      end subroutine incjobname_end_trap


!!------------------------------ todo_trap --------------------------------!!
!!------------------------------ todo_trap --------------------------------!!
!!------------------------------ todo_trap --------------------------------!!


      subroutine incjobname_todo_trap (keyword)
      implicit none
      character(len=*),intent(in)         :: keyword                  ! argument








      if (object%todo .le. 0) then
        call pc_error ('Todo must be greater than zero')
        object%todo = object%previous_todo
        return
      else
        object%previous_todo = object%todo
      endif

      call incutils_todo_trap (object%start,object%end,1,object%inc,1,  &
                                object%todo,object%favor_end)

      return
      end subroutine incjobname_todo_trap


!!------------------------------ selection --------------------------------!!
!!------------------------------ selection --------------------------------!!
!!------------------------------ selection --------------------------------!!


      subroutine incjobname_selection (obj,array,narray)
      implicit none
      type(incjobname_struct),pointer     :: obj                      ! argument
      character(len=*),pointer            :: array(:)                 ! argument
      integer                             :: narray                   ! argument

      integer                             :: i                        ! local

      if (.not. associated(obj)) return

      if (associated(array)) deallocate(array)
      narray = object%todo
      allocate(array(narray))
      call incutils_selection (object%root,'*',object%start,1,object%inc,1,  &
                                object%todo,array,narray,1,1,narray)
      do i=1,narray
        if (len_trim(array(i)) .gt. 15) then
          call pc_error ('Jobname '//trim(array(i))//' exceeds 15 character '//&
                         'maximum.')
        endif
      enddo

      return
      end subroutine incjobname_selection


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module incjobname_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

