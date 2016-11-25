
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- abrakadabra.f90 ------------------------------!!
!!---------------------------- abrakadabra.f90 ------------------------------!!
!!---------------------------- abrakadabra.f90 ------------------------------!!


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
! Name       : ABRAKADABRA
! Category   : migrations
! Written    : 2003-06-19   by: Tom Stoeckley
! Revised    : 2005-01-10   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Defines constants needed by tomography-related processes.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This module contains various named constants needed by several processes
! which do tomography and which generate and edit data for tomography.
!
! This module also contains several functions which return a description of the
! specified identification integer.
!
! As of this writing, ABRAKADABRA is used by KADABRA and DABRA.
! Potentially, ABRAKADABRA should also be used by ABRA and by any tomography
! process which uses output from ABRA or KADABRA or DABRA.
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
!                     o                                   i
!                idstring    = abrakadabra_idstring    (ident)
!                description = abrakadabra_description (ident)
!                message     = abrakadabra_message     (ident)
!
! integer           ident       = trace identification integer (named constant).
! character(len=10) idstring    = description of trace identification.
! character(len=30) description = description of trace identification.
! character(len=40) message     = description of trace identification.
!
! Example:
!
!                ident       = ABRAKADABRA_OUTPUT_BETA = 181
!                idstring    = 'ident 181'
!                description = 'edited BETA attribute'
!                message     = 'ident 181 edited BETA attribute'
!
!-------------------------------------------------------------------------------
!</calling_doc>



!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2005-01-10  Stoeckley  Add three functions to return ident descriptions.
!  2. 2003-08-22  Stoeckley  Add ident for GAMMA.
!  1. 2003-06-19  Stoeckley  Initial version.
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



!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


   module abrakadabra_module
   use string_module
   implicit none
   public

   character(len=100),public,save :: ABRAKADABRA_IDENT = &
'$Id: abrakadabra.f90,v 1.3 2005/01/10 14:11:59 Stoeckley prod sps $'


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


!<advice_doc>
!-------------------------------------------------------------------------------
!                      TRACE IDENTIFICATION INTEGERS

 integer,parameter :: ABRAKADABRA_INPUT_BETA  =  81 ! original BETA attribute.
 integer,parameter :: ABRAKADABRA_INPUT_XDIP  =  88 ! original XDIP attribute.
 integer,parameter :: ABRAKADABRA_INPUT_YDIP  =  89 ! original YDIP attribute.

 integer,parameter :: ABRAKADABRA_OUTPUT_BETA = 181 ! edited BETA attribute.
 integer,parameter :: ABRAKADABRA_OUTPUT_XDIP = 188 ! edited XDIP attribute.
 integer,parameter :: ABRAKADABRA_OUTPUT_YDIP = 189 ! edited YDIP attribute.

 integer,parameter :: ABRAKADABRA_RMS_AMPL    =  82 ! RMS amplitudes.
 integer,parameter :: ABRAKADABRA_MAX_AMPL    =  83 ! max absolute amplitudes.
 integer,parameter :: ABRAKADABRA_GATHER_SEMB =  71 ! gather semblance.
 integer,parameter :: ABRAKADABRA_STRUCT_SEMB =  90 ! structural semblance.

 integer,parameter :: ABRAKADABRA_WEIGHT      =  91 ! weight volume.
 integer,parameter :: ABRAKADABRA_GAMMA       =  84 ! GAMMA volume.

 integer,parameter :: ABRAKADABRA_INPUT_VAV   =  51 ! orig average velocity.
 integer,parameter :: ABRAKADABRA_INPUT_VINT  =  86 ! orig interval velocity.

 integer,parameter :: ABRAKADABRA_OUTPUT_VAV  =  85 ! edited average velocity.
 integer,parameter :: ABRAKADABRA_OUTPUT_VINT =  87 ! edited interval velocity.

 integer,parameter :: ABRAKADABRA_TIMEDEPTH   =  01 ! time or depth of horizon.

!-------------------------------------------------------------------------------
!</advice_doc>


   contains


!!-------------------------- description ----------------------------------!!
!!-------------------------- description ----------------------------------!!
!!-------------------------- description ----------------------------------!!


   function abrakadabra_description (ident) result (description)
   integer,intent(in) :: ident                          ! argument
   character(len=30)  :: description                    ! result

   select case (ident)

   case (ABRAKADABRA_INPUT_BETA ) ; description = 'original BETA attribute'
   case (ABRAKADABRA_INPUT_XDIP ) ; description = 'original XDIP attribute'
   case (ABRAKADABRA_INPUT_YDIP ) ; description = 'original YDIP attribute'

   case (ABRAKADABRA_OUTPUT_BETA) ; description = 'edited BETA attribute'
   case (ABRAKADABRA_OUTPUT_XDIP) ; description = 'edited XDIP attribute'
   case (ABRAKADABRA_OUTPUT_YDIP) ; description = 'edited YDIP attribute'

   case (ABRAKADABRA_RMS_AMPL   ) ; description = 'RMS amplitudes'
   case (ABRAKADABRA_MAX_AMPL   ) ; description = 'maximum absolute amplitudes'
   case (ABRAKADABRA_GATHER_SEMB) ; description = 'gather semblance'
   case (ABRAKADABRA_STRUCT_SEMB) ; description = 'structural semblance'

   case (ABRAKADABRA_WEIGHT     ) ; description = 'weight volume'
   case (ABRAKADABRA_GAMMA      ) ; description = 'GAMMA volume'

   case (ABRAKADABRA_INPUT_VAV  ) ; description = 'original average velocity'
   case (ABRAKADABRA_INPUT_VINT ) ; description = 'original interval velocity'

   case (ABRAKADABRA_OUTPUT_VAV ) ; description = 'edited average velocity'
   case (ABRAKADABRA_OUTPUT_VINT) ; description = 'edited interval velocity'

   case (ABRAKADABRA_TIMEDEPTH  ) ; description = 'time or depth of horizon'

   case default                   ; description = 'unknown identity'

   end select

   end function abrakadabra_description


!!-------------------------- idstring ----------------------------------!!
!!-------------------------- idstring ----------------------------------!!
!!-------------------------- idstring ----------------------------------!!


   function abrakadabra_idstring (ident) result (idstring)
   integer,intent(in) :: ident                          ! argument
   character(len=10)  :: idstring                       ! result

   idstring = 'ident '//string_ii2ss(ident)

   end function abrakadabra_idstring


!!-------------------------- message ----------------------------------!!
!!-------------------------- message ----------------------------------!!
!!-------------------------- message ----------------------------------!!


   function abrakadabra_message (ident) result (message)
   integer,intent(in) :: ident                          ! argument
   character(len=40)  :: message                        ! result

   message = abrakadabra_idstring(ident)//abrakadabra_description(ident)

   end function abrakadabra_message


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


   end module abrakadabra_module


!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!

