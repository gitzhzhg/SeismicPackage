
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- increment.f90 -----------------------------!!
!!------------------------------- increment.f90 -----------------------------!!
!!------------------------------- increment.f90 -----------------------------!!


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
! Name       : INCREMENT
! Category   : math
! Written    : 1996-10-30   by: Tom Stoeckley
! Revised    : 2000-10-06   by: Tom Stoeckley
! Maturity   : production   2000-10-19
! Purpose    : Get minimum, maximum, and increment of input values.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!        This primitive can be used to get the minimum, maximum, and
!        increment of input values which are available only one at a
!        time.
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
! To get the limits and largest increment of a set of values,
! when only one value is available at any one time:
!
!                                        opt   opt 
!                                    o    i     i   
!            CALL INCREMENT_INIT   (OBJ,DELTA,NUMINC)
!
!                                    b  i
!            CALL INCREMENT_UPDATE (OBJ,X)
!
!                                       opt  opt  opt   opt   opt opt
!                                    b   o    o    o     o     o   o
!            CALL INCREMENT_RESULT (OBJ,XMIN,XMAX,XINC,XBMIN,XBMAX,NX)
!
!
! type(increment_struct) OBJ = the INCREMENT data structure (not a pointer).
! real      DELTA = smallest increment to look for.
! integer  NUMINC = how many increments (multiples of DELTA) to look for.
! real          X = one of a set of values to find the largest increment of.
! real       XMIN = minimum value        of all X values previously provided.
! real       XMAX = maximum value        of all X values previously provided.
! real       XINC = largest increment    of all X values previously provided.
! real      XBMIN = minimum binned value of all X values previously provided.
! real      XBMAX = maximum binned value of all X values previously provided.
! integer      NX = number of locations from XBMIN to XBMAX with increment XINC.
!
! First the INIT routine must be called to initialize the data structure.
! Then the UPDATE routine must be called repeatedly for each value to consider.
! Finally the RESULT routine must be called to return the range and increment.
!
! XINC will be set to a multiple of DELTA, where the multiple has a
! value from 1 to NUMINC.  For example, if DELTA = 0.5 and NUMINC = 12,
! XINC will be set to one of these values:
!         0.5   1.0   1.5   2.0   2.5   3.0
!         3.5   4.0   4.5   5.0   5.5   6.0
! If NX is 1, XINC will be set to DELTA.
!
! The default value of DELTA is 1.0.   DELTA must be > 0.
! The default value of NUMINC is 1.   NUMINC must be > 0.
!
! XMIN and XMAX will be the actual limits of the X values.
! XBMIN and XBMAX will be the same limits rounded to the nearest multiple
!  of DELTA.
!
! This is how all this works:
!  (1) Each X value is considered to be equal to the nearest integer multiple
!        of DELTA (where the multiple is -infinity to +infinity).
!  (2) Each X value is compared with the first X value provided, and
!        the difference modulo I*DELTA (for all I=1,NUMINC) is determined.
!  (3) A flag is saved and updated for each of the NUMINC possibilities.
!        All flags not corresponding to the derived difference modulo
!        I*DELTA are set to one.
!  (4) After all values of X have been provided, XINC is set to the
!        largest difference modulo I*DELTA which still has a flag with
!        a value of zero.  If all flags have a value of one, XINC will
!        be set to DELTA.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  8. 2000-10-19  Stoeckley    Add required missing documentation section.
!  7. 2000-08-23  Stoeckley    Change output value of XINC when NX is 1.
!  6. 2000-05-26  Stoeckley    Add the XBMIN, XBMAX, and NX arguments, and
!                               change default for DELTA.
!  5. 1999-11-23  Stoeckley    Converted from part of the TOOLS primitive
!                               in the old system.
!  4. 1999-01-11  Goodger      Begin using the fortran90 compiler.
!  3. 1996-12-13  Goodger      Added PROG DOC cards for ease in printing
!                               a CPS manual.
!  2. 1996-12-06  Stoeckley    Added some subroutines useful in CFE.
!  1. 1996-10-30  Stoeckley    Initial version.
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


      module increment_module
      implicit none
      public

      character(len=100),public,save :: INCREMENT_IDENT = &
       '$Id: increment.f90,v 1.8 2000/10/19 18:01:10 sps prod sps $'

      type,public :: increment_struct
           private
           logical         :: starting
           real            :: xmin,xmax,delta
           integer         :: ifirst,numinc
           integer,pointer :: ibad(:)
      end type increment_struct

      contains


!!--------------------------- increment init -------------------------------!!
!!--------------------------- increment init -------------------------------!!
!!--------------------------- increment init -------------------------------!!


      subroutine increment_init (obj,delta,numinc)
      implicit none
      type(increment_struct),intent(out) :: obj               ! arguments
      real   ,intent(in),optional        :: delta             ! arguments
      integer,intent(in),optional        :: numinc            ! arguments

      obj%starting = .true.
      obj%xmin     = 0.0  
      obj%xmax     = 0.0 
      obj%delta    = 1.0
      obj%ifirst   = 0
      obj%numinc   = 1

      if (present(delta )) obj%delta  = delta
      if (present(numinc)) obj%numinc = numinc

      if (obj%delta  <= 0.0) obj%delta  = 1.0
      if (obj%numinc <=   0) obj%numinc = 1

      allocate (obj%ibad(obj%numinc))
      obj%ibad(:) = 0
      return
      end subroutine increment_init


!!--------------------------- increment update ---------------------------!!
!!--------------------------- increment update ---------------------------!!
!!--------------------------- increment update ---------------------------!!


      subroutine increment_update (obj,x)
      implicit none
      type(increment_struct),intent(inout) :: obj           ! arguments
      real                  ,intent(in)    :: x             ! arguments
      integer                              :: i,ix          ! local

      if (obj%starting) then
           obj%xmin     = x
           obj%xmax     = x
           obj%ifirst   = nint(x/obj%delta)
           obj%starting = .false.
      else
           obj%xmin = min(obj%xmin,x)
           obj%xmax = max(obj%xmax,x)
           ix       = nint(x/obj%delta)
           do i = 1,obj%numinc
               if (mod(ix - obj%ifirst,i) /= 0) obj%ibad(i) = 1
           end do
      end if
      return
      end subroutine increment_update


!!--------------------------- increment result ---------------------------!!
!!--------------------------- increment result ---------------------------!!
!!--------------------------- increment result ---------------------------!!


      subroutine increment_result (obj,xmin,xmax,xinc,xbmin,xbmax,nx)
      implicit none
      type(increment_struct),intent(inout) :: obj             ! arguments
      real         ,optional,intent(out)   :: xmin,xmax       ! arguments
      real         ,optional,intent(out)   :: xinc            ! arguments
      real         ,optional,intent(out)   :: xbmin,xbmax     ! arguments
      integer      ,optional,intent(out)   :: nx              ! arguments
      integer                              :: i,nx2           ! local
      real                                 :: xinc2           ! local
      real                                 :: xbmin2,xbmax2   ! local

      xinc2 = obj%delta
      do i = 1,obj%numinc
          if (obj%ibad(i) == 0) xinc2 = obj%delta*i
      end do
      xbmin2 = obj%delta * nint(obj%xmin / obj%delta)
      xbmax2 = obj%delta * nint(obj%xmax / obj%delta)
      nx2    = nint((xbmax2 - xbmin2)/xinc2) + 1
      if (nx2 == 1) xinc2 = obj%delta

      if (present(xmin )) xmin  = obj%xmin
      if (present(xmax )) xmax  = obj%xmax
      if (present(xinc )) xinc  = xinc2
      if (present(xbmin)) xbmin = xbmin2
      if (present(xbmax)) xbmax = xbmax2
      if (present(nx   )) nx    = nx2

      deallocate (obj%ibad)
      return
      end subroutine increment_result


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module increment_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

