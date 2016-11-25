
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- velfun.f90 --------------------------------!!
!!------------------------------- velfun.f90 --------------------------------!!
!!------------------------------- velfun.f90 --------------------------------!!


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
! Name       : VELFUN
! Category   : velocity
! Written    : 2000-06-20   by: Tom Stoeckley
! Revised    : 2000-10-06   by: Tom Stoeckley
! Maturity   : production   2000-10-20
! Purpose    : To manage a single velocity function.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
!                This primitive is to be used for managing
!                a single velocity function in memory,
!
!-------------------------------------------------------------------------------
!</descript_doc>

 
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2000-10-20  Stoeckley  Add required missing documentation section.
!  1. 2000-09-27  Stoeckley  Initial version.
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
!                             o 
!    call velfun_create     (obj)
!                            
!                                 opt    opt    opt    opt    opt
!                             b    i      i      i      i      i   
!    call velfun_set_values (obj,xcoord,ycoord,npicks,tpicks,vpicks,
!           velname,veltype,project,line,rdate,pdate,userid,comment)
!              i       i       i     i     i     i     i       i   
!             opt     opt     opt   opt   opt   opt   opt     opt  
!
!                                 opt    opt    opt    opt    opt
!                             i    o      o      o      o      o   
!    call velfun_get_values (obj,xcoord,ycoord,npicks,tpicks,vpicks,
!           velname,veltype,project,line,rdate,pdate,userid,comment)
!              o       o       o     o     o     o     o       o   
!             opt     opt     opt   opt   opt   opt   opt     opt  
!
!                             b
!    call velfun_clear      (obj)
!    call velfun_delete     (obj)
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(velfun_struct),pointer obj = pointer to VELFUN data structure.
!
! real    xcoord    = X coordinate of the velocity function.
! real    ycoord    = Y coordinate of the velocity function.
! integer npicks    = Number of time/vel picks in the velocity function.
! real    tpicks(:) = Array that holds the abscissae (  TIME   picks).
! real    vpicks(:) = Array that holds the ordinates (VELOCITY picks).
!
! char(*) velname   = Name of velocity function  (8 characters).
! char(*) veltype   = Type of velocity function  (4 characters).
! char(*) project   = Project name              (10 characters).
! char(*) line      = Line name                 (10 characters).
! char(*) rdate     = Recording date             (5 characters).
! char(*) pdate     = Processing date            (5 characters).
! char(*) userid    = User ID                    (3 characters).
! char(*) comment   = Comment                   (15 characters).
!
! NOTE: In VELFUN_SET_VALUES, the three arguments NPICKS, TPICKS, and VPICKS
! must all be present if any of them are present.  Otherwise those three
! arguments are ignored.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                           ADVICE FOR USERS
!
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
 
 
      module velfun_module
      implicit none
      public

      character(len=100),public,save :: VELFUN_IDENT = &
       '$Id: velfun.f90,v 1.2 2000/10/20 15:59:53 sps prod sps $'

      type,public :: velfun_struct
           private
           real              :: xcoord    
           real              :: ycoord    
           integer           :: npicks    
           real    ,pointer  :: tpicks(:) 
           real    ,pointer  :: vpicks(:) 
           character(len= 8) :: velname   
           character(len= 4) :: veltype   
           character(len=10) :: project   
           character(len=10) :: line      
           character(len= 5) :: rdate     
           character(len= 5) :: pdate     
           character(len= 3) :: userid    
           character(len=15) :: comment   
      end type velfun_struct

      contains


!!------------------------ velfun clear -----------------------------------!!
!!------------------------ velfun clear -----------------------------------!!
!!------------------------ velfun clear -----------------------------------!!
 
 
      subroutine velfun_clear (obj)
      implicit none
      type(velfun_struct),intent(inout) :: obj       ! arguments
 
      if (associated(obj%tpicks)) deallocate (obj%tpicks)
      if (associated(obj%vpicks)) deallocate (obj%vpicks)

      obj%xcoord   = 0.0
      obj%ycoord   = 0.0
      obj%npicks   = 0
      obj%velname  = 'none'
      obj%veltype  = 'VTNM'
      obj%project  = 'none'
      obj%line     = 'none'
      obj%rdate    = 'none'
      obj%pdate    = 'none'
      obj%userid   = 'none'
      obj%comment  = 'none'
      return
      end subroutine velfun_clear
 
 
!!--------------------------- velfun create -------------------------------!!
!!--------------------------- velfun create -------------------------------!!
!!--------------------------- velfun create -------------------------------!!
 
 
      subroutine velfun_create (obj)
      implicit none
      type(velfun_struct),pointer :: obj               ! arguments
 
      allocate          (obj)
      nullify           (obj%tpicks)
      nullify           (obj%vpicks)
      call velfun_clear (obj)
      return
      end subroutine velfun_create
 

!!--------------------------- velfun delete -------------------------------!!
!!--------------------------- velfun delete -------------------------------!!
!!--------------------------- velfun delete -------------------------------!!
 
 
      subroutine velfun_delete (obj)
      implicit none
      type(velfun_struct),pointer :: obj               ! arguments
 
      call velfun_clear (obj)
      deallocate        (obj)
      return
      end subroutine velfun_delete
 

!!--------------------------- velfun get values --------------------------!!
!!--------------------------- velfun get values --------------------------!!
!!--------------------------- velfun get values --------------------------!!


      subroutine velfun_get_values (obj,xcoord,ycoord,npicks,tpicks,vpicks, &
                                    velname,veltype,                        &
                                    project,line,rdate,pdate,userid,comment)
      implicit none
      type(velfun_struct),intent(in)        :: obj                 ! arguments
      real            ,intent(out),optional :: xcoord,ycoord       ! arguments
      integer         ,intent(out),optional :: npicks              ! arguments
      real            ,intent(out),optional :: tpicks(:),vpicks(:) ! arguments
      character(len=*),intent(out),optional :: velname,veltype     ! arguments
      character(len=*),intent(out),optional :: project,line        ! arguments
      character(len=*),intent(out),optional :: rdate,pdate         ! arguments
      character(len=*),intent(out),optional :: userid              ! arguments
      character(len=*),intent(out),optional :: comment             ! arguments

      if (present(xcoord )) xcoord               = obj%xcoord 
      if (present(ycoord )) ycoord               = obj%ycoord 
      if (present(npicks )) npicks               = obj%npicks 
      if (present(tpicks )) tpicks(1:obj%npicks) = obj%tpicks(1:obj%npicks)
      if (present(vpicks )) vpicks(1:obj%npicks) = obj%vpicks(1:obj%npicks)
      if (present(velname)) velname              = obj%velname
      if (present(veltype)) veltype              = obj%veltype
      if (present(project)) project              = obj%project
      if (present(line   )) line                 = obj%line
      if (present(rdate  )) rdate                = obj%rdate
      if (present(pdate  )) pdate                = obj%pdate
      if (present(userid )) userid               = obj%userid
      if (present(comment)) comment              = obj%comment
      return
      end subroutine velfun_get_values


!!--------------------------- velfun set values --------------------------!!
!!--------------------------- velfun set values --------------------------!!
!!--------------------------- velfun set values --------------------------!!


      subroutine velfun_set_values (obj,xcoord,ycoord,npicks,tpicks,vpicks, &
                                    velname,veltype,                        &
                                    project,line,rdate,pdate,userid,comment)
      implicit none
      type(velfun_struct),intent(inout)    :: obj                 ! arguments
      real            ,intent(in),optional :: xcoord,ycoord       ! arguments
      integer         ,intent(in),optional :: npicks              ! arguments
      real            ,intent(in),optional :: tpicks(:),vpicks(:) ! arguments
      character(len=*),intent(in),optional :: velname,veltype     ! arguments
      character(len=*),intent(in),optional :: project,line        ! arguments
      character(len=*),intent(in),optional :: rdate,pdate         ! arguments
      character(len=*),intent(in),optional :: userid              ! arguments
      character(len=*),intent(in),optional :: comment             ! arguments

      if (present(xcoord )) obj%xcoord  = xcoord 
      if (present(ycoord )) obj%ycoord  = ycoord 
      if (present(velname)) obj%velname = velname
      if (present(veltype)) obj%veltype = veltype
      if (present(project)) obj%project = project
      if (present(line   )) obj%line    = line
      if (present(rdate  )) obj%rdate   = rdate
      if (present(pdate  )) obj%pdate   = pdate
      if (present(userid )) obj%userid  = userid
      if (present(comment)) obj%comment = comment

      if (present(npicks) .and. present(tpicks) .and. present(vpicks)) then
           if (npicks /= obj%npicks) then
                if (associated(obj%tpicks)) deallocate (obj%tpicks)
                if (associated(obj%vpicks)) deallocate (obj%vpicks)
                allocate (obj%tpicks(npicks))
                allocate (obj%vpicks(npicks))
                obj%npicks = npicks 
           end if
           obj%tpicks = tpicks(1:npicks)
           obj%vpicks = vpicks(1:npicks)
      end if
      return
      end subroutine velfun_set_values


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module velfun_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
