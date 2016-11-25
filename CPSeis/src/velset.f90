
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- velset.f90 --------------------------------!!
!!------------------------------- velset.f90 --------------------------------!!
!!------------------------------- velset.f90 --------------------------------!!


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
! Name       : VELSET                (set of velocity functions)
! Category   : velocity
! Written    : 2002-10-24   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : To manage a set of velocity functions.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive maintains a set of velocity functions.
! This primitive does not perform any I/O.
! This primitive uses the VELFUN module to store individual velocity functions.
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
!                                                  opt    opt
!                             o     i     i   i     i      i
!    call velset_create     (obj,nfunmax,nhx,nhy,nmosign,nmoexp)
!                            
!                             b    i      i      i      i      i    
!    call velset_add_velfun (obj,xcoord,ycoord,npicks,tpicks,vpicks,
!                     velname,veltype,project,line,rdate,pdate,userid,comment)
!                        i       i       i     i     i     i     i       i   
!                       opt     opt     opt   opt   opt   opt   opt     opt  
!
!     o                               i
!    nfunmax  = velset_get_nfunmax  (obj)
!    nfun     = velset_get_nfun     (obj)
!    nhx      = velset_get_nhx      (obj)
!    nhy      = velset_get_nhy      (obj)
!    nmosign  = velset_get_nmosign  (obj)
!    nmoexp   = velset_get_nmoexp   (obj)
!    maxpicks = velset_get_maxpicks (obj)
!
!                             i   i     o      o      o      o      o    
!    call velset_get_velfun (obj,ifun,xcoord,ycoord,npicks,tpicks,vpicks,
!                     velname,veltype,project,line,rdate,pdate,userid,comment)
!                        o       o       o     o     o     o     o       o   
!                       opt     opt     opt   opt   opt   opt   opt     opt  
!
!                                opt  opt   opt
!                             b   i    i     i
!    call velset_sort       (obj,xdir,ydir,xfast)
!
!                             b
!    call velset_delete     (obj)
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(velset_struct),pointer obj = pointer to VELSET data structure.
!
! integer nfunmax   = maximum number of velocity functions allowed.
! integer nhx       = CPS trace header word containing X coordinate.
! integer nhy       = CPS trace header word containing Y coordinate.
! real    nmosign   = the   sign   used for the moveout (default 1.0).
! real    nmoexp    = the exponent used for the moveout (default 2.0).
!
! integer nfun      = number of velocity functions.
! integer maxpicks  = maximum number of picks in any saved velocity function.
! integer ifun      = velocity function number to get (1 thru nfun).
!
! real    xcoord    = X coordinate of the velocity function.
! real    ycoord    = Y coordinate of the velocity function.
! integer npicks    = number of time/vel picks in the velocity function.
! real    tpicks(:) = array that holds the abscissae (  TIME   picks).
! real    vpicks(:) = array that holds the ordinates (VELOCITY picks).
!
! char(*) velname   = name of velocity function  (8 characters).
! char(*) veltype   = type of velocity function  (4 characters).
! char(*) project   = project name              (10 characters).
! char(*) line      = line name                 (10 characters).
! char(*) rdate     = recording date             (5 characters).
! char(*) pdate     = processing date            (5 characters).
! char(*) userid    = user ID                    (3 characters).
! char(*) comment   = comment                   (15 characters).
!
! integer xdir      = direction to sort the X coordinates (default 1).
! integer ydir      = direction to sort the Y coordinates (default 1).
! logical xfast     = true to sort X fastest (default true).
!
! XDIR or YDIR >  0 sorts in ascending  order of the X or Y coordinate.
! XDIR or YDIR <  0 sorts in descending order of the X or Y coordinate.
! XDIR or YDIR == 0 does not change the order of the X or Y coordinate.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                           ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!004. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!003. 2006-01-10  B. Menger  Removed Unused Variables.
!  2. 2005-01-31  Stoeckley  Add optional argument XFAST to VELSET_SORT;
!                             replace the contents of VELSET_SORT with more
!                             general and faster sorting mechanism using
!                             SORTKEYS.
!  1. 2003-12-09  Stoeckley  Initial version.
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
 
 
      module velset_module
      use velfun_module
      use sortkeys_module
      implicit none
      public

      character(len=100),public,save :: VELSET_IDENT = &
'$Id: velset.f90,v 1.4 2006/10/17 13:45:50 Glover prod sps $'

      type,private :: velfun_container
           private
           type(velfun_struct),pointer :: velfun
      end type velfun_container

      type,public :: velset_struct
           private
           type(velfun_container),pointer :: vc(:)
           integer                        :: nfunmax
           integer                        :: nfun
           integer                        :: nhx  
           integer                        :: nhy   
           real                           :: nmosign
           real                           :: nmoexp 
           integer                        :: maxpicks
      end type velset_struct

      contains


!!--------------------------- get values -------------------------------!!
!!--------------------------- get values -------------------------------!!
!!--------------------------- get values -------------------------------!!


      function velset_get_nfunmax  (obj) result (nfunmax)
      type(velset_struct),intent(in) :: obj                   ! arguments
      integer                        :: nfunmax               ! result
      nfunmax = obj%nfunmax
      end function velset_get_nfunmax


      function velset_get_nfun     (obj) result (nfun)
      type(velset_struct),intent(in) :: obj                   ! arguments
      integer                        :: nfun                  ! result
      nfun = obj%nfun   
      end function velset_get_nfun   


      function velset_get_nhx      (obj) result (nhx)
      type(velset_struct),intent(in) :: obj                   ! arguments
      integer                        :: nhx                   ! result
      nhx = obj%nhx
      end function velset_get_nhx


      function velset_get_nhy      (obj) result (nhy)
      type(velset_struct),intent(in) :: obj                   ! arguments
      integer                        :: nhy                   ! result
      nhy = obj%nhy
      end function velset_get_nhy


      function velset_get_nmosign  (obj) result (nmosign)
      type(velset_struct),intent(in) :: obj                   ! arguments
      real                           :: nmosign               ! result
      nmosign = obj%nmosign
      end function velset_get_nmosign


      function velset_get_nmoexp   (obj) result (nmoexp)
      type(velset_struct),intent(in) :: obj                   ! arguments
      real                           :: nmoexp                ! result
      nmoexp = obj%nmoexp
      end function velset_get_nmoexp


      function velset_get_maxpicks (obj) result (maxpicks)
      type(velset_struct),intent(in) :: obj                   ! arguments
      integer                        :: maxpicks              ! result
      maxpicks = obj%maxpicks
      end function velset_get_maxpicks


!!------------------------ velset create ---------------------------------!!
!!------------------------ velset create ---------------------------------!!
!!------------------------ velset create ---------------------------------!!


      subroutine velset_create (obj,nfunmax,nhx,nhy,nmosign,nmoexp)
      implicit none
      type(velset_struct),pointer    :: obj                   ! arguments
      integer            ,intent(in) :: nfunmax,nhx,nhy       ! arguments
      real      ,optional,intent(in) :: nmosign,nmoexp        ! arguments

      allocate (obj)
      allocate (obj%vc(nfunmax))
      obj%nfunmax  = nfunmax
      obj%nfun     = 0
      obj%nhx      = nhx 
      obj%nhy      = nhy 
      obj%nmosign  = 1.0
      obj%nmoexp   = 2.0
      obj%maxpicks = 0
      if (present(nmosign)) obj%nmosign = nmosign
      if (present(nmoexp))  obj%nmoexp  = nmoexp
      return
      end subroutine velset_create


!!------------------------ velset delete ---------------------------------!!
!!------------------------ velset delete ---------------------------------!!
!!------------------------ velset delete ---------------------------------!!


      subroutine velset_delete (obj)
      implicit none
      type(velset_struct),pointer          :: obj               ! arguments
      integer                              :: ifun              ! local

      if (associated(obj)) then
           if (associated(obj%vc)) then
                do ifun = 1,obj%nfun
                     call velfun_delete (obj%vc(ifun)%velfun)
                end do
                deallocate (obj%vc)
           end if
           deallocate (obj)
      end if
      return
      end subroutine velset_delete


!!--------------------------- velset add velfun -------------------------!!
!!--------------------------- velset add velfun -------------------------!!
!!--------------------------- velset add velfun -------------------------!!


      subroutine velset_add_velfun &
                   (obj,xcoord,ycoord,npicks,tpicks,vpicks,  &          
                    velname,veltype,project,line,rdate,pdate,userid,comment)
      implicit none
      type(velset_struct),intent(inout)       :: obj              ! arguments
      real               ,intent(in)          :: xcoord,ycoord    ! arguments
      integer            ,intent(in)          :: npicks           ! arguments
      real               ,intent(in)          :: tpicks(:)        ! arguments
      real               ,intent(in)          :: vpicks(:)        ! arguments
      character(len=*)   ,intent(in),optional :: velname,veltype  ! arguments
      character(len=*)   ,intent(in),optional :: project,line     ! arguments
      character(len=*)   ,intent(in),optional :: rdate,pdate      ! arguments
      character(len=*)   ,intent(in),optional :: userid           ! arguments
      character(len=*)   ,intent(in),optional :: comment          ! arguments

      if (obj%nfun >= obj%nfunmax) return

      obj%nfun     = obj%nfun + 1
      obj%maxpicks = max(obj%maxpicks,npicks)

      call velfun_create     (obj%vc(obj%nfun)%velfun)
      call velfun_set_values (obj%vc(obj%nfun)%velfun,   &
                    xcoord,ycoord,npicks,tpicks,vpicks,  &          
                    velname,veltype,project,line,rdate,pdate,userid,comment)
      return
      end subroutine velset_add_velfun


!!--------------------------- velset get velfun -------------------------!!
!!--------------------------- velset get velfun -------------------------!!
!!--------------------------- velset get velfun -------------------------!!


      subroutine velset_get_velfun &
                   (obj,ifun,xcoord,ycoord,npicks,tpicks,vpicks,  &          
                    velname,veltype,project,line,rdate,pdate,userid,comment)
      implicit none
      type(velset_struct),intent(in)           :: obj              ! arguments
      integer            ,intent(in)           :: ifun             ! arguments
      real               ,intent(out)          :: xcoord,ycoord    ! arguments
      integer            ,intent(out)          :: npicks           ! arguments
      real               ,intent(out)          :: tpicks(:)        ! arguments
      real               ,intent(out)          :: vpicks(:)        ! arguments
      character(len=*)   ,intent(out),optional :: velname,veltype  ! arguments
      character(len=*)   ,intent(out),optional :: project,line     ! arguments
      character(len=*)   ,intent(out),optional :: rdate,pdate      ! arguments
      character(len=*)   ,intent(out),optional :: userid           ! arguments
      character(len=*)   ,intent(out),optional :: comment          ! arguments

      if (ifun < 1 .or. ifun > obj%nfun) then
           xcoord = 0.0
           ycoord = 0.0
           npicks = 0
           if (present(velname)) velname = 'xxx'
           if (present(veltype)) velname = 'xxx'
           if (present(project)) project = 'xxx'
           if (present(line   )) line    = 'xxx'
           if (present(rdate  )) rdate   = 'xxx'
           if (present(pdate  )) pdate   = 'xxx'
           if (present(userid )) userid  = 'xxx'
           if (present(comment)) comment = 'xxx'
           return
      end if

      call velfun_get_values (obj%vc(ifun)%velfun,       &
                    xcoord,ycoord,npicks,tpicks,vpicks,  &          
                    velname,veltype,project,line,rdate,pdate,userid,comment)
      return
      end subroutine velset_get_velfun


!!--------------------------- velset sort ---------------------------------!!
!!--------------------------- velset sort ---------------------------------!!
!!--------------------------- velset sort ---------------------------------!!


  !   subroutine velset_sort (obj,xdir,ydir)
  !   implicit none
  !   type(velset_struct),intent(inout) :: obj               ! arguments
  !   integer   ,optional,intent(in)    :: xdir,ydir         ! arguments
  !   real                              :: xcoord1,ycoord1   ! local
  !   real                              :: xcoord2,ycoord2   ! local
  !   integer                           :: ifun,ifun2        ! local
  !   logical                           :: switch,switched   ! local
  !   type(velfun_struct),pointer       :: velfun            ! local
  !   integer                           :: xdir2,ydir2       ! local

  !   xdir2 = 1
  !   ydir2 = 1
  !   if (present(xdir)) xdir2 = xdir
  !   if (present(ydir)) ydir2 = ydir

  !   do ifun2 = 1,obj%nfun
  !        switched = .false.
  !        do ifun = 2,obj%nfun
  !             call velfun_get_values &
  !                     (obj%vc(ifun-1)%velfun,xcoord=xcoord1,ycoord=ycoord1)
  !             call velfun_get_values &
  !                     (obj%vc(ifun  )%velfun,xcoord=xcoord2,ycoord=ycoord2)

  !             if      (xdir2 > 0) then ; switch = (xcoord1 > xcoord2)
  !             else if (xdir2 < 0) then ; switch = (xcoord1 < xcoord2)
  !             else if (ydir2 > 0) then ; switch = (ycoord1 > ycoord2)
  !             else if (ydir2 < 0) then ; switch = (ycoord1 < ycoord2)
  !             else                     ; switch = .false.
  !             end if

  !             if (switch) then
  !                  velfun                => obj%vc(ifun-1)%velfun
  !                  obj%vc(ifun-1)%velfun => obj%vc(ifun  )%velfun
  !                  obj%vc(ifun  )%velfun => velfun
  !                  switched = .true.
  !             end if
  !        end do
  !        if (.not.switched) return
  !   end do
  !   return
  !   end subroutine velset_sort


!!--------------------------- velset sort ---------------------------------!!
!!--------------------------- velset sort ---------------------------------!!
!!--------------------------- velset sort ---------------------------------!!


      subroutine velset_sort (obj,xdir,ydir,xfast)
      implicit none
      type(velset_struct),intent(inout) :: obj               ! arguments
      integer   ,optional,intent(in)    :: xdir,ydir         ! arguments
      logical   ,optional,intent(in)    :: xfast             ! arguments
      real                              :: xcoord,ycoord     ! local
      integer                           :: key1,key2         ! local
      integer                           :: ifun,irecord      ! local

      integer                           :: xdir2,ydir2       ! local
      logical                           :: xfast2,error      ! local
      type(sortkeys_struct),pointer     :: sortkeys          ! local
      integer,parameter                 :: NKEYS = 2         ! local
      integer,parameter                 :: NSORTS = 1        ! local
      type(velfun_container)            :: vc(obj%nfun)      ! local

      nullify (sortkeys) ! jpa
      xdir2  = 1
      ydir2  = 1
      xfast2 = .true.
      if (present(xdir))  xdir2  = xdir
      if (present(ydir))  ydir2  = ydir
      if (present(xfast)) xfast2 = xfast

      call sortkeys_create (sortkeys,obj%nfun,NKEYS,NSORTS,error)

      do ifun = 1,obj%nfun
           call velfun_get_values &
                         (obj%vc(ifun)%velfun,xcoord=xcoord,ycoord=ycoord)
           key1 = ydir2 * nint(ycoord)
           key2 = xdir2 * nint(xcoord)
           call sortkeys_set_keys (sortkeys,ifun,key1,key2)
      enddo

      if (xfast2) then
           call sortkeys_define_sort (sortkeys,1,1,2)
      else
           call sortkeys_define_sort (sortkeys,1,2,1)
      endif

      call sortkeys_select_sort (sortkeys,1)

      do ifun = 1,obj%nfun
           irecord = sortkeys_get_record(sortkeys,ifun)
           vc(ifun)%velfun => obj%vc(irecord)%velfun
      enddo

      do ifun = 1,obj%nfun
           obj%vc(ifun)%velfun => vc(ifun)%velfun
      enddo

      call sortkeys_delete (sortkeys)
      return
      end subroutine velset_sort


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module velset_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
