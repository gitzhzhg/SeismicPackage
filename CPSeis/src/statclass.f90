!<CPS_v1 type="PRIMITIVE"/>
!!----------------------------- statclass.f90 ------------------------------!!
!!----------------------------- statclass.f90 ------------------------------!!
!!----------------------------- statclass.f90 ------------------------------!!

 
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
! Name       : STATCLASS 
! Category   : math
! Written    : 2003-06-19   by: Tom Stoeckley
! Revised    : 2005-01-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : A class to encapsulate a static file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This class contains a static file.  It has methods for reading a static
! file into memory and accessing the static file values.  Currently it does
! not contain methods to modify or write out the static file.
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
!                                            o   i    i     i   i     i
!      call statclass_create               (obj,nwih,ndpt,tstrt,dt,lunprint)
!
!                                            b     i      i    i    i    i
!      call statclass_put_static_values    (obj,stattype,nhx1,nhy1,nhx2,nhy2,
!                                           x1,y1,xinc,yinc,nx,ny,statics)
!                                           i  i   i    i   i  i     i
!
!                                            b     i       o     o
!      call statclass_read_file            (obj,pathname,whoops,msg)
!
!                                            b    i
!      call statclass_supply_mute_taper    (obj,taper)
!
!                                            i
!      empty = statclass_is_empty          (obj)
!
!                                                  opt
!        o                                   i  i   i
!      static = statclass_get_static_value (obj,hd,shft)
!      imute  = statclass_get_mute_index   (obj,hd,shft)
!
!                                                             opt
!                                            i  i  b     i     i  
!      call statclass_apply_top_mute       (obj,hd,tr,valmute,shft)
!      call statclass_apply_bottom_mute    (obj,hd,tr,valmute,shft)
!
!                                              i        i      i  b     i
!      call statclass_apply_inside_mute    (top_obj,bottom_obj,hd,tr,valmute,
!                                           top_shft,bottom_shft)
!                                              i          i  
!                                             opt        opt
!
!                                            b
!      call statclass_delete               (obj)
!
!
! type(statclass_struct)      obj = pointer to the STATCLASS object.
! integer                    nwih = number of trace header words.
! integer                    ndpt = number of trace values.
! real                      tstrt = starting time on traces (seconds).
! real                         dt = trace sample interval (seconds).
! integer                lunprint = logical unit number for printing. 
! character(len=*)       pathname = name of static file.
! logical                  whoops = true if an error occurred.
! character(len=*)            msg = error message.
! real                      taper = length of mute taper to apply (seconds).
! double precision          hd(:) = trace header words.
! real                      tr(:) = trace values.
! real                     static = static value (milliseconds).
! logical                   empty = true if no static values are present.
! integer                   imute = mute index (between 1 and ndpt).
! real                    valmute = trace value to set in muted region.
! real                       shft = constant to add to static value (millisec).
!
! character(len=*)       stattype = static file parameter.
! integer                    nhx1 = static file parameter.
! integer                    nhy1 = static file parameter.
! integer                    nhx2 = static file parameter.
! integer                    nhy2 = static file parameter.
! real                         x1 = static file parameter.
! real                         y1 = static file parameter.
! real                       xinc = static file parameter.
! real                       yinc = static file parameter.
! integer                      nx = static file parameter.
! integer                      ny = static file parameter.
! real               statics(:,:) = static file parameter.
!
! If PATHNAME is NONE or blank: WHOOPS will be false and EMPTY will be true.
! Otherwise if an error occurs: WHOOPS and EMPTY will both be true. 
! Otherwise                   : WHOOPS and EMPTY will both be false.
!
! VALMUTE is the value to set the trace to in muted regions.
! VALMUTE is usually set to 0.0 or FNIL.
!
! An inside mute is applied only if the top mute is above the bottom mute.
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
!  2. 2005-01-31  Stoeckley  Add arguments TAPER and SHFT.
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



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module statclass_module
      use statio_module
      use statutil_module
      use pathcheck_module
      implicit none
      public
      private :: statclass_apply_top_taper
      private :: statclass_apply_bottom_taper

      character(len=100),public,save :: STATCLASS_IDENT = &
'$Id: statclass.f90,v 1.2 2005/01/31 14:10:18 Stoeckley prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


    type,public :: statclass_struct

      private
      integer                         :: nwih
      integer                         :: ndpt
      real                            :: tstrt
      real                            :: dt
      integer                         :: lunprint
      character(len=8)                :: stattype
      integer                         :: nhx1
      integer                         :: nhx2
      integer                         :: nhy1
      integer                         :: nhy2
      real                            :: x1
      real                            :: y1
      real                            :: xinc
      real                            :: yinc
      integer                         :: nx
      integer                         :: ny
      logical                         :: empty
      integer                         :: ntaper
      real                  ,pointer  :: taper(:)
      real                  ,pointer  :: statics(:,:)

    end type statclass_struct


!!--------------------------------- data ---------------------------------!!
!!--------------------------------- data ---------------------------------!!
!!--------------------------------- data ---------------------------------!!


    contains


!!-------------------------------- delete --------------------------------!!
!!-------------------------------- delete --------------------------------!!
!!-------------------------------- delete --------------------------------!!


    subroutine statclass_delete (obj)

    type(statclass_struct),pointer :: obj       ! arguments

    if (.not.associated(obj)) return

    if (associated(obj%taper))   deallocate(obj%taper)
    if (associated(obj%statics)) deallocate(obj%statics)

    deallocate(obj)

    end subroutine statclass_delete


!!-------------------------------- create --------------------------------!!
!!-------------------------------- create --------------------------------!!
!!-------------------------------- create --------------------------------!!


    subroutine statclass_create (obj,nwih,ndpt,tstrt,dt,lunprint)

    type(statclass_struct),pointer       :: obj             ! arguments
    integer               ,intent(in)    :: nwih            ! arguments
    integer               ,intent(in)    :: ndpt            ! arguments
    real                  ,intent(in)    :: tstrt           ! arguments
    real                  ,intent(in)    :: dt              ! arguments
    integer               ,intent(in)    :: lunprint        ! arguments

    allocate (obj)

    obj%nwih       = nwih
    obj%ndpt       = ndpt
    obj%tstrt      = tstrt
    obj%dt         = dt
    obj%lunprint   = lunprint
    obj%stattype   = ' '
    obj%nhx1       = 0
    obj%nhy1       = 0
    obj%nhx2       = 0
    obj%nhy2       = 0
    obj%x1         = 0.0
    obj%y1         = 0.0
    obj%xinc       = 0.0
    obj%yinc       = 0.0
    obj%nx         = 0
    obj%ny         = 0
    obj%empty      = .true.
    obj%ntaper     = 0

    nullify(obj%taper)
    nullify(obj%statics)

    end subroutine statclass_create


!!--------------------------- put static values ---------------------------!!
!!--------------------------- put static values ---------------------------!!
!!--------------------------- put static values ---------------------------!!


    subroutine statclass_put_static_values (obj,stattype,nhx1,nhy1,nhx2,nhy2, &
                                            x1,y1,xinc,yinc,nx,ny,statics)

    type(statclass_struct),intent(inout) :: obj               ! arguments
    character(len=8)      ,intent(in)    :: stattype          ! arguments
    integer               ,intent(in)    :: nhx1,nhy1         ! arguments
    integer               ,intent(in)    :: nhx2,nhy2         ! arguments
    real                  ,intent(in)    :: x1,y1             ! arguments
    real                  ,intent(in)    :: xinc,yinc         ! arguments
    integer               ,intent(in)    :: nx,ny             ! arguments
    real                  ,intent(in)    :: statics(:,:)      ! arguments

    obj%stattype   = stattype
    obj%nhx1       = nhx1
    obj%nhy1       = nhy1
    obj%nhx2       = nhx2
    obj%nhy2       = nhy2
    obj%x1         = x1
    obj%y1         = y1
    obj%xinc       = xinc
    obj%yinc       = yinc
    obj%nx         = nx
    obj%ny         = ny
    obj%empty      = .false.

    if(associated(obj%statics)) deallocate (obj%statics)
    allocate (obj%statics(nx,ny))
    obj%statics(:,:) = statics(1:nx,1:ny)

    end subroutine statclass_put_static_values


!!------------------------------- read file -------------------------------!!
!!------------------------------- read file -------------------------------!!
!!------------------------------- read file -------------------------------!!


    subroutine statclass_read_file (obj,pathname,whoops,msg)

    type(statclass_struct),intent(inout) :: obj            ! arguments
    character(len=*)      ,intent(in)    :: pathname       ! arguments
    logical               ,intent(out)   :: whoops         ! arguments
    character(len=*)      ,intent(out)   :: msg            ! arguments
    integer                              :: err            ! local

    if (pathname == PATHCHECK_EMPTY .or. pathname == ' ') then
         obj%empty = .true.
         whoops    = .false.
         return
    end if

    call statio_read_file (pathname,obj%stattype,               &
                           obj%nhx1,obj%nhy1,obj%nhx2,obj%nhy2, &
                           obj%x1,obj%y1,obj%xinc,obj%yinc,     &
                           obj%nx,obj%ny,obj%statics,           &
                           err,msg,lunprint=obj%lunprint)

    if (err == STATIO_ERROR) then
         whoops    = .true.
         obj%empty = .true.
    else if (obj%nhx1 > obj%nwih) then
         msg       = 'NHX1 too large'
         whoops    = .true.
         obj%empty = .true.
    else if (obj%nhy1 > obj%nwih) then
         msg       = 'NHY1 too large'
         whoops    = .true.
         obj%empty = .true.
    else if (obj%nhx2 > obj%nwih) then
         msg       = 'NHX2 too large'
         whoops    = .true.
         obj%empty = .true.
    else if (obj%nhy2 > obj%nwih) then
         msg       = 'NHY2 too large'
         whoops    = .true.
         obj%empty = .true.
    else
         whoops    = .false.
         obj%empty = .false.
    end if

    end subroutine statclass_read_file


!!----------------------------- is empty ----------------------------------!!
!!----------------------------- is empty ----------------------------------!!
!!----------------------------- is empty ----------------------------------!!


    function statclass_is_empty (obj)  result  (empty)

    type(statclass_struct),intent(in)    :: obj           ! arguments
    logical                              :: empty         ! result

    empty = obj%empty

    end function statclass_is_empty


!!------------------------ supply mute taper -----------------------------!!
!!------------------------ supply mute taper -----------------------------!!
!!------------------------ supply mute taper -----------------------------!!


    subroutine statclass_supply_mute_taper (obj,taper)

    type(statclass_struct),intent(inout) :: obj            ! arguments
    real                  ,intent(in)    :: taper          ! arguments
    real                                 :: factor         ! local
    integer                              :: indx           ! local

    if (associated(obj%taper)) deallocate (obj%taper)

    obj%ntaper = nint(taper / obj%dt)
    if (obj%ntaper <= 0) then
         obj%ntaper = 0
         return
    end if
        
    allocate (obj%taper(obj%ntaper))

    factor = (0.5*PI)/obj%ntaper

    do indx = 1,obj%ntaper
        obj%taper(indx) = sin(indx*factor)**2
    end do

    end subroutine statclass_supply_mute_taper


!!----------------------------- get static value --------------------------!!
!!----------------------------- get static value --------------------------!!
!!----------------------------- get static value --------------------------!!


    function statclass_get_static_value (obj,hd,shft)  result  (static)

    type(statclass_struct),intent(in)    :: obj            ! arguments
    double precision      ,intent(in)    :: hd(:)          ! arguments
    real         ,optional,intent(in)    :: shft           ! arguments
    real                                 :: static         ! result
    real                                 :: shft2          ! local

    shft2 = 0.0
    if (present(shft)) shft2 = shft

    if (obj%empty) then
         static = shft2
         return
    end if

    static = shft2 + statutil_get2 (hd,obj%statics,                      &
                                    obj%nhx1,obj%nhy1,obj%nhx2,obj%nhy2, &
                                    obj%x1,obj%y1,obj%xinc,obj%yinc,     &
                                    obj%nx,obj%ny)

    end function statclass_get_static_value


!!----------------------------- get mute index --------------------------!!
!!----------------------------- get mute index --------------------------!!
!!----------------------------- get mute index --------------------------!!


    function statclass_get_mute_index (obj,hd,shft)  result  (imute)

    type(statclass_struct),intent(in)    :: obj            ! arguments
    double precision      ,intent(in)    :: hd(:)          ! arguments
    real         ,optional,intent(in)    :: shft           ! arguments
    integer                              :: imute          ! result
    real                                 :: static         ! local

    static = statclass_get_static_value(obj,hd,shft)

    imute = 1 + nint((0.001*static - obj%tstrt) / obj%dt)

    call mth_constrain (imute, 1, obj%ndpt)

    end function statclass_get_mute_index


!!-------------------------- apply top mute -------------------------------!!
!!-------------------------- apply top mute -------------------------------!!
!!-------------------------- apply top mute -------------------------------!!


    subroutine statclass_apply_top_mute (obj,hd,tr,valmute,shft)

    type(statclass_struct),intent(in)    :: obj                 ! arguments
    double precision      ,intent(inout) :: hd(:)               ! arguments
    real                  ,intent(inout) :: tr(:)               ! arguments
    real                  ,intent(in)    :: valmute             ! arguments
    real         ,optional,intent(in)    :: shft                ! arguments
    integer                              :: imute               ! local

    if (obj%empty) return

    imute = statclass_get_mute_index(obj,hd,shft)

    tr(1:imute-1) = valmute
    hd(2) = imute

    call statclass_apply_top_taper (obj,tr,imute)

    end subroutine statclass_apply_top_mute


!!-------------------------- apply bottom mute -----------------------------!!
!!-------------------------- apply bottom mute -----------------------------!!
!!-------------------------- apply bottom mute -----------------------------!!


    subroutine statclass_apply_bottom_mute (obj,hd,tr,valmute,shft)

    type(statclass_struct),intent(in)    :: obj                 ! arguments
    double precision      ,intent(inout) :: hd(:)               ! arguments
    real                  ,intent(inout) :: tr(:)               ! arguments
    real                  ,intent(in)    :: valmute             ! arguments
    real         ,optional,intent(in)    :: shft                ! arguments
    integer                              :: imute               ! local

    if (obj%empty) return

    imute = statclass_get_mute_index(obj,hd,shft)

    tr(imute+1:obj%ndpt) = valmute
    hd(64) = imute

    call statclass_apply_bottom_taper (obj,tr,imute)

    end subroutine statclass_apply_bottom_mute


!!-------------------------- apply inside mute -----------------------------!!
!!-------------------------- apply inside mute -----------------------------!!
!!-------------------------- apply inside mute -----------------------------!!


    subroutine statclass_apply_inside_mute (top_obj,bottom_obj,hd,tr,valmute, &
                                            top_shft,bottom_shft)

    type(statclass_struct),intent(in)    :: top_obj          ! arguments
    type(statclass_struct),intent(in)    :: bottom_obj       ! arguments
    double precision      ,intent(in)    :: hd(:)            ! arguments
    real                  ,intent(inout) :: tr(:)            ! arguments
    real                  ,intent(in)    :: valmute          ! arguments
    real         ,optional,intent(in)    :: top_shft         ! arguments
    real         ,optional,intent(in)    :: bottom_shft      ! arguments
    integer                              :: imute1,imute2    ! local

    if (top_obj   %empty) return
    if (bottom_obj%empty) return

    imute1 = statclass_get_mute_index(top_obj   ,hd,top_shft)
    imute2 = statclass_get_mute_index(bottom_obj,hd,bottom_shft)

    if (imute2 <= imute1) return

    tr(imute1+1:imute2-1) = valmute

    call statclass_apply_bottom_taper (top_obj   ,tr,imute1)
    call statclass_apply_top_taper    (bottom_obj,tr,imute2)

    end subroutine statclass_apply_inside_mute


!!-------------------------- apply top taper -----------------------------!!
!!-------------------------- apply top taper -----------------------------!!
!!-------------------------- apply top taper -----------------------------!!


    subroutine statclass_apply_top_taper (obj,tr,imute)      ! private

    type(statclass_struct),intent(in)    :: obj              ! arguments
    real                  ,intent(inout) :: tr(:)            ! arguments
    integer               ,intent(in)    :: imute            ! arguments
    integer                              :: itaper,indx      ! local

    itaper = min(imute + obj%ntaper - 1, obj%ndpt)
    do indx = imute,itaper
         tr(indx) = tr(indx) * obj%taper(indx-imute+1)
    end do

    end subroutine statclass_apply_top_taper


!!------------------------- apply bottom taper ---------------------------!!
!!------------------------- apply bottom taper ---------------------------!!
!!------------------------- apply bottom taper ---------------------------!!


    subroutine statclass_apply_bottom_taper (obj,tr,imute)   ! private

    type(statclass_struct),intent(in)    :: obj              ! arguments
    real                  ,intent(inout) :: tr(:)            ! arguments
    integer               ,intent(in)    :: imute            ! arguments
    integer                              :: itaper,indx      ! local

    itaper = max(imute - obj%ntaper + 1, 1)
    do indx = imute,itaper,-1
         tr(indx) = tr(indx) * obj%taper(imute-indx+1)
    end do

    end subroutine statclass_apply_bottom_taper


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


    end module statclass_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!



