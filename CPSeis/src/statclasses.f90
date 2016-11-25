!<CPS_v1 type="PRIMITIVE"/>
!!--------------------------- statclasses.f90 -------------------------------!!
!!--------------------------- statclasses.f90 -------------------------------!!
!!--------------------------- statclasses.f90 -------------------------------!!


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
! Name       : STATCLASSES
! Category   : miscellaneous
! Written    : 2003-06-19   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : A class which simply contains a list of STATCLASS objects.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This class simply contains a list of STATCLASS objects.
! The number of STATCLASS objects to create are input to the constructor
! and cannot be changed.
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
!                              o   i    i     i   i     i
!    call statclasses_create (obj,nwih,ndpt,tstrt,dt,lunprint)
!    call statclasses_delete (obj)
!                              b
!
!                              b
!    call statclasses_clear  (obj)
!    call statclasses_init   (obj,nclass)
!                              b    i
!
!       o                                       i
!    nclass    =  statclasses_num_classes     (obj)
!    statclass => statclasses_fetch_statclass (obj,iclass)
!       o                                       i    i
!
!                                         b     i        i      o     o
!    call statclasses_read_static_files (obj,statfiles,nclass,whoops,msg)
!    call statclasses_read_modspec_file (obj,modfile         ,whoops,msg)
!                                         b     i               o     o
!
!     o                                        i       i
!    nlay = statclasses_num_modspec_layers (modfile,lunprint)
!
!
! type(statclasses_struct)        obj = pointer to the STATCLASSES object.
! integer                        nwih = number of trace header words.
! integer                        ndpt = number of trace values.
! real                          tstrt = starting time on traces (seconds).
! real                             dt = trace sample interval (seconds).
! integer                    lunprint = logical unit number for printing.
! integer                      nclass = number of STATCLASS objects. 
! integer                      iclass = which STATCLASS object to fetch. 
! type(statclass_struct)    statclass = pointer to a single STATCLASS object.
! character(len=*)            modfile = modspec file to read.
! character(len=*)  statfiles(nclass) = static files to read.
! logical                      whoops = true if an error occurred.
! character(len=*)                msg = error message.
! integer                        nlay = number of layers on modspec file.
!
! STATCLASSES_READ_STATIC_FILES:
! This object will create NCLASS statclass objects, and then will tell each
! statclass object to read in a static file.
!
! STATCLASSES_READ_MODSPEC_FILE:
! This object will read in the modspec file and will create and populate the
! number of STATCLASS objects matching the number of layers in the file.  The
! STATCLASS objects will contain the Z (depth) values of the layers.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!004. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!  3. 2005-01-31  Stoeckley  Fix more problems using the modspec primitive.
!  2. 2003-07-28  Stoeckley  Add statclasses_num_modspec_layers; fix errors
!                             in using the modspec primitive.
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



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module statclasses_module
      use statclass_module
      use modspec_module

      implicit none
      public

      character(len=100),public,save :: STATCLASSES_IDENT = &
'$Id: statclasses.f90,v 1.4 2006/10/17 13:45:47 Glover prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,private :: statclass_holder
           type(statclass_struct),pointer :: statclass
      end type statclass_holder


      type,public :: statclasses_struct

        private
        integer                         :: nwih
        integer                         :: ndpt
        real                            :: tstrt
        real                            :: dt
        integer                         :: lunprint
        integer                         :: nclass
        type(statclass_holder),pointer  :: classes(:)

      end type statclasses_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      contains


!!--------------------------- num classes ----------------------------------!!
!!--------------------------- num classes ----------------------------------!!
!!--------------------------- num classes ----------------------------------!!


      function statclasses_num_classes (obj) result (nclass)
      type(statclasses_struct),intent(in) :: obj                  ! arguments
      integer                             :: nclass               ! result

      nclass = obj%nclass
      end function statclasses_num_classes


!!-------------------------- fetch statclass -------------------------------!!
!!-------------------------- fetch statclass -------------------------------!!
!!-------------------------- fetch statclass -------------------------------!!


      function statclasses_fetch_statclass (obj,iclass) result (statclass)
      type(statclasses_struct),intent(inout) :: obj              ! arguments
      integer                 ,intent(in)    :: iclass           ! arguments
      type(statclass_struct)  ,pointer       :: statclass        ! result

      if (iclass >= 1 .and. iclass <= obj%nclass) then
           statclass => obj%classes(iclass)%statclass
      else
           nullify (statclass)
      end if

      end function statclasses_fetch_statclass


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine statclasses_create (obj,nwih,ndpt,tstrt,dt,lunprint)
      type(statclasses_struct),pointer    :: obj                  ! arguments
      integer                 ,intent(in) :: nwih,ndpt            ! arguments
      real                    ,intent(in) :: tstrt,dt             ! arguments
      integer                 ,intent(in) :: lunprint             ! arguments

      allocate (obj)

      obj%nwih       = nwih
      obj%ndpt       = ndpt
      obj%tstrt      = tstrt
      obj%dt         = dt
      obj%lunprint   = lunprint
      obj%nclass     = 0
      nullify (obj%classes)

      end subroutine statclasses_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine statclasses_delete (obj)
      type(statclasses_struct),pointer :: obj            ! arguments

      if (.not.associated(obj)) return

      call statclasses_clear (obj)

      deallocate(obj)

      end subroutine statclasses_delete


!!------------------------------- clear ------------------------------------!!
!!------------------------------- clear ------------------------------------!!
!!------------------------------- clear ------------------------------------!!


      subroutine statclasses_clear (obj)
      type(statclasses_struct),pointer :: obj            ! arguments
      integer                          :: iclass         ! local
      type(statclass_struct)  ,pointer :: statclass      ! local

      if (associated(obj%classes)) then
           do iclass = 1,obj%nclass
                statclass => obj%classes(iclass)%statclass
                call statclass_delete (statclass)
           end do
           deallocate (obj%classes)
      end if
      obj%nclass = 0

      end subroutine statclasses_clear


!!----------------------------- init -------------------------------------!!
!!----------------------------- init -------------------------------------!!
!!----------------------------- init -------------------------------------!!


      subroutine statclasses_init (obj,nclass)
      type(statclasses_struct),pointer    :: obj                  ! arguments
      integer                 ,intent(in) :: nclass               ! arguments
      integer                             :: iclass               ! local
      type(statclass_struct)  ,pointer    :: statclass            ! local

      nullify (statclass) ! jpa
      call statclasses_clear (obj)

      obj%nclass = nclass

      allocate (obj%classes(obj%nclass))

      do iclass = 1,obj%nclass
           call statclass_create &
                  (statclass,obj%nwih,obj%ndpt,obj%tstrt,obj%dt,obj%lunprint)
           obj%classes(iclass)%statclass => statclass
      end do

      end subroutine statclasses_init


!!------------------------- read static files -------------------------------!!
!!------------------------- read static files -------------------------------!!
!!------------------------- read static files -------------------------------!!


      subroutine statclasses_read_static_files &
                                   (obj,statfiles,nclass,whoops,msg)
      type(statclasses_struct),pointer     :: obj                  ! arguments
      character(len=*)        ,intent(in)  :: statfiles(:)         ! arguments
      integer                 ,intent(in)  :: nclass               ! arguments
      logical                 ,intent(out) :: whoops               ! arguments
      character(len=*)        ,intent(out) :: msg                  ! arguments
      integer                              :: iclass               ! local
      type(statclass_struct)  ,pointer     :: statclass            ! local

      call statclasses_init (obj,nclass)

      do iclass = 1,obj%nclass
           statclass => obj%classes(iclass)%statclass
           call statclass_read_file (statclass,statfiles(iclass),whoops,msg)
           if (whoops) return
      end do

      whoops = .false.
      msg = 'STATCLASSES: successfully read static files'

      end subroutine statclasses_read_static_files


!!--------------------------- num modspec layers ---------------------------!!
!!--------------------------- num modspec layers ---------------------------!!
!!--------------------------- num modspec layers ---------------------------!!


      function statclasses_num_modspec_layers (modfile,lunprint) result (nlay)
      character(len=*)        ,intent(in) :: modfile         ! arguments
      integer                 ,intent(in) :: lunprint        ! arguments
      integer                             :: nlay            ! result
      type(modspec_struct)    ,pointer    :: modspec         ! local
      character(len =8)                    :: units          ! local
      real                                 :: angle          ! local
      integer                              :: nx,ny,nz       ! local
      double precision                     :: ox,oy          ! local
      real                                 :: oz,dx,dy,dz    ! local

      nullify (modspec) ! jpa
      call modspec_create (modspec,lunprint,modfile,' ')
      if (associated(modspec)) then
           call modspec_getdesc (modspec,nlay,angle,units, &
                                 nx,ox,dx,ny,oy,dy,nz,oz,dz)
           call modspec_delete  (modspec)
      else
           nlay = 0
      end if

      end function statclasses_num_modspec_layers


!!------------------------- read modspec file -------------------------------!!
!!------------------------- read modspec file -------------------------------!!
!!------------------------- read modspec file -------------------------------!!


      subroutine statclasses_read_modspec_file (obj,modfile,whoops,msg)
      type(statclasses_struct),pointer     :: obj                  ! arguments
      character(len=*)        ,intent(in)  :: modfile              ! arguments
      logical                 ,intent(out) :: whoops               ! arguments
      character(len=*)        ,intent(out) :: msg                  ! arguments
      integer                              :: iclass               ! local

      type(modspec_struct)    ,pointer     :: modspec              ! local
      character(len =8)                    :: units                ! local
      real                                 :: angle                ! local
      integer                              :: nlay,nx,ny,nz        ! local
      integer                              :: hx,hy,hz,ix,iy       ! local
      double precision                     :: ox,oy                ! local
      real                                 :: oz,dx,dy,dz,xo,yo    ! local
      real                                 :: xgo,ygo,dx2,dy2      ! local
      real                                 :: a(3),b(3)            ! local
      real                    ,pointer     :: z(:,:,:)             ! local
      logical                              :: has_coef             ! local

      type(statclass_struct)  ,pointer     :: statclass            ! local
      character(len=8)                     :: stattype             ! local
      integer                              :: nhx1,nhy1,nhx2,nhy2  ! local
      real                                 :: x1,y1,xinc,yinc      ! local
      real                    ,pointer     :: statics(:,:)         ! local

      nullify (modspec) ! jpa
      call statclasses_clear (obj)

      call modspec_create (modspec,obj%lunprint,modfile,'GRID')

      if (.not.associated(modspec)) then
           whoops = .true.
           msg = 'STATCLASSES: error creating modspec object'
           return
      end if

      call modspec_getdesc (modspec,nlay,angle,units, &
                            nx,ox,dx,ny,oy,dy,nz,oz,dz)

      if (nlay == 0) then
           whoops = .true.
           msg = 'STATCLASSES: no horizons on modspec file'
           return
      end if

      whoops = .false.
      msg = 'STATCLASSES: successfully read modspec file'

      call modspec_get_hdrs (modspec,hx,hy,hz)

      stattype = 'modspec'
      nhx1 = hx
      nhy1 = hy
      nhx2 = 0
      nhy2 = 0
      x1   = ox
      y1   = oy
      xinc = dx
      yinc = dy

      has_coef = modspec_get_coef(modspec,a,b,xo,yo,dx2,dy2,xgo,ygo)

      if (has_coef) then
           x1   = xo
           y1   = yo
           xinc = dx2
           yinc = dy2
      end if

      write (obj%lunprint,*) ' '
      write (obj%lunprint,*) 'modspec file = ',trim(modfile)
      write (obj%lunprint,*) 'x and y header words        = ',nhx1,nhy1
      write (obj%lunprint,*) 'first x and y coordinates   = ',x1,y1
      write (obj%lunprint,*) 'x and y coord increments    = ',xinc,yinc
      write (obj%lunprint,*) 'last x and y coordinates    = ',x1+(nx-1)*xinc, &
                                                              y1+(ny-1)*yinc
      write (obj%lunprint,*) 'number of x and y locations = ',nx,ny
      write (obj%lunprint,*) 'number of layers            = ',nlay
      write (obj%lunprint,*) 'coefficients a1,a2,a3       = ',a
      write (obj%lunprint,*) 'coefficients b1,b2,b3       = ',b
      write (obj%lunprint,*) ' '

      nullify (z)
      call modspec_get_z (modspec,z)

      write (obj%lunprint,*) &
       '   ix   iy       xgrid      ygrid     values for layers 1 thru', &
                                      min(nlay,4)
      do iy = 1,ny,ny-1
      do ix = 1,nx,nx-1
      write (obj%lunprint,1000) ix,iy,x1+(ix-1)*xinc,y1+(iy-1)*yinc, &
                                (z(iclass,ix,iy),iclass=1,min(nlay,4))
1000  format (1x,2i5,1x,2f11.1,1x,4f11.3)
      end do
      end do
      write (obj%lunprint,*) ' '

      call statclasses_init (obj,nlay)

      do iclass = 1,obj%nclass
           statclass => obj%classes(iclass)%statclass
           statics => z(iclass,:,:)
           call statclass_put_static_values                             &
                               (statclass,stattype,nhx1,nhy1,nhx2,nhy2, &
                                x1,y1,xinc,yinc,nx,ny,statics)
      end do

      call modspec_delete (modspec)

      end subroutine statclasses_read_modspec_file


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module statclasses_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

