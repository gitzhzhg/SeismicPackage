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
! C P S   P R I M I T I V E
!
! Name       : modgmem
! Category   : velocity
! Written    : 2008-01-29   by: R.S.Day
! Revised    : 2008-01-29   by: R.S.Day
! Maturity   : beta
! Purpose    : Manipulate memory resident cartesian velocity grids 
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!  Utility module to manipulate 3D cartesian coordinate data for gridded
!  velocity objects. This module was extracted from the modgrid module which
!  was getting too unweildly. It only manipulates the porion of the object
!  which is memory resident.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!-------------------------------------------------------------------------------
!                             O   I     I      I I I       I 
!     status = modgmem_create(obj,pname,punits,n,o,dodata, msg)
!      type(prop_memstore_struct),pointer  :: obj
!      character(len=*),intent(in)   :: pname  !property name
!      character(len=*),intent(in)   :: punits !property units
!      integer,intent(in)   :: n(:)     !number of grid points
!      integer,intent(in)   :: o(:)     !origin coordinate
!      logical,intent(in)   :: dodata   !flag to control data allocation
!      character(len=*),intent(out)   :: msg  !message buffer
!
!</calling_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2008-01-29  RSDay        Initial version
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
!                           PROGRAMMING NOTES
!
! Notes:
!  
!
!-------------------------------------------------------------------------------
!</programming_doc>
                                                                                                      
      module modgmem_module



      implicit none
      private

      ! Data storage object for the memory storage of the grid data.
      type,public :: prop_memstore_struct
        private
        character(len=64) :: pname        !attribute name(e.g. velocity)
        character(len=16) :: punits       !attribute units(e.g. m/s)
        integer           :: bits         !8,16,32
        integer           :: axis         !slices normal to axis in memory
        integer           :: org(3)       !start index of dim1,dim2,dim3
        integer           :: dim(3)       !dimensions 
        real,pointer      :: data(:)      !buffer holding slices
        double precision,pointer    :: headers(:)   !optional buffer
        logical           :: is_inverted  !data state flag
        logical           :: owns_data    !data ownership flag
      end type prop_memstore_struct

      public :: modgmem_create
      public :: modgmem_initialize
      public :: modgmem_alloc
      public :: modgmem_delete
      public :: modgmem_use_data
      public :: modgmem_free_data
      public :: modgmem_tostring
      public :: modgmem_print

      public :: modgmem_set_pname
      public :: modgmem_pname
      public :: modgmem_set_punits
      public :: modgmem_punits
      public :: modgmem_set_org
      public :: modgmem_get_org
      public :: modgmem_set_dim
      public :: modgmem_get_dim
      public :: modgmem_set_inverted
      public :: modgmem_inverted
      public :: modgmem_data
      public :: modgmem_owns
      public :: modgmem_size
      public :: modgmem_data_size

      public :: modgmem_get_indeces
      public :: modgmem_get_pointsr
      public :: modgmem_get_slicer
      public :: modgmem_get_data_mem3
      public :: modgmem_get_trslice
      public :: modgmem_rep_pointsr
      public :: modgmem_rep_datar
      public :: modgmem_get_data_mem
      public :: modgmem_get_subvol

      ! math operations and analysis
      public :: modgmem_data_stats
      public :: modgmem_muladd
      public :: modgmem_vec_scale
      public :: modgmem_clip
      public :: modgmem_set_obj_vals  !controller and target objects
      public :: modgmem_set_data_vals !set values by controller object
      public :: modgmem_set_data_vals_rand !set random values by control object
      public :: modgmem_paint_data_by_uvw
      public :: modgmem_set_const     !set constant everywhere
      public :: modgmem_iset_gradvw   !grad set at (ivals(:),v,w)
      public :: modgmem_set_grad      !grad set at n v,w range
      public :: modgmem_invert        !take reciprocal of data
      public :: modgmem_has_zero_values
      public :: modgmem_has_nil_values
      public :: modgmem_has_data
      public :: modgmem_minval
      public :: modgmem_maxval
      public :: modgmem_1stval
      public :: modgmem_val_domain
      public :: modgmem_1stvals
      public :: modgmem_data_avg
      public :: modgmem_xyz_to_uvw    !compute u,v,w from x,y,z
      public :: modgmem_uvw_to_xyz
      public :: modgmem_match_data

     character(len=100),save,public :: modgmem_ident = &
     "$Id: modgmem.f90,v 1.1 2008/01/30 15:06:05 RSDay beta sps $"
       contains
      
      integer function modgmem_create(obj,pname,punits,n,o,dodata,&
        msg) result(status)
       type(prop_memstore_struct),pointer  :: obj
       character(len=*),intent(in)   :: pname
       character(len=*),intent(in)   :: punits
       integer,intent(in)   :: n(:)
       integer,intent(in)   :: o(:)
       logical,intent(in)   :: dodata   !flag to control data allocation
       character(len=*),intent(out)   :: msg
       integer(kind=8)      :: m
       integer  :: i_err
       integer  :: i
       status = -1
       nullify(obj)
       allocate(obj,stat=i_err)
       if(i_err/=0) then
          msg = 'modgmem_create: obj allocation error'
          return
       endif
       nullify(obj%data)
       nullify(obj%headers)
       status =  modgmem_initialize(obj,pname,punits,3,n,o,.true.,.false.)
       if(dodata) then
         m = 1
         do i =1,size(n)
           m = m*n(i)
         enddo
         allocate(obj%data(m),stat=i_err)
         if(i_err/=0) then
            status = -1
            msg = 'modgmem_create: data allocation error'
            return
         endif
         status =  modgmem_initialize(obj,pname,punits,3,n,o,.true.,.false.)
       endif
       call random_seed()
 
      return
      end function modgmem_create

       integer function modgmem_initialize(obj,pname,punits,axis,n,o,&
        owns_data, inverted, data) result(status)
       type(prop_memstore_struct),intent(inout)  :: obj
       character(len=*),intent(in)   :: pname
       character(len=*),intent(in)   :: punits
       integer,intent(in)   :: axis
       integer,intent(in)   :: n(:)
       integer,intent(in)   :: o(:)
       logical,intent(in)   :: owns_data
       logical,intent(in)   :: inverted
       real,optional,intent(in)   :: data(:)
       integer      :: i

       status = 0
       obj%pname = pname
       obj%punits = punits
       obj%dim(1) = max(1,n(1))
       obj%org(1) = max(1,o(1))
       obj%dim(2) = max(1,n(2))
       obj%org(2) = max(1,o(2))
       obj%dim(3) = max(1,n(3))
       obj%org(3) = max(1,o(3))
       obj%bits = 32
       obj%axis = axis
       obj%owns_data = owns_data
       obj%is_inverted = inverted
       if(present(data)) then
         if(associated(obj%data)) then
           obj%data = 0.
           do i = 1,min(size(data),size(obj%data))
             obj%data(i) = data(i)
           enddo
         endif
       else
         if(associated(obj%data)) then
           obj%data = 0.
         endif
       endif
 
       return
       end function modgmem_initialize
 
       integer function modgmem_alloc(obj,nelements) result(status)
       type(prop_memstore_struct),pointer  :: obj
       integer,intent(in)   :: nelements
       integer  :: i_err
       status = -1
       i_err = 0
       if(associated(obj)) then
         if(associated(obj%data)) then
           if(nelements < size(obj%data)) then
             i_err = 0
           else
             if(obj%owns_data) deallocate(obj%data)
             nullify(obj%data)
             allocate(obj%data(nelements),stat=i_err)
             if(i_err/=0) then
               print *,'modgmem_alloc: error- nelements=',nelements
             endif
           endif 
         else
           allocate(obj%data(nelements),stat=i_err)
           if(i_err/=0) then
             print *,'modgmem_alloc: data not allocated?'
           endif
         endif
       else
         print *,'modgmem_alloc: obj not allocated?'
         return
       endif
       if(i_err == 0) then
         obj%data=0
         obj%is_inverted=.false.
         status = nelements
       endif
       return
       end function modgmem_alloc

      ! use the data buffer supplied by the user
      integer function modgmem_use_data(obj,pname,punits,n1,n2,sslice,nslice,&
       data) result(status)
       type(prop_memstore_struct),pointer  :: obj
       type(prop_memstore_struct),pointer  :: memdata
      integer,intent(in)                 :: n1,n2,sslice,nslice
      real,target                        :: data(n1*n2*nslice)
      integer                            :: ierr
      character(len=*),intent(in)        :: pname
      character(len=*),intent(in)        :: punits
      status = -1
      nullify(memdata)
      allocate(memdata, stat=ierr)
      if(ierr/=0) then
        print *, 'modgrid_use_data: allocate error'
        return
      endif
      nullify (memdata%headers) ! jpa
      memdata%pname  = pname
      memdata%punits = punits
      if(associated(obj)) then
        if(associated(obj%data) .and. obj%owns_data) then
         deallocate(obj%data)
         deallocate(obj)
         nullify(obj)
        endif
      endif

      memdata%bits   = 32
      memdata%axis   = 3 !axis   !slices normal to axis in memory
      memdata%dim(1)   = n1     !1st dimension of slice
      memdata%dim(2)   = n2     !2nd dimension of slice
      memdata%dim(3)   = nslice !number of slices in memory
      memdata%org(1)   = 1
      memdata%org(2)   = 1
      memdata%org(3)   = sslice !starting slice in memory
      memdata%data   =>data
      memdata%owns_data = .false.
      obj => memdata
      status = 0
      return
      end function modgmem_use_data


      integer function modgmem_delete(obj) result(status)
      type(prop_memstore_struct),pointer  :: obj
      integer                :: ierr
      status = -1
      if(associated(obj)) then
        status = modgmem_free_data(obj)
        if(status .ne.0) return
        deallocate(obj, stat=ierr)
      endif
      nullify(obj)
      status = 0
      return
      end function modgmem_delete

      integer function modgmem_print(obj, stdo, nil) result(status)
      type(prop_memstore_struct),pointer  :: obj
      integer,intent(in)              :: stdo
      real,intent(in)                 :: nil
      character(len=1024)  :: ascrep
      
      status = -1
      if(stdo <=0) return
      ascrep = ' '
      status = modgmem_tostring(obj, nil, ascrep)
      if(status==0) then
        write(stdo,*) trim(ascrep)
      else
        write(stdo,*) 'modgmem_print: problem?'
      endif
      return
      end function modgmem_print

      integer function modgmem_tostring(obj, nil, ascrep) result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)                 :: nil
      character(len=*),intent(inout)  :: ascrep
      character(len=104) :: card
      integer  :: i_err
      real,pointer  :: a(:),b(:),davg(:)
      real          :: dbar
 
      status = 0
      ascrep = ' '
      if(associated(obj)) then
        write(card,*) '#modgmem: The modgrid object is holding data'
        ascrep=trim(ascrep)//' '//trim(card)//char(10)
        write(card,*) '#modgmem: bits=',obj%bits
        ascrep=trim(ascrep)//' '//trim(card)//char(10)
        write(card,*) '#modgmem: axis=',obj%axis
        ascrep=trim(ascrep)//' '//trim(card)//char(10)
        write(card,*) '#modgmem: dim1=',obj%dim(1),&
         ' org1=',obj%org(1)
        ascrep=trim(ascrep)//' '//trim(card)//char(10)
        write(card,*) '#modgmem: dim2=',obj%dim(2),&
         ' org2=',obj%org(2)
        ascrep=trim(ascrep)//' '//trim(card)//char(10)
        write(card,*) '#modgmem: dim3=',obj%dim(3),&
         ' org3=',obj%org(3)
        ascrep=trim(ascrep)//' '//trim(card)//char(10)
        if(associated(obj%data)) then
          allocate(a(obj%dim(3)), stat=i_err)
          allocate(b(obj%dim(3)), stat=i_err)
          allocate(davg(obj%dim(3)), stat=i_err)
          i_err = modgmem_data_stats(obj,nil,a,b,davg,dbar)
          if(i_err>=0) then
            write(card,*) '#modgmem: data min=',minval(a(1:i_err)),&
            ' data max=',maxval(b(1:i_err)),' average=',dbar
            ascrep=trim(ascrep)//' '//trim(card)//char(10)
          endif
          deallocate(a)
          deallocate(b)
          deallocate(davg)
        endif
      else
        write(card,*) '#modgmem: The modgrid object has no data'
        ascrep=trim(ascrep)//' '//trim(card)//char(10)
      endif
  
      return
      end function modgmem_tostring

      integer function modgmem_data_stats(obj,nil,dmin,dmax,davg,dbar)&
      result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)                 :: nil
      real,intent(out)                :: dmin(:),dmax(:)
      real,intent(out)                :: davg(:)
      real,intent(out)                :: dbar

      integer           :: i,j,k
      integer           :: cp
      integer           :: n12,n1,n2
      real,pointer      :: data(:)
      real              :: d1,d2,db
      status = -1
      if(.not. associated(obj)) return
      if(.not. associated(obj%data)) return
      data => obj%data
      dmin = nil
      dmax = nil
      
      dbar = 0.0
      cp = 1  ! current grid point
      n12 = obj%dim(1)*obj%dim(2)
      n1  = obj%dim(1)
      n2  = obj%dim(2)
      do k=1,obj%dim(3)
        d1 = data(cp)
        d2 = data(cp)
        db = 0.0
        do j=1,obj%dim(2)
          do i=1,obj%dim(1)

            if(data(cp) .ne. nil) then
              if(d1==nil) d1=data(cp)
              d1 = min(d1,data(cp))
              d2 = max(d2,data(cp))
              db = db + data(cp)
            endif
            cp = cp + 1
          enddo
        enddo
        db = db / (n12)
        dmin(k) = d1
        dmax(k) = d2
        davg(k) = db
        dbar = dbar + db
      enddo
      dbar = dbar/obj%dim(3)
      status = obj%dim(3)
      return
      end function modgmem_data_stats


      integer function modgmem_free_data(obj) result(status)
      type(prop_memstore_struct),pointer  :: obj
      integer                :: ierr
      status = -1
      if(associated(obj)) then
        if(associated(obj%data)) then
          if(obj%owns_data) then
            deallocate(obj%data,stat=ierr)
            if(ierr/=0) then
              return
            endif
          endif
          nullify(obj%data)
        endif
        if(associated(obj%headers)) then
          deallocate(obj%headers, stat=ierr)
          if(ierr/=0) then
            return
          endif
          nullify(obj%headers)
        endif
      endif
      status = 0
      return
      end function modgmem_free_data

      subroutine modgmem_set_punits(obj,punits)
      type(prop_memstore_struct),intent(inout) :: obj
      character(len=*),intent(in)        :: punits
      obj%punits = punits
      return
      end subroutine modgmem_set_punits

      function modgmem_punits(obj) result(punits)
      type(prop_memstore_struct),intent(in) :: obj
      character(len=32) :: punits  
      punits = obj%punits
      return
      end function modgmem_punits
 
      subroutine modgmem_set_pname(obj,pname)
      type(prop_memstore_struct),intent(inout) :: obj
      character(len=*),intent(in)        :: pname
      obj%pname = pname
      return
      end subroutine modgmem_set_pname

      function modgmem_pname(obj) result(pname)
      type(prop_memstore_struct),intent(in) :: obj
      character(len=32) :: pname
      pname = obj%pname
      return
      end function modgmem_pname

      logical function modgmem_inverted(obj) result(is_inverted)
      type(prop_memstore_struct),intent(in) :: obj
      is_inverted = obj%is_inverted
      return
      end function modgmem_inverted

      subroutine modgmem_set_inverted(obj,iflag)
      type(prop_memstore_struct),intent(inout) :: obj
      logical,intent(in)   :: iflag
      obj%is_inverted = iflag
      return
      end subroutine modgmem_set_inverted

      logical function modgmem_owns(obj) result(owns)
      type(prop_memstore_struct),intent(in) :: obj
      owns = obj%owns_data
      return
      end function modgmem_owns

      function modgmem_data(obj) result(data)
      type(prop_memstore_struct),pointer :: obj
      real,pointer :: data(:)
      nullify(data)
      if(.not.associated(obj)) then
        return
      endif
      data => obj%data(:)
      return
      end function modgmem_data

      subroutine modgmem_set_org(obj,org)
      type(prop_memstore_struct),pointer :: obj
      integer,intent(in)    :: org(:)
      integer   :: i
      if(.not.associated(obj)) return
      do i = 1,size(org)
        if(i==1) obj%org(1) = org(1)
        if(i==2) obj%org(2) = org(2)
        if(i==3) obj%org(3) = org(3)
      enddo
      return
      end subroutine modgmem_set_org

      integer function modgmem_get_org(obj,axis) result(org)
      type(prop_memstore_struct),pointer :: obj
      integer,intent(in)   :: axis
      org=0
      if(.not.associated(obj)) return
      if(axis==1) org = obj%org(1)
      if(axis==2) org = obj%org(2)
      if(axis==3) org = obj%org(3)
      return
      end function modgmem_get_org

      subroutine modgmem_set_dim(obj,dim)
      type(prop_memstore_struct),pointer :: obj
      integer,intent(in)    :: dim(:)
      integer   :: i
      if(.not.associated(obj)) return
      do i = 1,size(dim)
        if(i==1) obj%dim(1) = dim(1)
        if(i==2) obj%dim(2) = dim(2)
        if(i==3) obj%dim(3) = dim(3)
      enddo
      return
      end subroutine modgmem_set_dim

      integer function modgmem_get_dim(obj,axis) result(dim)
      type(prop_memstore_struct),pointer :: obj
      integer,intent(in)   :: axis
      dim=0
      if(.not.associated(obj)) return
      if(axis==1) dim = obj%dim(1)
      if(axis==2) dim = obj%dim(2)
      if(axis==3) dim = obj%dim(3)
      return
      end function modgmem_get_dim

      function modgmem_size(obj) result(msize)
      type(prop_memstore_struct),pointer :: obj
      integer(kind=8)       :: msize
      msize = 0
      if(.not.associated(obj)) return
      msize = obj%dim(1)
      msize = msize*obj%dim(2)
      msize = msize*obj%dim(3)
      return
      end function modgmem_size

      function modgmem_data_size(obj) result(msize)
      type(prop_memstore_struct),pointer :: obj
      integer(kind=8)       :: msize
      msize = 0
      if(.not.associated(obj)) return
      if(.not.associated(obj%data)) return
      msize = size(obj%data)
      return
      end function modgmem_data_size

      !copy a sub volume of the data to subvol
      !ss(i) ... starting indices
      !ns(i) ... number of point along each of u,v,w axis
      integer function modgmem_get_subvol(obj,subvol,ss,ns ) result(status)
      type(prop_memstore_struct),pointer :: obj
      real,intent(out)                   :: subvol(:)
      integer,intent(in)                 :: ss(:)
      integer,intent(inout)              :: ns(:)
      integer      :: u,v,w
      integer      :: iu,iv,iw
      integer      :: se(3)
      integer      :: m,k,ntodo

      status = -1
      iu = ss(1) - obj%org(1) + 1
      iv = ss(2) - obj%org(2) + 1
      iw = ss(3) - obj%org(3) + 1
      se(1) = min(obj%dim(1),ss(1) + ns(1) -1)
      se(2) = min(obj%dim(2),ss(2) + ns(2) -1)
      se(3) = min(obj%dim(3),ss(3) + ns(3) -1)
      ns(1) = se(1)-ss(1) + 1
      ns(2) = se(2)-ss(2) + 1
      ns(3) = se(3)-ss(3) + 1
      if(iu < 1 .or. se(1)>ss(1)+ns(1)-1) return
      if(iv < 1 .or. se(2)>ss(2)+ns(2)-1) return
      if(iw < 1 .or. se(3)>ss(3)+ns(3)-1) return
      ntodo = ns(1)*ns(2)*ns(3)
      if(size(subvol) < ntodo) then !output array too small for request
        status = -2
        return
      endif
      k = 0
      do w = ss(3),se(3)
        m= (w-1)*obj%dim(1)*obj%dim(2)
        do v = ss(2),se(2)
           m = m + (v-1)*obj%dim(1)
           do u = ss(1),se(1)
             k = k+1
             subvol(k) = obj%data(m+u)
          enddo
        enddo
      enddo
      status = 0
      return
      end function modgmem_get_subvol

      ! u,v,w are indices into the full cube
      ! only select strides are allowed
      integer function modgmem_get_pointsr(obj,data,npts,axis,stride,&
       u,v,w) result(status)
      type(prop_memstore_struct),pointer :: obj
      real,intent(out)                   :: data(:)
      integer,intent(inout)              :: npts
      integer,intent(in)                 :: axis
      integer,intent(in)                 :: stride !for axis 1 only
      integer,intent(in)                 :: u,v,w
      integer      :: n1,n2,n3,i1,i2
      integer      :: iu,iv,iw
      integer      :: inc
      integer      :: i
      status = -1
      if(.not. modgmem_has_data(obj) ) return
      if(stride<1) return
      n1 = obj%dim(1)
      n2 = obj%dim(2)
      n3 = obj%dim(3)
      iu = u - obj%org(1) + 1
      iv = v - obj%org(2) + 1
      iw = w - obj%org(3) + 1
      if(n3==1) iw = 1
     !if(obj%rank<3) iw = 1
      if(iu<1 .or. iu>n1) then
        status = -4
        return !u is out of core
      endif
      if(iv<1 .or. iv>n2) then
        status = -4
        return !v is out of core
      endif
      if(iw<1 .or. iw>n3) then
        status = -4
        return !w is out of core
      endif
      i1 = n1*n2*(iw-1)+ (iv-1)*n1 + iu   !sequential start index
      ! stride is along axis 1
      if(axis == 1) then
        if(stride == 1 ) then
          npts = min(npts,n1-iu+1)
          if(npts > size(data)) return    !out buffer too small
          data(1:npts) = obj%data(i1:i1+npts-1)
        else
          i2 = min(i1+stride*npts-1,n1*n2*n3)
          npts = 1 + (i2-i1)/stride
          if(npts > size(data)) then
             return    !out buffer too small
          endif
          do i = 1,npts
            data(i) = obj%data(i1)
            if(iu  > n1) then
             npts = 1
             exit
            endif
            i1 = i1 + stride
            iu = iu + stride
          enddo
        endif
        status = 0
        return
      endif
      ! stride is along axis 2 so memory stride=n1
      if(axis == 2) then
        npts = min(npts,n2-iv+1)
        if(npts > size(data)) then
             return      !out buffer too small
        endif
        do i = 1,npts
          data(i) = obj%data(i1)
          i1 = i1+n1
        enddo
        status = 0
        return
      endif
      ! stride is along axis 3 so memory stride = n1*n2
      !if(stride == obj%n_grid(1)*obj%n_grid(2) ) then
      if(axis == 3) then
        npts = min(npts,n3-iw+1)
        if(npts > size(data)) then      !out buffer too small
             return    !out buffer too small
        endif
        inc = n1*n2
        do i = 1,npts
          data(i) = obj%data(i1)
          i1 = i1+inc
        enddo
        status = 0
        return
      endif
      return
      end function modgmem_get_pointsr

      ! return ivals, the axis indeces where stored data matches tval
      ! npts is the returned number of points in ivals. The values
      ! are the integers along the appropriate axis u or v or w
      ! search starts at pt (u,v,w)
      ! u,v,w are indices into the full cube
      integer function modgmem_get_indeces(obj,ivals,npts,tval, axis,&
       nil, u,v,w) result(status)
      type(prop_memstore_struct),pointer :: obj
      integer,intent(out)                :: ivals(:)
      integer,intent(inout)              :: npts
      integer,intent(in)                 :: axis
      real,intent(in)                    :: tval !target value
      real,intent(in)                    :: nil
      integer,intent(in)                 :: u,v,w
      integer      :: n1,n2,n3,i1
      integer      :: iu,iv,iw
      integer      :: inc
       integer     :: cnt
      integer      :: i
      status = -1
      ivals = 0
      if(.not. associated(obj)) then
        status = -2
        return  !no data
      endif
      if(.not.associated(obj%data)) then
        status = -3
        return  !no data
      endif
      n1 = obj%dim(1)
      n2 = obj%dim(2)
      n3 = obj%dim(3)
      iu = u - obj%org(1) + 1
      iv = v - obj%org(2) + 1
      iw = w - obj%org(3) + 1
      if(n3==1) iw = 1
     !if(obj%rank<3) iw = 1
      if(iu<1 .or. iu>n1) then
        status = -4
        print *,'modgmem_get_indeces: iw=',iu,iv,iw,u,v,w
        print *,'modgmem_get_indeces: org=',obj%org(1),obj%org(2),obj%org(3)
        print *,'modgmem_get_indeces: n=',n1,n2,n3
        return !u is out of core
      endif
      if(iv<1 .or. iv>n2) then
        status = -4
        print *,'modgmem_get_indeces: iw=',iu,iv,iw,u,v,w
        print *,'modgmem_get_indeces: org=',obj%org(1),obj%org(2),obj%org(3)
        print *,'modgmem_get_indeces: n=',n1,n2,n3
        return !v is out of core
      endif
      if(iw<1 .or. iw>n3) then
        status = -4
        print *,'modgmem_get_indeces: iw=',iu,iv,iw,u,v,w
        print *,'modgmem_get_indeces: org=',obj%org(1),obj%org(2),obj%org(3)
        print *,'modgmem_get_indeces: n=',n1,n2,n3
        return !w is out of core
      endif
      cnt=0
      i1 = n1*n2*(iw-1)+ (iv-1)*n1 + iu   !sequential start index
      if(axis == 1) then
        status = -5
        npts = min(n1,n1-iu+1)
        if(npts > size(ivals)) return    !out buffer too small
        do i=1,npts 
          if(obj%data(i1 + i - 1) .ne.nil) then
            if(abs(obj%data(i1+i-1)-tval) < 0.01) then
              cnt = cnt+1
              ivals(cnt) = u + i - 1
            endif
          endif
        enddo
        npts = cnt
        status = 0
        return
      endif
      ! stride is along axis 2 so memory stride=n1
      if(axis == 2) then
        status = -6
        npts = min(n2,n2-iv+1)
        if(npts > size(ivals)) return      !out buffer too small
        inc = n1
        do i = 1,npts
          if(obj%data(i1) .ne.nil) then
            if(obj%data(i1)==tval) then
              cnt = cnt+1
              ivals(cnt) = v + i - 1
            endif
          endif
          i1 = i1+inc
        enddo
        npts = cnt
        status = 0
        return
      endif
      ! stride is along axis 3 so memory stride = n1*n2
      if(axis == 3) then
        status = -7
        npts = min(n3,n3-iw+1)
        if(npts > size(ivals)) return
        inc = n1*n2
        do i = 1,npts
          if(obj%data(i1) .ne.nil) then
            if(obj%data(i1)==tval) then
              cnt = cnt+1
              ivals(cnt) = w + i - 1
            endif
          endif
          i1 = i1+inc
        enddo
        npts = cnt
        status = 0
        return
      endif
      return
      end function modgmem_get_indeces

      integer function modgmem_get_data_mem3(obj,data,izs,ize)&
      result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,pointer                    :: data(:)
      integer,intent(inout)           :: izs,ize

      status = 0
      izs = 0
      ize = 0
      nullify(data)
      if(.not. associated(obj)) return
      data => obj%data

      izs   = obj%org(3)
      ize   = izs + obj%dim(3) - 1
      status= obj%dim(1)*obj%dim(2)*obj%dim(3)

      return
      end function modgmem_get_data_mem3


      ! Return pointer to a slice of the data along axis 3
      integer function modgmem_get_slicer(obj,data, slice) result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,pointer                       :: data(:)
      integer,intent(in)                 :: slice
      integer   :: n1,n2,i
      status = -1
      nullify(data)
      if(.not. associated(obj)) return
      n1 = obj%dim(1)
      n2 = obj%dim(2)
      if(obj%dim(3)==1) then
        i = 1
      else
        i  = slice - obj%org(3) + 1 !memory index
      endif
      if(i< 1 .or. i > obj%dim(3)) return
      data => obj%data(n1*n2*(i-1)+1:i*n1*n2)
      status = 0
      return

      return
      end function modgmem_get_slicer

! return a copy of a transposed slice
      integer function modgmem_get_trslice(obj, mwrds, datao,&
      slice, dim1, dim2)&
      result(status)
      type(prop_memstore_struct),pointer  :: obj
      integer,intent(in) :: mwrds
      real,intent(out)   :: datao(*)
      integer,intent(in) :: slice
      integer,intent(out):: dim1, dim2
      real,pointer       :: data(:)
      integer      :: j,k, nwrd,i_err
      integer      :: i1,i2, n1,n2,n3

      nullify (data)

      status = -1
      if(.not.associated(obj)) then
        return
      endif
      n1 = obj%dim(1)
      n2 = obj%dim(2)
      n3 = obj%dim(3)
      dim1 = n2
      dim2 = n1
      if(n1*n2>mwrds) return
      if(slice< 1 .or. slice > n3) return
      i_err = modgmem_get_slicer(obj,data, slice)
      if(i_err /= 0) return
      nwrd = n1*n2
      i1 = 1
      do j = 1, n2
        i2 = j
        do k = 1, n1
          datao(i2) = data(i1)
          i2 = i2+n2
          i1 = i1+1
        enddo
      enddo
      status = 0
      return
      end function modgmem_get_trslice

      integer function modgmem_get_data_mem(obj,data,ng,og)&
      result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,pointer                    :: data(:)
      integer,intent(inout)           :: ng(:)
      integer,intent(inout)           :: og(:)


      status = 0
      ng   = 0.0       !if zero implies no data in memory
      og   = 1
      nullify(data)
      if(.not. associated(obj)) return
      data => obj%data


      ng(1) = obj%dim(1)
      ng(2) = obj%dim(2)
      ng(3) = obj%dim(3)
      og(1) = obj%org(1)
      og(2) = obj%org(2)
      og(3) = obj%org(3)
     !og(1) = obj%o_grid(1) + (obj%memdata%org1-1)*obj%d_grid(1)
     !og(2) = obj%o_grid(2) + (obj%memdata%org2-1)*obj%d_grid(2)
     !og(3) = obj%o_grid(3) + (obj%memdata%org3-1)*obj%d_grid(3)
      status= ng(1)*ng(2)*ng(3) !elements in memory

      return
      end function modgmem_get_data_mem



      integer function modgmem_rep_datar(obj,data,n1,n2,sslice,nslice) &
       result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)        ::data(*)
      integer,intent(in)     :: n1,n2,sslice,nslice
      integer                :: i,i1,i2
      status = -1
      if(.not.associated(obj)) return
      if(.not.associated(obj%data)) return
      if(obj%dim(1) /= n1) return
      if(obj%dim(2) /= n2) return
      if(sslice < obj%org(3) .or.&
         sslice+nslice-1 > obj%org(3)+obj%dim(3) -1) return
      if(obj%dim(3)==1 ) then
        i=0   !WAS 1, A bug RSD 11-25-07
      else
        i = sslice - obj%org(3)
      endif
      i1 = i*n1*n2+1
      i2 = i1 + nslice*n1*n2 -1
      obj%data(i1:i2) = data(1:n1*n2*nslice)
      status = 0

      return
      end function modgmem_rep_datar

      integer function modgmem_rep_pointsr(obj,data,npts,axis, stride,u,v,w) &
       result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)                    :: data(*)
      integer,intent(inout)              :: npts
      integer,intent(in)                 :: axis
      integer,intent(in)                 :: stride
      integer,intent(in)                 :: u,v,w
      integer      :: n1,n2,n3,i1,i2
      integer      :: iu,iv,iw
      integer      :: inc
      integer      :: i
      status = -1
      if(.not.associated(obj)) return
      if(.not.associated(obj%data)) return
      if(stride<1) return
      n1 = obj%dim(1)
      n2 = obj%dim(2)
      n3 = obj%dim(3)
      iu = u - obj%org(1) + 1
      iv = v - obj%org(2) + 1
      iw = w - obj%org(3) + 1
      if(obj%dim(3)==1 ) iw = 1
      if(iu<1 .or. iu>n1) return !u is out of core
      if(iv<1 .or. iv>n2) return !v is out of core
      if(iw<1 .or. iw>n3) return !w is out of core
      i1 = n1*n2*(iw-1)+ (iv-1)*n1 + iu
     !if(stride < obj%n_grid(1) ) then
      if(axis==1) then
        if(stride == 1 ) then
          npts = min(npts,n1-iu+1)
          obj%data(i1:i1+npts-1) = data(1:npts)
        else
          i2 = min(i1+stride*npts-1,n1*n2*n3)
          npts = 1 + (i2-i1)/stride
          obj%data(i1:i2:stride) = data(1:npts)
        endif
        status = 0
      endif
      ! stride is along axis 2 so memory stride=n1
     !if(stride == obj%n_grid(1) ) then
      if(axis==2) then
        npts = min(npts,n2-iv+1)
        do i = 1,npts
          obj%data(i1) =  data(i)
          i1 = i1+n1
        enddo
        status = 0
      endif
      ! stride is along axis 3 so memory stride = n1*n2
     !if(stride == obj%n_grid(1)*obj%n_grid(2) ) then
      if(axis==3) then
        npts = min(npts,n3-iw+1)
        inc = n1*n2
        do i = 1,npts
          obj%data(i1) = data(i)
          i1 = i1+inc
        enddo
        status = 0
      endif
      return
      end function modgmem_rep_pointsr
! MATH BEG

      !multiply and add. y = m*F + a
      !nil values of input function are ignored
      integer function modgmem_muladd(obj, nil, scale, add) &
        result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)    :: nil
      real,intent(in)    :: scale
      real,intent(in)    :: add
      integer   :: i,j,k,m
      status = -1
      if(.not. modgmem_has_data(obj) ) return
      status = 0
      m=0
      do k=1,obj%dim(3)
        do j=1,obj%dim(2)
          do i=1,obj%dim(1)
            m = m+1
            if(obj%data(m) == nil) cycle
            obj%data(m) = scale*obj%data(m) + add
          enddo
        enddo
      enddo
      return
      end function modgmem_muladd


      !vector scale for fast axis. Same scaling for all points along axis 2,3
      integer function modgmem_vec_scale(obj, nil, scale) &
        result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)    :: nil
      real,intent(in)    :: scale(:)
      integer   :: i,j,k,m
      status = -1
      if(.not. modgmem_has_data(obj) ) return
      if(size(scale) .ne. obj%dim(1)) return
      status = 0
      m=0
      do k=1,obj%dim(3)
        do j=1,obj%dim(2)
          do i=1,obj%dim(1)
            m = m+1
            if(obj%data(m) == nil) cycle
            obj%data(m) = scale(i)*obj%data(m)
          enddo
        enddo
      enddo
      return
      end function modgmem_vec_scale

      !clip values to min and max range
      !clip_opt > 0, min and max
      !clip_opt = 0, min 
      !clip_opt < 0, max 
      integer function modgmem_clip(obj, nil, clip_min, clip_max,clip_opt) &
        result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)    :: nil
      real,intent(in)    :: clip_max
      real,intent(in)    :: clip_min
      integer,intent(in) :: clip_opt
      real      :: cmin,cmax
      integer   :: i,j,k,m
      status = -1
      if(.not. modgmem_has_data(obj) ) return
      status = 0
      cmin = min(clip_min,clip_max)
      cmax = max(clip_min,clip_max)
      if(clip_opt > 0 ) then   !clip to cmin and cmax
        m=0
        do k=1,obj%dim(3)
          do j=1,obj%dim(2)
            do i=1,obj%dim(1)
              m = m+1
              if(obj%data(m) == nil) cycle
              obj%data(m) = max(cmin,min(cmax, obj%data(m)))
            enddo
          enddo
        enddo
      endif
      if(clip_opt == 0 ) then  !clip to cmin
        m=0
        do k=1,obj%dim(3)
          do j=1,obj%dim(2)
            do i=1,obj%dim(1)
              m = m+1
              if(obj%data(m) == nil) cycle
              obj%data(m) = max(cmin,obj%data(m))
            enddo
          enddo
        enddo
      endif
      if(clip_opt < 0 ) then  !clip to cmax
        m=0
        do k=1,obj%dim(3)
          do j=1,obj%dim(2)
            do i=1,obj%dim(1)
              m = m+1
              if(obj%data(m) == nil) cycle
              obj%data(m) = min(cmax,obj%data(m))
            enddo
          enddo
        enddo
      endif

      return
      end function modgmem_clip

      !set to a constant value
      !nilopt <0, 0, >0 : skip nils, only nils, all values
      integer function  modgmem_set_const(obj, nil, nilopt, const ) &
       result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)    :: nil
      integer,intent(in) :: nilopt
      real,intent(in)    :: const
      integer     :: i,j,k,m
      status = -1
      if(.not. modgmem_has_data(obj) ) return
      status = 0
      if(nilopt <0) then  !ignore nils
        m=0
        do k=1,obj%dim(3)
          do j=1,obj%dim(2)
            do i=1,obj%dim(1)
              m = m+1
              if(obj%data(m) == nil)  cycle
              obj%data(m) = const
            enddo
          enddo
        enddo
      endif
      if(nilopt == 0) then  !process only nils
        m=0
        do k=1,obj%dim(3)
          do j=1,obj%dim(2)
            do i=1,obj%dim(1)
              m = m+1
              if(obj%data(m) .ne. nil)  cycle
              obj%data(m) = const
            enddo
          enddo
        enddo
      endif
      if(nilopt > 0) then  !process nils and non-nils
        m=0
        do k=1,obj%dim(3)
          do j=1,obj%dim(2)
            do i=1,obj%dim(1)
              m = m+1
              obj%data(m) = const
            enddo
          enddo
        enddo
      endif
      return
      end function modgmem_set_const

      ! Set gradient value at (ub:ue,v,w)
      ! nilopt <0, 0, >0 : skip nils, only nils, all values
      ! Gradr = physical gradient. Change per unit distance
      ! Gradu = bin gradient. Change per 1 bin
      ! Gradr = (P2 - P1)/(d2 - d1) = (P2 - P1)/{(u2-u1)*delta_r}
      ! delta_r = size of 1 sample bin
      ! Gradu = Gradr*delta_r = (P2 - P1)/(u2 - u1)    bin gradient
      ! Pi = Pr + (i - ur)*Gradu   ub <= i <= ue
      ! Pi = Pr + (u-ur)*Gu + (v-vr)*Gv + (w-wr)*Gw 
      integer function  modgmem_set_gradvw(obj, nil, nilopt,&
        Pr, Gradu, ur, ub, ue, v, w ) result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)    :: nil
      integer,intent(in) :: nilopt
      real,intent(in)    :: Pr
      real,intent(in)    :: Gradu
      integer,intent(in) :: ur,ub,ue
      integer,intent(in) :: v,w
      integer     :: u
     !integer     :: inc
      integer     :: m        !index of (u,v,w) into memory
      integer     :: iu,iv,iw !coord relative loaded data
      status = -1
      if(.not. modgmem_has_data(obj) ) return
      iv = v - obj%org(2) + 1
      iw = w - obj%org(3) + 1
      if(iw<1 .or. iw>obj%dim(3)) return
      if(iv<1 .or. iv>obj%dim(2)) return
      if(ue<ub ) return
     !axis = 1
     !if(axis==1) then
     !  inc = 1
     !endif
     !if(axis==2) then
     !  inc = obj%dim(1)
     !endif
     !if(axis==3) then
     !  inc = obj%dim(1)*obj%dim(2)
     !endif
      status = 0
      if(nilopt==0) then  !ignore nils
        m = (iw-1)*obj%dim(1)* obj%dim(2) + (iv-1)*obj%dim(1)
        do u=ub,ue
          iu = u - obj%org(1) + 1
          if(obj%data(m+iu) .ne. nil) obj%data(m+iu) = Pr + (u-ur)*Gradu
        enddo
      endif
      if(nilopt < 0) then  !process only nils
        m = (iw-1)*obj%dim(1)* obj%dim(2) + (iv-1)*obj%dim(1)
        do u=ub,ue
          iu = u - obj%org(1) + 1
          if(obj%data(m+iu) == nil) obj%data(m+iu) = Pr + (u-ur)*Gradu
        enddo
      endif
      if(nilopt > 0) then  !process nils and non-nils
        m = (iw-1)*obj%dim(1)* obj%dim(2) + (iv-1)*obj%dim(1)
        do u=ub,ue
          iu = u - obj%org(1) + 1
          obj%data(m+iu) = Pr + (u-ur)*Gradu
        enddo
      endif
      return
      end function modgmem_set_gradvw

      ! Set Gradient at v,w for u values in ivals(:)
      ! Pi = Pr + (i - ur)*Gradu
      ! ur ... reference index for gradient function
      ! Pr ... starting value for gradient function
      integer function  modgmem_iset_gradvw(obj,odata, nil, nilopt,&
        Pr, Gradu,npts, ivals,ur,v,w ) result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(inout) :: odata(:)
      real,intent(in)    :: nil
      integer,intent(in) :: nilopt
      real,intent(in)    :: Pr
      real,intent(in)    :: Gradu
      integer,intent(in) :: npts
      integer,intent(in) :: ivals(:)
      integer,intent(in) :: ur
      integer,intent(in) :: v,w
      integer    :: iu,iv,iw
      integer    :: u,m
      status = -1
      if(.not. modgmem_has_data(obj) ) then
        return
      endif
      iv = v - obj%org(2) + 1
      iw = w - obj%org(3) + 1
      if(iw<1 .or. iw>obj%dim(3)) then
        print *,'modgmem_iset_gradvw: w=',iw,obj%dim(3)
        status = -2
        return
      endif
      if(iv<1 .or. iv>obj%dim(2)) then
        print *,'modgmem_iset_gradvw: v=',v,obj%dim(2)
        status = -2
        return
      endif
      status = 0
      if(nilopt==0) then  !ignore nils
        m = (iw-1)*obj%dim(1)* obj%dim(2) + (iv-1)*obj%dim(1)
        do u=1,npts
          iu =  ivals(u) -obj%org(1) + 1
          if(obj%data(m+iu) .ne. nil) odata(m+iu) = Pr + (ivals(u)-ur)*Gradu
        enddo
      endif
      if(nilopt < 0) then  !process only nils
        m = (iw-1)*obj%dim(1)* obj%dim(2) + (iv-1)*obj%dim(1)
        do u=1,npts
          iu = ivals(u) -obj%org(1) + 1
          if(obj%data(m+iu) .eq. nil) odata(m+iu) = Pr + (ivals(u)-ur)*Gradu
        enddo
      endif
      if(nilopt > 0) then  !process nils and non-nils
        m = (iw-1)*obj%dim(1)* obj%dim(2) + (iv-1)*obj%dim(1)
        do u=1,npts
          iu = ivals(u) -obj%org(1) + 1
          odata(m+iu) = Pr + (ivals(u)-ur)*Gradu
          
        enddo
      endif
      return
      end function modgmem_iset_gradvw

      ! Set value via gradient algorithm for axis u for all v,w
      ! Gradr = physical gradient. Change per unit distance
      ! Gradu = bin gradient. Change per 1 bin
      ! Gradr = (P2 - P1)/(d2 - d1) = (P2 - P1)/{(u2-u1)*delta_r}
      ! delta_r = size of 1 sample bin
      ! Gradu = Gradr*delta_r = (P2 - P1)/(u2 - u1)    bin gradient
      ! Pi = Pr + (i - ir)*Gradu   ib <= i <= ie
      ! nilopt <0, 0, >0 : skip nils, only nils, all values
      integer function  modgmem_set_grad(obj, nil, nilopt,&
        Pr, Gradu, ur, ub, ue ) result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)    :: nil
      integer,intent(in) :: nilopt
      real,intent(in)    :: Pr(:,:)
      real,intent(in)    :: Gradu(:,:)
      integer,intent(in) :: ur
      integer,intent(in) :: ub(:,:)
      integer,intent(in) :: ue(:,:)
      integer     :: v,w
      integer     :: iv,iw
      integer     :: i_err
      integer     :: axis
      status = -1
      if(.not. modgmem_has_data(obj) ) return
      if(size(Pr,1).ne.obj%dim(2)) return
      if(size(Pr,2).ne.obj%dim(3)) return
      status= 0
      i_err = 0
      axis = 1
      if(axis==1) then
      endif
      if(axis==2) then
      endif
      if(axis==3) then
      endif
      do iw=1,obj%dim(3)
        w = obj%org(3) + iw - 1
        do iv=1,obj%dim(2)
          v = obj%org(2) + iv - 1
          if(ub(iv,iw) <1) cycle !use as mask for pts to ignore
          i_err =  i_err + modgmem_set_gradvw(obj, nil, nilopt,&
                   Pr(iv,iw), Gradu(iv,iw), ur,&
                   ub(iv,iw), ue(iv,iw), v, w)
        enddo
      enddo
      if(i_err == 0) status = 0
      return
      end function modgmem_set_grad

      !take reciprocal of all data but nil and zero
      integer function  modgmem_invert(obj, nil, zcnt, ncnt ) &
       result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)    :: nil
      integer,intent(out):: zcnt
      integer,intent(out):: ncnt
      integer     :: i,j,k,m
      status = -1
      if(.not. modgmem_has_data(obj) ) then
        print *,'modgmem_invert: no data to invert'
        return
      endif
      status = 0
      zcnt=0
      ncnt=0
      m=0
      do k=1,obj%dim(3)
        do j=1,obj%dim(2)
          do i=1,obj%dim(1)
            m = m+1
            if(obj%data(m)==0.0) then
              zcnt=zcnt+1
            else if(obj%data(m)==nil) then
              ncnt=ncnt+1
            else
              obj%data(m) = 1.0/obj%data(m)
            endif
          enddo
        enddo
      enddo
      obj%is_inverted = .not.  obj%is_inverted
     !if( obj%memdata%is_inverted) then
     !  obj%memdata%is_inverted = .false.
     !else
     !  obj%memdata%is_inverted = .true.
     !endif
      return
      end function modgmem_invert

      real function modgmem_minval(obj, nil, i_err) result(status)
      type(prop_memstore_struct),pointer  :: obj
      integer,intent(inout)  :: i_err
      real,intent(in)        :: nil
      integer   i,m
      status = nil
      i_err  = -1
      if(.not. modgmem_has_data(obj) ) return
      i_err = 0
      m = obj%dim(3)*obj%dim(2)*obj%dim(1)
      do i = 1,m
        if(obj%data(i) .ne. nil) then
          if(status==nil) status=obj%data(i)
          status = min(status,obj%data(i))
        endif
      enddo
      return
      end function modgmem_minval

      real function modgmem_maxval(obj, nil, i_err) result(status)
      type(prop_memstore_struct),pointer  :: obj
      integer,intent(inout)  :: i_err
      real,intent(in)        :: nil
      integer   i,m
      status = nil
      i_err  = -1
      if(.not. modgmem_has_data(obj) ) return
      i_err = 0
      m = obj%dim(3)*obj%dim(2)*obj%dim(1)
      do i = 1,m
        if(obj%data(i) .ne. nil) then
          if(status==nil) status=obj%data(i)
          status = max(status,obj%data(i))
        endif
      enddo
      return
      end function modgmem_maxval

      integer function  modgmem_1stvals(obj, nil, val,us,v,w) &
       result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)    :: nil
      real,intent(in)    :: val
      integer,intent(inout) :: us(:,:)
      integer,intent(in) :: v,w
      integer     :: i,j,k,m
      real        :: tol
      tol = 0.0001
      status = -1
      us = -1
      if(.not. modgmem_has_data(obj) ) then
        print *,'modgmem_istvals: no data scan'
        return
      endif
      m=0
      do k=1,obj%dim(3)
        do j=1,obj%dim(2)
          do i=1,obj%dim(1)
            m = m+1
            if(obj%data(m).ne.nil) then
              if( abs(obj%data(m)- val) < tol ) then
                us(j,k) = i
              endif
            endif
          enddo
        enddo
      enddo
      status = 0
      return
      end function modgmem_1stvals

      integer function  modgmem_1stval(obj, nil, val,v,w) &
       result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)    :: nil
      real,intent(in)    :: val
      integer,intent(in) :: v,w
      integer     :: i,m
      real        :: tol
      tol = 0.0001
      status = -1
      if(.not. modgmem_has_data(obj) ) then
        print *,'modgmem_istval: no data scan'
        return
      endif
      m=0
      do i=1,obj%dim(1)
        m = (w-1)*obj%dim(1)* obj%dim(2) + (v-1)*obj%dim(1) + i
        if(obj%data(m).ne.nil) then
          if( abs(obj%data(m)- val) < tol ) then
            status = i
            return
          endif
        endif
      enddo
      return
      end function modgmem_1stval

      integer function  modgmem_val_domain(obj, nil, val,v,w,us,ue) &
       result(status)
      type(prop_memstore_struct),pointer  :: obj
      real,intent(in)    :: nil
      real,intent(in)    :: val
      integer,intent(in) :: v,w
      integer,intent(out):: us,ue
      integer     :: u,m
      real        :: tol
      logical     :: found
      tol = val*0.0001
      status = -1
      us = 0
      ue = 0
      found = .false.
      if(.not. modgmem_has_data(obj) ) then
        print *,'modgmem_val_domain: no data scan'
        return
      endif
      status = 0
      m=0
      do u=1,obj%dim(1)
        m = (w-1)*obj%dim(1)* obj%dim(2) + (v-1)*obj%dim(1) + u
        if(found) then
          if(obj%data(m).ne.nil) then
            if( abs(obj%data(m)- val) < tol ) then
              ue = u
            endif
          endif
        else
          if(obj%data(m).ne.nil) then
            if( abs(obj%data(m)- val) < tol ) then
              us = u
              found = .true.
            endif
          endif
        endif
      enddo
      return
      end function modgmem_val_domain

      logical function modgmem_has_zero_values(obj, i_err) result(status)
      type(prop_memstore_struct),pointer  :: obj
      integer,intent(inout)  :: i_err       ! arguments
      integer   i,m
      status = .false.
      i_err = -1
      if(.not. modgmem_has_data(obj) ) return
      i_err = 0
      m = obj%dim(3)*obj%dim(2)*obj%dim(1)
      do i = 1,m
        if(obj%data(i)==0.0) then
          status = .true.
          return
        endif
      enddo
      return
      end function modgmem_has_zero_values

      logical function modgmem_has_nil_values(obj, nil, i_err) result(status)
      type(prop_memstore_struct),pointer  :: obj
      integer,intent(inout)  :: i_err
      real,intent(in)        :: nil
      integer   i,m
      status = .false.
      i_err = -1
      if(.not. modgmem_has_data(obj) ) return
      i_err = 0
      m = obj%dim(3)*obj%dim(2)*obj%dim(1)
      do i = 1,m
        if(obj%data(i)==nil) then
          status = .true.
          return
        endif
      enddo
      return
      end function modgmem_has_nil_values

      logical function modgmem_has_data(obj) result(status)
      type(prop_memstore_struct),pointer  :: obj
      status = .false.
      if(.not.associated(obj)) return
      if(.not.associated(obj%data)) return
      status = .true.
      return
      end function modgmem_has_data

      !return average value for slices perp. to axis
      integer function modgmem_data_avg(obj, axis, nil, avg) &
       result(status)
      type(prop_memstore_struct),pointer  :: obj
      integer,intent(in)              :: axis
      real,intent(in)                 :: nil
      real,intent(out)                :: avg(:)
      integer           :: i,j,k,m,dim1,dim2,dim3
      integer           :: i_err
      integer,pointer   :: cnt(:)
      real,pointer      :: sum(:)
      real,pointer      :: lavg(:)
      status = -1
      if(.not. modgmem_has_data(obj) ) return
      dim1 = obj%dim(1)
      dim2 = obj%dim(2)
      dim3 = obj%dim(3)
      m = max(dim1,dim2,dim3)
      allocate(cnt(m),stat=i_err)
      if(i_err .ne.0) return
      allocate(sum(m),stat=i_err)
      if(i_err .ne.0) return
      allocate(lavg(m),stat=i_err)
      if(i_err .ne.0) return
      sum(1:m) = 0.0
      cnt(1:m) = 0
      lavg(1:m)= 0.0
      if(axis==1) then
        m = 0
        do k=1,dim3
          do j=1,dim2
            do i=1,dim1
              m = m + 1
              if(obj%data(m) .ne. nil) then
                cnt(i) = cnt(i) + 1
                sum(i) = sum(i) + obj%data(m)
              endif
            enddo
          enddo
        enddo
        do i=1,min(size(avg),dim1)
          if(cnt(i)>0) lavg(i) = sum(i)/(cnt(i))
          avg(i) = lavg(i)
        enddo
        status = dim1
      endif
   
      if(axis==2) then
        m = 0
        do k=1,dim3
          do j=1,dim2
            do i=1,dim1
              m = m + 1
              if(obj%data(m) .ne. nil) then
                cnt(j) = cnt(j) + 1
                sum(j) = sum(j) + obj%data(m)
              endif
            enddo
          enddo
        enddo
        do j=1,min(size(avg),dim2)
          if(cnt(j)>0) lavg(j) = sum(j)/(cnt(j))
          avg(j) = lavg(j)
        enddo
        status = dim2
      endif
      if(axis==3) then
        m = 0
        do k=1,dim3
          do j=1,dim2
            do i=1,dim1
              m = m + 1
              if(obj%data(m) .ne. nil) then
                cnt(k) = cnt(k) + 1
                sum(k) = sum(k) + obj%data(m)
              endif
            enddo
          enddo
        enddo
        do k=1,min(size(avg),dim3)
          if(cnt(k)>0) lavg(k) = sum(k)/(cnt(k))
          avg(k) = lavg(k)
        enddo
        status = dim3
      endif
      deallocate(cnt,stat=i_err)
      deallocate(sum,stat=i_err)
      deallocate(lavg,stat=i_err)
      return
      end function modgmem_data_avg

      !set values of tobj to tval when cobj has value cval
      integer function modgmem_set_obj_vals(cobj,cval, tobj, nil,nilopt, tval ) &
       result(status)
      type(prop_memstore_struct),intent(in)    :: cobj !controller
      type(prop_memstore_struct),intent(inout) :: tobj !target
      real,intent(in)    :: nil
      integer,intent(in) :: nilopt
      real,intent(in)    :: cval
      real,intent(in)    :: tval
      integer     :: i,j,k,m
      status = -1
      if(.not. modgmem_ocomensurate(cobj,tobj)) return
      if(.not.associated(cobj%data)) return
      if(.not.associated(tobj%data)) return
      status = 0
      if(nilopt==0) then  !skip nils
        m=0
        do k=1,cobj%dim(3)
          do j=1,cobj%dim(2)
            do i=1,cobj%dim(1)
              m = m+1
              if(cobj%data(m) == nil)  cycle
              if(cobj%data(m) == cval)  then
                tobj%data(m) = tval
              endif
            enddo
          enddo
        enddo
      endif
      if(nilopt < 0) then  !process only nils
        m=0
        do k=1,cobj%dim(3)
          do j=1,cobj%dim(2)
            do i=1,cobj%dim(1)
              m = m+1
              if(cobj%data(m) .ne. nil)  cycle
              if(cobj%data(m) == cval)  then
                tobj%data(m) = tval
              endif
            enddo
          enddo
        enddo
      endif
      if(nilopt > 0) then  !process nils and non-nils
        m=0
        do k=1,cobj%dim(3)
          do j=1,cobj%dim(2)
            do i=1,cobj%dim(1)
              m = m+1
              if(cobj%data(m) == cval)  then
                tobj%data(m) = tval
              endif
            enddo
          enddo
        enddo
      endif
  
      return
      end function modgmem_set_obj_vals

      ! check if grids use identical bins for all components
      logical function modgmem_ocomensurate(mobj1,mobj2) result(isok)
       type(prop_memstore_struct),intent(in) :: mobj1 !controller
       type(prop_memstore_struct),intent(in) :: mobj2 !target

       isok = .true.
       if(mobj1%dim(1) /= mobj2%dim(1) .or. mobj1%dim(2) /= mobj2%dim(2) .or. &
          mobj1%dim(3) /= mobj2%dim(3) ) isok=.false.
       if(mobj1%org(1) /= mobj2%org(1) .or. mobj1%org(2) /= mobj2%org(2) .or. &
          mobj1%org(3) /= mobj2%org(3) ) isok=.false.
      return
      end function modgmem_ocomensurate

      !set values of data to tval when cobj has value cval
      ! data and obj must have same storage order
      integer function modgmem_set_data_vals(cobj,cval, data, nil,&
       nilopt, tval ) result(status)
      type(prop_memstore_struct),intent(in)    :: cobj !controller
      real,intent(in)    :: nil
      integer,intent(in) :: nilopt
      real,intent(in)    :: cval
      real,intent(in)    :: tval
      real,intent(inout) :: data(:)
      real        :: tol
      integer     :: i,j,k,m
      status = -1
      tol = 0.001*cval
      if(size(data) <  cobj%dim(1)*cobj%dim(2)*cobj%dim(3) ) return
      if(.not.associated(cobj%data)) return
      status = 0
      if(nilopt==0) then  !skip nils
        m=0
        do k=1,cobj%dim(3)
          do j=1,cobj%dim(2)
            do i=1,cobj%dim(1)
              m = m+1
              if(cobj%data(m) == nil)  cycle
              if(abs(cobj%data(m)-cval) < tol)  then
                data(m) = tval
              endif
            enddo
          enddo
        enddo
      endif
      if(nilopt < 0) then  !process only nils
        m=0
        do k=1,cobj%dim(3)
          do j=1,cobj%dim(2)
            do i=1,cobj%dim(1)
              m = m+1
              if(cobj%data(m) .ne. nil)  cycle
              if(abs(cobj%data(m)-cval) < tol)  then
                data(m) = tval
              endif
            enddo
          enddo
        enddo
      endif
      if(nilopt > 0) then  !process nils and non-nils
        m=0
        do k=1,cobj%dim(3)
          do j=1,cobj%dim(2)
            do i=1,cobj%dim(1)
              m = m+1
              if(abs(cobj%data(m)-cval) < tol)  then
                data(m) = tval
              endif
            enddo
          enddo
        enddo
      endif
  
      return
      end function modgmem_set_data_vals

      !set values of data to tval when cobj has value cval
      ! data and obj must have same storage order
      integer function modgmem_set_data_vals_rand(cobj,cval, data, nil,&
       nilopt, tval, tdelta ) result(status)
      type(prop_memstore_struct),intent(in)    :: cobj !controller
      real,intent(in)    :: nil
      integer,intent(in) :: nilopt
      real,intent(in)    :: cval
      real,intent(in)    :: tval
      real,intent(in)    :: tdelta
      real,intent(inout) :: data(:)
      real        :: tol,rnum
      integer     :: i,j,k,m
      status = -1
      tol = 0.001*cval
      if(size(data) <  cobj%dim(1)*cobj%dim(2)*cobj%dim(3) ) return
      if(.not.associated(cobj%data)) return
      status = 0
      if(nilopt==0) then  !skip nils
        m=0
        do k=1,cobj%dim(3)
          do j=1,cobj%dim(2)
            do i=1,cobj%dim(1)
              m = m+1
              if(cobj%data(m) == nil)  cycle
              if(abs(cobj%data(m)-cval) < tol)  then
                call random_number(rnum)
                data(m) = tval + (0.5-rnum)*tdelta
              endif
            enddo
          enddo
        enddo
      endif
      if(nilopt < 0) then  !process only nils
        m=0
        do k=1,cobj%dim(3)
          do j=1,cobj%dim(2)
            do i=1,cobj%dim(1)
              m = m+1
              if(cobj%data(m) .ne. nil)  cycle
              if(abs(cobj%data(m)-cval) < tol)  then
                call random_number(rnum)
                data(m) = tval + (0.5-rnum)*tdelta
              endif
            enddo
          enddo
        enddo
      endif
      if(nilopt > 0) then  !process nils and non-nils
        m=0
        do k=1,cobj%dim(3)
          do j=1,cobj%dim(2)
            do i=1,cobj%dim(1)
              m = m+1
              if(abs(cobj%data(m)-cval) < tol)  then
                call random_number(rnum)
                data(m) = tval + (0.5-rnum)*tdelta
              endif
            enddo
          enddo
        enddo
      endif
  
      return
      end function modgmem_set_data_vals_rand


      ! Convenience function
      ! Convert spatial coordinate (x,y,z) to logical coordinate (u,v,w) 
      ! Assumptions:
      !   1. one of u,v,w is aligned with the Z axis
      !   2. the other two axes have no component along Z
      !   3. the uvw space is based upon a regular cartesian grid
      ! oxyz(3) ... X,Y,Z coodinate of (u,v,w) = (1,1,1)
      ! uvw(3,ntodo), xyz(3,ntodo)
      ! zlab    ... indicates which axis is z axis  (1 to 3)
      ! ntodo   ... Number of 3D points to convert
      ! uv(3,3) ... unit vector matrix for du,dv,dw
      !             uv(xyz comp, uvw axis)
      !                   |dUx, dVx, dWx|
      !             uv =  |dUy  dVy  dWy|
      !                   |dUz  dVz  dWz|
      ! .e.g. when zlab=3
      ! {X-X0}  | a11  a12 | {u-1}            |a22    -a12|
      ! |    |= |          | |   |   Inv(M) = |           |  /det.
      ! {Y-Y0}  | a21  a22 | {v-1}            |-a21    a11|
      integer function modgmem_xyz_to_uvw(uv, oxyz, zlab,&
       ntodo, xyzs,uvws) result(status)
      real,intent(in)                 :: uv(3,3)
      real,intent(in)                 :: oxyz(:)
      real,intent(in)                 :: xyzs(:,:) !X(:),Y(:),Z(:)
      real,intent(out)                :: uvws(:,:) !u(:),v(:),w(:)
      integer,intent(in)              :: ntodo
      integer,intent(in)              :: zlab
      real      :: a11,a22,a12,a21
      real      :: idz, idet
      real      :: delx,dely,delz
      integer   :: i
      status = -1

      uvws = 0
      ! general, can further simplify when u,v,w align with x,y,z
      if(zlab == 1) then ! z axis = u axis
        a11 = uv(1,2)   !x,v
        a12 = uv(1,3)   !x,w
        a21 = uv(2,2)   !y,v
        a22 = uv(2,3)   !y,w
        idet= (a11*a22 - a12*a21)
        if(idet==0) then
          status = -2
          return
        endif
        idet= 1.0/idet
        idz = 1.0/uv(3,1)
        status = 0
      endif
      if(zlab == 2) then ! z axis = v axis
        a11 = uv(1,1)
        a12 = uv(1,3)
        a21 = uv(2,1)
        a22 = uv(2,3)
        idet= (a11*a22 - a12*a21)
        if(idet==0) then
          status = -3
          return
        endif
        idet= 1.0/idet
        idz = 1.0/uv(3,2)
        status = 0
      endif
      if(zlab == 3) then ! z axis = w axis
        a11 = uv(1,1)
        a12 = uv(1,2)
        a21 = uv(2,1)
        a22 = uv(2,2)
        idet= (a11*a22 - a12*a21)
        if(idet==0) then
          status = -4
          return
        endif
        idet= 1.0/idet
        idz = 1.0/uv(3,3)
        status = 0
      endif
      if(status < 0) return
      a11 = a11*idet  !divide by determinant
      a12 = a12*idet
      a21 = a21*idet
      a22 = a22*idet
      do i = 1,min(size(xyzs,2),ntodo)
        delx= xyzs(1,i) - oxyz(1)
        dely= xyzs(2,i) - oxyz(2)
        delz= xyzs(3,i) - oxyz(3)
        uvws(1,i) = 1.0 + (a22*delx - a12*dely)
        uvws(2,i) = 1.0 + (a11*dely - a21*delx)
        uvws(3,i) = 1.0 + delz * idz
      enddo

      return
      end function modgmem_xyz_to_uvw

      integer function modgmem_uvw_to_xyz(uv, oxyz, ntodo, xyzs, uvws) &
       result(status)
      real,intent(in)                 :: uv(3,3)
      real,intent(in)                 :: oxyz(:)
      real,intent(out)                :: xyzs(:,:) !X(:),Y(:),Z(:)
      real,intent(in)                 :: uvws(:,:) !u(:),v(:),w(:)
      integer,intent(in)              :: ntodo
      real      :: delu,delv,delw
      integer   :: i
      status = -1

      xyzs = 0
      do i = 1,min(size(xyzs,2),ntodo)
        delu= uvws(1,i) - 1.0
        delv= uvws(2,i) - 1.0
        delw= uvws(3,i) - 1.0
        xyzs(1,i) = oxyz(1) + delu*uv(1,1) + delv*uv(1,2) + delw*uv(1,3)
        xyzs(2,i) = oxyz(2) + delu*uv(2,1) + delv*uv(2,2) + delw*uv(2,3)
        xyzs(3,i) = oxyz(3) + delu*uv(3,1) + delv*uv(3,2) + delw*uv(3,3)
      enddo
      status = 0
      return
      end function modgmem_uvw_to_xyz

      ! Set values of data(u,v,w) to tval(k)  where (u,v,w)=uvws(:,k)
      ! data(n1,n2,n3)
      ! uvws may be indexed into a larger volume. The o1,o2,o3 indicate
      ! the start index in memory
      ! data and obj must have same storage order
      integer function modgmem_paint_data_by_uvw(n1,n2,n3,o1,o2,o3,&
       npts,data,  uvws, pvals ) result(status)
      integer,intent(in) :: n1,n2,n3
      integer,intent(in) :: o1,o2,o3
      integer,intent(in) :: npts
      real,intent(inout) :: data(n1,n2,n3)
      real,intent(in)    :: uvws(:,:)
      real,intent(in)    :: pvals(:)
      integer     :: k
      integer     :: u,v,w
      status = -1
      if(npts < 1) return
     !print *,'modgmem_paint: DBG0',n1,n2,n3,o1,o2,o3
     !print *,'modgmem_paint: DBG1',data(1,1,1)
     !print *,'modgmem_paint: DBG2',pvals(1), size(pvals)
      do k= 1,npts
          u = uvws(1,k) - o1 + 1
          v = uvws(2,k) - o2 + 1
          w = uvws(3,k) - o3 + 1
          if(u<1 .or.u> n1) cycle
          if(v<1 .or.v> n2) cycle
          if(w<1 .or.w> n3) cycle
         !if(k> (n1*n2-200) .and. mod(k,5)==0) then
         !if(k> (n1*n2-20) ) then
         !  print *,'modgmem_paint: DBG2',k,u,v,w
         !endif
          data(u,v,w) = pvals(k)
      enddo
     !print *,'modgmem_paint: DBG3'
      status = 0
      return
      end function modgmem_paint_data_by_uvw

      ! return u,v,w coordinate for all matches to tval
      subroutine modgmem_match_data(obj,tval,uvws,num)  
      type(prop_memstore_struct),intent(in)    :: obj !controller
      real,intent(in)   :: tval
      integer,intent(inout) :: uvws(:,:)
      integer,intent(out)   :: num
      integer  :: muvw
      integer  :: u,v,w
      integer  :: mlim,ival
      
        num = 0
        muvw = 0
        ival = nint(tval)
        mlim = size(uvws,2)
         do w = 1,obj%dim(3)
           do v = 1,obj%dim(2)
             do u = 1,obj%dim(1)
                muvw = muvw + 1
                if(ival == nint(obj%data(muvw)) ) then
                   num = num+1
                   uvws(1,num) = u
                   uvws(2,num) = v
                   uvws(3,num) = w
                   if(num==mlim) return
                endif
             enddo
           enddo
         enddo

      return
      end subroutine modgmem_match_data

! MATH END

      end module modgmem_module


