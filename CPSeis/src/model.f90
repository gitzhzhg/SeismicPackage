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
! Name       : MODEL
! Category   : velocity
! Written    : 2007-11-20   by: RSDay
! Revised    : 2008-01-29   by: RSDay
! Maturity   : beta
! Purpose    : manipulate multi attribute models
! Portability: No known problems
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! MODEL
!  Support for multi-attribute gridded models. A model can be stored as a
!  selection of files or as a single file. A storage class for multiple
!  modgrid objects.
!
! Objectives:
!   Support regular gridded models with arbitrary dimensionality
!   Register the grid with seismic data sets - Optional
!   Describe orientation-location of grid in XYZ space - Optional
!
!  Assumptions:
!  2. The grid bin spacing is regular
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
!     VARIABLE   DESCRIPTION
!-------------------------------------------------------------------------------
!</calling_doc>d.


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2008-01-29  RSDay        Initial version
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
!
!-----------------------------------------------------------------------------
! Information Categories
!    Global       - Parameters that pertain to any layered or gridded model
!                   e.g. rank, name, type
!    ModelLimits  - Defines the maximum size of the model window
!    GridModel    - Defines a regular gridded model
!    LayerModel   - Defines a model composed of connected regions
!
!-----------------------------------------------------------------------------
!    Global
! rank      Dimensionality of the model(1-4)
!           1-3 are static physical models. rank 4 is will be
!           an animation of a 3D model or a 4D data set
! modname   Model name
! modtype   Model type(e.g. GRID,LAYER,ZGRID,3DSHOTS)
!-----------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------
!    ModelLimits
!  hdwrd(i)  Header to use for registering model window to seismic.
!            -1 implies no matching header(e.g. time-depth axis)
!  label(i)  Ascii label for axis i.
!  modmin(i) Model window minimum for axis i.
!  modmax(i) Model window maximum for axis i.
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!    ExternRepresentation
!  datafile  name of an optional binary data file(SAME,NONE are legitimate)
!  databits  number of bits in the data words (1,8,16,32,64)
!  datapos   byte offset to data in datafile
!  endian    endian behavior of the data (0:little, 1:big, -1:unknown)

      module model_module
      use modgrid_module
      use string_module
      use named_constants_module
      use grid_module
      use pcpsx_module
      use trcio_module
      use cio_module
      use swap_module
      implicit none
       private

! modgrid methods - The API to the outside world
       public  :: model_create      !overloaded function
       public  :: model_delete
       public  :: model_add_property
       public  :: model_set_property
       public  :: model_del_property
       public  :: model_get_component
       public  :: model_get_property
       public  :: model_nattr       !number of allocated model components
       public  :: model_pnames      !access property names
       public  :: model_print       !ascii report of grid parameters
       public  :: model_consistent  !check all components
       public  :: model_oconsistent !check 2 object
       public  :: model_comensurate !check if all grids are same
       public  :: model_ocomensurate!check if 2 grids are same
       public  :: model_rddesc      !read the grid description(grid info)
       public  :: model_to_ascii
       public  :: model_regrid_prop
       public  :: model_get_rgndata
       public  :: model_set_rgndata
       !

       character(len=100),save,public :: model_ident = &
       "$Id: model.f90,v 1.1 2008/01/30 15:06:05 RSDay beta sps $"


       ! Support multi attribute models

       ! A model is composed of multiple modgrid objects. There is one
       ! modgrid structure for each property in a multi-attribute model.
       type,public :: model_struct
        private
        integer                      :: nattr       !allocated cobj count
        character(len=120)           :: fnames(16)  !grid files
        type(modgrids_struct)        :: mobjs(16)   !gridded components
        type(region_struct),pointer  :: rgndata
       end type model_struct
!
       interface model_create
         module procedure model_create_empty
         module procedure model_create_from_modgrids
         module procedure model_create_from_file
       end interface

       interface model_add_property
         module procedure model_add_property_file
         module procedure model_add_property_old
         module procedure model_add_property_data
       end interface

       real, parameter     :: MOD_RNIL = -99999.0
       integer, parameter  :: MOD_INIL = -999999
       contains



      integer function model_create_empty(model) result(status)
       type(model_struct),pointer  :: model       ! arguments
       integer   :: i, i_err
       status = 0
       nullify(model)
       allocate(model,stat=i_err)
       if(i_err /=0) then
         status = -1
         return
       endif
       model%nattr = 0
       nullify (model%rgndata)
       do i = 1,16
         nullify(model%mobjs(i)%mobj)
         model%fnames(i) = ' '
       enddo
      return
      end function model_create_empty

      integer function model_create_from_modgrids(model,objs,&
         pcnt,pnames) result(status)
       type(model_struct),pointer     :: model
       type(modgrids_struct),pointer  :: objs(:)
       integer,intent(out)            :: pcnt
       character(len=*),intent(out)   :: pnames(:)
       type(modgrid_struct),pointer  :: obj
       integer   :: i, i_err
       status = 0
       pcnt = 0
       pnames= ' '
       i_err =  model_create_empty(model)
       if(i_err /=0) then
         print *,'model_create_from_modgrids: error creating empty?'
         status = -1
         return
       endif
       if(associated(objs)) then
         do i = 1,min(size(model%mobjs),size(objs))
            obj => modgrid_modgrids_get(objs,i)
            if(associated(obj)) then
              i_err = model_set_property(model, obj, pcnt+1,&
                      modgrid_headfile(obj))
              if(i_err ==0) then
                if(modgrid_pname(obj) .ne. ' ') then
                  pcnt = pcnt + 1
                  pnames(pcnt) = modgrid_pname(obj)
                endif
              else
                print *,'model_create_from_modgrids: error adding property ',i
              endif
            endif
         enddo
       endif
      return
      end function model_create_from_modgrids

      integer function model_create_from_file(model,fname,stdo,&
       ftype,xhdr,yhdr,pcnt,pnames) result(status) 
       type(model_struct),pointer  :: model
       character(len=*),intent(in)    :: fname
       type(modgrids_struct),pointer  :: objs(:)
       integer,intent(in)             :: stdo
       character(len=*),intent(inout) :: ftype
       integer,intent(in)             :: xhdr      !for trace scans
       integer,intent(in)             :: yhdr      !for trace scans
       integer,intent(out)            :: pcnt
       character(len=*),intent(out)   :: pnames(:)
       character(len=120)  :: dfile
       character(len=8)    :: wtype
       character(len=8)    :: velout
       integer             :: i_err
       integer             :: lxhdr,lyhdr
       type(region_struct),pointer     :: rgndata
       status = 0
       nullify(objs)
       nullify(rgndata)
       velout=' '
       lxhdr = xhdr
       lyhdr = yhdr
       i_err =  modgrid_rddesc_modgrids(objs,fname,stdo,dfile,wtype,&
                 ftype,lxhdr,lyhdr,velout,pcnt,pnames,rgndata)
       if(i_err /=0) then
         status = -1
         return
       endif
       status = model_create_from_modgrids(model,objs,pcnt,pnames)
      return
      end function model_create_from_file

      subroutine model_delete(model)
       type(model_struct),pointer  :: model       ! arguments
       integer :: n,i_err
       if(.not. associated(model)) return
       do n = 1,model%nattr
         if( associated(model%mobjs(n)%mobj)) then
           call modgrid_delete(model%mobjs(n)%mobj)
         endif
       enddo
       model%nattr = 0
       if (associated(model%rgndata)) then
         deallocate (model%rgndata, stat=i_err)
       endif
       deallocate(model)
       nullify(model)
      return
      end subroutine model_delete

      integer function model_data_stats(model,stdo, dmin,dmax,davg,&
       gmax, dbar, pn, xhdr, yhdr) result(status)
       type(model_struct),intent(in)  :: model
      real,intent(out)                :: dmin(:),dmax(:)
      real,intent(out)                :: davg(:)
      real,intent(out)                :: gmax(*)
      real,intent(out)                :: dbar
     !real,intent(out)                :: dmin,dmax,davg
      integer,intent(in)              :: stdo
      integer,intent(inout)           :: pn
      integer,intent(in)              :: xhdr
      integer,intent(in)              :: yhdr
      type(modgrid_struct),pointer    :: obj
      character(len=120)     :: fname       ! arguments
      character(len=8) :: ftype

      nullify (obj)   ! jpa

      status = -1
      
      call model_get_component(model,pn,obj)
      if(.not.associated(obj)) then
        if(stdo>0) write(stdo,*) &
         'model_data_stats: null property,',pn
        return
      endif
      ftype = modgrid_ftype(obj)
      if(ftype=='UNKNOWN') then
        if(stdo>0) write(stdo,*) &
         'model_data_stats: UNKNOWN file type,',trim(fname)
        return
      endif
      fname = modgrid_headfile(obj)
      status = modgrid_data_stats_prop(fname,stdo, dmin,dmax,davg,&
       gmax, dbar, pn, xhdr, yhdr)

      return
      end function model_data_stats
!
      subroutine model_set_rgndata(model,flagfile,&
       flag_off,flag_esize,flag_bit_len, rcnt, rnames, rbits)
      type(model_struct),intent(inout) :: model
      character(len=*),intent(in)    :: flagfile
      integer,intent(in)    :: flag_off
      integer,intent(in)    :: flag_esize
      integer,intent(in)    :: flag_bit_len
      integer,intent(in)    :: rcnt
      integer,intent(in)    :: rbits(:)
      character(len=*),intent(in)    :: rnames(:)
      integer :: i_err
      if(associated(model%rgndata) ) then
        deallocate(model%rgndata, stat=i_err)
        if(i_err/=0) then
          print *,'model_set_rgndata: deallocate error'
          return
        endif
        nullify(model%rgndata)
      endif

      call modgrid_create_rgndata(model%rgndata,flagfile,&
           flag_off,flag_esize,flag_bit_len, rcnt, rnames, rbits)
      return
      end subroutine model_set_rgndata

      subroutine model_get_rgndata(model,flagfile,&
       flag_off,flag_esize,flag_bit_len, rcnt, rnames, rbits)
      type(model_struct),intent(in) :: model
      character(len=*),intent(inout)    :: flagfile
      integer,intent(inout)    :: flag_off
      integer,intent(inout)    :: flag_esize
      integer,intent(inout)    :: flag_bit_len
      integer,intent(inout)    :: rcnt
      integer,intent(inout)    :: rbits(:)
      character(len=*),intent(inout)    :: rnames(:)

      call modgrid_get_rgndata(model%rgndata,flagfile,&
       flag_off,flag_esize,flag_bit_len, rcnt, rnames, rbits)

      return
      end subroutine model_get_rgndata

      subroutine model_print(model,stdo)
      type(model_struct),intent(in) :: model       ! arguments
      integer,intent(in)              :: stdo
      integer       ::  n
      character(len=120) :: card
      xxif_pcpsx_i_pel : if ( pcpsx_i_pel() .eq. 0 ) then
      do n = 1,model%nattr
        write(card,*) '#model_print: start attribute index=',n
        write(stdo,'(a)') trim(card)
        card= '#model_print: file='//trim(model%fnames(n))
        write(stdo,'(a)') trim(card)
        call modgrid_print(model%mobjs(n)%mobj,stdo)
        write(card,*) '#model_print: end attribute index=',n
        write(stdo,'(a)') trim(card)
      enddo
      if(associated(model%rgndata) ) then
        card = '# FLAGS_FILE='//trim(model%rgndata%flagfile)
        write(stdo,'(a)') trim(card)
        write(card,*) '# FLAGS_ESIZE=',model%rgndata%flag_esize
        write(stdo,'(a)') trim(card)
        write(card,*) '# FLAGS_BIT_LENGTH=',model%rgndata%flag_bit_len
        write(stdo,'(a)') trim(card)
        write(card,*) '# FLAGS_OFFSET=',model%rgndata%flag_off
        write(stdo,'(a)') trim(card)
        do n = 1,model%rgndata%rcnt
          write(card,*) '# REGION '//trim(model%rgndata%rnames(n)),&
          model%rgndata%rbits(n)
          write(stdo,'(a)') trim(card)
        enddo
      endif
      end if xxif_pcpsx_i_pel 
      return
      end subroutine model_print

      subroutine model_to_ascii(model,ascrep)
      type(model_struct),intent(in) :: model       ! arguments
      character(len=*),intent(inout)  :: ascrep
      type(modgrid_struct),pointer :: obj
      character(len=120)  :: dfile
      character(len=16)   :: ftype
      double precision    :: fsize
      character(len=8)    :: wtype
      integer             :: wrdsz
      character(len=8)    :: wname
      character(len=132)  :: str
      character(len=64)   :: pnames(16)
      character(len=204)  :: pstr
      integer             :: pcnt,i,nc
      integer             :: nn,i_err
      character(len=3000) :: tstr
      
      !build a summary property string
      pstr=' '
      pcnt = model_nattr(model)
      call model_pnames(model, pnames) 
      do i = 1,pcnt
        if(i==1 ) pstr = '# PNAMES=('//pnames(i)
        if(i>1 .and. i<=pcnt) pstr = trim(pstr)//','//trim(pnames(i))
        if(i==pcnt) pstr = trim(pstr)//')'
      enddo

      ascrep = trim(ascrep)//trim(pstr)//char(10)
      nc = len_trim(ascrep)
      do i=1,pcnt
        call model_get_component(model,i,obj)
        ftype = 'UNKNOWN'
        dfile = ' '
        wtype = ' '
        wname = ' '
        fsize = 0
        wrdsz = 4
        call modgrid_dskdata(obj,ftype,fsize,dfile,wtype,wrdsz,wname)
        write(str,'("# DATA_FILE_SIZE=",f12.0," bytes(w/o extents)")') fsize
        str = trim(str)//char(10)//'# DATA_FILE='//trim(dfile)//char(10)
        ascrep(nc+1:)=trim(str)
        nn = len_trim(str)
        nc = nc + nn
        tstr = ' '
        call modgrid_to_ascii(obj,tstr,dfile,ostyle=0)
        ascrep(nc+1:) = trim(tstr)
        nc = nc + len_trim(tstr)
      enddo
      i_err = model_region_str(model,ascrep,nc)

      return
      end subroutine model_to_ascii

      integer function model_to_voxhdr(model, ascrep, nc) result(status)
      type(model_struct),intent(in) :: model
      character(len=*),intent(inout)  :: ascrep
      integer,intent(inout) :: nc  !loc. of last non-blank character in ascrep
      type(modgrid_struct),pointer    :: obj
      character(len=120)  :: dfile
      character(len=16)   :: ftype
      double precision    :: fsize
      character(len=8)    :: wtype
      integer             :: wrdsz
      character(len=8)    :: wname
      integer             :: pcnt,i,cnt,i_err
      character(len=3000) :: tstr

      status=0
      ascrep=' '
      nc = 0
      pcnt = model_nattr(model)
      if(pcnt <=0) then
        status= -1
        return
      endif

      do i=1,pcnt

        call model_get_component(model,i,obj)
        if(i==1) then   !grid parameters shared by all properties
          cnt =  modgrid_voxet_body(obj, ascrep)
          nc = len_trim(ascrep)
          i_err = model_region_str(model,ascrep,nc)
        endif
        ftype = 'UNKNOWN'
        dfile = ' '
        wtype = ' '
        wname = ' '
        fsize = 0
        wrdsz = 4
        call modgrid_dskdata(obj,ftype,fsize,dfile,wtype,wrdsz,wname)

        tstr = ' '
        cnt = cnt +  modgrid_voxet_property(obj,tstr,i,dfile)
        ascrep(nc+1:) = trim(tstr)
        nc = nc + len_trim(tstr)

      enddo
      tstr ='END'//char(10)
      ascrep(nc+1:) = trim(tstr)
      nc = nc + len_trim(tstr)
      if(nc .ne. len_trim(ascrep)) then
        print *,'model_to_voxhdr: error, the computed size=',nc
        print *,'model_to_voxhdr: error, the actual size=',&
        len_trim(ascrep)
        status = -1
      endif

      return
      end function model_to_voxhdr

      integer function model_region_str(model,ascrep,nc) result(status)
      type(model_struct),intent(in) :: model
      character(len=*),intent(inout)  :: ascrep
      integer,intent(inout) :: nc    !position of last character
      character(len=128) :: card
      integer       :: i
      status = 0
      if(associated(model%rgndata) ) then
        card = '# FLAGS_FILE='//trim(model%rgndata%flagfile)//char(10)
        ascrep(nc+1:) = trim(card)
        nc = nc + len_trim(card)
        write(card,*) '# FLAGS_ESIZE=',model%rgndata%flag_esize
        card = trim(card)//char(10)
        ascrep(nc+1:) = trim(card)
        nc = nc + len_trim(card)
        write(card,*) '# FLAGS_BIT_LENGTH=',model%rgndata%flag_bit_len
        card = trim(card)//char(10)
        ascrep(nc+1:) = trim(card)
        nc = nc + len_trim(card)
        write(card,*) '# FLAGS_OFFSET=',model%rgndata%flag_off
        card = trim(card)//char(10)
        ascrep(nc+1:) = trim(card)
        nc = nc + len_trim(card)
        do i = 1,model%rgndata%rcnt
          write(card,*) '# REGION '//trim(model%rgndata%rnames(i)),&
          model%rgndata%rbits(i)
          card = trim(card)//char(10)
          ascrep(nc+1:) = trim(card)
          nc = nc + len_trim(card)
        enddo
        if(nc .ne. len_trim(ascrep)) then
          print *,'model_region_str: error, nc=',nc,' len=',&
          len_trim(ascrep)
          status = -1
        endif 
      endif
      return
      end function model_region_str


      integer function model_rddesc(model,fname,stdo,dfile,wtype,&
       ftype,xhdr,yhdr,vel_type,pcnt,pnames) result(status)
      type(model_struct),pointer :: model
      character(len=*),intent(in)     :: fname
      integer,intent(in)              :: stdo
      character(len=*),intent(inout)  :: dfile     !data file for 1st prop
      character(len=*),intent(inout)  :: wtype     !word type for 1st prop
      character(len=*),intent(inout)  :: ftype     !file type
      integer,intent(in)              :: xhdr      !for trace scans
      integer,intent(in)              :: yhdr      !for trace scans
      character(len=*),intent(inout)  :: vel_type  !for CPSVEL files
      integer,intent(out)             :: pcnt
      character(len=*),intent(inout)  :: pnames(:) !return property names
      integer :: lxhdr,lyhdr
      type(modgrids_struct),pointer   :: objs(:)
      type(region_struct),pointer     :: rgndata
      nullify(model)
      nullify(objs)
      nullify(rgndata)
      lxhdr = xhdr
      lyhdr = yhdr
      pcnt  = 0
      pnames= ' '
      ! vel_type = ' ' implies no conversion
      status =  modgrid_rddesc_modgrids(objs,fname,stdo,dfile,wtype,&
       ftype,lxhdr,lyhdr,vel_type,pcnt,pnames,rgndata)
      if(status==0) then
        status = model_create_from_modgrids(model,objs,pcnt,pnames)
      if(.not.associated(model)) print *, 'error: no model?'
      endif
      return
      end function model_rddesc

      integer function model_add_property_old(model,&
      nattr, pname, fname, cval, stdo) result(status)
       type(model_struct),pointer  :: model   ! arguments
       integer,intent(in)             :: nattr
       integer,intent(in)             :: stdo
       character(len=*),intent(in)    :: pname        !property names
       real,         intent(in)       :: cval         !constant or default
       character(len=*),intent(in)    :: fname        !file names

       integer            :: i_err
       character(len=160) :: dfile
       character(len=16)  :: wtype
       character(len=16)  :: ftype
       character(len=16)  :: pn
       integer            :: hd(3)
       integer            :: np
       status = 0
       if(.not. associated(model) ) then
         i_err = model_create_empty(model)
         if(i_err /=0) then
           status = -1
           return
         endif
       endif
       np = model_nattr(model) !existing property count

       np = np + 1               !increment the attribute count
       if(np <1 .or. np >16) then
         status = -2
         return
       endif
       if(np /= nattr) then      !check for consistency
         status = -3
         return
       endif

       model%fnames(np) = fname
       if(fname==' ' .or. fname=='NONE') then
           hd(1) = HDR_MIDPOINT_XGRID
           hd(2) = HDR_MIDPOINT_YGRID
           hd(3) = modgrid_string_to_header('DEPTH')
           i_err = modgrid_create_const(model%mobjs(np)%mobj,hd,stdo,&
                   cval,pname)
       else
           i_err = modgrid_rddesc(model%mobjs(np)%mobj,fname,stdo,dfile,&
                   wtype, ftype)
       endif
       if(i_err /= 0) then
           status = -4
           print *,'model_add_property: error nattr=',np,'fname=',&
           trim(fname),' pname=',trim(pname)
           model%fnames(np) = ' '
           call model_delete(model)
           return
       endif
       !save the property name of this model component
       call modgrid_get_pname(model%mobjs(np)%mobj, pn)
       if(pname /= pn) then
         print *,'model_add_property:warn arg pname=',pname
         print *,'model_add_property:warn obj pname=',pn
       endif
       call modgrid_set_pname(model%mobjs(np)%mobj,pname)
       model%nattr = np
      return
      end function model_add_property_old
      !
      !add the property contained in the file fname
      integer function model_add_property_file(model,&
      nattr, pname, fname, stdo) result(status)
       type(model_struct),pointer  :: model   ! arguments
       integer,intent(in)             :: nattr
       integer,intent(in)             :: stdo
       character(len=*),intent(in)    :: pname        !property names
       character(len=*),intent(in)    :: fname        !file names

       integer            :: i_err
       character(len=160) :: dfile
       character(len=16)  :: wtype
       character(len=16)  :: ftype
       character(len=16)  :: pn

       integer            :: np
       status = 0
       if(.not. associated(model) ) then
         i_err = model_create_empty(model)
         if(i_err /=0) then
           status = -1
           return
         endif
       endif
       np = model_nattr(model) !existing property count

       np = np + 1               !increment the attribute count
       if(np <1 .or. np >16) then
         status = -2
         return
       endif
       if(np /= nattr) then      !check for consistency
         status = -3
         return
       endif

       model%fnames(np) = fname
       if(fname==' ' .or. fname=='NONE') then
           i_err = -1
       else
           i_err = modgrid_rddesc(model%mobjs(np)%mobj,fname,stdo,dfile,&
                   wtype, ftype)
       endif
       if(i_err /= 0) then
           status = -4
           print *,'model_add_property_file: error nattr=',np,'fname=',&
           trim(fname),' pname=',trim(pname)
           model%fnames(np) = ' '
           call model_delete(model)
           return
       endif
       call modgrid_print(model%mobjs(np)%mobj,stdo)
       !save the property name of this model component
       call modgrid_get_pname(model%mobjs(np)%mobj, pn)
       if(pname /= pn) then
         print *,'model_add_property_file:warn arg pname=',pname
         print *,'model_add_property_file:warn obj pname=',pn
       endif
       call modgrid_set_pname(model%mobjs(np)%mobj, pname)
       model%nattr = np
       return
      end function model_add_property_file

      !add the property passed in the argument list
      integer function model_add_property_data(model,&
      nattr, pname, hdi, ngi, ogi, dgi, datai, stdo) result(status)
       type(model_struct),pointer  :: model   ! arguments
       integer,intent(in)             :: nattr
       integer,intent(in)             :: stdo
       character(len=*),intent(in)    :: pname        !property names
       integer,      intent(in)       :: hdi(:)          !
       integer,      intent(in)       :: ngi(:)          !
       real,         intent(in)       :: ogi(:)          !
       real,         intent(in)       :: dgi(:)          !
       real,         intent(in)       :: datai(:)        !

       integer            :: i_err



       character(len=32)  :: pn


       integer            :: np
       status = 0
       if(.not. associated(model) ) then
         i_err = model_create_empty(model)
         if(i_err /=0) then
           status = -1
           return
         endif
       endif
       np = model_nattr(model) !existing property count

       np = np + 1               !increment the attribute count
       if(np <1 .or. np >16) then
         status = -2
         return
       endif
       if(np /= nattr) then      !check for consistency
         status = -3
         return
       endif

       model%fnames(np) = ' '
       i_err = modgrid_create_from_data(model%mobjs(np)%mobj,&
               hdi,ngi,ogi,dgi, stdo,datai,pname)
       if(i_err /= 0) then
           status = -4
           print *,'model_add_property_data: error nattr=',np,&
           ' pname=',trim(pname)
           call model_delete(model)
           return
       endif
       !save the property name of this model component
       call modgrid_get_pname(model%mobjs(np)%mobj, pn)
       if(pname /= pn) then
         print *,'model_add_property_data:warn arg pname=',pname
         print *,'model_add_property_data:warn obj pname=',pn
       endif
       call modgrid_set_pname(model%mobjs(np)%mobj, pname)
       model%nattr = np
       return
      end function model_add_property_data
      !
      integer function model_set_property(model, mobj, nattr,  fname)&
      result(status)
       type(model_struct),pointer  :: model       ! arguments
       type(modgrid_struct),pointer   :: mobj
       integer,intent(in   )          :: nattr
       character(len=*),intent(in)    :: fname        !file names

       integer            :: i_err
       status = 0
       if(.not. associated(model) ) then
         if(nattr > 1) then
           status = -1
           return
         endif
         i_err = model_create_empty(model)
         if(i_err /=0) then
           status = -2
           return
         endif
       endif
       if(associated(model%mobjs(nattr)%mobj)) then
         call modgrid_delete(model%mobjs(nattr)%mobj)
         model%fnames(nattr)    = ' '
       endif
       model%mobjs(nattr)%mobj=> mobj
       model%fnames(nattr)    = fname
       model%nattr = model_nattr(model)

       return
      end function model_set_property

      !delete the last property
      integer function model_del_property(nattr, model) result(status)
       type(model_struct),pointer  :: model       ! arguments
       integer,intent(inout)          :: nattr

       status = 0
       if(.not. associated(model) ) then
         status = -1
         return
         nattr = 0
       endif

       nattr = model_nattr(model)
       if(nattr > 0) then
         call modgrid_delete(model%mobjs(nattr)%mobj)
         model%fnames(nattr) = ' '
         model%nattr = nattr - 1
       endif
       return
      end function model_del_property

      subroutine model_get_component(model,nattr,mobj)
       type(model_struct),intent(in)  :: model       ! arguments
       integer,intent(in)             :: nattr
       type(modgrid_struct),pointer   :: mobj
       nullify(mobj)
       if(nattr < 1 .or. nattr>model%nattr) return
       if(associated(model%mobjs(nattr)%mobj))  then
         mobj=> model%mobjs(nattr)%mobj
       else
         print *,'model_get_component: null obj, nattr=',nattr
       endif
      return
      end subroutine model_get_component
      !
      subroutine model_get_property(model,pname,mobj)
       type(model_struct),intent(in)  :: model       ! arguments
       character(len=*),intent(in)    :: pname
       type(modgrid_struct),pointer   :: mobj
       integer  :: n
       nullify(mobj)
       do n = 1,model%nattr
         if(associated(model%mobjs(n)%mobj))  then
           if(pname==modgrid_pname(model%mobjs(n)%mobj)) then
             mobj=> model%mobjs(n)%mobj
             return
           endif
         endif
       enddo
      return
      end subroutine model_get_property

      ! return a count of allocated properties
      integer function model_nattr(model) result(nattr)
       type(model_struct),intent(in)  :: model       ! arguments
       integer :: i
       nattr = 0
       do i = 1,size(model%mobjs)
        if(associated(model%mobjs(i)%mobj)) nattr = nattr + 1
       enddo
      return
      end function model_nattr

      ! return property names 
      ! NOTE: an object can have a blank property name 
      subroutine model_pnames(model, pnames) 
       type(model_struct),intent(in)  :: model       ! arguments
       character(len=*),intent(inout) :: pnames(:)
       integer :: i,pcnt
       pcnt = model_nattr(model)
       do i = 1,pcnt
         if(i <= size(pnames) ) then
           pnames(i) = modgrid_pname(model%mobjs(i)%mobj)
         endif
       enddo
      return
      end subroutine model_pnames

      ! check if grids use consistent coordinates schemes for all components
      logical function model_consistent(model) result(isok)
       type(model_struct),intent(in)  :: model       ! arguments
       type(modgrid_struct),pointer   :: mobj1
       type(modgrid_struct),pointer   :: mobj
       integer              :: i
       isok= .true.
       nullify(mobj1)
       nullify(mobj)
       do i = 1,size(model%mobjs)
        mobj => model%mobjs(i)%mobj
        if(associated(mobj)) then
          if(i==1) then
            mobj1 => model%mobjs(1)%mobj
          else
            isok = model_oconsistent(mobj1,mobj)
            if(.not. isok) return
          endif
        else
          exit  !assumes first null is end of data
        endif
       enddo
      return
      end function model_consistent

      ! check if grids use consistent coordinates schemes for all components
      logical function model_oconsistent(mobj1,mobj2) result(isok)
       type(modgrid_struct),pointer    :: mobj1
       type(modgrid_struct),pointer    :: mobj2
       integer              :: rank
       character(len=64)    :: name
       character(len=32)    :: pname
       character(len=32)    :: punits
       integer              :: hdo(4), hd(4)
       integer              :: ng(4)
       real                 :: dg(4),og(4)
       integer              :: i_err
       integer              :: ix,iy,iz
       integer              :: ixo,iyo,izo
       isok= .false.
       hdo = -1
       hd  = -1
       iz  = 1
       izo = 1
       if(.not.associated(mobj1)) return
       if(.not.associated(mobj2)) return
       isok= .true.
       call modgrid_get_griddesc(mobj1,name,pname,punits,rank,&
            hdo,ng,og,dg)
       i_err = modgrid_xyz_order(mobj1,ixo,iyo,izo)
       if(i_err/=0)  isok=.false.
       call modgrid_get_griddesc(mobj2,name,pname,punits,rank,&
            hd,ng,og,dg)
       if(hd(1)/= hdo(1) .or. hd(2)/=hdo(2)  .or. &
          hd(3)/=hdo(3)) isok=.false.
       i_err = modgrid_xyz_order(mobj2,ix,iy,iz)
       if(i_err/=0)  isok=.false.
       if(izo /= iz) isok=.false.
      return
      end function model_oconsistent

      ! check if grids use identical bins for all components
      logical function model_comensurate(model) result(isok)
       type(model_struct),intent(in)  :: model       ! arguments
       type(modgrid_struct),pointer    :: mobj1
       type(modgrid_struct),pointer    :: mobj
       integer              :: i

       nullify(mobj1)
       nullify(mobj)
       isok = model_consistent(model)
       if(.not. isok) return
       isok = .true.
       do i = 1,16
        mobj => model%mobjs(i)%mobj
        if(associated(mobj)) then
          if(i==1) then
            mobj1 => mobj
          else
            isok = model_ocomensurate(mobj1,mobj)
            if(.not. isok) return
          endif
        else
          exit
        endif
       enddo
      return
      end function model_comensurate

      ! check if grids use identical bins for all components
      logical function model_ocomensurate(mobj1,mobj2) result(isok)
       type(modgrid_struct),pointer    :: mobj1
       type(modgrid_struct),pointer    :: mobj2
       integer              :: rank
       character(len=64)    :: name
       character(len=32)    :: pname
       character(len=32)    :: punits
       integer              :: hdo(4), hd(4)
       integer              :: ng(4),ngo(4)
       real                 :: dg(4),og(4)
       real                 :: dgo(4),ogo(4)
       real                 :: tol

       tol  = .0001
       isok = model_oconsistent(mobj1,mobj2)
       if(.not. isok) return
       ngo = 1
       ng  = 1
       isok = .false.
       if(.not. associated(mobj1)) return
       if(.not. associated(mobj2)) return
       isok = .true.

       call modgrid_get_griddesc(mobj1,name,pname,punits,rank,&
            hdo,ngo,ogo,dgo)
       call modgrid_get_griddesc(mobj2,name,pname,punits,rank,&
            hd,ng,og,dg)
       if(ng(1) /= ngo(1) .or. ng(2) /= ngo(2) .or. &
          ng(3) /= ngo(3) ) isok=.false.
       if(abs(dg(1)-dgo(1)) > tol  .or.&
          abs(dg(2)-dgo(2)) > tol  .or. &
          abs(dg(3)-dgo(3)) > tol) isok = .false.
       if(abs(og(1)-ogo(1)) > tol  .or.&
          abs(og(2)-ogo(2)) > tol  .or. &
          abs(og(3)-ogo(3)) > tol) isok = .false.
      return
      end function model_ocomensurate

      ! read in one or more model properties, regrid, and
      ! save to a new output file.
      ! Supported output styles: VOXET | TRCIO | SITI
      integer function model_regrid_prop(iname, oname, o_type,o_xyz,&
      ovors, ivors, ngo, ogo, dgo ,stdo, maxmem,pn, vtyp_out,&
      xhdr, yhdr,o_endian,&
      scale, clip_min,clip_max,gobj) result(status)

      implicit none
      !input arguments
      character(len=*),intent(in):: iname
      character(len=*),intent(in):: oname
      character(len=*),intent(in):: o_type
      character(len=*),intent(inout):: o_xyz
      character(len=*),intent(inout):: vtyp_out
      character(len=*),intent(in   ):: ovors  !slowness/velocity flag
      character(len=*),intent(in   ):: ivors  !slowness/velocity flag
      integer,intent(in):: ngo(*)
      real,   intent(in):: ogo(*)
      real,   intent(in):: dgo(*)
      integer,intent(in):: stdo
      integer,intent(in):: maxmem
      integer,intent(inout):: pn
      integer,optional,intent(in):: xhdr
      integer,optional,intent(in):: yhdr
      real,optional,intent(in):: scale
      real,optional,intent(in):: clip_min    !clip on out values after scale
      real,optional,intent(in):: clip_max    !clip on out values after scale
      integer,optional,intent(in):: o_endian !output endian for floats
      type(grid_struct),optional,intent(in) :: gobj
      type(grid_struct) :: gobj_tmp

      ! local variables
      type(model_struct),pointer :: model
      type(model_struct),pointer :: omodel
      type(modgrid_struct),pointer :: obj    !a component of input model
      type(modgrid_struct),pointer :: oobj   !a component of output model
      type(trcio_struct),pointer   :: trcio
      character(len=8)  :: wtype
      character(len=12) :: ftype
      character(len=128) :: dfile
      character(len=64) :: name
      character(len=64) :: pname
      character(len=16) :: punits
      character(len=128) :: ohname,obname

      character(len=4)  :: i_xyz
      character(len=2048) :: ascrep
      character(len=32) ::   ilabels(3)
      character(len=32) ::   olabels(3)

      integer      :: rank
      integer      :: i_err
      integer      :: hdi(4),hdo(4)
      integer      :: ngi(4)
      real         :: ogi(4),dgi(4)
      integer      :: sslice
      integer      :: ng_slice
      real         :: og_slice, dg_slice
      integer      :: ufi,ufib
      integer      :: gocad
      integer      :: i,j
      integer      :: m1,i1,i2,i3,j1,j2,j3
      integer      :: ixlab,iylab,izlab
      integer      :: oxlab,oylab,ozlab
      integer(kind=8) :: ipts8,opts8
      integer      :: ipts,opts
      integer      :: traceno
      real,pointer :: obuff(:)
      integer      :: endian  !0 for little endian, 1 for big-endian
      integer      :: nwrds,nc
      integer      :: bsiz, wblk
      double precision :: fsize
      integer      :: ngig
      integer      :: ext(2)
      integer      :: start_pos(2) !trcio data start position
      integer      :: offset       !where data starts in an output file
      integer      :: nwr, per_cent
      real         :: orig_i(3), orig_o(3)
      real         :: axis_i(3,3) !(axis index, coord index)
      real         :: axis_o(3,3) !(axis index, coord index)
      integer      :: itoo(3)
      integer      :: scanx, scany
      real         :: data_scale
      real         :: data_clip_min, def_clip_min
      real         :: data_clip_max, def_clip_max
      logical      :: l_doscale
      integer      :: ss(3),ng(3)
      integer      :: oendian = 1
      integer      :: pcnt,props,prope,iprop
      integer      :: do_all_prop
      character(len=32) :: pnames(16)
      character(len=12) :: oftype

      double precision ::  dmem,memi,memo
      character(len=4)  :: units
      integer       :: temp

      status= -1

      gocad = 1
      ufi = -1
      ufib= -1
      nullify(model)
      nullify(omodel)
      nullify(obj)
      nullify(oobj)
      nullify(trcio)
      nullify(obuff)

      if(iname == oname) then
        write(stdo,*) 'model_regrid_prop: ERROR,output=input,',trim(iname)
        return
      endif
      if(oname== ' ') then
        write(stdo,*) 'model_regrid_prop: ERROR, blank output'
        return
      endif


      data_scale = 1.0
      def_clip_min = MOD_RNIL
      def_clip_max = MOD_RNIL
      data_clip_min = def_clip_min
      data_clip_max = def_clip_max
      if(present(scale) ) data_scale = scale
      if(present(clip_min) ) data_clip_min = clip_min
      if(present(clip_max) ) data_clip_max = clip_max
      if(present(o_endian) ) oendian = o_endian
      l_doscale=.false.
      if(data_scale /=1.0 .or. data_clip_min /= def_clip_min .or.&
         data_clip_max /= def_clip_max) l_doscale=.true.

     ! Get the input data parameters from file iname
     ! The model object will have information on all input properties.
      scanx = HDR_MIDPOINT_XGRID
      scany = HDR_MIDPOINT_YGRID
      if(present(xhdr) ) scanx = xhdr
      if(present(yhdr) ) scany = yhdr
      i_err = model_rddesc(model,iname,stdo,dfile,wtype,ftype,&
               scanx,scany,vtyp_out,pcnt,pnames)
      if(i_err /=0 .or. ftype=='UNKNOWN') then
        status= -1
        write(stdo,*) 'model_regrid_prop: error in model_rddesc?,'
        write(stdo,*) 'model_regrid_prop: file=',trim(iname)
        return
      endif

      if(o_type(1:1)=='V') gocad=1   !VOXET output
      if(o_type(1:1)=='T') gocad=0   !TRCIO output
      if(o_type(1:1)=='C') gocad=0   !CPSVEL
      if(o_type(1:1)=='H') gocad=1   !reset HGRID to VOXET
      if(o_type(1:1)=='S') gocad=2   !SITI output
      oftype='VOXET'
      if(gocad==0) oftype='TRCIO'
      if(gocad==2) oftype='SITI'
      if(string_upper_compare(ftype,'GSURF') .and. gocad==1) then
        oftype='GSURF'
      endif

     
      !
      do_all_prop = 0
      props = pn
      prope = pn
      !only allow multi-prop output for VOXET output
      if(gocad==1) then
        if(pn>pcnt .or. pn < 0 ) then
          do_all_prop=1
          props = 1
          prope = pcnt
        endif
      else
        props = max(1,min(pn,pcnt))
        prope = max(1,min(pn,pcnt))
      endif

      !loop over the targeted properties of the model.
      do iprop = props, prope !start of property loop, iprop

        !Get input object for property iprop.
        call model_get_component(model,iprop,obj)

        !Check to see if we are dealing with a layered modspec model on input
        !We treat modspec models as follows.
        ! 1. build new modspec on the target output grid
        ! 2. interploate mspec_out to output grid points
        !Do not interpolate from input 'grid' to output 'grid'!
        !When there is a modspec input we create a modspec output
        !consistent with the output grid and then we use the
        !regridded modspec to reset the input obj
        if(ftype== 'MODSPEC') then

          call modgrid_regrid_modspec(obj,iname,o_xyz, ngo,ogo,dgo)

        endif
        
        !get detailed information about the input property grid 
        call modgrid_get_name_rank(obj,name,pname,punits,rank)
        do i=1,rank
          call modgrid_get_griddesc(obj,i,hdi(i),ngi(i),ogi(i),dgi(i))
        enddo
        call modgrid_get_xyz(obj,ilabels, orig_i, &
             axis_i(1,:),axis_i(2,:),axis_i(3,:))
        i_err = modgrid_xyz_order(obj, ixlab,iylab,izlab)
        if(i_err /=0) then
          write(stdo,*) 'model_regrid_prop: bad input order'
          goto 99
        endif
        i_xyz=' '
        i_xyz(ixlab:ixlab)='X'
        i_xyz(iylab:iylab)='Y'
        i_xyz(izlab:izlab)='Z'

        !create output object description
        ozlab = index(o_xyz,'Z')
        oxlab = index(o_xyz,'X')
        oylab = index(o_xyz,'Y')
        if(oylab < 1) oylab = index(o_xyz,'IL')
        if(oxlab<1 .or. oylab<1 .or. ozlab<1) then
          write(stdo,*) 'model_regrid_prop:2 bad output order,',o_xyz
          goto 99
        endif
        if(gocad==0 .and. ozlab /= 1) then
          write(stdo,*) 'model_regrid_prop: trcio output has to&
          & be in trace order'
          write(stdo,*) 'model_regrid_prop: trcio output order=',o_xyz
          goto 99
         endif

        !find the mapping, itoo,  from input to output grid ordering
        i_err =  modgrid_build_xyz_map(i_xyz,o_xyz,itoo)

        !Create the ouput object
        name = trim(name)//' regrid'
        call modgrid_create (oobj, rank, name, pname, punits, &
            ngo(1:3), ogo(1:3), dgo(1:3), stdo)

        !Set further details for the output object
        olabels(1)= ilabels(itoo(1))
        olabels(2)= ilabels(itoo(2))
        olabels(3)= ilabels(itoo(3))
        orig_o(1) = ogo(oxlab)
        orig_o(2) = ogo(oylab)
        orig_o(3) = ogo(ozlab)
 
        axis_o = 0.0
        axis_o(oxlab,1) = (ngo(oxlab)-1)*dgo(oxlab)
        if(axis_o(oxlab,1) == 0)  axis_o(oxlab,1) = dgo(oxlab)
        axis_o(oylab,2) = (ngo(oylab)-1)*dgo(oylab)
        if(axis_o(oylab,2) == 0)  axis_o(oylab,2) = dgo(oylab)
        axis_o(ozlab,3) = (ngo(ozlab)-1)*dgo(ozlab)
        if(axis_o(ozlab,3) == 0)  axis_o(ozlab,3) = dgo(ozlab)
        call modgrid_set_xyz(oobj,olabels, orig_o,&
           axis_o(1,:),axis_o(2,:),axis_o(3,:))
        hdo(1) = hdi(itoo(1))
        hdo(2) = hdi(itoo(2))
        hdo(3) = hdi(itoo(3))
        call modgrid_set_hdwd(oobj,hdo)
        !Preserve the input object grid transform information(if any)
        if(present(gobj) ) then !use any passed arguments
          call modgrid_set_grid(oobj,gobj)
        else
          call modgrid_get_grid(obj,gobj_tmp)
          call modgrid_set_grid(oobj,gobj_tmp)
        endif
        do i=1,rank
          call modgrid_get_aunit(obj,i,units)
          call modgrid_set_aunit(oobj,i,units)
        enddo

        !generate output file names
        if(oftype(1:1)=='V' .or. string_upper_compare(oftype,'GSURF') ) then
          if(string_upper_compare(oftype,'GSURF')) then
            call modgrid_create_names(oname, ohname,obname, 3,pname)
          else
            call modgrid_create_names(oname, ohname,obname, gocad,pname)
          endif
        else
          call modgrid_create_names(oname, ohname,obname, gocad)
        endif
        endian = swap_endian()
        if(gocad == 0 .and. o_type(1:1)=='T') then
          !write trcio header before traces are written
          write(stdo,*) 'model_regrid_prop: writing trcio header'
          trcio => modgrid_wr_trcio_header(oobj,obname,stdo)
          if(.not.associated(trcio)) then
            write(stdo,*) 'model_regrid_prop: error - null trcio'
            goto 99
          endif
          start_pos = trcio_get_data_start_pos(trcio)
          offset = start_pos(2)  !assuming we start in the 1st extent
        endif

        !set constraints on buffers for input and output
        offset = 0
        start_pos = 0
        opts8= ngo(1)*ngo(2)
        opts8= opts8*ngo(3)
        ipts8= ngi(1)*ngi(2)
        ipts8= ipts8*ngi(3)
        memo = ngo(1)*ngo(2)
        memo = memo*ngo(3)
        memi = ngi(1)*ngi(2)
        memi = memi*ngi(3)
        dmem = memi+memo
        if(dmem < maxmem) then
          ipts = ipts8
          opts = opts8
        else
          if(opts8 > maxmem/2) then
            opts8 = maxmem/2
          endif
          if(ipts8 > maxmem/2) then
            ipts8 = maxmem/2
          endif
          opts = opts8
          ipts = ipts8
        endif
        if(iprop==props)then
          allocate(obuff(opts),stat=i_err)
          if(i_err .ne. 0) then
            write(stdo,*) 'model_regrid_prop: obuff allocate error ',opts
            goto 99
          endif
          obuff = 0.0
        endif
        !Constrain the output slab size
        if(rank >=3) then
          ng_slice = opts/(ngo(1)*ngo(2))
          if(ng_slice>ngo(3)) ng_slice = ngo(3)
        else
          ng_slice = 1
        endif

        if(gocad/=0) then   !force non-trcio files to be a single extent
          fsize = 4*ngo(1)
          fsize = fsize*ngo(2)
          fsize = fsize*ngo(3)
          fsize = fsize + 8192
          ngig  = fsize/250000000 + 1
          ext(1)= ngig
          ext(2)= 0
          i_err = cio_set_file_ext_size(ext)
          ufib  = cio_fopen(obname,'w+')
          if(ufib==CIO_ERROR) then
            write(stdo,*) 'model_regrid_prop: error opening obname=',&
            trim(obname)
            goto 99
          endif
        endif

        !print *,'model_regrid DBGS'
        !print *,'model_regrid DBG ngo=',ngo(1:3)
        !print *,'model_regrid DBG ogo=',ogo(1:3)
        !print *,'model_regrid DBG dgo=',dgo(1:3)
        !print *,'model_regrid DBG ng_slice=',ng_slice
        !call  modgrid_print(obj,stdo)
        !call  modgrid_print(oobj,stdo)
        !print *,'model_regrid DBGE'

        !Now interpolate the input data and cycle over output slabs
        sslice = 1 !first output slice
        per_cent = 0
        do while (sslice <= ngo(3) )  !cycle over output slabs

          !set output slab coordinates
          ng_slice  = min(ng_slice,ngo(3)-sslice+1)
          og_slice  = ogo(3) + (sslice-1)*dgo(3)
          dg_slice  = dgo(3)
          nwrds = ngo(1)*ngo(2)*ng_slice
          obuff(1:nwrds) = 0.0

          !generate the output slab ngo(1) x ngo(2)x ng_slice
          !output has same x-y headers as input
          i_err = modgrid_paint_by_obj(obj,ipts,stdo,&
              ovors,ivors, &
              hdi(ixlab),hdi(iylab),&
              ngo(1),ogo(1),dgo(1),&
              ngo(2),ogo(2),dgo(2),&
              ng_slice,og_slice,dg_slice,&
              obuff, o_xyz, vtyp_out)
          if(i_err < 0) then
            write(stdo,*) '#model_regrid_prop: paint error !'
            write(stdo,*) '#model_regrid_prop: iprop=',iprop
            write(stdo,*) '#model_regrid_prop: pname=',trim(pname)
            write(stdo,*) '#model_regrid_prop: sslice=',sslice
            write(stdo,*) '#model_regrid_prop: ng_slice=',ng_slice
            write(stdo,*) '#model_regrid_prop: og_slice=',og_slice
            write(stdo,*) '#model_regrid_prop: ngo=',ngo(1:3)
            write(stdo,*) '#model_regrid_prop: ogo=',ogo(1:3)
            write(stdo,*) '#model_regrid_prop: dgo=',dgo(1:3)
            write(stdo,*) '#model_regrid_prop: ipts=',ipts
            write(stdo,*) '#model_regrid_prop: opts=',opts
            goto 99
          endif

          ! cycle through output grid points in disk order
          ! which may be reverse order in memory. Output a block of
          ! data that is ngo(1)*ngo(2)*ng_slice words.
          i1 = 1
          i2 = ng_slice
          i3 = 1
          j1 = 1
          j2 = ngo(2)
          j3 = 1
          bsiz = 4*ngo(1)           !for trcio output, make a block = 1 trace
          wblk = (sslice-1)*ngo(2)
          traceno=(sslice-1)*ngo(2)
          if(l_doscale) then
              call model_clip_scale(obuff,nwrds,data_clip_min,data_clip_max,&
              data_scale)
          endif
          if(gocad==0) then   !TRCIO or CPSVEL output
           if(o_type=='CPSVEL') then !output a CPSVEL ascii file
             !hand oobj the data buffer. May be used by multiple oobj.
             i_err = modgrid_use_data(oobj,ngo(1),ngo(2),1,ng_slice,obuff)
             if(i_err /=0) then
               write(stdo,*) '#model_regrid_prop: use_data error'
               goto 99
             endif
             ss(1:3) = 1
             ng(1:3) = ngo(1:3)
             i_err = modgrid_wr_cvf (oobj,oname,stdo,ss,ng,vtyp_out)
             if(i_err /=0) then
               write(stdo,*) '#model_regrid: wr_cvf error'
               goto 99
             endif
           else               !output a modified TRCIO file
             do i= i1,i2,i3
             do j= j1,j2,j3
               traceno = traceno + 1
               m1 = (i-1)*ngo(2)*ngo(1) + (j-1)*ngo(1) + 1
               i_err = modgrid_wr_trcio_trace(oobj,trcio,traceno,&
                obuff(m1:m1+ngo(1)-1))
               if(i_err /=0) then
                 write(stdo,*) '#model_regrid_prop: trcio write error traceno='&
                 ,traceno
                 goto 99
               endif
             enddo !trace within slice
             enddo !slice
           endif
          else    !VOXET or SITI binary output
           !! we normally save voxet output as big-endian,SITI as native
           if(oendian /= endian) then
             call swap_bytes(obuff(1:nwrds))
           endif
           ! seek to correct start point
           i_err = cio_fseek(ufib,bsiz,wblk,0,0)
           do i= i1,i2,i3 !slow output dimension
           do j= j1,j2,j3
                m1 = (i-1)*ngo(2)*ngo(1) + (j-1)*ngo(1) + 1
                nwr = cio_fwrite(obuff(m1),1,bsiz,ufib)
                if(nwr < 0) then
                  write(stdo,*) 'model_regrid_prop: cio write error'
                  i_err = -1
                  goto 99
                endif
               !off= off + bsiz  RSD 10/05
                wblk = wblk + 1
           enddo !trace within slice
           enddo !slice
          endif

         sslice = sslice + ng_slice
         per_cent = 100.0*(sslice-1)/(ngo(3)-1+1)
         write(stdo,*) '#model_regrid_prop:',trim(pname),' % done=',per_cent

        enddo  !end of slab output loop

        !if binary data write is OK set the dskdata infomation in oobj
        if(i_err /=0) then
          write(stdo,*) '#model_regrid_prop: error'
          goto 99
        else
          fsize = 4*ngo(1)
          fsize = fsize*ngo(2)
          fsize = fsize*ngo(3)
         !print *,'regrid_prop DBG: obname=',trim(obname)
          call modgrid_set_dskdata(oobj,oftype,fsize,obname,&
           'IEEE',4,'REAL',offset)
          call modgrid_set_headfile(oobj,ohname)
          i_err = model_set_property(omodel, oobj,&
          iprop,  ohname)
          if(ufib>0) then
            i_err = cio_fclose(ufib)
            if(i_err==0) ufib= 0
          endif
        endif

      enddo  !end of property loop, iprop

      ascrep = ' '
      if(gocad==2 .and. o_type(1:1)=='S') then
        call modgrid_to_ascii(oobj,ascrep,obname,gocad)
      endif
      if(gocad==1 .and. (oftype(1:1)=='V' .or. oftype(1:1)=='G')) then
        !write voxet header after all binary files have been written
        i_err = model_to_voxhdr(omodel, ascrep, nc)
        if(i_err < 0)then
          write(stdo,*) 'model_regrid_prop: voxet header error ascrep(1:nc)=',&
          ascrep(1:nc)
          write(stdo,*) 'model_regrid_prop: will try to write it anyway'
        endif
      endif
      if(gocad .ne. 0) then
        ! write the voxet or siti header
        ufi = cio_fopen(ohname,'w')
        if(ufi <= 0) then
          write(stdo,*) 'model_regrid_prop: error on open:',trim(ohname)
          goto 99
        endif
        temp = len_trim(ascrep)
        nwr = cio_fwrite(ascrep,1,temp,ufi)
        if(nwr <0) then
          write(stdo,*) 'model_regrid_prop: header write error nwr=',nwr
          goto 99
        endif
        i_err = cio_fclose(ufi)
      endif


      status = 0
 99   continue
      if(associated(obuff) .and. o_type(1:1)/='C') deallocate(obuff)
      if(gocad == 0) then
        if(associated(trcio)) then
        !print *,'CLOSING trcio file'
         i_err = trcio_close(trcio)
        endif
      else
        if(ufib>0) i_err = cio_fclose(ufib)
      endif
      call model_delete(model)
      call model_delete(omodel)

      return
      end function model_regrid_prop

      !scales before clipping
      subroutine model_clip_scale(data,n1,clip_min,clip_max,scale)
      integer,intent(in)  :: n1
      real,intent(inout)  :: data(n1)
      real,intent(in)     :: scale
      real,intent(in)     :: clip_min
      real,intent(in)     :: clip_max
      integer :: i1
      real    :: cmin, cmax
      cmin = clip_min
      cmax = clip_max
      if(cmin /= MOD_RNIL .and. cmax /= MOD_RNIL) then
        cmin = min(clip_min,clip_max)
        cmax = max(clip_min,clip_max)
        do i1=1,n1
          if(data(i1) /= MOD_RNIL) &
           data(i1) = min(cmax,max(cmin, scale* data(i1)))
        enddo
      else
        if(cmin /= MOD_RNIL) then
          do i1=1,n1
            if(data(i1) /= MOD_RNIL) data(i1) = max(cmin,scale* data(i1))
          enddo
        else if(cmax /= MOD_RNIL) then
          do i1=1,n1
            if(data(i1) /= MOD_RNIL) data(i1) = min(cmax,scale* data(i1))
          enddo
        else
          do i1=1,n1
            if(data(i1) /= MOD_RNIL) data(i1) = scale* data(i1)
          enddo
        endif
      endif
      return
      end subroutine model_clip_scale

      end module model_module
