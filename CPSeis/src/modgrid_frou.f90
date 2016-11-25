!<CPS_v1 type="AUXILIARY_FILE"/>
!!----------------------------- modgrid_frou.f90 -----------------------------!!
!!----------------------------- modgrid_frou.f90 -----------------------------!!
!!----------------------------- modgrid_frou.f90 -----------------------------!!

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
!                   C P S   P R I M I T I V E
!
! Name       : MODGRID_FROU
! Category   : VELOCITY
! Written    : 2004-03-16   by: R.S.Day
! Revised    : 2008-01-29   by: R.S.Day
! Maturity   : beta
! Purpose    : Manipulate velocity models. Functions for C access to modgrid
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!     Date        Author     Description
!     ----        ------     -----------
! 13. 2008-01-29  R.S.Day    Make use of model module
! 12. 2007-10-23  R.S.Day    Increased max number properties to 16.
!                            modified call to modgrid_regrid_prop.
! 11. 2007-09-13  R.S.Day    Additional changes for multi-attribute models
! 10. 2007-08-28  R.S.Day    modgrid_file_info modified to handle Gocad
!                            multi-attribute voxet models.
!  9. 2007-01-03  Brian Macy Changed prototype of modgrid_paint_c.  Added
!                            routines modgrid_get_griddesc_c,
!                            modgrid_set_griddesc_c, modgrid_get_xyz_order_c.
! 10. 2007-08-28  R.S.Day    modgrid_file_info modified to handle Gocad
!                            multi-attribute voxet models.
!  9. 2007-01-03  Brian Macy Changed prototype of modgrid_paint_c.  Added
!                            routines modgrid_get_griddesc_c,
!                            modgrid_set_griddesc_c, modgrid_get_xyz_order_c.
!  8. 2006-04-25  R.S.Day    Changed for prototype change in modgrid_dsdata.
!  7  2006-01-17  R.S.Day    Fixes for grid objects with rank<3.
!                            Added modgrid_dfile_c
!  6. 2006-01-09  R.S.Day    Changed prototype of modgrid_regrid_c.
!  5. 2005-08-18  R.S.Day    Delete unused variables.
!  4. 2005-07-28  R.S.Day    Changed prototype of modgrid_file_info
!  3. 2005-05-31  R.S.Day    Changed prototype of modgrid_data_stats_c
!  2. 2004-11-02  R.S.Day    Passing scan headers to all modgrid_regrid calls
!                            in modgrid_regrid_c call.
!  1. 2004-03-16  R.S.Day    Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>



!!-------------------------- start of coding ------------------------------!!
!!-------------------------- start of coding ------------------------------!!
!!-------------------------- start of coding ------------------------------!!


      module modgrid_frou_module
        use modgrid_module
        use model_module
        implicit none

        type :: modgrid_frou_struct
          type(modgrid_struct),pointer :: obj
        end type

        type :: model_frou_struct
          type(model_struct),pointer :: obj
        end type

      character(len=100),public,save :: MODGRID_FROU_IDENT = &
'$Id: modgrid_frou.f90,v 1.13 2008/01/30 15:06:06 R.S.Day beta sps $'

      end module modgrid_frou_module

      integer function modgrid_rddesc_c(fobj,i_fname,stdo,i_dfile,i_wtype,&
       i_ftype,xhdr,yhdr) result(status)
      use modgrid_module
      use modgrid_frou_module
      use string_module
      type(modgrid_frou_struct),intent(out)    :: fobj
      integer,intent(in)              :: stdo
      integer,intent(in)              :: i_fname(*)
      integer,intent(inout)           :: i_dfile(*)
      integer,intent(inout)           :: i_wtype(*)
      integer,intent(inout)           :: i_ftype(*)
      integer,intent(in)              :: xhdr   !scan header for trace files
      integer,intent(in)              :: yhdr   !scan header for trace files
      character(len=120)   :: fname
      character(len=120)   :: dfile
      character(len=16)    :: wtype
      character(len=16)    :: ftype
      type(modgrid_struct),pointer    :: obj
      call string_hh2cc(i_fname,fname)
      dfile = ' '
      wtype = ' '
      ftype = ' '
      status =  modgrid_rddesc(obj,fname,stdo,dfile,wtype,&
       ftype,xhdr,yhdr)
      fobj%obj => obj
      call string_cc2hh(ftype,i_ftype)
      call string_cc2hh(wtype,i_wtype)
      call string_cc2hh(dfile,i_dfile)
      return
      end function modgrid_rddesc_c


      subroutine modgrid_set_griddesc_c(fobj,axis,hdwd,ng,og,dg)
      use modgrid_module
      use modgrid_frou_module
      type(modgrid_frou_struct),intent(inout) :: fobj
      integer,intent(in)           :: axis
      integer,intent(in)           :: hdwd
      integer,intent(in)           :: ng
      real,   intent(in)           :: og
      real,   intent(in)           :: dg
      call modgrid_set_griddesc(fobj%obj,axis,hdwd,ng,og,dg)
      return
      end subroutine modgrid_set_griddesc_c


      subroutine modgrid_get_griddesc_c(fobj,i_name,i_pname,i_punits,rank,&
        hdwd,ng,og,dg)
      use modgrid_module
      use modgrid_frou_module
      use string_module
      type(modgrid_frou_struct),intent(in) :: fobj
      integer,intent(out)           :: i_name(*)
      integer,intent(out)           :: i_pname(*)
      integer,intent(out)           :: i_punits(*)
      integer,intent(out)           :: rank
      integer,intent(out)           :: hdwd(4)
      integer,intent(out)           :: ng(4)
      real,   intent(out)           :: og(4)
      real,   intent(out)           :: dg(4)
      character(len=64)   :: name
      character(len=64)   :: pname
      character(len=16)   :: punits
      name   = ' '
      pname  = ' '
      punits = ' '
      call modgrid_get_griddesc(fobj%obj,name,pname,punits,rank,&
        hdwd,ng,og,dg)
      call string_cc2hh(name,i_name)
      call string_cc2hh(pname,i_pname)
      call string_cc2hh(punits,i_punits)
      return
      end subroutine modgrid_get_griddesc_c


      integer function modgrid_get_xyz_order_c(fobj,ix,iy,iz) result(status)
      use modgrid_module
      use modgrid_frou_module
      type(modgrid_frou_struct),intent(in) :: fobj
      integer,intent(out)           :: ix,iy,iz
      status = modgrid_xyz_order(fobj%obj,ix,iy,iz)
      return
      end function modgrid_get_xyz_order_c


      subroutine modgrid_file_info(ifname,iftype,iascii,&
       rank,ng,og,dg,hd,&
       label1,label2,label3,&
       unit1,unit2,unit3,&
       xhscan, yhscan,ixyz,pcnt,ipstr)
      use modgrid_module
      use model_module
      use string_module
      implicit none
      integer,intent(in)     :: ifname(*)
      integer,intent(inout)  :: iftype(*)
      integer,intent(inout)  :: iascii(*)
      integer,intent(inout)  :: ixyz(*)
      integer,intent(inout)  :: rank,ng(*)
      integer,intent(out)    :: hd(*)
      real,intent(out)       :: og(*),dg(*)
      integer,intent(out)    :: label1(*)
      integer,intent(out)    :: label2(*)
      integer,intent(out)    :: label3(*)
      integer,intent(out)    :: unit1(*)
      integer,intent(out)    :: unit2(*)
      integer,intent(out)    :: unit3(*)
      integer,intent(in)     :: xhscan
      integer,intent(in)     :: yhscan
      integer,intent(out)    :: pcnt
      integer,intent(out)    :: ipstr(*)

      type(modgrid_struct),pointer    :: obj
      type(model_struct),pointer    :: model
      integer      :: stdo
      character(len=96):: fname
      character(len=160):: dfile
      character(len=8) :: ftype
      character(len=32):: name
      character(len=32):: punits
      character(len=64):: pname,pnames(16)
      character(len=8) :: wtype
      character(len=6400) :: asrep
      character(len=120)  :: str
      character(len=256)  :: pstr
      integer      :: nchar
      integer      :: hdwd(4)
      integer      :: i,pn,ierr

      real         :: axis1(3)
      real         :: axis2(3)
      real         :: axis3(3)
      real         :: oxyz(3)
      integer      :: xlab,ylab,zlab
      integer      :: wrdsz
      character(len=32) :: labels(3)
      character(len=4)  :: xyz_order
      character(len=4)  :: unit
      double precision  :: dfsize
      character(len=4)  :: velout


      xyz_order=' '
      ixyz(1)=0
      iftype(1)=0
      iascii(1)=0
      pcnt  = 0
      asrep = ' '
      dfile = ' '
      wtype = ' '
      stdo = 6
      ng(1:3) = 1
      og(1:3) = 0.0
      dg(1:3) = 0.0
      hd(1:3) = 0
      velout=' '
      pname =' '
      pnames=' '
      pstr  =' '
      ipstr(1)=0
      call string_hh2cc(ifname(1:20),fname)
      ftype =  modgrid_ftype_read(fname,stdo,dfsize)
      call string_cc2hh(ftype,iftype)
      asrep = '# FILE_TYPE='//trim(ftype)//char(10)
      write(str,'("# FILE_SIZE=",f12.0," bytes(w/o extents)")') dfsize
      asrep=trim(asrep)//trim(str)//char(10)
      if(ftype /= 'UNKNOWN') then
        print *,'modgrid_file_info: DBG0',trim(fname)
        ierr = model_rddesc(model,fname,stdo,dfile,wtype,ftype,&
         xhscan,yhscan,velout,pcnt,pnames)
        if(ierr < 0) then
          asrep = '#ERROR_1: modgrid_rddesc failed for '//trim(fname)
        else
          write(str,'("# PropertyCount=",I2)') pcnt
          asrep=trim(asrep)//trim(str)//char(10)
          nchar = len_trim(asrep)
          call model_to_ascii(model,asrep(nchar+1:))
          do pn=1,model_nattr(model)
            call model_get_component(model,pn,obj)
            ierr = modgrid_xyz_order(obj,xlab,ylab,zlab)
            if(ierr==0) then
              xyz_order(xlab:xlab)='X'
              xyz_order(ylab:ylab)='Y'
              xyz_order(zlab:zlab)='Z'
              call string_cc2hh(xyz_order,ixyz)
            endif
            call modgrid_get_pname(obj,pname)
            if(pn <= size(pnames)) pnames(pn)=pname
            
            dfile=' '
            call modgrid_dskdata(obj,ftype,dfsize,dfile,wtype,wrdsz)

            call modgrid_get_name_rank(obj,name,pname,punits,rank)
            do i = 1,min(3,rank)
              call modgrid_get_griddesc(obj,i, hdwd(i),ng(i),og(i),dg(i))
              hd(i) = hdwd(i)
              call modgrid_get_aunit(obj,i, unit)
              if(i==1) call string_cc2hh(unit,unit1)
              if(i==2) call string_cc2hh(unit,unit2)
              if(i==3) call string_cc2hh(unit,unit3)
            enddo
            if(rank < 3) then
              call string_cc2hh(unit,unit3)
            endif
            call modgrid_get_xyz(obj,labels, oxyz, axis1,axis2,axis3)
            nchar = len_trim(labels(1))
            call string_cc2hh(labels(1)(1:nchar),label1)
            nchar = len_trim(labels(2))
            call string_cc2hh(labels(2)(1:nchar),label2)
            nchar = len_trim(labels(3))
            call string_cc2hh(labels(3)(1:nchar),label3)
            if(ftype=='SEGY') then
              nchar = len_trim(asrep)
              asrep(nchar+1:nchar+1) = char(10)
              nchar = nchar + 2
              call modgrid_get_buffer(obj,asrep(nchar:))
            endif

            if(pn==1 ) pstr = 'PNAMES=('//pnames(pn)
            if(pn>1 .and. pn<=pcnt) pstr = trim(pstr)//','//trim(pnames(pn))
            if(pn==pcnt) pstr = trim(pstr)//')'

          enddo
          call model_delete(model)
        endif
      else
        asrep = trim(asrep)//'#ERROR_2: '&
        &//trim(fname)//' is UNKNOWN file type'
      endif

      nchar = len_trim(asrep)
      call string_cc2hh(asrep(1:nchar),iascii)
      nchar = len_trim(pstr)
      call string_cc2hh(pstr(1:nchar),ipstr)
      return
      end subroutine modgrid_file_info

      integer function modgrid_paint_by_file_c(i_file,maxmem, stdo, &
        i_ovors, i_ivors  ,            &
        hx_out, hy_out,            &
        n1_out, o1_out, d1_out,    &
        n2_out, o2_out, d2_out,    &
        n3_out, o3_out, d3_out,    &
        ocube,  i_out_xyz, i_vtyp_out ) result(status)
      use modgrid_module
      use string_module
      implicit none

      integer,intent(in)      :: i_file(*)     !input model file name
      integer,intent(in)      :: i_ovors(*)    !slowness/velocity flag
      integer,intent(in)      :: i_ivors(*)    !interp domain
      integer,intent(inout)   :: i_out_xyz(*)  ! grid order
      integer,intent(inout)   :: i_vtyp_out(*) ! output type (CPSVEL only)
      integer,intent(in)      :: maxmem !memory limit
      integer,intent(in)      :: stdo   !standard output

      integer, intent (in   ) :: hx_out            !x header
      integer, intent (in   ) :: hy_out            !y header
      integer, intent (in   ) :: n1_out
      integer, intent (in   ) :: n2_out
      integer, intent (in   ) :: n3_out
      real,    intent (in   ) :: d1_out
      real,    intent (in   ) :: d2_out
      real,    intent (in   ) :: d3_out
      real,    intent (in   ) :: o1_out
      real,    intent (in   ) :: o2_out
      real,    intent (in   ) :: o3_out
      real,    intent(inout)  :: ocube(n1_out,n2_out,n3_out)

      character(len=120)      :: c_file   !input model file name
      character(len=12)       :: c_ovors  !slowness/velocity flag
      character(len=12)       :: c_ivors  !interp domain
      character(len=4)        :: c_out_xyz ! grid order
      character(len=8)        :: c_vtyp_out ! output type (CPSVEL only)
      !
      call string_hh2cc(i_file,c_file)
      call string_hh2cc(i_ovors,c_ovors)
      call string_hh2cc(i_ivors,c_ivors)
      call string_hh2cc(i_out_xyz,c_out_xyz)
      call string_hh2cc(i_vtyp_out,c_vtyp_out)
      !
      status =  modgrid_paint_by_file(c_file,maxmem, stdo, &
        c_ovors, c_ivors  ,            &
        hx_out, hy_out,            &
        n1_out, o1_out, d1_out,    &
        n2_out, o2_out, d2_out,    &
        n3_out, o3_out, d3_out,    &
        ocube,  c_out_xyz, c_vtyp_out )
      call string_cc2hh(c_vtyp_out(1:8),i_vtyp_out)
      call string_cc2hh(c_out_xyz(1:4),i_out_xyz)
      return
      end function modgrid_paint_by_file_c

      integer function modgrid_paint_by_obj_c(fobj,maxmem, stdo,&
        i_ovors , i_ivors ,            &
        hx_out, hy_out,            &
        n1_out, o1_out, d1_out,    &
        n2_out, o2_out, d2_out,    &
        n3_out, o3_out, d3_out,    &
        ocube,  i_out_xyz, i_vtyp_out ) result(status)
      use modgrid_module
      use modgrid_frou_module
      use string_module
      implicit none
      type(modgrid_frou_struct),intent(inout)    :: fobj
      integer,intent(in)      :: i_ovors(*)    !slowness/velocity flag
      integer,intent(in)      :: i_ivors(*)    !interp domain
      integer,intent(inout)   :: i_out_xyz(*)  ! grid order
      integer,intent(inout)   :: i_vtyp_out(*) ! output type (CPSVEL only)
      integer,intent(in)      :: maxmem !memory limit
      integer,intent(in)      :: stdo   !standard output

      integer, intent (in   ) :: hx_out            !x header
      integer, intent (in   ) :: hy_out            !y header
      integer, intent (in   ) :: n1_out
      integer, intent (in   ) :: n2_out
      integer, intent (in   ) :: n3_out
      real,    intent (in   ) :: d1_out
      real,    intent (in   ) :: d2_out
      real,    intent (in   ) :: d3_out
      real,    intent (in   ) :: o1_out
      real,    intent (in   ) :: o2_out
      real,    intent (in   ) :: o3_out
      real,    intent(inout)  :: ocube(n1_out,n2_out,n3_out)

      type(modgrid_struct),pointer    :: obj
      character(len=12)       :: c_ovors  !slowness/velocity flag
      character(len=12)       :: c_ivors  !interp domain
      character(len=4)        :: c_out_xyz ! grid order
      character(len=8)        :: c_vtyp_out ! output type (CPSVEL only)
      !
      call string_hh2cc(i_ovors,c_ovors)
      call string_hh2cc(i_ivors,c_ivors)
      call string_hh2cc(i_out_xyz,c_out_xyz)
      call string_hh2cc(i_vtyp_out,c_vtyp_out)
      obj => fobj%obj
      status =  modgrid_paint_by_obj(obj,maxmem, stdo,&
        c_ovors , c_ivors ,            &
        hx_out, hy_out,            &
        n1_out, o1_out, d1_out,    &
        n2_out, o2_out, d2_out,    &
        n3_out, o3_out, d3_out,    &
        ocube,  c_out_xyz,c_vtyp_out )
      call string_cc2hh(c_vtyp_out(1:8),i_vtyp_out)
      call string_cc2hh(c_out_xyz(1:4),i_out_xyz)
      return
      end function modgrid_paint_by_obj_c

      subroutine modgrid_delete_c(fobj)
      use modgrid_frou_module
      type(modgrid_frou_struct),intent(inout)    :: fobj
      call modgrid_delete(fobj%obj)
      return
      end subroutine modgrid_delete_c

      subroutine modgrid_print_c(fobj,stdo)
      use modgrid_frou_module
      implicit none
      type(modgrid_frou_struct),intent(inout)    :: fobj
      integer,intent(in)      :: stdo
      call modgrid_print(fobj%obj,stdo)
      return
      end subroutine modgrid_print_c

      integer function modgrid_data_stats_c(ifname,dmin,dmax,dbar,gmax,pn,&
      xhdr,yhdr)&
      result(status)
      use string_module
      use modgrid_module
      implicit none
      integer,intent(in)     :: ifname(*)
      real,intent(out)       :: dmin
      real,intent(out)       :: dmax
      real,intent(out)       :: dbar
      real,intent(inout)     :: gmax(*) !3 element array for grads
      integer,intent(inout)  :: pn
      integer,intent(in)     :: xhdr
      integer,intent(in)     :: yhdr

      character(len=120)     :: iname
      real,pointer           :: d1(:)
      real,pointer           :: d2(:)
      real,pointer           :: davg(:)
      integer    :: i_err
      integer    :: ng3

      dmin=0
      dmax=0
      dbar=0
      status = -1
      nullify(d1)
      nullify(d2)
      nullify(davg)
      allocate(d1(5000),stat=i_err)
      if(i_err/=0) return
      allocate(d2(5000),stat=i_err)
      if(i_err/=0) return
      allocate(davg(5000),stat=i_err)
      if(i_err/=0) return
      call string_hh2cc(ifname(1:30),iname)
      ng3 = modgrid_data_stats_prop(iname,6, d1,d2,davg,&
       gmax, dbar,pn,xhdr,yhdr)
      if(ng3 >=0) then
        dmin = minval(d1(1:ng3))
        dmax = maxval(d2(1:ng3))
      else
        dmin = -1
        dmax = -1
      endif
      if(associated(d1)) deallocate(d1)
      if(associated(d2)) deallocate(d2)
      if(associated(davg)) deallocate(davg)
      status = 0
      return
      end function modgrid_data_stats_c
!
      integer function modgrid_data_zbnd_c(ifname,dbnd,zbnd,zval) result(status)
      use string_module
      use modgrid_module
      implicit none
      integer,intent(in)     :: ifname(*)
      real,intent(in   )     :: dbnd       !data bound value
      integer,intent(inout)  :: zbnd       !depth bound grid value
      real   ,intent(inout)  :: zval       !depth bound value

      character(len=88)      :: iname
      integer    :: stdo
      zbnd = 0
      zval = 0
      stdo = 6
      call string_hh2cc(ifname(1:22),iname)
      status = modgrid_data_zbnd(iname,stdo,dbnd,zbnd, zval)
      if(status /=0) then
        write(stdo,*) 'modgrid_data_zbnd_c: modgrid_data_zbnd error'
      endif
      return
      end function modgrid_data_zbnd_c
!
      integer function modgrid_modspec_to_gocad_vs(ifile, ofile)&
        result(status)
      use modgrid_module
      use modspec_module
      use string_module
      implicit none
      !input arguments
      integer,intent(in) :: ifile(*)
      integer,intent(in) :: ofile(*)

      type(modspec_struct),pointer :: obj
      character(len=120) :: iname
      character(len=120) :: oname
      character(len=32)  :: name
      character(len=16) :: ftype
      double precision  :: dfsize
      integer   :: stdo
      integer   :: i_err

      call string_hh2cc(ifile(1:30),iname)
      call string_hh2cc(ofile(1:30),oname)
      status = -1
      stdo = 6
      ftype = modgrid_ftype(iname,stdo,dfsize)
      if(ftype /= 'MODSPEC') then
        print *,' modgrid_modspec_to_gocad_vs: wrong file type ',trim(ftype)
        return
      endif
      if(dfsize <=0) then
         write(stdo,*) 'modgrid_modspec_to_gocad_vs: error in fsize=',dfsize
         write(stdo,*) 'modgrid_modspec_to_gocad_vs: error iname=',iname
         return
      endif
      call modspec_create(obj,stdo,iname,'GRID')
      if(.not. associated(obj)) then
        write(stdo,*) 'modspec_to_goc: modspec_create error'
        return
      endif
      name = 'velocity_mod'
      call modspec_to_gocad_vs(obj,oname,name, i_err)
      call modspec_delete(obj)
      if(i_err /=0) return
      status = 0
      return
      end function modgrid_modspec_to_gocad_vs

      !regrid a property from an input file which may hold multiple
      !properties. The output is saved to a new file.
      integer function modgrid_regrid_c(ifile, ofile, otype,&
       oorder, ngo, ogo, dgo, xhdr,yhdr,o_endian,&
       scale,clip_min,clip_max,reqmem,pn,islow) result(status)
      use modgrid_module
      use model_module
      use string_module

      implicit none
      !input arguments
      integer,intent(in):: ifile(*)
      integer,intent(in):: ofile(*)
      integer,intent(in):: otype(*)
      integer,intent(in):: oorder(*)
      integer,intent(in):: ngo(*)
      real,   intent(in):: ogo(*)
      real,   intent(in):: dgo(*)
      integer,intent(in):: xhdr
      integer,intent(in):: yhdr
      real,intent(in)   :: scale
      real,intent(in)   :: clip_min
      real,intent(in)   :: clip_max
      integer,intent(in):: o_endian
      integer,intent(in):: reqmem
      integer,intent(inout):: pn
      integer,intent(in):: islow !0,1 do not,do interpolate in slowness

      ! local variables
      character(len=96) :: iname, oname
      character(len=8)  :: o_type
      character(len=4)  :: o_xyz
      character(len=8)  :: ovors
      character(len=8)  :: ivors
      character(len=8)  :: vtypo
      integer :: stdo
      integer :: maxmem
      real    :: add
      real    :: def_clip_min
      real    :: def_clip_max
      logical :: l_doscale

      status= -1

      stdo  = 6
      maxmem= reqmem  ! max words to use
      if(reqmem>500000000) maxmem=500000000
      print *,'modgrid_regrid_c: DBG maxmem=',maxmem
      iname = ' '
      oname = ' '
      o_type= ' '
      o_xyz = 'ZXY'
      add = 0.0
      
      !convert from c strings to fortran characters
      call string_hh2cc(ifile(1:22),iname)
      call string_hh2cc(ofile(1:22),oname)
      call string_hh2cc(otype(1:2) ,o_type)
      call string_hh2cc(oorder(1:1),o_xyz)

      ovors = 'VELO'
      if(islow==1) then
        ivors = 'SLOW'
      else
        ivors = 'VELO'
      endif
      vtypo = 'VTNM'
      def_clip_min = 1.2345
      def_clip_max = 1.2345
      l_doscale=.false.
      if(scale /=1.0 .or. clip_min /= def_clip_min .or.&
         clip_max /= def_clip_max ) l_doscale=.true.
      if(l_doscale) then
        status = model_regrid_prop(iname, oname, o_type,&
         o_xyz, ovors,ivors, ngo, ogo, dgo ,stdo, maxmem,pn,vtypo,&
         xhdr, yhdr,o_endian,&
         scale,clip_min,clip_max)
      else
        status = model_regrid_prop(iname, oname, o_type,&
         o_xyz, ovors,ivors, ngo, ogo, dgo ,stdo, maxmem,pn,vtypo,&
         xhdr, yhdr,o_endian)
      endif
      return
      end function modgrid_regrid_c
      
      subroutine modgrid_data_smooth_c(ifname,lambda, vbar,i_err)
      use string_module
      use modgrid_module
      use cio_module
      implicit none
      integer,intent(in)     :: ifname(*)
      real,intent(in)        :: lambda
      real,intent(inout)     :: vbar
      integer,intent(inout)  :: i_err
      type ( modgrid_struct ), pointer :: obj
      character(len=2048):: ascrep
      character(len=88)  :: fname
      character(len=88)  :: oname
      character(len=88)  :: ohname
      character(len=32)  :: ftype
      character(len=64)  :: name
      character(len=64)  :: pname
      character(len=16)  :: punits
      character(len=88)  :: dfile

      character    :: wtype*16
      integer      :: rank
      integer      :: stdo
      integer      :: hdwd(4),ng(4)
      real         :: og(4),dg(4)
      real         :: avg
      real,pointer :: data(:)
      real,pointer :: wrk(:,:,:)
      integer      :: i,j,k,pt

      integer      :: ll1,ul1,ll2,ul2
      integer      :: ufi,win1,win2
      real         :: wl,aval
      integer      :: nwr
      integer      :: temp
      stdo = 6

      call string_hh2cc(ifname(1:22),fname)
      i_err = modgrid_rddesc(obj,fname,stdo,dfile,wtype,ftype)
      if(i_err /= 0) then
        return
      endif
      call modgrid_get_griddesc(obj,name,pname,punits,rank,&
       hdwd,ng,og,dg)
      nullify(data)
      nullify(wrk)
      allocate(wrk(ng(1),ng(2),2), stat=i_err)
      if(i_err /=0) then
        return
      endif
      i = index(fname,'.')
      if(i>0) then
        oname = trim(fname(1:i-1))//'_smooth.vodat'
        ohname = trim(fname(1:i-1))//'_smooth.vo'
      else
        oname = trim(fname)//'_smooth.vodat'
        ohname = trim(fname)//'_smooth.vo'
      endif

!
! open output file
! output the file header
      ascrep=' '
      ufi = cio_fopen(ohname,'w')
      if(ufi==CIO_ERROR) then
        i_err = -1
        return
      endif
      call modgrid_to_ascii(obj,ascrep,oname,1)
      temp = len_trim(ascrep)
      i   = cio_fwrite(ascrep,1,temp,ufi)
      i_err = cio_fclose(ufi)
!
! open output data file to hold smoothed data
      ufi = cio_fopen(oname,'w',.false.,&
            PREALLOCATE_FILE_SPACE_DISABLED,&
            FILE_LOCK_DISABLED,&
            AUTO_DELETE_DISABLED)
      vbar = 0
 ! average values over 1 wavelength
      wl = max(lambda,2.0*dg(1))
      win1 = max(1.,0.5*wl/dg(1))
      win2 = max(1.,0.5*wl/dg(2))
      do i = 1,ng(3)  !output plane
        i_err = modgrid_rd_data(obj,stdo,i,1)
        if(i_err /= 0) then
          call modgrid_delete(obj)
          return
        endif
        i_err = modgrid_get_data(obj,data)
        avg = 0
        pt = 0
        do j = 1,ng(2)  !output trace
          do k = 1,ng(1)   !output sample
            pt = pt+1
           !avg = avg + data(pt)
            wrk(k,j,1) = data(pt)
          enddo
        enddo

        do j = 1,ng(2)  !output trace
          wrk(k,:,2) = 0.0
          pt = 0
          ll2= max(1,j-win2)
          ul2= min(ng(2),j+win2)
          do k = 1,ng(1)   !output sample

            ll1= max(1,k-win1)
            ul1= min(ng(1),k+win1)
            call modgrid_avg_val(ng(1),ng(2),wrk(1,1,1), ll1,ul1,ll2,ul2,aval)
            wrk(k,j,2) = aval
          enddo
        enddo
        nwr   = cio_fwrite(wrk(:,:,2),1,4*ng(1)*ng(2),ufi)

        avg = avg/(ng(2)*ng(1))
        vbar = vbar + avg
      enddo
      i_err = cio_fclose(ufi)
      vbar = vbar/ng(3)
      deallocate(wrk)
      i_err=0
      return
      end subroutine modgrid_data_smooth_c

! A(i1:i2,j1:j2)
! B(i3:i4,j3:j4)
      subroutine modgrid_avg_val(n1,n2,d, l1,u1,l2,u2,val)
      implicit none
      integer,intent(in)   :: n1
      integer,intent(in)   :: n2
      real,intent(in)      :: d(n1,n2)
      integer,intent(in)   :: l1,l2,u1,u2
      real,intent(out)     :: val
      integer i1,i2

      integer pt


      val = 0
      pt  = max(1,(u1-l1+1) *(u2-l2+1))
      do i2=l2,u2
      do i1=l1,u1
        val = val + d(i1,i2)
      enddo
      enddo
      val = val/pt

     !old_ll1=1
     !old_ul1=hw1
     !do i=1,hw1
     !  s(1)= s(1) + d(i,j)
     !enddo
     !do i2=l2,u2
     !  wt(1) = hw1
     !  do i=2,n1
     !    ll1= max(1,i-hw1)
     !    ul1= min(n1,i+hw1)
     !    s(i) = s(i-1)
     !    wt(i)= wt(i-1)
     !    if(ll1 /= old_ll1)  then
     !      s(i)= s(i) - d(i-1,j)
     !      wt(i)= wt(i-1) - 1
     !    endif
     !    if(old_ul1 /= ul1)  then
     !      s(i)= s(i) + d(ul1,j)
     !      wt(i)= wt(i-1) + 1
     !    endif
     !    old_ll1 = ll1
     !    old_ul1 = ul1
     !  enddo
     !  do i=1,n1
     !    s(i) = s(i)/wt(i)
     !  enddo
     !enddo
      return
      end subroutine modgrid_avg_val

      subroutine modgrid_ftype_c(ifile,stdo,iftype, dfsize)
      use modgrid_module
      use string_module
      implicit none
      integer,intent(in)    :: ifile(*)
      integer,intent(in)    :: stdo
      integer,intent(inout) :: iftype(*)
      double precision,intent(out)  :: dfsize

      character(len=120)    :: file
      character(len=8)      :: ftype
     
      call string_hh2cc(ifile,file)
      dfsize = 0.0
      ftype = modgrid_ftype(file,stdo,dfsize)
      call string_cc2hh(ftype(1:8),iftype)
      return
      end subroutine modgrid_ftype_c

      subroutine modgrid_dfile_c(ifile,stdo,iftype, idfile)
      use modgrid_module
      use string_module
      implicit none
      integer,intent(in)    :: ifile(*)
      integer,intent(out)   :: idfile(*)
      integer,intent(in)    :: stdo
      integer,intent(inout) :: iftype(*)

      type(modgrid_struct),pointer    :: obj
      character(len=120)    :: file
      character(len=120)    :: dfile
      character(len=8)      :: ftype
      character(len=8)      :: wtype
      integer     :: status
      integer     :: xhdr=17   !scan header for trace files
      integer     :: yhdr=18   !scan header for trace files
     
      call string_hh2cc(ifile,file)
      call string_hh2cc(idfile,dfile)
      status =  modgrid_rddesc(obj,file,stdo,dfile,wtype,&
       ftype,xhdr,yhdr)
      call string_cc2hh(ftype(1:8),iftype)
      call string_cc2hh(dfile,idfile)
      call modgrid_delete(obj)
      return
      end subroutine modgrid_dfile_c

      integer function modgrid_model_rddesc_c(cmodel,ifname,stdo,idfile,iwtype,&
       iftype,xhdr,yhdr,ivel_type,pcnt,ipnames) result(status)
      use modgrid_frou_module
      use string_module
      type(model_frou_struct),intent(inout)  :: cmodel
      INTEGER,intent(in)     :: ifname(*)
      integer,intent(in)     :: stdo
      INTEGER,intent(inout)  :: idfile(*)    !data file for 1st prop
      INTEGER,intent(inout)  :: iwtype(*)    !word type for 1st prop
      INTEGER,intent(inout)  :: iftype(*)    !file type
      integer,intent(in)     :: xhdr         !for trace scans
      integer,intent(in)     :: yhdr         !for trace scans
      INTEGER,intent(inout)  :: ivel_type(*) !for CPSVEL files
      integer,intent(out)    :: pcnt
      INTEGER,intent(inout)  :: ipnames(*) !return property names

      character(len=120) :: fname
      character(len=160) :: dfile     !data file for 1st prop
      character(len=8)   :: wtype     !word type for 1st prop
      character(len=12)  :: ftype     !file type
      character(len=8)   :: vel_type  !for CPSVEL files
      character(len=64)  :: pnames(16) !return property names
      character(len=240) :: pstr      !return property names
      INTEGER   :: ierr
      INTEGER   :: pn
      type(model_struct),pointer  :: model

      nullify(cmodel%obj)
      call string_hh2cc(ifname,fname)
      call string_hh2cc(ivel_type,vel_type) ! vel_type = ' ' implies no conversion
      dfile = ' '
      wtype = ' '
      ftype = ' '
      pstr  = ' '
      pcnt  = 0
      pnames= 'unknown'
      ipnames(1) = 0

      ierr = model_rddesc(model,fname,stdo,dfile,wtype,ftype,&
      xhdr,yhdr,vel_type,pcnt,pnames)
      cmodel%obj => model
     !if(pcnt==0) stop
      if(ierr < 0) then
        status= -1
        return
      endif
      ! convert to c character strings
      nchar = len_trim(dfile)
      call string_cc2hh(dfile(1:nchar),idfile)
      nchar = len_trim(wtype)
      call string_cc2hh(wtype(1:nchar),iwtype)
      nchar = len_trim(ftype)
      call string_cc2hh(ftype(1:nchar),iftype)
      do pn=1,pcnt
         if(pn==1 ) pstr = 'PNAMES=('//pnames(pn)
         if(pn>1 .and. pn<=pcnt) pstr = trim(pstr)//','//trim(pnames(pn))
         if(pn==pcnt) pstr = trim(pstr)//')'
      enddo
      nchar = len_trim(pstr)
      call string_cc2hh(pstr(1:nchar),ipnames)
      status = 0
      return 
      end function modgrid_model_rddesc_c

      subroutine  modgrid_model_delete_c(cmodel)
       use modgrid_frou_module
       type(model_frou_struct),intent(inout)  :: cmodel
       call model_delete(cmodel%obj)
       return
      end subroutine  modgrid_model_delete_c

      ! return a a property object from a model. 
      ! nattr starts counting from 0
      subroutine modgrid_model_get_component_c(cmodel,nattr,cobj)
       use modgrid_frou_module
       type(model_frou_struct),intent(inout)  :: cmodel
       INTEGER,intent(in)                      :: nattr
       type(modgrid_frou_struct),intent(inout) :: cobj
       type(modgrid_struct),pointer    :: obj
       nullify(obj)
       nullify(cobj%obj)
       call  model_get_component(cmodel%obj,nattr,obj)
       if(.not. associated(obj)) print *,'modgrid_model_get_component_c: ???'
       cobj%obj => obj
       return
      end subroutine modgrid_model_get_component_c

      ! return a count of allocated properties
      INTEGER function modgrid_model_nattr_c(cmodel) result(nattr)
       use modgrid_frou_module
       type(model_frou_struct),intent(inout)  :: cmodel
       nattr = 0
       nattr = model_nattr(cmodel%obj)
      return
      end function modgrid_model_nattr_c
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
